(in-package :temp)

(defstruct ic
  (name)
  (event-processor))

(defstruct (cpu (:include ic))
  (word-size 8 :type (unsigned-byte 16))
  (byte-size 8 :type (unsigned-byte 16))
  (endianness :little-endian :type (member :little-endian :big-endian))
  (op-lib))

(defvar *component-pin-lib* (make-hash-table :test 'eq))
(defvar *component-register-lib* (make-hash-table :test 'eq))
(defun store-component-pin-list (component-type pin-data)
  (setf (gethash component-type *component-pin-lib*) pin-data))
(defun list-pins (component-type)
  (gethash component-type *component-pin-lib*))

(defun store-component-register-list (component-type register-data)
  (setf (gethash component-type *component-register-lib*) register-data))
(defun list-registers (component-type)
  (gethash component-type *component-register-lib*))

(defvar *chip-pinout-lib* (make-hash-table :test 'eq))

(defun record-chip-pin (chip-type pin-name accessor)
  "Stores and associates PIN-NAME with CHIP-TYPE to be accessed with function
PIN using ACCESSOR."
  (declare (symbol chip-type pin-name)
	   ((or symbol function) accessor))
  (let ((pin-table (or (gethash chip-type *chip-pinout-lib*)
		       (setf (gethash chip-type *chip-pinout-lib*)
			     (make-hash-table :test 'eq)))))
    (setf (gethash pin-name pin-table)
	  accessor)))

(defun pin (chip pin-name)
  (funcall (gethash pin-name (gethash (type-of chip) *chip-pinout-lib*))
	   chip))


(defvar *op-code-library* (make-hash-table :test 'eq))
(defvar *op-mnemonic-library* (make-hash-table :test 'eq))

(defun chip-op-lib (chip)
  (gethash (typecase chip
	     (symbol chip)
	     (t (type-of chip)))
	   *op-code-library*))

(defun list-op-mnemonics (chip)
  (gethash (typecase chip
	     (symbol chip)
	     (t (type-of chip)))
	   *op-mnemonic-library*))

(defun build-registry-build-form (reg-forms)
  (let ((reg-alist))
    (dolist (form reg-forms)
      (cond
	;; 2 elements -> second must be width
	((= (length form) 2)
	 (push (list (car form)
		     `(make-array ,(cadr form) :element-type 'bit))
	       reg-alist))
	((= (length form) 3)
	 (error "Strange register construction form ~a." form))
	(t
	 (let* ((total-width (loop for e in form
				   when (integerp e)
				     sum e))
		(master-array `(make-array ,total-width :element-type 'bit))
		(sub-arrays
		  (loop for (sub width) on (cdr form) by #'cddr
			for index = 0 then (+ index width)
			collect (list sub
				      `(make-array ,width :element-type 'bit
							  :displaced-to ,(car form);master-array
							  :displaced-index-offset ,index)))))
	   (dolist (sub sub-arrays)
	     (push sub reg-alist))
	   
	   (push (list (car form)
		       master-array)
		 reg-alist)))))
    reg-alist))
	

(defmacro define-ic (name &key cpu pins registers aux event-processor secondary-functions)
  (let* ((alias (or (when (listp name)
		      (getf (cdr name) :alias))
		    name))
	 (conc-name (when (and (listp name) (getf (cdr name) :alias))
		      (intern (concatenate 'string (symbol-name alias) "-"))))
	 (name (if (symbolp name) name (car name)))
	 (constructor-func (intern (format nil "MAKE-~a" (symbol-name alias))))
	 (raw-constructor-func (intern (format nil "RAW-MAKE-~a" (symbol-name alias))))
	 (register-names (remove-if #'numberp (apply #'append registers))))
    (format t "Name ~a  alias ~a~%" name alias)
    (labels ((struct-pin-list (pin-list)
	       (let ((keyword-plist '(:input make-input-pin
				      :drive make-drive-pin
				      :output make-output-pin
				      :bus make-bus-pin)))
		 (loop for (pin-name pin-type) in pin-list
		       collect `(,pin-name
				 (,(getf keyword-plist pin-type)
				  ,@(case pin-type (:drive `(:name ',pin-name))))))))
	     (accessor-pin-list (pin-list)
	       (loop for (pin-name x) in pin-list
		     collect `(,pin-name ,(intern (format nil "~a-~a" alias pin-name)))))
	     (accessor-register-list (reg-list)
	       (loop for reg-name in reg-list
		     collect `(,reg-name ,(intern (format nil "~a-~a" alias reg-name))))))
      
      `(progn
	 (defstruct (,name (:include ,(if cpu 'cpu 'ic))
			   ,@(when conc-name
			      `((:conc-name ,conc-name)))
			   (:constructor ,raw-constructor-func))
	   ,@(struct-pin-list pins)
	   ,@register-names
	   ,@aux)
	 
	  ;; compile & load work with clozure, sbcl needs execute
	 (eval-when (:compile-toplevel :load-toplevel :execute)
	   (setf (gethash ',name *op-code-library*) (make-op-node))
	   (store-component-pin-list ',name ',(accessor-pin-list pins))
	   (store-component-register-list
	    ',name
	    ',(accessor-register-list (append (mapcar #'car aux)
					      register-names)))

	   ,(when event-processor
	      `(setf (gethash ',name *event-processor-table*)
		     #'(lambda (CHIP SOURCE TIME)
			 (macrolet ((output (&rest pin-values)
				      `(progn
					 ,@(loop for (pin value) on pin-values by #'cddr
						 collect `(set-output ,pin ,value time))))
				    (bus-output (bit-array &rest bus-pins)
				      (let ((bits (gensym)))
					`(let ((,bits ,bit-array))
					   ,@(loop for pin in bus-pins
						   for index from 0
						   collect `(set-output
							     ,pin (bit ,bits ,index) time)))))
				    (floating (&rest pins)
				      `(progn
					 ,@(loop for pin in pins
						 collect `(cut-output ,pin time))))
				    (set-register (&rest register-values)
				      `(progn
					 ,@(loop for (register value) on register-values by #'cddr
						 collect `(setf ,register ,value))))
				    (execute (op) ; requires op library
				      `(execute-operation chip ,op trigger time))
				    (rising-p (pin-signal)
				      `(and (eq source ',pin-signal)
					    (= (pin-input ,pin-signal) 1)))
				    (falling-p (pin-signal)
				      `(and (eq source ',pin-signal)
					    (zerop (pin-input ,pin-signal)))))
			   
			   (with-pins-and-registers ,name chip
			     (labels
				 ,(when secondary-functions
				    (loop for (fname lambda-list . body) in secondary-functions
					  collect `(,fname ,lambda-list
							   ,@body)))
			       (case source ,@event-processor))))))))
	 
	 (defun ,constructor-func ()
	   (let ((chip (,raw-constructor-func)))
	     ,@(when cpu
		 `((setf (cpu-word-size chip) ,(getf cpu :word-size)
			 (cpu-byte-size chip) ,(or (getf cpu :byte-size)
						   (getf cpu :word-size))
			 (cpu-endianness chip) ,(or (getf cpu :endianness)
						    :little-endian)
			 (cpu-op-lib chip) (chip-op-lib chip))))
	     (with-pins-and-registers ,name chip
	       (setf
		,@(loop for (pin pin-type) in pins
			when (eq pin-type :drive)
			  append `((drive-pin-chip ,pin) chip))
		(ic-event-processor chip) (gethash ',name *event-processor-table*))

	       (setf
		,@(apply #'append (build-registry-build-form registers)))
	       
	       chip)))))))
