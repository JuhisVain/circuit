(in-package :temp)

(defmacro define-ic (name &key pins registers event-processor secondary-functions)
  (let* ((alias (or (when (listp name)
		      (getf (cdr name) :alias))
		    name))
	 (conc-name (when (and (listp name) (getf (cdr name) :alias))
		      (intern (concatenate 'string (symbol-name alias) "-"))))
	 (name (if (symbolp name) name (car name)))
	 (constructor-func (intern (format nil "MAKE-~a" (symbol-name alias))))
	 (raw-constructor-func (intern (format nil "RAW-MAKE-~a" (symbol-name alias)))))
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
	       (loop for (reg-name . rest) in reg-list
		     collect `(,reg-name ,(intern (format nil "~a-~a" alias reg-name))))))
      
      `(progn
	 (defstruct (,name (:include ic)
			   ,@(when conc-name
			      `((:conc-name ,conc-name)))
			   (:constructor ,raw-constructor-func))
	   ,@(struct-pin-list pins)
	   ,@registers)
	 
	  ;; compile & load work with clozure, sbcl needs execute
	 (eval-when (:compile-toplevel :load-toplevel :execute)
	   (store-component-pin-list ',name ',(accessor-pin-list pins))
	   (store-component-register-list ',name ',(accessor-register-list registers))

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
	     (with-pins-and-registers ,name chip
	       (setf
		,@(loop for (pin pin-type) in pins
			when (eq pin-type :drive)
			  append `((drive-pin-chip ,pin) chip))
		(ic-event-processor chip) (gethash ',name *event-processor-table*))
	       chip)))))))
