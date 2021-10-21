(in-package :temp)

;;; signal, and, or & not are external in :CL
(unless (fboundp 'bitp)
  (declaim (inline bitp))
  (defun bitp (x)
    (typep x 'bit)))

'(progn
  (defparameter *signal-1* (make-signal))
  (defparameter *signal-2* (make-signal))
  (defparameter *printer* (make-printer))
  (defparameter *and* (make-and-gate))
  (wire ((csignal-out *signal-1*) 1 (and-gate-a *and*)))
  (wire ((csignal-out *signal-2*) 1 (and-gate-b *and*)))
  (wire ((and-gate-out *and*) 1 (printer-in *printer*))))

(defstruct (csignal (:include ic) (:constructor make-signal))
  (out (make-output-pin)))
(store-component-pin-list 'csignal
			  '((out csignal-out)))

(defstruct (printer (:include ic) (:constructor raw-make-printer))
  (in (make-drive-pin :name 'in)))
(store-component-pin-list 'printer
			  '((in printer-in)))
(setf (gethash 'printer *event-processor-table*)
      #'(lambda (chip source time)
	  (macrolet ((output (&rest pin-values)
		       `(progn ,@(loop for (pin value) on pin-values by #'cddr
				       collect `(set-output ,pin ,value time))))
		     (floating (&rest pins)
		       `(progn ,@(loop for pin in pins
				       collect `(cut-output ,pin time)))))
	    (with-pins-and-registers printer chip
	      ;; Note that this will only print on change of input. That is the rise or fall of the input signal.
	      (format t "Receiving ~a!~%" (pin-input in))))))
(defun make-printer ()
  (let ((new-printer (raw-make-printer)))
    (setf (drive-pin-chip (printer-in new-printer)) new-printer
	  (ic-event-processor new-printer) (gethash 'printer *event-processor-table*))
    new-printer))

(defmacro define-ic (name &key pins registers event-processor)
  (let* ((constructor-func (intern (format nil "MAKE-~a" (symbol-name name))))
	 (raw-constructor-func (intern (format nil "RAW-MAKE-~a" (symbol-name name)))))

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
		     collect `(,pin-name ,(intern (format nil "~a-~a" name pin-name))))))
      
      `(progn
	 (defstruct (,name (:include ic) (:constructor ,raw-constructor-func))
	   ,@(struct-pin-list pins))
	 
	 (eval-when (:compile-toplevel :load-toplevel :execute)
	   (format t "Evaluating eval-when~%")
	   (store-component-pin-list ',name ',(accessor-pin-list pins))
	   
	   (setf (gethash ',name *event-processor-table*)
		 #'(lambda (CHIP SOURCE TIME)
		     (macrolet ((output (&rest pin-values)
				  `(progn ,@(loop for (pin value) on pin-values by #'cddr
						  collect `(set-output ,pin ,value time))))
				(floating (&rest pins)
				  `(progn ,@(loop for pin in pins
						  collect `(cut-output ,pin time))))
				(set-register (&rest register-values)
				  `(progn ,@(loop for (register value) on register-values by #'cddr
						  collect `(setf ,register ,value)))))
		       (with-pins-and-registers ,name chip
			 (case source ,@event-processor))))))

	 (defun ,constructor-func ()
	   (let ((chip (,raw-constructor-func)))
	     (with-pins-and-registers ,name chip
	       (setf
		,@(loop for (pin pin-type) in pins
			when (eq pin-type :drive)
			  append `((drive-pin-chip ,pin) chip))
		(ic-event-processor chip) (gethash ',name *event-processor-table*))
	       chip)))))))

(define-ic and-gate
  :pins
  ((a :drive)
   (b :drive)
   (out :output))
  :event-processor
  (((a b) (cond ((and (bitp (pin-input a)) (bitp (pin-input b)))
		(output out (logand (pin-input a) (pin-input b))))
	       (t
		(floating out))))))

(define-ic or-gate
  :pins
  ((a :drive)
   (b :drive)
   (out :output))
  :event-processor
  (((a b) (cond ((and (bitp (pin-input a)) (bitp (pin-input b)))
		 (output out (logior (pin-input a) (pin-input b))))
		(t
		 (floating out))))))

(define-ic xor-gate
  :pins
  ((a :drive)
   (b :drive)
   (out :output))
  :event-processor
  (((a b) (cond ((and (bitp (pin-input a)) (bitp (pin-input b)))
		 (output out (logxor (pin-input a) (pin-input b))))
		(t
		 (floating out))))))

(define-ic buffer-gate
  :pins ((in :drive)
	 (out :output))
  :event-processor
  ((in (if (bitp (pin-input in))
	   (output out (pin-input in))
	   (floating out)))))

(define-ic not-gate
  :pins ((in :drive)
	 (out :output))
  :event-processor
  ((in (if (bitp (pin-input in))
	   (output out (lognot (pin-input in)))
	   (floating out)))))
#|
(DEFUN MAKE-NOT ()
  (LET ((CHIP (RAW-MAKE-NOT)))
    (WITH-PINS-AND-REGISTERS NOT CHIP
      (SETF (DRIVE-PIN-CHIP IN) CHIP
	    (IC-EVENT-PROCESSOR CHIP) (GETHASH 'NOT *EVENT-PROCESSOR-TABLE*))
      CHIP)))

(defstruct (and (:include ic) (:constructor raw-make-and))
  ;; pins:
  (a (make-drive-pin :name 'a))
  (b (make-drive-pin :name 'b))
  (out (make-output-pin)))

(store-component-pin-list 'and
			  '((a and-a)
			    (b and-b)
			    (out and-out)))

(setf (gethash 'and *event-processor-table*)
      #'(lambda (chip source time)
	  (macrolet ((output (&rest pin-values)
		       `(progn ,@(loop for (pin value) on pin-values by #'cddr
				       collect `(set-output ,pin ,value time))))
		     (floating (&rest pins)
		       `(progn ,@(loop for pin in pins
				       collect `(cut-output ,pin time)))))
	    (with-pins-and-registers and chip
	      (case source
		((a b)
		 (cond ((and (bitp (pin-input a)) (bitp (pin-input b)))
			(output out (logand (pin-input a) (pin-input b))))
		       (t
			(floating out)))))))))

(defun make-and ()
  (let ((new-and (raw-make-and)))
    (setf (drive-pin-chip (and-a new-and)) new-and
	  (drive-pin-chip (and-b new-and)) new-and
	  (and-event-processor new-and) (gethash 'and *event-processor-table*))
    new-and))

(defstruct (or (:include ic) (:constructor raw-make-or))
  ;; pins:
  (a (make-drive-pin :name 'a))
  (b (make-drive-pin :name 'b))
  (out (make-output-pin)))
(store-component-pin-list 'or
			  '((a or-a)
			    (b or-b)
			    (out or-out)))
(setf (gethash 'or *event-processor-table*)
      #'(lambda (chip source time)
	  (macrolet ((output (&rest pin-values)
		       `(progn ,@(loop for (pin value) on pin-values by #'cddr
				       collect `(set-output ,pin ,value time))))
		     (floating (&rest pins)
		       `(progn ,@(loop for pin in pins
				       collect `(cut-output ,pin time)))))
	    (with-pins-and-registers or chip
	      (case source
		((a b)
		 (cond ((and (bitp (pin-input a)) (bitp (pin-input b)))
			(output out (logior (pin-input a) (pin-input b))))
		       (t
			(floating out)))))))))

(defun make-or ()
  (let ((new-or (raw-make-or)))
    (setf (drive-pin-chip (or-a new-or)) new-or
	  (drive-pin-chip (or-b new-or)) new-or
	  (ic-event-processor new-or) (gethash 'or *event-processor-table*))
    new-or))
|#
