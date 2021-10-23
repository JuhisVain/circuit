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
  (defparameter *and* (make-and))
  (wire ((csignal-out *signal-1*) 1 (and-a *and*)))
  (wire ((csignal-out *signal-2*) 1 (and-b *and*)))
  (wire ((and-out *and*) 1 (printer-in *printer*))))

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
	      ;; Note that this will only print on change of input.
	      ;; That is the rise or fall of the input signal.
	      (format t "Receiving ~a!~%" (pin-input in))))))
(defun make-printer ()
  (let ((new-printer (raw-make-printer)))
    (setf (drive-pin-chip (printer-in new-printer)) new-printer
	  (ic-event-processor new-printer) (gethash 'printer *event-processor-table*))
    new-printer))

(define-ic (and-gate :alias and)
  :pins
  ((a :drive)
   (b :drive)
   (out :output))
  :event-processor
  (((a b) (cond ((and (bitp (pin-input a)) (bitp (pin-input b)))
		(output out (logand (pin-input a) (pin-input b))))
	       (t
		(floating out))))))

(define-ic (or-gate :alias or)
  :pins
  ((a :drive)
   (b :drive)
   (out :output))
  :event-processor
  (((a b) (cond ((and (bitp (pin-input a)) (bitp (pin-input b)))
		 (output out (logior (pin-input a) (pin-input b))))
		(t
		 (floating out))))))

(define-ic (xor-gate :alias xor)
  :pins
  ((a :drive)
   (b :drive)
   (out :output))
  :event-processor
  (((a b) (cond ((and (bitp (pin-input a)) (bitp (pin-input b)))
		 (output out (logxor (pin-input a) (pin-input b))))
		(t
		 (floating out))))))

(define-ic buffer
  :pins ((in :drive)
	 (out :output))
  :event-processor
  ((in (if (bitp (pin-input in))
	   (output out (pin-input in))
	   (floating out)))))

(define-ic (not-gate :alias not)
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
