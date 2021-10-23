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
  (wire ((signal-out *signal-1*) 1 (and-a *and*)))
  (wire ((signal-out *signal-2*) 1 (and-b *and*)))
  (wire ((and-out *and*) 1 (printer-in *printer*)))
  (set-output (signal-out *signal-1*) 1 0)
  (set-output (signal-out *signal-2*) 1 0)
  (roll-events))

(define-ic (signal-source :alias signal)
  :pins
  ((out :output)))

(define-ic printer
  :pins
  ((in :drive))
  :event-processor
  ((in (format t "Receiving ~a!~%" (pin-input in)))))

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
