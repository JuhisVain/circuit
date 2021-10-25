(in-package :Temp)

;; Testing too many outputs
'(progn
  (defparameter *sig0* (make-signal))
  (defparameter *sig1* (make-signal))
  (defparameter *or* (make-or))
  (wire ((signal-out *sig0*) 1 (:fork sigfork) 1 (or-a *or*)) (sigfork 1 (signal-out *sig1*)))
  (defparameter *pr1* (make-printer))
  (wire ((or-out *or*) 1 (printer-in *pr1*)))
  (defparameter *sig2* (make-signal))
  (wire ((signal-out *sig2*) 1 (or-b *or*)))

  (set-output (signal-out *sig2*) 1 0)
  (set-output (signal-out *sig1*) 1 0)
  (set-output (signal-out *sig0*) 0 0))
