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


(define-ic dummy
  :pins ((bus0 :bus)
	 (bus1 :bus)
	 (bus2 :bus)
	 (bus3 :bus)
	 (clock-in1 :drive)
	 (clock-in2 :drive))
  :registers ((clock-counter 0))
  :event-processor
  (((clock-in1 clock-in2)
    ;;(format t "Dummy clock counter ~a at time ~a~%" clock-counter time)
    (cond ((or (and (evenp clock-counter)
	            (rising-p clock-in2))
	       (and (oddp clock-counter)
	            (rising-p clock-in1)))
	   (case clock-counter
	     (6 (format t "   -- Dummy bus outputting zeroes at time ~a!~%" time)
	      (output bus0 0 bus1 0 bus2 0 bus3 0))
	     (9 (format t "   -- Dummy bus will stop outputting at time ~a.~%" time))
	     ((0 10) (floating bus0 bus1 bus2 bus3)))))
    (setf clock-counter (if (< clock-counter 15)
			    (1+ clock-counter)
			    0)))))

'(progn
  (defparameter *clock-1* (make-clock))
  (defparameter *clock-2* (make-clock))
  (defparameter *i4004* (make-i4004))
  (defparameter *pr0* (make-printer))
  (defparameter *pr1* (make-printer))
  (defparameter *pr2* (make-printer))
  (defparameter *pr3* (make-printer))
  (defparameter *dummy* (make-dummy))

  (wire ((clock-out *clock-1*)
	 1 (:fork clock-wake-fork)
	 1 (:fork dummy-fork)
	 1 (i4004-clock-phase-1 *i4004*))
	(clock-wake-fork 99 (clock-waker *clock-1*))
	(dummy-fork
	 1 (dummy-clock-in1 *dummy*)))

  (wire ((clock-out *clock-2*)
	 1 (:fork clock-wake-fork)
	 1 (:fork dummy-fork)
	 1 (i4004-clock-phase-2 *i4004*))
	(clock-wake-fork 99 (clock-waker *clock-2*))
	(dummy-fork
	 1 (dummy-clock-in2 *dummy*)))

  (wire ((i4004-d0 *i4004*)
	 1 (:fork d0-fork)
	 1 (dummy-bus0 *dummy*))
	(d0-fork
	 1 (printer-in *pr0*)))
  (wire ((i4004-d1 *i4004*)
	 1 (:fork d1-fork)
	 1 (dummy-bus1 *dummy*))
	(d1-fork
	 1 (printer-in *pr1*)))
  (wire ((i4004-d2 *i4004*)
	 1 (:fork d2-fork)
	 1 (dummy-bus2 *dummy*))
	(d2-fork
	 1 (printer-in *pr2*)))
  (wire ((i4004-d3 *i4004*)
	 1 (:fork d3-fork)
	 1 (dummy-bus3 *dummy*))
	(d3-fork
	 1 (printer-in *pr3*)))

  (set-output (clock-out *clock-2*) 1 0)
  (set-output (clock-out *clock-1*) 1 50))
