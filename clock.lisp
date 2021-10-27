(in-package :temp)

(define-ic clock
  :pins
  ((out :output)
   (waker :drive)) ; connect to OUT for self driving clock
  :event-processor
  ((waker
    (format t "Waking at ~a~%" time)
    (when (bitp (pin-input waker))
      (output out (=not (pin-input waker)))))))
