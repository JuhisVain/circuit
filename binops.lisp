(in-package :temp)

(deftype binary-data ()
  `(or bit null))

(deftype valid-logic-input ()
  `(or pin binary-data))

(defmacro def=fun (name lambda-list &body body)
  `(progn
     (declaim (inline ,name))
     (defun ,name ,lambda-list
       (declare (valid-logic-input ,@lambda-list)
		(optimize speed))
       (let ,(loop for var in lambda-list
		   collect `(,var (etypecase ,var
				    (pin (pin-input ,var))
				    (binary-data ,var))))
	 (declare (binary-data ,@lambda-list))
	 ,@body))))

(def=fun =and (a b)
  (when (and a b)
    (logand a b)))

(def=fun =or (a b)
  (when (and a b)
    (logior a b)))

(def=fun =xor (a b)
  (when (and a b)
    (logxor a b)))

(def=fun =not (a)
  (when a
    (logxor a 1)))
