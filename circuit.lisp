(in-package :temp)

(ql:quickload "serapeum")
(setf *print-circle* t)

(defvar *event-queue* (serapeum:make-heap :key #'car :test #'<))

(defstruct (wire (:print-object print-wire))
  (length)
  ;; connections should hold 2 pins. Either or both can be exchanged for any amount of wires
  (connections () :type list))

(defun seek-pins (wire &optional parent)
  (loop for link in (remove parent (wire-connections wire) :test #'eq)
	when (wire-p link)
	  append (seek-pins link wire)
	when (pin-p link)
	  collect link))

(defun print-wire (wire stream)
  (format stream "#S(WIRE :LENGTH ~a)" (wire-length wire)))

(defstruct pin
  (wire nil :type (or null wire)))

(defstruct (output-pin (:include pin))  (output))

(defstruct (input-pin (:include pin) (:print-object print-input-pin))
  (input))
(defun print-input-pin (pin stream)
  (format stream "#S(INPUT-PIN :INPUT ~a :WIRE ~a)"
	  (input-pin-input pin) (pin-wire pin)))

(defstruct (drive-pin (:include pin) (:print-object print-drive-pin))
  (input)
  (name)
  (chip))
(defun print-drive-pin (pin stream)
  (format stream "#S(DRIVE-PIN :WIRE ~a :INPUT ~a :CHIP ~a)"
	  (pin-wire pin) (drive-pin-input pin) (drive-pin-chip pin)))

(defstruct (bus-pin (:include pin))
  (input)
  (output))

(declaim (inline pin-input (setf pin-input)))
(defun pin-input (pin)
  (typecase pin
    (input-pin (input-pin-input pin))
    (drive-pin (drive-pin-input pin))
    (bus-pin (bus-pin-input pin))
    (t (error "Pin ~a incapable of accepting input!" pin))))

(defun (setf pin-input) (value pin)
  (typecase pin
    (input-pin (setf (input-pin-input pin) value))
    (drive-pin (setf (drive-pin-input pin) value))
    (bus-pin (setf (bus-pin-input pin) value))
    (t (error "Pin ~a does not have input slot to set!" pin))))

(declaim (inline pin-output (setf pin-output)))
(defun pin-output (pin)
  (typecase pin
    (output-pin (output-pin-output pin))
    (bus-pin (bus-pin-output pin))
    (t (error "Pin ~a incapable of outputting!" pin))))

(defun (setf pin-output) (value pin)
  (typecase pin
    (output-pin (setf (output-pin-output pin) value))
    (bus-pin (setf (bus-pin-output pin) value))
    (t (error "Pin ~a does not have output slot to set!" pin))))

(defmacro wire (main &rest branching)
  "Convenience macro to form a wire sequentially starting from a pin and moving
to another pin while passing through forks defined in BRANCHING.
wire
(pin {wire-length pin | (:fork fork-name)}*)
(pin | fork-name {wire-length pin | (:fork fork-name)}*)*"
  (let* ((forklet)
	 (wiring (mapcar #'(lambda (x)
			     (cond ((and (listp x) (eq :fork (car x)))
				    (push `(,(cadr x) (make-wire)) forklet)
				    (cadr x))
				   (t x)))
			 main)))
    `(let ,(reverse forklet)
       ,@(loop for (start length target) on wiring by #'cddr
	       when length ;; when not at end
		 collect `(set-up-wire ,start ,target ,length))
       ,(when branching
	   `(wire ,(car branching) ,@(cdr branching))))))

(defmethod connect-wire (wire (target pin))
  (setf (pin-wire target) wire)
  (push target (wire-connections wire))
  wire)

(defmethod connect-wire (wire (target wire))
  (push wire (wire-connections target))
  (push target (wire-connections wire))
  wire)

(defun set-up-wire (a b &optional (length 1))
  (cond ((wire-p a)
	 (connect-wire a b)
	 (setf (wire-length a) length))
	((wire-p b)
	 (connect-wire b a)
	 (setf (wire-length b) length))
	(t (let ((wire (make-wire :length length)))
	     (connect-wire wire a)
	     (connect-wire wire b)))))

(defun update-wire-state (source-pin time)
  (serapeum:heap-insert
   *event-queue*
   (cons time
	 #'(lambda (time)
	     (labels ((seek-pins (wire time &optional parent)
			(loop for link in (remove parent (wire-connections wire) :test #'eq)
			      when (wire-p link)
				append (seek-pins link (+ time (wire-length wire)) wire)
			      when (pin-p link)
				collect (cons (+ time (wire-length wire))
					      link))))
	       (let ((connected-pins (seek-pins (pin-wire source-pin) time))
		     (current-input nil))
		 ;; Get wire's current signal:
		 (dolist (pin (mapcar #'cdr connected-pins))
		   (typecase pin
		     ((or output-pin bus-pin)
		      (cond ((null current-input)
			     (setf current-input (pin-output pin)))
			    ((and current-input
				  (pin-output pin))
			     (error "2 or more active outputs in wire assembly!"))))))
		 ;; Schedule events:
		 (dolist (sche-pin connected-pins)
		   (propagate (cdr sche-pin) current-input (car sche-pin)))))))))
	       

(defun next-event (queue)
  (destructuring-bind (scheduled-time . event)
      (serapeum:heap-extract-maximum queue)
    (funcall event scheduled-time)))

(defun roll-events ()
  (loop while (serapeum:heap-maximum *event-queue*)
	do (next-event *event-queue*)))

(defun schedule (time action)
  (serapeum:heap-insert *event-queue*
			(cons time action)))

(defun propagate (pin value scheduled-time)
  (typecase pin
    (drive-pin (schedule scheduled-time
			 #'(lambda (time)
			     (setf (pin-input pin) value)
			     (wake (drive-pin-chip pin)
				   (drive-pin-name pin)
				   time))))
    (pin-with-input (schedule scheduled-time
			 #'(lambda (time)
			     (declare (ignore time))
			     (setf (pin-input pin) value))))))

(defun wake (chip source time)
  (funcall (ic-event-processor chip) chip source time))

(defvar *event-processor-table* (make-hash-table :test 'eq))
(defun event-processor (component)
  (gethash component *event-processor-table*))

(defstruct operation
  (function)
  (length)) ;; length of op-code in bits??

(defstruct (op-node (:print-object print-op-node))
  (operation)
  (zero)
  (one))

(defun print-op-node (op-node stream)
  (print-unreadable-object (op-node stream :type t)
    (format stream "~a nodes in tree" (count-nodes op-node))))

(defun add-op-code (operation op-code-listing op-tree)
  (let ((op-code-listing ;; Cull trailing var symbols
	  (reverse (loop for rest on (reverse op-code-listing)
			 when (bitp (car rest))
			   do (return rest)))))
    (labels ((add-op-code-without-vars (op-code-list op-tree)
	       (when (op-node-operation op-tree)
		 (warn "Adding new operation ~a where one already exists!"
		       op-code-listing))
	       (let ((bit (car op-code-list)))
		 (cond ((null bit) (setf (op-node-operation op-tree) operation))
		       (t
			(case bit
			  (0 (add-op-code-without-vars
			      (cdr op-code-list)
			      (or (op-node-zero op-tree)
				  (setf (op-node-zero op-tree) (make-op-node)))))
			  (1 (add-op-code-without-vars
			      (cdr op-code-list)
			      (or (op-node-one op-tree)
				  (setf (op-node-one op-tree) (make-op-node)))))
			  (otherwise
			   (add-op-code-without-vars
			    (cdr op-code-list)
			    (or (op-node-zero op-tree)
				(setf (op-node-zero op-tree) (make-op-node))))
			   (add-op-code-without-vars
			    (cdr op-code-list)
			    (or (op-node-one op-tree)
				(setf (op-node-one op-tree) (make-op-node)))))))))))
      (add-op-code-without-vars op-code-listing op-tree))))

(defun count-nodes (op-tree)
  (when op-tree
    (+ 1
       (or (count-nodes (op-node-zero op-tree))
	   0)
       (or (count-nodes (op-node-one op-tree))
	   0))))

(defun list-chip-ops (chip)
  (let ((found))
    (labels ((traverse-collect (op-tree id)
	       (cond ((op-node-operation op-tree)
		      (push (list (op-node-operation op-tree) (bits id)) found))
		     (t
		      (when (op-node-zero op-tree)
			(traverse-collect (op-node-zero op-tree) (bits id 0)))
		      (when (op-node-one op-tree)
			(traverse-collect (op-node-one op-tree) (bits id 1)))))))
      (traverse-collect (chip-op-lib chip) #*)
      found)))

(defun locate-op-node (op-code op-tree)
  (cond ((op-node-operation op-tree) op-tree)
	((zerop (length op-code)) op-tree)
	((zerop (bit op-code 0))
	 (locate-op-node (subseq op-code 1) (op-node-zero op-tree)))
	(t
	 (locate-op-node (subseq op-code 1) (op-node-one op-tree)))))

(defun chip-op (chip op-code)
  (let ((op (op-node-operation (locate-op-node op-code (chip-op-lib chip)))))
    (when op
      (values (operation-function op)
	      (operation-length op)))))

(defun collect-op-code-variables (op-code-spec)
  (do* ((bits op-code-spec (cdr bits))
	(bit (car bits) (car bits))
	(index 0 (1+ index))
	(vars ()))
       ((null bits) vars)
    (when (symbolp bit)
      (cond ((assoc bit vars)
	     (setf (cdr (assoc bit vars)) (append (cdr (assoc bit vars)) (list index))))
	    (t (push (list bit index) vars))))))

(defun add-op-args (chip-type op bits)
  "Stores operation arg lists as (OP BINARY-CODE (ARG-NAME ARG-BIT-LENGTH)*)"
  (let ((op-lambda-form (or (assoc op
				   (gethash chip-type *op-library*))
			    (car (push (list op)
				       (gethash chip-type *op-library*))))))
    (setf (cdr op-lambda-form)
	  (list* (bits (loop for b in bits when (bitp b) collect b))
		 (mapcar #'(lambda (arg)
			     (list (car arg) (length (cdr arg))))
			 (sort (collect-op-code-variables bits)
			       #'< :key #'cadr))))))

(defmacro defoperation (chip-type op-name (&rest bits) &body body)
  (let ((variables (collect-op-code-variables bits)))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
	 (add-op-args ',chip-type ',op-name ',bits)
	 (add-op-code
	  (make-operation
	   :function
	   #'(lambda (op-code chip trigger time)
	       (declare (ignorable op-code chip trigger time))
	       (let ,(mapcar #'(lambda (op-var-indexes)
				 `(,(car op-var-indexes)
				   (make-array ,(length (cdr op-var-indexes))
					       :element-type 'bit
					       :initial-contents (mapcar #'(lambda (index)
									     (bit op-code index))
									 ',(cdr op-var-indexes)))))
		      variables)
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
			      `(execute-operation chip ,op trigger time)))
		   (with-pins-and-registers ,chip-type chip
		     ,@body))))
	   :length ,(length bits))
	  ',bits
	  (chip-op-lib ',chip-type))))))

(defmacro add-to-cycle (&rest trigger-case-statements)
  `(case trigger
     ,@trigger-case-statements))

(defun bit-truep (bit-array)
  (not (bit-zerop bit-array)))

(defun execute-operation (chip op-code trigger time)
  ;;(format t "Chip type ~a~% - Op code ~a~%" (type-of chip) op-code)
  (funcall (chip-op chip op-code) op-code chip trigger time))

(defmacro with-pins-and-registers (component-type component &body body)
  `(symbol-macrolet ,(loop for (name accessor) in (append (list-pins component-type)
							  (list-registers component-type))
			   collect `(,name ,(list accessor component)))
     ,@body))

(defun set-output (pin value time)
  ;;(format t "Setting old ~a output to ~a at pin ~a!~%" (output-pin-output pin) value pin)
  (when (and (not (eq value (pin-output pin))) ; No updates if no change
	     (pin-wire pin)) ; a wire is connected?
    ;;(format t "Changes!~%")
    (setf (pin-output pin) value)
    (update-wire-state pin
		       (+ time
			  (wire-length (pin-wire pin))))))

(defun cut-output (pin time)
  (set-output pin nil time))

(defun bit-right-shift (bit-vector &optional (delta 1))
  (let ((shifted (make-array (length bit-vector) :element-type 'bit :initial-element 0)))
    (dotimes (i (- (length bit-vector) delta))
      (setf (aref shifted i) (aref bit-vector (+ i delta))))
    shifted))

(defun bit-left-shift (bit-vector &optional (delta 1))
  (let ((shifted (make-array (length bit-vector) :element-type 'bit :initial-element 0)))
    (do ((i 0 (1+ i)))
	((= i (- (length bit-vector) delta))
	 (values shifted (aref bit-vector i))) ; shifted bits + carry bit
      (setf (aref shifted (+ i delta)) (aref bit-vector i)))))

(defun bits (&rest bits)
  ;; Flatten bit vectors into list:
  (let ((bits (loop for e in bits
		    when (bitp e) collect e
		    when (vectorp e)
		      append (loop for bit across e
				   collect bit)
		    when (listp e)
		      append (loop for bit in e
				   collect bit))))
    (make-array (length bits) :element-type 'bit :initial-contents bits)))

(declaim (inline make-bits))
(defun make-bits (n &optional (default 0))
  (declare (fixnum n)
	   (bit default)
	   (optimize speed))
  (make-array n :element-type 'bit :initial-element default))

(defun bit-zero (a)
  "Set all bits to 0"
  (declare (simple-bit-vector a)
	   (optimize speed))
  (dotimes (i (array-total-size a))
    (setf (bit a i) 0)))

(defun bit-plus (a b)
  (do* (c)
       ((bit-zerop a) b)
    (multiple-value-bind (shifted carry)
	  (bit-left-shift (bit-and a b))
      (setf c shifted
	    b (bit-xor a b)
	    a c)
      (when (bit-zerop a)
	(return-from bit-plus (values b carry))))))

(defun bit-zerop (bit-array)
  (loop for bit across bit-array
	unless (zerop bit)
	  do (return nil)
	finally (return t)))

(defun bit-1+ (bits)
  (let ((one (make-array (length bits) :element-type 'bit :initial-element 0)))
    (setf (bit one 0) 1)
    (bit-plus bits one)))

(defun bit-neg (a)
  "2's complement negate."
  (loop for b across a
	for index from 0
	with neg = (make-array (length a) :element-type 'bit :initial-element 0)
	do (setf (bit neg index) (logxor b 1))
	finally (return (bit-1+ neg))))

(defun bit-integer (bit-array)
  (loop for bit across bit-array
	for power from 0
	sum (* bit (expt 2 power))))

(defun bit-sign (bit-array)
  (bit bit-array (1- (length bit-array))))

(defun bit-minusp (bit-array)
  (= 1 (bit-sign bit-array)))

(defun bit-plusp (bit-array)
  (= 0 (bit-sign bit-array)))

(defun bit-= (a b)
  (declare (bit-vector a b))
  (equal a b))

(defun uinteger-bits (integer &optional (minimum-length 1))
  (cond ((zerop integer)
	 (make-array minimum-length :element-type 'bit :initial-element 0))
	(t
	 (let ((bits nil))
	   (loop while (plusp integer)
		 for i from 1
		 do (multiple-value-bind (quotient remainder)
			(floor integer 2)
		      (setf integer quotient)
		      (push remainder bits))
		 finally (if (< i minimum-length)
			     (setf bits (append (make-list (- minimum-length i)
							   :initial-element 0)
						bits))))
	   (reverse (bits bits))))))

(defun signed-bit-integer (bit-array)
  (cond ((bit-plusp bit-array)
	 (bit-integer bit-array))
	(t (- (1+ (loop for bit across bit-array
			for power from 0
			sum (* (logxor bit 1) (expt 2 power))))))))
