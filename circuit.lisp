(in-package :temp)

(ql:quickload "serapeum")
(setf *print-circle* t)

(defvar *event-queue* (serapeum:make-heap :key #'car :test #'<))

#|
(defstruct clock
  counter
  interval
  (value nil)
  (out :type wire))
|#
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

;;A sequential definition might be easier:
'(wire
  ((i4004-d0 cpu)
   5
   (:fork rom0-fork)
   2
   (:fork rambank12-fork)
   4
   (:fork rom1-fork)
   8
   (:fork rambank34-fork)
   ;;...etc
   )
  ;;branching
  (rom0-fork
   2
   (i4001-d0 rom0))
  (rambank12fork
   6
   (:fork rams0010-fork)
   6
   (:fork rams0111-fork)
   6
   (:fork rams0212-fork)
   6
   (:fork rams0313-fork)
   1
   (i4002-d0 ram03)))

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
	 (unless (wire-length a)
	   (setf (wire-length a) length)))
	((wire-p b)
	 (connect-wire b a)
	 (unless (wire-length b)
	   (setf (wire-length b) length)))
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
		     ((or output-pin bus-pin) (cond ((null current-input)
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
    (input-pin (schedule scheduled-time
			 #'(lambda (time)
			     (declare (ignore time))
			     (setf (pin-input pin) value))))))

(defun wake (chip source time)
  (funcall (ic-event-processor chip) chip source time))

(defun register (name chip)
  (gethash name (ic-registers chip)))
(defun (setf register) (value name chip)
  (setf (gethash name (ic-registers chip)) value))

(defstruct ic
  (name)
  (event-processor))

(defvar *event-processor-table* (make-hash-table :test 'eq))
(defun event-processor (component)
  (gethash component *event-processor-table*))

(defstruct (i4004 (:include ic) (:constructor raw-make-i4004))
  ;;Pinout
  (d0 (make-bus-pin) :type bus-pin)
  (d1 (make-bus-pin) :type bus-pin)
  (d2 (make-bus-pin) :type bus-pin)
  (d3 (make-bus-pin) :type bus-pin)
  (v-ss)
  (clock-phase-1 (make-drive-pin :name 'clock-phase-1) :type drive-pin)
  (clock-phase-2 (make-drive-pin :name 'clock-phase-2) :type drive-pin)
  (sync (make-output-pin) :type output-pin)
  (reset (make-input-pin) :type input-pin)
  (test (make-input-pin) :type input-pin)
  (cm-rom (make-output-pin) :type output-pin)
  (v-dd)
  (cm-ram-3 (make-output-pin) :type output-pin)
  (cm-ram-2 (make-output-pin) :type output-pin)
  (cm-ram-1 (make-output-pin) :type output-pin)
  (cm-ram-0 (make-output-pin) :type output-pin)
  ;;Registers
  (clock-counter 0)
  (ROM-address #*000000000000)
  (RAM-command-line 0)
  (op-memory (list #*00000000 #*00000000)) ; one-byte-op & two-byte-op's vars
  (op-memory-pointer 0) ; 0 or 1, index of op-memory to fill, usually 0
  (index-register (make-array 64 :element-type 'bit))
  (carry-bit 0)
  (accumulator (make-array 4 :element-type 'bit))
  (stack (make-array 3
		     :initial-contents
		     (list 
		      (make-array 12 :element-type 'bit)
		      (make-array 12 :element-type 'bit)
		      (make-array 12 :element-type 'bit))))
  (stack-pointer 0))

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

(store-component-pin-list
 'i4004
 '((d0 i4004-d0)
   (d1 i4004-d1)
   (d2 i4004-d2)
   (d3 i4004-d3)
   (v-ss i4004-v-ss)
   (clock-phase-1 i4004-clock-phase-1)
   (clock-phase-2 i4004-clock-phase-2)
   (sync i4004-sync)
   (reset i4004-reset)
   (test i4004-test)
   (cm-rom i4004-cm-rom)
   (v-dd i4004-v-dd)
   (cm-ram-3 i4004-cm-ram-3)
   (cm-ram-2 i4004-cm-ram-2)
   (cm-ram-1 i4004-cm-ram-1)
   (cm-ram-0 i4004-cm-ram-0)))

(store-component-register-list
 'i4004
 '((clock-counter i4004-clock-counter)
   (ROM-address i4004-ROM-address)
   (RAM-command-line i4004-RAM-command-line)
   (op-memory i4004-op-memory)
   (op-memory-pointer i4004-op-memory-pointer)
   (index-register i4004-index-register)
   (carry-bit i4004-carry-bit)
   (accumulator i4004-accumulator)
   (stack i4004-stack)
   (stack-pointer i4004-stack-pointer)))

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

(defun make-i4004 ()
  (let ((new-i4004 (raw-make-i4004)))
    (setf (drive-pin-chip (i4004-clock-phase-1 new-i4004)) new-i4004
	  (drive-pin-chip (i4004-clock-phase-2 new-i4004)) new-i4004)
    new-i4004))

(defvar *op-code-library* (make-hash-table :test 'eq))

(defun chip-op-lib (chip)
  (gethash (typecase chip
	     (symbol chip)
	     (t (type-of chip)))
	   *op-code-library*))

(defstruct operation
  (function)
  (length)) ;; length of op-code in bits??

(defstruct op-node
  (operation)
  (zero)
  (one))

(setf (gethash 'i4004 *op-code-library*) (make-op-node))

(defun add-op-code (operation op-code-listing op-tree)
  (let ((op-code-listing ;; Cull trailing var symbols
	  (reverse (loop for rest on (reverse op-code-listing)
			 when (bitp (car rest))
			   do (return rest)))))
    (labels ((add-op-code-without-vars (op-code-list op-tree)
	       (when (op-node-operation op-tree)
		 (error "Attempting to add new operation where one already exists!"))
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

(defmacro defoperation (chip mnemonic (&rest bits) &body body)
  (let ((variables (collect-op-code-variables bits)))
    `(add-op-code
      (make-operation :function
		      #'(lambda (op-code chip trigger)
			  (let ,(mapcar #'(lambda (op-var-indexes)
					    `(,(car op-var-indexes)
					      (make-array ,(length (cdr op-var-indexes))
							  :element-type 'bit
							  :initial-contents (mapcar #'(lambda (index)
											(bit op-code index))
										    ',(cdr op-var-indexes)))))
				 variables)
			    ,@body))
		      :length ,(length bits))
      ',bits
      (chip-op-lib ',chip))))

(defmacro add-to-cycle (&rest trigger-case-statements)
  `(case trigger
     ,@trigger-case-statements))

(defun bit-truep (bit-array)
  (not (bit-zerop bit-array)))

'(defoperation i4004 SRC (0 1 0 0 1 R R R)
  (let* ((index (bit-integer R))
	 (address (subseq index-register index (+ index 8))))
    (add-to-cycle
     ((12 13) (bitarray-output (d0 d1 d2 d3) (subseq address 0 4)))
     ((14 15) (bitarray-output (d0 d1 d2 d3) (subseq address 4 8))))))

'(defoperation i4004 JCN (1 0 0 0 C4 C3 C2 C1 A2 A2 A2 A2 A1 A1 A1 A1)
  (case (i4004-op-memory-pointer chip)
    (0
     (add-to-cycle
      (15 (setf (i4004-op-memory-pointer chip) 1))))
    (1
     (add-to-cycle
      (15
       (let ((jump (or (and (bit-zerop C1)
			    (or
			     (and (bit-truep C2) (bit-zerop (i4004-accumulator chip)))
			     (and (bit-truep C3) (not (zerop (i4004-carry-bit chip))))
			     (and (bit-truep C4) (zerop (pin-input (i4004-test chip))))))
		       (and (bit-truep C1)
			    (not (or
				  (and (bit-truep C2) (bit-zerop (i4004-accumulator chip)))
				  (and (bit-truep C3) (not (zerop (i4004-carry-bit chip))))
				  (and (bit-truep C4) (zerop (pin-input (i4004-test chip))))))))))
	 (when jump
	   (setf (i4004-rom-address chip)
		 (bit-ior (bit-and (i4004-rom-address chip)
				   #*000000001111)
			  (bits A1 A2 #*0000)))) ;; Unsure bit order!!
	 (setf (i4004-op-memory-pointer chip) 0)))))))

(defun execute-operation (chip op-code trigger)
  (funcall (chip-op chip op-code) op-code chip trigger))

(defmacro with-pins-and-registers (component-type component &body body)
  `(symbol-macrolet ,(loop for (name accessor) in (append (list-pins component-type)
							  (list-registers component-type))
			   collect `(,name ,(list accessor component)))
     ,@body))

'(setf (gethash 'i4004 *event-processor-table*)
  #'(lambda (chip source time)
      (macrolet ((output (&rest pin-values)
		   `(progn ,@(loop for (pin value) on pin-values by #'cddr
				   collect `(set-output ,pin ,value time))))
		 (floating (&rest pins) ; Maybe not the right word
		   `(progn ,@(loop for pin in pins
				   collect `(cut-output ,pin time))))
		 (set-register (&rest register-values)
		   `(progn ,@(loop for (register value) on register-values by #'cddr
				   collect `(setf ,register ,value))))
		 (execute (op)
		   `(execute-operation chip
				       (if (= op-memory-pointer 0)
					   (car ,op)
					   (apply #'bits ,op))
				       trigger)))
	(with-pins-and-registers i4004 chip
	  (flet ((TRIGGER-clock-counter (trigger)
		   (case trigger
		     (0 (output sync 1
				cm-rom 1
				d0 (bit 0 ROM-address)
				d1 (bit 1 ROM-address)
				d2 (bit 2 ROM-address)
				d3 (bit 3 ROM-address)))
		     (2 (output d0 (bit 4 ROM-address)
				d1 (bit 5 ROM-address)
				d2 (bit 6 ROM-address)
				d3 (bit 7 ROM-address)))
		     (4 (output d0 (bit 8 ROM-address)
				d1 (bit 9 ROM-address)
				d2 (bit 10 ROM-address)
				d3 (bit 11 ROM-address)))
		     (5 (output cm-rom 0))
		     (6
		      (output cm-rom 1)
		      (floating d0 d1 d2 d3))
		     (7
		      (set-register (nth op-memory-pointer op-memory)
				    (bits d0 d1 d2 d3 #*0000)))
		     (8
		      (when (equal (nth 0 op-memory)
				   #*01110000)
			(output cm-rom 0
				cm-ram-0 0
				cm-ram-1 0
				cm-ram-2 0
				cm-ram-3 0)))
		     (9
		      (set-register (nth op-memory-pointer op-memory)
				    (bit-ior op-memory
					     (bits #*0000 d0 d1 d2 d3))))

		     (10 (output cm-rom 1)
		      (case RAM-command-line
			(0 (output cm-ram-0 1
				   cm-ram-1 0
				   cm-ram-2 0
				   cm-ram-3 0))
			(1 (output cm-ram-0 0
				   cm-ram-1 1
				   cm-ram-2 0
				   cm-ram-3 0))
			(2 (output cm-ram-0 0
				   cm-ram-1 0
				   cm-ram-2 1
				   cm-ram-3 0))
			(3 (output cm-ram-0 0
				   cm-ram-1 0
				   cm-ram-2 0
				   cm-ram-3 1))))
		     (12 (execute op-memory))
		     (13
		      (when (equal (bit-and (first op-memory) #*11110000)
				   #*01000000)
			(output cm-ram-0 0
				cm-ram-1 0
				cm-ram-2 0
				cm-ram-3 0))
		      (output cm-rom 1)
		      (execute op-memory))
		     (14 (output sync 0)
		      (case RAM-command-line
			(0 (output cm-ram-0 1
				   cm-ram-1 0
				   cm-ram-2 0
				   cm-ram-3 0))
			(1 (output cm-ram-0 0
				   cm-ram-1 1
				   cm-ram-2 0
				   cm-ram-3 0))
			(2 (output cm-ram-0 0
				   cm-ram-1 0
				   cm-ram-2 1
				   cm-ram-3 0))
			(3 (output cm-ram-0 0
				   cm-ram-1 0
				   cm-ram-2 0
				   cm-ram-3 1)))
		      (execute op-memory)))))
	    (case source
	      (('clock-phase-1 'clock-phase-2)
	       (TRIGGER-clock-counter (set-register clock-counter (1+ clock-counter)))))
	    )))))



(defun set-output (pin value time)
  (format t "Setting old ~a output to ~a at pin ~a!~%" (output-pin-output pin) value pin)
  (when (not (eq value (output-pin-output pin))) ; No updates if no change
    (format t "Changes!~%")
    (setf (output-pin-output pin) value)
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
				   collect bit))))
    (make-array (length bits) :element-type 'bit :initial-contents bits)))

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

(defun signed-bit-integer (bit-array)
  (cond ((bit-plusp bit-array)
	 (bit-integer bit-array))
	(t (- (1+ (loop for bit across bit-array
			for power from 0
			sum (* (logxor bit 1) (expt 2 power))))))))



#|
(defcycle i4004-cycle
  send-rom-address
  receive-instruction
  execute-instruction)

(defcyclephase send-rom-address (clock-counter)
  ((0 1) (output sync 1
		 cm-rom 1
		 d0 (bit 0 ROM-address)
		 d1 (bit 1 ROM-address)
		 d2 (bit 2 ROM-address)
		 d3 (bit 3 ROM-address)))
  ((2 3) (output sync 1
		 cm-rom 1
		 d0 (bit 4 ROM-address)
		 d1 (bit 5 ROM-address)
		 d2 (bit 6 ROM-address)
		 d3 (bit 7 ROM-address)))
  (4 (output sync 1
	     cm-rom 1
	     d0 (bit 8 ROM-address)
	     d1 (bit 9 ROM-address)
	     d2 (bit 10 ROM-address)
	     d3 (bit 11 ROM-address)))
  (5 (output sync 1
	     cm-rom 0
	     d0 (bit 8 ROM-address)
	     d1 (bit 9 ROM-address)
	     d2 (bit 10 ROM-address)
	     d3 (bit 11 ROM-address))))

(defcyclephase receive-instruction (clock-counter)
  (6 (output sync 1
	     cm-rom 1))
  (7 (output sync 1
	     cm-rom 1)
     (set-register op-memory
		   (bits d0 d1 d2 d3 #*0000)))
  (8 (output sync 1)
     (cond ((equal op-memory #*01110000)
	    (output cm-rom 0
		    cm-ram-0 0
		    cm-ram-1 0
		    cm-ram-2 0
		    cm-ram-3 0)
	    (t (output cm-rom 1)))))
  (9 (output sync 1)
     (set-register op-memory
		   (bit-ior op-memory
			    (bits #*0000 d0 d1 d2 d3)))
     (cond ((equal op-memory #*01110000)
	    (output cm-rom 0
		    cm-ram-0 0
		    cm-ram-1 0
		    cm-ram-2 0
		    cm-ram-3 0)
	    (t (output cm-rom 1))))))

(defcyclephase execute-instruction (clock-counter)
  ((10 11) (output sync 1
		   cm-rom 1)
	   (cm-ram RAM-command-line))
  (12 (output sync 1
	      cm-rom 1)
      (cm-ram RAM-command-line)
      (execute op-memory))
  (13 (output sync 1)
      (cond ((equal (bit-and op-memory #*11110000)
		    #*01000000)
	     (cm-ram nil)
	     (output cm-rom 1))
	    (t (cm-ram RAM-command-line)
	       (output cm-rom 1)))
      (execute op-memory))
  ((14 15) (output sync 0
		   cm-rom 1)
	   (cm-ram RAM-command-line)
	   (execute op-memory)))

'(defoperation SRC (0 1 0 0 0 R R R)
  (let* ((index (bit-integer R))
	 (address (subseq index-register index (+ index 8))))
    (add-to-cycle
     ((12 13) (bitarray-output (d0 d1 d2 d3) (subseq address 0 4)))
     ((14 15) (bitarray-output (d0 d1 d2 d3) (subseq address 4 8))))))


(defstruct ic
  (name "i4004")
  (pinout d0
	   d1
	   d2
	   d3
	   v-ss
	   clock-phase-1
	   clock-phase-2
	   sync
	   reset
	   test
	   cm-rom
	   v-dd
	   cm-ram-3
	   cm-ram-2
	   cm-ram-1
	   cm-ram-0))


(defmacro defic (name &key pinout registers)
  )

;;could actually select more than one
(defmacro cm-ram (pin) ;;selector in pinout list will generate a local macro like this
  `(case ,pin
     (3 (output cm-ram-3 1
		cm-ram-2 0
		cm-ram-1 0
		cm-ram-0 0))
     (2 (output cm-ram-3 0
		cm-ram-2 1
		cm-ram-1 0
		cm-ram-0 0))
     (1 (output cm-ram-3 0
		cm-ram-2 0
		cm-ram-1 1
		cm-ram-0 0))
     (0 (output cm-ram-3 0
		cm-ram-2 0
		cm-ram-1 0
		cm-ram-0 1))))

(defic i4004
  :pinout (d0
	   d1
	   d2
	   d3
	   v-ss
	   ;; valid types would be input, output, bus & drive. If no drive specified all inputs will be drives:
	   (clock-phase-1 :input)
	   (clock-phase-2 :input)
	   sync
	   reset
	   test
	   cm-rom
	   v-dd
	   (selector (cm-ram 1 0) ; name output@selected output@elsewhere
		     (3 cm-ram-3)
		     (2 cm-ram-2)
		     (1 cm-ram-1)
		     (0 cm-ram-0)))
  :registers ((clock-counter 0)
	      (ROM-address #*000000000000)
	      (RAM-command-line 0)
	      (op-memory)
	      (index-register (make-array 64 :element-type 'bit))
	      (carry-bit 0)
	      (accumulator (make-array 4 :element-type 'bit))
	      (stack (make-array 3
				 :initial-contents
				 (make-array 12 :element-type 'bit)
				 (make-array 12 :element-type 'bit)
				 (make-array 12 :element-type 'bit)))
	      (stack-pointer 0))
#| ;maybe
  :cycle ((send-rom-address ((or clock-phase-1 clock-phase-2))
	     (case clock-counter
	       ((0 1) (output (sync cm-rom d0 d1 d2 d3)
			      1 1
			      (bit 0 ROM-address)
			      (bit 1 ROM-address)
			      (bit 2 ROM-address)
			      (bit 3 ROM-address)))
	       (2 3) (output (sync cm-rom d0 d1 d2 d3)
			      1 1
			      (bit 0 ROM-address)
			      (bit 1 ROM-address)
			      (bit 2 ROM-address)
			      (bit 3 ROM-address)))
	     (RECEIVE-INSTRUCTION))
	  (receive-instruction ())
	  (execute-instruction ()))
  |#
  ;;:registers () ;; don't really need internal details (yet)
  )
|#
