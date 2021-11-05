(in-package :temp)

(define-ic i4004
  :cpu (:word-size 4 :byte-size 8 :endianness :big-endian)
  :pins
  ((d0 :bus)
   (d1 :bus)  
   (d2 :bus)
   (d3 :bus)
   ;;(v-ss ) ; define a dummy pin?
   (clock-phase-1 :drive)
   (clock-phase-2 :drive)
   (sync :output)
   (reset :input)
   (test :input)
   (cm-rom :output)
   ;;(v-dd )
   (cm-ram-3 :output)
   (cm-ram-2 :output)
   (cm-ram-1 :output)
   (cm-ram-0 :output))

  :registers
  ((clock-counter 0 :type (integer 0 15))
   (ROM-address #*000000000000 :type bit-vector)
   (RAM-command-line #*0000 :type bit-vector)
   (op-memory (list #*00000000 #*00000000) :type list) ; one-byte-op & two-byte-op's vars
   (op-memory-pointer 0 :type (integer 0 1)) ; 0 or 1, index of op-memory to fill, usually 0
   (index-register (make-array 64 :element-type 'bit)
		   :type bit-vector)
   (carry-bit 0 :type bit)
   (accumulator (make-array 4 :element-type 'bit)
		:type bit-vector)
   (stack (make-array 3
		      :initial-contents
		      (list 
		       (make-array 12 :element-type 'bit)
		       (make-array 12 :element-type 'bit)
		       (make-array 12 :element-type 'bit)))
	  :type vector)
   (stack-pointer 0 :type (integer 0 2)))

  :event-processor
  (((clock-phase-1 clock-phase-2)
    (cond ((or (and (evenp clock-counter)
	            (rising-p clock-phase-2))
	       (and (oddp clock-counter)
	            (rising-p clock-phase-1)))
	   (trigger-clock-counter clock-counter)
	   (set-register clock-counter (if (< clock-counter 15)
					   (1+ clock-counter)
					   0)))
	  ((or (falling-p clock-phase-1)
	       (falling-p clock-phase-2))
	   nil)
	  (t (error "Clock synchronization error. ~a rising at clock counter ~a."
		   source clock-counter)))))

  :secondary-functions
  ((trigger-clock-counter (trigger)
    (case trigger
      (0 (output sync 1
		 cm-rom 1
		 d0 (bit ROM-address 0)
		 d1 (bit ROM-address 1)
		 d2 (bit ROM-address 2)
		 d3 (bit ROM-address 3)))
      (2 (output d0 (bit ROM-address 4)
		 d1 (bit ROM-address 5)
		 d2 (bit ROM-address 6)
		 d3 (bit ROM-address 7)))
      (4 (output d0 (bit ROM-address 8)
		 d1 (bit ROM-address 9)
		 d2 (bit ROM-address 10)
		 d3 (bit ROM-address 11)))
      (5 (output cm-rom 0)
	 (set-register ROM-address (bit-1+ ROM-address)))
      (6
       (output cm-rom 1)
       (floating d0 d1 d2 d3))
      (7
       (set-register (nth op-memory-pointer op-memory)
		     (bits (mapcar #'pin-input (list d0 d1 d2 d3))
			   #*0000)))
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
		     (bit-ior (nth op-memory-pointer op-memory)
			      (bits #*0000
				    (mapcar #'pin-input
					    (list d0 d1 d2 d3))))))

      (10 (output cm-rom 1)
       (bus-output RAM-command-line
		   cm-ram-0 cm-ram-1 cm-ram-2 cm-ram-3))
      (12 (execute (if (zerop op-memory-pointer)
		       (first op-memory)
		       (apply #'bits op-memory))))
      (13
       (when (equal (bit-and (first op-memory) #*11110000)
		    #*01000000) ;SRC?
	 (output cm-ram-0 0
		 cm-ram-1 0
		 cm-ram-2 0
		 cm-ram-3 0))
       (output cm-rom 1)
       (execute (if (zerop op-memory-pointer)
		    (first op-memory)
		    (apply #'bits op-memory))))
      (14 (output sync 0)
       (bus-output RAM-command-line
		   cm-ram-0 cm-ram-1 cm-ram-2 cm-ram-3)
       (execute (if (zerop op-memory-pointer)
		    (first op-memory)
		    (apply #'bits op-memory))))))))



(defoperation i4004 SRC (0 1 0 0 1 R R R)
  (let* ((index (bit-integer R))
	 (address (subseq index-register index (+ index 8))))
    (add-to-cycle
     ((12 13) (output d0 (bit address 0)
		      d1 (bit address 1)
		      d2 (bit address 2)
		      d3 (bit address 3)))
     ((14 15) (output d0 (bit address 4)
		      d1 (bit address 5)
		      d2 (bit address 6)
		      d3 (bit address 7))))))

(defoperation i4004 NOP (0 0 0 0 0 0 0 0)
  nil)

;; Yes, C4 is output by d0...
;;;                      d0 d1 d2 d3,d0 d1 d2 d3/d0 d1 d2 d3,d0 d1 d2 d3
(defoperation i4004 JCN (1  0  0  0  C4 C3 C2 C1 A2 A2 A2 A2 A1 A1 A1 A1)
  (case op-memory-pointer
    (0
     (add-to-cycle
      (15 (set-register op-memory-pointer 1))))
    (1
     (add-to-cycle
      (15
       (let ((jump (or (and (bit-zerop C1)
			    (or
			     (and (bit-truep C2) (bit-zerop accumulator))
			     (and (bit-truep C3) (not (zerop carry-bit)))
			     (and (bit-truep C4) (zerop test))))
		       (and (bit-truep C1)
			    (not (or
				  (and (bit-truep C2) (bit-zerop accumulator))
				  (and (bit-truep C3) (not (zerop carry-bit)))
				  (and (bit-truep C4) (zerop test))))))))
	 (when jump
	   (set-register rom-address
			 (bit-ior (bit-and rom-address
					   #*000000001111)
				  (bits A1 A2 #*0000)))) ;; Unsure bit order!!
	 (set-register op-memory-pointer 0)))))))

(defoperation i4004 LDM (1 0 1 1 D D D D)
  (add-to-cycle
   (12 (set-register accumulator D))))

(defoperation i4004 LD (0 1 0 1 R R R R)
  (add-to-cycle
   (12 (let ((reg-adr (* 4 (bit-integer R))))
	 (set-register accumulator
		       (subseq index-register reg-adr (+ 4 reg-adr)))))))

(defoperation i4004 XCH (1 1 0 1 R R R R)
  (add-to-cycle
   (12 (let* ((reg-adr (* 4 (bit-integer R)))
	      ;; Standard says subseq is copy unless setfing
	      (ir-contents (subseq index-register reg-adr (+ 4 reg-adr))))
	 (setf (subseq index-register reg-adr (+ 4 reg-adr)) accumulator)
	 (set-register accumulator ir-contents)))))

(defoperation i4004 DCL (1 1 1 1 1 0 1 1)
  (add-to-cycle
   (12 (set-register RAM-command-line
		     (subseq (bit-and accumulator #*1110) 0 3)))))

(defoperation i4004 WRM (0 1 1 1 0 0 0 0)
  (add-to-cycle
   (12 (bus-output accumulator d0 d1 d2 d3))
   (15 (floating d0 d1 d2 d3)))) ;; too early?

(defoperation i4004 IAC (1 1 1 1 0 1 0 0)
  (add-to-cycle
   (12 (multiple-value-bind (sum carry)
	   (bit-1+ accumulator)
	 (setf accumulator sum
	       carry-bit carry)))))

(defoperation i4004 INC (0 1 1 0 R R R R)
  (add-to-cycle
   (12 (let* ((reg-adr (* 4 (bit-integer R)))
	      (ir-contents (subseq index-register reg-adr (+ 4 reg-adr))))
	 (setf (subseq index-register reg-adr (+ 4 reg-adr))
	       (bit-1+ ir-contents))))))

(defoperation i4004 JUN (0 0 1 0 A3 A3 A3 A3 A2 A2 A2 A2 A1 A1 A1 A1)
  (case op-memory-pointer
    (0
     (add-to-cycle
      (15 (set-register op-memory-pointer 1))))
    (1
     (add-to-cycle
      (12 (setf ROM-address (bits A1 A2 A3))
	  (set-register op-memory-pointer 0))))))

;; i4004 is big endian as far as I can tell
;; and bit-level wise bit 0 is least significant

(defun i4004-op-length (mnemonic)
  "Return bit width of operation associated with symbol MNEMONIC."
  (loop for part in (cdr (assoc mnemonic (list-op-mnemonics 'i4004)))
	sum (etypecase part
	      (bit-vector (length part))
	      (list (cadr part)))))

(defun form-bytes (bit-width bit-arrays)
  (let ((long-bits (apply #'concatenate 'bit-vector bit-arrays)))
    (loop for i from 0 to (- (length long-bits) bit-width) by bit-width
	  collect (subseq long-bits i (+ i bit-width)))))

(defun i4004-op-binary (op)
  ;; GOOD ENOUGH
  (do* ((spec-head (cdr (assoc (car op) (list-op-mnemonics 'i4004))))
	(parameter (car spec-head) (car spec-head))
	(arg-head (cdr op))
	(argument (car arg-head) (car arg-head))
	(length-sum 0)
	(binary))
       ((null spec-head) (reverse binary))
    (etypecase parameter
      (bit-vector
       (push parameter binary)
       (incf length-sum (length parameter))
       (setf spec-head (cdr spec-head)))
      (list
       (let ((arg-length (cadr parameter)))
	 (case arg-length
	   (1 ; flag array incoming
	    (let ((flags-length (loop for x on spec-head
				      counting (or (= 1 (cadr (car x)))
						   (return (progn
							     (setf spec-head x)
							     sum)))
					into sum)))
	      (incf length-sum flags-length)
	      (etypecase argument
		(bit-vector (push argument binary))
		(integer (uinteger-bits argument flags-length)))
	      (setf arg-head (cdr arg-head))))
	   (t ; has to be an integer
	    (let ((actual-arg
		    (cond ((cdr arg-head)
			   (uinteger-bits argument arg-length)
			   (setf arg-head (cdr arg-head))
			   (setf spec-head (cdr spec-head)))
			  (t (setf spec-head nil)
			     (uinteger-bits argument (- (i4004-op-length (car op))
							length-sum))))))
	      (cond ((> (length actual-arg)
			4) ; i4004 word size
		     (setf binary
			   (nconc (loop for i from 0 to (- (length actual-arg) 4) by 4
					collect (subseq actual-arg i (+ i 4)))
				  binary)))
		    (t (push actual-arg binary)))))))))))
  
(defun i4004-asm (code)
  (let* ((tag-adr (loop for op in code
			for index from 0
			when (keywordp op)
			  collect (list op index)
			  and do (decf index)))
	 (jump-code
	   (loop for op in code
		 when (listp op)
		   collect (loop for part in op
				 collect (if (keywordp part)
					     (cadr (assoc part tag-adr))
					     part)))))
    (form-bytes 8 (mapcan #'i4004-op-binary jump-code))))

;; A testing program
'((LDM 0) ;
  (XCH 0) ; 0 @ RAM pointer
  (LDM 0) ;
  (XCH 1) ; RAM bank designator
  (LD 1)  ;
  (DCL)   ; set RAM bank command line to 0
  :start
  (LDM 0) ; 0 @ accumulator
  (SRC 0) ; target RAM 0
  (WRM)   ; write 0 to RAM 0
  
  :incrementer
  (IAC)   ; increment acc
  (JCN #*1000 :ram-bank-inc)
  (INC 0) ; increment RAM pointer
  (SRC 0) ; target new RAM address
  (WRM)   ; write
  (JUN :incrementer)
  
  :ram-bank-inc
  (INC 1) ; increment RAM bank line
  (LD 1)
  (JCN #*1000 :end) ; if RAM bank 0, start doing nothing
  (LDM 0)
  (XCH 0) ; Point to RAM address 0
  (JUN :start)

  :end
  (NOP)
  (JUN :end))

(define-ic i4001
  :pins
  ((d0 :bus)
   (d1 :bus)
   (d2 :bus)
   (d3 :bus)
   ;;(v-ss)
   (clock-phase-1 :drive)
   (clock-phase-2 :drive)
   (sync :input)
   (i/o0 :bus)
   (i/o1 :bus)
   (i/o2 :bus)
   (i/o3 :bus)
   ;;(v-dd)
   (cm :input)
   (cl :input)
   (reset :input))
  :registers
  ((id #*0000 :type bit-vector)
   (counter 0 :type (integer 0 15))
   (current-address #*00000000 :type bit-vector)
   (chip-selection #*0000 :type bit-vector)
   (ROM (make-array 256 :element-type 'bit-vector :initial-element #*00000000)))
  :event-processor
  (((clock-phase-1 clock-phase-2)
    (when (= (pin-input sync) 1)
      (setf counter 0))
    (case counter
      (1 (setf current-address (bits d0 d1 d2 d3 #*00000000)))
      (3 (setf current-address
	       (bit-ior current-address (bits #*0000 d0 d1 d2 d3 #*0000))))
      (5 (setf chip-selection (bits d0 d1 d2 d3)))
      (6 (when (bit-= id chip-selection)
	   (bus-output (subseq (svref ROM (bit-integer current-address))
			       0 4)
		       d0 d1 d2 d3)))
      (8 (when (bit-= id chip-selection)
	   (bus-output (subseq (svref ROM (bit-integer current-address))
			       4)
		       d0 d1 d2 d3)))
      (10 (when (bit-= id chip-selection)
	    (floating d0 d1 d2 d3))))
    (if (= counter 15)
	(progn
	  (format t "ROM phase counter reached 15! This should not happen!~%")
	  (setf counter 0))
	(incf counter)))))

(defun i4001-program (asm-code i4001-list)
  (let ((binary (i4004-asm asm-code)))
    (loop for rom in i4001-list
	  while binary
	  do (loop for i from 0 to 255
		   while binary
		   do (setf (svref (i4001-rom rom) i)
			    (pop binary))))))

