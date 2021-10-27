(in-package :temp)

(define-ic i4004
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
   (RAM-command-line 0 :type (integer 0 3))
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
    (trigger-clock-counter clock-counter)
    (set-register clock-counter (if (< clock-counter 15)
				    (1+ clock-counter)
				    0))))

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
		     (bit-ior (nth op-memory-pointer op-memory)
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
  )



(setf (gethash 'i4004 *op-code-library*) (make-op-node))


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