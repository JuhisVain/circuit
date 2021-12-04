(in-package :temp)

(define-ic z-80
  :cpu (:word-size 8 :byte-size 8 :endianness :little-endian)
  :pins
  ((a11 :output)
   (a12 :output)
   (a13 :output)
   (a14 :output)
   (a15 :output)
   (clk :drive)
   (d4 :bus)
   (d3 :bus)
   (d5 :bus)
   (d6 :bus)
   ;;(+5v)
   (d2 :bus)
   (d7 :bus)
   (d0 :bus)
   (d1 :bus)
   (int :input)
   (nmi :input)
   (halt :output)
   (mreq :output)
   (iorq :output)

   (rd :output)
   (wr :output)
   (busak :output)
   (wait :input)
   (busreq :input)
   (reset :input)
   (m1 :output)
   (rfsh :output)
   ;;(GND)
   (a0 :output)
   (a1 :output)
   (a2 :output)
   (a3 :output)
   (a4 :output)
   (a5 :output)
   (a6 :output)
   (a7 :output)
   (a8 :output)
   (a9 :output)
   (a10 :output))

  :registers
  ((AF flags 8 A 8)
   (BC C 8 B 8)
   (DE E 8 D 8)
   (HL L 8 H 8)
   ;; alternate general purpose registers:
   (AF^ flags^ 8 A^ 8)
   (BC^ C^ 8 B^ 8)
   (DE^ E^ 8 D^ 8)
   (HL^ L^ 8 H^ 8)
   
   (IX 16)
   (IY 16)
   (SP 16)
   (I 8)
   (R 8)
   (PC 16)
   ;; op memory
   (INSTRUCTION 32))

  :aux
  ((t-cycle 0 :type (integer 0 6))
   (m-cycle 'opcode-fetch :type symbol)
   (states-in-m 4 :type (integer 0 6)))

  :event-processor
  ((clk
    (ecase m-cycle
      (OPCODE-FETCH (opcode-fetch)))))

  :secondary-functions
  ((opcode-fetch ()
     (cond ((rising-p clk)
	    (case t-cycle
	      (0
	       (bus-output
		PC a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15) ; unsure
	       (set-register PC (bit-1+ PC)
			     R (bit-1+ R))
	       (bit-zero INSTRUCTION))
	      (2
	       (setf INSTRUCTION (bits (mapcar #'pin-input (list d0 d1 d2 d3 d4 d5 d6 d7))
				       (make-bits 24)))
	       (output mreq 1
		       rd 1
		       rfsh 0
		       m1 1)
	       (bus-output #*10101010 ;; TODO: memory refresh address ???
			   a0 a1 a2 a3 a4 a5 a6 a7))))
	   ((falling-p clk)
	    (case t-cycle
	      (0
	       (output mreq 0
		      rd 0))
	      (2
	       (output mreq 0)))
	    ;;; NOTE: Documentation says FETCH cycle is 4, 5 or 6 T-cycles long
	    ;;; -> No timings found for mystery T5 & T6
	    (incf t-cycle) ;; TODO: when end of m-cycle, must set to zero and switch m
	    )))))
