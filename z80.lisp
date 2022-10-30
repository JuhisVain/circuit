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
   ;; TODO: rewrite   build-registry-build-form
  ;;(AF (flags
;;	S 1 Z 1 -1 1 H 1 -2 1 P/V 1 N 1 C 1)
 ;;      A 8)
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
			   a0 a1 a2 a3 a4 a5 a6 a7))
	      (3
	       (execute INSTRUCTION))))
	   ((falling-p clk)
	    (case t-cycle
	      (0
	       (output mreq 0
		      rd 0))
	      (2
	       (output mreq 0)))

	    ;;; Currently I'm thinking OP execution should set m-cycle
	    (cond ((< t-cycle (1- states-in-m))
		   (incf t-cycle))
		  (t (setf t-cycle 0))
	    ))))))

(defoperation z-80 no-operation (0 0 0 0 0 0 0 0)
  (set-register states-in-m 4))

;;There's not enough timing data to implement arithmetic
;; meaning this is all useless
'(defoperation z-80 add-with-carry-16-bit
    (1 0 1 1 0 1 1 1
     0 1 0 1 s s 1 0)
  ;;
  (set-register states-in-m 4)
  ;;(case s
  ;;(#*00 ))
  
  )
