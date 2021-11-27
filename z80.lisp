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
  ((instruction (make-array 32 :element-type 'bit))
   ;; register pairs should be done with displaced arrays
   ))