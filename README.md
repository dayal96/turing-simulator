# turing-simulator
A Turing Machine Simulator, can be programmed

View this README in raw for better indentation.

A sample turing program that accepts a string of '0's and '1's that ends with a 0:

'(;; State q0
  (q0 (0 0 R q0)
      (1 1 R q1)
      (_ _ R accept)
      (* * R reject))
   ;; State q1
  (q1 (0 0 R q0)
      (1 1 R q1)
      (* * R reject)))

This program is written in ISL, a functional programming language meant for teaching. It is a subset of Racket and can be obtained by installing the IDE DrRacket.

To run the program, open TuringMachine.rkt in DrRacket and read the instructions.
