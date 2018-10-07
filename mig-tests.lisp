;;;; TANGLED WEB FROM "mig.clw". DO NOT EDIT.

(IN-PACKAGE #:MIG)
(DEFUN TEST ()
  (SHOW-PLOT)
  (CLEAR-PLOT)
  (DOTIMES (I 6)
    (PLOT-FILL-RECT (* I 9) (* I 9) 8 8 I)
    (PLOT-FRAME-RECT (* I 9) (* I 9) 8 8))
  (DOTIMES (I 50)
    (PLOT-SIZE-RECT
      (+ 160 (RANDOM 200))
      (RANDOM 100)
      (RANDOM 20)
      (RANDOM 20)
      (RANDOM 5)))
  (DOTIMES (I 4)
    (PLOT-STRING (* I 10)
                 (+ 150 (* I 22))
                 "Mark's plot utilities...."
                 (* I 5)))
  (PLOT-STRING-BOLD 20 260 "This is a test... of BOLD" 20)
  (PLOT-STRING-ITALIC 20 280 "This is a test... of ITALIC" 15))