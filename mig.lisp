;;;; TANGLED WEB FROM "mig.clw". DO NOT EDIT.

(DEFINE-SYMBOL-MACRO *W* (GET-FRAME-PANE *FRAME* 'DISPLAY))
(DEFVAR *FRAME* NIL)
(DEFINE-APPLICATION-FRAME
  PLOT
  NIL
  NIL
  (:PANES (DISPLAY :APPLICATION))
  (:LAYOUTS (DEFAULT DISPLAY)))
(DEFUN INIT-PLOT (&OPTIONAL (TITLE "Plot Window") (XSIZE 250)
                  (YSIZE 250))
  "Initializes a standard plot window"
  (FLET ((RUN NIL
           (LET ((FRAME
                  (MAKE-APPLICATION-FRAME
                    'PLOT
                    :WIDTH
                    XSIZE
                    :HEIGHT
                    YSIZE
                    :PRETTY-NAME
                    TITLE)))
             (SETQ *FRAME* FRAME)
             (RUN-FRAME-TOP-LEVEL FRAME))))
    (CLIM-SYS:MAKE-PROCESS (FUNCTION RUN) :NAME "plot")))
(DEFUN PLOT-FILL-RECT (X Y XSIZE YSIZE PATTERN &AUX
                       (PATTERN (TRUNCATE PATTERN)))
  "Fills in a rectangle with one of five gray-scale values"
  (DRAW-RECTANGLE*
    *W*
    X
    Y
    (+ X XSIZE)
    (+ Y YSIZE)
    :INK
    (COND ((< PATTERN 1) +WHITE+)
          ((EQUAL PATTERN 1) +GREY70+)
          ((EQUAL PATTERN 2) +GREY50+)
          ((EQUAL PATTERN 3) +GREY30+)
          (T +BLACK+))
    :FILLED
    T))
(DEFUN PLOT-SIZE-RECT (X Y XSIZE YSIZE VAL &AUX
                       (VAL (MIN VAL XSIZE)))
  (DRAW-RECTANGLE*
    *W*
    X
    Y
    (+ X XSIZE)
    (+ Y YSIZE)
    :INK
    +WHITE+
    :FILLED
    T)
  (DRAW-RECTANGLE*
    *W*
    X
    Y
    (+ X VAL)
    (+ Y VAL)
    :INK
    +BLACK+
    :FILLED
    T))
(DEFUN CLEAR-PLOT () (WINDOW-CLEAR *W*))
(DEFVAR *THICKNESS* 1)
(DEFUN PEN-WIDTH (NIBS)
  "Sets the drawing size for the pen"
  (SETF *THICKNESS* NIBS))
(DEFUN PLOT-FRAME-RECT (X Y XSIZE YSIZE)
  (LET ((X2 (+ X XSIZE)) (Y2 (+ Y YSIZE)))
    (DRAW-RECTANGLE*
      *W*
      X
      Y
      X2
      Y2
      :LINE-THICKNESS
      *THICKNESS*
      :FILLED
      NIL)))
(DEFUN PLOT-LINE (X1 Y1 X2 Y2)
  (DRAW-LINE* *W* X1 Y1 X2 Y2 :LINE-THICKNESS *THICKNESS*))
(DEFUN SHOW-PLOT () NIL)
(DEFUN PLOT-STRING (X Y STR &OPTIONAL (SIZE 10))
  (DRAW-TEXT* *W* STR X Y :TEXT-SIZE SIZE :TEXT-FACE :ROMAN))
(DEFUN PLOT-STRING-BOLD (X Y STR &OPTIONAL (SIZE 12))
  (DRAW-TEXT* *W* STR X Y :TEXT-SIZE SIZE :TEXT-FACE :BOLD))
(DEFUN PLOT-STRING-ITALIC (X Y STR &OPTIONAL (SIZE 12))
  (DRAW-TEXT* *W* STR X Y :TEXT-SIZE SIZE :TEXT-FACE :ITALIC))
#+:CCL
(progn
  #.(setf *package* (find-package '#:zpb-ttf))

  (defun open-font-loader-from-file (thing)
    (let ((stream (open thing
                        :direction :input
                        :element-type '(unsigned-byte 8) :sharing :lock)))
      (let ((font-loader (open-font-loader-from-stream stream)))
        (arrange-finalization font-loader stream)
        font-loader)))

  (defun open-font-loader (thing)
    (typecase thing
      (font-loader
       (unless (open-stream-p (input-stream thing))
         (setf (input-stream thing) (open (input-stream thing) :sharing :lock)))
       thing)
      (stream
       (if (open-stream-p thing)
           (open-font-loader-from-stream thing)
           (error "~A is not an open stream" thing)))
      (t
       (open-font-loader-from-file thing))))
  #.(setf *package* (find-package '#:mig)))
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
                 "Mark's plot utilities...."))
  (PLOT-STRING-BOLD 20 260 "This is a test... of BOLD")
  (PLOT-STRING-ITALIC 20 280 "This is a test... of ITALIC"))