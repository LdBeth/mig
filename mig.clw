@*Introduction. This is a \.{CLIM} implementation of plot lib demonstrated in
the book {\it Common LISP Modules} by Mark Watson. So finally, I'm starting to
learn how to do Artificial Intelligence in Common Lisp.
@^CLIM@>
@^Watson, Mark@>

@1*Front matters and license issue. This file and generated files are in public
domain. You can do what you want including but not limited to copying,
modifying and distributing it without prior consent from the author.

@ Begin our prelude. Shadow conflicted symbols. And this is a load script.

@(load.lisp@>=
@e
(ql:quickload :mcclim)

@e
(cl:defpackage #:mig
  (:use #:common-lisp #:clim)
  (:shadowing-import-from
   #:common-lisp
   #:interactive-stream-p))

(load #P"mig.lisp")

@ @l
@e
(in-package #:mig)

@ Since there's a delay before a frame get initialized, we use a symbol macro
for |*w*|, which refers to the global variable |*frame*|.

@l
(define-symbol-macro *w* (get-frame-pane *frame* 'display))
(defvar *frame* nil)

@ We define the basic plot window here.

@l
(define-application-frame plot ()
  ()
  (:panes
   (display :application))
  (:layouts
   (default display)))
#|
(define-plot-command (com-quit :menu t) ()
  (frame-exit *application-frame*))
|#

@ Define |init-plot|. Further portability issues for |process-run-function|.

@l
(defun init-plot (&optional (title "Plot Window") (xSize 250) (ySize 250))
  "Initializes a standard plot window"
  (flet ((run ()
           (let ((frame (make-application-frame
                         'plot
                         :width xSize :height ySize :pretty-name title)))
             (setq *frame* frame)
             (run-frame-top-level frame))))
    (clim-sys:make-process #'run :name "plot")))

@ Define plot function for rectangles.

@l
(defun plot-fill-rect (x y xsize ysize pattern
                       &aux (pattern (truncate pattern)))
  "Fills in a rectangle with one of five gray-scale values"
  (draw-rectangle* *w* x y (+ x xsize) (+ y ysize)
                   :ink (cond ((< pattern 1) +white+)
                              ((equal pattern 1)
                               +grey70+)
                              ((equal pattern 2)
                               +grey50+)
                              ((equal pattern 3)
                               +grey30+)
                              (t +black+))
                   :filled t))

@ Makes a black rectangle of size proportional to val. This is an alternative
 to using function |plot-fill-rect| for showing graphically the value of a
 number.

@l
(defun plot-size-rect (x y xsize ysize val &aux (val (min val xsize)))
  (draw-rectangle* *w* x y (+ x xsize) (+ y ysize)
                   :ink +white+
                   :filled t)
  (draw-rectangle* *w* x y (+ x val) (+ y val)
                   :ink +black+
                   :filled t))

@ Clears (erases) the plot window.

@l
(defun clear-plot ()
  (window-clear *w*))

@ Since \.{CLIM} doesn't memorize line thickness, we create a variable for it.

@l
(defvar *thickness* 1)

(defun pen-width (nibs)
  "Sets the drawing size for the pen"
  (setf *thickness* nibs))

@ Frames a rectangle of size (xsize ysize) at position (x y).

@l
(defun plot-frame-rect (x y xsize ysize)
  (let ((x2 (+ x xsize))
        (y2 (+ y ysize)))
    (draw-rectangle* *w* x y x2 y2 :line-thickness *thickness* :filled nil)))

@ Draws a line between two points.

@l
(defun plot-line (x1 y1 x2 y2)
  (draw-line* *w* x1 y1 x2 y2 :line-thickness *thickness*))

@ This function is just a stub.

@l
(defun show-plot ()
  nil)

@ Draw text.

@l
(defun plot-string  (x y str &optional (size 10))
  (draw-text* *w* str x y :text-size size :text-face :roman))

(defun plot-string-bold  (x y str &optional (size 12))
  (draw-text* *w* str x y :text-size size :text-face :bold))

(defun plot-string-italic  (x y str &optional (size 12))
  (draw-text* *w* str x y :text-size size :text-face :italic))

@ We have to override these two functions for CCL.

@l
#+:ccl
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

@ And finally, the test.

@l
(defun test ()
  (show-plot)
  (clear-plot)
  (dotimes (i 6)
    (plot-fill-rect (* i 9)
                    (* i 9)
                    8 8
                    i)
    (plot-frame-rect (* i 9) (* I 9) 8 8))
  (dotimes (i 50)
    (plot-size-rect
     (+ 160 (random 200))
     (random 100) (random 20) (random 20) (random 5)))
  (dotimes (i 4)
    (plot-string (* i 10) (+ 150 (* i 22))
                 "Mark's plot utilities...." (* i 5)))
  (plot-string-bold 20 260 "This is a test... of BOLD" 20)
  (plot-string-italic 20 280 "This is a test... of ITALIC" 15))
