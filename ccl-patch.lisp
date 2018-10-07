(in-package #:zpb-ttf)

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
