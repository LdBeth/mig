;;;; TANGLED WEB FROM "mig.clw". DO NOT EDIT.

(QUICKLISP-CLIENT:QUICKLOAD :MCCLIM)
(DEFPACKAGE #:MIG
            (:USE #:COMMON-LISP #:CLIM)
            (:SHADOWING-IMPORT-FROM #:COMMON-LISP
             #:INTERACTIVE-STREAM-P))
(LOAD #P"mig.lisp")
(IN-PACKAGE #:MIG)