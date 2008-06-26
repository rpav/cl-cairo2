(in-package :cl-cairo2)

(setf *context* (create-pdf-context "/tmp/foo.pdf" 100 100))
(move-to 0 0)
(line-to 100 100)
(set-source-rgb 0 0 1)
(stroke)

;; destroy object, after this, it will be ready to be GC'd
(setf *context* nil)

;; call GC
#+sbcl (sb-ext:gc)
