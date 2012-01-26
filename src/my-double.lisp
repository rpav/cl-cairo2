(in-package :cl-cairo2)

;; define our own alias for double float, so we can automatically
;; convert other numerical types in the arguments
(define-foreign-type my-double-type ()
  ()
  (:actual-type :double)
  (:simple-parser my-double))
                                                                              
(defmethod translate-to-foreign (value (type my-double-type))
  (coerce value 'double-float))

(defmethod expand-to-foreign (value (type my-double-type))
  `(coerce ,value 'double-float))
