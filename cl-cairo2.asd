(defpackage #:cl-cairo2-asd
  (:use :cl :asdf))

(in-package #:cl-cairo2-asd)

(defsystem cl-cairo2
  :description "Cairo 1.6 bindings"
  :version "0.5"
  :author "Tamas K Papp, Kei Suzuki"
  :license "GPL"
  :serial t
  :components
  ((:module 
    "package-init"
    :pathname #P"src/"
    :components 
    ((:file "package")))
   (:module
    "foreign-interface"
    :pathname #P"src/"
    :serial t
    :components 
    ((:file "load-libraries")
     (:file "my-double")
     (:file "cl-cairo2-swig")))
   (:module
    "core-interface"
    :pathname #P"src/"
    :serial t
    :components
    ((:file "tables")
     (:file "surface")
     (:file "context")
     (:file "transformations")
     (:file "pattern")
     (:file "path")
     (:file "text"))))
  :depends-on (:cffi :cl-colors :cl-utilities :trivial-garbage
                     :trivial-features))
