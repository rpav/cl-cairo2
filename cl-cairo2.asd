(defpackage #:cl-cairo2-asd
  (:use :cl :asdf))

(in-package #:cl-cairo2-asd)

(defsystem cl-cairo2
  :description "Cairo 1.6 bindings"
  :version "0.4"
  :author "Tamas K Papp"
  :license "GPL"
  :components ((:file "package")
               (:file "cairo" :depends-on ("package"))
	       (:file "my-double" :depends-on ("package"))
  	       (:file "cl-cairo2-swig" :depends-on ("cairo" "my-double"))
               (:file "tables" :depends-on ("cl-cairo2-swig"))
               (:file "surface" :depends-on ("cairo" "tables" "cl-cairo2-swig"))
               (:file "context" :depends-on ("surface" "tables" "cl-cairo2-swig"))
               (:file "pattern" :depends-on ("context" "surface" "tables"
						       "cl-cairo2-swig"
						       "transformations"))
               (:file "path" :depends-on ("context"))
               (:file "text" :depends-on ("context"))
               (:file "transformations" :depends-on ("context")))
  :depends-on (:cffi :cl-colors :cl-utilities :trivial-garbage
		     :trivial-features))
