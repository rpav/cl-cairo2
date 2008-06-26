;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(defpackage #:cl-cairo2-asd
  (:use :cl :asdf))

(in-package :cl-cairo2-asd)

(defsystem #:cl-cairo2
  :description "Cairo 1.4 bindings"
  :version "0.3"
  :author "Tamas K Papp"
  :license "GPL"
  :components ((:file "package")
               (:file "cairo" :depends-on ("package"))
  	       (:file "cl-cairo2-swig" :depends-on ("cairo"))
               (:file "tables" :depends-on ("cl-cairo2-swig"))
               (:file "surface" :depends-on ("cairo" "tables" "cl-cairo2-swig"))
               (:file "context" :depends-on ("surface" "tables" "cl-cairo2-swig"))
               (:file "pattern" :depends-on ("context" "surface" "tables" "cl-cairo2-swig" "transformations"))
               (:file "path" :depends-on ("context"))
               (:file "text" :depends-on ("context"))
               (:file "transformations" :depends-on ("context"))
	       (:file "xlib" :depends-on ("context")
		      :in-order-to ((load-op (feature :unix))
				    (compile-op (feature :unix))))
	       (:file "xlib-image-context" :depends-on ("xlib")
		      :in-order-to ((load-op (feature :unix))
				    (compile-op (feature :unix))))
	       (:file "gtk-context" :depends-on ("context")
		      :in-order-to ((load-op (feature :unix))
				    (compile-op (feature :unix)))))
  :depends-on (:cffi :cl-colors :cl-utilities :trivial-garbage))
