(defpackage #:cl-cairo2-x11-asd
  (:use :cl :asdf))

(in-package #:cl-cairo2-x11-asd)

(defsystem cl-cairo2-x11
  :description "Cairo 1.6 bindings, X11 and GTK extension"
  :version "0.1"
  :author "Tamas K Papp"
  :license "GPL"
  :serial t
  :components ((:file "cl-cairo2-x11-swig")
	       (:file "libraries-x11" :depends-on ("cl-cairo2-x11-swig"))
	       (:file "xlib" :depends-on ("libraries-x11"))
	       (:file "xlib-image-context" :depends-on ("xlib"))
	       (:file "gtk-context" :depends-on ("libraries-x11")))
  :depends-on (:cl-cairo2))
