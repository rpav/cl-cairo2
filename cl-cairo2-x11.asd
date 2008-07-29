(defpackage #:cl-cairo2-x11-asd
  (:use :cl :asdf))

(in-package #:cl-cairo2-x11-asd)

(defsystem cl-cairo2-x11
  :description "Cairo 1.6 bindings, X11 and GTK extension"
  :version "0.1"
  :author "Tamas K Papp"
  :license "GPL"
  :components ((:file "package-x11")
	       (:file "xlib" :depends-on ("package-x11"))
	       (:file "xlib-image-context" :depends-on ("xlib"))
	       (:file "gtk-context" :depends-on ("package-x11")))
  :depends-on (:cl-cairo2))
