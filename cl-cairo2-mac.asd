 ;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(defpackage #:cl-cairo2-mac-asd
  (:use :cl :asdf :cl-cairo2))

(in-package #:cl-cairo2-mac-asd)

(defsystem cl-cairo2-mac
  :description "Cairo 1.6 bindings, X11, GTK and Quartz(not yet) extension"
  :version "0.1"
  :author "Tamas K Papp, Kei Suzuki"
  :license "GPL"
  :components ((:file "cl-cairo2-mac-swig" :depends-on ("package-mac"))
	       (:file "xlib" :depends-on ("package-mac"))
	       (:file "xlib-image-context" :depends-on ("xlib"))
	       (:file "gtk-context" :depends-on ("package-mac")))
  :depends-on (:cl-cairo2))
