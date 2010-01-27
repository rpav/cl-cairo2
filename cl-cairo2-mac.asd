 ;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(defpackage #:cl-cairo2-mac-asd
  (:use :cl :asdf :cl-cairo2))

(in-package #:cl-cairo2-mac-asd)

(defsystem cl-cairo2-mac
  :description "Cairo 1.6 bindings, X11, GTK and Quartz(not yet) extension"
  :version "0.1"
  :author "Tamas K Papp, Kei Suzuki"
  :license "GPL"
  :serial t
  :components
  ((:module
    "mac swig"
    :pathname #P"src/"
    :serial t
    :components
    ((:file "cl-cairo2-mac-swig")))
   (:module
    "x11 stuff"
    :pathname #P"src/x11/"
    :serial t
    :components
    ((:file "load-libraries-x11")
     (:file "xlib")
     (:file "xlib-image-context")
     (:file "gtk-context"))))
  :depends-on (:cl-cairo2))
