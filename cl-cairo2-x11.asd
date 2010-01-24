(defpackage #:cl-cairo2-x11-asd
  (:use :cl :asdf))

(in-package #:cl-cairo2-x11-asd)

(defsystem cl-cairo2-x11
  :description "Cairo 1.6 bindings, X11 and GTK extension"
  :version "0.1"
  :author "Tamas K Papp"
  :license "GPL"
  :serial t
  :components
  ((:module
    "foreign-interface"
    :pathname #P"src/x11/"
    :serial t
    :components
    ((:file "cl-cairo2-x11-swig")
     (:file "load-libraries-x11")
     (:file "xlib")))
   (:module
    "core"
    :pathname #P"src/x11/"
    :serial t
    :components
    ((:file "xlib-image-context")
     (:file "gtk-context"))))
  :depends-on (:cl-cairo2))
