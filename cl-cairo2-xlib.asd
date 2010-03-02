(defpackage #:cl-cairo2-xlib-asd
  (:use :cl :asdf))

(in-package #:cl-cairo2-xlib-asd)

(defsystem cl-cairo2-x11
  :description "Cairo 1.6 bindings, xlib surface and GTK extension"
  :version "0.1"
  :author "Tamas K Papp"
  :license "GPL"
  :serial t
  :components
  ((:module
    "xlib surface"
    :pathname #P"src/xlib/"
    :serial t
    :components
    ((:file "cl-cairo2-xlib-swig")
     (:file "load-libraries-x11")
     (:file "xlib")))
   (:module
    "core"
    :pathname #P"src/xlib/"
    :serial t
    :components
    ((:file "xlib-image-context")
     (:file "gtk-context"))))
  :depends-on (:cl-cairo2))
