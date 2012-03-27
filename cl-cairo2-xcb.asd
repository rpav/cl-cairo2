(defpackage #:cl-cairo2-xcb-asd
  (:use :cl :asdf))

(in-package #:cl-cairo2-xcb-asd)

(defsystem cl-cairo2-xcb
  :description "Cairo binding bridge for cl-xcb-xlib"
  :version "0.1"
  :author "Ryan Pavlik"
  :license "BOOST 1.0"
  :serial t
  :components
  ((:module "xcb"
    :pathname #P"src/xcb/"
    :serial t
    :components
    ((:file "cl-cairo2-xcb-swig")
     (:file "xcb"))))
  :depends-on (:cl-cairo2 :cl-xcb-xlib))
