;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(defpackage #:cl-cairo2-win32-asd
  (:use :cl :asdf))

(in-package #:cl-cairo2-win32-asd)

(defsystem cl-cairo2-win32
  :description "Cairo 1.8 bindings, win32 surface"
  :version "0.1"
  :author "Tamas K Papp, Kei Suzuki"
  :license "BOOST 1.0"
  :serial t
  :components
  ((:module
    "Win32 surface"
    :pathname #P"src/win32/"
    :serial t
    :components
    ((:file "cl-cairo2-win32-swig")
     (:file "win32"))))
  :depends-on (:cl-cairo2))
