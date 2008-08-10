;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(defpackage #:cl-cairo2-win-asd
  (:use :cl :asdf :cl-cairo2))

(in-package #:cl-cairo2-win-asd)

(defsystem cl-cairo2-win
  :description "Cairo 1.6 bindings, MS-Windows extension"
  :version "0.1"
  :author "Tamas K Papp, Kei Suzuki"
  :license "GPL"
  :components ((:file "cl-cairo2-win-swig")
	       (:file "win32" :depends-on ("package-win")))
  :depends-on (:cl-cairo2))
