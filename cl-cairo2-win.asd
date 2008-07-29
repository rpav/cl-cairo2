(defpackage #:cl-cairo2-win-asd
  (:use :cl :asdf))

(in-package #:cl-cairo2-win-asd)

(defsystem cl-cairo2-win
  :description "Cairo 1.6 bindings, MS-Windows extension"
  :version "0.1"
  :author "Kei Suzuki"
  :license "GPL"
  :components ((:file "package-win")
	       (:file "win32" :depends-on ("package-win")))
  :depends-on (:cl-cairo2))
