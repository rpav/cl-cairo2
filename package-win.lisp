(in-package #:cl-cairo2-win-asd)

(defpackage cl-cairo2-win
  (:use :common-lisp :cl-cairo2 :cl-utilities)
  (:export

   ;; win32

   win32-create-surface win32-get-surface-dc win32-get-surface-image
   with-win32-context win32-create-font-face-for-logfontw
   win32-create-font-face-for-hfont
   #|win32-create-font-face-for-logfontw-hfont|#
   win32-select-font-scaled-font win32-done-font-scaled-font
   win32-get-metrics-factor-scaled-font
   win32-get-device-to-logical-scaled-font)
  (:nicknames :cairo-win))
