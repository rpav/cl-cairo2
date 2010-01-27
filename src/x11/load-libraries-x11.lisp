(in-package :cl-cairo2)

;;;; Loading X11 library - you are supposed to set it up on the path where
;;;; the system's library loader looks up.
;;;; Also, the library search order should look like below because on Mac both
;;;; 'darwin' and 'unix' are defined in *feature* and we want to load .dylib
;;;; version of library for Mac.
 
(define-foreign-library :libX11
  (:darwin "libX11.dylib")
  (:unix (:or "libX11.so" "libX11.so.6"))
  #|(:windows "libX11.dll")|#)

(load-foreign-library :libX11)

(define-foreign-library :gdk
  (:darwin "libgdk-x11-2.0.dylib")
  (:unix (:or "libgdk-x11-2.0.so" "libgdk-x11-2.0.so.0"))
  (:windows "libgdk-win32-2.0-0.dll"))

(load-foreign-library :gdk)
