(in-package :cl-cairo2)

;; is this really needed?  OS should set this up properly
#+darwin (pushnew "/usr/X11/lib/" *foreign-library-directories*)

(define-foreign-library :libX11
  (:darwin "libX11.dylib")
  (:unix "libX11.so"))

(load-foreign-library :libX11)

(define-foreign-library :gdk
  ;; 'darwin' comes before 'unix' because Mac OS X defines them both.
  (:darwin "libgdk-x11-2.0.dylib")
  (:unix "libgdk-x11-2.0.so")
  (:windows "libgdk-win32-2.0-0.dll"))

(load-foreign-library :gdk)
