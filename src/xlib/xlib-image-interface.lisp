(in-package :cl-cairo2)

;;; The classes defined in this file is always exported, regardless of
;;; X11 being present, so that other libraries can define methods on
;;; it.

(defclass xlib-image-context (context)
  ((display :initarg :display)
   (background-color :initarg :background-color)
   dest-surface
   window graphics-context signal-window
   (xlib-context :accessor xlib-context)
   wm-delete-window
   (width :initarg :width)
   (height :initarg :height)
   thread 
   (sync-counter :initform 0 :accessor sync-counter)))
