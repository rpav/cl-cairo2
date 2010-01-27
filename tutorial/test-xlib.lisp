(in-package :cl-cairo2)

(defun random-size ()
  (+ 200 (random 100)))
(defparameter *list-of-contexts* nil)
(defparameter *max-number-of-contexts* 50)

(defun x-on-window (context)
  (let ((width (image-surface-get-width context))
        (height (image-surface-get-height context)))
    ;; clear
    (rectangle 0 0 width height context)
    (set-source-color +white+ context)
    (fill-path context)
    ;; draw X
    (move-to 0 0 context)
    (line-to width height context)
    (set-source-color +green+ context)
    (stroke context)
    (move-to 0 height context)
    (line-to width 0 context)
    (set-source-color +blue+ context)
    (stroke context)))

(defun remove-random-window (list)
  (assert (not (null list)))
  (let* ((length (length list))
	 (index (random length))
	 (context (nth index list)))
    (format t "killing ~a~%" index)
    (destroy context)
    (remove context list)))

;; create contexts with an x on them
(dotimes (i *max-number-of-contexts*)
  (let ((context (create-xlib-image-context (random-size) (random-size))))
    (x-on-window context)
    (push context *list-of-contexts*)))

;; close all, in random order
(do ()
    ((not *list-of-contexts*))
  (setf *list-of-contexts* (remove-random-window *list-of-contexts*)))


(defparameter *c1* (create-xlib-image-context 100 100))
(x-on-window *c1*)
(defparameter *c2* (create-xlib-image-context 140 200))
(x-on-window *c2*)

(destroy *c1*)
(destroy *c2*)
