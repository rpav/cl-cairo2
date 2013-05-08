(asdf:load-system :cl-cairo2)
(asdf:load-system :cl-cairo2-xlib)

;;;; Make a test package
(defpackage :cairo-xlib-example
  (:use :common-lisp :cl-cairo2))

(in-package :cairo-xlib-example)

(defparameter *context* nil)

;; open display
(let ((width 400)
      (height 300))
  (setf *context*
        (create-xlib-image-context width height :window-name "diagonal lines"))
  ;; clear the whole canvas with blue
  (rectangle 0 0 width height)
  (set-source-rgb 0.2 0.2 0.5)
  (fill-path)
  ;; draw a white diagonal line
  (move-to width 0)
  (line-to 0 height)
  (set-source-rgb 1 1 1)
  (set-line-width 5)
  (stroke)
  ;; draw a green diagonal line
  (move-to 0 0)
  (line-to width height)
  (set-source-rgb 0 1 0)
  (set-line-width 5)
  (stroke))
;; need to close window manually


(defun random-square (alpha)
  "Draw a blue rectangle with fixed size and the given transparency alpha."
  (move-to 1 1)
  (line-to -1 1)
  (line-to -1 -1)
  (line-to 1 -1)
  (close-path)
  (set-source-rgba
                   (/ (random 100) 100)
                   (/ (random 100) 100)
                   (/ (random 100) 100)
                   alpha)
  (fill-path))

(defparameter width 800)
(defparameter height 600)
(defparameter max-angle 90d0)
(setf *context* (create-xlib-image-context width height :window-name "rectangles"))

;; fill with white
(rectangle 0 0 width height)
(set-source-rgb 1 1 1)
(fill-path)
;; draw the rectangles
(dotimes (i 500)
  (let ((scaling (+ 5d0 (random 40d0))))
    (reset-trans-matrix)                       ; reset matrix
    (translate (random width) (random height)) ; move the origin
    (scale scaling scaling)                    ; scale
    (rotate (deg-to-rad (random max-angle)))   ; rotate
    (random-square (+ 0.1 (random 0.4)))))
;; need to close window manually
