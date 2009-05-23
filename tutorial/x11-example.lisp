(asdf:operate 'asdf:load-op :cl-cairo2-x11)

;;;; Make a test package
(defpackage :cairo-xlib-example
  (:use :common-lisp :cl-cairo2))

(in-package :cairo-xlib-example)

(defparameter *context* nil)

;; open display
(let ((width 400)
      (height 300))
  (setf *context* (create-xlib-image-context width height :window-name "diagonal lines"))
  ;; clear the whole canvas with blue
  (rectangle *context* 0 0 width height)
  (set-source-rgb *context* 0.2 0.2 0.5)
  (fill-path *context*)
  ;; draw a white diagonal line
  (move-to *context* width 0)
  (line-to *context* 0 height)
  (set-source-rgb *context* 1 1 1)
  (set-line-width *context* 5)
  (stroke *context*)
  ;; draw a green diagonal line
  (move-to *context* 0 0)
  (line-to *context* width height)
  (set-source-rgb *context* 0 1 0)
  (set-line-width *context* 5)
  (stroke *context*))
;; need to close window manually


(defun random-square (alpha)
  "Draw a blue rectangle with fixed size and the given transparency alpha."
  (move-to *context* 1 1)
  (line-to *context* -1 1)
  (line-to *context* -1 -1)
  (line-to *context* 1 -1)
  (close-path *context*)
  (set-source-rgba *context* 
	           (/ (random 100) 100)
                   (/ (random 100) 100)
                   (/ (random 100) 100)
	           alpha)
  (fill-path *context*))

(defparameter width 800)
(defparameter height 600)
(defparameter max-angle 90d0)
(setf *context* (create-xlib-image-context width height :window-name "rectangles"))

;; fill with white
(rectangle *context* 0 0 width height)
(set-source-rgb *context* 1 1 1)
(fill-path *context*)
;; draw the rectangles
(dotimes (i 500)
  (let ((scaling (+ 5d0 (random 40d0))))
    (reset-trans-matrix *context*)		       ; reset matrix
    (translate *context* (random width) (random height)) ; move the origin
    (scale *context* scaling scaling)		       ; scale
    (rotate *context* (deg-to-rad (random max-angle)))   ; rotate
    (random-square (+ 0.1 (random 0.4)))))
;; need to close window manually
