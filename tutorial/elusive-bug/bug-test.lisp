(asdf:operate 'asdf:load-op :cl-cairo2)

;;;; Make a test package
(defpackage :cairo-example-bug-test
  (:use :common-lisp :cl-cairo2))

(declaim (optimize (debug 3)))

(in-package :cairo-example-bug-test)

(defun show-text-aligned (text x y &optional (x-align 0.5) (y-align 0.5)
			  (context *context*))
  "Show text aligned relative to (x,y)."
  (let ((*context* context))
    (multiple-value-bind (x-bearing y-bearing width height)
	(text-extents text)
      (move-to (- x (* width x-align) x-bearing)
	       (- y (* height y-align) y-bearing))
      (show-text text))))
;;;;
;;;;  text placement example
;;;;
;;;;  This example demonstrates the use of text-extents, by placing
;;;;  text aligned relative to a red marker.

(defun mark-at (x y d red green blue)
  "Make a rectangle of size 2d around x y with the given colors,
  50% alpha.  Used for marking points."
  (rectangle (- x d) (- y d) (* 2 d) (* 2 d))
  (set-source-rgba red green blue 0.5)
  (fill-path))

(defun show-text-with-marker (text x y x-align y-align)
  "Show text aligned relative to a red market at (x,y)."
  (mark-at x y 2 1 0 0)
  (set-source-rgb 0 0 1)
  (show-text-aligned text x y x-align y-align))


(progn

(defparameter width 500)
(defparameter height 500)
(defparameter text "Fog")		; contains g, which goes below baseline
(defparameter size 50)
(defparameter x 20d0)
(defparameter y 50d0)
(setf *context* (create-ps-context "/tmp/text.ps" width height))
;;(setf *context* (create-svg-context "text.svg" width height))
;;(setf *context* (create-pdf-context "text.pdf" width height))
;; white background
(set-source-rgb 1 1 1)
(paint)
;; setup font
(select-font-face "Arial" 'font-slant-normal 'font-weight-normal)
(set-font-size size)
;; starting point
;;(mark-at x y 2 1 0 0)			; red
;; first text in a box
(multiple-value-bind (x-bearing y-bearing text-width text-height)
    (text-extents text)
  (let ((rect-x (+ x x-bearing))
	(rect-y (+ y y-bearing)))
    (rectangle rect-x rect-y text-width text-height)
    (set-source-rgba 0 0 1 0.3)		; blue
    (set-line-width 1)
    (set-dash 0 '(5 5))
    (stroke)))
(set-source-rgba 0 0 0 0.6)
(move-to x y)
(show-text text)
;; text automatically aligned
;; (dolist (x-align '(0 0.5 1))
;;   (dolist (y-align '(0 0.5 1))
;;     (show-text-with-marker text (+ x (* x-align 300)) (+ y (* y-align 300) 100)
;; 			   x-align y-align)))
;; (dolist (x-align '(0))
;;   (dolist (y-align '(0))
;;     (show-text-with-marker text (+ x (* x-align 300)) (+ y (* y-align 300) 100)
;; 			   x-align y-align)))

;; (move-to 50 50)
;; (show-text "Bar")
(show-text-with-marker text x (+ y 100d0) 0d0 0d0)
;; done
(destroy *context*)

)

