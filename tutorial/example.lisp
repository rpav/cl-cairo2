(require :asdf)
(asdf:operate 'asdf:load-op :cl-cairo2)

;;;; Make a test package
(defpackage :cairo-example
  (:use :common-lisp :cl-cairo2))

(in-package :cairo-example)

;;;;
;;;;  short example for the tutorial
;;;;

(defparameter *surface* (create-pdf-surface "example.pdf" 200 100))
(defparameter *context* (create-context *surface*))
(destroy *surface*)
;; clear the whole canvas with blue
(set-source-rgb *context* 0.2 0.2 1)
(paint *context*)
;; draw a white diagonal line
(move-to *context* 200 0)
(line-to *context* 0 100)
(set-source-rgb *context* 1 1 1)
(set-line-width *context* 5)
(stroke *context*)
;; destroy context, this also destroys the surface and closes the file
(destroy *context*)

;;;;
;;;; helper functions
;;;;

(defun show-text-aligned (context text x y &optional (x-align 0.5) (y-align 0.5))
  "Show text aligned relative to (x,y)."
  (multiple-value-bind (x-bearing y-bearing width height)
      (text-extents context text)
    (move-to context (- x (* width x-align) x-bearing)
	     (- y (* height y-align) y-bearing))
    (show-text context text)))

;;;; very simple text example
(defparameter *context* (create-pdf-context "simpletext.pdf" 100 100))
(move-to *context* 0 100)
(set-font-size *context* 50)
(show-text *context* "foo")
(destroy *context*)


;;;;
;;;;  text placement example
;;;;
;;;;  This example demonstrates the use of text-extents, by placing
;;;;  text aligned relative to a red marker.

(defun mark-at (context x y d red green blue)
  "Make a rectangle of size 2d around x y with the given colors,
  50% alpha.  Used for marking points."
  (rectangle context (- x d) (- y d) (* 2 d) (* 2 d))
  (set-source-rgba context red green blue 0.5)
  (fill-path context))

(defun show-text-with-marker (context text x y x-align y-align)
  "Show text aligned relative to a red market at (x,y)."
  (mark-at context x y 2 1 0 0)
  (set-source-rgba context 0 0 0 0.6)
  (show-text-aligned context text x y x-align y-align))

(defparameter width 500)
(defparameter height 500)
(defparameter text "Fog")		; contains g, which goes below baseline
(defparameter size 50)
(defparameter x 20d0)
(defparameter y 50d0)
(defparameter *context* (create-pdf-context "text.pdf" width height))
;;(setf *context* (create-svg-context "text.svg" width height))
;;(setf *context* (create-pdf-context "text.pdf" width height))
;; white background
(set-source-rgb *context* 1 1 1)
(paint *context*)
;; setup font
(select-font-face *context* "Arial" :normal :normal)
(set-font-size *context* size)
;; starting point
(mark-at *context* x y 2 1 0 0)		; red
;; first text in a box
(multiple-value-bind (x-bearing y-bearing text-width text-height)
    (text-extents *context* text)
  (let ((rect-x (+ x x-bearing))
	(rect-y (+ y y-bearing)))
    (rectangle *context* rect-x rect-y text-width text-height)
    (set-source-rgba *context* 0 0 1 0.3)		; blue
    (set-line-width *context* 1)
    (set-dash *context* 0 '(5 5))
    (stroke *context*)))
(set-source-rgba *context* 0 0 0 0.6)
(move-to *context* x y)
(show-text *context* text)
(dolist (x-align '(0 0.5 1))
  (dolist (y-align '(0 0.5 1))
    (show-text-with-marker *context* text (+ x (* x-align 300))
			   (+ y (* y-align 300) 100)
			   x-align y-align)))
;; done
(destroy *context*)



;;;;
;;;;  Lissajous curves
;;;;

(defparameter size 500)
(defparameter margin 20)
(defparameter a 9)
(defparameter b 8)
(defparameter delta (/ pi 2))
(defparameter density 2000)
(setf *context* (create-pdf-context "lissajous.pdf" size size))
;; pastel blue background
(rectangle *context* 0 0 width height)
(set-source-rgb *context* 0.9 0.9 1)
(fill-path *context*)
;; Lissajous curves, blue
(labels ((stretch (x) (+ (* (1+ x) (- (/ size 2) margin)) margin)))
  (move-to *context* (stretch (sin delta)) (stretch 0))
  (dotimes (i density)
    (let* ((v (/ (* i pi 2) density))
	   (x (sin (+ (* a v) delta)))
	   (y (sin (* b v))))
      (line-to *context* (stretch x) (stretch y)))))
(close-path *context*)
(set-line-width *context* .5)
(set-source-rgb *context* 0 0 1)
(stroke *context*)
;; "cl-cairo2" in Arial bold to the center
(select-font-face *context* "Arial" :normal :bold)
(set-font-size *context* 100)
(set-source-rgba *context* 1 0.75 0 0.5)		; orange
(show-text-aligned *context* "cl-cairo2" (/ size 2) (/ size 2))
;; done
(destroy *context*)

;;;;
;;;;  transformation matrix example (for Judit, with love)
;;;;
;;;;  This example uses the function heart which fills a heart-shape
;;;;  with given transparency at the origin, using a fixed size.
;;;;  Rotation, translation and scaling is achieved using the
;;;;  appropriate cairo functions.

(defun heart (context alpha)
  "Draw a heart with fixed size and the given transparency alpha.
  Heart is upside down."
  (let ((radius (sqrt 0.5)))
    (move-to context 0 -2)
    (line-to context 1 -1)
    (arc context 0.5 -0.5 radius (deg-to-rad -45) (deg-to-rad 135))
    (arc context -0.5 -0.5 radius (deg-to-rad 45) (deg-to-rad 215))
    (close-path context)
    (set-source-rgba context 1 0 0 alpha)
    (fill-path context)))

(defparameter width 1024)
(defparameter height 768)
(defparameter max-angle 40d0)
;(with-png-file 
(defparameter *context* (create-pdf-context "hearts.pdf" width height))
;; fill with white
(rectangle *context* 0 0 width height)
(set-source-rgb *context* 1 1 1)
(fill-path *context*)
;; draw the hearts
(dotimes (i 200)
  (let ((scaling (+ 5d0 (random 40d0))))
    (reset-trans-matrix *context*)	; reset matrix
    (translate *context* (random width) (random height)) ; move the origin
    (scale *context* scaling scaling)		    ; scale
    (rotate *context* (deg-to-rad (- (random (* 2 max-angle))
				     max-angle 180))) ; rotate
    (heart *context* (+ 0.1 (random 0.7)))))
(destroy *context*)

;;;;
;;;; make a rainbow-like pattern
;;;;
;;;;

(defparameter width 100)
(defparameter height 40)
(defparameter *context* (create-pdf-context "pattern.pdf" width height))
(with-linear-pattern rainbow (0 0 width 0)
    `((0   (0.7 0 0.7 0))      ;rgb(a) color as list
      (1/6 ,cl-colors:+blue+)  ;color as cl-color
      (2/6 ,cl-colors:+green+)
      (3/6 ,cl-colors:+yellow+)
      (4/6 ,cl-colors:+orange+)
      (5/6 ,cl-colors:+red+)
      (1   ,cl-colors:+violetred+))
  (rectangle *context* 0 0 width height)
  (set-source *context* rainbow)
  (fill-path *context*))
(destroy *context*)
