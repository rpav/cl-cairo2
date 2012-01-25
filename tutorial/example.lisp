(require :asdf)
(asdf:operate 'asdf:load-op :cl-cairo2)

;;;; Make a test package
(defpackage :cairo-example
  (:use :common-lisp :cl-cairo2))

(in-package :cairo-example)

;;;;
;;;;  pathname where files will end up, you need to change it to
;;;;  something that makes sense on your system if you are running
;;;;  these examples
;;;;

(setf *default-pathname-defaults*
      (merge-pathnames (make-pathname :directory '(:relative "tutorial"))
                       (asdf:system-source-directory :cl-cairo2)))
(assert (directory *default-pathname-defaults*))

;;;;
;;;;  short example for the tutorial
;;;;

(defparameter *surface* (create-pdf-surface "example.pdf" 200 100))
(setf *context* (create-context *surface*))
(destroy *surface*)
;; clear the whole canvas with blue
(set-source-rgb 0.2 0.2 1)
(paint)
;; draw a white diagonal line
(move-to 200 0)
(line-to 0 100)
(set-source-rgb 1 1 1)
(set-line-width 5)
(stroke)
;; destroy context, this also destroys the surface and closes the file
(destroy *context*)

;;;; very simple text example
(setf *context* (create-pdf-context "simpletext.pdf" 100 100))
(move-to 0 100)
(set-font-size 50)
(show-text "foo")
(destroy *context*)


;;;;
;;;;  text placement example
;;;;
;;;;  This example demonstrates the use of text-extents, by placing
;;;;  text aligned relative to a red marker.

;;;;
;;;; helper functions
;;;;

(defun show-text-aligned (text x y x-align y-align &optional
			  (context *context*))
  "Show text aligned relative to (x,y)."
  (with-context (context)
    (multiple-value-bind (x-bearing y-bearing width height)
	(text-extents text)
      (move-to (- x (* width x-align) x-bearing)
	       (- y (* height y-align) y-bearing))
      (show-text text))))

(defun mark-at (x y d red green blue &optional (context *context*))
  "Make a rectangle of size 2d around x y with the given colors,
  50% alpha.  Used for marking points."
  (with-context (context)
    (rectangle (- x d) (- y d) (* 2 d) (* 2 d))
    (set-source-rgba red green blue 0.5)
    (fill-path)))

(defun show-text-with-marker (text x y x-align y-align &optional (context *context*))
  "Show text aligned relative to a red market at (x,y)."
  (with-context (context)
    (mark-at x y 2 1 0 0)
    (set-source-rgba 0 0 0 0.6)
    (show-text-aligned text x y x-align y-align)))

(defparameter width 500)
(defparameter height 500)
(defparameter text "Fog")		; contains g, which goes below baseline
(defparameter size 50)
(defparameter x 20d0)
(defparameter y 50d0)
(setf *context* (create-pdf-context "text.pdf" width height))
;; white background
(set-source-rgb 1 1 1)
(paint)
;; setup font
(select-font-face "Arial" :normal :normal)
(set-font-size size)
;; starting point
(mark-at x y 2 1 0 0)		; red
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
(dolist (x-align '(0 0.5 1))
  (dolist (y-align '(0 0.5 1))
    (show-text-with-marker text (+ x (* x-align 300))
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
(rectangle 0 0 width height)
(set-source-rgb 0.9 0.9 1)
(fill-path)
;; Lissajous curves, blue
(labels ((stretch (x) (+ (* (1+ x) (- (/ size 2) margin)) margin)))
  (move-to (stretch (sin delta)) (stretch 0))
  (dotimes (i density)
    (let* ((v (/ (* i pi 2) density))
	   (x (sin (+ (* a v) delta)))
	   (y (sin (* b v))))
      (line-to (stretch x) (stretch y)))))
(close-path)
(set-line-width .5)
(set-source-rgb 0 0 1)
(stroke)
;; "cl-cairo2" in Arial bold to the center
(select-font-face "Arial" :normal :bold)
(set-font-size 100)
(set-source-rgba 1 0.75 0 0.5)		; orange
(show-text-aligned "cl-cairo2" (/ size 2) (/ size 2) 0.5 0.5)
;; done
(destroy *context*)

;;;;
;;;;  transformation matrix example (for Judit, with love)
;;;;
;;;;  This example uses the function heart which fills a heart-shape
;;;;  with given transparency at the origin, using a fixed size.
;;;;  Rotation, translation and scaling is achieved using the
;;;;  appropriate cairo functions.

(defun heart (alpha &optional (context *context*))
  "Draw a heart with fixed size and the given transparency alpha.
  Heart is upside down."
  (with-context (context)
    (let ((radius (sqrt 0.5)))
      (move-to 0 -2)
      (line-to 1 -1)
      (arc 0.5 -0.5 radius (deg-to-rad -45) (deg-to-rad 135))
      (arc -0.5 -0.5 radius (deg-to-rad 45) (deg-to-rad 215))
      (close-path)
      (set-source-rgba 1 0 0 alpha)
      (fill-path))))

(defparameter width 1024)
(defparameter height 768)
(defparameter max-angle 40d0)
(setf *context* (create-pdf-context "hearts.pdf" width height))
;; fill with white
(rectangle 0 0 width height)
(set-source-rgb 1 1 1)
(fill-path)
;; draw the hearts
(dotimes (i 200)
  (let ((scaling (+ 5d0 (random 40d0))))
    (reset-trans-matrix)	; reset matrix
    (translate (random width) (random height)) ; move the origin
    (scale scaling scaling)		    ; scale
    (rotate (deg-to-rad (- (random (* 2 max-angle))
				     max-angle 180))) ; rotate
    (heart (+ 0.1 (random 0.7)))))
(destroy *context*)

;;;;
;;;; make a rainbow-like pattern
;;;;
;;;;

(defparameter width 100)
(defparameter height 40)
(setf *context* (create-pdf-context "pattern.pdf" width height))
(with-linear-pattern rainbow (0 0 width 0)
    `((0   (0.7 0 0.7 0))      ;rgb(a) color as list
      (1/6 ,cl-colors:+blue+)  ;color as cl-color
      (2/6 ,cl-colors:+green+)
      (3/6 ,cl-colors:+yellow+)
      (4/6 ,cl-colors:+orange+)
      (5/6 ,cl-colors:+red+)
      (1   ,cl-colors:+violetred+))
  (rectangle 0 0 width height)
  (set-source rainbow)
  (fill-path))
(destroy *context*)

;;;;
;;;;  example for with-png-file
;;;;
(with-png-file ("simple.png" :rgb24 200 100)
  ;; clear the whole canvas with blue
  (set-source-rgb 0.2 0.2 1)
  (paint)
  ;; draw a white diagonal line
  (move-to 200 0)
  (line-to 0 100)
  (set-source-rgb 1 1 1)
  (set-line-width 5)
  (stroke))
