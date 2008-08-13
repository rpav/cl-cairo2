(in-package :cl-cairo2)
;;;;
;;;;  Lissajous curves
;;;;

(defun show-text-aligned (text x y &optional (x-align 0.5) (y-align 0.5)
			  (context *context*))
  "Show text aligned relative to (x,y)."
  (let ((*context* context))
    (multiple-value-bind (x-bearing y-bearing width height)
	(text-extents text)
      (move-to (- x (* width x-align) x-bearing)
	       (- y (* height y-align) y-bearing))
      (show-text text))))

(defparameter size 500)
(defparameter margin 20)
(defparameter a 9)
(defparameter b 8)
(defparameter delta (/ pi 2))
(defparameter density 2000)
(defparameter macp (or (member :darwin *features*)
					   (member :macos *features*)
					   (member :macosx *features*)))

;; get context
(setf *context* (create-xlib-image-context
				 size size
				 :window-name (concatenate 'string "Lissajous" (if macp "-mac" ""))))
;; pastel blue background
(rectangle 0 0 size size)
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
(show-text-aligned "cl-cairo2" (/ size 2) (/ size 2))
;; ask to quit
(loop until (y-or-n-p "quit?"))
;; done
(destroy *context*)
