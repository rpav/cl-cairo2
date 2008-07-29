;;;
;;; How to run this sample code
;;;
;;; 1) create Temp/png directory under the current *default-pathname-defaults*,
;;;    which is referenced as *png-dir* in the code,
;;; 2) copy flowers.png to *png-dir*,
;;; 3) compile the code, and then
;;; 4) call the exported functions.
;;;
;;; Result png files are saved to *png-dir*.
;;;
(defpackage :cairo-samples
  (:use :cl :cl-cairo2)
  (:export :arc-sample
		   :clip-sample
		   :clip-image-sample
		   :curve-rectangle-sample
		   :curve-to-sample
		   :dash-sample
		   :fill-and-stroke2-sample
		   :gradient-sample
		   :image-sample
		   :imagepattern-sample
		   :multi-segment-caps-sample
		   :set-line-cap-sample
		   :set-line-join-sample
		   :text-sample
		   :text-align-center-sample
		   :text-extents-sample
		   :run-all-samples))

(in-package :cairo-samples)

(defvar *png-dir*			(merge-pathnames "Temp/png/"))
(defvar *sample-src-file*	"flowers")
(defvar *png-width*			400)
(defvar *png-height*		400)

(defun clear-surface-rgba ()
  (save)
  (set-source-rgba 1.0 1.0 1.0 1.0)
  (set-operator :source)
  (paint)
  (restore))

(defun clear-surface-rgb ()
  (save)
  (set-source-rgb 1.0 1.0 1.0)
  (set-operator :source)
  (paint)
  (restore))

(defun png-pathname-string (fname &optional (check-existance nil))
  (let ((pngdir  *png-dir*))
	(if (probe-file pngdir)
		(let ((path (namestring (merge-pathnames (concatenate 'string fname ".png") pngdir))))
		  (if check-existance
			  (if (probe-file path)
				  path
				  (error "~A doesn't exist" path))
			  path))
		(error "~A (see *png-dir*) doesn't exist" pngdir))))

(defmacro with-png-surface-rgba ((fname width height) &body body)
  `(with-png-file (,fname :argb32 ,width ,height)
	 (clear-surface-rgba)
	 (set-source-rgb 0 0 0)
;	 (scale ,width ,height)
	 ,@body))

(defmacro with-png-surface-rgb ((fname width height) &body body)
  `(with-png-file (,fname :rgb24 ,width ,height)
	 (clear-surface-rgb)
	 (set-source-rgb 0 0 0)
;	 (scale ,width ,height)
	 ,@body))

(defmacro write-to-png ((verb) &body body)
  `(with-png-surface-rgba ((png-pathname-string ,verb) *png-width* *png-height*)
	   ,@body))

(defmacro write-to-png24 ((verb) &body body)
  `(with-png-surface-rgb ((png-pathname-string ,verb) *png-width* *png-height*)
	   ,@body))

;;; executable examples start from here

(defun arc-sample (&optional (arc-positive t))
  (write-to-png ("arc")
	(let ((xc 128.0)
		  (yc 128.0)
		  (radius 100.0)
		  (angle1 (* 45.0  (/ pi 180.0)))	; angles are specified
		  (angle2 (* 180.0 (/ pi 180.0))))	; in radians
	  (set-line-width 10.0)
	  (if arc-positive
		  (arc xc yc radius angle1 angle2)
		  (arc-negative xc yc radius angle1 angle2))
	  (stroke)
	  ;; draw helping lines
	  (set-source-rgba 1 0.2 0.2 0.6)
	  (set-line-width 6.0)
	  (arc xc yc 10.0 0 (* 2 pi))
	  (fill-path)
	  ;;
;	  (arc xc yc radius angle1 angle1) ; This causes floating point invalid operation on cairo-arc call
	  (arc xc yc radius angle1 (+ angle1 (deg-to-rad 0.01))) ; so do this instead as workaround.
	  (line-to xc yc)
;	  (arc xc yc radius angle2 angle2);
	  (arc xc yc radius angle2 (+ angle2 (deg-to-rad 0.01))) ; ditto
	  (line-to xc yc)
	  (stroke))))

(defun clip-sample ()
  (write-to-png ("clip")
	(arc 128.0 128.0 76.8 0 (* 2 pi))
	(clip)
	;;
	(new-path) ; current path is not consumed by clip
	;;
	(rectangle 0 0 256 256)
	(fill-path)
	(set-source-rgb 0 1 0)
	(move-to 0 0)
	(line-to 256 256)
	(move-to 256 0)
	(line-to 0 256)
	(set-line-width 10.0)
	(stroke)))

(defun clip-image-sample ()
  (write-to-png ("clip-image")
	(arc 128.0 128.0 76.8 0 (* 2 pi))
	(clip)
	(new-path) ; path not consumed by clip
	;;
	(with-png-surface ((png-pathname-string *sample-src-file* t) image)
	  (let ((w (image-surface-get-width image))
			(h (image-surface-get-height image)))
		;;
		(scale (/ 256.0 w) (/ 256.0 h))
		;;
		(set-source-surface image 0 0)
		(paint)))))

(defun curve-rectangle-sample ()
  (write-to-png ("curve-rectangle")
	;; a custom shape that could be wrapped in a function
	(let* ((x0 25.6)
		   (y0 25.6)
		   (rect-width 204.8)
		   (rect-height 204.8)
		   (radius 102.4)
		   (x1 (+ x0 rect-width))
		   (y1 (+ y0 rect-height)))
	  (if (< (/ rect-width 2) radius)
		  (if (< (/ rect-height 2) radius)
			  (progn
				(move-to x0 (/ (+ y0 y1) 2))
				(curve-to x0 y0 x0 y0 (/ (+ x0 x1) 2) y0)
				(curve-to x1 y0 x1 y0 x1 (/ (+ y0 y1) 2))
				(curve-to x1 y1 x1 y1 (/ (+ x1 x0) 2) y1)
				(curve-to x0 y1 x0 y1 x0 (/ (+ y0 y1) 2)))
			  (progn
				(move-to x0 (+ y0 radius))
				(curve-to x0 y0 x0 y0 (/ (+ x0 x1) 2) y0)
				(curve-to x1 y0 x1 y0 x1 (+ y0 radius))
				(line-to x1 (- y1 radius))
				(curve-to x1 y1 x1 y1 (/ (+ x1 x0) 2) y1)
				(curve-to x0 y1 x0 y1 x0 (- y1 radius))))
		  (if (< (/ rect-height 2) radius)
			  (progn
				(move-to x0 (/ (+ y0 y1) 2))
				(curve-to x0 y0 x0 y0 (/ (+ x0 radius) y0))
				(line-to (- x1 radius) y0)
				(curve-to x1 y0 x1 y0 x1 (/ (+ y0 y1) 2))
				(curve-to x1 y1 x1 y1 (- x1 radius) y1)
				(line-to (+ x0 radius) y1)
				(curve-to x0 y1 x0 y1 x0 (/ (+ y0 y1) 2)))
			  (progn
				(move-to x0 (+ y0 radius))
				(curve-to x0 y0 x0 y0 (+ x0 radius) y0)
				(line-to (- x1 radius) y0)
				(curve-to x1 y0 x1 y0 x1 (+ y0 radius))
				(line-to x1 (- y1 radius))
				(curve-to x1 y1 x1 y1 (- x1 radius) y1)
				(line-to (+ x0 radius) y1)
				(curve-to x0 y1 x0 y1 x0 (- y1 radius)))))
	  (close-path)
	  ;;
	  (set-source-rgb 0.5 0.5 1)
	  (fill-preserve)
	  (set-source-rgba 0.5 0 0 0.5)
	  (set-line-width 10.0)
	  (stroke))))

(defun curve-to-sample ()
  (write-to-png ("curve-to")
	(let ((x 25.6) (y 128.0)
		  (x1 102.4) (y1 230.4)
		  (x2 153.6) (y2 25.6)
		  (x3 230.4) (y3 128.0))
	  (move-to x y)
	  (curve-to x1 y1 x2 y2 x3 y3)
	  ;;
	  (set-line-width 10.0)
	  (stroke)
	  ;;
	  (set-source-rgba 1 0.2 0.2 0.6)
	  (set-line-width 6.0)
	  (move-to x y) (line-to x1 y1)
	  (move-to x2 y2) (line-to x3 y3)
	  (stroke))))

(defun dash-sample ()
  (write-to-png ("dash")
	(let* ((dashes (make-array 4 :element-type 'float :initial-contents '(50.0 10.0 10.0 10.0)))
		   (offset -50.0))
	  (set-dash offset dashes)
	  (set-line-width 10.0)
	  ;;
	  (move-to 128.0 25.6)
	  (line-to 230.4 230.4)
	  (rel-line-to -102.4 0.0)
	  (curve-to 51.2 230.4 51.2 128.0 128.0 128.0)
	  ;;
	  (stroke))))

(defun fill-and-stroke2-sample ()
  (write-to-png ("fill-and-stroke2")
	(move-to 128.0 25.6)
	(line-to 230.4 230.4)
	(rel-line-to -102.4 0.0)
	(curve-to 51.2 230.4 51.2 128.0 128.0 128.0)
	(close-path)
	;;
	(move-to 64.0 25.6)
	(rel-line-to 51.2 51.2)
	(rel-line-to -51.2 51.2)
	(rel-line-to -51.2 -51.2)
	(close-path)
	;;
	(set-line-width 10.0)
	(set-source-rgb 0 0 1)
	(fill-preserve)
	(set-source-rgb 0 0 0)
	(stroke)))

(defun gradient-sample ()
  (write-to-png ("gradient")
	(with-patterns ((pat (create-linear-pattern 0.0 0.0 0.0 256.0)))
	  (pattern-add-color-stop-rgba pat 1 0 0 0 1)
	  (pattern-add-color-stop-rgba pat 0 1 1 1 1)
	  (rectangle 0 0 256 256)
	  (set-source pat)
	  (fill-path))
	;;
	(with-patterns ((pat (create-radial-pattern 115.2 102.4 25.6 102.4 102.4 128.0)))
	  (pattern-add-color-stop-rgba pat 0 1 1 1 1)
	  (pattern-add-color-stop-rgba pat 1 0 0 0 1)
	  (set-source pat)
	  (arc 128.0 128.0 76.8 0 (* 2 pi))
	  (fill-path))))

(defun image-sample ()
  (write-to-png ("image")
	(with-png-surface ((png-pathname-string *sample-src-file* t) image)
	  (let ((w (image-surface-get-width image))
			(h (image-surface-get-height image)))
		(translate (/ *png-width* 2) (/ *png-height* 2)) ; was 128.0, 128.0
		(rotate (* 45 (/ pi 180)))
		(scale (/ 256.0 w) (/ 256.0 h))
		(translate (* -0.5 w) (* -0.5 h))
		;;
		(set-source-surface image 0 0)
		(paint)))))

(defun imagepattern-sample (&optional (draw-frame nil))
  (write-to-png ("imagepattern")
	(with-png-surface ((png-pathname-string *sample-src-file* t) image)
	  (let ((w (image-surface-get-width image))
			(h (image-surface-get-height image))
			(matrix nil))
		;;
		(when draw-frame
		  (move-to 0 0)
		  (line-to 0 *png-height*)
		  (line-to *png-width* *png-height*)
		  (line-to *png-width* 0)
		  (line-to 0 0)
		  (stroke))
		;;
		(with-patterns ((pattern (create-pattern-for-surface image)))
		  (pattern-set-extend pattern :repeat)
		  ;;
		  (translate (/ *png-width* 2) (/ *png-height* 2)) ; was 128.0, 128.0
		  (rotate (/ pi 4))
		  (scale (/ 1 (sqrt 2)) (/ 1 (sqrt 2)))
		  (translate (/ *png-width* -2) (/ *png-height* -2)) ; was -128.0, -128.0
		  ;;
		  (setf matrix (trans-matrix-init-scale (* (/ w 200.0) 2.5) (* (/ h 200.0) 2.5))) ; was 256.0 * 5.0, 256.0 * 5.0
		  (pattern-set-matrix pattern matrix)
		  ;;
		  (set-source pattern)
		  ;;
		  (rectangle 0 0 *png-width* *png-height*) ; was 256.0 256.0
		  (fill-path))))))

(defun multi-segment-caps-sample ()
  (write-to-png ("multi-segment-caps")
	(move-to 50.0 75.0)
	(line-to 200.0 75.0)
	;;
	(move-to 50.0 125.0)
	(line-to 200.0 125.0)
	;;
	(move-to 50.0 175.0)
	(line-to 200.0 175.0)
	;;
	(set-line-width 30.0)
	(set-line-cap :round)
	(stroke)))

(defun set-line-cap-sample ()
  (write-to-png ("set-line-cap")
	(set-line-width 30.0)
	(set-line-cap :butt) ; default
	(move-to 64.0 50.0) (line-to 64.0 200.0)
	(stroke)
	(set-line-cap :round)
	(move-to 128.0 50.0) (line-to 128.0 200.0)
	(stroke)
	(set-line-cap :square)
	(move-to 192.0 50.0) (line-to 192.0 200.0)
	(stroke)
	;;
	;; draw helping lines
	(set-source-rgb 1 0.2 0.2)
	(set-line-width 2.56)
	(move-to 64.0 50.0) (line-to 64.0 200.0)
	(move-to 128.0 50.0) (line-to 128.0 200.0)
	(move-to 192.0 50.0) (line-to 192.0 200.0)
	(stroke)))

(defun set-line-join-sample ()
  (write-to-png ("set-line-join")
	(set-line-width 40.96)
	(move-to 76.8 84.48)
	(rel-line-to 51.2 -51.2)
	(rel-line-to 51.2 51.2)
	(set-line-join :miter) ; default
	(stroke)
	;;
	(move-to 76.8 161.28)
	(rel-line-to 51.2 -51.2)
	(rel-line-to 51.2 51.2)
	(set-line-join :bevel)
	(stroke)
	;;
	(move-to 76.8 238.08)
	(rel-line-to 51.2 -51.2)
	(rel-line-to 51.2 51.2)
	(set-line-join :round)
	(stroke)))

(defun text-sample ()
  (write-to-png ("text")
	(select-font-face "Sans" :normal :bold)
	(set-font-size 90.0)
	;;
	(move-to 10.0 135.0)
	(show-text "Hello")
	;;
	(move-to 70.0 165.0)
	(text-path "void")
	(set-source-rgb 0.5 0.5 1)
	(fill-preserve)
	(set-source-rgb 0 0 0)
	(set-line-width 2.56)
	(stroke)
	;;
	;; draw helping lines
	(set-source-rgba 1 0.2 0.2 0.6)
	(arc 10.0 135.0 5.12 0 (* 2 pi))
	(close-path)
	(arc 70.0 165.0 5.12 0 (* 2 pi))
	(fill-path)))

(defun text-align-center-sample ()
  (write-to-png ("text-align-center")
	(let ((extents nil)
		  (utf8 "cairo")
		  (x nil) (y nil))
	  ;;
	  (select-font-face "Sans" :normal :normal)
	  ;;
	  (set-font-size 52.0)
	  (setf extents (get-text-extents utf8))
	  (setf x (- (/ *png-width* 2) (+ (/ (text-width extents) 2) (text-x-bearing extents)))) ; was 128.0
	  (setf y (- (/ *png-height* 2) (+ (/ (text-height extents) 2) (text-y-bearing extents)))) ; was 128.0
	  ;;
	  (move-to x y)
	  (show-text utf8)
	  ;;
	  ;; draw helping lines
	  (set-source-rgba 1 0.2 0.2 0.6)
	  (set-line-width 6.0)
	  (arc x y 10.0 0 (* 2 pi))
	  (fill-path)
	  (move-to (/ *png-width* 2) 0) ; was 128.0
	  (rel-line-to 0 *png-height*) ; was 256
	  (move-to 0 (/ *png-height* 2)) ; was 128.0
	  (rel-line-to *png-width* 0) ; was 256
	  (stroke))))

(defun text-extents-sample ()
  (write-to-png ("text-extents")
	(let ((extents nil)
		  (utf8 "cairo")
		  (x nil) (y nil))
	  ;;
	  (select-font-face "Sans" :normal :normal)
	  ;;
	  (set-font-size 100.0)
	  (setf extents (get-text-extents utf8))
	  ;;
	  (setf x 25.0)
	  (setf y 150.0)
	  ;;
	  (move-to x y)
	  (show-text utf8)
	  ;;
	  ;; draw helping lines
	  (set-source-rgba 1 0.2 0.2 0.6)
	  (set-line-width 6.0)
	  (arc x y 10.0 0 (* 2 pi))
	  (fill-path)
	  (move-to x y)
	  (rel-line-to 0 (- (text-height extents)))
	  (rel-line-to (text-width extents) 0)
	  (rel-line-to (text-x-bearing extents) (- (text-y-bearing extents)))
	  (stroke))))

(defun run-all-samples (&optional (debug nil))
  (let* ((-sample "-SAMPLE")
		 (-sample-len (length -sample))
		 (pkg #.(package-name *package*)))
	(mapcar #'(lambda (sym)
				(let* ((sym-name (symbol-name sym))
					   (idx (search -sample sym-name :from-end t)))
				  (if (and (< 0 idx)
						   (eql (length sym-name) (+ idx -sample-len))
						   (fboundp (intern sym-name pkg)))
					  (if (not debug)
						  (funcall (symbol-function (intern sym-name pkg)))
						  (list sym sym-name))
					  (when debug
						(list sym nil)))))
			(apropos-list -sample pkg))))
