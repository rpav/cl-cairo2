;;;
;;; How to run this sample code
;;;
;;; 1) create Temp/png directory under the current *default-pathname-defaults*,
;;;    which is referenced as *png-dir* in the code,
;;; 2) compile the code, and then
;;; 3) call the exported functions.
;;;
;;; Result png files are saved to *png-dir*.
;;;
(defpackage :cairo-tutorial
  (:use :cl :cl-cairo2)
  (:export :stroke-example
           :fill-example
           :show-text-example
           :paint-example
           :mask-example
           :preparing-and-selecting-source-example
           :preparing-and-selecting-source2-example
           :path-example
           :text-example
           :line-width1-example
           :line-width2-example
           :text-alignment-example
           :run-all-examples))

(in-package :cairo-tutorial)

(defvar *png-dir* (merge-pathnames "Temp/png/"))
(defvar *png-width* 400)
(defvar *png-height* 400)

(defun clear-surface-rgba ()
  (set-source-rgba 1.0 1.0 1.0 1.0)
  (paint))

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

(defmacro with-surface-rgba ((fname width height) &body body)
  `(with-png-file (,fname :argb32 ,width ,height)
	 (clear-surface-rgba)
	 (scale ,width ,height)
	 ,@body))

(defmacro write-to-png ((verb) &body body)
  `(with-surface-rgba ((png-pathname-string ,verb) *png-width* *png-height*)
	 ,@body))

;;; executable examples start from here

(defun stroke-example ()
  (write-to-png ("stroke")
	(set-line-width 0.1)
	(set-source-rgb 0 0 0)
	(rectangle 0.25 0.25 0.5 0.5)
	(stroke)))

(defun fill-example ()
  (write-to-png ("fill")
	(set-source-rgb 0 0 0)
	(rectangle 0.25 0.25 0.5 0.5)
	(fill-path)))

(defun show-text-example ()
  (write-to-png ("show-text")
	(set-source-rgb 0 0 0)
	(select-font-face "Georgia" :normal :bold)
	(set-font-size 1.2)
	(let ((te (get-text-extents "a")))
	  (move-to (- 0.5 (/ (text-width te) 2) (text-x-bearing te))
			   (- 0.5 (/ (text-height te) 2) (text-y-bearing te)))
	  (show-text "a"))))

(defun paint-example ()
  (write-to-png ("paint")
	(set-source-rgb 0 0 0)
	(paint-with-alpha 0.5)))

(defun mask-example ()
  (write-to-png ("mask")
	(with-patterns ((linpat (create-linear-pattern 0 0 1 1))
					(radpat (create-radial-pattern 0.5 0.5 0.25 0.5 0.5 0.6)))
	  (pattern-add-color-stop-rgb linpat 0 0 0.3 0.8)
	  (pattern-add-color-stop-rgb linpat 1 0 0.8 0.3)
	  ;;
	  (pattern-add-color-stop-rgba radpat 0 0 0 0 1)
	  (pattern-add-color-stop-rgba radpat 1 0 0 0 0)
	  ;;
	  (set-source linpat)
	  (mask radpat))))

(defun preparing-and-selecting-source-example ()
  (write-to-png ("prep-select-source")
	(set-source-rgb 0 0 0)
	(move-to 0 0)
	(line-to 1 1)
	(move-to 1 0)
	(line-to 0 1)
	(set-line-width 0.2)
	(stroke)
	;;
	(rectangle 0 0 0.5 0.5)
	(set-source-rgba 1 0 0 0.8)
	(fill-path)
	;;
	(rectangle 0 0.5 0.5 0.5)
	(set-source-rgba 0 1 0 0.6)
	(fill-path)
	;;
	(rectangle 0.5 0 0.5 0.5)
	(set-source-rgba 0 0 1 0.4)
	(fill-path)))

(defun preparing-and-selecting-source2-example (&key (1st-fill-only nil) (2nd-fill-only nil))
  (write-to-png ("prep-select-source2")
	(block exit-anchor
	  (with-patterns ((radpat (create-radial-pattern 0.25 0.25 0.1 0.5 0.5 0.5))
					  (linpat (create-linear-pattern 0.25 0.35 0.75 0.65)))
		;;
		(unless 2nd-fill-only
		  (pattern-add-color-stop-rgb radpat 0 1.0 0.8 0.8)
		  (pattern-add-color-stop-rgb radpat 1 0.9 0.0 0.0)
		  (dotimes  (i 8)
			(dotimes (j 8)
			  (rectangle (- (/ (1+ i) 10.0) 0.04) (- (/ (1+ j) 10.0) 0.04) 0.08 0.08)))
		  (set-source radpat)
		  (fill-path)
		  (when 1st-fill-only (return-from exit-anchor)))
		;;
		(pattern-add-color-stop-rgba linpat 0.00 1.0 1.0 1.0 0.0)
		(pattern-add-color-stop-rgba linpat 0.25 0.0 1.0 0.0 0.5)
		(pattern-add-color-stop-rgba linpat 0.50 1.0 1.0 1.0 0.0)
		(pattern-add-color-stop-rgba linpat 0.75 0.0 0.0 1.0 0.5)
		(pattern-add-color-stop-rgba linpat 1.00 1.0 1.0 1.0 0.0)
		(rectangle 0 0 1 1)
		(set-source linpat)
		(fill-path)))))

(defun path-example (&key (do-stroke t) (do-fill nil))
  (write-to-png ("path")
	(set-line-width 0.1)
	(set-source-rgb 0 0 0)
	;;
	(move-to 0.25 0.25)
	(line-to 0.5 0.375)
	(rel-line-to 0.25 -0.125)
	(arc 0.5 0.5 (* 0.25 (sqrt 2)) (* -0.25 pi) (* 0.25 pi))
	(rel-curve-to -0.25 -0.125 -0.25 0.125 -0.5 0)
	(close-path)
	;;
	(when do-stroke
	  (set-line-width 0.04)
	  (set-source-rgb 0 0.64 0)
	  (stroke-preserve))
	;;
	(when do-fill
	  (set-source-rgba 0 0 0.8 0.4)
	  (fill-path))))

(defun text-example ()
  (write-to-png ("textextents")
	;; variable declarations
	(let ((x nil)
		  (y nil)
		  (px nil)
		  (ux 1)
		  (uy 1)
		  (dashlength (make-array 1 :element-type 'float :initial-element 0.0))
		  (text "joy")
		  (fe nil)
		  (te nil))
	  ;;
	  ;; prepare drawing context
	  ;;
	  (set-font-size 0.5)
	  ;;
	  ;; drawing code starts here
	  ;;
	  (set-source-rgb 0 0 0)
	  (select-font-face "Georgia" :normal :bold)
	  (setf fe (get-font-extents))
	  ;;
	  (multiple-value-setq (ux uy) (device-to-user-distance ux uy))
	  (setf px (max ux uy))
	  (setf fe (get-font-extents))
	  (setf te (get-text-extents text))
	  (setf x (- 0.5 (text-x-bearing te) (/ (text-width te) 2)))
	  (setf y (+ (- 0.5 (font-descent fe)) (/ (font-height fe) 2)))
	  ;;
	  ;; baseline, descent, ascent, height
	  ;;
	  (set-line-width (* 4 px))
	  (setf (aref dashlength 0) (* 9 px))
	  (set-dash 0 dashlength)
	  (set-source-rgba 0 0.6 0 0.5)
	  (move-to (+ x (text-x-bearing te)) y)
	  (rel-line-to (text-width te) 0)
	  (move-to (+ x (text-x-bearing te)) (+ y (font-descent fe)))
	  (rel-line-to (text-width te) 0)
	  (move-to (+ x (text-x-bearing te)) (- y (font-ascent fe)))
	  (rel-line-to (text-width te) 0)
	  (move-to (+ x (text-x-bearing te)) (- y (font-height fe)))
	  (rel-line-to (text-width te) 0)
	  (stroke)
	  ;;
	  ;; extents: width & height
	  ;;
	  (set-source-rgba 0 0 0.75 0.5)
	  (set-line-width px)
	  (setf (aref dashlength 0) (* 3 px))
	  (set-dash 0 dashlength)
	  (rectangle (+ x (text-x-bearing te)) (+ y (text-y-bearing te)) (text-width te) (text-height te))
	  (stroke)
	  ;;
	  ;; text
	  ;;
	  (move-to x y)
	  (set-source-rgb 0 0 0)
	  (show-text text)
	  ;;
	  ;; bearing
	  ;;
	  (set-dash 0 (make-array 0))
	  (set-line-width (* 2 px))
	  (set-source-rgba 0 0 0.75 0.5)
	  (move-to x y)
	  (rel-line-to (text-x-bearing te) (text-y-bearing te))
	  (stroke)
	  ;;
	  ;; text's advance
	  ;;
	  (set-source-rgba 0 0 0.75 0.5)
	  (arc (+ x (text-x-advance te)) (+ y (text-y-advance te)) (* 5 px) 0 (* 2 pi))
	  (fill-path)
	  ;;
	  ;; reference point
	  ;;
	  (arc x y (* 5 px) 0 (* 2 pi))
	  (set-source-rgba 0.75 0 0 0.5)
	  (fill-path))))

(defun line-width1-example ()
  (write-to-png ("line-width1")
	(let ((ux 1) (uy 1))
	  (multiple-value-setq (ux uy) (device-to-user-distance ux uy))
	  (when (< ux uy) (setf ux uy))
	  (set-line-width ux)
	  (set-source-rgb 0 0.2 0.8)
	  (rectangle 0 0 1 1)
	  (stroke)
	  (move-to 0 0)
	  (line-to 1 1)
	  (move-to 0 1)
	  (line-to 1 0)
	  (move-to 0 0.5)
	  (line-to 1 0.5)
	  (move-to 0.5 0)
	  (line-to 0.5 1)
	  (stroke))))

(defun line-width2-example ()
  (write-to-png ("line-width2")
	(set-source-rgb 0 1 0)
	(set-line-width 0.1)
	;;
	(save)
	(scale 0.5 1)
	(arc 0.5 0.5 0.4 0 (* 2 pi))
	(stroke)
	;;
	(translate 1 0)
	(arc 0.5 0.5 0.4 0 (* 2 pi))
	(restore)
	(stroke)))

(defun text-alignment-example (&key (font-name "Georgia") use-fe)
  (write-to-png ("text-alignment")
	(set-source-rgb 0 0 0)
	(select-font-face font-name :normal :normal)
	(set-font-size 0.1)
	;;
	(let* ((alphabet "QrStUvWxYz")
		   (alphalen (length alphabet))
		   (fe (get-font-extents)))
	  (dotimes (i alphalen)
		(let* ((str (string (aref alphabet i)))
			   (te (get-text-extents str))
			   (x (+ (/ 1 (* alphalen 2)) (* (/ 1 alphalen) i))))
		  (move-to (- x (text-x-bearing te) (/ (text-width te) 2))
				   (if use-fe
					   (+ (- 0.5 (font-descent fe)) (/ (font-height fe) 2))
					   (- 0.5 (text-y-bearing te) (/ (text-height te) 2))))
		  (show-text str))))))


(defun run-all-examples (&optional (debug nil))
  (let* ((-example "-EXAMPLE")
		 (-example-len (length -example))
		 (pkg #.(package-name *package*)))
	(mapcar #'(lambda (sym)
				(let* ((sym-name (symbol-name sym))
					   (idx (search -example sym-name :from-end t)))
				  (if (and (< 0 idx)
						   (eql (length sym-name) (+ idx -example-len))
						   (fboundp (intern sym-name pkg)))
					  (if (not debug)
						  (funcall (symbol-function (intern sym-name pkg)))
						  (list sym sym-name))
					  (when debug
						(list sym nil)))))
			(apropos-list -example pkg))))
