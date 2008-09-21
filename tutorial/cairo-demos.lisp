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

(defpackage :cairo-demos
  (:use :common-lisp :cl-cairo2)
  #+sbcl
  (:use :sb-sys)
  (:export :image-data-demo1
		   :image-data-demo2))

(in-package :cairo-demos)

;;; helpers

(defvar *png-dir*		(merge-pathnames "Temp/png/"))
(defvar *demo-src-file*	"flowers")
(defvar *png-width*		256)
(defvar *png-height*	256)

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

(defmacro argb-to-argb32 (a r g b)
  #+(or (and sbcl (or x86 ppc)) (and ccl 32-bit-host little-endian-host))
  `(logior (ash ,a 24) (ash ,r 16) (ash ,g 8) ,b)
  #-(or (and sbcl (or x86 ppc)) (and ccl 32-bit-host little-endian-host))
  (error "no implemented"))

(defmacro write-argb32 (sap idx argb32)
  #+sbcl
  `(setf (sap-ref-32 ,sap ,idx) ,argb32)
  #+ccl
  `(setf (ccl:%get-unsigned-long ,sap ,idx) ,argb32)
  #-(or sbcl ccl)
  (error "not implemented"))

(defmacro write-argb (sap idx a r g b)
  `(write-argb32 ,sap ,idx (argb-to-argb32 ,a ,r ,g ,b)))

(defmacro argb-from-argb32 (argb32)
  #+(or (and sbcl (or x86 ppc)) (and ccl 32-bit-host little-endian-host))
  `(values (logand #xff (ash ,argb32 -24))
		   (logand #xff (ash ,argb32 -16))
		   (logand #xff (ash ,argb32 -8))
		   (logand #xff ,argb32))
  #-(or (and sbcl (or x86 ppc)) (and ccl 32-bit-host little-endian-host))
  (error "not implemented"))

(defmacro read-argb32 (sap idx)
  #+sbcl
  `(sap-ref-32 ,sap ,idx)
  #+ccl
  `(ccl:%get-unsigned-long ,sap ,idx)
  #-(or sbcl ccl)
  (error "not implemented"))

(defmacro read-argb (sap idx)
  `(argb-from-argb32 (read-argb32 ,sap ,idx)))

;;; demos

(defun image-data-demo1 (&key (a #xff) (r -1) (g -1) (b -1))
  "Create a rectangle png image in specified alpha, r, g, b color"
  (if (and (eq r -1) (eq g -1) (eq b -1))
	  (setf r (random 256) g (random 256) b (random 256))
	  (progn
		(when (eq r -1)
		  (setf r 0))
		(when (eq g -1)
		  (setf g 0))
		(when (eq b -1)
		  (setf b 0))))
  (let ((width 256) (height 256))
	(with-png-file ((png-pathname-string "demo1") :argb32 width height argbsf)
	  (let ((sfwidth	(image-surface-get-width	argbsf))
			(sfheight	(image-surface-get-height	argbsf))
			(sfbpr		(image-surface-get-stride	argbsf))
			(image-data	(image-surface-get-data		argbsf :pointer-only t)))
		;;
		(format t "surface width, height, bytes-per-row: ~a, ~a ~a ~%" sfwidth sfheight sfbpr)
		(format t "image-data type:~a ~%" (type-of image-data))
		;;
		(loop for h from 0 to (1- height) do
			 (loop for w from 0 to (1- width) do
				  (let ((idx (+ (* h sfbpr) (* w 4))))
					(write-argb image-data idx a r g b))))))))

(defun image-data-demo2 (&key (zoomw 2) (zoomh 2))
  "Expand a png image to the one with width and height zoom rates specified."
  (when (or (< zoomh 1) (< zoomw 1))
	(error "zoom value must be integer greater than or equals to 1"))
  (setf zoomw (floor zoomw))
  (setf zoomh (floor zoomh))
  ;;
  ;; get surface of source png image
  ;;
  (with-png-surface ((png-pathname-string *demo-src-file*) srcsf)
	(let ((srcwidth		(image-surface-get-width	srcsf))
		  (srcheight	(image-surface-get-height	srcsf))
		  (srcfmt		(image-surface-get-format	srcsf))
		  (srcbpr		(image-surface-get-stride	srcsf))
		  (src-data		(image-surface-get-data		srcsf :pointer-only t)))
	  (format t "source surface width, height, bpr, format: ~a, ~a, ~a ~a ~%" srcwidth srcheight srcbpr srcfmt)
	  (unless (or (eq srcfmt :argb32) (eq srcfmt :rgb24))
		(error "source image format isn't argb32 or rgb24:~a~%" srcfmt))
	  ;;
	  ;; write destination png image with specified zoom size
	  ;;
	  (let ((dstwidth	(* srcwidth zoomw))
			(dstheight	(* srcheight zoomh)))
		(with-png-file ((png-pathname-string "demo2") :argb32 dstwidth dstheight dstsf)
		  (let ((dstbpr		(image-surface-get-stride	dstsf))
				(dst-data	(image-surface-get-data		dstsf :pointer-only t))
				(bpp 4))
			(loop for h from 0 to (1- srcheight) do
				 (loop for w from 0 to (1- srcwidth) do
					  (let ((srcidx (+ (* h srcbpr) (* w bpp)))
							(dstidx (+ (* h dstbpr zoomh) (* w bpp zoomw))))
						(loop for zh from 0 to (1- zoomh) do
							 (loop for zw from 0 to (1- zoomw) do
								  (write-argb32 dst-data
												(+ dstidx (* zh dstbpr) (* bpp zw))
												(read-argb32 src-data srcidx)))))))))))))
