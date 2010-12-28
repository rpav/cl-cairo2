(in-package :cl-cairo2)
;;;;
;;;;  Notes
;;;;  - functions that write to/read from streams are not implemented



;;;;
;;;;  class cairo-object
;;;;

(defgeneric width (object)
  (:documentation "return the width of an object"))

(defgeneric height (object)
  (:documentation "return the height of an object"))

(defgeneric pixel-based-p (object)
  (:documentation "return t iff the object uses a pixel-based backend"))




(defclass cairo-object ()
  ((pointer :initarg :pointer :initform nil :reader get-pointer)))

;;;;
;;
;; the following two functions serve as wrappers for cairo's status and destroy functions.
;; they are needed by the general-purpose macros below and implemented by the surface,
;; context and pattern classes.
;;


(defgeneric lowlevel-status (object)
  (:documentation "calls the approriate cairo function for getting this object's status and looks it up"))

(defgeneric lowlevel-destroy (object)
  (:documentation "calls the approriate cairo function for destroying this object"))


(defmacro with-alive-object ((object pointer) &body body)
  "Execute body with pointer pointing to cairo object, if nil,
  signal error."
  (let ((pointer-name pointer))
    `(with-slots ((,pointer-name pointer)) ,object
       (if ,pointer-name
	   (progn ,@body)
	   (warn "surface is not alive")))))

(defmacro with-checked-status (object &body body)
  "Check status of cairo-object after executing body."
  (let ((status (gensym)))
    `(multiple-value-prog1 (progn ,@body)
       (let ((,status (lowlevel-status ,object)))
	 (unless (eq ,status :success)
	   (warn "function returned with status ~a." ,status))))))

(defmacro with-cairo-object ((object  pointer) &body body)
  "Execute body with pointer pointing to surface, and check status."
  `(with-alive-object (,object ,pointer)
     (with-checked-status ,object
       ,@body)))

(defmethod destroy ((object cairo-object))
  (with-alive-object (object pointer)
    (lowlevel-destroy object)
    (setf pointer nil))
  ;; deregister finalizer
  (tg:cancel-finalization object))


;;;;
;;;; class surface
;;;;
;;;;


(defclass surface (cairo-object)
  ((width :initarg :width :reader width)
   (height :initarg :height :reader height)
   (pixel-based-p :initarg :pixel-based-p :reader pixel-based-p)))

(defmethod lowlevel-destroy ((surface surface))
  (cairo_surface_destroy (get-pointer surface)))

(defmethod lowlevel-status ((surface surface))
  (with-alive-object (surface pointer)
    (lookup-cairo-enum (cairo_surface_status (get-pointer surface)) table-status)))


(defun new-surface-with-check (pointer width height &optional (pixel-based-p nil))
  "Check if the creation of new surface was successful, if so, return new class."
  (let ((surface (make-instance 'surface :width width :height height
				:pixel-based-p pixel-based-p)))
    (with-checked-status surface
      (setf (slot-value surface 'pointer) pointer)
      ;; register finalizer
      (tg:finalize surface #'(lambda () (lowlevel-destroy surface)))
      ;; return surface
      surface)))


;;;;
;;;; Macros to create surfaces (that are written into files) and
;;;; direct creation of contexts for these surfaces.
;;;;

(defmacro define-create-surface (type)
  "Define the function create-<type>-surface."
  `(defun ,(prepend-intern "create-" type :replace-dash nil :suffix "-surface")
      (filename width height)
    (new-surface-with-check
     (,(prepend-intern "cairo_" type :replace-dash nil
		       :suffix "_surface_create")
       filename width height)
     width height)))

;;;;
;;;; PDF surface
;;;;

(define-create-surface pdf)

;;;;
;;;; PostScript surface
;;;;

(define-create-surface ps)

;;;;
;;;; SVG surface
;;;;

(define-create-surface svg)

;;;;
;;;;  image surface
;;;;

(defun create-image-surface (format width height)
  (new-surface-with-check
   (cairo_image_surface_create (lookup-enum format table-format)
			       width height)
   width height t))

(defun create-image-surface-for-data (data format width height stride)
  (new-surface-with-check
   (cairo_image_surface_create_for_data data (lookup-enum format table-format)
										width height stride)
   width height t))

(defun create-image-surface-for-array (data)
  "Create a cairo image surface from DATA. The dimensions and color
format of the created surface are determined based on the shaped of
DATA:
+ WxH   -> BW
+ WxHx3 -> RGB
+ WxHx4 -> ARGB"
  ;; Make sure we can interpret DATA based on its shape.
  (check-type data (or (array t (* *))
		       (array t (* * 3))
		       (array t (* * 4))))

  (let* ((format (cond
		   ((typep data '(array t (* *)))
		    :a8)
		   ((typep data '(array t (* * 3)))
		    :rgb24)))
	 (epp    (cond
		   ((typep data '(array t (* *)))
		    1)
		   ((typep data '(array t (* * 3)))
		    3)))
	 (format (lookup-enum format table-format))
	 (height (array-dimension data 0))
	 (width  (array-dimension data 1))
	 (stride (cairo_format_stride_for_width format width))
	 (size   (* stride height))
	 (buffer (cffi:foreign-alloc :unsigned-char
				     :count size)))
    (dotimes (i height)
      (let ((row (make-array (* width epp)
			     :displaced-to           data
			     :displaced-index-offset (* i width epp)))
	    (offset (* stride i)))
	(dotimes (j (* width epp))
	  (setf (mem-aref buffer :unsigned-char offset) (aref row j))
	  (incf offset (if (and (= epp 3) (zerop (mod (1+ j) epp))) 2 1))))) ;; TODO slow
    (new-surface-with-check
     (cairo_image_surface_create_for_data
      buffer format width height stride)
     width height t)))

(defun get-bytes-per-pixel (format)
  (case format
    (format-argb32 4)
    (format-rgb24 3)
    (format-a8 1)
    (otherwise (error (format nil "unknown format: ~a" format))))) ;todo: how does format-a1 fit in here?

(defun image-surface-get-data (surface &key (pointer-only nil))
  "get the pointer referencing the image data directly. Then return it immediately when pointer-only is t.
Otherwise, return the copy of the image data along with the pointer."
  (with-cairo-object (surface pointer)
	(let ((data-pointer (cairo_image_surface_get_data pointer)))
	  #+sbcl
	  (when (sb-sys:sap= data-pointer (sb-sys:int-sap 0))
		(warn "null surface data pointer returned."))
	  (if pointer-only
		  data-pointer
		  (let* ((width (image-surface-get-width surface))
				 (height (image-surface-get-height surface))
				 (bytes-per-pixel (get-bytes-per-pixel (image-surface-get-format surface)))
				 (buffer (make-array (* width height bytes-per-pixel) :element-type '(unsigned-byte 8) :fill-pointer 0)))
			(loop for i from 0 below (* width height bytes-per-pixel) do
				 (vector-push-extend (cffi:mem-ref data-pointer :uint8 i) buffer))
			(values buffer data-pointer))))))

(defun image-surface-get-format (surface)
  (with-cairo-object (surface pointer)
    (lookup-cairo-enum (cairo_image_surface_get_format pointer) table-format)))

(defun image-surface-get-width (surface)
  (with-cairo-object (surface pointer)
    (cairo_image_surface_get_width pointer)))

(defun image-surface-get-height (surface)
  (with-cairo-object (surface pointer)
    (cairo_image_surface_get_height pointer)))

(defun image-surface-get-stride (surface)
  (with-cairo-object (surface pointer)
	(cairo_image_surface_get_stride pointer)))

;;;;
;;;;  PNG surfaces
;;;;

(defun image-surface-create-from-png (filename)
  (let ((surface
	 (new-surface-with-check (cairo_image_surface_create_from_png filename)
				 0 0)))
    (with-slots (width height) surface
      (setf width (image-surface-get-width surface)
	    height (image-surface-get-height surface))
      surface)))

(defun surface-write-to-png (surface filename)
  (with-cairo-object (surface pointer)
	(let ((status (cairo_surface_write_to_png pointer filename)))
	  (unless (eq (lookup-cairo-enum status table-status) :success)
		(warn "function returned with status ~a." status)))))

(defmacro with-png-surface ((png-file surface-name) &body body)
  `(let ((,surface-name (image-surface-create-from-png ,png-file)))
	 (unwind-protect (progn ,@body)
	   (destroy ,surface-name))))
