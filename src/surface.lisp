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

(define-foreign-type surface-type () ()
  (:actual-type :pointer)
  (:simple-parser surface))

(defmethod translate-to-foreign (surface (type surface-type))
  (get-pointer surface))

(defmethod expand-to-foreign (surface (type surface-type))
  `(get-pointer ,surface))

(defun create-surface-from-foreign
    (pointer &optional (pixel-based-p t) (assume-memory-p t))
  "Wrap a foreign pointer to a cairo surface in a CL-CAIRO2:SURFACE object. If
ASSUME-MEMORY-P is true, takes control of freeing the memory for the pointer when
no longer needed."
  (let ((surface
         (make-instance 'surface
                        :width (cairo_image_surface_get_width pointer)
                        :height (cairo_image_surface_get_height pointer)
                        :pointer pointer
                        :pixel-based-p pixel-based-p)))
    (when assume-memory-p
      (tg:finalize surface #'(lambda () (cairo_surface_destroy pointer))))
    surface))

(defmethod lowlevel-destroy ((surface surface))
  (cairo_surface_destroy (get-pointer surface)))

(defmethod lowlevel-status ((surface surface))
  (with-alive-object (surface pointer)
    (lookup-cairo-enum (cairo_surface_status (get-pointer surface)) table-status)))

(defmethod reference-count ((surface surface))
  (with-alive-object (surface pointer)
    (cairo_surface_get_reference_count pointer)))

(defun new-surface-with-check (pointer width height &optional (pixel-based-p nil) (needs-ref nil))
  "Check if the creation of new surface was successful, if so, return new class.
Optional NEEDS-REF parameter specifies the surface is owned by the foreign side
and needs to be referenced before use."
  (let ((surface (make-instance 'surface :width width :height height
				:pixel-based-p pixel-based-p)))
    (with-checked-status surface
      (when needs-ref (cairo_surface_reference pointer))
      (setf (slot-value surface 'pointer) pointer)
      ;; register finalizer
      ;; See CREATE-CONTEXT for why (lowlevel-destroy object) cannot be used here
      (tg:finalize surface #'(lambda () (cairo_surface_destroy pointer)))
      ;; return surface
      surface)))

(defun surface-flush (surface)
  (with-checked-status surface
    (cairo_surface_flush (get-pointer surface))))

(defun surface-finish (surface)
  (with-checked-status surface
    (cairo_surface_finish (get-pointer surface))))

(defun surface-mark-dirty (surface)
  (with-checked-status surface
    (cairo_surface_mark_dirty (get-pointer surface))))

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
       (namestring (merge-pathnames filename)) width height)
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

;;;
;;; Recording surface
;;;

(defun set-rect (rect x y w h)
  (let ((x0 x) (y0 y))
    (with-foreign-slots ((x y width height) rect (:struct cairo_rectangle_t))
      (setf x x0)
      (setf y y0)
      (setf width w)
      (setf height h))))

(defun create-recording-surface (content &optional x y width height)
  (flet ((create-surface (&optional rect)
           (new-surface-with-check
            (cairo_recording_surface_create (lookup-enum content table-content)
                                            (if rect rect (null-pointer)))
            width height)))
    (if x
        (with-foreign-object (rect '(:struct cairo_rectangle_t))
          (set-rect rect x y width height)
          (create-surface rect))
        (create-surface))))

;;;;
;;;;  image surface
;;;;

(defun create-image-surface (format width height)
  (new-surface-with-check
   (cairo_image_surface_create (lookup-enum format table-format)
			       width height)
   width height t))

(defun create-similar-image (other-surface format width height)
  "Create a new image surface suitable for fast blitting to OTHER-SURFACE
via cairo_surface_create_similar_image."
  (with-cairo-object (other-surface ptr)
   (new-surface-with-check
    (cairo_surface_create_similar_image ptr (lookup-enum format table-format)
                                        width height)
    width height t)))

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
    (:argb32 4)
    (:rgb24 4)
    (:a8 1)
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

(defmacro with-surface ((surface &optional surface-name) &body body)
  (let ((var-name (or surface-name '*surface*)))
    `(let ((,var-name ,surface))
       (unwind-protect
	    (progn ,@body)
       
	 (progn (surface-finish ,var-name)
		(destroy ,var-name))))))

(defmacro with-context-from-surface ((surface) &body body)
  (let ((context (gensym "context")))
    `(let ((,context (create-context ,surface)))
       (unwind-protect
	    (with-context (,context)
	      ,@body)	      
	 (destroy ,context)))))


(defmacro with-surface-and-context ((surface &optional surface-name) &body body)
  (let ((var-name (or surface-name '*surface*)))
    `(with-surface (,surface ,var-name)
       (with-context-from-surface (,var-name)
	 ,@body))))


;;;;
;;;;  PNG surfaces
;;;;

(defun image-surface-create-from-png (filename)
  (let* ((path (namestring (merge-pathnames filename)))
         (surface
           (new-surface-with-check (cairo_image_surface_create_from_png path)
                                   0 0)))
    (with-slots (width height) surface
      (setf width (image-surface-get-width surface)
	    height (image-surface-get-height surface))
      surface)))

(declaim (special *read-callback*))

(defvar *read-callback* nil
  "Stores callback functions to be called from
cairo_image_surface_create_from_png_stream.")

(cffi:defcallback read-function cairo_status_t
    ((closure :pointer)
     (data    (:pointer :unsigned-char))
     (length  :unsigned-int))
  (declare (ignore closure))
  (let ((length (convert-from-foreign length :unsigned-int))
	(read   (funcall *read-callback* length)))
    (dotimes (i length)
      (setf (cffi:mem-aref data :unsigned-char i) (aref read i))))
  :CAIRO_STATUS_SUCCESS)

(defun image-surface-create-from-png-callback (callback)
  "Construct a cairo image surface by repeatedly calling CALLBACK
retrieving one chunk of PNG data at a time. CALLBACK should take a
single argument which is the amount of data that to be retrieved."
  ;; This implementation is a bit hackish: We do not use the closure
  ;; pointer provided in the cairo api, but store the callback in the
  ;; special variable *read-callback*.
  (let* ((*read-callback* callback)
	 (surface	  (new-surface-with-check
			   (with-foreign-object (closure :pointer)
			     (cairo_image_surface_create_from_png_stream
			      (cffi:callback read-function) closure))
			   0 0)))
    (with-slots (width height) surface
      (setf width (image-surface-get-width surface)
	    height (image-surface-get-height surface))
     surface)))

(defun image-surface-create-from-png-stream (stream)
  "Construct a cairo image surface by reading PNG data from STREAM."
  (flet ((read-chunk (size)
	   (let ((buffer (make-array size :element-type 'unsigned-byte)))
	     (read-sequence buffer stream)
	     buffer)))
    (image-surface-create-from-png-stream #'read-chunk)))

(defun surface-write-to-png (surface filename)
  (with-cairo-object (surface pointer)
	(let ((status (cairo_surface_write_to_png
                       pointer (namestring (merge-pathnames filename)))))
	  (unless (eq (lookup-cairo-enum status table-status) :success)
		(warn "function returned with status ~a." status)))))
    
(defmacro with-png-surface ((png-file surface-name) &body body)
  `(let ((,surface-name (image-surface-create-from-png ,png-file)))
	 (unwind-protect (progn ,@body)
	   (destroy ,surface-name))))
