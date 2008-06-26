(in-package :cl-cairo2)

;;;;
;;;;  Notes
;;;;
;;;;  need to write:
;;;;    cairo-get-target
;;;;    push-group-with-content
;;;;    get-group-target
;;;;    set-source
;;;;    set-source-surface
;;;;    get-source
;;;;    mask
;;;;    mask-surface
;;;;
;;;;
;;;;  not sure anyone needs:
;;;;    get/set-user-data
;;;;    get-reference-count

;;;;
;;;; context class
;;;;

(defclass context (cairo-object) 
  ((width :initarg :width :reader get-width)
   (height :initarg :height :reader get-height)
   (pixel-based-p :initarg :pixel-based-p :reader pixel-based-p)))


(defmethod lowlevel-destroy ((context context))
  (cairo_destroy (get-pointer context)))

(defmethod lowlevel-status ((context context))
  (lookup-cairo-enum (cairo_status (get-pointer context)) table-status))

(defmethod print-object ((obj context) stream)
  "Print a context object."
  (print-unreadable-object (obj stream :type t)
    (with-slots (pointer width height pixel-based-p) obj
      (format stream "pointer: ~a, width: ~a, height: ~a, pixel-based-p: ~a"
	      pointer width height pixel-based-p))))

(defun create-context (surface)
  (with-cairo-object (surface pointer)
    (let ((context (make-instance 'context
				  :pointer (cairo_create pointer)
				  :width (get-width surface)
				  :height (get-height surface)
				  :pixel-based-p (pixel-based-p surface))))
      ;; register finalizer
     ; (let ((context-pointer (slot-value context 'pointer)))
      (tg:finalize context 
		     #'(lambda ()
			 (lowlevel-destroy context)))
      ;; return context
      context)))

; cairo-objects' destroy calling lowlevel-destroy should suffice. todo: check this
;(defmethod destroy ((object context))
;  (with-slots (pointer) object
;    (when pointer
;      (cairo_destroy pointer)
;      (setf pointer nil)))
;  ;; deregister finalizer
;  (tg:cancel-finalization object))

(defgeneric sync (object)
  (:documentation "Synchronize contents of the object with the
  physical device if needed."))
(defgeneric sync-lock (object)
  (:documentation "Suspend syncing (ie sync will have no effect) until
  sync-unlock is called.  Calls to sync-lock nest."))
(defgeneric sync-unlock (object)
  (:documentation "Undo a call to sync-lock."))
(defgeneric sync-reset (object)
  (:documentation "Undo all calls to sync, ie object will be
synced (if necessary) no matter how many times sync was called before."))

;; most contexts don't need syncing
(defmethod sync ((object context)))
(defmethod sync-lock ((object context)))
(defmethod sync-unlock ((object context)))
(defmethod sync-reset ((object context)))

(defmacro with-sync-lock ((context) &body body)
  "Lock sync for context for the duration of body.  Protected against
nonlocal exits."
  (once-only (context)
    `(progn
       (sync-lock ,context)
       (unwind-protect (progn ,@body)
	 (sync-unlock ,context)))))

;;;;
;;;; default context and convenience macros 
;;;;

(defvar *context* nil "default cairo context")

(defmacro with-png-file ((filename format width height) &body body)
  "Execute the body with context bound to a newly created png
   file, and close it after executing body."
  (let ((surface-name (gensym)))
    `(let* ((,surface-name (create-image-surface ,format ,width ,height))
	    (*context* (create-context ,surface-name)))
       (progn
	 ,@body
	 (surface-write-to-png ,surface-name ,filename)
	 (destroy ,surface-name)
	 (destroy *context*)))))

(defmacro with-context ((context pointer) &body body)
  "Execute body with pointer pointing to context, and check status."
  (let ((status (gensym))
	(pointer-name pointer))
    `(with-slots ((,pointer-name pointer)) ,context
       (if ,pointer-name
	   (multiple-value-prog1 (progn ,@body)
	     (let ((,status 
		    (lookup-cairo-enum (cairo_status ,pointer-name) table-status)))
	       (unless (eq ,status :success)
		 (warn "function returned with status ~a." ,status))))
	   (warn "context is not alive")))))

(defmacro define-with-default-context (name &rest args)
  "Define cairo function with *context* as its first argument and
  args as the rest, automatically mapping name to the appropriate
  cairo function."
  `(defun ,name (,@args &optional (context *context*))
     (with-context (context pointer)
       (,(prepend-intern "cairo_" name) pointer ,@args))))

(defmacro define-with-default-context-sync (name &rest args)
  "Define cairo function with *context* as its first argument and
  args as the rest, automatically mapping name to the appropriate
  cairo function.  sync will be called after the operation."
  `(defun ,name (,@args &optional (context *context*))
     (with-context (context pointer)
       (,(prepend-intern "cairo_" name) pointer ,@args))
     (sync context)))

(defmacro define-flexible ((name pointer &rest args) &body body)
  "Like define-with-default context, but with arbitrary body,
  pointer will point to the context."
  `(defun ,name (,@args &optional (context *context*))
     (with-context (context ,pointer)
       ,@body)))

(defmacro define-many-with-default-context (&body args)
  "Apply define-with-default context to a list.  Each item is
  itself a list, first element gives the function name, the rest
  the arguments."
  `(progn
     ,@(loop for arglist in args
	  collect `(define-with-default-context ,(car arglist) ,@(cdr arglist)))))

(defmacro define-get-set (property)
  "Define set-property and get-property functions."
  `(progn
     (define-with-default-context ,(prepend-intern "get-" property :replace-dash nil))
     (define-with-default-context ,(prepend-intern "set-" property :replace-dash nil)
	 ,property)))

(defmacro define-get-set-using-table (property)
  "Define set-property and get-property functions, where property
  is looked up in table-property for conversion into Cairo's enum
  constants."
  `(progn
     (define-flexible (,(prepend-intern "get-" property :replace-dash nil) pointer)
       (lookup-cairo-enum (,(prepend-intern "cairo_get_" property) pointer)
			  ,(prepend-intern "table-" property :replace-dash nil)))
     (define-flexible (,(prepend-intern "set-" property :replace-dash nil)
			pointer ,property)
       (,(prepend-intern "cairo_set_" property) pointer 
	 (lookup-enum ,property ,(prepend-intern "table-"
						 property :replace-dash nil))))))

;;;;
;;;; simple functions using context
;;;;

(define-many-with-default-context
  (save)
  (restore)
  (push-group)
  (pop-group)
  (pop-group-to-source)
  (set-source-rgb red green blue)
  (set-source-rgba red green blue alpha)
  (clip)
  (clip-preserve)
  (reset-clip)
  (copy-page)
  (show-page))

(define-with-default-context-sync fill-preserve)
(define-with-default-context-sync paint)
(define-with-default-context-sync paint-with-alpha alpha)
(define-with-default-context-sync stroke)
(define-with-default-context-sync stroke-preserve)

;;;; get-target

(defun get-target (context)
  "Obtain the target surface of a given context.  Width and height
will be nil, as cairo can't provide that in general."
  (new-surface-with-check (cairo_get_target (slot-value context 'pointer))
			  nil nil))

;;;;
;;;; set colors using the cl-colors library
;;;;

(defgeneric set-source-color (color &optional context))

(defmethod set-source-color ((color rgb) &optional (context *context*))
  (with-slots (red green blue) color
    (set-source-rgb red green blue context)))

(defmethod set-source-color ((color rgba) &optional (context *context*))
  (with-slots (red green blue alpha) color
    (set-source-rgba red green blue alpha context)))

(defmethod set-source-color ((color hsv) &optional (context *context*))
  (with-slots (red green blue) (hsv->rgb color)
    (set-source-rgb red green blue context)))

  
;;;; 
;;;; functions that get/set a property without any conversion
;;;; 

(define-get-set line-width)
(define-get-set miter-limit)
(define-get-set tolerance)

;;;; 
;;;; functions that get/set a property using a lookup table
;;;; 

(define-get-set-using-table antialias)
(define-get-set-using-table fill-rule)
(define-get-set-using-table line-cap)
(define-get-set-using-table line-join)
(define-get-set-using-table operator)

;; fill-path: it should simply be fill, but it is renamed so it does
;; not clash with cl-user:fill
(define-flexible (fill-path pointer)
  (cairo_fill pointer)
  (sync context))

(define-flexible (set-dash pointer offset dashes)
  (let ((num-dashes (length dashes)))
    (with-foreign-object (dashes-pointer :double num-dashes)
      (copy-double-vector-to-pointer (coerce dashes 'vector) dashes-pointer)
      (cairo_set_dash pointer dashes-pointer num-dashes offset))))

(define-flexible (get-dash pointer)
  "Return two values: dashes as a vector and the offset."
  (let ((num-dashes (cairo_get_dash_count pointer)))
    (with-foreign-objects ((dashes-pointer :double num-dashes)
			   (offset-pointer :double))
      (cairo_get_dash pointer dashes-pointer offset-pointer)
      (values (copy-pointer-to-double-vector num-dashes dashes-pointer)
	      (mem-ref offset-pointer :double)))))

(defmacro define-get-extents (name)
  "Define functions that query two coordinate pairs."
  `(define-flexible (,name pointer)
     (with-foreign-objects ((x1 :double) (y1 :double)
			    (x2 :double) (y2 :double))
       (,(prepend-intern "cairo_" name) pointer x1 y1 x2 y2)
       (values (mem-ref x1 :double) (mem-ref y1 :double)
	       (mem-ref x2 :double) (mem-ref y2 :double)))))

(define-get-extents clip-extents)
(define-get-extents fill-extents)

(define-flexible (in-fill pointer x y)
  (not (zerop (cairo_in_fill pointer x y))))

(define-flexible (in-stroke pointer x y)
  (not (zerop (cairo_in_stroke pointer x y))))

;;;;
;;;;  convenience functions for creating contexts directly
;;;;

(defmacro define-create-context (type)
  `(defun ,(prepend-intern "create-" type :replace-dash nil :suffix "-context")
       (filename width height)
     "Create a surface, then a context for a file, then
destroy (dereference) the surface.  The user only needs to
destroy the context when done."
     (let* ((surface (,(prepend-intern "create-"
				       type :replace-dash nil :suffix "-surface")
		       filename width height))
	    (context (create-context surface)))
       (destroy surface)
       context)))

(define-create-context ps)
(define-create-context pdf)
(define-create-context svg)
