(in-package :cl-cairo2)

(defclass pattern (cairo-object)
  ())

(defmethod lowlevel-destroy ((pattern pattern))
  (cairo_pattern_destroy (get-pointer pattern)))

(defmethod lowlevel-status ((pattern pattern))
  (lookup-cairo-enum (cairo_pattern_status (get-pointer pattern)) table-status))
  

(defmacro define-create-pattern (type &rest args)
  "make create-<type>-pattern defun"
  `(defun ,(prepend-intern "create-" type :suffix "-pattern") ,args
     (let ((pattern (make-instance 'pattern)))
       (with-checked-status pattern
		 (setf (slot-value pattern 'pointer) (,(prepend-intern "cairo_pattern_create_" type :replace-dash nil) ,@args))
                 (let ((ptr (slot-value pattern 'pointer)))
                   ;; See CREATE-CONTEXT for why (lowlevel-destroy object) cannot be used here
                   (tg:finalize pattern #'(lambda () (cairo_pattern_destroy ptr))))))))

(defun create-pattern-from-foreign (pointer &optional (assume-memory-p t))
  (let ((pattern (make-instance 'pattern :pointer pointer)))
    (when assume-memory-p
      (tg:finalize pattern (lambda () (cairo_pattern_destroy pointer))))
    pattern))

(define-create-pattern :rgb red green blue)
(define-create-pattern :rgba red green blue alpha)
(define-create-pattern :linear start-x start-y end-x end-y)
(define-create-pattern :radial center0-x center0-y radius0 center1-x center1-y radius1)

#+cairo-1.12
(define-create-pattern :mesh)

(defun create-pattern-for-surface (image)
  (with-alive-object (image i-pointer)
	(let ((pattern (make-instance 'pattern)))
	  (with-checked-status pattern
		(setf (slot-value pattern 'pointer) (cairo_pattern_create_for_surface i-pointer))
		(let ((ptr (slot-value pattern 'pointer)))
                   ;; See CREATE-CONTEXT for why (lowlevel-destroy object) cannot be used here
                   (tg:finalize pattern #'(lambda () (cairo_pattern_destroy ptr))))))))

(defmacro define-pattern-function-flexible (name (pattern-name pointer-name &rest args) &body body)
  "make a defun of the appropriate name with a wrapped body and the pattern's pointer bound to ,pointer-name"
  `(defun ,(prepend-intern "pattern-" name :replace-dash nil) ,(cons pattern-name args)
     (with-alive-object (,pattern-name ,pointer-name)
       (with-checked-status ,pattern-name
	 ,@body))))

(defmacro define-pattern-function (name &rest args)
  "define pattern function which don't require wrapping anything except the pattern itself"
  `(define-pattern-function-flexible ,name (pattern pointer ,@args)
    (,(prepend-intern "cairo_pattern_" name :replace-dash t) pointer ,@args)))

;(defmacro define-pattern-function (name &rest args)
;  (with-unique-names (pattern-name pointer-name)
;  `(defun ,(prepend-intern "pattern-" name) ,(cons pattern-name args)
;     (with-alive-object (,pattern-name ,pointer-name)
;       (with-checked-status ,pattern-name
;	 (,(prepend-intern "cairo-pattern-" name) ,pointer-name ,@args))))))


(define-pattern-function add-color-stop-rgb offset red green blue)
(define-pattern-function add-color-stop-rgba offset red green blue alpha)

(define-pattern-function-flexible set-matrix (pattern pattern-pointer matrix)
  (with-trans-matrix-in matrix matrix-pointer
    (cairo_pattern_set_matrix pattern-pointer matrix-pointer)))

(define-pattern-function-flexible get-matrix (pattern pattern-pointer)
  (with-trans-matrix-out matrix-pointer
    (cairo_pattern_get_matrix pattern-pointer matrix-pointer)))

;maybe extend the define-get-set-using-table macro to support the cairo_pattern_set_... naming scheme?

(define-pattern-function-flexible get-type (pattern pattern-pointer)
  (lookup-cairo-enum (cairo_pattern_get_type pattern-pointer) table-pattern-type))

(define-pattern-function-flexible get-extend (pattern pattern-pointer)
  (lookup-cairo-enum (cairo_pattern_get_extend pattern-pointer) table-extend))

(define-pattern-function-flexible set-extend (pattern pattern-pointer extend)
  (cairo_pattern_set_extend pattern-pointer (lookup-enum extend table-extend)))


(define-pattern-function-flexible get-filter (pattern pattern-pointer)
  (lookup-cairo-enum (cairo_pattern_get_extend pattern-pointer) table-filter))

(define-pattern-function-flexible set-filter (pattern pattern-pointer filter)
  (cairo_pattern_set_filter pattern-pointer (lookup-enum filter table-filter)))

#+cairo-1.12
(progn
  (define-pattern-function-flexible mesh-begin-patch (pattern pattern-pointer)
    (cairo_mesh_pattern_begin_patch pattern-pointer))

  (define-pattern-function-flexible mesh-end-patch (pattern pattern-pointer)
    (cairo_mesh_pattern_end_patch pattern-pointer))

  (define-pattern-function-flexible mesh-move-to (pattern pattern-pointer x y)
    (cairo_mesh_pattern_move_to pattern-pointer x y))

  (define-pattern-function-flexible mesh-line-to (pattern pattern-pointer x y)
    (cairo_mesh_pattern_line_to pattern-pointer x y))

  (define-pattern-function-flexible mesh-curve-to (pattern pattern-pointer x1 y1 x2 y2 x3 y3)
    (cairo_mesh_pattern_curve_to pattern-pointer x1 y1 x2 y2 x3 y3))

  (define-pattern-function-flexible mesh-set-control-point (pattern pattern-pointer point-num x y)
    (cairo_mesh_pattern_set_control_point pattern-pointer point-num x y))

  (define-pattern-function-flexible mesh-set-corner-color-rgb (pattern pattern-pointer corner-num r g b)
    (cairo_mesh_pattern_set_corner_color_rgb pattern-pointer corner-num r g b))

  (define-pattern-function-flexible mesh-set-corner-color-rgba (pattern pattern-pointer corner-num r g b a)
    (cairo_mesh_pattern_set_corner_color_rgba pattern-pointer corner-num r g b a))

  (define-pattern-function-flexible mesh-get-patch-count (pattern pattern-pointer)
    (with-foreign-object (count :double)
      (cairo_mesh_pattern_get_patch_count pattern-pointer count)
      (mem-ref count :double)))

  ;; FIXME: mesh-get-path, needs paths implemented

  (define-pattern-function-flexible mesh-get-control-point (pattern pattern-pointer patch-num point-num)
    (with-foreign-objects ((x :double) (y :double))
      (cairo_mesh_pattern_get_control_point pattern-pointer patch-num point-num x y)
      (values (mem-ref x :double)
              (mem-ref y :double))))

  (define-pattern-function-flexible mesh-get-corner-rgba (pattern pattern-pointer patch-num corner-num)
    (with-foreign-objects ((r :double) (g :double) (b :double) (a :double))
      (cairo_mesh_pattern_get_corner_color_rgba pattern-pointer patch-num corner-num r g b a)
      (values (mem-ref r :double)
              (mem-ref g :double)
              (mem-ref b :double)
              (mem-ref a :double)))))

(define-pattern-function-flexible get-rgba (pattern pattern-pointer)
  (with-foreign-objects ((r :double) (g :double) (b :double) (a :double))
    (cairo_pattern_get_rgba pattern-pointer r g b a)
    (values (mem-ref r :double)
            (mem-ref g :double)
            (mem-ref b :double)
            (mem-ref a :double))))

(define-pattern-function-flexible get-surface (pattern pattern-pointer)
  (with-foreign-object (surf :pointer)
    (cairo_pattern_get_surface pattern-pointer surf)
    (let ((surface-pointer (mem-ref surf :pointer)))
      (cairo_surface_reference surface-pointer)
      (create-surface-from-foreign surface-pointer))))

(define-flexible (get-source ctx)
  (let ((pattern-pointer (cairo_get_source ctx)))
    (cairo_pattern_reference pattern-pointer)
    (create-pattern-from-foreign pattern-pointer)))

(define-pattern-function-flexible get-color-stop-rgba (pattern pattern-pointer index)
  (with-foreign-objects ((offset :double)
                         (r :double) (g :double) (b :double) (a :double))
    (cairo_pattern_get_color_stop_rgba pattern-pointer index offset r g b a)
    (values (mem-ref offset :double)
            (mem-ref r :double)
            (mem-ref g :double)
            (mem-ref b :double)
            (mem-ref a :double))))

(define-pattern-function-flexible get-color-stop-count (pattern pattern-pointer)
  (with-foreign-object (count :int)
    (cairo_pattern_get_color_stop_count pattern-pointer count)
    (mem-ref count :int)))

(defun pattern-get-color-stops (pattern)
  (loop for i from 0 below (pattern-get-color-stop-count pattern)
        collect (list* i (multiple-value-list (pattern-get-color-stop-rgba pattern i)))))

(define-pattern-function-flexible get-linear-points (pattern pattern-pointer)
  (with-foreign-objects ((x0 :double) (y0 :double)
                         (x1 :double) (y1 :double))
    (cairo_pattern_get_linear_points pattern-pointer x0 y0 x1 y1)
    (values (mem-ref x0 :double)
            (mem-ref y0 :double)
            (mem-ref x1 :double)
            (mem-ref y1 :double))))

(define-pattern-function-flexible get-radial-circles (pattern pattern-pointer)
  (with-foreign-objects ((x0 :double) (y0 :double) (r0 :double)
                         (x1 :double) (y1 :double) (r1 :double))
    (cairo_pattern_get_radial_circles pattern-pointer x0 y0 r0 x1 y1 r1)
    (values (mem-ref x0 :double)
            (mem-ref y0 :double)
            (mem-ref r0 :double)
            (mem-ref x1 :double)
            (mem-ref y1 :double)
            (mem-ref r1 :double))))

(define-flexible (set-source context-pointer pattern)
  ;set a context's source to pattern
  (with-alive-object (pattern pattern-pointer)
    (with-checked-status pattern
      (cairo_set_source context-pointer pattern-pointer))))

(define-flexible (mask context-pointer pattern)
  (with-alive-object (pattern pattern-pointer)
	(with-checked-status pattern
	  (cairo_mask context-pointer pattern-pointer))))

;;;;
;;;; convenience methods and macros for use with cl-colors:
;;;;
;;;;

(defgeneric create-color-pattern (color)
  (:documentation "create a rgb or rgba pattern from the supplied color"))

(defmethod create-color-pattern ((color rgb))
  (create-rgb-pattern (red color) (green color) (blue color)))

(defmethod create-color-pattern ((color rgba))
  (create-rgba-pattern (red color) (green color) (blue color) (alpha color)))


(defgeneric pattern-add-color-stop (pattern offset color) ;todo: remove leading "pattern-"?
  (:documentation "add a color stop to the pattern. color must be of class rgb, rgba or a list (r g b) or (r g b a)"))

(defmethod pattern-add-color-stop :around ((pattern pattern) offset color)
  (declare (ignore offset color))
  (if (member (pattern-get-type pattern) '(:linear :radial))
    (call-next-method)
    (error "cannot add a color stop to this pattern type")))

(defmethod pattern-add-color-stop ((pattern pattern) (offset number) (color cl-colors:rgb))
  (pattern-add-color-stop-rgb pattern offset
			      (red color)
			      (green color)
			      (blue color)))

(defmethod pattern-add-color-stop ((pattern pattern) (offset number) (color rgba))
  (pattern-add-color-stop-rgba pattern offset
			      (red color)
			      (green color)
			      (blue color)
			      (alpha color)))

(defmethod pattern-add-color-stop ((pattern pattern) (offset number) (color cons))
  (unless (every #'numberp color)
    (error "invalid color given: not a number"))
  (apply (case (length color)
           (3 #'pattern-add-color-stop-rgb)
	   (4 #'pattern-add-color-stop-rgba)
	   (otherwise (error "invalid color given: list is not of length 3 or 4")))
	 pattern offset color))

(defmacro make-with-pattern (type &rest args)
  "makes a macro that creates and binds a <type> pattern to pattern-name, adds color stops to the pattern
   (calling each element of color-stops with pattern-add-color-stop) before evaluating a body and destroying
   the pattern."
  `(defmacro ,(prepend-intern "with-" type :suffix "-pattern") (pattern-name ,args color-stops &body body)
    (with-unique-names (offset-name color-name)
    `(let ((,pattern-name ,(list ',(prepend-intern "create-" type :suffix "-pattern") ,@args)))
       (loop for (,offset-name ,color-name) in ,color-stops
	     do (format t "~a~%" (class-of ,color-name))
	     do (pattern-add-color-stop ,pattern-name ,offset-name ,color-name))
       (prog1
	   (progn ,@body)
           (destroy ,pattern-name))))))

(make-with-pattern :radial center0-x center0-y radius0 center1-x center1-y radius1)
(make-with-pattern :linear start-x start-y end-x end-y)

(defun pattern-forms-p (pflist)
  "pattern-forms := (pattern-form+)
   pattern-form  := (pattern-name (create-xxxx-pattern args))"
  (if (consp pflist)
	  (dolist (pf pflist t)
		(if (and (consp pf) (eql (length pf) 2))
			(if (and (atom (car pf)) (consp (cadr pf)))
				t
				(error "invalid create-xxx-pattern form:~A" pf))
			(error "invalid pattern-form:~A" pf)))
	  (error "invalid pattern-forms:~A" pflist)))

(defmacro with-patterns (pattern-forms &body body)
  "create patterns from pattern-forms, each has its name as specified in the corresponding pattern-form,
and then execute body in which the patterns can be referenced using the names."
  (when (pattern-forms-p pattern-forms)
	  `(let ,(loop for f in pattern-forms collect `(,(car f) ,(cadr f)))
		 (unwind-protect (progn ,@body)
		   (progn
			 ,@(loop for f in pattern-forms collect
				`(destroy ,(car f))))))))
