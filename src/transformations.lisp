(in-package :cl-cairo2)

;;;;  Notes
;;;;
;;;;  cairo-matrix-init is not defined, as we have a structure in lisp
;;;;    with an appropriate constructor
;;;;  
;;;;  cairo_identity_matrix is reset-trans-matrix
;;;;  
;;;;  functions that manipulate transformation matrices have
;;;;    trans-matrix instead of matrix in their name
;;;;
;;;;  cairo_matrix_transform_distance and cairo_matrix_transform_point
;;;;    are simply transform-distance and transform-point
;;;; 
;;;;  cairo_matrix_init is not defined, make-trans-matrix will give
;;;;    you an identity matrix

;;;;
;;;;  simple functions
;;;;

(define-many-with-default-context 
  (translate tx ty)
  (scale sx sy)
  (rotate angle))

(define-flexible (reset-trans-matrix pointer)
  (cairo_identity_matrix pointer))
  

;;;;
;;;;  transition matrix structure and helper functions/macros
;;;;

(defstruct trans-matrix
  (xx 1d0 :type double-float)
  (yx 0d0 :type double-float)
  (xy 0d0 :type double-float)
  (yy 1d0 :type double-float)
  (x0 0d0 :type double-float)
  (y0 0d0 :type double-float))

(defun trans-matrix-copy-in (pointer matrix)
  "Copy matrix to a memory location."
  (with-foreign-slots ((xx yx xy yy x0 y0) pointer (:STRUCT CAIRO_MATRIX_T))
    (setf xx (trans-matrix-xx matrix)
	  yx (trans-matrix-yx matrix)
	  xy (trans-matrix-xy matrix)
	  yy (trans-matrix-yy matrix)
	  x0 (trans-matrix-x0 matrix)
	  y0 (trans-matrix-y0 matrix))))

(defun trans-matrix-copy-out (pointer matrix)
  "Copy contents of a memory location to a transition matrix."
  (with-foreign-slots ((xx yx xy yy x0 y0) pointer (:STRUCT CAIRO_MATRIX_T))
    (setf (trans-matrix-xx matrix) xx
	  (trans-matrix-yx matrix) yx
	  (trans-matrix-xy matrix) xy
	  (trans-matrix-yy matrix) yy
	  (trans-matrix-x0 matrix) x0
	  (trans-matrix-y0 matrix) y0)))

(defmacro with-trans-matrix-in (matrix pointer &body body)
  "Execute body with pointer pointing to a memory location with matrix."
  `(with-foreign-pointer (,pointer (foreign-type-size '(:STRUCT CAIRO_MATRIX_T)))
     (trans-matrix-copy-in ,pointer ,matrix)
     ,@body))

(defmacro with-trans-matrix-out (pointer &body body)
  "Execute body with pointer pointing to an uninitialized location,
  then copy this to matrix and return the matrix."
  (let ((matrix-name (gensym)))
    `(with-foreign-pointer (,pointer (foreign-type-size '(:STRUCT CAIRO_MATRIX_T)))
       (let ((,matrix-name (make-trans-matrix)))
	 ,@body
	 (trans-matrix-copy-out ,pointer ,matrix-name)
	 ,matrix-name))))

(defmacro with-trans-matrix-in-out (matrix pointer &body body)
  (let ((matrix-name (gensym)))
    `(with-foreign-pointer (,pointer (foreign-type-size '(:STRUCT CAIRO_MATRIX_T)))
       (let ((,matrix-name (make-trans-matrix)))
	 (trans-matrix-copy-in ,pointer ,matrix)
	 ,@body
	 (trans-matrix-copy-out ,pointer ,matrix-name)
	 ,matrix-name))))

(defmacro with-x-y (&body body)
  "Creates temporary variables on the stack with pointers xp and yp,
  and copies x and y in/out before/after (respectively) the
  execution of body."
  `(with-foreign-objects ((xp :double) (yp :double))
     (setf (mem-ref xp :double) (coerce x 'double-float)
	   (mem-ref yp :double) (coerce y 'double-float))
     ,@body
     (values (mem-ref xp :double) (mem-ref yp :double))))

(defmacro define-with-x-y (name)
  "Defines a function that is called with context, x and y, and
  returns the latter two."
  `(define-flexible (,name pointer x y)
     (with-x-y 
       (,(prepend-intern "cairo_" name) pointer xp yp))))

;;;;
;;;;  transformation and conversion functions
;;;;

(define-flexible (transform pointer matrix)
  (with-trans-matrix-in matrix matrix-pointer
    (cairo_transform pointer matrix-pointer)))

(define-flexible (set-trans-matrix pointer matrix)
  (with-trans-matrix-in matrix matrix-pointer
    (cairo_set_matrix pointer matrix-pointer)))

(define-flexible (get-trans-matrix pointer)
  (with-trans-matrix-out matrix-pointer
    (cairo_get_matrix pointer matrix-pointer)))

(define-with-x-y user-to-device)
(define-with-x-y user-to-device-distance)
(define-with-x-y device-to-user)
(define-with-x-y device-to-user-distance)

;;;;
;;;;  transformations
;;;;

(defmacro define-matrix-init (name &rest args)
  "Define a matrix initializer function with args, which returns the
  new matrix."
  `(defun ,(prepend-intern "trans-matrix-init-" name :replace-dash nil) ,args
     (with-trans-matrix-out matrix-pointer
       (,(prepend-intern "cairo_matrix_init_" name) 
	 matrix-pointer
	 ,@args))))

(define-matrix-init translate tx ty)
(define-matrix-init scale sx sy)
(define-matrix-init rotate radians)

(defmacro define-matrix-transformation (name &rest args)
  "Define a matrix transformation function with matrix and args,
  which returns the new matrix."
  `(export
    (defun ,(prepend-intern "trans-matrix-" name :replace-dash nil) (matrix ,@args)
      (with-trans-matrix-in-out matrix matrix-pointer
	(,(prepend-intern "cairo_matrix_" name)
	  matrix-pointer
	  ,@args)))))

(define-matrix-transformation translate tx ty)
(define-matrix-transformation scale sx sy)
(define-matrix-transformation rotate radians)
(define-matrix-transformation invert)

(defun trans-matrix-multiply (a b)
  (with-trans-matrix-in a a-pointer
    (with-trans-matrix-in b b-pointer
      (with-trans-matrix-out result-pointer
	(cairo_matrix_multiply result-pointer
			       a-pointer
			       b-pointer)))))

(defun transform-distance (matrix x y)
  (with-trans-matrix-in matrix matrix-pointer
    (with-x-y 
      (cairo_matrix_transform_distance matrix-pointer xp yp))))

(defun transform-point (matrix x y)
  (with-trans-matrix-in matrix matrix-pointer
    (with-x-y 
      (cairo_matrix_transform_point matrix-pointer xp yp))))
