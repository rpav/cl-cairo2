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
		 (tg:finalize pattern #'(lambda () (lowlevel-destroy pattern)))))))

(define-create-pattern :rgb red green blue)
(define-create-pattern :rgba red green blue alpha)
(define-create-pattern :linear start-x start-y end-x end-y)
(define-create-pattern :radial center0-x center0-y radius0 center1-x center1-y radius1)

(defun create-pattern-for-surface (image)
  (with-alive-object (image i-pointer)
	(let ((pattern (make-instance 'pattern)))
	  (with-checked-status pattern
		(setf (slot-value pattern 'pointer) (cairo_pattern_create_for_surface i-pointer))
		(tg:finalize pattern #'(lambda () (lowlevel-destroy pattern)))))))

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


;; the following functions are missing:
;get-rgba
;get-surface
;get-source
;get-color-stop-rgba
;get-color-stop-count
;get-linear-points
;get-radial-circles

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
  (if (member (pattern-get-type pattern) '(:linear :radial))
    (call-next-method)
    (error "cannot add a color stop to this pattern type")))

(defmethod pattern-add-color-stop ((pattern pattern) (offset number) (color cl-colors:rgb))
  (pattern-add-color-stop-rgb pattern offset
			      (red color)
			      (green color)
			      (blue color)))

(defmethod pattern-add-color-stop ((pattern pattern) (offset number) (color cl-colors:rgba))
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
