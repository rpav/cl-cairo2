(in-package :cl-cairo2)

 ;; toy interface
;;;;
;;;;  Notes
;;;;
;;;;  The text interface is still preliminary.  I have postponed
;;;;  writing it until I have some knowledge of what people want to
;;;;  use it for, for me, what is below suffices.
;;;;
;;;;  The long-term solution would be integration with Pango (to
;;;;  manage glyphs) and a CLOS-wrapped system for fonts.
;;;;
;;;;  Need to write:
;;;;
;;;;  set-font-matrix, get-font-matrix, set-font-options,
;;;;  get-font-options, set-font-face, get-font-face, set-scaled-font,
;;;;  get-scaled-font
;;;;
;;;;  cairo_show_glyphs, cairo_glyph_extents

(define-flexible (select-font-face pointer family slant weight)
  (cairo_select_font_face pointer 
			  family 
			  (lookup-enum slant table-font-slant)
			  (lookup-enum weight table-font-weight)))

(define-with-default-context set-font-size size)

(define-flexible (text-extents pointer text)
  (with-foreign-pointer (extents-pointer 
			 (foreign-type-size '(:struct cairo_text_extents_t)))
    (cairo_text_extents pointer text extents-pointer)
    (with-foreign-slots ((x_bearing y_bearing width height
				    x_advance y_advance)
			 extents-pointer (:struct cairo_text_extents_t))
      (values x_bearing y_bearing width height x_advance y_advance))))

(define-with-default-context-sync show-text text)

 ;; low level types and utilities

;;;
;;; The structures, text-extents-t and font-extnts-t, are opaque to the client.
;;; shorthand accessor functions are defined to reference the member variables.
;;;

(defmacro def-extents-t-shortname (prefix struct-name slot)
  (let ((shortname (intern (concatenate 'string (symbol-name prefix)
					"-" (symbol-name slot))))
	(longname (intern (concatenate 'string (symbol-name struct-name)
				       "-" (symbol-name slot)))))
    `(progn
       (defun ,shortname (extents-t)
         (,longname extents-t))
       (defun (setf ,shortname) (value extents-t)
         (setf (,longname extents-t) value)))))

(defmacro defstruct-extents-t (prefix &rest slots)
  (let ((struct-name (intern (concatenate 'string (symbol-name prefix)
					  "-EXTENTS-T")))
        (make-name (intern (concatenate 'string
                                        "MAKE-" (symbol-name prefix) "-EXTENTS-T"))))
	`(progn
           (declaim (inline ,make-name))
	   (defstruct ,struct-name ,@slots)
	   ,@(loop for slot in slots collect
		  `(def-extents-t-shortname ,prefix ,struct-name ,slot)))))

(defstruct-extents-t text x-bearing y-bearing width height x-advance y-advance)
(defstruct-extents-t font ascent descent height max-x-advance max-y-advance)

(defun text-extents-t-copy-out (pointer text-extents-t)
  "Copy the contents of a memory location to a text-extents-t object."
  (with-foreign-slots ((x_bearing y_bearing width height x_advance
				  y_advance) pointer cairo_text_extents_t)
    (setf (text-extents-t-x-bearing	text-extents-t) x_bearing
	  (text-extents-t-y-bearing	text-extents-t) y_bearing
	  (text-extents-t-width		text-extents-t) width
	  (text-extents-t-height	text-extents-t) height
	  (text-extents-t-x-advance	text-extents-t) x_advance
	  (text-extents-t-y-advance	text-extents-t) y_advance)))

(defun text-extents-t-copy-in (pointer text-extents-t)
  "Copy the contents of a memory location to a text-extents-t object."
  (with-foreign-slots ((x_bearing y_bearing width height x_advance
                                  y_advance) pointer cairo_text_extents_t)
    (setf x_bearing (text-extents-t-x-bearing text-extents-t)
          y_bearing (text-extents-t-y-bearing text-extents-t)
          width (text-extents-t-width text-extents-t)
          height (text-extents-t-height text-extents-t)
          x_advance (text-extents-t-x-advance text-extents-t)
          y_advance (text-extents-t-y-advance text-extents-t))))

(defmacro with-text-extents-t-out (pointer &body body)
  "Execute body with pointer pointing to an uninitialized location,
   then copy this to text extents and return the text extents."
  (let ((extents-name (gensym)))
    `(with-foreign-pointer (,pointer (foreign-type-size 'cairo_text_extents_t))
       (let ((,extents-name (make-text-extents-t)))
		 ,@body
		 (text-extents-t-copy-out ,pointer ,extents-name)
		 ,extents-name))))

(define-flexible (get-text-extents ctx-pointer utf8)
  (with-text-extents-t-out tet-pointer
	(cairo_text_extents ctx-pointer utf8 tet-pointer)))

(defun font-extents-t-copy-out (pointer font-extents-t)
  "Copy the contents of a memory location to a font-extents-t object."
  (with-foreign-slots ((ascent descent height max_x_advance
			       max_y_advance) pointer cairo_font_extents_t)
    (setf (font-extents-t-ascent        font-extents-t) ascent
          (font-extents-t-descent       font-extents-t) descent
          (font-extents-t-height        font-extents-t) height
          (font-extents-t-max-x-advance font-extents-t) max_x_advance
          (font-extents-t-max-y-advance font-extents-t) max_y_advance)))

(defun font-extents-t-copy-in (pointer font-extents-t)
  "Copy the contents of a font-extents-t object to a cairo_font_extents_t
pointer."
  (with-foreign-slots ((ascent descent height max_x_advance
			       max_y_advance) pointer cairo_font_extents_t)
    (setf ascent (font-extents-t-ascent font-extents-t)
          descent (font-extents-t-descent font-extents-t)
          height (font-extents-t-height font-extents-t)
          max_x_advance (font-extents-t-max-x-advance font-extents-t)
          max_y_advance (font-extents-t-max-y-advance font-extents-t))))

(defmacro with-font-extents-t-out (pointer &body body)
  "Execute body with pointer pointing to an uninitialized location,
   then copy this to text extents and return the text extents."
  (let ((extents-name (gensym)))
    `(with-foreign-pointer (,pointer (foreign-type-size 'cairo_font_extents_t))
       (let ((,extents-name (make-font-extents-t)))
		 ,@body
		 (font-extents-t-copy-out ,pointer ,extents-name)
		 ,extents-name))))

(define-flexible (get-font-extents ctx-pointer)
  (with-font-extents-t-out fet-pointer
	(cairo_font_extents ctx-pointer fet-pointer)))

;;;
;;; This is a bit of a massive hack to handle glyph arrays in a
;;; somewhat efficient manner (i.e., we don't allocate everytime
;;; we want to display, and copy lots of struct values to lots of
;;; foreign struct values).
;;;
(defstruct (glyph-array (:constructor %make-glyph-array))
  (count 0 :type integer)
  (filled 0 :type integer)
  (pointer))

(declaim (inline set-glyph))
(defun set-glyph (glyph-ptr index x y)
  (setf (mem-ref glyph-ptr :unsigned-long) index)
  (setf (mem-ref (inc-pointer glyph-ptr
                              #.(foreign-slot-offset 'cairo_glyph_t 'x))
                 :double)
        (coerce x 'double-float))
  (setf (mem-ref (inc-pointer glyph-ptr
                              #.(foreign-slot-offset 'cairo_glyph_t 'y))
                 :double)
        (coerce y 'double-float)))

(defun get-glyph (glyph-array n)
  (let ((glyph-ptr (inc-pointer (glyph-array-pointer glyph-array)
                                (* n #.(foreign-type-size 'cairo_glyph_t)))))
    (list
     (mem-ref glyph-ptr :unsigned-long)
     (mem-ref (inc-pointer glyph-ptr #.(foreign-slot-offset 'cairo_glyph_t 'x))
              :double)
     (mem-ref (inc-pointer glyph-ptr #.(foreign-slot-offset 'cairo_glyph_t 'y))
              :double))))

(defun make-glyph-array (count)
  (let* ((ptr (cffi:foreign-alloc 'cairo_glyph_t :count count))
         (array (%make-glyph-array :count count :pointer ptr)))
    (tg:finalize array (lambda () (foreign-free ptr)))
    array))

(defun glyph-array-add (glyph-array index x y)
  (when (>= (glyph-array-filled glyph-array)
            (glyph-array-count glyph-array))
    (error "Glyph array too small (length ~A)" (glyph-array-count glyph-array)))
  (let* ((next (glyph-array-filled glyph-array))
         (glyph (inc-pointer (glyph-array-pointer glyph-array)
                             (* next #.(cffi:foreign-type-size 'cairo_glyph_t)))))
    (set-glyph glyph index x y)
    (incf (glyph-array-filled glyph-array))
    (values)))

(defun glyph-array-set-glyph (glyph-array array-index glyph-index x y)
  (when (>= array-index (glyph-array-count glyph-array))
    (error "Glyph array too small (length ~A)" (glyph-array-count glyph-array)))
  (let* ((glyph (inc-pointer (glyph-array-pointer glyph-array)
                             (* array-index #.(cffi:foreign-type-size 'cairo_glyph_t)))))
    (set-glyph glyph glyph-index x y)
    (when (>= array-index (glyph-array-filled glyph-array))
      (setf (glyph-array-filled glyph-array) (1+ array-index)))
    (values)))

(defun glyph-array-reset-fill (glyph-array)
  (setf (glyph-array-filled glyph-array) 0))

 ;; low-level functions

(define-flexible (set-font-matrix ctx matrix)
  (with-trans-matrix-in matrix mptr
    (cairo_set_font_matrix ctx mptr)))

(define-flexible (get-font-matrix ctx)
  (with-trans-matrix-out mptr
    (cairo_get_font_matrix ctx mptr)))

(define-flexible (set-font-options ctx font-options)
  (with-alive-object (font-options ptr)
    (cairo_set_font_options ctx ptr)))

(define-flexible (get-font-options ctx)
  (let ((font-options (create-font-options)))
    (cairo_get_font_options ctx (get-pointer font-options))
    font-options))

(define-flexible (set-font-face ctx font-face)
  (if (null font-face)
      (cairo_set_font_face ctx (null-pointer))
      (with-alive-object (font-face ptr)
        (cairo_set_font_face ctx ptr))))

(define-flexible (get-font-face ctx)
  (let* ((ptr (with-checked-status (cairo_get_font_face ctx)))
         (font-face (make-instance 'font-face :pointer ptr)))
    (cairo_font_face_reference ptr)
    (tg:finalize font-face (lambda () (cairo_font_face_destroy ptr)))
    font-face))

(define-flexible (set-scaled-font ctx scaled-font)
  (with-alive-object (scaled-font ptr)
    (cairo_set_scaled_font ctx ptr)))

(define-flexible (get-scaled-font ctx)
  (let* ((ptr (with-checked-status (cairo_get_scaled_font ctx)))
         (font-face (make-instance 'scaled-font :pointer ptr)))
    (cairo_scaled_font_reference ptr)
    (tg:finalize font-face (lambda () (cairo_scaled_font_destroy ptr)))
    font-face))

(define-flexible (show-glyphs ctx glyph-array)
  (cairo_show_glyphs ctx
                     (glyph-array-pointer glyph-array)
                     (glyph-array-filled glyph-array)))

;;; cairo_show_text_glyphs - not implemented

(define-flexible (glyph-extents ctx glyph-array)
  (with-text-extents-t-out ptr
    (cairo_glyph_extents ctx
                         (glyph-array-pointer glyph-array)
                         (glyph-array-filled glyph-array)
                         ptr)))
