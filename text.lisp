(in-package :cl-cairo2)

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
			 (foreign-type-size 'cairo_text_extents_t))
    (cairo_text_extents pointer text extents-pointer)
    (with-foreign-slots ((x_bearing y_bearing width height
				    x_advance y_advance)
			 extents-pointer cairo_text_extents_t)
      (values x_bearing y_bearing width height x_advance y_advance))))

(define-with-default-context-sync show-text text)

;;;
;;; low level text and font support
;;;
;;; The structures, text-extents-t and font-extnts-t, are opaque to the client.
;;; shorthand accessor functions are defined to reference the member variables.
;;;

(defmacro def-extents-t-shortname (prefix struct-name slot)
  (let ((shortname (intern (concatenate 'string (symbol-name prefix)
					"-" (symbol-name slot))))
	(longname (intern (concatenate 'string (symbol-name struct-name)
				       "-" (symbol-name slot)))))
    `(defun ,shortname (extents-t)
       (,longname extents-t))))

(defmacro defstruct-extents-t (prefix &rest slots)
  (let ((struct-name (intern (concatenate 'string (symbol-name prefix)
					  "-EXTENTS-T"))))
	`(progn
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
    (setf (font-extents-t-ascent		font-extents-t) ascent
		  (font-extents-t-descent		font-extents-t) descent
		  (font-extents-t-height		font-extents-t) height
		  (font-extents-t-max-x-advance	font-extents-t) max_x_advance
		  (font-extents-t-max-y-advance	font-extents-t) max_y_advance)))

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
