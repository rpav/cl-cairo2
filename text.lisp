(in-package :cl-cairo2)

;;;;
;;;;  Notes
;;;;
;;;;  The text interface is still preliminary.  I have postponed
;;;;  writing it until I have some knowledge of what people want to
;;;;  use it for, for me, what is below suffices.
;;;;
;;;;  The long-term solution would be integration with Pango (go
;;;;  manage glyphs) and a CLOS-wrapped system for fonts.
;;;;
;;;;  Need to write:
;;;;
;;;;  set-font-matrix, get-font-matrix, set-font-options,
;;;;  get-font-options, set-font-face, get-font-face, set-scaled-font,
;;;;  get-scaled-font
;;;;
;;;;  cairo_show_glyphs, cairo_font_extents, cairo_glyph_extents



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
