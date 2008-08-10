
(in-package :cl-cairo2)

;; typedefs: we don't want to create all of them automatically,
;; because typedefs for structures confuse with-foreign-slots
;; the ones we don't want are commented out
(cffi:defctype cairo_bool_t :int)
(cffi:defctype cairo_t :pointer)
(cffi:defctype cairo_surface_t :pointer)
;; (cffi:defctype cairo_matrix_t :pointer)
(cffi:defctype cairo_pattern_t :pointer)
(cffi:defctype cairo_destroy_func_t :pointer)
(cffi:defctype cairo_user_data_key_t :pointer)
(cffi:defctype cairo_write_func_t :pointer)
(cffi:defctype cairo_read_func_t :pointer)
;; (cffi:defctype cairo_rectangle_t :pointer)
(cffi:defctype cairo_rectangle_list_t :pointer)
(cffi:defctype cairo_scaled_font_t :pointer)
(cffi:defctype cairo_font_face_t :pointer)
(cffi:defctype cairo_font_options_t :pointer)
(cffi:defctype cairo_path_data_t :pointer)
(cffi:defctype cairo_path_t :pointer)



;;;SWIG wrapper code starts here

(cl:defmacro defanonenum (&body enums)
   "Converts anonymous enums to defconstants."
  `(cl:progn ,@(cl:loop for value in enums
                        for index = 0 then (cl:1+ index)
                        when (cl:listp value) do (cl:setf index (cl:second value)
                                                          value (cl:first value))
                        collect `(cl:defconstant ,value ,index))))

(cl:eval-when (:compile-toplevel :load-toplevel)
  (cl:unless (cl:fboundp 'swig-lispify)
    (cl:defun swig-lispify (name flag cl:&optional (package cl:*package*))
      (cl:labels ((helper (lst last rest cl:&aux (c (cl:car lst)))
                    (cl:cond
                      ((cl:null lst)
                       rest)
                      ((cl:upper-case-p c)
                       (helper (cl:cdr lst) 'upper
                               (cl:case last
                                 ((lower digit) (cl:list* c #\- rest))
                                 (cl:t (cl:cons c rest)))))
                      ((cl:lower-case-p c)
                       (helper (cl:cdr lst) 'lower (cl:cons (cl:char-upcase c) rest)))
                      ((cl:digit-char-p c)
                       (helper (cl:cdr lst) 'digit 
                               (cl:case last
                                 ((upper lower) (cl:list* c #\- rest))
                                 (cl:t (cl:cons c rest)))))
                      ((cl:char-equal c #\_)
                       (helper (cl:cdr lst) '_ (cl:cons #\- rest)))
                      (cl:t
                       (cl:error "Invalid character: ~A" c)))))
        (cl:let ((fix (cl:case flag
                        ((constant enumvalue) "+")
                        (variable "*")
                        (cl:t ""))))
          (cl:intern
           (cl:concatenate
            'cl:string
            fix
            (cl:nreverse (helper (cl:concatenate 'cl:list name) cl:nil cl:nil))
            fix)
           package))))))

;;;SWIG wrapper code ends here


(cl:defconstant CAIRO_VERSION_MAJOR 1)

(cl:defconstant CAIRO_VERSION_MINOR 6)

(cl:defconstant CAIRO_VERSION_MICRO 4)

(cl:defconstant CAIRO_HAS_SVG_SURFACE 1)

(cl:defconstant CAIRO_HAS_PDF_SURFACE 1)

(cl:defconstant CAIRO_HAS_PS_SURFACE 1)

(cl:defconstant CAIRO_HAS_FT_FONT 1)

(cl:defconstant CAIRO_HAS_PNG_FUNCTIONS 1)

(cl:defconstant CAIRO_HAS_XCB_SURFACE 1)

(cl:defconstant CAIRO_HAS_XLIB_XRENDER_SURFACE 1)

(cl:defconstant CAIRO_HAS_XLIB_SURFACE 1)

(cl:defconstant CAIRO_FORMAT_RGB16_565 4)

(cffi:defcfun ("cairo_version" cairo_version) :int)

(cffi:defcfun ("cairo_version_string" cairo_version_string) :string)

(cffi:defcstruct cairo_matrix_t
	(xx my-double)
	(yx my-double)
	(xy my-double)
	(yy my-double)
	(x0 my-double)
	(y0 my-double))

(cffi:defcstruct cairo_user_data_key_t
	(unused :int))

(cffi:defcenum cairo_status_t
	(:CAIRO_STATUS_SUCCESS 0)
	:CAIRO_STATUS_NO_MEMORY
	:CAIRO_STATUS_INVALID_RESTORE
	:CAIRO_STATUS_INVALID_POP_GROUP
	:CAIRO_STATUS_NO_CURRENT_POINT
	:CAIRO_STATUS_INVALID_MATRIX
	:CAIRO_STATUS_INVALID_STATUS
	:CAIRO_STATUS_NULL_POINTER
	:CAIRO_STATUS_INVALID_STRING
	:CAIRO_STATUS_INVALID_PATH_DATA
	:CAIRO_STATUS_READ_ERROR
	:CAIRO_STATUS_WRITE_ERROR
	:CAIRO_STATUS_SURFACE_FINISHED
	:CAIRO_STATUS_SURFACE_TYPE_MISMATCH
	:CAIRO_STATUS_PATTERN_TYPE_MISMATCH
	:CAIRO_STATUS_INVALID_CONTENT
	:CAIRO_STATUS_INVALID_FORMAT
	:CAIRO_STATUS_INVALID_VISUAL
	:CAIRO_STATUS_FILE_NOT_FOUND
	:CAIRO_STATUS_INVALID_DASH
	:CAIRO_STATUS_INVALID_DSC_COMMENT
	:CAIRO_STATUS_INVALID_INDEX
	:CAIRO_STATUS_CLIP_NOT_REPRESENTABLE
	:CAIRO_STATUS_TEMP_FILE_ERROR
	:CAIRO_STATUS_INVALID_STRIDE)

(cffi:defcenum cairo_content_t
	(:CAIRO_CONTENT_COLOR #x1000)
	(:CAIRO_CONTENT_ALPHA #x2000)
	(:CAIRO_CONTENT_COLOR_ALPHA #x3000))

(cffi:defcfun ("cairo_create" cairo_create) :pointer
  (target :pointer))

(cffi:defcfun ("cairo_reference" cairo_reference) :pointer
  (cr :pointer))

(cffi:defcfun ("cairo_destroy" cairo_destroy) :void
  (cr :pointer))

(cffi:defcfun ("cairo_get_reference_count" cairo_get_reference_count) :unsigned-int
  (cr :pointer))

(cffi:defcfun ("cairo_get_user_data" cairo_get_user_data) :pointer
  (cr :pointer)
  (key :pointer))

(cffi:defcfun ("cairo_set_user_data" cairo_set_user_data) cairo_status_t
  (cr :pointer)
  (key :pointer)
  (user_data :pointer)
  (destroy :pointer))

(cffi:defcfun ("cairo_save" cairo_save) :void
  (cr :pointer))

(cffi:defcfun ("cairo_restore" cairo_restore) :void
  (cr :pointer))

(cffi:defcfun ("cairo_push_group" cairo_push_group) :void
  (cr :pointer))

(cffi:defcfun ("cairo_push_group_with_content" cairo_push_group_with_content) :void
  (cr :pointer)
  (content cairo_content_t))

(cffi:defcfun ("cairo_pop_group" cairo_pop_group) :pointer
  (cr :pointer))

(cffi:defcfun ("cairo_pop_group_to_source" cairo_pop_group_to_source) :void
  (cr :pointer))

(cffi:defcenum cairo_operator_t
	:CAIRO_OPERATOR_CLEAR
	:CAIRO_OPERATOR_SOURCE
	:CAIRO_OPERATOR_OVER
	:CAIRO_OPERATOR_IN
	:CAIRO_OPERATOR_OUT
	:CAIRO_OPERATOR_ATOP
	:CAIRO_OPERATOR_DEST
	:CAIRO_OPERATOR_DEST_OVER
	:CAIRO_OPERATOR_DEST_IN
	:CAIRO_OPERATOR_DEST_OUT
	:CAIRO_OPERATOR_DEST_ATOP
	:CAIRO_OPERATOR_XOR
	:CAIRO_OPERATOR_ADD
	:CAIRO_OPERATOR_SATURATE)

(cffi:defcfun ("cairo_set_operator" cairo_set_operator) :void
  (cr :pointer)
  (op cairo_operator_t))

(cffi:defcfun ("cairo_set_source" cairo_set_source) :void
  (cr :pointer)
  (source :pointer))

(cffi:defcfun ("cairo_set_source_rgb" cairo_set_source_rgb) :void
  (cr :pointer)
  (red my-double)
  (green my-double)
  (blue my-double))

(cffi:defcfun ("cairo_set_source_rgba" cairo_set_source_rgba) :void
  (cr :pointer)
  (red my-double)
  (green my-double)
  (blue my-double)
  (alpha my-double))

(cffi:defcfun ("cairo_set_source_surface" cairo_set_source_surface) :void
  (cr :pointer)
  (surface :pointer)
  (x my-double)
  (y my-double))

(cffi:defcfun ("cairo_set_tolerance" cairo_set_tolerance) :void
  (cr :pointer)
  (tolerance my-double))

(cffi:defcenum cairo_antialias_t
	:CAIRO_ANTIALIAS_DEFAULT
	:CAIRO_ANTIALIAS_NONE
	:CAIRO_ANTIALIAS_GRAY
	:CAIRO_ANTIALIAS_SUBPIXEL)

(cffi:defcfun ("cairo_set_antialias" cairo_set_antialias) :void
  (cr :pointer)
  (antialias cairo_antialias_t))

(cffi:defcenum cairo_fill_rule_t
	:CAIRO_FILL_RULE_WINDING
	:CAIRO_FILL_RULE_EVEN_ODD)

(cffi:defcfun ("cairo_set_fill_rule" cairo_set_fill_rule) :void
  (cr :pointer)
  (fill_rule cairo_fill_rule_t))

(cffi:defcfun ("cairo_set_line_width" cairo_set_line_width) :void
  (cr :pointer)
  (width my-double))

(cffi:defcenum cairo_line_cap_t
	:CAIRO_LINE_CAP_BUTT
	:CAIRO_LINE_CAP_ROUND
	:CAIRO_LINE_CAP_SQUARE)

(cffi:defcfun ("cairo_set_line_cap" cairo_set_line_cap) :void
  (cr :pointer)
  (line_cap cairo_line_cap_t))

(cffi:defcenum cairo_line_join_t
	:CAIRO_LINE_JOIN_MITER
	:CAIRO_LINE_JOIN_ROUND
	:CAIRO_LINE_JOIN_BEVEL)

(cffi:defcfun ("cairo_set_line_join" cairo_set_line_join) :void
  (cr :pointer)
  (line_join cairo_line_join_t))

(cffi:defcfun ("cairo_set_dash" cairo_set_dash) :void
  (cr :pointer)
  (dashes :pointer)
  (num_dashes :int)
  (offset my-double))

(cffi:defcfun ("cairo_set_miter_limit" cairo_set_miter_limit) :void
  (cr :pointer)
  (limit my-double))

(cffi:defcfun ("cairo_translate" cairo_translate) :void
  (cr :pointer)
  (tx my-double)
  (ty my-double))

(cffi:defcfun ("cairo_scale" cairo_scale) :void
  (cr :pointer)
  (sx my-double)
  (sy my-double))

(cffi:defcfun ("cairo_rotate" cairo_rotate) :void
  (cr :pointer)
  (angle my-double))

(cffi:defcfun ("cairo_transform" cairo_transform) :void
  (cr :pointer)
  (matrix :pointer))

(cffi:defcfun ("cairo_set_matrix" cairo_set_matrix) :void
  (cr :pointer)
  (matrix :pointer))

(cffi:defcfun ("cairo_identity_matrix" cairo_identity_matrix) :void
  (cr :pointer))

(cffi:defcfun ("cairo_user_to_device" cairo_user_to_device) :void
  (cr :pointer)
  (x :pointer)
  (y :pointer))

(cffi:defcfun ("cairo_user_to_device_distance" cairo_user_to_device_distance) :void
  (cr :pointer)
  (dx :pointer)
  (dy :pointer))

(cffi:defcfun ("cairo_device_to_user" cairo_device_to_user) :void
  (cr :pointer)
  (x :pointer)
  (y :pointer))

(cffi:defcfun ("cairo_device_to_user_distance" cairo_device_to_user_distance) :void
  (cr :pointer)
  (dx :pointer)
  (dy :pointer))

(cffi:defcfun ("cairo_new_path" cairo_new_path) :void
  (cr :pointer))

(cffi:defcfun ("cairo_move_to" cairo_move_to) :void
  (cr :pointer)
  (x my-double)
  (y my-double))

(cffi:defcfun ("cairo_new_sub_path" cairo_new_sub_path) :void
  (cr :pointer))

(cffi:defcfun ("cairo_line_to" cairo_line_to) :void
  (cr :pointer)
  (x my-double)
  (y my-double))

(cffi:defcfun ("cairo_curve_to" cairo_curve_to) :void
  (cr :pointer)
  (x1 my-double)
  (y1 my-double)
  (x2 my-double)
  (y2 my-double)
  (x3 my-double)
  (y3 my-double))

(cffi:defcfun ("cairo_arc" cairo_arc) :void
  (cr :pointer)
  (xc my-double)
  (yc my-double)
  (radius my-double)
  (angle1 my-double)
  (angle2 my-double))

(cffi:defcfun ("cairo_arc_negative" cairo_arc_negative) :void
  (cr :pointer)
  (xc my-double)
  (yc my-double)
  (radius my-double)
  (angle1 my-double)
  (angle2 my-double))

(cffi:defcfun ("cairo_rel_move_to" cairo_rel_move_to) :void
  (cr :pointer)
  (dx my-double)
  (dy my-double))

(cffi:defcfun ("cairo_rel_line_to" cairo_rel_line_to) :void
  (cr :pointer)
  (dx my-double)
  (dy my-double))

(cffi:defcfun ("cairo_rel_curve_to" cairo_rel_curve_to) :void
  (cr :pointer)
  (dx1 my-double)
  (dy1 my-double)
  (dx2 my-double)
  (dy2 my-double)
  (dx3 my-double)
  (dy3 my-double))

(cffi:defcfun ("cairo_rectangle" cairo_rectangle) :void
  (cr :pointer)
  (x my-double)
  (y my-double)
  (width my-double)
  (height my-double))

(cffi:defcfun ("cairo_close_path" cairo_close_path) :void
  (cr :pointer))

(cffi:defcfun ("cairo_path_extents" cairo_path_extents) :void
  (cr :pointer)
  (x1 :pointer)
  (y1 :pointer)
  (x2 :pointer)
  (y2 :pointer))

(cffi:defcfun ("cairo_paint" cairo_paint) :void
  (cr :pointer))

(cffi:defcfun ("cairo_paint_with_alpha" cairo_paint_with_alpha) :void
  (cr :pointer)
  (alpha my-double))

(cffi:defcfun ("cairo_mask" cairo_mask) :void
  (cr :pointer)
  (pattern :pointer))

(cffi:defcfun ("cairo_mask_surface" cairo_mask_surface) :void
  (cr :pointer)
  (surface :pointer)
  (surface_x my-double)
  (surface_y my-double))

(cffi:defcfun ("cairo_stroke" cairo_stroke) :void
  (cr :pointer))

(cffi:defcfun ("cairo_stroke_preserve" cairo_stroke_preserve) :void
  (cr :pointer))

(cffi:defcfun ("cairo_fill" cairo_fill) :void
  (cr :pointer))

(cffi:defcfun ("cairo_fill_preserve" cairo_fill_preserve) :void
  (cr :pointer))

(cffi:defcfun ("cairo_copy_page" cairo_copy_page) :void
  (cr :pointer))

(cffi:defcfun ("cairo_show_page" cairo_show_page) :void
  (cr :pointer))

(cffi:defcfun ("cairo_in_stroke" cairo_in_stroke) :int
  (cr :pointer)
  (x my-double)
  (y my-double))

(cffi:defcfun ("cairo_in_fill" cairo_in_fill) :int
  (cr :pointer)
  (x my-double)
  (y my-double))

(cffi:defcfun ("cairo_stroke_extents" cairo_stroke_extents) :void
  (cr :pointer)
  (x1 :pointer)
  (y1 :pointer)
  (x2 :pointer)
  (y2 :pointer))

(cffi:defcfun ("cairo_fill_extents" cairo_fill_extents) :void
  (cr :pointer)
  (x1 :pointer)
  (y1 :pointer)
  (x2 :pointer)
  (y2 :pointer))

(cffi:defcfun ("cairo_reset_clip" cairo_reset_clip) :void
  (cr :pointer))

(cffi:defcfun ("cairo_clip" cairo_clip) :void
  (cr :pointer))

(cffi:defcfun ("cairo_clip_preserve" cairo_clip_preserve) :void
  (cr :pointer))

(cffi:defcfun ("cairo_clip_extents" cairo_clip_extents) :void
  (cr :pointer)
  (x1 :pointer)
  (y1 :pointer)
  (x2 :pointer)
  (y2 :pointer))

(cffi:defcstruct cairo_rectangle_t
	(x my-double)
	(y my-double)
	(width my-double)
	(height my-double))

(cffi:defcstruct cairo_rectangle_list_t
	(status cairo_status_t)
	(rectangles :pointer)
	(num_rectangles :int))

(cffi:defcfun ("cairo_copy_clip_rectangle_list" cairo_copy_clip_rectangle_list) :pointer
  (cr :pointer))

(cffi:defcfun ("cairo_rectangle_list_destroy" cairo_rectangle_list_destroy) :void
  (rectangle_list :pointer))

(cffi:defcstruct cairo_glyph_t
	(index :unsigned-long)
	(x my-double)
	(y my-double))

(cffi:defcstruct cairo_text_extents_t
	(x_bearing my-double)
	(y_bearing my-double)
	(width my-double)
	(height my-double)
	(x_advance my-double)
	(y_advance my-double))

(cffi:defcstruct cairo_font_extents_t
	(ascent my-double)
	(descent my-double)
	(height my-double)
	(max_x_advance my-double)
	(max_y_advance my-double))

(cffi:defcenum cairo_font_slant_t
	:CAIRO_FONT_SLANT_NORMAL
	:CAIRO_FONT_SLANT_ITALIC
	:CAIRO_FONT_SLANT_OBLIQUE)

(cffi:defcenum cairo_font_weight_t
	:CAIRO_FONT_WEIGHT_NORMAL
	:CAIRO_FONT_WEIGHT_BOLD)

(cffi:defcenum cairo_subpixel_order_t
	:CAIRO_SUBPIXEL_ORDER_DEFAULT
	:CAIRO_SUBPIXEL_ORDER_RGB
	:CAIRO_SUBPIXEL_ORDER_BGR
	:CAIRO_SUBPIXEL_ORDER_VRGB
	:CAIRO_SUBPIXEL_ORDER_VBGR)

(cffi:defcenum cairo_hint_style_t
	:CAIRO_HINT_STYLE_DEFAULT
	:CAIRO_HINT_STYLE_NONE
	:CAIRO_HINT_STYLE_SLIGHT
	:CAIRO_HINT_STYLE_MEDIUM
	:CAIRO_HINT_STYLE_FULL)

(cffi:defcenum cairo_hint_metrics_t
	:CAIRO_HINT_METRICS_DEFAULT
	:CAIRO_HINT_METRICS_OFF
	:CAIRO_HINT_METRICS_ON)

(cffi:defcfun ("cairo_font_options_create" cairo_font_options_create) :pointer)

(cffi:defcfun ("cairo_font_options_copy" cairo_font_options_copy) :pointer
  (original :pointer))

(cffi:defcfun ("cairo_font_options_destroy" cairo_font_options_destroy) :void
  (options :pointer))

(cffi:defcfun ("cairo_font_options_status" cairo_font_options_status) cairo_status_t
  (options :pointer))

(cffi:defcfun ("cairo_font_options_merge" cairo_font_options_merge) :void
  (options :pointer)
  (other :pointer))

(cffi:defcfun ("cairo_font_options_equal" cairo_font_options_equal) :int
  (options :pointer)
  (other :pointer))

(cffi:defcfun ("cairo_font_options_hash" cairo_font_options_hash) :unsigned-long
  (options :pointer))

(cffi:defcfun ("cairo_font_options_set_antialias" cairo_font_options_set_antialias) :void
  (options :pointer)
  (antialias cairo_antialias_t))

(cffi:defcfun ("cairo_font_options_get_antialias" cairo_font_options_get_antialias) cairo_antialias_t
  (options :pointer))

(cffi:defcfun ("cairo_font_options_set_subpixel_order" cairo_font_options_set_subpixel_order) :void
  (options :pointer)
  (subpixel_order cairo_subpixel_order_t))

(cffi:defcfun ("cairo_font_options_get_subpixel_order" cairo_font_options_get_subpixel_order) cairo_subpixel_order_t
  (options :pointer))

(cffi:defcfun ("cairo_font_options_set_hint_style" cairo_font_options_set_hint_style) :void
  (options :pointer)
  (hint_style cairo_hint_style_t))

(cffi:defcfun ("cairo_font_options_get_hint_style" cairo_font_options_get_hint_style) cairo_hint_style_t
  (options :pointer))

(cffi:defcfun ("cairo_font_options_set_hint_metrics" cairo_font_options_set_hint_metrics) :void
  (options :pointer)
  (hint_metrics cairo_hint_metrics_t))

(cffi:defcfun ("cairo_font_options_get_hint_metrics" cairo_font_options_get_hint_metrics) cairo_hint_metrics_t
  (options :pointer))

(cffi:defcfun ("cairo_select_font_face" cairo_select_font_face) :void
  (cr :pointer)
  (family :string)
  (slant cairo_font_slant_t)
  (weight cairo_font_weight_t))

(cffi:defcfun ("cairo_set_font_size" cairo_set_font_size) :void
  (cr :pointer)
  (size my-double))

(cffi:defcfun ("cairo_set_font_matrix" cairo_set_font_matrix) :void
  (cr :pointer)
  (matrix :pointer))

(cffi:defcfun ("cairo_get_font_matrix" cairo_get_font_matrix) :void
  (cr :pointer)
  (matrix :pointer))

(cffi:defcfun ("cairo_set_font_options" cairo_set_font_options) :void
  (cr :pointer)
  (options :pointer))

(cffi:defcfun ("cairo_get_font_options" cairo_get_font_options) :void
  (cr :pointer)
  (options :pointer))

(cffi:defcfun ("cairo_set_font_face" cairo_set_font_face) :void
  (cr :pointer)
  (font_face :pointer))

(cffi:defcfun ("cairo_get_font_face" cairo_get_font_face) :pointer
  (cr :pointer))

(cffi:defcfun ("cairo_set_scaled_font" cairo_set_scaled_font) :void
  (cr :pointer)
  (scaled_font :pointer))

(cffi:defcfun ("cairo_get_scaled_font" cairo_get_scaled_font) :pointer
  (cr :pointer))

(cffi:defcfun ("cairo_show_text" cairo_show_text) :void
  (cr :pointer)
  (utf8 :string))

(cffi:defcfun ("cairo_show_glyphs" cairo_show_glyphs) :void
  (cr :pointer)
  (glyphs :pointer)
  (num_glyphs :int))

(cffi:defcfun ("cairo_text_path" cairo_text_path) :void
  (cr :pointer)
  (utf8 :string))

(cffi:defcfun ("cairo_glyph_path" cairo_glyph_path) :void
  (cr :pointer)
  (glyphs :pointer)
  (num_glyphs :int))

(cffi:defcfun ("cairo_text_extents" cairo_text_extents) :void
  (cr :pointer)
  (utf8 :string)
  (extents :pointer))

(cffi:defcfun ("cairo_glyph_extents" cairo_glyph_extents) :void
  (cr :pointer)
  (glyphs :pointer)
  (num_glyphs :int)
  (extents :pointer))

(cffi:defcfun ("cairo_font_extents" cairo_font_extents) :void
  (cr :pointer)
  (extents :pointer))

(cffi:defcfun ("cairo_font_face_reference" cairo_font_face_reference) :pointer
  (font_face :pointer))

(cffi:defcfun ("cairo_font_face_destroy" cairo_font_face_destroy) :void
  (font_face :pointer))

(cffi:defcfun ("cairo_font_face_get_reference_count" cairo_font_face_get_reference_count) :unsigned-int
  (font_face :pointer))

(cffi:defcfun ("cairo_font_face_status" cairo_font_face_status) cairo_status_t
  (font_face :pointer))

(cffi:defcenum cairo_font_type_t
	:CAIRO_FONT_TYPE_TOY
	:CAIRO_FONT_TYPE_FT
	:CAIRO_FONT_TYPE_WIN32
	:CAIRO_FONT_TYPE_QUARTZ)

(cffi:defcfun ("cairo_font_face_get_type" cairo_font_face_get_type) cairo_font_type_t
  (font_face :pointer))

(cffi:defcfun ("cairo_font_face_get_user_data" cairo_font_face_get_user_data) :pointer
  (font_face :pointer)
  (key :pointer))

(cffi:defcfun ("cairo_font_face_set_user_data" cairo_font_face_set_user_data) cairo_status_t
  (font_face :pointer)
  (key :pointer)
  (user_data :pointer)
  (destroy :pointer))

(cffi:defcfun ("cairo_scaled_font_create" cairo_scaled_font_create) :pointer
  (font_face :pointer)
  (font_matrix :pointer)
  (ctm :pointer)
  (options :pointer))

(cffi:defcfun ("cairo_scaled_font_reference" cairo_scaled_font_reference) :pointer
  (scaled_font :pointer))

(cffi:defcfun ("cairo_scaled_font_destroy" cairo_scaled_font_destroy) :void
  (scaled_font :pointer))

(cffi:defcfun ("cairo_scaled_font_get_reference_count" cairo_scaled_font_get_reference_count) :unsigned-int
  (scaled_font :pointer))

(cffi:defcfun ("cairo_scaled_font_status" cairo_scaled_font_status) cairo_status_t
  (scaled_font :pointer))

(cffi:defcfun ("cairo_scaled_font_get_type" cairo_scaled_font_get_type) cairo_font_type_t
  (scaled_font :pointer))

(cffi:defcfun ("cairo_scaled_font_get_user_data" cairo_scaled_font_get_user_data) :pointer
  (scaled_font :pointer)
  (key :pointer))

(cffi:defcfun ("cairo_scaled_font_set_user_data" cairo_scaled_font_set_user_data) cairo_status_t
  (scaled_font :pointer)
  (key :pointer)
  (user_data :pointer)
  (destroy :pointer))

(cffi:defcfun ("cairo_scaled_font_extents" cairo_scaled_font_extents) :void
  (scaled_font :pointer)
  (extents :pointer))

(cffi:defcfun ("cairo_scaled_font_text_extents" cairo_scaled_font_text_extents) :void
  (scaled_font :pointer)
  (utf8 :string)
  (extents :pointer))

(cffi:defcfun ("cairo_scaled_font_glyph_extents" cairo_scaled_font_glyph_extents) :void
  (scaled_font :pointer)
  (glyphs :pointer)
  (num_glyphs :int)
  (extents :pointer))

(cffi:defcfun ("cairo_scaled_font_get_font_face" cairo_scaled_font_get_font_face) :pointer
  (scaled_font :pointer))

(cffi:defcfun ("cairo_scaled_font_get_font_matrix" cairo_scaled_font_get_font_matrix) :void
  (scaled_font :pointer)
  (font_matrix :pointer))

(cffi:defcfun ("cairo_scaled_font_get_ctm" cairo_scaled_font_get_ctm) :void
  (scaled_font :pointer)
  (ctm :pointer))

(cffi:defcfun ("cairo_scaled_font_get_font_options" cairo_scaled_font_get_font_options) :void
  (scaled_font :pointer)
  (options :pointer))

(cffi:defcfun ("cairo_get_operator" cairo_get_operator) cairo_operator_t
  (cr :pointer))

(cffi:defcfun ("cairo_get_source" cairo_get_source) :pointer
  (cr :pointer))

(cffi:defcfun ("cairo_get_tolerance" cairo_get_tolerance) :double
  (cr :pointer))

(cffi:defcfun ("cairo_get_antialias" cairo_get_antialias) cairo_antialias_t
  (cr :pointer))

(cffi:defcfun ("cairo_has_current_point" cairo_has_current_point) :int
  (cr :pointer))

(cffi:defcfun ("cairo_get_current_point" cairo_get_current_point) :void
  (cr :pointer)
  (x :pointer)
  (y :pointer))

(cffi:defcfun ("cairo_get_fill_rule" cairo_get_fill_rule) cairo_fill_rule_t
  (cr :pointer))

(cffi:defcfun ("cairo_get_line_width" cairo_get_line_width) :double
  (cr :pointer))

(cffi:defcfun ("cairo_get_line_cap" cairo_get_line_cap) cairo_line_cap_t
  (cr :pointer))

(cffi:defcfun ("cairo_get_line_join" cairo_get_line_join) cairo_line_join_t
  (cr :pointer))

(cffi:defcfun ("cairo_get_miter_limit" cairo_get_miter_limit) :double
  (cr :pointer))

(cffi:defcfun ("cairo_get_dash_count" cairo_get_dash_count) :int
  (cr :pointer))

(cffi:defcfun ("cairo_get_dash" cairo_get_dash) :void
  (cr :pointer)
  (dashes :pointer)
  (offset :pointer))

(cffi:defcfun ("cairo_get_matrix" cairo_get_matrix) :void
  (cr :pointer)
  (matrix :pointer))

(cffi:defcfun ("cairo_get_target" cairo_get_target) :pointer
  (cr :pointer))

(cffi:defcfun ("cairo_get_group_target" cairo_get_group_target) :pointer
  (cr :pointer))

(cffi:defcenum cairo_path_data_type_t
	:CAIRO_PATH_MOVE_TO
	:CAIRO_PATH_LINE_TO
	:CAIRO_PATH_CURVE_TO
	:CAIRO_PATH_CLOSE_PATH)

(cffi:defcunion _cairo_path_data_t
	(point :pointer)
	(header :pointer))

(cffi:defcstruct _cairo_path_data_t_point
	(x my-double)
	(y my-double))

(cffi:defcstruct _cairo_path_data_t_header
	(type cairo_path_data_type_t)
	(length :int))

(cffi:defcstruct cairo_path_t
	(status cairo_status_t)
	(data :pointer)
	(num_data :int))

(cffi:defcfun ("cairo_copy_path" cairo_copy_path) :pointer
  (cr :pointer))

(cffi:defcfun ("cairo_copy_path_flat" cairo_copy_path_flat) :pointer
  (cr :pointer))

(cffi:defcfun ("cairo_append_path" cairo_append_path) :void
  (cr :pointer)
  (path :pointer))

(cffi:defcfun ("cairo_path_destroy" cairo_path_destroy) :void
  (path :pointer))

(cffi:defcfun ("cairo_status" cairo_status) cairo_status_t
  (cr :pointer))

(cffi:defcfun ("cairo_status_to_string" cairo_status_to_string) :string
  (status cairo_status_t))

(cffi:defcfun ("cairo_surface_create_similar" cairo_surface_create_similar) :pointer
  (other :pointer)
  (content cairo_content_t)
  (width :int)
  (height :int))

(cffi:defcfun ("cairo_surface_reference" cairo_surface_reference) :pointer
  (surface :pointer))

(cffi:defcfun ("cairo_surface_finish" cairo_surface_finish) :void
  (surface :pointer))

(cffi:defcfun ("cairo_surface_destroy" cairo_surface_destroy) :void
  (surface :pointer))

(cffi:defcfun ("cairo_surface_get_reference_count" cairo_surface_get_reference_count) :unsigned-int
  (surface :pointer))

(cffi:defcfun ("cairo_surface_status" cairo_surface_status) cairo_status_t
  (surface :pointer))

(cffi:defcenum cairo_surface_type_t
	:CAIRO_SURFACE_TYPE_IMAGE
	:CAIRO_SURFACE_TYPE_PDF
	:CAIRO_SURFACE_TYPE_PS
	:CAIRO_SURFACE_TYPE_XLIB
	:CAIRO_SURFACE_TYPE_XCB
	:CAIRO_SURFACE_TYPE_GLITZ
	:CAIRO_SURFACE_TYPE_QUARTZ
	:CAIRO_SURFACE_TYPE_WIN32
	:CAIRO_SURFACE_TYPE_BEOS
	:CAIRO_SURFACE_TYPE_DIRECTFB
	:CAIRO_SURFACE_TYPE_SVG
	:CAIRO_SURFACE_TYPE_OS2
	:CAIRO_SURFACE_TYPE_WIN32_PRINTING
	:CAIRO_SURFACE_TYPE_QUARTZ_IMAGE)

(cffi:defcfun ("cairo_surface_get_type" cairo_surface_get_type) cairo_surface_type_t
  (surface :pointer))

(cffi:defcfun ("cairo_surface_get_content" cairo_surface_get_content) cairo_content_t
  (surface :pointer))

(cffi:defcfun ("cairo_surface_write_to_png" cairo_surface_write_to_png) cairo_status_t
  (surface :pointer)
  (filename :string))

(cffi:defcfun ("cairo_surface_write_to_png_stream" cairo_surface_write_to_png_stream) cairo_status_t
  (surface :pointer)
  (write_func :pointer)
  (closure :pointer))

(cffi:defcfun ("cairo_surface_get_user_data" cairo_surface_get_user_data) :pointer
  (surface :pointer)
  (key :pointer))

(cffi:defcfun ("cairo_surface_set_user_data" cairo_surface_set_user_data) cairo_status_t
  (surface :pointer)
  (key :pointer)
  (user_data :pointer)
  (destroy :pointer))

(cffi:defcfun ("cairo_surface_get_font_options" cairo_surface_get_font_options) :void
  (surface :pointer)
  (options :pointer))

(cffi:defcfun ("cairo_surface_flush" cairo_surface_flush) :void
  (surface :pointer))

(cffi:defcfun ("cairo_surface_mark_dirty" cairo_surface_mark_dirty) :void
  (surface :pointer))

(cffi:defcfun ("cairo_surface_mark_dirty_rectangle" cairo_surface_mark_dirty_rectangle) :void
  (surface :pointer)
  (x :int)
  (y :int)
  (width :int)
  (height :int))

(cffi:defcfun ("cairo_surface_set_device_offset" cairo_surface_set_device_offset) :void
  (surface :pointer)
  (x_offset my-double)
  (y_offset my-double))

(cffi:defcfun ("cairo_surface_get_device_offset" cairo_surface_get_device_offset) :void
  (surface :pointer)
  (x_offset :pointer)
  (y_offset :pointer))

(cffi:defcfun ("cairo_surface_set_fallback_resolution" cairo_surface_set_fallback_resolution) :void
  (surface :pointer)
  (x_pixels_per_inch my-double)
  (y_pixels_per_inch my-double))

(cffi:defcfun ("cairo_surface_copy_page" cairo_surface_copy_page) :void
  (surface :pointer))

(cffi:defcfun ("cairo_surface_show_page" cairo_surface_show_page) :void
  (surface :pointer))

(cffi:defcenum cairo_format_t
	:CAIRO_FORMAT_ARGB32
	:CAIRO_FORMAT_RGB24
	:CAIRO_FORMAT_A8
	:CAIRO_FORMAT_A1)

(cffi:defcfun ("cairo_image_surface_create" cairo_image_surface_create) :pointer
  (format cairo_format_t)
  (width :int)
  (height :int))

(cffi:defcfun ("cairo_format_stride_for_width" cairo_format_stride_for_width) :int
  (format cairo_format_t)
  (width :int))

(cffi:defcfun ("cairo_image_surface_create_for_data" cairo_image_surface_create_for_data) :pointer
  (data :pointer)
  (format cairo_format_t)
  (width :int)
  (height :int)
  (stride :int))

(cffi:defcfun ("cairo_image_surface_get_data" cairo_image_surface_get_data) :pointer
  (surface :pointer))

(cffi:defcfun ("cairo_image_surface_get_format" cairo_image_surface_get_format) cairo_format_t
  (surface :pointer))

(cffi:defcfun ("cairo_image_surface_get_width" cairo_image_surface_get_width) :int
  (surface :pointer))

(cffi:defcfun ("cairo_image_surface_get_height" cairo_image_surface_get_height) :int
  (surface :pointer))

(cffi:defcfun ("cairo_image_surface_get_stride" cairo_image_surface_get_stride) :int
  (surface :pointer))

(cffi:defcfun ("cairo_image_surface_create_from_png" cairo_image_surface_create_from_png) :pointer
  (filename :string))

(cffi:defcfun ("cairo_image_surface_create_from_png_stream" cairo_image_surface_create_from_png_stream) :pointer
  (read_func :pointer)
  (closure :pointer))

(cffi:defcfun ("cairo_pattern_create_rgb" cairo_pattern_create_rgb) :pointer
  (red my-double)
  (green my-double)
  (blue my-double))

(cffi:defcfun ("cairo_pattern_create_rgba" cairo_pattern_create_rgba) :pointer
  (red my-double)
  (green my-double)
  (blue my-double)
  (alpha my-double))

(cffi:defcfun ("cairo_pattern_create_for_surface" cairo_pattern_create_for_surface) :pointer
  (surface :pointer))

(cffi:defcfun ("cairo_pattern_create_linear" cairo_pattern_create_linear) :pointer
  (x0 my-double)
  (y0 my-double)
  (x1 my-double)
  (y1 my-double))

(cffi:defcfun ("cairo_pattern_create_radial" cairo_pattern_create_radial) :pointer
  (cx0 my-double)
  (cy0 my-double)
  (radius0 my-double)
  (cx1 my-double)
  (cy1 my-double)
  (radius1 my-double))

(cffi:defcfun ("cairo_pattern_reference" cairo_pattern_reference) :pointer
  (pattern :pointer))

(cffi:defcfun ("cairo_pattern_destroy" cairo_pattern_destroy) :void
  (pattern :pointer))

(cffi:defcfun ("cairo_pattern_get_reference_count" cairo_pattern_get_reference_count) :unsigned-int
  (pattern :pointer))

(cffi:defcfun ("cairo_pattern_status" cairo_pattern_status) cairo_status_t
  (pattern :pointer))

(cffi:defcfun ("cairo_pattern_get_user_data" cairo_pattern_get_user_data) :pointer
  (pattern :pointer)
  (key :pointer))

(cffi:defcfun ("cairo_pattern_set_user_data" cairo_pattern_set_user_data) cairo_status_t
  (pattern :pointer)
  (key :pointer)
  (user_data :pointer)
  (destroy :pointer))

(cffi:defcenum cairo_pattern_type_t
	:CAIRO_PATTERN_TYPE_SOLID
	:CAIRO_PATTERN_TYPE_SURFACE
	:CAIRO_PATTERN_TYPE_LINEAR
	:CAIRO_PATTERN_TYPE_RADIAL)

(cffi:defcfun ("cairo_pattern_get_type" cairo_pattern_get_type) cairo_pattern_type_t
  (pattern :pointer))

(cffi:defcfun ("cairo_pattern_add_color_stop_rgb" cairo_pattern_add_color_stop_rgb) :void
  (pattern :pointer)
  (offset my-double)
  (red my-double)
  (green my-double)
  (blue my-double))

(cffi:defcfun ("cairo_pattern_add_color_stop_rgba" cairo_pattern_add_color_stop_rgba) :void
  (pattern :pointer)
  (offset my-double)
  (red my-double)
  (green my-double)
  (blue my-double)
  (alpha my-double))

(cffi:defcfun ("cairo_pattern_set_matrix" cairo_pattern_set_matrix) :void
  (pattern :pointer)
  (matrix :pointer))

(cffi:defcfun ("cairo_pattern_get_matrix" cairo_pattern_get_matrix) :void
  (pattern :pointer)
  (matrix :pointer))

(cffi:defcenum cairo_extend_t
	:CAIRO_EXTEND_NONE
	:CAIRO_EXTEND_REPEAT
	:CAIRO_EXTEND_REFLECT
	:CAIRO_EXTEND_PAD)

(cffi:defcfun ("cairo_pattern_set_extend" cairo_pattern_set_extend) :void
  (pattern :pointer)
  (extend cairo_extend_t))

(cffi:defcfun ("cairo_pattern_get_extend" cairo_pattern_get_extend) cairo_extend_t
  (pattern :pointer))

(cffi:defcenum cairo_filter_t
	:CAIRO_FILTER_FAST
	:CAIRO_FILTER_GOOD
	:CAIRO_FILTER_BEST
	:CAIRO_FILTER_NEAREST
	:CAIRO_FILTER_BILINEAR
	:CAIRO_FILTER_GAUSSIAN)

(cffi:defcfun ("cairo_pattern_set_filter" cairo_pattern_set_filter) :void
  (pattern :pointer)
  (filter cairo_filter_t))

(cffi:defcfun ("cairo_pattern_get_filter" cairo_pattern_get_filter) cairo_filter_t
  (pattern :pointer))

(cffi:defcfun ("cairo_pattern_get_rgba" cairo_pattern_get_rgba) cairo_status_t
  (pattern :pointer)
  (red :pointer)
  (green :pointer)
  (blue :pointer)
  (alpha :pointer))

(cffi:defcfun ("cairo_pattern_get_surface" cairo_pattern_get_surface) cairo_status_t
  (pattern :pointer)
  (surface :pointer))

(cffi:defcfun ("cairo_pattern_get_color_stop_rgba" cairo_pattern_get_color_stop_rgba) cairo_status_t
  (pattern :pointer)
  (index :int)
  (offset :pointer)
  (red :pointer)
  (green :pointer)
  (blue :pointer)
  (alpha :pointer))

(cffi:defcfun ("cairo_pattern_get_color_stop_count" cairo_pattern_get_color_stop_count) cairo_status_t
  (pattern :pointer)
  (count :pointer))

(cffi:defcfun ("cairo_pattern_get_linear_points" cairo_pattern_get_linear_points) cairo_status_t
  (pattern :pointer)
  (x0 :pointer)
  (y0 :pointer)
  (x1 :pointer)
  (y1 :pointer))

(cffi:defcfun ("cairo_pattern_get_radial_circles" cairo_pattern_get_radial_circles) cairo_status_t
  (pattern :pointer)
  (x0 :pointer)
  (y0 :pointer)
  (r0 :pointer)
  (x1 :pointer)
  (y1 :pointer)
  (r1 :pointer))

(cffi:defcfun ("cairo_matrix_init" cairo_matrix_init) :void
  (matrix :pointer)
  (xx my-double)
  (yx my-double)
  (xy my-double)
  (yy my-double)
  (x0 my-double)
  (y0 my-double))

(cffi:defcfun ("cairo_matrix_init_identity" cairo_matrix_init_identity) :void
  (matrix :pointer))

(cffi:defcfun ("cairo_matrix_init_translate" cairo_matrix_init_translate) :void
  (matrix :pointer)
  (tx my-double)
  (ty my-double))

(cffi:defcfun ("cairo_matrix_init_scale" cairo_matrix_init_scale) :void
  (matrix :pointer)
  (sx my-double)
  (sy my-double))

(cffi:defcfun ("cairo_matrix_init_rotate" cairo_matrix_init_rotate) :void
  (matrix :pointer)
  (radians my-double))

(cffi:defcfun ("cairo_matrix_translate" cairo_matrix_translate) :void
  (matrix :pointer)
  (tx my-double)
  (ty my-double))

(cffi:defcfun ("cairo_matrix_scale" cairo_matrix_scale) :void
  (matrix :pointer)
  (sx my-double)
  (sy my-double))

(cffi:defcfun ("cairo_matrix_rotate" cairo_matrix_rotate) :void
  (matrix :pointer)
  (radians my-double))

(cffi:defcfun ("cairo_matrix_invert" cairo_matrix_invert) cairo_status_t
  (matrix :pointer))

(cffi:defcfun ("cairo_matrix_multiply" cairo_matrix_multiply) :void
  (result :pointer)
  (a :pointer)
  (b :pointer))

(cffi:defcfun ("cairo_matrix_transform_distance" cairo_matrix_transform_distance) :void
  (matrix :pointer)
  (dx :pointer)
  (dy :pointer))

(cffi:defcfun ("cairo_matrix_transform_point" cairo_matrix_transform_point) :void
  (matrix :pointer)
  (x :pointer)
  (y :pointer))

(cffi:defcfun ("cairo_debug_reset_static_data" cairo_debug_reset_static_data) :void)

(cffi:defcfun ("cairo_ft_font_face_create_for_pattern" cairo_ft_font_face_create_for_pattern) :pointer
  (pattern :pointer))

(cffi:defcfun ("cairo_ft_font_options_substitute" cairo_ft_font_options_substitute) :void
  (options :pointer)
  (pattern :pointer))

(cffi:defcfun ("cairo_ft_font_face_create_for_ft_face" cairo_ft_font_face_create_for_ft_face) :pointer
  (face :pointer)
  (load_flags :int))

(cffi:defcfun ("cairo_ft_scaled_font_lock_face" cairo_ft_scaled_font_lock_face) :pointer
  (scaled_font :pointer))

(cffi:defcfun ("cairo_ft_scaled_font_unlock_face" cairo_ft_scaled_font_unlock_face) :void
  (scaled_font :pointer))

(cffi:defcenum cairo_ps_level_t
	:CAIRO_PS_LEVEL_2
	:CAIRO_PS_LEVEL_3)

(cffi:defcfun ("cairo_ps_surface_create" cairo_ps_surface_create) :pointer
  (filename :string)
  (width_in_points my-double)
  (height_in_points my-double))

(cffi:defcfun ("cairo_ps_surface_create_for_stream" cairo_ps_surface_create_for_stream) :pointer
  (write_func :pointer)
  (closure :pointer)
  (width_in_points my-double)
  (height_in_points my-double))

(cffi:defcfun ("cairo_ps_surface_restrict_to_level" cairo_ps_surface_restrict_to_level) :void
  (surface :pointer)
  (level cairo_ps_level_t))

(cffi:defcfun ("cairo_ps_get_levels" cairo_ps_get_levels) :void
  (levels :pointer)
  (num_levels :pointer))

(cffi:defcfun ("cairo_ps_level_to_string" cairo_ps_level_to_string) :string
  (level cairo_ps_level_t))

(cffi:defcfun ("cairo_ps_surface_set_eps" cairo_ps_surface_set_eps) :void
  (surface :pointer)
  (eps :int))

(cffi:defcfun ("cairo_ps_surface_get_eps" cairo_ps_surface_get_eps) :int
  (surface :pointer))

(cffi:defcfun ("cairo_ps_surface_set_size" cairo_ps_surface_set_size) :void
  (surface :pointer)
  (width_in_points my-double)
  (height_in_points my-double))

(cffi:defcfun ("cairo_ps_surface_dsc_comment" cairo_ps_surface_dsc_comment) :void
  (surface :pointer)
  (comment :string))

(cffi:defcfun ("cairo_ps_surface_dsc_begin_setup" cairo_ps_surface_dsc_begin_setup) :void
  (surface :pointer))

(cffi:defcfun ("cairo_ps_surface_dsc_begin_page_setup" cairo_ps_surface_dsc_begin_page_setup) :void
  (surface :pointer))

(cffi:defcfun ("cairo_pdf_surface_create" cairo_pdf_surface_create) :pointer
  (filename :string)
  (width_in_points my-double)
  (height_in_points my-double))

(cffi:defcfun ("cairo_pdf_surface_create_for_stream" cairo_pdf_surface_create_for_stream) :pointer
  (write_func :pointer)
  (closure :pointer)
  (width_in_points my-double)
  (height_in_points my-double))

(cffi:defcfun ("cairo_pdf_surface_set_size" cairo_pdf_surface_set_size) :void
  (surface :pointer)
  (width_in_points my-double)
  (height_in_points my-double))

(cffi:defcenum cairo_svg_version_t
	:CAIRO_SVG_VERSION_1_1
	:CAIRO_SVG_VERSION_1_2)

(cffi:defcfun ("cairo_svg_surface_create" cairo_svg_surface_create) :pointer
  (filename :string)
  (width_in_points my-double)
  (height_in_points my-double))

(cffi:defcfun ("cairo_svg_surface_create_for_stream" cairo_svg_surface_create_for_stream) :pointer
  (write_func :pointer)
  (closure :pointer)
  (width_in_points my-double)
  (height_in_points my-double))

(cffi:defcfun ("cairo_svg_surface_restrict_to_version" cairo_svg_surface_restrict_to_version) :void
  (surface :pointer)
  (version cairo_svg_version_t))

(cffi:defcfun ("cairo_svg_get_versions" cairo_svg_get_versions) :void
  (versions :pointer)
  (num_versions :pointer))

(cffi:defcfun ("cairo_svg_version_to_string" cairo_svg_version_to_string) :string
  (version cairo_svg_version_t))


