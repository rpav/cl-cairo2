
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

(cl:defconstant CAIRO_HAS_QUARTZ_IMAGE_SURFACE 1)

(cl:defconstant CAIRO_HAS_QUARTZ_FONT 1)

(cl:defconstant CAIRO_HAS_QUARTZ_SURFACE 1)

(cl:defconstant CAIRO_HAS_XLIB_XRENDER_SURFACE 1)

(cl:defconstant CAIRO_HAS_XLIB_SURFACE 1)

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

(cffi:defcfun ("cairo_xlib_surface_create_with_xrender_format" cairo_xlib_surface_create_with_xrender_format) :pointer
  (dpy :pointer)
  (drawable :pointer)
  (screen :pointer)
  (format :pointer)
  (width :int)
  (height :int))

(cffi:defcfun ("cairo_xlib_surface_get_xrender_format" cairo_xlib_surface_get_xrender_format) :pointer
  (surface :pointer))

(cffi:defcfun ("cairo_quartz_surface_create" cairo_quartz_surface_create) :pointer
  (format :pointer)
  (width :unsigned-int)
  (height :unsigned-int))

(cffi:defcfun ("cairo_quartz_surface_create_for_cg_context" cairo_quartz_surface_create_for_cg_context) :pointer
  (cgContext :pointer)
  (width :unsigned-int)
  (height :unsigned-int))

(cffi:defcfun ("cairo_quartz_surface_get_cg_context" cairo_quartz_surface_get_cg_context) :pointer
  (surface :pointer))

(cffi:defcfun ("cairo_quartz_font_face_create_for_cgfont" cairo_quartz_font_face_create_for_cgfont) :pointer
  (font :pointer))

(cffi:defcfun ("cairo_quartz_font_face_create_for_atsu_font_id" cairo_quartz_font_face_create_for_atsu_font_id) :pointer
  (font_id :pointer))


