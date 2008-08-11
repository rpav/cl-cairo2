
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

(cl:defconstant CAIRO_HAS_PNG_FUNCTIONS 1)

(cl:defconstant CAIRO_HAS_WIN32_FONT 1)

(cl:defconstant CAIRO_HAS_WIN32_SURFACE 1)

(cffi:defcfun ("cairo_win32_surface_create" cairo_win32_surface_create) :pointer
  (hdc :pointer))

(cffi:defcfun ("cairo_win32_printing_surface_create" cairo_win32_printing_surface_create) :pointer
  (hdc :pointer))

(cffi:defcfun ("cairo_win32_surface_create_with_ddb" cairo_win32_surface_create_with_ddb) :pointer
  (hdc :pointer)
  (format :pointer)
  (width :int)
  (height :int))

(cffi:defcfun ("cairo_win32_surface_create_with_dib" cairo_win32_surface_create_with_dib) :pointer
  (format :pointer)
  (width :int)
  (height :int))

(cffi:defcfun ("cairo_win32_surface_get_dc" cairo_win32_surface_get_dc) :pointer
  (surface :pointer))

(cffi:defcfun ("cairo_win32_surface_get_image" cairo_win32_surface_get_image) :pointer
  (surface :pointer))

(cffi:defcfun ("cairo_win32_font_face_create_for_logfontw" cairo_win32_font_face_create_for_logfontw) :pointer
  (logfont :pointer))

(cffi:defcfun ("cairo_win32_font_face_create_for_hfont" cairo_win32_font_face_create_for_hfont) :pointer
  (font :pointer))

(cffi:defcfun ("cairo_win32_font_face_create_for_logfontw_hfont" cairo_win32_font_face_create_for_logfontw_hfont) :pointer
  (logfont :pointer)
  (font :pointer))

(cffi:defcfun ("cairo_win32_scaled_font_select_font" cairo_win32_scaled_font_select_font) :pointer
  (scaled_font :pointer)
  (hdc :pointer))

(cffi:defcfun ("cairo_win32_scaled_font_done_font" cairo_win32_scaled_font_done_font) :void
  (scaled_font :pointer))

(cffi:defcfun ("cairo_win32_scaled_font_get_metrics_factor" cairo_win32_scaled_font_get_metrics_factor) :double
  (scaled_font :pointer))

(cffi:defcfun ("cairo_win32_scaled_font_get_logical_to_device" cairo_win32_scaled_font_get_logical_to_device) :void
  (scaled_font :pointer)
  (logical_to_device :pointer))

(cffi:defcfun ("cairo_win32_scaled_font_get_device_to_logical" cairo_win32_scaled_font_get_device_to_logical) :void
  (scaled_font :pointer)
  (device_to_logical :pointer))


