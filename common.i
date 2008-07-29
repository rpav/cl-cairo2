%ignore CAIRO_VERSION;
%ignore CAIRO_VERSION_STRING;

%typemap(cin) double "my-double";

%insert("lisphead") %{
(in-package :cl-cairo2)

;; define our own alias for double float, so we can automatically
;; convert other numerical types in the arguments
(define-foreign-type my-double-type ()
  ()
  (:actual-type :double)
  (:simple-parser my-double))
                                                                              
(defmethod translate-to-foreign (value (type my-double-type))
  (coerce value 'double-float))

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
%}

