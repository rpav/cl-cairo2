(in-package :cl-cairo2)

;;;; TODO:
;;;;
;;;; - text clusters (maybe)
;;;;
;;;; NOT TODO:
;;;;
;;;; - cairo_scaled_font_text_to_glyphs: this seems to be essentially
;;;;   identical to the toy interface, and neither Quartz nor Freetype
;;;;   backends even seem to implement this, and there doesn't seem to
;;;;   be a way to tell.  I'm not sure what the point is.

 ;; Types

;;;; These are mostly for specialization; specific implementations are
;;;; needed to get a useful instance (FreeType, etc).

(defclass font-face (cairo-object) ())
(defclass scaled-font (cairo-object)
  ((font-face :initarg :font-face :initform nil
              :accessor scaled-font-face)))

(defclass font-options (cairo-object) ())

 ;; Generic Functions

(defgeneric create-font (source-face &key &allow-other-keys)
  (:documentation "Create a FONT-FACE (cairo_font_t) from SOURCE-FACE"))

(defgeneric set-font (font-face &optional context)
  (:documentation "Set the current font to FONT-FACE"))

 ;; Methods

(defmethod set-font ((font-face font-face) &optional (context *context*))
  (set-font-face font-face context))

(defmethod set-font ((font-face scaled-font) &optional (context *context*))
  (set-scaled-font font-face context))

(defmethod lowlevel-destroy ((object font-face))
  (cairo_font_face_destroy (get-pointer object)))

(defmethod lowlevel-destroy ((object scaled-font))
  (cairo_scaled_font_destroy (get-pointer object)))

(defmethod lowlevel-destroy ((object font-options))
  (cairo_font_options_destroy (get-pointer object)))

(defmethod lowlevel-status ((object font-face))
  (cairo_font_face_status (get-pointer object)))

(defmethod lowlevel-status ((object scaled-font))
  (cairo_scaled_font_status (get-pointer object)))

(defmethod lowlevel-status ((object font-options))
  (cairo_font_options_status (get-pointer object)))

 ;; cairo_font_face_t functions

(defun font-face-get-type (font)
  (cairo_font_face_get_type (get-pointer font)))

 ;; cairo_scaled_font_t functions

(defun create-scaled-font (font font-matrix matrix options)
  (with-alive-object (font f-ptr)
    (with-alive-object (options o-ptr)
      (let ((fm-ptr (foreign-alloc 'cairo_matrix_t))
            (m-ptr (foreign-alloc 'cairo_matrix_t)))
        (trans-matrix-copy-in fm-ptr font-matrix)
        (trans-matrix-copy-in m-ptr matrix)
        (let* ((s-ptr (cairo_scaled_font_create f-ptr fm-ptr m-ptr o-ptr))
               (scaled-font (make-instance 'scaled-font
                                           :pointer s-ptr
                                           :font-face font)))
          (tg:cancel-finalization options)
          (tg:finalize scaled-font
                       (lambda ()
                         (cairo_scaled_font_destroy s-ptr)
                         (foreign-free fm-ptr)
                         (foreign-free m-ptr)
                         (cairo_font_options_destroy o-ptr)))
          scaled-font)))))

(defun scaled-font-extents (scaled-font)
  (with-alive-object (scaled-font font-pointer)
    (with-font-extents-t-out extents-pointer
      (cairo_scaled_font_extents font-pointer extents-pointer))))

(defun scaled-font-text-extents (scaled-font string)
  (with-alive-object (scaled-font font-pointer)
    (with-font-extents-t-out extents-pointer
      (with-foreign-string (cstr string)
        (cairo_scaled_font_text_extents font-pointer cstr extents-pointer)))))

(defun scaled-font-glyph-extents (scaled-font glyphs)
  (with-alive-object (scaled-font font-pointer)
    (with-font-extents-t-out extents-pointer
      (with-foreign-object (arr :int (length glyphs))
        (loop for g across glyphs
              for i from 0
              do (setf (mem-aref arr :int i) (aref glyphs i)))
        (cairo_scaled_font_glyph_extents font-pointer arr (length glyphs)
                                         extents-pointer)))))

;; *_get_font_face is covered by scaled-font-face
;; *_get_font_options
;; *_get_font_matrix
;; *_get_ctm
;; *_get_scale_matrix

(defun scaled-font-get-type (scaled-font)
  (with-alive-object (scaled-font font-pointer)
    (cairo_scaled_font_get_type font-pointer)))

 ;; cairo_font_options_t functions

(defun create-font-options ()
  (let* ((pointer (cairo_font_options_create))
         (options (make-instance 'font-options :pointer pointer)))
    (tg:finalize options (lambda () (cairo_font_options_destroy pointer)))
    options))

(defun font-options-copy (font-options)
  (with-alive-object (font-options src)
    (let* ((dest (cairo_font_options_copy src))
           (options (make-instance 'font-options :pointer dest)))
      (tg:finalize options (lambda () (cairo_font_options_destroy dest)))
      options)))

(defun font-options-merge (fo1 fo2)
  (with-alive-object (fo1 ptr1)
    (with-alive-object (fo2 ptr2)
      (cairo_font_options_merge fo1 fo2))))

(defun font-options-hash (font-options)
  (with-alive-object (font-options ptr)
    (cairo_font_options_hash ptr)))

(defun font-options-equal (fo1 fo2)
  (with-alive-object (fo1 ptr1)
    (with-alive-object (fo2 ptr2)
      (cairo_font_options_equal fo1 fo2))))

(defun font-options-set-antialias (font-options antialias-type)
  (with-alive-object (font-options ptr)
    (cairo_font_options_set_antialias ptr antialias-type)))

(defun font-options-get-antialias (font-options)
  (with-alive-object (font-options ptr)
    (cairo_font_options_get_antialias ptr)))

(defun font-options-set-subpixel-order (font-options subpixel-order)
  (with-alive-object (font-options ptr)
    (cairo_font_options_set_subpixel_order ptr subpixel-order)))

(defun font-options-get-subpixel-order (font-options)
  (with-alive-object (font-options ptr)
    (cairo_font_options_get_subpixel_order ptr)))

(defun font-options-set-hint-style (font-options hint-style)
  (with-alive-object (font-options ptr)
    (cairo_font_options_set_hint_style ptr hint-style)))

(defun font-options-get-hint-style (font-options)
  (with-alive-object (font-options ptr)
    (cairo_font_options_get_hint_style ptr)))

(defun font-options-set-hint-metrics (font-options hint-metrics)
  (with-alive-object (font-options ptr)
    (cairo_font_options_set_hint_metrics ptr hint-metrics)))

(defun font-options-get-hint-metrics (font-options)
  (with-alive-object (font-options ptr)
    (cairo_font_options_get_hint_metrics ptr)))
