(in-package :cl-cairo2)

 ;; Variables

(defvar *user-font-ptr-to-object* (make-hash-table))

(defvar *user-font-temp-context*
  (make-instance 'context :width 0 :height 0 :pixel-based-p nil
                 :pointer nil)
  "Temporary context so we don't allocate a new CONTEXT just to wrap
a pointer when rendering.  Not threadsafe, but neither is cairo.")

(defvar *user-font-temp-scaled-font*
  (make-instance 'scaled-font :font-face nil :pointer nil)
  "Temporary scaled-font so we don't allocate.")


 ;; Types

(defclass user-font-face (font-face)
  ((init-fun :initarg :init :accessor user-font-init-fun)
   (render-glyph-fun :initarg :render-glyph :accessor user-font-render-glyph-fun)
   (unicode-to-glyph-fun :initarg :unicode-to-glyph
                         :accessor user-font-unicode-to-glyph-fun)))

 ;; Callbacks

(defcallback user-font-init-cb cairo_status_t
    ((scaled-font :pointer)
     (ctx :pointer)
     (extents :pointer))

  #+sbcl (declaim (optimize (debug 1) (speed 1) (safety 1)))
  (let* ((font-ptr (cairo_scaled_font_get_font_face scaled-font))
         (user-font (gethash (pointer-address font-ptr)
                             *user-font-ptr-to-object*))
         (font-extents (make-font-extents-t
                        :ascent 1.0 :descent 0.0 :height 1.0
                        :max-x-advance 1.0 :max-y-advance 0.0)))
    (declare (dynamic-extent font-extents))
    (when (and user-font (slot-boundp user-font 'init-fun))
      (setf (slot-value *user-font-temp-context* 'pointer) ctx
            (slot-value *user-font-temp-scaled-font* 'pointer) scaled-font
            (slot-value *user-font-temp-scaled-font* 'font-face) user-font)
      (funcall (user-font-init-fun user-font)
               *user-font-temp-scaled-font*
               *user-font-temp-context*
               font-extents)
      (font-extents-t-copy-in extents font-extents)))
  :cairo_status_success)

(defcallback user-font-render-glyph-cb cairo_status_t
    ((scaled-font :pointer)
     (glyph :unsigned-long)
     (ctx :pointer)
     (extents :pointer))
  (let* ((font-ptr (cairo_scaled_font_get_font_face scaled-font))
         (user-font (gethash (pointer-address font-ptr)
                             *user-font-ptr-to-object*))
         (font-extents (make-text-extents-t)))
    (declare (dynamic-extent font-extents))
    (text-extents-t-copy-out extents font-extents)
    (setf (slot-value *user-font-temp-context* 'pointer) ctx
          (slot-value *user-font-temp-scaled-font* 'pointer) scaled-font
          (slot-value *user-font-temp-scaled-font* 'font-face) user-font)
    (funcall (user-font-render-glyph-fun user-font)
             *user-font-temp-scaled-font*
             glyph
             *user-font-temp-context*
             font-extents)
    (text-extents-t-copy-in extents font-extents))
  :cairo_status_success)

(defcallback user-font-unicode-to-glyph-cb cairo_status_t
    ((scaled-font :pointer)
     (unicode :unsigned-long)
     (glyph-index :pointer))
  (let* ((font-ptr (cairo_scaled_font_get_font_face scaled-font))
         (user-font (gethash (pointer-address font-ptr)
                             *user-font-ptr-to-object*)))
    (when (slot-boundp user-font 'unicode-to-glyph-fun)
      (setf (slot-value *user-font-temp-scaled-font* 'pointer) scaled-font
            (slot-value *user-font-temp-scaled-font* 'font-face) user-font)
      (let ((index
              (funcall (user-font-unicode-to-glyph-fun user-font)
                       *user-font-temp-scaled-font*
                       unicode)))
        (setf (mem-ref glyph-index :unsigned-long) index))))
  :cairo_status_success)

 ;; Methods

(defmethod initialize-instance ((user-font user-font-face)
                                &rest rest &key &allow-other-keys)
  (declare (ignore rest))
  (call-next-method)
  (unless (slot-boundp user-font 'render-glyph-fun)
    (error "No render function specified for USER-FONT"))
  (let ((ptr (cairo_user_font_face_create)))
    (setf (slot-value user-font 'pointer) ptr)
    (setf (gethash (pointer-address ptr)
                   *user-font-ptr-to-object*) user-font)
    (cairo_user_font_face_set_init_func
     ptr (callback user-font-init-cb))
    (cairo_user_font_face_set_unicode_to_glyph_func
     ptr (callback user-font-unicode-to-glyph-cb))
    (cairo_user_font_face_set_render_glyph_func
     ptr (callback user-font-render-glyph-cb))))
