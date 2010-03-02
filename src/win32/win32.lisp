(in-package :cl-cairo2)

(load-foreign-library '(:default "user32"))

;;;
;;; win32 surfaces
;;;

(cffi:defcstruct RECT 
  (left :long) 
  (top :long) 
  (right :long) 
  (bottom :long))

(cffi:defcfun ("GetClientRect" get-client-rect) :int 
  (arg0 :pointer) 
  (arg1 :pointer))

(defun win32-surface-create (hwnd hdc)
  (let ((width 0)
        (height 0)
        (surface (make-instance 'surface :width 0 :height 0 :pixel-based-p t)))
    ;;
    (setf (slot-value surface 'pointer) (cairo_win32_surface_create hdc))
    ;;
    (with-foreign-object (r 'rect)
      (if (eql (get-client-rect hwnd r) 1)
          (let ((w (foreign-slot-value r 'rect 'right))
                (h (foreign-slot-value r 'rect 'bottom)))
            (setf (slot-value surface 'width) w
                  (slot-value surface 'height) h
                  width w
                  height h))
          (warn "failed to get window size")))
    ;;
    (values surface width height)))

(define-compiler-macro win32-create-surface (&rest args)
  `(deprecate win32-create-surface win32-surface-create ,@args))

(defun win32-surface-get-dc (surface)
  (let ((hdc (cairo_win32_surface_get_dc (slot-value surface 'pointer))))
    (if (null-pointer-p hdc)
        nil
        hdc)))

(define-compiler-macro win32-get-surface-dc (&rest args)
  `(deprecate win32-get-surface-dc win32-surface-get-dc ,@args))

(defun win32-surface-get-image (surface)
  (let ((image-surface (cairo_win32_surface_get_image (slot-value surface 'pointer))))
    (if (null-pointer-p image-surface)
        nil
        image-surface)))

(define-compiler-macro win32-get-surface-image (&rest args)
  `(deprecate win32-get-surface-image win32-surface-get-image ,@args))

(defmacro with-win32-context ((hwnd hdc width height &optional (surface-name (gensym)))
                              &body body)
  `(multiple-value-bind (,surface-name ,width ,height)
       (win32-surface-create ,hwnd ,hdc)
     (let ((*context* (create-context ,surface-name)))
       (unwind-protect (progn ,@body)
         (progn
           (destroy *context*)
           (destroy ,surface-name))))))

;;;
;;; win32 fonts
;;;

(cffi:defcstruct LOGFONTW 
  (lfHeight :long) 
  (lfWidth :long) 
  (lfEscapement :long) 
  (lfOrientation :long) 
  (lfWeight :long) 
  (lfItalic :unsigned-char) 
  (lfUnderline :unsigned-char) 
  (lfStrikeOut :unsigned-char) 
  (lfCharSet :unsigned-char) 
  (lfOutPrecision :unsigned-char) 
  (lfClipPrecision :unsigned-char) 
  (lfQuality :unsigned-char) 
  (lfPitchAndFamily :unsigned-char) 
  (lfFaceName :pointer))

(defun win32-font-face-create-for-logfontw (logfontw)
  (cairo_win32_font_face_create_for_logfontw logfontw))

(define-compiler-macro win32-create-font-face-for-logfontw (&rest args)
  `(deprecate win32-create-font-face-for-logfontw win32-font-face-create-for-logfontw ,@args))

(defun win32-font-face-create-for-hfont (hfont)
  (cairo_win32_font_face_create_for_hfont hfont))

(define-compiler-macro win32-create-font-face-for-hfont (&rest args)
  `(deprecate win32-create-font-face-for-hfont win32-font-face-create-for-hfont ,@args))

#|(defun win32-font-face-create-for-logfontw-hfont (logfontw hfont)
  (cairo_win32_font_face_create_for_logfontw_hfont logfontw hfont))

(define-compiler-macro win32-create-font-face-for-logfontw-hfont (&rest args)
  `(deprecate win32-create-font-face-for-logfontw-hfont win32-font-face-create-for-logfontw-hfont ,@args))|#

(defun win32-scaled-font-select-font (scaled-font hdc)
  (let ((status (cairo_win32_scaled_font_select_font scaled-font hdc)))
    (if (eq status :success)
        t
        (warn "function returned with status ~A." status))))

(define-compiler-macro win32-select-font-scaled-font (&rest args)
  `(deprecate win32-select-font-scaled-font win32-scaled-font-select-font ,@args))

(defun win32-scaled-font-done-font (scaled-font)
  (cairo_win32_scaled_font_done_font scaled-font))

(define-compiler-macro win32-done-font-scaled-font (&rest args)
  `(deprecate win32-done-font-scaled-font win32-scaled-font-done-font ,@args))

(defun win32-scaled-font-get-metrics-factor (scaled-font)
  (cairo_win32_scaled_font_get_metrics_factor scaled-font))

(define-compiler-macro win32-get-metrics-factor-scaled-font (&rest args)
  `(deprecate win32-get-metrics-factor-scaled-font win32-scaled-font-get-metrics-factor ,@args))

(defun win32-scaled-font-get-device-to-logical (scaled-font device-to-logical)
  (cairo_win32_scaled_font_get_device_to_logical scaled-font device-to-logical))

(define-compiler-macro win32-get-device-to-logical-scaled-font (&rest args)
  `(deprecate win32-get-device-to-logical-scaled-font win32-scaled-font-get-device-to-logical ,@args))

;; export manually
(export '(win32-create-surface                      win32-surface-create
          win32-get-surface-dc                      win32-surface-get-dc
          win32-get-surface-image                   win32-surface-get-image
          win32-create-font-face-for-logfontw       win32-font-face-create-for-logfontw
          win32-create-font-face-for-hfont          win32-font-face-create-for-hfont
        #|win32-create-font-face-for-logfontw-hfont win32-font-face-create-for-logfontw-hfont|#
          win32-select-font-scaled-font             win32-scaled-font-select-font
          win32-done-font-scaled-font               win32-scaled-font-done-font
          win32-get-metrics-factor-scaled-font      win32-scaled-font-get-metrics-factor
          win32-get-device-to-logical-scaled-font   win32-scaled-font-get-device-to-logical
          with-win32-context))
