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

(defun win32-create-surface (hwnd hdc)
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

(defun win32-get-surface-dc (surface)
  (let ((hdc (cairo_win32_surface_get_dc (slot-value surface 'pointer))))
	(if (null-pointer-p hdc)
		nil
		hdc)))

(defun win32-get-surface-image (surface)
  (let ((image-surface (cairo_win32_surface_get_image (slot-value surface 'pointer))))
	(if (null-pointer-p image-surface)
		nil
		image-surface)))

(defmacro with-win32-context ((context hwnd hdc width height 
				       &optional (surface-name (gensym)))
			      &body body)
  (once-only (context)
    `(multiple-value-bind (,surface-name ,width ,height)
	 (win32-create-surface ,hwnd ,hdc)
	 (let ((,context (create-context ,surface-name)))
	   (unwind-protect (progn ,@body)
		 (progn
		   (destroy ,context)
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

(defun win32-create-font-face-for-logfontw (logfontw)
  (cairo_win32_font_face_create_for_logfontw logfontw))

(defun win32-create-font-face-for-hfont (hfont)
  (cairo_win32_font_face_create_for_hfont hfont))

#|(defun win32-create-font-face-for-logfontw-hfont (logfontw hfont)
  (cairo_win32_font_face_create_for_logfontw_hfont logfontw hfont))|#

(defun win32-select-font-scaled-font (scaled-font hdc)
  (let ((status (cairo_win32_scaled_font_select_font scaled-font hdc)))
	(if (eq status :success)
		t
		(warn "function returned with status ~A." status))))

(defun win32-done-font-scaled-font (scaled-font)
  (cairo_win32_scaled_font_done_font scaled-font))

(defun win32-get-metrics-facotor-scaled-font (scaled-font)
  (cairo_win32_scaled_font_get_metrics_factor scaled-font))

(defun win32-get-device-to-logical-scaled-font (scaled-font device-to-logical)
  (cairo_win32_scaled_font_get_device_to_logical scaled-font device-to-logical))

;; export manually
(export '(win32-create-surface win32-get-surface-dc win32-get-surface-image
	  with-win32-context win32-create-font-face-for-logfontw
	  win32-create-font-face-for-hfont
	  #|win32-create-font-face-for-logfontw-hfont|#
	  win32-select-font-scaled-font win32-done-font-scaled-font
	  win32-get-metrics-factor-scaled-font
	  win32-get-device-to-logical-scaled-font))
