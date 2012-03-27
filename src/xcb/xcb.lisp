(in-package :cl-cairo2)

(defun create-xcb-surface (drawable &key visual height width)
  "Create a surface for the given XCB.CLX:DRAWABLE, optionally
specifying VISUAL, HEIGHT, and WIDTH.

VISUAL may be either a VISUAL-INFO or a VISUAL-ID, or NIL if DRAWABLE
is a WINDOW."
  (when (null visual)
    (unless (xlib:window-p drawable)
      (error "You must specify VISUAL unless DRAWABLE is a WINDOW")))
  (let ((visual (etypecase visual
                  (xlib:visual-info visual)
                  (integer (xlib:x-find-visual-info drawable visual))
                  (null (xlib:x-find-visual-info drawable
                                                 (xlib:window-visual drawable)))))
        (width (or width (xlib:drawable-width drawable)))
        (height (or height (xlib:drawable-height drawable))))
    (let* ((display (xlib:display-for drawable))
           (c (xlib:display-ptr-xcb display))
           (ptr (cairo_xcb_surface_create c (xlib:xid drawable)
                                          (xlib:visual-info-ptr visual)
                                          width height)))
      (unless (find 'cairo-xcb-destroy-device
                    (xlib:close-display-hook display))
        (setf (getf (xlib:display-plist display) 'cairo-device)
              (cairo_surface_get_device ptr))
        (xlib:add-hook (xlib:close-display-hook display)
                       'cairo-xcb-destroy-device))
      (create-surface-from-foreign ptr))))

(defun xcb-surface-set-size (surface width height)
  (cairo_xcb_surface_set_size (get-pointer surface) width height))

(defun cairo-xcb-destroy-device (display valid-p)
  (declare (ignore valid-p))
  (let ((device-ptr (getf (xlib:display-plist display) 'cairo-device)))
    (cairo_device_finish device-ptr)))
