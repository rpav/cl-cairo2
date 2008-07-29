(in-package :cl-cairo2)

(setf *context* (cairo-x11:create-xlib-image-context 400 200 :display-name ":0"))
(move-to 0 0)
(line-to 400 200)
(set-source-color +green+)
(stroke)

(let* ((display (slot-value *context* 'display))
       (screen (xdefaultscreen display))
       (depth (xdefaultdepth display screen)))
  depth)

(with-foreign-slots ((width height format data
			    byte-order bitmap-unit
			    bitmap-bit-order bitmap-pad
			    depth bytes-per-line
			    bits-per-pixel red-mask
			    green-mask blue-mask
			    xoffset) (slot-value *context* 'ximage) ximage)
  (values width height format data
	  byte-order bitmap-unit
	  bitmap-bit-order bitmap-pad
	  depth bytes-per-line
	  bits-per-pixel red-mask
	  green-mask blue-mask
	  xoffset))
