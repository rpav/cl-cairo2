(in-package :cl-cairo2)

;;;;
;;;; a limited interface to certain Xlib functions
;;;;

;;;; types

(defctype display :pointer)
(defctype xid :unsigned-long)	; X Id type
(defctype drawable xid)
(defctype window xid)
(defctype pixmap xid)
(defctype cursor xid)
(defctype colormap xid)
(defctype graphics-context xid)
(defctype visual :pointer)
(defctype xatom :unsigned-long)
(defctype bool :int)

;; constants

(defmacro define-bitmask-constants (&body name-power-pairs)
  "Define a list of constants from name-value pairs, raising 2 to
the power value."
  (labels ((dbc (pairs)
	     (case (length pairs)
	       (0 nil)
	       (1 (error "no power after ~a" (car name-power-pairs)))
	       (t (destructuring-bind (name power &rest rest) pairs
		    `((defconstant ,name (expt 2 ,power))
		       ,@(dbc rest)))))))
    `(progn
       ,@(dbc name-power-pairs))))

(defconstant noeventmask 0)
(define-bitmask-constants 
  keypressmask 0
  keyreleasemask 1
  buttonpressmask 2            
  buttonreleasemask 3          
  enterwindowmask 4            
  leavewindowmask 5            
  pointermotionmask 6          
  pointermotionhintmask 7      
  button1motionmask 8          
  button2motionmask 9          
  button3motionmask 10         
  button4motionmask 11         
  button5motionmask 12         
  buttonmotionmask 13          
  keymapstatemask 14           
  exposuremask 15                
  visibilitychangemask 16        
  structurenotifymask 17         
  resizeredirectmask 18          
  substructurenotifymask 19      
  substructureredirectmask 20    
  focuschangemask 21             
  propertychangemask 23          
  colormapchangemask 23          
  ownergrabbuttonmask 24)


;;;; error code handling
(defmacro check-zero-status (call)
  "Check the return calue of call, if zero, display an error message."
  (with-unique-names (status)
  `(let ((,status ,call))
     (if (zerop ,status)
	 (error "operations ~a returned status (error) ~a" ',call ,status)
	 (values)))))

;;;; display operations

(defcfun ("XOpenDisplay" xopendisplay) display
  (display-name :string))

(defcfun ("XCloseDisplay" xclosedisplay) :int
  (display display))


;;;; defaults for the X11 display & screen

(defcfun ("XDefaultDepth" xdefaultdepth) :int 
  (display display)
  (screen-number :int))

(defcfun ("XDefaultRootWindow" xdefaultrootwindow) window
  (display display))

(defcfun ("XDefaultScreen" xdefaultscreen) :int
  (display display))

(defcfun ("XDefaultVisual" xdefaultvisual) visual
  (display display)
  (screen-number :int))

(defcfun ("XBlackPixel" xblackpixel) :unsigned-long
  (display display)
  (screen-number :int))

(defcfun ("XWhitePixel" xwhitepixel) :unsigned-long
  (display display)
  (screen-number :int))


;;;; graphics contexts

(defcfun ("XDefaultGC" xdefaultgc) graphics-context
  (display display)
  (screen-number :int))

(defcfun ("XCreateGC" xcreategc) graphics-context
  (display display)
  (drawable drawable)
  (valuemask :unsigned-long)
  (xgcvalues :pointer))

(defcfun ("XFreeGC" xfreegc) :int
  (display display)
  (graphics-context graphics-context))

;;;; window and pixmap management

(defcfun ("XMapWindow" xmapwindow) :int
  (display display)
  (window window))

(defcfun ("XCreateSimpleWindow" xcreatesimplewindow) window
  (display display)
  (parent window)
  (x :int)
  (y :int)
  (width :unsigned-int)
  (height :unsigned-int)
  (border-width :unsigned-int)
  (border :unsigned-long)
  (background :unsigned-long))

(defcfun ("XCreateWindow" xcreatewindow) window
  (display display)
  (parent window)
  (x :int)
  (y :int)
  (width :unsigned-int)
  (height :unsigned-int)
  (border-width :unsigned-int)
  (depth :int)
  (class :unsigned-int)
  (visual visual)
  (valuemask :unsigned-long)
  (attributes :pointer))

(defcstruct xsetwindowattributes
  (background-pixmap pixmap)
  (background-pixel :unsigned-long)
  (border-pixmap pixmap)
  (border-pixel :unsigned-long)
  (bit-gravity :int)
  (win-gravity :int)
  (backing-store :int)
  (backing-planes :unsigned-long)
  (backing-pixel :unsigned-long)
  (save-under bool)
  (event-mask :long)
  (do-not-propagate_mask :long)
  (override-redirect bool)
  (colormap colormap)
  (cursor cursor))

(define-bitmask-constants
  CWBackPixmap       0
  CWBackPixel        1
  CWBorderPixmap     2
  CWBorderPixel      3
  CWBitGravity       4
  CWWinGravity       5
  CWBackingStore     6
  CWBackingPlanes    7
  CWBackingPixel     8
  CWOverrideRedirect 9
  CWSaveUnder        10
  CWEventMask        11
  CWDontPropagate    12
  CWColormap         13
  CWCursor           14)

(defcfun ("XChangeWindowAttributes" xchangewindowattributes) :int
  (display display)
  (window window)
  (valuemask :unsigned-long)
  (attributes :pointer))

(defcfun ("XDestroyWindow" xdestroywindow) :int
  (display display)
  (window window))

(defcfun ("XCreatePixmap" xcreatepixmap) pixmap
  (display display)
  (drawable drawable)
  (width :unsigned-int)
  (height :unsigned-int)
  (depth :unsigned-int))

(defcfun ("XFreePixmap" xfreepixmap) :int
  (display display)
  (pixmap pixmap))
  
(defcfun ("XSelectInput" xselectinput) :int
  (display display)
  (window window)
  (event-mask :long))

(defcfun ("XCopyArea" xcopyarea) :int
  (display display)
  (source drawable)
  (destination drawable)
  (graphics-context graphics-context)
  (source-x :int)
  (source-y :int)
  (width :unsigned-int)
  (height :unsigned-int)
  (destination-x :int)
  (destination-y :int))

(defcfun ("XSetGraphicsExposures" xsetgraphicsexposures) :int
  (display display)
  (graphics-context graphics-context)
  (graphics-exposures bool))

		      
;; synchronization & threads 

(defcfun ("XInitThreads" xinitthreads) :int)

(defcfun ("XLockDisplay" xlockdisplay) :int
  (display display))

(defcfun ("XUnlockDisplay" xunlockdisplay) :int
  (display display))

(defcfun ("XSynchronize" xsynchronize) :int
  (display display)
  (onoff :int))

(defcfun ("XFlush" xflush) :int
  (display display))

(defcfun ("XSync" xsync) :int
  (display display)
  (discard :int))

;; atoms & protocols 

(defcfun ("XInternAtom" xinternatom) xatom
  (display display)
  (atom-name :string)
  (only-if-exists :int))

(defcfun ("XSetWMProtocols" xsetwmprotocols) :int
  (display display)
  (window window)
  (protocols :pointer)
  (count :int))


;; events

(defcstruct xanyevent
  (type :int)
  (serial :unsigned-long)
  (send-event bool)
  (display display)
  (window window))

(defcstruct xexposeevent
  (type :int)
  (serial :unsigned-long)
  (send-event bool)
  (display display)
  (drawable drawable)
  (x :int)
  (y :int)
  (width :int)
  (height :int)
  (count :int)
  (major-code :int)
  (minor-code :int))

(defcstruct xdestroywindowevent
  (type :int)
  (serial :unsigned-long)
  (send-event bool)
  (display display)
  (event window)
  (window window))

(defcstruct xclientmessageevent
  (type :int)
  (serial :unsigned-long)
  (send-event bool)
  (display display)
  (window window)
  (message-type xatom)
  (format :int)
  ;; we only use first field, union of message data is not included
  (data0 :unsigned-long))

(defcstruct xvisibilityevent
  (type :int)
  (serial :unsigned-long)
  (send-event bool)
  (display display)
  (window window)
  (state :int))

(defcfun ("XNextEvent" xnextevent) :int
  (display display)
  (event-return :pointer))

(defcfun ("XSendEvent" xsendevent) :int
  (display display)
  (window window)
  (propagate bool)
  (event-mask :long)
  (xevent :pointer))

;; hints & misc

(defcstruct xsizehints
  (flags :long)	    ; marks which fields in this structure are defined
  (x :int)				; Obsolete
  (y :int)				; Obsolete
  (width :int)				; Obsolete
  (height :int)				; Obsolete
  (min-width :int)
  (min-height :int)
  (max-width :int)
  (max-height :int)
  (min-aspect-x :int)			; numerator
  (min-aspect-y :int)			; denominator
  (max-aspect-x :int)			; numerator
  (max-aspect-y :int)			; denominator
  (base-width :int)
  (base_height :int)
  (win_gravity :int))

(define-bitmask-constants 
    USPosition 0
  USSize 1
  PPosition 2
  PSize 3
  PMinSize 4
  PMaxSize 5
  PResizeInc 6
  PAspect 7
  PBaseSize 8
  PWinGravity 9)

(defcfun ("XAllocSizeHints" xallocsizehints) :pointer)

(defcfun ("XSetWMNormalHints" xsetwmnormalhints) :void
  (display display)
  (window window)
  (hints :pointer))

(defcfun ("XStoreName" xstorename) :int
  (display display)
  (window window)
  (window-name :string))

(defcfun ("XFree" xfree) :int
  (data :pointer))


;; extensions

(defcfun ("XAddExtension" xaddextension) :pointer
  (display display))

(defcstruct xextcodes
  (extensions :int)
  (major-opcode :int)
  (first-event :int)
  (first-error :int))

;; image manipulation

(cffi:defcstruct XImage
        (width :int)
        (height :int)
        (xoffset :int)
        (format :int)
        (data :pointer)
        (byte-order :int)
        (bitmap-unit :int)
        (bitmap-bit-order :int)
        (bitmap-pad :int)
        (depth :int)
        (bytes-per-line :int)
        (bits-per-pixel :int)
        (red-mask :unsigned-long)
        (green-mask :unsigned-long)
        (blue-mask :unsigned-long)
        (obdata :pointer)
	;; funcs
	(create-image :pointer)
	(destroy-image :pointer)
	(get-pixel :pointer)
	(put-pixel :pointer)
	(sub-image :pointer)
	(add-pixel :pointer))

(defcfun ("XInitImage" xinitimage) :int
  (ximage :pointer))

(defcfun ("XPutImage" xputimage) :int
  (display display)
  (drawable drawable)
  (graphics-context graphics-context)
  (ximage :pointer)
  (src-x :int)
  (src-y :int)
  (dest-x :int)
  (dest-y :int)
  (width :unsigned-int)
  (height :unsigned-int))

;; call xinitthreads

(xinitthreads)


;; various higher level functions

(defun set-window-size-hints (display window 
			      min-window-width max-window-width
			      min-window-height max-window-height)
  ;; set size hints on window (most window managers will respect this)
  (let ((hints (xallocsizehints)))
    (with-foreign-slots ((flags x y min-width min-height
				max-width max-height)
			 hints
			 xsizehints)
      ;; we only set the first four values because old WM's might
      ;; get confused if we don't, they should be ignored
      (setf flags (logior pminsize pmaxsize)
	    x 0
	    y 0
	    ;; we don't need to set the following, but some WMs go
	    ;; crazy if we don't
	    (foreign-slot-value hints 'xsizehints 'width) max-window-width
	    (foreign-slot-value hints 'xsizehints 'height) max-window-height
	    ;; set desired min/max width/height
	    min-width min-window-width
	    max-width max-window-width
	    min-height min-window-height
	    max-height max-window-height)
      (xsetwmnormalhints display window hints)
      (xfree hints))))

(defun create-window (display parent width height class visual background-pixel
		      event-mask &optional (backing-store t))
  "Create an x11 window, placed at 0 0, with the given attributes.
For internal use in the cl-cairo2 package."
  ;; call xcreatewindow with attributes
  (with-foreign-object (attributes 'xsetwindowattributes)
    (setf (foreign-slot-value attributes 'xsetwindowattributes 'event-mask)
	  event-mask
	  (foreign-slot-value attributes 'xsetwindowattributes 'background-pixel)
	  background-pixel
	  (foreign-slot-value attributes 'xsetwindowattributes 'backing-store)
	  (if backing-store 1 0))
    (xcreatewindow display parent 0 0 width height 
		   0 			; zero border width
		   0 			; depth - copy from parent
		   (ecase class
		     (copyfromparent 0)
		     (inputoutput 1)
		     (inputonly 2))	; class
		   visual
		   (if (eq class 'inputonly)
		       cweventmask
		       (logior cwbackpixel cwbackingstore cweventmask))
		   attributes)))
