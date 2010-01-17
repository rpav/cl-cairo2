;;;
;;; Lissajous curve demo using Win32 surface
;;;
;;; Note: running this code via the default swank server wouldn't work.
;;; One sure way to run this code is to start your Lisp system from a DOS
;;; command window, load this code from the REPL and then call lissajous-win:run.
;;;

(defpackage :lissajous-win
  (:use #:cl #:cffi #:cl-cairo2)
  (:shadowing-import-from
   #:cl-cairo2 #:context #:arc #:fill-path #:get-miter-limit #:line-to #:mask #:set-miter-limit #:rectangle)
  (:export
   #:run))

(in-package :lissajous-win)

;;; load Win DLLs, define Win consts and functions

(load-foreign-library '(:default "gdi32"))
(load-foreign-library '(:default "kernel32"))
(load-foreign-library '(:default "user32"))

(cl:defconstant +cs-hredraw+ 2)
(cl:defconstant +cs-vredraw+ 1)
(cl:defconstant +ws-overlappedwindow+ #xcf0000)
(cl:defconstant +cw-usedefault+ #x80000000)
(cl:defconstant +white-brush+ 0)
(cl:defconstant +sw-shownormal+ 1)
(cl:defconstant +wm-create+ 1)
(cl:defconstant +wm-paint+ 15)
(cl:defconstant +wm-destroy+ 2)
(cl:defconstant +dt-center+ 1)
(cl:defconstant +dt-vcenter+ 4)
(cl:defconstant +dt-singleline+ 32)

(cffi:defcstruct wndclassexw 
  (cbSize :unsigned-int) 
  (style :unsigned-int) 
  (lpfnWndProc :pointer) 
  (cbClsExtra :int) 
  (cbWndExtra :int) 
  (hInstance :pointer) 
  (hIcon :pointer) 
  (hCursor :pointer) 
  (hbrBackground :pointer) 
  (lpszMenuName :pointer) 
  (lpszClassName :pointer) 
  (hIconSm :pointer))

(cffi:defcstruct rect 
  (left :long) 
  (top :long) 
  (right :long) 
  (bottom :long))

(cffi:defcstruct paintstruct 
  (hdc :pointer) 
  (fErase :INT) 
  (rcPaint RECT) 
  (fRestore :INT) 
  (fIncUpdate :INT) 
  (rgbReserved :pointer))

(cffi:defcstruct point 
  (x :long) 
  (y :long))

(cffi:defcstruct msg 
  (hwnd :pointer) 
  (message :unsigned-int) 
  (wParam :unsigned-int) 
  (lParam :long) 
  (time :unsigned-long) 
  (pt point))

(cffi:defcfun ("GetModuleHandleW" get-module-handle-w) :pointer 
  (arg0 :pointer))

(cffi:defcfun ("LoadIconW" load-icon-w) :pointer 
  (arg0 :pointer) 
  (arg1 :pointer))

(cffi:defcfun ("LoadCursorW" load-cursor-w) :pointer 
  (arg0 :pointer) 
  (arg1 :pointer))

(cffi:defcfun ("GetStockObject" get-stock-object) :pointer 
  (arg0 :int))

(cffi:defcfun ("RegisterClassExW" register-class-ex-w) :unsigned-short 
  (arg0 :pointer))

(cffi:defcfun ("GetLastError" get-last-error) :unsigned-long)

(cffi:defcfun ("CreateWindowExW" create-window-ex-w) :pointer 
  (arg0 :unsigned-long) 
  (arg1 :pointer) 
  (arg2 :pointer) 
  (arg3 :unsigned-long) 
  (arg4 :int) 
  (arg5 :int) 
  (arg6 :int) 
  (arg7 :int) 
  (arg8 :pointer) 
  (arg9 :pointer) 
  (arg10 :pointer) 
  (arg11 :pointer))

(cffi:defcfun ("DefWindowProcW" def-window-proc-w) :long 
  (arg0 :pointer) 
  (arg1 :unsigned-int) 
  (arg2 :unsigned-int) 
  (arg3 :long))

(cffi:defcfun ("DestroyWindow" destroy-window) :int 
  (arg0 :pointer))

(cffi:defcfun ("UnregisterClassW" unregister-class-w) :int 
  (arg0 :pointer) 
  (arg1 :pointer))

(cffi:defcfun ("ShowWindow" show-window) :int 
  (arg0 :pointer) 
  (arg1 :int))

(cffi:defcfun ("SetForegroundWindow" set-foreground-window) :int 
  (arg0 :pointer))

(cffi:defcfun ("GetMessageW" get-message-w) :int 
  (arg0 :pointer) 
  (arg1 :pointer) 
  (arg2 :unsigned-int) 
  (arg3 :unsigned-int))

(cffi:defcfun ("TranslateMessage" translate-message) :int 
  (arg0 :pointer))

(cffi:defcfun ("DispatchMessageW" dispatch-message-w) :long 
  (arg0 :pointer))

(cffi:defcfun ("MoveWindow" move-window) :int 
  (arg0 :pointer) 
  (arg1 :int) 
  (arg2 :int) 
  (arg3 :int) 
  (arg4 :int) 
  (arg5 :int))

(cffi:defcfun ("BeginPaint" begin-paint) :pointer 
  (arg0 :pointer) 
  (arg1 :pointer))

(cffi:defcfun ("EndPaint" end-paint) :int 
  (arg0 :pointer) 
  (arg1 :pointer))

(cffi:defcfun ("GetClientRect" get-client-rect) :int 
  (arg0 :pointer) 
  (arg1 :pointer))

(cffi:defcfun ("PostQuitMessage" post-quit-message) :void 
  (arg0 :int))

;;;  Lissajous curves

(defparameter x 80)
(defparameter y 16)
(defparameter size 500)
(defparameter margin 20)
(defparameter a 9)
(defparameter b 8)
(defparameter delta (/ pi 2))
(defparameter density 2000)

(defun show-text-aligned (text x y &optional (x-align 0.5) (y-align 0.5))
  "Show text aligned relative to (x,y)."
  (multiple-value-bind (x-bearing y-bearing width height)
	  (text-extents text)
	(move-to (- x (* width x-align) x-bearing)
			 (- y (* height y-align) y-bearing))
	(show-text text)))

(defun draw-lissajous (hwnd hdc ps)
  (declare (ignore ps))
  (with-win32-context (hwnd hdc width height)
	(let ((*default-foreign-encoding* :utf-8)
		  (size (if (< width height) width height)))
	  ;; pastel blue background
	  (rectangle 0 0 width height)
	  (set-source-rgb 0.9 0.9 1)
	  (fill-path)
	  ;; Lissajous curves, blue
	  (labels ((stretch (x) (+ (* (1+ x) (- (/ size 2) margin)) margin)))
		(move-to (stretch (sin delta)) (stretch 0))
		(dotimes (i density)
		  (let* ((v (/ (* i pi 2) density))
				 (x (sin (+ (* a v) delta)))
				 (y (sin (* b v))))
			(line-to (stretch x) (stretch y)))))
	  (close-path)
	  (set-line-width .5)
	  (set-source-rgb 0 0 1)
	  (stroke)
	  ;; "cl-cairo2" in Arial bold to the center
	  (select-font-face "Arial" :normal :bold)
	  (set-font-size 100)
	  (set-source-rgba 1 0.75 0 0.5)	; orange
	  (show-text-aligned "cl-cairo2" (/ size 2) (/ size 2)))))

;;;

(defmacro make-int-resource (id)
  `(make-pointer ,id))

(defparameter *null* (null-pointer))
(defparameter *hinstance* (get-module-handle-w *null*))
(defparameter *idi-application* (make-int-resource 32512))
(defparameter *idc-arrow* (make-int-resource 32512))
(defparameter *appname*	"Lissajous-win")

(defmacro with-begin-paint ((hwnd hdc ps) &body body)
  `(with-foreign-object (,ps 'paintstruct)
	 (let ((,hdc (begin-paint ,hwnd ,ps)))
	   ,@body
	   (end-paint ,hwnd ,ps))))

(defcallback wind-proc :long ((hwnd :pointer) (msg :unsigned-int) (wparam :unsigned-int) (lparam :long))
  (let ((rval 0))
	(cond ((eql msg +wm-create+)
		   (move-window hwnd x y size size 1))
		  ((eql msg +wm-paint+)
		   (with-begin-paint (hwnd hdc ps)
			 (draw-lissajous hwnd hdc ps)))
		   ((eql msg +wm-destroy+)
			(post-quit-message 0))
		  (t
		   (setf rval (def-window-proc-w hwnd msg wparam lparam))))
	rval))

(defmacro fsv (ptr struct slot)
  `(foreign-slot-value ,ptr ,struct ,slot))

(defun register-window-class (wc-name)
  (let ((wnd-proc (get-callback 'wind-proc)))
	(with-foreign-object (wc 'wndclassexw)
	  (setf (fsv wc 'wndclassexw 'cbSize)			(foreign-type-size 'wndclassexw)
			(fsv wc 'wndclassexw 'style)			(logior +cs-hredraw+ +cs-vredraw+)
			(fsv wc 'wndclassexw 'lpfnWndProc)		wnd-proc
			(fsv wc 'wndclassexw 'cbClsExtra)		0
			(fsv wc 'wndclassexw 'cbWndExtra)		0
			(fsv wc 'wndclassexw 'hInstance)		*hinstance*
			(fsv wc 'wndclassexw 'hIcon)			(load-icon-w *null* *idi-application*)
			(fsv wc 'wndclassexw 'hIconSm)			(load-icon-w *null* *idi-application*)
			(fsv wc 'wndclassexw 'hCursor)			(load-cursor-w *null* *idc-arrow*)
			(fsv wc 'wndclassexw 'hbrBackground)	(get-stock-object +white-brush+)
			(fsv wc 'wndclassexw 'lpszClassName)	wc-name
			(fsv wc 'wndclassexw 'lpszMenuName)		*null*)
	  (let ((wcatom (register-class-ex-w wc)))
		(if (zerop wcatom)
			(error "register-class-ex-w failed:~A~%" (get-last-error))
			wcatom)))))

(defun cleanup (wc-name hwnd)
  (when (not (null-pointer-p hwnd))
	(destroy-window hwnd))
  (unregister-class-w wc-name *hinstance*))

(defun create-window (wc-name wnd-name)
  (let ((hwnd (create-window-ex-w
			   0 wc-name wnd-name +ws-overlappedwindow+
			   (- 0 +cw-usedefault+) (- 0 +cw-usedefault+) (- 0 +cw-usedefault+) (- 0 +cw-usedefault+)
			   *null* *null* *hinstance* *null*)))
	(when (null-pointer-p hwnd)
	  (cleanup wc-name *null*)
	  (error "create-window-ex-w failed:~A~%" (get-last-error)))
	hwnd))

(defun message-loop ()
  (with-foreign-object (msg 'msg)
	(do ((gm (get-message-w msg *null* 0 0)
			 (get-message-w msg *null* 0 0)))
		((zerop gm) (fsv msg 'msg 'wParam))
	  (translate-message msg)
	  (dispatch-message-w msg))))

(defun run ()
  (let ((*default-foreign-encoding* :utf-16))
	(with-foreign-string (app-name *appname*)
	  (register-window-class app-name)
	  (with-foreign-string (wnd-name *appname*)
		(let ((hwnd (create-window app-name wnd-name))
			  (rval -1))
		  (when hwnd
			(show-window hwnd +sw-shownormal+)
			(set-foreground-window hwnd)
			(setf rval (message-loop))
			(cleanup app-name hwnd))
		  rval)))))
