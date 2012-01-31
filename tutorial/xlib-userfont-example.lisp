;;;; This is an example of USER-FONT-FACE derived from the Freetype
;;;; example.  Freetype isn't necessary to use USER-FONT-FACE, of
;;;; course, but it's a useful place to grab actual fonts from.
;;;;
;;;; A few things of note; first, the output of this seems to produce
;;;; darker/heavier-looking glyphs than the built-in cairo renderer.
;;;; This is particularly evident at higher point sizes.
;;;;
;;;; I am not entirely certain why this is, but my current theory is
;;;; that it's because cairo rasterizes freetype fonts itself.  It may
;;;; be possible to get similar-looking output by scaling the alpha in
;;;; the bitmaps before handing it to cairo.  I don't see cairo
;;;; scaling this itself, or rendering in another way, but I haven't
;;;; been through all its code.
;;;;
;;;; Cairo does its own caching, so you don't really need to cache on
;;;; the Lisp side as I had thought before.
;;;;
;;;; Manually pulling glyphs may seem like more work, but as a bonus,
;;;; you have full control over Freetype assets (or you can draw your
;;;; own).
;;;;
;;;; Finally, the metrics passed back to cairo might not be 100%
;;;; perfect.  You may need to fill in more values for some
;;;; functionality.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:load-system :cl-freetype2)
  (asdf:load-system :cl-cairo2-xlib))

(defpackage :cairo-userfont-example
  (:use :common-lisp :cl-cairo2 :freetype2 :freetype2-types)
  (:export #:run))

(in-package :cairo-userfont-example)

;; Set this to a valid font path if none of the below work
(defvar *example-face*)

(defvar *default-face-list*
  '("/usr/share/fonts/corefonts/times.ttf"
    "/usr/share/fonts/freefont-ttf/FreeSerif.ttf"
    "/usr/share/fonts/TTF/luxirr.ttf"))

(defvar *example-text* "Hello cl-cairo2 + freetype + user-font")

(defclass ft-user-font (user-font-face)
  ((ft-face :initarg :ft-face :accessor ft-user-font-face)))

(defun find-default-face ()
  "Try to find a path to a usable font"
  (loop for path in *default-face-list*
        do (when (ft2:check-font-file path)
             (return path))
        finally
           (error "No usable font file found in default list.")))

(defun render-user-glyph (scaled-font glyph ctx text-extents)
  (let* ((user-font (scaled-font-face scaled-font))
         (face (ft-user-font-face user-font)))
    (with-context (ctx)
      (ft2:load-glyph face glyph)
      (let* ((glyphslot (render-glyph face))
             (bitmap (bitmap-convert (ft-glyphslot-bitmap glyphslot) 4))
             (width (ft-bitmap-width bitmap))
             (height (ft-bitmap-rows bitmap))
             (stride (ft-bitmap-pitch bitmap))
             (ptr (ft-bitmap-buffer bitmap))
             (mask (create-image-surface-for-data ptr :a8 width height stride)))
        (setf (text-width text-extents) width)
        (setf (text-height text-extents) (1+ height))
        (mask-surface mask 0 0)))))

(defun render-to-glyph-array (glyph-array text face)
  "Take the string TEXT and calculate the metrics for each glyph using
FACE, and store each into GLYPH-ARRAY."
  (glyph-array-reset-fill glyph-array)
  (do-string-render (face text bmp x y :with-char c)
    (glyph-array-add glyph-array (get-char-index face c) x y)))

(defun run (&key (display ":0") face-path)
  "Make a new window and put some text in it."
  (unless (boundp '*example-face*)
    (setf *example-face* (or face-path (find-default-face))))
  (let* ((context (create-xlib-image-context 500 500
                                             :display-name display
                                             :window-name "Cairo + user-font"))
         (face (ft2:new-face *example-face*))
         (user-font (make-instance 'ft-user-font
                                   :ft-face face
                                   :render-glyph 'render-user-glyph))

         (font-matrix (make-trans-matrix))
         (ctm (make-trans-matrix))
         (options (create-font-options))
         (scaled-font (create-scaled-font user-font font-matrix ctm options))

         (glyph-array (make-glyph-array (length *example-text*))))
    (with-context (context)
      (translate 50 50)
      (set-source-rgb 0 0 0)
      (set-font scaled-font)

      (ft2:set-char-size face (* 16 64) 0 72 72)
      (render-to-glyph-array glyph-array *example-text* face)

      (show-glyphs glyph-array)

      ;; This is an cl-cairo2-xlib-specific thing:
      (sync context))))
