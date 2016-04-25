;;;;
;;;; This probably seems more complicated than you want for basic font
;;;; rendering.  Well, it is.  This requires a good bit of custom
;;;; framework, but you can (presumably) get much faster font
;;;; rendering once it's in place.
;;;;
;;;; There are a number of comments below which make things less
;;;; readable, but are notable if you're using this interface.
;;;;
;;;; Of special note perhaps is GLYPH-ARRAY.  The goal of this is to
;;;; give an efficient interface to arrays of cairo_glyph_t, but
;;;; that's not really demonstrated here.  In a real app, you can
;;;; probably make a single GLYPH-ARRAY of reasonable size, and reuse
;;;; this to write out chunks of text, much like a static I/O buffer.
;;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:load-system :cl-freetype2)
  (asdf:load-system :cl-cairo2-xlib))

(defpackage :cairo-ft2-example
  (:use :common-lisp :cl-cairo2)
  (:export #:run))

(in-package :cairo-ft2-example)

;; Set this to a valid font path if none of the below work
(defvar *example-face*)

(defvar *default-face-list*
  '("/usr/share/fonts/corefonts/times.ttf"
    "/usr/share/fonts/freefont-ttf/FreeSerif.ttf"
    "/usr/share/fonts/TTF/luxirr.ttf"))

(defparameter *example-text* "Hello cl-cairo2 + freetype")

(defun find-default-face ()
  "Try to find a path to a usable font"
  (loop for path in *default-face-list*
        do (when (ft2:check-font-file path)
             (return path))
        finally
           (error "No usable font file found in default list.")))

(defun glyph-index-load-render (face char vertical-p)
  "Return the glyph index instead of a bitmap"
  (multiple-value-bind (bitmap advance top left)
      (ft2:default-load-render face char vertical-p)

    ;; We don't care about bitmap here, but it's almost always
    ;; rendered to get the metrics anyway---so this isn't inefficient
    ;; because of that.  It's inefficient because we don't cache
    ;; the results.
    (declare (ignore bitmap))
    (values (ft2:get-char-index face char) advance top left)))

(defun render-to-glyph-array (glyph-array text scaled-font)
  "Take the string TEXT and calculate the metrics for each glyph using
SCALED-FONT, and store each into GLYPH-ARRAY."

  ;; This assumes things don't happen in the background; with an
  ;; xlib-image-surface, they probably won't.  With other surfaces
  ;; (e.g., gtk-cairo-context) they might in some situations:
  (with-ft-scaled-face-locked (face scaled-font)
    (glyph-array-reset-fill glyph-array)

    ;; Note that it doesn't appear to matter what you set the scaling
    ;; matrices to, or if you use CAIRO:SET-FONT-SIZE, you need to do
    ;; this manually.  It also obviates CAIRO:SET-FONT-SIZE.
    (ft2:set-char-size face (* 16 64) 0 72 72)

    ;; Note that cairo wants baseline Y and calculates bitmap
    ;; offsets itself:
    (ft2:do-string-render (face text index x y
                           :load-function #'glyph-index-load-render
                           :baseline-y-p t
                           :offsets-p nil)
      (glyph-array-add glyph-array index x y))))

(defun run (&key (display ":0") face-path)
  "Make a new window and put some text in it."
  (unless (boundp '*example-face*)
    (setf *example-face* (or face-path (find-default-face))))
  (let ((context (create-xlib-image-context 500 500
                                            :display-name display
                                            :window-name "Cairo + Freetype2"))
        ;; Basic FONT-FACE:
        (font (create-font (ft2:new-face *example-face*)))

        ;; For making a SCALED-FONT, though matrices don't seem to matter:
        (font-matrix (make-trans-matrix))
        (ctm (make-trans-matrix))
        (options (create-font-options))

        ;; For placing glyphs:
        (glyph-array (make-glyph-array (length *example-text*))))

    ;; We need a SCALED-FONT so we can lock it and get a sized
    ;; FT_Face back.  We could size and use our FT_Face before we
    ;; do any of this, but that's not particularly realistic.
    (let ((scaled-font (create-scaled-font font font-matrix ctm options)))
      (with-context (context)
        (set-source-rgb 1 0 0)
        (paint)

        (render-to-glyph-array glyph-array *example-text* scaled-font)

        (translate 50 50)
        (set-source-rgb 0 0 0)
        (set-font scaled-font)
        (show-glyphs glyph-array)

        ;; This is an cl-cairo2-xlib-specific thing:
        (sync context)))))
