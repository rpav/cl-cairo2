(in-package #:cl-cairo2-asd)

(defpackage cl-cairo2
  (:use :common-lisp :cffi :cl-colors :cl-utilities :metabang-bind)
  (:nicknames :cairo)
  (:export

   ;; cairo

   destroy deg-to-rad

   ;; surface

   surface pointer width height pixel-based-p destroy
   create-ps-surface create-pdf-surface create-svg-surface
   create-image-surface create-image-surface-for-data
   create-image-surface-for-array
   image-surface-get-format image-surface-get-width
   image-surface-get-height image-surface-get-data
   image-surface-get-stride image-surface-create-from-png
   image-surface-create-from-png-callback
   image-surface-create-from-png-stream
   surface-write-to-png with-png-surface
   create-surface-from-foreign
   surface-flush surface-finish

   ;; context

   context *context* with-context with-png-file create-context sync
   sync-lock sync sync-unlock sync-reset with-sync-lock save restore
   push-group pop-group pop-group-to-source set-source-rgb
   set-source-rgba clip clip-preserve reset-clip copy-page show-page
   fill-preserve paint paint-with-alpha stroke stroke-preserve
   set-source-color get-line-width set-line-width get-miter-limit
   set-miter-limit get-antialias set-antialias get-fill-rule
   set-fill-rule get-line-cap set-line-cap get-line-join set-line-join
   get-operator set-operator fill-path set-dash get-dash clip-extents
   fill-extents in-fill in-stoke create-ps-context create-pdf-context
   create-svg-context get-target mask-surface set-source-surface

   ;; pattern

   pattern create-rgb-pattern create-rgba-pattern
   create-linear-pattern create-radial-pattern
   create-pattern-for-surface pattern-add-color-stop-rgb
   pattern-add-color-stop-rgba pattern-add-color-stop pattern-get-type
   pattern-set-matrix pattern-get-matrix pattern-set-extend
   pattern-get-extend pattern-set-filet pattern-get-filter set-source
   mask create-color-pattern with-linear-pattern with-radial-pattern
   with-patterns

   ;; path

   new-path new-sub-path close-path arc arc-negative curve-to line-to
   move-to rectangle rel-move-to rel-curve-to rel-line-to text-path
   get-current-point

   ;; text

   select-font-face set-font-size text-extents show-text
   text-x-bearing text-y-bearing
   text-width text-height
   text-x-advance text-y-advance
   get-text-extents
   font-ascent font-descent font-height
   font-max-x-advance font-max-y-advance
   get-font-extents

   glyph-array make-glyph-array glyph-array-add glyph-array-set-glyph
   glyph-array-reset-fill

   set-font-matrix get-font-matrix set-font-options get-font-options
   set-font-face get-font-face set-scaled-font get-scaled-font
   show-glyphs glyph-extents

   ;; font

   font-face scaled-font font-options

   create-font set-font

   create-scaled-font scaled-font-extents scaled-font-text-extents
   scaled-font-glyph-extents scaled-font-get-type
   scaled-font-face

   create-font-options font-options-copy font-options-merge font-options-hash
   font-options-equal font-options-set-antialias font-options-get-antialias
   font-options-set-subpixel-order font-options-get-subpixel-order
   font-options-set-hint-style font-options-get-hint-style
   font-options-set-hint-metrics font-options-get-hint-metrics

   ;; user-font

   user-font-face

   ;; transformations

   translate scale rotate reset-trans-matrix make-trans-matrix
   trans-matrix-xx trans-matrix-yx trans-matrix-xy trans-matrix-yy
   trans-matrix-x0 trans-matrix-y0 trans-matrix-p transform
   set-trans-matrix get-trans-matrix user-to-device
   user-to-device-distance device-to-user device-to-user-distance
   trans-matrix-init-translate trans-matrix-init-scale
   trans-matrix-init-rotate trans-matrix-rotate trans-matrix-scale
   trans-matrix-rotate trans-matrix-invert trans-matrix-multiply
   trans-matrix-distance transform-point

   ;; xlib/xlib-image-interface

   xlib-image-context

   ;; xcb

   create-xcb-surface xcb-surface-set-size

   ;; freetype

   freetype-font-face ft-scaled-font-lock-face ft-scaled-font-unlock-face
   with-ft-scaled-face-locked

   ;; gtk2/gtk2-interface

   gtk2-xlib-context

   )
  (:nicknames :cairo))
