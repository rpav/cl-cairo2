(in-package #:cl-cairo2-asd)

(defpackage cl-cairo2
  (:use :common-lisp :cffi :cl-colors :cl-utilities)
  (:export

   ;; cairo
    
   destroy deg-to-rad
    
   ;; surface
    
   surface pointer width height get-width get-height pixel-based-p
   destroy create-ps-surface create-pdf-surface create-svg-surface
   create-image-surface image-surface-get-format
   image-surface-get-width image-surface-get-height
   image-surface-get-data
   image-surface-create-from-png surface-write-to-png
    
   ;; context
    
   context with-png-file create-context sync sync-lock sync
   sync-unlock sync-reset with-sync-lock *context* save restore
   push-group pop-group pop-group-to-source set-source-rgb
   set-source-rgba clip clip-preserve reset-clip copy-page show-page
   fill-preserve paint paint-with-alpha stroke stroke-preserve
   set-source-color get-line-width set-line-width get-miter-limit
   set-miter-limit get-antialias set-antialias get-fill-rule
   set-fill-rule get-line-cap set-line-cap get-line-join set-line-join
   get-operator set-operator fill-path set-dash get-dash clip-extents
   fill-extents in-fill in-stoke create-ps-context create-pdf-context
   create-svg-context get-target

   ;;pattern
   
   pattern
   create-rgb-pattern create-rgba-pattern create-linear-pattern create-radial-pattern
   pattern-add-color-stop-rgb pattern-add-color-stop-rgba pattern-add-color-stop
   pattern-get-type pattern-set-matrix pattern-get-matrix pattern-set-extend
   pattern-get-extend pattern-set-filet pattern-get-filter
   set-source create-color-pattern with-linear-pattern with-radial-pattern
   ;; path

   new-path new-sub-path close-path arc arc-negative curve-to line-to
   move-to rectangle rel-move-to rel-curve-to rel-line-to text-path
   get-current-point 

   ;; text

   select-font-face set-font-size text-extents show-text

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

   ;; xlib-image-context

   xlib-image-context create-xlib-image-context

   ;; gtk-context
    
   gtk-context create-gtk-context with-gtk-context)
  (:nicknames :cairo))
