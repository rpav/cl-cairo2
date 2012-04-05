(in-package :cl-cairo2)

(defmacro exporting-table (name definition)
  `(progn
     (defparameter ,name ,definition)
     (export ',name)))
 ;    (dolist (i ,name)
      ; (export (car i))
       ;(export (cdr i)))))

(exporting-table table-format
  '((:CAIRO_FORMAT_ARGB32 . :argb32)
    (:CAIRO_FORMAT_RGB24 . :rgb24)
    (:CAIRO_FORMAT_A8 . :a8)
    (:CAIRO_FORMAT_A1 . :a1)
    (:CAIRO_FORMAT_RGB16_565 . :rgb16_565)
    (:CAIRO_FORMAT_RGB30 . :rgb30)))

(exporting-table table-antialias
  '((:CAIRO_ANTIALIAS_DEFAULT . :default)
    (:CAIRO_ANTIALIAS_NONE . :none)
    (:CAIRO_ANTIALIAS_GRAY . :gray)
    (:CAIRO_ANTIALIAS_SUBPIXEL . :subpixel)
    (:CAIRO_ANTIALIAS_FAST . :fast)
    (:CAIRO_ANTIALIAS_GOOD . :good)
    (:CAIRO_ANTIALIAS_BEST . :best)))

(exporting-table table-fill-rule
  '((:CAIRO_FILL_RULE_WINDING . :winding)
    (:CAIRO_FILL_RULE_EVEN_ODD . :odd)))

(exporting-table table-line-cap
  '((:CAIRO_LINE_CAP_BUTT . :butt)
    (:CAIRO_LINE_CAP_ROUND . :round)
    (:CAIRO_LINE_CAP_SQUARE . :square)))

(exporting-table table-line-join
  '((:CAIRO_LINE_JOIN_MITER . :miter)
    (:CAIRO_LINE_JOIN_ROUND . :round)
    (:CAIRO_LINE_JOIN_BEVEL . :bevel)))

(exporting-table table-operator
  '((:CAIRO_OPERATOR_CLEAR . :clear)
    (:CAIRO_OPERATOR_SOURCE . :source)
    (:CAIRO_OPERATOR_OVER . :over)
    (:CAIRO_OPERATOR_IN . :in)
    (:CAIRO_OPERATOR_OUT . :out)
    (:CAIRO_OPERATOR_ATOP . :atop)
    (:CAIRO_OPERATOR_DEST . :dest)
    (:CAIRO_OPERATOR_DEST_OVER . :dest-over)
    (:CAIRO_OPERATOR_DEST_IN . :dest-in)
    (:CAIRO_OPERATOR_DEST_OUT . :dest-out)
    (:CAIRO_OPERATOR_DEST_ATOP . :dest-atop)
    (:CAIRO_OPERATOR_XOR . :xor)
    (:CAIRO_OPERATOR_ADD . :add)
    (:CAIRO_OPERATOR_SATURATE . :saturate)
    (:CAIRO_OPERATOR_MULTIPLY . :multiply)
    (:CAIRO_OPERATOR_SCREEN . :screen)
    (:CAIRO_OPERATOR_OVERLAY . :overlay)
    (:CAIRO_OPERATOR_DARKEN . :darken)
    (:CAIRO_OPERATOR_LIGHTEN . :lighten)
    (:CAIRO_OPERATOR_COLOR_DODGE . :dodge)
    (:CAIRO_OPERATOR_COLOR_BURN . :burn)
    (:CAIRO_OPERATOR_HARD_LIGHT . :hard-light)
    (:CAIRO_OPERATOR_SOFT_LIGHT . :soft-light)
    (:CAIRO_OPERATOR_DIFFERENCE . :difference)
    (:CAIRO_OPERATOR_EXCLUSION . :exclusion)
    (:CAIRO_OPERATOR_HSL_HUE . :hue)
    (:CAIRO_OPERATOR_HSL_SATURATION . :saturation)
    (:CAIRO_OPERATOR_HSL_COLOR . :color)
    (:CAIRO_OPERATOR_HSL_LUMINOSITY . :luminosity)))

(exporting-table table-font-slant
  '((:CAIRO_FONT_SLANT_NORMAL . :normal)
    (:CAIRO_FONT_SLANT_ITALIC . :italic)
    (:CAIRO_FONT_SLANT_OBLIQUE . :oblique)))

(exporting-table table-font-weight
  '((:CAIRO_FONT_WEIGHT_NORMAL . :normal)
    (:CAIRO_FONT_WEIGHT_BOLD . :bold)))

(exporting-table table-subpixel-order
  '((:CAIRO_SUBPIXEL_ORDER_DEFAULT . :default)
    (:CAIRO_SUBPIXEL_ORDER_RGB . :rgb)
    (:CAIRO_SUBPIXEL_ORDER_BGR . :bgr)
    (:CAIRO_SUBPIXEL_ORDER_VRGB . :vrgb)
    (:CAIRO_SUBPIXEL_ORDER_VBGR . :vbgr)))

(exporting-table table-hint-style
  '((:CAIRO_HINT_STYLE_DEFAULT . :default)
    (:CAIRO_HINT_STYLE_NONE . :none)
    (:CAIRO_HINT_STYLE_SLIGHT . :slight)
    (:CAIRO_HINT_STYLE_MEDIUM . :medium)
    (:CAIRO_HINT_STYLE_FULL . :full)))

(exporting-table table-hint-metrics
 '((:CAIRO_HINT_METRICS_DEFAULT . :default)
   (:CAIRO_HINT_METRICS_OFF . :off)
   (:CAIRO_HINT_METRICS_ON . :on)))

(exporting-table table-status
  '((:CAIRO_STATUS_SUCCESS . :success)
    (:CAIRO_STATUS_NO_MEMORY . :no-memory)
    (:CAIRO_STATUS_INVALID_RESTORE . :invalid-restore)
    (:CAIRO_STATUS_INVALID_POP_GROUP . :invalid-pop-group)
    (:CAIRO_STATUS_NO_CURRENT_POINT . :no-current-point)
    (:CAIRO_STATUS_INVALID_MATRIX . :invalid-matrix)
    (:CAIRO_STATUS_INVALID_STATUS . :invalid-status)
    (:CAIRO_STATUS_NULL_POINTER . :null-pointer)
    (:CAIRO_STATUS_INVALID_STRING . :invalid-string)
    (:CAIRO_STATUS_INVALID_PATH_DATA . :invalid-path-data)
    (:CAIRO_STATUS_READ_ERROR . :read-error)
    (:CAIRO_STATUS_WRITE_ERROR . :write-error)
    (:CAIRO_STATUS_SURFACE_FINISHED . :surface-finished)
    (:CAIRO_STATUS_SURFACE_TYPE_MISMATCH . :surface-type-mismatch)
    (:CAIRO_STATUS_PATTERN_TYPE_MISMATCH . :pattern-type-mismatch)
    (:CAIRO_STATUS_INVALID_CONTENT . :invalid-content)
    (:CAIRO_STATUS_INVALID_FORMAT . :invalid-format)
    (:CAIRO_STATUS_INVALID_VISUAL . :invalid-visual)
    (:CAIRO_STATUS_FILE_NOT_FOUND . :file-not-found)
    (:CAIRO_STATUS_INVALID_DASH . :invalid-dash)
    (:CAIRO_STATUS_INVALID_DSC_COMMENT . :invalid-dsc-comment)
    (:CAIRO_STATUS_INVALID_INDEX . :invalid-index)
    (:CAIRO_STATUS_CLIP_NOT_REPRESENTABLE . :clip-not-representable)
    (:CAIRO_STATUS_TEMP_FILE_ERROR . :temp-file-error)
    (:CAIRO_STATUS_INVALID_STRIDE . :invalid-stride)
    (:CAIRO_STATUS_FONT_TYPE_MISMATCH . :font-type-mismatch)
    (:CAIRO_STATUS_USER_FONT_IMMUTABLE . :user-font-immutable)
    (:CAIRO_STATUS_USER_FONT_ERROR . :user-font-error)
    (:CAIRO_STATUS_NEGATIVE_COUNT . :negative-count)
    (:CAIRO_STATUS_INVALID_CLUSTERS . :invalid-clusters)
    (:CAIRO_STATUS_INVALID_SLANT . :invalid-slant)
    (:CAIRO_STATUS_INVALID_WEIGHT . :invalid-weight)
    (:CAIRO_STATUS_INVALID_SIZE . :invalid-size)
    (:CAIRO_STATUS_USER_FONT_NOT_IMPLEMENTED . :user-font-not-implemented)
    (:CAIRO_STATUS_DEVICE_TYPE_MISMATCH . :device-type-mismatch)
    (:CAIRO_STATUS_DEVICE_ERROR . :device-error)
    (:CAIRO_STATUS_INVALID_MESH_CONSTRUCTION . :invalid-mesh-construction)
    (:CAIRO_STATUS_DEVICE_FINISHED . :device-finished)
    
    (:CAIRO_STATUS_LAST_STATUS . :last-status)))

(exporting-table table-pattern-type
   '((:CAIRO_PATTERN_TYPE_SOLID . :solid)
     (:CAIRO_PATTERN_TYPE_SURFACE . :surface)
     (:CAIRO_PATTERN_TYPE_LINEAR . :linear)
     (:CAIRO_PATTERN_TYPE_RADIAL . :radial)))

(exporting-table table-extend
   '((:CAIRO_EXTEND_NONE . :none)
     (:CAIRO_EXTEND_REPEAT . :repeat) 
     (:CAIRO_EXTEND_REFLECT . :reflect)
     (:CAIRO_EXTEND_PAD . :pad)))

(exporting-table table-filter
   '((:CAIRO_FILTER_FAST . :fast)
     (:CAIRO_FILTER_GOOD . :good)
     (:CAIRO_FILTER_BEST . :best)
     (:CAIRO_FILTER_NEAREST . :nearest)
     (:CAIRO_FILTER_BILINEAR . :bilinear)
     (:CAIRO_FILTER_GAUSSIAN . :gaussian)))

(defun lookup-cairo-enum (cairo-enum table)
  (let ((enum (cdr (assoc cairo-enum table))))
    (unless enum
      (error "Could not find cairo-enum ~a in ~a." cairo-enum table))
    enum))

(defun lookup-enum (enum table)
  (let ((cairo-enum (car (rassoc enum table))))
    (unless cairo-enum
      (error "Could not find enum ~a in ~a." enum table))
    cairo-enum))
