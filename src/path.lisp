(in-package :cl-cairo2)

(define-many-with-default-context
  (new-path)
  (new-sub-path)
  (close-path)
  (arc xc yc radius angle1 angle2)
  (arc-negative xc yc radius angle1 angle2)
  (curve-to x1 y1 x2 y2 x3 y3)
  (line-to x y)
  (move-to x y)
  (rectangle x y width height)
  (rel-move-to dx dy)
  (rel-curve-to dx1 dy1 dx2 dy2 dx3 dy3)
  (rel-line-to dx dy)
  (text-path text))

(define-flexible (get-current-point pointer)
  (with-foreign-objects ((xp :double) (yp :double))
    (cairo_get_current_point pointer xp yp)
    (values (mem-ref xp :double) (mem-ref yp :double))))

;; !!! not done yet: glyph-path

;; !!! need to write: path data type iterators, copy-path,
;; !!! copy-path-flat, path-destroy, append-path
