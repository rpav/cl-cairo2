(in-package :cairo-demo)

;; Arc demo from cairographics.org/samples/
(defun arc1 (w h surface)
  (let* ((ctx (cairo:create-context surface))
         (size (min h w))
         (xc (/ w 2.0))
         (yc (/ h 2.0))
         (radius (- (/ size 2.0) (/ size 10.0)))
         (angle1 (* 45.0 (/ pi 180.0)))
         (angle2 (* 180.0 (/ pi 180.0))))
    (unwind-protect
         (cairo:with-context (ctx)
           (cairo:set-source-rgb 1 1 1)
           (cairo:paint)

           (cairo:set-source-rgb 0 0 0)
           (cairo:set-line-width 10.0)
           (cairo:arc xc yc radius angle1 angle2)
           (cairo:stroke)

           (cairo:set-source-rgba 1 0.2 0.2 0.6)
           (cairo:set-line-width 6.0)
           (cairo:arc xc yc 10.0 0 (* 2.0 pi))
           (cairo:fill-path)

           (cairo:arc xc yc radius angle1 angle1)
           (cairo:line-to xc yc)
           (cairo:arc xc yc radius angle2 angle2)
           (cairo:line-to xc yc)
           (cairo:stroke))
      (cairo:destroy ctx))))
