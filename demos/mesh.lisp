(in-package :cairo-demo)

;; From the documentation for cairo_pattern_create_mesh

(defun mesh1 (w h surface)
  (let* ((ctx (cairo:create-context surface))
         (pat (cairo:create-mesh-pattern)))
    (cairo:with-context (ctx)
      (progn
        (cairo:pattern-mesh-begin-patch pat)
        (cairo:pattern-mesh-move-to pat 0 0)
        (cairo:pattern-mesh-curve-to pat 30 -30  60  30 100 0)
        (cairo:pattern-mesh-curve-to pat 60  30 130  60 100 100)
        (cairo:pattern-mesh-curve-to pat 60  70  30 130   0 100)
        (cairo:pattern-mesh-curve-to pat 30  70 -30  30   0 0)
        (cairo:pattern-mesh-set-corner-color-rgb pat 0 1 0 0)
        (cairo:pattern-mesh-set-corner-color-rgb pat 1 0 1 0)
        (cairo:pattern-mesh-set-corner-color-rgb pat 2 0 0 1)
        (cairo:pattern-mesh-set-corner-color-rgb pat 3 1 1 0)
        (cairo:pattern-mesh-end-patch pat)

        (cairo:pattern-mesh-begin-patch pat)
        (cairo:pattern-mesh-move-to pat 100 100)
        (cairo:pattern-mesh-line-to pat 130 130)
        (cairo:pattern-mesh-line-to pat 130  70)
        (cairo:pattern-mesh-set-corner-color-rgb pat 0 1 0 0)
        (cairo:pattern-mesh-set-corner-color-rgb pat 1 0 1 0)
        (cairo:pattern-mesh-set-corner-color-rgb pat 2 0 0 1)
        (cairo:pattern-mesh-end-patch pat))

      (cairo:set-source-rgba 1 1 1 1)
      (cairo:paint)

      (progn
        (cairo:set-source pat)
        (cairo:translate (- (/ w 2.0)) (- (/ h 2.0)))
        (cairo:pattern-set-matrix pat (cairo:get-trans-matrix))
        (cairo:reset-trans-matrix)
        (cairo:paint)))

    (cairo:destroy pat)
    (cairo:destroy ctx)))

