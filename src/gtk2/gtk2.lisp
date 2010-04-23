;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-cairo2)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass cairo-drawing-area (gtk:drawing-area)
    ((source-surface :accessor source-surface :initarg :source-surface))
    (:metaclass gobject:gobject-class)))

(defun repaint-drawing-area (widget)
  (cl-gtk2-cairo:with-gdk-context (target-context (gtk:widget-window widget))
    (set-source-surface (source-surface widget) 0 0 target-context)
    (paint target-context)))

(defmethod initialize-instance :after ((w cairo-drawing-area) &rest initargs)
           (declare (ignore initargs))
           (gobject:connect-signal w "configure-event"
                                   (lambda (widget event)
                                     (declare (ignore event))
                                     (repaint-drawing-area widget)
                                     t))
           (gobject:connect-signal w "expose-event"
                                   (lambda (widget event)
                                     (declare (ignore event))
                                     (repaint-drawing-area widget)
                                     t)))

(defmethod sync ((object gtk2-xlib-context))
  (gtk:within-main-loop
    (with-slots (sync-counter cairo-drawing-area) object
      (when (zerop sync-counter)
        (gtk:widget-queue-draw cairo-drawing-area)))))

(defmethod sync-lock ((object gtk2-xlib-context))
  (incf (sync-counter object)))

(defmethod sync-unlock ((object gtk2-xlib-context))
  (with-slots (sync-counter) object
    (when (plusp sync-counter)
      (when (zerop (decf sync-counter))
        (sync object)))))

(defmethod sync-reset ((object gtk2-xlib-context))
  (setf (sync-counter object) 0)
  (sync object))

(defun create-gtk2-xlib-context (width height &key (title "gtk2") (background-color +white+))
  (let (context)
    (gtk:within-main-loop-and-wait ; important to wait until it finishes
      (gtk:let-ui (gtk:gtk-window 
                   :var window
                   :title title
                   :default-width width
                   :default-height height
                   :width-request width
                   :height-request height
                   :resizable nil
                   :type :toplevel
                   (gtk:v-box
                    (cairo-drawing-area :var cairo-drawing-area)))
        ;; create the surface and context
        (let* ((surface (create-image-surface :rgb24 width height))
               (pointer (get-pointer (create-context surface))))
          ;; create and save context
          (setf context (make-instance 'gtk2-xlib-context 
                                       :background-color background-color
                                       :cairo-drawing-area cairo-drawing-area
                                       :pixel-based-p t
                                       :height height
                                       :width width
                                       :pointer pointer))
          ;; save surface
          (setf (source-surface cairo-drawing-area) surface)
          ;; now we can destroy the surface, it will be free when the
          ;; context is destroyed
          (cairo_surface_destroy (get-pointer surface))
          ;; paint if background color is given
          (when background-color
            (set-source-color background-color context)
            (paint context))
          ;; show window
          (gtk:widget-show window))))
    context))

(export 'create-gtk2-xlib-context)

;; (setf *context* (create-gtk2-xlib-context 500 400))

;; (set-source-color cl-colors:+green+)
;; (move-to 200 200)
;; (line-to 300 300)
;; (stroke)


