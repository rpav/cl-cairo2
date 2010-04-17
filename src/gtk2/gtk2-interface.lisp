;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-cairo2)

;;; We export the class so that it can be used to spezialize methods, etc.

(defclass gtk2-xlib-context (context)
  ((cairo-drawing-area :accessor cairo-drawing-area :initarg :cairo-drawing-area)
   (background-color :initarg :background-color)
   (sync-counter :accessor sync-counter :initarg :sync-counter :initform 0)))
