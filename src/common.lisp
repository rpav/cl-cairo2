(in-package :cl-cairo2)

(defun deg-to-rad (deg)
  "Convert degrees to radians."
  (* deg (/ pi 180.0d0)))

(defgeneric destroy (object)
  (:documentation "Destroys Cairo object."))
(export 'destroy)

(defgeneric reference-count (object)
  (:documentation "Return Cairo's reference count for OBJECT"))
(export 'reference-count)

;;;;
;;;;  commonly used macros/functions
;;;;

(defun prepend-intern (prefix name &key (replace-dash t) (suffix ""))
  "Create and intern symbol PREFIXNAME from NAME, optionally
  replacing dashes in name.  PREFIX is converted to upper case.
  If given, suffix is appended at the end."
  (let ((name-as-string (symbol-name name)))
    (when replace-dash
      (setf name-as-string (substitute #\_ #\- name-as-string))
            suffix (substitute #\_ #\- suffix))
    (intern (concatenate 'string (string-upcase prefix)
			 name-as-string (string-upcase suffix)))))

(defun copy-double-vector-to-pointer (vector pointer)
  "Copies vector of double-floats to a memory location."
  (dotimes (i (length vector))
    (setf (mem-aref pointer :double i) (coerce (aref vector i) 'double-float))))

(defun copy-pointer-to-double-vector (length pointer)
  "Copies the contents of a memory location to a vector of a double-floats."
  (let ((vector (make-array length)))
    (dotimes (i length vector)
      (setf (aref vector i) (mem-aref pointer :double i)))))

;;;
;;; RGBA was apparently removed from cl-colors2
;;;

(declaim (inline red green blue alpha hsv->rgb))
(defstruct (rgba (:include cl-colors:rgb)
                 (:constructor rgba (red green blue alpha)))
  (alpha nil :type (real 0 1) :read-only t))

(defun red (rgb) (rgb-red rgb))
(defun green (rgb) (rgb-green rgb))
(defun blue (rgb) (rgb-blue rgb))
(defun hsv->rgb (hsv) (cl-colors:hsv-to-rgb hsv))
