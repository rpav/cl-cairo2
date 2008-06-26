(in-package :cl-cairo2)

;; (define-foreign-library libcairo
;;   (:unix (:or "libcairo.so.2" "libcairo.so"))
;;   (t (:default "libcairo")))

;; (use-foreign-library libcairo)

(load-foreign-library '(:default "libcairo"))

(defun deg-to-rad (deg)
  "Convert degrees to radians."
  (* deg (/ pi 180.0d0)))

(defgeneric destroy (object)
  (:documentation "Destroys Cairo object."))
(export 'destroy)

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
