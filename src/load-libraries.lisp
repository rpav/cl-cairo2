(in-package :cl-cairo2)

;;;; Loading Cairo library - you are supposed to set it up on the path where
;;;; the system's library loader looks up.
;;;; Also, the library search order should look like below because on Mac both
;;;; 'darwin' and 'unix' are defined in *feature* and we want to load .dylib
;;;; version of library.

(define-foreign-library :libcairo
  (cffi-features:darwin	"libcairo.dylib")
  (cffi-features:unix (:or "libcairo.so.2" "libcairo.so"))
  (cffi-features:windows "libcairo-2.dll"))

(load-foreign-library :libcairo)

(defcfun ("cairo_version" %cairo-version) :int)

(truncate (%cairo-version) 1)

(defun version (&optional (ver (%cairo-version)))
  (let* ((major (truncate ver 10000))
         (minor (truncate (- ver (* 10000 major)) 100))
         (micro (- ver (* 10000 major) (* 100 minor))))
    (values major minor micro)))

(defparameter *known-versions*
  '(11200 11002 10810))

(loop with ver = (%cairo-version)
      for known in *known-versions* do
        (when (<= known ver)
          (multiple-value-bind (maj min mic) (version known)
            (declare (ignore mic))
            (let ((feature (intern (format nil "CAIRO-~A.~A" maj min)
                                   :keyword)))
              (pushnew feature *features*)))))
