(defsystem a-cl-cairo2-loader :depends-on (:cl-cairo2))

;;;  This is a quick (and dirty? :-) fix for ASDF-Install, which tries
;;;  to load the first (in alphabetical order) .asd file, and would
;;;  fail because that is usually not cl-cairo2.asd.
