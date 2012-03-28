(defsystem :cl-cairo2-demos
  :description "Demos for cl-cairo2"
  :author "Ryan Pavlik"
  :license "BOOST 1.0"

  :depends-on (:cl-cairo2)

  :serial t
  :pathname "demos"

  :components
  ((:file "package")
   (:file "run-demo")
   (:file "arc")
   (:file "mesh")))
