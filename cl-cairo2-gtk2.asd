(defsystem cl-cairo2-gtk2
  :description "Cairo 1.8 bindings, GTK context."
  :version "0.1"
  :author "Tamas K Papp"
  :license "LLGPL"
  :serial t
  :components
  ((:module
    "gtk2 surface"
    :pathname #P"src/gtk2/"
    :serial t
    :components
    ((:file "gtk2"))))
  :depends-on (:cl-cairo2 :cl-cairo2-xlib :cl-gtk2-cairo))
