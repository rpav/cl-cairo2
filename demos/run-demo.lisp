(in-package :cairo-demo)

(defun run (demo-name &key (output-file "cairo-demo.png") (w 300) (h 300))
  "Run DEMO-NAME on an image surface of WxH.  Output is a PNG file, written
to OUTPUT-FILE.

DEMO-NAME should refer to a function which takes (WIDTH HEIGHT SURFACE)."
  (let ((surface (cairo:create-image-surface :argb32 w h)))
    (funcall demo-name w h surface)
    (cairo:surface-write-to-png surface output-file)))
