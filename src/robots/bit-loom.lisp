(in-package :magitek.robots.bit-loom)

(defun pnm-to-png (pnm png)
  (sb-ext:run-program "pnmtopng" (list pnm)
                      :search t
                      :output png
                      :if-output-exists :supersede)
  (sb-ext:run-program "mogrify" (list "-resize" "800x800" png)
                      :search t))

(defun loom-1 (seed)
  (let ((depth (random-range-inclusive 16 19)))
    (flax.looms.001-triangles:loom seed depth "out.pnm" 3000 3000)
    `(depth ,depth)))

(defun generate-image (seed)
  (prog1 (loom-1 seed)
    (pnm-to-png "out.pnm" "out.png")))

(defun random-tweet ()
  (let* ((seed (random (expt 2 32)))
         (parameters (append `(seed ,seed) (generate-image seed))))
    (values (string-upcase (format nil "Loom ~R, ~{~A ~A~^, ~}" 1 parameters))
            "out.png")))
