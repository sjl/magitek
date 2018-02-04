(in-package :magitek.robots.bit-loom)

(defun pnm-to-png (pnm png)
  (sb-ext:run-program "pnmtopng" (list pnm)
                      :search t
                      :output png
                      :error nil
                      :if-output-exists :supersede)
  (sb-ext:run-program "mogrify" (list "-resize" "800x800" png)
                      :error :output
                      :search t))

(defun loom-1 (seed)
  (let ((depth (random-range-inclusive 16 19)))
    (flax.looms.001-triangles:loom seed depth "out.pnm" 3000 3000)
    (format nil "depth ~D" depth)))

(defun loom-2 (seed)
  (let ((ticks (* 1000 (random-range-inclusive 3 8))))
    (flax.looms.002-wobbly-lines:loom seed ticks "out.pnm" 2000 400)
    (format nil "~R ticks" ticks)))

(defparameter *looms* '(loom-1 loom-2))

(defun generate-image (seed)
  (let* ((loom-index (random (length *looms*)))
         (loom (elt *looms* loom-index)))
    (pr 'running loom)
    (let ((extra (funcall loom seed)))
      (pnm-to-png "out.pnm" "out.png")
      (values (1+ loom-index) extra))))

(defun random-tweet ()
  (let ((seed (random (expt 2 32))))
    (multiple-value-bind (loom-number extra-information)
        (generate-image seed)
      (values (string-upcase
                (format nil "loom ~R, seed ~D~A"
                        loom-number seed
                        (if extra-information
                          (concatenate 'string ", " extra-information)
                          "")))
              "out.png"))))
