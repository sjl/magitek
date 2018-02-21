(in-package :magitek.robots.bit-loom)

(defun resize (filename size)
  (sb-ext:run-program "mogrify"
                      (list "-resize" (format nil "~Dx~D" size size) filename)
                      :error :output
                      :search t))

(defun loom-1 (seed)
  (let ((depth (random-range-inclusive 16 19)))
    (flax.looms.001-triangles:loom seed depth "out.png" 3000 3000)
    (format nil "depth ~D" depth)))

(defun loom-2 (seed)
  (let ((ticks (* 1000 (random-range-inclusive 3 8))))
    (flax.looms.002-wobbly-lines:loom seed ticks "out.png" 2000 400)
    (format nil "~R ticks" ticks)))

(defun loom-3 (seed)
  (flax.looms.003-basic-l-systems::loom-anabaena-catenula seed "out.png" 2000 2000)
  (format nil "variety: anabaena catenula"))

(defun loom-4 (seed)
  (destructuring-bind (lsystem iterations mutagen)
      (flax.looms.004-turtle-curves::loom seed "out.png" 1000 1000)
    (format nil "~R iterations of ~A, ~A" iterations lsystem
            (if mutagen
              (format nil "mutagen ~D" mutagen)
              "pure"))))

(chancery:define-rule (select-loom :distribution :weighted)
  (1.0 1)
  (0.5 2)
  (0.2 3)
  (2.0 4))

(defparameter *looms* '(loom-1 loom-2 loom-3 loom-4))

(defun generate-image (seed &key force-loom)
  (let* ((loom-index (random (length *looms*)))
         (loom-index (1- (or force-loom (select-loom))))
         (loom (elt *looms* loom-index)))
    (format t "Running ~A~%" loom)
    (force-output)
    (let ((extra (funcall loom seed)))
      (resize "out.png" 1200)
      (values (1+ loom-index) extra))))

(defun random-tweet (&key force-loom)
  (let ((seed (random (expt 2 32))))
    (multiple-value-bind (loom-number extra-information)
        (generate-image seed :force-loom force-loom)
      (values (string-upcase
                (format nil "loom ~R, seed ~D~A"
                        loom-number seed
                        (if extra-information
                          (concatenate 'string ", " extra-information)
                          "")))
              "out.png"))))
