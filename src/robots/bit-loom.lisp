(in-package :magitek.robots.bit-loom)

(defun resize (filename size)
  (sb-ext:run-program "mogrify"
                      (list "-resize" (format nil "~Dx~D" size size) filename)
                      :error :output
                      :search t))

(defmacro define-loom ((number loom &optional (width 1200) (height 1200))
                       loom-results &body body)
  `(defun ,(symb 'loom- number) (seed)
     (multiple-value-bind ,loom-results
         (,loom seed "out" :png ,width ,height)
       ,@body)))

(define-loom (1 flax.looms.001-triangles:loom 3000 3000)
    (depth)
  (format nil "depth ~D" depth))

(define-loom (2 flax.looms.002-wobbly-lines:loom 2000 400)
    (mode ticks)
  (format nil "~R ticks (~A)" ticks mode))

(define-loom (3 flax.looms.003-basic-l-systems:loom)
    ()
  (format nil "variety: anabaena catenula"))

(define-loom (4 flax.looms.004-turtle-curves:loom)
    (lsystem iterations mutagen)
  (format nil "~R iterations of ~A, ~A" iterations lsystem
          (if mutagen
            (format nil "mutagen ~D" mutagen)
            "pure")))

(define-loom (5 flax.looms.005-simple-triangulations:loom)
    (generator points ratio)
  (format nil "~R points, ~A generator, ~A triangulation"
          points generator
          (if (= 1 ratio)
            "full"
            (format nil "~R percent" (truncate (* 100 ratio))))))

(define-loom (6 flax.looms.006-tracing-lines:loom)
    (points lines)
  (format nil "~R lines of ~R points" lines points))

(define-loom (7 flax.looms.007-stipple:loom)
    (shapes)
  (format nil "stippling of ~R shapes" shapes))


(chancery:define-rule (select-loom :distribution :weighted)
  (1.0 1)
  (0.5 2)
  (0.2 3)
  (3.0 4)
  (1.0 5)
  (1.0 6)
  (1.0 7))

(defparameter *looms* '(loom-1 loom-2 loom-3 loom-4 loom-5 loom-6 loom-7))

(defun generate-image (seed &key force-loom)
  (let* ((loom-index (1- (or force-loom (select-loom))))
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
