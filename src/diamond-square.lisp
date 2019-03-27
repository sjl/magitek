(in-package :magitek.diamond-square)


(defun heightmap-size (heightmap)
  (array-dimension heightmap 0))

(defun average (a b c d)
  (/ (+ a b c d) 4))

(defun ref (heightmap x y)
  (let ((size (array-dimension heightmap 0)))
    (aref heightmap (mod x size) (mod y size))))


(defun allocate (size)
  (let ((heightmap (make-array (list size size)
                     ;; :element-type 'single-float
                     :initial-element 0.0
                     :adjustable nil)))
    (setf (aref heightmap 0 0) (random 1.0))
    heightmap))


(defun heightmap-extrema (heightmap)
  (iterate
    (for v :across-flat-array heightmap)
    (maximize v :into max)
    (minimize v :into min)
    (finally (return (values min max)))))

(defun normalize (heightmap)
  (multiple-value-bind (min max) (heightmap-extrema heightmap)
    ;; (do-array (v heightmap)
    ;;   (setf v (/ (- v min) span)))
    (iterate
      (with span = (- max min))
      (for v :across-flat-array heightmap :with-index i)
      (setf (row-major-aref heightmap i)
            (/ (- v min) span)))))


(defun ds-square (heightmap x y radius spread)
  (setf (aref heightmap x y)
        (random-around (average (ref heightmap (- x radius) (- y radius))
                                (ref heightmap (- x radius) (+ y radius))
                                (ref heightmap (+ x radius) (- y radius))
                                (ref heightmap (+ x radius) (+ y radius)))
                       spread)))

(defun ds-diamond (heightmap x y radius spread)
  (setf (aref heightmap x y)
        (random-around (average (ref heightmap (- x radius) y)
                                (ref heightmap (+ x radius) y)
                                (ref heightmap x (- y radius))
                                (ref heightmap x (+ y radius)))
                       spread)))


(defun squares (heightmap radius spread)
  (iterate
    (with size = (heightmap-size heightmap))
    (for-nested ((x :from radius :below size :by (* 2 radius))
                 (y :from radius :below size :by (* 2 radius))))
    (ds-square heightmap x y radius spread)))

(defun diamonds (heightmap radius spread)
  (iterate
    (with size = (heightmap-size heightmap))
    (for i :from 0)
    (for y :from 0 :below size :by radius)
    (iterate
      (with shift = (if (evenp i) radius 0))
      (for x :from shift :below size :by (* 2 radius))
      (ds-diamond heightmap x y radius spread))))


(defun generate-heightmap
    (size &key (spread 0.8) (spread-reduction 0.7))
  (let ((heightmap (allocate size)))
    (recursively ((radius (floor size 2))
                  (spread spread))
      (when (>= radius 1)
        (squares heightmap radius spread)
        (diamonds heightmap radius spread)
        (recur (/ radius 2)
               (* spread spread-reduction))))
    (normalize heightmap)
    heightmap))


(defun heightmap-to-pixels (heightmap)
  (do-array (h heightmap)
    (setf h (floor (* 255 h)))))

(defun color-pixel (pixel)
  (cond
    ((< pixel 100) (vector 0 0 200))
    ((< pixel 200) (vector 0 150 0))
    (t (vector 255 255 255))))

(defun color (pixels)
  (do-array (p pixels)
    (setf p (color-pixel p))))

(defun dump ()
  (netpbm:write-to-file
    "heightmap.pbm"
    (color (heightmap-to-pixels (generate-heightmap 512)))
    :if-exists :supersede))
