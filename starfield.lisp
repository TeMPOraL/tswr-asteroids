;;;; A pretty starfield background.

(in-package #:tswr-asteroids)



(defun make-starfield (num-points)
  (let ((stars (make-array num-points)))
    (dotimes (i num-points)
      (setf (aref stars i)
            (p2dm:make-vector-2d (p2dm:random-float 0.0 800.0)
                                 (p2dm:random-float 0.0 600.0))))
    stars))

(defun draw-starfield (starfield)
  (p2dg:with-color (1 1 1)
    (gl:with-primitive :points
      (loop for point across starfield
         do
           (gl:vertex (p2dm:vec-x point) (p2dm:vec-y point))))))

