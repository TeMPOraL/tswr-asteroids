(in-package #:tswr-asteroids)

(p2de:defsystem game-area-wrapper 
  (position wraps-around))

(defmethod p2de:do-system ((system game-area-wrapper) entity dt)
  (declare (ignore system dt))
  (with-slots (position) (p2de:find-component entity 'position)
    (setf (p2dm:vec-x position) (wrap-in-range (p2dm:vec-x position) -1 801)
          (p2dm:vec-y position) (wrap-in-range (p2dm:vec-y position) -1 600))))

(defun wrap-in-range (x a b)
  (setf x (+ a (mod (- x a) (- b a)))))
