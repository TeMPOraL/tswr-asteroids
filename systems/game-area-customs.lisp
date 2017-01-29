(in-package #:tswr-asteroids)

(defparameter *game-area-min-x* 0)
(defparameter *game-area-max-x* 800)
(defparameter *game-area-min-y* 0)
(defparameter *game-area-max-y* 600)

(p2de:defsystem game-area-customs
  (position game-area-border-policy))

(defmethod p2de:do-system ((system game-area-customs) entity dt)
  (declare (ignore system dt))
  (with-slots (position) (p2de:find-component entity 'position)
    (with-slots (policy) (p2de:find-component entity 'game-area-border-policy)
      (case policy
        (:wraps-around (setf (p2dm:vec-x position) (wrap-in-range (p2dm:vec-x position) (1- *game-area-min-x*) (1+ *game-area-max-x*))
                             (p2dm:vec-y position) (wrap-in-range (p2dm:vec-y position) (1- *game-area-min-y*) (1+ *game-area-max-y*))))
        (:dies (when (outside-game-area-p position)
                 (entity-killed-on-border-hit entity)))))))

(defun outside-game-area-p (position)
  (or (< (p2dm:vec-x position) 0)
      (> (p2dm:vec-x position) 800)
      (< (p2dm:vec-y position) 0)
      (> (p2dm:vec-y position) 600)))

(defun wrap-in-range (x a b)
  (setf x (+ a (mod (- x a) (- b a)))))
