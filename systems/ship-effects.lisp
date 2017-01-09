(in-package #:tswr-asteroids)

(p2de:defsystem ship-effects
  (ship kinematics renderable))

(defmethod p2de:do-system ((system ship-effects) entity dt)
  (with-slots (acceleration) (p2de:find-component entity 'kinematics)
    (with-slots (sprite) (p2de:find-component entity 'renderable)
      (if (> (p2dm:vector-value acceleration) 0.01)
          (setf sprite :ship-accelerating) ;FIXME magic
          (setf sprite :ship)))))
