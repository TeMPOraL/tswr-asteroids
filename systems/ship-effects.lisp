(in-package #:tswr-asteroids)

(p2de:defsystem ship-effects
  (ship kinematics renderable))

(defmethod p2de:do-system ((system ship-effects) entity dt)
  (with-slots (acceleration) (p2de:find-component entity 'kinematics)
    (with-slots (sprite) (p2de:find-component entity 'renderable)
      (if (> (p2dm:vector-value acceleration) 0.01)
          ;; FIXME actually store those sprites somewhere and just swap them
          (setf sprite (make-ship-accelerating-sprite))              ;FIXME magic
          (setf sprite (make-ship-sprite))))))
