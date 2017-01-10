(in-package #:tswr-asteroids)

(p2de:defsystem gun-cooldown-updater
  (gun))

(defmethod p2de:do-system ((system gun-cooldown-updater) entity dt)
  (with-slots (cooldown-left) (p2de:find-component entity 'gun)
    (setf cooldown-left (max (- cooldown-left dt)
                             0.0))))
