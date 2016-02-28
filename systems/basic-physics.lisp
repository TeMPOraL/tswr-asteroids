(in-package #:tswr-asteroids)

(p2de:defsystem basic-physics
  (position kinematics))

(defmethod p2de:do-system ((system basic-physics) entity dt)
  (declare (ignore system))
  (with-slots (position) (p2de:find-component entity 'position)
    (with-slots (velocity acceleration angular-velocity angular-acceleration speed-limit rotation-speed-limit) (p2de:find-component entity 'kinematics)
      (p2dm:add-to-vector position (p2dm:scaled-vector velocity dt))
      (p2dm:add-to-vector velocity (p2dm:scaled-vector acceleration dt))

      (when (and speed-limit
                 (> (p2dm:vector-value velocity) speed-limit))
        (p2dm:scale-vector velocity (/ speed-limit
                                       (p2dm:vector-value velocity))))

      (when-let ((orientation (p2de:find-component entity 'orientation)))
        (incf (orientation orientation) (* angular-velocity dt))
        (incf angular-velocity (* angular-acceleration dt))
        (when (and rotation-speed-limit
                   (> (abs angular-velocity) rotation-speed-limit))
          (setf angular-velocity (* rotation-speed-limit
                                    (signum angular-velocity))))))))
