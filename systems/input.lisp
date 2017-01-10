(in-package #:tswr-asteroids)

(p2de:defsystem input
  (player-controlled))

(defmethod p2de:do-system ((system input) entity dt)
  (declare (ignore system dt))
  ;; MOVEMENT
  (when-let ((kinematics (p2de:find-component entity 'kinematics))
             (orientation (p2de:find-component entity 'orientation)))
    (if (key-pressed-p :scancode-up)
        (accelerate kinematics orientation 1.0)
        (if (key-pressed-p :scancode-down)
            (accelerate kinematics orientation -0.5)
            (stop-accelerating kinematics)))
    
    (if (key-pressed-p :scancode-left)
        (turn kinematics 1)
        (if (key-pressed-p :scancode-right)
            (turn kinematics -1)
            (turn kinematics 0))))
  
  ;; SHOOTING
  (when-let ((gun (p2de:find-component entity 'gun))
             (position (p2de:find-component entity 'position))
             (orientation (p2de:find-component entity 'orientation)))
    (when (key-pressed-p :scancode-space)
      (shoot gun position orientation))))


(defun key-pressed-p (scancode)
  (sdl2:keyboard-state-p scancode))

;;; the most dumb way to make it work...

(defun orientation-value->direction (orientation)
  (p2dm:rotated-vector-2d (p2dm:make-vector-2d 0.0 1.0) orientation))

(defun accelerate (kinematics orientation scale)
  (let ((direction (orientation-value->direction (slot-value orientation 'orientation))))
    (setf (slot-value kinematics 'acceleration)
          (p2dm:scaled-vector direction (* 150.0 scale)))))

(defun stop-accelerating (kinematics)
  (setf (slot-value kinematics 'acceleration)
        (p2dm:make-vector-2d)))

(defun turn (kinematics angle-sign)
  (setf (slot-value kinematics 'angular-velocity)
        (p2dm:deg->rad (* 180.0 angle-sign))))

(defun shoot (gun position orientation)
  (let ((direction (orientation-value->direction (slot-value orientation 'orientation)))
        (pos (slot-value position 'position)))
    (with-slots (cooldown-left
                 cooldown-default
                 bullet-type
                 default-bullet-velocity
                 buffs)
        gun
      (unless (> cooldown-left 0.0)
        (setf cooldown-left cooldown-default)

        ;; TODO bullet shooting should be handled by the gun directly, IMO - to support things like triple-fire, etc.
        ;; lambdas are data too, btw.
        (shoot-bullet :position (p2dm:scaled-vector pos 1.0)
                      :velocity (p2dm:scaled-vector direction default-bullet-velocity)
                      :type bullet-type
                      :buffs buffs)))))
