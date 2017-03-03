(in-package #:tswr-asteroids)

(defparameter *cooldown-multiplier-buff* 0.5) ;FIXME should be in a more centralized location with other buff constants
(defparameter *bullet-speed-multiplier-buff* 2.0) ;FIXME should be in a more centralized location with other buff constants

(p2de:defsystem input
  (player-controlled))

(defmethod p2de:do-system ((system input) entity dt)
  (declare (ignore system dt))
  ;; MOVEMENT
  (if (key-pressed-p :scancode-up)
      (key-accelerate entity)
      (if (key-pressed-p :scancode-down)
          (key-decelerate entity)
          (no-key-stop entity)))

  (if (key-pressed-p :scancode-left)
      (key-turn entity 1)
      (if (key-pressed-p :scancode-right)
          (key-turn entity -1)
          (key-turn entity 0)))
  
  ;; SHOOTING
  (when (key-pressed-p :scancode-space)
    (key-shoot entity)))

(defun key-pressed-p (scancode)
  (sdl2:keyboard-state-p scancode))

