;;;; Game constants - all in one place, for easier tweaking.
(in-package #:tswr-asteroids)

(defparameter +default-respawn-shield-duration+ 1.5 "How long the initial invulnerability lasts.")

(defparameter +initial-asteroids+ 3)
(defparameter +asteroids-increment-per-level+ 2)
(defparameter +starting-asteroid-size+ 48)
(defparameter +default-powerup-life+ 7)
(defparameter +asteroid-children+ 2 "How many pieces an asteroids splits into.")
(defparameter +minimum-asteroid-size+ 10 "We don't spawn asteroids smaller than this.")
(defparameter +default-asteroid-spread-speed+ 30 "Speed at which asteroids spread out.")

(defparameter +base-asteroid-score+ 1000)
(defparameter +asteroid-powerup-drop-chance+ 0.01)
(defparameter +default-ship-size+ 8)
(defparameter +default-powerup-size+ 20)
(defparameter +default-powerup-score+ 2000)
(defparameter +default-explosion-life+ 1.0)
(defparameter +default-explosion-size+ 10.0)

(defparameter +default-bullet-life+ 0.75)
(defparameter +default-bullet-speed+ 1.0)
(defparameter +default-bullet-size+ 2.0)
(defparameter +default-bullet-velocity+ 300.0)

(defparameter +default-gun-cooldown+ 0.25)

(defparameter +default-ship-speed-limit+ 150.0)
(defparameter +default-ship-rotation-speed-limit+ p2dm:+2pi+)

(defparameter +triple-fire-angle-offset+ (p2dm:deg->rad 15.0))
(defparameter +bullet-life-multiplier-buff+ 1.5)
(defparameter +bullet-size-multiplier-buff+ 2.5)

