(in-package #:tswr-asteroids)

(defparameter *base-asteroid-score* 1000) ;TODO move to game-rules, or sth.
(defparameter *asteroid-powerup-drop-chance* 0.05) ;TODO move to game-rules, or sth.
(defparameter *default-ship-size* 20)   ;TODO move to game-rules, or sth.
(defparameter *default-powerup-size* 20)   ;TODO move to game-rules, or sth.
(defparameter *default-powerup-score* 2000)   ;TODO move to game-rules, or sth.

(defun spawn-asteroid (position size velocity)
  (let ((e (p2de:make-entity)))
    (add-component e 'position
                   :position position)
    
    (add-component e 'orientation
                   :orientation (p2dm:random-float 0.0 p2dm:+2pi+))
    
    (add-component e 'kinematics
                   :velocity velocity
                   :angular-velocity (p2dm:random-float -1.0 1.0))
    
    (add-component e 'collision-sphere
                   :radius size)
    
    (add-component e 'asteroid
                   )
    
    (add-component e 'wraps-around)
    
    (add-component e 'gives-score
                   :score (/ *base-asteroid-score* size))
    
    (add-component e 'drops-powerup
                   :chance *asteroid-powerup-drop-chance*)
    (add-component e 'renderable
                   :sprite :asteroid
                   :color (p2dg:make-color-4 1.0 1.0 1.0 1.0)
                   :scale size)
    e))

(defun spawn-ship (position)
  (let ((e (p2de:make-entity)))
    (add-component e 'position
                   :position position)
    
    (add-component e 'orientation
                   :orientation (p2dm:random-float 0 p2dm:+2pi+))
    
    (add-component e 'kinematics
                   :speed-limit 400.0
                   :rotation-speed-limit p2dm:+2pi+)
    
    (add-component e 'collision-sphere
                   :radius *default-ship-size*)
    
    (add-component e 'wraps-around)
    
    (add-component e 'ship)
    
    (add-component e 'gun
                   :bullet-type :standard
                   :cooldown-left 0.0)
    
    (add-component e 'player-controlled)
    
    (add-component e 'renderable
                   :sprite :ship
                   :color (p2dg:make-color-4 0.0 1.0 0.0 1.0)
                   :scale size)
    e))

(defun spawn-bullet (position velocity size life passthrough)
  (let ((e p2de:make-entity))
    (add-component e 'position
                   :position position)
    
    (add-component e 'kinematics
                   :velocity velocity)

    (add-component e 'collision-sphere
                   :size size)
    
    (add-component e 'bullet
                   :passthrough passthrough)
    
    (add-component e 'decays
                   :life-remaining life)
    
    (add-component e 'wraps-around)
    
    (add-component e 'renderable
                   :sprite :bullet
                   :color (p2dg:make-color-4 1.0 0.0 0.0 1.0) ;TODO different types = different colors
                   :scale size
                   )
    e))

(defun spawn-powerup (position type bonus life)
  (let ((e (p2de:make-entity)))
    (add-component e 'position
                   :position position)
    
    (add-component e 'kinematics)       ;TODO random velocity
    
    (add-component e 'collision-sphere
                   :size *default-powerup-size*)
    
    (add-component e 'wraps-around)
    
    (add-component e 'pickup)
    
    (add-component e 'decays
                   :life-remaining life)
    
    (add-component e 'gives-score
                   :score *default-powerup-score*)
    
    (add-component e 'renderable
                   :sprite :powerup     ;TODO different types of powerups
                   :color (p2dg:make-color-4 1.0 1.0 0.0 1.0)
                   :scale *default-powerup-size*)
    e))

(defun spawn-explosion (position)
  (let ((e p2de:make-entity))
    (add-component e 'position
                   :position position)
    
    (add-component e 'decays
                   :life *default-explosion-life*)

    ;; TODO explosion is animated, we need to handle it somehow.
    
    (add-component e 'renderable
                   :sprite :explosion
                   :color (p2dg:make-color-4 1.0 0.0 0.0 1.0)
                   :scale *default-explosion-size*)
    e))
