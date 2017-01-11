(in-package #:tswr-asteroids)

(defparameter *base-asteroid-score* 1000) ;TODO move to game-rules, or sth.
(defparameter *asteroid-powerup-drop-chance* 0.05) ;TODO move to game-rules, or sth.
(defparameter *default-ship-size* 10)   ;TODO move to game-rules, or sth.
(defparameter *default-powerup-size* 20)   ;TODO move to game-rules, or sth.
(defparameter *default-powerup-score* 2000)   ;TODO move to game-rules, or sth.
(defparameter *default-explosion-life* 1.0)
(defparameter *default-explosion-size* 10.0)


;;; Asteroids

(defun spawn-asteroid (position size velocity)
  (let ((e (p2de:make-entity)))
    (p2de:add-component e 'position
                        :position position)
    
    (p2de:add-component e 'orientation
                        :orientation (p2dm:random-float 0.0 p2dm:+2pi+))
    
    (p2de:add-component e 'kinematics
                        :velocity velocity
                        :angular-velocity (p2dm:random-float -1.0 1.0))
    
    (p2de:add-component e 'collision-sphere
                        :radius size
                        :layer :asteroid)
    
    (p2de:add-component e 'asteroid
                        )
    
    (p2de:add-component e 'wraps-around)
    
    (p2de:add-component e 'gives-score
                        :score (/ *base-asteroid-score* size))
    
    (p2de:add-component e 'drops-powerup
                        :chance *asteroid-powerup-drop-chance*)
    (p2de:add-component e 'renderable
                        :sprite :asteroid
                        :color (p2dg:make-color-4 1.0 1.0 1.0 1.0)
                        :scale size)
    e))


;;; Ships

(defun spawn-ship (position)
  (let ((e (p2de:make-entity)))
    (p2de:add-component e 'position
                        :position position)
    
    (p2de:add-component e 'orientation
                        :orientation (p2dm:random-float 0 p2dm:+2pi+))
    
    (p2de:add-component e 'kinematics
                        :speed-limit 400.0
                        :rotation-speed-limit p2dm:+2pi+)
    
    (p2de:add-component e 'collision-sphere
                        :radius *default-ship-size*
                        :layer :ship)
    
    (p2de:add-component e 'wraps-around)
    
    (p2de:add-component e 'ship)
    
    (p2de:add-component e 'gun
                        :bullet-type :special ;:standard
                        :cooldown-left 0.0
                        :cooldown-default 0.5) ;FIXME magic
    
    (p2de:add-component e 'player-controlled)
    
    (p2de:add-component e 'renderable
                        :sprite :ship-accelerating
                        :color (p2dg:make-color-4 1.0 1.0 0.0 1.0)
                        :scale *default-ship-size*)
    e))


;;; Bullets

(defun spawn-basic-bullet (&key position velocity)
  (let ((e (p2de:make-entity)))
    (p2de:add-component e 'position
                        :position position)
    (p2de:add-component e 'kinematics
                        :velocity velocity)
    (p2de:add-component e 'wraps-around)
    
    e))

(defun shoot-bullet (&key position velocity type buffs)
  (let ((e (spawn-basic-bullet :position position
                               :velocity velocity)))

    (p2de:add-component e 'bullet
                        :passthrough nil
                        :buffs buffs)

    (case type
      (:special
       (p2de:add-component e 'collision-sphere
                           :radius 4.0 ;FIXME (size) magic, bullet-type dependent
                           :layer :bullet
                           )

       (p2de:add-component e 'decays
                           :life-remaining 0.5) ;FIXME (life) magic, bullet-type dependent
    
       (p2de:add-component e 'renderable
                           :sprite :huge-bullet
                           :color (p2dg:make-color-4 1.0 0.0 1.0 1.0) ;TODO different types = different colors
                           :scale 4.0   ;FIXME (size) magic, bullet-type dependent
                           )
       )
      (otherwise
       (p2de:add-component e 'collision-sphere
                           :radius 2.0 ;FIXME (size) magic, bullet-type dependent
                           :layer :bullet
                           )
    
       (p2de:add-component e 'decays
                           :life-remaining 1.5) ;FIXME (life) magic, bullet-type dependent
    
       (p2de:add-component e 'renderable
                           :sprite :bullet
                           :color (p2dg:make-color-4 1.0 0.0 0.0 1.0) ;TODO different types = different colors
                           :scale 2.0   ;FIXME (size) magic, bullet-type dependent
                           )
       ))
    e))


;;; Powerups

(defun spawn-powerup (position type bonus life)
  (declare (ignore type bonus))
  (let ((e (p2de:make-entity)))
    (p2de:add-component e 'position
                        :position position)
    
    (p2de:add-component e 'kinematics)       ;TODO random velocity
    
    (p2de:add-component e 'collision-sphere
                        :radius *default-powerup-size*
                        :layer :powerup)
    
    (p2de:add-component e 'wraps-around)
    
    (p2de:add-component e 'pickup)
    
    (p2de:add-component e 'decays
                        :life-remaining life)
    
    (p2de:add-component e 'gives-score
                        :score *default-powerup-score*)
    
    (p2de:add-component e 'renderable
                        :sprite :powerup     ;TODO different types of powerups
                        :color (p2dg:make-color-4 1.0 1.0 0.0 1.0)
                        :scale *default-powerup-size*)
    e))


;;; FX

(defun spawn-explosion (position)
  (let ((e (p2de:make-entity)))
    (p2de:add-component e 'position
                        :position position)
    
    (p2de:add-component e 'decays
                        :life-remaining *default-explosion-life*)

    ;; TODO explosion is animated, we need to handle it somehow.
    
    (p2de:add-component e 'renderable
                        :sprite :explosion
                        :color (p2dg:make-color-4 1.0 0.0 0.0 1.0)
                        :scale *default-explosion-size*)
    e))
