(in-package #:tswr-asteroids)

(defparameter *base-asteroid-score* 1000) ;TODO move to game-rules, or sth.
(defparameter *asteroid-powerup-drop-chance* 0.05) ;TODO move to game-rules, or sth.
(defparameter *default-ship-size* 10)   ;TODO move to game-rules, or sth.
(defparameter *default-powerup-size* 20)   ;TODO move to game-rules, or sth.
(defparameter *default-powerup-score* 2000)   ;TODO move to game-rules, or sth.
(defparameter *default-explosion-life* 1.0)
(defparameter *default-explosion-size* 10.0)

(defparameter *triple-fire-angle-offset* (p2dm:deg->rad 30.0))
(defparameter *bullet-life-multiplier-buff* 2.0)
(defparameter *bullet-size-multiplier-buff* 3.0)


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
                        :bullet-type :standard ;:special
                        :buffs '() ;(:bidi-fire :triple-fire :big-bullets :longer-bullet-life :lower-cooldown :faster-bullets)
                        :cooldown-left 0.0
                        :cooldown-default 0.25) ;FIXME magic
    
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
                        :position (p2dm:scaled-vector position 1.0))
    (p2de:add-component e 'kinematics
                        :velocity (p2dm:scaled-vector velocity 1.0))
    (p2de:add-component e 'wraps-around)
    
    e))

(defun shoot-gun (&key position bullet-velocity bullet-type buffs)
  (let ((bullets-to-fire-velocities (list bullet-velocity)))
    ;; Apply gun buffs to number/parameters of launched projectiles.
    (when (member :triple-fire buffs)
      (setf bullets-to-fire-velocities
            (mapcan (lambda (vel)
                      (list vel
                            (p2dm:rotated-vector-2d vel *triple-fire-angle-offset*)
                            (p2dm:rotated-vector-2d vel (- *triple-fire-angle-offset*))))
                    bullets-to-fire-velocities)))
    
    (when (member :bidi-fire buffs)
      (setf bullets-to-fire-velocities
            (mapcan (lambda (vel)
                      (list vel
                            (p2dm:negative-vector vel)))
                    bullets-to-fire-velocities)))

    ;; Actually shoot bullets.
    (mapc (lambda (vel)
            (shoot-bullet :position position
                          :velocity vel
                          :type bullet-type
                          :buffs buffs))
          bullets-to-fire-velocities)))

(defun shoot-bullet (&key position velocity type buffs)
  ;; TODO consider gun buffs like multiple bullets, bigger bullets, faster shooting, etc.
  (let ((e (spawn-basic-bullet :position position
                               :velocity velocity)))

    (p2de:add-component e 'bullet
                        :passthrough nil
                        :buffs buffs)

    (let ((life-multiplier (if (member :longer-bullet-life buffs)
                               *bullet-life-multiplier-buff*
                               1.0))
          (size-multiplier (if (member :big-bullets buffs)
                               *bullet-size-multiplier-buff*
                               1.0))
          )
      (case type
        ;; TODO
        ;; - :normal explosive
        ;; - :normal fragmentary
        ;; - :missile :explosive
        ;; - :missile :fragmentary
        ;; - :hanzo
        (:special
         (p2de:add-component e 'collision-sphere
                             :radius (* size-multiplier 4.0) ;FIXME (size) magic, bullet-type dependent
                             :layer :bullet
                             )

         (p2de:add-component e 'decays
                             :life-remaining (* life-multiplier 0.5)) ;FIXME (life) magic, bullet-type dependent
    
         (p2de:add-component e 'renderable
                             :sprite :huge-bullet
                             :color (p2dg:make-color-4 1.0 0.0 1.0 1.0) ;TODO different types = different colors
                             :scale (* size-multiplier 4.0) ;FIXME (size) magic, bullet-type dependent
                             )
         )
        (otherwise
         (p2de:add-component e 'collision-sphere
                             :radius (* size-multiplier 2.0) ;FIXME (size) magic, bullet-type dependent
                             :layer :bullet
                             )
    
         (p2de:add-component e 'decays
                             :life-remaining (* life-multiplier 1.5)) ;FIXME (life) magic, bullet-type dependent
    
         (p2de:add-component e 'renderable
                             :sprite :bullet
                             :color (p2dg:make-color-4 1.0 0.0 0.0 1.0) ;TODO different types = different colors
                             :scale (* size-multiplier 2.0) ;FIXME (size) magic, bullet-type dependent
                             )
         )))
    e))


;;; Powerups

(defun spawn-powerup (position type bonus life)
  (declare (ignore bonus))
  (let ((e (p2de:make-entity)))
    (p2de:add-component e 'position
                        :position position)
    
    (p2de:add-component e 'kinematics)  ;TODO random velocity
    
    (p2de:add-component e 'collision-sphere
                        :radius *default-powerup-size*
                        :layer :powerup)
    
    (p2de:add-component e 'wraps-around)
    
    (p2de:add-component e 'powerup
                        :powerup-type type)
    
    (p2de:add-component e 'decays
                        :life-remaining life)
    
    (p2de:add-component e 'gives-score
                        :score *default-powerup-score*)
    
    (p2de:add-component e 'renderable
                        :sprite :powerup ;TODO different types of powerups
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



;;; Debug spawning utils

(defun debug-spawn-asteroid ()
  (spawn-asteroid (p2dm:make-vector-2d (random (float p2d:*canvas-width*)) (random (float p2d:*canvas-height*)))
                  (+ 5.0 (* 5.0 (random 5.0)))
                  (p2dm:scaled-vector (p2dm:normalized-vector (p2dm:make-vector-2d (random 1.0) (random 1.0)))
                                      (+ 5.0 (random 100.0)))))

(defun debug-spawn-powerup ()
  (spawn-powerup (p2dm:make-vector-2d (random (float p2d:*canvas-width*)) (random (float p2d:*canvas-height*)))
                 (whichever :triple-fire
                            :bidi-fire
                            :points)
                 :whatever
                 10.0))


