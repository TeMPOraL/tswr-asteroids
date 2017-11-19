(in-package #:tswr-asteroids)


;;; Asteroids

(defun spawn-asteroid (position size velocity)
  (let ((e (p2de:make-entity)))
    (p2de:add-component e 'position
                        :position (p2dm:scaled-vector position 1.0))
    
    (p2de:add-component e 'orientation
                        :orientation (p2dm:random-float 0.0 p2dm:+2pi+))
    
    (p2de:add-component e 'kinematics
                        :velocity (p2dm:scaled-vector velocity 1.0)
                        :angular-velocity (p2dm:random-float -1.0 1.0))
    
    (p2de:add-component e 'collision-sphere
                        :radius size
                        :layer :asteroid)
    
    (p2de:add-component e 'asteroid
                        )
    
    (p2de:add-component e 'game-area-border-policy :policy :wraps-around)
    
    (p2de:add-component e 'gives-score
                        :score (/ +base-asteroid-score+ size))
    
    (p2de:add-component e 'drops-powerup
                        :chance +asteroid-powerup-drop-chance+)
    (p2de:add-component e 'renderable
                        :sprite (make-asteroid-sprite)
                        :color (p2dg:make-color-4 1.0 1.0 1.0 1.0)
                        :scale size)
    e))


;;; Ships

(defun spawn-ship (position &optional starting-buffs)
  (let ((e (p2de:make-entity)))
    (p2de:add-component e 'position
                        :position position)
    
    (p2de:add-component e 'orientation
                        :orientation (p2dm:random-float 0 p2dm:+2pi+))
    
    (p2de:add-component e 'kinematics
                        :speed-limit +default-ship-speed-limit+
                        :rotation-speed-limit +default-ship-rotation-speed-limit+)
    
    (p2de:add-component e 'collision-sphere
                        :radius +default-ship-size+
                        :layer :ship)
    
    (p2de:add-component e 'game-area-border-policy :policy :dies)
    
    (p2de:add-component e 'ship)
    
    (p2de:add-component e 'gun
                        :bullet-type :standard ;:special
                        :buffs starting-buffs ;(:bidi-fire :triple-fire :big-bullets :longer-bullet-life :lower-cooldown :faster-bullets)
                        :cooldown-left 0.0
                        :cooldown-default +default-gun-cooldown+)
    
    (p2de:add-component e 'player-controlled)
    
    (p2de:add-component e 'renderable
                        :sprite (make-ship-accelerating-sprite)
                        :color (p2dg:make-color-4 1.0 1.0 0.0 1.0)
                        :scale +default-ship-size+)
    e))


;;; Bullets

(defun spawn-basic-bullet (&key position velocity)
  (let ((e (p2de:make-entity)))
    (p2de:add-component e 'position
                        :position (p2dm:scaled-vector position 1.0))
    (p2de:add-component e 'kinematics
                        :velocity (p2dm:scaled-vector velocity 1.0))
    (p2de:add-component e 'game-area-border-policy :policy :dies)
    e))

(defun shoot-gun (&key position bullet-velocity shooter-velocity bullet-type buffs)
  (let ((bullets-to-fire-velocities (list bullet-velocity)))
    ;; Apply gun buffs to number/parameters of launched projectiles.
    (when (member :triple-fire buffs)
      (setf bullets-to-fire-velocities
            (mapcan (lambda (vel)
                      (list vel
                            (p2dm:rotated-vector-2d vel +triple-fire-angle-offset+)
                            (p2dm:rotated-vector-2d vel (- +triple-fire-angle-offset+))))
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
                          :velocity (p2dm:add-vectors vel shooter-velocity)
                          :type bullet-type
                          :buffs buffs))
          bullets-to-fire-velocities)))

(defun shoot-bullet (&key position velocity type buffs)
  (let ((e (spawn-basic-bullet :position position
                               :velocity velocity)))

    (p2de:add-component e 'bullet
                        :passthrough nil
                        :buffs buffs)

    (let ((life-multiplier (if (member :longer-bullet-life buffs)
                               +bullet-life-multiplier-buff+
                               1.0))
          (size-multiplier (if (member :big-bullets buffs)
                               +bullet-size-multiplier-buff+
                               1.0))
          )
      (case type
        ;; TODO
        ;; - :normal explosive
        ;; - :normal fragmentary
        ;; - :missile :explosive
        ;; - :missile :fragmentary
        ;; - :hanzo
        (:special                       ;TODO remove and replace with real bullet types
         (p2de:add-component e 'collision-sphere
                             :radius (* size-multiplier 4.0) ;FIXME (size) magic, bullet-type dependent
                             :layer :bullet
                             )

         (p2de:add-component e 'decays
                             :life-remaining (* life-multiplier 0.5)) ;FIXME (life) magic, bullet-type dependent
    
         (p2de:add-component e 'renderable
                             :sprite :huge-bullet ;TODO repace
                             :color (p2dg:make-color-4 1.0 0.0 1.0 1.0) ;TODO different types = different colors
                             :scale (* size-multiplier 4.0) ;FIXME (size) magic, bullet-type dependent
                             )
         )
        (otherwise
         (p2de:add-component e 'collision-sphere
                             :radius (* size-multiplier +default-bullet-size+)
                             :layer :bullet
                             )
    
         (p2de:add-component e 'decays
                             :life-remaining (* life-multiplier +default-bullet-life+))
    
         (p2de:add-component e 'renderable
                             :sprite (make-bullet-sprite)
                             :color (p2dg:make-color-4 1.0 0.0 0.0 1.0) ;TODO different types = different colors
                             :scale (* size-multiplier +default-bullet-size+)
                             )
         )))
    e))


;;; Powerups

(defun spawn-powerup (position type life)
  (let ((e (p2de:make-entity)))
    (p2de:add-component e 'position
                        :position position)
    
    (p2de:add-component e 'kinematics)
    
    (p2de:add-component e 'collision-sphere
                        :radius +default-powerup-size+
                        :layer :powerup)
    
    (p2de:add-component e 'powerup
                        :powerup-type type)
    
    (p2de:add-component e 'decays
                        :life-remaining life)
    
    (p2de:add-component e 'gives-score
                        :score +default-powerup-score+)
    
    (p2de:add-component e 'renderable
                        :sprite (make-powerup-sprite)
                        :color (powerup-type->powerup-color type)
                        :scale +default-powerup-size+)
    e))

(defun powerup-type->powerup-color (powerup-type)
  "Get appropriate color for `POWERUP-TYPE'."
  (declare (ignore powerup-type))       ;TODO actually use it
  (p2dg:make-color-4 1.0 1.0 0.0 1.0))


;;; FX

(defun spawn-explosion (position)
  (let ((e (p2de:make-entity)))
    (p2de:add-component e 'position
                        :position position)
    
    (p2de:add-component e 'decays
                        :life-remaining +default-explosion-life+)

    ;; TODO explosion is animated, we need to handle it somehow.
    
    (p2de:add-component e 'renderable
                        :sprite :explosion
                        :color (p2dg:make-color-4 1.0 0.0 0.0 1.0)
                        :scale +default-explosion-size+)
    e))



;;; Debug spawning utils

(defun debug-spawn-asteroid ()
  (spawn-asteroid (p2dm:make-vector-2d (random (float p2d:*canvas-width*)) (random (float p2d:*canvas-height*)))
                  (+ 5.0 (* 5.0 (random 5.0)))
                  (p2dm:scaled-vector (p2dm:normalized-vector (p2dm:make-vector-2d (random 1.0) (random 1.0)))
                                      (+ 5.0 (random 100.0)))))

(defun debug-spawn-powerup ()
  (spawn-powerup (p2dm:make-vector-2d (random (float p2d:*canvas-width*)) (random (float p2d:*canvas-height*)))
                 (pick-random-powerup-type)
                 10.0))


