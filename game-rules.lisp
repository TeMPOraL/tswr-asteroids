(in-package #:tswr-asteroids)

;;; Game rules stuff dropped in here somewhat at random.

(defparameter +default-lives+ 3)

(defvar *lives* +default-lives+)
(defvar *score* 0)
(defvar *high-score* 0)

(defun clear-game-state ()
  "Clear all state related to main game and its rules."
  (setf *lives* +default-lives+
        *score* 0)
  ;; TODO read *high-score* from some data file or sth.
  )

(defun handle-collision (entity-1-id entity-2-id)
  (let ((entity-1 (p2de:entity-by-id entity-1-id))
        (entity-2 (p2de:entity-by-id entity-2-id)))

    (cond ((bulletp entity-1)           ;NOTE rule - bullet disappears when asteroid hit
           (kill-bullet entity-1 :killer entity-2))

          ((asteroidp entity-1)         ;NOTE rule - asteroid disappears when hit by something
           (kill-asteroid entity-1 :killer entity-2))

          ((and (shipp entity-1)        ;NOTE rule - ship killed when hit by an asteroid
                (asteroidp entity-2))
           (kill-ship entity-1 :killer entity-2))

          ((powerupp entity-1)
           (award-and-kill-powerup entity-1 :collector entity-2)))))

(defun award-score (score)
  "Award `SCORE' points."
  (incf *score* score)
  (log:debug *score* score))

(defun award-and-kill-powerup (powerup &key collector)
  "Award `POWERUP' bonus to player."
  (let ((powerup-type (slot-value (p2de:find-component powerup 'powerup) 'powerup-type)))
    (log:info "Awarding powerup..." powerup-type)
    (when-let ((gives-score (p2de:find-component powerup 'gives-score)))
      (award-score (slot-value gives-score 'score)))

    (when collector
      (when-let ((gun (p2de:find-component collector 'gun)))
        (appendf (slot-value gun 'buffs)
                 (list powerup-type)))))

    (p2de:schedule-entity-for-deletion powerup)))

(defun kill-bullet (bullet &key killer)
  (p2de:schedule-entity-for-deletion bullet))

(defun kill-asteroid (asteroid &key killer)
  (spawn-child-asteroids asteroid)
  (when-let ((points (p2de:find-component asteroid 'gives-score)))
    (when (bulletp killer)
      (award-score (slot-value points 'score))))

  ;; TODO maybe spawn powerup
  
  (p2de:schedule-entity-for-deletion asteroid))

(defun kill-ship (ship &key killer)
  (log:info "Oops, you're dead. ~A killed by ~A." ship killer))

(defun spawn-child-asteroids (original-asteroid)
  (when-let ((pos (p2de:find-component original-asteroid 'position))
             (kinematics (p2de:find-component original-asteroid 'kinematics))
             (score (p2de:find-component original-asteroid 'gives-score))
             (bounds (p2de:find-component original-asteroid 'collision-sphere)))
    ;; TODO spawn N smaller asteroids with score = parent-score/2N
    ))

(defun pick-random-powerup-type ()
  (whichever :bidi-fire :triple-fire :big-bullets :longer-bullet-life :lower-cooldown :faster-bullets))


;;; Functions to check whether entity is of given game-rules type.

(defun bulletp (entity)
  (p2de:find-component entity 'bullet))

(defun shipp (entity)
  (p2de:find-component entity 'ship))

(defun asteroidp (entity)
  (p2de:find-component entity 'asteroid))

(defun powerupp (entity)
  (p2de:find-component entity 'powerup))
