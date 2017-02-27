(in-package #:tswr-asteroids)

;;; Game rules stuff dropped in here somewhat at random.

(defparameter +default-lives+ 3)

(defvar *lives* +default-lives+)
(defvar *score* 0)
(defvar *high-score* 0)

(defvar *game-over* t "T if game is not running (e.g. player just died), nil if running.")

(defparameter +default-respawn-shield-duration+ 1.0)
(defvar *respawn-shield-remaining* 0 "How much respawn shield is remaining.")

(defparameter +starting-asteroid-size+ 48)
(defparameter +default-powerup-life+ 10)
(defparameter +asteroid-children+ 4 "How many pieces an asteroids splits into.")
(defparameter +minimum-asteroid-size+ 10 "We don't spawn asteroids smaller than this.")
(defparameter +default-asteroid-spread-speed+ 16 "Speed at which asteroids spread out.")

(defvar *last-ship-buffs* '() "Store for ship buffs between level progress.")

(defun clear-game-state ()
  "Clear all state related to main game and its rules."
  (setf *lives* +default-lives+
        *score* 0)
  ;; TODO read *high-score* from some data file or sth.
  (setf *last-ship-buffs* nil))

(defun handle-collision (entity-1-id entity-2-id)
  (let ((entity-1 (p2de:entity-by-id entity-1-id))
        (entity-2 (p2de:entity-by-id entity-2-id)))

    (cond ((bulletp entity-1)           ;NOTE rule - bullet disappears when asteroid hit
           (kill-bullet entity-1 :killer entity-2))

          ((asteroidp entity-1)         ;NOTE rule - asteroid disappears when hit by something
           (kill-asteroid entity-1 :killer entity-2))

          ((and (shipp entity-1)        ;NOTE rule - ship killed when hit by an asteroid
                (asteroidp entity-2))
           (unless (has-respawn-shield) ;NOTE rule - only if respawn shield is not active
             (kill-ship entity-1 :killer entity-2)))

          ((powerupp entity-1)
           (award-and-kill-powerup entity-1 :collector entity-2)))))

(defun award-score (score)
  "Award `SCORE' points."
  (incf *score* score)
  (when (> *score* *high-score*)
    (setf *high-score* *score*))
  (log:debug *score* score))

(defun award-and-kill-powerup (powerup &key collector)
  "Award `POWERUP' bonus to player."
  (let ((powerup-type (slot-value (p2de:find-component powerup 'powerup) 'powerup-type)))
    (log:info "Awarding powerup..." powerup-type)
    (when-let ((gives-score (p2de:find-component powerup 'gives-score)))
      (award-score (slot-value gives-score 'score)))

    (when collector
      (cond ((powerup-for-gun powerup-type)
             (when-let ((gun (p2de:find-component collector 'gun)))
               (appendf (slot-value gun 'buffs)
                        (list powerup-type))))
            (t (award-generic-powerup powerup-type)))))

  (p2de:schedule-entity-for-deletion powerup))

(defun award-generic-powerup (type)
  (when (eql type :1up)
    (incf *lives*)))

(defun powerup-for-gun (type)
  (not (eql type :1up)))

(defun kill-bullet (bullet &key killer)
  (declare (ignore killer))
  (p2de:schedule-entity-for-deletion bullet))

(defun kill-asteroid (asteroid &key killer)
  (maybe-spawn-child-asteroids asteroid)

  (when-let ((points (p2de:find-component asteroid 'gives-score)))
    (when (bulletp killer)
      (award-score (slot-value points 'score))))

  (when-let ((position (p2de:find-component asteroid 'position))
             (powerup (p2de:find-component asteroid 'drops-powerup)))
    (when (< (p2dm:random-float) (slot-value powerup 'chance))
      (spawn-powerup (slot-value position 'position)
                     (pick-random-powerup-type) ;FIXME determined by asteroid
                     +default-powerup-life+)))
  
  (p2de:schedule-entity-for-deletion asteroid))

(defun kill-ship (ship &key killer)
  (log:info "Oops, you're dead. ~A killed by ~A." ship killer)
  (decf *lives*)
  (if (> *lives* 0)
      (respawn-ship-after-death ship)
      (game-over)))

(defun entity-killed-on-border-hit (entity)
  (if (shipp entity)
      (kill-ship entity)
      (p2de:schedule-entity-for-deletion entity)))

(defun maybe-spawn-child-asteroids (original-asteroid)
  (flet ((noise-up-angle (angle)
           (+ angle (p2dm:random-float -1 1)))
         (noise-up-speed (speed)
           (+ speed (p2dm:random-float 0.0 (float (/ speed 2))))))

    (when-let ((pos (p2de:find-component original-asteroid 'position))
               (kinematics (p2de:find-component original-asteroid 'kinematics))
               (score (p2de:find-component original-asteroid 'gives-score))
               (bounds (p2de:find-component original-asteroid 'collision-sphere)))
      ;; TODO spawn N smaller asteroids with score = parent-score/2N
      (let* ((original-size (slot-value bounds 'radius))
             (original-position (slot-value pos 'position))
             (child-size (floor (/ original-size 2))))
        (when (> child-size +minimum-asteroid-size+)
          (dotimes (n +asteroid-children+)
            (spawn-asteroid original-position
                            child-size
                            (p2dm:scaled-vector (p2dm:rotated-vector-2d (p2dm:make-vector-2d 0.0 1.0)
                                                                        (noise-up-angle (coerce (* n (/ p2dm:+2pi+ +asteroid-children+)) 'p2dm:standard-float)))
                                                (noise-up-speed +default-asteroid-spread-speed+)))))))))

(defun pick-random-powerup-type ()
  (whichever :bidi-fire :triple-fire :big-bullets :longer-bullet-life :lower-cooldown :faster-bullets :1up))


;;; Some generic stuff
(defun tick-generic-game-rules (dt)
  (when *game-over*
    (start-game)
    (return-from tick-generic-game-rules))

  (when (= (count-live-asteroids) 0)
    (next-level)
    (return-from tick-generic-game-rules))

  (when (> *respawn-shield-remaining* 0)
    (decf *respawn-shield-remaining* dt)))

(defun count-live-asteroids ()
  (let ((total 0))
    (maphash (lambda (id entity)
               (declare (ignore id))
               (when (asteroidp entity)
                 (incf total)))
             (p2de::entities p2de:*ecs-manager*))
    total))

(defun get-player-ship ()
  (let (ship)
    (maphash (lambda (id entity)
               (declare (ignore id))
               (when (shipp entity)
                 (setf ship entity)))
             (p2de::entities p2de:*ecs-manager*))
    ship))


;;; Functions to check whether entity is of given game-rules type.

(defun bulletp (entity)
  (p2de:find-component entity 'bullet))

(defun shipp (entity)
  (p2de:find-component entity 'ship))

(defun asteroidp (entity)
  (p2de:find-component entity 'asteroid))

(defun powerupp (entity)
  (p2de:find-component entity 'powerup))


;;; Setting up game levels
(defun start-game ()
  (unless (eql *game-over* :continue)
    (clear-game-state))
  (set-up-entities-for-level)
  (setf *game-over* nil))

(defun set-up-entities-for-level ()
  ;; Spawn some random asteroids in the vicinity of player.
  (dotimes (i 10)
    (let ((r (p2dm:random-float 100.0 200.0))
          (theta-pos (p2dm:random-float 0.0 p2dm:+2pi+))
          (speed (p2dm:random-float 1.0 70.0))
          (theta-vel (p2dm:random-float 0.0 p2dm:+2pi+)))
      (spawn-asteroid (p2dm:rotated-vector-2d (p2dm:make-vector-2d r 0.0) theta-pos)
                      +starting-asteroid-size+
                      (p2dm:rotated-vector-2d (p2dm:make-vector-2d speed 0.0) theta-vel))))

  ;; Spawn player ship in the centre.
  (spawn-ship (p2dm:make-vector-2d 400.0 300.0) *last-ship-buffs*)
  (add-respawn-shield))

(defun respawn-ship-after-death (ship)
  (when-let ((pos (p2de:find-component ship 'position))
             (kin (p2de:find-component ship 'kinematics))
             (orn (p2de:find-component ship 'orientation)))
    (setf (slot-value pos 'position) (p2dm:make-vector-2d 400.0 300.0)
          (slot-value kin 'velocity) (p2dm:make-vector-2d)
          (slot-value orn 'orientation) (p2dm:random-float 0 p2dm:+2pi+)))
  (add-respawn-shield))

(defun game-over ()
  (log:info "Game over, restarting...")
  (p2de:schedule-all-entities-for-deletion)
  (setf *game-over* :restart))

(defun next-level ()
  (log:info "Progressing to next level!")
  ;; FIXME ensure ship (or at least its buffs) is saved!
  (p2de:schedule-all-entities-for-deletion)
  (setf *last-ship-buffs* (get-current-ship-buffs))
  (setf *game-over* :continue))

(defun get-current-ship-buffs ()
  (when-let ((ship (get-player-ship)))
    (slot-value (p2de:find-component ship 'gun) 'buffs)))

(defun add-respawn-shield ()
  (setf *respawn-shield-remaining* +default-respawn-shield-duration+))

(defun has-respawn-shield ()
  (> *respawn-shield-remaining* 0))
