(in-package #:tswr-asteroids)

;;; Game rules stuff dropped in here somewhat at random.

(defparameter +default-lives+ 3)

(defvar *lives* +default-lives+)
(defvar *score* 0)
(defvar *high-score* 0)
(defvar *current-level* 0)

(defvar *game-over* t "T if game is not running (e.g. player just died), nil if running.")

(defvar *respawn-shield-remaining* 0 "How much respawn shield is remaining.")

(defvar *last-ship-buffs* '() "Store for ship buffs between level progress.")

(defparameter *powerups-by-category-alist*
  '((:positive . (:bidi-fire
                  :triple-fire
                  :big-bullets
                  :longer-bullet-life
                  :lower-cooldown
                  :faster-bullets
                  :1up))
    (:negative . (:kill-ship
                  :spawn-extra-asteroid
                  :drop-random-gun-buff
                  :decrease-score
                  ;; TODO reverse controls?
                  ;; TODO invert shooting direction?
))))

(defun clear-game-state ()
  "Clear all state related to main game and its rules."
  (setf *lives* +default-lives+
        *score* 0
        *current-level* 0)
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
      (let ((category (pick-random-powerup-category)))
        (spawn-powerup (slot-value position 'position)
                       category         ;FIXME determined by asteroid
                       (pick-powerup-type-by-category category)
                       +default-powerup-life+))))
  
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
(defun pick-random-powerup-category ()
  (whichever :positive :negative :random))

(defun pick-powerup-type-by-category (category)
  (let* ((actual-category (if (eq category :random)
                              (whichever :positive :negative)
                              category))
         (types (cdr (assoc actual-category *powerups-by-category-alist*))))
    (elt types
         (random (length types)))))


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
    (maphash-values (lambda (entity)
                      (when (asteroidp entity)
                        (incf total)))
                    (p2de::entities p2de:*ecs-manager*))
    total))

(defun get-player-ship ()
  (let (ship)
    (maphash-values (lambda (entity)
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
(defun set-up-entities-for-level ()
  ;; Spawn some random asteroids in the vicinity of player.
  (dotimes (i (+ +initial-asteroids+ (* *current-level* +asteroids-increment-per-level+)))
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

(defun get-current-ship-buffs ()
  (when-let ((ship (get-player-ship)))
    (slot-value (p2de:find-component ship 'gun) 'buffs)))

(defun add-respawn-shield ()
  (setf *respawn-shield-remaining* +default-respawn-shield-duration+))

(defun has-respawn-shield ()
  (> *respawn-shield-remaining* 0))


;;; Call these to trigger game end or progression.

(defun start-game ()
  (unless (eql *game-over* :continue)
    (clear-game-state))
  (set-up-entities-for-level)
  (setf *game-over* nil))

(defun game-over ()
  (log:info "Game over, restarting...")
  (p2de:schedule-all-entities-for-deletion)
  (setf *game-over* :restart)
  (switch-game-screen :defeat))

(defun abort-game ()
  (log:info "User chickenig out...")
  (p2de:schedule-all-entities-for-deletion)
  (setf *game-over* :restart)
  (switch-game-screen :menu))

(defun next-level ()
  (log:info "Progressing to next level!")
  (p2de:schedule-all-entities-for-deletion)

  (setf *last-ship-buffs* (get-current-ship-buffs)) ; Ensure ship buffs are carried over.
  (incf *current-level*)
  
  (setf *game-over* :continue)
  (switch-game-screen :get-ready))


;;; Related to input handling.

(defun key-accelerate (entity)
  "Accelerate ship."
  (when-let ((kinematics (p2de:find-component entity 'kinematics))
             (orientation (p2de:find-component entity 'orientation)))
    (accelerate kinematics orientation 1.0)))

(defun key-decelerate (entity)
  "Decelerate ship."
  (when-let ((kinematics (p2de:find-component entity 'kinematics))
             (orientation (p2de:find-component entity 'orientation)))
    (accelerate kinematics orientation -0.5)))

(defun no-key-stop (entity)
  (when-let ((kinematics (p2de:find-component entity 'kinematics))
             (orientation (p2de:find-component entity 'orientation)))
    (stop-accelerating kinematics)))

(defun key-turn (entity direction)
  "Turn in `DIRECTION' (:left, :right or :stop). FIXME"
  (when-let ((kinematics (p2de:find-component entity 'kinematics)))
    (turn kinematics direction)))

(defun key-shoot (entity)
  "Shoot gun."
  (when-let ((gun (p2de:find-component entity 'gun))
             (position (p2de:find-component entity 'position))
             (orientation (p2de:find-component entity 'orientation))
             (kinematics (p2de:find-component entity 'kinematics)))
    (shoot gun position orientation kinematics)))

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

(defun shoot (gun position orientation kinematics)
  (let ((direction (orientation-value->direction (slot-value orientation 'orientation)))
        (pos (slot-value position 'position))
        (shooter-vel (slot-value kinematics 'velocity)))
    (with-slots (cooldown-left
                 cooldown-default
                 bullet-type
                 default-bullet-velocity
                 buffs)
        gun
      (unless (> cooldown-left 0.0)
        (let ((cooldown-multiplier (if (member :lower-cooldown buffs)
                                       *cooldown-multiplier-buff*
                                       1.0))
              (speed-multiplier (if (member :faster-bullets buffs)
                                    *bullet-speed-multiplier-buff*
                                    1.0)))
          (setf cooldown-left (* cooldown-multiplier cooldown-default))
          (shoot-gun :position pos
                     :bullet-velocity (p2dm:scaled-vector direction (* speed-multiplier default-bullet-velocity))
                     :shooter-velocity shooter-vel
                     :bullet-type bullet-type
                     :buffs buffs))))))
