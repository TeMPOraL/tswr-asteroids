(in-package #:tswr-asteroids)

(defclass asteroids-game (p2d:game)
  ())

(defparameter *player-ship-entity* nil)

(defmethod p2d:preinit ((game asteroids-game))
  ;; TODO preconfiguration (if any)
  (setf p2d:*window-title* "TSWR - Asteroids") ; FIXME maybe pass it through the game class?
  (setf p2d:*use-fixed-timestep* t))

(defmethod p2d:initialize ((game asteroids-game))
  (log:info "TSWR - Asteroids game init.")
  (gl:clear-color 0.0 0.0 0.0 1.0)

  ;; add some systems
  (log:debug "Booting up ECS...")
  (mapc #'p2de:register-system '(basic-physics
                                 game-area-wrapper
                                 decayer
                                 ship-effects
                                 renderer))
  
  ;; and an entity
  (spawn-asteroid (p2dm:make-vector-2d 600.0 400.0) 40 (p2dm:make-vector-2d 40.0 60.0))
  (spawn-asteroid (p2dm:make-vector-2d 200.0 300.0) 30 (p2dm:make-vector-2d -50.0 45.0))
  (setf *player-ship-entity* (spawn-ship (p2dm:make-vector-2d 200.0 400.0))))

(defmethod p2d:deinitialize ((game asteroids-game))
  (log:info "TSWR - Asteroids game deinit."))

(defmethod p2d:on-key-event ((game asteroids-game) key state repeat)
  (declare (ignorable repeat))

  (with-slots (position) (p2de:find-component *player-ship-entity* 'position)
    (with-slots (orientation) (p2de:find-component *player-ship-entity* 'orientation)
      (with-slots (acceleration angular-velocity) (p2de:find-component *player-ship-entity* 'kinematics)
        (let ((key-code (sdl2:scancode-symbol (sdl2:scancode-value key)))
              (direction (p2dm:rotated-vector-2d (p2dm:make-vector-2d 0.0 1.0) orientation)))
          (log:trace key state key-code repeat)
          
          (case key-code
            (:scancode-up (setf acceleration (p2dm:scaled-vector direction (if (sdl2:key-down-p state) 150.0 0.0))))
            (:scancode-down (setf acceleration (p2dm:scaled-vector direction (if (sdl2:key-down-p state) -100.0 0.0))))
            (:scancode-left (setf angular-velocity (if (sdl2:key-down-p state) (p2dm:deg->rad 180.0) 0.0)))
            (:scancode-right (setf angular-velocity (if (sdl2:key-down-p state) (p2dm:deg->rad -180.0) 0.0)))
            (:scancode-space (shoot-bullet position direction))
            (t nil))

          (when (eql key-code :scancode-escape)
            (sdl2:push-event :quit)))))))

(defmethod p2d:on-tick ((game asteroids-game) dt)
  ;; handled entirely by ECS for now
  )

(defmethod p2d:on-idle ((game asteroids-game))
  ;; FIXME temporary hack to make ECS work together with current system
  (gl:load-identity)
  (gl:clear :color-buffer))

(defmethod p2d:on-render ((game asteroids-game))
  ;; draw stuff

  (gl:load-identity)

  ;; handled entirely by ECS for now
  
  (gl:flush)
  (sdl2:gl-swap-window p2d:*main-window*))

(defun run ()
  (p2d:run (make-instance 'asteroids-game)))

;;; game

(defun shoot-bullet (start-position direction)
  (spawn-bullet (p2dm:scaled-vector start-position 1.0) ;FIXME hack for missing (clone ...) ability
                      (p2dm:scaled-vector direction 500)
                      2.0
                      1.5
                      nil))
