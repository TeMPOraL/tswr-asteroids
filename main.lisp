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
  (mapc #'p2de:register-system '(input
                                 basic-physics
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
  (let ((key-code (sdl2:scancode-symbol (sdl2:scancode-value key))))
    (log:trace key state key-code repeat)

    ;; player input handled by ECS for now

    (when (and (eql key-code :scancode-f1)
               (sdl2:key-down-p state))
      (setf *debug-render-kinematics* (not *debug-render-kinematics*)))
    (when (eql key-code :scancode-escape)
      (sdl2:push-event :quit))))

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


