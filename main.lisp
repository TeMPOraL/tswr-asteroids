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
  (initialize-systems '((input :priority 0)
                        (basic-physics :priority 1)
                        (collision-detector :priority 2)
                        (game-area-wrapper :priority 10)
                        (gun-cooldown-updater :priority 20)
                        (decayer :priority 30)
                        (ship-effects :priority 40)
                        (renderer :priority 50)))
  
  ;; and an entity
  (spawn-asteroid (p2dm:make-vector-2d 600.0 400.0) 40 (p2dm:make-vector-2d 40.0 60.0))
  (spawn-asteroid (p2dm:make-vector-2d 200.0 300.0) 30 (p2dm:make-vector-2d -50.0 45.0))
  (setf *player-ship-entity* (spawn-ship (p2dm:make-vector-2d 200.0 400.0))))

(defun initialize-systems (systems)
  (mapc (lambda (system-definition)
          (apply #'p2de:register-system system-definition))
        systems))

(defmethod p2d:deinitialize ((game asteroids-game))
  (log:info "TSWR - Asteroids game deinit."))

(defmethod p2d:on-key-event ((game asteroids-game) key state repeat)
  (macrolet ((on-key-down (scancode &body code)
               `(when (and (eql key-code ,scancode)
                           (sdl2:key-down-p state))
                  ,@code)))
   (let ((key-code (sdl2:scancode-symbol (sdl2:scancode-value key))))
     (log:trace key state key-code repeat)

     ;; player input handled by ECS for now
     (on-key-down :scancode-f1
                  (renderer-toggle-debug-kinematics))

     (on-key-down :scancode-f2
                  (renderer-toggle-debug-collision))

     (on-key-down :scancode-escape
                  (sdl2:push-event :quit)))))

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


