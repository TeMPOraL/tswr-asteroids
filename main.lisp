(in-package #:tswr-asteroids)

(defclass asteroids-game (p2d:game)
  ())

(defmethod p2d:preinit ((game asteroids-game))
  ;; TODO preconfiguration (if any)
  (setf p2d:*window-title* "TSWR - Asteroids") ; FIXME maybe pass it through the game class?
  (setf p2d:*use-fixed-timestep* t)
  ;; (setf p2d:*window-width* 1024)
  ;; (setf p2d:*window-height* 768)
  (setf p2d:*window-width* 800)
  (setf p2d:*window-height* 600)
  (setf p2d:*window-resizable* nil))

(defmethod p2d:initialize ((game asteroids-game))
  (log:info "TSWR - Asteroids game init.")
  (gl:clear-color 0.0 0.0 0.0 1.0)

  ;; (sdl2:gl-set-swap-interval 0) <-- use to disable vsync

  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:enable :blend)
  (gl:enable :line-smooth)
  (gl:hint :line-smooth-hint :nicest)
  (gl:enable :polygon-smooth)
  (gl:hint :polygon-smooth-hint :nicest)
  (gl:enable :texture-2d)

  ;; add some systems
  (log:debug "Booting up ECS...")
  (p2de:init-ecs)
  (initialize-systems '((input :priority 0 :type :simulation)
                        (collision-detector :priority 2 :type :simulation)
                        (collision-handler :priority 3 :type :simulation)
                        (basic-physics :priority 10 :type :simulation)
                        (game-area-customs :priority 15 :type :simulation)
                        (gun-cooldown-updater :priority 20 :type :simulation)
                        (decayer :priority 30 :type :simulation)
                        (ship-effects :priority 40 :type :simulation)
                        (animation-updater :priority 45 :type :simulation)
                        (renderer :priority 50 :type :frame)))

  (initialize-game-screens)
  (switch-game-screen :menu)

  (setf *game-over* t))

(defun initialize-systems (systems)
  (mapc (lambda (system-definition)
          (apply #'p2de:register-system system-definition))
        systems))

(defmethod p2d:deinitialize ((game asteroids-game))
  (log:info "TSWR - Asteroids game deinit.")

  (deinitialize-game-screens)

  (p2de:deinit-ecs)

  ;; FIXME shouldn't be called by the game, IMO -.-
  (p2dg::clear-font-cache))

(defmethod p2d:on-key-event ((game asteroids-game) key state repeat)
  (declare (ignore game))
  (on-key-event *current-game-screen* key state repeat))

(defmethod p2d:on-tick ((game asteroids-game) dt)
  (process-game-screen-lifecycle)
  (on-tick *current-game-screen* dt))

(defmethod p2d:on-idle ((game asteroids-game) dt)
  (declare (ignore game))
  ;; Nothing to do for now.
  (on-idle *current-game-screen* dt)
  
  (p2dprof:count-value (hash-table-count (p2de::entities p2de:*ecs-manager*))
                       'ecs-entities
                       :description "no. of entities that exist on current frame"
                       :interval :frame
                       :history-size 120))

(defmethod p2d:on-render ((game asteroids-game) dt)
  (declare (ignore game))
  (on-render *current-game-screen* dt))

(defun run ()
  (p2d:run :game (make-instance 'asteroids-game)))

(defun run-with-profiling (&optional (mode :cpu))
  (p2d:run :game (make-instance 'asteroids-game)
           :profiling-mode mode))
