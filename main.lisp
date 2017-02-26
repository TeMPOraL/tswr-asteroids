(in-package #:tswr-asteroids)

(defclass asteroids-game (p2d:game)
  ())

(defparameter *default-mono-font* nil)
(defparameter *smaller-mono-font* nil)

(defmethod p2d:preinit ((game asteroids-game))
  ;; TODO preconfiguration (if any)
  (setf p2d:*window-title* "TSWR - Asteroids") ; FIXME maybe pass it through the game class?
  (setf p2d:*use-fixed-timestep* t)
  (setf p2d:*window-width* 1024)
  (setf p2d:*window-height* 768)
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
                        (renderer :priority 50 :type :frame)))

  ;; Load additional resources needed
  (setf *default-mono-font* (p2dg:get-rendered-font "assets/fonts/VeraMoBd.ttf" :size 16))
  (setf *smaller-mono-font* (p2dg:get-rendered-font "assets/fonts/VeraMono.ttf" :size 12))

  (setf *game-over* t))

(defun initialize-systems (systems)
  (mapc (lambda (system-definition)
          (apply #'p2de:register-system system-definition))
        systems))

(defmethod p2d:deinitialize ((game asteroids-game))
  (log:info "TSWR - Asteroids game deinit.")

  (p2dg:free-font *smaller-mono-font*)
  (p2dg:free-font *default-mono-font*)
  
  (p2de:deinit-ecs))

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

     (on-key-down :scancode-f5
                  (debug-spawn-asteroid))

     (on-key-down :scancode-f6
                  (debug-spawn-powerup))

     (on-key-down :scancode-escape
                  (sdl2:push-event :quit)))))

(defmethod p2d:on-tick ((game asteroids-game) dt)
  (tick-generic-game-rules dt)
  (p2de:tick-simulation-systems dt))

(defmethod p2d:on-idle ((game asteroids-game) dt)
  (declare (ignore game dt))
  ;; Nothing to do for now.
  (p2dprof:count-value (hash-table-count (p2de::entities p2de:*ecs-manager*))
                       'ecs-entities
                       :description "no. of entities that exist on current frame"
                       :interval :frame
                       :history-size 120))

(defmethod p2d:on-render ((game asteroids-game) dt)
  (declare (ignore game))
  ;; draw stuff

  (p2de:tick-frame-systems dt)          ;FIXME why it's here and not in on-idle??

  ;; Draw UI
  ;; FIXME move somewhere else
  ;; Score
  (p2dg:with-color (0 1 0)
   (p2dg::draw-text (format nil "~10D" (floor *score*))
                    :font *default-mono-font*
                    :x 680
                    :y 580))
  (p2dg:with-color (1 1 1)
    (p2dg::draw-text (format nil "High score: ~10D" (floor *high-score*))
                     :font *smaller-mono-font*
                     :x 600
                     :y 560))

  ;; Lives
  (p2dg:with-color (1 1 0)
    (dotimes (i *lives*)
      (gl:with-pushed-matrix
        (gl:translate (+ 10 (* i 20))
                      580
                      0)
        (gl:scale 6 9 6)
        (p2dglu:draw-triangle-outline)))))

(defun run ()
  (p2d:run :game (make-instance 'asteroids-game)))

(defun run-with-profiling (&optional (mode :cpu))
  (p2d:run :game (make-instance 'asteroids-game)
           :profiling-mode mode))
