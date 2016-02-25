(in-package #:tswr-asteroids)

(defclass asteroids-game (p2d:game)
  ())

(defmethod p2d:preinit ((game asteroids-game))
  ;; TODO preconfiguration (if any)
  (setf p2d:*window-title* "TSWR - Asteroids") ; FIXME maybe pass it through the game class?
  (setf p2d:*use-fixed-timestep* t)
  (setf p2d:*update-step* 0.5)
  (setf p2d:*max-accumulated-timestep* 2))

(defmethod p2d:initialize ((game asteroids-game))
  (log:info "TSWR - Asteroids game init."))

(defmethod p2d:deinitialize ((game asteroids-game))
  (log:info "TSWR - Asteroids game deinit."))

(defmethod p2d:on-key-event ((game asteroids-game) key state)
  (log:info key state (sdl2:scancode-symbol (sdl2:scancode-value key))))

(defmethod p2d:on-tick ((game asteroids-game) dt)
  ;; TICK
  (log:info dt))

(defmethod p2d:on-render ((game asteroids-game))
  ;; draw stuff
  )

(defun run ()
  (p2d:run (make-instance 'asteroids-game)))
