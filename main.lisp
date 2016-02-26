(in-package #:tswr-asteroids)

(defclass asteroids-game (p2d:game)
  ())

(defparameter *player-ship* (make-instance 'ship
                                           :position (p2dm:make-vector-2d 400.0 300.0)
                                           :color (p2dm:make-vector-4d 0.0 1.0 0.0 1.0)))

(defparameter *asteroids* (list (make-instance 'asteroid
                                               :position (p2dm:make-vector-2d 100.0 100.0)
                                               :color (p2dm:make-vector-4d 1.0 1.0 1.0 1.0))))

(defmethod p2d:preinit ((game asteroids-game))
  ;; TODO preconfiguration (if any)
  (setf p2d:*window-title* "TSWR - Asteroids") ; FIXME maybe pass it through the game class?
  (setf p2d:*use-fixed-timestep* t))

(defmethod p2d:initialize ((game asteroids-game))
  (log:info "TSWR - Asteroids game init.")
  (gl:clear-color 0.0 0.0 0.0 1.0))

(defmethod p2d:deinitialize ((game asteroids-game))
  (log:info "TSWR - Asteroids game deinit."))

(defmethod p2d:on-key-event ((game asteroids-game) key state repeat)
  (declare (ignore repeat))
  (with-slots (velocity angular-velocity) *player-ship*
    (let ((key-code (sdl2:scancode-symbol (sdl2:scancode-value key))))
      (log:trace key state key-code repeat)
      (case key-code
        (:scancode-up (setf (p2dm:vec-y velocity) (if (sdl2:key-down-p state) 200.0 0.0)))
        (:scancode-down (setf (p2dm:vec-y velocity) (if (sdl2:key-down-p state) -200.0 0.0)))
        (:scancode-left (setf angular-velocity (if (sdl2:key-down-p state) -180.0 0.0)))
        (:scancode-right (setf angular-velocity (if (sdl2:key-down-p state) 180.0 0.0)))
        (t nil))

      (when (eql key-code :scancode-escape)
        (sdl2:push-event :quit)))))

(defmethod p2d:on-tick ((game asteroids-game) dt)
  (update-motion *player-ship* dt)
  (mapc (lambda (entity)
          (update-motion entity dt))
        *asteroids*))

(defmethod p2d:on-render ((game asteroids-game))
  ;; draw stuff

  (gl:clear :color-buffer)
  (gl:load-identity)

  (render *player-ship*)
  (mapc #'render *asteroids*)

  (gl:flush)
  (sdl2:gl-swap-window p2d:*main-window*))

(defun run ()
  (p2d:run (make-instance 'asteroids-game)))


;;; game

(defun draw-ship ()

)
