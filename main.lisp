(in-package #:tswr-asteroids)

(defparameter *ship-x* 0)
(defparameter *ship-y* 0)
(defparameter *ship-vx* 0)
(defparameter *ship-vy* 0)
(defparameter *ship-rot* 0)
(defparameter *ship-vrot* 0)

(defclass asteroids-game (p2d:game)
  ())

(defmethod p2d:preinit ((game asteroids-game))
  ;; TODO preconfiguration (if any)
  (setf p2d:*window-title* "TSWR - Asteroids") ; FIXME maybe pass it through the game class?
  (setf p2d:*use-fixed-timestep* t))

(defmethod p2d:initialize ((game asteroids-game))
  (log:info "TSWR - Asteroids game init.")
  (gl:clear-color 0.0 0.0 0.0 1.0))

(defmethod p2d:deinitialize ((game asteroids-game))
  (log:info "TSWR - Asteroids game deinit."))

(defmethod p2d:on-key-event ((game asteroids-game) key state)
  (let ((key-code (sdl2:scancode-symbol (sdl2:scancode-value key))))
    (log:trace key state key-code)
    (case key-code
      (:scancode-up (setf *ship-vy* (if (sdl2:key-down-p state) 1 0.0)))
      (:scancode-down (setf *ship-vy* (if (sdl2:key-down-p state) -1 0.0)))
      (:scancode-left (setf *ship-vrot* (if (sdl2:key-down-p state) -180 0.0)))
      (:scancode-right (setf *ship-vrot* (if (sdl2:key-down-p state) 180 0.0)))
      (t nil))

    (when (eql key-code :scancode-escape)
      (sdl2:push-event :quit))))

(defmethod p2d:on-tick ((game asteroids-game) dt)
  (update-ship dt))

(defmethod p2d:on-render ((game asteroids-game))
  ;; draw stuff

  (gl:clear :color-buffer)

  (draw-ship)

  (gl:flush)
  (sdl2:gl-swap-window p2d:*main-window*))

(defun run ()
  (p2d:run (make-instance 'asteroids-game)))


;;; game
(defun update-ship (dt)
  (incf *ship-x* (* *ship-vx* dt))
  (incf *ship-y* (* *ship-vy* dt))

  (setf *ship-rot* (mod (+ *ship-rot* (* *ship-vrot* dt))
                        360)))

(defun draw-ship ()
  (gl:load-identity)
  (gl:translate *ship-x* *ship-y* 0)
  (gl:rotate *ship-rot* 0 0 1)

  (gl:with-primitive :triangles
    (gl:color 1.0 0.0 0.0)
    (gl:vertex 0.0 0.1)
    (gl:vertex -0.07 -0.1)
    (gl:vertex 0.07 -0.1)))
