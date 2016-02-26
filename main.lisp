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
  
  (with-slots (acceleration angular-velocity orientation) *player-ship*
    (let ((key-code (sdl2:scancode-symbol (sdl2:scancode-value key)))
          (direction (p2dm:rotated-vector-2d (p2dm:make-vector-2d 0.0 1.0) (p2dm:deg->rad orientation))))
      (log:trace key state key-code repeat)
      
      (case key-code
        (:scancode-up (setf acceleration (p2dm:scaled-vector direction (if (sdl2:key-down-p state) 150.0 0.0))))
        (:scancode-down (setf acceleration (p2dm:scaled-vector direction (if (sdl2:key-down-p state) -100.0 0.0))))
        (:scancode-left (setf angular-velocity (if (sdl2:key-down-p state) 180.0 0.0)))
        (:scancode-right (setf angular-velocity (if (sdl2:key-down-p state) -180.0 0.0)))
        (t nil))

      (when (eql key-code :scancode-escape)
        (sdl2:push-event :quit)))))

(defmethod p2d:on-tick ((game asteroids-game) dt)
  (update-motion *player-ship* dt)
  (mapc (lambda (entity)
          (update-motion entity dt))
        *asteroids*)

  (mapc #'wrap-into-play-area (append *asteroids* (list *player-ship*))))

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

(defun wrap-into-play-area (object)
  (with-slots (position) object
    (setf (p2dm:vec-x position) (wrap-in-range (p2dm:vec-x position) -1 801)
          (p2dm:vec-y position) (wrap-in-range (p2dm:vec-y position) -1 601))))

;;; util
(defun wrap-in-range (x a b)
  (setf x (+ a (mod (- x a) (- b a)))))
