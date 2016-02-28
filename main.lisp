(in-package #:tswr-asteroids)

(defclass asteroids-game (p2d:game)
  ())

(defparameter *player-ship* (make-instance '~ship
                                           :position (p2dm:make-vector-2d 400.0 300.0)
                                           :color (p2dm:make-vector-4d 0.0 1.0 0.0 1.0)))

(defparameter *asteroids* (list (make-instance '~asteroid
                                               :position (p2dm:make-vector-2d 100.0 100.0)
                                               :velocity (p2dm:make-vector-2d (* 100 (p2dm:random-float -1.0 1.0)) (* 100 (p2dm:random-float -1.0 1.0)))
                                               :angular-velocity (p2dm:random-float -100 100)
                                               :color (p2dm:make-vector-4d 1.0 1.0 1.0 1.0))))

(defparameter *bullets* '())

(defmethod p2d:preinit ((game asteroids-game))
  ;; TODO preconfiguration (if any)
  (setf p2d:*window-title* "TSWR - Asteroids") ; FIXME maybe pass it through the game class?
  (setf p2d:*use-fixed-timestep* t))

(defmethod p2d:initialize ((game asteroids-game))
  (log:info "TSWR - Asteroids game init.")
  (gl:clear-color 0.0 0.0 0.0 1.0)

  ;; add some systems
  (log:debug "Booting up ECS...")
  (p2de:register-system 'basic-physics)
  (p2de:register-system 'game-area-wrapper)
  (p2de:register-system 'decayer)
  (p2de:register-system 'renderer)

  ;; and an entity
  (spawn-asteroid (p2dm:make-vector-2d 600.0 400.0) 40 (p2dm:make-vector-2d 40.0 60.0))
  (spawn-asteroid (p2dm:make-vector-2d 200.0 300.0) 30 (p2dm:make-vector-2d -50.0 45.0))
  (spawn-bullet (p2dm:make-vector-2d 100.0 100.0) (p2dm:make-vector-2d 0.0 50.0) 5.0 5.0 nil)
  (spawn-ship (p2dm:make-vector-2d 200.0 400.0)))



(defmethod p2d:deinitialize ((game asteroids-game))
  (log:info "TSWR - Asteroids game deinit."))

(defmethod p2d:on-key-event ((game asteroids-game) key state repeat)
  (declare (ignore repeat))
  
  (with-slots (position acceleration angular-velocity orientation) *player-ship*
    (let ((key-code (sdl2:scancode-symbol (sdl2:scancode-value key)))
          (direction (p2dm:rotated-vector-2d (p2dm:make-vector-2d 0.0 1.0) (p2dm:deg->rad orientation))))
      (log:trace key state key-code repeat)
      
      (case key-code
        (:scancode-up (setf acceleration (p2dm:scaled-vector direction (if (sdl2:key-down-p state) 150.0 0.0))))
        (:scancode-down (setf acceleration (p2dm:scaled-vector direction (if (sdl2:key-down-p state) -100.0 0.0))))
        (:scancode-left (setf angular-velocity (if (sdl2:key-down-p state) 180.0 0.0)))
        (:scancode-right (setf angular-velocity (if (sdl2:key-down-p state) -180.0 0.0)))
        (:scancode-space (shoot-bullet position direction))
        (t nil))

      (when (eql key-code :scancode-escape)
        (sdl2:push-event :quit)))))

(defmethod p2d:on-tick ((game asteroids-game) dt)
  (mapc (lambda (ent)
          (update-motion ent dt))
        (append (list  *player-ship*) *asteroids* *bullets*))

  (mapc (lambda (ent)
          (update-logic ent dt))
        (append (list  *player-ship*) *asteroids* *bullets*))
  
  ;; TODO process collisions & stuff

  (setf *bullets* (delete-if #'deadp *bullets*))
  
  (mapc #'wrap-into-play-area (append *asteroids* *bullets* (list *player-ship*))))

(defmethod p2d:on-idle ((game asteroids-game))
  ;; FIXME temporary hack to make ECS work together with current system
  (gl:load-identity)
  (gl:clear :color-buffer))

(defmethod p2d:on-render ((game asteroids-game))
  ;; draw stuff

  (gl:load-identity)

  (render *player-ship*)
  (mapc #'render *asteroids*)
  (mapc #'render *bullets*)

  (gl:flush)
  (sdl2:gl-swap-window p2d:*main-window*))

(defun run ()
  (p2d:run (make-instance 'asteroids-game)))

;;; game

(defun wrap-into-play-area (object)
  (with-slots (position) object
    (setf (p2dm:vec-x position) (wrap-in-range (p2dm:vec-x position) -1 801)
          (p2dm:vec-y position) (wrap-in-range (p2dm:vec-y position) -1 601))))

(defun shoot-bullet (start-position direction)
  (push (make-instance '~bullet
                       :position (p2dm:scaled-vector start-position 1.0)  ;FIXME hack for missing (clone ...) ability
                       :velocity (p2dm:scaled-vector direction 500)
                       :color (p2dm:make-vector-4d 1.0 1.0 0.0 1.0)
                       :life-time 1.5) *bullets*))

;;; util
(defun wrap-in-range (x a b)
  (setf x (+ a (mod (- x a) (- b a)))))

