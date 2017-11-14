;;;; Main game screen

(in-package #:tswr-asteroids)



(defclass main-game-screen (game-screen)
  ((primary-ui-font :initform nil
                    :accessor main-game-primary-ui-font)
   (secondary-ui-font :initform nil
                      :accessor main-game-secondary-ui-font)))



(defmethod on-create ((screen main-game-screen))
  (setf (main-game-primary-ui-font screen)
        (or (main-game-primary-ui-font screen)
            (p2dg:get-rendered-font "fonts/Vera/VeraMoBd.ttf" :size 16))

        (main-game-secondary-ui-font screen)
        (or (main-game-secondary-ui-font screen)
            (p2dg:get-rendered-font "fonts/Vera/VeraMono.ttf" :size 12))))

(defmethod on-destroy ((screen main-game-screen))
  ;; We let the end-game deinitialization handle these.
  ;;(p2dg:free-font (main-game-primary-ui-font screen))
  ;;(p2dg:free-font (main-game-secondary-ui-font screen))
  )

(defmethod on-phase-in :before ((screen main-game-screen) previous-screen)
  (declare (ignore previous-screen))
  ;; TODO generic game initialization
  )

(defmethod on-phase-in ((screen main-game-screen) (previous-screen get-ready-screen))
  ;; TODO additional initialization - clear score, et al.
  ;; Done by checking if the get-ready screen was invoked from menu.
  ;; Or maybe the get-ready screen will handle resetting things by itself.
  )

(defmethod on-phase-out ((screen main-game-screen) next-screen)
  (declare (ignore screen next-screen))
  ;; Not sure if there's anything to do in here.
)

(defmethod on-key-event ((screen main-game-screen) key state repeat)
  ;; TODO maybe make that macrolet part of the engne? It's too useful.
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

(defmethod on-tick ((screen main-game-screen) dt)
  (tick-generic-game-rules dt)
  (p2de:tick-simulation-systems dt))

(defmethod on-render ((screen main-game-screen) dt)
  ;; draw stuff

  (p2de:tick-frame-systems dt) ;FIXME why it's here and not in on-idle??

  ;; Draw UI
  ;; FIXME move somewhere else
  ;; Score
  (p2dg:with-color (0 1 0)
    (p2dg::draw-text (format nil "~10D" (floor *score*))
                     :font (main-game-primary-ui-font screen)
                     :x 680
                     :y 580))
  (p2dg:with-color (1 1 1)
    (p2dg::draw-text (format nil "High score: ~10D" (floor *high-score*))
                     :font (main-game-secondary-ui-font screen)
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
