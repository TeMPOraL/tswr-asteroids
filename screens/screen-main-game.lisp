;;;; Main game screen

(in-package #:tswr-asteroids)



(defclass main-game-screen (game-screen)
  ((paused-p :initform nil
             :accessor main-game-paused-p)
   (starfield :initform nil
              :accessor main-game-starfield)))



(defmethod on-phase-in :before ((screen main-game-screen) previous-screen)
  (declare (ignore previous-screen))
  ;; generic game initialization
  (setf (main-game-paused-p screen) nil)
  (setf (main-game-starfield screen) (make-starfield 200)))

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
                   (setf (main-game-paused-p screen)
                         (not (main-game-paused-p screen))))
      (on-key-down :scancode-q
                   (when (main-game-paused-p screen)
                     (abort-game))))))

(defmethod on-tick ((screen main-game-screen) dt)
  (unless (main-game-paused-p screen)
    (tick-generic-game-rules dt)
    (p2de:tick-simulation-systems dt)))

(defmethod on-render ((screen main-game-screen) dt)
  ;; draw stuff
  (draw-starfield (main-game-starfield screen))

  ;; XXX that we don't have to condition this out on pause
  ;; XXX is only because of accident - the only :frame system
  ;; XXX we use is the rendering system.
  (p2de:tick-frame-systems dt) ;FIXME why it's here and not in on-idle??

  ;; Draw UI
  ;; FIXME move somewhere else
  ;; Score
  (p2dg:with-color (0 1 0)
    (draw-text (format nil "~10D" (floor *score*))
               :font +bold-font+
               :size 12
               :x 790
               :y 585
               :alignment-x :right
               :transient t))
  (p2dg:with-color (1 1 1)
    (draw-text (format nil "High score: ~10D" (floor *high-score*))
               :size 12
               :x 790
               :y 570
               :alignment-x :right
               :transient t))

  ;; Lives
  (p2dg:with-color (1 1 0)
    (dotimes (i *lives*)
      (gl:with-pushed-matrix
        (gl:translate (+ 10 (* i 20))
                      580
                      0)
        (gl:scale 6 9 6)
        (p2dglu:draw-triangle-outline))))

  (when (main-game-paused-p screen)
    (p2dg:with-color (1 1 1)
      (draw-text "PAUSED"
                 :size 72
                 :x 400
                 :y +layout-title-y-centerline+
                 :alignment-x :center)
      (draw-text "Press Q to quit to main menu."
                 :size 16
                 :x 400
                 :y +layout-secondary-first-centerline+
                 :alignment-x :center)
      (draw-text "Press ESC to unpause."
                 :size 16
                 :x 400
                 :y (- +layout-secondary-first-centerline+
                       20)
                 :alignment-x :center))))
