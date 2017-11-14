;;;; Defeat screen

(in-package #:tswr-asteroids)



(defparameter +default-defeat-screen-timeout+ 1.5)



(defclass defeat-screen (game-screen)
  ((main-font :initform nil
              :accessor defeat-main-font)

   (time-to-switch :initform +default-defeat-screen-timeout+
                   :initarg :time-to-switch
                   :accessor defeat-time-to-switch)

   (switch-counter :initform 0
                   :accessor defeat-switch-counter)))



(defmethod on-create ((screen defeat-screen))
  (setf (defeat-main-font screen)
        (p2dg:get-rendered-font "fonts/Vera/VeraMoBd.ttf" :size 24)))

(defmethod on-phase-in ((screen defeat-screen) previous-screen)
  (declare (ignore previous-screen))
  (setf (defeat-switch-counter screen)
        (defeat-time-to-switch screen)))

(defmethod on-tick ((screen defeat-screen) dt)
  (decf (defeat-switch-counter screen) dt)
  (when (< (defeat-switch-counter screen) 0.0)
    (switch-game-screen :menu)))

(defmethod on-render ((screen defeat-screen) dt)
  (p2dg:with-color (1 1 1)
    (p2dg::draw-text "DEFEAT!"
                     :font (defeat-main-font screen)
                     :x 300
                     :y 300)))

