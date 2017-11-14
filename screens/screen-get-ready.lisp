;;;; Get ready screen

(in-package #:tswr-asteroids)



(defparameter +default-banner-screen-timeout+ 1.5)



(defclass get-ready-screen (game-screen)
  ((new-game-p :initform nil
               :accessor get-ready-new-game-p)

   (main-font :initform nil
              :accessor get-ready-main-font)

   (time-to-switch :initform +default-banner-screen-timeout+
                   :initarg :time-to-switch
                   :accessor get-ready-time-to-switch)

   (switch-counter :initform 0
                   :accessor get-ready-switch-counter)))



(defmethod on-create ((screen get-ready-screen))
  (setf (get-ready-main-font screen)
        (p2dg:get-rendered-font "fonts/Vera/VeraMoBd.ttf" :size 24)))

(defmethod on-phase-in :before ((screen get-ready-screen) previous-screen)
  (declare (ignore previous-screen))
  (setf (get-ready-switch-counter screen)
        (get-ready-time-to-switch screen)))

(defmethod on-phase-in ((screen get-ready-screen) previous-screen)
  (declare (ignore previous-screen))
  (setf (get-ready-new-game-p screen) nil))

(defmethod on-phase-in ((screen get-ready-screen) (previous-screen menu-screen))
  (declare (ignore previous-screen))
  (setf (get-ready-new-game-p screen) t))

(defmethod on-tick ((screen get-ready-screen) dt)
  (decf (get-ready-switch-counter screen) dt)
  (when (< (get-ready-switch-counter screen) 0.0)
    (switch-game-screen :main-game)))

(defmethod on-render ((screen get-ready-screen) dt)
  (p2dg:with-color (1 1 1)
    (p2dg::draw-text "GET READY!"
                     :font (get-ready-main-font screen)
                     :x 300
                     :y 300)))

