;;;; Defeat screen

(in-package #:tswr-asteroids)



(defclass defeat-screen (banner-screen)
  ())



(defmethod on-render ((screen defeat-screen) dt)
  (declare (ignore dt))
  (p2dg:with-color (1 0 0)
    (draw-text "DEFEAT!"
               :size 72
               :x 400
               :y +layout-title-y-centerline+
               :alignment-x :center)))

