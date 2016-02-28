(in-package #:tswr-asteroids)

(p2de:defcomponent renderable
  ((sprite nil)
   (color (p2dg:make-color-4))
   (scale 1.0)))
