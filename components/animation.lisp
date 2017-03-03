(in-package #:tswr-asteroids)

(p2de:defcomponent animation
  ((animation-type nil)
   (current-time 0.0)
   (animation-length 0.0)
   (looped nil)))
