(in-package #:tswr-asteroids)

(p2de:defcomponent gun
  ((bullet-type nil)
   (cooldown-left 0.0)
   (buffs '())))
