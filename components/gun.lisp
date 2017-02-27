(in-package #:tswr-asteroids)

(p2de:defcomponent gun
  ((bullet-type nil)
   (cooldown-default 1.0)
   (cooldown-left 0.0)
   (default-bullet-velocity 300.0)             ;FIXME magic
   (buffs '())))
