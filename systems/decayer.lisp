(in-package #:tswr-asteroids)

(p2de:defsystem decayer
  (decays))

(defmethod p2de:do-system ((system decayer) entity dt)
  (with-slots (life-remaining) (p2de:find-component entity 'decays)
    (decf life-remaining dt)
    (when (< life-remaining 0)
      (p2de:schedule-entity-for-deletion entity))))
