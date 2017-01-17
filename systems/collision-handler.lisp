(in-package #:tswr-asteroids)

(p2de:defsystem collision-handler
  (collision-pair))

(defmethod p2de:do-system ((system collision-handler) entity dt)
  (declare (ignore system dt))
  (let ((collision-pair (p2de:find-component entity 'collision-pair)))
    (handle-collision (slot-value collision-pair 'entity-1-id)
                      (slot-value collision-pair 'entity-2-id)))
  (p2de:schedule-entity-for-deletion entity))

