(in-package #:tswr-asteroids)

(p2de:defsystem collision-handler
  (collision-pair))

(defmethod p2de:do-system ((system collision-handler) entity dt)
  (declare (ignore system dt))
  (let ((collision-pair (p2de:find-component entity 'collision-pair)))
    (handle-collision (slot-value collision-pair 'entity-1-id)
                      (slot-value collision-pair 'entity-2-id)))
  (p2de:schedule-entity-for-deletion entity))

(defun handle-collision (entity-1-id entity-2-id)
  (let ((entity-1 (p2de:entity-by-id entity-1-id))
        (entity-2 (p2de:entity-by-id entity-2-id)))

    ;; TODO dispatch; basically, game rules go here (so maybe migrate it to some game rules file?)
    
    (when (bulletp entity-1)            ;NOTE test rule - bullet disappears when asteroid hit
      (p2de:schedule-entity-for-deletion entity-1))
    (when (asteroidp entity-1)          ;NOTE test rule - asteroid disappears when hit by something
      (spawn-child-asteroids entity-1)
      ;; TODO maybe spawn powerup
      (p2de:schedule-entity-for-deletion entity-1))))

;;; FIXME those function REALLY need to be locally scoped...
;;; Either flet them, or introduce more namespaces via packages.
(defun bulletp (entity)
  (p2de:find-component entity 'bullet))

(defun shipp (entity)
  (p2de:find-component entity 'ship))

(defun asteroidp (entity)
  (p2de:find-component entity 'asteroid))
