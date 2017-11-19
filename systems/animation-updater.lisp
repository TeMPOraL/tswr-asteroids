(in-package #:tswr-asteroids)

(p2de:defsystem animation-updater
  (animation))

(defmethod p2de:do-system ((system animation-updater) entity dt)
  (declare (ignore system))
  (let ((animation (p2de:find-component entity 'animation)))
    (with-slots (animation-type current-time animation-length looped)
        animation
      (setf current-time
            (if looped
                (mod (+ current-time dt) animation-length)
                (min (+ current-time dt) animation-length)))

      (apply-animation-action entity animation))))

(defun apply-animation-action (entity animation)
  ;; NOTE we're doing animations like sprites - with lambdas.
  (funcall (slot-value animation 'animation-type)
           entity animation))

(defun make-null-animation ()
  (lambda (entity animation)
    (declare (ignore entity animation))))

(defun make-blink-decay-animation ()
  "Starts blinking near the end of animation time.
To be used with decaying objects.
Decay is done by setting color of a renderable."
  (let ((stored-color (p2dg:make-color-4))
        (null-color (p2dg:make-color-4)))
    (lambda (entity animation)
      (when-let ((renderable (p2de:find-component entity 'renderable)))
        (with-slots (current-time animation-length) animation
          (with-slots (color) renderable
            ;; 30% - XXX hardcoded
            (let ((blink-threshold (- animation-length
                                      (* animation-length 0.3))))

              (if (< current-time blink-threshold)
                  ;; store current color
                  (setf stored-color color)

                  ;; else, do the blinking for 0.15 second every 0.5 second?
                  ;; XXX hardcoded
                  (if (multiple-value-bind (main frac)
                          (truncate (* 2.0 current-time))
                        (declare (ignore main))
                        (< frac 0.3))
                      (setf color null-color)
                      (setf color stored-color))))))))))
