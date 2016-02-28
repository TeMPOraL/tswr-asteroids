(in-package #:tswr-asteroids)

(p2de:defsystem renderer
  (position renderable))

(defmethod p2de:do-system ((system renderer) entity dt)
  (declare (ignore system dt))
  (with-slots (position) (p2de:find-component entity 'position)
    (with-slots (sprite color scale) (p2de:find-component entity 'renderable)
      ;;
      (gl:load-identity)
      (p2dg:translate2 position)
      (if-let ((orientation (p2de:find-component entity 'orientation)))
        (p2dg:rotatez* (orientation orientation)))
      (p2dg:scale2-uniform scale)
      (p2dg:color4 color)
      (draw-pseudo-sprite sprite))))

(defun draw-pseudo-sprite (sprite)
  "Pseudo because it's not really a `SPRITE', but rather immediate-mode geometry."
  (case sprite
    (:asteroid (p2dg:draw-regular-polygon-outline 7))
    (:ship (gl:scale 1 1.5 1)
           (p2dg:draw-triangle))
    (:bullet (p2dg:draw-circle-outline :resolution 16))
    (t (p2dg:draw-square))))
