(in-package #:tswr-asteroids)

(defclass asteroids-game (p2d:game)
  ())

(defmethod p2d:preinit ((game asteroids-game))
  ;; TODO preconfiguration (if any)
  (setf p2d:*window-title* "TSWR - Asteroids") ; FIXME maybe pass it through the game class?
  )

(defmethod p2d:initialize ((game asteroids-game))
  (log:info "TSWR - Asteroids game init."))

(defmethod p2d:deinitialize ((game asteroids-game))
  (log:info "TSWR - Asteroids game deinit."))

(defun run ()
  (p2d:run (make-instance 'asteroids-game)))
