;;;; Menu screen

(in-package #:tswr-asteroids)



(defclass menu-screen (game-screen)
  ((options :accessor game-menu-options)
   (current-option :accessor game-menu-current-option)))


(defmethod on-create ((screen menu-screen))
  (setf (game-menu-options screen)
        `(("New Game" . ,(lambda () (switch-game-screen :get-ready)))
          ("Quit" . ,(lambda () (sdl2:push-event :quit))))))

(defmethod on-destroy ((screen menu-screen))
  (setf (game-menu-options screen) nil))

(defmethod on-phase-in ((screen menu-screen) previous-screen)
  (declare (ignore previous-screen))
  (setf (game-menu-current-option screen) 0))

(defmethod on-key-event ((screen menu-screen) key state repeat)
  (flet ((advance-option (step)
           (setf (game-menu-current-option screen)
                 (mod (+ (game-menu-current-option screen) step)
                      (length (game-menu-options screen)))))
         (activate-option ()
           (funcall (cdr (nth (game-menu-current-option screen)
                              (game-menu-options screen))))))
    
    ;; TODO maybe make that macrolet part of the engne? It's too useful.
    (macrolet ((on-key-down (scancode &body code)
                 `(when (and (eql key-code ,scancode)
                             (sdl2:key-down-p state))
                    ,@code)))
      (let ((key-code (sdl2:scancode-symbol (sdl2:scancode-value key))))
        (log:trace key state key-code repeat)

        ;; player input handled by ECS for now
        (on-key-down :scancode-up
                     (advance-option -1))

        (on-key-down :scancode-down
                     (advance-option +1))

        (on-key-down :scancode-return
                     (activate-option))

        ;; FIXME XXX (sdl2:push-event :quit) seems like too direct
        ;;           a call; needs a proper engine-provided function
        (on-key-down :scancode-escape
                     (sdl2:push-event :quit))))))

(defmethod on-tick ((screen menu-screen) dt)
  ;; TODO tick special FX, if any
  )

(defmethod on-render ((screen menu-screen) dt)
  ;; TODO render menu
  (p2dg:with-color (1 1 1)
    (draw-text "TSWR: ASTEROIDS"
               :font +bold-font+
               :size 72
               :x 400
               :y 400
               :alignment-x :center
               :alignment-y :center)

    (loop for option in (game-menu-options screen)
       for i = 0 then (1+ i)
       do (let ((selectedp (= (game-menu-current-option screen) i)))
            (draw-text (car option)
                       :font (if selectedp
                              +bold-font+
                              +default-font+)
                       :size (if selectedp 18 16)
                       :x 400
                       :y (- 200 (* i 50))
                       :alignment-x :center)))))

