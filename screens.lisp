;;;; Prorotype implementation of game screens.

(in-package #:tswr-asteroids)



(defparameter *current-game-screen* nil "Currently executing game screen.")
(defparameter *upcoming-game-screen* nil "The screen game will switch to next frame.")

(defparameter *registered-game-screens-alist* '() "Alist mapping a keyword to a registered game screen.")


;;; Lifecycle management
(defgeneric on-create (screen)
  (:documentation "Executed once in the lifetime of the `SCREEN',
during initialization of the game."))

(defgeneric on-destroy (screen)
  (:documentation "Executed once in the lifetime of the `SCREEN',
during deinitialization of the game."))

(defgeneric on-phase-in (screen previous-screen)
  (:documentation "`PREVIOUS-SCREEN' will be `NIL' if `SCREEN' is
the starting screen for the whole game."))

(defgeneric on-phase-out (screen next-screen)
  (:documentation "`NEXT-SCREEN' will be `NIL' if `SCREEN' is
the last screen for the whole game."))

;;; Input handlers
(defgeneric on-mouse-move (screen x y xrel yrel state))
(defgeneric on-mouse-button-event (screen x y button state))
(defgeneric on-mouse-wheel-event (screen x y))
(defgeneric on-key-event (screen key state repeat))
(defgeneric on-touch-event (screen touch-id finger-id direction x y dx dy pressure))

;;; Game event handlers
(defgeneric on-tick (screen dt))
(defgeneric on-idle (screen dt))
(defgeneric on-render (screen dt))


;;; Lifecycle management interface
(defun process-game-screen-lifecycle ()
  (unless (eq *current-game-screen* *upcoming-game-screen*)
    (log:debug "Switching game screens." *current-game-screen* *upcoming-game-screen*)
    (on-phase-out *current-game-screen* *upcoming-game-screen*)
    (let ((previous-screen *current-game-screen*))
      (setf *current-game-screen* *upcoming-game-screen*)
      (on-phase-in *current-game-screen* previous-screen))))

(defun create-registered-screens ()
  (setf *registered-game-screens-alist*
        `((:menu . ,(make-instance 'menu-screen))
          (:get-ready . ,(make-instance 'get-ready-screen))
          (:defeat-screen . ,(make-instance 'defeat-screen))
          (:main-game . ,(make-instance 'main-game-screen))))

  (mapc (lambda (entry)
          (log:debug "Creating game screen" entry)
          (on-create (cdr entry)))
        *registered-game-screens-alist*))

(defun destroy-registered-screens ()
  (mapc (lambda (entry)
          (log:debug "Destroying game screen" entry)
          (on-destroy (cdr entry)))
        *registered-game-screens-alist*)
  
  (setf *registered-game-screens-alist* '()))

(defun switch-game-screen (new-screen)
  "Call it to change the active game screen to `NEW-SCREEN'
at the beginning of the next tick."
  (log:info "Will switch to another game screen the next tick." new-screen)
  (setf *upcoming-game-screen* (cdr (assoc new-screen *registered-game-screens-alist*))))


;;; Base class and default implementations

(defclass game-screen ()
  ())

(defmethod on-create (screen)
  (declare (ignore screen)))

(defmethod on-destroy (screen)
  (declare (ignore screen)))

(defmethod on-phase-in (screen previous-screen)
  (declare (ignore screen previous-screen)))

(defmethod on-phase-out (screen next-screen)
  (declare (ignore screen next-screen)))

(defmethod on-mouse-move (screen x y xrel yrel state)
  (declare (ignore screen x y xrel yrel state)))

(defmethod on-mouse-button-event (screen x y button state)
  (declare (ignore screen x y button state)))

(defmethod on-mouse-wheel-event (screen x y)
  (declare (ignore screen x y)))

(defmethod on-key-event (screen key state repeat)
  (declare (ignore screen key state repeat)))

(defmethod on-touch-event (screen touch-id finger-id direction x y dx dy pressure)
  (declare (ignore touch-id finger-id direction x y dx dy pressure)))

(defmethod on-tick (screen dt)
  (declare (ignore screen dt)))

(defmethod on-idle (screen dt)
  (declare (ignore screen dt)))

(defmethod on-render (screen dt)
  (declare (ignore screen dt)))


;;; Menu screen
(defclass menu-screen (game-screen)
  ((options :reader game-menu-options)
   (current-option :reader game-menu-current-option)))

(defmethod on-phase-in ((screen menu-screen) previous-screen)
  (declare (ignore previous-screen))
  ;; TODO initialize
  )

(defmethod on-key-event ((screen menu-screen) key state repeat)
  ;; TODO
  )

(defmethod on-tick ((screen menu-screen) dt)
  ;; TODO tick special FX, if any
  )

(defmethod on-render ((screen menu-screen) dt)
  ;; TODO render menu
  )


;;; Get ready screen
(defclass get-ready-screen (game-screen)
  ()
  )


;;; Defeat screen
(defclass defeat-screen (game-screen)
  ()
  )


;;; Main game screen
(defclass main-game-screen (game-screen)
  ((primary-ui-font :initform nil
                    :accessor main-game-primary-ui-font)
   (secondary-ui-font :initform nil
                      :accessor main-game-secondary-ui-font)))

(defmethod on-create ((screen main-game-screen))
  (setf (main-game-primary-ui-font screen)
        (or (main-game-primary-ui-font screen)
            (p2dg:get-rendered-font "fonts/Vera/VeraMoBd.ttf" :size 16))

        (main-game-secondary-ui-font screen)
        (or (main-game-secondary-ui-font screen)
            (p2dg:get-rendered-font "fonts/Vera/VeraMono.ttf" :size 12))))

(defmethod on-destroy ((screen main-game-screen))
  (p2dg:free-font (main-game-primary-ui-font screen))
  (p2dg:free-font (main-game-secondary-ui-font screen)))

(defmethod on-phase-in :before ((screen main-game-screen) previous-screen)
  (declare (ignore previous-screen))
  ;; TODO generic game initialization
  )

(defmethod on-phase-in ((screen main-game-screen) (previous-screen menu-screen))
  ;; TODO additional initialization - clear score, et al.
  )

(defmethod on-phase-out ((screen main-game-screen) next-screen)
  (declare (ignore screen next-screen))
  ;; Not sure if there's anything to do in here.
)

(defmethod on-key-event ((screen main-game-screen) key state repeat)
  ;; TODO maybe make that macrolet part of the engne? It's too useful.
  (macrolet ((on-key-down (scancode &body code)
               `(when (and (eql key-code ,scancode)
                           (sdl2:key-down-p state))
                  ,@code)))
    (let ((key-code (sdl2:scancode-symbol (sdl2:scancode-value key))))
      (log:trace key state key-code repeat)

      ;; player input handled by ECS for now
      (on-key-down :scancode-f1
                   (renderer-toggle-debug-kinematics))

      (on-key-down :scancode-f2
                   (renderer-toggle-debug-collision))

      (on-key-down :scancode-f5
                   (debug-spawn-asteroid))

      (on-key-down :scancode-f6
                   (debug-spawn-powerup))

      (on-key-down :scancode-escape
                   (sdl2:push-event :quit)))))

(defmethod on-tick ((screen main-game-screen) dt)
  (tick-generic-game-rules dt)
  (p2de:tick-simulation-systems dt))

(defmethod on-render ((screen main-game-screen) dt)
  ;; draw stuff

  (p2de:tick-frame-systems dt) ;FIXME why it's here and not in on-idle??

  ;; Draw UI
  ;; FIXME move somewhere else
  ;; Score
  (p2dg:with-color (0 1 0)
    (p2dg::draw-text (format nil "~10D" (floor *score*))
                     :font (main-game-primary-ui-font screen)
                     :x 680
                     :y 580))
  (p2dg:with-color (1 1 1)
    (p2dg::draw-text (format nil "High score: ~10D" (floor *high-score*))
                     :font (main-game-secondary-ui-font screen)
                     :x 600
                     :y 560))

  ;; Lives
  (p2dg:with-color (1 1 0)
    (dotimes (i *lives*)
      (gl:with-pushed-matrix
        (gl:translate (+ 10 (* i 20))
                      580
                      0)
        (gl:scale 6 9 6)
        (p2dglu:draw-triangle-outline)))))
