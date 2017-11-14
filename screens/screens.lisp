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

(defun initialize-game-screens ()
  (setf *registered-game-screens-alist*
        `((:menu . ,(make-instance 'menu-screen))
          ;; TODO get-ready and defeat could be refactored into single
          ;;      splash-screen; it would also allow for intro/credits
          ;;      screen, victory screen, or whatever
          (:get-ready . ,(make-instance 'get-ready-screen))
          (:defeat . ,(make-instance 'defeat-screen))
          (:main-game . ,(make-instance 'main-game-screen))))

  (mapc (lambda (entry)
          (log:debug "Creating game screen" entry)
          (on-create (cdr entry)))
        *registered-game-screens-alist*)

  (setf *current-game-screen* nil
        *upcoming-game-screen* nil))

(defun deinitialize-game-screens ()
  (mapc (lambda (entry)
          (log:debug "Destroying game screen" entry)
          (on-destroy (cdr entry)))
        *registered-game-screens-alist*)
  
  (setf *registered-game-screens-alist* '()
        *current-game-screen* nil
        *upcoming-game-screen* nil))

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

