(in-package #:tswr-asteroids)

(p2de:defcomponent kinematics
  ((velocity (p2dm:make-vector-2d))
   (acceleration (p2dm:make-vector-2d))
   (angular-velocity 0.0)
   (angular-acceleration 0.0)

   (speed-limit nil)                    ;TODO maybe a component "speed limter"?
   (rotation-speed-limit nil)))         ;TODO same as above.
