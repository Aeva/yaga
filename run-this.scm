
(include "shader-structs.scm")
(include "global-munging.scm")
(include "taxonomy.scm")


;; Contrived example for defining a vertex shader or something:

(struct* 'camera
  '(perspective-matrix #:mat4)
  '(view-matrix #:mat4)
  '(matrix (* (camera perspective-matrix) (camera view-matrix))))

(struct* 'model
  '(position #:vec3 #:vertex)
  '(world-matrix #:mat4)
  '(clip-space (* (camera matrix) (model world-matrix) (model position))))


(display "\nThe following is the token stream for (model clip-space):\n\n  ")
(display (value-or-path 'model 'clip-space))
(display "\n\n")

(display "The global variable declarations for such a vertex shader might\n")
(display "look like this:\n\n")
(display (print-globals-for-stream (value-or-path 'model 'clip-space)))
;; attribute vec3 a_Model_Position;
;; uniform mat4 u_Model_WorldMatrix;
;; uniform mat4 u_Camera_ViewMatrix;
;; uniform mat4 u_Camera_PerspectiveMatrix;
