
(define-type <camera>
  (perspective-matrix #:matrix4)
  (view-matrix #:matrix4)
  (camera-matrix (* perspective-matrix view-matrix)))


(define-type <model>
  (vertex-position #:float3 #:position-buffer)
  (vertex-normal #:float3 #:buffer)
  (vertex-uv #:float2 #:buffer)
  (world-matrix #:matrix4)
  (world-position (* (world-matrix) (vertex-position)))
  (screen-position
   (lambda (camera) 
     (* (camera camera-matrix) (world-matrix) (vertex-position))))
  (texture-map #:pixmap))


(define-shader-stage basic-vertex-shader
  (input model <model>)
  (input camera <camera>)
  ((model screen-position) camera))


(define-shader-stage basic-fragment-shader
  (interpolate model <model>)
  (sample (model texture-map) (model vertex-uv)))


(define-shader-program basic-shader
  (#:vertex basic-vertex-shader)
  (#:fragment basic-fragment-shader))

                
