
(define-type <camera>
  (perspective-matrix #:matrix4)
  (view-matrix #:matrix4)
  (matrix (* perspective-matrix view-matrix)))


(define-type <model>
  (vertex-position #:float3 #:position-buffer)
  (vertex-normal #:float3 #:buffer)
  (vertex-uv #:float2 #:buffer)
  (world-matrix #:matrix4)
  (world-position (* (world-matrix) (vertex-position)))
  (screen-position
   (lambda (camera) 
     (* (get camera camera-matrix) (world-matrix) (vertex-position))))
  (texture-map #:pixmap))


(define-vertex-shader <basic-vertex-shader>
  (input model <model>)
  (input camera <camera>)
  ((model screen-position) camera))


(define-fragment-shader <basic-fragment-shader>
  (interpolate model <model>)
  (let*
    ([diffuse-color (sample (model texture-map) (model vertex-uv))])
    (diffuse-color)))


(define-program <basic-shader>
  (<basic-vertex-shader> <basic-fragment-shader>))

                
