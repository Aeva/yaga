
(define-module (yaga primitives)
  #:use-module (yaga common)
  #:use-module (srfi srfi-1) ;; provides 'find', 'every'
  #:export (buffer-keyword?
            primitive?
            vector-size))


;; attribute array identifier
(define (buffer-keyword? keyword)
  (or (eq? keyword #:buffer)
      (eq? keyword #:position-buffer)))


;; primitive type names
(define scalar-types '(#:float #:int #:bool))
(define vector-types '(#:float2 #:float3 #:float4
                       #:int2 #:int3 #:int4
                       #:bool2 #:bool3 #:bool4))
(define matrix-types '(#:matrix2 #:matrix3 #:matrix4))
(define sampler-types '(#:pixmap #:cubemap #:volume))

;;
(define (scalar? keyword) (member keyword scalar-types))
(define (vector? keyword) (member keyword vector-types))
(define (matrix? keyword) (member keyword matrix-types))
(define (sampler? keyword) (member keyword sampler-types))

;; "numeric" types are all scalar, vector, or matrix types
(define (numeric? keyword)
  (or (scalar? keyword) (vector? keyword) (matrix? keyword)))

(define (primitive? keyword)
  (or (sampler? keyword) (numeric? keyword)))


;;
(define (vector-size variable)
  (define (parse-size type-string)
    (string->number (substring (string-reverse type-string) 0 1)))
  (cond
   [(and (numeric? variable) (not (sampler? variable)))
    (let ([kstring (keyword->string variable)]
          [scalar "^(float|int|bool)$"]
          [vector "^(float|int|bool)[2-4]$"]
          [matrix "^matrix[2-4]$"])
      (cond [(string-match scalar kstring) 1]
            [(string-match vector kstring) (parse-size kstring)]
            [(string-match matrix kstring) (expt (parse-size kstring) 2)]))]
   [else (error "cannot parse vector size of non-primitive")]))


;;
(define (atom? thing) (and (not (null? thing)) (not (pair? thing))))


;;
(define (infer-type thing)
  
  (define (identify-atom atom)
    (cond [(boolean? atom) #:bool]
          [(number? atom) '(#:int #:float)]
          [else atom]))
  
  (define (identify-lat lat)
    (cond [(eveny numeric? (map identify-atom? lat)) (vector-size lat)]
          ;; TODO function invocations?
          [else (map infer-type lat)]))
  
  (cond [(null? thing) '()]
        [(atom? thing) (identify-atom thing)]
        [else (identify-lat thing)]))
