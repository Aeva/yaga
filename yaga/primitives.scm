
(use-modules (srfi srfi-1)) ;; provides 'find'


(define (buffer-keyword? keyword)
  (or (eq? keyword #:buffer)
      (eq? keyword #:position-buffer)))


(define (primitive? keyword)
  (let ([types (list
                #:float #:float2 #:float3 #:float4
                #:int #:int2 #:int3 #:int4
                #:bool #:bool2 #:bool3 #:bool4
                #:matrix2 #:matrix3 #:matrix4)])
    (and (keyword? keyword)
         (find (lambda (check) (equal? keyword check)) types))))


;; Guile doesn't define this.  Weird.
(define (keyword->string keyword)
  (if (keyword? keyword)
      (symbol->string (keyword->symbol keyword))
      ""))


(define (vector-size variable)
  (define (parse-size type-string)
    (string->number (substring (string-reverse type-string) 0 1)))
  (cond
   [(primitive? variable)
    (let ([kstring (keyword->string variable)]
          [scalar "^(float|int|bool)$"]
          [vector "^(float|int|bool)[2-4]$"]
          [matrix "^matrix[2-4]$"])
      (cond [(string-match scalar kstring) 1]
            [(string-match vector kstring) (parse-size kstring)]
            [(string-match matrix kstring) (expt (parse-size kstring) 2)]))]
   [else (error "cannot parse vector size of non-primitive")]))
