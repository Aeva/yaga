
(define-module (yaga shader-stage)
  #:use-module (yaga environment)
  #:use-module (yaga primitives)
  #:use-module (yaga common)
  #:export (action-shader-stage))


;; This function is for processing the "define-vertex-shader" and
;; "define-fragment-shader" special forms, and inserting the resulting
;; definition into the current environment.
(define (action-shader-stage params env)
  
  ;; Returns true if the line is a valid input expression.  May raise
  ;; an error the input expression is malformed.
  (define (validate-input line)
    (let ([is-input (or (eq? (car line) 'input) (eq? (car line) 'interpolate))])
      (if (and is-input (not (eq? (length line) 3)))
          (error "expected (input|interpolate binding-name type-name)"))
      is-input))

  ;; Build an associative list representing the shader.
  (define (collect-inputs body )
    (define (collect body inputs transports)
      (if (null? body)
          (error "shader has no return value")
          (let* ([line (car body)]
                 [is-input (validate-input line)]
                 [input-mode (if is-input (car line) '())]
                 [binding (if is-input (cdr line) '())])
            (cond
             [is-input
              (if (eq? input-mode 'interpolate)
                  (collect (cdr body) inputs (cons binding transports))
                  (collect (cdr body) (cons binding inputs) transports))]
             [else
              (list
               (cons 'body body)
               (cons 'inputs inputs)
               (cons 'transports transports))]))))
    (collect body '() '()))
  
  (let* ([name (car params)]
         [body (cdr params)]
         [inputs (collect-inputs body)]
         [shader (cons name inputs)]
         [types (environment-types env)]
         [shaders (cons shader (environment-shaders env))]
         [programs (environment-programs env)])
    (make-environment types shaders programs)))

