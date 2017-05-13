(use-modules (srfi srfi-1)) ;; find
(use-modules (srfi srfi-9)) ;; records

(define-record-type <environment>
  (make-environment types shaders programs)
  environment?
  (types get-env-types)
  (shaders get-env-shaders)
  (programs get-env-programs))

(define (find-in-alist query alist)
  (find (lambda (line) (eq? query (car line))) alist))

(define (lookup-type type-name env)
  (find-in-alist type-name (get-env-types env)))

(define (lookup-shader type-name env)
  (find-in-alist type-name (get-env-shaders env)))

(define (lookup-programs type-name env)
  (find-in-alist type-name (get-env-program env)))


(define (parse shader-src)
  (define shader-file (open-input-file shader-src))
  (define (step-reader!)
    (let* ([expr (read shader-file)])
      (cond
       [(eof-object? expr) '()]
       [else (cons expr (step-reader!))])))
  (step-reader!))


(define (action-type params env)
  (let* ([name (car params)]
         [fields (cdr params)]
         [new-type (cons name fields)]
         [new-types (cons new-type (get-env-types env))]
         [shaders (get-env-shaders env)]
         [programs (get-env-programs env)])
    
    (display " - defining type: ")
    (display name)
    (display (newline))
    (make-environment new-types shaders programs)))


(define (action-shader shader-type params env)
  (let ([name (car params)]
        [body (cdr params)])
    (display " - defining shader: ")
    (display name)
    (display (newline)))
    env)


(define (action-program params env)
  (let ([name (car params)]
        [shaders (cdr params)])

    (display " - defining program: ")
    (display name)
    (display (newline))
    (display "   - using: ")
    (display (car (car shaders)))
    (display (newline))
    (display "   - using: ")
    (display (cadr (car shaders)))
    (display (newline)))
  env)


(define (dispatch-action expr env)
  (let ([action (car expr)]
        [params (cdr expr)])
    (cond
     [(eq? action 'define-type) (action-type params env)]
     [(eq? action 'define-vertex-shader) (action-shader 'vertex params env)]
     [(eq? action 'define-fragment-shader) (action-shader 'fragment params env)]
     [(eq? action 'define-program) (action-program params env)]
     [else (error "unknown action")])))


(define (inspect shader-src)
  (define (ingest page env)
    (cond
     [(null? page) env]
     [else (ingest (cdr page) (dispatch-action (car page) env))]))
  (ingest (parse shader-src) (make-environment '() '() '())))



(define test (inspect "shader.scm"))
