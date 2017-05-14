
(include "environment.scm")
(include "shader-struct.scm")


(define (parse shader-src)
  (define shader-file (open-input-file shader-src))
  (define (step-reader!)
    (let* ([expr (read shader-file)])
      (cond
       [(eof-object? expr) '()]
       [else (cons expr (step-reader!))])))
  (step-reader!))


(define (dispatch-action expr env)
  (let ([action (car expr)]
        [params (cdr expr)])
    (cond
     [(eq? action 'define-type) (action-struct params env)]
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
(newline)
(newline)
(map print-struct (environment-types test))
