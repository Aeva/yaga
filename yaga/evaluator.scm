
(define-module (yaga evaluator)
  #:use-module (yaga environment)
  #:use-module (yaga shader-struct)
  #:export (parse inspect))


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
     [(eq? action 'define-shader-stage) (action-shader-stage params env)]
     [(eq? action 'define-shader-program) (action-shader-program params env)]
     [else (error "unknown action")])))


(define (inspect shader-src)
  (define (ingest page env)
    (cond
     [(null? page) env]
     [else (ingest (cdr page) (dispatch-action (car page) env))]))
  (ingest (parse shader-src) (make-environment '() '() '())))
