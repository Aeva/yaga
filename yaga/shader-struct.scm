
(define-module (yaga shader-struct)
  #:use-module (yaga environment)
  #:use-module (yaga primitives)
  #:use-module (yaga common)
  #:export (action-struct)
  #:export (print-struct))


;; This function is for processing the "define-type" special form, and
;; inserting the resulting struct definition into the current
;; environment.
(define (action-struct params env)
  
  ;; Determines if a field definition describes a vertex attribute.
  (define (is-buffer? spec)
    (and (eq? (length spec) 2)
         (or (and (buffer-keyword? (car spec)) (primitive? (cadr spec)))
             (and (primitive? (car spec)) (buffer-keyword? (cadr spec))))))

  ;; Determines if a field definition describes a uniform variable.
  (define (is-control? spec)
    (and (eq? (length spec) 1)
         (primitive? (car spec))))

  ;; Determines if a field definition describes a constant, alias, or
  ;; function.
  (define (is-function? spec)
    (not (or (is-buffer? spec) (is-control? spec))))

  ;; Consumes a list of field definitions and sorts them into an
  ;; association list for the different field types.
  (define (build-struct fields)
    (define (split fields buffers controls functions)
      (cond
       [(null? fields) (list buffers controls functions)]
       [else 
        (let* ([field (car fields)]
               [remainder (cdr fields)]
               [name (car field)]
               [spec (cdr field)])
          (cond
           [(is-buffer? spec)
            (split remainder (cons field buffers) controls functions)]
           [(is-control? spec)
            (split remainder buffers (cons field controls) functions)]
           [(is-function? spec)
            (split remainder buffers controls (cons field functions))]))]))
    (let ([result (split fields '() '() '())])
      (list
       (cons 'buffers (car result))
       (cons 'controls (cadr result))
       (cons 'functions (caddr result)))))

  (let* ([name (car params)]
         [fields (cdr params)]
         [struct (build-struct fields)]
         [new-type (cons name struct)]
         [types (cons new-type (environment-types env))]
         [shaders (environment-shaders env)]
         [programs (environment-programs env)])
    (make-environment types shaders programs)))


;;
(define (print-struct struct)
  (let* ([name (car struct)]
         [fields (cdr struct)]
         [buffers (fetch 'buffers fields)]
         [controls (fetch 'controls fields)]
         [functions (fetch 'functions fields)])
    (newline)
    (display "Definition of ")
    (display name)
    (newline)
    
    (if (not (null? buffers))
        (begin
          (display "Buffers:\n")
          (map (lambda (field)
                 (display " - ")
                 (display (car field))
                 (display " : ")
                 (display (cdr field))
                 (newline))
               (reverse buffers))))
        
    (if (not (null? controls))
        (begin
          (display "Controls:\n")
          (map (lambda (field)
                 (display " - ")
                 (display (car field))
                 (display " : ")
                 (display (cdr field))
                 (newline))
               (reverse controls))))
    
    (if (not (null? functions))
        (begin
          (display "Functions:\n")
          (map (lambda (field)
                 (display " - ")
                 (display (car field))
                 (newline))
               (reverse functions))))
    (newline)))
