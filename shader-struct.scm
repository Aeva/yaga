
(include "primitives.scm")


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


;; This function is for processing the "define-vertex-shader" and
;; "define-fragment-shader" special forms, and inserting the resulting
;; definition into the current environment.
(define (action-shader shader-type params env)
  
  ;; Returns true if the line is a valid input expression.  May raise
  ;; an error the input expression is malformed.
  (define (validate-input line)
    (if (and (not (eq? shader-type 'fragment)) (eq? (car line) 'interpolate))
        (error "'interpolate' special form can only be used with fragment shaders"))
    (let ([is-input (or (eq? (car line) 'input) (eq? (car line) 'interpolate))])
      (if (and is-input (not (eq? (length line) 3)))
          (error "expected (input binding-name type-name)"))
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
               (cons 'type shader-type)
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


;; 
(define (action-program params env)

  ;;
  (define (make-program shaders)
    (define (filter shaders vertex fragment)
      (cond
       [(and (null? shaders) (or (null? vertex) (null? fragment)))
        (error "incomplete shader program")]
       [(null? shaders)
        (list
         (cons 'vertex vertex)
         (cons 'fragment fragment))]
       [else
        (let* ([name (car shaders)]
               [shader (assoc name (environment-shaders env))]
               [type (if shader (cdr (assoc 'type (cdr shader))) '())])
          (cond
           [(not shader) (error "missing definition for named shader")]
           [(eq? type 'vertex)
            (if (null? vertex)
                (filter (cdr shaders) name fragment)
                (error "multiple vertex shaders defined in program"))]
           [(eq? type 'fragment)
            (if (null? fragment)
                (filter (cdr shaders) vertex name)
                (error "multiple fragment shaders defined in program"))]
           [else (error "unknown shader type")]))]))
    (filter shaders '() '()))

  ;;
  (define (validate-inputs shader validated)
    (define (accumulate validated inputs)
      (cond
       [(null? inputs) validated]
       [else
        (let* ([check (car inputs)]
               [name (car check)]
               [value (cdr check)]
               [found (assoc name validated)]
               [found-name (if found (car found) '())]
               [found-value (if found (cdr found) '())]
               [collision (and found
                               (not (and (eq? name found-name)
                                         (equal? value found-value))))])
          (if collision (error "conflicting definiton for shader inputs"))
          (accumulate (cons check validated) (cdr inputs)))]))
    (accumulate validated (cdr (assoc 'inputs (cdr shader)))))

  ;;
  (define (validate-transports shader validated)
    (define (validate validated transports)
      (cond
       [(null? transports) validated]
       [else
        (let* ([check (car transports)]
               [name (car check)]
               [value (cdr check)]
               [found (assoc name validated)]
               [found-name (if found (car found) '())]
               [found-value (if found (cdr found) '())]
               [valid (and found (eq? name found-name) (equal? value found-value))])
          (cond
           [(not found)
            (error "named interpolated type missing in vertex shader inputs")]
           [(not valid)
            (error "named interpolated type does not match vertex shader input")]
           [else (validate validated (cdr transports))]))]))
    (validate validated (cdr (assoc 'transports (cdr shader)))))
  
  ;;
  (define (validate-program program)
    (let* ([vertex-name (cdr (assoc 'vertex program))]
           [fragment-name (cdr (assoc 'fragment program))]
           [shaders (environment-shaders env)]
           [vertex-shader (cdr (assoc vertex-name shaders))]
           [fragment-shader (cdr (assoc fragment-name shaders))])
      (validate-transports
       fragment-shader
       (validate-inputs fragment-shader (validate-inputs vertex-shader '())))))

  
  (let* ([name (car params)]
         [refs (car (cdr params))]
         [program (cons name (make-program refs))]
         [types (environment-types env)]
         [shaders (environment-shaders env)]
         [programs (cons program (environment-programs env))])
    (validate-program (cdr program))
    (make-environment types shaders programs)))


(define (print-struct struct)
  (let* ([name (car struct)]
         [fields (cdr struct)]
         [buffers (cdr (assoc 'buffers fields))]
         [controls (cdr (assoc 'controls fields))]
         [functions (cdr (assoc 'functions fields))])
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
  
