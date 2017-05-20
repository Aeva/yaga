
(define-module (yaga shader-struct)
  #:use-module (yaga environment)
  #:use-module (yaga primitives)
  #:use-module (yaga common)
  #:use-module (srfi srfi-1) ;; provides 'find'
  #:export (action-struct)
  #:export (action-shader-stage)
  #:export (action-shader-program)
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


;; Validate a given shader program.
(define (validate-program program)
  
  ;; Compare two bindings by name.
  (define (same-binding? lhs rhs)
    (eq? (car lhs) (car rhs)))

  ;; Compare two bindings by name and type.
  (define (equal-input? lhs rhs)
    (and (same-binding? lhs rhs)
         (equal? (cdr lhs) (cdr rhs))))

  (define (input-collision? lhs rhs)
    (and (same-binding? lhs rhs)
         (not (equal-input? lhs rhs))))
    
  ;; Validate a list of inputs to verify that there are no
  ;; contradicting inputs.  Duplicates are fine.
  (define (validate-inputs inputs)
    (cond
     [(null? inputs) '()]
     [else
      (let* ([input (car inputs)]
             [contradiciton?
              (lambda (other-input) (input-collision? input other-input))]
             [collision (find contradiciton? (cdr inputs))])
        (if collision
            (error "shader program links shaders with contradicting inputs"))
        (validate-inputs (cdr inputs)))]))
 
  ;; Verify a list of inputs and transports such that the transports
  ;; match to corresponding inputs.  Duplicates are fine.
  (define (validate-transports inputs transports)
    (cond
     [(null? transports) '()]
     [else
      (let* ([transport (car transports)]
             [matching-input?
              (lambda (input) (equal-input? transport input))]
             [found (find matching-input? (cdr inputs))])
        (if (not found)
            (error "shader program transport lacks corresponding input"))
        (validate-transports inputs (cdr transports)))]))

  (let ([types (fetch 'types program)]
        [inputs (fetch 'inputs program)]
        [transports (fetch 'transports program)]
        [shaders (fetch 'shaders program)])
    (validate-inputs inputs)
    (validate-transports inputs transports)))


;;
(define (action-shader-program params env)

  ;; Create an object representing the shader program, and perform
  ;; validation on the resulting combination.
  (define (make-program stages)
    
    ;; Takes a description of a shader stage, validates that the
    ;; shader type is correct, and then returns the matching shader
    ;; definition.
    (define (validate-shader-stage stage)
      (let* ([type (car stage)]
             [lookup (cadr stage)]
             [shader (fetch lookup (environment-shaders env))])
        (if (not shader)
            (error "cannot find named shader"))
        (if (not (member type '(#:vertex #:fragment)))
            (error "unknown shader type"))
        (cons type shader)))

    ;; Validates shader stages, collects the combined inputs and
    ;; transports for later validation.  Returns an a-list of the
    ;; accumulated information.
    (define (accumulate-inputs stages types inputs transports)
      (cond
       [(null? stages)
        (list (cons 'types types)
              (cons 'inputs inputs)
              (cons 'transports transports))]
       [(pair? (car stages))
        (let* ([validation (validate-shader-stage (car stages))]
               [shader (cdr validation)]
               [shader-type (car validation)]
               [shader-inputs (fetch 'inputs (cdr shader))]
               [shader-transports (fetch 'transports (cdr shader))]
               [new-types (cons shader-type types)]
               [new-inputs (append shader-inputs inputs)]
               [new-transports (append shader-transports transports)])
          (if (member shader-type types)
              (error "Shader program may only have one shader of each type."))
          (if (not (or (eq? shader-type #:fragment)
                       (null? shader-transports)))
              (error "Only fragment shaders may define 'interpolate' inputs!"))
          (accumulate-inputs (cdr stages) new-types new-inputs new-transports))]
       [else (error "malformed program definition - expected association list")]))

    ;; Build the program object and perform validation.
    (cons (list 'shaders stages) (accumulate-inputs stages '() '() '())))

  (let* ([name (car params)]
         [stages (cdr params)]
         [program (cons name (make-program stages))]
         [types (environment-types env)]
         [shaders (environment-shaders env)]
         [programs (cons program (environment-programs env))])
    (validate-program (cdr program))
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
  
