
(define-module (yaga shader-program)
  #:use-module (yaga shader-validator)
  #:use-module (yaga environment)
  #:use-module (yaga primitives)
  #:use-module (yaga common)
  #:use-module (srfi srfi-1) ;; find
  #:export (action-shader-program gather-program-vars))


;;
(define (action-shader-program params env)

  ;; Create an object representing the shader program, and perform
  ;; validation on the resulting combination.
  (define (make-program stages)
    
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
        (let* ([validation (validate-shader-stage (car stages) env)]
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


;; Produce a list of all unique record lookups used in a given shader
;; program.
(define (gather-program-vars program env)

  ;; For a given named type and named field, return the value of that
  ;; field, as well as a context object for the named type record.
  (define (open-ref type-name field-name)
    (let* ([type-def (fetch type-name (environment-types env))]
           [clear (lambda (pair) (cons (car pair) '()))]
           [buffers (map clear (fetch 'buffers type-def))]
           [controls (map clear (fetch 'controls type-def))]
           [functions (fetch 'functions type-def)]
           [local-vars (append buffers controls functions)])
      (cons (fetch field-name local-vars) (cons type-name local-vars))))

  ;; Determine if the given expression is a valid record lookup,
  ;; according to the current bindings.
  (define (is-lookup? expr bindings)
    (cond [(null? expr) #f]
          [(null? bindings) #f]
          [(pair? expr) (assoc (car expr) bindings)]
          [else #f]))

  ;; Traverse an execution path and return all unique referenced
  ;; compound type records.  This can recurse into records.
  (define (traverse expr bindings context)
    (cond
     [(null? expr) '()]
     [(and (not (pair? expr)) (null? context)) '()]
     [(not (pair? expr))
      ;; We have an environment type as the context, so see if the
      ;; atom is a local reference and return or recurse accordingly.
      (let* ([type-name (car context)]
             [all-vars (cdr context)]
             [found (fetch expr all-vars)])
        (cond
         ;; If the atom is a field in the context, either return just
         ;; the reference id, or the reference id and recurse into the
         ;; next expression if applicable.
         [(not found) '()]
         [(null? found) (list (list type-name expr))]
         [else (traverse found '() context)]))]
     [(pair? expr)
      ;; TODO - this path currently does not account for if the pair
      ;; in question is a function call, all it does is check to see
      ;; if the sexpr is a record lookup.
      (let ([found (is-lookup? expr bindings)])
        (cond
         [found (let* ([type-name (cadr found)]
                       [field-name (cadr expr)]
                       [found (list (list type-name field-name))]
                       [inspect (open-ref type-name field-name)]
                       [ref-expr (car inspect)]
                       [new-context (cdr inspect)])
                  (cond
                   [(null? ref-expr) found]
                   [else (append found (traverse ref-expr '() new-context))]))]
         [else
          (let ([recurse (lambda (nexpr) (traverse nexpr bindings context))])
            (apply append (map recurse expr)))]))]))
  
  (let* ([shader-names (map cadr (car (fetch 'shaders program)))]
         [shaders (map (lambda (name) (lookup-shader name env)) shader-names)]
         [inputs (fetch 'inputs program)]
         [transports (fetch 'transports program)]
         [bindings (append inputs transports)]
         [inspect
          (lambda (shader) (traverse (fetch 'body (cdr shader)) bindings '()))])
    (delete-duplicates (apply append (map inspect shaders)))))
