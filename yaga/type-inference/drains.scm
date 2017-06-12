(define-module (yaga type-inference drains)
  #:use-module (yaga type-inference built-ins)
  #:use-module (srfi srfi-1)
  #:export (solve-simple-program))

;; "Solve-simple-program" takes an expression tree representing a
;; shader program, builds a type inference constraint network from it,
;; and then either raises an error or returns the list of drains
;; created by the type inference network for that program.
(define (solve-simple-program expr)
  
  ;; The "all-drains" list catalogues all of the drains in the type
  ;; inference graph, so that they can be inspected later to check to
  ;; see if the graph is solved.
  (define all-drains '())

  ;; "Register-drain!" prepends a drain on to the "all-drains" list.
  (define (register-drain! drain)
    (set! all-drains (cons drain all-drains))
    drain)

  ;; The "solved?" predicate returns true if none of the drains in
  ;; all-drains are null.
  (define (solved?)
    (every (lambda (drain) (not (null? (drain 'type)))) all-drains))

  ;; The "drain" method creates a drain object, in the form of a
  ;; dispatch method that provides access to enclosed values.
  ;;
  ;; Drains are used for propogating events between constraints, as well
  ;; as for functioning as a "bucket" for containing the type of a given
  ;; input or output of a function.
  ;;
  ;; A drain represent one value, but is the glue between one function's
  ;; output and another function's input.
  (define (drain value)
    (define callbacks '())
    (define (connect! callback) (set! callbacks (cons callback callbacks)))
    (define (propogate-update) (map (lambda (callback) (callback)) callbacks))

    (define type
      (cond
       [(keyword? value) value]
       [(boolean? value) #:bool]
       [(number? value) #:float]
       [(not (null? value)) (error "Invalid drain input:" value)]
       [else '()]))

    (define (set-type! new-type)
      (if (not (eq? type new-type))
          (begin
            (set! type new-type)
            (propogate-update))))

    (define (dispatch route)
      (cond
       [(eq? route 'type) type]
       [(eq? route 'set-type!) set-type!]
       [(eq? route 'connect!) connect!]
       [else (error "Unkown drain method:" route)]))

    (register-drain! dispatch))

  ;; "Make-invocation" takes a callback (ostensibly for constraint
  ;; solving) and some number of drains, and then returns the output
  ;; drain.
  (define (make-invocation callback output . inputs)
    (map (lambda (pipe) ((pipe 'connect!) callback)) inputs)
    ((output 'connect!) callback)
    (callback)
    output)

  ;; "Make-constraint" takes a symbol and some number of input drains,
  ;; and returns an output drain.
  ;;
  ;; The symbol passed in should match one of the built-in symbols, so
  ;; that the associated solver can be used as the constraint callback.
  ;;
  ;; This function essentially takes the type patter matching
  ;; functionality described in patterns.scm, and wraps it as a
  ;; constraint, resulting in a type inference solver.
  ;;
  ;; If the type inference system throws an error, this will also catch
  ;; it and attempt to rewrite it with useful information to tie it back
  ;; to the source that produced it.
  (define (make-constraint symbol . inputs)
    (define output (drain '()))
    (define pipes (cons output inputs))
    (define solver (cdr (assoc symbol built-in-solvers)))
    (define expected-size (cdr (assoc symbol built-in-sizes)))
    (define (callback)
      (define old-types (map (lambda (drain) (drain 'type)) pipes))
      (define new-types
        (catch #t
          (lambda () (apply solver old-types))
          (lambda (key . params)
            ;; Catch and re-throw the type error to add context.
            ;; (source-properties thing) might be useful for line number etc.
            (error "type error in method:" symbol))))
      (for-each
       (lambda (drain old-type new-type)
         (if (and (not (null? new-types)) (not (eq? old-type new-type)))
             ((drain 'set-type!) new-type)))
       pipes old-types new-types))
    (apply make-invocation (cons callback pipes)))

  ;; The "parse" method takes a symbol tree describing a program, and
  ;; builds a constraint tree from the function symbols and inputs.
  ;; This will in turn populate the "all-drains" list for later
  ;; validation.  It also might result in a type error being thrown.
  (define (parse expr)
    (define binary-ops '(* / + -))

    (define (op-handler subexpr)
      ;; break binary operators of more than 2 terms apart
      (define symbol (car expr))
      (if (eq? (length subexpr) 2)
          (make-constraint symbol (parse (car subexpr)) (parse (cadr subexpr)))
          (make-constraint symbol (parse (car subexpr)) (op-handler (cdr subexpr)))))
    
    (define (method-handler subexpr)
      (define symbol (car expr))
      (apply make-constraint (cons symbol (map parse subexpr))))
    
    (cond [(and (pair? expr) (member (car expr) binary-ops)) (op-handler (cdr expr))]
          [(pair? expr) (method-handler (cdr expr))]
          [(or (number? expr) (keyword? expr)) (drain expr)]
          [else (error "parser error" expr)]))

  ;; Build the type inference constraint network, check that the graph
  ;; is solved, and return all of the drains.
  (parse expr)
  (if (not (solved?))
      (error "Shader program is underspecified."))
  all-drains)
