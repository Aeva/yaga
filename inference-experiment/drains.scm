(use-modules (srfi srfi-1))
(load "patterns.scm")
(load "built-ins.scm")

(define all-drains '())
(define (register-drain! drain)
  (set! all-drains (cons drain all-drains))
  drain)

(define (solved?)
  (every (lambda (drain) (not (null? (drain 'type)))) all-drains))

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


(define (make-invocation callback output . inputs)
  (map (lambda (pipe) ((pipe 'connect!) callback)) inputs)
  ((output 'connect!) callback)
  (callback)
  output)


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


(define perspective-matrix #:matrix4)
(define view-matrix #:matrix4)
(define world-matrix #:matrix4)
(define vertex-position #:float4)
(define fudge-factor #:float)

(define arbitrary-example
  `(* ,perspective-matrix
      ,view-matrix
      ,world-matrix
      ,vertex-position
      (radians ,fudge-factor)))

;; parse a source tree and built a constraint system for type solving
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
        

;; run the parser
(define outflow (parse arbitrary-example))


(define (check-graph)
  (for-each
   (lambda (drain) (display " - ") (display (drain 'type)) (newline))
   all-drains)

  (if (solved?)
      (display "Graph is solved!\n")
      (display "Graph is unsolved!\n")))

(check-graph)
