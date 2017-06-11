(define-module (yaga type-inference built-ins)
  #:use-module (yaga type-inference patterns)
  #:use-module (srfi srfi-1)
  #:export (built-in-solvers built-in-sizes))

(define built-in-solvers '())
(define built-in-sizes '())

(define (register! name permutatrix )
  (define arg-count (length (car permutatrix)))
  (define (solver . args)
    (if (not (eq? (length args) arg-count))
        (error "Invalid invocation of solver:" name args))
    (apply solve (cons permutatrix args)))
  (set! built-in-solvers (cons (cons name solver) built-in-solvers))
  (set! built-in-sizes (cons (cons name arg-count) built-in-sizes)))

(define binary-operator
  (multi-permutate
   '((#:float #:float #:float)
     (#:vector #:vector #:vector)
     (#:matrix #:matrix #:matrix))))

(register! '*
 (multi-permutate
  '((#:float #:float #:float)       ; all binary operators
    (#:vector #:vector #:vector)    ; all binary operators
    (#:matrix #:matrix #:matrix)    ; all binary operators
    (#:vector #:float #:vector)     ; scale a vector
    (#:vector #:vector #:float)     ; scale a vector
    (#:matrix #:float #:matrix)     ; scale a matrix
    (#:matrix #:matrix #:float)     ; scale a matrix
    (#:matrix #:vector #:matrix)    ; multiply vector by transposed matrix
    (#:matrix #:matrix #:vector)))) ; multiply vector by matrix

(register! '/ binary-operator)
(register! '+ binary-operator)
(register! '- binary-operator)

(register! 'radians (permutate '(#:vector #:vector)))
(register! 'degrees (permutate '(#:vector #:vector)))
