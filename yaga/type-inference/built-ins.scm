(define-module (yaga type-inference built-ins)
  #:use-module (yaga type-inference patterns)
  #:use-module (srfi srfi-1)
  #:export (built-in-solvers built-in-sizes))

;; The "built-in-solvers" is an a-list where the keys are the symbols
;; for all known built-in functions, and the associated values are
;; the appropriate constraint solver function for that symbol.
(define built-in-solvers '())

;; The "built-in-sizes" list has a 1:1 relationship with
;; "built-in-solvers" and represents the number of inputs each
;; built-in function takes.
(define built-in-sizes '())

;; Register takes a name and a permutation matrix, generates a solver
;; function, and then populates "built-in-solvers" and "built-in-sizes".
(define (register! name permutatrix)
  (define arg-count (length (car permutatrix)))
  (define (solver . args)
    (if (not (eq? (length args) arg-count))
        (error "Invalid invocation of solver:" name args))
    (apply solve (cons permutatrix args)))
  (set! built-in-solvers (cons (cons name solver) built-in-solvers))
  (set! built-in-sizes (cons (cons name arg-count) built-in-sizes)))

;; "Binary-operator" generates a permutation matrix appropriate for
;; operators that can take any type so long as both operands are the
;; same type.
(define binary-operator
  (multi-permutate
   '((#:float #:float #:float)
     (#:vector #:vector #:vector)
     (#:matrix #:matrix #:matrix))))




;; What follows are definitions for all builtin functions (so far):

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
