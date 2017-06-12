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

;; "Binary-operator" is a permutation matrix appropriate for operators
;; that can take any type so long as both operands are the same type.
(define binary-operator
  (multi-permutate
   '((#:float #:float #:float)
     (#:vector #:vector #:vector)
     (#:matrix #:matrix #:matrix))))

;; "Vector-operator" is a permutation matrix for operators that only
;; take vector operands of the same type.
(define vector-operator (permutate '(#:vector #:vector #:vector)))

;; "Matrix-operator" is a permutation matrix for operators that only
;; take matrix operands of the same type.
(define matrix-operator (permutate '(#:matrix #:matrix #:matrix)))

;; "Vector-function" is a permutation matrix for functions that both
;; take and return a vector type.
(define vector-function (permutate '(#:vector #:vector)))


;; -------------------------------------------------------
;; What follows are definitions for all builtin functions:
;; -------------------------------------------------------

;; Misc Operators
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

(register! 'component-wise-multiply '((#:matrix2 #:matrix2 #:matrix2)
                                      (#:matrix3 #:matrix3 #:matrix3)
                                      (#:matrix4 #:matrix4 #:matrix4)))

;; Comparison Functions
(register! '< (multi-permutate '((#:bool #:vector #:vector)
                                 (#:bool #:int-vector #:int-vector))))
(register! '<= (multi-permutate '((#:bool #:vector #:vector)
                                  (#:bool #:int-vector #:int-vector))))
(register! '> (multi-permutate '((#:bool #:vector #:vector)
                                 (#:bool #:int-vector #:int-vector))))
(register! '>= (multi-permutate '((#:bool #:vector #:vector)
                                  (#:bool #:int-vector #:int-vector))))
(register! '= (multi-permutate '((#:bool #:vector #:vector)
                                 (#:bool #:int-vector #:int-vector))))
(register! '!= (multi-permutate '((#:bool #:vector #:vector)
                                  (#:bool #:int-vector #:int-vector))))
(register! 'any '((#:bool #:bool2)
                  (#:bool #:bool3)
                  (#:bool #:bool4)))
(register! 'every '((#:bool #:bool2)
                    (#:bool #:bool3)
                    (#:bool #:bool4)))
;(register! 'not (permutate '(#:bool #:bool-vector))) ;; this will throw an error?

;; Trig Functions
(register! 'radians vector-function)
(register! 'degrees vector-function)
(register! 'sin vector-function)
(register! 'cos vector-function)
(register! 'tan vector-function)
(register! 'asin vector-function)
(register! 'acos vector-function)
(register! 'atan vector-function) ;; note, "atan" should also support
                                  ;; (vec vec vec), but disjoint
                                  ;; argument lengths are not yet
                                  ;; supported.

;; Exponent Functions
(register! 'pow vector-operator) ;; op
(register! 'exp vector-function)
(register! 'log vector-function)
(register! 'exp-2 vector-function)
(register! 'log-2 vector-function)
(register! 'square-root vector-function)
(register! 'inverse-square-root vector-function)

;; Common Math Functions
(register! 'abs vector-function)
(register! 'sign vector-function)
(register! 'floor vector-function)
(register! 'ceiling vector-function)
(register! 'fraction vector-function)
(register! 'modulus (multi-permutate '((#:vector #:vector)
                                       (#:vector #:float))))
(register! 'min (multi-permutate '((#:vector #:vector)
                                   (#:vector #:float))))
(register! 'max (multi-permutate '((#:vector #:vector)
                                   (#:vector #:float))))
(register! 'clamp (multi-permutate '((#:vector #:vector)
                                     (#:vector #:float))))
(register! 'lerp (multi-permutate '((#:vector #:vector #:vector)
                                    (#:vector #:vector #:float))))
(register! 'step (multi-permutate '((#:vector #:vector)
                                    (#:float #:vector))))
(register! 'smooth-step (multi-permutate '((#:vector #:vector #:vector)
                                           (#:float #:float #:vector))))

;; Geometry Functions
(register! 'length (permutate '(#:float #:vector)))
(register! 'distance (permutate '(#:float #:vector #:vector)))
(register! 'dot-product (permutate '(#:float #:vector #:vector)))
(register! 'cross-product '((#:float3 #:float3 #:float3)))
(register! 'normalize vector-function)
(register! 'face-forward (permutate '(#:vector #:vector #:vector #:vector)))
(register! 'reflect vector-operator) ;; op
(register! 'refract (permutate '(#:vector #:vector #:vector #:float)))

;; Texture Lookup Functions
(register! 'sample '((#:float4 #:pixmap #:float2)
                     (#:float4 #:cubemap #:float3)
                     (#:float4 #:volume #:float3)))
(register! 'sample-lod '((#:float4 #:pixmap #:float2 #:float)
                         (#:float4 #:cubemap #:float3 #:float)
                         (#:float4 #:volume #:float3 #:float)))
(register! 'projective-sample '((#:float4 #:pixmap #:float3)
                                (#:float4 #:volume #:float4)))
(register! 'projective-sample-lod '((#:float4 #:pixmap #:float3 #:float)
                                    (#:float4 #:volume #:float4 #:float)))

;; Derivative Functions
(register! 'dFdx vector-function)
(register! 'dFdy vector-function)
(register! 'f-width vector-function)

