(define-module (yaga type-inference patterns)
  #:use-module (yaga common)
  #:use-module (srfi srfi-1)
  #:export (solve permutate multi-permutate))

;; "Solved-portion" is a helper function to "solve".  The
;; "permutations" input is a list of types that have been determined
;; to be valid subsets of the full permutation matrix for the
;; currently known inputs.  This function then scans that possibility
;; space, and notes which columns all contain the same type.  This
;; function then returns a list where each slot is either null or a
;; locked type.
(define (solved-portion permutations)
  (define slots (range (length (car permutations))))
  (define (same-for-slot? index)
    (define slot-values
      (map (lambda (row) (list-ref row index)) permutations))
    (if (every (lambda (check) (eq? check (car slot-values))) (cdr slot-values))
        (car slot-values)
        '()))
  (map same-for-slot? slots))

;; "Solve" takes a matrix of permutations (or, permutatrix) and a list
;; of types associated to the drains of a function (or null, if the
;; drain's type is unknown).  The first drain represent the function's
;; output, and the remaining drains are the ordered inputs.  So a call
;; might be:
;;
;;   (solve '((#:float #:float)
;;            (#:float #:float2)
;;            (#:float #:float3)
;;            (#:float #:float4)) '() #:float4)
;;
;; ...and the output is a new list of types.  So in the example above,
;; the output would be:
;;
;;   '(#:float #:float4)
;;
;; If a contradiction is discovered, then a type error will be thrown
;; from this function.
(define (solve permutatrix . drain-types)
  (define (validate-permutation? permute)
    (define (type-matches-pattern? drain-type pattern-type)
        (or (null? drain-type) (eq? drain-type pattern-type)))
    (every type-matches-pattern? drain-types permute))

  (define variations
    (if
     (every null? drain-types)
     '()
     (let ([reduced-permutatrix (limit validate-permutation? permutatrix)])
       (if (null? reduced-permutatrix)
           (error "type error")
           reduced-permutatrix))))

  (cond [(null? variations) '()]
        [else (solved-portion variations)]))

;; "Permutate" generates a permutation matrix for the "solve" method
;; to consume.  It recognizes two virtual types in addition to all
;; primitive types: #:vector and #:matrix.  These virtual types are
;; respectively standins for:
;;
;;   (#:float #:float2 #:float3 #:float4)
;;
;; and,
;;
;;   (#:float #:matrix2 #:matrix3 #:matrix4)
;;
;; These virtual types are roughly the same idea as OpenGL's various
;; "GenType" keywords used for simplifying the valid type signatures
;; of builtin methods in the spec.  In the future, the meanings of
;; #:vector and #:matrix might be expanded to circumstantially include
;; non-float types when the output language supports it.  In this
;; event, additional types like #:float-vector will be introduced.
;;
;; But, the future aside, a call to "permutate" looks something like
;; this:
;;
;;   (permutate '(#:float #:vector))
;;
;; and returns something like this:
;;
;;   '((#:float #:float)
;;     (#:float #:float2)
;;     (#:float #:float3)
;;     (#:float #:float4))
;;
(define (permutate pattern)
  (define sizes
    (if 
     (any (lambda (slot) (or (eq? slot #:matrix) (eq? slot #:vector))) pattern)
     4
     0))
  (define vectors '(#:float #:float2 #:float3 #:float4))
  (define int-vectors '(#:int #:int2 #:int3 #:int4))
  (define bool-vectors '(#:bool #:bool2 #:bool3 #:bool4))
  (define matrices '(#:float #:matrix2 #:matrix3 #:matrix4))
  (define (permute size)
    (define (subst input)
      (cond [(eq? input #:vector) (list-ref vectors size)]
            [(eq? input #:matrix) (list-ref matrices size)]
            [(eq? input #:int-vector) (list-ref int-vectors size)]
            [(eq? input #:bool-vector) (list-ref bool-vectors size)]
            [else input]))
    (map subst pattern))
  (map permute (range sizes)))

;; "Multi-permutate" is a convienience method on top of "permutate",
;; where you provide a list of permutations you want to contactenate
;; togther, and it returns a single permutation matrix with no
;; duplicates.  For example, binary types that only accept operands of
;; the same type would look like this:
;;
;;   (multi-permutate '((#:float #:float)
;;                      (#:vector #:vector)
;;                      (#:matrix #:matrix)))
;;
(define (multi-permutate rows)
  (delete-duplicates (apply append (map permutate rows))))
