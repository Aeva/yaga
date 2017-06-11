(define-module (yaga type-inference patterns)
  #:use-module (srfi srfi-1)
  #:export (solve permutate multi-permutate))


(define (limit matcher list)
  (define matched '())
  (for-each
   (lambda (item) (if (matcher item) (set! matched (cons item matched))))
   list)
  matched)


(define (range count)
  (define (build-range count acc)
    (cond [(zero? count) acc]
          [else (let ([next (- count 1)])
                  (build-range next (cons next acc)))]))
  (cond
   [(and (number? count) (>= count 0))
    (build-range count '())]
   [else (error "Range input must be a number greater than or equal to zero")]))
  

(define (solved-portion permutations)
  (define slots (range (length (car permutations))))
  (define (same-for-slot? index)
    (define slot-values
      (map (lambda (row) (list-ref row index)) permutations))
    (if (every (lambda (check) (eq? check (car slot-values))) (cdr slot-values))
        (car slot-values)
        '()))
  (map same-for-slot? slots))


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


(define (permutate pattern)
  (define sizes
    (if 
     (any (lambda (slot) (or (eq? slot #:matrix) (eq? slot #:vector))) pattern)
     4
     0))
  (define vectors '(#:float #:float2 #:float3 #:float4))
  (define matrices '(#:float #:matrix2 #:matrix3 #:matrix4))
  (define (permute size)
    (define (subst input)
      (cond [(eq? input #:vector) (list-ref vectors size)]
            [(eq? input #:matrix) (list-ref matrices size)]
            [else input]))
    (map subst pattern))
  (map permute (range sizes)))


(define (multi-permutate rows)
  (delete-duplicates (apply append (map permutate rows))))


;; (define (*-solve out lhs rhs)
;;   (define permutatrix
;;     ;; I later looked up the rules for this, and this isn't actually
;;     ;; the correct permutation matrix for the GLSL * operator, but it
;;     ;; is good enough for example porpoises.
    
;;     ;; output input input
;;     '((#:number #:number #:number)
;;       (#:vector #:vector #:number)
;;       (#:vector #:vector #:vector)
;;       (#:vector #:matrix #:vector)
;;       (#:matrix #:matrix #:matrix)))
;;   (solve permutatrix out lhs rhs))

;; (display (*-solve '() '() '())) (newline)
;; (display (*-solve #:number '() '())) (newline)
;; (display (*-solve '() #:vector '())) (newline)
;; (display (*-solve '() '() #:matrix)) (newline)
;; ;;(display (*-solve #:number '() #:matrix)) (newline) ; will be type error
