
;; Return the first valid type keyword in a list or false.
(define (get-type variable-data)
  (find keyword-is-type? variable-data))

;; Get the type associated to a given path s-expression.  Returns an
;; empty list on error.
(define (path-type-or-value thing)
  (let ([found (lookup-by-path thing)])
    (if found
        (or (get-type found)
            (car (cdr found)))
        '())))

;; Determine if the given item is a boolean value, a numerical value,
;; or a scalar type keyword.
(define (scalar? thing)
  (or
   (boolean? thing)
   (number? thing)
   (and (keyword-is-type? thing)
        (string-match "^(bool|int|float)$" (keyword->string thing)))
   (and (not (eq? thing '()))
        (scalar? (path-type-or-value thing)))))

;; Determine if the given item is a vector type keyword.  For the sake
;; of strictness, this does not include matrices as vectors.
(define (vector? thing)
  (or
   (and (keyword-is-type? thing)
        (string-match "^(i|b)?vec[2-4]$" (keyword->string thing)))
   (and (not (eq? thing '()))
        (vector? (path-type-or-value thing)))))

;; Determine if the given item is a matrix type keyword.
(define (matrix? thing)
  (or
   (and (keyword-is-type? thing)
        (string-match "^mat[2-4]$" (keyword->string thing)))
   (and (not (eq? thing '()))
        (matrix? (path-type-or-value thing)))))

;; Casts the last character in a keyword to a number.
(define (parse-size type-keyword)
  (string->number (substring (string-reverse (keyword->string type-keyword)) 0 1)))

;; For a given sexpression, compute the vector size.  For example,
;; scalars return 1, vectors return 2 through 4, matrices return 4
;; through 16, and lists of any combination of the above return the
;; total size.
(define (vector-size thing)
  (cond
   [(eq? thing '()) 0] ;; maybe throw an error instead here?
   [(list? thing)
    (let* ([type-or-value (path-type-or-value thing)]
           [is-record (not (eq? type-or-value '()))])
      (cond
       [is-record (vector-size type-or-value)]
       [else (apply + (map vector-size thing))]))]
   ;; run lists first so that thing is probably a value or keyword at this point
   [(scalar? thing) 1]
   [(vector? thing) (parse-size thing)]
   [(matrix? thing) (expt (parse-size thing) 2)] ;; will trip over non-keywords
   [else 0])) ;; maybe throw an error instead if it comes to this
