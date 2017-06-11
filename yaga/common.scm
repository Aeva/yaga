
(define-module (yaga common)
  #:export (fetch keyword->string limit range))

;; Similar to (assoc key alist), but only returns the matching value.
(define (fetch key alist)
  (let ([found (assoc key alist)])
    (if found (cdr found) #f)))

;; Guile doesn't define this.  Weird.
(define (keyword->string keyword)
  (if (keyword? keyword)
      (symbol->string (keyword->symbol keyword))
      ""))

;; Return a subset of an input list that matches the matcher function.
(define (limit matcher list)
  (define matched '())
  (for-each
   (lambda (item) (if (matcher item) (set! matched (cons item matched))))
   list)
  matched)

;; Analagous to python's range builtin.  Eg, (range 4) --> (0 1 2 3)
(define (range count)
  (define (build-range count acc)
    (cond [(zero? count) acc]
          [else (let ([next (- count 1)])
                  (build-range next (cons next acc)))]))
  (cond
   [(and (number? count) (>= count 0))
    (build-range count '())]
   [else (error "Range input must be a number greater than or equal to zero")]))
