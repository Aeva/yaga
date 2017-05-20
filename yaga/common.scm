
(define-module (yaga common)
  #:export (fetch keyword->string))


;; Similar to (assoc key alist), but only returns the matching value.
(define (fetch key alist)
  (let ([found (assoc key alist)])
    (if found (cdr found) #f)))


;; Guile doesn't define this.  Weird.
(define (keyword->string keyword)
  (if (keyword? keyword)
      (symbol->string (keyword->symbol keyword))
      ""))
