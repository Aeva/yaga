(use-modules (srfi srfi-1))

;; "record-types" will end up looking something like this:
;;
;; ((camera (matrix #:mat4))
;;  (model (position #:vec3)
;;         (matrix #:mat4)
;;         (clip-space (* (camera matrix) (model matrix) (model position)))))
(define record-types '())

;; Find a record by name.  Eg, (find-record 'camera) would return
;; (camera (matrix #:mat4)) for the above example.
(define (find-record record-name)
  (find (lambda (record) (eq? (car record) record-name)) record-types))

;; Create a new record if it doesn't exist.
(define (create-record! record-name)  
  (let ([found (find-record record-name)])
    (if found #f
        (set! record-types (cons `(,record-name) record-types)))))

;; Find a named variable's definition within a record.
(define (find-in-record record variable-name)
  (find (lambda (variable) (eq? (car variable) variable-name)) (cdr record)))

;; Find a named variable's definiton within a named record.
(define (find-variable record-name variable-name)
  (let ([record (find-record record-name)])
    (if record (find-in-record record variable-name) #f)))

;; Returns the value of the find-variable call for a given record, if
;; the input s-expression describes a record.
(define (lookup-by-path thing)
  (if (and (list? thing) (not (eq? '() thing)))
      (let* ([record-name (car thing)]
             [record (find-record record-name)]
             [variable-name (if record (car (cdr thing)) #f)])
        (if (and record variable-name) (find-in-record record variable-name) #f))
      #f))

;; Update or add a named variable definition to a named record.
(define (update-record! record-name variable-name data)
  (let ([record (find-record record-name)]
        [variable (find-variable record-name variable-name)])
    (if (and record (not variable))
        (let* ([var-line (cons variable-name data)]
               [all-vars (cons var-line (cdr record))]
               [new-record (cons record-name all-vars)])
          (set! record-types
                (map (lambda (record)
                       (cond
                        [(equal? (car record) record-name) new-record]
                        [else record])) record-types)))
        #f)))

;; Returns the value associated to a specific variable in a named
;; record, if applicable, otherwise returns the arguments to the call.
(define (value-or-path record-name variable-name)
  (let ([found (find-variable record-name variable-name)])
    (if found
        (let* ([value (cdr found)]
               [first (car value)])
          (if (keyword? first) (cons* record-name variable-name '()) first))
        #f))) ;; maybe throw an error if not found

;; Take a nested AST and lazy evaluate anything that looks like a
;; record lookup.
(define (solve thing)
  (cond
   [(eq? thing '()) '()]
   [(list? thing)
    (let* ([record-name (car thing)]
           [record (find-record record-name)]
           [variable-name (if record (car (cdr thing)) #f)]
           [value (if variable-name (value-or-path record-name variable-name) #f)])
      (if record
          value
          (cons (solve (car thing)) (solve (cdr thing)))))]
   [else thing]))

;; To do something with the above methods:

;; (define identity-matrix
;;   '(1 0 0 0
;;     0 1 0 0
;;     0 0 1 0
;;     0 0 0 1))

;; (create-record! 'camera)
;; (update-record! 'camera 'identity `(,identity-matrix))
;; ;;(update-record! 'camera 'matrix '(#:mat4))
;; (update-record! 'camera 'matrix `(,(value-or-path 'camera 'identity)))

;; (create-record! 'model)
;; (update-record! 'model 'position '(#:vec3 #:vertex))
;; (update-record! 'model 'matrix '(#:mat4))
;; (update-record! 'model 'clip-space `((* ,(value-or-path 'camera 'matrix)
;;                                         ,(value-or-path 'model 'matrix)
;;                                         ,(value-or-path 'model 'position))))


;; A macro which simplifies the usage of the above methods.
(define-syntax-rule (struct* record-name body ...)
  (begin
    (create-record! record-name)
    (map
     (lambda (line)
       (let* ([variable-name (car line)]
              [variable-data (solve (cdr line))])
         (apply update-record! `(,record-name ,variable-name ,variable-data))))
     `(,body ...))))
