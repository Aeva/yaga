
;; List contains the keyword "vertex"
(define (is-vertex? variable-data)
  (find (lambda (symbol) (eq? #:vertex symbol)) variable-data))

;; List contains the keyword "transport"
(define (is-transport? variable-data)
  (find (lambda (symbol) (eq? #:transport symbol)) variable-data))

;; List is not a vertex or a transport.
(define (is-uniform? variable-data)
  (not (or (is-vertex? variable-data) (is-transport? variable-data))))

;; Determine if a given symbol is a keyword and also a valid glsl
;; type.
(define (keyword-is-type? global)
  (let ([types '(#:float #:vec2 #:vec3 #:vec4
                 #:int #:ivec2 #:ivec3 #:ivec4
                 #:bool #:bvec2 #:bvec3 #:bvec4
                 #:mat2 #:mat3 #:mat4)])
    (and (keyword? global)
         (find (lambda (check) (equal? global check)) types))))

;; Guile doesn't define this.  Weird.
(define (keyword->string keyword)
  (if (keyword? keyword)
      (symbol->string (keyword->symbol keyword))
      ""))

;; Parses out the global variables from an AST.
(define (find-variables stream)
  (cond
   [(eq? stream '()) '()]
   [(list? stream)
    (let*
        ([record-name (if (list? stream) (car stream) #f)]
         [record (if record-name (find-record record-name) #f)]
         [variable-name (if record (car (cdr stream)) #f)]
         [variable-data
          (if variable-name (find-in-record record variable-name) #f)]
         [variable
          (if variable-name (cons record-name variable-data) #f)])
      (if
       variable
       (cons variable '())
       (apply append (map (lambda (symbol) (find-variables symbol)) stream))))]
   [else '()]))

;; Takes a list of global variable definitions and sorts them by
;; modifier type.
;; Eg: (sort-globals (find-variables (value-or-path 'model 'clip-space)))
(define (sort-globals variable-list)
  (let ([vertices '()]
        [uniforms '()]
        [transports '()])
    (begin
      (map
       (lambda (variable)
         (let ([record-name (car variable)]
               [var-name (car (cdr variable))]
               [var-data (cdr (cdr variable))])
           (cond
            [(is-vertex? var-data) (set! vertices (cons variable vertices))]
            [(is-transport? var-data) (set! transports (cons variable transports))]
            [else (set! uniforms (cons variable uniforms))])))
       variable-list)
      (cons* vertices uniforms transports '()))))

;; Convert a pair of symbols into something that looks like a glsl
;; variable name.  The record and variable names are converted to
;; title case, and then joined with a "_".  The result is ugly, but it
;; shouldn't be prone to collisions.
(define (global-to-glsl-name record-name variable-name)
  (let ([format-name
         (lambda (symbol)
           (let* ([as-string (symbol->string symbol)]
                  [titlecase (string-titlecase as-string)])
             (apply string-append (string-split titlecase #\-))))])
   (string-append (format-name record-name) "_" (format-name variable-name))))

;; Take a modifier (string), type (keyword), record name (symbol) and
;; variable name (symbol); returns a valid glsl variable declaration.
(define (global-to-glsl-declaration modifier type record-name variable-name)
  (let ([prefix (string-append (substring modifier 0 1) "_")])

    (string-append modifier " " (keyword->string type) " " prefix
                   (global-to-glsl-name record-name variable-name) ";\n")))
  
;; Produce the global variable declarations for a given shader AST.  
(define (print-globals-for-stream stream)
  (let*
      ([all-variables (sort-globals (find-variables stream))]
       [vertices (car all-variables)]
       [uniforms (car (cdr all-variables))]
       [transports (car (cdr (cdr all-variables)))]
       [find-type (lambda (tokens) (find keyword-is-type? tokens))]
       [print-var
        (lambda (modifier line)
          (let ([record-name (car line)]
                [variable-name (car (cdr line))]
                [type (find-type line)])
            (global-to-glsl-declaration modifier type record-name variable-name)))])
    (apply
     string-append
     (append
      (map (lambda (line) (print-var "attribute" line)) vertices)
      (map (lambda (line) (print-var "varying" line)) transports)
      (map (lambda (line) (print-var "uniform" line)) uniforms)))))
