
(define-module (yaga shader-validator)
  #:use-module (yaga environment)
  #:use-module (yaga primitives)
  #:use-module (yaga common)
  #:use-module (srfi srfi-1) ;; provides 'find'
  #:export (validate-shader-stage validate-program))


;; Takes a description of a shader stage, validates that the
;; shader type is correct, and then returns the matching shader
;; definition.
(define (validate-shader-stage stage env)
  (let* ([type (car stage)]
         [lookup (cadr stage)]
         [shader (fetch lookup (environment-shaders env))])
    (if (not shader)
        (error "cannot find named shader"))
    (if (not (member type '(#:vertex #:fragment)))
        (error "unknown shader type"))
    (cons type shader)))


;; Validate a given shader program.
(define (validate-program program)
  
  ;; Compare two bindings by name.
  (define (same-binding? lhs rhs)
    (eq? (car lhs) (car rhs)))

  ;; Compare two bindings by name and type.
  (define (equal-input? lhs rhs)
    (and (same-binding? lhs rhs)
         (equal? (cdr lhs) (cdr rhs))))

  (define (input-collision? lhs rhs)
    (and (same-binding? lhs rhs)
         (not (equal-input? lhs rhs))))
    
  ;; Validate a list of inputs to verify that there are no
  ;; contradicting inputs.  Duplicates are fine.
  (define (validate-inputs inputs)
    (cond
     [(null? inputs) '()]
     [else
      (let* ([input (car inputs)]
             [contradiciton?
              (lambda (other-input) (input-collision? input other-input))]
             [collision (find contradiciton? (cdr inputs))])
        (if collision
            (error "shader program links shaders with contradicting inputs"))
        (validate-inputs (cdr inputs)))]))
 
  ;; Verify a list of inputs and transports such that the transports
  ;; match to corresponding inputs.  Duplicates are fine.
  (define (validate-transports inputs transports)
    (cond
     [(null? transports) '()]
     [else
      (let* ([transport (car transports)]
             [matching-input?
              (lambda (input) (equal-input? transport input))]
             [found (find matching-input? (cdr inputs))])
        (if (not found)
            (error "shader program transport lacks corresponding input"))
        (validate-transports inputs (cdr transports)))]))

  (let ([types (fetch 'types program)]
        [inputs (fetch 'inputs program)]
        [transports (fetch 'transports program)]
        [shaders (fetch 'shaders program)])
    (validate-inputs inputs)
    (validate-transports inputs transports)))
