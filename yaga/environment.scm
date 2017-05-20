
(define-module (yaga environment)
  #:use-module (srfi srfi-9) ;; records
  #:export (lookup-type
            lookup-shader
            lookup-programs
            <environment>
            make-environment
            environment?
            environment-types
            environment-shaders
            environment-programs))


(define-record-type <environment>
  (make-environment types shaders programs)
  environment?
  (types environment-types)
  (shaders environment-shaders)
  (programs environment-programs))


(define (lookup-type type-name env)
  (assoc type-name (environment-types env)))

(define (lookup-shader shader-name env)
  (assoc shader-name (environment-shaders env)))

(define (lookup-programs program-name env)
  (assoc program-name (environment-program env)))
