
(use-modules (srfi srfi-9)) ;; records


(define-record-type <environment>
  (make-environment types shaders programs)
  environment?
  (types environment-types)
  (shaders environment-shaders)
  (programs environment-programs))


(define (lookup-type type-name env)
  (assoc type-name (environment-types env)))

(define (lookup-shader type-name env)
  (assoc type-name (environment-shaders env)))

(define (lookup-programs type-name env)
  (assoc type-name (environment-program env)))
