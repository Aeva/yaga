
(use-modules (yaga evaluator))
(use-modules (yaga environment))
(use-modules (yaga shader-struct))

(define test (inspect "shader.scm"))
(newline)
(newline)
(map print-struct (environment-types test))
