
(use-modules (yaga evaluator))
(use-modules (yaga environment))
(use-modules (yaga shader-struct))
(use-modules (yaga shader-program))

(define test (inspect "shader.scm"))
(newline)
(newline)
(map print-struct (environment-types test))

(define shader-program (cdr (car (environment-programs test))))
(display (gather-program-vars shader-program test))
(newline)(newline)
(display (car (inlined-program shader-program test)))
(newline)
