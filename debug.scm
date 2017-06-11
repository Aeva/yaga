
(use-modules (yaga evaluator))
(use-modules (yaga environment))
(use-modules (yaga shader-struct))
(use-modules (yaga shader-program))
(use-modules (yaga type-inference drains))

(define test (inspect "shader.scm"))
(newline)
(newline)
(map print-struct (environment-types test))

(define shader-program (cdr (car (environment-programs test))))
(display (gather-program-vars shader-program test))
(newline)(newline)

(define vertex-shader-inlined (car (inlined-program shader-program test)))
(display vertex-shader-inlined)
(newline)

(define vertex-shader-outflow (parse vertex-shader-inlined))
(check-graph)

(display "----\n")
