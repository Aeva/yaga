
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

(define inlined-shader-pair (inlined-program shader-program test))
(define vertex-shader-inlined (car inlined-shader-pair))
(define fragment-shader-inlined (cadr inlined-shader-pair))

(define (solve-and-print name inlined-program)
  (display name)(display " shader inlined:")(newline)
  (display "  ")
  (display inlined-program)(newline)(newline)
  (display name)(display " shader drains:")(newline)
  (let ([drains (solve-simple-program inlined-program)])
    (for-each
     (lambda (drain) (display " - ") (display (drain 'type)) (newline))
     drains))
  (newline))

(solve-and-print "Vertex" vertex-shader-inlined)
(solve-and-print "Fragment" fragment-shader-inlined)
