












































(define 
(define (compiling-machine

(define (constant x)
  (lambda (vm)
(define (test then else)
  (lambda (vm a s e c) (vm a s e ((cons 

(define (%evaluate sexp)
  (cond
    ((is-a? sexp <pair>))
    ((is-a? sexp <symbol>))
    (else `(,constant ,sexp))))

(%add addr incremence)
 (#x03 (modR/M addr incremence)
