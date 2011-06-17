(define (calc) ((lambda (a b c) (a a b c)) (lambda (cont arg1 arg2) (if (= arg1 0) arg2 (cont cont (+ arg1 -1) (+ arg1 arg2)))) 200000 1))
(define (loop n) (calc) (if (= n 0) (calc) (loop (- n 1))))
(print (loop 8))
