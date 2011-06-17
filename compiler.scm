(define (size x)
  (cond
    ((null? x) 0)
    ((not (pair? x)) #f)
    ((number? (car x)) (+ 1 (size (cdr x))))
    ((not (pair? (car x))) #f)
    ((eq? (caar x) 'relocation) (+ (size (caddar x)) (size (cdr x))))
    (else #f)))

(define (compile-and-link s)
  (define place-to-load 0)
  (define procedures (sexp->procs s))
  (for-each (lambda (proc)
  (let1 code (compile proc)
    (set! (car proc)  place-to-load)
    (set! (cadr proc) code)
    (set! place-to-load (+ place-to-load (size code))
    (for-each link x)))

(define (sexp->procs s)
(lambda (x)
  (lambda (i) (i x)))

PROC 0 : (lambda (x) /PROC 1/)
PROC 1 : (lambda (i) (i (lref 1 0)))

  (define proc (list 0 #f s))
(allocate x)
(length x)
