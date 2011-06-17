(use srfi-1)

(define (freevars t) (find-free-vars t '()))

(define (find-free-vars term bound-vars)
  (cond
    ((and (pair? term) (eq? (car term) 'lambda)) (find-free-vars (cddr term) (cons bound-vars (cadr term))))
    ((pair? term)  (apply append (map (lambda (x) (find-free-vars x bound-vars)) term)))
    (else  (if (and (not (any (cut eq? term <>) bound-vars)) (symbol? term)) (list term) '()))))

(define (heap-vars term vars)
  (cond
    ((and (pair? term) (eq? (car term) 'lambda)) (filter (lambda (x) (any (cut eq? <> x) vars)) (freevars term)))
    ((pair? term) (apply append (map (cut heap-vars <> vars) term)))
    (else '())))

(define (stack-vars heap-vars vars)
  (filter (lambda (x) (not (any (lambda (y) (eq? #?=x #?=y)) heap-vars))) #?=vars))

(define (compile a)
  (compile* (cddr a) (cadr a) '() (cadr a)))

(define (compile* body args frame vars)
  (let* ((heap (heap-vars body args)) (stack (stack-vars heap args)))
    `((lambda . syntax) ,(transform args heap stack frame vars) ,@(transform body heap stack frame vars))))

(define (transform term heap stack frame vars)
  (cond
    ((and (pair? term) (eq? (car term) 'lambda))
      `((close . syntax)
         ,(map (cut transform <> heap stack frame vars) (filter (lambda (x) (any (cut eq? <> x) vars)) (freevars term)))
         ,(compile* (cddr term) (cadr term) heap (append (cadr term) vars))))
    ((list? term) (map (cut transform <> heap stack frame vars) term))
    ((any (cut eq? <> term) heap)  `(,term . heap))
    ((any (cut eq? <> term) stack) `(,term . stack))
    ((any (cut eq? <> term) vars)  `(,term . frame))
    (else `(,term . gref))))

(define (disp-tree term)
  (cond
    ((and (pair? term) (eq? (cdr term) 'stack))  (display (format "[35m~a[m " (car term))))
    ((and (pair? term) (eq? (cdr term) 'gref))   (display (format "[37m~a[m " (car term))))
    ((and (pair? term) (eq? (cdr term) 'frame)) (display (format "[32m~a[m " (car term))))
    ((and (pair? term) (eq? (cdr term) 'syntax))   (display (format "[33m~a[m " (car term))))
    ((and (pair? term) (eq? (cdr term) 'heap))  (display (format "[34m~a[m " (car term))))
    ((pair? term) (display "(") (for-each disp-tree term) (display ") "))
    (else (display term) (display " "))))

