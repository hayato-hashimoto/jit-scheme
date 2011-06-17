;(if (defined? 'flush) #f (define flush (lambda () #f)))
(cond-expand
  ((and srfi-1 srfi-8 srfi-26)
(define (print x) (display x) (newline))
(define (join list) (fold-right append '() list))
(define (undefined) '<undefined>)

;; the debug function :p
(define (p x) (print x) x)

;; vm instructions
(define (vm a s e c)
 ((car c) vm a s e (cdr c)))
(define (halt)
  (lambda (vm a s e c) (values a (lambda (vm) (vm a s e c)))))
(define (refer sym)
  (lambda (vm a s e c) (vm (cdr (lookup sym e)) s e c)))
(define (constant obj)
  (lambda (vm a s e c) (vm obj s e c)))
(define (close body)
  (lambda (vm a s e c) (vm (list '<closure> e body) s e c)))
(define (test then else)
  (lambda (vm a s e c) (vm a s e (append (if a then else) c))))
(define (push)
  (lambda (vm a s e c) (vm a (cons (cons a (car s)) (cdr s)) e c)))
(define (pop)
  (lambda (vm a s e c) (vm (caar s) (cons (cdar s) (cdr s)) e c)))
(define (assign sym)
  (lambda (vm a s e c)
    (set-cdr! (lookup sym e) a)
    (vm a s e c)))
(define (bind sym)
  (lambda (vm a s e c)
    (vm a s (cons (cons (cons sym a) (car e)) (cdr e)) c)))
(define (conti x)
  (lambda (vm a s e c)
    (vm (continuation s) s e x)))
(define (frame)
  (lambda (vm a s e c)
    (vm a (cons '() s) e c)))
(define (return)
  (lambda (vm a s e c)
    (vm a s (cdr e) c)))
(define (vm-apply)
  (lambda (vm a s e c)
    (vm (apply-function (reverse (car s)) s) (cdr s) e c)))

(define (apply-function lst s)
  (case (caar lst)
     ((<builtin>) (apply (cdar lst) (cdr lst)))
     ((<closure>) (receive (value cont) (vm #f s (cadar lst) (caddar lst)) value))
     (else (print (format #t "err: unknown procedure: ~a" (car lst))))))
(define (lookup sym env)
  (any (cut assq sym <>) env))

; special forms
; top-level define
(define (%define key value)
  `(,(constant (undefined))
    ,(bind key)
    ,@(%eval value)
    ,(assign key)
    ,(constant (undefined))))

(define (%eval sexp)
  (cond
    ((symbol? sexp) (list (refer sexp)))
    ((list? sexp) (if (assq (car sexp) special-forms)
       (apply (cdr (assq (car sexp) special-forms)) (cdr sexp))
       `(,@(%eval (car sexp)) ,(macro?) ,(test `(,(apply-macro sexp)) `(,(frame) ,@(join (map (lambda (it) `(,@(%eval it) ,(push))) sexp)) ,(vm-apply) )))))
    (else (list (constant sexp)))))

(define (macro?)
  (lambda (vm a s e c)
    (vm (eq? (car a) '<macro>) s e c)))

(define (apply-macro sexp)
  (lambda (vm a s e c)
    (vm a s e (append 
      (%eval (receive (value cont) (vm a s e `(,(frame) ,(frame) ,@(%eval 'cdr) ,(push) ,@(%eval (car sexp)) ,(push) ,(vm-apply) ,(push)
        ,@(join (map (lambda (it) `(,(constant it) ,(push))) (cdr sexp))) ,(vm-apply) ,(halt))) value)) c))))

(define (macro proc)
  (cons '<macro> proc))

(define (%lambda args . body)
  (list (close `(
    ,@(join (reverse (map (lambda (x) `(,(pop) ,(bind x))) args)))
    ,@(join (map %eval body))
    ,(halt)))))

(define (%quote sexp)
  (list (constant sexp)))

(define (%if pred then else)
  `(,@(%eval pred) ,(test (%eval then) (%eval else))))

(define special-forms
 `((define . ,%define)
   (eval   . ,%eval)    ;actually not ..
   (lambda . ,%lambda)
   (if     . ,%if)
   (quote  . ,%quote)))

;built-in functions
(define environment
  `((car <builtin> . ,car)
    (cdr <builtin> . ,cdr)
    (cons <builtin> . ,cons)
    (list <builtin> . ,list)
    (+   <builtin> . ,+)
    (-   <builtin> . ,-)
    (=   <builtin> . ,=)
    (>   <builtin> . ,>)
    (<   <builtin> . ,<)
    (*   <builtin> . ,*)
    (/   <builtin> . ,/)
    (macro <builtin> . ,macro)))

; now everything got ready ..
(define (vm-evaluate-sexp sexp)
  (lambda (a s e c)
    (vm a s e `(,@(%eval sexp) ,(halt)))))

(define (repl continuation)
  (display "scheme> ")
  (flush)
  (receive (value cont)
    (continuation (vm-evaluate-sexp (read)))
    (print value)
    (repl cont)))

(repl (lambda (vm) (vm #f '(()) (list environment) #f)))
));cond-expand
