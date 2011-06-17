(define state-type *cons)







(use srfi-1)

(define logior bitwise-ior)
(define lognot bitwise-not)
(define logand bitwise-and)
(define ash arithmetic-shift)
(define mod modulo)
(require 'posix)

(define-syntax let1                     ;single variable bind
  (syntax-rules ()
    [(let1 var exp . body)
     (let ((var exp)) . body)]))

(define-syntax begin0
  (syntax-rules ()
    [(begin0 ret . rest)
     (let ((var ret)) (begin . rest) var)]))

(define-syntax push!
  (syntax-rules ()
    [(push! place value)
     (set! place (cons value place))]))

(define (write-byte b) (display (integer->char b)))
(define (flush) (flush-output))

(define registers32 '(eax ecx edx ebx esp ebp esi edi))
(define registers64 '(rax rcx rdx rbx rsp rbp rsi rdi))
(define registers64-extra '(r8 r9 r10 r11 r12 r13 r14 r15))

(define (relocation-proc p)
  (lambda (x y)
    (cond 
      ((and (number? x) (number? y)) (p x y))
      ((number? x) (relocation-symbol (lambda (i) (p x ((cadr y) i))) (lambda (i) (p x ((caddr y) i)))))
      ((number? y) (relocation-symbol (lambda (i) (p ((cadr x) i) y)) (lambda (i) (p ((caddr x) i) y))))
      (else (relocation-symbol (lambda (i) (p ((cadr x) i) ((cadr y) i))) (lambda (i) (p ((caddr x) i) ((caddr y) i))))))))
  
(define (enc bits len value)
  (if (number? value)
    (let1 v (mod value (expt (expt 2 bits) len))
      (if (eq? len 0)
        '()
        (cons (logand (lognot (ash -1 bits)) v) (enc bits (- len 1) (ash v (- bits))))))
    (list (relocation-symbol (lambda (i) (enc bits len ((cadr value) i))) (lambda (i) (enc bits len ((caddr value) i)))))))

(define (regi regsym)
    (or (list-index (cut eq? regsym <>) registers32)
        (list-index (cut eq? regsym <>) registers64)))

(define (machine-code x)
  `((builtin (machine-code ,(map opcode-filter x)) ,(lambda _ x))))

; register-in register-out instructions
(define (ropadd64 r1 r2)
  `(#x48 #x01 ,(logior #xc0 (regi r1) (ash (regi r2) 3))))

(define (ropstore64 r1 r2)
  `(#x48 #x89 ,(logior (regi r1) (ash (regi r2) 3))))

(define (ropread64 r1 r2)
  `(#x48 #x8b ,(logior (regi r2) (ash (regi r1) 3))))

(define (ropconst64 r1 i1)
  `(#x48 ,(logior #xB8 (regi r1)) ,@(enc 8 8 i1)))

(define (ropimul64 r1 r2)
  `(#x48 #x0F #xAF ,(logior #xC0 (regi r2) (ash (regi r1) 3))))
 
(define (ropcall64 r1)
  `(#xFF ,(logior #xD0 (regi r1))))

; immediate value jmp/call     
(define (roprelcall32 i)
  `(#xE8 ,@(enc 8 4 i)))

(define (opje32 i)
  `(#x0F #x84 ,@(enc 8 4 i)))

(define (opjmp32 i)
  `(#xE9 ,@(enc 8 4 i)))

; stack-in register-out instructions
(define (sopadd64 r1 idx1)
  (cond
    ((< #x-80 idx1 #x7f)
     `(#x48 #x03 ,(logior #x44 (ash (regi r1) 3)) #x24 ,@(enc 8 1 idx1)))))

(define (sopimul64 r1 idx1)
  (cond
    ((< #x-80 idx1 #x7f)
      `(#x48 #x0F #xAF ,(logior #x44 (ash (regi r1) 3)) #x24 ,@(enc 8 1 idx1)))))

(define (sopmov64 r1 idx1)
  (cond
    ((< #x-80 idx1 #x7f)
      `(#x48 #x8B ,(logior #x44 (ash (regi r1) 3)) #x24 ,@(enc 8 1 idx1)))))

(define (sopcmp64 r1 idx1)
  (cond
    ((< #x-80 idx1 #x7f)
     `(#x48 #x3B ,(logior #x44 (ash (regi r1) 3)) #x24 ,@(enc 8 1 idx1)))))

  ; emulation
(define (sopread64 r1 idx1)
  `(,@(sopmov64 r1 idx1)
    ,@(ropread64 r1 r1)))

(define (sopstore64 idx1 idx2)
  `(,@(sopmov64 'rax idx1)
    ,@(sopmov64 'rbx idx2)
    ,@(ropstore64 'rax 'rbx)))

(define (sopcall64 idx)
  `(,@(sopmov64 'rax idx)
    ,@(ropcall64 'rax)))

; register-in stack-out instructions
(define (rsopmov64 idx1 r1)
  (cond
    ((< #x-80 idx1 #x7f)
      `(#x48 #x89 ,(logior #x44 (ash (regi r1) 3)) #x24 ,@(enc 8 1 idx1)))))

(define (rspush r1)
  `(,(logior #x50 (regi r1))))

; return
(define (cret)
  '(#xc3))

; add n to rsp
(define (leave n)
  (if (= n 0)
   '()
   `(#x48 #x83 #xC4 ,@(enc 8 1 n))))

; substract n from rsp
(define (frame n)
  (if (= n 0)
   '()
   `(#x48 #x83 #xC4 ,@(enc 8 1 n))))

; refer and load
(define (lref n)
  (sopmov64 'rax n))

(define (const64 imm)
  (ropconst64 'rax imm))

(define (func-2-2-1 inst)
  (lambda (idx)
    (inst 'rax idx)))

(define (func-2-1-1 inst)
  (lambda (idx)
    (inst 'rax idx)))

(define add64  (func-2-2-1 sopadd64))
(define imul64 (func-2-2-1 sopimul64))
(define cmp64  (func-2-2-1 sopcmp64))
(define store64 (func-2-2-1 sopstore64))
(define read64 (cut ropread64 'rax 'rax))
(define push (cut rspush 'rax))

(define (make-closure proc frame)
  (call-with-memory (lambda (m)
    (store m #xeb049090909048b8) ; mov rax, ...
    (store (+ m 1) frame)
    (store (+ m 2)  #x50eb0490909090e9) ; push rax; jmp ... 
    (store (+ m 3) (- proc (+ m 28))
    m))))

(define ?number? number?)
(define ?symbol? symbol?)
(define (?procedure? l) (and (pair? l) (eq? (car l) 'lambda)))
(define (?builtin? l) (and (pair? l) (eq? (car l) 'builtin)))
(define (?syntax? l)  (and (pair? l) (eq? (car l) 'syntax)))
(define (relocation-symbol value subst) `(relocation ,value ,subst))

(define (size procs)
  (define s 0)
  (for-each (lambda (p)
    (set! (car p) s)
    (set! (caddr p) (compile-lambda-p5 (cadr p) #f))
    (let loop ((a (caddr p)))
      (for-each (lambda (b)
        (cond
          ((number? b) (set! s (+ 1 s)))
          ((not (pair? b)) b)
          ((eq? (car b) 'relocation) (loop ((cadr b) s)))
          (else b))) a))
    (set! s (+ s 3))) procs)) 


(define (compile-lambda-p3 s)
;  args
;
;  body
;    (builtin expr)
;    => (eval expr) (builtin 'rax)
;    (builtin pair expr)
;    => (eval expr) (push <a>) (eval pair) (builtin 'rax <a>) 
;    (builtin sym expr)
;    => (eval expr) (builtin 'rax (<refer> sym))
;    ((lambda ...) expr ...) ; renaming; equivalent to let form ; path1
;    (expr ...)
;     => (eval expr) (push <proc>) ... (call <proc>)
;    number 
;     => const number
;    (lambda ...)
;     => ((const 0) (const @lambda))
;
; XXX: use destructive-bind pattern match
;
  (define args (lambda-args s))
  (define body (compile (lambda-body s)))
  (define frame-variable-count 0)
  (define frame-variable-table '())
  (define code '())
  (define procs '())
  (define (make-frame-variable)
    (begin0 `(frame-variable ,frame-variable-count) (set! frame-variable-count (+ 1 frame-variable-count))))
;  arguments
  (for-each (lambda (a)
    (push! frame-variable-table (cons a (make-frame-variable)))) args)
  (push! code `(arguments ,(length args)))
  (make-frame-variable) ; room for the return address
;  body
  (let loop ((sexp body))
  (define body1-addr #f)
  (define body2-addr #f)
  (define conti-addr #f)
  (cond
    ((?number? sexp)    (push! code `((builtin const64 ,const64) ,sexp)))
    ((?procedure? sexp)
      (set! procs (append (compile-lambda-p3 sexp) procs))
      (let1 p (car procs)
        (push! code `((builtin const64 ,const64) ,(relocation-symbol (lambda (i) 0) (lambda (i) (car p)))))))
    ((?symbol? sexp)    (push! code `(,b-lref ,(cdr (assq sexp frame-variable-table)))))
    ((not (pair? sexp)) `(<unexpected> ,sexp))
    ((and (?builtin? (car sexp)) (null? (cdr sexp)) `(<unexpected> ,sexp)))
    ((and (?builtin? (car sexp)) (null? (cddr sexp))) (loop (cadr sexp)) (push! code `(,(car sexp))))
    ((and (?builtin? (car sexp)) (null? (cdddr sexp)) (symbol? (cadr sexp))) (loop (caddr sexp)) (push! code `(,(car sexp) ,(cdr (assq (cadr sexp) frame-variable-table)))))
    ((and (?builtin? (car sexp)) (null? (cdddr sexp))) (loop (caddr sexp)) (let1 y (make-frame-variable) (push! code `(push ,y)) (loop (cadr sexp)) (push! code `(,(car sexp) ,y))))
    ((and (?syntax? (car sexp))) ;if
        (loop (cadr sexp))
        (push! code `(,b-if
          ,(relocation-symbol (lambda (i) 0) (lambda (i) body1-addr))
          ,(relocation-symbol (lambda (i) 0) (lambda (i) body2-addr))))
        (push! code (machine-code `(,(relocation-symbol (lambda (i) (set! body1-addr i) '()) (lambda (i) '())))))
        (loop (caddr sexp))
        (push! code `((builtin jmp ,opjmp32) ,(relocation-symbol (lambda (i) 0) (lambda (i) (- conti-addr i 4)))))
        (push! code (machine-code `(,(relocation-symbol (lambda (i) (set! body2-addr i) '()) (lambda (i) '())))))
        (loop (cadddr sexp))
        (push! code (machine-code `(,(relocation-symbol (lambda (i) (set! conti-addr i) '()) (lambda (i) '()))))))
    ((?procedure? (car sexp))
      (set! procs (append (compile-lambda-p3 (car sexp)) procs))
      (let1 p (car procs)
       (for-each (lambda (e) (loop e) (push! code `(push ,(make-frame-variable)))) (cdr sexp))
       (push! code `(rel-call ,(relocation-symbol (lambda (i) 0) (lambda (i) (- (car p) i 4)))))))
    (else (let1 proc (make-frame-variable)
       (loop (car sexp)) (push! code `(push ,proc))
       (for-each (lambda (e) (loop e) (push! code `(push ,(make-frame-variable)))) (cdr sexp))
       (push! code `(call ,proc))))))
  (cons (list 0 (reverse code) '() s) procs))

(define b-lref `(builtin lref ,lref))
(define b-push `(builtin push ,push))
(define b-lstore `(builtin lstore ,(cut rsopmov64 <> 'rax)))
(define b-code-const64 `(builtin code-const64 ,(cut sopmov64 'rbx <>)))
(define b-call     `(builtin call ,(lambda () `(#x48 #x8B #x3C #x24 #x48 #x8B #x74 #x24 #x08 ,@(ropcall64 'rbx)))))
(define b-rel-call `(builtin rel-call ,roprelcall32))
(define b-if `(builtin if ,(lambda (proc1 proc2) `(,@(opje32 (relocation-symbol (lambda (i) 0) (lambda (i) ((relocation-proc -) proc1 (+ i 4))))) ,@(opjmp32 (relocation-symbol (lambda (i) 0) (lambda (i) ((relocation-proc -) proc2 (+ i 4)))))))))
(define b-frame `(builtin frame ,frame))
(define rel-call (lambda (i) `(,@(frame -8) #x48 #x89 #xC7 ,@(roprelcall32 i) ,@(leave 8))))

(define (compile-lambda-p5 opcode relocation)
  (apply append (map (lambda (s)
    (cond
      ((and (pair? (car s)) (eq? (caar s) 'relocation)) (if relocation s (apply (caddr (car (cadar s))) (cdr (cadar s)))))
      (else (apply (caddr (car s)) (cdr s)))))
     (reverse (compile-lambda-p4-human opcode)))))

(define (compile-lambda-p4-human opcode)
  (define depth -8)
  (define ret 0)
  (define code '())
  (define (frame-variable-substitute s)
    (cond
      ((not (pair? s)) s)
      ((eq? (car s) 'frame-variable) (- ret (* 8 (cadr s))))
      (else (map frame-variable-substitute s))))
  (for-each (lambda (s)
    (cond
      ((eq? (car s) 'arguments) (set! depth (+ depth (* 8 (cadr s)) 8)) (set! ret (* 8 (cadr s))))
      ((eq? (car s) 'push)      (set! depth (+ depth 8)) (push! code `(,b-lstore ,(- ret depth))))
      ((eq? (car s) 'rel-call)  (set! code (append `((,b-frame ,(- depth ret)) (,b-rel-call ,(cadr s)) (,b-frame ,(- ret depth))) code)))
      ((eq? (car s) 'call) (set! code (append `((,b-frame ,(- depth ret)) (,b-call) (,b-frame ,(- ret depth)) (,b-code-const64 ,(frame-variable-substitute (cadr s)))) code)))
      (else (push! code (frame-variable-substitute s)))))  opcode)
   ;(push! code `((builtin leave ,leave) ,(- depth ret)))
   (push! code `((builtin cret ,cret)))
  code)

(define (binary-compile s)
  (let1 procs (compile-lambda-p3 s)
    (size procs)
    (with-output-to-string 
      (lambda ()
        (dump-procs procs)))))

(define (dump-procs procs)
  (define i 0)
  (let loop ((p procs))
    (when (not (null? p))
      (for-each (lambda _ (write-byte #x90) (set! i (+ i 1))) (iota (- (car (car p)) i)))
      (let loop2 ((seq (caddr (car p))))
        (for-each (lambda (b)
            (if (and (pair? b) b (eq? (car b) 'relocation))
              (loop2 ((caddr b) i))
              (begin (write-byte b) (set! i (+ i 1))))) seq))
      (loop (cdr p)))))

  (define (opcode-filter a)
    (cond
      ((not (pair? a)) a)
      ((eq? 'frame-variable (car a)) (cadr a))
      ((eq? 'builtin (car a)) (cadr a))
      ((eq? 'relocation (car a)) "#<reloc>")
      ((not (pair? (cdr a))) (map opcode-filter a))
      ((eq? 'frame-variable (cadr a)) (caddr a) )
      (else (map opcode-filter a))))

(define (lambda-args l)
  (cond
    ((not (pair? l)) '())
    ((eq? 'lambda (car l)) (cadr l))
    (else '())))

(define (lambda-body l)
  (cond 
    ((not (pair? l)) l)
    ((eq? 'lambda (car l)) (caddr l))
    (else l)))

(define (compile s)
  (cond
    ((eq? s '+)    `(builtin add ,add64))
    ((eq? s '*)    `(builtin imul ,imul64))
    ((eq? s '=)    `(builtin cmp  ,cmp64))
    ((eq? s 'read) `(builtin read ,read64))
    ((eq? s 'if) `(syntax if))
    ((not (pair? s)) s)
    ((and (pair? (car s)) (eq? (caar s) 'lambda)) (cons (car s) (map compile (cdr s))))
    ((eq? (car s) 'lambda) `((builtin add ,add64) base-addr ,s))
    (else (map compile s))))

(return-to-host)
