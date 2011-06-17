(use srfi-1)
(use gauche.sequence)

(define registers32 '(eax ecx edx ebx esp ebp esi edi))
(define registers64 '(rax rcx rdx rbx rsp rbp rsi rdi))
(define registers64-extra '(r8 r9 r10 r11 r12 r13 r14 r15))

(define (enc bits len value)
  (let1 v (mod value (expt (expt 2 bits) len))
    (if (eq? len 0)
      '() 
      (cons (logand (lognot (ash -1 bits)) v) (enc bits (- len 1) (ash v (- bits)))))))

(define (regi regsym)
    (or (list-index (pa$ eq? regsym) registers32)
        (list-index (pa$ eq? regsym) registers64)))

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
 
(define (roprelcall32 i)
  `(#xE8 ,@(enc 8 4 i)))

(define (ropcall64 r1)
  `(#xFF ,(logior #xD0 (regi r1))))
     
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

;(define (push)
;  (append
;    (opstore64 'rcx 'eax)
;    (opconst64 'ebx -8)
;    (opadd64   'rcx 'ebx)))
;
;(define (pop reg)
;  (append
;    (opconst64 reg 8)
;    (opadd64 'rcx reg)
;    (opread64 reg 'rcx)))
;
;(define (ret)
;  (append
;    (opconst64 'rbx 8)
;    (opadd64 'rcx 'rbx)
;    (opstore64 'rcx 'rax) ;debug - obsolete
;    (cret)))
;
;(define (lref dep n)
;  (if (eq? dep 0)
;    (sopmov64 'eax n)))
;
;(define (opcode expr)
;  (cond
;    ((pair? expr)
;      (append
;        (apply append (map (^e
;          (append
;            (opcode e)
;            (push))) (cdr expr)))
;        (if (procedure? (car expr))
; ;inline procedure
;          ((car expr))
;          (list (lambda (i wr alloc) (let1 addr (alloc 4) (wr addr ((caar expr))) (opjmp32- addr)))))))
;; load constant
;    ((number? expr)
;      (opconst64 'eax expr))
;    (else
;      (opconst64- 'eax expr))))

(define (func-2-2-1 inst)
  (lambda (idx)
    (inst 'rax idx)))

(define (func-2-1-1 inst)
  (lambda (idx)
    (inst 'rax idx)))

(define add64  (func-2-2-1 sopadd64))
(define imul64 (func-2-2-1 sopimul64))
(define store64 (func-2-2-1 sopstore64))
(define read64 (cut ropread64 'rax 'rax))
(define push (cut rspush 'rax))

;(lambda (x) (+ (* 2 x) x))
;  const 2
;  fimul64 'rax 0
;  fadd64  'rax 0
;  ret 
;
;(lambda (x) (+ (* 2 x) (* x x)))
;  const 2
;  fimul64 'rax <x> (0) 
;  push => <y> (0)
;  srmov64 'rax <x> (8)
;  fimul64 'rax <x> (8)
;  push => <z> (0)
;  srmov64 'rax <y> (8)
;  fadd64 'rax <z> (0)
;  ret
;
;(define (compile-p2 body)
;  (commutable atom pair ...)
;  => (commutable pair (commutable atom ...))
;
;  (incommutable atom pair ...)
;  => ((reverse incommutable) pair (incommutable atom ...))
;
;  (begin atom pair ...)
;  => (begin atom pair ...) )
;
;(define (compile-p1 body)
;  ((lambda ...) expr ...) ; substitute
;  (builtin imm imm) ; calculate

(define ?number? number?)
(define ?symbol? symbol?)
(define (?procedure? l) (and (pair? l) (eq? (car l) 'lambda)))
(define (?builtin? l) (and (pair? l) (eq? (car l) 'builtin)))
(define (relocation-symbol value subst) `(relocation ,value ,subst))

(define (size x)
  (cond
    ((null? x) 0)
    ((not (pair? x)) #f)
    ((number? (car x)) (+ 1 (size (cdr x))))
    ((not (pair? (car x))) #f)
    ((eq? (caar x) 'relocation) (+ (size (caddar x)) (size (cdr x))))
    (else #f)))

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
  (cond
    ((?number? sexp)    (push! code `((builtin const64 ,const64) ,sexp)))
    ((?procedure? sexp)
      (set! procs (append (compile-lambda-p3 sexp) procs))
      (let1 p (car procs)
        (push! code `(,(relocation-symbol `((builtin const64 ,const64) 0) (lambda (i) (const64 (car p))))))))
    ((?symbol? sexp)    (push! code `(,b-lref ,(cdr (assq sexp frame-variable-table)))))
    ((not (pair? sexp)) `(<unexpected> ,sexp))
    ((and (?builtin? (car sexp)) (null? (cdr sexp)) `(<unexpected> ,sexp)))
    ((and (?builtin? (car sexp)) (null? (cddr sexp))) (loop (cadr sexp)) (push! code `(,(car sexp))))
    ((and (?builtin? (car sexp)) (null? (cdddr sexp)) (symbol? (cadr sexp))) (loop (caddr sexp)) (push! code `(,(car sexp) ,(cdr (assq (cadr sexp) frame-variable-table)))))
    ((and (?builtin? (car sexp)) (null? (cdddr sexp))) (loop (caddr sexp)) (let1 y (make-frame-variable) (push! code `(push ,y)) (loop (cadr sexp)) (push! code `(,(car sexp) ,y))))
    ((?procedure? (car sexp))
      (set! procs (append (compile-lambda-p3 (car sexp)) procs))
      (let1 p (car procs)
       (for-each (lambda (e) (loop e) (push! code `(push ,(make-frame-variable)))) (cdr sexp))
       (push! code `(,(relocation-symbol `(,b-rel-call 0) (lambda (i) (rel-call (- (car p) i 12))))))))
    (else (let1 proc (make-frame-variable)
       (loop (car sexp)) (push! code `(push ,proc))
       (for-each (lambda (e) (loop e) (push! code `(push ,(make-frame-variable)))) (cdr sexp))
       (push! code `(,b-call ,proc))))))
  (cons (list 0 (reverse code) '() s) procs))

(define b-lref `(builtin lref ,lref))
(define b-push `(builtin push ,push))
(define b-lstore `(builtin lstore ,(cut rsopmov64 <> 'rax)))
(define b-call     `(builtin call ,(lambda (i) `(,@(sopmov64 'rbx i) ,@(frame -16) #x48 #x89 #xC7 ,@(ropcall64 'rbx) ,@(leave 16)))))
(define rel-call (lambda (i) `(,@(frame -8) #x48 #x89 #xC7 ,@(roprelcall32 i) ,@(leave 8))))
(define b-rel-call `(builtin rel-call ,rel-call))

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
      (else (push! code (frame-variable-substitute s)))))  opcode)
   ;(push! code `((builtin leave ,leave) ,(- depth ret)))
   (push! code `((builtin cret ,cret)))
   code)

(define dump #f)
(define (main args)
  (define index 0)
  (display "test> ") (flush)
  (let1 s (read)
    (cond
      ((and (pair? s) (eq? (car s) 'exit)) (exit))
      ((and (pair? s) (eq? (car s) 'dump)) (set! dump (cadr s)))
      (else
        (let1 procs (compile-lambda-p3 `(lambda (call-with-freed-memory call-with-memory call-with-output-char call-with-input-char base-addr) ,s))
          (set! index 0)
          (for-each (lambda (p)
            (set! (car p) index)
            (set! (caddr p) (compile-lambda-p5 (cadr p) #f))
            (set! index (+ index 3 (size (caddr p))))) procs)
          (for-each (lambda (p)
            (set! (caddr p) (compile-lambda-p5 (cadr p) #t))) procs)
          (when dump (opcode-print procs))
          (with-output-to-file "output.bin" (^()
            (dump-procs procs)))
          (when dump (sys-system "ndisasm output.bin -b64"))
          (sys-system "./load-binary output.bin")))))
  (main args))

(define (dump-procs procs)
  (define i 0)
  (let loop ((p procs))
    (when (not (null? p))
      (for-each (lambda _ (write-byte #x90) (set! i (+ i 1))) (iota (- (car (car p)) i)))
      (let loop2 ((seq (caddr (car p))))
        (for-each (lambda (b)
            (if (and (pair? b) (eq? (car b) 'relocation))
              (loop2 ((caddr b) i))
              (begin (write-byte b) (set! i (+ i 1))))) seq))
      (loop (cdr p)))))

(define (opcode-print s)
  (define (opcode-filter a)
    (cond
      ((not (pair? a)) a)
      ((eq? 'frame-variable (car a)) (cadr a))
      ((eq? 'builtin (car a)) (format "#~a" (cadr a)))
      ((eq? 'relocation (car a)) (format "#reloc<~a>" (opcode-filter (cadr a))))
      ((not (pair? (cdr a))) (map opcode-filter a))
      ((eq? 'frame-variable (cadr a)) (format "~a<~a>"  (caddr a) (car a)))
      (else (map opcode-filter a))))
  (for-each
    (^a (print (opcode-filter a)))
    (reverse s)))

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
;(define (dump++ s)
;  (define i 0)
;  (let dump- ((s s))
;    (define lis '())
;    (let dump+ ((s s))
;      (for-each (^b 
;       (cond 
;         ((number? b) (write-byte b) (set! i (+ i 1)))
;         ((procedure? b) (dump+ (b i (lambda (a x) (push! lis (cons a x))) (lambda (size) (+ 13 (length s)))))))) s))
;    (set! lis (reverse lis))
;  ; padding
;    (when (not (null? lis))
;      (while (< i (caar lis)) (write-byte 0) (set! i (+ i 1))))
;    (for-each dump- (map (^e (cdr e)) (reverse lis)))))

;(define (compile s a level)
;  (cond
;    ((assq s a) ((cdr (assq s a)) level))
;    ((not (pair? s)) s)
;    ((eq? (car s) '+)      (cons func-add64   (map (cut compile <> a level) (cdr s))))
;    ((eq? (car s) '*)      (cons func-imul64  (map (cut compile <> a level) (cdr s))))
;    ((eq? (car s) 'read)   (cons func-read64  (map (cut compile <> a level) (cdr s))))
;    ((eq? (car s) 'set!)   (list func-store64 (compile (caddr s) a level) (compile (cadr s) a level)))
;    ((eq? (car s) 'lambda) (lambda () (append (opcode (compile (caddr s) (cons (cons (cadr s) (lambda (l) (list (lambda () (lref (- l level 1) 0))))) a) (+ 1 level))) (ret))))
;    (else `(,(compile (car s) a level) ,@(map (cut compile <> a level) (cdr s))))))

(define (compile s)
  (cond
    ((eq? s '+)    `(builtin add ,add64))
    ((eq? s '*)    `(builtin imul ,imul64))
    ((eq? s 'read) `(builtin imul ,read64))
    ((not (pair? s)) s)
    ((and (pair? (car s)) (eq? (caar s) 'lambda)) (cons (car s) (map compile (cdr s))))
    ((eq? (car s) 'lambda) `((builtin add ,add64) base-addr ,s))
    (else (map compile s))))
