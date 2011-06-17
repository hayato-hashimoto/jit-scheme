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

(define (opadd64 r1 r2)
  `(#x48 #x01 ,(logior #xc0 (regi r1) (ash (regi r2) 3))))

(define (opstore64 r1 r2)
  `(#x48 #x89 ,(logior (regi r1) (ash (regi r2) 3))))

(define (opread64 r1 r2)
  `(#x48 #x8b ,(logior (regi r2) (ash (regi r1) 3))))

(define (opconst64 r1 i1)
  `(#x48 ,(logior #xB8 (regi r1)) ,@(enc 8 8 i1)))

(define (opconst64- r1 i1)
  `(#x48 ,(logior #xB8 (regi r1)) ,(lambda (i wr alloc) (let1 addr (alloc 8) (wr addr (i1)) (enc 8 8 addr)))))

(define (opimul64 r1 r2)
  `(#x48 #x0F #xAF ,(logior #xC0 (regi r2) (ash (regi r1) 3))))
 
(define (opjmp32- i1)
  `(#xE8 ,(lambda (i wr alloc) (enc 8 4 (- i1 i 2)))))
   
(define (cret)
  '(#xc3))

(define (push)
  (append
    (opstore64 'rcx 'eax)
    (opconst64 'ebx -8)
    (opadd64   'rcx 'ebx)))

(define (push-heap)
  (append
    (opstore64 'rdx 'eax)
    (opconst64 'ebx 8)
    (opadd64   'rdx 'ebx)))

(define (pop reg)
  (append
    (opconst64 reg 8)
    (opadd64 'rcx reg)
    (opread64 reg 'rcx)))

(define (ret)
  (append
    (opconst64 'rbx 8)
    (opadd64 'rcx 'rbx)
    (opstore64 'rcx 'eax)
    (cret)))

(define (lref dep)
  (if (eq? dep 0)
    (append
      (pop 'eax)
      (push))))

(define (opcode expr)
  (cond
    ((pair? expr)
      (append
        (apply append (map (^e
          (append
            (opcode e)
            (push))) (cdr expr)))
        (if (procedure? (car expr))
 ;inline procedure
          ((car expr))
          (list (lambda (i wr alloc) (let1 addr (alloc 4) (wr addr ((caar expr))) (opjmp32- addr)))))))
; load constant
    ((number? expr)
      (opconst64 'eax expr))
    (else
      (opconst64- 'eax expr))))

(define (func-2-2-1 inst)
  (lambda ()
  (append
    (pop 'ebx)
    (pop 'eax)
    (inst 'eax 'ebx))))
(define (func-2-1-1 inst)
  (lambda ()
  (append
    (pop 'eax)
    (inst 'eax 'eax))))

(define func-add64   (func-2-2-1 opadd64))
(define func-imul64  (func-2-2-1 opimul64))
(define func-store64 (func-2-2-1 opstore64))
(define func-read64  (func-2-1-1 opstore64))

(define dump #f)
(define (main args)
  (display "binary-test> ") (flush)
  (let1 s (read)
    (cond
      ((and (pair? s) (eq? (car s) 'exit)) (exit))
      ((and (pair? s) (eq? (car s) 'dump)) (set! dump (cadr s)))
      (else (with-output-to-file "output.bin" (^()
          (dump++ (append (opconst64 'rcx 8448) (opcode (compile s '() 0)) (opstore64 'rcx 'eax) (cret)))))
        (when dump (sys-system "ndisasm output.bin -b64"))
        (sys-system "./load-binary output.bin"))))
  (main args))

(define (dump++ s)
  (define i 0)
  (let dump- ((s s))
    (define lis '())
    (let dump+ ((s s))
      (for-each (^b 
       (cond 
         ((number? b) (write-byte b) (set! i (+ i 1)))
         ((procedure? b) (dump+ (b i (lambda (a x) (push! lis (cons a x))) (lambda (size) (+ 13 (length s)))))))) s))
    (set! lis (reverse lis))
  ; padding
    (when (not (null? lis))
      (while (< i (caar lis)) (write-byte 0) (set! i (+ i 1))))
    (for-each dump- (map (^e (cdr e)) (reverse lis)))))

(define (compile s a level)
  (cond
    ((assq s a) ((cdr (assq s a)) level)) 
    ((not (pair? s)) s)
    ((eq? (car s) '+)      (cons func-add64   (map (cut compile <> a level) (cdr s))))
    ((eq? (car s) '*)      (cons func-imul64  (map (cut compile <> a level) (cdr s))))
    ((eq? (car s) 'read)   (cons func-read64  (map (cut compile <> a level) (cdr s))))
    ((eq? (car s) 'set!)   (list func-store64 (compile (caddr s) a level) (compile (cadr s) a level)))
    ((eq? (car s) 'lambda) (lambda () (append (opcode (compile (caddr s) (cons (cons (cadr s) (lambda (l) (list (lambda () (lref (- l level 1)))))) a) (+ 1 level))) (ret))))
    (else `(,(compile (car s) a level) ,@(map (cut compile <> a level) (cdr s))))))
