(use srfi-1)
(use gauche.sequence)

(define registers32 '(eax ecx edx ebx esp ebp esi edi))
(define registers64 '(rax rcx rdx rbx rsp rbp rsi rdi))
(define registers64-extra '(r8 r9 r10 r11 r12 r13 r14 r15))

(define (enc bits len value)
  (if (eq? len 0) 
    '() 
    (cons (logand (lognot (ash -1 bits)) value) (enc bits (- len 1) (ash value (- bits))))))

(define (regi regsym)
    (or (list-index (pa$ eq? regsym) registers32)
        (list-index (pa$ eq? regsym) registers64)))


(define (opadd32 r1 r2)
  `(#x01 ,(logior #xc0 (regi r1) (ash (regi r2) 3))))

(define (opadd64 r1 r2)
  `(#x48 #x01 ,(logior #xc0 (regi r1) (ash (regi r2) 3))))

(define (opstore64 r1 r2)
  `(#x48 #x89 ,(logior (regi r1) (ash (regi r2) 3))))

(define (opread64 r1 r2)
  `(#x48 #x8B ,(logior (regi r1) (ash (regi r2) 3))))

(define (opconst64 r1 i1)
  `(#x48 ,(logior #xB8 (regi r1)) ,@(enc 8 8 i1)))

(define (alist->hash-table list)
  (define hash (make-hash-table))
  (for-each (lambda (entry) (hash-table-put! hash (car entry) (cdr entry)) hash) list)
  hash)

;abstraction of 2-address opcode
(define (reg2op-type x y)
  (lambda (x y) x))

(define (id0 x . y)   x)
(define (id1 x y . z) y)
(define (id01 x y . z) (values x y))
(define (nothing . x)   (values))

;(opcode read-regs write-regs dependency arguments)

(define mcodes (alist->hash-table `(
  (add64   ,opadd64   ,id01 ,id0 ,id0 'reg64 'reg64)
  (read64  ,opread64  ,id1 ,id0 ,nothing 'reg64 'reg64)
  (store64 ,opstore64 ,id01 ,nothing ,nothing 'reg64 'reg64)
  (const64 ,opconst64 ,id1 ,id0 ,nothing 'reg64 'imm64)
;  (xor64   (,opxor64   (r1 reg64) (r2 reg64)) (r1 r2) (r1))
;  (cmp32   (,opcmp32   (r1 reg32) (r2 reg32) (f1 flag)) (r1 r2) (f1)) 
)))

(define (compile-eval expression outreg regn)
  (if (pair? #?=expression)
    (let* ((opspec (assq (car expression) mcodes))
           (regtbl (map
             (lambda (regent) 
               (cons (car regent)
                 (cdr (or (assq (car regent) (map cons (list-ref opspec 3) (list outreg))) 
                          (cons '() (list-ref registers64 (regn (+ 1 (regn)))))))))
             (cdr (list-ref opspec 1)))))
      (append (apply append (map (lambda (e r) #?=(compile-eval e (cdr (assq r #?=regtbl)) regn)) (cdr expression) (list-ref opspec 2)))
         (apply (eval (caadr opspec) (interaction-environment)) (map (lambda (regent) (cdr (assq (car regent) regtbl))) (cdr (list-ref opspec 1))))))
    (opconst64 outreg expression)))

(define (register-pressure expression)
   (if (pair? expression)
     (max (map + (sort (map register-pressure (cdr expression) >)) '(0 1 2 3 4 5 6 7)))
     1))

(define (opcode expression available-registers output-register spill)
  (if (pair? expression)
      (let loop ((spill-from '()) (spill-to '()) (avail available-registers) (avail2 available-registers)
                 (args-code '()) (args-outs '()) (rest-args (cdr expression)))
        (if (null? rest-args)
          ; main expression
          (let1 opformat (caadr (assq (car expression) mcodes))
            `(,@args-code
              ,@(apply append (map (lambda (x y) `(,(opconst64 'edi x) ,(opread64 'edi y))) spill-to spill-from)) ;restore spill-outs
              ,(apply opformat (format-oparg expression (reverse args-outs) output-register)) ;eval expression
              ))
          ; evaluate arguments
          (let* ((arg-out  (allocate-register expression (car rest-args) output-register avail2 spill))
                 (arg-code (opcode (car rest-args) avail arg-out spill)))
                (if (spill? arg-out)
                    (loop (cons (reg arg-out) spill-from) (cons (mem arg-out) spill-to) avail (remove (pa$ eq? arg-out) avail2)
                          `(,@args-code ,@arg-code ,(opconst64 'edi (to arg-out)) ,(opstore64 'edi arg-out))
                           (cons arg-out args-outs) (cdr rest-args))
                    (loop spill-from spill-to (remove (pa$ eq? arg-out) avail) (remove (pa$ eq? arg-out) avail2)
                          `(,@args-code ,@arg-code) (cons arg-out args-outs) (cdr rest-args))))))
      (list (opconst64 output-register expression))))

(define (index->true len . trues)
  (if (= len 0) '()
   (cons (any (pa$ = 0) trues) (apply (pa$ index->true (- len 1)) (map (cut - <> 1) trues)))))
                                       
(define (allocate-register caller callee caller-out avail-regs spill)
  (let ((i (find-index (pa$ eq? callee) caller)) (a #?=(ref mcodes (car caller))))
  (cond
    ((apply (list-ref a 3) (index->true 4 i)) caller-out)
    ((enough-register? caller callee avail-regs) (car avail-regs))
    (else (cons (car avail-regs) (spill))))))

(define reg car)
(define mem cdr)
(define spill? pair?)
(define (enough-register? a b avail-regs) (< 1 (length avail-regs)))

(define (make-spill)
  (lambda ()
    (define a 0)
    (set! a (+ a 8))
    a))

(define (format-oparg exp opregs out)
  (map (lambda (x) (cond ((eq? (car x) (car (list-ref (assq (car exp) mcodes) 3))) out)
                       (else (let1 i (find-index (pa$ eq? (car x)) (list-ref (assq (car exp) mcodes) 2)) (list-ref opregs i)))))
     (cdadr (assq (car exp) mcodes))))

(define (dump x)
  (print x)
  x)

(define (test)
  (display "> ") (flush)
  (with-output-to-file "output.bin" (lambda ()
      (for-each (lambda (x) (write-byte x)) (apply append (opcode (dump (compile (read))) '(rax rbx rcx rdx) 'rax (make-spill))))))
    (+ 1 (sys-system "ndisasm output.bin -b64"))
  (test))

(define (compile s)
  (cond
    ((not (pair? s)) s)
    ((eq? (car s) '+) (cons 'add64 (compile (cdr s))))
    ((eq? (car s) 'read) (cons 'read64 (compile (cdr s))))
    ((pair? (car s)) (cons (compile (car s)) (compile (cdr s))))
    (else (cons (car s) (compile (cdr s))))))

(test)
