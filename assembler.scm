(use srfi-1)
(use gauche.sequence)

(define registers32 '(eax ecx edx ebx esp ebp esi edi))
(define registers64 '(rax rcx rdx rbx rsp rbp rsi rdi r8 r9 r10 r11 r12 r13 r14 r15))

(define (enc bits len value)
  (if (eq? len 0) 
    '() 
    (cons (logand (lognot (ash -1 bits)) value) (enc bits (- len 1) (ash value (- bits))))))

(define (regi regsym)
    (or (list-index (pa$ eq? regsym) registers32)
        (list-index (pa$ eq? regsym) registers64)))

(lambda (opr64-r/m64 opcode)
  (lambda (r1 r2) `(#x48 ,opcode ,(logior (regi r1) (ash (regi r2) 3)))))

(define opadd64   (opr64-r/m64 #x03))
(define opsub64   (opr64-r/m64 #x2b))
(define opread64  (opr64-r/m64 #x89))
(define opstore64 (opr64-r/m64 #x8b))
(define opand64   (opr64-r/m64 #x23))
(define opor64    (opr64-r/m64 #x0b))
(define opxor64   (opr64-r/m64 #x33))
(define (opconst64 r1 i1)
  `(#x48 ,(logior #xB8 (regi r1)) ,@(enc 8 8 i1)))

(define mcodes `(
  (add32   (,opadd32   (r1 reg32) (r2 reg32)) (r1 r2) (r1))
  (add64   (,opadd64   (r1 reg64) (r2 reg64)) (r1 r2) (r1))
;  (store32 (,opstore32 (r1 reg32) (r2 reg64)) (r1 r2) ())
  (read64  (,opread64  (r1 reg64) (r2 reg64)) (r2)    (r1))
  (store64 (,opstore64 (r1 reg64) (r2 reg64)) (r1 r2) ())
  (const64 (,opconst64 (r1 reg32) (i1 imm32)) (i1)    (r1))
;  (xor64   (,opxor64   (r1 reg64) (r2 reg64)) (r1 r2) (r1))
;  (cmp32   (,opcmp32   (r1 reg32) (r2 reg32) (f1 flag)) (r1 r2) (f1)) 
))

(define mcodes `(
  (add64  ,opadd64   (r1 r2) (r1 r2) (r1)    (reg64 reg64))
  (sub64  ,opsub64   (r1 r2) (r1 r2) (r1)    (reg64 reg64))
  (mul64  ,opmul64   (r1) (r1 rax) (rax rdx) (reg64))
  (imul64 ,opimul64  (r1) (r1 rax) (rax rdx) (reg64))
  (div64  ,opdiv64   (r1) (r1 rax rdx) (rax) (reg64))
  (idiv64 ,opidiv64  (r1) (r1 rax rdx) (rax) (reg64))))
  (neg64  ,opneg64   (r1) (r1) (r1) (reg64))
  (xor64  ,opneg64   (r1 r2) (r1 r2) (r1) (reg64 reg64))
  (and64  ,opneg64   (r1 r2) (r1 r2) (r1) (reg64 reg64))
  (or64   ,opneg64   (r1 r2) (r1 r2) (r1) (reg64 reg64))
  (not64  ,opnot64   (r1) (r1) (r1)     (reg64))
  (shl64  ,opshl64   (r1) (r1 rcx) (r1) (reg64)) 
  (sar64  ,opsar64   (r1) (r1 rcx) (r1) (reg64))

(define polymorph '(
  (+ add8 add16 add32 add64 addbignum)
  (- add8 add16 sub32 sub64 subbignum)
  (logior or8 or16 or32 or64 orbignum)
  (logand and8 and16 and32 and64 andbignum)
  (ash shr8 shr16 shr32 shr64 shl8 shl16 shl32 shl64 ashbignum)
  (const const32 const64)))

path 3 :
  reg-pressure , special-pressure arg1
  reg-pressure , <special-pressure arg2>
  reg-pressure , special-pressure arg3

  1 sort with reg-pressure
  2 isoregisterize in
  3 isoregisterize out
  4 remove special-registers
  5 registerize from avail

  if spill requested:
    1 request parent to spill
    2 spill if possible
    3 report i am not responsible
  
  if avail runs out   
    1 request parent to spill

 path 3 :
  generate code 

(define (burn-opcode-p2 exp)
  (if (pair? expression)
    (map-rec (lambda (x)
      (if (and (eq? (car x) '+) (> 2 (length (filter constant? (cdr x)))))
          (append '(+) (remove constant? (cdr x)) (apply + (filter constant? (cdr x))))

(define (burn-opcode-p3 exp outregs splidx)
  (if (pair? expression)
      ; expression_evaluation : (0:expr 1:pressure 2:register(out) 3:register(in) 3:special-deserve)
      (let* ((sorted-subexp (sort (map (lambda (subexp) (list subexp (reg-pressure subexp) (output-register subexp) 'none (special-regs subexp))) (cdr exp))
        (lambda (x) (cadr x)))) (reversed-subexp (reverse sorted-subexp)))
        (match reversed-subexp) (match templatize)<isoregisterize out>
       
      (car (polymorph-apply (map class-of subexp))))
      
(define (opcode expression available-registers output-register spillindex)
  (if (pair? expression)
      (let loop ((spillfrom '()) (spillto '()) (avail available-registers) (avail2 available-registers)
                 (out output-register) (code '()) (opregs '()) (e (cdr expression)))
        (if (null? e)
          ; main expression
          (let1 opformat (caadr (assq (car expression) mcodes))
            `(,@code  ;eval arguments
              ,@(apply append (map (lambda (x y) `(,(opconst64 'edi x) ,(opread64 'edi y))) spillto spillfrom)) ;restore spill-outs
              ,(apply opformat (format-oparg expression (reverse opregs) output-register)) ;eval expression
              ))
           ; evaluate subexpression and go to next expression
          (let* ((out (or (bound-register expression (car e) output-register) (car avail2)))
                 (c   (opcode (car e) avail out (if (pair? spillto) (+ 8 (car spillto)) spillindex))))
              (if (and (null? (cddr avail)) (or (null? (cdr avail)) (and (pair? (car e)) (not (null? (cdr e))))))
                (let1 this_spillto (if (pair? spillto) (+ 8 (car spillto)) spillindex)
                  (loop (cons out spillfrom) (cons this_spillto spillto) avail (remove (pa$ eq? out) avail2) out 
                    `(,@code ,@c ,(opconst64 'edi this_spillto) ,(opstore64 'edi out)) (cons out opregs) (cdr e)))
                (loop spillfrom spillto (remove (pa$ eq? out) avail) (remove (pa$ eq? out) avail2) out `(,@code ,@c) (cons out opregs) (cdr e))))))
      (list (opconst64 output-register expression))))

(define (bound-register exp subexp out)
  (and (pair? (list-ref (assq (car exp) mcodes) 3))
    (if (eq? 
      (car (list-ref (assq (car exp) mcodes) 3))
      (car (list-ref (cadr (assq (car exp) mcodes))
        (find-index (pa$ eq? subexp) exp))))
    out #f)))

(define (format-oparg exp opregs out)
  (map (lambda (x) (cond ((eq? (car x) (car (list-ref (assq (car exp) mcodes) 3))) out)
                       (else (let1 i (find-index (pa$ eq? (car x)) (list-ref (assq (car exp) mcodes) 2)) (list-ref opregs i)))))
     (cdadr (assq (car exp) mcodes))))

(define (test)
  (display "> ") (flush)
  (with-output-to-file "output.bin" (lambda ()
    (for-each (lambda (x) (write-byte x)) (apply append (opcode (compile (read)) '(rax rbx rcx rdx) 'rax 0)))))
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
