(use srfi-1)
(use matchable)

(define (map-with-index proc lis)
  (map-with-index% proc 0 lis))

(define (map-with-index% proc i lis)
  (if (pair? lis)
    (cons (proc i (car lis)) (map-with-index% proc (+ i 1) (cdr lis)))
    '()))

(define (element? elem set)
  (any (cut eq? elem <>) set))

(define (append-map proc lis)
  (apply append (map proc lis)))

(define (tree-walk walker proc tree)
  (walker
    (lambda (elem)
      (if (list? elem)
        (tree-walk walker proc elem)
        (proc elem))) tree))

(define (find-tree proc tree)
  (tree-walk append-map (lambda (x) (if (proc x) (list x) (list))) tree))

(define (find-vars code class)
  (delete-duplicates! (find-tree (lambda (x) (and (pair? x) (eq? (cdr x) class))) code)))

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

(define (opcall dest)
  (norex-instruction/subcode '(#xFF) #x02 dest))

(define (p x) x)
(define (opadd dest src)
  (cond
    ((integer? src) `(,@(instruction/subcode '(#x81) #x00 dest) ,@(enc 8 4 src)))
    ((pair? dest)   (instruction '(#x01) src dest))
    (else           (instruction '(#x03) dest src))))

(define (opsub dest src)
  (cond
    ((integer? src) `(,@(instruction/subcode '(#x81) #x05 dest) ,@(enc 8 4 src)))
    ((pair? dest)   (instruction '(#x29) src dest))
    (else           (instruction '(#x2b) dest src))))

(define (label? a)
  (and (pair? a) (pair? (cdr a)) (eq? (cddr a) 'label)))

(define (opmov dest src)
  (cond
    ((label? src)   `(,@(rex-prefix 0 0) ,(logior #xB8 (regi dest)) ,src))
    ((integer? src) `(,@(instruction/subcode '(#xC7) 0 dest) ,@(enc 8 4 src)))
    ((pair? dest) (instruction '(#x89) src dest))
    (else (instruction '(#x8B) dest src))))

(define (oppush reg)
  `(,(logior #x50 (regi reg))))

(define (oppop reg)
  `(,(logior #x58 (regi reg))))

(define (oplea dest src)
  (instruction '(#x8D) dest src)) 

(define (instruction opcode reg r/m)
  (p (format "instruction ~a" opcode))
  `(,@(rex-prefix reg r/m)
    ,@opcode
    ,@(modR/M (regi reg) r/m)
    ,@(sib r/m)
    ,@(displacement r/m)))

(define (instruction/subcode opcode opcode-sub r/m)
  (p (format "instruction ~a ~a: ~a" opcode opcode-sub r/m))
  `(,@(rex-prefix #f r/m)
    ,@opcode
    ,@(modR/M opcode-sub r/m)
    ,@(sib r/m)
    ,@(displacement r/m)))

(define (norex-instruction/subcode opcode opcode-sub r/m)
  (p (format "instruction ~a ~a: ~a" opcode opcode-sub r/m))
  `(,@opcode
    ,@(modR/M opcode-sub r/m)
    ,@(sib r/m)
    ,@(displacement r/m)))

(define (rex-prefix reg place)
  (list #x48))

(define (modR/M reg place)
  (define (r/m-byte mode reg rm)
    (logior (ash mode 6) (ash reg 3) rm))
  (cond
    ; mov rax rbx
    ((not-pair? place)
      (list (r/m-byte #b11 reg (regi place))))
    ; mov rax [rax]
    ((and (eq? (cdr place) 'ref) (regi (first (car place))))
      (list (r/m-byte #b00 reg (regi (first (car place))))))
    ; mov rax [+ rax 8]
    ((and (eq? (cdr place) 'ref)
          (or (eq? (first (car place)) '+) (eq? (first (car place)) '-))
          (< #x-80 (third (car place)) #x79)) ; XXX
      (list (r/m-byte #b01 reg (regi (second (car place))))))
    ; mov rax [+ rax #x4edf]
    ((and (eq? (cdr place) 'ref)
          (or (eq? (first (car place)) '+) (eq? (first (car place)) '-))
          (< #x-80000000 (third (car place)) #x79999999)) ; XXX
      (list (r/m-byte #b10 reg (regi (second (car place))))))))

(define (sib place)
  (cond
    ((and (pair? place) (eq? (first (car place)) 'rsp)) '(#b00100100))
    ((and (pair? place) (pair? (cdr (car place))) (eq? (second (car place)) 'rsp)) '(#b00100100))
    (else '())))

(define (displacement place)
  (cond
    ((not-pair? place) '())
    ((and (eq? (cdr place) 'ref) (regi (first (car place)))) '())
    ((and (eq? (cdr place) 'ref)
          (eq? (first (car place)) '+)
          (< #x-80 (third (car place)) #x79))
       (enc 8 1 (third (car place))))
    ((and (eq? (cdr place) 'ref)
          (eq? (first (car place)) '-)
          (< #x-79 (third (car place)) #x80))
       (enc 8 1 (- (third (car place)))))
    ((and (eq? (cdr place) 'ref)
          (eq? (first (car place)) '+)
          (< #x-80000000 (third (car place)) #x79999999))
       (enc 8 4 (third (car place))))
    ((and (eq? (cdr place) 'ref)
          (eq? (first (car place)) '-)
          (< #x-79999999 (- (third (car place))) #x80000000))
       (enc 8 4 (- (third (car place)))))))

(define (immediate value)
  (cond
    ((not value) '())
    ((< #x-8000     value #x7999)     (enc 8 2 value))
    ((< #x-80000000 value #x79999999) (enc 8 4 value))
    ((< #x-8000000000000000 value #x7999999999999999) (enc 8 8 value))))

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

(define (malloc n)
  (riopadd 'rsi n))

(define (pair a b)
  (riopadd64 'rsi n)
  (rsopmov64 b 'rsi)
  (rsopmov64 a 'rsi))


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

;mov rax, rdx
(define (ropmov64)
  '(#x48 #x89 #xD0))

(define (riopadd8 r1 imm1)
  `(#x48 #x83 ,(logior #xc0 (regi r1)) ,imm1))

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
(define (opret)
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

(define add64   (func-2-2-1 sopadd64))
(define imul64  (func-2-2-1 sopimul64))
(define cmp64   (func-2-2-1 sopcmp64))
(define store64 (func-2-2-1 sopstore64))
(define read64  (cut ropread64 'rax 'rax))
(define (values2 idx) (sopmov64 'rdx idx))
(define push (cut rspush 'rax))

(define ?number? number?)
(define ?symbol? symbol?)
(define (?procedure? l) (and (pair? l) (eq? (car l) 'lambda)))
(define (?builtin? l) (and (pair? l) (eq? (car l) 'builtin)))
(define (?syntax? l)  (and (pair? l) (eq? (car l) 'syntax)))
(define (relocation-symbol value subst) `(relocation ,value ,subst))

(define (size procs)
  (map (lambda (p) (pre-code p)) procs))

(define b-lref         `(builtin lref ,lref))
(define b-push         `(builtin push ,push))
(define b-lstore       `(builtin lstore ,(cut rsopmov64 <> 'rax)))
(define b-call         `(builtin call   ,(cut ropcall64 'rax)))
(define b-rel-call     `(builtin rel-call ,roprelcall32))
(define b-if `(builtin if ,(lambda (proc1 proc2) `(,@(opje32 (relocation-symbol (lambda (i) 0) (lambda (i) ((relocation-proc -) proc1 (+ i 4))))) ,@(opjmp32 (relocation-symbol (lambda (i) 0) (lambda (i) ((relocation-proc -) proc2 (+ i 4)))))))))
(define b-frame `(builtin frame ,frame))
(define rel-call (lambda (i) `(,@(frame -8) #x48 #x89 #xC7 ,@(roprelcall32 i) ,@(leave 8))))

(define (pre-code opcode)
  (apply append (map (lambda (s)
    (match s
      (((proc . 'syntax) args ...)  (apply proc args)))) opcode)))

(define (binary-compile s base-addr)
  (let1 procs (code (compile s) base-addr)
    (disp-inst procs)
    (size procs)
    (with-output-to-string 
      (lambda ()
        (dump-procs procs)))))

(define (opcode-print procs)
   (define (p s) (for-each (lambda (i) (for-each (lambda (x) (display "  ") (display x)) i) (newline)) (opcode-filter s)))
   (for-each (lambda (x) (print "#<lambda addr=" (car x) ">") (p (cadr x)) (print "-----------------------")) procs))

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

(define (free-vars t) (free-vars% t '()))

(define (free-vars% term bound-vars)
  (cond
    ((and (pair? term) (or (eq? (car term) 'fn) (eq? (car term) 'lambda))) (free-vars% (cddr term) (cons bound-vars (cadr term))))
    ((pair? term)  (apply append (map (lambda (x) (free-vars% x bound-vars)) term)))
    (else  (if (and (not (any (cut eq? term <>) bound-vars)) (symbol? term)) (list term) '()))))

(define (heap-vars term vars)
  (cond
    ((and (pair? term) (or (eq? (car term) 'lambda) (eq? (car term) 'fn))) (filter (lambda (x) (any (cut eq? <> x) vars)) (free-vars term)))
    ((pair? term) (apply append (map (cut heap-vars <> vars) term)))
    (else '())))

(define (stack-vars heap-vars vars)
  (filter (lambda (x) (not (any (lambda (y) (eq? x y)) heap-vars))) vars))

(define (compile-to-tagged s)
  (match s
    (((or 'lambda 'fn) args body ...) (%compile-to-tagged body args '() args))
    (sexp (%compile-to-tagged (list sexp) '() '() '()))))

(define (%compile-to-tagged body args frame vars)
  (let* ((heap (heap-vars body args)) (stack (stack-vars heap args)))
    (receive (codes procs)
      (%%compile-to-tagged body heap stack frame vars)
      (cons `((lambda . syntax) ,(%%compile-to-tagged args heap stack frame vars) ,@codes) procs))))


; heap  - variables that should be allocated on heap (i.e. )
; stack - variables that could be allocated on stack.
; frame - variables refering outer frame.
(define (%%compile-to-tagged s heap stack frame vars)
  (define procs '())
  (define (loop s)
    (match s
      (((or 'lambda 'fn) args body ...)
        (let1 p (%compile-to-tagged body args frame vars)
            (set! procs (append p procs))
            `((close . syntax)
              ,(map loop (filter (lambda (x) (any (cut eq? <> x) (free-vars s))) vars))
              (,(car p) . procedure))))
      ((s ...) (map loop s))
      ((? (cut element? <> heap))  `(,s . heap))
      ((? (cut element? <> stack)) `(,s . stack))
      ((? (cut element? <> vars))  `(,s . frame))
      (s `(,s . gref))))
    (define proc (loop s))
    (values proc procs))

(define (disp-tree term)
  (cond
    ((and (pair? term) (eq? (cdr term) 'stack))  (display (format "[35m~a[m " (car term))))
    ((and (pair? term) (eq? (cdr term) 'gref))   (display (format "[36m~a[m " (car term))))
    ((and (pair? term) (eq? (cdr term) 'frame)) (display (format "[32m~a[m " (car term))))
    ((and (pair? term) (eq? (cdr term) 'syntax))   (display (format "[1;33m~a[m " (car term))))
    ((and (pair? term) (eq? (cdr term) 'builtin))   (display (format "[33m~a[m " (car term))))
    ((and (pair? term) (eq? (cdr term) 'heap))  (display (format "[34m~a[m " (car term))))
    ((and (pair? term) (eq? (cdr term) 'in))  (display (format "[1;35min~a[m " (car term))))
    ((and (pair? term) (eq? (cdr term) 'frame-in))  (display (format "[1;35mframe-in~a[m " (car term))))
    ((and (pair? term) (eq? (cdr term) 'out))  (display (format "[1;35mout~a[m " (car term))))
    ((and (pair? term) (eq? (cdr term) 'sys))  (display (format "[1;35msys~a[m " (car term))))
    ((and (pair? term) (eq? (cdr term) 'procedure))  (display "[1;36m#proc[m "))
    ((and (pair? term) (eq? (cdr term) 'ref)) (display "[") (for-each disp-tree (car term)) (display "] "))
    ((pair? term) (display "(") (for-each disp-tree term) (display ") "))
    (else (display term) (display " "))))

(define (disp-inst insts)
  (for-each 
    (lambda (x)
      (disp-tree (first x))
      (newline)
      (for-each 
        (lambda (y)
          (display "  ") (disp-tree y)
          (newline))
        (second x))
      (or (null? (cddr x)) (for-each
        (lambda (x)
          (if (number? x)
            (display (format " ~x" x))
            (display (format " ~a" x))))
        (third x)))
      (newline))
    insts))

; compile tagged to vm
(define (compile-to-vm s)
  (define (%compile-to-vm c j)
    (match c
     (('(lambda . syntax) (args ...) body ...) `(
       ,@(map-with-index (lambda (i arg) `((<- . syntax) ,arg (,i . in))) args)
       ,@(apply append (map (cut %compile-to-vm <> j) body))))
     ((`(close . syntax) (args ...) body) `(
       ((close . syntax) ,args ,body)))
     (('(set! . gref) sym arg) `(
      ,@(%compile-to-vm arg (+ j 1))
      ((<- . syntax) ,sym (0 . out))))
     ((proc args ...) `(
       ,@(apply append (map-with-index (lambda (i arg) `(
           ,@(%compile-to-vm arg (+ j 1))
           ((<- . syntax) ((,j ,i) . stack) (0 . out))))
         args))
       ,@(%compile-to-vm proc (+ j 1))
       ,@(apply append (map-with-index (lambda (i arg) `(
           ((<- . syntax) (,i . in) ((,j ,i) . stack))))
         args))
       ((call . syntax) (0 . out))))
     (sym `(((<- . syntax) (0 . out) ,sym)))))
   (list s (%compile-to-vm s 0)))

(define (ssa c)
  (define lis '(()))
  (define (inc var)
    (if (assoc var lis)
      (set! (cadr (assoc var lis)) (+ 1 (cadr (assoc var lis))))
      (push! lis (list var 1))))
  (define (ref var)
    (list-copy (or (assoc var lis) (list var 0))))
  (define (loop c)
    (if (null? c) '()
    (match (car c)
      (('(assign . syntax) out in)
        (let1 in (ref in)
        (inc out)
       `(((assign . syntax) ,(ref out) ,in) ,@(loop (cdr c)))))
      ((`(close . syntax) arg ... )
       `(((close . syntax)  ,@arg) ,@(loop (cdr c))))
      ((`(call . syntax) arg)
        (let1 a (ref arg)
        (inc '(0 . out))
       `(((assign . syntax) ,(ref arg) ((call . syntax) ,a)) ,@(loop (cdr c))))))))
  (loop c))

(define (optimize c)
  (define lis '(()))
  (define lis2 '(()))
  (define (ref var)
    (if (assoc var lis) (cdr (assoc var lis)) var))
  (define (loop c)
    (if (null? c) '()
      (match (car c)
        (('(assign . syntax) out ('(call . syntax) arg))
           `(((call . syntax) ,(ref arg)) ,@(loop (cdr c))))
        (('(assign . syntax) out in)
           (push! lis (cons out (ref in)))
           `(((assign . syntax) ,out ,(ref in)) ,@(loop (cdr c))))
        (((proc . 'builtin) in ...)
           `(((,proc . builtin) ,@(map (cut ref <>) in)) ,@(loop (cdr c))))
        (('(call . syntax) in ...)
           `(((call . syntax) ,@(map (cut ref <>) in)) ,@(loop (cdr c))))
        (('(close . syntax) in ...)
           `(((close . syntax) ,@in) ,@(loop (cdr c)))))))

  (define (loop4 c)
    (if (null? c) '()
      (match (car c)
        ('((call . syntax) ((+ . gref) 0))
          `(((add . builtin) ((0 . out) #f) ((0 . in) #f) ((1 . in) #f)) ,@(loop4 (cdr c))))
        ('((call . syntax) ((* . gref) 0))
          `(((mul . builtin) ((0 . out) #f) ((0 . in) #f) ((1 . in) #f)) ,@(loop4 (cdr c))))
        (s (cons s (loop4 (cdr c)))))))

  (define (loop5 c)
    (if (null? c) '()
       `((,(caar c) ,@(map (cut car <>) (cdar c))) ,@(loop5 (cdr c)))))
   

  (define (loop2 c)
    (if (null? c) '()
      (match (car c)
        (('(assign . syntax) out in)
          (set! lis2 (remove! (cut eq? in <>) lis2))
          (push! lis2 out)
          (loop2 (cdr c)))
        (((proc . 'builtin) in ...)
          (for-each (lambda (i) (set! lis2 (remove! (cut eq? i <>) lis2))) in)
          (loop2 (cdr c)))
        (('(call . syntax) in)
          (set! lis2 (remove! (cut eq? in <>) lis2))
          (loop2 (cdr c))))))

  (define (loop3 c)
    (if (null? c) '()
    (match (car c)
      (('(assign . syntax) out in)
        (if (and (or (eq? (cdar out) 'out) (eq? (cdar out) 'stack)) (any (cut eq? <> out) lis2))
            (loop3 (cdr c))
            `(((assign . syntax) ,out ,in) ,@(loop3 (cdr c)))))
      (('(call . syntax) in) `(((call . syntax) ,in) ,@(loop3 (cdr c)))))))
  
  (define (loop6 c)
    (if (null? c) '()
    (match (car c)
      (('(assign . syntax) out in)
        (if (and (or (eq? (cdr out) 'in) (eq? (cdr out) 'out)) (any (cut eq? <> out) lis2))
            (loop6 (cdr c))
            `(((assign . syntax) ,out ,in) ,@(loop6 (cdr c)))))
      (('(call . syntax) in) `(((call . syntax) ,in) ,@(loop6 (cdr c))))
      (((proc . 'builtin) in ...) `(((,proc . builtin) ,@in) ,@(loop6 (cdr c)))))))

   (define ssa-p (loop c))
   (define a #f)
   ssa-p
   ;(loop2 ssa-p)
   ;(set! a (loop (loop5 (loop4 (loop3 ssa-p)))))
   ;(loop2 a)
   ;(loop6 a))
)


(define size-of-ptr 8)

(define (compile-to-assembly c)
  (define (instruction-to-assembly c)
    (match c
      (('(close . syntax) vars body) `(
         ((add . builtin) (1 . sys) ,(* size-of-ptr (+ 1 (length vars))))
         ((mov  . builtin)  (0 . out) (,(car body) (0 0 0 0 0 0 0 0) . label))
         ((mov  . builtin)  (((1 . sys)) . ref) (0 . out))
         ,@(apply append (map-with-index (lambda (i x) `(
             ((lea . builtin) (0 . out) ,x)
             ((mov . builtin) ((- (1 . sys) ,(* size-of-ptr (+ i 1))) . ref) (0 . out))))
           vars))
         ((mov  . builtin) (0 . out) (1 . sys))))
      (('(<- . syntax) dest src) `(((mov . builtin) ,dest ,src)))
      (('(call . syntax) proc) `(
         ((mov . builtin) (0 . frame-in) ((- ,proc ,(* size-of-ptr 1)) . ref))
         ((mov . builtin) (1 . frame-in) ((- ,proc ,(* size-of-ptr 2)) . ref))
         ((call . builtin) ((,proc) . ref))))
      (x (list x))))
  (let ((s (find-vars c 'stack)) (h (find-vars c 'heap)) (f (find-vars c 'frame)))
    (define (rep x)
      (if (pair? x)
        (case (cdr x)
          ((stack)  `((+ (0 . sys) ,(* size-of-ptr (list-index (cut equal? <> x) s))) . ref))
          ((heap)   `((- (2 . sys) ,(* size-of-ptr (list-index (cut equal? <> x) h))) . ref))
          ((frame)  `(((,(list-index (cut equal? <> x) f) . frame-in)) . ref))
          (else x)) x))
    (list (first c)
    `(,@(if (= 0 (length h)) '()
       `(((push . builtin) (2 . sys))
         ((add . builtin)  (1 . sys) ,(* size-of-ptr (length h)))
         ((mov . builtin)  (2 . sys) (1 . sys))))
      ((sub . builtin)  (0 . sys) ,(* size-of-ptr (length s)))
      ,@(tree-walk map rep (append-map instruction-to-assembly (second c)))
      ((add . builtin)  (0 . sys) ,(* size-of-ptr (length s)))
      ,@(if (= 0 (length h)) '()
          '(((pop . builtin)  (2 . sys))))
      ((ret . builtin))))))

(define (render-register c)
  (list (first c)
    (let loop ((a (second c)))
    (tree-walk map (lambda (x)
      (match x
        ((index . 'out) (list-ref '(rax rcx rbx rbp) index))
        ((index . 'in)  (list-ref '(rbp rbx rsi rdx) index))
        ((index . 'frame-in) (list-ref '(rsi rdx) index))
        ((index . 'sys) (list-ref '(rsp rdi rbp) index))
        (((? number? x) . 'gref) x)
        ((x . y) (cons (car (loop (list x))) y))
        (x x))) a))))

(define (compile-to-binary c)
  (list (first c) (second c)
    (append-map
      (lambda (i)
        (let ((x (car i)) (args (cdr i)))
          (case (car x)
            ((mov)  (apply opmov args))
            ((add)  (apply opadd args))
            ((sub)  (apply opsub args))
            ((call) (apply opcall args))
            ((pop)  (apply oppop args))
            ((push) (apply oppush args))
            ((ret)  (apply opret args))
            ((lea)  (apply oplea args))
            (else i)))) (second c))))

(define (link-label base codes)
  (define table '())
  (define x 0)
  (for-each (lambda (code)
    (push! table (cons (first code) x))
    (let loop ((binary (third code)))
      (for-each (lambda (b)
        (if (label? b)
          (loop (second b))
          (set! x (+ x 1)))) binary))) codes)
  (append-map (lambda (code)
    (append-map (lambda (b)
       (if (label? b)
          (enc 8 8 (+ base (cdr (assoc (first b) table))))
          (list b))) (third code))) codes))

(define (binary-compile s base)
  (define c (map compile-to-binary (map render-register (map compile-to-assembly (map compile-to-vm (compile-to-tagged s))))))
  (disp-inst c)
  (list->string  (map integer->char (link-label base c))))

(define (read-compile base)
  (display "test> ")
  (binary-compile (read) base))

(return-to-host)
