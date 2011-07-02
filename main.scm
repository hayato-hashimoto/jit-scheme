(use srfi-1)
(use srfi-4)
(use srfi-38)
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

(define (p x) (disp-tree x)(newline) x)
(define (px x) x)
(define (opadd dest src)
  (cond
    ((integer? src) `(,@(instruction/subcode '(#x81) #x00 dest) ,@(enc 8 4 src)))
    ((pair? dest)   (instruction '(#x01) src dest))
    (else           (instruction '(#x03) dest src))))

(define (opcmp dest src)
  (cond
    ((integer? src) `(,@(instruction/subcode '(#x81) #x07 dest) ,@(enc 8 4 src)))
    ((pair? dest)   (instruction '(#x39) src dest))
    (else           (instruction '(#x3B) dest src))))

(define (opsub dest src)
  (cond
    ((integer? src) `(,@(instruction/subcode '(#x81) #x05 dest) ,@(enc 8 4 src)))
    ((pair? dest)   (instruction '(#x29) src dest))
    (else           (instruction '(#x2b) dest src))))

(define (label-symbol? a)
  (and (pair? a) (pair? (cdr a)) (eq? (cddr a) 'label)))
(define (rel-label-symbol? a)
  (and (pair? a) (pair? (cdr a)) (eq? (cddr a) 'rel-label)))
(define (label? a)
  (and (pair? a) (pair? (car a)) (eq? (caar a) 'label)))

(define (opmov dest src)
  (cond
    ((label-symbol? src)   `(,@(rex-prefix 0 0) ,(logior #xB8 (regi dest)) ,src))
    ((and (integer? src) (< #x-80000000 src #x79999999)) `(,@(instruction/subcode '(#xC7) 0 dest) ,@(enc 8 4 src)))
    ((integer? src) `(,@(rex-prefix 0 0) ,(logior #xB8 (regi dest)) ,@(enc 8 8 src)))
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

; return
(define (opret)
  '(#xc3))

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
    (((or 'lambda 'fn) args body ...) 
       (%compile-to-tagged body args '() args))))
  ;  (sexp (%compile-to-tagged (list sexp) '() '() '()))))

(define (%compile-to-tagged body args frame vars)
  (let* ((heap (heap-vars body args)) (stack (stack-vars heap args)))
    (receive (codes procs)
      (%%compile-to-tagged body heap stack frame vars)
      (cons `((lambda . syntax) ,(%%compile-to-tagged args heap stack frame vars) ,@codes) procs))))


; heap  - variables that should be allocated on heap (i.e. variables to be captured in other lambdas)
; stack - variables that could be allocated on stack.
; frame - variables refering outer frame.
(define (%%compile-to-tagged s heap stack frame vars)
  (p vars)
  (define procs '())
  (define (loop s)
    (match s
      (((or 'lambda 'fn) args body ...)
        (let1 p (%compile-to-tagged body args frame (append vars args))
            (set! procs (append p procs))
            `((close . syntax)
              ,(map loop (filter (lambda (x) (any (cut eq? <> x) (free-vars s))) vars))
              (,(car p) . procedure))))
      (('if p t f) `((if . syntax) ,(loop p) ,(loop t) ,(loop f)))
      ((s ...) (map loop s))
      ((? (cut element? <> heap))  `(,s . heap))
      ((? (cut element? <> stack)) `(,s . stack))
      ((? (cut element? <> vars))  `(,s . frame))
      ((? number? x) x)
      ((? lookup) (lookup s))
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
    ((and (pair? term) (not (pair? (cdr term))) (not (null? (cdr term)))) (display "(") (disp-tree (car term)) (display ". ") (disp-tree (cdr term)) (display ") "))
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
  (define (%compile-to-vm-pred p)
    (case (caar p)
       ((>) 'g)
       ((=) 'z)))
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
     (('(read . gref) arg1) `(
       ,@(%compile-to-vm arg1 (+ j 1))
       ((<- . syntax) (0 . out) (((0 . out)) . ref))))
     (('(+ . gref) arg1 arg2) `(
       ,@(%compile-to-vm arg1 (+ j 1))
       ((<- . syntax) ((,j 0) . stack) (0 . out))
       ,@(%compile-to-vm arg2 (+ j 1))
       ((add . builtin) (0 . out) ((,j 0) . stack))))
     (('(- . gref) arg1 arg2) `(
       ,@(%compile-to-vm arg2 (+ j 1))
       ((<- . syntax) ((,j 0) . stack) (0 . out))
       ,@(%compile-to-vm arg1 (+ j 1))
       ((sub . builtin) (0 . out) ((,j 0) . stack))))
     (('(> . gref) arg1 arg2) `(
       ,@(%compile-to-vm arg2 (+ j 1))
       ((<- . syntax) ((,j 0) . stack) (0 . out))
       ,@(%compile-to-vm arg1 (+ j 1))
       ((cmp . builtin) (0 . out) ((,j 0) . stack))))
     (('(= . gref) arg1 arg2) `(
       ,@(%compile-to-vm arg2 (+ j 1))
       ((<- . syntax) ((,j 0) . stack) (0 . out))
       ,@(%compile-to-vm arg1 (+ j 1))
       ((cmp . builtin) (0 . out) ((,j 0) . stack))))
     (('(if . syntax) pred true-case false-case) `(
        ,@(%compile-to-vm pred j)
        (,(case (%compile-to-vm-pred pred)
            ((z) '(jnz . builtin))
            ((g) '(jng . builtin))) (,(lambda (table self base) (- (px (cdr (assq 1 table))) (p self))) (0 0 0 0) . rel-label))
        ,@(%compile-to-vm true-case j)
        ((jmp . builtin) (,(lambda (table self base) (- (px (cdr (assq 2 table))) (p self))) (0 0 0 0) . rel-label))
        ((label . builtin) 1)
        ,@(%compile-to-vm false-case j)
        ((label . builtin) 2)))
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
        ((index . 'out) (list-ref '(rax rdx) index))
        ((index . 'in)  (list-ref '(rbp rbx rsi rdx) index))
        ((index . 'frame-in) (list-ref '(rsi rdx) index))
        ((index . 'sys) (list-ref '(rsp rdi rcx) index))
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
            ((cmp)  (apply opcmp args))
            ((jnz) `(#x0F #x85 ,@args))
            ((jng) `(#x0F #x8E ,@args))
            ((jmp) `(#xE9 ,@args))
            (else (list i))))) (second c))))

(define (link-label base codes)
  (define table '())
  (define x 0)
  (for-each (lambda (code)
    (push! table (cons (first code) x))
    (let loop ((binary (third code)))
      (for-each (lambda (b)
        (cond 
          ((label-symbol? b)     (loop (second b)))
          ((rel-label-symbol? b) (loop (second b)))
          ((label? b) (push! table (cons (second b) x)))
          (else (set! x (+ x 1))))) binary))) codes)
  (set! x 0)
  (append-map (lambda (code)
    (append-map (lambda (b)
       (cond
          ((label-symbol? b)     (set! x (+ x (length (second b)))) ;XXX
                                 (enc 8 (length (second b)) (p (+ base (p (cdr (assoc (first b) (p table))))))))
          ((rel-label-symbol? b) (set! x (+ x (length (second b))))
                                 (enc 8 (length (second b)) ((first b) table x base)))
          ((label? b) '())
          (else (set! x (+ x 1)) (list b)))) (third code))) codes))

(define (setup-user-namespace)
  (eval '(define builtin-namespace (current-namespace)))
  (eval '(define meta-namespace    (make-namespace)))
  (eval '(import-namespace-to meta-namespace builtin-namespace))
  (eval '(select-namespace meta-namespace))
  (eval '(define meta-namespace (current-namespace)))
  (eval '(define user (make-namespace)))
  (eval '(import-namespace-to user builtin-namespace))
  (eval '(import-namespace-to user meta-namespace))
  (eval '(select-namespace user))
  (eval '(define namespace-name "user")))
 
(define (compile-and-eval expr)
  (define c (map compile-to-binary (map render-register (map compile-to-assembly (map compile-to-vm (p (compile-to-tagged expr)))))))
  (disp-inst c)
  (define b (list->u8vector (link-label (binary-address) c)))
  (define a ((foreign-lambda unsigned-long "exec_binary" integer u8vector) (u8vector-length b) b))
  (define r 
  (+ (ash (px (modulo ((foreign-lambda integer "fetch_result_higher32")) #x100000000)) 32)
          (px (modulo ((foreign-lambda integer "fetch_result_lower32"))  #x100000000))))
  r)


; Due to poor support of 64-bit variables in chicken scheme ...
(define (binary-address)
  (+ (ash (modulo ((foreign-lambda integer "binary_address_higher32")) #x100000000) 32)
     (modulo ((foreign-lambda integer "binary_address_lower32"))  #x100000000)))

(define (repl)
  (define expr   #f)
  (define result #f)
  (display "test> ")
  (set! expr (read))
  (set! result (eval expr))
  (cond
    ((number? result) (display (format " => ~a (0x~x)\n" result result)))
    ((pair? result) (display (format " => ~a\n" (with-output-to-string (lambda () (write/ss result))))))
    (else          (display (format " => ~a\n" result))))
  (repl))

(define default-namespace
  '(((define               . (define . syntax))
     (make-namespace       . (make-namespace . meta))
     (select-namespace     . (select-namespace . meta))
     (import-namespace-to  . (import-namespace-to . meta))
     (current-namespace    . (current-namespace . meta))
     (exit                 . (exit . meta)))))

(define (assq-ref lis sym false-case)
  (or (and (assq sym lis) (cdr (assq sym lis))) false-case))
(define (assq-ref-list lis sym)
  (or (and (assq sym lis) (list (cdr (assq sym lis)))) '()))

(define (lookup sym)
  (car* (append-map (cut assq-ref-list <> sym) current-namespace)))

(define (car* maybe-lis)
  (if (null? maybe-lis) #f (car maybe-lis)))

(define (eval expr)
  (cond 
    ((pair? expr) (match (eval (car expr))
      ('(define . syntax) (set-cdr! (last-pair (car current-namespace)) (list (cons (cadr expr) (eval (caddr expr))))))
      (`(,meta  . meta)   (apply (assq-ref meta-procs meta #f) (map eval (cdr expr))))
      (else (compile-and-eval `(lambda () ,expr)))))
    ((symbol? expr) (lookup expr))
    (else expr)))

(define current-namespace default-namespace)

(define (get-current-namespace) current-namespace)
(define (select-namespace namespace) (set! current-namespace namespace))
(define (make-namespace) (list (list (cons 1 1))))
(define (import-namespace-to dest src)
  (set-cdr! (last-pair dest) (list (car src))))

(define meta-procs
  `((make-namespace      . ,make-namespace)
    (select-namespace    . ,select-namespace)
    (current-namespace   . ,get-current-namespace)
    (import-namespace-to . ,import-namespace-to)
    (exit                . ,exit)))

(setup-user-namespace)
(repl)
