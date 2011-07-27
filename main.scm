(use srfi-1)
(use srfi-4)
(use srfi-38)
(use matchable)
(use lolevel)

(define type '(! type))
(define type-namespace `(
  (int_62    . (,type int_62))
  (cons      . (,type cons))
  (procedure . (,type procedure))
  (object    . (,type object))))

(define tag-namespace `(
  (0 . ,(cdr (assq 'int_62    type-namespace)))
  (1 . ,(cdr (assq 'cons      type-namespace)))
  (2 . ,(cdr (assq 'procedure type-namespace)))
  (3 . ,(cdr (assq 'object    type-namespace)))))

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

(define genid
  (let1 counter 0
    (lambda () 
      (set! counter (+ 1 counter))
       counter)))

(define (pa x) (display "a:") (print x) x)
(define (pb x) (display "b:") (print x) x)
(define (p x) x)
(define (p-i x) x)
(define (px x) x)
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
      ((? number? x) (fixnum x))
      ((? lookup) (lookup s))
      (s `(,s . gref))))
    (define proc (loop s))
    (values proc procs))

(define (fixnum x) x)

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
  (define (%compile-to-vm-pred a)
    (p a)
    (cond
      ((and (pair? a) (eq? builtin-gt (car a))) 'g)
      ((and (pair? a) (eq? builtin-eq (car a))) 'z)
      (else 'cmpz)))
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
     (('(builtin read) arg1) `(
       ,@(%compile-to-vm arg1 (+ j 1))
       ((<- . syntax) (0 . tmp) #xfffffffffffff)
       (and (0 . out) (0 . tmp))
       ((<- . syntax) (0 . out) ((0 . out)))))
     (('(builtin write) arg1 arg2) `(
       ,@(%compile-to-vm arg2 (+ j 1))
       ((<- . syntax) ((,j 0) . stack) (0 . out))
       ,@(%compile-to-vm arg1 (+ j 1))
       ((<- . syntax) (1 . out) ((,j 0) . stack))
       ((<- . syntax) (0 . tmp) #xfffffffffffff)
       (and (0 . out) (0 . tmp))
       ((<- . syntax) ((0 . out)) (1 . out))))
     (('(builtin cons) arg1 arg2) `(
       ,@(%compile-to-vm arg1 (+ j 1))
       ((<- . syntax) ((,j 0) . stack) (0 . out))
       ,@(%compile-to-vm arg2 (+ j 1))
       (cons ((,j 0) . stack) (0 . out))))
     (('(builtin tri) arg1 arg2 arg3) `(
       ,@(%compile-to-vm arg1 (+ j 1))
       ((<- . syntax) ((,j 0) . stack) (0 . out))
       ,@(%compile-to-vm arg2 (+ j 1))
       ((<- . syntax) ((,j 1) . stack) (0 . out))
       ,@(%compile-to-vm arg3 (+ j 1))
       (tri ((,j 0) . stack) ((,j 1) . stack) (0 . out))))
     (('(builtin cons) arg1 arg2) `(
       ,@(%compile-to-vm arg1 (+ j 1))
       ((<- . syntax) ((,j 0) . stack) (0 . out))
       ,@(%compile-to-vm arg2 (+ j 1))
       (cons ((,j 0) . stack) (0 . out))))
     (('(builtin tri) arg1 arg2 arg3) `(
       ,@(%compile-to-vm arg1 (+ j 1))
       ((<- . syntax) ((,j 0) . stack) (0 . out))
       ,@(%compile-to-vm arg2 (+ j 1))
       ((<- . syntax) ((,j 1) . stack) (0 . out))
       ,@(%compile-to-vm arg3 (+ j 1))
       (tri ((,j 0) . stack) ((,j 1) . stack) (0 . out))))
     (('(builtin +) arg1 arg2) `(
       ,@(%compile-to-vm arg1 (+ j 1))
       ((<- . syntax) ((,j 0) . stack) (0 . out))
       ,@(%compile-to-vm arg2 (+ j 1))
       (add (0 . out) ((,j 0) . stack))))
     (('(builtin *) arg1 arg2) `(
       ,@(%compile-to-vm arg1 (+ j 1))
       ((<- . syntax) ((,j 0) . stack) (0 . out))
       ,@(%compile-to-vm arg2 (+ j 1))
       (imul (0 . out) ((,j 0) . stack))))
     (('(builtin -) arg1 arg2) `(
       ,@(%compile-to-vm arg2 (+ j 1))
       ((<- . syntax) ((,j 0) . stack) (0 . out))
       ,@(%compile-to-vm arg1 (+ j 1))
       (sub (0 . out) ((,j 0) . stack))))
     (('(builtin and) arg1 arg2) `(
       ,@(%compile-to-vm arg2 (+ j 1))
       ((<- . syntax) ((,j 0) . stack) (0 . out))
       ,@(%compile-to-vm arg1 (+ j 1))
       (and (0 . out) ((,j 0) . stack))))
     (('(builtin ior) arg1 arg2) `(
       ,@(%compile-to-vm arg2 (+ j 1))
       ((<- . syntax) ((,j 0) . stack) (0 . out))
       ,@(%compile-to-vm arg1 (+ j 1))
       (ior (0 . out) ((,j 0) . stack))))
     (('(builtin >) arg1 arg2) `(
       ,@(%compile-to-vm arg2 (+ j 1))
       ((<- . syntax) ((,j 0) . stack) (0 . out))
       ,@(%compile-to-vm arg1 (+ j 1))
       (cmp (0 . out) ((,j 0) . stack))))
     (('(builtin =) arg1 arg2) `(
       ,@(%compile-to-vm arg2 (+ j 1))
       ((<- . syntax) ((,j 0) . stack) (0 . out))
       ,@(%compile-to-vm arg1 (+ j 1))
       (cmp (0 . out) ((,j 0) . stack))))
     (('(if . syntax) pred true-case false-case) (let* ((i (genid)) (ii (genid))) `(
        ,@(%compile-to-vm pred j)
        ,@(let1 label1 `(rel-label-ref ,(lambda (table self base) (- (px (cdr (assq i table))) (p self))) (0 0 0 0))
          (case (%compile-to-vm-pred pred)
            ((cmpz) `((cmp (0 . out) 0) (jnz ,label1)))
            ((z)    `((jnz ,label1)))
            ((g)    `((jng ,label1)))))
        ,@(%compile-to-vm true-case j)
        (jmp (rel-label-ref ,(lambda (table self base) (- (px (cdr (assq ii table))) (p self))) (0 0 0 0)))
        (label ,i)
        ,@(%compile-to-vm false-case j)
        (label ,ii))))
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
           `((,proc ,@(map (cut ref <>) in)) ,@(loop (cdr c))))
        (('(call . syntax) in ...)
           `(((call . syntax) ,@(map (cut ref <>) in)) ,@(loop (cdr c))))
        (('(close . syntax) in ...)
           `(((close . syntax) ,@in) ,@(loop (cdr c)))))))

  (define (loop4 c)
    (if (null? c) '()
      (match (car c)
        ('((call . syntax) ((+ . gref) 0))
          `((add ((0 . out) #f) ((0 . in) #f) ((1 . in) #f)) ,@(loop4 (cdr c))))
        ('((call . syntax) ((* . gref) 0))
          `((mul ((0 . out) #f) ((0 . in) #f) ((1 . in) #f)) ,@(loop4 (cdr c))))
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
      (((proc . 'builtin) in ...) `((,proc ,@in) ,@(loop6 (cdr c)))))))

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
         (add (1 . sys) ,(* size-of-ptr (+ 1 (length vars))))
         (mov (0 . out) (label-ref (,(car body) . proc) (0 0 0 0 0 0 0 0)))
         (mov ((1 . sys)) (0 . out))
         ,@(apply append (map-with-index (lambda (i x) `(
             (lea (0 . out) ,x)
             (mov (- (1 . sys) ,(* size-of-ptr (+ i 1))) (0 . out))))
           vars))
         (mov  (0 . out) (1 . sys))))
      (('cons arg1 arg2) `(
; allocate
         (add (1 . sys) ,(* size-of-ptr 2))
         (mov ((1 . sys)) ,arg2)
         (mov (0 . out) ,arg1)
         (mov (- (1 . sys) ,size-of-ptr) (0 . out))
; return
         (mov (0 . out) (1 . sys))))
      (('tri arg1 arg2 arg3) `(
; allocate
         (add (1 . sys) ,(* size-of-ptr 3))
         (mov (0 . out) ,arg3)
         (mov ((1 . sys)) (0 . out))
         (mov (0 . out) ,arg2)
         (mov (- (1 . sys) ,size-of-ptr) (0 . out))
         (mov (0 . out) ,arg1)
         (mov (- (1 . sys) ,(* 2 size-of-ptr)) (0 . out))
; return
         (mov (0 . out) (1 . sys))))
      (('(<- . syntax) dest src) `((mov ,dest ,src)))
      (('(call . syntax) proc) `(
         (push (0 . frame-in))
         (mov  (0 . frame-in) (- ,proc ,(* size-of-ptr 1)))
         ;(mov (1 . frame-in) (- ,proc ,(* size-of-ptr 2)) )
         (call (,proc))
         (pop  (0 . frame-in))))
      (x (list x))))
  (let1 asm (append-map instruction-to-assembly (second c))
  (let ((s (find-vars asm 'stack)) (h (find-vars asm 'heap)) (f (find-vars asm 'frame)))
    (define (rep x)
      (if (pair? x)
        (case (cdr x)
          ((stack) `(+ (0 . sys) ,(* size-of-ptr (list-index (cut equal? <> x) s))))
          ((heap)  `(- (2 . sys) ,(* size-of-ptr (list-index (cut equal? <> x) h))))
          ((frame) `((,(list-index (cut equal? <> x) f) . frame-in)))
          (else x))
        x))
    (list (first c)
    `(,@(if (= 0 (length h)) '()
       `((push (2 . sys))
         (add  (1 . sys) ,(* size-of-ptr (length h)))
         (mov  (2 . sys) (1 . sys))))
      (sub  (0 . sys) ,(* size-of-ptr (length s)))
      ,@(tree-walk map rep (append-map instruction-to-assembly (second c)))
      (add  (0 . sys) ,(* size-of-ptr (length s)))
      ,@(if (= 0 (length h)) '()
          '((pop  (2 . sys))))
      (ret))))))

(define (render-register c)
  (list (first c)
    (let loop ((a (second c)))
    (tree-walk map (lambda (x)
      (match x
        ((index . 'out) (list-ref '(rax rsi rdx rcx r8 r9) index))
        ((index . 'in)  (list-ref '(rdi rsi rdx rcx r8 r9) index))
        ((index . 'frame-in) (list-ref '(r15 r14 r13) index))
        ((index . 'sys) (list-ref '(rsp rbx rbp) index))
        ((index . 'tmp) (list-ref '(r10 r11 r12) index))
        ((x . y) (cons (car (loop (list x))) y))
        (x x))) a))))

(define (link-label base codes)
  (define table '())
  (define x 0)
  (for-each (lambda (code)
    (push! table (cons (cons (first code) 'proc)  x))
    (let loop ((binary (second code)))
      (for-each (lambda (b)
        (cond 
          ((label-ref? b)     (loop (third b)))
          ((rel-label-ref? b) (loop (third b)))
          ((label? b)         (push! table (cons (second b) x)))
          (else (set! x (+ x 1))))) binary))) codes)
  (set! x 0)
  (append-map (lambda (code)
    (append-map (lambda (b)
       (cond
          ((label-ref? b)      (set! x (+ x (length (third b)))) ;XXX
                               (enc 8 (length (third b)) (p (+ base (p (cdr (assoc (second b) (p table))))))))
          ((rel-label-ref? b)  (set! x (+ x (length (third b))))
                               (enc 8 (length (third b)) ((second b) table x base)))
          ((label? b) '())
          (else (set! x (+ x 1)) (list b)))) (second code))) codes))

(define (map-second proc lis)
  (map (lambda (x) (cons (car x) (cons (proc (cadr x)) (cddr x)))) lis))

(define (compile-and-eval expr)
  (define c (map-second (cut append-map compile-to-binary <>) (p-i (map render-register (map compile-to-assembly (p-i (map compile-to-vm (compile-to-tagged expr))))))))
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
  (display "[1;35mscheme>[m ")
  (set! expr (read))
  (set! result (eval* expr))
  (if (integer? result) (set! result (decode-value result)))
  (cond
    ((number? result) (display (format " => ~a (0x~x)\n" result result)))
    ((any (cut eq? <> result) namespaces) (display (format " => #<namespace>\n")))
    ((pair? result) (display (format " => ~a\n" (with-output-to-string (lambda () (write/ss result))))))
    (else          (display (format " => ~a\n" result))))
  (repl))


(define (add-to-namespace name value)
  (push! (car current-namespace) (cons name value)))
(define (syntax-define name tree)
  (add-to-namespace name (eval* tree)))
(define (get-current-namespace) current-namespace)
(define (select-namespace namespace) (set! current-namespace namespace))
(define namespaces '())
(define (make-namespace)
  (let1 ns (list (list)) 
    (push! namespaces ns)
  ns))
(define (import-namespace-to dest src)
  (set-cdr! (last-pair dest) (list (car src))))
(define current-namespace (make-namespace))
(define (export-to-namespace alist name)
  (let1 ns (list alist)
    (push! namespaces ns)
    (add-to-namespace name ns)
    ns))
(define (dummy) (foreign-code " return 0;}
#include <dlfcn.h>
static C_word foo () {
"))
(define dlerror (foreign-lambda c-string "dlerror"))
(define (dlopen file) ((foreign-lambda c-pointer "dlopen" c-string integer) file (foreign-code "return C_fix(RTLD_LAZY);")))
(define (dlsym handle str)
  (define return (allocate 8))
  (define-external x c-pointer)
  (set! x ((foreign-lambda c-pointer "dlsym" c-pointer c-string) handle str))
  ((foreign-lambda void "assign" c-pointer c-pointer) return x)
  (pointer->address return))

(define-external (system_print (integer n)) void (print n))
(define dummy2  (foreign-code " return 0;}
#include \"jit.h\"
static C_word bar () {
"))

(define system-print (foreign-code "return C_fix((long) system_print);"))
(print system_print) 
(print system-print) 
(define (export-proc proc)
  (define return (allocate 8))
  ((foreign-lambda void "assign" c-pointer int) return proc)
  (pointer->address return))

(define (make-macro x) `(,macro ,(lambda args (eval* (cons x (encode args))))))
(define (define-to-sys-define name value)
    (if (pair? name)
      `(,sys-define ,(car name) (fn ,(cdr name) ,value))
      `(,sys-define ,name ,value)))

(define system '(! system))
(define (system? a) (and (pair? a) (eq? (car a) system)))
(define sys-define              `(,system define))
(define sys-make-namespace      `(,system ,make-namespace))
(define sys-select-namespace    `(,system ,select-namespace))
(define sys-import-namespace-to `(,system ,import-namespace-to))
(define sys-current-namespace   `(,system ,get-current-namespace))
(define sys-exit                `(,system ,exit))
(define sys-dlopen              `(,system ,dlopen))
(define sys-dlsym               `(,system ,dlsym))
(define sys-macro               `(,system ,make-macro))
(define sys-display             `(,system ,display))
(define sys-eval                `(,system eval))

(define builtin-add         '(builtin +))
(define builtin-sub         '(builtin -))
(define builtin-imul        '(builtin *))
(define builtin-div         '(builtin /))
(define builtin-eq          '(builtin =))
(define builtin-gt          '(builtin >))
(define builtin-and         '(builtin and))
(define builtin-ior         '(builtin ior))
(define builtin-read        '(builtin read))
(define builtin-write       '(builtin write))
(define builtin-cons        '(builtin cons))
(define builtin-tri         '(builtin tri))

(define macro '(! macro))
(define (macro? a) (and (pair? a) (eq? (car a) macro)))
(define macro-define `(,macro ,define-to-sys-define))

(define namespace-function-namespace
  `((internal-define      . ,sys-define)
    (make-namespace       . ,sys-make-namespace)
    (select-namespace     . ,sys-select-namespace)
    (import-namespace-to  . ,sys-import-namespace-to)
    (current-namespace    . ,sys-current-namespace)))

(define internal-namespace
   `((internal-eval . ,sys-eval)
     (dlopen        . ,sys-dlopen)
     (dlsym         . ,sys-dlsym)
     (display       . ,sys-display)
     (print         . ,(export-proc system-print))
     (exit          . ,sys-exit )))

(define arithmetic-namespace
  `((+ . ,builtin-add) 
    (- . ,builtin-sub) 
    (* . ,builtin-imul) 
    (/ . ,builtin-div)
    (and . ,builtin-and) 
    (ior . ,builtin-ior) 
    (= . ,builtin-eq) 
    (> . ,builtin-gt)))

(define machine-namespace
  `((read  . ,builtin-read)
    (write . ,builtin-write)))

(define builtin-namespace
  `((pair . ,builtin-cons)
    (tri  . ,builtin-tri)))

(define macro-namespace 
  `((make-macro  . ,sys-macro)
    (define . ,macro-define)))

(define (assq-ref lis sym false-case)
  (or (and (assq sym lis) (cdr (assq sym lis))) false-case))
(define (assq-ref-list lis sym)
  (or (and (assq sym lis) (list (cdr (assq sym lis)))) '()))
(define (lookup sym)
  (define (car* maybe-lis) (if (null? maybe-lis) #f (car maybe-lis)))
  (car* (append-map (cut assq-ref-list <> sym) current-namespace)))

(define external-table '())
(define (encode x)
  (let ((res
    (cond
      ((pair? x)
        (let ((m (allocate 16)))
          (assign m              (external (car x)))
          (assign (pointer+ m 8) (external (cdr x)))
          (address->number m)))
      ((number? x) x))))
    (push! external-table (cons x res))
    res))

(define (eval* expr)
  (cond 
    ((system? expr) expr)
    ((macro? expr)  expr)
    ((pair? expr) 
      (let1 f (eval* (car expr))
        (cond
          ((assq f system-syntax)    => (lambda (x) (apply (cdr x) (cdr expr))))
          ((system? f) (apply (cadr f) (map eval* (cdr expr))))
          ((macro? f)  (eval* (apply (cadr f) (cdr expr))))
          (else (compile-and-eval `(lambda () ,expr))))))
    ((symbol? expr) (lookup expr))
    (else expr)))

(define (decode-value val)
  (if (and (eval* 'atom?) (not (zero? (eval* `(atom? ,val)))))
      (cons (decode-value (eval* `(car ,val))) (decode-value (eval* `(cdr ,val))))
      val))

(import-namespace-to current-namespace (export-to-namespace namespace-function-namespace 'namespace-function))
(import-namespace-to current-namespace (export-to-namespace macro-namespace      'macro))
(export-to-namespace arithmetic-namespace 'arithmetic)
(export-to-namespace machine-namespace    'machine)
(export-to-namespace builtin-namespace    'builtin)
(export-to-namespace internal-namespace   'internal)
(export-to-namespace tag-namespace        'tag)
(export-to-namespace type-namespace       'type)

(define system-syntax
  `((,sys-define              . ,syntax-define)
    (,sys-eval                . ,eval)))

(with-input-from-file "default.scm"
  (lambda ()
    (let loop ((s (read)))
      (cond 
        ((eof-object? s) #t)
        (else (eval* s) (loop (read)))))))

(repl)
