(use srfi-1)
(use srfi-4)
(use srfi-38)
(use matchable)
(use lolevel)

(define (p x) x)

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

(define-syntax push!
  (syntax-rules ()
    [(push! place value)
     (set! place (cons value place))]))

(define registers8  '(al cl dl bl ah ch dh bh))
(define registers16 '(ax cx dx bx sp bp si di))
(define registers32 '(eax ecx edx ebx esp ebp esi edi))
(define registers64 '(rax rcx rdx rbx rsp rbp rsi rdi r8 r9 r10 r11 r12 r13 r14 r15))

(define (register-8bit? x)  (any (cut eq? <> x) registers8)) 
(define (register-16bit? x) (any (cut eq? <> x) registers16)) 
(define (register-32bit? x) (any (cut eq? <> x) registers32)) 
(define (register-64bit? x) (any (cut eq? <> x) registers64)) 

(define (relocation-proc p)
  (lambda (x y)
    (cond 
      ((and (number? x) (number? y)) (p x y))
      ((number? x) (relocation-symbol (lambda (i) (p x ((cadr y) i))) (lambda (i) (p x ((caddr y) i)))))
      ((number? y) (relocation-symbol (lambda (i) (p ((cadr x) i) y)) (lambda (i) (p ((caddr x) i) y))))
      (else (relocation-symbol (lambda (i) (p ((cadr x) i) ((cadr y) i))) (lambda (i) (p ((caddr x) i) ((caddr y) i))))))))

(define (opcall dest)
  (norex-instruction/subcode '(#xFF) #x02 dest))

(define (opadd dest src)
  (cond
    ((integer? src) `(,@(instruction/subcode '(#x81) #x00 dest) ,@(enc 8 4 src)))
    ((pair? dest)   (instruction '(#x01) src dest))
    (else           (instruction '(#x03) dest src))))

(define (opor dest src)
  (cond
    ((integer? src) `(,@(instruction/subcode '(#x81) #x01 dest) ,@(enc 8 4 src)))
    ((pair? dest)   (instruction '(#x09) src dest))
    (else           (instruction '(#x0B) dest src))))

(define (opadc dest src)
  (cond
    ((integer? src) `(,@(instruction/subcode '(#x81) #x02 dest) ,@(enc 8 4 src)))
    ((pair? dest)   (instruction '(#x11) src dest))
    (else           (instruction '(#x13) dest src))))

(define (opsbb dest src)
  (cond
    ((integer? src) `(,@(instruction/subcode '(#x81) #x03 dest) ,@(enc 8 4 src)))
    ((pair? dest)   (instruction '(#x19) src dest))
    (else           (instruction '(#x1B) dest src))))

(define (opand dest src)
  (cond
    ((integer? src) `(,@(instruction/subcode '(#x81) #x04 dest) ,@(enc 8 4 src)))
    ((pair? dest)   (instruction '(#x21) src dest))
    (else           (instruction '(#x23) dest src))))

(define (opsub dest src)
  (cond
    ((integer? src) `(,@(instruction/subcode '(#x81) #x05 dest) ,@(enc 8 4 src)))
    ((pair? dest)   (instruction '(#x29) src dest))
    (else           (instruction '(#x2b) dest src))))

(define (opxor dest src)
  (cond
    ((integer? src) `(,@(instruction/subcode '(#x81) #x06 dest) ,@(enc 8 4 src)))
    ((pair? dest)   (instruction '(#x31) src dest))
    (else           (instruction '(#x33) dest src))))

(define (opcmp dest src)
  (cond
    ((integer? src) `(,@(instruction/subcode '(#x81) #x07 dest) ,@(enc 8 4 src)))
    ((pair? dest)   (instruction '(#x39) src dest))
    (else           (instruction '(#x3B) dest src))))

(define (opimul dest src)
  (cond
    ((integer? src) `(,@(instruction/subcode '(#x69) #x00 dest) ,@(enc 8 4 src)))
    (else           (instruction '(#x0F #xAF) dest src))))

(define (oppop dest)
  (cond
    ((regi dest) `(,@(rex-prefix/b dest) ,(logior #x58 (regi dest))))
    (else        (instruction/subcode '(#x8F) #x0 dest))))

(define (oppopf) '(#x9D))

(define (oppush dest)
  (cond
    ((regi dest) `(,@(rex-prefix/b dest) ,(logior #x50 (regi dest))))
    (else        (instruction/subcode '(#xFF) #x06 dest))))

(define (oppushf) '(#x9C))

(define (optest src1 src2)
  (cond
    ((and (eq? src1 'al) (integer? src2))   `(#xA8 ,@(enc 8 1 src2)))
    ((and (eq? src1 'rax) (integer? src2))  `(#xA9 ,@(enc 8 4 src2)))
    ((integer? src2) `(,(instruction/subcode '(#xF7) #x00 src1) ,@(enc 8 8 src2)))
    ((pair? src1)    (instruction '(#x85) src2 src1))
    (else            (instruction '(#x85) src1 src2))))

(define (opnot dest)
  (instruction/subcode '(#xF7) #x02 dest))

(define (opneg dest)
  (instruction/subcode '(#xF7) #x03 dest))

(define (opxchg dest1 dest2)
  (cond
    ((pair? dest2) (instruction '(#x87) dest1 dest2))
    (else          (instruction '(#x87) dest2 dest1))))

(define (oprol dest src)
  (cond
    ((integer? dest) `(,(instruction/subcode '(#xC1) #x00 dest) ,@(enc 8 1 src)))
    ((eq? src 'cl)   (instruction/subcode '(#xD3) #x00 dest))))

(define (opror dest src)
  (cond
    ((integer? dest) `(,(instruction/subcode '(#xC1) #x01 dest) ,@(enc 8 1 src)))
    ((eq? src 'cl)   (instruction/subcode '(#xD3) #x01 dest))))

(define (oprcl dest src)
  (cond
    ((integer? dest) `(,(instruction/subcode '(#xC1) #x02 dest) ,@(enc 8 1 src)))
    ((eq? src 'cl)   (instruction/subcode '(#xD3) #x02 dest))))

(define (oprcr dest src)
  (cond
    ((integer? dest) `(,(instruction/subcode '(#xC1) #x03 dest) ,@(enc 8 1 src)))
    ((eq? src 'cl)   (instruction/subcode '(#xD3) #x03 dest))))

(define (opsal dest src)
  (cond
    ((integer? dest) `(,(instruction/subcode '(#xC1) #x04 dest) ,@(enc 8 1 src)))
    ((eq? src 'cl)   (instruction/subcode '(#xD3) #x04 dest))))

(define (opshr dest src)
  (cond
    ((integer? dest) `(,(instruction/subcode '(#xC1) #x05 dest) ,@(enc 8 1 src)))
    ((eq? src 'cl)   (instruction/subcode '(#xD3) #x05 dest))))

(define (opsar dest src)
  (cond
    ((integer? dest) `(,(instruction/subcode '(#xC1) #x06 dest) ,@(enc 8 1 src)))
    ((eq? src 'cl)   (instruction/subcode '(#xD3) #x07 dest))))

(define (opint imm)
   `((#xCD) ,@(enc 8 1 imm)))

(define (opseto dest)
  (cond
    ((register-8bit? dest) (norex-instruction/subcode '(#x0F #x90) #x00 dest))))

(define (opsetno dest)
  (cond
    ((register-8bit? dest) (norex-instruction/subcode '(#x0F #x91) #x00 dest))))

(define (opsetb dest)
  (cond
    ((register-8bit? dest) (norex-instruction/subcode '(#x0F #x92) #x00 dest))))

(define (opsetnb dest)
  (cond
    ((register-8bit? dest) (norex-instruction/subcode '(#x0F #x93) #x00 dest))))

(define (opsetz dest)
  (cond
    ((register-8bit? dest) (norex-instruction/subcode '(#x0F #x94) #x00 dest))))

(define (opsetnz dest)
  (cond
    ((register-8bit? dest) (norex-instruction/subcode '(#x0F #x95) #x00 dest))))

(define (opsetna dest)
  (cond
    ((register-8bit? dest) (norex-instruction/subcode '(#x0F #x96) #x00 dest))))

(define (opseta dest)
  (cond
    ((register-8bit? dest) (norex-instruction/subcode '(#x0F #x97) #x00 dest))))

(define (opsets dest)
  (cond
    ((register-8bit? dest) (norex-instruction/subcode '(#x0F #x98) #x00 dest))))

(define (opsetns dest)
  (cond
    ((register-8bit? dest) (norex-instruction/subcode '(#x0F #x99) #x00 dest))))

(define (opsetp dest)
  (cond
    ((register-8bit? dest) (norex-instruction/subcode '(#x0F #x9A) #x00 dest))))

(define (opsetnp dest)
  (cond
    ((register-8bit? dest) (norex-instruction/subcode '(#x0F #x9B) #x00 dest))))

(define (opsetl dest)
  (cond
    ((register-8bit? dest) (norex-instruction/subcode '(#x0F #x9C) #x00 dest))))

(define (opsetnl dest)
  (cond
    ((register-8bit? dest) (norex-instruction/subcode '(#x0F #x9D) #x00 dest))))

(define (opsetng dest)
  (cond
    ((register-8bit? dest) (norex-instruction/subcode '(#x0F #x9E) #x00 dest))))

(define (opsetg dest)
  (cond
    ((register-8bit? dest) (norex-instruction/subcode '(#x0F #x9F) #x00 dest))))


(define (opmov dest src)
  (cond
    ((label-ref? src)   `(,@(rex-prefix 'rax dest) ,(logior #xB8 (regi dest)) ,src))
    ((and (integer? src) (< #x-80000000 src #x79999999)) `(,@(instruction/subcode '(#xC7) 0 dest) ,@(enc 8 4 src)))
    ((integer? src) `(,@(rex-prefix 'rax dest) ,(logior #xB8 (regi dest)) ,@(enc 8 8 src)))
    ((pair? dest) (instruction '(#x89) src dest))
    (else (instruction '(#x8B) dest src))))

(define (rex-prefix/b r)
  (if (= (regrex r) 0) '() '(#x41)))

(define (oplea dest src)
  (instruction '(#x8D) dest src))

(define (label? a)
  (and (pair? a) (eq? (car a) 'label)))

(define (label-ref? a)
  (and (pair? a) (eq? (car a) 'label-ref)))

(define (rel-label-ref? a)
  (and (pair? a) (eq? (car a) 'rel-label-ref)))

(define (instruction opcode reg r/m)
  `(,@(rex-prefix reg r/m)
    ,@opcode
    ,@(modR/M (regi reg) r/m)
    ,@(sib r/m)
    ,@(displacement r/m)))

(define (instruction/subcode opcode opcode-sub r/m)
  `(,@(rex-prefix 'rax r/m)
    ,@opcode
    ,@(modR/M opcode-sub r/m)
    ,@(sib r/m)
    ,@(displacement r/m)))

(define (norex-instruction/subcode opcode opcode-sub r/m)
  `(,@opcode
    ,@(modR/M opcode-sub r/m)
    ,@(sib r/m)
    ,@(displacement r/m)))

(define (rex-prefix reg place)
  (let1 prefix 
    (logior #x40
      (if (register-64bit? reg) 8 0)
      (ash (regrex reg) 2)
      (ash (regrex (sib-index place)) 1)
      (regrex (sib-base place)))
    (if (= prefix #x40) '() (list prefix))))

(define (sib-base place)
  (cond
    ((and (pair? place)
      (or (eq? (first place) '+)
          (eq? (first place) '-)))
       (second place))
    ((and (pair? place) (first place)))
    (else place)))

(define (sib-index a) 'rax)

(define (modR/M reg place)
  (define (r/m-byte mode reg rm)
    (logior (ash mode 6) (ash reg 3) rm))
  (cond
    ; mov rax rbx
    ((not-pair? place)
      (list (r/m-byte #b11 reg (regi place))))
    ; mov rax [rax]
    ((regi (first place))
      (list (r/m-byte #b00 reg (regi (first place)))))
    ; mov rax [+ rax 8]
    ((and 
          (or (eq? (first place) '+)
              (eq? (first place) '-))
          (< #x-80 (third place) #x79)) ; XXX (- rax #x-80) ==  (+ rax #x80)
      (list (r/m-byte #b01 reg (regi (second place)))))
    ; mov rax [+ rax #x4edf]
    ((and 
          (or (eq? (first place) '+)
              (eq? (first place) '-))
          (< #x-80000000 (third place) #x79999999)) ; XXX
      (list (r/m-byte #b10 reg (regi (second place)))))))

(define (sib place)
  (cond
    ((and (pair? place) (eq? (first place) 'rsp)) '(#b00100100))
    ((and (pair? place) (pair? (cdr place)) (eq? (second place) 'rsp)) '(#b00100100))
    (else '())))

(define (displacement place)
  (cond
    ((not-pair? place) '())
    ((and  (regi (first place))) '())
    ((and 
          (eq? (first place) '+)
          (< #x-80 (third place) #x79))
       (enc 8 1 (third place)))
    ((and 
          (eq? (first place) '-)
          (< #x-79 (third place) #x80))
       (enc 8 1 (- (third place))))
    ((and 
          (eq? (first place) '+)
          (< #x-80000000 (third place) #x79999999))
       (enc 8 4 (third place)))
    ((and 
          (eq? (first place) '-)
          (< #x-79999999 (- (third place)) #x80000000))
       (enc 8 4 (- (third place))))))

(define (immediate value)
  (cond
    ((not value) '())
    ((< #x-8000     value #x7999)     (enc 8 2 value))
    ((< #x-80000000 value #x79999999) (enc 8 4 value))
    ((< #x-8000000000000000 value #x7999999999999999) (enc 8 8 value))))

(define (enc bits len value)
  (if (number? value)
    (let1 v (mod value (expt (expt 2 bits) len))
      (if (= len 0)
        '()
        (cons
          (logand (lognot (ash -1 bits)) v)
          (enc bits (- len 1) (ash v (- bits))))))
    (list (relocation-symbol
      (lambda (i) (enc bits len ((cadr value) i)))
      (lambda (i) (enc bits len ((caddr value) i)))))))

(define (regi regsym)
  (let1 ret
    (or 
        (list-index (cut eq? regsym <>) registers64)
        (list-index (cut eq? regsym <>) registers32)
        (list-index (cut eq? regsym <>) registers16)
        (list-index (cut eq? regsym <>) registers8))
    (if (number? ret) (logand ret 7) ret)))

(define (regrex regsym)
  (ash
    (or (list-index (cut eq? regsym <>) registers64)
      0)
    -3))
  

; return
(define (opret)
  '(#xc3))

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

(define (compile-to-binary c)
      ((lambda (i)
        (let ((x (car i)) (args (cdr i)))
          (case x
            ((int)     (apply opint args))
            ((neg)     (apply opneg args))
            ((not)     (apply opnot args))
            ((seto)               (apply opseto args))
            ((setno)              (apply opsetno args))
            ((setb setnae setc)   (apply opsetb args))
            ((setnb setae setnc)  (apply opsetnb args))
            ((setz sete)          (apply opsetz args))
            ((setnz setne)        (apply opsetnz args))
            ((setbe setna)        (apply opsetna args))
            ((setnbe seta)        (apply opseta args))
            ((sets)               (apply opsets args))
            ((setns)              (apply opsetns args))
            ((setp setpe)         (apply opsetp args))
            ((setnp setpo)        (apply opsetnp args))
            ((setl setnge)        (apply opsetl args))
            ((setnl setge)        (apply opsetnl args))
            ((setle setng)        (apply opsetng args))
            ((setnle setg)        (apply opsetg args))
            ((rol)     (apply oprol args))
            ((ror)     (apply opror args))
            ((rcl)     (apply oprcl args))
            ((rcr)     (apply oprcr args))
            ((sal shl) (apply opsal args))
            ((sar)     (apply opsar args))
            ((shr)     (apply opshr args))
            ((add)     (apply opadd args))
            ((or ior)  (apply opor args))
            ((adc)     (apply opadc args))
            ((sbb)     (apply opsbb args))
            ((and)     (apply opand args))
            ((sub)     (apply opsub args))
            ((xor)     (apply opxor args))
            ((imul)    (apply opimul args))
            ((mov)     (apply opmov args))
            ((test)    (apply optest args))
            ((cmp)     (apply opcmp args))
            ((xchg)    (apply opxchg args))
            ((pop)     (apply oppop args))
            ((popf)    (apply oppopf))
            ((push)    (apply oppush args))
            ((pushf)   (apply oppushf))
            ((call)    (apply opcall args))
            ((ret)     (apply opret args))
            ((lea)     (apply oplea args))
            ((jnz) `(#x0F #x85 ,@args))
            ((jng) `(#x0F #x8E ,@args))
            ((jmp) `(#xE9 ,@args))
            (else (list i))))) c))

(define (link-label base codes)
  (define table '())
  (define x 0)
  (for-each (lambda (code)
    (push! table (cons (first code) x))
    (let loop ((binary (third code)))
      (for-each (lambda (b)
        (cond 
          ((label-ref? b)     (loop (second b)))
          ((rel-label-ref? b) (loop (second b)))
          ((label? b) (push! table (cons (second b) x)))
          (else (set! x (+ x 1))))) binary))) codes)
  (set! x 0)
  (append-map (lambda (code)
    (append-map (lambda (b)
       (cond
          ((label-ref? b)     (set! x (+ x (length (second b)))) ;XXX
                                 (enc 8 (length (second b)) (p (+ base (p (cdr (assoc (first b) (p table))))))))
          ((rel-label-ref? b) (set! x (+ x (length (second b))))
                                 (enc 8 (length (second b)) ((first b) table x base)))
          ((label? b) '())
          (else (set! x (+ x 1)) (list b)))) (third code))) codes))

(define (repl history print)
  (display "[1;35masm>[m ")
  (let1 expr (read)
    (case expr
      ((undo)    (pretty-inst-print (apply append (reverse (cdr history)))) (repl (cdr history) print))
      ((noprint) (repl history #f))
      ((print)   (pretty-inst-print (apply append (reverse history))) (repl history #t))
      ((exit quit) (exit))
      (else
       (let1 result (compile-to-binary expr)
         (if print (pretty-inst-print (apply append (reverse (cons result history)))))
         (repl (cons result history) print))))))

(define (pretty-inst-print x)
  (define (print-loop x i)
    (if (null? x) #t
      (begin 
        (if (and (not (zero? i)) (zero? (bitwise-and i #xf))) (newline))
        (if (zero? (bitwise-and i #xf)) (display (format "~x:" i)))
        (if (integer? (car x)) (display (format " ~x" (car x))) (display (car x)))
        (print-loop (cdr x) (+ 1 i)))))
  (print-loop x 0)
  (newline))

;(repl '() #t)
