(define-insn-set (addi x y)
  (provided n) (eq? (reg n) x)
  (#x03 (modR/M 0 n) y)
  (provided n) (eq? (mem n) x)

  (provided n) (eq? (mem (reg n)))
