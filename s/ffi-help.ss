(define extract-varargs-after-conv
  (lambda (conv*)
    (ormap (lambda (conv)
             (and (pair? conv) (eq? (car conv) 'varargs) (cdr conv)))
           conv*)))

(define get-allocable-callee-save-regs
  (lambda (type)
    (let loop ([i 0])
      (cond
        [(fx= i (vector-length regvec)) '()]
        [else (let ([reg (vector-ref regvec i)])
                (if (and (reg-callee-save? reg)
                         (or (eq? type 'all)
                             (eq? type (reg-type reg))))
                    (cons reg (loop (fx+ i 1)))
                    (loop (fx+ i 1))))]))))

;; With more newer 64-bit ABIs, we often need to move memory
;; into registers or vice-versa, and we have to not read past
;; the end of memory

(define memory-to-reg
  (lambda (ireg x from-offset size unsigned? tmp)
    (safe-assert (not (eq? ireg x)))
    (with-output-language (L13 Effect)
      (let loop ([ireg ireg] [from-offset from-offset] [size size] [unsigned? unsigned?])
        (case size
          [(8) `(set! ,ireg ,(%mref ,x ,from-offset))]
          [(7 6 5)
           (%seq
            ,(loop ireg (fx+ from-offset 4) (fx- size 4) #t)
            ,(loop tmp from-offset 4 #t)
            (set! ,ireg ,(%inline sll ,ireg (immediate 32)))
            (set! ,ireg ,(%inline + ,ireg ,tmp)))]
          [(3)
           (%seq
            ,(loop ireg from-offset 2 #t)
            ,(loop tmp (fx+ from-offset 2) 1 #t)
            (set! ,tmp ,(%inline sll ,tmp (immediate 16)))
            (set! ,ireg ,(%inline + ,ireg ,tmp)))]
          [else
           `(set! ,ireg ,(case size
                           [(1) `(inline ,(make-info-load (if unsigned? 'unsigned-8 'integer-8) #f) ,%load ,x ,%zero (immediate ,from-offset))]
                           [(2) `(inline ,(make-info-load (if unsigned? 'unsigned-16 'integer-16) #f) ,%load ,x ,%zero (immediate ,from-offset))]
                           [(4) `(inline ,(make-info-load (if unsigned? 'unsigned-32 'integer-32) #f) ,%load ,x ,%zero (immediate ,from-offset))]
                           [else (sorry! 'memory-to-reg "unexpected size ~s" size)]))])))))

(define reg-to-memory
  (lambda (dest offset size from-reg)
    ;; can trash `from-reg`, cannot use other registers
    (let loop ([offset offset] [size size])
      (with-output-language (L13 Effect)
        (case size
          [(1) `(inline ,(make-info-load 'integer-8 #f) ,%store ,dest ,%zero (immediate ,offset) ,from-reg)]
          [(2) `(inline ,(make-info-load 'integer-16 #f) ,%store ,dest ,%zero (immediate ,offset) ,from-reg)]
          [(3) (%seq
                ,(loop offset 2)
                (set! ,from-reg ,(%inline srl ,from-reg (immediate 16)))
                ,(loop (fx+ offset 2) 1))]
          [(4) `(inline ,(make-info-load 'integer-32 #f) ,%store ,dest ,%zero (immediate ,offset) ,from-reg)]
          [(8) `(set! ,(%mref ,dest ,offset) ,from-reg)]
          [(7 6 5) (%seq
                    ,(loop offset 4)
                    (set! ,from-reg ,(%inline srl ,from-reg (immediate 32)))
                    ,(loop (fx+ offset 4) (fx- size 4)))])))))

(define memory-to-memory
  (lambda (dest offset x from-offset size tmp)
    (with-output-language (L13 Effect)
      (let loop ([size size] [offset offset] [from-offset from-offset])
        (case size
          [(8) `(set! ,(%mref ,dest ,offset) ,(%mref ,x ,from-offset))]
          [(7 6 5)
           (%seq
            ,(loop 4 offset from-offset)
            ,(loop (fx- size 4) (fx+ offset 4) (fx+ from-offset 4)))]
          [(1 2 4)
           (let ([info (case size
                         [(1) (make-info-load 'integer-8 #f)]
                         [(2) (make-info-load 'integer-16 #f)]
                         [(4) (make-info-load 'integer-32 #f)])])
             `(seq
               (set! ,tmp (inline ,info ,%load ,x ,%zero (immediate ,from-offset)))
               (inline ,info ,%store ,dest ,%zero (immediate ,offset) ,tmp)))]
          [(3)
           (%seq
            ,(loop 2 offset from-offset)
            ,(loop 1 (fx+ offset 2) (fx+ from-offset 2)))]
          [else
           (%seq
            ,(loop 8 offset from-offset)
            ,(loop (fx- size 8) (fx+ offset 8) (fx+ from-offset 8)))])))))
