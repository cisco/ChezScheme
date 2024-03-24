;; pbchunk conversion uses the fasl parser from "strip.ss"; it mutates
;; code in the parsed structure to generate references to C chunks
;; that implement a shadow version of a chunk of bytecode instructions,
;; and then the printer of "strip.ss" is used to write the updated
;; fasl content

(if-feature pbchunk

(let ()

(include "strip-types.ss")

;; configuration, but `#t` is not practial for the boot files
(define one-chunklet-per-chunk? #f)

;; state for the chunk writer:
(define-record-type chunk-info
  (fields (mutable counter)
          seen
          code-accum)
  (nongenerative))

;; A chunklet represents a potential entry point into a code
;; object. It may have a prefix before the entry point that
;; is not generated as in C. A code object can have multiple
;; chunklets or just one.
(define-record-type chunklet
  (fields i        ; code offset for this start of this chunklet
          start-i  ; code offset for the entry point (= end-i if there's no entry)
          end-i    ; code offset aftet the end of this chunklet
          uses-flag? ; does the chunklet involve def-use pair of the branch flag
          continue-only? ; only render if part of a larger chunk?
          relocs   ; list of offset, none before this chunklet but maybe some after
          headers  ; list of (cons offset size), none before this chunklet but maybe some after
          labels)  ; list of `label`s, none before this chunklet but maybe some after
  (nongenerative))

;; A label within a chunklet, especially for interior branches
(define-record-type label
  (fields to        ; the label offset
          min-from  ; earliest offset that jumps here
          max-from  ; latest offset that jumps here
          all-from) ; all offsets that jump here
  (nongenerative))

(define (fasl-pbchunk! who c-ofns reg-proc-names start-index entry* handle-entry finish-fasl-update)
  ;; first "print" C code into a buffer representing the output text,
  ;; then break up that text into into separate files named by `c-ofns`
  (let* ([seen-table (make-eq-hashtable)]
         [o (box '())]
         [end-index
          (let loop ([entry* entry*] [index start-index])
            (cond
              [(null? entry*)
               index]
              [else
               (handle-entry
                (car entry*)
                (lambda (write-k)
                  (loop (cdr entry*) index))
                (lambda (situation x)
                  (loop (cdr entry*)
                        (search-pbchunk! x o index seen-table))))]))]
         [per-file (fxquotient (fx+ (fx- end-index start-index)
                                    (fx- (length c-ofns) 1))
                               (length c-ofns))]
         [i (box (reverse (unbox o)))])
    ;; before continuing, write out updated fasl:
    (finish-fasl-update)
    (let ()
      ;; at this point, chunks are in in `i`, so extract lines and
      ;; farm them out to the destination files;
      ;; start by opening all the destinations:
      (define (call-with-all-files k)
        (let p-loop ([todo-c-ofns c-ofns]
                     [rev-c-ops '()])
          (cond
            [(pair? todo-c-ofns)
             (let* ([c-ofn (car todo-c-ofns)]
                    [c-op ($open-file-output-port who c-ofn (file-options replace)
                                                  (buffer-mode block)
                                                  (native-transcoder))])
               (on-reset
                (delete-file c-ofn #f)
                (on-reset
                 (close-port c-op)
                 (fprintf c-op "#include \"system.h\"\n")
                 (fprintf c-op "#include <math.h>\n")
                 (fprintf c-op "#include \"pb.h\"\n")
                 (p-loop (cdr todo-c-ofns) (cons c-op rev-c-ops)))))]
            [else (k (reverse rev-c-ops))])))
      ;; helper to write out chunk registration:
      (define (write-registration c-op reg-proc-name start-index index)
        (newline c-op)
        (fprintf c-op "static void *~a_chunks[~a] = {\n" reg-proc-name (fx- index start-index))
        (let loop ([i start-index])
          (unless (fx= i index)
            (fprintf c-op "  chunk_~a~a\n"
                     i
                     (if (fx= (fx+ i 1) index) "" ","))
            (loop (fx+ i 1))))
        (fprintf c-op "};\n\n")
        (fprintf c-op "void ~a() {\n" reg-proc-name)
        (fprintf c-op "  Sregister_pbchunks(~a_chunks, ~a, ~a);\n"
                 reg-proc-name start-index index)
        (fprintf c-op "}\n"))
      ;; helper to recognize chunk starts:
      (define (chunk-start-line? line)
        (eq? (car line) chunk-header-fmt))
      ;; now loop to read lines and redirect:
      (call-with-all-files
       (lambda (c-ops)
         (let c-loop ([c-ops c-ops]
                      [reg-proc-names reg-proc-names]
                      [index start-index]
                      [line (get-c-line i)])
           (unless (null? c-ops)
             (let ([c-op (car c-ops)])
               (let chunk-loop ([fuel per-file] [n 0] [line line])
                 (cond
                   [(or (eof-object? line)
                        (and (fxzero? fuel)
                             (chunk-start-line? line)))
                    (write-registration c-op (car reg-proc-names) index (fx+ index n))
                    (close-port c-op)
                    (c-loop (cdr c-ops)
                            (cdr reg-proc-names)
                            (fx+ index n)
                            line)]
                   [else
                    (apply fprintf c-op line)
                    (let ([next-line (get-c-line i)])
                      (cond
                        [(chunk-start-line? line)
                         (chunk-loop (fx- fuel 1) (fx+ n 1) next-line)]
                        [else
                         (chunk-loop fuel n next-line)]))])))))))
      ;; fies written; return index after last chunk
      end-index)))

(define (emit-c o fmt . args)
  (set-box! o (cons (cons fmt args)
                    (unbox o))))

(define (get-c-line i)
  ;; The result is only roughly a textual line of code. The relevant
  ;; delimiting of code is to detect the start of a new chunk as using
  ;; `chunk-header-fmt`.
  (define lines (unbox i))
  (cond
    [(null? lines)
     #!eof]
    [else
     (let ([line (car lines)])
       (set-box! i (cdr lines))
       line)]))

;; The main pbchunk handler: takes a fasl object in "strip.ss" form,
;; find code objects inside, and potentially generates chunks and updates
;; the code object with references to chunks. Takes the number of
;; chunks previously written and returns the total number written after.
(define (search-pbchunk! v code-accum start-index seen-table)
  (let ([ci (make-chunk-info start-index
                             seen-table
                             code-accum)])
    (chunk! v ci)
    (chunk-info-counter ci)))

(define (chunk! v ci)
  (unless (eq-hashtable-ref (chunk-info-seen ci) v #f)
    (eq-hashtable-set! (chunk-info-seen ci) v #t)
    (do-chunk! v ci)))

(define (chunk-vector! vec ci)
  (vector-for-each (lambda (e) (chunk! e ci)) vec))

(define (do-chunk! v ci)
  (fasl-case* v
    [(pair vec)
     (chunk-vector! vec ci)]
    [(tuple ty vec)
     (constant-case* ty
       [(fasl-type-box fasl-type-immutable-box)
        (chunk! (vector-ref vec 0) ci)]
       [(fasl-type-weak-pair)
        ($oops 'chunk "weak pair not supported")]
       [(fasl-type-ephemeron)
        ($oops 'chunk "ephemeron pair not supported")]
       [else (void)])]
    [(vector ty vec)
     (constant-case* ty
       [(fasl-type-vector fasl-type-immutable-vector)
        (chunk-vector! vec ci)]
       [else (void)])]
    [(stencil-vector mask vec sys?)
     (chunk-vector! vec ci)]
    [(record maybe-uid size nflds rtd pad-ty* fld*)
     (for-each (lambda (fld)
                 (field-case fld [ptr (elem) (chunk! elem ci)] [else (void)]))
               fld*)]
    [(closure offset c)
     (chunk! c ci)]
    [(code flags free name arity-mask info pinfo* bytes m vreloc)
     (chunk-code! name bytes vreloc ci)
     (chunk-vector! vreloc ci)]
    [(reloc type-etc code-offset item-offset elem)
     (chunk! elem ci)]
    [(symbol-hashtable mutable? minlen subtype veclen vpfasl)
     (vector-for-each (lambda (p)
                        (chunk! (car p) ci)
                        (chunk! (cdr p) ci))
                      vpfasl)]
    [(indirect g i) (chunk! (vector-ref g i) ci)]
    [else
     ;; nothing else contains references that can reach code
     (void)]))

(define min-chunk-len 3)
(define instr-bytes 4)
(define reloc-instrs 4)

(define (instr-op instr) (bitwise-and instr #xFF))

(define (instr-d-dest instr) (bitwise-and (bitwise-arithmetic-shift-right instr 8) #xF))

(define (instr-dr-dest instr) (instr-d-dest instr))
(define (instr-dr-reg instr) (bitwise-and (bitwise-arithmetic-shift-right instr 16) #xF))

(define (instr-di-dest instr) (instr-d-dest instr))
(define (instr-di-imm instr) (bitwise-arithmetic-shift-right instr 16))

(define (instr-adr-dest instr) (instr-di-dest instr))
(define (instr-adr-imm instr) (bitwise-arithmetic-shift-right instr 12))

(define (instr-drr-dest instr) (instr-d-dest instr))
(define (instr-drr-reg1 instr) (bitwise-and (bitwise-arithmetic-shift-right instr 12) #xF))
(define (instr-drr-reg2 instr) (bitwise-and (bitwise-arithmetic-shift-right instr 16) #xF))

(define (instr-dri-dest instr) (instr-d-dest instr))
(define (instr-dri-reg instr) (bitwise-and (bitwise-arithmetic-shift-right instr 12) #xF))
(define (instr-dri-imm instr) (bitwise-arithmetic-shift-right instr 16))

(define (instr-i-imm instr) (bitwise-arithmetic-shift-right instr 8))

(define (make-chunk-instr index sub-index)
  (unless (eqv? index (bitwise-and index #xFFFF))
    ($oops 'pbchunk "chunk index ~a is too large" index))
  (unless (eqv? sub-index (bitwise-and sub-index #xFF))
    ($oops 'pbchunk "chunk sub-index ~a is too large" sub-index))
  (bitwise-ior (constant pb-chunk)
               (bitwise-arithmetic-shift-left sub-index 8)
               (bitwise-arithmetic-shift-left index 16)))
(define MAX-SUB-INDEXES 256)

;; expands to a binary search for the right case
(define-syntax (instruction-case stx)
  (syntax-case stx ()
    [(_ instr emit [op . shape] ...)
     (let ([vec (make-vector 256 0)]
           [emits (list->vector #'(($oops 'chunk "unrecognized instruction ~s" instr)
                                   (emit op . shape) ...))])
       (let loop ([ops (datum (op ...))] [pos 1])
         (unless (null? ops)
           (vector-set! vec (lookup-constant (car ops)) pos)
           (loop (cdr ops) (fx+ pos 1))))
       #`(let ([pos (vector-ref '#,vec (instr-op instr))])
           #,(let loop ([start 0] [end (vector-length emits)])
               (cond
                 [(fx= (fx+ start 1) end)
                  (vector-ref emits start)]
                 [else
                  (let ([mid (quotient (+ start end) 2)])
                    #`(if (fx>= pos #,mid)
                          #,(loop mid end)
                          #,(loop start mid)))]))))]))

;; di = destination register and immediate
;; dr = destination register and immediate
;; etc.
;; .../x = not handled, so return to interpret
;; .../u = unsigned immediate
;; .../f = sets flag
;; .../b = branch, uses flag deending on branch kind
;; .../c = foreign call
(define-syntax (instruction-cases stx)
  (syntax-case stx ()
    [(_ instr emit)
     #'(instruction-case
        instr emit
        ;; every instruction implemented in "pb.c" needs to be here,
        ;; except for the `pb-chunk` instruction
        [pb-nop nop]
        [pb-literal literal]
        [pb-mov16-pb-zero-bits-pb-shift0 di/u]
        [pb-mov16-pb-zero-bits-pb-shift1 di/u]
        [pb-mov16-pb-zero-bits-pb-shift2 di/u]
        [pb-mov16-pb-zero-bits-pb-shift3 di/u]
        [pb-mov16-pb-keep-bits-pb-shift0 di/u]
        [pb-mov16-pb-keep-bits-pb-shift1 di/u]
        [pb-mov16-pb-keep-bits-pb-shift2 di/u]
        [pb-mov16-pb-keep-bits-pb-shift3 di/u]
        [pb-mov-pb-i->i dr]
        [pb-mov-pb-d->d dr]
        [pb-mov-pb-i->d dr]
        [pb-mov-pb-d->i dr]
        [pb-mov-pb-s->d dr]
        [pb-mov-pb-d->s dr]
        [pb-mov-pb-d->s->d dr]
        [pb-mov-pb-i-bits->d-bits dr]
        [pb-mov-pb-d-bits->i-bits dr]
        [pb-mov-pb-i-i-bits->d-bits drr]
        [pb-mov-pb-d-lo-bits->i-bits dr]
        [pb-mov-pb-d-hi-bits->i-bits dr]
        [pb-bin-op-pb-no-signal-pb-add-pb-register drr]
        [pb-bin-op-pb-no-signal-pb-add-pb-immediate dri]
        [pb-bin-op-pb-no-signal-pb-sub-pb-register drr]
        [pb-bin-op-pb-no-signal-pb-sub-pb-immediate dri]
        [pb-bin-op-pb-no-signal-pb-mul-pb-register drr]
        [pb-bin-op-pb-no-signal-pb-mul-pb-immediate dri]
        [pb-bin-op-pb-no-signal-pb-div-pb-register drr]
        [pb-bin-op-pb-no-signal-pb-div-pb-immediate dri]
        [pb-bin-op-pb-no-signal-pb-and-pb-register drr]
        [pb-bin-op-pb-no-signal-pb-and-pb-immediate dri]
        [pb-bin-op-pb-no-signal-pb-ior-pb-register drr]
        [pb-bin-op-pb-no-signal-pb-ior-pb-immediate dri]
        [pb-bin-op-pb-no-signal-pb-xor-pb-register drr]
        [pb-bin-op-pb-no-signal-pb-xor-pb-immediate dri]
        [pb-bin-op-pb-no-signal-pb-lsl-pb-register drr]
        [pb-bin-op-pb-no-signal-pb-lsl-pb-immediate dri]
        [pb-bin-op-pb-no-signal-pb-lsr-pb-register drr]
        [pb-bin-op-pb-no-signal-pb-lsr-pb-immediate dri]
        [pb-bin-op-pb-no-signal-pb-asr-pb-register drr]
        [pb-bin-op-pb-no-signal-pb-asr-pb-immediate dri]
        [pb-bin-op-pb-no-signal-pb-lslo-pb-register drr]
        [pb-bin-op-pb-no-signal-pb-lslo-pb-immediate dri]
        [pb-bin-op-pb-signal-pb-add-pb-register drr/f]
        [pb-bin-op-pb-signal-pb-add-pb-immediate dri/f]
        [pb-bin-op-pb-signal-pb-sub-pb-register drr/f]
        [pb-bin-op-pb-signal-pb-sub-pb-immediate dri/f]
        [pb-bin-op-pb-signal-pb-mul-pb-register drr/f]
        [pb-bin-op-pb-signal-pb-mul-pb-immediate dri/f]
        [pb-bin-op-pb-signal-pb-subz-pb-register drr/f]
        [pb-bin-op-pb-signal-pb-subz-pb-immediate dri/f]
        [pb-bin-op-pb-signal-pb-subp-pb-register drr/f]
        [pb-bin-op-pb-signal-pb-subp-pb-immediate dri/f]
        [pb-cmp-op-pb-eq-pb-register dr/f]
        [pb-cmp-op-pb-eq-pb-immediate di/f]
        [pb-cmp-op-pb-lt-pb-register dr/f]
        [pb-cmp-op-pb-lt-pb-immediate di/f]
        [pb-cmp-op-pb-gt-pb-register dr/f]
        [pb-cmp-op-pb-gt-pb-immediate di/f]
        [pb-cmp-op-pb-le-pb-register dr/f]
        [pb-cmp-op-pb-le-pb-immediate di/f]
        [pb-cmp-op-pb-ge-pb-register dr/f]
        [pb-cmp-op-pb-ge-pb-immediate di/f]
        [pb-cmp-op-pb-ab-pb-register dr/f]
        [pb-cmp-op-pb-ab-pb-immediate di/f]
        [pb-cmp-op-pb-bl-pb-register dr/f]
        [pb-cmp-op-pb-bl-pb-immediate di/f]
        [pb-cmp-op-pb-cs-pb-register dr/f]
        [pb-cmp-op-pb-cs-pb-immediate di/f]
        [pb-cmp-op-pb-cc-pb-register dr/f]
        [pb-cmp-op-pb-cc-pb-immediate di/f]
        [pb-fp-bin-op-pb-add-pb-register drr]
        [pb-fp-bin-op-pb-sub-pb-register drr]
        [pb-fp-bin-op-pb-mul-pb-register drr]
        [pb-fp-bin-op-pb-div-pb-register drr]
        [pb-un-op-pb-not-pb-register dr]
        [pb-un-op-pb-not-pb-immediate di]
        [pb-fp-un-op-pb-sqrt-pb-register dr]
        [pb-fp-cmp-op-pb-eq-pb-register dr/f]
        [pb-fp-cmp-op-pb-lt-pb-register dr/f]
        [pb-fp-cmp-op-pb-le-pb-register dr/f]
        [pb-rev-op-pb-int16-pb-register dr]
        [pb-rev-op-pb-uint16-pb-register dr]
        [pb-rev-op-pb-int32-pb-register dr]
        [pb-rev-op-pb-uint32-pb-register dr]
        [pb-rev-op-pb-int64-pb-register dr]
        [pb-ld-op-pb-int8-pb-register drr]
        [pb-ld-op-pb-int8-pb-immediate dri]
        [pb-ld-op-pb-uint8-pb-register drr]
        [pb-ld-op-pb-uint8-pb-immediate dri]
        [pb-ld-op-pb-int16-pb-register drr]
        [pb-ld-op-pb-int16-pb-immediate dri]
        [pb-ld-op-pb-uint16-pb-register drr]
        [pb-ld-op-pb-uint16-pb-immediate dri]
        [pb-ld-op-pb-int32-pb-register drr]
        [pb-ld-op-pb-int32-pb-immediate dri]
        [pb-ld-op-pb-uint32-pb-register drr]
        [pb-ld-op-pb-uint32-pb-immediate dri]
        [pb-ld-op-pb-int64-pb-register drr]
        [pb-ld-op-pb-int64-pb-immediate dri]
        [pb-ld-op-pb-double-pb-register drr]
        [pb-ld-op-pb-double-pb-immediate dri]
        [pb-ld-op-pb-single-pb-register drr]
        [pb-ld-op-pb-single-pb-immediate dri]
        [pb-st-op-pb-int8-pb-register drr]
        [pb-st-op-pb-int8-pb-immediate dri]
        [pb-st-op-pb-int16-pb-register drr]
        [pb-st-op-pb-int16-pb-immediate dri]
        [pb-st-op-pb-int32-pb-register drr]
        [pb-st-op-pb-int32-pb-immediate dri]
        [pb-st-op-pb-int64-pb-register drr]
        [pb-st-op-pb-int64-pb-immediate dri]
        [pb-st-op-pb-double-pb-register drr]
        [pb-st-op-pb-double-pb-immediate dri]
        [pb-st-op-pb-single-pb-register drr]
        [pb-st-op-pb-single-pb-immediate dri]
        [pb-b-op-pb-fals-pb-register r/b "if (!flag) "]
        [pb-b-op-pb-fals-pb-immediate i/b "if (!flag) "]
        [pb-b-op-pb-true-pb-register r/b  "if (flag) "]
        [pb-b-op-pb-true-pb-immediate i/b "if (flag) "]
        [pb-b-op-pb-always-pb-register r/b ""]
        [pb-b-op-pb-always-pb-immediate i/b ""]
        [pb-b*-op-pb-register dr/b]
        [pb-b*-op-pb-immediate di/b]
        [pb-return n/x]
        [pb-adr adr]
        [pb-interp d/x]
        [pb-call dri/c]
        [pb-inc-pb-register dr/f]
        [pb-inc-pb-immediate di/f]
        [pb-lock d/f]
        [pb-cas drr/f]
        [pb-fence-pb-fence-store-store n]
        [pb-fence-pb-fence-acquire n]
        [pb-fence-pb-fence-release n]
        [pb-call-arena-in di]
        [pb-fp-call-arena-in di]
        [pb-call-arena-out di]
        [pb-fp-call-arena-out di]
        [pb-stack-call dr])]))

(define (advance l sel i)
  (let loop ([l l])
    (cond
      [(null? l) '()]
      [(fx>= (sel (car l)) i) l]
      [else (loop (cdr l))])))

(define (advance-relocs relocs i)
  (advance relocs values i))

(define (advance-headers headers i)
  (advance headers car i))

(define (advance-labels labels i)
  (advance labels label-to i))

(define (ensure-label i labels)
  (cond
    [(and (pair? labels)
          (fx= i (label-to (car labels))))
     (let ([l (car labels)])
       (cons (make-label i
                         (fxmin i (label-min-from l))
                         (fxmax i (label-max-from l))
                         (cons i (label-all-from l)))
             (cdr labels)))]
    [(and (pair? labels)
          (fx> i (label-to (car labels))))
     (cons (car labels) (ensure-label i (cdr labels)))]
    [else
     (cons (make-label i i i (list i))
           labels)]))

(define (sort-and-combine-labels labels)
  (let ([labels (sort (lambda (a b) (< (label-to a) (label-to b))) labels)])
    (let remove-dups ([labels labels])
      (cond
        [(null? labels) '()]
        [(null? (cdr labels)) labels]
        [else
         (let ([a (car labels)]
               [b (cadr labels)])
           (if (fx= (label-to a) (label-to b))
               (remove-dups (cons (make-label (label-to a)
                                              (fxmin (label-min-from a)
                                                     (label-min-from b))
                                              (fxmax (label-max-from a)
                                                     (label-max-from b))
                                              (append (label-all-from a)
                                                      (label-all-from b)))
                                  (cddr labels)))
               (cons a (remove-dups (cdr labels)))))]))))

(define (empty-chunklet? c)
  (or (fx= (chunklet-start-i c)
           (chunklet-end-i c))
      (chunklet-continue-only? c)))

;; Found a code object, maybe generate a chunk
(define (chunk-code! name bv vreloc ci)
  (let ([len (bytevector-length bv)]
        [o (chunk-info-code-accum ci)]
        [relocs (let loop ([off 0] [rels (vector->list vreloc)])
                  (cond
                    [(null? rels) '()]
                    [else
                     (fasl-case* (car rels)
                       [(reloc type-etc code-offset item-offset elem)
                        (let ([off (+ off code-offset)])
                          (cons (fx- off (constant code-data-disp))
                                (loop off (cdr rels))))]
                       [else '()])]))]
        [name (extract-name name)])
    (emit-c o "\n/* code ~a */\n" name)
    (unless (equal? name "winder-dummy") ; hack to avoid special rp header in dounderflow
      (let ([chunklets
             ;; use `select-instruction-range` to partition the code into chunklets
             (let-values ([(headers labels) (gather-targets bv len)])
               #;(emit-c o "/* labels: ~s */\n" (map (lambda (l) (format "0x~x" (label-to l))) labels))
               (let loop ([i 0] [relocs relocs] [headers headers] [labels labels])
                 (cond
                   [(fx= i len) '()]
                   [else
                    (let-values ([(start-i end-i uses-flag?)
                                  (select-instruction-range bv i len relocs headers labels)])
                      (when (fx= i end-i)
                        ($oops 'chunk-code "failed to make progress at ~a out of ~a" i len))
                      (let ([continue-only?
                             ;; when the chunk would be too small to save us any time, so don't
                             ;; bother make it stand-alone; a threshold greater than 1 also avoids
                             ;; code that wouldn't even use `ms` or `ip`:
                             (fx< (fx- end-i start-i)
                                  (fx* min-chunk-len instr-bytes))])
                        (cons (make-chunklet i start-i end-i uses-flag? continue-only? relocs headers labels)
                              (loop end-i
                                    (advance-relocs relocs end-i)
                                    (advance-headers headers end-i)
                                    (advance-labels labels end-i)))))])))]
            [index (chunk-info-counter ci)])
        ;; We can either generate each chunklet as its own chunk
        ;; function or generate one chunk function with multiple
        ;; chunklets
        (let ([count (fold-left (lambda (sum c) (if (empty-chunklet? c) sum (fx+ 1 sum)))
                                0
                                chunklets)])
          (cond
            [(fx> count 256)
             ;; this many chunklets suggests that compilation is not productive,
             ;; so just show the disassembly
             (emit-c o "/* (too many entry points) */\n")
             (let ([all-chunklets chunklets])
               (let loop ([chunklets chunklets])
                 (unless (null? chunklets)
                   (let ([c (car chunklets)])
                     (emit-chunklet o bv
                                    (chunklet-i c) 0
                                    (chunklet-relocs c) (chunklet-headers c) '()
                                    (chunklet-end-i c) ; => treat as empty
                                    (chunklet-end-i c)
                                    all-chunklets
                                    ;; fallthrough?
                                    #t)
                     (loop (cdr chunklets))))))]
            [(or one-chunklet-per-chunk?
                 ;; also use this more if there's 0 or 1 chunklets to emit,
                 ;; or more than `MAX-SUB-INDEXES`:
                 (let ()
                   (or (fx< count 2)
                       (fx> count MAX-SUB-INDEXES))))
             (let loop ([chunklets chunklets] [index index])
               (cond
                 [(null? chunklets)
                  (chunk-info-counter-set! ci index)]
                 [else
                  (let ([c (car chunklets)])
                    ;; generate a non-empty chunk as its own function
                    (unless (empty-chunklet? c)
                      (emit-chunk-header o index #f (chunklet-uses-flag? c)))
                    (emit-chunklet o bv
                                   (chunklet-i c) (chunklet-start-i c)
                                   (chunklet-relocs c) (chunklet-headers c) (chunklet-labels c)
                                   (if (chunklet-continue-only? c)
                                       (chunklet-end-i c)
                                       (chunklet-start-i c))
                                   (chunklet-end-i c)
                                   (list c) ; `goto` branches contrained to this chunklet
                                   ;; fallthrough?
                                   (empty-chunklet? c))
                    (unless (empty-chunklet? c)
                      (emit-chunk-footer o)
                      (bytevector-u32-set! bv (chunklet-start-i c) (make-chunk-instr index 0) (constant fasl-endianness)))
                    (loop (cdr chunklets) (if (empty-chunklet? c) index (fx+ index 1))))]))]
            [else
             ;; one chunk for the whole code object, where multiple entry points are
             ;; supported by a sub-index
             (emit-chunk-header o index #t (ormap chunklet-uses-flag? chunklets))
             (chunk-info-counter-set! ci (fx+ 1 index))
             ;; dispatch to label on entry via sub-index
             (emit-c o "  switch (sub_index) {\n")
             ;; dispatch to a chunklet 
             (let loop ([chunklets chunklets] [sub-index 0])
               (unless (null? chunklets)
                 (let ([c (car chunklets)])
                   (cond
                     [(empty-chunklet? c) (loop (cdr chunklets) sub-index)]
                     [else
                      (emit-c o "    case ~a:~a ip -= 0x~x; goto label_~x;\n"
                              sub-index
                              (if (andmap empty-chunklet? (cdr chunklets))
                                  " default:"
                                  "")
                              (chunklet-start-i c)
                              (chunklet-start-i c))
                      (loop (cdr chunklets) (fx+ 1 sub-index))]))))
             (emit-c o "  }\n")
             (let ([all-chunklets chunklets])
               (let loop ([chunklets chunklets] [sub-index 0])
                 (unless (null? chunklets)
                   (let ([c (car chunklets)])
                     ;; emit a chunklet within the function
                     (emit-chunklet o bv
                                    (chunklet-i c) 0
                                    (chunklet-relocs c) (chunklet-headers c)
                                    (if (empty-chunklet? c)
                                        (chunklet-labels c)
                                        (ensure-label (chunklet-start-i c) (chunklet-labels c)))
                                    (chunklet-start-i c) (chunklet-end-i c)
                                    all-chunklets ; `goto` branches allowed across chunklets
                                    ;; fallthrough?
                                    (and (pair? (cdr chunklets))
                                         (fx= (chunklet-end-i c)
                                              (chunklet-start-i (cadr chunklets)))))
                     (unless (empty-chunklet? c)
                       (bytevector-u32-set! bv (chunklet-start-i c) (make-chunk-instr index sub-index) (constant fasl-endianness)))
                     (loop (cdr chunklets) (if (empty-chunklet? c) sub-index (fx+ 1 sub-index)))))))
             (emit-chunk-footer o)]))))))

;; Find all branch targets in the code object
(define (gather-targets bv len)
  (let loop ([i 0] [headers '()] [labels '()])
    (cond
      [(fx= i len) (values '() (sort-and-combine-labels labels))]
      [(and (pair? headers)
            (fx= i (caar headers)))
       (let ([size (cdar headers)])
         (let ([i (+ i size)])
           (let-values ([(rest-headers labels) (loop i (cdr headers) labels)])
             (values (cons (car headers) rest-headers)
                     labels))))]
      [else
       (let ([instr (bytevector-s32-ref bv i (constant fasl-endianness))]
             [uinstr (bytevector-u32-ref bv i (constant fasl-endianness))])
         (define (next)
           (loop (fx+ i instr-bytes) headers labels))

         (define (next/add-label new-label)
           (loop (fx+ i instr-bytes) headers (cons new-label labels)))

         (define (next/adr)
           (let ([delta (fx* instr-bytes (instr-adr-imm instr))])
             (cond
               [(> delta 0)
                (let* ([after (fx+ i instr-bytes delta)]
                       [size (if (fx= 1 (fxand 1 (bytevector-u8-ref bv (fx- after
                                                                            (if (eq? (constant fasl-endianness) 'little)
                                                                                (constant ptr-bytes)
                                                                                1)))))
                                 (constant size-rp-compact-header)
                                 (constant size-rp-header))]
                       [start (fx- after size)]
                       [header (cons start size)])
                  (loop (fx+ i instr-bytes)
                        ;; insert keeping headers sorted
                        (let loop ([headers headers])
                          (cond
                            [(null? headers) (list header)]
                            [(fx<= start (caar headers)) (cons header headers)]
                            [else (cons (car headers) (loop (cdr headers)))]))
                        labels))]
               [else (next)])))

         (define (next-branch)
           (let* ([delta (instr-i-imm instr)]
                  [target-label (fx+ i instr-bytes delta)])
             (next/add-label (make-label target-label i i (list i)))))

         (define (next/literal)
           (loop (fx+ i instr-bytes (constant ptr-bytes)) headers labels))

         (define-syntax (dispatch stx)
           (syntax-case stx (i/b adr literal)
             [(_ op i/b test) #'(next-branch)]
             [(_ op adr) #'(next/adr)]
             [(_ op literal) #'(next/literal)]
             [_ #'(next)]))

         (instruction-cases instr dispatch))])))

;; Select next chunklet within a code object
(define (select-instruction-range bv i len relocs headers labels)
  (let loop ([i i] [relocs relocs] [headers headers] [labels labels] [start-i #f]
             [flag-ready? #f] [uses-flag? #f])
    (cond
      [(fx= i len) (values (or start-i i) i uses-flag?)]
      [(and (pair? headers)
            (fx= i (caar headers)))
       (cond
         [start-i
          ;; we want to start a new chunk after the header, so end this one
          (values start-i i uses-flag?)]
         [else
          (let* ([size (cdar headers)]
                 [i (+ i size)])
            (loop i
                  (advance-relocs relocs i)
                  (cdr headers)
                  labels
                  start-i
                  #f
                  uses-flag?))])]
      [(and (pair? labels)
            (fx= i (label-to (car labels))))
       ;; we want to stop at this label if it's a target outside the range
       ;; that we're trying to build
       (cond
         [(< (label-min-from (car labels)) (or start-i i))
          ;; target from jump before this chunk
          (if start-i
              (values start-i i uses-flag?)
              (loop i relocs headers (cdr labels) #f #f uses-flag?))]
         [(< (label-max-from (car labels)) i)
          ;; always a forward jump within this chunk
          (loop i relocs headers (cdr labels) start-i #f uses-flag?)]
         [else
          ;; some backward jump exists, but tenatively assume that
          ;; it's within the chunk, then check;
          ;; WARNING: this makes overall chunking not linear-time, but
          ;; it's probably ok in practice
          (let-values ([(maybe-start-i end-i maybe-uses-flag?)
                        (loop i relocs headers (cdr labels) start-i #f uses-flag?)])
            (cond
              [(fx>= maybe-start-i i)
               ;; chunk here or starts later, anyway
               (values maybe-start-i end-i maybe-uses-flag?)]
              [(fx< (label-max-from (car labels)) end-i)
               ;; backward jumps stay within chunk
               (values maybe-start-i end-i maybe-uses-flag?)]
              [else
               ;; not within chunk
               (values start-i i uses-flag?)]))])]
      [(and (pair? relocs)
            (fx>= i (car relocs)))
       ($oops 'pbchunk "landed at a relocation")]
      [else
       ;; if the instruction always has to trampoline back, then the instruction
       ;; after can start a chunk to resume
       (let ([instr (bytevector-s32-ref bv i (constant fasl-endianness))])
         (define (check-flag)
           (unless flag-ready?
             ($oops 'pbchunk "branch not immediately after signal at 0x~x" i)))
         (define (keep now-uses-flag?)
           (when now-uses-flag? (check-flag))
           (loop (fx+ i instr-bytes) relocs headers labels (or start-i i) #f (or uses-flag?
                                                                                 now-uses-flag?)))
         (define (keep-signalling)
           (loop (fx+ i instr-bytes) relocs headers labels (or start-i i) #t uses-flag?))
         (define (skip)
           (loop (fx+ i instr-bytes) relocs headers labels (or start-i i) flag-ready? uses-flag?))
         (define (stop-before)
           (if start-i
               (values start-i i uses-flag?)
               (loop (fx+ i instr-bytes) relocs headers labels #f #f uses-flag?)))
         (define (stop-after)
           (values (or start-i i) (fx+ i instr-bytes) uses-flag?))

         (define (keep-literal)
           (unless (and (pair? relocs)
                        (fx= (fx+ i instr-bytes) (car relocs)))
             ($oops 'pbchunk "no relocation after pb-literal"))
           (let ([next-i (fx+ i instr-bytes (constant ptr-bytes))])
             (loop next-i (cdr relocs) headers labels (or start-i i) #f uses-flag?)))

         (define-syntax (dispatch stx)
           (syntax-case stx (dri/x dr/x d/x n/x r/b i/b d/f dr/b di/b
                                   dr/f di/f drr/f dri/f literal nop)
             [(_ op dri/x) #'(stop-before)]
             [(_ op dr/x) #'(stop-before)]
             [(_ op d/x) #'(stop-before)]
             [(_ op n/x) #'(stop-before)]
             [(_ op r/b "") #'(stop-after)]
             [(_ op i/b "") #'(keep #f)]
             [(_ op r/b . _) #'(keep #t)]
             [(_ op i/b . _) #'(keep #t)]
             [(_ op d/f) #'(keep-signalling)]
             [(_ op dr/b) #'(stop-after)]
             [(_ op di/b) #'(stop-after)]
             [(_ op dr/f) #'(keep-signalling)]
             [(_ op di/f) #'(keep-signalling)]
             [(_ op drr/f) #'(keep-signalling)]
             [(_ op dri/f) #'(keep-signalling)]
             [(_ op literal) #'(keep-literal)]
             [_ #'(skip)]))
         (instruction-cases instr dispatch))])))

(define chunk-header-fmt "static uptr chunk_~a(MACHINE_STATE ms, uptr ip~a) {\n")

(define (emit-chunk-header o index sub-index? uses-flag?)
  (emit-c o chunk-header-fmt
          index
          (if sub-index? ", int sub_index" " UNUSED_SUB_INDEX"))
  (when uses-flag?
    (emit-c o "  int flag;\n")))

(define (emit-chunk-footer o)
  (emit-c o "}\n"))
  
;; just show decoded instructions from `i` until `start-i`, then
;; generate a chunk function from `start-i` to `end-i`
(define (emit-chunklet o bv i base-i relocs headers labels start-i end-i chunklets fallthrough?)
  (define (in-chunk? target)
    (ormap (lambda (c)
             (and (fx>= target (chunklet-start-i c))
                  (fx< target (chunklet-end-i c))))
           chunklets))
  (let loop ([i i] [relocs relocs] [headers headers] [labels labels])
    (cond
      [(and (pair? headers)
            (fx= i (caar headers)))
       (cond
         [(fx>= i start-i)
          (unless (fx= i end-i) ($oops 'emit-chunk "should have ended at header ~a/~a" i end-i))]
         [else
          (let ([size (cdar headers)])
            (emit-c o "/* data: ~a bytes */\n" size)
            (let ([i (fx+ i size)])
              (loop i
                    (advance-relocs relocs i)
                    (cdr headers)
                    labels)))])]
      [(fx= i end-i)
       (unless fallthrough?
         (emit-c o "  return ip+code_rel(0x~x, 0x~x);\n" base-i i))]
      [(and (pair? labels)
            (fx= i (label-to (car labels))))
       (when (fx>= i start-i)
         (let ([a (car labels)])
           (when (ormap in-chunk? (label-all-from a))
             (emit-c o "label_~x:\n" i))))
       (loop i relocs headers (cdr labels))]
      [else
       (let ([instr (bytevector-s32-ref bv i (constant fasl-endianness))]
             [uinstr (bytevector-u32-ref bv i (constant fasl-endianness))])
         (define (next)
           (loop (fx+ i instr-bytes) relocs headers labels))

         (define (done)
           (next))

         (define (emit-pre)
           (emit-c o "/* 0x~x */" i)
           (unless (>= i start-i)
             (emit-c o " /*")))
         (define (emit-post)
           (unless (>= i start-i)
             (emit-c o " */")))

         (define (emit-desc fmt . args)
           (apply emit-c o fmt args))

         (define (emit-do _op . args)
           (emit-pre)
           (emit-c o " do_~a(~{~a~^, ~});" _op args)
           (emit-post))

         (define (emit-return)
           (emit-pre)
           (emit-c o " return ip+code_rel(0x~x, 0x~x);" base-i i)
           (emit-post))

         (define (r-form _op)
           (emit-do _op (instr-dr-reg instr))
           (emit-c o "\n")
           (next))
         
         (define (d-form _op)
           (emit-do _op (instr-dr-dest instr))
           (emit-c o "\n")
           (next))

         (define (dr-form _op)
           (emit-do _op (instr-dr-dest instr) (instr-dr-reg instr))
           (emit-desc " /* r~a <- r~a */\n"
                      (instr-dr-dest instr)
                      (instr-dr-reg instr))
           (next))
         
         (define (di-form _op di-imm)
           (emit-do _op (instr-di-dest instr) di-imm)
           (emit-desc " /* r~a <- 0x~x */\n"
                      (instr-di-dest instr)
                      di-imm)
           (next))
         
         (define (drr-form _op)
           (emit-do _op (instr-drr-dest instr) (instr-drr-reg1 instr) (instr-drr-reg2 instr))
           (emit-desc " /* r~a <- r~a, r~a */\n"
                      (instr-drr-dest instr)
                      (instr-drr-reg1 instr)
                      (instr-drr-reg2 instr))
           (next))
         
         (define (dri-form _op)
           (emit-do _op
                    (instr-dri-dest instr)
                    (instr-dri-reg instr)
                    (instr-dri-imm instr))
           (emit-desc " /* r~a <- r~a, 0x~x */\n"
                      (instr-dri-dest instr)
                      (instr-dri-reg instr)
                      (instr-dri-imm instr))
           (next))

         (define (n-form _op)
           (emit-do _op)
           (emit-c o "\n")
           (next))

         (define (call-form _op)
           (emit-foreign-call o instr)
           (next))

         (define-syntax (emit stx)
           (with-syntax ([_op (syntax-case stx ()
                                [(_ op . _)
                                 (datum->syntax #'op
                                                (list->string
                                                 (fold-right (lambda (x rest) 
                                                               (case x
                                                                 [(#\-) (cons #\_ rest)]
                                                                 [(#\>) rest]
                                                                 [(#\*) (cons #\s rest)]
                                                                 [else (cons x rest)]))
                                                             '()
                                                             (string->list (symbol->string (syntax->datum #'op))))))])])
             (syntax-case stx (di/u
                               di di/f dr dr/f dr/x
                               drr dri drr/f dri/f dri/c
                               dri/x r d/f d/x i r/b i/b dr/b di/b n n/x
                               adr literal nop)
               [(_ op di/u) #'(di-form '_op (instr-di-imm uinstr))]
               [(_ op di) #'(di-form '_op (instr-di-imm instr))]
               [(_ op di/f) #'(di-form '_op (instr-di-imm instr))]
               [(_ op dr) #'(dr-form '_op)]
               [(_ op dr/f) #'(dr-form '_op)]
               [(_ op dr/x)
                #'(begin
                    (emit-return)
                    (emit-desc " /* ~a: r~a <- r~a */\n"
                               '_op
                               (instr-dr-dest instr)
                               (instr-dr-reg instr))
                    (done))]
               [(_ op drr) #'(drr-form '_op)]
               [(_ op drr/f) #'(drr-form '_op)]
               [(_ op dri) #'(dri-form '_op)]
               [(_ op dri/f) #'(dri-form '_op)]
               [(_ op dri/c) #'(call-form '_op)]
               [(_ op dri/x)
                #'(begin
                    (emit-return)
                    (emit-desc " /* ~a: r~a <- r~a, 0x~x */\n"
                               '_op
                               (instr-dri-dest instr)
                               (instr-dri-reg instr)
                               (instr-dri-imm instr))
                    (done))]
               [(_ op r) #'(r-form '_op)]
               [(_ op d/f) #'(d-form '_op)]
               [(_ op d/x)
                #'(begin
                    (emit-return)
                    (emit-desc " /* ~a: ~a */\n"
                               '_op
                               (instr-dr-dest instr))
                    (done))]
               [(_ op i)
                #'(begin
                    (emit-do '_op (instr-i-imm instr))
                    (emit-desc " /* 0x~x */\n"
                               (instr-i-imm instr))
                    (next))]
               [(_ op r/b test)
                #'(begin
                    (emit-pre)
                    (emit-c o " ~areturn regs[~a];"
                            test
                            (instr-dr-reg instr))
                    (emit-post)
                    (emit-desc " /* ~a */\n"
                               '_op)
                    (if (equal? test "")
                        (done)
                        (next)))]
               [(_ op i/b test)
                #'(let* ([delta (instr-i-imm instr)]
                         [target-label (fx+ i instr-bytes delta)])
                    (cond
                      [(in-chunk? target-label)
                       (emit-pre)
                       (emit-c o " ~agoto label_~x;"
                               test
                               target-label)
                       (emit-post)
                       (emit-desc " /* ~a: 0x~x */\n"
                                  '_op
                                  delta)
                       (next)]
                      [else
                       (emit-pre)
                       (emit-c o " ~areturn ip+code_rel(0x~x, 0x~x);"
                               test
                               base-i
                               target-label)
                       (emit-post)
                       (emit-desc " /* ~a: 0x~x */\n"
                                  '_op
                                  delta)
                       (if (equal? test "")
                           (done)
                           (next))]))]
               [(_ op dr/b)
                #'(begin
                    (emit-pre)
                    (emit-c o " return get_~a_addr(~a, ~a);"
                            '_op
                            (instr-dr-dest instr)
                            (instr-dr-reg instr))
                    (emit-post)
                    (emit-desc " /* r~a + r~a */\n"
                               (instr-dr-dest instr)
                               (instr-dr-reg instr))
                    (done))]
               [(_ op di/b)
                #'(let* ([delta (instr-i-imm instr)]
                         [target-label (fx+ i instr-bytes delta)])
                    (emit-pre)
                    (emit-c o " return get_~a_addr(~a, ~a);"
                            '_op
                            (instr-di-dest instr)
                            (instr-di-imm instr))
                    (emit-post)
                    (emit-desc " /* r~a + 0x~x */\n"
                               (instr-di-dest instr)
                               (instr-di-imm instr))
                    (done))]
               [(_ op n) #'(n-form '_op)]
               [(_ op n/x)
                #'(begin
                    (emit-return)
                    (emit-desc " /* ~a */\n" '_op)
                    (done))]
               [(_ op adr)
                #'(let ([delta (fx+ i instr-bytes (fx* instr-bytes (instr-adr-imm instr)))])
                    (emit-pre)
                    (emit-c o " load_code_relative(~a, ip+code_rel(0x~x, ~a));"
                            (instr-adr-dest instr)
                            base-i
                            (if (fx< delta 0)
                                (format "-0x~x" (fx- delta))
                                (format "0x~x" delta)))
                    (emit-post)
                    (emit-desc "\n")
                    (next))]
               [(_ op literal)
                #'(let ([dest (instr-di-dest instr)])
                    (unless (and (pair? relocs)
                                 (fx= (fx+ i instr-bytes) (car relocs)))
                      ($oops 'pbchunk "no relocation after pb-literal?"))
                    (emit-pre)
                    (emit-c o " load_from_relocation(~a, ip+code_rel(0x~x, 0x~x));"
                            dest
                            base-i
                            (fx+ i instr-bytes))
                    (emit-post)
                    (emit-desc "\n")
                    (loop (fx+ i instr-bytes (constant ptr-bytes)) (cdr relocs) headers labels))]
               [(_ op nop) #'(next)])))

         (instruction-cases instr emit))])))

(define (emit-foreign-call o instr)
  (let* ([proto-index (instr-dri-imm instr)]
         [proto (ormap (lambda (p) (and (eqv? (cdr p) proto-index) (car p)))
                       (constant pb-prototype-table))])
    (unless proto ($oops 'pbchunk "could not find foreign-call prototype"))
    (emit-c o "  ")
    (case (car proto)
      [(void) (void)]
      [(double) (emit-c o "fpregs[Cfpretval] = ")]
      [(void*) (emit-c o "regs[Cretval] = TO_PTR(")]
      [else (emit-c o "regs[Cretval] = ")])
    (emit-c o "((pb~a_t)TO_VOIDP(regs[~a]))("
            (apply string-append
                   (map (lambda (t)
                          (string-append
                           "_"
                           (list->string
                            (fold-right (lambda (x rest) 
                                          (case x
                                            [(#\-) (cons #\_ rest)]
                                            [(#\*) (cons #\s rest)]
                                            [else (cons x rest)]))
                                        '()
                                        (string->list (symbol->string t))))))
                        proto))
            (instr-dri-dest instr))
    (let loop ([proto (cdr proto)] [int 1] [fp 1])
      (unless (null? proto)
        (unless (and (fx= int 1) (fx= fp 1))
          (emit-c o ", "))
        (case (car proto)
          [(double)
           (emit-c o "fpregs[Cfparg~a]" fp)
           (loop (cdr proto) int (fx+ fp 1))]
          [(void*)
           (emit-c o "TO_VOIDP(regs[Carg~a])" int)
           (loop (cdr proto) (fx+ int 1) fp)]
          [else
           (emit-c o "regs[Carg~a]" int)
           (loop (cdr proto) (fx+ int 1) fp)])))
    (case (car proto)
      [(void*) (emit-c o ")")] ; close `TO_PTR`
      [else (void)])
    (emit-c o "); /* pb_call ~a */\n" proto-index)))

(define (extract-name name)
  (fasl-case* name
    [(string ty string) (list->string
                         (let loop ([l (string->list string)])
                           (cond
                             [(null? l) '()]
                             [(and (eqv? #\* (car l))
                                   (pair? (cdr l))
                                   (eqv? #\/ (cadr l)))
                              ;; mangle to avoid "*/" in name
                              (cons* (car l) #\space (loop (cdr l)))]
                             [else (cons (car l) (loop (cdr l)))])))]
    [(indirect g i) (extract-name (vector-ref g i))]
    [else "???"]))

(set! $fasl-pbchunk! fasl-pbchunk!)

)

;; else not `pbchunk` feature:
(set-who! $fasl-pbchunk!
  (lambda args
    ($oops 'pbchunk-convert-file "not supported for this machine configuration"))))
