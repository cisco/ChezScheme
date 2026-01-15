;;; Copyright (C) 2008  Abdulaziz Ghuloum, R. Kent Dybvig
;;; Copyright (C) 2006,2007  Abdulaziz Ghuloum
;;; 
;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:
;;; 
;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.
;;; 
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE. 

(import (scheme) (unicode-data))

; dropping support for s16 inner vectors for now
(include "extract-common.ss")

(define code-point-limit #x110000) ; as of Unicode 5.1
#;(define table-limit #x30000)
(define table-limit code-point-limit)
(define-table (make-table table-ref table-set! table-ref-code)
  (make-vector vector-ref vector-set!)
  table-limit #x40 #x40)

(define-record-type chardata
  (fields (immutable ucchar)
          (immutable lcchar)
          (immutable tcchar)
          (mutable fcchar)
          (mutable ucstr)
          (mutable lcstr)
          (mutable tcstr)
          (mutable fcstr)
          (immutable decomp-canon)
          (immutable decomp-compat)
          (mutable grapheme-cluster-break)
          (mutable indic-conjunct-break)
          (mutable extended-pictographic?))
  (protocol
    (lambda (new)
      (lambda (ucchar lcchar tcchar decomp-canon decomp-compat)
        (new ucchar lcchar tcchar 0 ucchar lcchar tcchar 0
             decomp-canon decomp-compat 0 0 #f)))))

(define (find-cdrec idx ls)
  (cond
    [(assq idx ls) => cdr]
    [else (errorf 'find-cdrec "~s is missing" idx)]))

(define data-case
  (lambda (fields)
    (let ([n (hex->num (car fields))]
          [uc (list-ref fields 12)]
          [lc (list-ref fields 13)]
          [tc (list-ref fields 14)])
      (define (f x) (if (string=? x "") 0 (- (hex->num x) n)))
      (cons n (make-chardata (f uc) (f lc) (f tc)
                (parse-decomp n (list-ref fields 5) #f)
                (parse-decomp n (list-ref fields 5) #t))))))

(define (split str)
  (remove ""
    (let f ([i 0] [n (string-length str)])
      (cond
        [(= i n) (list (substring str 0 n))]
        [(char=? (string-ref str i) #\space)
         (cons (substring str 0 i) 
               (split (substring str (+ i 1) n)))]
        [else (f (add1 i) n)]))))

(define (improperize ls)
  (cond
    [(null? (cdr ls)) (car ls)]
    [else (cons (car ls) (improperize (cdr ls)))]))

(define (c*->off c* n)
  (if (= (length c*) 1)
      (- (car c*) n)
      (improperize (map integer->char c*))))

(define (codes->off str n)
  (c*->off (map hex->num (split str)) n))

;;; decomposition field looks like:
;;;    hex-value*
;;;    <tag> hex-value*
;;; latter appear to be for compatibility decomposition only
(define (parse-decomp n str compat?)
  (let f ([ls (split str)])
    (cond
      [(null? ls) 0]
      [(char=? (string-ref (car ls) 0) #\<)
       (if compat? (c*->off (map hex->num (cdr ls)) n) 0)]
      [else (c*->off (map hex->num ls) n)])))

(define (insert-foldcase-data! ls data)
  (for-each 
    (lambda (fields)
      (let ([n (hex->num (car fields))])
        (let ([cdrec (find-cdrec n ls)]
              [offset (codes->off (caddr fields) n)])
          (chardata-fcchar-set! cdrec offset)
          (chardata-fcstr-set! cdrec offset))))
    (filter (lambda (fields) (member (cadr fields) '("C" "S"))) data))
  (for-each 
    (lambda (fields)
      (let ([n (hex->num (car fields))])
        (chardata-fcstr-set!
          (find-cdrec n ls)
          (codes->off (caddr fields) n))))
    (filter (lambda (fields) (equal? (cadr fields) "F")) data)))

(define (insert-specialcase-data! ls data)
  (for-each
    (lambda (fields)
      (let ([n (hex->num (car fields))])
        (let ([cdrec (find-cdrec n ls)])
          (chardata-lcstr-set! cdrec (codes->off (list-ref fields 1) n))
          (chardata-tcstr-set! cdrec (codes->off (list-ref fields 2) n))
          (chardata-ucstr-set! cdrec (codes->off (list-ref fields 3) n)))))
    (filter
      (lambda (fields) (= 0 (string-length (list-ref fields 4))))
      data)))

(define grapheme-cluster-break-props '(Other
                                       CR
                                       LF
                                       Control
                                       Extend
                                       ZWJ
                                       Regional_Indicator
                                       Prepend
                                       SpacingMark
                                       L
                                       V
                                       T
                                       LV
                                       LVT))

(define indic-conjunct-break-props '(None
                                     Consonant
                                     Extend
                                     Linker))

;; encoding for per-character information needed by `char-grapheme-step`
(define extended-pictographic-shift (integer-length (sub1 (length grapheme-cluster-break-props))))
(define extended-pictographic-bit (fxsll 1 extended-pictographic-shift))
(define indic-conjunct-break-shift (add1 extended-pictographic-shift))
(define grapheme-cluster-break-mask (sub1 (fxsll 1 (sub1 indic-conjunct-break-shift))))

;; extends per-character information with current state, forming the full input
;; to the `char-grapheme-step` table lookup
(define grapheme-break-state-shift (+ indic-conjunct-break-shift (integer-length (sub1 (length indic-conjunct-break-props)))))

;; encoding of `char-grapheme-step` table result
(define grapheme-break-step-terminated-bit 1)
(define grapheme-break-step-state-shift 1)

(define (chardata-grapheme-break cd)
  (fxior (chardata-grapheme-cluster-break cd)
         (if (chardata-extended-pictographic? cd)
             extended-pictographic-bit
             0)
         (fxsll (chardata-indic-conjunct-break cd)
                indic-conjunct-break-shift)))

(define (find-prop-index str props)
  (let loop ([i 0] [ls props] [s (string->symbol str)])
    (cond
     [(null? ls) (errorf #f "invalid property name: ~a" s)]
     [(eq? s (car ls)) i]
     [else (loop (fx+ i 1) (cdr ls) s)])))

(define (insert-grapheme-cluster-break-data! ls data)
  (for-each
   (lambda (fields)
     (let ([val (find-prop-index (list-ref fields 1) grapheme-cluster-break-props)])
       (for-each-hex-in-range (car fields)
         (lambda (i)
           (unless (assq i ls)
             (let ([cdrec (make-chardata 0 0 0 0 0)])
               (set! ls (cons (cons i cdrec) ls))))
           (chardata-grapheme-cluster-break-set! (find-cdrec i ls) val)))))
   data)
  ls)

(define (insert-indic-conjunct-break-data! ls data)
  (for-each
   (lambda (fields)
     (when (equal? (list-ref fields 1) "InCB")
       (let ([val (find-prop-index (list-ref fields 2) indic-conjunct-break-props)])
         (for-each-hex-in-range (car fields)
           (lambda (i)
             (chardata-indic-conjunct-break-set! (find-cdrec i ls) val))))))
   data))

;; although extended-pictographic information is also available via the table
;; created by "extract-info.ss",  we need it here for the `char-grapheme-step` tables
(define (insert-extended-pictographic-data! ls data)
  (for-each
   (lambda (x)
     (when (equal? (list-ref x 1) "Extended_Pictographic")
       (for-each-hex-in-range (list-ref x 0)
         (lambda (i)
           (unless (assq i ls)
             (let ([cdrec (make-chardata 0 0 0 0 0)])
               (set! ls (cons (cons i cdrec) ls))))
           (chardata-extended-pictographic?-set! (find-cdrec i ls) #t)))))
   data)
  ls)


(define (build-grapheme-step-table ls)
  ;; We encode the state of finding cluster boundaries as a fixnum:
  ;; - Low bits are the previous character's grapheme-break property plus one
  ;; - Two bits for the GB9c Indic_Conjunct_Break match state:
  ;;   - 0: nothing
  ;;   - 1: Consonant Extend*
  ;;   - 2: Consonant Extend* Linker (Extend|Linker)*
  ;; - Two bits for the GB11 Extended_Pictographic match state:
  ;;   - 0: nothing
  ;;   - 1: Extended_Pictographic Extend*
  ;;   - 2: Extended_Pictographic Extend* ZWJ
  ;;
  ;; A 0 state is treated as a previous property that doesn't match anything
  ;; (and that's why we add one to the previous character's property
  ;; otherwise).
  ;;
  ;; Use 0 for the state for the start of a sequence.
  ;;
  ;; The result of taking a step is two values:
  ;;   * a boolean indicating whether a cluster end was found
  ;;   * a new state
  ;;
  ;; The result state is 0 only if the character sent in is consumed
  ;; as part of a cluster (in which case the first result will be #t).
  ;; Otherwise, a true first result indicates that a boundary was
  ;; found just before the provided character (and the provided character's
  ;; grapheme end is still pending).
  ;;
  ;; So, if you get to the end of a string with a non-0 state, then
  ;; "flush" the state by consuming that last grapheme cluster.
  (define grapheme-cluster-break-bits (bitwise-length (length grapheme-cluster-break-props)))

  (define GB9c-none 0)
  (define GB9c-one 1)
  (define GB9c-two 2)

  (define GB11-none 0)
  (define GB11-one 1)
  (define GB11-two 2)

  (define Other (find-prop-index "Other" grapheme-cluster-break-props))

  (define (encode-state prop-n GB9c-state GB11-state)
    (fxior (fx+ prop-n 1)
           (fxsll GB9c-state grapheme-cluster-break-bits)
           (fxsll GB11-state (fx+ grapheme-cluster-break-bits 2))))

  (define (decode-state state)
    (values
     (fx- (fxand state (fx- (fxsll 1 grapheme-cluster-break-bits) 1)) 1)
     (fxand (fxsrl state grapheme-cluster-break-bits) #b11)
     (fxsrl state (fx+ grapheme-cluster-break-bits 2))))

  (define initial-state 0)

  (define (grapheme-next-state GB9c-state GB11-state prop prop-n incb-type extended-pictographic?)
    (encode-state
     prop-n
     (cond
       [(fx= GB9c-state GB9c-none)
        (cond
          [(eq? incb-type 'Consonant) GB9c-one]
          [else GB9c-none])]
       [(fx= GB9c-state GB9c-one) ; Consonant Extend*
        (cond
          [(eq? incb-type 'Linker) GB9c-two]
          [(eq? incb-type 'None) GB9c-none]
          [else GB9c-one])]
       [else ; Consonant Extend* Linker (Extend|Linker)*
        (cond
          [(eq? incb-type 'Consonant) GB9c-one]
          [(eq? incb-type 'None) GB9c-none]
          [else GB9c-two])])
     (cond
       [(fx= GB11-state GB11-none)
        (cond
          [extended-pictographic? GB11-one]
          [else GB11-none])]
       [(fx= GB11-state GB11-one) ; Extended_Pictographic Extend*
        (cond
          [(eq? prop 'ZWJ) GB11-two]
          [(eq? prop 'Extend) GB11-one]
          [extended-pictographic? GB11-one]
          [else GB11-none])]
       [else                     ; Extended_Pictographic Extend* ZWJ
        (cond
          [extended-pictographic? GB11-one]
          [else GB11-none])])))

  (define (grapheme-step cd state)
    (let-values ([(prev-n GB9c-state GB11-state) (decode-state state)])
      (let* ([br (chardata-grapheme-break cd)]
             [prop-n (fxand br grapheme-cluster-break-mask)]
             [prev (and (>= prev-n 0)
                        (list-ref grapheme-cluster-break-props prev-n))]
             [prop (list-ref grapheme-cluster-break-props prop-n)]
             [extended-pictographic? (fx= extended-pictographic-bit (fxand extended-pictographic-bit br))]
             [incb-type-n (fxsrl br indic-conjunct-break-shift)]
             [incb-type (list-ref indic-conjunct-break-props incb-type-n)])

        (define (next-state)
          (grapheme-next-state GB9c-state GB11-state prop prop-n incb-type extended-pictographic?))

        ;; These are the rules from UAX #29.
        ;; GB1 and GB2 are implicit and external to this stepping function.
        (cond
          ;; some of GB999 as common case;
          ;; a variant of this is inlined in unsafe mode
          [(and (eq? prev 'Other)
                (eq? prop 'Other)
                (eq? incb-type 'None)
                (not extended-pictographic?))
           (values #t (encode-state Other GB9c-none GB11-none))]
          ;; some of GB3 and some of GB4
          [(eq? prev 'CR)
           (if (eq? prop 'LF)
               (values #t initial-state)
               (values #t (next-state)))]
          ;; some of GB3 and some of GB5
          [(eq? prop 'CR)
           (values (fx> state 0) (next-state))]
          ;; rest of GB4
          [(or (eq? prev 'Control)
               (eq? prev 'LF))
           (values #t (next-state))]
          ;; rest of GB5
          [(or (eq? prop 'Control)
               (eq? prop 'LF))
           (values #t (if (fx= state 0)
                          initial-state
                          (next-state)))]
          ;; GB6
          [(and (eq? prev 'L)
                (or (eq? prop 'L)
                    (eq? prop 'V)
                    (eq? prop 'LV)
                    (eq? prop 'LVT)))
           (values #f (next-state))]
          ;; GB7
          [(and (or (eq? prev 'LV)
                    (eq? prev 'V))
                (or (eq? prop 'V)
                    (eq? prop 'T)))
           (values #f (next-state))]
          ;; GB8
          [(and (or (eq? prev 'LVT)
                    (eq? prev 'T))
                (eq? prop 'T))
           (values #f (next-state))]
          ;; GB9
          [(or (eq? prop 'Extend)
               (eq? prop 'ZWJ))
           (values #f (next-state))]
          ;; GB9a
          [(eq? prop 'SpacingMark)
           (values #f (next-state))]
          ;; GB9b
          [(eq? prev 'Prepend)
           (values #f (next-state))]
          ;; GB9c
          [(and (fx= GB9c-state GB9c-two)
                (eq? incb-type 'Consonant))
           (values #f (next-state))]
          ;; GB11
          [(and (fx= GB11-state GB11-two)
                extended-pictographic?)
           (values #f (next-state))]
          ;; GB12 and GB13
          [(eq? prev 'Regional_Indicator)
           (if (eq? prop 'Regional_Indicator)
               (values #f (encode-state Other GB9c-none GB11-none))
               (values #t (next-state)))]
          ;; GB999
          [else
           (values (fx> state 0) (next-state))]))))

  (define (build-input state br)
    (fxior br
           (fxsll state grapheme-break-state-shift)))

  (let ([states (make-eqv-hashtable)]
        [transitions (make-eqv-hashtable)])
    (hashtable-set! states 0 0)
    (let loop ()
      (define added? #f)
      (for-each (lambda (x)
                  (define cd (cdr x))
                  (vector-for-each (lambda (state)
                                     (let-values ([(terminated? new-state) (grapheme-step cd state)])
                                       (unless (hashtable-ref states new-state #f)
                                         (set! added? #t)
                                         (hashtable-set! states new-state new-state))
                                       (hashtable-set! transitions
                                                       (build-input state (chardata-grapheme-break cd))
                                                       (fxior (fxsll new-state grapheme-break-step-state-shift)
                                                              (if terminated? grapheme-break-step-terminated-bit 0)))))
                                   (hashtable-keys states)))
                ls)
      (when added?
        (loop)))
    (values (vector->list (hashtable-cells transitions))
            (encode-state Other GB9c-none GB11-none))))

(define verify-identity!
  (lambda (n cdrec)
    (define (zeros? . args) (andmap (lambda (x) (eqv? x 0)) args))
    (unless (zeros? (chardata-ucchar cdrec)
                    (chardata-lcchar cdrec)
                    (chardata-tcchar cdrec)
                    (chardata-fcchar cdrec)
                    (chardata-ucstr cdrec)
                    (chardata-lcstr cdrec)
                    (chardata-tcstr cdrec)
                    (chardata-fcstr cdrec)
                    (chardata-decomp-canon cdrec)
                    (chardata-decomp-compat cdrec))
      (errorf 'verify-identity "failed for ~x, ~s" n cdrec))))

(define build-uncommonized-table
  (lambda (acc ls)
    (let ([table (make-table 0)])
      (for-each 
        (lambda (x)
          (let ([n (car x)] [cdrec (cdr x)])
            (unless (< n code-point-limit)
              (errorf 'build-table
                "code point value ~s is at or above declared limit ~s"
                n code-point-limit))
            (if (>= n table-limit)
                (verify-identity! n cdrec)
                (table-set! table n (acc cdrec)))))
        ls)
      table)))

(define build-table
  (lambda (acc ls)
    (commonize* (build-uncommonized-table acc ls))))

(define (get-composition-pairs decomp-canon-table)
  (define ($str-decomp-canon c) 
    (define (strop tbl c)
      (let ([n (char->integer c)])
        (if (and (fx< table-limit code-point-limit)
                 (fx>= n table-limit))
            c
            (let ([x (table-ref tbl n)])
              (if (fixnum? x)
                  (integer->char (fx+ x n))
                  x)))))
    (strop decomp-canon-table c))
  (let ([exclusions 
         (map hex->num
           (map car (get-unicode-data
                      "UNIDATA/CompositionExclusions.txt")))]
        [from* '()]
        [to* '()])
    (define (enter i)
      (unless (memv i exclusions)
        (let* ([c (integer->char i)]  [c* ($str-decomp-canon c)])
          (when (pair? c*)
            (set! from* (cons c* from*))
            (set! to* (cons c to*))))))
    (do ([i 0 (fx+ i 1)]) ((fx= i #xD800)) (enter i))
    (do ([i #xE000 (fx+ i 1)]) ((fx= i code-point-limit)) (enter i))
    (commonize* (cons (list->vector from*) (list->vector to*)))))

(let ([ls (map data-case (get-unicode-data "UNIDATA/UnicodeData.txt"))])
  (insert-foldcase-data! ls (get-unicode-data "UNIDATA/CaseFolding.txt"))
  (insert-specialcase-data! ls (get-unicode-data "UNIDATA/SpecialCasing.txt"))
  (let* ([ls (insert-grapheme-cluster-break-data! ls
              (get-unicode-data "UNIDATA/GraphemeBreakProperty.txt"))]
         [ls (insert-extended-pictographic-data! ls
               (get-unicode-data "UNIDATA/emoji-data.txt"))])
    (insert-indic-conjunct-break-data! ls (get-unicode-data "UNIDATA/DerivedCoreProperties.txt"))
   ; insert final sigma flag for char-downcase conversion
    (chardata-lcstr-set! (find-cdrec #x3a3 ls) 'sigma)
    (let-values ([(step-table grapheme-other-state) (build-grapheme-step-table ls)])
     (with-output-to-file* "unicode-char-cases.ss"
      (lambda ()
       (parameterize ([print-graph #t] [print-vector-length #f] [print-unicode #f])
        (pretty-print
          `(module ($char-upcase $char-downcase $char-titlecase $char-foldcase
                    $str-upcase $str-downcase $str-titlecase $str-foldcase
                    $str-decomp-canon $str-decomp-compat
                    $char-grapheme-break
                    $char-grapheme-break-property
                    $char-indic-conjunct-break-property
                    $char-grapheme-step-lookup
                    $composition-pairs
                    grapheme-break-step-terminated-bit
                    grapheme-break-step-state-shift
                    grapheme-other-state)
             (define char-upcase-table ',(build-table chardata-ucchar ls))
             (define char-downcase-table ',(build-table chardata-lcchar ls))
             (define char-titlecase-table ',(build-table chardata-tcchar ls))
             (define char-foldcase-table ',(build-table chardata-fcchar ls))
             (define string-upcase-table ',(build-table chardata-ucstr ls))
             (define string-downcase-table ',(build-table chardata-lcstr ls))
             (define string-titlecase-table ',(build-table chardata-tcstr ls))
             (define string-foldcase-table ',(build-table chardata-fcstr ls))
             (define decomp-canon-table ',(build-table chardata-decomp-canon ls))
             (define decomp-compat-table ',(build-table chardata-decomp-compat ls))
             (define grapheme-and-indic-break-table ',(build-table chardata-grapheme-break ls))
             (define grapheme-step-table ',(build-table values step-table))

             (define grapheme-break-step-terminated-bit ,grapheme-break-step-terminated-bit)
             (define grapheme-break-step-state-shift ,grapheme-break-step-state-shift)
             (define grapheme-other-state ,grapheme-other-state)

             (define table-limit ,table-limit)
             (define code-point-limit ,code-point-limit)
             (define table-ref ,table-ref-code)
             (define (charop tbl c)
               (let ([n (char->integer c)])
                 (if (and (fx< table-limit code-point-limit)
                          (fx>= n table-limit))
                     c
                     (integer->char (fx+ (table-ref tbl n) n)))))
             (define (strop tbl c)
               (let ([n (char->integer c)])
                 (if (and (fx< table-limit code-point-limit)
                          (fx>= n table-limit))
                     c
                     (let ([x (table-ref tbl n)])
                       (if (fixnum? x)
                           (integer->char (fx+ x n))
                           x)))))
             (define (intop tbl c)
               (let ([n (char->integer c)])
                 (if (and (fx< table-limit code-point-limit)
                          (fx>= n table-limit))
                     0
                     (table-ref tbl n))))
             (define ($char-upcase c) (charop char-upcase-table c))
             (define ($char-downcase c) (charop char-downcase-table c))
             (define ($char-titlecase c) (charop char-titlecase-table c))
             (define ($char-foldcase c) (charop char-foldcase-table c))
             (define ($str-upcase c) (strop string-upcase-table c))
             (define ($str-downcase c) (strop string-downcase-table c))
             (define ($str-titlecase c) (strop string-titlecase-table c))
             (define ($str-foldcase c) (strop string-foldcase-table c))
             (define ($str-decomp-canon c) (strop decomp-canon-table c))
             (define ($str-decomp-compat c) (strop decomp-compat-table c))
             (define ($char-grapheme-break c) (intop grapheme-and-indic-break-table c))
             (define ($char-grapheme-break-property c)
               (vector-ref ',(list->vector grapheme-cluster-break-props)
                 (fxand ($char-grapheme-break c) ,grapheme-cluster-break-mask)))
             (define ($char-indic-conjunct-break-property c)
               (vector-ref ',(list->vector indic-conjunct-break-props)
                 (fxsrl ($char-grapheme-break c) ,indic-conjunct-break-shift)))
             (define ($char-grapheme-step-lookup br state)
               (table-ref grapheme-step-table (fxior br (fxsll state ,grapheme-break-state-shift))))
             (define ($composition-pairs)
               ',(get-composition-pairs
                   (build-uncommonized-table chardata-decomp-canon ls)))))))))))

(printf "Happy Happy Joy Joy ~a\n" (sizeof cache))
