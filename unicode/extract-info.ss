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

(include "extract-common.ss")

(define code-point-limit #x110000)
(define-table (make-table table-ref table-set! table-ref-code)
  (make-vector vector-ref vector-set!)
  code-point-limit #x40 #x40)

(define (string-suffix? str suffix)
  (let ([n (string-length str)] [m (string-length suffix)])
    (and (fx>= n m) (string=? (substring str (fx- n m) n) suffix))))
       
(define (extract-range str)
  (define (find-char c s)
    (let f ([i 0] [n (string-length s)])
      (cond
        [(= i n) #f]
        [(char=? (string-ref s i) c) i]
        [else (f (+ i 1) n)])))
  (cond
    [(find-char #\. str) =>
     (lambda (i)
       (cons
         (hex->num (substring str 0 i))
         (hex->num (substring str (+ i 2) (string-length str)))))]
    [else (let ([n (hex->num str)]) (cons n n))]))

; fixnum field laid out as follows:
;   bits 0-5: category number
;   bits 6-9: wordbreak property
;   bits 10-17: combining class
;   bits 18-29: case/type property bits

(define-syntax define-bitfields
  (lambda (x)
    (define construct-name
      (lambda (template-identifier . args)
        (datum->syntax template-identifier
          (string->symbol
            (apply string-append
                   (map (lambda (x) (format "~a" (syntax->datum x)))
                        args))))))
    (define extract
      (lambda (fld* bit def*)
        (assert (< bit (fixnum-width)))
        (if (null? fld*)
            def*
            (syntax-case (car fld*) (flag enumeration integer)
              [(flag name) (identifier? #'name)
               (extract (cdr fld*) (+ bit 1)
                 #`((define name #,(fxsll 1 bit)) #,@def*))]
              [(enumeration name id ...)
               (and (identifier? #'name) (for-all identifier? #'(id ...)))
               (let ([width (bitwise-length (length #'(id ...)))])
                 (with-syntax ([name-shift (construct-name #'name #'name "-shift")]
                               [name-mask (construct-name #'name #'name "-mask")])
                   (extract (cdr fld*) (+ bit width)
                     #`((define name-shift #,bit)
                        (define name-mask #,(fx- (fxsll 1 width) 1))
                        #,@(map (lambda (id val) #`(define #,id #,val))
                                #'(id ...)
                                (enumerate #'(id ...)))
                        #,@def*))))]
              [(integer name width) (identifier? #'name)
               (let ([width (syntax->datum #'width)])
                 (with-syntax ([name-shift (construct-name #'name #'name "-shift")]
                               [name-mask (construct-name #'name #'name "-mask")])
                   (extract (cdr fld*) (+ bit width)
                     #`((define name-shift #,bit)
                        (define name-mask #,(fx- (fxsll 1 width) 1))
                        #,@def*))))]))))
    (syntax-case x ()
      [(_ fld ...)
       #`(begin #,@(extract #'(fld ...) 0 #'()))])))

(define-bitfields
  (flag cased-property)
  (flag case-ignorable-property)
  (flag constituent-property)
  (flag subsequent-property)
  (flag uppercase-property)
  (flag lowercase-property)
  (flag titlecase-property)
  (flag alphabetic-property)
  (flag numeric-property)
  (flag whitespace-property)
  (enumeration category Lu-cat Ll-cat Lt-cat Lm-cat Lo-cat
    Mn-cat Mc-cat Me-cat Nd-cat Nl-cat No-cat Pc-cat Pd-cat
    Ps-cat Pe-cat Pi-cat Pf-cat Po-cat Sm-cat Sc-cat Sk-cat
    So-cat Zs-cat Zl-cat Zp-cat Cc-cat Cf-cat Cs-cat Co-cat
    Cn-cat)
 ; default wb-other-property must be zero, so must be listed first
  (enumeration wbproperty wb-other-property wb-aletter-property
    wb-numeric-property wb-katakana-property
    wb-extend-property wb-format-property wb-midnum-property
    wb-midletter-property wb-midnumlet-property
    wb-extendnumlet-property wb-cr-property wb-lf-property
    wb-newline-property
    ; UNICODE 7.0.0
    wb-double-quote-property wb-single-quote-property
    wb-hebrew-letter-property wb-regional-indicator-property
    ; UNICODE 14.0
    wb-zwj-property wb-wsegspace-property)
  (integer combining-class 8))

;;; Uppercase = Lu + Other_Uppercase
;;; Lowercase = Ll + Other_Lowercase
;;; Titlecase = Lt
;;; Alphabetic = Lu + Ll + Lt + Lm + Lo + Nl + Other_Alphabetic 
;;; Numeric = ???
;;; White_Space = 

;;; cased property:
;;;   D135: A character C is defined to be cased if and only if C has the
;;;   Lowercase or Uppercase property or has a General_Category value of
;;;   Titlecase_Letter.
;;;
;;; case-ignorable property:
;;;   D136 A character C is defined to be case-ignorable if C has the
;;;   value MidLetter, MidNumLet, or Single_Quote for the Word_Break property
;;;   or its General_Category is one of Nonspacing_Mark (Mn),
;;;   Enclosing_Mark (Me), Format (Cf), Modifier_Letter (Lm), or
;;;   Modifier_Symbol (Sk).

(define name->wbprop
  (lambda (name)
    (case (string->symbol name)
      [(ALetter) (fxsll wb-aletter-property wbproperty-shift)]
      [(Numeric) (fxsll wb-numeric-property wbproperty-shift)]
      [(Katakana) (fxsll wb-katakana-property wbproperty-shift)]
      [(MidLetter) (fxior (fxsll wb-midletter-property wbproperty-shift) case-ignorable-property)]
      [(Extend) (fxsll wb-extend-property wbproperty-shift)]
      [(Format) (fxsll wb-format-property wbproperty-shift)]
      [(MidNum) (fxsll wb-midnum-property wbproperty-shift)]
      [(MidNumLet) (fxior (fxsll wb-midnumlet-property wbproperty-shift) case-ignorable-property)]
      [(ExtendNumLet) (fxsll wb-extendnumlet-property wbproperty-shift)]
      [(CR) (fxsll wb-cr-property wbproperty-shift)]
      [(LF) (fxsll wb-lf-property wbproperty-shift)]
      [(Newline) (fxsll wb-newline-property wbproperty-shift)]
      [(Double_Quote) (fxsll wb-double-quote-property wbproperty-shift)]
      [(Single_Quote) (fxior (fxsll wb-single-quote-property wbproperty-shift) case-ignorable-property)]
      [(Hebrew_Letter) (fxsll wb-hebrew-letter-property wbproperty-shift)]
      [(Regional_Indicator) (fxsll wb-regional-indicator-property wbproperty-shift)]
      [(ZWJ) (fxsll wb-zwj-property wbproperty-shift)]
      [(WSegSpace) (fxsll wb-wsegspace-property wbproperty-shift)]
      [else (errorf 'name->wbprop "unexpected property ~a" name)])))

(define proplist-properties
  `(["Other_Uppercase"  ,uppercase-property]
    ["Other_Lowercase"  ,lowercase-property]
    ["Other_Alphabetic" ,alphabetic-property]
    ["White_Space"      ,whitespace-property]))

(define categories
  ;;; 30 categories
  `([Lu ,(+ (fxsll Lu-cat category-shift) constituent-property uppercase-property alphabetic-property) "Letter, Uppercase"]
    [Ll ,(+ (fxsll Ll-cat category-shift) constituent-property lowercase-property alphabetic-property) "Letter, Lowercase"]
    [Lt ,(+ (fxsll Lt-cat category-shift) constituent-property titlecase-property alphabetic-property cased-property) "Letter, Titlecase"]
    [Lm ,(+ (fxsll Lm-cat category-shift) constituent-property alphabetic-property case-ignorable-property)  "Letter, Modifier"]
    [Lo ,(+ (fxsll Lo-cat category-shift) constituent-property alphabetic-property)  "Letter, Other"]
    [Mn ,(+ (fxsll Mn-cat category-shift) constituent-property case-ignorable-property)  "Mark, Nonspacing"]
    [Mc ,(+ (fxsll Mc-cat category-shift) subsequent-property)   "Mark, Spacing Combining"]
    [Me ,(+ (fxsll Me-cat category-shift) subsequent-property case-ignorable-property) "Mark, Enclosing"]
    [Nd ,(+ (fxsll Nd-cat category-shift) subsequent-property)   "Number, Decimal Digit"]
    [Nl ,(+ (fxsll Nl-cat category-shift) constituent-property alphabetic-property)  "Number, Letter"]
    [No ,(+ (fxsll No-cat category-shift) constituent-property)  "Number, Other"]
    [Pc ,(+ (fxsll Pc-cat category-shift) constituent-property)  "Punctuation, Connector"]
    [Pd ,(+ (fxsll Pd-cat category-shift) constituent-property)  "Punctuation, Dash"]
    [Ps ,(+ (fxsll Ps-cat category-shift) )                      "Punctuation, Open"]
    [Pe ,(+ (fxsll Pe-cat category-shift) )                      "Punctuation, Close"]
    [Pi ,(+ (fxsll Pi-cat category-shift) )                      "Punctuation, Initial quote"]
    [Pf ,(+ (fxsll Pf-cat category-shift) )                      "Punctuation, Final quote"]
    [Po ,(+ (fxsll Po-cat category-shift) constituent-property)  "Punctuation, Other"]
    [Sm ,(+ (fxsll Sm-cat category-shift) constituent-property)  "Symbol, Math"]
    [Sc ,(+ (fxsll Sc-cat category-shift) constituent-property)  "Symbol, Currency"]
    [Sk ,(+ (fxsll Sk-cat category-shift) constituent-property case-ignorable-property)  "Symbol, Modifier"]
    [So ,(+ (fxsll So-cat category-shift) constituent-property)  "Symbol, Other"]
    [Zs ,(+ (fxsll Zs-cat category-shift) )                      "Separator, Space"]
    [Zl ,(+ (fxsll Zl-cat category-shift) )                      "Separator, Line"]
    [Zp ,(+ (fxsll Zp-cat category-shift) )                      "Separator, Paragraph"]
    [Cc ,(+ (fxsll Cc-cat category-shift) )                      "Other, Control"]
    [Cf ,(+ (fxsll Cf-cat category-shift) case-ignorable-property)                      "Other, Format"]
    [Cs ,(+ (fxsll Cs-cat category-shift) )                      "Other, Surrogate"]
    [Co ,(+ (fxsll Co-cat category-shift) constituent-property)  "Other, Private Use"]
    [Cn ,(+ (fxsll Cn-cat category-shift) )                      "Other, Not Assigned"]
  ))

(define (category/flags x)
  (cond
    [(assq x categories) => cadr]
    [else (errorf 'category/flags "invalid cat ~s" x)]))

(define (make-cats-table ls)
  (let f ([i 1] [st (car ls)] [ls (cdr ls)] [ac '()])
    (cond
      [(null? ls) (reverse (cons (cons i st) ac))]
      [(equal? (cdar ls) (cdr st)) (f (+ i 1) st (cdr ls) ac)]
      [else (f 1 (car ls) (cdr ls) (cons (cons i st) ac))])))

; create table, placing all in category Cn until proven otherwise
(let ([tbl (make-table (category/flags 'Cn))])
  (define (setprop n prop) (table-set! tbl n prop))
  (define (getprop n) (table-ref tbl n))
  ;;; interesting parts of each element in UnicodeData.txt are:
  ;;; field0: the character index, numeric
  ;;; field1: the description, possibly with First or Last marker
  ;;; field2: the category, symbolic
  ;;; field3: the combining class (0-255)
  ;;; field8: if set, then the char has the numeric property
  ;;; field12: if set, then the char has upper-case mapping and is thus cased
  ;;; field13: if set, then the char has lower-case mapping and is thus cased
  (let f ([ls (get-unicode-data "UNIDATA/UnicodeData.txt")])
    (unless (null? ls)
      (let ([x (car ls)] [ls (cdr ls)])
        (let ([n (hex->num (list-ref x 0))]
              [cclass (string->number (list-ref x 3))]
              [cat/flags (category/flags (string->symbol (list-ref x 2)))]
              [num (if (string=? (list-ref x 8) "") 0 numeric-property)]
              [cased (if (and (string=? (list-ref x 12) "") (string=? (list-ref x 13) ""))
                         0 cased-property)])
          (let ([props (fxior num cased
                         (fxsll cclass combining-class-shift)
                         cat/flags)])
            (if (string-suffix? (list-ref x 1) "First>")
                (let ([y (car ls)] [ls (cdr ls)])
                  (unless (string-suffix? (list-ref y 1) "Last>")
                    (errorf #f "expected entry marked Last following entry marked First for ~x" n))
                  (let ([m (hex->num (list-ref y 0))])
                    (do ([n n (fx+ n 1)])
                        ((fx> n m))
                      (setprop n props)))
                  (f ls))
                (begin (setprop n props) (f ls))))))))
  ;;; interesting parts of each element in WordBreakProperty.txt are:
  ;;; field0: the character index, numeric
  ;;; field1: the word-break property
  (for-each
    (lambda (x)
      (let ([range (extract-range (list-ref x 0))])
        (let f ([i (car range)] [j (cdr range)])
          (unless (> i j)
            (let ([prop (getprop i)])
              (unless (fx= (fxand (fxsrl prop wbproperty-shift) wbproperty-mask) 0)
                (errorf #f "multiple word break properties found for ~x" i))
              (setprop i (fxior prop (name->wbprop (list-ref x 1))))
              (f (+ i 1) j))))))
    (get-unicode-data "UNIDATA/WordBreakProperty.txt"))
  ;;; interesting parts of each element in PropList.txt are:
  ;;; field0: range of character indices
  ;;; field1: property name
  (for-each
    (lambda (x)
      (let ([range (extract-range (list-ref x 0))]
            [name (list-ref x 1)])
        (cond
          [(assoc name proplist-properties) =>
           (lambda (a) 
             (let ([n (cadr a)])
               (let f ([i (car range)] [j (cdr range)])
                 (unless (> i j) 
                   (setprop i (fxlogor (getprop i) n))
                   (f (+ i 1) j)))))])))
    (get-unicode-data "UNIDATA/PropList.txt"))
  ;;; clear constituent property for first 128 characters
  (do ([i 0 (fx+ i 1)])
      ((fx= i 128))
    (setprop i (fxand (getprop i) (fxnot constituent-property))))
  (commonize* tbl)
  (with-output-to-file* "unicode-charinfo.ss"
    (lambda () 
      (parameterize ([print-graph #t] [print-vector-length #f])
        (pretty-print
          `(module ($char-constituent? $char-subsequent? $char-upper-case? $char-lower-case? $char-title-case? $char-alphabetic?
                    $char-numeric? $char-whitespace? $char-cased? $char-case-ignorable? $char-category
                    $wb-aletter? $wb-numeric? $wb-katakana? $wb-extend? $wb-format? $wb-midnum? $wb-midletter?
                    $wb-midnumlet? $wb-extendnumlet? $char-combining-class $char-dump
                    ; UNICODE 7.0.0
                    $wb-hebrew-letter? $wb-single-quote? $wb-double-quote? $wb-regional-indicator?
                    ; UNICODE 14.0
                    $wb-zwj? $wb-wsegspace?)
             (define category-mask ,category-mask)
             (define unicode-category-table ',tbl)
             (define unicode-category-names
               ',(list->vector (map car categories)))
             (define table-ref ,table-ref-code)
             (define (getprop n) (table-ref unicode-category-table n))
             (define $char-constituent?
               (lambda (c)
                 (fxlogtest (getprop (char->integer c)) ,constituent-property)))
             (define $char-subsequent?
               (lambda (c)
                 (fxlogtest (getprop (char->integer c)) ,subsequent-property)))
             (define $char-upper-case?
               (lambda (c)
                 (fxlogtest (getprop (char->integer c)) ,uppercase-property)))
             (define $char-lower-case?
               (lambda (c)
                 (fxlogtest (getprop (char->integer c)) ,lowercase-property)))
             (define $char-title-case?
               (lambda (c)
                 (fxlogtest (getprop (char->integer c)) ,titlecase-property)))
             (define $char-alphabetic?
               (lambda (c)
                 (fxlogtest (getprop (char->integer c)) ,alphabetic-property)))
             (define $char-numeric?
               (lambda (c)
                 (fxlogtest (getprop (char->integer c)) ,numeric-property)))
             (define $char-whitespace?
               (lambda (c)
                 (fxlogtest (getprop (char->integer c)) ,whitespace-property)))
             (define $char-cased?
               (lambda (c)
                 (fxlogtest (getprop (char->integer c)) ,cased-property)))
             (define $char-case-ignorable?
               (lambda (c)
                 (fxlogtest (getprop (char->integer c)) ,case-ignorable-property)))
             (define (wb prop)
               (lambda (c)
                 (fx= (fxand
                        (fxsrl
                          (getprop (char->integer c))
                          ,wbproperty-shift)
                        ,wbproperty-mask)
                      prop)))
             (define $wb-aletter? (wb ,wb-aletter-property))
             (define $wb-numeric? (wb ,wb-numeric-property))
             (define $wb-katakana? (wb ,wb-katakana-property))
             (define $wb-extend? (wb ,wb-extend-property))
             (define $wb-format? (wb ,wb-format-property))
             (define $wb-midnum? (wb ,wb-midnum-property))
             (define $wb-midletter? (wb ,wb-midletter-property))
             (define $wb-midnumlet? (wb ,wb-midnumlet-property))
             (define $wb-extendnumlet? (wb ,wb-extendnumlet-property))
             (define $wb-hebrew-letter? (wb ,wb-hebrew-letter-property))
             (define $wb-double-quote? (wb ,wb-double-quote-property))
             (define $wb-single-quote? (wb ,wb-single-quote-property))
             (define $wb-regional-indicator? (wb ,wb-regional-indicator-property))
             (define $wb-zwj? (wb ,wb-zwj-property))
             (define $wb-wsegspace? (wb ,wb-wsegspace-property))
             (define $char-combining-class
               (lambda (c)
                 (fxand (fxsrl (getprop (char->integer c)) ,combining-class-shift)
                        ,combining-class-mask)))
             (define $char-category
               (lambda (c)
                 (vector-ref unicode-category-names
                   (fxand (fxsrl (getprop (char->integer c)) ,category-shift)
                          ,category-mask))))
             (define $char-dump
               (lambda (c)
                 (define (list-true . args) (remq #f args))
                 (list-true
                   (and ($char-constituent? c) 'constituent)
                   (and ($char-subsequent? c) 'subsequent)
                   (and ($char-upper-case? c) 'upper-case)
                   (and ($char-lower-case? c) 'lower-case)
                   (and ($char-title-case? c) 'title-case)
                   (and ($char-alphabetic? c) 'alphabetic)
                   (and ($char-numeric? c) 'whitespace)
                   (and ($char-whitespace? c) 'whitespace)
                   (and ($char-cased? c) 'cased)
                   (and ($char-case-ignorable? c) 'case-ignorable)
                   (and ($wb-aletter? c) 'aletter)
                   (and ($wb-numeric? c) 'numeric)
                   (and ($wb-katakana? c) 'katakana)
                   (and ($wb-extend? c) 'extend)
                   (and ($wb-format? c) 'format)
                   (and ($wb-midnum? c) 'midnum)
                   (and ($wb-midletter? c) 'midletter)
                   (and ($wb-midnumlet? c) 'midnumlet)
                   (and ($wb-extendnumlet? c) 'extendnumlet)
                   (and ($wb-hebrew-letter? c) 'hebrew-letter)
                   (and ($wb-double-quote? c) 'double-quote)
                   (and ($wb-single-quote? c) 'single-quote)
                   (and ($wb-regional-indicator? c) 'regional-indicator)
                   (and ($wb-zwj? c) 'zwj)
                   (and ($wb-wsegspace? c) 'wsegspace)
                   `(combining-class ,($char-combining-class c))
                   ($char-category c))))))))))

(printf "Happy Happy Joy Joy ~s\n" (sizeof cache))
