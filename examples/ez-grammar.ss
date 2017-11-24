;;; Copyright 2017 Cisco Systems, Inc.
;;; 
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;; 
;;; http://www.apache.org/licenses/LICENSE-2.0
;;; 
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

;;; See http://www.cs.nott.ac.uk/~pszgmh/monparsing.pdf for origins of 
;;; some of the monadic combinators.

;;; Authors: Jon Rossie, Kent Dybvig

;;; The define-grammar form produces a parser:
;;;
;;;   parser : token-stream -> ((Tree token-stream) ...)
;;;
;;; If the return value is the empty list, a parse error occurred.
;;; If the return value has multiple elements, the parse was ambiguous.
;;; The token-stream in each (Tree token-stream) is the tail of the
;;; input stream that begins with the last token consumed by the parse.
;;; This gives the consumer access to both the first and last token,
;;; allowing it to determine cheaply the extent of the parse, including
;;; source locations if source information is attached to the tokens.

;;; Internally, backtracking occurs whenever a parser return value
;;; has multiple elements.

;;; This code should be included into a lexical context that supplies:
;;;
;;;    token-bfp : token -> token's beginning file position
;;;    token-efp : token -> token's ending file position
;;;    meta constant? : syntax-object -> boolean
;;;    sep->parser : sep -> parser
;;;    constant->parser : constant -> parser
;;;    make-src : bfp x efp -> src.  bfp > efp => no tokens consumed.
;;;
;;; See ez-grammar-test.ss for an example.

(module (define-grammar
          is sat item peek seq ++ +++ many many+ ?
          parse-consumed-all? parse-result-value parse-result-unused
          grammar-trace
          )
  (import (streams))

  (define grammar-trace (make-parameter #f))

  (define-record-type parse-result
    (nongenerative parse-result)
    (sealed #t)
    (fields value unused))

  ;; to enable $trace-is to determine the ending file position (efp) of a parse
  ;; form, the input stream actually points to the preceding token rather than
  ;; to the current token.  the next few routines establish, maintain, and deal
  ;; with that invariant.
  (define make-top-level-parser
    (lambda (parser)
      (lambda (inp)
        (parser (stream-cons 'dummy-token inp)))))

  (define preceding-token
    (lambda (inp)
      (stream-car inp)))

  (define current-token
    (lambda (inp)
      (stream-car (stream-cdr inp))))

  (define remaining-tokens
    (lambda (inp)
      (stream-cdr inp)))

  (define no-more-tokens?
    (lambda (inp)
      (stream-null? (stream-cdr inp))))

  (define parse-consumed-all?
    (lambda (res)
      (no-more-tokens? (parse-result-unused res))))

  ;; A parser generator
  (define result
    (lambda (v)
      ;; this is a parser that ignores its input and produces v
      (lambda (inp)
        (stream (make-parse-result v inp)))))

  ;; A parse that always generates a parse error
  (define zero
    (lambda (inp)
      stream-nil))

  ;; For a non-empty stream, successfully consume the first element
  (define item
    (lambda (inp)
      (cond
        [(no-more-tokens? inp) '()]
        [else
          (stream (make-parse-result (current-token inp) (remaining-tokens inp)))])))

  (define (peek p)
    (lambda (inp)
      (stream-map (lambda (pr)
                    (make-parse-result (parse-result-value pr) inp))
        (p inp))))

  ;;------------------------------------------

  (define bind
    (lambda (parser receiver)
      (lambda (inp)
        (let ([res* (parser inp)])
          (stream-append-all
            (stream-map (lambda (res)
                          ((receiver (parse-result-value res))
                           (parse-result-unused res)))
              res*))))))

  ;; monad comprehensions
  (define-syntax is-where ; used by is and trace-is
    (lambda (x)
      (syntax-case x (where <-)
        [(_ expr (where)) #'expr]
        [(_ expr (where [x <- p] clauses ...))
         #'(bind p (lambda (x) (is-where expr (where clauses ...))))]
        [(_ expr (where pred clauses ...))
         #'(if pred (is-where expr (where clauses ...)) zero)]
        [(_ expr where-clause) (syntax-error #'where-clause)])))
  (indirect-export is-where bind)

  (define-syntax is
    (syntax-rules ()
      [(_ expr where-clause) (is-where (result expr) where-clause)]))
  (indirect-export is is-where)

  (module (trace-is)
    (define ($trace-is name proc head)
      (lambda (unused)
        (let ([res (proc (token-bfp (current-token head)) (token-efp (preceding-token unused)))])
          (when (and name (grammar-trace)) (printf "<<~s = ~s~%" name res))
          (stream (make-parse-result res unused)))))

    (define-syntax trace-is
      (syntax-rules ()
        [(_ name proc-expr where-clause)
         (lambda (inp) ((is-where ($trace-is 'name proc-expr inp) where-clause) inp))]))
    (indirect-export trace-is $trace-is))

  (define (seq2 p q) (is (cons x y) (where [x <- p] [y <- q])))

  (define seq
    (lambda p*
      (let loop ([p* p*])
        (cond
          [(null? p*) (result '())]
          [else (seq2 (car p*) (loop (cdr p*)))]))))

  (define (sat pred) (is x (where [x <- item] (pred x))))

  (define ++ ;; introduce ambiguity
    (lambda (p q)
      (lambda (inp)
        (stream-append2 (p inp)
          (lambda ()
            (q inp))))))

  (define (many+ p) (is (cons x xs) (where [x <- p] [xs <- (many p)])))

  (define (many p) (++ (many+ p) (result '())))

  (define (? p) (++ (sat p) (result #f)))

  (define (sepby1 p sep)
    (is (cons x xs)
      (where
        [x <- p]
        [xs <- (many (is y (where [_ <- sep] [y <- p])))])))

  (define (sepby p sep) (++ (sepby1 p sep) (result '())))

  (define (bracket open p close) (is x (where [_ <- open] [x <- p] [_ <- close])))

  (define (optional p default)
    (lambda (inp)
      (let ([res (p inp)])
        (if (stream-null? res)
            (stream (make-parse-result default inp))
            res))))

  (define (first p)
    (lambda (inp)
      (let ([res (p inp)])
        (if (stream-null? res)
            res
            (stream (stream-car res))))))

  (define (+++ p q) (first (++ p q))) ;; choose first match, cut backtracking

  (define-syntax infix-expression-parser
    (lambda (x)
      (syntax-case x ()
        [(_ ((L/R ?op-parser) ...) ?term-parser ?receiver)
         (with-syntax ([(op-parser ...) (generate-temporaries #'(?op-parser ...))])
           #`(let ([op-parser ?op-parser] ... [term-parser (lambda (inp) (?term-parser inp))] [receiver ?receiver])
               #,(let f ([ls #'((L/R op-parser) ...)])
                   (if (null? ls)
                       #'term-parser
                       #`(let ([next #,(f (cdr ls))])
                           #,(syntax-case (car ls) (LEFT RIGHT)
                               [(LEFT op-parser)
                                #'(let ()
                                    (define-record-type frob (nongenerative) (sealed #t) (fields op y efp))
                                    (trace-is binop-left (lambda (bfp ignore-this-efp)
                                                           (fold-left
                                                             (lambda (x f) (receiver bfp (frob-efp f) (frob-op f) x (frob-y f)))
                                                             x f*))
                                      (where
                                        [x <- next]
                                        [f* <- (rec this
                                                 (optional
                                                   (is (cons f f*)
                                                     (where
                                                       [f <- (trace-is binop-left-tail (lambda (bfp efp) (make-frob op y efp))
                                                               (where
                                                                 [op <- op-parser]
                                                                 [y <- next]))]
                                                       [f* <- this]))
                                                   '()))])))]
                               [(RIGHT op-parser)
                                #'(rec this
                                    (+++
                                      (trace-is binop-right (lambda (bfp efp) (receiver bfp efp op x y))
                                        (where
                                          [x <- next]
                                          [op <- op-parser]
                                          [y <- this]))
                                      next))]))))))])))

  (define (format-inp inp)
    (if (no-more-tokens? inp)
        "#<null-stream>"
        (format "(~s ...)" (current-token inp))))

  (define-syntax define-grammar
    (lambda (x)
      (define-record-type grammar
        (nongenerative)
        (sealed #t)
        (fields title paragraph* section*))
      (define-record-type section
        (nongenerative)
        (sealed #t)
        (fields title paragraph* suppressed? clause*))
      (define-record-type clause
        (nongenerative)
        (fields id alias* before-paragraph* after-paragraph*))
      (define-record-type regular-clause
        (nongenerative)
        (sealed #t)
        (parent clause)
        (fields prod*))
      (define-record-type binop-clause
        (nongenerative)
        (sealed #t)
        (parent clause)
        (fields level* term receiver)
        (protocol
          (lambda (pargs->new)
            (lambda (nt alias* before-paragraph* after-paragraph* level* term src? receiver)
              ((pargs->new nt alias* before-paragraph* after-paragraph*) level* term
                #`(lambda (bfp efp op x y)
                    #,(if src?
                          #`(#,receiver (make-src bfp efp) op x y)
                          #`(#,receiver op x y))))))))
      (define-record-type terminal-clause
        (nongenerative)
        (sealed #t)
        (fields term*))
      (define-record-type terminal
        (nongenerative)
        (sealed #t)
        (fields parser alias* paragraph*))
      (define-record-type production
        (nongenerative)
        (sealed #t)
        (fields name paragraph* elt* receiver)
        (protocol
          (let ()
            (define (check-elts elt*)
              (for-each (lambda (elt) (unless (elt? elt) (errorf 'make-production "~s is not an elt" elt))) elt*))
            (lambda (new)
              (case-lambda
                [(name elt* receiver)
                 (check-elts elt*)
                 (new name #f elt* receiver)]
                [(name paragraph* elt* receiver)
                 (check-elts elt*)
                 (new name paragraph* elt* receiver)])))))
      (define-record-type elt
        (nongenerative))
      (define-record-type sep-elt
        (nongenerative)
        (sealed #t)
        (parent elt)
        (fields +? elt sep))
      (define-record-type opt-elt
        (nongenerative)
        (sealed #t)
        (parent elt)
        (fields elt default))
      (define-record-type kleene-elt
        (nongenerative)
        (sealed #t)
        (parent elt)
        (fields +? elt))
      (define-record-type constant-elt
        (nongenerative)
        (sealed #t)
        (parent elt)
        (fields k))
      (define-record-type id-elt
        (nongenerative)
        (sealed #t)
        (parent elt)
        (fields id))
      (define paragraph?
        (lambda (x)
          (syntax-case x (include)
            [(include filename) (string? (datum filename))]
            [(str  ...) (andmap string? (datum (str ...)))])))
      (define (gentemp) (datum->syntax #'* (gensym)))
      (define (elt-temps elt*)
        (for-each (lambda (elt) (unless (elt? elt) (errorf 'elt-temps "~s is not an elt" elt))) elt*)
        (fold-left
          (lambda (t* elt)
            (if (constant-elt? elt) t* (cons (gentemp) t*)))
          '()
          elt*))
      (define (left-factor clause*)
        (define syntax-equal?
          (lambda (x y)
            (equal? (syntax->datum x) (syntax->datum y))))
        (define (elt-equal? x y)
          (cond
            [(sep-elt? x) 
             (and (sep-elt? y)
                  (eq? (sep-elt-+? x) (sep-elt-+? y))
                  (elt-equal? (sep-elt-elt x) (sep-elt-elt y))
                  (syntax-equal? (sep-elt-sep x) (sep-elt-sep y)))]
            [(opt-elt? x) 
             (and (opt-elt? y)
                  (elt-equal? (opt-elt-elt x) (opt-elt-elt y))
                  (syntax-equal? (opt-elt-default x) (opt-elt-default y)))]
            [(kleene-elt? x) 
             (and (kleene-elt? y)
                  (eq? (kleene-elt-+? x) (kleene-elt-+? y))
                  (elt-equal? (kleene-elt-elt x) (kleene-elt-elt y)))]
            [(constant-elt? x)
             (and (constant-elt? y)
                  (syntax-equal? (constant-elt-k x) (constant-elt-k y)))]
            [(id-elt? x)
             (and (id-elt? y)
                  (syntax-equal? (id-elt-id x) (id-elt-id y)))]
            [else #f]))
        (let lp1 ([clause* clause*] [new-clause* '()])
          (if (null? clause*)
              (reverse new-clause*)
              (let ([clause (car clause*)])
                (let lp2 ([prod* (regular-clause-prod* clause)] [new-prod* '()] [clause* (cdr clause*)])
                  (if (null? prod*)
                      (lp1 clause* (cons (make-regular-clause (clause-id clause) (clause-alias* clause) '() '() (reverse new-prod*)) new-clause*))
                      (let ([prod (car prod*)] [prod* (cdr prod*)])
                        (let ([elt* (production-elt* prod)])
                          (if (null? elt*)
                              (lp2 prod* (cons prod new-prod*) clause*)
                              (let ([elt (car elt*)])
                                (let-values ([(haves have-nots) (partition
                                                                  (lambda (prod)
                                                                    (let ([elt* (production-elt* prod)])
                                                                      (and (not (null? elt*))
                                                                           (elt-equal? (car elt*) elt))))
                                                                  prod*)])
                                  (if (null? haves)
                                      (lp2 prod* (cons prod new-prod*) clause*)
                                      (let ([haves (cons prod haves)])
                                        ; "haves" start with the same elt.  to cut down on the number of new
                                        ; nonterminals and receiver overhead, find the largest common prefix
                                        (let ([prefix (cons elt
                                                        (let f ([elt** (map production-elt* haves)])
                                                          (let ([elt** (map cdr elt**)])
                                                            (if (ormap null? elt**)
                                                                '()
                                                                (let ([elt (caar elt**)])
                                                                  (if (andmap (lambda (elt*) (elt-equal? (car elt*) elt)) (cdr elt**))
                                                                      (cons elt (f elt**))
                                                                      '()))))))])
                                          (let ([t (gentemp)] [n (length prefix)] [t* (elt-temps prefix)])
                                            (lp2 have-nots
                                              (cons (make-production #f (append prefix (list (make-id-elt t)))
                                                      #`(lambda (bfp efp #,@t* p) (p bfp #,@t*)))
                                                new-prod*)
                                              (cons (make-regular-clause t '() '() '()
                                                      (map (lambda (prod)
                                                             (let ([elt* (list-tail (production-elt* prod) n)])
                                                               (make-production (production-name prod) elt*
                                                                 (let ([u* (elt-temps elt*)])
                                                                   #`(lambda (bfp efp #,@u*)
                                                                       (lambda (bfp #,@t*)
                                                                         (#,(production-receiver prod) bfp efp #,@t* #,@u*)))))))
                                                        haves))
                                                clause*)))))))))))))))))
      (define (make-env tclause* clause*)
        (let ([env (make-hashtable (lambda (x) (symbol-hash (syntax->datum x))) free-identifier=?)])
          (define (insert parser)
            (lambda (name)
              (let ([a (hashtable-cell env name #f)])
                (when (cdr a) (syntax-error name "duplicate terminal/non-terminal name"))
                (set-cdr! a parser))))
          (for-each
            (lambda (tclause)
              (for-each
                (lambda (term)
                  (let ([parser (terminal-parser term)])
                    (for-each (insert parser) (cons parser (terminal-alias* term)))))
                (terminal-clause-term* tclause)))
            tclause*)
          (for-each
            (lambda (clause)
              (let ([id (clause-id clause)])
                (for-each (insert id) (cons id (clause-alias* clause)))))
            clause*)
          env))
      (define (lookup id env)
        (or (hashtable-ref env id #f)
            (syntax-error id "unrecognized terminal or nonterminal")))
      (define (render-markdown name grammar mdfn env)
        (define (separators sep ls)
          (if (null? ls)
              ""
              (apply string-append
                (cons (car ls)
                  (map (lambda (s) (format "~a~a" sep s)) (cdr ls)))))) 
        (define (render-paragraph hard-leading-newline?)
          (lambda (paragraph)
            (define (md-text s)
              (list->string
                (fold-right
                  (lambda (c ls)
                    (case c
                      [(#\\) (cons* c c ls)]
                      [else (cons c ls)]))
                  '()
                  (string->list s))))
            (syntax-case paragraph (include)
              [(include filename)
               (string? (datum filename))
               (let ([text (call-with-port (open-input-file (datum filename)) get-string-all)])
                 (unless (equal? text "")
                   (if hard-leading-newline? (printf "\\\n") (newline))
                   (display-string text)))]
              [(sentence ...)
               (andmap string? (datum (sentence ...)))
               (let ([sentence* (datum (sentence ...))])
                 (unless (null? sentence*)
                   (if hard-leading-newline? (printf "\\\n") (newline))
                   (printf "~a\n" (separators " " (map md-text sentence*)))))])))
        (define (format-elt x)
          (cond
            [(sep-elt? x)
             (let* ([one (format-elt (sep-elt-elt x))]
                    [sep (constant->markdown (syntax->datum (sep-elt-sep x)))]
                    [seq (format "~a&nbsp;&nbsp;~a&nbsp;&nbsp;`...`" one sep)])
               (if (sep-elt-+? x)
                   seq
                   (format "OPT(~a)" seq)))]
            [(opt-elt? x)
             (format "~a~~opt~~" (format-elt (opt-elt-elt x)))]
            [(kleene-elt? x)
             (let ([one (format-elt (kleene-elt-elt x))])
               (if (kleene-elt-+? x)
                   (format "~a&nbsp;&nbsp;`...`" one)
                   (format "OPT(~a)" one)))]
            [(constant-elt? x) (constant->markdown (syntax->datum (constant-elt-k x)))]
            [(id-elt? x) (format "[*~s*](#~s)"
                           (syntax->datum (id-elt-id x))
                           (syntax->datum (lookup (id-elt-id x) env)))]
            [else (errorf 'format-elt "unexpected elt ~s" x)]))
        (define (render-elt x)
          (printf "&nbsp;&nbsp;~a" (format-elt x)))
        (define (render-production prod)
          (unless (null? (production-elt* prod))
            (printf "  : ")
            (for-each render-elt (production-elt* prod))
            (printf "\n"))
          (when (and (null? (production-elt* prod))
                     (not (null? (production-paragraph* prod))))
            (errorf 'render-production "empty production must not have description: ~a" (production-paragraph* prod)))
          (for-each (render-paragraph #t) (production-paragraph* prod)))
        (define (render-clause clause)
          (define (render-aliases alias*)
            (unless (null? alias*)
              (printf "  \naliases: ~{*~a*~^, ~}\n" (map syntax->datum alias*))))
          (if (terminal-clause? clause)
              (for-each
                (lambda (term)
                  (printf "\n#### *~a* {#~:*~a}\n" (syntax->datum (terminal-parser term)))
                  (render-aliases (terminal-alias* term))
                  (for-each (render-paragraph #f) (terminal-paragraph* term)))
                (terminal-clause-term* clause))
              (let ([id (syntax->datum (clause-id clause))])
                (printf "\n#### *~a* {#~:*~a}\n" id)
                (render-aliases (clause-alias* clause))
                (for-each (render-paragraph #f) (clause-before-paragraph* clause))
                (printf "\nsyntax:\n")
                (if (binop-clause? clause)
                    (let ([level* (binop-clause-level* clause)])
                      (let loop ([level* level*] [first? #t])
                        (unless (null? level*)
                          (let ([level (syntax->datum (car level*))] [level* (cdr level*)])
                            (let ([L/R (car level)] [op* (cdr level)])
                              (printf "  : _~(~a~)-associative" L/R)
                              (if first?
                                  (if (null? level*)
                                      (printf ":_\n")
                                      (printf ", highest precedence:_\n"))
                                  (if (null? level*)
                                      (printf ", lowest precedence:_\n")
                                      (printf ":_\n")))
                              (for-each
                                (lambda (op) (printf "    : ~s ~a ~s\n" id (constant->markdown op) id))
                                op*))
                            (loop level* #f))))
                      (printf "  : _leaves:_\n")
                      (printf "    : ")
                      (render-elt (binop-clause-term clause))
                      (printf "\n"))
                    (for-each render-production (or (regular-clause-prod* clause) '())))
                (for-each (render-paragraph #f) (clause-after-paragraph* clause)))))
        (define (render-section section)
          (unless (section-suppressed? section)
            (printf "\n## ~a\n" (or (section-title section) "The section"))
            (for-each (render-paragraph #f) (section-paragraph* section))
            (for-each render-clause (section-clause* section))))
        (with-output-to-file mdfn
          (lambda ()
            (printf "# ~a\n" (or (grammar-title grammar) "The grammar"))
            (for-each (render-paragraph #f) (grammar-paragraph* grammar))
            (for-each render-section (grammar-section* grammar)))
          'replace))
      (module (parse-grammar)
        (define parse-elt
          (lambda (elt)
            (syntax-case elt (SEP+ SEP* OPT K* K+)
              [(SEP+ p sep) (make-sep-elt #t (parse-elt #'p) #'sep)]
              [(SEP* p sep) (make-sep-elt #f (parse-elt #'p) #'sep)]
              [(OPT p default) (make-opt-elt (parse-elt #'p) #'default)]
              [(K+ p) (make-kleene-elt #t (parse-elt #'p))]
              [(K* p) (make-kleene-elt #f (parse-elt #'p))]
              [k (constant? #'k) (make-constant-elt #'k)]
              [id (identifier? #'id) (make-id-elt #'id)]
              [_ (syntax-error elt "invalid production element")])))
        (define parse-production
          (lambda (prod)
            (define (finish name src? paragraph* elt* receiver)
              (let ([elt* (map parse-elt elt*)])
                (make-production name paragraph* elt*
                  (with-syntax ([(t ...) (elt-temps elt*)])
                    #`(lambda (bfp efp t ...)
                        #,(if src?
                              #`(#,receiver (make-src bfp efp) t ...)
                              #`(#,receiver t ...)))))))
            (syntax-case prod (:: src =>)
              [[name :: src elt ... => receiver]
               (finish #'name #t '() #'(elt ...) #'receiver)]
              [[name :: elt ... => receiver]
               (finish #'name #f '() #'(elt ...) #'receiver)])))
        (define (parse-terminal term)
          (syntax-case term (DESCRIPTION)
            [(parser (alias ...) (DESCRIPTION paragraph ...))
             (and (identifier? #'parser) (andmap identifier? #'(alias ...)) (andmap paragraph? #'(paragraph ...)))
             (make-terminal #'parser #'(alias ...) #'(paragraph ...))]
            [(parser (alias ...))
             (and (identifier? #'parser) (andmap identifier? #'(alias ...)))
             (make-terminal #'parser #'(alias ...) '())]))
        (define (parse-clause clause nt alias* before-paragraph* after-paragraph* stuff*)
          (syntax-case stuff* (BINOP :: src =>)
            [((BINOP src (level ...) term) => receiver)
             (make-binop-clause nt alias* before-paragraph* after-paragraph* #'(level ...) (parse-elt #'term) #t #'receiver)]
            [((BINOP (level ...) term) => receiver)
             (make-binop-clause nt alias* before-paragraph* after-paragraph* #'(level ...) (parse-elt #'term) #f #'receiver)]
            [(prod prods ...)
             (make-regular-clause nt alias* before-paragraph* after-paragraph* (map parse-production #'(prod prods ...)))]
            [else (syntax-error clause)]))
        (define (parse-top top* knull kgrammar ksection kclause)
          (if (null? top*)
              (knull)
              (let ([top (car top*)] [top* (cdr top*)])
                (syntax-case top (GRAMMAR SECTION SUPPRESSED DESCRIPTION BINOP TERMINALS src =>)
                  [(GRAMMAR title paragraph ...)
                   (andmap paragraph? #'(paragraph ...))
                   (kgrammar top* (datum title) #'(paragraph ...))]
                  [(SECTION SUPPRESSED title paragraph ...)
                   (andmap paragraph? #'(paragraph ...))
                   (ksection top* (datum title) #'(paragraph ...) #t)]
                  [(SECTION title paragraph ...)
                   (andmap paragraph? #'(paragraph ...))
                   (ksection top* (datum title) #'(paragraph ...) #f)]
                  [(TERMINALS term ...)
                   (kclause top* (make-terminal-clause (map parse-terminal #'(term ...))))]
                  [(TERMINALS term ...)
                   (kclause top* (make-terminal-clause (map parse-terminal #'(term ...))))]
                  [(nt (alias ...) (DESCRIPTION paragraph1 ...) stuff ... (DESCRIPTION paragraph2 ...))
                   (and (identifier? #'nt) (andmap identifier? #'(alias ...)) (andmap paragraph? #'(paragraph1 ...)) (andmap paragraph? #'(paragraph2 ...)))
                   (kclause top* (parse-clause top #'nt #'(alias ...) #'(paragraph1 ...) #'(paragraph2 ...) #'(stuff ...)))]
                  [(nt (alias ...) (DESCRIPTION paragraph ...) stuff ...)
                   (and (identifier? #'nt) (andmap identifier? #'(alias ...)) (andmap paragraph? #'(paragraph ...)))
                   (kclause top* (parse-clause top #'nt #'(alias ...) #'(paragraph ...) '() #'(stuff ...)))]
                  [(nt (alias ...) stuff ... (DESCRIPTION paragraph ...))
                   (and (identifier? #'nt) (andmap identifier? #'(alias ...)) (andmap paragraph? #'(paragraph ...)))
                   (kclause top* (parse-clause top #'nt #'(alias ...) '() #'(paragraph ...) #'(stuff ...)))]
                  [(nt (alias ...) stuff ...)
                   (and (identifier? #'nt) (andmap identifier? #'(alias ...)))
                   (kclause top* (parse-clause top #'nt #'(alias ...) '() '() #'(stuff ...)))]))))
        (define (parse-grammar top*)
          (define (misplaced-grammar-error top)
            (syntax-error top "unexpected GRAMMAR element after other elements"))
          (define (s1 top*) ; looking for GRAMMAR form, first SECTION form, or clause
            (parse-top top*
              (lambda () (make-grammar #f '() '()))
              (lambda (top* title paragraph*)
                (make-grammar title paragraph* (s2 top*)))
              (lambda (top* title paragraph* suppressed?)
                (make-grammar #f '()
                  (s3 top* title paragraph* suppressed? '() '())))
              (lambda (top* clause)
                (make-grammar #f '()
                  (s3 top* #f '() #f (list clause) '())))))
          (define (s2 top*) ; looking for first SECTION form or clause
            (parse-top top*
              (lambda () '())
              (lambda (title paragraph*) (misplaced-grammar-error (car top*)))
              (lambda (top* title paragraph* suppressed?)
                (s3 top* title paragraph* suppressed? '() '()))
              (lambda (top* clause)
                (s3 top* #f '() #f (list clause) '()))))
          (define (s3 top* title paragraph* suppressed? rclause* rsection*) ; steady state: looking for remaining SECTION forms and clauses
            (define (finish-section)
              (cons (make-section title paragraph* suppressed? (reverse rclause*)) rsection*))
            (parse-top top*
              (lambda () (reverse (finish-section)))
              (lambda (title paragraph*) (misplaced-grammar-error (car top*)))
              (lambda (top* title paragraph* suppressed?)
                (s3 top* title paragraph* suppressed? '() (finish-section)))
              (lambda (top* clause)
                (s3 top* title paragraph* suppressed? (cons clause rclause*) rsection*))))
          (s1 top*)))
      (define (go init-nts top* mddir)
        (let ([grammar (parse-grammar top*)])
          (let* ([clause* (apply append (map section-clause* (grammar-section* grammar)))]
                 [terminal-clause* (filter terminal-clause? clause*)]
                 [binop-clause* (filter binop-clause? clause*)]
                 [regular-clause* (left-factor (filter regular-clause? clause*))]
                 [env (make-env terminal-clause* (append binop-clause* regular-clause*))])
            (define (elt-helper x)
              (cond
                [(sep-elt? x) #`(#,(if (sep-elt-+? x) #'sepby1 #'sepby) #,(elt-helper (sep-elt-elt x)) (sep->parser #,(sep-elt-sep x)))]
                [(opt-elt? x) #`(optional #,(elt-helper (opt-elt-elt x)) #,(opt-elt-default x))]
                [(kleene-elt? x) #`(#,(if (kleene-elt-+? x) #'many+ #'many) #,(elt-helper (kleene-elt-elt x)))]
                [(constant-elt? x) #`(constant->parser '#,(constant-elt-k x))]
                [(id-elt? x) (lookup (id-elt-id x) env)]
                [else (errorf 'elt-helper "unhandled elt ~s\n" x)]))
            (define (binop-helper clause)
              #`[#,(clause-id clause)
                  (infix-expression-parser
                    #,(map (lambda (level)
                             (syntax-case level ()
                               [(L/R op1 ... op2)
                                (or (free-identifier=? #'L/R #'LEFT) (free-identifier=? #'L/R #'RIGHT))
                                #`(L/R #,(fold-right (lambda (op next) #`(++ (binop->parser '#,op) #,next)) #'(binop->parser 'op2) #'(op1 ...)))]))
                        (binop-clause-level* clause))
                    #,(elt-helper (binop-clause-term clause))
                    #,(binop-clause-receiver clause))])
            (define (nt-helper clause)
              #`[#,(clause-id clause)
                  #,(let f ([prod* (regular-clause-prod* clause)])
                      (if (null? prod*)
                          #'zero
                          (let ([elt* (production-elt* (car prod*))])
                            (with-syntax ([name (production-name (car prod*))]
                                          [(elt ...) elt*]
                                          [receiver (production-receiver (car prod*))])
                              (with-syntax ([(x ...) (generate-temporaries elt*)])
                                (with-syntax ([([y _] ...) (filter (lambda (pr) (not (constant-elt? (cadr pr)))) #'([x elt] ...))])
                                  (with-syntax ([(where-nt ...) (map elt-helper elt*)])
                                    #`(+++ ;; use +++ if you don't ever need to backtrack to a previous production for the same non-terminal
                                        (lambda (inp)
                                          (when (and 'name (grammar-trace)) (printf ">>~s(~a)~%" 'name (format-inp inp)))
                                          (let ([res ((trace-is name (lambda (bfp efp) (receiver bfp efp y ...)) (where [x <- where-nt] ...)) inp)])
                                            (when (and 'name (grammar-trace))
                                              (if (stream-null? res)
                                                  (printf "<<~s(~a) failed~%" 'name (format-inp inp))
                                                  (printf "<<~s(~a) succeeded~%" 'name (format-inp inp))))
                                            res))
                                        #,(f (cdr prod*))))))))))])
            (with-syntax ([(init-nt ...)
                           (syntax-case init-nts ()
                             [(id1 id2 ...) (andmap identifier? #'(id1 id2 ...)) #'(id1 id2 ...)]
                             [id (identifier? #'id) (list #'id)])])
              (when mddir
                (for-each
                  (lambda (init-nt)
                    (let ([mdfn (format "~a/~a.md" mddir (syntax->datum init-nt))])
                      (render-markdown init-nt grammar mdfn env)))
                  #'(init-nt ...)))
              (with-syntax ([((lhs rhs) ...)
                             (append
                               (map binop-helper binop-clause*)
                               (map nt-helper regular-clause*))])
                #'(module (init-nt ...)
                    (module M (init-nt ...) (define lhs rhs) ...)
                    (define init-nt
                      (let ()
                        (import M)
                        (make-top-level-parser init-nt)))
                    ...))))))
      (syntax-case x (markdown-directory)
        [(_ init-nts (markdown-directory mddir) top ...)
         (string? (datum mddir))
         (go #'init-nts #'(top ...) (datum mddir))]
        [(_ init-nts top ...) (go #'init-nts #'(top ...) #f)])))

  (indirect-export define-grammar
    result
    zero
    is
    trace-is
    sepby1
    sepby
    optional
    many
    many+
    +++
    infix-expression-parser

    grammar-trace
    format-inp
    trace-is

    make-top-level-parser
  )
)
