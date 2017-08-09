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
          is sat peek seq ++ +++ many many+ ?
          parse-consumed-all? parse-result-value parse-result-unused
          grammar-trace
          )
  (import (streams))

  (define grammar-trace (make-parameter #f))

  (define-record-type parse-result
    (nongenerative parse-result)
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
          (when (and 'name (grammar-trace)) (printf "<<~s = ~s~%" 'name res))
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

  (define (format-inp inp)
    (if (no-more-tokens? inp)
        "#<null-stream>"
        (format "(~s ...)" (current-token inp))))

  (define-syntax define-grammar
    (lambda (x)
      (define-record-type production
        (nongenerative)
        (fields name elt* receiver))
      (define-record-type clause
        (nongenerative)
        (fields id prod*))
      (define (gentemp) (datum->syntax #'* (gensym)))
      (define (elt-temps elt*)
        (fold-left
          (lambda (t* elt)
            (if (constant? elt) t* (cons (gentemp) t*)))
          '()
          elt*))
      (define parse-production
        (lambda (cl)
          (syntax-case cl (:: src =>)
            [[name :: src elt ... => receiver]
             (make-production #'name #'(elt ...)
               (with-syntax ([(t ...) (elt-temps #'(elt ...))])
                 #'(lambda (bfp efp t ...)
                     (receiver (make-src bfp efp) t ...))))]
            [[name :: elt ... => receiver]
             (make-production #'name #'(elt ...)
               (with-syntax ([(t ...) (elt-temps #'(elt ...))])
                 #'(lambda (bfp efp t ...)
                     (receiver t ...))))])))
      (define (left-factor clause*)
        (define syntax-equal?
          (lambda (x y)
            (equal? (syntax->datum x) (syntax->datum y))))
        (let lp1 ([clause* clause*] [new-clause* '()])
          (if (null? clause*)
              (reverse new-clause*)
              (let ([clause (car clause*)])
                (let lp2 ([prod* (clause-prod* clause)] [new-prod* '()] [clause* (cdr clause*)])
                  (if (null? prod*)
                      (lp1 clause* (cons (make-clause (clause-id clause) (reverse new-prod*)) new-clause*))
                      (let ([prod (car prod*)] [prod* (cdr prod*)])
                        (let ([elt* (production-elt* prod)])
                          (if (null? elt*)
                              (lp2 prod* (cons prod new-prod*) clause*)
                              (let ([elt (car elt*)])
                                (let-values ([(haves have-nots) (partition
                                                                  (lambda (prod)
                                                                    (let ([elt* (production-elt* prod)])
                                                                      (and (not (null? elt*))
                                                                           (syntax-equal? (car elt*) elt))))
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
                                                                  (if (andmap (lambda (elt*) (syntax-equal? (car elt*) elt)) (cdr elt**))
                                                                      (cons (caar elt**) (f elt**))
                                                                      '()))))))])
                                          (let ([t (gentemp)] [n (length prefix)] [t* (elt-temps prefix)])
                                            (lp2 have-nots
                                              (cons (make-production #f (append prefix (list t))
                                                      #`(lambda (bfp efp #,@t* p) (p bfp #,@t*)))
                                                new-prod*)
                                              (cons (make-clause t
                                                      (map (lambda (prod)
                                                             (let ([elt* (list-tail (production-elt* prod) n)])
                                                               (make-production (production-name prod) elt*
                                                                 (let ([u* (elt-temps elt*)])
                                                                   #`(lambda (bfp efp #,@u*)
                                                                       (lambda (bfp #,@t*)
                                                                         (#,(production-receiver prod) bfp efp #,@t* #,@u*)))))))
                                                        haves))
                                                clause*)))))))))))))))))
      (define (nt-helper clause*)
        (define (elt-helper x)
          (syntax-case x (SEP+ SEP* OPT K* K+)
            [(SEP+ p sep) #`(sepby1 #,(elt-helper #'p) (sep->parser sep))]
            [(SEP* p sep) #`(sepby  #,(elt-helper #'p) (sep->parser sep))]
            [(OPT p dflt) #`(optional #,(elt-helper #'p) dflt)]
            [(K* p) #`(many #,(elt-helper #'p))]
            [(K+ p) #`(many+ #,(elt-helper #'p))]
            [k (constant? #'k) #'(constant->parser 'k)]
            [p #'p]))
        (let loop ([clause* clause*] [binding* '()])
          (if (null? clause*)
              binding*
              (loop
                (cdr clause*)
                (cons
                  #`[#,(clause-id (car clause*))
                      #,(let f ([prod* (clause-prod* (car clause*))])
                          (if (null? prod*)
                              #'zero
                              (with-syntax ([name (production-name (car prod*))]
                                            [(elt ...) (production-elt* (car prod*))]
                                            [receiver (production-receiver (car prod*))])
                                (with-syntax ([(x ...) (generate-temporaries #'(elt ...))])
                                  (with-syntax ([([y _] ...) (filter (lambda (pr) (not (constant? (cadr pr)))) #'([x elt] ...))])
                                    (with-syntax ([(where-nt ...) (map elt-helper #'(elt ...))])
                                      #`(+++ ;; use +++ if you don't ever need to backtrack to a previous production for the same non-terminal
                                          (lambda (inp)
                                            (when (and 'name (grammar-trace)) (printf ">>~s(~a)~%" 'name (format-inp inp)))
                                            (let ([res ((trace-is name (lambda (bfp efp) (receiver bfp efp y ...)) (where [x <- where-nt] ...)) inp)])
                                              (when (and 'name (grammar-trace))
                                                (if (stream-null? res)
                                                    (printf "<<~s(~a) failed~%" 'name (format-inp inp))
                                                    (printf "<<~s(~a) succeeded~%" 'name (format-inp inp))))
                                              res))
                                          #,(f (cdr prod*)))))))))]
                  binding*)))))
      (syntax-case x ()
        [(_ init-nt [nt prod prods ...] ...)
         (with-syntax ([(binding ...)
                        (nt-helper
                          (left-factor
                            (map (lambda (nt prod*) (make-clause nt (map parse-production prod*)))
                              #'(nt ...)
                              #'((prod prods ...) ...))))])
           #'(define init-nt
               (letrec (binding ...)
                 (make-top-level-parser init-nt))))])))

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

    grammar-trace
    format-inp
    trace-is

    make-top-level-parser
  )
)
