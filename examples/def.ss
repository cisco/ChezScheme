;;; def.ss
;;; Copyright (C) 1987 R. Kent Dybvig

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

;;; Prototype code for definition facility that remembers definitions and
;;; allows you to pretty-print or edit them (using the structure editor
;;; defined in the file "edit.ss").

;;; def can be in place of define at top level (i.e., not within a lambda,
;;; let, let*, or letrec body).  It saves the source for the definition
;;; as well as performing the defintion.  Type (ls-def) for a list of
;;; variables defined this session, and (pp-def variable) to return the
;;; definition of a particular variable.

;;; Possible exercises/enhancements:
;;;
;;;  1) Write a "dskout" function that pretty-prints the definitions of
;;;     all or selected variables defined this session to a file.
;;;
;;;  2) In place of "def", write a modified "load" that remembers where
;;;     (that is, in which file) it saw the definition for each variable
;;;     defined in a particular session.  This would be used instead of
;;;     the "def" form.  "ls-def" would be similar to what it is now.
;;;     "pp-def" could be similar to what it is now, or it could involve
;;;     rereading the corresponding file.  "ed-def" could invoke the
;;;     structure editor and (as an option) print the modified definition
;;;     back to the corresponding file, or "ed-def" could invoke a host
;;;     editor (such as Unix "vi" or VMS "edit") on the corresponding
;;;     source file, with an option to reload.  If this tool is smart
;;;     enough, it could get around the limitation that definitions use
;;;     define at top-level, i.e., (let ([x #f]) (set! foo (lambda () x)))
;;;     could be recognized as a definition for foo.

(define-syntax def
  ;; only makes sense for "top level" definitions
  (syntax-rules ()
    [(_ (var . formals) . body)
     (begin (define (var . formals) . body)
            (insert-def! 'var '(def (var . formals) . body) var)
            'var)]
    [(_ var exp)
     (begin (define var exp)
            (insert-def! 'var '(def var exp) var)
            'var)]))

(define-syntax pp-def
  (syntax-rules (quote)
   ; allow var to be unquoted or quoted
    [(_ var) (pp-def-help 'var var)]
    [(_ 'var) (pp-def-help 'var var)]))

(define-syntax ed-def
  (syntax-rules (quote)
   ; allow var to be unquoted or quoted
    [(_ var) (ed-def-help 'var var)]
    [(_ 'var) (ed-def-help 'var var)]))


(define insert-def! #f) ; assigned within the let below
(define ls-def #f) ; assigned within the let below
(define pp-def-help #f) ; assigned within the let below
(define ed-def-help #f) ; assigned within the let below
(let ([defs '()])
  (define tree-copy
    (rec tree-copy
      (lambda (x)
        (if (pair? x)
            (cons (tree-copy (car x)) (tree-copy (cdr x)))
            x))))
   (set! insert-def!
      (lambda (var defn val)
         (unless (symbol? var)
            (error 'insert-def! "~s is not a symbol" var))
         (let ([a (assq var defs)])
            (if a
                (set-cdr! a (cons defn val))
                (set! defs (cons (cons var (cons defn val)) defs))))))
   (set! ls-def
      (lambda ()
         (map car defs)))
   (set! pp-def-help
      (lambda (var val)
         (unless (symbol? var)
            (error 'pp-def "~s is not a symbol" var))
         (let ([a (assq var defs)])
            (unless a
               (error 'pp-def
                      "~s has not been defined during this session"
                      var))
            (unless (eq? (cddr a) val)
               (printf "Warning: ~s has been reassigned since definition"
                       var))
            (cadr a))))
   (set! ed-def-help
      (lambda (var val)
         (unless (symbol? var)
            (error 'ed-def "~s is not a symbol" var))
         (let ([a (assq var defs)])
            (unless a
               (error 'ed-def
                      "~s has not been defined during this session"
                      var))
            (unless (eq? (cddr a) val)
               (printf "Warning: ~s reassigned since last definition"
                       var))
            ; edit is destructive; the copy allows the defined name to
            ; be changed without affecting the old name's definition
            (eval (edit (tree-copy (cadr a))))))))
