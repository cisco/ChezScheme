;;; edit.ss
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

;;; This file contains an implementation of a simple interactive structure
;;; editor for Scheme.  The editor is invoked with an expression as it's
;;; single argument.  It prompts for, reads, and processes editor commands.

;;; The editor commands recognized are those documented in the Texas
;;; Instruments' PC Scheme manual.  They are summarized below.

;;; Command syntax          Action
;;;
;;;   q or <eof>            Quit the editor, returning edited expression.
;;;
;;;   p                     Write the current expression.
;;;
;;;   ?                     Write to level 2, length 10.
;;;
;;;   pp                    Pretty print the current expression.
;;;
;;;   ??                    Pretty print to level 2, length 10.
;;;
;;;   <pos>                 Move to subexpression of current expression
;;;                         <pos> = 0 is the current expression, <pos> > 0
;;;                         is the numbered subexpression (1 for first, 2
;;;                         for second, ...), <pos> < 0 is the numbered
;;;                         subexpression from the right (-1 for last, -2
;;;                         for second to last, ...), and <pos> = * is the
;;;                         "last cdr" of the current expression.  If <pos>
;;;                         is not 0, the current expression must be a list.
;;;
;;;   b                     Move back to parent expression.
;;;
;;;   t                     Move to top-level expression.
;;;
;;;   pr                    Move to expression on the left (previous).
;;;
;;;   n                     Move to expression on the right (next).
;;;
;;;   (f <obj>)             Find <obj> within or to the right of the current
;;;                         expression using equal?.
;;;
;;;   f or (f)              Find <obj> of last (f <obj>) command.
;;;
;;;   (d <pos>)             Delete the expression at position <pos>.
;;;
;;;   (r <pos> <obj>)       Replace the expression at position <pos> with
;;;                         <obj>.
;;;
;;;   (s <obj1> <obj2>)     Replace all occurrences of <obj1> by <obj2>
;;;                         within the current expression.
;;;
;;;   (dp <pos>)            Remove parens from around expression at position
;;;                         <pos>.
;;;
;;;   (ap <pos1> <pos2>)    Insert parens around expressions from position
;;;                         <pos1> through <pos2> (inclusive).  If <pos1> is
;;;                         0 or *, <pos2> is ignored and may be omitted.
;;;
;;;   (ib <pos> <obj>)      Insert <obj> before expression at position <pos>.
;;;
;;;   (ia <pos> <obj>)      Insert <obj> after expression at position <pos>.
;;;
;;;   (sb <pos> <obj>)      Splice <obj> before expression at position <pos>.
;;;
;;;   (sa <pos> <obj>)      Splice <obj> after expression at position <pos>.

;;; Possible exercises/enhancements:
;;;
;;;  1) Implement an infinite undo ("u") command in the editor.  This
;;;     can be done by creating an "inverse" function for each operation
;;;     that causes a side-effect, i.e, a closure that "remembers" the
;;;     list cells involved and knows how to put them back the way they
;;;     were.  An undo (u) variable could then be added to the editor's
;;;     main loop; it would be bound to a list containing the set of
;;;     registers at the point of the last side-effect (similarly to the
;;;     "back" (b) variable) and the undo function for the side-effect.
;;;
;;;  2) Implement an infinite redo ("r") command in the editor.  This
;;;     can be done by remembering the undo functions and registers for
;;;     the undo's since the last non-undo command.
;;;
;;;  3) Handle circular structures better in the editor.  Specifically,
;;;     modify the find ("f") command so that it always terminates, and
;;;     devise a method for printing circular structures with the "p"
;;;     and "pp" commands.  Cure the bug mentioned in the overview of
;;;     the code given later in the file.
;;;
;;;  4) Add a help ("h") command to the editor.  This could be as simple
;;;     as listing the available commands.
;;;
;;;  5) Make the editor "extensible" via user-defined macros or editor
;;;     commands written in Scheme.
;;;
;;;  6) Modify the editor to provide more descriptive error messages that
;;;     diagnose the problem and attempt to give some help.  For example,
;;;     if the editor receives "(r 1)" it might respond with:
;;;     "Two few arguments:
;;;         Type (r pos exp) to replace the expression at position pos
;;;         with the expression exp."
;;;     This should be implemented in conjunction with the help command.
;;;     Should it be possible to disable such verbose error messages?

;;; Implementation:
;;;
;;; The main editor loop and many of the help functions operate on a
;;; set of "registers".  These registers are described below:
;;;
;;; s     The current find object.  s is initially #f, and is bound to a
;;;       pair containing the find object when the first (f <obj>) command
;;;       is seen.  The identical f and (f) commands use the saved object.
;;;
;;; p     The parent of the current expression.  This is initially a list
;;;       of one element, the argument to edit.  It is updated by various
;;;       movement commands.
;;;
;;; i     The index of the current expression in the parent (p).  This is
;;;       initially 0.  It is updated by various movement commands.
;;;
;;; b     The "back" chain; actually a list containing the registers p, i
;;;       and b for the parent of the current expression.  It is initially
;;;       ().  It is updated by various movement commands.
;;;
;;; Bugs:
;;;
;;; When editing a circular structure, it is possible for the editor to
;;; get lost.  That is, when the parent node of the current expression
;;; is changed by a command operating on a subexpression of the current
;;; expression, the index for the current expression may become incorrect.
;;; This can result in abnormal termination of the editor.  It would be
;;; fairly simple to check for this (in list-ref) and reset the editor,
;;; and it may be possible to use a different set of registers to avoid
;;; the problem altogether.

(define edit #f) ; assigned within the let expression below
(let ()
   (define cmdeq?
      ;; used to check command syntax
      (lambda (cmd pat)
         (and (pair? cmd)
              (eq? (car cmd) (car pat))
              (let okargs? ([cmd (cdr cmd)] [pat (cdr pat)])
                 (if (null? pat)
                     (null? cmd)
                     (and (not (null? cmd))
                          (okargs? (cdr cmd) (cdr pat))))))))
   (define find
      ;; find expression within or to right of current expression
      (lambda (s0 p0 i0 b0)
         (define check
            (lambda (p i b)
               (if (equal? (list-ref p i) (car s0))
                   (wrlev s0 p i b)
                   (continue p i b))))
         (define continue
            (lambda (p i b)
               (let ([e (list-ref p i)])
                  (if (atom? e)
                      (let next ([p p] [i i] [b b])
                         (let ([n (maxref p)])
                            (if (or (not n) (< i n))
                                (check p (+ i 1) b)
                                (if (null? b)
                                    (search-failed s0 p0 i0 b0)
                                    (apply next b)))))
                      (check e 0 (list p i b))))))
         (continue p0 i0 b0)))
   (define maxref
      ;; use "hare and tortoise" algorithm to check for circular lists.
      ;; return maximum reference index (zero-based) for a list x.  return
      ;; -1 for atoms and #f for circular lists.
      (lambda (x)
         (let f ([hare x] [tortoise x] [n -1])
            (cond
               [(atom? hare) n]
               [(atom? (cdr hare)) (+ n 1)]
               [(eq? (cdr hare) tortoise) #f]
               [else (f (cddr hare) (cdr tortoise) (+ n 2))]))))
   (define move
      ;; move to subexpression specified by x and pass current state to k.
      (lambda (x s p i b k)
         (cond
            [(eqv? x 0) (k s p i b)]
            [(eq? x '*)
             (let ([m (maxref (list-ref p i))])
                (if m
                    (k s (list-ref p i) '* (list p i b))
                    (invalid-movement s p i b)))]
            [(> x 0)
             (let ([m (maxref (list-ref p i))] [x (- x 1)])
                (if (or (not m) (>= m x))
                    (k s (list-ref p i) x (list p i b))
                    (invalid-movement s p i b)))]
            [else
             (let ([m (maxref (list-ref p i))] [x (- -1 x)])
                (if (and m (>= m x))
                    (let ([x (- m x)])
                       (k s (list-ref p i) x (list p i b)))
                    (invalid-movement s p i b)))])))
   (define proper-list?
      ;; return #t if x is a proper list.
      (lambda (x)
         (and (maxref x)
              (or (null? x) (null? (cdr (last-pair x)))))))
   (define list-ref
      ;; reference list ls element i.  i may be *, in which case return
      ;; the last pair of ls.
      (lambda (ls i)
         (if (eq? i '*)
             (cdr (last-pair ls))
             (car (list-tail ls i)))))
   (define list-set!
      ;; change element i of ls to x.
      (lambda (ls i x)
         (if (eq? i '*)
             (set-cdr! (last-pair ls) x)
             (set-car! (list-tail ls i) x))))
   (define list-cut!
      ;; remove element i from ls.
      (lambda (ls i)
         (let ([a (cons '() ls)])
            (set-cdr! (list-tail a i) (list-tail a (+ i 2)))
            (cdr a))))
   (define list-splice!
      ;; insert ls2 into ls1 in place of element i.
      (lambda (ls1 i ls2)
         (let ([a (list-tail ls1 i)])
            (unless (null? (cdr a))
               (set-cdr! (last-pair ls2) (cdr a)))
            (set-car! a (car ls2))
            (set-cdr! a (cdr ls2)))
         ls1))
   (define list-ap*!
      ;; place parens from element i through last pair of ls.
      (lambda (ls i)
         (let ([a (list-tail ls i)])
            (let ([c (cons (car a) (cdr a))])
               (set-car! a c)
               (set-cdr! a '())))
         ls))
   (define list-ap!
      ;; place parens from element i0 through element i1.
      (lambda (ls i0 i1)
         (let ([a (list-tail ls i0)] [b (list-tail ls i1)])
            (let ([c (cons (car a) (cdr a))])
               (set-car! a c)
               (if (eq? a b)
                   (set-cdr! c '())
                   (begin (set-cdr! a (cdr b))
                          (set-cdr! b '())))))
         ls))
   (define wrlev
      ;; write current expression to level 2, length 10 and continue.
      (lambda (s p i b)
         (parameterize ([print-level 2] [print-length 10])
            (printf "~s~%" (list-ref p i)))
         (edit-loop s p i b)))
   (define wr
      ;; write current expression and continue.
      (lambda (s p i b)
         (printf "~s~%" (list-ref p i))
         (edit-loop s p i b)))
   (define pplev
      ;; pretty print current expression to level 2, length 10 and continue.
      (lambda (s p i b)
         (parameterize ([print-level 2] [print-length 10])
            (pretty-print (list-ref p i)))
         (edit-loop s p i b)))
   (define pp
      ;; pretty print current expression and continue.
      (lambda (s p i b)
         (pretty-print (list-ref p i))
         (edit-loop s p i b)))
   (define not-a-proper-list
      ;; complain and continue.
      (lambda (s p i b)
         (printf "structure is not a proper list~%")
         (edit-loop s p i b)))
   (define cannot-dp-zero
      ;; complain and continue.
      (lambda (s p i b)
         (printf "cannot remove parens from current expression~%")
         (edit-loop s p i b)))
   (define pos2-before-pos1
      ;; complain and continue.
      (lambda (s p i b)
         (printf "second position before first~%")
         (edit-loop s p i b)))
   (define invalid-movement
      ;; complain and continue.
      (lambda (s p i b)
         (printf "no such position~%")
         (edit-loop s p i b)))
   (define unrecognized-command-syntax
      ;; complain and continue.
      (lambda (s p i b)
         (printf "unrecognized command syntax~%")
         (edit-loop s p i b)))
   (define search-failed
      ;; complain and continue.
      (lambda (s p i b)
         (printf "search failed~%")
         (edit-loop s p i b)))
   (define no-previous-find
      ;; complain and continue.
      (lambda (s p i b)
         (printf "no previous find command~%")
         (edit-loop s p i b)))
   (define edit-loop
      ;; read command and process.
      (lambda (s p i b)
         (let ([x (begin (printf "edit> ") (read))])
            (cond
               [(eof-object? x) (newline)] ; need newline after eof
               [(eq? x 'q)] ; do not need newline after q
               [(eq? x 'p) (wr s p i b)]
               [(eq? x '?) (wrlev s p i b)]
               [(eq? x 'pp) (pp s p i b)]
               [(eq? x '??) (pplev s p i b)]
               [(or (integer? x) (eqv? x '*)) (move x s p i b wrlev)]
               [(eq? x 't)
                (let f ([p p] [i i] [b b])
                   (if (null? b)
                       (wrlev s p i b)
                       (apply f b)))]
               [(eq? x 'b)
                (if (pair? b)
                    (apply wrlev s b)
                    (invalid-movement s p i b))]
               [(eq? x 'n)
                (let ([n (maxref p)])
                   (if (and (not (eq? i '*)) (or (not n) (< i n)))
                       (wrlev s p (+ i 1) b)
                       (invalid-movement s p i b)))]
               [(eq? x 'pr)
                (if (and (not (eq? i '*)) (> i 0))
                    (wrlev s p (- i 1) b)
                    (invalid-movement s p i b))]
               [(or (eq? x 'f) (cmdeq? x '(f)))
                (if s
                    (find s p i b)
                    (no-previous-find s p i b))]
               [(cmdeq? x '(f x))
                (find (cons (cadr x) '()) p i b)]
               [(and (cmdeq? x '(r x x))
                     (or (integer? (cadr x)) (eq? (cadr x) '*)))
                (move (cadr x) s p i b
                   (lambda (s0 p0 i0 b0)
                      (list-set! p0 i0 (caddr x))))
                (wrlev s p i b)]
               [(cmdeq? x '(s x x))
                (list-set! p i (subst! (caddr x) (cadr x) (list-ref p i)))
                (wrlev s p i b)]
               [(and (cmdeq? x '(d x)) (eqv? (cadr x) 0))
                (list-set! p i '())
                (wrlev s p i b)]
               [(and (cmdeq? x '(d x)) (eq? (cadr x) '*))
                (move (cadr x) s p i b
                   (lambda (s0 p0 i0 b0)
                      (set-cdr! (last-pair p0) '())
                      (wrlev s p i b)))]
               [(and (cmdeq? x '(d x)) (integer? (cadr x)))
                (move (cadr x) s p i b
                   (lambda (s0 p0 i0 b0)
                      (list-set! p i (list-cut! p0 i0))
                      (wrlev s p i b)))]
               [(and (cmdeq? x '(dp x)) (eqv? (cadr x) 0))
                (let ([e (list-ref p i)])
                   (if (and (pair? e) (null? (cdr e)))
                       (begin (list-set! p i (car e))
                              (wrlev s p i b))
                       (cannot-dp-zero s p i b)))]
               [(and (cmdeq? x '(dp x))
                     (and (integer? (cadr x)) (not (= (cadr x) 0))))
                (move (cadr x) s p i b
                   (lambda (s0 p0 i0 b0)
                      (let ([e0 (list-ref p0 i0)])
                         (if (or (proper-list? e0)
                                 (and (pair? e0) (eqv? i0 (maxref p0))))
                             (begin (if (null? e0)
                                        (list-set! p i (list-cut! p0 i0))
                                        (list-splice! p0 i0 e0))
                                    (wrlev s p i b))
                             (not-a-proper-list s  p i b)))))]
               [(and (or (cmdeq? x '(ap x)) (cmdeq? x '(ap x x)))
                     (memv (cadr x) '(0 *)))
                (move (cadr x) s p i b
                   (lambda (s0 p0 i0 b0)
                      (list-set! p0 i0 (list (list-ref p0 i0)))
                      (wrlev s p i b)))]
               [(and (cmdeq? x '(ap x x))
                     (and (integer? (cadr x)) (not (= (cadr x) 0)))
                     (eq? (caddr x) '*))
                (move (cadr x) s p i b
                   (lambda (s0 p0 i0 b0)
                      (list-ap*! p0 i0)
                      (wrlev s p i b)))]
               [(and (cmdeq? x '(ap x x))
                     (and (integer? (cadr x)) (not (= (cadr x) 0)))
                     (and (integer? (caddr x)) (not (= (caddr x) 0))))
                (move (cadr x) s p i b
                   (lambda (s0 p0 i0 b0)
                      (move (caddr x) s p i b
                         (lambda (s1 p1 i1 b1)
                            (if (>= i1 i0)
                                (begin (list-ap! p0 i0 i1)
                                       (wrlev s p i b))
                                (pos2-before-pos1 s p i b))))))]
               [(and (cmdeq? x '(ib x x))
                     (and (integer? (cadr x)) (not (= (cadr x) 0))))
                (move (cadr x) s p i b
                   (lambda (s0 p0 i0 b0)
                      (list-splice! p0 i0 (list (caddr x) (list-ref p0 i0)))
                      (wrlev s p i b)))]
               [(and (cmdeq? x '(ia x x))
                     (and (integer? (cadr x)) (not (= (cadr x) 0))))
                (move (cadr x) s p i b
                   (lambda (s0 p0 i0 b0)
                      (list-splice! p0 i0 (list (list-ref p0 i0) (caddr x)))
                      (wrlev s p i b)))]
               [(and (cmdeq? x '(sb x x))
                     (and (integer? (cadr x)) (not (= (cadr x) 0))))
                (move (cadr x) s p i b
                   (lambda (s0 p0 i0 b0)
                      (list-splice! p0 i0
                         (append (caddr x) (list (list-ref p0 i0))))
                      (wrlev s p i b)))]
               [(and (cmdeq? x '(sa x x))
                     (and (integer? (cadr x)) (not (= (cadr x) 0))))
                (move (cadr x) s p i b
                   (lambda (s0 p0 i0 b0)
                      (list-splice! p0 i0 (cons (list-ref p0 i0) (caddr x)))
                      (wrlev s p i b)))]
               [else
                (unrecognized-command-syntax s p i b)]))))
   (set! edit
      ;; set up keyboard interrupt handler and go.
      (lambda (e)
         (let ([p (cons e '())])
            (let ([k (call/cc (lambda (k) k))]) ; return here on interrupt
               (parameterize ([keyboard-interrupt-handler
                               (lambda ()
                                  (printf "reset~%")
                                  (k k))])
                  (wrlev #f p 0 '())
                  (car p)))))))
