;;; setof.ss
;;; Copyright (C) 1996 R. Kent Dybvig
;;; from "The Scheme Programming Language, 2ed" by R. Kent Dybvig

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

;;; set-of uses helper syntactic extension set-of-help, passing it
;;; an initial base expression of '()
(define-syntax set-of
  (syntax-rules ()
    ((_ e m ...)
     (set-of-help e '() m ...))))

;;; set-of-help recognizes in, is, and predicate expressions and
;;; changes them into nested named let, let, and if expressions.
(define-syntax set-of-help
  (syntax-rules (in is)
    ((_ e base)
     (set-cons e base))
    ((_ e base (x in s) m ...)
     (let loop ((set s))
       (if (null? set)
           base
           (let ((x (car set)))
             (set-of-help e (loop (cdr set)) m ...)))))
    ((_ e base (x is y) m ...)
     (let ((x y)) (set-of-help e base m ...)))
    ((_ e base p m ...)
     (if p (set-of-help e base m ...) base))))

;;; set-cons returns the original set y if x is already in y.
(define set-cons
  (lambda (x y)
    (if (memv x y)
        y
        (cons x y))))
