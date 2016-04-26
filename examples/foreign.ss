;;; foreign.ss
;;; Copyright (c) 1997 R. Kent Dybvig

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

;;; Prototype code for converting ``foreign-callable'' declarations into
;;; C interface routines to support C calls to Scheme procedures with
;;; automatic datatype conversion analogous to that provided for Scheme
;;; calls to C procedures via foreign-procedure.

;;; Todo
;;;   - support for foreign-pointer and foreign-object
;;;   - similar support for foreign-procedure declarations

(define spec->decl
  (lambda (spec)
    (case spec
      [(integer-32 boolean) "int"]
      [(unsigned-32) "unsigned int"]
      [(char) "char"]
      [(string) "char *"]
      [(fixnum) "int"]
      [(double-float) "double"]
      [(single-float) "float"]
      [(void) "void"]
      [(scheme-object) "ptr"]
      [else
       (record-case spec
         [(foreign-pointer foreign-object) ()
          (error 'spec->decl "unsupported specifier ~s" spec)]
         [else (error 'spec->decl "unexpected specifier ~s" spec)])])))

(define C->Scheme
  (lambda (spec id)
    (case spec
      [(boolean) (format "Sboolean(~a)" id)]
      [(char) (format "Schar(~a)" id)]
      [(fixnum) (format "Sfixnum(~a)" id)]
      [(integer-32) (format "Sinteger(~a)" id)]
      [(unsigned-32) (format "Sunsigned(~a)" id)]
      [(single-float) (format "Sflonum((double)~a)" id)]
      [(double-float) (format "Sflonum(~a)" id)]
      [(scheme-object) id]
      [(string) (format "Sstring(~a)" id)]
      [else
       (record-case spec
         [(foreign-pointer foreign-object) ()
          (error 'C->Scheme "unsupported specifier ~s" spec)]
         [else (error 'C->Scheme "unexpected specifier ~s" spec)])])))

(define Scheme->C
  (lambda (op spec src)
    (case spec
      [(boolean) (fprintf op "Sboolean_value(~a)" src)]
      [(char) (fprintf op "Schar_value(~a)" src)]
      [(fixnum) (fprintf op "Sfixnum_value(~a)" src)]
      [(integer-32) (fprintf op "Sinteger_value(~a)" src)]
      [(unsigned-32) (fprintf op "Sunsigned_value(~a)" src)]
      [(single-float) (fprintf op "(float)Sflonum_value(~a)" src)]
      [(double-float) (fprintf op "Sflonum_value(~a)" src)]
      [(scheme-object) (display src op)]
      [(string) (fprintf op "Sstring_value(~a)" src)]
      [else
       (record-case spec
         [(foreign-pointer foreign-object) ()
          (error 'Scheme->C "unsupported specifier ~s" spec)]
         [else (error 'Scheme->C "unexpected specifier ~s" spec)])])))

(define gen-fcallable
  (case-lambda
    [(cname arg-specs res-spec)
     (gen-fcallable (current-output-port) cname arg-specs res-spec)]
    [(op cname arg-specs res-spec)
     (let ((names (let loop ((ls arg-specs) (i 1))
                    (if (null? ls)
                        '()
                        (cons (format "x~d" i) (loop (cdr ls) (+ i 1))))))
           (count (length arg-specs)))
       (newline op)
       (fprintf op "~a ~a(ptr proc" (spec->decl res-spec) cname) ;)
       (let loop ((arg-specs arg-specs) (names names))
         (unless (null? arg-specs)
           (fprintf op ", ~a ~a" (spec->decl (car arg-specs)) (car names))
           (loop (cdr arg-specs) (cdr names)))) ;(
       (fprintf op ") {~%")
       (if (<= 0 count 3)
           (begin
             (display "    return " op)
             (Scheme->C op res-spec
               (let ((op (open-output-string)))
                 (fprintf op "Scall~d(proc" count) ;)
                 (let loop ((arg-specs arg-specs) (names names))
                   (unless (null? arg-specs)
                     (display ", " op)
                     (display (C->Scheme (car arg-specs) (car names)) op)
                     (loop (cdr arg-specs) (cdr names)))) ;(
                 (fprintf op ")")
                 (get-output-string op))))
             (begin
               (fprintf op "    Sinitframe(~d);~%" count)
               (let loop ([arg-specs arg-specs] [names names] [num 1])
                 (unless (null? arg-specs)
                   (fprintf op "    Sput_arg(~d, ~a);~%"
                      num (C->Scheme (car arg-specs) (car names)))
                   (loop (cdr arg-specs) (cdr names) (+ num 1))))
               (fprintf op "    return ")
               (Scheme->C op res-spec
                 (format "Scall(proc, ~d)" count))))
       (fprintf op ";~%}~%"))]))

(define-syntax foreign-callable
  (syntax-rules ()
    ((_ n args res)
     (gen-fcallable n 'args 'res))))

(define gen-file
  (lambda (fnroot)
    (let ((ifn (format "~a.ss" fnroot))
          (ofn (format "~a.xx" fnroot)))
      (with-output-to-file ofn
        (lambda () (load ifn))
        'replace))))

#!eof ; cut off the input here so we can give examples w/o comment chars

Example input file:

------------------------------------------------------------------------
(foreign-callable "foo"
  (boolean single-float double-float)
  scheme-object)

(foreign-callable "bar"
  (boolean char integer-32 unsigned-32 single-float
    double-float scheme-object)
  string)

(foreign-callable "baz" () fixnum)
------------------------------------------------------------------------

Generated output file:

------------------------------------------------------------------------
ptr foo(ptr proc, int x1, float x2, double x3) {
    return Scall3(proc, Sboolean(x1), Sflonum((double)x2), Sflonum(x3));
}

char * bar(ptr proc, int x1, char x2, int x3, unsigned int x4, float x5, double x6, ptr x7) {
    Sinitframe(7);
    Sput_arg(1, Sboolean(x1));
    Sput_arg(2, Schar(x2));
    Sput_arg(3, Sinteger(x3));
    Sput_arg(4, Sunsigned(x4));
    Sput_arg(5, Sflonum((double)x5));
    Sput_arg(6, Sflonum(x6));
    Sput_arg(7, x7);
    return Sstring_value(Scall(proc, 7));
}

int baz(ptr proc) {
    return Sfixnum_value(Scall0(proc));
}
------------------------------------------------------------------------
