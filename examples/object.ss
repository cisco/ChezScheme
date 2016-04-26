;;; object.ss
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

;;; define-object creates an object constructor that uses let* to bind
;;; local fields and letrec to define the exported procedures.  An
;;; object is itself a procedure that accepts messages corresponding
;;; to the names of the exported procedures.  The second pattern is
;;; used to allow the set of local fields to be omitted.
(define-syntax define-object
  (syntax-rules ()
    ((_ (name . varlist)
        ((var1 val1) ...)
        ((var2 val2) ...))
     (define name
       (lambda varlist
         (let* ((var1 val1) ...)
           (letrec ((var2 val2) ...)
             (lambda (msg . args)
               (case msg
                 ((var2) (apply var2 args)) ...
                 (else
                  (error 'name "invalid message ~s"
                     (cons msg args))))))))))
    ((_ (name . varlist)
        ((var2 val2) ...))
     (define-object (name . varlist)
       ()
       ((var2 val2) ...)))))

;;; send-message abstracts the act of sending a message from the act
;;; of applying a procedure and allows the message to be unquoted.
(define-syntax send-message
  (syntax-rules ()
    ((_ obj msg arg ...)
     (obj 'msg arg ...))))
