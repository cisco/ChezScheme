;;; Ikarus Scheme -- A compiler for R6RS Scheme.
;;; Copyright (C) 2006,2007,2008  Abdulaziz Ghuloum

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

(library (unicode-data)
  (export get-unicode-data)
  (import (rnrs))
  
  (define (find-semi/hash str i n)
    (cond
      [(or (fx=? i n) (memv (string-ref str i) '(#\; #\#))) i]
      [else (find-semi/hash str (+ i 1) n)]))
  
  (define (cleanup str)
    (let ([lo 
           (let f ([i 0] [n (string-length str)])
             (cond
               [(= i n) n]
               [(char=? #\space (string-ref str i)) 
                (f (+ i 1) n)]
               [else i]))]
          [hi
           (let f ([i (- (string-length str) 1)])
             (cond
               [(< i 0) i]
               [(char=? #\space (string-ref str i)) 
                (f (- i 1))]
               [else (+ i 1)]))])
      (if (> hi lo)
          (substring str lo hi)
          "")))

  (define (split str)
    (let f ([i 0] [n (string-length str)])
      (cond
        [(or (= i n) (memv (string-ref str i) '(#\#)))
         '("")]
        [else
         (let ([j (find-semi/hash str i n)])
           (cond
             [(or (= j n) (memv (string-ref str i) '(#\#)))
              (list (cleanup (substring str i j)))]
             [else
              (cons (cleanup (substring str i j))
                    (f (+ j 1) n))]))])))
  
  (define (extract-uni-data)
    (let f ([ls '()])
      (let ([line (get-line (current-input-port))])
        (cond
          [(eof-object? line)
           (reverse ls)]
          [else
           (let ([fields (split line)])
             (if (or (null? fields) (equal? fields '("")))
                 (f ls)
                 (f (cons fields ls))))]))))

  (define (get-unicode-data filename)
    (with-input-from-file 
       filename
       extract-uni-data)))
