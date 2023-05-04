;;; 5-7.ms
;;; Copyright 1984-2017 Cisco Systems, Inc.
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

(mat string->symbol
    (eq? (string->symbol "foo") 'foo)
    (eq? (string->symbol "a") (string->symbol "a"))
    (error? (string->symbol 3))
    (error? (string->symbol 'a))
 )

(mat gensym
    (not (eq? (gensym "hi") 'hi))
    (not (eq? (gensym "hi")
         (gensym "hi")))
    (equal? (symbol->string (gensym "hi")) "hi")
    (immutable-string? (symbol->string (gensym "hi")))
    (error? (gensym '#(a b c)))
 )

(mat gensym
  (error? (gensym 'hitme!))
  (error? (gensym 17))
  (error? (gensym #f))
  (error? (gensym 'hitme "a"))
  (error? (gensym 17 "a"))
  (error? (gensym #f "a"))
  (error? (gensym "a" 'hitme))
  (error? (gensym "a" 17))
  (error? (gensym "a" #f))
  (symbol? (gensym))
  (gensym? (gensym))
  (not (eq? (gensym) (gensym)))
  (not (equal? (symbol->string (gensym)) (symbol->string (gensym))))
  (parameterize ([gensym-count 1000] [gensym-prefix "xxx"])
    (equal? (symbol->string (gensym)) "xxx1000"))
  (error? (gensym-count -1))
  (error? (gensym-count 'a))
  (error? (gensym-count "3.4"))
  (equal? (parameterize ([gensym-count 73]) (format "~a" (gensym)))
    "g73")
  (equal?
    (let* ([g1 (with-input-from-string "#{pn1 un1}" read)] [g2 (gensym "pn1" "un1")])
      (list (gensym? g1) (gensym? g2) (eq? g1 g2)))
    '(#t #t #t))
  (equal?
    (let* ([g1 (gensym "pn2" "un2")] [g2 (with-input-from-string "#{pn2 un2}" read)])
      (list (gensym? g1) (gensym? g2) (eq? g1 g2)))
    '(#t #t #t))
 )

(mat gensym?
    (gensym? (gensym "foo"))
    (not (gensym? 'foo))
    (not (gensym? (string->symbol "foo")))
    (not (gensym? '(a b)))
 )

(mat symbol->string
    (equal? (symbol->string 'foo) "foo")
    (immutable-string? (symbol->string 'foo))
    (equal? (symbol->string (string->symbol "hi")) "hi")
    (equal? (symbol->string (gensym "hi there")) "hi there")
    (error? (symbol->string 3))
 )

(mat gensym->unique-string
  (error? ; not a gensym
    (gensym->unique-string "spam"))
  (error? ; not a gensym
    (gensym->unique-string 3))
  (error? ; not a gensym
    (gensym->unique-string 'spam))
  (immutable-string? (gensym->unique-string (gensym)))
  (equal?
    (gensym->unique-string '#{g0 e6sfz8u1obe67hsew4stu0-0})
    "e6sfz8u1obe67hsew4stu0-0")
  (let* ([s (gensym->unique-string (gensym))]
         [l (string-length s)]
         [dash-pos (let loop ([i (- l 1)])
                     (if (char=? #\- (string-ref s i))
                         (add1 i)
                         (loop (sub1 i))))]
         [k (substring s 0 dash-pos)]
         [n (string->number (substring s dash-pos l))]
         [next-un (string-append k (number->string (+ n 1)))])
    (define thief (gensym "pn" next-un)) ; steals next-un
    (define gs (gensym))
    (guard (c [(and (who-condition? c)
                    (equal? (condition-who c) "intern-gensym")
                    (format-condition? c)
                    (equal? (condition-message c) "unique name ~s already interned")
                    (irritants-condition? c)
                    (equal? (condition-irritants c) (list next-un)))
               ; This exception is expected, swallow it.
               (void)])
      (gensym->unique-string gs)
      (error "mat" "(gensym->unique-string gs) is expected to fail but succeeded"))
    ; A second try of (gensym->unique-string gs) should succeed
    ; and gs should be properly interned.
    (let ([gs-un (gensym->unique-string gs)])
      (and (eq? gs (gensym (symbol->string gs) gs-un))
           (not (equal? gs-un (gensym->unique-string thief))))))
)

(mat putprop-getprop
    (begin (putprop 'xyz 'key 'value) (eq? (getprop 'xyz 'key) 'value))
    (begin (putprop 'xyz 'key 'new-value) (eq? (getprop 'xyz 'key) 'new-value))
    (begin (putprop 'xyz 'key #f) (not (getprop 'xyz 'key)))
    (begin (putprop 'xyz 'key #t)
           (remprop 'xyz 'key)
           (not (getprop 'xyz 'key)))
    (let ([g (gensym)] [flag (box 0)])
       (and (eq? (getprop g 'a flag) flag)
            (begin (putprop g 'a 'b)
                   (and (eq? (getprop g 'a) 'b)
                        (equal? (property-list g) '(a b))))))
    (begin (putprop 'x 'a 'b)
           (putprop 'x 'b 'c)
           (eq? (getprop 'x (getprop 'x (getprop 'x '? 'a) 0) 1) 'c))
    (error? (getprop 3 'key))
    (error? (putprop "hi" 'key 'value))
    (error? (property-list '(a b c)))
 )

(mat uninterned-symbol
     (symbol? (string->uninterned-symbol "hello"))
     (uninterned-symbol? (string->uninterned-symbol "hello"))
     (not (gensym? (string->uninterned-symbol "hello")))

     (equal? "hello" (symbol->string (string->uninterned-symbol "hello")))
     (immutable-string? (symbol->string (string->uninterned-symbol "hello")))

     (not (eq? (string->uninterned-symbol "hello")
               (string->uninterned-symbol "hello")))

     (let-values ([(o get) (open-bytevector-output-port)])
       (let ([s (string->uninterned-symbol "hello")])
         (fasl-write (list s s) o)
         (let ([r (fasl-read (open-bytevector-input-port (get)))])
           (and (eq? (car r) (cadr r))
                (equal? "hello" (symbol->string (car r)))))))

     )
