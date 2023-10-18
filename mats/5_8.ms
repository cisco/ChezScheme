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

(mat box
    (box? (box 3))
    (equal? (box 'a) '#&a)
    (equal? (box '(a b c)) '#&(a b c))
    (not (eq? (box '()) (box '())))
 )

(mat unbox
    (equal? (unbox '#&3) 3)
    (equal? (unbox (box 3)) 3)
 )

(mat set-box!
    (let ((x (box 3)))
        (set-box! x 4)
        (and (equal? x '#&4) (equal? (unbox x) 4)))
 )

(mat box-cas!
  (begin
    (define bx1 (box 1))
    (define bx2 (box 'apple))
    (eq? 1 (unbox bx1)))
  (not (box-cas! bx1 0 1))
  (eq? 1 (unbox bx1))
  (retry-for-spurious (box-cas! bx1 1 2))
  (eq? 2 (unbox bx1))

  (not (box-cas! bx2 #f 'banana))
  (retry-for-spurious (box-cas! bx2 'apple 'banana))
  (not (box-cas! bx2 'apple 'banana))
  (eq? 'banana (unbox bx2))

  (not (box-cas! (box (bitwise-arithmetic-shift-left 1 40))
                 (bitwise-arithmetic-shift-left 2 40)
                 'wrong))

  (error? (box-cas! bx1)) ; arity
  (error? (box-cas! bx1 1)) ; arity
  (error? (box-cas! 1 bx1 2)) ; not a box
  (error? (box-cas! (box-immutable 1) 1 2)) ; not a mutable box

  ;; make sure `box-cas!` works with GC generations:
  (begin
    (collect 0)
    (let ([g1 (gensym)])
      (and (retry-for-spurious (box-cas! bx2 'banana g1))
           (begin
             (collect 0)
             (eq? g1 (unbox bx2))))))
  )
