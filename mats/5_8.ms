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
