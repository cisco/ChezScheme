;;; 5-4.ms
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

(mat char=?/char-ci=?
    (error? (char=?))
    (error? (char=? 'a))
    (error? (char=? #\a 'a))
    (error? (char=? #\a 'a #\b))
    (error? (char=? 'a #\b #\a))
    (error? (char=? #\a #\c 'a #\b))
    (error? (char-ci=?))
    (error? (char-ci=? 'a))
    (error? (char-ci=? #\a 'a))
    (error? (char-ci=? #\a 'a #\b))
    (error? (char-ci=? 'a #\b #\a))
    (error? (char-ci=? #\a #\c 'a #\b))
    (char=? #\a #\a)
    (char-ci=? #\a #\a)
    (not (char=? #\a #\b))
    (not (char-ci=? #\a #\b))
    (not (char=? #\b #\a))
    (not (char-ci=? #\b #\a))
    (not (char=? #\a #\A))
    (char-ci=? #\a #\A)
    (char=? #\a)
    (char=? #\a #\a #\a #\a)
    (not (char=? #\a #\b #\c #\d))
    (not (char=? #\z #\t #\m #\d))
    (not (char=? #\a #\t #\m #\d))
    (not (char=? #\a #\A #\a #\A))
    (not (char=? #\a #\B #\C #\d))
    (not (char=? #\Z #\t #\m #\D))
    (char-ci=? #\a)
    (char-ci=? #\a #\a #\a #\a)
    (not (char-ci=? #\a #\b #\c #\d))
    (not (char-ci=? #\z #\t #\m #\d))
    (not (char-ci=? #\a #\t #\m #\d))
    (char-ci=? #\a #\A #\a #\A)
    (not (char-ci=? #\a #\B #\C #\d))
    (not (char-ci=? #\Z #\t #\m #\D))
    (guard (c [#t #t]) (char=? #\3 #\4 (error #f "oops")))
    (guard (c [#t #t]) (char=? #\3 (error #f "oops") #\4))
    (guard (c [#t #t]) (char=? (error #f "oops") #\3 #\4))
    (guard (c [#t #t]) (not (char=? (error #f "oops"))))
    (guard (c [#t #t]) (char-ci=? #\3 #\4 (error #f "oops")))
    (guard (c [#t #t]) (char-ci=? #\3 (error #f "oops") #\4))
    (guard (c [#t #t]) (char-ci=? (error #f "oops") #\3 #\4))
    (guard (c [#t #t]) (not (char-ci=? (error #f "oops"))))
 )

(mat char<?/char-ci<?
    (error? (char<?))
    (error? (char<? 'a))
    (error? (char<? #\a 'a))
    (error? (char<? #\a 'a #\b))
    (error? (char<? 'a #\b #\a))
    (error? (char<? #\a #\c 'a #\b))
    (error? (char-ci<?))
    (error? (char-ci<? 'a))
    (error? (char-ci<? #\a 'a))
    (error? (char-ci<? #\a 'a #\b))
    (error? (char-ci<? 'a #\b #\a))
    (error? (char-ci<? #\a #\c 'a #\b))
    (not (char<? #\a #\a))
    (not (char-ci<? #\a #\a))
    (char<? #\a #\b)
    (char-ci<? #\a #\b)
    (not (char<? #\b #\a))
    (not (char-ci<? #\b #\a))
    (char<? #\A #\a)
    (not (char-ci<? #\A #\a))
    (char<? #\a)
    (not (char<? #\a #\a #\a #\a))
    (char<? #\a #\b #\c #\d)
    (not (char<? #\z #\t #\m #\d))
    (not (char<? #\a #\t #\m #\d))
    (not (char<? #\a #\A #\a #\A))
    (not (char<? #\a #\B #\C #\d))
    (not (char<? #\Z #\t #\m #\D))
    (char-ci<? #\a)
    (not (char-ci<? #\a #\a #\a #\a))
    (char-ci<? #\a #\b #\c #\d)
    (not (char-ci<? #\z #\t #\m #\d))
    (not (char-ci<? #\a #\t #\m #\d))
    (not (char-ci<? #\a #\A #\a #\A))
    (char-ci<? #\a #\B #\C #\d)
    (not (char-ci<? #\Z #\t #\m #\D))
    (guard (c [#t #t]) (char<? #\4 #\3 (error #f "oops")))
    (guard (c [#t #t]) (char<? #\4 (error #f "oops") #\3))
    (guard (c [#t #t]) (char<? (error #f "oops") #\4 #\3))
    (guard (c [#t #t]) (not (char<? (error #f "oops"))))
    (guard (c [#t #t]) (char-ci<? #\4 #\3 (error #f "oops")))
    (guard (c [#t #t]) (char-ci<? #\4 (error #f "oops") #\3))
    (guard (c [#t #t]) (char-ci<? (error #f "oops") #\4 #\3))
    (guard (c [#t #t]) (not (char-ci<? (error #f "oops"))))
 )

(mat char>?/char-ci>?
    (error? (char>?))
    (error? (char>? 'a))
    (error? (char>? #\a 'a))
    (error? (char>? #\a 'a #\b))
    (error? (char>? 'a #\b #\a))
    (error? (char>? #\a #\c 'a #\b))
    (error? (char-ci>?))
    (error? (char-ci>? 'a))
    (error? (char-ci>? #\a 'a))
    (error? (char-ci>? #\a 'a #\b))
    (error? (char-ci>? 'a #\b #\a))
    (error? (char-ci>? #\a #\c 'a #\b))
    (not (char>? #\a #\a))
    (not (char-ci>? #\a #\b))
    (char>? #\b #\a)
    (char-ci>? #\b #\a)
    (char>? #\a #\A)
    (not (char-ci>? #\a #\A))
    (char>? #\a)
    (not (char>? #\a #\a #\a #\a))
    (not (char>? #\a #\b #\c #\d))
    (char>? #\z #\t #\m #\d)
    (not (char>? #\a #\t #\m #\d))
    (not (char>? #\a #\A #\a #\A))
    (not (char>? #\a #\B #\C #\d))
    (not (char>? #\Z #\t #\m #\D))
    (char-ci>? #\a)
    (not (char-ci>? #\a #\a #\a #\a))
    (not (char-ci>? #\a #\b #\c #\d))
    (char-ci>? #\z #\t #\m #\d)
    (not (char-ci>? #\a #\t #\m #\d))
    (not (char-ci>? #\a #\A #\a #\A))
    (not (char-ci>? #\a #\B #\C #\d))
    (char-ci>? #\Z #\t #\m #\D)
    (guard (c [#t #t]) (char>? #\3 #\4 (error #f "oops")))
    (guard (c [#t #t]) (char>? #\3 (error #f "oops") #\4))
    (guard (c [#t #t]) (char>? (error #f "oops") #\3 #\4))
    (guard (c [#t #t]) (not (char>? (error #f "oops"))))
    (guard (c [#t #t]) (char-ci>? #\3 #\4 (error #f "oops")))
    (guard (c [#t #t]) (char-ci>? #\3 (error #f "oops") #\4))
    (guard (c [#t #t]) (char-ci>? (error #f "oops") #\3 #\4))
    (guard (c [#t #t]) (not (char-ci>? (error #f "oops"))))
 )

(mat char<=?/char-ci<=?
    (error? (char<=?))
    (error? (char<=? 'a))
    (error? (char<=? #\a 'a))
    (error? (char<=? #\a 'a #\b))
    (error? (char<=? 'a #\b #\a))
    (error? (char<=? #\a #\c 'a #\b))
    (error? (char-ci<=?))
    (error? (char-ci<=? 'a))
    (error? (char-ci<=? #\a 'a))
    (error? (char-ci<=? #\a 'a #\b))
    (error? (char-ci<=? 'a #\b #\a))
    (error? (char-ci<=? #\a #\c 'a #\b))
    (char<=? #\a #\a)
    (char-ci<=? #\a #\a)
    (char<=? #\a #\b)
    (char-ci<=? #\a #\b)
    (not (char<=? #\b #\a))
    (not (char-ci<=? #\b #\a))
    (not (char<=? #\a #\A))
    (char-ci<=? #\a #\A)
    (char<=? #\a)
    (char<=? #\a #\a #\a #\a)
    (char<=? #\a #\b #\c #\d)
    (not (char<=? #\z #\t #\m #\d))
    (not (char<=? #\a #\t #\m #\d))
    (not (char<=? #\a #\A #\a #\A))
    (not (char<=? #\a #\B #\C #\d))
    (not (char<=? #\Z #\t #\m #\D))
    (char-ci<=? #\a)
    (char-ci<=? #\a #\a #\a #\a)
    (char-ci<=? #\a #\b #\c #\d)
    (not (char-ci<=? #\z #\t #\m #\d))
    (not (char-ci<=? #\a #\t #\m #\d))
    (char-ci<=? #\a #\A #\a #\A)
    (char-ci<=? #\a #\B #\C #\d)
    (not (char-ci<=? #\Z #\t #\m #\D))
    (guard (c [#t #t]) (char<=? #\4 #\3 (error #f "oops")))
    (guard (c [#t #t]) (char<=? #\4 (error #f "oops") #\3))
    (guard (c [#t #t]) (char<=? (error #f "oops") #\4 #\3))
    (guard (c [#t #t]) (not (char<=? (error #f "oops"))))
    (guard (c [#t #t]) (char-ci<=? #\4 #\3 (error #f "oops")))
    (guard (c [#t #t]) (char-ci<=? #\4 (error #f "oops") #\3))
    (guard (c [#t #t]) (char-ci<=? (error #f "oops") #\4 #\3))
    (guard (c [#t #t]) (not (char-ci<=? (error #f "oops"))))
 )

(mat char>=?/char-ci>=?
    (error? (char>=?))
    (error? (char>=? 'a))
    (error? (char>=? #\a 'a))
    (error? (char>=? #\a 'a #\b))
    (error? (char>=? 'a #\b #\a))
    (error? (char>=? #\a #\c 'a #\b))
    (error? (char-ci>=?))
    (error? (char-ci>=? 'a))
    (error? (char-ci>=? #\a 'a))
    (error? (char-ci>=? #\a 'a #\b))
    (error? (char-ci>=? 'a #\b #\a))
    (error? (char-ci>=? #\a #\c 'a #\b))
    (char>=? #\a #\a)
    (char-ci>=? #\a #\a)
    (not (char>=? #\a #\b))
    (not (char-ci>=? #\a #\b))
    (char>=? #\b #\a)
    (char-ci>=? #\b #\a)
    (not (char>=? #\A #\a))
    (char-ci>=? #\A #\a)
    (char>=? #\a)
    (char>=? #\a #\a #\a #\a)
    (not (char>=? #\a #\b #\c #\d))
    (char>=? #\z #\t #\m #\d)
    (not (char>=? #\a #\t #\m #\d))
    (not (char>=? #\a #\A #\a #\A))
    (not (char>=? #\a #\B #\C #\d))
    (not (char>=? #\Z #\t #\m #\D))
    (char-ci>=? #\a)
    (char-ci>=? #\a #\a #\a #\a)
    (not (char-ci>=? #\a #\b #\c #\d))
    (char-ci>=? #\z #\t #\m #\d)
    (not (char-ci>=? #\a #\t #\m #\d))
    (char-ci>=? #\a #\A #\a #\A)
    (not (char-ci>=? #\a #\B #\C #\d))
    (char-ci>=? #\Z #\t #\m #\D)
    (guard (c [#t #t]) (char>=? #\3 #\4 (error #f "oops")))
    (guard (c [#t #t]) (char>=? #\3 (error #f "oops") #\4))
    (guard (c [#t #t]) (char>=? (error #f "oops") #\3 #\4))
    (guard (c [#t #t]) (not (char>=? (error #f "oops"))))
    (guard (c [#t #t]) (char-ci>=? #\3 #\4 (error #f "oops")))
    (guard (c [#t #t]) (char-ci>=? #\3 (error #f "oops") #\4))
    (guard (c [#t #t]) (char-ci>=? (error #f "oops") #\3 #\4))
    (guard (c [#t #t]) (not (char-ci>=? (error #f "oops"))))
 )

(mat r6rs:char=?/r6rs:char-ci=?
    (error? (r6rs:char=?))
    (error? (r6rs:char=? 'a))
    (error? (r6rs:char=? #\a 'a))
    (error? (r6rs:char=? #\a 'a #\b))
    (error? (r6rs:char=? 'a #\b #\a))
    (error? (r6rs:char=? #\a #\c 'a #\b))
    (error? (r6rs:char-ci=?))
    (error? (r6rs:char-ci=? 'a))
    (error? (r6rs:char-ci=? #\a 'a))
    (error? (r6rs:char-ci=? #\a 'a #\b))
    (error? (r6rs:char-ci=? 'a #\b #\a))
    (error? (r6rs:char-ci=? #\a #\c 'a #\b))
    (r6rs:char=? #\a #\a)
    (r6rs:char-ci=? #\a #\a)
    (not (r6rs:char=? #\a #\b))
    (not (r6rs:char-ci=? #\a #\b))
    (not (r6rs:char=? #\b #\a))
    (not (r6rs:char-ci=? #\b #\a))
    (not (r6rs:char=? #\a #\A))
    (r6rs:char-ci=? #\a #\A)
    (r6rs:char=? #\a #\a #\a #\a)
    (not (r6rs:char=? #\a #\b #\c #\d))
    (not (r6rs:char=? #\z #\t #\m #\d))
    (not (r6rs:char=? #\a #\t #\m #\d))
    (not (r6rs:char=? #\a #\A #\a #\A))
    (not (r6rs:char=? #\a #\B #\C #\d))
    (not (r6rs:char=? #\Z #\t #\m #\D))
    (r6rs:char-ci=? #\a #\a #\a #\a)
    (not (r6rs:char-ci=? #\a #\b #\c #\d))
    (not (r6rs:char-ci=? #\z #\t #\m #\d))
    (not (r6rs:char-ci=? #\a #\t #\m #\d))
    (r6rs:char-ci=? #\a #\A #\a #\A)
    (not (r6rs:char-ci=? #\a #\B #\C #\d))
    (not (r6rs:char-ci=? #\Z #\t #\m #\D))
    (guard (c [#t #t]) (r6rs:char=? #\3 #\4 (error #f "oops")))
    (guard (c [#t #t]) (r6rs:char=? #\3 (error #f "oops") #\4))
    (guard (c [#t #t]) (r6rs:char=? (error #f "oops") #\3 #\4))
    (guard (c [#t #t]) (not (r6rs:char=? (error #f "oops"))))
    (guard (c [#t #t]) (r6rs:char-ci=? #\3 #\4 (error #f "oops")))
    (guard (c [#t #t]) (r6rs:char-ci=? #\3 (error #f "oops") #\4))
    (guard (c [#t #t]) (r6rs:char-ci=? (error #f "oops") #\3 #\4))
    (guard (c [#t #t]) (not (r6rs:char-ci=? (error #f "oops"))))
 )

(mat r6rs:char<?/r6rs:char-ci<?
    (error? (r6rs:char<?))
    (error? (r6rs:char<? 'a))
    (error? (r6rs:char<? #\a 'a))
    (error? (r6rs:char<? #\a 'a #\b))
    (error? (r6rs:char<? 'a #\b #\a))
    (error? (r6rs:char<? #\a #\c 'a #\b))
    (error? (r6rs:char-ci<?))
    (error? (r6rs:char-ci<? 'a))
    (error? (r6rs:char-ci<? #\a 'a))
    (error? (r6rs:char-ci<? #\a 'a #\b))
    (error? (r6rs:char-ci<? 'a #\b #\a))
    (error? (r6rs:char-ci<? #\a #\c 'a #\b))
    (not (r6rs:char<? #\a #\a))
    (not (r6rs:char-ci<? #\a #\a))
    (r6rs:char<? #\a #\b)
    (r6rs:char-ci<? #\a #\b)
    (not (r6rs:char<? #\b #\a))
    (not (r6rs:char-ci<? #\b #\a))
    (r6rs:char<? #\A #\a)
    (not (r6rs:char-ci<? #\A #\a))
    (not (r6rs:char<? #\a #\a #\a #\a))
    (r6rs:char<? #\a #\b #\c #\d)
    (not (r6rs:char<? #\z #\t #\m #\d))
    (not (r6rs:char<? #\a #\t #\m #\d))
    (not (r6rs:char<? #\a #\A #\a #\A))
    (not (r6rs:char<? #\a #\B #\C #\d))
    (not (r6rs:char<? #\Z #\t #\m #\D))
    (not (r6rs:char-ci<? #\a #\a #\a #\a))
    (r6rs:char-ci<? #\a #\b #\c #\d)
    (not (r6rs:char-ci<? #\z #\t #\m #\d))
    (not (r6rs:char-ci<? #\a #\t #\m #\d))
    (not (r6rs:char-ci<? #\a #\A #\a #\A))
    (r6rs:char-ci<? #\a #\B #\C #\d)
    (not (r6rs:char-ci<? #\Z #\t #\m #\D))
    (guard (c [#t #t]) (r6rs:char<? #\4 #\3 (error #f "oops")))
    (guard (c [#t #t]) (r6rs:char<? #\4 (error #f "oops") #\3))
    (guard (c [#t #t]) (r6rs:char<? (error #f "oops") #\4 #\3))
    (guard (c [#t #t]) (not (r6rs:char<? (error #f "oops"))))
    (guard (c [#t #t]) (r6rs:char-ci<? #\4 #\3 (error #f "oops")))
    (guard (c [#t #t]) (r6rs:char-ci<? #\4 (error #f "oops") #\3))
    (guard (c [#t #t]) (r6rs:char-ci<? (error #f "oops") #\4 #\3))
    (guard (c [#t #t]) (not (r6rs:char-ci<? (error #f "oops"))))
 )

(mat r6rs:char>?/r6rs:char-ci>?
    (error? (r6rs:char>?))
    (error? (r6rs:char>? 'a))
    (error? (r6rs:char>? #\a 'a))
    (error? (r6rs:char>? #\a 'a #\b))
    (error? (r6rs:char>? 'a #\b #\a))
    (error? (r6rs:char>? #\a #\c 'a #\b))
    (error? (r6rs:char-ci>?))
    (error? (r6rs:char-ci>? 'a))
    (error? (r6rs:char-ci>? #\a 'a))
    (error? (r6rs:char-ci>? #\a 'a #\b))
    (error? (r6rs:char-ci>? 'a #\b #\a))
    (error? (r6rs:char-ci>? #\a #\c 'a #\b))
    (not (r6rs:char>? #\a #\a))
    (not (r6rs:char-ci>? #\a #\b))
    (r6rs:char>? #\b #\a)
    (r6rs:char-ci>? #\b #\a)
    (r6rs:char>? #\a #\A)
    (not (r6rs:char-ci>? #\a #\A))
    (not (r6rs:char>? #\a #\a #\a #\a))
    (not (r6rs:char>? #\a #\b #\c #\d))
    (r6rs:char>? #\z #\t #\m #\d)
    (not (r6rs:char>? #\a #\t #\m #\d))
    (not (r6rs:char>? #\a #\A #\a #\A))
    (not (r6rs:char>? #\a #\B #\C #\d))
    (not (r6rs:char>? #\Z #\t #\m #\D))
    (not (r6rs:char-ci>? #\a #\a #\a #\a))
    (not (r6rs:char-ci>? #\a #\b #\c #\d))
    (r6rs:char-ci>? #\z #\t #\m #\d)
    (not (r6rs:char-ci>? #\a #\t #\m #\d))
    (not (r6rs:char-ci>? #\a #\A #\a #\A))
    (not (r6rs:char-ci>? #\a #\B #\C #\d))
    (r6rs:char-ci>? #\Z #\t #\m #\D)
    (guard (c [#t #t]) (r6rs:char>? #\3 #\4 (error #f "oops")))
    (guard (c [#t #t]) (r6rs:char>? #\3 (error #f "oops") #\4))
    (guard (c [#t #t]) (r6rs:char>? (error #f "oops") #\3 #\4))
    (guard (c [#t #t]) (not (r6rs:char>? (error #f "oops"))))
    (guard (c [#t #t]) (r6rs:char-ci>? #\3 #\4 (error #f "oops")))
    (guard (c [#t #t]) (r6rs:char-ci>? #\3 (error #f "oops") #\4))
    (guard (c [#t #t]) (r6rs:char-ci>? (error #f "oops") #\3 #\4))
    (guard (c [#t #t]) (not (r6rs:char-ci>? (error #f "oops"))))
 )

(mat r6rs:char<=?/r6rs:char-ci<=?
    (error? (r6rs:char<=?))
    (error? (r6rs:char<=? 'a))
    (error? (r6rs:char<=? #\a 'a))
    (error? (r6rs:char<=? #\a 'a #\b))
    (error? (r6rs:char<=? 'a #\b #\a))
    (error? (r6rs:char<=? #\a #\c 'a #\b))
    (error? (r6rs:char-ci<=?))
    (error? (r6rs:char-ci<=? 'a))
    (error? (r6rs:char-ci<=? #\a 'a))
    (error? (r6rs:char-ci<=? #\a 'a #\b))
    (error? (r6rs:char-ci<=? 'a #\b #\a))
    (error? (r6rs:char-ci<=? #\a #\c 'a #\b))
    (r6rs:char<=? #\a #\a)
    (r6rs:char-ci<=? #\a #\a)
    (r6rs:char<=? #\a #\b)
    (r6rs:char-ci<=? #\a #\b)
    (not (r6rs:char<=? #\b #\a))
    (not (r6rs:char-ci<=? #\b #\a))
    (not (r6rs:char<=? #\a #\A))
    (r6rs:char-ci<=? #\a #\A)
    (r6rs:char<=? #\a #\a #\a #\a)
    (r6rs:char<=? #\a #\b #\c #\d)
    (not (r6rs:char<=? #\z #\t #\m #\d))
    (not (r6rs:char<=? #\a #\t #\m #\d))
    (not (r6rs:char<=? #\a #\A #\a #\A))
    (not (r6rs:char<=? #\a #\B #\C #\d))
    (not (r6rs:char<=? #\Z #\t #\m #\D))
    (r6rs:char-ci<=? #\a #\a #\a #\a)
    (r6rs:char-ci<=? #\a #\b #\c #\d)
    (not (r6rs:char-ci<=? #\z #\t #\m #\d))
    (not (r6rs:char-ci<=? #\a #\t #\m #\d))
    (r6rs:char-ci<=? #\a #\A #\a #\A)
    (r6rs:char-ci<=? #\a #\B #\C #\d)
    (not (r6rs:char-ci<=? #\Z #\t #\m #\D))
    (guard (c [#t #t]) (r6rs:char<=? #\4 #\3 (error #f "oops")))
    (guard (c [#t #t]) (r6rs:char<=? #\4 (error #f "oops") #\3))
    (guard (c [#t #t]) (r6rs:char<=? (error #f "oops") #\4 #\3))
    (guard (c [#t #t]) (not (r6rs:char<=? (error #f "oops"))))
    (guard (c [#t #t]) (r6rs:char-ci<=? #\4 #\3 (error #f "oops")))
    (guard (c [#t #t]) (r6rs:char-ci<=? #\4 (error #f "oops") #\3))
    (guard (c [#t #t]) (r6rs:char-ci<=? (error #f "oops") #\4 #\3))
    (guard (c [#t #t]) (not (r6rs:char-ci<=? (error #f "oops"))))
 )

(mat r6rs:char>=?/r6rs:char-ci>=?
    (error? (r6rs:char>=?))
    (error? (r6rs:char>=? 'a))
    (error? (r6rs:char>=? #\a 'a))
    (error? (r6rs:char>=? #\a 'a #\b))
    (error? (r6rs:char>=? 'a #\b #\a))
    (error? (r6rs:char>=? #\a #\c 'a #\b))
    (error? (r6rs:char-ci>=?))
    (error? (r6rs:char-ci>=? 'a))
    (error? (r6rs:char-ci>=? #\a 'a))
    (error? (r6rs:char-ci>=? #\a 'a #\b))
    (error? (r6rs:char-ci>=? 'a #\b #\a))
    (error? (r6rs:char-ci>=? #\a #\c 'a #\b))
    (r6rs:char>=? #\a #\a)
    (r6rs:char-ci>=? #\a #\a)
    (not (r6rs:char>=? #\a #\b))
    (not (r6rs:char-ci>=? #\a #\b))
    (r6rs:char>=? #\b #\a)
    (r6rs:char-ci>=? #\b #\a)
    (not (r6rs:char>=? #\A #\a))
    (r6rs:char-ci>=? #\A #\a)
    (r6rs:char>=? #\a #\a #\a #\a)
    (not (r6rs:char>=? #\a #\b #\c #\d))
    (r6rs:char>=? #\z #\t #\m #\d)
    (not (r6rs:char>=? #\a #\t #\m #\d))
    (not (r6rs:char>=? #\a #\A #\a #\A))
    (not (r6rs:char>=? #\a #\B #\C #\d))
    (not (r6rs:char>=? #\Z #\t #\m #\D))
    (r6rs:char-ci>=? #\a #\a #\a #\a)
    (not (r6rs:char-ci>=? #\a #\b #\c #\d))
    (r6rs:char-ci>=? #\z #\t #\m #\d)
    (not (r6rs:char-ci>=? #\a #\t #\m #\d))
    (r6rs:char-ci>=? #\a #\A #\a #\A)
    (not (r6rs:char-ci>=? #\a #\B #\C #\d))
    (r6rs:char-ci>=? #\Z #\t #\m #\D)
    (guard (c [#t #t]) (r6rs:char>=? #\3 #\4 (error #f "oops")))
    (guard (c [#t #t]) (r6rs:char>=? #\3 (error #f "oops") #\4))
    (guard (c [#t #t]) (r6rs:char>=? (error #f "oops") #\3 #\4))
    (guard (c [#t #t]) (not (r6rs:char>=? (error #f "oops"))))
    (guard (c [#t #t]) (r6rs:char-ci>=? #\3 #\4 (error #f "oops")))
    (guard (c [#t #t]) (r6rs:char-ci>=? #\3 (error #f "oops") #\4))
    (guard (c [#t #t]) (r6rs:char-ci>=? (error #f "oops") #\3 #\4))
    (guard (c [#t #t]) (not (r6rs:char-ci>=? (error #f "oops"))))
 )

(mat char-alphabetic?
    (error? (char-alphabetic?))
    (error? (char-alphabetic? #\a #\b))
    (error? (char-alphabetic? 'a))
    (char-alphabetic? #\z)
    (not (char-alphabetic? #\3))
    (char-alphabetic? #\A)
    (not (char-alphabetic? #\space))
 )

(mat char-numeric?
    (error? (char-numeric?))
    (error? (char-numeric? #\a #\b))
    (error? (char-numeric? 'a))
    (not (char-numeric? #\k))
    (char-numeric? #\0)
    (char-numeric? #\4)
    (char-numeric? #\9)
    (not (char-numeric? #\newline))
 )

(mat char-lower-case?
    (error? (char-lower-case?))
    (error? (char-lower-case? #\a #\b))
    (error? (char-lower-case? 'a))
    (char-lower-case? #\z)
    (not (char-lower-case? #\A))
 )

(mat char-upper-case?
    (error? (char-upper-case?))
    (error? (char-upper-case? #\a #\b))
    (error? (char-upper-case? 'a))
    (char-upper-case? #\A)
    (not (char-upper-case? #\z))
 )

(mat char-title-case?
    (error? (char-title-case?))
    (error? (char-title-case? #\a #\b))
    (error? (char-title-case? 'a))
    (char-title-case? #\x01C5)
    (not (char-title-case? #\z))
 )

(mat char-general-category
    (error? (char-general-category))
    (error? (char-general-category #\a #\b))
    (error? (char-general-category 'a))
    (eq? (char-general-category #\A) 'Lu)
    (eq? (char-general-category #\z) 'Ll)
 )

(mat char-whitespace?
    (error? (char-whitespace?))
    (error? (char-whitespace? #\a #\b))
    (error? (char-whitespace? 'a))
    (char-whitespace? #\space)
    (char-whitespace? #\return)
    (not (char-whitespace? #\F))
    (char-whitespace? #\newline)
    (char-whitespace? #\tab)
    (not (char-whitespace? #\%))
    (char-whitespace? #\page)
    (not (char-whitespace? #\3))
    (char-whitespace? #\linefeed)
 )

(mat char-upcase
    (error? (char-upcase))
    (error? (char-upcase #\a #\b))
    (error? (char-upcase 'a))
    (eqv? (char-upcase #\a) #\A)
    (eqv? (char-upcase #\Z) #\Z)
 )

(mat char-titlecase
    (error? (char-titlecase))
    (error? (char-titlecase #\a #\b))
    (error? (char-titlecase 'a))
    (eqv? (char-titlecase #\a) #\A)
    (eqv? (char-titlecase #\Z) #\Z)
 )

(mat char-downcase
    (error? (char-downcase))
    (error? (char-downcase #\a #\b))
    (error? (char-downcase 'a))
    (eqv? (char-downcase #\a) #\a)
    (eqv? (char-downcase #\Z) #\z)
 )

(mat char-foldcase
    (error? (char-foldcase))
    (error? (char-foldcase #\a #\b))
    (error? (char-foldcase 'a))
    (eqv? (char-foldcase #\a) #\a)
    (eqv? (char-foldcase #\Z) #\z)
 )

(mat integer->char
    (error? (integer->char))
    (error? (integer->char 17 3))
    (error? (integer->char 'a))
    (error? (integer->char #f))
    (error? (integer->char #\a))
    (error? (integer->char -1))
    (error? (integer->char (+ (most-positive-fixnum) 1)))
    (error? (integer->char (- (most-negative-fixnum) 1)))
    (error? (integer->char #xD800))
    (error? (integer->char #xD900))
    (error? (integer->char #xDA00))
    (error? (integer->char #xDB00))
    (error? (integer->char #xDC00))
    (error? (integer->char #xDD00))
    (error? (integer->char #xDE00))
    (error? (integer->char #xDF00))
    (error? (integer->char #xDFFF))
    (error? (integer->char #x110000))
    (error? (integer->char #x120000))
    (error? (integer->char #x7fffffff))
    (eqv? (integer->char #x20) #\space)
    (eqv? (integer->char #x41) #\A)
    (eqv? (integer->char #x61) #\a)
    (eqv? (integer->char #x7f) #\rubout)
    (eqv? (integer->char #xD7FF) #\xD7FF)
    (eqv? (integer->char #xE000) #\xE000)
    (eqv? (integer->char #x10FFFF) #\x10FFFF)
 )

(mat char->integer
    (error? (char->integer))
    (error? (char->integer #\a #\b))
    (error? (char->integer 'a))
    (error? (char->integer #x20))
    (eqv? (char->integer #\1) #x31)
    (eqv? (char->integer #\z) #x7a)
    (eqv? (char->integer #\~) #x7e)
    (eqv? (char->integer #\nul) #x00)
    (eqv? (char->integer #\backspace) #x08)
    (eqv? (char->integer #\return) #x0d)
    (eqv? (char->integer #\page) #x0c)
    (eqv? (char->integer #\linefeed) #x0a)
    (eqv? (char->integer #\newline) #x0a)
    (eqv? (char->integer #\rubout) #x7f)
    (eqv? (char->integer #\space) #x20)
    (eqv? (char->integer #\tab) #x09)
    (begin
      (do ([i 0 (fx+ i 1)])
          ((fx>= i #xD800))
        (unless (eqv? (char->integer (integer->char i)) i)
          (errorf #f "failed for ~s" i)))
      (do ([i #xE000 (fx+ i 1)])
          ((fx>= i #x110000))
        (unless (eqv? (char->integer (integer->char i)) i)
          (errorf #f "failed for ~s" i)))
      #t)
 )

(mat string-for-each
  (error? ; invalid number of arguments
    (string-for-each))
  (error? ; invalid number of arguments
    (string-for-each '#()))
  (error? ; invalid number of arguments
    (string-for-each +))
  (error? ; non procedure ""
    (string-for-each "" ""))
  (error? ; non procedure ""
    (string-for-each "" "" ""))
  (error? ; non procedure ""
    (string-for-each "" "" "" '()))
  (error? ; non procedure ""
    (string-for-each "" "" "" "" ""))
  (error? ; non string 3
    (string-for-each + 3))
  (error? ; non string (3)
    (string-for-each + "" '(3)))
  (error? ; non string (3)
    (string-for-each + "" "" '(3)))
  (error? ; non string (3)
    (string-for-each + "" "" '(3) ""))
  (error? ; non string 7
    (string-for-each + 7 "" "" "" ""))
  (error? ; lengths differ
    (string-for-each + "" "x"))
  (error? ; lengths differ
    (string-for-each + "" "" "x"))
  (error? ; lengths differ
    (string-for-each + "" "" "x" ""))
  (error? ; lengths differ
    (string-for-each + "y" "" "x" ""))
  (error? ; lengths differ
    (string-for-each + "y" "" "" "" ""))
  (equal? (string-for-each + "") (void))
  (equal? (string-for-each + "" "") (void))
  (equal? (string-for-each + "" "" "") (void))
  (equal? (string-for-each + "" "" "" "" "") (void))
  (equal?
    (let ([ls '()])
      (string-for-each (lambda (x) (set! ls (cons x ls))) "abcdef")
      ls)
    '(#\f #\e #\d #\c #\b #\a))
  (equal?
    (let ([ls '()])
      (string-for-each
        (lambda (x y) (set! ls (cons (cons x y) ls)))
        "abcdef"
        "327654")
      ls)
    '((#\f . #\4) (#\e . #\5) (#\d . #\6) (#\c . #\7) (#\b . #\2) (#\a . #\3)))
  (equal?
    (let ([ls '()])
      (string-for-each
        (lambda r (set! ls (cons r ls)))
        "abcdef"
        "327654"
        "!@#$%^")
      ls)
    '((#\f #\4 #\^) (#\e #\5 #\%) (#\d #\6 #\$) (#\c #\7 #\#) (#\b #\2 #\@) (#\a #\3 #\!)))
  (equal?
    (let ([ls '()])
      (string-for-each
        (lambda r (set! ls (cons r ls)))
        "abcdef"
        "327654"
        "!@#$%^"
        "hello!"
        "hello?"
        "3.1415")
      (map list->string ls))
    '("f4^!?5" "e5%oo1" "d6$ll4" "c7#ll1" "b2@ee." "a3!hh3"))
  (begin
    (define ($string-for-each-f1 p x1 x2 x3 x4 x5)
      (begin
        (string-for-each p "")
        (string-for-each p "" x1)
        (string-for-each p "" x1 x2)
        (string-for-each p "" x1 x2 x3)
        (string-for-each p "" x1 x2 x3 x4)
        (string-for-each p "" x1 x2 x3 x4 x5)
        (string-for-each p x1 "")
        (string-for-each p x1 "" x2)
        (string-for-each p x1 "" x2 x3)
        (string-for-each p x1 "" x2 x3 x4)
        (string-for-each p x1 "" x2 x3 x4 x5)
        (string-for-each p x1 x2 "")
        (string-for-each p x1 x2 "" x3)
        (string-for-each p x1 x2 "" x3 x4)
        (string-for-each p x1 x2 "" x3 x4 x5)
        (string-for-each p x1 x2 x3 "")
        (string-for-each p x1 x2 x3 "" x4)
        (string-for-each p x1 x2 x3 "" x4 x5)
        (string-for-each p x1 x2 x3 x4 "")
        (string-for-each p x1 x2 x3 x4 "" x5)
        (string-for-each p x1 x2 x3 x4 x5 "")))
    (procedure? $string-for-each-f1))
  (equal?
    (let ([ls '()])
      (define q (lambda args (set! ls (cons (reverse args) ls))))
      ($string-for-each-f1 q "" "" "" "" "")
      (reverse ls))
    '())
  (begin
    (define ($string-for-each-f1 p x1 x2 x3 x4 x5)
      (begin
        (string-for-each p "a")
        (string-for-each p "a" x1)
        (string-for-each p "a" x1 x2)
        (string-for-each p "a" x1 x2 x3)
        (string-for-each p "a" x1 x2 x3 x4)
        (string-for-each p "a" x1 x2 x3 x4 x5)
        (string-for-each p x1 "a")
        (string-for-each p x1 "a" x2)
        (string-for-each p x1 "a" x2 x3)
        (string-for-each p x1 "a" x2 x3 x4)
        (string-for-each p x1 "a" x2 x3 x4 x5)
        (string-for-each p x1 x2 "a")
        (string-for-each p x1 x2 "a" x3)
        (string-for-each p x1 x2 "a" x3 x4)
        (string-for-each p x1 x2 "a" x3 x4 x5)
        (string-for-each p x1 x2 x3 "a")
        (string-for-each p x1 x2 x3 "a" x4)
        (string-for-each p x1 x2 x3 "a" x4 x5)
        (string-for-each p x1 x2 x3 x4 "a")
        (string-for-each p x1 x2 x3 x4 "a" x5)
        (string-for-each p x1 x2 x3 x4 x5 "a")))
    (procedure? $string-for-each-f1))
  (equal?
    (let ([ls '()])
      (define q (lambda args (set! ls (cons (reverse args) ls))))
      ($string-for-each-f1 q "1" "f" "k" "p" "u")
      (map list->string (reverse ls)))
    '("a" "1a" "f1a" "kf1a" "pkf1a" "upkf1a" "a1" "fa1"
      "kfa1" "pkfa1" "upkfa1" "af1" "kaf1" "pkaf1" "upkaf1"
      "akf1" "pakf1" "upakf1" "apkf1" "uapkf1" "aupkf1"))
  (begin
    (define ($string-for-each-f1 p x1 x2 x3 x4 x5)
      (begin
        (string-for-each p "ab")
        (string-for-each p "ab" x1)
        (string-for-each p "ab" x1 x2)
        (string-for-each p "ab" x1 x2 x3)
        (string-for-each p "ab" x1 x2 x3 x4)
        (string-for-each p "ab" x1 x2 x3 x4 x5)
        (string-for-each p x1 "ab")
        (string-for-each p x1 "ab" x2)
        (string-for-each p x1 "ab" x2 x3)
        (string-for-each p x1 "ab" x2 x3 x4)
        (string-for-each p x1 "ab" x2 x3 x4 x5)
        (string-for-each p x1 x2 "ab")
        (string-for-each p x1 x2 "ab" x3)
        (string-for-each p x1 x2 "ab" x3 x4)
        (string-for-each p x1 x2 "ab" x3 x4 x5)
        (string-for-each p x1 x2 x3 "ab")
        (string-for-each p x1 x2 x3 "ab" x4)
        (string-for-each p x1 x2 x3 "ab" x4 x5)
        (string-for-each p x1 x2 x3 x4 "ab")
        (string-for-each p x1 x2 x3 x4 "ab" x5)
        (string-for-each p x1 x2 x3 x4 x5 "ab")))
    (procedure? $string-for-each-f1))
  (equal?
    (let ([ls '()])
      (define q (lambda args (set! ls (cons (reverse args) ls))))
      ($string-for-each-f1 q "12" "fg" "kl" "pq" "uv")
      (map list->string (reverse ls)))
    '("a" "b" "1a" "2b" "f1a" "g2b" "kf1a" "lg2b" "pkf1a"
      "qlg2b" "upkf1a" "vqlg2b" "a1" "b2" "fa1" "gb2" "kfa1"
      "lgb2" "pkfa1" "qlgb2" "upkfa1" "vqlgb2" "af1" "bg2"
      "kaf1" "lbg2" "pkaf1" "qlbg2" "upkaf1" "vqlbg2" "akf1"
      "blg2" "pakf1" "qblg2" "upakf1" "vqblg2" "apkf1"
      "bqlg2" "uapkf1" "vbqlg2" "aupkf1" "bvqlg2"))
  (begin
    (define ($string-for-each-f1 p x1 x2 x3 x4 x5)
      (begin
        (string-for-each p "abc")
        (string-for-each p "abc" x1)
        (string-for-each p "abc" x1 x2)
        (string-for-each p "abc" x1 x2 x3)
        (string-for-each p "abc" x1 x2 x3 x4)
        (string-for-each p "abc" x1 x2 x3 x4 x5)
        (string-for-each p x1 "abc")
        (string-for-each p x1 "abc" x2)
        (string-for-each p x1 "abc" x2 x3)
        (string-for-each p x1 "abc" x2 x3 x4)
        (string-for-each p x1 "abc" x2 x3 x4 x5)
        (string-for-each p x1 x2 "abc")
        (string-for-each p x1 x2 "abc" x3)
        (string-for-each p x1 x2 "abc" x3 x4)
        (string-for-each p x1 x2 "abc" x3 x4 x5)
        (string-for-each p x1 x2 x3 "abc")
        (string-for-each p x1 x2 x3 "abc" x4)
        (string-for-each p x1 x2 x3 "abc" x4 x5)
        (string-for-each p x1 x2 x3 x4 "abc")
        (string-for-each p x1 x2 x3 x4 "abc" x5)
        (string-for-each p x1 x2 x3 x4 x5 "abc")))
    (procedure? $string-for-each-f1))
  (equal?
    (let ([ls '()])
      (define q (lambda args (set! ls (cons (reverse args) ls))))
      ($string-for-each-f1 q "123" "fgh" "klm" "pqr" "uvw")
      (map list->string (reverse ls)))
    '("a" "b" "c" "1a" "2b" "3c" "f1a" "g2b" "h3c" "kf1a"
      "lg2b" "mh3c" "pkf1a" "qlg2b" "rmh3c" "upkf1a" "vqlg2b"
      "wrmh3c" "a1" "b2" "c3" "fa1" "gb2" "hc3" "kfa1" "lgb2"
      "mhc3" "pkfa1" "qlgb2" "rmhc3" "upkfa1" "vqlgb2"
      "wrmhc3" "af1" "bg2" "ch3" "kaf1" "lbg2" "mch3" "pkaf1"
      "qlbg2" "rmch3" "upkaf1" "vqlbg2" "wrmch3" "akf1"
      "blg2" "cmh3" "pakf1" "qblg2" "rcmh3" "upakf1" "vqblg2"
      "wrcmh3" "apkf1" "bqlg2" "crmh3" "uapkf1" "vbqlg2"
      "wcrmh3" "aupkf1" "bvqlg2" "cwrmh3"))
  (begin
    (define ($string-for-each-f1 p x1 x2 x3 x4 x5)
      (begin
        (string-for-each p "abcd")
        (string-for-each p "abcd" x1)
        (string-for-each p "abcd" x1 x2)
        (string-for-each p "abcd" x1 x2 x3)
        (string-for-each p "abcd" x1 x2 x3 x4)
        (string-for-each p "abcd" x1 x2 x3 x4 x5)
        (string-for-each p x1 "abcd")
        (string-for-each p x1 "abcd" x2)
        (string-for-each p x1 "abcd" x2 x3)
        (string-for-each p x1 "abcd" x2 x3 x4)
        (string-for-each p x1 "abcd" x2 x3 x4 x5)
        (string-for-each p x1 x2 "abcd")
        (string-for-each p x1 x2 "abcd" x3)
        (string-for-each p x1 x2 "abcd" x3 x4)
        (string-for-each p x1 x2 "abcd" x3 x4 x5)
        (string-for-each p x1 x2 x3 "abcd")
        (string-for-each p x1 x2 x3 "abcd" x4)
        (string-for-each p x1 x2 x3 "abcd" x4 x5)
        (string-for-each p x1 x2 x3 x4 "abcd")
        (string-for-each p x1 x2 x3 x4 "abcd" x5)
        (string-for-each p x1 x2 x3 x4 x5 "abcd")))
    (procedure? $string-for-each-f1))
  (equal?
    (let ([ls '()])
      (define q (lambda args (set! ls (cons (reverse args) ls))))
      ($string-for-each-f1 q "1234" "fghi" "klmn" "pqrs" "uvwx")
      (map list->string (reverse ls)))
    '("a" "b" "c" "d" "1a" "2b" "3c" "4d" "f1a" "g2b" "h3c"
      "i4d" "kf1a" "lg2b" "mh3c" "ni4d" "pkf1a" "qlg2b"
      "rmh3c" "sni4d" "upkf1a" "vqlg2b" "wrmh3c" "xsni4d"
      "a1" "b2" "c3" "d4" "fa1" "gb2" "hc3" "id4" "kfa1"
      "lgb2" "mhc3" "nid4" "pkfa1" "qlgb2" "rmhc3" "snid4"
      "upkfa1" "vqlgb2" "wrmhc3" "xsnid4" "af1" "bg2" "ch3"
      "di4" "kaf1" "lbg2" "mch3" "ndi4" "pkaf1" "qlbg2"
      "rmch3" "sndi4" "upkaf1" "vqlbg2" "wrmch3" "xsndi4"
      "akf1" "blg2" "cmh3" "dni4" "pakf1" "qblg2" "rcmh3"
      "sdni4" "upakf1" "vqblg2" "wrcmh3" "xsdni4" "apkf1"
      "bqlg2" "crmh3" "dsni4" "uapkf1" "vbqlg2" "wcrmh3"
      "xdsni4" "aupkf1" "bvqlg2" "cwrmh3" "dxsni4"))
  (begin
    (define ($string-for-each-f1 p x1 x2 x3 x4 x5)
      (begin
        (string-for-each p "abcde")
        (string-for-each p "abcde" x1)
        (string-for-each p "abcde" x1 x2)
        (string-for-each p "abcde" x1 x2 x3)
        (string-for-each p "abcde" x1 x2 x3 x4)
        (string-for-each p "abcde" x1 x2 x3 x4 x5)
        (string-for-each p x1 "abcde")
        (string-for-each p x1 "abcde" x2)
        (string-for-each p x1 "abcde" x2 x3)
        (string-for-each p x1 "abcde" x2 x3 x4)
        (string-for-each p x1 "abcde" x2 x3 x4 x5)
        (string-for-each p x1 x2 "abcde")
        (string-for-each p x1 x2 "abcde" x3)
        (string-for-each p x1 x2 "abcde" x3 x4)
        (string-for-each p x1 x2 "abcde" x3 x4 x5)
        (string-for-each p x1 x2 x3 "abcde")
        (string-for-each p x1 x2 x3 "abcde" x4)
        (string-for-each p x1 x2 x3 "abcde" x4 x5)
        (string-for-each p x1 x2 x3 x4 "abcde")
        (string-for-each p x1 x2 x3 x4 "abcde" x5)
        (string-for-each p x1 x2 x3 x4 x5 "abcde")))
    (procedure? $string-for-each-f1))
  (equal?
    (let ([ls '()])
      (define q (lambda args (set! ls (cons (reverse args) ls))))
      ($string-for-each-f1 q "12345" "fghij" "klmno" "pqrst" "uvwxy")
      (map list->string (reverse ls)))
    '("a" "b" "c" "d" "e" "1a" "2b" "3c" "4d" "5e" "f1a"
      "g2b" "h3c" "i4d" "j5e" "kf1a" "lg2b" "mh3c" "ni4d"
      "oj5e" "pkf1a" "qlg2b" "rmh3c" "sni4d" "toj5e" "upkf1a"
      "vqlg2b" "wrmh3c" "xsni4d" "ytoj5e" "a1" "b2" "c3" "d4"
      "e5" "fa1" "gb2" "hc3" "id4" "je5" "kfa1" "lgb2" "mhc3"
      "nid4" "oje5" "pkfa1" "qlgb2" "rmhc3" "snid4" "toje5"
      "upkfa1" "vqlgb2" "wrmhc3" "xsnid4" "ytoje5" "af1"
      "bg2" "ch3" "di4" "ej5" "kaf1" "lbg2" "mch3" "ndi4"
      "oej5" "pkaf1" "qlbg2" "rmch3" "sndi4" "toej5" "upkaf1"
      "vqlbg2" "wrmch3" "xsndi4" "ytoej5" "akf1" "blg2"
      "cmh3" "dni4" "eoj5" "pakf1" "qblg2" "rcmh3" "sdni4"
      "teoj5" "upakf1" "vqblg2" "wrcmh3" "xsdni4" "yteoj5"
      "apkf1" "bqlg2" "crmh3" "dsni4" "etoj5" "uapkf1"
      "vbqlg2" "wcrmh3" "xdsni4" "yetoj5" "aupkf1" "bvqlg2"
      "cwrmh3" "dxsni4" "eytoj5"))
 ; check for proper tail recursion
   (equal?
     (list
       (let ([s (statistics)])
         (let ([k 100000] [str "abc"])
           (let ([n k] [m 0])
             (define (f) (unless (fx= n 0) (string-for-each foo str)))
             (define (foo x)
               (set! m (+ m 1))
               (when (char=? x (string-ref str (fx- (string-length str) 1)))
                 (set! n (- n 1))
                 (f)
                 17)) ; blow tail recursion here
             (f)
             (list (> (sstats-bytes (sstats-difference (statistics) s))
                      10000)
                   (eqv? n 0)
                   (eqv? m (* k (string-length str)))))))
       (let ([s (statistics)])
         (let ([k 100000] [str "abc"])
           (let ([n k] [m 0])
             (define (f) (unless (fx= n 0) (string-for-each foo str)))
             (define (foo x)
               (set! m (+ m 1))
               (when (char=? x (string-ref str (fx- (string-length str) 1)))
                 (set! n (- n 1))
                 (f)))
             (f)
             (list (<= 0
                      (sstats-bytes (sstats-difference (statistics) s))
                      1000)
                   (eqv? n 0)
                   (eqv? m (* k (string-length str))))))))
     '((#t #t #t) (#t #t #t)))
 )

(mat string-xcase-errors
  (error? (string-upcase))
  (error? (string-upcase "hello" "goodbye"))
  (error? (string-upcase 'ouch))
  (error? (string-downcase))
  (error? (string-downcase "hello" "goodbye"))
  (error? (string-downcase 'ouch))
  (error? (string-titlecase))
  (error? (string-titlecase "hello" "goodbye"))
  (error? (string-titlecase 'ouch))
  (error? (string-foldcase))
  (error? (string-foldcase "hello" "goodbye"))
  (error? (string-foldcase 'ouch))
)

(mat normalization-tests
  (error? (string-normalize-nfd))
  (error? (string-normalize-nfd "hello" "goodbye"))
  (error? (string-normalize-nfd 'ouch))
  (error? (string-normalize-nfkd))
  (error? (string-normalize-nfkd "hello" "goodbye"))
  (error? (string-normalize-nfkd 'ouch))
  (error? (string-normalize-nfc))
  (error? (string-normalize-nfc "hello" "goodbye"))
  (error? (string-normalize-nfc 'ouch))
  (error? (string-normalize-nfkc))
  (error? (string-normalize-nfkc "hello" "goodbye"))
  (error? (string-normalize-nfkc 'ouch))
  (begin
    (load (format "~a/../unicode/unicode-data.ss" *mats-dir*))
    #t)
  (let ()
    (import (unicode-data))
    (define (split str)
      (remove ""
        (let f ([i 0] [n (string-length str)])
          (cond
            [(= i n) (list (substring str 0 n))]
            [(char=? (string-ref str i) #\space)
             (cons (substring str 0 i) 
                   (split (substring str (+ i 1) n)))]
            [else (f (add1 i) n)]))))

    (define (conv x)
      (list->string
        (map (lambda (x) (integer->char (string->number x 16)))
             (split x))))

    (let ([data (map (lambda (x) (map conv (list-head x 5)))
                     (filter (lambda (x) (>= (length x) 5))
                       (get-unicode-data
                         (format "~a/../unicode/UNIDATA/NormalizationTest.txt" *mats-dir*))))])
      (define NFD string-normalize-nfd)
      (define NFKD string-normalize-nfkd)
      (define NFC string-normalize-nfc)
      (define NFKC string-normalize-nfkc)

      (printf "found ~s tests\n" (length data))

     ; test 1
      (for-each
        (lambda (x testno)
          (apply
            (lambda (c1 c2 c3 c4 c5)
              (unless (and (string=? c2 (NFC c1) (NFC c2) (NFC c3))
                           (string=? c4 (NFC c4) (NFC c5)))
                (parameterize ([print-unicode #f])
                  (printf "test 1[~s] failed for ~s\n" testno x)
                  (printf "       c2 = ~s\n" c2)
                  (printf "  NFC(c1) = ~s\n" (NFC c1))
                  (printf "  NFC(c2) = ~s\n" (NFC c2))
                  (printf "  NFC(c3) = ~s\n" (NFC c3))
                  (printf "       c4 = ~s\n" c4)
                  (printf "  NFC(c4) = ~s\n" (NFC c4))
                  (printf "  NFC(c5) = ~s\n" (NFC c5))
                  (errorf #f "test 1 failed: see make output"))))
            x))
        data (enumerate data))

     ; test 2
      (for-each
        (lambda (x testno)
          (apply
            (lambda (c1 c2 c3 c4 c5)
              (unless (and (string=? c3 (NFD c1) (NFD c2) (NFD c3))
                           (string=? c5 (NFD c4) (NFD c5)))
                (parameterize ([print-unicode #f])
                  (printf "test 2[~s] failed for ~s\n" testno x)
                  (printf "       c3 = ~s\n" c3)
                  (printf "  NFD(c1) = ~s\n" (NFD c1))
                  (printf "  NFD(c2) = ~s\n" (NFD c2))
                  (printf "  NFD(c3) = ~s\n" (NFD c3))
                  (printf "       c5 = ~s\n" c5)
                  (printf "  NFD(c4) = ~s\n" (NFD c4))
                  (printf "  NFD(c5) = ~s\n" (NFD c5))
                  (errorf #f "test 2 failed: see make output"))))
            x))
        data (enumerate data))

     ; test 3
      (for-each
        (lambda (x testno)
          (apply
            (lambda (c1 c2 c3 c4 c5)
              (unless (string=? c4 (NFKC c1) (NFKC c2) (NFKC c3) (NFKC c4) (NFKC c5))
                (parameterize ([print-unicode #f])
                  (printf "test 3[~s] failed for ~s\n" testno x)
                  (printf "       c4 = ~s\n" c4)
                  (printf "  NFKC(c1) = ~s\n" (NFKC c1))
                  (printf "  NFKC(c2) = ~s\n" (NFKC c2))
                  (printf "  NFKC(c3) = ~s\n" (NFKC c3))
                  (printf "  NFKC(c4) = ~s\n" (NFKC c4))
                  (printf "  NFKC(c5) = ~s\n" (NFKC c5))
                  (errorf #f "test 3 failed: see make output"))))
            x))
        data (enumerate data))

     ; test 4
      (for-each
        (lambda (x testno)
          (apply
            (lambda (c1 c2 c3 c4 c5)
              (unless (string=? c5 (NFKD c1) (NFKD c2) (NFKD c3) (NFKD c4) (NFKD c5))
                (parameterize ([print-unicode #f])
                  (printf "test 4[~s] failed for ~s\n" testno x)
                  (printf "       c5 = ~s\n" c5)
                  (printf "  NFKD(c1) = ~s\n" (NFKD c1))
                  (printf "  NFKD(c2) = ~s\n" (NFKD c2))
                  (printf "  NFKD(c3) = ~s\n" (NFKD c3))
                  (printf "  NFKD(c4) = ~s\n" (NFKD c4))
                  (printf "  NFKD(c5) = ~s\n" (NFKD c5))
                  (errorf #f "test 4 failed: see make output"))))
            x))
        data (enumerate data)))
    #t)
 )

(mat r6rs-unicode-tests ; from Flatt's R6RS test suite
  (begin
    (define test equal?)
    (test test equal?))

  (test (char-upcase #\i) #\I)
  (test (char-downcase #\i) #\i)
  (test (char-titlecase #\i) #\I)
  (test (char-foldcase #\i) #\i)
  
  (test (char-upcase #\xDF) #\xDF)
  (test (char-downcase #\xDF) #\xDF)
  (test (char-titlecase #\xDF) #\xDF)
  (test (char-foldcase #\xDF) #\xDF)
  
  (test (char-upcase #\x3A3) #\x3A3)
  (test (char-downcase #\x3A3) #\x3C3)
  (test (char-titlecase #\x3A3) #\x3A3)
  (test (char-foldcase #\x3A3) #\x3C3)

  (test (char-upcase #\x3C2) #\x3A3)
  (test (char-downcase #\x3C2) #\x3C2)
  (test (char-titlecase #\x3C2) #\x3A3)
  (test (char-foldcase #\x3C2) #\x3C3)

  (test (char-ci<? #\z #\Z) #f)
  (test (char-ci<? #\Z #\z) #f)
  (test (char-ci<? #\a #\Z) #t)
  (test (char-ci<? #\Z #\a) #f)
  (test (char-ci<=? #\z #\Z) #t)
  (test (char-ci<=? #\Z #\z) #t)
  (test (char-ci<=? #\a #\Z) #t)
  (test (char-ci<=? #\Z #\a) #f)
  (test (char-ci=? #\z #\a) #f)
  (test (char-ci=? #\z #\Z) #t)
  (test (char-ci=? #\x3C2 #\x3C3) #t)
  (test (char-ci>? #\z #\Z) #f)
  (test (char-ci>? #\Z #\z) #f)
  (test (char-ci>? #\a #\Z) #f)
  (test (char-ci>? #\Z #\a) #t)
  (test (char-ci>=? #\Z #\z) #t)
  (test (char-ci>=? #\z #\Z) #t)
  (test (char-ci>=? #\z #\Z) #t)
  (test (char-ci>=? #\a #\z) #f)

  (test (char-alphabetic? #\a) #t)
  (test (char-alphabetic? #\1) #f)
  (test (char-numeric? #\1) #t)
  (test (char-numeric? #\a) #f)
  (test (char-whitespace? #\space) #t)
  (test (char-whitespace? #\x00A0) #t)
  (test (char-whitespace? #\a) #f)
  (test (char-upper-case? #\a) #f)
  (test (char-upper-case? #\A) #t)
  (test (char-upper-case? #\x3A3) #t)
  (test (char-lower-case? #\a) #t)
  (test (char-lower-case? #\A) #f)
  (test (char-lower-case? #\x3C3) #t)
  (test (char-lower-case? #\x00AA) #t)
  (test (char-title-case? #\a) #f)
  (test (char-title-case? #\A) #f)
  (test (char-title-case? #\I) #f)
  (test (char-title-case? #\x01C5) #t)

  (test (char-general-category #\a) 'Ll)
  (test (char-general-category #\space) 'Zs)
  (test (char-general-category #\x10FFFF) 'Cn)

  (test (string-upcase "Hi") "HI")
  (test (string-upcase "HI") "HI")
  (test (string-downcase "Hi") "hi")
  (test (string-downcase "hi") "hi")
  (test (string-foldcase "Hi") "hi")
  (test (string-foldcase "HI") "hi")
  (test (string-foldcase "hi") "hi")

  (test (string-upcase "Stra\xDF;e") "STRASSE")
  (test (string-downcase "Stra\xDF;e") "stra\xDF;e")
  (test (string-foldcase "Stra\xDF;e") "strasse")
  (test (string-downcase "STRASSE")  "strasse")
  
  (test (string-downcase "\x3A3;") "\x3C3;")

  (test (string-upcase "\x39E;\x391;\x39F;\x3A3;") "\x39E;\x391;\x39F;\x3A3;")
  (test (string-downcase "\x39E;\x391;\x39F;\x3A3;") "\x3BE;\x3B1;\x3BF;\x3C2;")
  (test (string-downcase "\x39E;\x391;\x39F;\x3A3;\x3A3;") "\x3BE;\x3B1;\x3BF;\x3C3;\x3C2;")
  (test (string-downcase "\x39E;\x391;\x39F;\x3A3; \x3A3;") "\x3BE;\x3B1;\x3BF;\x3C2; \x3C3;")
  (test (string-foldcase "\x39E;\x391;\x39F;\x3A3;") "\x3BE;\x3B1;\x3BF;\x3C3;")
  (test (string-upcase "\x3BE;\x3B1;\x3BF;\x3C3;") "\x39E;\x391;\x39F;\x3A3;") 
  (test (string-upcase "\x3BE;\x3B1;\x3BF;\x3C2;") "\x39E;\x391;\x39F;\x3A3;") 

  (test (string-titlecase "kNock KNoCK") "Knock Knock")
  (test (string-titlecase "who's there?") "Who's There?")
  (test (string-titlecase "r6rs") "R6rs") ; this example appears to be wrong in R6RS (Sept 2007 version)
  (test (string-titlecase "R6RS") "R6rs") ; this one, too

  (test (string-downcase "A\x3A3;'x") "a\x3C3;'x") ; ' is a MidLetter

  (test (string-ci<? "a" "Z") #t)
  (test (string-ci<? "A" "z") #t)
  (test (string-ci<? "Z" "a") #f)
  (test (string-ci<? "z" "A") #f)
  (test (string-ci<? "z" "Z") #f)
  (test (string-ci<? "Z" "z") #f)
  (test (string-ci>? "a" "Z") #f)
  (test (string-ci>? "A" "z") #f)
  (test (string-ci>? "Z" "a") #t)
  (test (string-ci>? "z" "A") #t)
  (test (string-ci>? "z" "Z") #f)
  (test (string-ci>? "Z" "z") #f)
  (test (string-ci=? "z" "Z") #t)
  (test (string-ci=? "z" "a") #f)
  (test (string-ci=? "Stra\xDF;e" "Strasse") #t)
  (test (string-ci=? "Stra\xDF;e" "STRASSE") #t)
  (test (string-ci=? "\x39E;\x391;\x39F;\x3A3;" "\x3BE;\x3B1;\x3BF;\x3C2;") #t)
  (test (string-ci=? "\x39E;\x391;\x39F;\x3A3;" "\x3BE;\x3B1;\x3BF;\x3C3;") #t)
  (test (string-ci<=? "a" "Z") #t)
  (test (string-ci<=? "A" "z") #t)
  (test (string-ci<=? "Z" "a") #f)
  (test (string-ci<=? "z" "A") #f)
  (test (string-ci<=? "z" "Z") #t)
  (test (string-ci<=? "Z" "z") #t)
  (test (string-ci>=? "a" "Z") #f)
  (test (string-ci>=? "A" "z") #f)
  (test (string-ci>=? "Z" "a") #t)
  (test (string-ci>=? "z" "A") #t)
  (test (string-ci>=? "z" "Z") #t)
  (test (string-ci>=? "Z" "z") #t)

  (test (string-normalize-nfd "\xE9;") "\x65;\x301;")
  (test (string-normalize-nfc "\xE9;") "\xE9;")
  (test (string-normalize-nfd "\x65;\x301;") "\x65;\x301;")
  (test (string-normalize-nfc "\x65;\x301;") "\xE9;")

  (test (string-normalize-nfkd "\xE9;") "\x65;\x301;")
  (test (string-normalize-nfkc "\xE9;") "\xE9;")
  (test (string-normalize-nfkd "\x65;\x301;") "\x65;\x301;")
  (test (string-normalize-nfkc "\x65;\x301;") "\xE9;")
 )

(mat unicode-tests.sch ; adapted from Clinger's unicode-tests.sch
  ; Copyright 2006 William D Clinger.
  ;
  ; Permission to copy this software, in whole or in part, to use this
  ; software for any lawful purpose, and to redistribute this software
  ; is granted subject to the restriction that all copies made of this
  ; software must include this copyright and permission notice in full.
  ;
  ; I also request that you send me a copy of any improvements that you
  ; make to this software so that they may be incorporated within it to
  ; the benefit of the Scheme community.

  (begin
    (define es-zed (integer->char #x00df))
    (define final-sigma (integer->char #x03c2))
    (define lower-sigma (integer->char #x03c3))
    (define upper-sigma (integer->char #x03a3))
    (define upper-chi (integer->char #x03a7))
    (define upper-alpha (integer->char #x0391))
    (define upper-omicron (integer->char #x039f))
    (define lower-chi (integer->char #x03c7))
    (define lower-alpha (integer->char #x03b1))
    (define lower-omicron (integer->char #x03bf))
    (define strasse (string #\S #\t #\r #\a es-zed #\e))
    (define upper-chaos (string upper-chi upper-alpha upper-omicron upper-sigma))
    (define final-chaos (string lower-chi lower-alpha lower-omicron final-sigma))
    (define lower-chaos (string lower-chi lower-alpha lower-omicron lower-sigma))
   ; Given a unary predicate on characters, returns a sorted
   ; list of all characters that satisfy the predicate.
    (define (filter-all-chars p?)
      (do ((i 0 (+ i 1))
           (chars '()
                  (if (and (not (<= #xd800 i #xdfff))
                           (p? (integer->char i)))
                      (cons (integer->char i) chars)
                      chars)))
          ((= i #x110000)
           (reverse chars))))
   ; Given a list of characters, prints its length and returns 0.
    (define (report chars n)
      (display "  ")
      (display (length chars))
      (display " characters")
      (if (not (= n (length chars)))
          (begin (display " but expected ")
                 (write n)
                 (display " in Unicode 5.0.0")))
      (newline)
      0)
    (define-syntax test
      (syntax-rules (=> error)
        [(test name exp => result)
         (equal? exp result)]))
   ; According to SRFI 77, this is a complete list of all code points
   ; above 127 in Unicode 4.1 whose Unicode general category is
   ; Ps, Pe, Pi, Pf, Zs, Zp, Zl, Cc, or Cf.
   ;
   ; In Unicode 5.0, the general category of
   ; #\x23B4 (TOP SQUARE BRACKET)
   ; and
   ; #\x23B5 (BOTTOM SQUARE BRACKET)
   ; was changed from Ps and Pe to So.
   ; rkd: Unicode 5.1 adds
   ; #x2064 #x27EC #x27ED #x27EE #x27EF #x2E20 #x2E21 #x2E22
   ; #x2E23 #x2E24 #x2E25 #x2E26 #x2E27 #x2E28 #x2E29
   ; rkd: Unicode 7.0 adds:
   ; #x604 #x605 #x61C #x2066 #x2067 #x2068 #x2069 #x2308 #x2309
   ; #x230A #x230B #x2E42 #x110BD #x1BCA0 #x1BCA1 #x1BCA2 #x1BCA3
   ; rkd: Unicode 7.0 removes:
   ; #x17B4 #x17B5
    (define excluded-code-points-above-127
      '(
    
     #x80 #x81 #x82 #x83 #x84 #x85 #x86 #x87 #x88 #x89
     #x8A #x8B #x8C #x8D #x8E #x8F #x90 #x91 #x92 #x93
     #x94 #x95 #x96 #x97 #x98 #x99 #x9A #x9B #x9C #x9D
     #x9E #x9F #xA0 #xAB #xAD #xBB #x600 #x601 #x602 #x603
     #x604 #x605 #x61C                                       ; Unicode 7.0
     #x6DD #x70F #xF3A #xF3B #xF3C #xF3D #x1680 #x169B
     #x169C
     #;#x17B4 #;#x17B5                                       ; Unicode 7.0
     #x180E #x2000 #x2001 #x2002 #x2003
     #x2004 #x2005 #x2006 #x2007 #x2008 #x2009 #x200A #x200B
     #x200C #x200D #x200E #x200F #x2018 #x2019 #x201A #x201B
     #x201C #x201D #x201E #x201F #x2028 #x2029 #x202A #x202B
     #x202C #x202D #x202E #x202F #x2039 #x203A #x2045 #x2046
     #x205F #x2060 #x2061 #x2062 #x2063
     #x2064                                                  ; Unicode 5.1
     #x2066 #x2067 #x2068 #x2069                             ; Unicode 7.0
     #x206A #x206B #x206C
     #x206D #x206E #x206F #x207D #x207E #x208D #x208E
     #x2308 #x2309 #x230A #x230B                             ; Unicode 7.0
     #x2329 #x232A 
    ;       #x23B4 #x23B5 ; see note above for Unicode 5.0
                          #x2768 #x2769 #x276A #x276B #x276C
     #x276D #x276E #x276F #x2770 #x2771 #x2772 #x2773 #x2774
     #x2775 #x27C5 #x27C6 #x27E6 #x27E7 #x27E8 #x27E9 #x27EA
     #x27EB 
     #x27EC #x27ED #x27EE #x27EF                             ; Unicode 5.1
     #x2983 #x2984 #x2985 #x2986 #x2987 #x2988 #x2989
     #x298A #x298B #x298C #x298D #x298E #x298F #x2990 #x2991
     #x2992 #x2993 #x2994 #x2995 #x2996 #x2997 #x2998 #x29D8
     #x29D9 #x29DA #x29DB #x29FC #x29FD #x2E02 #x2E03 #x2E04
     #x2E05 #x2E09 #x2E0A #x2E0C #x2E0D #x2E1C #x2E1D
     #x2E20 #x2E21 #x2E22 #x2E23 #x2E24 #x2E25 #x2E26 #x2E27 ; Unicode 5.1
     #x2E28 #x2E29                                           ; Unicode 5.1
     #x2E42                                                  ; Unicode 7.0
     #x3000
     #x3008 #x3009 #x300A #x300B #x300C #x300D #x300E #x300F
     #x3010 #x3011 #x3014 #x3015 #x3016 #x3017 #x3018 #x3019
     #x301A #x301B #x301D #x301E #x301F #xFD3E #xFD3F #xFE17
     #xFE18 #xFE35 #xFE36 #xFE37 #xFE38 #xFE39 #xFE3A #xFE3B
     #xFE3C #xFE3D #xFE3E #xFE3F #xFE40 #xFE41 #xFE42 #xFE43
     #xFE44 #xFE47 #xFE48 #xFE59 #xFE5A #xFE5B #xFE5C #xFE5D
     #xFE5E #xFEFF #xFF08 #xFF09 #xFF3B #xFF3D #xFF5B #xFF5D
     #xFF5F #xFF60 #xFF62 #xFF63 #xFFF9 #xFFFA #xFFFB
     #x110BD #x1BCA0 #x1BCA1 #x1BCA2 #x1BCA3                 ; Unicode 7.0
     #x1D173
     #x1D174 #x1D175 #x1D176 #x1D177 #x1D178 #x1D179 #x1D17A
     #xE0001 #xE0020 #xE0021 #xE0022 #xE0023 #xE0024 #xE0025
     #xE0026 #xE0027 #xE0028 #xE0029 #xE002A #xE002B #xE002C
     #xE002D #xE002E #xE002F #xE0030 #xE0031 #xE0032 #xE0033
     #xE0034 #xE0035 #xE0036 #xE0037 #xE0038 #xE0039 #xE003A
     #xE003B #xE003C #xE003D #xE003E #xE003F #xE0040 #xE0041
     #xE0042 #xE0043 #xE0044 #xE0045 #xE0046 #xE0047 #xE0048
     #xE0049 #xE004A #xE004B #xE004C #xE004D #xE004E #xE004F
     #xE0050 #xE0051 #xE0052 #xE0053 #xE0054 #xE0055 #xE0056
     #xE0057 #xE0058 #xE0059 #xE005A #xE005B #xE005C #xE005D
     #xE005E #xE005F #xE0060 #xE0061 #xE0062 #xE0063 #xE0064
     #xE0065 #xE0066 #xE0067 #xE0068 #xE0069 #xE006A #xE006B
     #xE006C #xE006D #xE006E #xE006F #xE0070 #xE0071 #xE0072
     #xE0073 #xE0074 #xE0075 #xE0076 #xE0077 #xE0078 #xE0079
     #xE007A #xE007B #xE007C #xE007D #xE007E #xE007F))
   #t)

  (test type1 (integer->char 32) => #\space)
  (test type2 (char->integer (integer->char 5000)) => 5000)
  ;(test type3 (integer->char #xd800) => error)

  (test comp1 (char<? #\z es-zed) => #t)
  (test comp2 (char<? #\z #\Z) => #f)
  (test comp3 (char-ci<? #\z #\Z) => #f)
  (test comp4 (char-ci=? #\z #\Z) => #t)
  (test comp5 (char-ci=? final-sigma lower-sigma) => #t)

  (test case1 (char-upcase #\i) => #\I)
  (test case2 (char-downcase #\i) => #\i)
  (test case3 (char-titlecase #\i) => #\I)
  (test case4 (char-foldcase #\i) => #\i)

  (test case5 (char-upcase es-zed) => es-zed)
  (test case6 (char-downcase es-zed) => es-zed)
  (test case7 (char-titlecase es-zed) => es-zed)
  (test case8 (char-foldcase es-zed) => es-zed)

  (test case9 (char-upcase upper-sigma) => upper-sigma)
  (test case10 (char-downcase upper-sigma) => lower-sigma)
  (test case11 (char-titlecase upper-sigma) => upper-sigma)
  (test case12 (char-foldcase upper-sigma) => lower-sigma)

  (test case13 (char-upcase final-sigma) => upper-sigma)
  (test case14 (char-downcase final-sigma) => final-sigma)
  (test case15 (char-titlecase final-sigma) => upper-sigma)
  (test case16 (char-foldcase final-sigma) => lower-sigma)

  (test cat1 (char-general-category #\a) => 'Ll)
  (test cat2 (char-general-category #\space) => 'Zs)
  (test cat3 (char-general-category (integer->char #x10FFFF)) => 'Cn)

  (test alpha1 (char-alphabetic? #\a) => #t)
  (test numer1 (char-numeric? #\1) => #t)
  (test white1 (char-whitespace? #\space) => #t)
  (test white2 (char-whitespace? (integer->char #x00A0)) => #t)
  (test upper1 (char-upper-case? upper-sigma) => #t)
  (test lower1 (char-lower-case? lower-sigma) => #t)
  (test lower2 (char-lower-case? (integer->char #x00AA)) => #t)
  (test title1 (char-title-case? #\I) => #f)
  (test title2 (char-title-case? (integer->char #x01C5)) => #t)

  ; 01/30/15 rkd: modified to print the exceptions
  (test excluded
        (let f ((i 128) (excluded excluded-code-points-above-127) (okay? #t))
          (if (= i #x110000)
              okay?
              (if (and (not (null? excluded)) (> i (car excluded)))
                  (begin
                    (printf "missed excluded char \\x~x\n" (car excluded))
                    (f i (cdr excluded) #f))
                  (let ([excluded? (and (not (<= #xd800 i #xdfff))
                                        (memq (char-general-category (integer->char i))
                                          '(Ps Pe Pi Pf Zs Zp Zl Cc Cf)))])
                    (if excluded?
                        (if (and (not (null? excluded)) (eqv? i (car excluded)))
                            (f (+ i 1) (cdr excluded) okay?)
                            (begin
                              (printf "excluding non-excluded char \\x~x\n" i)
                              (f (+ i 1) excluded #f)))
                        (f (+ i 1) excluded okay?))))))
        => #t)

  (test upcase
        (filter-all-chars (lambda (c) (char-upcase c) #f))
        => '())

  (test downcase
        (filter-all-chars (lambda (c) (char-downcase c) #f))
        => '())

  (test titlecase
        (filter-all-chars (lambda (c) (char-titlecase c) #f))
        => '())

  (test foldcase
        (filter-all-chars (lambda (c) (char-foldcase c) #f))
        => '())

  (test general-category
        (report (filter-all-chars (lambda (c)
                                    (char-general-category c)))
                1112064)
        => 0)

  (test alphabetic?
        (report (filter-all-chars char-alphabetic?) 93217)
        => 0)

  (test numeric?
        (report (filter-all-chars char-numeric?) 282)
        => 0)

  (test whitespace?
        (report (filter-all-chars char-whitespace?) 26)
        => 0)

  (test upper-case?
        (report (filter-all-chars char-upper-case?) 1362)
        => 0)

  (test lower-case?
        (report (filter-all-chars char-lower-case?) 1791)
        => 0)

  (test title-case?
        (report (filter-all-chars char-title-case?) 31)
        => 0)

  (test scomp1 (string<? "z" (string es-zed)) => #t)
  (test scomp2 (string<? "z" "zz") => #t)
  (test scomp3 (string<? "z" "Z") => #f)
  (test scomp4 (string=? strasse "Strasse") => #f)

  (test sup1 (string-upcase "Hi") => "HI")
  (test sdown1 (string-downcase "Hi") => "hi")
  (test sfold1 (string-foldcase "Hi") => "hi")

  (test sup2  (string-upcase strasse) => "STRASSE")
  (test sdown2 (string-downcase strasse)
               => (string-append "s" (substring strasse 1 6)))
  (test sfold2 (string-foldcase strasse) => "strasse")
  (test sdown3 (string-downcase "STRASSE")  => "strasse")

  (test chaos1 (string-upcase upper-chaos) => upper-chaos)
  (test chaos2 (string-downcase (string upper-sigma))
               => (string lower-sigma))
  (test chaos3 (string-downcase upper-chaos) => final-chaos)
  (test chaos4 (string-downcase (string-append upper-chaos
                                               (string upper-sigma)))
               => (string-append (substring lower-chaos 0 3)
                                 (string lower-sigma final-sigma)))
  (test chaos5 (string-downcase (string-append upper-chaos
                                               (string #\space
                                                       upper-sigma)))
               => (string-append final-chaos
                                 (string #\space lower-sigma)))
  (test chaos6 (string-foldcase (string-append upper-chaos
                                               (string upper-sigma)))
               => (string-append lower-chaos
                                 (string lower-sigma)))
  (test chaos7 (string-upcase final-chaos) => upper-chaos)
  (test chaos8 (string-upcase lower-chaos) => upper-chaos)

  (test stitle1 (string-titlecase "kNock KNoCK") => "Knock Knock")
  (test stitle2 (string-titlecase "who's there?") => "Who's There?")
  (test stitle3 (string-titlecase "r6rs") => "R6rs")
  (test stitle4 (string-titlecase "R6RS") => "R6rs")

  (test norm1 (string-normalize-nfd (string #\xE9))
              => (string #\x65 #\x301))
  (test norm2 (string-normalize-nfc (string #\xE9))
              => (string #\xE9))
  (test norm3 (string-normalize-nfd (string #\x65 #\x301))
              => (string #\x65 #\x301))
  (test norm4 (string-normalize-nfc (string #\x65 #\x301))
              => (string #\xE9))

  (test sci1 (string-ci<? "z" "Z") => #f)
  (test sci2 (string-ci=? "z" "Z") => #t)
  (test sci3 (string-ci=? strasse "Strasse") => #t)
  (test sci4 (string-ci=? strasse "STRASSE") => #t)
  (test sci5 (string-ci=? upper-chaos lower-chaos) => #t)

 ; eliminate macro binding for test so it doesn't screw up later mats
  (begin (define test) #t)
)

(mat string-titlecase
  (equal? (string-titlecase "ciao12") "Ciao12")
  (equal? (string-titlecase "ciao123") "Ciao123")
  (equal? (string-titlecase "ciao123 futzmo") "Ciao123 Futzmo")
  (equal? (string-titlecase "ciao123 futzmo.  goobar") "Ciao123 Futzmo.  Goobar")
  (equal? (string-titlecase "ciao123 futzmo.  goob33ar") "Ciao123 Futzmo.  Goob33ar")
  (equal? (string-titlecase "ciao123 futzmo.  33ar") "Ciao123 Futzmo.  33Ar")
)
