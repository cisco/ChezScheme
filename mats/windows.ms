;;; windows.ms
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

(if (windows?)
(begin
(mat registry
  (error? (get-registry))
  (error? (get-registry 1 2))
  (error? (put-registry! "hi"))
  (error? (put-registry! 1))
  (error? (put-registry! 1 2 3))
  (error? (remove-registry!))
  (error? (remove-registry! 1 2))
  (error? (get-registry 'pooh))
  (error? (put-registry! "hi" 3))
  (error? (put-registry! 3 "hi"))
  (error? (remove-registry! '(a b c)))
  (error? (get-registry "bogus, is it not?"))

  (not (get-registry "hkey_current_user\\CSmat\\FratRat"))
  (eq? (put-registry! "hkey_current_user\\CSmat\\FratRat" "7233259") (void))
  (equal? (get-registry "hkey_current_user\\CSmat\\FratRat") "7233259")
  (equal? (get-registry "HkEy_CuRrEnT_UsER\\CSmat\\FratRat") "7233259")
  (eq? (remove-registry! "hkey_current_user\\CSmat\\FratRat") (void))
  (error? (remove-registry! "hkey_current_user\\CSmat\\FratRat"))
  (not (get-registry "hkey_current_user\\CSmat\\FratRat"))

  (eq? (put-registry! "hkey_current_user\\CSmat\\North\\South" "east") (void))
  (equal? (get-registry "hkey_current_user\\CSmat\\North\\South") "east")
  (eq? (remove-registry! "hkey_current_user\\CSmat\\North") (void))
  (not (get-registry "hkey_current_user\\CSmat\\North\\South"))

  (eq? (put-registry! "hkey_current_user\\CSmat\\Apple\\Orange\\Banana" "kumquat") (void))
  (equal? (get-registry "hkey_current_user\\CSmat\\Apple\\Orange\\Banana") "kumquat")
  (error? (remove-registry! "hkey_current_user\\CSmat\\Apple"))
  (equal? (get-registry "hkey_current_user\\CSmat\\Apple\\Orange\\Banana") "kumquat")
  (eq? (remove-registry! "hkey_current_user\\CSmat\\Apple\\Orange\\Banana") (void))
  (not (get-registry "hkey_current_user\\CSmat\\Apple\\Orange\\Banana"))
  (error? (remove-registry! "hkey_current_user\\CSmat\\Apple"))
  (eq? (remove-registry! "hkey_current_user\\CSmat\\Apple\\Orange") (void))
  (eq? (remove-registry! "hkey_current_user\\CSmat\\Apple") (void))
  (not (get-registry "hkey_current_user\\CSmat\\Apple\\Orange\\Banana"))
 )
)
(begin ; provide expected errors
  (set! bad-arg-count
    (lambda (who . args)
      (if (#%$suppress-primitive-inlining)
          (errorf #f "incorrect number of arguments ~s to #<procedure ~a>" (length args) who)
          (errorf #f "incorrect argument count in call ~s" (cons who args)))))
(mat registry
  (error? (bad-arg-count 'get-registry))
  (error? (bad-arg-count 'get-registry 1 2))
  (error? (bad-arg-count 'put-registry! "hi"))
  (error? (bad-arg-count 'put-registry! 1))
  (error? (bad-arg-count 'put-registry! 1 2 3))
  (error? (bad-arg-count 'remove-registry!))
  (error? (bad-arg-count 'remove-registry! 1 2))
  (error? (errorf 'get-registry "pooh is not a string"))
  (error? (errorf 'put-registry! "3 is not a string"))
  (error? (errorf 'put-registry! "3 is not a string"))
  (error? (errorf 'remove-registry! "(a b c) is not a string"))
  (error? (errorf 'get-registry "invalid registry key \"bogus, is it not?\""))
  (error? (errorf 'remove-registry! "cannot remove hkey_current_user\\CSmat\\FratRat (not found)"))
  (error? (errorf 'remove-registry! "cannot remove hkey_current_user\\CSmat\\Apple (insufficient permission or subkeys exist)"))
  (error? (errorf 'remove-registry! "cannot remove hkey_current_user\\CSmat\\Apple (insufficient permission or subkeys exist)"))
 )
))

(when (windows?)
(mat multibyte
  (guard (c [(equal? (condition-message c) "invalid code page ~s")])
    (string->multibyte -1 "hello")
    #t)
  (guard (c [(equal? (condition-message c) "invalid code page ~s")])
    (string->multibyte 'cp-what? "hello")
    #t)
  (guard (c [(equal? (condition-message c) "invalid code page ~s")])
    (multibyte->string -1 #vu8(#x61 #x62))
    #t)
  (guard (c [(equal? (condition-message c) "invalid code page ~s")])
    (multibyte->string 'cp-not! #vu8(#x61 #x62))
    #t)
  (guard (c [(equal? (condition-message c) "~s is not a bytevector")])
    (multibyte->string 'cp-acp "hello")
    #t)
  (guard (c [(equal? (condition-message c) "~s is not a string")])
    (string->multibyte 'cp-acp 'hello)
    #t)
  (let ()
    (define (f str)
      (let ([bv (string->utf8 str)])
        (equal? (multibyte->string 'cp-utf8 bv) str)))
    (and
      (f "hello\n")
      (f "hel\x0;lo\n")
      (f "hel\x0;\x3bb;lo\n")))
  (let ()
    (define (g str)
      (let ([bv (string->multibyte 'cp-utf8 str)])
        (equal? (utf8->string bv) str)))
    (and
      (g "hello\n")
      (g "hel\x0;lo\n")
      (g "hel\x0;\x3bb;lo\n")))
  (let ()
    (define (f str)
      (let ([bv (string->multibyte 'cp-acp str)])
        (equal? (multibyte->string 'cp-acp bv) str)))
    (and
      (f "hello\n")
      (f "hel\x0;lo\n")))
  (let ()
    (define (f str)
      (let ([bv (string->multibyte 'cp-oemcp str)])
        (equal? (multibyte->string 'cp-oemcp bv) str)))
    (and
      (f "hello\n")
      (f "hel\x0;lo\n")))
  (let ()
    (define (f str)
      (let ([bv (string->multibyte 'cp-thread-acp str)])
        (equal? (multibyte->string 'cp-thread-acp bv) str)))
    (and
      (f "hello\n")
      (f "hel\x0;lo\n")))
  (let ()
    (define (f str)
      (let ([bv (string->multibyte 'cp-utf7 str)])
        (equal? (multibyte->string 'cp-utf7 bv) str)))
    (and
      (f "hello\n")
      (f "hel\x0;lo\n")
      (f "hel\x0;\x3bb;lo\n")))
  (let ()
    (define (f str)
      (let ([bv (string->multibyte 'cp-utf8 str)])
        (equal? (multibyte->string 'cp-utf8 bv) str)))
    (and
      (f "hello\n")
      (f "hel\x0;lo\n")
      (f "hel\x0;\x3bb;lo\n")))
))
