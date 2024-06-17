;;; format.ms
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

; need some for printf and fprintf
; need some when string is not known at compile time
; need some when number of args is not known at compile time, e.g.,
;    (apply format "hi ~s" args)
; construct testing mechanism that gives us all of these from a
; single set of directives
; need some tabulate tests

(mat format-errors
  (error? (format "hi ~s"))
  (error? (format "hi ~s" 'a 'b))
  (error? (format "hi ~"))
  (error? (format "hi ~23"))
  (error? (format "hi ~23@"))
  (error? (format "hi ~@"))
  (error? (format 7 "hi ~@"))
  (error? (format "~@%"))
  (error? (format "~@c%"))
  (error? (format "~c" 3))
  (error? (format "~@~"))
  (error? (format "~@@s"))
  (error? (format "~@3"))
  (error? (format "~q"))
  (error? (format "~,,,-3:d" (expt 2 100)))
  (error? (format "~,,-3,3e" -3.14159))
  (error? (printf 'hello "there"))
  (error? (fprintf 'not-a-port "~a ~s" 17 34))
  (error? (format 'not-a-string-port-or-boolean "~a ~s" 17 34))
  (error? (format "bad~\rdirective"))
  (error? (printf "~a~:*"))
)

(mat format-continuation ; like slib tests, but with \r\n for DOS
  (equal? (format "abc~\r\n    123") "abc123")
  (equal? (format "abc~\r\n   ") "abc")
  (equal? (format "abc~:\r\n   def") "abc   def")
  (equal? (format "abc~@\r\n   def") "abc\ndef")
)

(mat format-plain
  (equal? (format "") "")
  (equal? (format "a") "a")
  (equal? (format "ab") "ab")
  (equal? (format "ab~%cd") "ab\ncd")
  (equal? (format "ab\ncd") "ab\ncd")
  (equal? (format "a\nb\ncc\nddd\neeee") "a\nb\ncc\nddd\neeee")
  (equal? (format "a~&b~%cc~&ddd~%eeee") "a\nb\ncc\nddd\neeee")
  (equal? (format "a\nb\ncc\nddd\neeee\n") "a\nb\ncc\nddd\neeee\n")
  (equal? (format "a~%b~&cc~%ddd~&eeee\n") "a\nb\ncc\nddd\neeee\n")
  (equal? (format "\na\nb\ncc\nddd\neeee") "\na\nb\ncc\nddd\neeee")
  (equal? (format "~%a~&b~%cc~&ddd~%eeee") "\na\nb\ncc\nddd\neeee")
  (equal? (format "\na\nb\ncc\nddd\neeee\n") "\na\nb\ncc\nddd\neeee\n")
  (equal? (format "~%a~%b~&cc~%ddd~&eeee\n") "\na\nb\ncc\nddd\neeee\n")
)

(mat format-object
  (equal? (format "hi ~s" "a") "hi \"a\"")
  (equal? (format "hi ~10s" "a") "hi \"a\"       ")
  (equal? (format "hi ~10@s" "a") "hi        \"a\"")
  (equal? (format "~10,3,2,'$@s" 345) "$$$$$$$$345")
  (equal? (format "~10,3,2,'$@s" 3456) "$$$$$$$$3456")
  (equal? (format "~10,3,2,'$@s" 34567) "$$$$$34567")
  (equal? (format "~10,3,2,'$@s" 345678) "$$$$$345678")
  (equal? (format "~10,3,2,'$@s" 3456789) "$$$$$3456789")
  (equal? (format "~10,3,2,'$@s" 34567890) "$$34567890")
  (equal? (format "~10,3,2,'$@s" 345678901) "$$345678901")
  (equal? (format "~10,3,2,'$@s" 3456789012) "$$3456789012")
  (equal? (format "~7,,4,a~3%~10,3,,'#@s" "hello" 345)
          "hello    \n\n\n#########345")
  (equal? (format "~:s" '#{g0 ymnnefx976kvhp9-a}) "g0")
  (equal? (format "~s" '#{g0 ymnnefx976kvhp9-a}) "#{g0 ymnnefx976kvhp9-a}")
  (equal? (format "~,,2@s" 345678901) "  345678901")
  (equal? (format "~,,2s" 345678901) "345678901  ")
)

(mat format-char
  (equal? (format "~c" #\a) "a")
  (equal? (format "~c" #\space) " ")
  (equal? (format "~:c" #\a) "a")
  (equal? (format "~:c" #\space) "<space>")
  (equal? (format "~:c" #\034) "^\\")
  (equal? (format "~:c" #\003) "^C")
  (equal? (format "~@c" #\a) "#\\a")
  (equal? (format "~@c" #\space) "#\\space")
  (equal? (format "~:@c" #\a) "a")
  (equal? (format "~:@c" #\space) "<space>")
  (equal? (format "~:@c" #\034) "^\\")
  (equal? (format "~:@c" #\003) "^C")
  (equal? (format "~@:c" #\a) "a")
  (equal? (format "~@:c" #\space) "<space>")
  (equal? (format "~@:c" #\034) "^\\")
  (equal? (format "~@:c" #\003) "^C")
)

(mat format-plural
  (error? (format "abc~:p" 1))
  (error? (format "abc~:p"))
  (error? (format "abc~:@p"))
  (error? (format "abc~:p~s" 2))
  (error? (format "abc~:@p~s" 2))
  (equal? (format "~s abc~:p" 1) "1 abc")
  (equal? (format "~s abc~:p" 2) "2 abcs")
  (equal? (format "~s abc~:p" 1.0) "1.0 abcs")
  (equal? (format "~s abc~:p" 'one) "one abcs")
  (equal? (format "abc~p" 1) "abc")
  (equal? (format "abc~p" 2) "abcs")
  (equal? (format "abc~p" 'kumquat) "abcs")
  (equal? (format "abc~@p" 1) "abcy")
  (equal? (format "abc~@p" 'kumquat) "abcies")
  (equal? (format "~s~@:p" 1) "1y")
  (equal? (format "~s~@:p" 2) "2ies")
)

(mat format-convert-case
  (error? (format "~23:(abc)"))
  (error? (format "~,:(abc)"))
  (error? (format "~(abc"#|)|#))
  (error? (format "~:(abc)"))
  (error? (format #|(|#"abc~)"))
  (error? (format "~(~r ~(~a~)~) ~:@(~a)" 1621 "piNK" "bLuE"))
  (equal? (format "~(AbC 123A DEF g~)") "abc 123a def g")
  (equal? (format "~:(AbC 123A DEF g~)") "Abc 123a Def G")
  (equal? (format "~@(AbC 123A DEF g~)") "Abc 123a def g")
  (equal? (format "~:@(AbC 123A DEF g~)") "ABC 123A DEF G")
  (equal? (format "~@:(AbC 123A DEF g~)") "ABC 123A DEF G")
  (equal? (format "~@:(~r ~a~) ~a" 1621 "piNK" "bLuE")
          "SIXTEEN HUNDRED TWENTY-ONE PINK bLuE")
  (equal? (format "~:@(~r ~a~) ~a" 1621 "piNK" "bLuE")
          "SIXTEEN HUNDRED TWENTY-ONE PINK bLuE")
  (equal? (format "~@(~r ~a~) ~a" 1621 "piNK" "bLuE")
          "Sixteen hundred twenty-one pink bLuE")
  (equal? (format "~:(~r ~a~) ~a" 1621 "piNK" "bLuE")
          "Sixteen Hundred Twenty-One Pink bLuE")
  (equal? (format "~(~r ~a~) ~a" 1621 "piNK" "bLuE")
          "sixteen hundred twenty-one pink bLuE")
  (equal? (format "~(~r ~(~a~)~) ~a" 1621 "piNK" "bLuE")
          "sixteen hundred twenty-one pink bLuE")
  (equal? (format "~(~r ~(~a~)~) ~:@(~a~)" 1621 "piNK" "bLuE")
          "sixteen hundred twenty-one pink BLUE")

 ; cltl2 tests
  (equal? (format "~@R ~(~@R~)" 14 14) "XIV xiv")
  (begin
    (define $f (lambda (n) (format "~@(~R~) error~:P detected." n)))
    (procedure? $f))
  (equal? ($f 0) "Zero errors detected.")
  (equal? ($f 1) "One error detected.")
  (equal? ($f 23) "Twenty-three errors detected.")
)

(mat format-indirect
  (error? (format "~?" 3))
  (error? (format "~@?" 3))
  (error? (format "~?" "abc"))
  (error? (format "~?" "~a" 4))
  (error? (format "~?" "~a" '()))
;  (error? (format "~@?" "abc" '())) ; too many args
  (error? (format "~@?" "~(abc"#|)|#))
  (error? (format "~@?" "~:?"))
  (equal? (format "==> ~? <==" "~a" '(5)) "==> 5 <==")
  (equal? (format "<~@?>" "abc") "<abc>")
  (equal? (format "~:(<~@?>~)" "abc") "<Abc>")
  (equal? (format "<~@?>" "~:@(abc~)") "<ABC>")
  (equal? (format "<~@?>" "~r ~a" 101 "dalmations")
          "<one hundred one dalmations>")
  (equal? (format "<~?~a>" "~r ~a" '(101 "dalmations") "!!!")
          "<one hundred one dalmations!!!>")
  (equal? (format "<~?>" "[~?]" '("(~?)" ("~a" (3)))) "<[(3)]>")
  (equal? (format "<~@?>" "[~@?]" "(~@?)" "~a" 3) "<[(3)]>")
  (error? (format "<~@?>" "[~@?]" "(~@?)" "~a"))
;  (error? (format "<~@?>" "[~@?]" "(~@?)" "~a" 3 4)) ; too many args
;  (error? (format "<~?>" "[~?]" '("(~?)" ("~a" (3 4))))) ; too many args
;  (error? (format "<~?>" "[~?]" '("(~?)" ("~a" (3) 4)))) ; too many args

 ; cltl2 tests
  (equal? (format "~? ~d" "<~a ~d>" '("Foo" 5) 7) "<Foo 5> 7")

 ; cltl2 doesn't want us to complain about too many arguments
  (equal? (format "~? ~d" "<~a ~d>" '("Foo" 5 14) 7) "<Foo 5> 7")

  (equal? (format "~@? ~D" "<~A ~D>" "Foo" 5 7) "<Foo 5> 7")
  (equal? (format "~@? ~D" "<~A ~D>" "Foo" 5 14 7) "<Foo 5> 14")

  (begin
    (define (format-error ctl-index ctl-string string . args)
      (format "~?~%~v@tv~%~3@t\"~a\"~%"
        string args (+ ctl-index 3) ctl-string))
    (procedure? format-error))
  (equal? (format-error 16 "The item is a ~[Foo~;Bar~;Loser~]."
            "ERROR: The argument to the format ~s command must be a number."
            "~[")
          "ERROR: The argument to the format \"~[\" command must be a number.\n                   v\n   \"The item is a ~[Foo~;Bar~;Loser~].\"\n")
)

(mat format-conditional/at
  (error? (format "~@[abc~;def~]"))
  (error? (format "~@[abc]"))
  (error? (format #|(|# "~@[abc~)"))
  (equal? (format "<~@[[in ~s]~]>" #f) "<>")
  (equal? (format "<~@[[in ~s]~]>" 'foo) "<[in foo]>")
;  (error? (format "<~@[[hey!]~]>" 'foo)) ; too many args
  (equal? (format "<~@[~]> ~s" #t) "<> #t")
  (error? (format "<~@[~]> ~s" #f))
)

(mat format-conditional/colon
  (error? (format "~:[abc~:;def~]"))
  (error? (format "~:[abc]"))
  (error? (format #|(|# "~:[abc~)"))
  (equal? (format "<~:[abc~;def~]>" #f) "<abc>")
  (equal? (format "<~:[abc~;def~]>" #t) "<def>")
  (error? (format "<~:[abc~;def~;ghi~]>"))
  (equal? (format "<~:[abc~;~a~]>" #f) "<abc>")
  (equal? (format "<~:[abc~;~a~]>" #t 'yow!) "<yow!>")
  (equal? (format "<~:[abc~;~:*~a~]>" #t) "<#t>")
  (error? (format "<~:[abc~;~a~]>" #t))
  (error? (format "<~:[abc~]>" #f))
)

(mat format-conditional
  (error? (format "~[abc~:;def~;ghi~]"))
  (error? (format "~[abc]"))
  (error? (format #|(|# "~[abc~)"))
  (equal? (format "<~[abc~;def~]>" 0) "<abc>")
  (equal? (format "<~[abc~;def~]>" 1) "<def>")
  (equal? (format "<~[abc~;def~]>" -15) "<>")
  (equal? (format "<~[abc~;def~:;ghi~]>" 0) "<abc>")
  (equal? (format "<~[abc~;def~:;ghi~]>" 1) "<def>")
  (equal? (format "<~[abc~;def~:;ghi~]>" 2) "<ghi>")
  (equal? (format "<~[abc~;def~:;ghi~]>" 'huh?) "<ghi>")
  (equal? (format "+++~[~s~;~r ~s~]---" 52) "+++---")
;  (error? (format "+++~[~s~;~r ~s~]---" 52 23)) ; too many args
;  (error? (format "+++~[~s~;~@r~s~]---" 52 23 '*)) ; too many args
  (equal? (format "+++~[~s~;~r ~s~]---" 0 23) "+++23---")
  (error? (format "+++~[~s~;~r ~s~]---" 0))
  (equal? (format "+++~[~s~;~@r~s~]---" 1 23 '*) "+++XXIII*---")
  (error? (format "+++~[~s~;~@r~s~]---" 1 23))
  (equal? (format "+++~[~]---" 1) "+++---")
)

(mat format-tabulate
  (error? (format "~-7t***"))
  (error? (format "~8,'xt"))
  (error? (format "~8,-3t"))
  (error? (format "~8,5,4t"))
  (error? (format "~-7@t***"))
  (error? (format "~8,'x@t"))
  (error? (format "~8,-3@t"))
  (error? (format "~8,5,4@t"))
  (equal? (format "~t***") " ***")
  (equal? (format "x~t***") "x ***")
  (equal? (format "~,3t***") " ***")
  (equal? (format "xxxx~,3t***") "xxxx   ***")
  (equal? (format "xxxx~1,3t***") "xxxx   ***")
  (equal? (format "~0t***") " ***")
  (equal? (format "~1t***") " ***")
  (equal? (format "~2t***") "  ***")
  (equal? (format "~7t***") "       ***")
  (equal? (format "~0,0t***") "***")
  (equal? (format "~1,0t***") " ***")
  (equal? (format "~2,0t***") "  ***")
  (equal? (format "~7,0t***") "       ***")
  (equal? (format "~0,8t***") "        ***")
  (equal? (format "~1,8t***") " ***")
  (equal? (format "~2,8t***") "  ***")
  (equal? (format "~7,8t***") "       ***")
  (equal? (format "~8,8t***") "        ***")
  (equal? (format "~9,8t***") "         ***")
  (equal? (format "x~0t***") "x ***")
  (equal? (format "x~1t***") "x ***")
  (equal? (format "x~2t***") "x ***")
  (equal? (format "x~7t***") "x      ***")
  (equal? (format "x~0,0t***") "x***")
  (equal? (format "x~1,0t***") "x***")
  (equal? (format "x~2,0t***") "x ***")
  (equal? (format "x~7,0t***") "x      ***")
  (equal? (format "x~0,8t***") "x       ***")
  (equal? (format "x~1,8t***") "x        ***")
  (equal? (format "x~2,8t***") "x ***")
  (equal? (format "x~7,8t***") "x      ***")
  (equal? (format "x~8,8t***") "x       ***")
  (equal? (format "x~9,8t***") "x        ***")
  (equal? (format "xxx~7,0@tyyy") "xxx       yyy")
  (equal? (format "xxx~7,1@tyyy") "xxx       yyy")
  (equal? (format "xxx~7,8@tyyy") "xxx             yyy")
)

(mat format-justify
  (equal? (format "~<ab~^c~>") "") ; not checking to make sure ~^ is at front of segment
  (equal? (format "~<abc~>") "abc")
  (equal? (format "~:@<abc~>") "abc")
  (equal? (format "~,,1,'*:@<abc~>") "*abc*")
  (equal? (format "~10<abc~>") "       abc")
  (equal? (format "~10:<abc~>") "       abc")
  (equal? (format "~10@<abc~>") "abc       ")
  (equal? (format "~10:@<abc~>") "    abc   ")
  (equal? (format "~,8<abc~>") "     abc")
  (equal? (format "~,8<abc~;def~;ghi~>") "abc    def   ghi")
  (equal? (format "~7,8<abc~;def~;ghi~>") "abc   def   ghi")
  (equal? (format "~7,8:<abc~;def~;ghi~>") "  abc  def  ghi")
  (equal? (format "~7,8@<abc~;def~;ghi~>") "abc  def  ghi  ")
  (equal? (format "~7,8:@<abc~;def~;ghi~>") "  abc def  ghi ")
  (equal? (format "~&~7,8:@<abc~;def~;ghi~>~&~&") "  abc def  ghi \n")
  (equal? (format "~7,8,5,'*<abc~;def~;ghi~>") "abc*******def*******ghi")
  (equal? (format "~5,8,4,'*:@<abc~;def~;ghi~>")
          "*****abc*****def*****ghi*****")
  (equal? (format "~1,8,4,'*:@<abc~;def~;ghi~>")
          "****abc****def****ghi****")
  (equal? (format "~,,4,'*:@<abc~;def~;ghi~>")
          "****abc****def****ghi****")
  (equal? (format "~7,8,5,'*<~%~,10:;abc~;def~;ghi~>")
          "\nabc*******def*******ghi")
  (equal? (format "~7,8,5,'*<~&~,10:;abc~;def~;ghi~>")
          "abc*******def*******ghi")
  (equal? (format "~7,8,5,'*<~%~,25:;abc~;def~;ghi~>")
          "abc*******def*******ghi")
  (equal? (format "~7,8,5,'*<~%~2,25:;abc~;def~;ghi~>")
          "abc*******def*******ghi")
  (equal? (format "~7,8,5,'*<~%~3,25:;abc~;def~;ghi~>")
          "\nabc*******def*******ghi")
  (equal? (format "~7,8,5,'*<~%~:;abc~;def~;ghi~>")
          "abc*******def*******ghi")
  (equal? (format "~72,,,'-<~%~:;abc~;def~;ghi~>")
          "abc--------------------------------def-------------------------------ghi")
  (equal? (format "~73,,,'-<~%~:;abc~;def~;ghi~>")
          "\nabc--------------------------------def--------------------------------ghi")
  (equal? (format "~73,,,'-:@<~%~:;abc~;def~;ghi~>")
          "\n----------------abc----------------def----------------ghi----------------")
  (equal? (format "~10<~^~a~;~^~a~>" "abc" "def") "abc    def")
  (equal? (format "~10<~^~a~;~^~a~>" "abc") "       abc")
  (equal? (format "~10<~^~a~;~^~a~>") "          ")
  (equal? (format "~10<~^~a~,9:;~a~;~a~>" "\n" "1" "2") "\n1        2")
  (equal? (format "~10<~^~a~,9:;~a~;~a~>") "          ")
  (equal? (format "~10<~^~a~,9:;~a~;~^~a~>" "\n" "1" "2") "\n1        2")
  (equal? (format "~10<~^~a~,9:;~a~;~^~a~;~a~>" "\n" "1" "2" "3")
          "\n1    2   3")
  (error? (format "~10<~^~a~,9:;~a~;~^~a~;~a~>" "\n" "1" "2"))
  (equal? (format "~10<~^~a~,9:;~a~;~^~a~;~a~>" "\n" "1") "\n         1")
  (error? (format "~10<~^~a~,9:;~a~;~^~a~;~a~>" "\n"))
  (equal? (format "~10<~^~a~,9:;~a~;~^~a~;~a~>") "          ")
  (error? (format "~10<~^~a~,9:;~a~;~a~:^~;~a~>" "\n"))
  (error? (format "~10<~^~a~,9:;~a~;~^~^~a~;~a~>" "\n"))
  (error? (format "~10<~^~a~,9:;~a~;~a~@^~;~a~>" "\n" "1"))
  (error? (format "~10<~(abc~>def~)"))
  (equal? (format "~10<~@:(abc~)~;~@(def~)~>") "ABC    Def")
  (equal? (format "~(~10<~a~;~x~>~)" "PiEs" 221) "pies    dd")
  (equal? (format "~13<~s~;~s~;~s~>" 3.4 4.5 5.6) "3.4  4.5  5.6")
  (equal? (format "~16<~f~;~e~;~g~>" 3.4 4.5 5.6) "3.4  4.5e+0  5.6")

 ; test nested ~<...~>
  (equal? (format "~20<abc~;d~5<e~;f~>g~>") "abc          de   fg")

 ; from cltl2:
  (equal? (format "~10<foo~;bar~>") "foo    bar")
  (equal? (format "~10:<foo~;bar~>") "  foo  bar")
  (equal? (format "~10<foobar~>") "    foobar")
  (equal? (format "~10:<foobar~>") "    foobar")
  (equal? (format "~10:@<foo~;bar~>") "  foo bar ")
  (equal? (format "~10@<foobar~>") "foobar    ")
  (equal? (format "~10:@<foobar~>") "  foobar  ")
  (equal? (format "~%;; ~{~<~%;; ~1:; ~s~>~^,~}.~%" '(a b c))
          "\n;;  a, b, c.\n")
  (equal? (format "~%;; ~{~<~%;; ~1:; ~s~>~^,~}.~%"
            '(list-procedure stack $system-environment $active-threads
              #{source yqrk281einmw7sg-a} $c-info placeholder
              make-record-type join-subst trace-let set-top-level-value!
              integer? error result make-resolved-interface
              single->double word eleven clear-input-port reverse!
              eighteen zero write-radix-commas? symbol-value exact->inexact
              subst! type $apply-procedure loop/p write-radix-sign?))
          "\n;;  list-procedure, stack, $system-environment, $active-threads,\n;;  #{source yqrk281einmw7sg-a}, $c-info, placeholder, make-record-type,\n;;  join-subst, trace-let, set-top-level-value!, integer?, error,\n;;  result, make-resolved-interface, single->double, word, eleven,\n;;  clear-input-port, reverse!, eighteen, zero, write-radix-commas?,\n;;  symbol-value, exact->inexact, subst!, type, $apply-procedure,\n;;  loop/p, write-radix-sign?.\n")
  (equal? (format "~%;; ~{~<~%;; ~1,50:; ~s~>~^,~}.~%"
            '(list-procedure stack $system-environment $active-threads
              #{source yqrk281einmw7sg-a} $c-info placeholder
              make-record-type join-subst trace-let set-top-level-value!
              integer? error result make-resolved-interface
              single->double word eleven clear-input-port reverse!
              eighteen zero write-radix-commas? symbol-value exact->inexact
              subst! type $apply-procedure loop/p write-radix-sign?))
          "\n;;  list-procedure, stack, $system-environment,\n;;  $active-threads, #{source yqrk281einmw7sg-a},\n;;  $c-info, placeholder, make-record-type,\n;;  join-subst, trace-let, set-top-level-value!,\n;;  integer?, error, result,\n;;  make-resolved-interface, single->double, word,\n;;  eleven, clear-input-port, reverse!, eighteen,\n;;  zero, write-radix-commas?, symbol-value,\n;;  exact->inexact, subst!, type,\n;;  $apply-procedure, loop/p, write-radix-sign?.\n")
  (equal? (format "~&;; ~{~<~%~&;; ~1:; ~s~>~^,~}.~&"
            '(list-procedure stack $system-environment $active-threads
              #{source yqrk281einmw7sg-a} $c-info placeholder
              make-record-type join-subst trace-let set-top-level-value!
              integer? error result make-resolved-interface
              single->double word eleven clear-input-port reverse!
              eighteen zero write-radix-commas? symbol-value exact->inexact
              subst! type $apply-procedure loop/p write-radix-sign?))
          ";;  list-procedure, stack, $system-environment, $active-threads,\n;;  #{source yqrk281einmw7sg-a}, $c-info, placeholder, make-record-type,\n;;  join-subst, trace-let, set-top-level-value!, integer?, error,\n;;  result, make-resolved-interface, single->double, word, eleven,\n;;  clear-input-port, reverse!, eighteen, zero, write-radix-commas?,\n;;  symbol-value, exact->inexact, subst!, type, $apply-procedure,\n;;  loop/p, write-radix-sign?.\n")
  (equal? (format "~15<~s~;~^~s~;~^~s~>" 'foo) "            foo")
  (equal? (format "~15<~s~;~^~s~;~^~s~>" 'foo 'bar) "foo         bar")
  (equal? (format "~15<~s~;~^~s~;~^~s~>" 'foo 'bar 'baz) "foo   bar   baz")
)

(mat format-iterate
  (error? (format "~{abc~:~}")) ; ~ directive has no : flag
  (error? (format "~{abc~;~}")) ; misplaced directive "~;"

 ; ~{...}
  (error? (format "~{|~s~}")) ; too few args
;  (error? (format "~{|~s~}" '() "$")) ; too many args
  (equal? (format "~{|~s~}" '()) "")
  (equal? (format "~a~{|~s~}~a" "^" '(a b c) "$") "^|a|b|c$")
  (equal? (format "~a~{|~s~:}~a" "^" '(a b c) "$") "^|a|b|c$")
  (equal? (format "~a~{abc~:}~a" "^" '() "$") "^abc$")
  (equal? (format "~a~2{|~s~:}~a" "^" '(a b c) "$") "^|a|b$")
  (error? (format "~a~2{|~s~:}~a" "^" '() "$")) ; too few args
  (equal? (format "+~{<~s~^~s>~}+~{<~s~^~s>~}+" '(a b c d) '(a b c))
          "+<ab><cd>+<ab><c+")
  (equal? (format "+~{<~s~:^~s>~}+~{<~s~^~s>~}+" '(a b c d) '(a b c))
          "+<ab><cd>+<ab><c+")
  (error? (format "+~{<~s~:^~s>~}+~{<~s~:^~s>~}+" '(a b c d) '(a b c))) ; too few args
  (equal? (format "~a~{~}~a"
            "^"
            "+~{<~s~^~s>~}+"
            '((a b c d) (a b c) () (a))
            "$")
          "^+<ab><cd>++<ab><c++++<a+$")
  (error? (format "~a~{~}~a"
            "^"
            "+~{<~s~s>~}+"
            '((a b c d) (a b c) () (a))
            "$")) ; too few args for "+~{<~s~s>~}+"

 ; ~:{...}
  (error? (format "~:{|~s~}")) ; too few args
;  (error? (format "~:{|~s~}" '() "$")) ; too many args
  (equal? (format "~:{|~s~}" '()) "")
  (equal? (format "~a~:{|~s~}~a" "^" '((a) (b) (c)) "$") "^|a|b|c$")
  (equal? (format "~a~:{|~s~:}~a" "^" '((a) (b) (c)) "$") "^|a|b|c$")
  (equal? (format "~a~:{abc~:}~a" "^" '() "$") "^abc$")
  (equal? (format "~a~2:{|~s~:}~a" "^" '((a) (b) (c)) "$") "^|a|b$")
  (equal? (format "~:{<~s~^~s>~:}" '((a b) (c) (e f))) "<ab><c<ef>")
  (equal? (format "~:{<~s~:^~s>~:}" '((a b) (c d) (e f))) "<ab><cd><e")
  (equal? (format "~a~:{~}~a" "^" "<~s~:^~s>" '() "$") "^$")
  (error? (format "~a~:{~}~a" "^" "<~s~:^~s>" '(a b) "$")) ; a is not a pair
  (equal? (format "~a~:{~}~a" "^" "<~s~^~s>" '((a b)) "$") "^<ab>$")
  (equal? (format "~a~:{~}~a" "^" "<~s~:^~s>" '((a b)) "$") "^<a$")
;  (error? (format "~a~:{~}~a" "^" "<~s~^~s>" '((a b c)) "$")) ; too many args for "<~s~^~s>"

  (equal? (format "~a~:{~}~a" "^" "<~s~^~s>" '((a b) (c)) "$") "^<ab><c$")
  (error? (format "~a~:{~}~a" "^" "<~s~:^~s>" '((a b) (c) (d e)) "$")) ; too few args for "<~s~:^~s>" 
  (equal? (format "~a~:{~}~a" "^" "<~s~:^~s>" '((a b) (c d)) "$") "^<ab><c$")

  (equal? (format "~:{~}" "~s" '()) "")
  (error? (format "~:{~}" "~s" '(()))) ; too few args for "~s"
  (error? (format "~:{~}" "~s" '(a b))) ; a is not a list
;  (error? (format "~:{~}" "~s" '((a b)))) ; too many args for "~s"
  (error? (format "~:{~s~:}" '())) ; too few args
  (error? (format "~:{~s~}" '(a b))) ; a is not a list
;  (error? (format "~:{|~s~}" '((a b)))) ; too many args

 ; ~@{...}
  (equal? (format "~@{|~s~}") "")
  (equal? (format "~@{|~s~}" 'a 'b 'c) "|a|b|c")
  (equal? (format "~@{|~s~:}" '(a) '(b) '(c)) "|(a)|(b)|(c)")
  (equal? (format "~@{abc~:}") "abc")
  (equal? (format "~2@{|~s~:}~s" 'a 'b 'c) "|a|bc")
  (error? (format "~2@{|~s~:}")) ; too few args
  (equal? (format "~@{<~s~^~s>~}" 'a 'b 'c 'd) "<ab><cd>")
  (equal? (format "~@{<~s~^~s>~}" 'a 'b 'c 'd 'e) "<ab><cd><e")
  (equal? (format "~@{<~s~:^~s>~}" 'a 'b 'c 'd) "<ab><cd>")
  (error? (format "~@{<~s~:^~s>~}" 'a 'b 'c 'd 'e)) ; too few args

 ; ~@:{...}
  (equal? (format "~@:{|~s~}") "")
  (equal? (format "~@:{|~s~}" '(a) '(b) '(c)) "|a|b|c")
  (equal? (format "~@:{|~s~:}" '(a) '(b) '(c)) "|a|b|c")
  (equal? (format "~@:{abc~:}") "abc")
  (equal? (format "~2@:{|~s~:}~s" '(a) '(b) '(c)) "|a|b(c)")
  (equal? (format "~@:{<~s~^~s>~:}" '(a b) '(c) '(e f)) "<ab><c<ef>")
  (equal? (format "~@:{<~s~:^~s>~:}" '(a b) '(c d) '(e f)) "<ab><cd><e")
  (equal? (format "~@:{~:}" "<~s~:^~s>" '(a b) '(c d) '(e f)) "<ab><cd><e")
  (error? (format "~@:{~}" "~s" '()))
;  (error? (format "~@:{~}" "~s" '(a b))) ; too many args
  (error? (format "~@:{~s~}" '()))
;  (error? (format "~@:{~s~}" '(a b))) ; too many args
  (error? (format "~:@{|~s~}" 'a 'b 'c)) ; a is not a list

 ; ~{...~} tests from cltl2
  (equal? (format "The winners are:~{ ~S~}." '(fred harry jill))
          "The winners are: fred harry jill.")
  (equal? (format "Pairs:~{ <~S,~S>~}." '(a 1 b 2 c 3))
          "Pairs: <a,1> <b,2> <c,3>.")
  (equal? (format "Pairs:~:{ <~S,~S>~}." '((a 1) (b 2) (c 3)))
          "Pairs: <a,1> <b,2> <c,3>.")
  (equal? (format "Pairs:~@{ <~S,~S>~}." 'a 1 'b 2 'c 3)
          "Pairs: <a,1> <b,2> <c,3>.")
  (equal? (format "Pairs:~:@{ <~S,~S>~}." '(a 1) '(b 2) '(c 3))
          "Pairs: <a,1> <b,2> <c,3>.")

  (equal? (format "~1{~:}" "a ~b c" '(5)) "a 101 c")
  (equal? (format "~1{~:}" "a ~b c" '(5))
          (apply format "a ~b c" '(5)))

 ; ~^ tests from cltl2
  (equal? (format "~:{/~s~^ ...~}"
                  '((hot dog) (hamburger) (ice cream) (french fries)))
          "/hot .../hamburger/ice .../french ...")
  (equal? (format "~:{/~s~:^ ...~}"
                  '((hot dog) (hamburger) (ice cream) (french fries)))
          "/hot .../hamburger .../ice .../french")
 ; this one appears not to be supported by cltl text, but it's one of
 ; the examples
  (equal? (format "~:{/~s~#:^ ...~}"
                  '((hot dog) (hamburger) (ice cream) (french fries)))
          "/hot .../hamburger")
 ; was all screwed up in cltl2, and didn't illustrate ~^ within
 ; ~[...~] as advertised:
  ; (begin (define tellstr "~@{~@[~R~]~^ ~A.~}") (string? tellstr))
  ; (equal? (format tellstr 23) "Twenty-three.")
  ; (equal? (format tellstr #f "losers") "losers.")
  ; (equal? (format tellstr 23 "losers") "Twenty-three losers.")
 ; probably meant something more like this:
  (begin (define tellstr "~@{~@[~R~^ ~]~A~}.") (string? tellstr))
  (equal? (format tellstr 23) "twenty-three.")
  (equal? (format tellstr #f "losers") "losers.")
  (equal? (format tellstr 23 "losers") "twenty-three losers.")
)

(mat format-goto
  (error? (format "~*"))
  (error? (format "~s ~*" 0))
  (equal? (format "~*~s" 0 1) "1")
;  (error? (format "~0*~s" 0 1)) ; too many args
  (error? (format "~2*~s" 0 1))
  (error? (format "~3*~s" 0 1))
  (error? (format "~-3*~s" 0 1))
;  (error? (format "~s ~:*" 0)) ; too many args
  (equal? (format "~s~:*~s" 0) "00")
  (error? (format "~s~2:*~s" 0))
  (error? (format "~s~0:*~s" 0))
  (error? (format "~s~:@*~s" 0))
  (error? (format "~s~@:*~s" 0))
  (error? (format "~:*"))
  (error? (format "~:* ~s" 0))
  (equal? (format "~@*") "")
;  (error? (format "~s~@*" 0)) ; too many args
;  (error? (format "~@*" 0)) ; too many args
  (equal? (format "~s~:*~s~s~s~2:*~s~3*~s~@*~s~s~1@*~s~5@*~s" 'a 'b 'c 'd 'e 'f)
          "aabcbfabbf")
  (equal? (format "~s~?~:*~s~s" '< "~s~s~:*~s~*~s" '(a b c d) '>)
          "<abbd(a b c d)>")
  (equal? (format "~s~@?~:*~s~s" '< "~s~s~:*~s~*~s" 'a 'b 'c 'd '>)
          "<abbdd>")
)

(mat format-radix
  (equal? (format "~d" 3) "3")
  (equal? (format "~3d" 1) "  1")
  (equal? (format "~:d" 12345) "12,345")
  (equal? (format "~:@d" 12345) "+12,345")
  (equal? (format "~@:d" 12345) "+12,345")
  (equal? (format "~:d" -12345) "-12,345")
  (equal? (format "~:@d" -12345) "-12,345")
  (equal? (format "~@:d" -12345) "-12,345")
  (equal? (format "~:b" #b10110110101) "10,110,110,101")
  (equal? (format "~20,'q,'%,4:@b" #b10110110101) "qqqqqq+101%1011%0101")
  (equal? (format "~,,' ,4b" #xface) "1111101011001110") ; cltl2 example: "1111 1010 1100 1110"
  (equal? (format "~,,' ,4:b" #xface) "1111 1010 1100 1110")
  (equal? (format "~19,,' ,4:b" #x1ce) "        1 1100 1110") ; cltl2 example: "0000 0001 1100 1110"
  (equal? (format "~x" #x1ce) "1CE")
  (equal? (format "#o~:o" #o1234567076543210) "#o1,234,567,076,543,210")
  (equal? (format "~36r" 35) "Z")
  (equal? (format "~36,10r and ~26r" #36rzeus #26rapollo)
          "      ZEUS and APOLLO")
  (equal? (format "~,10r" -1234567) "  -1234567")
  (equal? (format "~3,20,'*,'|,2:@r" #3r20202020) "********+20|20|20|20")
  (equal? (format "~10d" '(a 10 c)) "  (a 10 c)")
  (equal? (format "~10x" '(10 11 12)) "   (A B C)")
  (equal? (format "~36,10,'*r" '(10 20 30)) "***(A K U)")
)

(mat format-roman
  (equal? (format "~@r ~@r ~@r" 1999 -1999 4000) "MCMXCIX -1999 4000")
  (equal? (format "~@r ~@r ~@r" 3999 3998 347) "MMMCMXCIX MMMCMXCVIII CCCXLVII")
  (equal? (format "~@r" 2599) "MMDXCIX")
  (equal? (format "~@r" 4736) "4736")
  (equal? (format "~@r" 1782) "MDCCLXXXII")
  (equal? (format "~@r" 2251) "MMCCLI")
  (equal? (format "~@r" 1009) "MIX")
  (equal? (format "~@r" 544) "DXLIV")
  (equal? (format "~@r" 7) "VII")
  (equal? (format "~@r" 5) "V")
)

(mat format-old-roman
  (equal? (format "~@:r ~@:r ~@:r" 1999 -1999 5000)
          "MDCCCCLXXXXVIIII -1999 5000")
  (equal? (format "~@:r ~@:r ~@:r" 4999 4998 347)
          "MMMMDCCCCLXXXXVIIII MMMMDCCCCLXXXXVIII CCCXXXXVII")
  (equal? (format "~@:r" 2599) "MMDLXXXXVIIII")
  (equal? (format "~@:r" 4736) "MMMMDCCXXXVI")
  (equal? (format "~@:r" 1782) "MDCCLXXXII")
  (equal? (format "~@:r" 2251) "MMCCLI")
  (equal? (format "~@:r" 1009) "MVIIII")
  (equal? (format "~@:r" 544) "DXXXXIIII")
  (equal? (format "~@:r" 7) "VII")
  (equal? (format "~@:r" 5) "V")
)

(mat format-cardinal
  (equal? (format "~r" 1000000000) "1,000,000,000")
  (equal? (format "~r" 1000000001) "1,000,000,001")
  (equal? (format "~r" -2) "minus two")
  (equal? (format "~r" -1023) "minus one thousand twenty-three")
  (equal? (format "~r" 999999999) "nine hundred ninety-nine million nine hundred ninety-nine thousand nine hundred ninety-nine")
  (equal? (format "~r" -954321098) "minus nine hundred fifty-four million three hundred twenty-one thousand ninety-eight")
  (equal? (format "~r" 2599) "two thousand five hundred ninety-nine")
  (equal? (format "~r" 4736) "four thousand seven hundred thirty-six")
  (equal? (format "~r" -4730) "minus four thousand seven hundred thirty")
  (equal? (format "~r" -4719) "minus four thousand seven hundred nineteen")
  (equal? (format "~r" 1782) "seventeen hundred eighty-two")
  (equal? (format "~r" 2251) "two thousand two hundred fifty-one")
  (equal? (format "~r" 1009) "one thousand nine")
  (equal? (format "~r" 544) "five hundred forty-four")
  (equal? (format "~r ~r ~r ~r ~r ~r ~r ~r ~r ~r" 0 1 2 3 4 5 6 7 8 9)
          "zero one two three four five six seven eight nine")
  (equal? (format "~r ~r ~r ~r ~r ~r ~r ~r ~r ~r" 10 11 12 13 14 15 16 17 18 19)
          "ten eleven twelve thirteen fourteen fifteen sixteen seventeen eighteen nineteen")
)

(mat format-ordinal
  (equal? (format "~:r" 1000000000) "1,000,000,000th")
  (equal? (format "~:r" -1000000001) "-1,000,000,001st")
  (equal? (format "~:r" -1000000002) "-1,000,000,002nd")
  (equal? (format "~:r"   1000000003)   "1,000,000,003rd")
  (equal? (format "~:r" 300000000004) "300,000,000,004th")
  (equal? (format "~:r" 700000000008) "700,000,000,008th")
  (equal? (format "~:r" 800000000010) "800,000,000,010th")
  (equal? (format "~:r" 800000000011) "800,000,000,011th")
  (equal? (format "~:r" 800000000012) "800,000,000,012th")
  (equal? (format "~:r" 800000000013) "800,000,000,013th")
  (equal? (format "~:r" 800000000019) "800,000,000,019th")
  (equal? (format "~:r" 800000000021) "800,000,000,021st")
  (equal? (format "~:r" 800000000073) "800,000,000,073rd")
  (equal? (format "~:r" -2) "minus second")
  (equal? (format "~:r" -1023) "minus one thousand twenty-third")
  (equal? (format "~:r" 999999999) "nine hundred ninety-nine million nine hundred ninety-nine thousand nine hundred ninety-ninth")
  (equal? (format "~:r" 999000000) "nine hundred ninety-nine millionth")
  (equal? (format "~:r" -999000000) "minus nine hundred ninety-nine millionth")
  (equal? (format "~:r" 912304000) "nine hundred twelve million three hundred four thousandth")
  (equal? (format "~:r" 912004000) "nine hundred twelve million four thousandth")
  (equal? (format "~:r" -312001900) "minus three hundred twelve million nineteen hundredth")
  (equal? (format "~:r" 2599) "two thousand five hundred ninety-ninth")
  (equal? (format "~:r" 4736) "four thousand seven hundred thirty-sixth")
  (equal? (format "~:r" -4730) "minus four thousand seven hundred thirtieth")
  (equal? (format "~:r" -4716) "minus four thousand seven hundred sixteenth")
  (equal? (format "~:r" 1782) "seventeen hundred eighty-second")
  (equal? (format "~:r" 2251) "two thousand two hundred fifty-first")
  (equal? (format "~:r" 1009) "one thousand ninth")
  (equal? (format "~:r" 544) "five hundred forty-fourth")
  (equal? (format "~:r ~:r ~:r ~:r ~:r ~:r ~:r ~:r ~:r ~:r" 0 1 2 3 4 5 6 7 8 9)
          "zeroth first second third fourth fifth sixth seventh eighth ninth")
  (equal? (format "~:r ~:r ~:r ~:r ~:r ~:r ~:r ~:r ~:r ~:r" 10 11 12 13 14 15 16 17 18 19)
          "tenth eleventh twelfth thirteenth fourteenth fifteenth sixteenth seventeenth eighteenth nineteenth")
)

(mat format-fixed1
  (equal? (format "~8,3f" 0.0) "   0.000")
  (equal? (format "~8,3f" -0.0) "  -0.000")
  (equal? (format "~8,3f" 1234567.89) "1234567.890")
  (equal? (format "~8,3f" 123456.789) "123456.789")
  (equal? (format "~8,3f" 1.23456) "   1.235")
  (equal? (format "~8,3f" 1.23456789) "   1.235")
  (equal? (format "~8,3f" 321.23456789) " 321.235")
  (equal? (format "~8,3f" 4321.23456789) "4321.235")
  (equal? (format "~8,3,-2,,'zf" 0.0) "zzz0.000")
  (equal? (format "~8,3,-2,,'zf" -0.0) "zz-0.000")
  (equal? (format "~8,3,-2,,'0f" 4321.23456789) "0043.212")
  (equal? (format "~8,3f" 54321.23456789) "54321.235")
  (equal? (format "~8,3f" -1.23456789) "  -1.235")
  (equal? (format "~8,3@f" 0.0) "  +0.000")
  (equal? (format "~8,3@f" -0.0) "  -0.000")
  (equal? (format "~8,3@f" 1.23456789) "  +1.235")
  (equal? (format "~8,3f" .0023456789) "   0.002")
  (equal? (format "~8,3f" .002) "   0.002")
  (equal? (format "~8,3@f" 123456789) "+123456789.000")
  (equal? (format "~8,3@f" 123456789123456789) "+12345678912345678#.###")
  (equal? (format "~8,3f" 12345678912345678) "12345678912345678.###")
  (equal? (format "~8,3f" 1234567891234567) "1234567891234567.0##")
  (equal? (format "~8,3f" 12345678912345) "12345678912345.000")
  (equal? (format "~8,3f" 1e23) "9999999999999999#######.###")
  (equal? (format "~8,3,23f" 0.0) "   0.000")
  (equal? (format "~8,3,23f" -0.0) "  -0.000")
  (equal? (format "~8,3,23f" 1.0) "10000000000000000#######.###")
  (equal? (format "~8,3f" 1e-23) "   0.000")
  (equal? (format "~8,3,-23f" 0.0) "   0.000")
  (equal? (format "~8,3,-23f" -0.0) "  -0.000")
  (equal? (format "~8,3,-23f" 1.0) "   0.000")
  (equal? (format "~8,3f" 1e-7) "   0.000")
  (equal? (format "~8,3f" 9e-7) "   0.000")
  (equal? (format "~8,3f" 1e-6) "   0.000")
  (equal? (format "~8,3f" 1e-5) "   0.000")
  (equal? (format "~8,3f" 1e-4) "   0.000")
  (equal? (format "~8,3f" 1e-3) "   0.001")
  (equal? (format "~8,3f" 1e-2) "   0.010")
  (equal? (format "~8,3f" 1e-1) "   0.100")
)

(mat format-fixed2
  (equal? (format "~10,3,2f" 3.14159)   "   314.159")
  (equal? (format "~10,3,-1f" 3.14159)  "     0.314")
  (equal? (format "~6,3,-1f" 3.14159)   " 0.314")
  (equal? (format "~5,3,-1f" 3.14159)   "0.314")
  (equal? (format "~4,3,-1f" 3.14159)   ".314")
  (equal? (format "~3,3,-1f" 3.14159)   ".314")
  (equal? (format "~10,3,2f" -3.14159)  "  -314.159")
  (equal? (format "~10,3,-1f" -3.14159) "    -0.314")
  (equal? (format "~6,3,-1f" -3.14159)  "-0.314")
  (equal? (format "~5,3,-1f" -3.14159)  "-.314")
  (equal? (format "~3,3,-1f" -3.14159)  "-.314")
  (equal? (format "~6,3,-1@f" 3.14159)  "+0.314")
  (equal? (format "~5,3,-1@f" 3.14159)  "+.314")
  (equal? (format "~,3,-1@f" 3.14159)   "+0.314")
  (equal? (format "~,3,-8f" 3.14159)    "0.000")
  (equal? (format "~4,3,-8f" 3.14159)   ".000")
)

(mat format-fixed3
  (equal? (format "~10,,2f" 3.14159)  "   314.159")
  (equal? (format "~10,,-1f" 3.14159) "  0.314159")
  (equal? (format "~8,,-1f" 3.14159)  "0.314159")
  (equal? (format "~7,,-1f" 3.14159)  ".314159")
  (equal? (format "~6,,-1f" 3.14159)  ".31416")
  (equal? (format "~5,,-1f" 3.14159)  ".3142")
  (equal? (format "~4,,-1f" 3.14159)  ".314")
  (equal? (format "~3,,-1f" 3.14159)  ".31")
  (equal? (format "~2,,-1f" 3.14159)  ".3")
  (equal? (format "~1f" .314159)      ".3")
  (equal? (format "~1,,-1f" 3.14159)  ".3")
  (equal? (format "~0,,-1f" 3.14159)  ".3")

  (equal? (format "~0,,5f" 3.14159)  "314159.")

  (equal? (format "~5f" -231.2) "-231.")
  (equal? (format "~5f" 231.2)  "231.2")
  (equal? (format "~5f" -23.12) "-23.1")
  (equal? (format "~5f" 23.12)  "23.12")
  (equal? (format "~5f" -23.1)  "-23.1")
  (equal? (format "~5f" -231)   "-231.")
  (equal? (format "~5f" -2.31)  "-2.31")
  (equal? (format "~5f" -2310)  "-2310.")
  (equal? (format "~5f" 1e23)   "9999999999999999#######.")

  (equal? (format "~5,,1f" -231.2) "-2312.")
  (equal? (format "~5,,1f" 231.2)  "2312.")
  (equal? (format "~5,,1f" -23.12) "-231.")
  (equal? (format "~5,,1f" 23.12)  "231.2")
  (equal? (format "~5,,1f" -23.1)  "-231.")
  (equal? (format "~5,,1f" -231)   "-2310.")
  (equal? (format "~5,,1f" -2.31)  "-23.1")
  (equal? (format "~5,,1f" -2310)  "-23100.")
  (equal? (format "~5,,1f" 1e23)   "9999999999999999########.")

  (equal? (format "~5,,-1f" -231.2) "-23.1")
  (equal? (format "~5,,-1f" 231.2)  "23.12")
  (equal? (format "~5,,-1f" -23.12) "-2.31")
  (equal? (format "~5,,-1f" 23.12)  "2.312")
  (equal? (format "~5,,-1f" -23.1)  "-2.31")
  (equal? (format "~5,,-1f" -231)   "-23.1")
  (equal? (format "~5,,-1f" -2.31)  "-.231")
  (equal? (format "~5,,-1f" -2310)  "-231.")
  (equal? (format "~5,,-1f" 1e23)   "9999999999999999######.")

  (equal? (format "~,,1f" 3.14159) "31.4159")
  (equal? (format "~,,5f" 3.14159) "314159.0")
  (equal? (format "~,,10f" 3.14159) "31415900000.0")
  (equal? (format "~,,-1f" 3.14159) "0.314159")
  (equal? (format "~,,-2f" 3.14159) "0.0314159")
)

(mat format-fixed4
  (equal? (format "~8f" 0.0) "     0.0")
  (equal? (format "~8f" -0.0) "    -0.0")
  (equal? (format "~8@f" 0.0) "    +0.0")
  (equal? (format "~8@f" -0.0) "    -0.0")
  (equal? (format "~8f" 1234567.89) "1234568.")
  (equal? (format "~8f" 123456.789) "123456.8")
  (equal? (format "~8f" 1.23456) " 1.23456")
  (equal? (format "~8f" 1.23456789) "1.234568")
  (equal? (format "~8f" 321.23456789) "321.2346")
  (equal? (format "~8f" 4321.23456789) "4321.235")
  (equal? (format "~8f" 54321.23456789) "54321.23")
  (equal? (format "~8f" -1.23456789) "-1.23457")
  (equal? (format "~8@f" 1.23456789) "+1.23457")
  (equal? (format "~8f" .0023456789) ".0023457")
  (equal? (format "~8f" .002) "   0.002")
  (equal? (format "~8@f" 123456789) "+123456789.")
  (equal? (format "~8@f" 123456789123456789) "+12345678912345678#.")
  (equal? (format "~8f" 12345678912345678) "12345678912345678.")
  (equal? (format "~8f" 1234567891234567) "1234567891234567.")
  (equal? (format "~8f" 12345678912345) "12345678912345.")
  (equal? (format "~8f" 1e23) "9999999999999999#######.")
  (equal? (format "~8f" 1e-23) "     0.0")
  (equal? (format "~8f" 1e-8)  "     0.0")
  (equal? (format "~8f" 1e-7)  ".0000001")
  (equal? (format "~8f" 9e-7)  ".0000009")
  (equal? (format "~8f" 1e-6)  "0.000001")
  (equal? (format "~7f" 9e-7)  ".000001")
  (equal? (format "~7f" 1e-6)  ".000001")
  (equal? (format "~6f" 9e-9)  "   0.0")
  (equal? (format "~6f" 9e-7)  "   0.0")
  (equal? (format "~6f" 1e-6)  "   0.0")
  (equal? (format "~6f" 1e-5)  ".00001")

  (equal? (format "~6f" 1.0)      "   1.0")
  (equal? (format "~6f" 10.0)     "  10.0")
  (equal? (format "~6f" 100.0)    " 100.0")
  (equal? (format "~6f" 1e3)      "1000.0")
  (equal? (format "~6f" 123.0)    " 123.0")
  (equal? (format "~6f" 1234.0)   "1234.0")
  (equal? (format "~6f" 12345.0)  "12345.")
  (equal? (format "~6f" 123456.0) "123456.")
  (equal? (format "~6f" 10000.0)  "10000.")
  (equal? (format "~6,,1f" 1e3)   "10000.")
  (equal? (format "~5,,1f" 1e3)   "10000.")
  (equal? (format "~7f" -1.0)      "   -1.0")
  (equal? (format "~7f" -10.0)     "  -10.0")
  (equal? (format "~7f" -100.0)    " -100.0")
  (equal? (format "~7f" -1e3)      "-1000.0")
  (equal? (format "~7f" -123.0)    " -123.0")
  (equal? (format "~7f" -1234.0)   "-1234.0")
  (equal? (format "~7f" -12345.0)  "-12345.")
  (equal? (format "~7f" -123456.0) "-123456.")
  (equal? (format "~7f" -10000.0)  "-10000.")
  (equal? (format "~7,,1f" -1e3)   "-10000.")
  (equal? (format "~6,,1f" -1e3)   "-10000.")
)

(mat format-fixed5
  (equal? (format "~f" 0.0) "0.0")
  (equal? (format "~f" -0.0) "-0.0")
  (equal? (format "~@f" 0.0) "+0.0")
  (equal? (format "~@f" -0.0) "-0.0")
  (equal? (format "~f" 1234567.89) "1234567.89")
  (equal? (format "~f" 123456.789) "123456.789")
  (equal? (format "~f" 1.23456) "1.23456")
  (equal? (format "~f" 1.23456789) "1.23456789")
  (equal? (format "~f" 321.23456789) "321.23456789")
  (equal? (format "~f" 4321.23456789) "4321.23456789")
  (equal? (format "~f" 54321.23456789) "54321.23456789")
  (equal? (format "~f" -1.23456789) "-1.23456789")
  (equal? (format "~@f" 1.23456789) "+1.23456789")
  (equal? (format "~f" .0023456789) "0.0023456789")
  (equal? (format "~f" .002) "0.002")
  (equal? (format "~@f" 123456789) "+123456789.0")
  (equal? (format "~@f" 123456789123456789) "+12345678912345678#.#")
  (equal? (format "~f" 12345678912345678) "12345678912345678.#")
  (equal? (format "~f" 1234567891234567) "1234567891234567.0")
  (equal? (format "~f" 12345678912345) "12345678912345.0")
  (equal? (format "~f" 1e23) "9999999999999999#######.#")
  (equal? (format "~f" 1e-23) "0.00000000000000000000001")
  (equal? (format "~f" 1e-7) "0.0000001")
  (equal? (format "~f" 9e-7) "0.0000009")
  (equal? (format "~f" 1e-6) "0.000001")
)

(mat format-fixed6
  (equal? (format "~2,1@f" 0.003) "+.0")
  (equal? (format "~2@f" 0.003) "+.0")
  (equal? (format "~2@f" 1.34) "+1.")
  (equal? (format "~2,1@f" 1.34) "+1.3")
  (equal? (format "~2,2@f" 1.34) "+1.34")
  (equal? (format "~0f" 1e23) "9999999999999999#######.")
  (equal? (format "~0f" 1e-23) ".0")
  (equal? (format "~0f" -1e-23) "-.0")
  (equal? (format "~0f" 0.0) "0.")
  (equal? (format "~0f" -0.0) "-0.")
)

(mat format-fixed7
  (equal? (format "~2,1,,'*@f" 0.0) "**")
  (equal? (format "~2,1,,'*@f" -0.0) "**")
  (equal? (format "~2,1,,'*@f" 0.003) "**")
  (equal? (format "~2,,,'*,@f" 0.003) "**")
  (equal? (format "~4,2,,'*@f" 1.34) "****")
  (equal? (format "~2,1,4,'q,'p@f" 0.0) "qq")
  (equal? (format "~2,1,4,'q,'p@f" -0.0) "qq")
  (equal? (format "~2,1,4,'q,'p@f" 1.34) "qq")
  (equal? (format "~10,1,4,'q,'p@f" 0.0) "pppppp+0.0")
  (equal? (format "~10,1,4,'q,'p@f" -0.0) "pppppp-0.0")
  (equal? (format "~10,1,4,'q,'p@f" 1.34) "pp+13400.0")
  (equal? (format "~10,1,-4,'q,'p@f" 1.34) "pppppp+0.0")
  (equal? (format "~10,5,-4,'q,'p@f" 1.34) "pp+0.00013")
  (equal? (format "~10,6,-4,'q,'p@f" 1.34) "p+0.000134")
  (equal? (format "~10,6,-4,'q,'p@f" 0.0) "p+0.000000")
  (equal? (format "~10,6,-4,'q,'p@f" -0.0) "p-0.000000")
  (equal? (format "~2,2,,'q,'p@f" 1.34) "qq")
  (equal? (format "~10,2,,'q,'p@f" 1.34) "ppppp+1.34")
  (equal? (format "~0,,,'*f" 1e23) "")
  (equal? (format "~0,,,'*f" 1e-23) "")
  (equal? (format "~0,,,'*f" -1e-23) "")
)

(mat format-fixed8
  (let () ; example adapted from cltl2
    (define (foo x)
      (format "~6,2F|~6,2,1,'*F|~6,2,,'?F|~6F|~,2F|~F"
        x x x x x x))
    (and (equal? (foo 3.14159)  "  3.14| 31.42|  3.14|3.1416|3.14|3.14159")
         (equal? (foo -3.14159) " -3.14|-31.42| -3.14|-3.142|-3.14|-3.14159")
         (equal? (foo 100.0)    "100.00|******|100.00| 100.0|100.00|100.0")
         (equal? (foo 1234.0)   "1234.00|******|??????|1234.0|1234.00|1234.0")
         (equal? (foo 0.006)    "  0.01|  0.06|  0.01| 0.006|0.01|0.006")
         (equal? (foo 0.0)      "  0.00|  0.00|  0.00|   0.0|0.00|0.0")
         (equal? (foo -0.0)     " -0.00| -0.00| -0.00|  -0.0|-0.00|-0.0")))
)

(mat format-exp1
  (equal? (format "~10,5,,2e" 0.0)        " 0.0000e+0")
  (equal? (format "~10,5,,2e" -0.0)       "-0.0000e+0")
  (equal? (format "~9,5,,2e" 0.0)         "0.0000e+0")
  (equal? (format "~9,5,,2e" -0.0)        "-.0000e+0")
  (equal? (format "~10,5,,2e" 3.14159)    "31.4159e-1")
  (equal? (format "~10,0e" 3.4e-200)      "   3.e-200")
  (equal? (format "~10,3,2,0e" 123456789) " 0.123e+09")
  (equal? (format "~9,3,2,0e" 123456789)  "0.123e+09")
  (equal? (format "~8,3,2,0e" 0.0)        ".000e+00")
  (equal? (format "~8,3,2,0e" -0.0)       "-.000e+00")
  (equal? (format "~8,3,2,0e" 123456789)  ".123e+09")
  (equal? (format "~7,3,2,0e" 123456789)  ".123e+09")
  (equal? (format "~3,3,2,0e" 123456789)  ".123e+09")
)

(mat format-exp2
  (equal? (format "~10e" 0.0)      "    0.0e+0")
  (equal? (format "~10e" -0.0)     "   -0.0e+0")
  (equal? (format "~10e" 3.4)      "    3.4e+0")
  (equal? (format "~10e" 3.4e10)   "   3.4e+10")
  (equal? (format "~10e" 3.4e-10)  "   3.4e-10")
  (equal? (format "~10e" 3.4e-200) "  3.4e-200")

  (equal? (format "~10,,,2e" 0.0)       "    0.0e+0")
  (equal? (format "~10,,,2e" -0.0)      "   -0.0e+0")
  (equal? (format "~10,,,2e" 3.4e-200)  " 34.0e-201")
  (equal? (format "~10,,,3e" 3.4e-200)  "340.0e-202")
  (equal? (format "~10,,,-2e" 0.0)      "    0.0e+0")
  (equal? (format "~10,,,-2e" -0.0)     "   -0.0e+0")
  (equal? (format "~10,,,-2e" 3.4e-200) "0.003e-197")

  (equal? (format "~10@e" 3.6e99) "  +3.6e+99")
  (equal? (format "~9@e" 3.6e99)  " +3.6e+99")
  (equal? (format "~8@e" 3.6e99)  "+3.6e+99")
  (equal? (format "~7@e" 3.6e99)  "+4.e+99")
  (equal? (format "~6@e" 3.6e99)  "+4.e+99")
  (equal? (format "~5@e" 3.6e99)  "+4.e+99")
  (equal? (format "~0@e" 3.6e99)  "+4.e+99")

  (equal? (format "~9@e" 0.0)  "  +0.0e+0")
  (equal? (format "~7@e" 0.0)  "+0.0e+0")
  (equal? (format "~6@e" 0.0)  "+0.e+0")
  (equal? (format "~0@e" 0.0)  "+0.e+0")
  (equal? (format "~9@e" -0.0) "  -0.0e+0")
  (equal? (format "~7@e" -0.0) "-0.0e+0")
  (equal? (format "~6@e" -0.0) "-0.e+0")
  (equal? (format "~0@e" -0.0) "-0.e+0")

  (equal? (format "~9,,,0@e" 0.0)  "  +0.0e+0")
  (equal? (format "~7,,,0@e" 0.0)  "+0.0e+0")
  (equal? (format "~6,,,0@e" 0.0)  "+.0e+0")
  (equal? (format "~0,,,0@e" 0.0)  "+.0e+0")
  (equal? (format "~9,,,0@e" -0.0) "  -0.0e+0")
  (equal? (format "~7,,,0@e" -0.0) "-0.0e+0")
  (equal? (format "~6,,,0@e" -0.0) "-.0e+0")
  (equal? (format "~0,,,0@e" -0.0) "-.0e+0")

  (equal? (format "~10e" 9.999e9) "  9.999e+9")
  (equal? (format "~9e" 9.999e9)  " 9.999e+9")
  (equal? (format "~8e" 9.999e9)  "9.999e+9")
  (equal? (format "~7e" 9.999e9)  "1.0e+10")
  (equal? (format "~6e" 9.999e9)  "1.e+10")
  (equal? (format "~5e" 9.999e9)  "1.e+10")
  (equal? (format "~0e" 9.999e9)  "1.e+10")

  (equal? (format "~10e" 9.999e-10) " 9.999e-10")
  (equal? (format "~9e" 9.999e-10)  "9.999e-10")
  (equal? (format "~8e" 9.999e-10)  "  1.0e-9")
  (equal? (format "~7e" 9.999e-10)  " 1.0e-9")
  (equal? (format "~6e" 9.999e-10)  "1.0e-9")
  (equal? (format "~5e" 9.999e-10)  "1.e-9")
  (equal? (format "~2e" 9.999e-10)  "1.e-9")

  (equal? (format "~10e" 1e23)      "   1.0e+23")
)

(mat format-exp3
  (equal? (format "~e" 0.0) "0.0e+0")
  (equal? (format "~e" -0.0) "-0.0e+0")
  (equal? (format "~e" 1e23) "1.0e+23")
  (equal? (format "~e" .000345) "3.45e-4")
  (equal? (format "~e" 345) "3.45e+2")
  (equal? (format "~e" 345e20) "3.45e+22")
  (equal? (format "~,,3,e" 0.0) "0.0e+000")
  (equal? (format "~,,3,e" -0.0) "-0.0e+000")
  (equal? (format "~,,3,e" 1e23) "1.0e+023")
  (equal? (format "~,,3,3e" 0.0) "0.0e+000")
  (equal? (format "~,,3,3e" -0.0) "-0.0e+000")
  (equal? (format "~,,3,3e" 1e23) "100.0e+021")
  (equal? (format "~,,,3e" 0.0) "0.0e+0")
  (equal? (format "~,,,3e" -0.0) "-0.0e+0")
  (equal? (format "~,,,3e" 1e23) "100.0e+21")
  (equal? (format "~,,,3e" 3.14159) "314.159e-2")
  (equal? (format "~,,2,3e" 3.14159) "314.159e-02")
  (equal? (format "~,,2,3@e" 3.14159) "+314.159e-02")
  (equal? (format "~,,,-3e" 0.0) "0.0e+0")
  (equal? (format "~,,,-3e" -0.0) "-0.0e+0")
  (equal? (format "~,,,-3e" -3.14159) "-0.000314159e+4")
  (equal? (format "~0e" 0.0) "0.e+0")
  (equal? (format "~0e" -0.0) "-0.e+0")
  (equal? (format "~0,,,0e" 0.0) ".0e+0")
  (equal? (format "~0,,,0e" -0.0) "-.0e+0")
  (equal? (format "~,,1,,'*e" 3e20) "3.0e+20") ; can't fill with oc; no w
  (equal? (format "~,2,1,,'*e" 3e20) "3.00e+20") ; can't fill with oc; no w
  (equal? (format "~10,2,1,,'*e" 3e20) "**********") ; no room for exponent
  (equal? (format "~10,,1,,'*e" 3e20) "**********") ; no room for exponent
  (equal? (format "~10,2,,-2,'*e" 3e20) "**********") ; d to small for given k
  (equal? (format "~,2,,-2,'*e" 3e20) "0.003e+23") ; can't fill with oc; no w
)

(mat format-exp4
  (let () ; example adapted from cltl2
    (define (foo x)
      (format "~9,2,1,,'*E|~10,3,2,2,'?,,'$E|~9,3,2,-2,'%@E|~9,2E"
        x x x x))
    (and (equal? (foo 3.14159)  "  3.14e+0| 31.42$-01|+.003e+03|  3.14e+0")
         (equal? (foo -3.14159) " -3.14e+0|-31.42$-01|-.003e+03| -3.14e+0")
         (equal? (foo 1100.0)   "  1.10e+3| 11.00$+02|+.001e+06|  1.10e+3")
         (equal? (foo 1.1E13)   "*********| 11.00$+12|+.001e+16| 1.10e+13")
         (equal? (foo 1.1E120)  "*********|??????????|%%%%%%%%%|1.10e+120")
         (equal? (foo 1.1E1200) "   +inf.0|    +inf.0|   +inf.0|   +inf.0")   ; cltl2 assumes L (128-bit?) floats
         (equal? (foo 0.0)      "  0.00e+0|  0.00$+00|+.000e+00|  0.00e+0")
         (equal? (foo -0.0)     " -0.00e+0| -0.00$+00|-.000e+00| -0.00e+0")))
  (let () ; like above but without d parameters
    (define (foo x)
      (format "~9,,1,,'*E|~10,,2,2,'?,,'$E|~9,,2,-2,'%@E|~9E"
        x x x x))
    (and (equal? (foo 3.14159)  "3.1416e+0|31.416$-01|+.003e+03|3.1416e+0")
         (equal? (foo -3.14159) "-3.142e+0|-31.42$-01|-.003e+03|-3.142e+0")
         (equal? (foo 1100.0)   "   1.1e+3|  11.0$+02|+.001e+06|   1.1e+3")
         (equal? (foo 1.1E13)   "*********|  11.0$+12|+.001e+16|  1.1e+13")
         (equal? (foo 1.1E120)  "*********|??????????|%%%%%%%%%| 1.1e+120")
         (equal? (foo 1.1E1200) "   +inf.0|    +inf.0|   +inf.0|   +inf.0")   ; cltl2 assumes L (128-bit?) floats
         (equal? (foo 0.0)      "   0.0e+0|   0.0$+00| +0.0e+00|   0.0e+0")
         (equal? (foo -0.0)     "  -0.0e+0|  -0.0$+00| -0.0e+00|  -0.0e+0")))
  (equal? ; example adapted from cltl2
    (with-output-to-string
      (lambda ()
        (do ([k -5 (fx+ k 1)])
            ((fx= k 8))
          (printf
            (format "Scale factor ~2d: |~~13,6,2,~de|~~%" k k)
            3.14159))))
    (format "Scale factor -5: | 0.000003e+06|~@
             Scale factor -4: | 0.000031e+05|~@
             Scale factor -3: | 0.000314e+04|~@
             Scale factor -2: | 0.003142e+03|~@
             Scale factor -1: | 0.031416e+02|~@
             Scale factor  0: | 0.314159e+01|~@
             Scale factor  1: | 3.141590e+00|~@
             Scale factor  2: | 31.41590e-01|~@
             Scale factor  3: | 314.1590e-02|~@
             Scale factor  4: | 3141.590e-03|~@
             Scale factor  5: | 31415.90e-04|~@
             Scale factor  6: | 314159.0e-05|~@
             Scale factor  7: | 3141590.e-06|\n"))
)

(mat format-general
  (equal? (format "~g" 0.0) "0.")
  (equal? (format "~g" -0.0) "-0.")
  (equal? (format "~10g" 0.0) "    0.    ")
  (equal? (format "~10g" -0.0) "   -0.    ")
  (let () ; example adapted from cltl2
    (define (foo x)
      (format "~9,2,1,,'*G|~9,3,2,3,'?,,'$G|~9,3,2,0,'%G|~9,2G"
          x x x x))
    (and (equal? (foo 0.0314159) "  3.14e-2|314.2$-04|0.314e-01|  3.14e-2")
         (equal? (foo 0.314159)  "  0.31   |0.314    |0.314    | 0.31    ")
         (equal? (foo 3.14159)   "   3.1   | 3.14    | 3.14    |  3.1    ")
         (equal? (foo 31.4159)   "   31.   | 31.4    | 31.4    |  31.    ")
         (equal? (foo 314.159)   "  3.14e+2| 314.    | 314.    |  3.14e+2")
         (equal? (foo 3141.59)   "  3.14e+3|314.2$+01|0.314e+04|  3.14e+3")
         (equal? (foo 3141.59L0) "  3.14e+3|314.2$+01|0.314e+04|  3.14e+3")
         (equal? (foo 3.14E12)   "*********|314.0$+10|0.314e+13| 3.14e+12")
         (equal? (foo 3.14L120)  "*********|?????????|%%%%%%%%%|3.14e+120")
         (equal? (foo 3.14L1200) "   +inf.0|   +inf.0|   +inf.0|   +inf.0")))
  (equal?
    (list (format "~,3g" .9999) (format "~,3g" .999) (format "~,3g" 1.0))
    '("1.00" "0.999" "1.00"))

)

(mat format-dollar
  (equal? (format "~$" 0.0) "0.00")
  (equal? (format "~$" -0.0) "-0.00")
  (equal? (format "~$" 3.4) "3.40")
  (equal? (format "~$" 23.99) "23.99")
  (equal? (format "~$" -12345.67830) "-12345.68")
  (equal? (format "~$" .153) "0.15")
  (equal? (format "~$" -.01) "-0.01")
  (equal? (format "~$" .0159) "0.02")
  (equal? (format "~3$" 0.0) "0.000")
  (equal? (format "~3$" -0.0) "-0.000")
  (equal? (format "~3$" 3.4) "3.400")
  (equal? (format "~3$" 23.99) "23.990")
  (equal? (format "~3$" -12345.67830) "-12345.678")
  (equal? (format "~3$" .153) "0.153")
  (equal? (format "~3$" -.01) "-0.010")
  (equal? (format "~3$" .0159) "0.016")
  (equal? (format "~1,2$" 0.0) "00.0")
  (equal? (format "~1,2$" -0.0) "-00.0")
  (equal? (format "~1,2$" 3.4) "03.4")
  (equal? (format "~1,2$" 23.99) "24.0")
  (equal? (format "~1,2$" 12345.678) "12345.7")
  (equal? (format "~1,2$" .153) "00.2")
  (equal? (format "~1,2$" -.01) "-00.0")
  (equal? (format "~1,2$" .0159) "00.0")
  (equal? (format "~1,2$" .0159) "00.0")
  (equal? (format "~1,2,7$" 0.0) "   00.0")
  (equal? (format "~1,2,7$" -0.0) "  -00.0")
  (equal? (format "~1,2,7$" 3.4) "   03.4")
  (equal? (format "~1,2,7$" 23.99) "   24.0")
  (equal? (format "~1,2,7$" -12345.678) "-12345.7")
  (equal? (format "~1,2,7$" .153) "   00.2")
  (equal? (format "~1,2,7$" -.01) "  -00.0")
  (equal? (format "~1,2,7$" .0159) "   00.0")
  (equal? (format "~1,2,7$" .0159) "   00.0")
  (equal? (format "~1,2,7,'0$" 0.0) "00000.0")
  (equal? (format "~1,2,7,'0$" -0.0) "00-00.0")
  (equal? (format "~1,2,7,'0$" 3.4) "00003.4")
  (equal? (format "~1,2,7,'0$" 23.99) "00024.0")
  (equal? (format "~1,2,7,'0$" -12345.678) "-12345.7")
  (equal? (format "~1,2,7,'0$" .153) "00000.2")
  (equal? (format "~1,2,7,'0$" -.01) "00-00.0")
  (equal? (format "~1,2,7,'0$" .0159) "00000.0")
  (equal? (format "~1,2,7,'0$" .0159) "00000.0")
  (equal? (format "~1,2,7,'0:$" 0.0) "00000.0")
  (equal? (format "~1,2,7,'0:$" -0.0) "-0000.0")
  (equal? (format "~1,2,7,'0:$" 3.4) "00003.4")
  (equal? (format "~1,2,7,'0:$" 23.99) "00024.0")
  (equal? (format "~1,2,7,'0:$" -12345.678) "-12345.7")
  (equal? (format "~1,2,7,'0:$" .153) "00000.2")
  (equal? (format "~1,2,7,'0:$" -.01) "-0000.0")
  (equal? (format "~1,2,7,'0:$" .0159) "00000.0")
  (equal? (format "~1,2,7,'0:$" .0159) "00000.0")
  (equal? (format "~1,2,7,'0@:$" 0.0) "+0000.0")
  (equal? (format "~1,2,7,'0@:$" -0.0) "-0000.0")
  (equal? (format "~1,2,7,'0@:$" 3.4) "+0003.4")
  (equal? (format "~1,2,7,'0@:$" 23.99) "+0024.0")
  (equal? (format "~1,2,7,'0@:$" -12345.678) "-12345.7")
  (equal? (format "~1,2,7,'0@:$" .153) "+0000.2")
  (equal? (format "~1,2,7,'0@:$" -.01) "-0000.0")
  (equal? (format "~1,2,7,'0@:$" .0159) "+0000.0")
  (equal? (format "~1,2,7,'0@:$" .0159) "+0000.0")
  (equal? (format "~1,,7,'*@:$" 0.0) "+***0.0")
  (equal? (format "~1,,7,'*@:$" -0.0) "-***0.0")
  (equal? (format "~1,,7,'*@:$" 3.4) "+***3.4")
  (equal? (format "~,2,7,'*@$" 23.99) "*+23.99")
  (equal? (format "~1,,7,@$" -12345.678) "-12345.7")
  (equal? (format "~1,2,,'*@:$" .153) "+00.2")
  (equal? (format "~,,10,'*$" -.01) "*****-0.01")
  (equal? (format "~1,,,'*@:$" .0159) "+0.0")
  (equal? (format "~,2,7,'*@:$" 0.0) "+*00.00")
  (equal? (format "~,2,7,'*@:$" -0.0) "-*00.00")
  (equal? (format "~,2,7,'*@:$" .0159) "+*00.02")

 ; check to see if exact inputs are accepted
  (equal? (format "~1,2,7,'0@:$" #e23.99) "+0024.0")
  (equal? (format "~$" #e3.4) "3.40")
  (equal? (format "~$" 1/3) "0.33")

 ; check to see if nonreal inputs are rejected
  (error? (format "~$" 'a))
  (error? (format "~$" 1.0+3.0i))
  (error? (format "~$" 1+3i))
)

(mat format-cltl2 ; misc cltl2 tests
  (equal? (format "foo") "foo")
  (begin (define fmt-x 5) #t)
  (equal? (format "The answer is ~D." fmt-x) "The answer is 5.")
  (equal? (format "The answer is ~3D." fmt-x) "The answer is   5.")
  (equal? (format "The answer is ~3,'0D." fmt-x) "The answer is 005.")
  (equal? (format "The answer is ~:D." (expt 47 fmt-x))
          "The answer is 229,345,007.")
  (begin (define fmt-y "elephant") #t)
  (equal? (format "Look at the ~A!" fmt-y) "Look at the elephant!")
  (equal? (format "Type ~:C to ~A." #\004 "delete all your files")
          "Type ^D to delete all your files.")
  (begin (define fmt-n 3) #t)
  (equal? (format "~D item~:P found." fmt-n) "3 items found.")
  (equal? (format "~R dog~:[s are~; is~] here." fmt-n (= fmt-n 1))
          "three dogs are here.")
  (equal? (format "~R dog~:*~[s are~; is~:;s are~] here." fmt-n)
          "three dogs are here.")
  (equal? (format "Here ~[are~;is~:;are~] ~:*~R pupp~:@P." fmt-n)
          "Here are three puppies.")
  (begin (define fmt-n 1) #t)
  (equal? (format "~D item~:P found." fmt-n) "1 item found.")
  (equal? (format "~R dog~:[s are~; is~] here." fmt-n (= fmt-n 1))
          "one dog is here.")
  (equal? (format "~R dog~:*~[s are~; is~:;s are~] here." fmt-n)
          "one dog is here.")
  (equal? (format "Here ~[are~;is~:;are~] ~:*~R pupp~:@P." fmt-n)
          "Here is one puppy.")
  (begin (define fmt-n 0) #t)
  (equal? (format "~D item~:P found." fmt-n) "0 items found.")
  (equal? (format "~R dog~:[s are~; is~] here." fmt-n (= fmt-n 1))
          "zero dogs are here.")
  (equal? (format "~R dog~:*~[s are~; is~:;s are~] here." fmt-n)
          "zero dogs are here.")
  (equal? (format "Here ~[are~;is~:;are~] ~:*~R pupp~:@P." fmt-n)
          "Here are zero puppies.")
)

; format-slib* tests are adapted from:

;; "formatst.scm" SLIB FORMAT Version 3.0 conformance test
; Written by Dirk Lutzebaeck (lutzeb@cs.tu-berlin.de)
;
; This code is in the public domain.

(mat format-slib1
  (equal? (format "abc") "abc")
  (equal? (format "~a" 10) "10")
  (equal? (format "~a" -1.2) "-1.2")
  (equal? (format "~a" 'a) "a")
  (equal? (format "~a" #t) "#t")
  (equal? (format "~a" #f) "#f")
  (equal? (format "~a" "abc") "abc")
  (equal? (format "~a" '#(1 2 3)) "#(1 2 3)")         ; ans was "#(1 2 3)"
  (equal? (format "~a" '()) "()")
  (equal? (format "~a" '(a)) "(a)")
  (equal? (format "~a" '(a b)) "(a b)")
  (equal? (format "~a" '(a (b c) d)) "(a (b c) d)")
  (equal? (format "~a" '(a . b)) "(a . b)")
)

(mat format-slib2
  (equal? (format "~a ~a" 10 20) "10 20")
  (equal? (format "~a abc ~a def" 10 20) "10 abc 20 def")
)

(mat format-slib3
  (equal? (format "~d" 100) "100")
  (equal? (format "~x" 100) "64")
  (equal? (format "~o" 100) "144")
  (equal? (format "~b" 100) "1100100")
  (equal? (format "~@d" 100) "+100")
  (equal? (format "~@d" -100) "-100")
  (equal? (format "~@x" 100) "+64")
  (equal? (format "~@o" 100) "+144")
  (equal? (format "~@b" 100) "+1100100")
  (equal? (format "~10d" 100) "       100")
  (equal? (format "~:d" 123) "123")
  (equal? (format "~:d" 1234) "1,234")
  (equal? (format "~:d" 12345) "12,345")
  (equal? (format "~:d" 123456) "123,456")
  (equal? (format "~:d" 12345678) "12,345,678")
  (equal? (format "~:d" -123) "-123")
  (equal? (format "~:d" -1234) "-1,234")
  (equal? (format "~:d" -12345) "-12,345")
  (equal? (format "~:d" -123456) "-123,456")
  (equal? (format "~:d" -12345678) "-12,345,678")
  (equal? (format "~10:d" 1234) "     1,234")
  (equal? (format "~10:d" -1234) "    -1,234")
  (equal? (format "~10,'*d" 100) "*******100")
  (equal? (format "~10,,'|:d" 12345678) "12|345|678")
  (equal? (format "~10,,,2:d" 12345678) "12,34,56,78")
  (equal? (format "~14,'*,'|,4:@d" 12345678) "****+1234|5678")
)

(mat format-slib4 ; ~r tests
  (equal? (format "~10r" 100) "100")
  (equal? (format "~2r" 100) "1100100")
  (equal? (format "~8r" 100) "144")
  (equal? (format "~16r" 100) "64")
  (equal? (format "~16,10,'*r" 100) "********64")

  (equal? (format "~@r" 4) "IV")
  (equal? (format "~@r" 19) "XIX")
  (equal? (format "~@r" 50) "L")
  (equal? (format "~@r" 100) "C")
  (equal? (format "~@r" 1000) "M")
  (equal? (format "~@r" 99) "XCIX")
  (equal? (format "~@r" 1994) "MCMXCIV")

  ; old roman numeral test
  (equal? (format "~:@r" 4) "IIII")
  (equal? (format "~:@r" 5) "V")
  (equal? (format "~:@r" 10) "X")
  (equal? (format "~:@r" 9) "VIIII")

  ; cardinal/ordinal English number test
  (equal? (format "~r" 4) "four")
  (equal? (format "~r" 10) "ten")
  (equal? (format "~r" 19) "nineteen")
  (equal? (format "~r" 1984) "nineteen hundred eighty-four") ; ans was "one thousand, nine hundred eighty-four")
  (equal? (format "~:r" -1984) "minus nineteen hundred eighty-fourth") ; ans was "minus one thousand, nine hundred eighty-fourth")
)

(mat format-slib5 ; character tests
  (equal? (format "~c" #\a) "a")
  (equal? (format "~@c" #\a) "#\\a")
  (equal? (format "~@c" (integer->char 32)) "#\\space")
  (equal? (format "~@c" (integer->char 0)) "#\\nul")
;  (equal? (format "~@c" (integer->char 27)) "#\\esc")
;  (equal? (format "~@c" (integer->char 127)) "#\\del")
;  (equal? (format "~@c" (integer->char 128)) "#\\200")
;  (equal? (format "~@c" (integer->char 255)) "#\\377")
;  (equal? (format "~65c") "A")
;  (equal? (format "~7@c") "#\\bel")
  (equal? (format "~:c" #\a) "a")
  (equal? (format "~:c" (integer->char 1)) "^A")
  (equal? (format "~:c" (integer->char 27)) "<esc>")
;  (equal? (format "~7:c") "^G")
;  (equal? (format "~:c" (integer->char 128)) "#\\200")
;  (equal? (format "~:c" (integer->char 127)) "#\\177")
;  (equal? (format "~:c" (integer->char 255)) "#\\377")
)

(mat format-slib6 ; plural test
  (equal? (format "test~p" 1) "test")
  (equal? (format "test~p" 2) "tests")
  (equal? (format "test~p" 0) "tests")
  (equal? (format "tr~@p" 1) "try")
  (equal? (format "tr~@p" 2) "tries")
  (equal? (format "tr~@p" 0) "tries")
  (equal? (format "~a test~:p" 10) "10 tests")
  (equal? (format "~a test~:p" 1) "1 test")
)

(mat format-slib-slib7 ; tilde tests
  (equal? (format "~~~~") "~~")
  (equal? (format "~3~") "~~~")
)

(mat format-slib8 ; whitespace character test
  (equal? (format "~%") "\n")
  (equal? (format "~3%") "\n\n\n")

  (equal? (format "~&") "")
  (equal? (format "abc~&") "abc\n")
  (equal? (format "abc~&def") "abc\ndef")
  (equal? (format "~3&") "\n\n")
  (equal? (format "abc~3&") "abc\n\n\n") ; ans was "abc\n\n\n"
; not yet  (equal? (format "~_~_~_") "   ")
; not yet  (equal? (format "~3_") "   ")
)

(mat format-slib9 ; tabulate test
 ; removed leading ~0& from control strings in following
  (equal? (format "~3t") "   ")
  (equal? (format "~10t") "          ") ; ans was ""
  (equal? (format "1234567890~,8tABC")  "1234567890       ABC")
  (equal? (format "1234567890~0,8tABC") "1234567890      ABC")
  (equal? (format "1234567890~1,8tABC") "1234567890       ABC")
  (equal? (format "1234567890~2,8tABC") "1234567890        ABC") ; ans was "1234567890ABC"
  (equal? (format "1234567890~3,8tABC") "1234567890 ABC")
  (equal? (format "1234567890~4,8tABC") "1234567890  ABC")
  (equal? (format "1234567890~5,8tABC") "1234567890   ABC")
  (equal? (format "1234567890~6,8tABC") "1234567890    ABC")
  (equal? (format "1234567890~7,8tABC") "1234567890     ABC")
  (equal? (format "1234567890~8,8tABC") "1234567890      ABC")
  (equal? (format "1234567890~9,8tABC") "1234567890       ABC")
  (equal? (format "1234567890~10,8tABC") "1234567890        ABC") ; ans was "1234567890ABC"
  (equal? (format "1234567890~11,8tABC") "1234567890 ABC")
  (equal? (format "12345~,8tABCDE~,8tXYZ") "12345    ABCDE   XYZ")
  (equal? (format "~,8t+++~,8t===") " +++     ===") ; ans was "     +++     ==="
;  (equal? (format "ABC~,8,'.tDEF") "ABC......DEF")
  (equal? (format "~3,8@tABC") "        ABC")
  (equal? (format "1234~3,8@tABC") "1234    ABC")
  (equal? (format "12~3,8@tABC~3,8@tDEF") "12      ABC     DEF")
)

(mat format-slib10 ; indirection test
  (equal? (format "~a ~? ~a" 10 "~a ~a" '(20 30) 40) "10 20 30 40")
  (equal? (format "~a ~@? ~a" 10 "~a ~a" 20 30 40) "10 20 30 40")
)

(mat format-slib11
  (equal? (format "~10a" "abc") "abc       ")
  (equal? (format "~10@a" "abc") "       abc")
  (equal? (format "~10a" "0123456789abc") "0123456789abc")
  (equal? (format "~10@a" "0123456789abc") "0123456789abc")
)

(mat format-slib12 ; pad character test
  (equal? (format "~10,,,'*a" "abc") "abc*******")
  (equal? (format "~10,,,'Xa" "abc") "abcXXXXXXX")
  ; bad test (equal? (format "~10,,,42a" "abc") "abc*******")
  (equal? (format "~10,,,'*@a" "abc") "*******abc")
  (equal? (format "~10,,3,'*a" "abc") "abc*******")
  (equal? (format "~10,,3,'*a" "0123456789abc") "0123456789abc***") ; min. padchar length
  (equal? (format "~10,,3,'*@a" "0123456789abc") "***0123456789abc")
)

(mat format-slib13 ; colinc, minpad padding test
  (equal? (format "~10,8,0,'*a" 123)  "123********")
  (equal? (format "~10,9,0,'*a" 123)  "123*********")
  (equal? (format "~10,10,0,'*a" 123) "123**********")
  (equal? (format "~10,11,0,'*a" 123) "123***********")
  (equal? (format "~8,1,0,'*a" 123) "123*****")
  (equal? (format "~8,2,0,'*a" 123) "123******")
  (equal? (format "~8,3,0,'*a" 123) "123******")
  (equal? (format "~8,4,0,'*a" 123) "123********")
  (equal? (format "~8,5,0,'*a" 123) "123*****")
  (equal? (format "~8,1,3,'*a" 123) "123*****")
  (equal? (format "~8,1,5,'*a" 123) "123*****")
  (equal? (format "~8,1,6,'*a" 123) "123******")
  (equal? (format "~8,1,9,'*a" 123) "123*********")
)

(mat format-slib14 ; slashify test
  (equal? (format "~s" "abc") "\"abc\"")
  (equal? (format "~s" "abc \\ abc") "\"abc \\\\ abc\"")
  (equal? (format "~a" "abc \\ abc") "abc \\ abc")
  (equal? (format "~s" "abc \" abc") "\"abc \\\" abc\"")
  (equal? (format "~a" "abc \" abc") "abc \" abc")
  (equal? (format "~s" #\space) "#\\space")
  (equal? (format "~s" #\newline) "#\\newline")
  (equal? (format "~s" #\tab) "#\\tab")                 ; ans was "#\\ht"
  (equal? (format "~s" #\a) "#\\a")
  (equal? (format "~a" '(a "b" c)) "(a b c)")            ; ans was "(a \"b\" c)"
)

(mat format-slib15 ; continuation line test
  (equal? (format "abc~\n    123") "abc123")
  (equal? (format "abc~\n   ") "abc")
  (equal? (format "abc~:\n   def") "abc   def")
  (equal? (format "abc~@\n   def") "abc\ndef")
)

(mat format-slib16 ; string case conversion
  (equal? (format "~a ~(~a~) ~a" "abc" "HELLO WORLD" "xyz") "abc hello world xyz")
  (equal? (format "~a ~:(~a~) ~a" "abc" "HELLO WORLD" "xyz") "abc Hello World xyz")
  (equal? (format "~a ~@(~a~) ~a" "abc" "HELLO WORLD" "xyz") "abc Hello world xyz")
  (equal? (format "~a ~:@(~a~) ~a" "abc" "hello world" "xyz") "abc HELLO WORLD xyz")
  (equal? (format "~:@(~a~)" '(a b c)) "(A B C)")
  (equal? (format "~:@(~x~)" 255) "FF")
  (equal? (format "~:@(~p~)" 2) "S")
;  (test `("~:@(~a~)" ,display) (string-upcase (format:iobj->str display)))
  (equal? (format "~:(~a ~a ~a~) ~a" "abc" "xyz" "123" "world") "Abc Xyz 123 world")
)

(mat format-slib17 ; variable parameter
  (equal? (format "~va" 10 "abc") "abc       ")
  (equal? (format "~Va" 10 "abc") "abc       ")
 ; changed 42 to #\* below
  (equal? (format "~v,,,va" 10 #\* "abc") "abc*******")
  (equal? (format "~V,,,Va" 10 #\* "abc") "abc*******")
)

(mat format-slib18 ; number of remaining arguments as parameter
  (equal? (format "~#,,,'*@a ~a ~a ~a" 1 1 1 1) "***1 1 1 1")
)

(mat format-slib19 ; argument jumping
  (equal? (format "~a ~* ~a" 10 20 30) "10  30")
  (equal? (format "~a ~2* ~a" 10 20 30 40) "10  40")
  (equal? (format "~a ~:* ~a" 10) "10  10")
  (equal? (format "~a ~a ~2:* ~a ~a" 10 20) "10 20  10 20")
  (equal? (format "~a ~a ~@* ~a ~a" 10 20) "10 20  10 20")
  (equal? (format "~a ~a ~4@* ~a ~a" 10 20 30 40 50 60) "10 20  50 60")
)

(mat format-slib20 ; conditionals
  (equal? (format "~[abc~;xyz~]" 0) "abc")
  (equal? (format "~[abc~;xyz~]" 1) "xyz")
  (equal? (format "~[abc~;xyz~:;456~]" 99) "456")
  (equal? (format "~0[abc~;xyz~:;456~]") "abc")
  (equal? (format "~1[abc~;xyz~:;456~] ~a" 100) "xyz 100")
  (equal? (format "~#[no arg~;~a~;~a and ~a~;~a, ~a and ~a~]") "no arg")
  (equal? (format "~#[no arg~;~a~;~a and ~a~;~a, ~a and ~a~]" 10) "10")
  (equal? (format "~#[no arg~;~a~;~a and ~a~;~a, ~a and ~a~]" 10 20)
          "10 and 20")
  (equal? (format "~#[no arg~;~a~;~a and ~a~;~a, ~a and ~a~]" 10 20 30)
          "10, 20 and 30")
  (equal? (format "~:[hello~;world~] ~a" #t 10) "world 10")
  (equal? (format "~:[hello~;world~] ~a" #f 10) "hello 10")
  (equal? (format "~@[~a tests~]" #f) "")
  (equal? (format "~@[~a tests~]" 10) "10 tests")
  (equal? (format "~@[~a test~:p~] ~a" 10 'done) "10 tests done")
  (equal? (format "~@[~a test~:p~] ~a" 1 'done) "1 test done")
  (equal? (format "~@[~a test~:p~] ~a" 0 'done) "0 tests done")
  (equal? (format "~@[~a test~:p~] ~a" #f 'done) " done")
  (equal? (format "~@[ level = ~d~]~@[ length = ~d~]" #f 5) " length = 5")
  (equal? (format "~[abc~;~[4~;5~;6~]~;xyz~]" 0) "abc")   ; nested conditionals (irrghh)
  (equal? (format "~[abc~;~[4~;5~;6~]~;xyz~]" 2) "xyz")
  (equal? (format "~[abc~;~[4~;5~;6~]~;xyz~]" 1 2) "6")

 ; cltl2 tests
  (begin
    (define foo "Items:~#[ none~; ~S~; ~S and ~S~
                 ~:;~@{~#[~; and~] ~S~^,~}~].")
    (string? foo))
  (equal? (format foo) "Items: none.")
  (equal? (format foo 'foo) "Items: foo.")
  (equal? (format foo 'foo 'bar) "Items: foo and bar.")
  (equal? (format foo 'foo 'bar 'baz) "Items: foo, bar, and baz.")
  (equal? (format foo 'foo 'bar 'baz 'quux) "Items: foo, bar, baz, and quux.")
)

(mat format-slib21 ; iteration
  (equal? (format "~{ ~a ~}" '(a b c)) " a  b  c ")
  (equal? (format "~{ ~a ~}" '()) "")
  (equal? (format "~{ ~a ~5,,,'*a~}" '(a b c d)) " a b**** c d****")
  (equal? (format "~{ ~a,~a ~}" '(a 1 b 2 c 3)) " a,1  b,2  c,3 ")
  (equal? (format "~2{ ~a,~a ~}" '(a 1 b 2 c 3)) " a,1  b,2 ")
  (equal? (format "~3{~a ~} ~a" '(a b c d e) 100) "a b c  100")
  (equal? (format "~0{~a ~} ~a" '(a b c d e) 100) " 100")
 ; replaced (c d e f) with (c d) below
  (equal? (format "~:{ ~a,~a ~}" '((a b) (c d) (g h))) " a,b  c,d  g,h ")
 ; replaced (c d e f) with (c d) below
  (equal? (format "~2:{ ~a,~a ~}" '((a b) (c d) (g h))) " a,b  c,d ")
  (equal? (format "~@{ ~a,~a ~}" 'a 1 'b 2 'c 3) " a,1  b,2  c,3 ")
  (equal? (format "~2@{ ~a,~a ~} <~a|~a>" 'a 1 'b 2 'c 3) " a,1  b,2  <c|3>")
  (equal? (format "~:@{ ~a,~a ~}" '(a 1) '(b 2) '(c 3)) " a,1  b,2  c,3 ")
  (equal? (format "~2:@{ ~a,~a ~} ~a" '(a 1) '(b 2) '(c 3)) " a,1  b,2  (c 3)")
  (equal? (format "~{~}" "<~a,~a>" '(a 1 b 2 c 3)) "<a,1><b,2><c,3>")
  (equal? (format "~{ ~a ~{<~a>~}~} ~a" '(a (1 2) b (3 4)) 10)
          " a <1><2> b <3><4> 10")
)

(mat format-slib22 ; up and out
  (equal? (format "abc ~^ xyz") "abc ")
  (equal? (format "~@(abc ~^ xyz~) ~a" 10) "Abc  xyz 10") ; ans was "ABC  xyz 10"
  (equal? (format "done. ~^ ~d warning~:p. ~^ ~d error~:p.") "done. ")
  (equal? (format "done. ~^ ~d warning~:p. ~^ ~d error~:p." 10)
          "done.  10 warnings. ")
  (equal? (format "done. ~^ ~d warning~:p. ~^ ~d error~:p." 10 1)
          "done.  10 warnings.  1 error.")
  (equal? (format "~{ ~a ~^<~a>~} ~a" '(a b c d e f) 10)
          " a <b> c <d> e <f> 10")
  (equal? (format "~{ ~a ~^<~a>~} ~a" '(a b c d e) 10)
          " a <b> c <d> e  10")
  (equal? (format "abc~0^ xyz") "abc")
  (equal? (format "abc~9^ xyz") "abc xyz")
  (equal? (format "abc~7,4^ xyz") "abc xyz")
  (equal? (format "abc~7,7^ xyz") "abc")
;  (equal? (format "abc~3,7,9^ xyz") "abc")
;  (equal? (format "abc~8,7,9^ xyz") "abc xyz")
;  (equal? (format "abc~3,7,5^ xyz") "abc xyz")
)

(mat format-slib23 ; complexity tests
  (begin
    (define fmt "Items:~#[ none~; ~a~; ~a and ~a~:;~@{~#[~; and~] ~a~^,~}~].")
    (string? fmt))
  (equal? (format fmt)  "Items: none.")
  (equal? (format fmt 'foo) "Items: foo.")
  (equal? (format fmt 'foo 'bar) "Items: foo and bar.")
  (equal? (format fmt 'foo 'bar 'baz) "Items: foo, bar, and baz.")
  (equal? (format fmt 'foo 'bar 'baz 'zok) "Items: foo, bar, baz, and zok.")
)

(mat format-slib24 ; fixed floating points
  (equal? (format "~6,2f" 3.14159) "  3.14")
  (equal? (format "~6,1f" 3.14159) "   3.1")
  (equal? (format "~6,0f" 3.14159) "    3.")
  (equal? (format "~5,1f" 0) "  0.0")
  (equal? (format "~10,7f" 3.14159) " 3.1415900")
  (equal? (format "~10,7f" -3.14159) "-3.1415900")
  (equal? (format "~10,7@f" 3.14159) "+3.1415900")
  (equal? (format "~6,3f" 0.0) " 0.000")
  (equal? (format "~6,4f" 0.007) "0.0070")
  (equal? (format "~6,3f" 0.007) " 0.007")
  (equal? (format "~6,2f" 0.007) "  0.01")
  (equal? (format "~3,2f" 0.007) ".01")
  (equal? (format "~3,2f" -0.007) "-.01")
  (equal? (format "~6,2,,,'*f" 3.14159) "**3.14")
  (equal? (format "~6,3,,'?f" 12345.56789) "??????")
  (equal? (format "~6,3f" 12345.6789) "12345.679")
  (equal? (format "~,3f" 12345.6789) "12345.679")
  (equal? (format "~,3f" 9.9999) "10.000")
  (equal? (format "~6f" 23.4) "  23.4")
  (equal? (format "~6f" 1234.5) "1234.5")
  (equal? (format "~6f" 12345678) "12345678.")         ; ans was "12345678.0"
  (equal? (format "~6,,,'?f" 12345678) "??????")
  (equal? (format "~6f" 123.56789) "123.57")
  (equal? (format "~6f" 123.0) " 123.0")
  (equal? (format "~6f" -123.0) "-123.0")
  (equal? (format "~6f" 0.0) "   0.0")
  (equal? (format "~3f" 3.141) "3.1")
  (equal? (format "~2f" 3.141) "3.")
  (equal? (format "~1f" 3.141) "3.")                   ; ans was "3.141"
  (equal? (format "~f" 123.56789) "123.56789")
  (equal? (format "~f" -314.0) "-314.0")
  (equal? (format "~f" 1e4) "10000.0")
  (equal? (format "~f" -1.23e10) "-12300000000.0")
  (equal? (format "~f" 1e-4) "0.0001")
  (equal? (format "~f" -1.23e-10) "-0.000000000123")
  (equal? (format "~@f" 314.0) "+314.0")
  (equal? (format "~,,3f" 0.123456) "123.456")
  (equal? (format "~,,-3f" -123.456) "-0.123456")
  (equal? (format "~5,,3f" 0.123456) "123.5")
)

(mat format-slib25 ; exponent floating points
  (equal? (format "~e" 3.14159) "3.14159e+0")
  (equal? (format "~e" 0.00001234) "1.234e-5")
  (equal? (format "~,,,0e" 0.00001234) "0.1234e-4")
  (equal? (format "~,3e" 3.14159) "3.142e+0")
  (equal? (format "~,3@e" 3.14159) "+3.142e+0")
  (equal? (format "~,3@e" 0.0) "+0.000e+0")
  (equal? (format "~,0e" 3.141) "3.e+0")
  (equal? (format "~,3,,0e" 3.14159) "0.314e+1")
  (equal? (format "~,5,3,-2e" 3.14159) "0.00314e+003")
  (equal? (format "~,5,3,-5e" -3.14159) "-0.000003e+006")  ; ans was "-0.00000e+006"; this is a case where we have to grow d to accommodate given k
  (equal? (format "~,5,2,2e" 3.14159) "31.4159e-01")
  (equal? (format "~,5,2,,,,'ee" 0.0) "0.00000e+00")
  (equal? (format "~12,3e" -3.141) "   -3.141e+0")
  (equal? (format "~12,3,,,,'#e" -3.141) "###-3.141e+0")
  (equal? (format "~10,2e" -1.236e-4) "  -1.24e-4")
  (equal? (format "~5,3e" -3.141) "-3.141e+0")
  (equal? (format "~5,3,,,'*e" -3.141) "*****")
  (equal? (format "~3e" 3.14159) "3.e+0")     ; ans was "3.14159e+0"
  (equal? (format "~4e" 3.14159) "3.e+0")     ; ans was "3.14159e+0"
  (equal? (format "~5e" 3.14159) "3.e+0")
  (equal? (format "~5,,,,'*e" 3.14159) "3.e+0")
  (equal? (format "~6e" 3.14159) "3.1e+0")
  (equal? (format "~7e" 3.14159) "3.14e+0")
  (equal? (format "~7e" -3.14159) "-3.1e+0")
  (equal? (format "~8e" 3.14159) "3.142e+0")
  (equal? (format "~9e" 3.14159) "3.1416e+0")
  (equal? (format "~9,,,,,,'ee" 3.14159) "3.1416e+0")
  (equal? (format "~10e" 3.14159) "3.14159e+0")
  (equal? (format "~11e" 3.14159) " 3.14159e+0")
  (equal? (format "~12e" 3.14159) "  3.14159e+0")
  (equal? (format "~13,6,2,-5e" 3.14159) " 0.000003e+06")
  (equal? (format "~13,6,2,-4e" 3.14159) " 0.000031e+05")
  (equal? (format "~13,6,2,-3e" 3.14159) " 0.000314e+04")
  (equal? (format "~13,6,2,-2e" 3.14159) " 0.003142e+03")
  (equal? (format "~13,6,2,-1e" 3.14159) " 0.031416e+02")
  (equal? (format "~13,6,2,0e" 3.14159)  " 0.314159e+01")
  (equal? (format "~13,6,2,1e" 3.14159)  " 3.141590e+00")
  (equal? (format "~13,6,2,2e" 3.14159)  " 31.41590e-01")
  (equal? (format "~13,6,2,3e" 3.14159)  " 314.1590e-02")
  (equal? (format "~13,6,2,4e" 3.14159)  " 3141.590e-03")
  (equal? (format "~13,6,2,5e" 3.14159)  " 31415.90e-04")
  (equal? (format "~13,6,2,6e" 3.14159)  " 314159.0e-05")
  (equal? (format "~13,6,2,7e" 3.14159)  " 3141590.e-06")
  (equal? (format "~13,6,2,8e" 3.14159)  "31415900.e-07")
  (equal? (format "~7,3,,-2e" 0.001) ".001e+0")
  (equal? (format "~8,3,,-2@e" 0.001) "+.001e+0")
  (equal? (format "~8,3,,-2@e" -0.001) "-.001e+0")
  (equal? (format "~8,3,,-2e" 0.001) "0.001e+0")
  (equal? (format "~7,,,-2e" 0.001) ".001e+0")              ; ans was "0.00e+0"
  (equal? (format "~12,3,1e" 3.14159e12) "   3.142e+12")
  (equal? (format "~12,3,1,,'*e" 3.14159e12) "************")
  (equal? (format "~5,3,1e" 3.14159e12) "3.142e+12")
)

(mat format-slib26 ; general floating point (this test is from Steele's CL book)
  (equal? (format "~9,2,1,,'*g|~9,3,2,3,'?,,'$g|~9,3,2,0,'%g|~9,2g"
            0.0314159 0.0314159 0.0314159 0.0314159)
          "  3.14e-2|314.2$-04|0.314e-01|  3.14e-2")
  (equal? (format "~9,2,1,,'*g|~9,3,2,3,'?,,'$g|~9,3,2,0,'%g|~9,2g"
            0.314159 0.314159 0.314159 0.314159)
          "  0.31   |0.314    |0.314    | 0.31    ")
  (equal? (format "~9,2,1,,'*g|~9,3,2,3,'?,,'$g|~9,3,2,0,'%g|~9,2g"
            3.14159 3.14159 3.14159 3.14159)
          "   3.1   | 3.14    | 3.14    |  3.1    ")
  (equal? (format "~9,2,1,,'*g|~9,3,2,3,'?,,'$g|~9,3,2,0,'%g|~9,2g"
            31.4159 31.4159 31.4159 31.4159)
          "   31.   | 31.4    | 31.4    |  31.    ")
  (equal? (format "~9,2,1,,'*g|~9,3,2,3,'?,,'$g|~9,3,2,0,'%g|~9,2g"
            314.159 314.159 314.159 314.159)
          "  3.14e+2| 314.    | 314.    |  3.14e+2")
  (equal? (format "~9,2,1,,'*g|~9,3,2,3,'?,,'$g|~9,3,2,0,'%g|~9,2g"
            3141.59 3141.59 3141.59 3141.59)
          "  3.14e+3|314.2$+01|0.314e+04|  3.14e+3")
  (equal? (format "~9,2,1,,'*g|~9,3,2,3,'?,,'$g|~9,3,2,0,'%g|~9,2g"
            3.14e12 3.14e12 3.14e12 3.14e12)
          "*********|314.0$+10|0.314e+13| 3.14e+12")
  (equal? (format "~9,2,1,,'*g|~9,3,2,3,'?,,'$g|~9,3,2,0,'%g|~9,2g"
            3.14e120 3.14e120 3.14e120 3.14e120)
          "*********|?????????|%%%%%%%%%|3.14e+120")

 ; the fixed-format in the following were followed by the apparently
 ; required backpadding that we omit when w is not supplied
  (equal? (format "~g" 0.0) "0.")
  (equal? (format "~g" 0.1) "0.1")
  (equal? (format "~g" 0.01) "1.0e-2")
  (equal? (format "~g" 123.456) "123.456")
  (equal? (format "~g" 123456.7) "123456.7")
  (equal? (format "~g" 123456.78) "123456.78")
  (equal? (format "~g" 0.9282) "0.9282")
  (equal? (format "~g" 0.09282) "9.282e-2")
  (equal? (format "~g" 1) "1.")
  (equal? (format "~g" 12) "12.")
)

(mat format-slib27 ; dollar floating point
  (equal? (format "~$" 1.23) "1.23")
  (equal? (format "~$" 1.2) "1.20")
  (equal? (format "~$" 0.0) "0.00")
  (equal? (format "~$" 9.999) "10.00")
  (equal? (format "~3$" 9.9999) "10.000")
  (equal? (format "~,4$" 3.2) "0003.20")
  (equal? (format "~,4$" 10000.2) "10000.20")
  (equal? (format "~,4,10$" 3.2) "   0003.20")
  (equal? (format "~,4,10@$" 3.2) "  +0003.20")
  (equal? (format "~,4,10:@$" 3.2) "+  0003.20")
  (equal? (format "~,4,10:$" -3.2) "-  0003.20")
  (equal? (format "~,4,10$" -3.2) "  -0003.20")
  (equal? (format "~,,10@$" 3.2) "     +3.20")
  (equal? (format "~,,10:@$" 3.2) "+     3.20")
  (equal? (format "~,,10:@$" -3.2) "-     3.20")
  (equal? (format "~,,10,'_@$" 3.2) "_____+3.20")
  (equal? (format "~,,4$" 1234.4) "1234.40")
)
