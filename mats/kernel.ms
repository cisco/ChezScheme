;;; kernel.ms
;;; Copyright 2024 Chez Scheme Contributors
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

;;; Tests for Kernel language features ($vau, operatives, environment-current)
;;; Based on John Shutt's Kernel language

;;; $vau - basic operative creation and compilation
(mat $vau-basic
  ;; $vau creates a procedure (compiled as case-lambda)
  (procedure? ($vau () e #t))

  ;; $vau with no formals, just env parameter
  (procedure? ($vau () env 42))

  ;; $vau with single formal
  (procedure? ($vau (x) e x))

  ;; $vau with multiple formals
  (procedure? ($vau (x y z) e (list x y z)))

  ;; $vau with rest parameter
  (procedure? ($vau args e args))

  ;; $vau with dotted list
  (procedure? ($vau (x . rest) e (cons x rest)))
)

;;; $vau - calling compiled operatives
;;; Note: In this implementation, operatives are compiled as procedures
;;; that expect (env arg1 arg2 ...) as arguments
(mat $vau-calling
  ;; Call operative with environment and arguments
  (let ([op ($vau (x) e x)])
    (eq? (op (interaction-environment) 42) 42))

  ;; Operative body can access the environment parameter
  (let ([op ($vau () e (environment? e))])
    (op (interaction-environment)))

  ;; Multiple arguments
  (let ([op ($vau (a b) e (+ a b))])
    (= (op (interaction-environment) 3 4) 7))

  ;; Rest arguments
  (let ([op ($vau args e (length args))])
    (= (op (interaction-environment) 1 2 3 4 5) 5))

  ;; Dotted list arguments
  (let ([op ($vau (first . rest) e (cons first rest))])
    (equal? (op (interaction-environment) 'a 'b 'c) '(a b c)))
)

;;; environment-current
(mat environment-current
  ;; environment-current returns an environment
  (environment? (environment-current))

  ;; environment-current returns the interaction environment (baseline impl)
  (eq? (environment-current) (interaction-environment))
)

;;; operative? predicate
(mat operative?
  ;; Note: In current implementation, operative? always returns #f
  ;; since operatives are compiled as regular procedures
  (not (operative? ($vau () e #t)))
  (not (operative? (lambda () #t)))
  (not (operative? 42))
  (not (operative? "string"))
)

;;; wrap - convert operative to applicative
(mat wrap
  ;; wrap takes a procedure
  (procedure? (wrap (lambda (env) 42)))

  ;; wrap errors on non-procedure
  (error? (wrap 42))
  (error? (wrap "string"))

  ;; wrapped operative can be called without explicit env
  (let ([op ($vau () e 'result)])
    (let ([app (wrap op)])
      (eq? (app) 'result)))

  ;; wrapped operative with arguments
  (let ([op ($vau (x y) e (+ x y))])
    (let ([app (wrap op)])
      (= (app 3 4) 7)))
)

;;; unwrap - extract operative from applicative
(mat unwrap
  ;; unwrap takes a procedure
  (procedure? (unwrap (lambda (x) x)))

  ;; unwrap errors on non-procedure
  (error? (unwrap 42))
  (error? (unwrap "string"))

  ;; unwrapped procedure expects env as first arg
  (let ([app (lambda (x y) (+ x y))])
    (let ([op (unwrap app)])
      (= (op 'ignored-env 3 4) 7)))
)

;;; Integration tests - combining features
(mat kernel-integration
  ;; Basic operative pattern: receive expression, evaluate in env
  ;; Note: This tests the compiled form, not true fexpr semantics
  (let ([op ($vau (expr) e
              ;; In full implementation, expr would be unevaluated
              ;; and we'd use (eval expr e) to evaluate it
              expr)])
    (= (op (interaction-environment) 42) 42))

  ;; Chain of wrap/unwrap preserves behavior
  (let ([f (lambda (x) (* x 2))])
    (let ([op (unwrap f)])
      (let ([f2 (wrap op)])
        (= (f2 21) 42))))

  ;; Operative in higher-order context
  (let ([apply-op (lambda (op env . args)
                    (apply op env args))])
    (let ([op ($vau (x) e (+ x 1))])
      (= (apply-op op (interaction-environment) 41) 42)))
)
