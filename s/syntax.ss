;;; syntax.ss
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

;;; Authors: R. Kent Dybvig, Oscar Waddell, Bob Hieb, Carl Bruggeman, Andy Keep

;;; Portions of this code have been made available as psyntax.ss, with
;;; the following notice.

;;; Portable implementation of syntax-case
;;; Extracted from Chez Scheme Version 7.3 (Feb 26, 2007)
;;; Authors: R. Kent Dybvig, Oscar Waddell, Bob Hieb, Carl Bruggeman

;;; Copyright (c) 1992-2002 Cadence Research Systems
;;; Permission to copy this software, in whole or in part, to use this
;;; software for any lawful purpose, and to redistribute this software
;;; is granted subject to the restriction that all copies made of this
;;; software must include this copyright notice in full.  This software
;;; is provided AS IS, with NO WARRANTY, EITHER EXPRESS OR IMPLIED,
;;; INCLUDING BUT NOT LIMITED TO IMPLIED WARRANTIES OF MERCHANTABILITY
;;; OR FITNESS FOR ANY PARTICULAR PURPOSE.  IN NO EVENT SHALL THE
;;; AUTHORS BE LIABLE FOR CONSEQUENTIAL OR INCIDENTAL DAMAGES OF ANY
;;; NATURE WHATSOEVER.

;;; The library support code borrows ideas from the portable R6RS libraries
;;; and syntax-case system written by Abdulaziz Ghuloum and R. Kent Dybvig.

; TODO:
; * fix to provide source information:
;   Error: missing definition for export(s) (y).
; * give ourselves better syntax-error formatting tools
; * unload module before loading it in sc-put-cte
; * consider allowing exports to be defined externally
;   - e.g., (let ((x 3)) (module foo (x)) (let ((x 4)) (import foo) x))
;   - implement simply by discarding check-module-exports
; * consider adding three-argument free-id=?:
;   - (free-id=? x y) => x & y free-id= in all environments
;   - (free-id=? x y r) => x & y free-id= in environment r
; * uncomment transformer support?  rename to sc-transformer
;   - fact that we currently strip lexicals from the transformer-env prevents
;     us from doing (let ((x 3)) (define-syntax y (transformer x)) y)
;   - compiler currently expects us to generate code with only valid lexicals
; * module init expressions
;   - allow inits to be interspersed with defns?
;   - return value of last init or void if none? (void if last form is defn?)
;   - should (let () (module () 3)) be okay?

; ? consider making symbol changes that will allow top-level references to
;   track top-level imports
; ? lazy prefix import
; ? figure out some way to collect superceded modules
;
; ? try implementing "stub" interface idea
;     - likely doomed because we can export two identifiers with same symname but different binding names
;       symnames not necessarily different at top-level if inserted into macro output...
;     - anonymous modules could probably be handled by matching their exports (if we can solve above problem)
; ? try to use vectors to contain module variable locations
; ? parse-define et al. should wrap the id for us since we seem to do that anyway
; ? lobotomize residual syntax objects

; ? chi-internal and chi-external both maintain a separate expression store
;   er for each form in the body, allegedly to allow "let-syntax or
;   letrec-syntax forms local to a portion or all of the body to shadow the
;   definition bindings".  In fact, since visibility is controlled solely
;   by the wrap, the separate environments determine only how long the local
;   syntax bindings are visible.  In particular, they rule out things like
;   the following:
;
;      (let ()
;        (let-syntax ((a (identifier-syntax 3)))
;          (define-syntax b (identifier-syntax a)))
;        b)
;
;   maintaining the separate stores just to rule this sort of thing out may
;   not be worth the code complexity or small efficiency hit.  consider
;   flushing
;
;   DONE 8/3/2004 (version 6.9c)

; ? avoid mark wraps in favor of marks (ie. cut fluff of building xtra pairs)
; ? have syntax-error generate warning and continue
; ? update the algebra sometime.

;;; Implementation notes:

;;; "begin" is treated as a splicing construct at top level and at
;;; the beginning of bodies.  Any sequence of expressions that would
;;; be allowed where the "begin" occurs is allowed.

;;; "let-syntax" and "letrec-syntax" are also treated as splicing
;;; constructs, in violation of the R5RS.  A consequence is that let-syntax
;;; and letrec-syntax do not create local contours, as do let and letrec.
;;; Although the functionality is greater as it is presently implemented,
;;; we will probably change it to conform to the R5RS.  modules provide
;;; similar functionality to nonsplicing letrec-syntax when the latter is
;;; used as a definition.

;;; Objects with no standard print syntax, including objects containing
;;; cycles and syntax objects, are allowed in quoted data as long as they
;;; are contained within a syntax form or produced by datum->syntax.
;;; Such objects are never copied.

;;; When the expander encounters a reference to an identifier that has
;;; no global or lexical binding, it treats it as a global-variable
;;; reference.  This allows one to write mutually recursive top-level
;;; definitions, e.g.:
;;;
;;;   (define f (lambda (x) (g x)))
;;;   (define g (lambda (x) (f x)))
;;;
;;; but may not always yield the intended when the variable in question
;;; is later defined as a keyword.

;;; Top-level variable definitions of syntax keywords are permitted.
;;; In order to make this work, top-level define not only produces a
;;; top-level definition in the core language, but also modifies the
;;; compile-time environment (using $sc-put-cte) to record the fact
;;; that the identifier is a variable.

;;; Top-level definitions of macro-introduced identifiers are visible
;;; only in code produced by the macro.  That is, a binding for a
;;; hidden (generated) identifier is created instead, and subsequent
;;; references within the macro output are renamed accordingly.  For
;;; example:
;;;
;;; (define-syntax a
;;;   (syntax-rules ()
;;;     ((_ var exp)
;;;      (begin
;;;        (define secret exp)
;;;        (define var
;;;          (lambda ()
;;;            (set! secret (+ secret 17))
;;;            secret))))))
;;; (a x 0)
;;; (x) => 17
;;; (x) => 34
;;; secret => Error: variable secret is not bound

;;; Identifiers and syntax objects are implemented as vectors for
;;; portability.  As a result, it is possible to "forge" syntax
;;; objects.

;;; The input to sc-expand may contain "annotations" describing, e.g., the
;;; source file and character position from where each object was read if
;;; it was read from a file.  These annotations are handled properly by
;;; sc-expand only if the annotation? hook (see hooks below) is implemented
;;; properly and the operators annotation-expression and annotation-stripped
;;; are supplied.  If annotations are supplied, the proper annotated
;;; expression is passed to the various output constructors, allowing
;;; implementations to accurately correlate source and expanded code.
;;; Contact one of the authors for details if you wish to make use of
;;; this feature.

;;; Implementation of modules:
;;;
;;; The implementation of modules requires that indirect top-level exports
;;; be listed with the exported macro at some level where both are visible,
;;; e.g.,
;;;
;;;   (module M (alpha (beta b))
;;;     (module ((alpha a) b)
;;;       (define-syntax alpha (identifier-syntax a))
;;;       (define a 'a)
;;;       (define b 'b))
;;;     (define-syntax beta (identifier-syntax b)))
;;;
;;; Listing of indirect imports is not needed for macros that do not make
;;; it out to top level, including all macros that are local to a "body".
;;; (They may be listed in this case, however.)  We need this information
;;; for top-level modules since a top-level module expands into a letrec
;;; for non-top-level variables and top-level definitions (assignments) for
;;; top-level variables.  Because of the general nature of macro
;;; transformers, we cannot determine the set of indirect exports from the
;;; transformer code, so without the user's help, we'd have to put all
;;; variables at top level.
;;;
;;; Each such top-level identifier is given a generated name (gensym).
;;; When a top-level module is imported at top level, a compile-time
;;; alias is established from the top-level name to the generated name.
;;; The expander follows these aliases transparently.  When any module is
;;; imported anywhere other than at top level, the label of the
;;; import identifier is set to the label of the export identifier.
;;;
;;; All identifiers defined within a local module are folded into the
;;; letrec created for the enclosing body.  Visibility is controlled in
;;; this case and for nested top-level modules by introducing a new wrap
;;; for each module.

;;; top-level semantics (6/30/2008)
;;; - all aux keywords are defined in scheme environment
;;;   - most signal syntax error when used out of context
;;;   - exceptions: eval-when aux keywords (eval, load, compile, visit, revisit)

;;; - interaction-environment semantics:
;;;   - initially same set of bindings and locations as Scheme environment
;;;     - behaves as if session began with an implicit (import scheme)
;;;     - variables implicitly imported thusly are immutable
;;;   - definitions of other identifiers as variables implied
;;;     - programmers are advised, however, to define all variables before
;;;       entering code containing assignments or references to them,
;;;       using (define id) if necessary for forward references, so that
;;;       subsequent additions to the scheme environment do not invalidate
;;;       existing programs
;;;   - top-level definition of unmarked symbol in a given environment maps
;;;     the name to a label that is uniquely determined for the environment
;;;     and name.  that is, multiple top-level definitions of the same name
;;;     always resolve to the same label.  So, for example,
;;;       (define cons 3) (define (f) cons) (import scheme)
;;;         (define cons 4) (f) ;=> 4
;;;     and
;;;       (define cons 3) (define (g) #'cons) (import scheme)
;;;         (define cons 4) (free-identifier=? (g) #'cons) ;=> #t
;;;     Also, for example, if t1.ss contains (define x 3) and t2.ss contains
;;;     (define x 4), x will refer to the same location in the two files.
;;;   - free-identifier=? equates identifiers that are truly free-identifier=?
;;;     - redefinition of aux keyword thus ruins its use as aux keyword
;;;       So, for example:
;;;         (define else) (case [else 3]) ;=> syntax error
;;;       and
;;;         (define else #f) (cond [else 3]) ;=> unspecified
;;;   - literal-identifier=? is the same as free-identifier=?
;;;   - eval-when checks aux keywords with free-id=?, then symbolic-id=?
;;;     - okay because its aux keywords don't appear in expression context,
;;;       i.e., they appear only where keywords can appear
;;;     - allows user to redefine or trace, e.g., load without breaking eval-when
;;;     - only form that uses names of predefined variables as aux keywords
;;;   - (define / -) gives no warning, as it previously did when optimize-level
;;;     is 2 or more, and does not set *flags* and *real-primref-name* on
;;;     #{/ *top*:/}.
;;;   - for copy-environment, operations on the environment and its copy
;;;     should behave similarly.  if an assignment in one affects the export
;;;     of an imported module's bindings, an assignment in the other should
;;;     as well.  similarly, if an assignment in one affects its own default
;;;     location, an assignment in the other should affect its own default
;;;     location.  for example, suppose in environment A, cons is the
;;;     (immutable) cons from the scheme library, foo is the (mutable) foo
;;;     from the bar module, and joe is a variable whose location is
;;;     specific to A.  then in the copy B of A, cons should be the
;;;     (immutable) cons from the scheme library, foo should be the
;;;     (mutable) foo from the bard module, and joe should be a variable
;;;     whose location is specific to B.  then the corresponding operation
;;;     (definition, assignment, reference) on each has the same affect as
;;;     on the other.  in particular, assigning cons is an error in both,
;;;     assigning foo changes module bar's foo, and assigning joe assigns
;;;     the environment's local joe.  defining joe in either amounts to the
;;;     same as an assignment to joe.
;;;
;;; - implementation
;;;   - symbols no longer have system-value slot
;;;   - top-level labels are (once again) always symbols
;;;   - sym.*system* = sym implied for all sym
;;;   - sym.token = #{sym token:sym} implied otherwise
;;;   - id.token = gensym if id is marked (as always)
;;;   - sym.*top* = sym explicitly set for sym in scheme environment
;;;   - sym.*cte* = (global . sym) implied for all sym
;;;   - sym.*read-only* set for *system* variables
;;;   - label.*read-only* set for library variables
;;;   - top-level definitions:
;;;     - import sets id.token to module's internal label
;;;     - if id=sym is unmarked, define, define-syntax, module, alias, and
;;;       dtlv set sym.token = #{sym token:sym} (mutable envs only)
;;;     - if id is marked and id.token resolves to mutable variable, define
;;;       residualizes an assignment to the variable's location
;;;       Q: does define also residualize visit-time code to set id.token
;;;          to the same label and label.*cte* to the same binding?
;;;     - any other definition of a marked id sets id.token = gensym
;;;     - definitions also set label.*cte* appropriately, and define and
;;;       dtlv also set label.value appropriately
;;;   - ref, set!, tlb?, tlv, and stlv! set sym.token = #{sym token:sym} for
;;;     mutable envs if sym.token is not yet set.  ref, set!, tlv, and stlv!
;;;     complain if sym.token does not resolve to a variable.  set! and stlv!
;;;     complain if sym.token resolves to an immutable variable.
;;;   - copy-environment creates new top-level labels/locations for and only
;;;     for variables whose locations are the default ones for the old
;;;     environment.  All other mappings from symbol to label should be
;;;     transferred from the old to the new environment.

;;; Bootstrapping:

;;; When changing syntax-object representations, it is necessary to support
;;; both old and new syntax-object representations in id->label.  It
;;; should be sufficient to redefine syntax-object-expression to work for
;;; both old and new representations and syntax-object-wrap to return the
;;; empty-wrap for old representations.
;;; See "module oldrep" below.

;;; ../bin/scheme cmacros.so
;;; > (subset-mode 'system)
;;; > (current-expand (lambda args (apply sc-expand args)))
;;; > (optimize-level 2)
;;; > (load "syntax.ss")
;;; > (compile-file "syntax.ss" "syntax.patch")

;;; WARNING:
;;;   the algebra below makes it appear that substitutions could come
;;;   between a shift-mark and the anti-mark
;;;     join-wraps(({m_0 ... m_n-1 m_n} . S1), (m_n:Marks . shift:Subs))
;;;   cannot get:
;;;     join-wraps(({m_0 ... m_n-1 m_n} . S1), (m_n:Marks . Subs:shift:Subs))

;;; The expander now uses the identifier's symbolic name when constructing
;;; a substitution rib for that id.  We believe there's no reason for the
;;; lazy expander to do all the substitutions that the eager expander would
;;; do.  When determining an identifier's binding name, we can stop at the
;;; first substitution we find.  This suggests a new representation for wraps:
;;;
;;;   wrap ::== ((mark ...) . (subst ...))
;;;   subst ::== #(symbolic-name label (mark ...))  |  shift
;;;
;;;   top-wrap = ((top))
;;;
;;;   (add-subst sname bname (Marks . Substs))
;;;      = (Marks . (#(sname bname Marks) . Substs))
;;;
;;;   (add-mark m_1 W)
;;;      = (join-wraps (new-mark-wrap m_1) W)
;;;
;;;   (new-mark-wrap m_1)
;;;      = ((m_1) . (shift))
;;;
;;;   (join-wraps ((m_0 ... m_n-1 m_n) . (S1 ... Sm shift))
;;;               ((m_n . M2) . (shift . S2)))
;;;      = (join-wraps ((m_0 ... m_n-1) . S1) (M2 . S2))
;;;      else like append
;;;      {does add-mark if marks don't cancel}
;;;
;;;   (id->label id (M . (#(id id' M) . S)))
;;;      = id'
;;;   (id->label id ((m . M) . (shift . S)))
;;;      = (id->label id (M . S))
;;;
;;; NB: This does screw up strange examples such as the following:
;;;
;;; (define-syntax a
;;;   (lambda (?)
;;;     (with-syntax ((xx ((lambda (x) (syntax x)) 4)))
;;;       (syntax (let ((xx 3) (x 4)) (list x xx))))))
;;;
;;; a ;=> (3 4) in v5.0b
;;; a ;=> error (duplicate bound ids) in v5.9a and beyond
;;;
;;; Which is correct?  Should we substitute based on symbolic name and
;;; marks or based on the current "name" (label)?  It's not clear, and
;;; substitution based on symbolic name and marks is both easier and
;;; more efficient, so we've gone with that.  On the other hand, the
;;; other approach is more directly based on alpha conversion and thus
;;; perhaps more theoretically appealing.  Both approaches yield
;;; (slightly different interpretations of) lexical scope.
;;; Conjecture: if we disregard out-of-context identifiers, the two
;;; mechanisms yield identical substitutions.

(let ()
(define noexpand "noexpand")

;;; hooks to nonportable run-time helpers

(include "types.ss")
(import (nanopass))
(include "base-lang.ss")
(include "expand-lang.ss")

(begin
(define top-level-eval-hook
  ; for top-level macro transformers and eval-when, use default
  ; system evaluator
  (lambda (x)
    (eval `(,noexpand ,x))))

(define local-eval-hook
  ; for local macro transformers, use interpreter unless profiling is enabled
  (lambda (x)
    ((if (compile-profile) eval interpret) `(,noexpand ,x))))

(define define-top-level-value-hook $set-top-level-value!)

(define get-clo-info
  (lambda (sym)
    ($sgetprop sym '*clo* #f)))

(define put-clo-info
  (lambda (sym info)
    ($sputprop sym '*clo* info)))

(define get-global-definition-hook
  (lambda (sym)
    ($sgetprop sym '*cte* #f)))

(define put-global-definition-hook
  (lambda (sym x)
    (with-tc-mutex
      (if (not x)
          ($sremprop sym '*cte*)
          ($sputprop sym '*cte* x)))))

(define put-library-descriptor
  (lambda (symbol desc)
    ($sputprop symbol '*library* desc)))

(define get-library-descriptor
  (lambda (symbol)
    ($sgetprop symbol '*library* #f)))

(define rem-library-descriptor
  (lambda (symbol)
    ($sremprop symbol '*library*)))

(define put-program-descriptor
  (lambda (symbol desc)
    ($sputprop symbol '*program* desc)))

(define get-program-descriptor
  (lambda (symbol)
    ($sgetprop symbol '*program* #f)))

(define rem-program-descriptor
  (lambda (symbol)
    ($sremprop symbol '*program*)))

(define get-global-substs
  (lambda (symbol token)
    ($sgetprop symbol token #f)))

(define update-global-substs!
  (lambda (symbol token p)
    (with-tc-mutex
      (let ([x (p ($sgetprop symbol token #f))])
        (if (not x)
            ($sremprop symbol token)
            ($sputprop symbol token x))))))

(define generate-id
  (lambda (sym)
    ((current-generate-id) sym)))

(define make-token:sym
  (lambda (token sym)
    (let ([sym-pname (symbol->string sym)])
      (gensym
        sym-pname
        (let ([token-name (if (gensym? token) (gensym->unique-string token) (symbol->string token))])
          (if (gensym? sym)
             ; assuming that token pnames/unames never contain : or %
              (format "~a%~a" token-name (gensym->unique-string sym))
              (format "~a:~a" token-name sym-pname)))))))
)

;;; output constructors
(with-output-language (Lsrc Expr)
(define ae->src
  (lambda (ae)
    (and (and (annotation? ae) (fxlogtest (annotation-flags ae) (constant annotation-debug)))
         (annotation-source ae))))

(define build-profile
  (lambda (ae e)
    (define ae->profile-src
      (lambda (ae)
        (and (and (annotation? ae) (fxlogtest (annotation-flags ae) (constant annotation-profile)))
             (annotation-source ae))))
    (if (and (eq? ($compile-profile) 'source)
             (generate-profile-forms))
        (let ([src (ae->profile-src ae)])
          (if src `(seq (profile ,src) ,e) e))
        e)))

(module (build-lambda build-lambda/lift-barrier build-library-case-lambda build-case-lambda)
  (define build-clause
    (lambda (fmls body)
      (let f ((ids fmls) (n 0))
        (in-context CaseLambdaClause
          (cond
            ((pair? ids) (f (cdr ids) (fx+ n 1)))
            ((null? ids) `(clause (,fmls ...) ,n ,body))
            (else
             `(clause
                (,(let f ((ids fmls))
                    (if (pair? ids)
                        (cons (car ids) (f (cdr ids)))
                        (list ids))) ...)
                ,(fx- -1 n)
                ,body)))))))

  (define build-clauses
    (lambda (clauses)
      (map (lambda (x) (build-clause (car x) (cadr x))) clauses)))

  (define build-lambda
    (lambda (ae vars exp)
      (build-profile ae
         `(case-lambda ,(make-preinfo-lambda (ae->src ae))
            ,(build-clause vars exp)))))

  (define build-lambda/lift-barrier
    (lambda (ae vars exp)
      (build-profile ae
        `(case-lambda ,(make-preinfo-lambda (ae->src ae) #f #f #f (constant code-flag-lift-barrier))
           ,(build-clause vars exp)))))

  (define build-case-lambda
    (lambda (ae clauses)
      (build-profile ae
        `(case-lambda ,(make-preinfo-lambda (ae->src ae) #f)
           ,(build-clauses clauses) ...))))

  (define build-library-case-lambda
    (lambda (ae libspec clauses)
      (build-profile ae
        (let ([clauses (build-clauses clauses)])
          (unless (equal? (list (libspec-interface libspec))
                    (map (lambda (clause)
                           (nanopass-case (Lsrc CaseLambdaClause) clause
                             [(clause (,x* ...) ,interface ,body) interface]))
                      clauses))
            ($oops #f "libspec interface mismatch ~s" libspec))
          `(case-lambda ,(make-preinfo-lambda (ae->src ae) #f libspec (symbol->string (libspec-name libspec))) ,clauses ...))))))

(define build-call
  (lambda (ae e e*)
    (build-profile ae
      (let ([flags (if (or (fx>= (optimize-level) 3)
                           (enable-unsafe-application))
                       (preinfo-call-mask unchecked)
                       (preinfo-call-mask))])
        `(call ,(make-preinfo-call (ae->src ae) #f flags) ,e ,e* ...)))))

(define build-application
  ; used by chi-application.  pulls profile form off e if e is a lambda expression
  ; so it won't interfere with cprep and cpvalid let recognition
  (lambda (ae e e*)
    (if (eq? ($compile-profile) 'source)
        (nanopass-case (Lsrc Expr) e
          [(seq ,e1 ,e2)
           (guard
             (nanopass-case (Lsrc Expr) e1 [(profile ,src) #t] [else #f])
             (nanopass-case (Lsrc Expr) e2 [(case-lambda ,preinfo ,cl* ...) #t] [else #f]))
           `(seq ,e1 ,(build-call ae e2 e*))]
          [else (build-call ae e e*)])
        (build-call ae e e*))))

(define-syntax build-primcall
  ; written as a macro to give lookup-primref a chance to lookup the primref at expansion time
  (syntax-rules ()
    [(_ ?ae ?level ?name ?arg ...)
     (build-call ?ae (lookup-primref ?level ?name) (list ?arg ...))]))

(define build-let
  (lambda (ae x* e* body)
    (build-call ae (build-lambda #f x* body) e*)))

(define build-conditional
  (lambda (ae test-exp then-exp else-exp)
    (build-profile ae `(if ,test-exp ,then-exp ,else-exp))))

(define build-lexical-reference
  (lambda (ae prelex)
    (if (prelex-referenced prelex)
        (set-prelex-multiply-referenced! prelex #t)
        (set-prelex-referenced! prelex #t))
    (build-profile ae `(ref ,(ae->src ae) ,prelex))))

(define build-lexical-assignment
  (lambda (ae var exp)
    (set-prelex-assigned! var #t)
    (build-profile ae `(set! ,(ae->src ae) ,var ,exp))))

(define build-cte-optimization-loc
  (lambda (box exp exts)
    ; box is for cp0 to store optimization info, if it pleases.  the box is eq? to
    ; the box on the system property list for the library global label and
    ; stored in the library/ct-info record for the file.
    `(cte-optimization-loc ,box ,exp ,exts)))

(define build-primitive-reference
  (lambda (ae name)
    (if ($suppress-primitive-inlining)
        (build-primcall ae 3 '$top-level-value `(quote ,name))
        (build-profile ae (lookup-primref (fxmax (optimize-level) 2) name)))))

(define build-primitive-assignment
  (lambda (ae name val)
    (build-primcall ae 3 '$set-top-level-value! `(quote ,name) val)))

(module (build-global-reference build-global-assignment)
  (define unbound-warning
    (lambda (src what name)
      (unless (or (gensym? name) ($sgetprop name 'no-unbound-warning #f))
        ($source-warning #f src #t "undeclared variable ~a ~s" what name)
        ($sputprop name 'no-unbound-warning #t))))

  (define build-global-reference
    (lambda (ae name safe?)
      (when (eq? (subset-mode) 'system) (unbound-warning (ae->src ae) "reference to" name))
      (build-primcall ae (if (or safe? (fx= (optimize-level) 3)) 3 2) '$top-level-value `(quote ,name))))

  (define build-global-assignment
    (lambda (ae name val)
      (when (eq? (subset-mode) 'system) (unbound-warning (ae->src ae) "assignment to" name))
      (build-primcall ae 3 '$set-top-level-value! `(quote ,name) val))))

(define build-cte-install
  (lambda (sym exp token)
    (build-primcall #f 3 '$sc-put-cte
      `(quote ,sym) exp `(quote ,token))))

(define build-checking-cte-install
  (lambda (sym exp token)
    (build-cte-install sym
      (build-primcall #f 3 '$transformer->binding exp)
      token)))

(define build-visit-only
  (lambda (exp)
    (with-output-language (Lexpand Outer)
      `(visit-only ,exp))))

(define build-revisit-only
  (lambda (exp)
    (with-output-language (Lexpand Outer)
      `(revisit-only ,exp))))

(define safe-to-defer?
  (lambda (x)
    (nanopass-case (Lsrc Expr) x
      [(seq (profile ,src) (case-lambda ,preinfo ,cl* ...)) #t]
      [(case-lambda ,preinfo ,cl* ...) #t]
      [else #f])))

(define build-moi
  (let ([moi-record `(moi)])
    (lambda () moi-record)))

(module (build-foreign-procedure build-foreign-callable)
  (define build-fp-specifier
    (lambda (who what x void-okay?)
      (with-output-language (Ltype Type)
        (or (case x
              [(scheme-object) `(fp-scheme-object)]
              [(u8*) `(fp-u8*)]
              [(u16*) `(fp-u16*)]
              [(u32*) `(fp-u32*)]
              [(fixnum) `(fp-fixnum)]
              [(double-float) `(fp-double-float)]
              [(single-float) `(fp-single-float)]
              [(integer-8) `(fp-integer 8)]
              [(unsigned-8) `(fp-unsigned 8)]
              [(integer-16) `(fp-integer 16)]
              [(unsigned-16) `(fp-unsigned 16)]
              [(integer-24 integer-32) `(fp-integer 32)]
              [(unsigned-24 unsigned-32) `(fp-unsigned 32)]
              [(integer-40 integer-48 integer-56 integer-64) `(fp-integer 64)]
              [(unsigned-40 unsigned-48 unsigned-56 unsigned-64) `(fp-unsigned 64)]
              [(void) (and void-okay? `(fp-void))]
              [else
               (cond
                [($ftd? x) `(fp-ftd ,x)]
                [($ftd-as-box? x) `(fp-ftd& ,(unbox x))]
                [else #f])])
            ($oops #f "invalid ~a ~a specifier ~s" who what x)))))

  (define build-foreign-procedure
    (lambda (ae conv* foreign-name foreign-addr params result)
      (build-profile ae
        `(foreign (,conv* ...) ,foreign-name ,foreign-addr
           (,(map (lambda (x) (build-fp-specifier 'foreign-procedure 'parameter x #f)) params) ...)
           ,(build-fp-specifier 'foreign-procedure "result" result #t)))))

  (define build-foreign-callable
    (lambda (ae conv* proc params result)
      (build-profile ae
        `(fcallable (,conv* ...) ,proc
           (,(map (lambda (x) (build-fp-specifier 'foreign-callable 'parameter x #f)) params) ...)
           ,(build-fp-specifier 'foreign-callable "result" result #t))))))

(define build-pariah
  (lambda (ae e)
    (build-profile ae `(seq (pariah) ,e))))

(define-syntax build-primref
  (syntax-rules ()
    [(_ ?level ?name) (lookup-primref ?level ?name)]))

(define build-primref?
  (lambda (ae level name)
    (let ([pr ($sgetprop name (if (eqv? level 2) '*prim2* '*prim3*) #f)])
      (and pr (build-profile ae pr)))))

(define build-data
  (lambda (ae exp)
    (build-profile ae `(quote ,exp))))

(define build-void
  (let ([void-record `(quote ,(void))])
    (case-lambda
      [() void-record]
      [(ae) (build-profile ae void-record)])))

(define build-sequence
  (lambda (ae x*)
    (if (null? x*)
        (build-void ae)
        (build-profile ae
          (let loop ([x* x*])
            (let ([x (car x*)] [x* (cdr x*)])
              (if (null? x*)
                  x
                  (if (nanopass-case (Lsrc Expr) x
                        [(quote ,d) (eq? d (void))]
                        [else #f])
                      (loop x*)
                      `(seq ,x ,(loop x*))))))))))

(define build-group
  (lambda (x*)
    (with-output-language (Lexpand Outer)
      (if (null? x*)
          (build-void no-source)
          (let f ([x* x*])
            (let ([x (car x*)] [x* (cdr x*)])
              (if (null? x*)
                  x
                  (if (and (Lsrc? x)
                           (nanopass-case (Lsrc Expr) x
                             [(quote ,d) (eq? d (void))]
                             [else #f]))
                      (f x*)
                      (let ([y (f x*)])
                        (if (and (Lsrc? x) (Lsrc? y))
                            (with-output-language (Lsrc Expr) `(seq ,x ,y))
                            `(group ,x ,y)))))))))))

(define build-letrec
  (lambda (ae vars val-exps body-exp)
    (build-profile ae
      (if (null? vars)
          body-exp
          `(letrec ([,vars ,val-exps] ...) ,body-exp)))))

(define build-letrec*
  (lambda (ae vars val-exps body-exp)
    (build-profile ae
      (if (null? vars)
          body-exp
          `(letrec* ([,vars ,val-exps] ...) ,body-exp)))))

(define build-body
  (lambda (ae vars val-exps body-exp)
    ((if (internal-defines-as-letrec*) build-letrec* build-letrec)
     ae vars val-exps body-exp)))

(define build-top-module
  (lambda (ae types vars val-exps body-exp)
    (if (internal-defines-as-letrec*)
        (let-values ([(vars val-exps)
                      (let f ([types types] [vars vars] [val-exps val-exps])
                        (if (null? types)
                            (values '() '())
                            (let ([var (car vars)] [val-exp (car val-exps)])
                              (let-values ([(vars val-exps) (f (cdr types) (cdr vars) (cdr val-exps))])
                                (if (eq? (car types) 'global)
                                    (values
                                      (cons (build-lexical-var no-source 'ignore) vars)
                                      (cons (build-global-assignment no-source var val-exp) val-exps))
                                    (values
                                      (cons var vars)
                                      (cons val-exp val-exps)))))))])
          (build-letrec* ae vars val-exps body-exp))
        (let-values ([(vars sets)
                      (let f ([types types] [vars vars])
                        (if (null? types)
                            (values '() '())
                            (let ([var (car vars)])
                              (let-values ([(vars sets) (f (cdr types) (cdr vars))])
                                (if (eq? (car types) 'global)
                                    (let ([x (build-lexical-var no-source var)])
                                      (values
                                        (cons x vars)
                                        (cons (build-global-assignment no-source var (build-lexical-reference no-source x)) sets)))
                                    (values (cons var vars) sets))))))])
          (build-letrec ae vars val-exps
            (if (null? sets)
                body-exp
                (build-sequence no-source (append sets (list body-exp)))))))))

(define build-top-program
  (lambda (uid body-exp)
    (with-output-language (Lexpand Program)
      `(program ,uid ,body-exp))))

(define build-recompile-info
  (lambda (import-req* include-req*)
    (with-output-language (Lexpand Outer)
      `(recompile-info
         ,(make-recompile-info
            (remp (lambda (x) (libdesc-system? (get-library-descriptor (libreq-uid x)))) import-req*)
            include-req*)))))

(define build-library/ct-info
  (lambda (linfo/ct)
    (with-output-language (Lexpand Inner)
      `(library/ct-info ,linfo/ct))))

(define build-library/rt-info
  (lambda (linfo/rt)
    (with-output-language (Lexpand Inner)
      `(library/rt-info ,linfo/rt))))

(define build-program-info
  (lambda (pinfo)
    (with-output-language (Lexpand Inner)
      `(program-info ,pinfo))))

(define build-top-library/rt
  (lambda (uid dl* db* dv* de* init*)
    (with-output-language (Lexpand rtLibrary)
      `(library/rt ,uid (,dl* ...) (,db* ...) (,dv* ...) (,de* ...)
         ,(build-sequence no-source init*)))))

(define build-top-library/ct
  (lambda (uid export-id* interface import-code* visit-code*)
    (with-output-language (Lexpand ctLibrary)
      `(library/ct ,uid
         (,export-id* ...)
         ,(build-case-lambda no-source ;; case-lambda to simplify bootstrapping
            (list
             (list '() (build-sequence no-source import-code*))
             (list (list (gen-var 'ignored)) (build-data no-source interface))))
         ,(if (null? visit-code*)
                (build-primref 3 'void)
                (build-lambda no-source '()
                  (build-sequence no-source visit-code*)))))))

(define build-library-body
  (lambda (ae labels boxes vars val-exps body-exp)
    (let ([exts (build-library-exts labels vars)])
      (build-letrec* ae vars val-exps
        (fold-right
          (lambda (label box var body)
            (if label
                `(seq
                   ,(build-global-assignment no-source label
                      (build-cte-optimization-loc box
                        (build-lexical-reference no-source var)
                        exts))
                   ,body)
                body))
          body-exp labels boxes vars)))))

(define (build-library-exts labels vars)
  (fold-left (lambda (exts label var)
               (if label
                   (cons (cons var label) exts)
                   exts))
             '()
             labels
             vars))

(define build-lexical-var
  (lambda (ae id)
    (make-prelex id 0 ae #f)))

(define lexical-var-assigned? (lambda (x) (prelex-assigned x)))

(define build-input-profile (lambda (src) `(profile ,src)))
)

(define unannotate
  (lambda (e)
    (if (annotation? e)
        (annotation-expression e)
        e)))

(define no-source #f)

;;; compile-time environments

;;; wrap and environment comprise two level mapping.
;;;   wrap : id --> label
;;;   env : label --> <binding>

;;; to handle define-property, wraps actually map id -> label/pl
;;;   label/pl -> label | (label . (property ...))
;;;   property -> (label . label)

;;; environments are represented in two parts: a lexical part and a global
;;; part.  The lexical part is a simple list of associations from labels
;;; to bindings.  The global part is implemented by
;;; {put,get}-global-definition-hook and associates symbols with
;;; bindings.

;;; global (assumed global variable) and displaced-lexical (see below)
;;; do not usually show up in any environment; instead, they are fabricated by
;;; lookup when it finds no other bindings.

;;; <environment>              ::= ((<label> . <binding>)*)

;;; identifier bindings include a type and a value

;;; <binding> ::= binding(type, value)
;;;    type                    value              explanation
;;;    -------------------------------------------------------------------
;;;    alias                   none               alias keyword
;;;    begin                   none               begin keyword
;;;    core                    procedure          core keyword
;;;    ctv                     ctv record         user-defined compile-time value
;;;    deferred                thunk              macro keyword w/lazily evaluated transformer
;;;    define                  none               define keyword
;;;    define-property         none               define-property keyword
;;;    define-syntax           none               define-syntax keyword
;;;    displaced-lexical       <why>              id's label not found in env
;;;    eval-when               none               eval-when keyword
;;;    export                  none               export keyword
;;;    global                  symbol             assumed global variable
;;;    immutable-global        symbol             immutable global variable (created by copy-environment)
;;;    implicit-exports        none               implicit-exports keyword
;;;    $import                 none               $import keyword
;;;    indirect-export         none               indirect-export keyword
;;;    lexical                 <var>              lexical variables
;;;    library-global          (uid . sym)        immutable library variable
;;;    $library-key            none               $library keyword
;;;    library-meta-global     (uid . sym)        library meta variable
;;;    local-syntax            boolean            let-syntax (#f)/letrec-syntax (#t) keyword
;;;    macro!                  procedure          extended identifier macro keyword
;;;    macro                   procedure          macro keyword
;;;    meta                    none               meta keyword
;;;    meta-variable           symbol             meta variable
;;;    $module                 interface          modules
;;;    $module-key             none               $module keyword
;;;    primitive               symbol             primitive global variable
;;;    $program-key            none               $program keyword
;;;    set!                    none               set! keyword
;;;    syntax                  (<var> . <level>)  pattern variables
;;;    visit                   uid                visit library to determine actual binding
;;; <level>   ::= <nonnegative integer>
;;; <var>     ::= variable returned by build-lexical-var
;;; <why>     ::= #f | string
;;; <cp0>     ::= #f | association list of machine type to optimization information

;;; a macro is a user-defined syntactic-form.  a core is a system-defined
;;; syntactic form.  begin, define, define-syntax, let-syntax, letrec-syntax,
;;; eval-when, and meta are treated specially since they are sensitive to
;;; whether the form is at top-level and can denote valid internal
;;; definitions.

;;; a pattern variable is a variable introduced by syntax-case and can
;;; be referenced only within a syntax form.

;;; any identifier for which no top-level syntax definition or local
;;; binding of any kind has been seen is assumed to be a global
;;; variable.

;;; a lexical variable is a lambda- or letrec-bound variable.

;;; a displaced-lexical identifier is a lexical identifier removed from
;;; it's scope by the return of a syntax object containing the identifier.
;;; a displaced lexical can also appear when a letrec-syntax-bound
;;; keyword is referenced on the rhs of one of the letrec-syntax clauses.
;;; a displaced lexical should never occur with properly written macros.

(define-record-type core-transformer
  (fields (immutable binding))
  (nongenerative #{g0 cy54rjf3ozvorudk-a})
  (opaque #t)
  (sealed #t))

(define-record-type (variable-transformer $make-variable-transformer variable-transformer?)
  (fields (immutable procedure))
  (nongenerative #{g0 cz18zz6lfwg7mc7m-a})
  (sealed #t))

(define-record-type (compile-time-value $make-compile-time-value $compile-time-value?)
  (fields (immutable value $compile-time-value-value))
  (nongenerative #{g0 c0f3a5187l98t2ef-a})
  (sealed #t))

(define make-rho
  (lambda ()
    (make-eq-hashtable)))

(define extend-rho!
  (lambda (r label binding level)
    (eq-hashtable-set! r label (make-local-label binding level))))

(define retract-rho!
  (lambda (r label)
    (eq-hashtable-delete! r label)))

(define lookup-rho
  (lambda (r label)
    (eq-hashtable-ref r label #f)))

(define displaced-lexical-binding (make-binding 'displaced-lexical #f))
(define unexported-assigned-binding (make-binding 'displaced-lexical "assigned hence unexported library variable"))
(define unexported-binding (make-binding 'displaced-lexical "unexported identifier"))
(define out-of-phase-binding (make-binding 'displaced-lexical "out-of-phase identifier"))

(define (displaced-lexical? id r)
  (let ([n (id->label id empty-wrap)])
    (and n
         (let ([b (lookup n r)])
           (eq? (binding-type b) 'displaced-lexical)))))

(define displaced-lexical-error
  (lambda (id what why)
    (cond
      [(string? why) (syntax-error id (format "attempt to ~a ~a" what why))]
      [(id->label id empty-wrap) (syntax-error id (format "attempt to ~a out-of-context identifier" what))]
      [else ($undefined-violation id (format "attempt to ~a unbound identifier" what))])))

(define lookup-global*
  (lambda (label)
    (cond
      [(get-global-definition-hook label) =>
       (lambda (b)
         (case (binding-type b)
           [(visit)
            (visit-loaded-library (binding-value b))
            (get-global-definition-hook label)]
           [else b]))]
      [else (make-binding 'global label)])))

(define lookup-global
 ; undefers deferred bindings
  (lambda (label)
    (let ([b (lookup-global* label)])
      (case (binding-type b)
        [(deferred)
         (set-binding-value! b ((binding-value b)))
         (set-binding-type! b 'macro)
         b]
        [else b]))))

(define lookup*
  (lambda (x r)
    (define lookup-local-label*
      (lambda (x)
        (let ([xml (local-label-level x)] [ml (meta-level)])
          (if (or (fx= ml xml) (and (fx< xml 0) (fx>= ml (fxlognot xml))))
              (local-label-binding x)
              out-of-phase-binding))))
    (cond
      [(symbol? x)
       (cond
         [(lookup-rho r x) => lookup-local-label*]
         [else (lookup-global* x)])]
      [(not x) displaced-lexical-binding]
      [else (lookup-local-label* x)])))

(define lookup
 ; undefers deferred bindings
  (lambda (x r)
    (let ([b (lookup* x r)])
      (case (binding-type b)
        [(deferred)
         (set-binding-value! b ((binding-value b)))
         (set-binding-type! b 'macro)
         b]
        [else b]))))

(define lookup-pattern-variable
  (lambda (x r)
   ; pattern variable bindings are never global, so this doesn't go to
   ; lookup-global, which might cause a library to be visited.  it doesn't
   ; undefer deferred bindings
    (cond
      [(and (local-label? x)
         (let ([xml (local-label-level x)] [ml (meta-level)])
           (and (or (fx= ml xml) (and (fx< xml 0) (fx>= ml (fxlognot xml))))
                (local-label-binding x)))) =>
       (lambda (b)
         (case (binding-type b)
           [(syntax) (binding-value b)]
           [else #f]))]
      [else #f])))

(define transformer->binding
  (lambda (who x)
    (cond
      [(procedure? x) (make-binding 'macro x)]
      [(core-transformer? x) (core-transformer-binding x)]
      [(variable-transformer? x) (make-binding 'macro! (variable-transformer-procedure x))]
      [($compile-time-value? x) (make-binding 'ctv x)]
      [else ($oops who "invalid transformer ~s" x)])))

(define defer-or-eval-transformer
  (lambda (who eval x)
    (if (safe-to-defer? x)
        (make-binding 'deferred (lambda () (eval x)))
        (transformer->binding who (eval x)))))

(define global-extend
  (lambda (type sym val)
    ($sc-put-cte
      (make-resolved-id sym (wrap-marks top-wrap) sym)
      (make-binding type val)
      '*system*)))


;;; Conceptually, identifiers are always syntax objects.  Internally,
;;; however, the wrap is sometimes maintained separately (a source of
;;; efficiency and confusion), so that symbols are also considered
;;; identifiers by id?.  Externally, they are always wrapped.

(define nonsymbol-id?
  (lambda (x)
    (and (syntax-object? x)
         (symbol? (unannotate (syntax-object-expression x))))))

(define id?
  (lambda (x)
    (cond
      ((symbol? x) #t)
      ((syntax-object? x) (symbol? (unannotate (syntax-object-expression x))))
      ((annotation? x) (symbol? (annotation-expression x)))
      (else #f))))

(define-syntax id-sym-name
  (syntax-rules ()
    ((_ e)
     (let ((x e))
       (unannotate (if (syntax-object? x) (syntax-object-expression x) x))))))

(define id-marks
  (lambda (id)
    (if (syntax-object? id)
        (wrap-marks (syntax-object-wrap id))
        (wrap-marks top-wrap))))

(define id-subst
  (lambda (id)
    (if (syntax-object? id)
        (wrap-subst (syntax-object-wrap id))
        (wrap-marks top-wrap))))

(define id-sym-name&marks
  (lambda (x w)
    (if (syntax-object? x)
        (values
          (unannotate (syntax-object-expression x))
          (join-marks (wrap-marks w) (wrap-marks (syntax-object-wrap x))))
        (values (unannotate x) (wrap-marks w)))))

(define id->unprefixed-id
  (lambda (id)
    (let* ([sym (id-sym-name id)]
           [unprefixed-sym ($sgetprop sym '*unprefixed* sym)])
      (if (eq? sym unprefixed-sym)
          id
          (make-syntax-object unprefixed-sym (syntax-object-wrap id))))))

;;; syntax object wraps

;;;         <wrap>     ::= ((<mark> ...) . (<subst> ...))
;;;        <subst>     ::= <ribcage> | shift
;;;      <ribcage>     ::= #[extensible-ribcage (<chunk> ...)) ; extensible, for chi-internal/external
;;;                      | #[fixed-ribcage #(<symname> ...) #(<mark> ...) #(<label/pl> ...)] ; nonextensible
;;;                      | #[top-ribcage <token> <mutable?>]
;;;        <chunk>     ::= <hashtable> | <import interface> | <barrier>

(define make-wrap cons)
(define wrap-marks car)
(define wrap-subst cdr)

;;; would like to use record for wraps, but we don't have any way to
;;; create record constants for empty-wrap and top-wrap, since reader
;;; won't recognize them until records have been defined.
;;;
;;; (define-record #{wrap cgos0c9ufi1rq-ej} ((immutable marks) (immutable subst)))

(define-syntax empty-wrap (identifier-syntax '(())))

(define-syntax top-wrap (identifier-syntax '((top))))

(define-syntax top-marked?
  (syntax-rules ()
    ((_ w) (memq 'top (wrap-marks w)))))

(define-syntax only-top-marked?
  (syntax-rules ()
    ((_ id) (same-marks? (wrap-marks (syntax-object-wrap id)) (wrap-marks top-wrap)))))

;;; labels

;;; labels must be comparable with "eq?".  we use gensyms for global labels
;;; for read/write consistency and because we store bindings on their property
;;; lists.  for local labels, one-character strings take up less room.

(define gen-global-label (lambda (sym) (generate-id sym)))

(define-record-type local-label
  (nongenerative #{local-label a0vrcedkvxwbnsyv-0})
  (fields (mutable binding) (mutable level))
  (sealed #t))

(define meta-level
  (case-lambda
    [() ($tc-field 'meta-level ($tc))]
    [(x) ($tc-field 'meta-level ($tc) x)]))

; variant that builds lexical bindings
(define make-lexical-label
  (lambda (var)
    (make-local-label (make-binding 'lexical var) (meta-level))))

(define kill-label!
  (lambda (r)
    (lambda (x)
      (if (symbol? x)
          (retract-rho! r x)
          (kill-local-label! x)))))

(define kill-local-label!
  (lambda (x)
    (local-label-binding-set! x displaced-lexical-binding)))

;;; label/pls

(define make-label/pl
  (case-lambda
    [(label pl)
     (if (null? pl)
         label
         (cons label pl))]
    [(label p pl)
     (cons* label p (remp (lambda (q) (eq? (car q) (car p))) pl))]))

(define label/pl->label
  (lambda (label/pl)
    (if (pair? label/pl)
        (car label/pl)
        label/pl)))

(define label/pl->pl
  (lambda (label/pl)
    (if (pair? label/pl)
        (cdr label/pl)
        '())))

(define-record-type fixed-ribcage
  (fields (immutable symnames) (immutable marks) (immutable label/pls))
  (nongenerative #{fixed-ribcage cqxefau3fa3vz4m0-0})
  (sealed #t))
(define-record-type extensible-ribcage
  (fields (mutable chunks))
  (nongenerative #{extensible-ribcage cqxefau3fa3vz4m0-1})
  (sealed #t))
(define-record-type top-ribcage
  (fields (immutable key) (mutable mutable?))
  (nongenerative #{top-ribcage fxdfzth2q3h88vd-a})
  (sealed #t))
(define-record-type import-interface
  (fields (immutable interface) (immutable new-marks))
  (nongenerative #{import-interface fzyvk56r66o8ft5-a})
  (sealed #t))
(define-record-type env
  (fields (immutable top-ribcage) (immutable wrap))
  (nongenerative #{env f2zvr2zlvyfdhyo-a})
  (sealed #t))

(define get-indirect-interface
 ; in $module bindings and import-interface records, the interface
 ; field might be a symbol whose value field holds the actual
 ; interface.  this is done for built-in modules and libraries to
 ; reduce the size of the wrap for a syntax object scoped within
 ; an import of one of these modules or libraries
  (lambda (x)
    (if (symbol? x) ($top-level-value x) x)))

;;; Marks must be comparable with "eq?" and distinct from pairs and
;;; the symbol top.  We do not use integers so that marks will remain
;;; unique even across file compiles.

(define-syntax the-anti-mark (identifier-syntax #f))

(define anti-mark
  (lambda (w)
    (make-wrap (cons the-anti-mark (wrap-marks w))
               (cons 'shift (wrap-subst w)))))

(define new-mark (lambda () (string #\m)))

(define-record-type barrier
  (fields (immutable marks))
  (nongenerative #{barrier cqxefau3fa3vz4m0-2})
  (sealed #t))

;;; make-empty-ribcage and extend-ribcage maintain list-based ribcages for
;;; internal definitions, in which the ribcages are built incrementally
(define-syntax make-empty-ribcage
  (syntax-rules ()
    ((_) (make-extensible-ribcage '()))))

(define extend-ribcage!
 ; must receive ids with complete wraps
 ; ribcage guaranteed to be extensible
  (lambda (ribcage id label/pl)
    (let ([sym (unannotate (syntax-object-expression id))]
          [chunks (extensible-ribcage-chunks ribcage)])
      (let ([ht (if (and (pair? chunks) (symbol-hashtable? (car chunks)))
                    (car chunks)
                    (let ([ht (make-hashtable symbol-hash eq?)])
                      (extensible-ribcage-chunks-set! ribcage (cons ht chunks))
                      ht))])
        (let ([a (symbol-hashtable-cell ht sym '())])
          (set-cdr! a (cons (cons (wrap-marks (syntax-object-wrap id)) label/pl) (cdr a))))))))

(define import-extend-ribcage!
 ; must receive resolved ids
 ; ribcage guaranteed to be extensible
  (lambda (ribcage id)
    (extend-ribcage! ribcage id (resolved-id->label/pl id))))

(define extend-ribcage-barrier!
 ; must receive ids with complete wraps
 ; ribcage guaranteed to be extensible
  (lambda (ribcage killer-id)
    (extensible-ribcage-chunks-set! ribcage
      (cons (make-barrier (wrap-marks (syntax-object-wrap killer-id)))
            (extensible-ribcage-chunks ribcage)))))

(define extend-ribcage-subst!
 ; ribcage guaranteed to be extensible
  (lambda (ribcage import-iface)
    (extensible-ribcage-chunks-set! ribcage
      (cons import-iface (extensible-ribcage-chunks ribcage)))))

(define lookup-global-label/pl
  (lambda (sym marks token)
    (let ([new (get-global-substs sym token)])
      (or (and new
            (let f ([new new])
              (cond
                [(pair? new) (or (f (car new)) (f (cdr new)))]
                [(symbol? new)
                 (and (same-marks? marks (wrap-marks top-wrap)) new)]
                [(same-marks? marks (wrap-marks (syntax-object-wrap new))) (resolved-id->label/pl new)]
                [else #f])))
          (and (eq? token '*system*)
               (same-marks? marks (wrap-marks top-wrap))
               (or ($sgetprop sym '*flags* #f) (eq? (subset-mode) 'system))
               sym)))))

(define lookup-global-label
  (lambda (sym marks token)
    (label/pl->label (lookup-global-label/pl sym marks token))))

(define store-global-subst
  (lambda (id token new-marks)
    (define cons-id
      (lambda (id x)
        (if (not x) id (cons id x))))
    (define weed
      (lambda (marks x)
        (if (pair? x)
            (if (same-marks? (id-marks (car x)) marks)
                (weed marks (cdr x))
                (cons-id (car x) (weed marks (cdr x))))
            (and x (not (same-marks? (id-marks x) marks)) x))))
    (let ([id (if (null? new-marks)
                  id
                  (make-syntax-object (id-sym-name id)
                    (make-wrap
                      (join-marks new-marks (id-marks id))
                      (id-subst id))))])
      (let ((sym (id-sym-name id)))
        (update-global-substs! sym token
          (lambda (old-substs)
           ; substs is an improper list of ids (symbols or resolved ids)
           ; if a subst is a symbol, label, it abbreviates an id whose sym-name is
           ; sym, whose marks are the top marks, and whose label is label.
            (let ([marks (id-marks id)])
             ; remove existing subst for same name and marks, if any
              (let ([x (weed marks old-substs)])
                (if (and (same-marks? marks (wrap-marks top-wrap))
                         (or (symbol? id) (null? (resolved-id->pl id))))
                   ; need full id only if more than top-marked
                    (let ([label (if (symbol? id) id (resolved-id->label id))])
                     ; need binding only if it's not already implicit
                     ; keep in sync with lookup-global-label/pl
                      (if (and (eq? token '*system*)
                               (eq? label sym)
                               (or ($sgetprop sym '*flags* #f) (eq? (subset-mode) 'system)))
                          x
                          (cons-id label x)))
                    (cons-id id x))))))))))

;;; make-binding-wrap creates vector-based ribcages
(define make-binding-wrap
  (lambda (ids labels w)
    (if (null? ids)
        w
        (make-wrap
          (wrap-marks w)
          (cons
            (let ((labelvec (list->vector labels)))
              (let ((n (vector-length labelvec)))
                (let ((symnamevec (make-vector n)) (marksvec (make-vector n)))
                  (let f ((ids ids) (i 0))
                    (unless (null? ids)
                      (let-values ([(symname marks) (id-sym-name&marks (car ids) w)])
                        (vector-set! symnamevec i symname)
                        (vector-set! marksvec i marks)
                        (f (cdr ids) (fx+ i 1)))))
                  (make-fixed-ribcage symnamevec marksvec labelvec))))
            (wrap-subst w))))))

;;; resolved ids contain no unnecessary substitutions or marks.  they are
;;; used essentially as indirects or aliases in modules interfaces.
(define make-resolved-id
  (lambda (sym marks label/pl)
   ; make sure gensym is visible in the oblist to copy-environment
    (when (gensym? sym) (gensym->unique-string sym))
    (make-syntax-object sym
      (make-wrap marks
        (list (make-fixed-ribcage (vector sym) (vector marks) (vector label/pl)))))))

(define resolved-id->label/pl
  (lambda (id)
    (vector-ref
      (fixed-ribcage-label/pls (car (wrap-subst (syntax-object-wrap id))))
      0)))

(define resolved-id->label
  (lambda (id)
    (label/pl->label
      (resolved-id->label/pl id))))

(define resolved-id->pl
  (lambda (id)
    (label/pl->pl
      (resolved-id->label/pl id))))

;;; Scheme's append should not copy the first argument if the second is
;;; empty, but it does, so we define a smart version here.
(define smart-append
  (lambda (m1 m2)
    (if (null? m2)
        m1
        (append m1 m2))))

;;; should tool to see what kinds of inputs we get
(define join-wraps
  (lambda (w1 w2)
    (let ((m1 (wrap-marks w1)) (s1 (wrap-subst w1)))
      (if (null? m1)
          (if (null? s1)
              w2
              (make-wrap
                (wrap-marks w2)
                (join-subst s1 (wrap-subst w2))))
          (make-wrap
            (join-marks m1 (wrap-marks w2))
            (join-subst s1 (wrap-subst w2)))))))

(define join-marks
  (lambda (m1 m2)
    (smart-append m1 m2)))

(define join-subst
  (lambda (s1 s2)
    (smart-append s1 s2)))

(define same-mark? eq?)

(define same-marks?
  (lambda (x y)
    (or (eq? x y)
        (and (not (null? x))
             (not (null? y))
             (same-mark? (car x) (car y))
             (same-marks? (cdr x) (cdr y))))))

(module (top-id-bound-label top-id-free-label/pl top-id-free-label)
  ;; top-id-bound-label is used to establish new top-level substitutions,
  ;; while top-id-free-label is used to look up existing
  ;; (possibly implicit) substitutions.  Implicit substitutions exist
  ;; for top-marked names in all mutable environments, but we represent
  ;; them explicitly only on demand.
  ;;
  ;; top-id-free-label first looks for an existing substitution for sym
  ;; and the given marks.  Otherwise, it extends the specified top-level
  ;; environment.  top-id-bound-label directly extends the specified
  ;; top-level environment.
  ;;
  ;; For top-id-bound-label, we extend the environment with a substitution
  ;; keyed by the given marks, so that top-level definitions introduced by
  ;; a macro are distinct from other top-level definitions for the same
  ;; name.  For example, if macros a and b both introduce definitions and
  ;; bound references to identifier x, the two x's should be different,
  ;; i.e., keyed by their own marks.
  ;;
  ;; For top-id-free-label, we extend the environment with a substitution
  ;; keyed by the top marks, since top-level free identifier references
  ;; should refer to the existing implicit (top-marked) substitution.  For
  ;; example, if macros a and b both introduce free references to identifier
  ;; x, they should both refer to the same (global, unmarked) x.

  (define top-id-bound-label
   ; should be called only when top-ribcage is mutable
    (lambda (sym marks top-ribcage)
      (let ([token (top-ribcage-key top-ribcage)])
        (let ([label (or (and (eq? token '*system*)
                              (same-marks? marks (wrap-marks top-wrap))
                              sym)
                         (if (same-marks? marks (wrap-marks top-wrap))
                             (make-token:sym token sym)
                             (generate-id sym)))])
          (let ([id (make-resolved-id sym marks label)])
            (store-global-subst id token '())
            (values label id))))))

  (define top-id-free-label/pl
    (lambda (sym marks top-ribcage)
      (let ([token (top-ribcage-key top-ribcage)])
        (or (lookup-global-label/pl sym marks token)
            (and (top-ribcage-mutable? top-ribcage)
                 (same-marks? marks (wrap-marks top-wrap))
                 (let ([label (make-token:sym token sym)])
                   (let ([id (make-resolved-id sym marks label)])
                     (store-global-subst id token '())
                     label)))))))

  (define top-id-free-label
    (lambda (sym marks top-ribcage)
      (label/pl->label (top-id-free-label/pl sym marks top-ribcage)))))

(define iface-id->label/pl
  (lambda (sym marks iface new-marks)
    (let loop ([marks marks] [new-marks new-marks])
      (if (null? new-marks)
          (let loop ([ls (symbol-hashtable-ref (interface-ht iface) sym '())])
            (and (not (null? ls))
              (if (same-marks? (caar ls) marks)
                  (cdar ls)
                  (loop (cdr ls)))))
          (and (not (null? marks))
               (same-mark? (car marks) (car new-marks))
               (loop (cdr marks) (cdr new-marks)))))))

(define iface-id->label
  (lambda (sym marks iface new-marks)
    (label/pl->label (iface-id->label/pl sym marks iface new-marks))))

(define-syntax make-id->label/pl
  (syntax-rules ()
    [(_ k?)
     (let ()
       (define-syntax return
         (if k?
             (syntax-rules () [(_ label retry) (values label retry)])
             (syntax-rules () [(_ label retry) label])))
       (define kfailed
         (lambda ()
           ($oops 'sc-expand "internal error in id->label/pl: attempt to continue failed search")))
       (define search
         (lambda (sym subst marks)
           (if (null? subst)
               (return #f kfailed)
               (let ([fst (car subst)])
                 (cond
                   [(eq? fst 'shift) (search sym (cdr subst) (cdr marks))]
                   [(fixed-ribcage? fst) (search-fixed-ribcage sym subst marks fst)]
                   [(extensible-ribcage? fst) (search-extensible-ribcage sym subst marks fst)]
                   [(top-ribcage? fst)
                    (cond
                      [(top-id-free-label/pl sym marks fst) =>
                       (lambda (label/pl)
                         (return label/pl
                           (lambda () (search sym (cdr subst) marks))))]
                      [else (search sym (cdr subst) marks)])]
                   [else
                    ($oops 'sc-expand
                      "internal error in id->label/pl: improper subst ~s"
                      subst)])))))
       (define search-fixed-ribcage
         (lambda (sym subst marks ribcage)
           (let ([symnames (fixed-ribcage-symnames ribcage)])
             (let ((n (vector-length symnames)))
               (let f ((i 0))
                 (cond
                   ((fx= i n) (search sym (cdr subst) marks))
                   ((and (eq? (vector-ref symnames i) sym)
                         (same-marks? marks (vector-ref (fixed-ribcage-marks ribcage) i)))
                    (return (vector-ref (fixed-ribcage-label/pls ribcage) i)
                      (lambda () (f (fx+ i 1)))))
                   (else (f (fx+ i 1)))))))))
       (define search-extensible-ribcage
         (lambda (sym subst marks ribcage)
           (let f ([chunks (extensible-ribcage-chunks ribcage)])
             (if (null? chunks)
                 (search sym (cdr subst) marks)
                 (let ([chunk (car chunks)])
                   (cond
                     [(symbol-hashtable? chunk)
                      (let g ([ls (symbol-hashtable-ref chunk sym '())])
                        (if (null? ls)
                            (f (cdr chunks))
                            (if (same-marks? marks (caar ls))
                                (return (cdar ls)
                                  (lambda () (f (cdr chunks))))
                                (g (cdr ls)))))]
                     [(import-interface? chunk)
                      (cond
                        [(iface-id->label/pl sym marks
                           (get-indirect-interface (import-interface-interface chunk))
                           (import-interface-new-marks chunk)) =>
                         (lambda (label/pl)
                           (return label/pl
                             (lambda () (f (cdr chunks)))))]
                        [else (f (cdr chunks))])]
                     [(barrier? chunk)
                      (if (same-marks? marks (barrier-marks chunk))
                          (return #f kfailed)
                          (f (cdr chunks)))]
                     [else ($oops 'sc-expand "internal error in search-extensible-ribcage: unexpected chunk ~s" chunk)]))))))
       (lambda (id w)
         (cond
           [(symbol? id) (search id (wrap-subst w) (wrap-marks w))]
           [(syntax-object? id)
            (let ([w1 (syntax-object-wrap id)])
              (search (unannotate (syntax-object-expression id))
                (join-subst (wrap-subst w) (wrap-subst w1))
                (join-marks (wrap-marks w) (wrap-marks w1))))]
           [(annotation? id) (search (unannotate id) (wrap-subst w) (wrap-marks w))]
           [else ($oops 'sc-expand "internal error in id->label/pl: invalid id ~s" id)])))]))

(define id->label/pl (make-id->label/pl #f))
(define id->label/pl/retry (make-id->label/pl #t))

(define id->label
  (lambda (id w)
    (label/pl->label (id->label/pl id w))))

;;; free-id=? must be passed fully wrapped ids since (free-id=? x y)
;;; may be true even if (free-id=? (wrap x w) (wrap y w)) is not.

(define free-id=?
  (lambda (i j)
    (let ([x (id->label i empty-wrap)])
      (and (eq? x (id->label j empty-wrap))
           (if (eq? x #f) (eq? (id-sym-name i) (id-sym-name j)) #t)))))

;;; bound-id=? may be passed unwrapped (or partially wrapped) ids as
;;; long as the missing portion of the wrap is common to both of the ids
;;; since (bound-id=? x y) iff (bound-id=? (wrap x w) (wrap y w))

(define bound-id=?
  (lambda (i j)
    (and (eq? (id-sym-name i) (id-sym-name j))
         (same-marks? (id-marks i) (id-marks j)))))

;;; "valid-bound-ids?" returns #t if it receives a list of distinct ids.
;;; valid-bound-ids? may be passed unwrapped (or partially wrapped) ids
;;; as long as the missing portion of the wrap is common to all of the
;;; ids.

(define valid-bound-ids?
  (lambda (ids)
     (and (let all-ids? ((ids ids))
            (or (null? ids)
                (and (id? (car ids))
                     (all-ids? (cdr ids)))))
          (distinct-bound-ids? ids))))

;;; distinct-bound-ids? expects a list of ids and returns #t if there are
;;; no duplicates.  It is quadratic on the length of the id list; long
;;; lists could be sorted to make it more efficient.  distinct-bound-ids?
;;; may be passed unwrapped (or partially wrapped) ids as long as the
;;; missing portion of the wrap is common to all of the ids.

(define distinct-bound-ids?
  (lambda (ids)
    (let distinct? ((ids ids))
      (or (null? ids)
          (and (not (bound-id-member? (car ids) (cdr ids)))
               (distinct? (cdr ids)))))))

(define invalid-ids-error
 ; find first bad one and complain about it
  (lambda (ids exp class)
    (let find ((ids ids) (gooduns '()))
      (if (null? ids)
          (syntax-error exp) ; shouldn't happen
          (if (id? (car ids))
              (if (bound-id-member? (car ids) gooduns)
                  (syntax-error exp
                    (format "duplicate ~a ~s in" class
                      (strip (car ids) empty-wrap)))
                  (find (cdr ids) (cons (car ids) gooduns)))
              (syntax-error exp
                (format "invalid ~a ~s in" class
                  (strip (car ids) empty-wrap))))))))

(define bound-id-member?
   (lambda (x list)
      (and (not (null? list))
           (or (bound-id=? x (car list))
               (bound-id-member? x (cdr list))))))

;;; symbolic-id=? ignores the wrap and compares the names

(define symbolic-id=?
  (lambda (id sym)
    (eq? (id-sym-name id) sym)))

(define-syntax sym-kwd?
  (syntax-rules ()
    [(_ id kwd ...)
     (and (id? #'id)
          (or (symbolic-id=? #'id 'kwd) ...))]))

;;; wrapping expressions and identifiers

(define wrap
  (lambda (x w)
    (cond
      ((and (null? (wrap-marks w)) (null? (wrap-subst w))) x)
      ((syntax-object? x)
       (make-syntax-object
         (syntax-object-expression x)
         (join-wraps w (syntax-object-wrap x))))
      ((null? x) x)
      (else (make-syntax-object x w)))))

(define source-wrap
  (lambda (x w ae)
    (wrap (if (annotation? ae)
              (if (eq? (annotation-expression ae) x)
                  ae
                  (make-annotation x (annotation-source ae) (strip x w) (annotation-flags ae)))
              x)
          w)))

;;; expanding

(define chi-when-list
  (lambda (when-list w)
    ; when-list is syntax'd version of list of situations
    (map (lambda (x)
           (cond
             [(free-id=? x #'compile) 'compile]
             [(free-id=? x #'load) 'load]
             [(free-id=? x #'visit) 'visit]
             [(free-id=? x #'revisit) 'revisit]
             [(free-id=? x #'eval) 'eval]
            ; be kind in case, say, load has been traced or redefined
            ; at top level...there's nothing else these could be anyway
            ; but do these tests after above in case some clown has
            ; aliased, say, eval to compile
             [(symbolic-id=? x 'compile) 'compile]
             [(symbolic-id=? x 'load) 'load]
             [(symbolic-id=? x 'visit) 'visit]
             [(symbolic-id=? x 'revisit) 'revisit]
             [(symbolic-id=? x 'eval) 'eval]
             [else (syntax-error (wrap x w) "invalid eval-when situation")]))
         (map (lambda (x) (wrap x w)) when-list))))

;;; syntax-type returns five values: type, value, e, w, and ae.  The first
;;; two are described in the table below.
;;;
;;;    type                    value              explanation
;;;    -------------------------------------------------------------------
;;;    alias-form              none               alias expression
;;;    alias                   none               alias keyword
;;;    begin-form              none               begin expression
;;;    begin                   none               begin keyword
;;;    call                    none               any other call
;;;    constant                none               self-evaluating datum
;;;    core                    procedure          core form (including singleton)
;;;    ctv                     ctv record         user-defined compile-time value
;;;    define-form             none               variable definition
;;;    define                  none               define keyword
;;;    define-property-form    none               property definition
;;;    define-property         none               define-property keyword
;;;    define-syntax-form      none               syntax definition
;;;    define-syntax           none               define-syntax keyword
;;;    displaced-lexical       none               displaced lexical identifier
;;;    eval-when-form          none               eval-when form
;;;    eval-when               none               eval-when keyword
;;;    export-form             none               export form
;;;    export                  none               export keyword
;;;    global                  name               global variable reference
;;;    immutable-global        name               immutable global variable reference (created by copy-environment)
;;;    implicit-exports-form   none               implicit-exports form
;;;    implicit-exports        none               implicit-exports keyword
;;;    $import-form            none               $import form
;;;    $import                 none               $import keyword
;;;    indirect-export-form    none               indirect-export form
;;;    indirect-export         none               indirect-export keyword
;;;    lexical                 var                lexical variable reference
;;;    $library-form           name               $library definition
;;;    library-global          (uid . name)       immutable library variable reference
;;;    library-meta-global     (uid . name)       library meta variable
;;;    $library                name               $library keyword
;;;    local-syntax-form       rec?               syntax definition
;;;    local-syntax            rec?               letrec-syntax/let-syntax keyword
;;;    meta-form               none               meta form
;;;    meta                    none               meta keyword
;;;    meta-variable           name               meta variable
;;;    $module-form            none               $module definition
;;;    $module                 none               $module keyword
;;;    primitive               name               primitive reference
;;;    $program-form           name               $program definition
;;;    $program                name               $program keyword
;;;    syntax                  level              pattern variable
;;;    other                   none               anything else
;;;
;;; For all forms, e is the form, w is the wrap for e. and ae is the
;;; (possibly) source-annotated form.
;;;
;;; syntax-type expands macros and unwraps as necessary to get to
;;; one of the forms above.

(define syntax-type
  (lambda (e r w ae rib)
    (cond
      [(pair? e)
       (let ([first (car e)])
         (if (id? first)
             (let* ([b (lookup (id->label first w) r)]
                    [type (binding-type b)])
               (case type
                 [(macro macro!)
                  (syntax-type (chi-macro (binding-value b) e r w ae rib)
                    r empty-wrap ae rib)]
                 [(core) (values type (binding-value b) e w ae)]
                 [(begin) (values 'begin-form #f e w ae)]
                 [(alias) (values 'alias-form #f e w ae)]
                 [(define) (values 'define-form #f e w ae)]
                 [(define-syntax) (values 'define-syntax-form #f e w ae)]
                 [(define-property) (values 'define-property-form #f e w ae)]
                 [(set!) (chi-set! e r w ae rib)]
                 [($library-key) (values '$library-form #f e w ae)]
                 [($program-key) (values '$program-form #f e w ae)]
                 [($module-key) (values '$module-form #f e w ae)]
                 [($import) (values '$import-form #f e w ae)]
                 [(export) (values 'export-form #f e w ae)]
                 [(indirect-export) (values 'indirect-export-form #f e w ae)]
                 [(implicit-exports) (values 'implicit-exports-form #f e w ae)]
                 [(eval-when) (values 'eval-when-form #f e w ae)]
                 [(meta) (values 'meta-form #f e w ae)]
                 [(local-syntax) (values 'local-syntax-form (binding-value b) e w ae)]
                 [else (values 'call #f e w ae)]))
             (values 'call #f e w ae)))]
      [(syntax-object? e)
       (syntax-type (syntax-object-expression e) r
         (join-wraps w (syntax-object-wrap e)) #f rib)]
      [(annotation? e)
       (syntax-type (annotation-expression e) r w e rib)]
      [(symbol? e)
       (let* ([b (lookup (id->label e w) r)]
              [type (binding-type b)])
         (case type
           [(macro macro!)
            (syntax-type (chi-macro (binding-value b) e r w ae rib)
              r empty-wrap ae rib)]
           [else (values type (binding-value b) e w ae)]))]
      [(and (self-evaluating-vectors) (vector? e))
       (values 'constant #f (vector-map (lambda (e) (strip e w)) e) w ae)]
      [(self-evaluating? e) (values 'constant #f e w ae)]
      [else (values 'other #f e w ae)])))

(define chi-top*
  (lambda (e w ctem rtem top-ribcage outfn)
    (define complaining-library-collector
      (lambda (what)
        (lambda (uid)
          ($oops 'sc-expand-internal "somebody didn't capture ~s requirement for ~s" what uid))))
    (fluid-let ([require-import (library-collector #f)]
                [require-include (include-collector)]
                [require-invoke (complaining-library-collector 'invoke)]
                [require-visit (complaining-library-collector 'visit)])
      (let ([ribcage (make-empty-ribcage)])
        (let ([w (make-wrap (wrap-marks w) (cons ribcage (wrap-subst w)))])
          (let ([code* (chi-top (list (make-frob (wrap e w) #f)) (make-rho) ctem rtem ribcage top-ribcage outfn)])
            (build-group
              (if (and (not (eq? (subset-mode) 'system))
                       (or (or (memq 'L ctem) (memq 'R ctem) (memq 'V ctem))
                           (or (memq 'L rtem) (memq 'R rtem) (memq 'V rtem))))
                  (cons (build-recompile-info (require-import) (require-include)) code*)
                  code*))))))))

(define chi-top
  (lambda (frob* r ctem rtem ribcage top-ribcage outfn)
    (define-datatype bodit
      (define id label binding rhs ae)
      (system-define label rhs ae)
      (meta-define id label binding expr import* visit* invoke*)
      (define-syntax id binding rhs import* visit* invoke*)
      (define-property id association propval propvalexpr import* visit* invoke*)
      (import mid imps import*)
      (alias id)
      (module id iface)
      (code c)
      (meta-eval expr import* visit* invoke*)
      (init expr))
    (define chi-begin-body
      (lambda (frob*)
       ; when we extend r here, we always use level -1, since all top-level
       ; bindings are visible at all meta levels
        (let parse ([frob* frob*] [bf* '()] [meta-seen? #f] [label* '()])
          (if (null? frob*)
              (values (reverse bf*) label*)
              (let* ([fr (car frob*)] [e (frob-e fr)] [meta? (frob-meta? fr)])
                (let-values ([(type value e w ae) (syntax-type e r empty-wrap no-source ribcage)])
                  (case type
                    [(define-form)
                     (let-values ([(id rhs w ae) (parse-define e w ae)])
                       (let ([id (wrap id w)])
                         (when (displaced-lexical? id r) (displaced-lexical-error id "define" #f))
                         (unless (or (top-ribcage-mutable? top-ribcage) (eq? (subset-mode) 'system))
                           (syntax-error (source-wrap e w ae)
                             "invalid definition in immutable environment"))
                         (let ([sym (id-sym-name id)])
                           (if (and (eq? (subset-mode) 'system)
                                    (not meta?)
                                    (eq? (top-ribcage-key top-ribcage) '*system*)
                                    (only-top-marked? id))
                               (begin
                                 (extend-ribcage! ribcage id sym)
                                 (parse (cdr frob*)
                                   (cons (bodit-system-define sym (wrap rhs w) ae) bf*)
                                   #f label*))
                               (let-values ([(label bound-id)
                                             (let ([marks (wrap-marks (syntax-object-wrap id))])
                                               (cond
                                                 [(and (not (equal? marks (wrap-marks top-wrap)))
                                                       (let ([label (lookup-global-label sym marks (top-ribcage-key top-ribcage))])
                                                         (and label
                                                              (let ([b (lookup-global label)])
                                                                (and (eq? (binding-type b) 'global)
                                                                     (eq? (binding-value b) label)))
                                                              label))) =>
                                                  (lambda (label) (values label (make-resolved-id sym marks label)))]
                                                 [else (top-id-bound-label sym marks top-ribcage)]))])
                                 (extend-ribcage! ribcage id label)
                                 (unless (eq? (id->label id empty-wrap) label)
                                  ; must be an enclosing local-syntax binding for id
                                   (syntax-error (source-wrap e w ae)
                                     "definition not permitted"))
                                 (if meta?
                                     (let ([b (make-binding 'meta-variable label)])
                                       (extend-rho! r label b (fxlognot 0))
                                      ; chi rhs after establishing lhs mapping to label to allow
                                      ; recursive meta definitions.
                                       (fluid-let ([require-import (propagating-library-collector require-import #f)]
                                                   [require-invoke (library-collector #t)]
                                                   [require-visit (library-collector #f)])
                                         (let ([exp (not-at-top (meta-chi rhs r w))])
                                           (define-top-level-value-hook label (top-level-eval-hook exp))
                                           (parse (cdr frob*)
                                             (cons (bodit-meta-define bound-id label b exp (require-import) (require-visit) (require-invoke)) bf*)
                                             #f label*))))
                                     (let ([b (make-binding 'global label)])
                                       (extend-rho! r label b (fxlognot 0))
                                       (parse (cdr frob*)
                                         (cons (bodit-define bound-id label b (wrap rhs w) ae) bf*)
                                         #f label*))))))))]
                    [(define-syntax-form)
                     (let-values ([(id rhs w) (parse-define-syntax e w ae)])
                       (let ([id (wrap id w)])
                         (when (displaced-lexical? id r) (displaced-lexical-error id "define" #f))
                         (unless (or (top-ribcage-mutable? top-ribcage) (eq? (subset-mode) 'system))
                           (syntax-error (source-wrap e w ae)
                             "invalid definition in immutable environment"))
                         (let-values ([(label bound-id)
                                       (top-id-bound-label (id-sym-name id)
                                         (wrap-marks (syntax-object-wrap id))
                                         top-ribcage)])
                           (extend-ribcage! ribcage id label)
                           (unless (eq? (id->label id empty-wrap) label)
                            ; must be an enclosing local-syntax binding for id
                             (syntax-error (source-wrap e w ae)
                               "definition not permitted"))
                           (fluid-let ([require-import (propagating-library-collector require-import #f)]
                                       [require-invoke (library-collector #t)]
                                       [require-visit (library-collector #f)])
                             (let ([exp (not-at-top (meta-chi rhs r w))])
                               (let ([b (defer-or-eval-transformer 'define-syntax top-level-eval-hook exp)])
                                 (extend-rho! r label b (fxlognot 0))
                                 (parse (cdr frob*)
                                   (cons (bodit-define-syntax bound-id b exp (require-import) (require-visit) (require-invoke)) bf*)
                                   #f label*)))))))]
                    [(define-property-form)
                     (let-values ([(id key-id expr w) (parse-define-property e w ae)])
                       (let* ([id (wrap id w)]
                              [id-label/pl (id->label/pl id empty-wrap)]
                              [key-id-label (id->label key-id w)]
                              [prop-label (gen-global-label (id-sym-name id))])
                         (unless (or (top-ribcage-mutable? top-ribcage) (eq? (subset-mode) 'system))
                           (syntax-error (source-wrap e w ae)
                             "invalid definition in immutable environment"))
                         (unless id-label/pl (syntax-error id "no visible binding for define-property id"))
                         (unless key-id-label (syntax-error (wrap key-id w) "no visible binding for define-property key"))
                         (let* ([id-label (label/pl->label id-label/pl)]
                                [id-label/pl (make-label/pl id-label (cons key-id-label prop-label) (label/pl->pl id-label/pl))])
                           (extend-ribcage! ribcage id id-label/pl)
                           (unless (eq? (id->label id empty-wrap) id-label)
                            ; must be an enclosing local-syntax binding for id
                             (syntax-error (source-wrap e w ae)
                               "definition not permitted"))
                           (fluid-let ([require-import (propagating-library-collector require-import #f)]
                                       [require-invoke (library-collector #t)]
                                       [require-visit (library-collector #f)])
                             (let* ([propvalexpr (not-at-top (meta-chi expr r w))]
                                    [propval (top-level-eval-hook propvalexpr)]
                                    [binding (make-binding 'property propval)])
                               (extend-rho! r prop-label binding (fxlognot 0))
                               (parse (cdr frob*)
                                 (cons
                                   (bodit-define-property (make-resolved-id (id-sym-name id) (id-marks id) id-label/pl)
                                     (cons key-id-label prop-label) propval propvalexpr (require-import) (require-visit) (require-invoke))
                                   bf*)
                                 #f label*))))))]
                    [($import-form)
                     (let-values ([(orig impspec* only? std?) (parse-import e w ae)])
                      ; silently ignore only? and treat top-level import-only like import
                       (define process-impspecs
                         (lambda (impspec*)
                           (if (null? impspec*)
                               (begin
                                 (unless (or (top-ribcage-mutable? top-ribcage) (eq? (subset-mode) 'system))
                                   (syntax-error orig
                                     "invalid definition in immutable environment"))
                                 bf*)
                               (fluid-let ([require-import (propagating-library-collector require-import #f)]
                                           [require-visit (library-collector #f)])
                                 (let-values ([(mid tid imps) (determine-imports (car impspec*) r std?)])
                                   (let ([bf* (process-impspecs (cdr impspec*))])
                                     (if (import-interface? imps)
                                         (extend-ribcage-subst! ribcage imps)
                                         (for-each (lambda (id) (import-extend-ribcage! ribcage id)) imps))
                                     (cons (bodit-import mid imps (require-import)) bf*)))))))
                       (parse (cdr frob*) (process-impspecs impspec*) #f label*))]
                    [(export-form)
                     (parse-export e w ae)
                     (syntax-error (source-wrap e w ae) "export form outside of a module or library")]
                    [(indirect-export-form)
                     (parse-indirect-export e w ae)
                     (parse (cdr frob*) bf* #f label*)]
                    [(implicit-exports-form)
                     (parse-implicit-exports e w ae)
                     (syntax-error (source-wrap e w ae) "implicit-exports form outside of a module or library")]
                    [(alias-form)
                     (let-values ([(new-id old-id) (parse-alias e w ae)])
                       (let ([new-id (wrap new-id w)]
                             [label/pl (id->label/pl old-id w)])
                         (when (displaced-lexical? new-id r) (displaced-lexical-error new-id "define" #f))
                         (unless (or (top-ribcage-mutable? top-ribcage) (eq? (subset-mode) 'system))
                           (syntax-error (source-wrap e w ae)
                             "invalid definition in immutable environment"))
                         (extend-ribcage! ribcage new-id label/pl)
                         (unless (eq? (id->label new-id empty-wrap) (label/pl->label label/pl))
                          ; must be an enclosing local-syntax binding for new-id
                           (syntax-error (source-wrap e w ae) "definition not permitted"))
                         (parse (cdr frob*)
                           (let ([id (make-resolved-id (id-sym-name new-id) (wrap-marks (syntax-object-wrap new-id)) label/pl)])
                             (cons (bodit-alias id) bf*))
                           #f label*)))]
                    [(begin-form)
                     (parse (append (map (lambda (e) (make-frob (wrap e w) meta?)) (parse-begin e w ae #t))
                                    (cdr frob*))
                       bf* #f label*)]
                    [(meta-form)
                     (parse (cons (make-frob (wrap (parse-meta e w ae) w) #t) (cdr frob*))
                       bf* #t label*)]
                    [(local-syntax-form)
                     (fluid-let ([require-invoke (library-collector #t)]
                                 [require-visit (library-collector #f)])
                       (let-values ([(forms w ae new-label*) (chi-local-syntax value #t e r w ae)])
                         (parse (let f ([forms forms])
                                  (if (null? forms)
                                      (cdr frob*)
                                      (cons (make-frob (wrap (car forms) w) meta?)
                                            (f (cdr forms)))))
                           bf* #f (append new-label* label*))))]
                    [($module-form)
                     (let ([new-ribcage (make-empty-ribcage)])
                       (let-values ([(orig id forms)
                                     (parse-module e w ae
                                       (make-wrap (wrap-marks w) (cons new-ribcage (wrap-subst w))))])
                         (when (displaced-lexical? id r) (displaced-lexical-error (wrap id w) "define" #f))
                         (unless (or (top-ribcage-mutable? top-ribcage) (eq? (subset-mode) 'system))
                           (syntax-error orig
                             "invalid definition in immutable environment"))
                         (fluid-let ([require-import (propagating-library-collector require-import #f)]
                                     [require-invoke (library-collector #t)]
                                     [require-visit (library-collector #f)])
                           (let-values ([(code* iface-vector)
                                         (chi-top-module orig r top-ribcage new-ribcage ctem rtem meta? id forms)])
                             (let-values ([(label bound-id)
                                           (top-id-bound-label (id-sym-name id)
                                             (wrap-marks (syntax-object-wrap id))
                                             top-ribcage)])
                               (extend-ribcage! ribcage id label)
                               (unless (eq? (id->label id empty-wrap) label)
                                ; must be an enclosing local-syntax binding for id
                                 (syntax-error orig "definition not permitted"))
                               (let ([iface (make-interface (wrap-marks (syntax-object-wrap id)) iface-vector)])
                                 (let ([b (make-binding '$module iface)])
                                   (extend-rho! r label b (fxlognot 0))
                                   (parse (cdr frob*)
                                     (cons*
                                       (bodit-module bound-id iface)
                                       (bodit-code
                                         (build-group
                                           `(,@(map (build-requirement '$import-library) (require-import))
                                             ,@(map (build-requirement '$invoke-library) (require-invoke))
                                             ,@(map (build-requirement '$visit-library) (require-visit))
                                             ,@code*)))
                                       bf*)
                                     #f label*))))))))]
                    [($library-form)
                     (parse (cdr frob*)
                       (let ([ribcage (make-empty-ribcage)])
                         (let-values ([(orig library-path library-version uid tid forms)
                                       (parse-library e w ae (make-wrap (wrap-marks w) (cons ribcage (wrap-subst w))))])
                           (install-library library-path uid #f)
                           (on-reset (uninstall-library library-path uid)
                             (unless (or (top-ribcage-mutable? top-ribcage) (eq? (subset-mode) 'system))
                               (syntax-error orig "invalid definition in immutable environment"))
                             (extend-ribcage-barrier! ribcage tid)
                             (cons (bodit-code
                                     (chi-top-library orig library-path library-version r top-ribcage ribcage
                                       ctem rtem uid tid forms outfn))
                               bf*))))
                       #f label*)]
                    [($program-form)
                     (parse (cdr frob*)
                       (let ([ribcage (make-empty-ribcage)])
                         (let-values ([(orig tid forms)
                                       (parse-program e w ae (make-wrap (wrap-marks w) (cons ribcage (wrap-subst w))))])
                           (unless (or (top-ribcage-mutable? top-ribcage) (eq? (subset-mode) 'system))
                             (syntax-error orig "invalid definition in immutable environment"))
                           (extend-ribcage-barrier! ribcage tid)
                           (cons (bodit-code (chi-top-program ae orig r top-ribcage ribcage rtem forms))
                                 bf*)))
                       #f label*)]
                    [(eval-when-form)
                     (parse (cdr frob*)
                       (let-values ([(when-list e*) (parse-eval-when e w ae)])
                         (let ([ctem (update-mode-set when-list ctem)]
                               [rtem (update-mode-set when-list rtem)])
                           (cons (bodit-code
                                   (build-group
                                     (if (and (null? ctem) (null? rtem))
                                         '()
                                         (chi-top (map (lambda (e) (make-frob (wrap e w) meta?)) e*)
                                           r ctem rtem ribcage top-ribcage outfn))))
                             bf*)))
                       #f label*)]
                    [else ; found an init expression
                     (let ([e (source-wrap e w ae)])
                       (when meta-seen? (syntax-error e "invalid meta definition"))
                       (parse (cdr frob*)
                         (cons
                           (if meta?
                               (fluid-let ([require-import (propagating-library-collector require-import #f)]
                                           [require-invoke (library-collector #t)]
                                           [require-visit (library-collector #f)])
                                 (let ([exp (not-at-top (meta-chi e r empty-wrap))])
                                   (top-level-eval-hook exp)
                                   (bodit-meta-eval exp (require-import) (require-visit) (require-invoke))))
                               (bodit-init e))
                           bf*)
                         #f label*))])))))))
    (let-values ([(bf* label*) (chi-begin-body frob*)])
      (let process-forms ([bf* bf*] [rcode* '()])
        (if (null? bf*)
            (begin
              (for-each kill-local-label! label*) ; just local-syntax labels, which are all local
              (reverse rcode*))
            (bodit-case (car bf*)
              [define (id label binding rhs ae)
               (process-forms (cdr bf*)
                 (cons*
                   (let ([top-token (top-ribcage-key top-ribcage)])
                     (ct-eval/residualize ctem
                       (lambda () ($sc-put-cte id binding top-token))
                       (lambda ()
                         (build-cte-install id
                           (build-data no-source binding)
                           top-token))))
                   (rt-eval/residualize rtem
                     (lambda ()
                       (fluid-let ([require-invoke (library-collector #f)]
                                   [require-visit (library-collector #f)])
                         (residualize-invoke-requirements
                           (build-global-assignment ae label
                             (not-at-top (chi rhs r empty-wrap)))))))
                   rcode*))]
              [system-define (label rhs ae)
               (process-forms (cdr bf*)
                 (cons
                   (rt-eval/residualize rtem
                     (lambda ()
                       (fluid-let ([require-invoke (library-collector #f)]
                                   [require-visit (library-collector #f)])
                         (residualize-invoke-requirements
                           (let ([rhs (not-at-top (chi rhs r empty-wrap))])
                             (if (eq? (binding-type (lookup-global label)) 'primitive)
                                 (build-primitive-assignment ae label rhs)
                                 (build-global-assignment ae label rhs)))))))
                   rcode*))]
              [define-syntax (id binding rhs import* visit* invoke*)
               (process-forms (cdr bf*)
                 (cons
                   (let ([top-token (top-ribcage-key top-ribcage)])
                     (ct-eval/residualize ctem
                       (lambda () ($sc-put-cte id binding top-token))
                       (lambda ()
                         (residualize-invoke-requirements import* visit* invoke*
                           (build-checking-cte-install id rhs top-token)))))
                   rcode*))]
              [define-property (id association propval propvalexpr import* visit* invoke*)
               (process-forms (cdr bf*)
                 (cons
                   (let ([top-token (top-ribcage-key top-ribcage)])
                     (ct-eval/residualize ctem
                       (lambda () ($sc-put-property! id association propval top-token))
                       (lambda ()
                         (residualize-invoke-requirements import* visit* invoke*
                           (build-primcall no-source 3 '$sc-put-property!
                             (build-data no-source id)
                             (build-data no-source association)
                             propvalexpr
                             (build-data no-source top-token))))))
                   rcode*))]
              [import (mid imps import*)
               (process-forms (cdr bf*)
                 (cons
                   (let ()
                     (define do-top-import
                       (lambda (mid binding)
                         (let ([top-token (top-ribcage-key top-ribcage)])
                           (ct-eval/residualize ctem
                             (lambda () ($sc-put-cte mid binding top-token))
                             (lambda ()
                               (residualize-import-requirements import*
                                 (build-cte-install mid
                                            (build-data no-source binding)
                                            top-token)))))))
                     (if (import-interface? imps)
                         (do-top-import mid
                           (make-binding 'do-import
                             (import-interface-new-marks imps)))
                         (do-top-import #f
                           (make-binding 'do-anonymous-import
                             (list->vector imps)))))
                   rcode*))]
              [alias (id)
               (process-forms (cdr bf*)
                 (cons
                   (let ([top-token (top-ribcage-key top-ribcage)]
                         [binding (make-binding 'do-alias #f)])
                     (ct-eval/residualize ctem
                       (lambda () ($sc-put-cte id binding top-token))
                       (lambda () (build-cte-install id (build-data no-source binding) top-token))))
                   rcode*))]
              [meta-define (id label binding expr import* visit* invoke*)
               (process-forms (cdr bf*)
                 (cons
                   (ct-eval/residualize ctem
                     (lambda () ($sc-put-cte label binding #f))
                     (lambda ()
                       (residualize-invoke-requirements import* visit* invoke*
                         (build-sequence no-source
                           (list
                             (build-cte-install label (build-data no-source binding) #f)
                             (build-global-assignment no-source label expr))))))
                   rcode*))]
              [meta-eval (expr import* visit* invoke*)
               (process-forms (cdr bf*)
                 (cons
                   (ct-eval/residualize ctem void
                     (lambda () (residualize-invoke-requirements import* visit* invoke* expr)))
                   rcode*))]
              [init (expr)
               (process-forms (cdr bf*)
                 (cons
                   (rt-eval/residualize rtem
                     (lambda ()
                       (fluid-let ([require-invoke (library-collector #f)]
                                   [require-visit (library-collector #f)])
                         (residualize-invoke-requirements
                           (not-at-top (chi expr r empty-wrap))))))
                   rcode*))]
              [module (id iface)
               (process-forms (cdr bf*)
                 (cons
                   (let ([top-token (top-ribcage-key top-ribcage)]
                         [binding (make-binding '$module iface)])
                     (ct-eval/residualize ctem
                       (lambda () ($sc-put-cte id binding top-token))
                       (lambda () (build-cte-install id (build-data no-source binding) top-token))))
                   rcode*))]
              [code (c) (process-forms (cdr bf*) (cons c rcode*))]))))))

(define-threaded at-top-level?)
(define-syntax at-top
  (syntax-rules ()
    [(_ e) (fluid-let ([at-top-level? #t]) e)]))
(define-syntax not-at-top
  (syntax-rules ()
    [(_ e) (fluid-let ([at-top-level? #f]) e)]))

(define-record-type interface
  (fields (immutable marks) (immutable exports) (immutable ht))
  (nongenerative #{interface u0r6vonmfvk3pe1-0})
  (sealed #t)
  (protocol
    (lambda (new)
      (lambda (marks exports)
        (new marks exports
          (let ([ht (make-hashtable symbol-hash eq?)])
            (vector-for-each
              (lambda (id)
                (let ([a (symbol-hashtable-cell ht (id-sym-name id) '())])
                  (set-cdr! a (cons (cons (id-marks id) (resolved-id->label/pl id)) (cdr a)))))
              exports)
            ht))))))

(define-datatype (mbodit (immutable meta?) (mutable exported))
  (define id label binding)
  (meta-define id label binding expr)
  (define-syntax id label binding rhs)
  (define-property id association propval propvalexpr)
  (module id label iface mb*)
  (meta-eval expr)
  (interleaved-init frob))

(define mbodit-id
  (lambda (mb)
    (mbodit-case mb
      [define (id label binding) id]
      [meta-define (id label binding expr) id]
      [define-syntax (id label binding rhs) id]
      [define-property (id association propval propvalexpr) id]
      [module (id label iface mb*) id]
      [else #f])))

;;; frobs represent body forms
(define-record-type frob
  (fields (immutable e) (immutable meta?))
  (nongenerative)
  (sealed #t))

(define-record-type progdesc
  (fields (immutable invoke-req*))
  (nongenerative #{progdesc j4n0mzzi31anj92-0})
  (sealed #t))

(define-record-type libdesc
  (fields
    (immutable path)
    (immutable version)
    (immutable outfn)        ; string if imported from or compiled to an object file, else #f
    (immutable importer)     ; string if we know why this was imported, for error messages
    (immutable system?)
    (immutable visible?)
    (immutable ctdesc)
    (immutable rtdesc))
  (nongenerative #{libdesc c9z2lszhwazzhbi56x5v5p-3})
  (sealed #t))

(define-record-type ctdesc
  (fields
    (immutable import-req*)       ; libraries imported when this library was imported
    (immutable visit-visit-req*)  ; libraries that must be visited (for meta definitions) when this library is visited
    (immutable visit-req*)        ; libraries that must be invoked (for regular definitions) when this library is visited
    (mutable loaded-import-reqs)
    (mutable loaded-visit-reqs)
    (mutable export-id*)          ; ids that need to be reset when visit-code raises an exception
    (mutable import-code)
    (mutable visit-code))
  (nongenerative #{ctdesc bthma8spr7lds76z4hlmr9-4})
  (sealed #t))

(define-record-type rtdesc
  (fields
    (immutable invoke-req*)       ; libraries that must be invoked (for regular definitions) when this library is invoked
    (mutable loaded-invoke-reqs)
    (mutable invoke-code))
  (nongenerative #{rtdesc bthtzrrbhp7w9d02grnlh7-0})
  (sealed #t))

(module (libdesc-import-req* libdesc-visit-visit-req* libdesc-visit-req*
         libdesc-loaded-import-reqs libdesc-loaded-import-reqs-set!
         libdesc-loaded-visit-reqs libdesc-loaded-visit-reqs-set!
         libdesc-import-code libdesc-import-code-set!
         libdesc-visit-code libdesc-visit-code-set!
         libdesc-visit-id* libdesc-visit-id*-set!)
  (define get-ctdesc
    (lambda (desc)
      (or (libdesc-ctdesc desc)
          ($oops #f "compile-time information for library ~s has not been loaded" (libdesc-path desc)))))
  (define libdesc-import-req*
    (lambda (desc)
      (ctdesc-import-req* (get-ctdesc desc))))
  (define libdesc-visit-visit-req*
    (lambda (desc)
      (ctdesc-visit-visit-req* (get-ctdesc desc))))
  (define libdesc-visit-req*
    (lambda (desc)
      (ctdesc-visit-req* (get-ctdesc desc))))
  (define libdesc-loaded-import-reqs
    (lambda (desc)
      (ctdesc-loaded-import-reqs (get-ctdesc desc))))
  (define libdesc-loaded-import-reqs-set!
    (lambda (desc x)
      (ctdesc-loaded-import-reqs-set! (get-ctdesc desc) x)))
  (define libdesc-loaded-visit-reqs
    (lambda (desc)
      (ctdesc-loaded-visit-reqs (get-ctdesc desc))))
  (define libdesc-loaded-visit-reqs-set!
    (lambda (desc x)
      (ctdesc-loaded-visit-reqs-set! (get-ctdesc desc) x)))
  (define libdesc-import-code
    (lambda (desc)
      (ctdesc-import-code (get-ctdesc desc))))
  (define libdesc-import-code-set!
    (lambda (desc x)
      (ctdesc-import-code-set! (get-ctdesc desc) x)))
  (define libdesc-visit-code
    (lambda (desc)
      (ctdesc-visit-code (get-ctdesc desc))))
  (define libdesc-visit-code-set!
    (lambda (desc x)
      (ctdesc-visit-code-set! (get-ctdesc desc) x)))
  (define libdesc-visit-id*
    (lambda (desc)
      (ctdesc-export-id* (get-ctdesc desc))))
  (define libdesc-visit-id*-set!
    (lambda (desc x)
      (ctdesc-export-id*-set! (get-ctdesc desc) x))))

(module (libdesc-invoke-req*
         libdesc-loaded-invoke-reqs libdesc-loaded-invoke-reqs-set!
         libdesc-invoke-code libdesc-invoke-code-set!)
  (define get-rtdesc
    (lambda (desc)
      (or (libdesc-rtdesc desc)
          ($oops #f "run-time information for library ~s has not been loaded" (libdesc-path desc)))))
  (define libdesc-invoke-req*
    (lambda (desc)
      (rtdesc-invoke-req* (get-rtdesc desc))))
  (define libdesc-loaded-invoke-reqs
    (lambda (desc)
      (rtdesc-loaded-invoke-reqs (get-rtdesc desc))))
  (define libdesc-loaded-invoke-reqs-set!
    (lambda (desc x)
      (rtdesc-loaded-invoke-reqs-set! (get-rtdesc desc) x)))
  (define libdesc-invoke-code
    (lambda (desc)
      (rtdesc-invoke-code (get-rtdesc desc))))
  (define libdesc-invoke-code-set!
    (lambda (desc x)
      (rtdesc-invoke-code-set! (get-rtdesc desc) x))))

(define-syntax with-message
  (syntax-rules ()
    [(_ msg e1 e2 ...)
     (begin
       (when (import-notify) (fprintf (console-output-port) "~a\n" msg))
       e1 e2 ...)]))

(define visit-loaded-library
 ; library must already have been loaded, as well as those in its visit-req* list
  (lambda (uid)
    (define (go desc)
      (cond
        [(libdesc-visit-code desc) =>
         (lambda (p)
           (when (eq? p 'loading)
             ($oops #f "attempt to visit library ~s while it is still being loaded" (libdesc-path desc)))
           (when (eq? p 'pending)
             ($oops #f "cyclic dependency involving visit of library ~s" (libdesc-path desc)))
           (libdesc-visit-code-set! desc 'pending)
           (on-reset
             (begin
               (for-each (lambda (id) ($sc-put-cte id (make-binding 'visit uid) #f)) (libdesc-visit-id* desc))
               (libdesc-visit-code-set! desc p))
             (for-each (lambda (req) (visit-loaded-library (libreq-uid req))) (libdesc-visit-visit-req* desc))
             (for-each (lambda (req) (invoke-loaded-library (libreq-uid req))) (libdesc-visit-req* desc))
             (p))
           (libdesc-visit-code-set! desc #f)
           (libdesc-visit-id*-set! desc '()))]))
    (cond
      [(get-library-descriptor uid) =>
       (lambda (desc)
         (unless (libdesc-visible? desc) ($oops #f "attempt to visit invisible library ~s" (libdesc-path desc)))
         (if (libdesc-ctdesc desc)
             (go desc)
             (let ([fn (libdesc-outfn desc)])
               ; this probably can't happen...we have probably already imported the library
               ; for us to encounter something that forces us to visit the library
               (with-message (format "attempting to 'visit' previously 'revisited' ~s for library ~s compile-time info" fn (libdesc-path desc))
                 ($visit #f fn #f))
               (let ([desc (get-library-descriptor uid)])
                 (unless (libdesc-ctdesc desc)
                   ($oops #f "visiting ~a did not define compile-time information for library ~s" fn (libdesc-path desc)))
                 (go desc)))))]
      [else ($oops #f "library ~:s is not defined" uid)])))

(define invoke-loaded-library
 ; library must already have been loaded, as well as those in its invoke-req* list
  (lambda (uid)
    (define (go desc)
      (cond
        [(libdesc-invoke-code desc) =>
         (lambda (p)
           (when (eq? p 'loading)
             ($oops #f "attempt to invoke library ~s while it is still being loaded" (libdesc-path desc)))
           (when (eq? p 'pending)
             ($oops #f "cyclic dependency involving invocation of library ~s" (libdesc-path desc)))
           (libdesc-invoke-code-set! desc 'pending)
           (on-reset (libdesc-invoke-code-set! desc p)
             (for-each (lambda (req) (invoke-loaded-library (libreq-uid req))) (libdesc-invoke-req* desc))
             (p))
           (libdesc-invoke-code-set! desc #f))]))
    (cond
      [(get-library-descriptor uid) =>
       (lambda (desc)
         (unless (libdesc-visible? desc) ($oops #f "attempt to invoke invisible library ~s" (libdesc-path desc)))
         (if (libdesc-rtdesc desc)
             (go desc)
             (let ([fn (libdesc-outfn desc)])
               (with-message (format "attempting to 'revisit' previously 'visited' ~s for library ~s run-time info" fn (libdesc-path desc))
                 ($revisit #f fn #f))
               (let ([desc (get-library-descriptor uid)])
                 (unless (libdesc-ctdesc desc)
                   ($oops #f "revisiting ~a did not define run-time information for library ~s" fn (libdesc-path desc)))
                 (go desc)))))]
        [else ($oops #f "library ~:s is not defined" uid)])))

(define-threaded require-invoke
  (lambda (uid)
    ($oops 'sc-expand-internal "no one is collecting invoke requirements")))

(define-threaded require-visit
  (lambda (uid)
    ($oops 'sc-expand-internal "no one is collecting visit requirements")))

(define build-requirement
  (lambda (prim)
    (lambda (req)
      (build-primcall no-source 3 prim
        (build-data no-source (libreq-path req))
        (build-data no-source (libreq-version req))
        (build-data no-source (libreq-uid req))))))

(define (include-collector)
  (let ([ht (make-hashtable string-hash string=?)])
    (case-lambda
      [(path) (hashtable-set! ht path #t)]
      [() (vector->list (hashtable-keys ht))])))

(define library-collector
  (lambda (invoke-now?)
   ; set invoke-now? when collecting requirements for expand-time expressions,
   ; e.g., define-syntax RHS expressions, so that the libraries needed by the
   ; RHS will be invoked before the RHS is evaluated.
    (let ([req* '()])
      (case-lambda
        [(uid)
         (let retry ()
           (cond
             [(get-library-descriptor uid) =>
              (lambda (desc)
                (when invoke-now?
                  (cond
                    [(not (libdesc-visible? desc))
                     ($oops #f "attempt to invoke invisible library ~s" (libdesc-path desc))]
                    [(not (libdesc-rtdesc desc))
                     (with-message (format "attempting to 'revisit' previously 'visited' ~s for library ~s run-time info" (libdesc-outfn desc) (libdesc-path desc))
                       ($revisit #f (libdesc-outfn desc) #f))
                     (retry)]
                    [(libdesc-invoke-code desc) =>
                     (lambda (p)
                       (when (eq? p 'pending)
                         ($oops #f "cyclic dependency involving invocation of library ~s" (libdesc-path desc)))
                       (libdesc-invoke-code-set! desc 'pending)
                       (on-reset (libdesc-invoke-code-set! desc p)
                         (for-each (lambda (req) (invoke-loaded-library (libreq-uid req))) (libdesc-invoke-req* desc))
                         (p))
                       (libdesc-invoke-code-set! desc #f))]))
                (unless (memp (lambda (x) (eq? (libreq-uid x) uid)) req*)
                  (set! req* (cons (make-libreq (libdesc-path desc) (libdesc-version desc) uid) req*))))]
             [else ($oops #f "library ~:s is not defined" uid)]))]
        [() req*]))))

(define propagating-library-collector
  (lambda (outer-collector invoke-now?)
    (let ([inner-collector (library-collector invoke-now?)])
      (case-lambda
        [(x) (inner-collector x) (outer-collector x)]
        [() (inner-collector)]))))

(define chi-top-library
 ;  - speed up (prefix (rnrs) foo:), perhaps by passing responsibility
 ;    along to $import and letting it deal with symbols rather than
 ;    identifiers
 ;    - may be able to construct symbols and hide them in a record that is
 ;      included as an $import subform
 ;  - mats
 ;    - version references
 ;  - library-search-path -> (path*)
 ;    path -> " path-elt "
 ;    path-elt -> xchar | %% | %m | %v | %x
 ;    xchar -> any valid pathname char except %
 ;    %v filled in with Chez Scheme version
 ;    %m filled in with Chez Scheme (machine-type)
 ;    %V filled in with all applicable versions
 ;    %X filled in with each element of (library-extensions) in turn
 ;    %L filled in library name, e.g., rnrs/io/simple
 ;    - to handle %V, do directory list with with * in place of %v, isolate
 ;      version, and prefer highest, most specific version
 ;    - after finding file in a given directory with a given extension, try
 ;      same pathname root with remaining extensions and warn if newer file
 ;      found
 ;  - procedure interface and source information for cross-library optimization
 ;  - local libraries
 ;  - use hash table for large, complete ribcages
 ;    - do check-module-exports while building table
  (lambda (orig library-path library-version r top-ribcage ribcage ctem rtem library-uid template-id forms outfn)
    (fluid-let ([require-import (library-collector #f)]
                [require-include (include-collector)]
                [require-invoke (library-collector #t)]
                [require-visit (library-collector #f)])
      (let-values ([(mb* inits exports iface-vector chexports label*)
                    (chi-external* ribcage orig
                      (map (lambda (d) (make-frob d #f)) forms)
                      r 'library orig)])
       ; NB: mb* is in reverse order of original appearance
        (let ([visit-visit-req* (require-visit)] [visit-req* (require-invoke)])
         ; dl*: define label (#f for locals), dv* & de*: define var & expr
          (let process-bindings ([mb* mb*] [env* '()] [vthunk void] [vcode* '()] [dl* '()] [dv* '()] [de* '()])
            (if (null? mb*)
               ; NB: dl*/dv*/de*/inits should be in proper order at this point
                (fluid-let ([require-invoke (library-collector #f)]
                            [require-visit (library-collector #f)])
                  (let* ([de* (not-at-top (chi-frobs de* r))]
                         [inits (not-at-top (chi-frobs inits r))]
                         [import-req* (require-import)]
                         [include-req* (require-include)]
                         [invoke-req* (require-invoke)])
                   ; verify exports are defined only after processing rhs, init, and
                   ; body expressions so we get syntax, invalid define context, and
                   ; other errors that might explain why exports are actually missing
                    (chexports)
                   ; check to make sure our direct exports aren't assigned
                   ; not checking explicit or implicit indirect exports...that's done
                   ; only if and when an exported macro expands into a reference
                    (for-each
                      (lambda (id)
                        (let ([b (lookup (id->label id empty-wrap) r)])
                          (when (and (eq? (binding-type b) 'lexical)
                                     (lexical-var-assigned? (binding-value b)))
                            (syntax-error id "attempt to export assigned variable"))))
                      exports)
                    (let ([bound-id (make-resolved-id library-uid (wrap-marks top-wrap) library-uid)])
                      (let ([env* (map (lambda (x)
                                        ; mark assigned exports unexported
                                         (let ([label (car x)])
                                           (if (and (eq? (binding-type (cdr x)) 'library-global)
                                                    (lexical-var-assigned? (binding-value (lookup label r))))
                                               `(,label . ,unexported-assigned-binding)
                                               x)))
                                   env*)]
                            [db* (map (lambda (dl) (and dl (box '()))) dl*)]
                            [interface-binding
                             (make-binding '$module
                               (make-interface (wrap-marks (syntax-object-wrap template-id)) iface-vector))])

                       ; finish setting up the library in the current process, even if ctem and rtem
                       ; would tell us not to, so the library isn't only partly set up
                        (for-each (lambda (x) ($sc-put-cte (car x) (cdr x) #f)) env*)
                        (for-each (lambda (dl db) (when dl (put-clo-info dl db))) dl* db*)
                       ; module bindings don't belong in any environment, so send in #f for the token
                        ($sc-put-cte bound-id interface-binding #f)
                        (vthunk) ; might as well do this now.  visit-req* have already been invoked
                        (install-library library-path library-uid
                         ; import-code & visit-code is #f because vthunk invocation has already set up compile-time environment
                          (make-libdesc library-path library-version outfn #f #f #t
                            (make-ctdesc import-req* visit-visit-req* visit-req* #t #t '() #f #f)
                            (make-rtdesc invoke-req* #t
                              (top-level-eval-hook
                                (build-lambda/lift-barrier no-source '()
                                  (build-library-body no-source dl* db* dv* de*
                                    (build-sequence no-source `(,@inits ,(build-void)))))))))

                       ; must be after last reference to r
                        (for-each (kill-label! r) label*)

                        (build-group
                          `(,(if (or (or (memq 'L ctem) (memq 'R ctem) (memq 'V ctem))
                                     (or (memq 'L rtem) (memq 'R rtem) (memq 'V rtem)))
                                 (build-recompile-info import-req* include-req*)
                                 (build-void no-source))
                            ,(rt-eval/residualize rtem
                               build-void
                               (lambda ()
                                 (build-library/rt-info
                                   (make-library/rt-info library-path library-version library-uid #t
                                     invoke-req*))))
                            ,(ct-eval/residualize ctem
                               build-void
                               (lambda ()
                                 (build-library/ct-info
                                   (make-library/ct-info library-path library-version library-uid #t
                                     import-req* visit-visit-req* visit-req*))))
                            ,(rt-eval/residualize rtem
                               build-void
                               (lambda ()
                                 (build-top-library/rt library-uid
                                   ; invoke code
                                   dl* db* dv* de* inits)))
                            ,(ct-eval/residualize ctem
                               build-void
                               (lambda ()
                                 (build-top-library/ct library-uid
                                   ; visit-time exports (making them available for reset on visit-code failure)
                                   (fold-left (lambda (ls x)
                                                (let ([label (car x)] [exp (cdr x)])
                                                  (if (and (pair? exp) (eq? (car exp) 'visit))
                                                      (cons label ls)
                                                      ls)))
                                     '() env*)
                                   ; interface
                                   (binding-value interface-binding)
                                   ; import code
                                   `(,(build-cte-install bound-id (build-data no-source interface-binding) '*system*)
                                     ,@(let ([clo* (fold-left (lambda (clo* dl db)
                                                                (if dl
                                                                    (cons (cons dl db) clo*)
                                                                    clo*))
                                                     '() dl* db*)])
                                         (if (null? clo*)
                                             '()
                                             `(,(build-primcall #f 3 '$install-library-clo-info (build-data #f clo*)))))
                                     ,@(if (null? env*)
                                           '()
                                           `(,(build-sequence no-source
                                                (map (lambda (x)
                                                       (build-cte-install (car x) (build-data no-source (cdr x)) #f))
                                                  env*)))))
                                   ; visit code
                                   vcode*)))))))))
                (let ([mb (car mb*)] [mb* (cdr mb*)])
                  (mbodit-case mb
                    [define (id label b)
                     (let ([var (gen-var id)] [val (binding-value b)])
                       (set-binding-type! b 'lexical)
                       (set-binding-value! b var)
                       (if (mbodit-exported mb)
                           (process-bindings mb*
                             (cons `(,label . ,(make-binding 'library-global (cons library-uid label))) env*)
                             vthunk vcode*
                             (cons label dl*)
                             (cons var dv*)
                             (cons val de*))
                           (process-bindings mb*
                             (cons `(,label . ,unexported-binding) env*)
                             vthunk vcode*
                             (cons #f dl*)
                             (cons var dv*)
                             (cons val de*))))]
                    [define-syntax (id label binding rhs)
                     (if (mbodit-exported mb)
                         (process-bindings mb*
                           (cons `(,label . ,(make-binding 'visit library-uid)) env*)
                           (lambda () ($sc-put-cte label binding #f) (vthunk))
                           (cons (build-checking-cte-install label rhs #f) vcode*)
                           dl* dv* de*)
                         (process-bindings mb*
                           (cons `(,label . ,unexported-binding) env*)
                           vthunk vcode* dl* dv* de*))]
                    [define-property (id association propval propvalexpr)
                     (if (mbodit-exported mb)
                         (process-bindings mb*
                           (cons `(,(cdr association) . ,(make-binding 'visit library-uid)) env*)
                           (lambda () ($sc-put-property! id association propval #f) (vthunk))
                           (cons (build-primcall #f 3 '$sc-put-property!
                                   (build-data #f id)
                                   (build-data #f association)
                                   propvalexpr
                                   (build-data #f #f))
                                 vcode*)
                           dl* dv* de*)
                         (process-bindings mb* env* vthunk vcode* dl* dv* de*))]
                    [module (id label iface module-mb*)
                     (let ([mb* (append module-mb* mb*)])
                       (if (mbodit-exported mb)
                           (process-bindings mb*
                             (cons `(,label . ,(make-binding '$module iface)) env*)
                             vthunk vcode* dl* dv* de*)
                           (process-bindings mb*
                             (cons `(,label . ,unexported-binding) env*)
                             vthunk vcode* dl* dv* de*)))]
                    [meta-define (id label binding expr)
                     (if (mbodit-exported mb)
                         (let ([binding (make-binding 'library-meta-global (cons library-uid (binding-value binding)))])
                           (process-bindings mb*
                             (cons `(,label . ,(make-binding 'visit library-uid)) env*)
                             (lambda () ($sc-put-cte label binding #f) (vthunk))
                             (cons*
                               (build-cte-install label (build-data no-source binding) #f)
                               (build-global-assignment no-source label expr)
                               vcode*)
                             dl* dv* de*))
                         (process-bindings mb*
                           (cons `(,label . ,unexported-binding) env*)
                           vthunk
                           (cons (build-global-assignment no-source label expr) vcode*)
                           dl* dv* de*))]
                    [meta-eval (expr)
                     (process-bindings mb* env* vthunk (cons expr vcode*) dl* dv* de*)]
                    [interleaved-init (frob) ($oops 'sc-expand-internal "unexpected interleaved init mbodit")])))))))))

(define chi-top-program
  (lambda (ae orig r top-ribcage ribcage rtem forms)
   ; we collect and discard import and visit requirements so they don't show
   ; up incorrectly as top-level invoke requirements
    (fluid-let ([require-include (include-collector)]
                [require-invoke (library-collector #t)]
                [require-visit (library-collector #f)]
                [require-import (library-collector #f)])
      (let-values ([(mb* inits exports iface-vector chexports label*)
                    (chi-external* ribcage orig
                      (map (lambda (d) (make-frob d #f)) forms)
                      r 'program orig)])
        (let process-bindings ([mb* mb*] [r r] [dv* '()] [de* '()])
          (if (null? mb*)
             ; collect our invoke requirements, both to report the result to
             ; the user, and to put our invokes before the program code.
              (fluid-let ([require-invoke (library-collector #f)]
                          [require-visit (library-collector #f)])
                (let ([de* (not-at-top (chi-frobs de* r))]
                      [inits (not-at-top (chi-frobs inits r))])

                 ; must be after last reference to r
                  (for-each (kill-label! r) label*)

                  (let ([invoke-req* (require-invoke)])
                   ; tell compile-program about the libraries that need to be shipped with this
                   ; program.  call/collector reports only import requirements, which we don't let
                   ; through, and we just want invoke requirements anyway, so we report them here.
                    (($require-libraries) (map libreq-path invoke-req*))
                    (let* ([prog-uid (gensym "program")]
                           [pinfo (make-program-info prog-uid invoke-req*)])
                      (build-group
                        `(,(if (or (memq 'L rtem) (memq 'R rtem) (memq 'V rtem))
                               (build-recompile-info (require-import) (require-include))
                               (build-void no-source))
                          ,(rt-eval/residualize rtem
                             (lambda ()
                               (build-primcall no-source 3 '$install-program-desc
                                 (build-data no-source pinfo)))
                             (lambda () (build-program-info pinfo)))
                          ,(rt-eval/residualize rtem
                             (lambda ()
                               (build-top-program prog-uid
                                 (build-letrec* ae dv* de*
                                   (build-sequence no-source
                                     (append inits (list (build-void))))))))))))))
              (let ([mb (car mb*)] [mb* (cdr mb*)])
                (mbodit-case mb
                  [define (id label b)
                   (let ([var (gen-var id)] [val (binding-value b)])
                     (set-binding-type! b 'lexical)
                     (set-binding-value! b var)
                     (process-bindings mb* r
                       (cons var dv*)
                       (cons val de*)))]
                  [module (id label iface module-mb*)
                   (process-bindings (append module-mb* mb*) r dv* de*)]
                  [interleaved-init (frob)
                   (process-bindings mb* r
                     (cons (build-lexical-var no-source 't) dv*)
                     (cons (make-frob #`(begin #,(frob-e frob) (void)) (frob-meta? frob))
                      de*))]
                  [else (process-bindings mb* r dv* de*)]))))))))

(define chi-top-module
  (lambda (orig r top-ribcage ribcage ctem rtem meta? id forms)
    (let-values ([(mb* inits exports iface-vector chexports label*)
                  (chi-external* ribcage orig
                    (map (lambda (d) (make-frob d meta?)) forms)
                    r 'module orig)])
     ; NB: mb* is in reverse order of original appearance
     ; dt*: define type (local/global), dv* & de*: define lhs & rhs
      (let process-bindings ([mb* mb*] [env* '()] [vthunk void] [vcode* '()] [dt* '()] [dv* '()] [de* '()])
        (if (null? mb*)
           ; NB: dt*/dv*/de*/inits should be in proper order at this point
            (values
              (let ([de* (not-at-top (chi-frobs de* r))]
                    [inits (not-at-top (chi-frobs inits r))])
               ; verify exports are defined only after processing rhs, init, and
               ; body expressions so we get syntax, invalid define context, and
               ; other errors that might explain why exports are actually missing
                (chexports)

               ; must be after last reference to r
                (for-each (kill-label! r) label*)

               ; we wait to establish global compile-time definitions so that
               ; expansion of des use local versions of modules and macros
               ; in case ctem tells us not to eval ctdefs now.  this means that
               ; local code can use exported compile-time values (modules, macros,
               ; meta variables) just as it can unexported ones.
                (list
                  (ct-eval/residualize ctem
                    (lambda ()
                      (for-each
                        (lambda (x)
                          (let ([label (car x)] [b (cdr x)])
                            ($sc-put-cte label b #f)))
                        env*)
                      (vthunk))
                    (lambda ()
                      (build-sequence no-source
                        (append
                          (map (lambda (x)
                                 (let ([label (car x)] [b (cdr x)])
                                   (build-cte-install label (build-data no-source b) #f)))
                               env*)
                          vcode*))))
                  (rt-eval/residualize rtem
                    (lambda ()
                      (build-top-module no-source dt* dv* de*
                        (build-sequence no-source
                          (append inits (list (build-void)))))))))
              iface-vector)
            (let ([mb (car mb*)] [mb* (cdr mb*)])
              (mbodit-case mb
                [define (id label b)
                 (let ([val (binding-value b)])
                   (if (mbodit-exported mb)
                       (begin
                         (set-binding-type! b 'global)
                         (set-binding-value! b label)
                         (process-bindings mb*
                           (cons `(,label . ,b) env*)
                           vthunk vcode*
                           (cons 'global dt*)
                           (cons label dv*)
                           (cons val de*)))
                       (let ([var (gen-var id)])
                         (set-binding-type! b 'lexical)
                         (set-binding-value! b var)
                         (process-bindings mb*
                           (cons `(,label . ,unexported-binding) env*)
                           vthunk vcode*
                           (cons 'local dt*)
                           (cons var dv*)
                           (cons val de*)))))]
                [define-syntax (id label binding rhs)
                 (if (mbodit-exported mb)
                     (process-bindings mb* env*
                       (lambda () ($sc-put-cte label binding #f) (vthunk))
                       (cons (build-checking-cte-install label rhs #f) vcode*)
                       dt* dv* de*)
                     (process-bindings mb*
                       (cons `(,label . ,unexported-binding) env*)
                       vthunk vcode* dt* dv* de*))]
                [define-property (id association propval propvalexpr)
                 (if (mbodit-exported mb)
                     (process-bindings mb* env*
                       (lambda () ($sc-put-property! id association propval #f) (vthunk))
                       (cons (build-primcall #f 3 '$sc-put-property!
                               (build-data #f id)
                               (build-data #f association)
                               propvalexpr
                               (build-data #f #f))
                             vcode*)
                       dt* dv* de*)
                     (process-bindings mb* env* vthunk vcode* dt* dv* de*))]
                [module (id label iface module-mb*)
                 (let ([mb* (append module-mb* mb*)])
                   (if (mbodit-exported mb)
                       (process-bindings mb*
                         (cons `(,label . ,(make-binding '$module iface)) env*)
                         vthunk vcode* dt* dv* de*)
                       (process-bindings mb*
                         (cons `(,label . ,unexported-binding) env*)
                         vthunk vcode* dt* dv* de*)))]
                [meta-define (id label binding expr)
                 (if (mbodit-exported mb)
                     (process-bindings mb* env*
                       (lambda () ($sc-put-cte label binding #f) (vthunk))
                       (cons*
                         (build-cte-install label (build-data no-source binding) #f)
                         (build-global-assignment no-source label expr)
                         vcode*)
                       dt* dv* de*)
                     (process-bindings mb*
                       (cons `(,label . ,unexported-binding) env*)
                       vthunk
                       (cons (build-global-assignment no-source label expr) vcode*)
                       dt* dv* de*))]
                [meta-eval (expr)
                 (process-bindings mb* env* vthunk (cons expr vcode*) dt* dv* de*)]
                [interleaved-init (frob) ($oops 'sc-expand-internal "unexpected interleaved init mbodit")])))))))

(define id-set-diff
  (lambda (exports defs)
    (cond
      ((null? exports) '())
      ((bound-id-member? (car exports) defs) (id-set-diff (cdr exports) defs))
      (else (cons (car exports) (id-set-diff (cdr exports) defs))))))

(module (make-defn-table record-id! record-iface! record-property! report-duplicates! check-exports!)
  (define-record-type defn-table
    (fields (immutable ht) (mutable dup*))
    (nongenerative)
    (sealed #t)
    (protocol (lambda (new) (lambda () (new (make-hashtable symbol-hash eq?) '())))))
  (define-record-type entry
    (fields (immutable marks) (immutable label))
    (nongenerative)
    (sealed #t))
  (define check-and-record!
    (lambda (tbl sym marks label)
      (let ([ht (defn-table-ht tbl)])
        (let ([orig-entry* (symbol-hashtable-ref ht sym '())])
          (let f ([entry* orig-entry*])
            (if (null? entry*)
                (symbol-hashtable-set! ht sym (cons (make-entry marks label) orig-entry*))
                (let ([entry (car entry*)])
                  (if (same-marks? (entry-marks entry) marks)
                      (unless (eq? (entry-label entry) label)
                        (defn-table-dup*-set! tbl (cons sym (defn-table-dup* tbl))))
                      (f (cdr entry*))))))))))
  (define record-only!
    (lambda (tbl sym marks label)
      (let ([ht (defn-table-ht tbl)])
        (let ([orig-entry* (symbol-hashtable-ref ht sym '())])
          (let f ([entry* orig-entry*])
            (if (null? entry*)
                (symbol-hashtable-set! ht sym (cons (make-entry marks label) orig-entry*))
                (let ([entry (car entry*)])
                  (if (same-marks? (entry-marks entry) marks)
                      (unless (eq? (entry-label entry) label)
                        (symbol-hashtable-set! ht sym
                          (cons (make-entry marks label) (remq entry orig-entry*))))
                      (f (cdr entry*))))))))))
  (define record-id!
    (lambda (tbl id label)
      (check-and-record! tbl (id-sym-name id) (id-marks id) label)))
  (define record-iface!
    (lambda (tbl import-iface)
      (let ([iface (get-indirect-interface (import-interface-interface import-iface))]
            [new-marks (import-interface-new-marks import-iface)])
        (vector-for-each
          (lambda (id)
            (check-and-record! tbl (id-sym-name id)
              (join-marks new-marks (id-marks id))
              (resolved-id->label id)))
          (interface-exports iface)))))
  (define record-property!
    (lambda (tbl id label)
      (record-only! tbl (id-sym-name id) (id-marks id) label)))
  (define report-duplicates!
    (lambda (tbl source-exp)
     ; collected in reverse, reverse again so error mentions first-listed duplicate
      (let ([dup* (defn-table-dup* tbl)])
        (unless (null? dup*)
          (let ([dup* (reverse dup*)])
            (syntax-error source-exp
              (format "multiple definitions for ~a in body"
                (cond
                  [(fx= (length dup*) 1) (format "~s" (car dup*))]
                  [(fx= (length dup*) 2) (format "~s and ~s" (car dup*) (cadr dup*))]
                  [else (format "~s and other identifiers" (car dup*))]))))))))
  (define check-exports!
    ; After processing the definitions of a module this is called to verify that the
    ; module has defined or imported each exported identifier.  Because ids in fexports are
    ; wrapped with the given ribcage, they will contain substitutions for anything defined
    ; or imported here.
    (lambda (tbl source-exp fexports)
      (let ([ht (defn-table-ht tbl)])
        (define defined?
          (lambda (id)
            (let ([sym (id-sym-name id)] [marks (id-marks id)])
              (let f ([entry* (symbol-hashtable-ref ht sym '())])
                (and (not (null? entry*))
                  (or (same-marks? (entry-marks (car entry*)) marks)
                      (f (cdr entry*))))))))
        (let loop ([fexports fexports] [missing '()])
          (if (null? fexports)
              (unless (null? missing)
                (syntax-error (car missing)
                  (if (= (length missing) 1)
                      "missing definition for export"
                      "missing definition for multiple exports, including")))
              (let ([id (car fexports)] [fexports (cdr fexports)])
                (if (defined? id)
                    (loop fexports missing)
                    (loop fexports (cons id missing))))))))))

(define-record-type iexports-list
  (fields (mutable id*)) ; #f if already been or being processed
  (nongenerative)
  (sealed #t))

(define chi-external*
 ; while running:
 ;   iexports-ht = hashtable mapping labels to lists of lists of indirect exports
 ;   mbs-ht = hashtable mapping labels to lists of module binding records
  (lambda (ribcage source-exp body r what src)
    (let ([iexports-ht (make-eq-hashtable)])
      (let-values ([(mb* inits exports iface-vector impind? chexports label*)
                    (chi-external ribcage source-exp body r iexports-ht what src '())])
        (unless (eq? what 'program)
         ; mark directly or indirectly exported bindings
          (let ([mbs-ht (make-eq-hashtable)])
           ; populate ht with id -> mb*
            (let populate! ([mb* mb*])
              (for-each
                (lambda (mb)
                  (cond
                    [(mbodit-id mb) =>
                     (lambda (id)
                       (let ([label (id->label id empty-wrap)])
                         (unless label ($oops 'sc-expand "internal error: mb ~s id has no label" mb))
                         (let ([a (eq-hashtable-cell mbs-ht label '())])
                           (set-cdr! a (cons mb (cdr a))))))])
                  (mbodit-case mb
                    [module (id label iface mb*) (populate! mb*)]
                    [else (void)]))
                mb*))
            (let ()
              (define mark-exported-id!
                (lambda (id)
                  (cond
                    [(id->label id empty-wrap) =>
                     (lambda (label)
                       (let ([a (eq-hashtable-cell mbs-ht label '())])
                         (cond
                           [(cdr a) =>
                            (lambda (mb*)
                              (set-cdr! a #f)
                              (for-each
                                (lambda (mb)
                                  (mbodit-exported-set! mb #t)
                                  (mbodit-case mb
                                    [module (id label iface mb*)
                                     (vector-for-each mark-exported-id! (interface-exports iface))]
                                    [else (void)]))
                                mb*)
                              (for-each
                                (lambda (x)
                                  (cond
                                    [(iexports-list-id* x) =>
                                     (lambda (id*)
                                       (iexports-list-id*-set! x #f)
                                       (for-each mark-exported-id! id*))]))
                                (eq-hashtable-ref iexports-ht label '())))])))])))
              (for-each mark-exported-id! exports))))
        (values mb* inits exports iface-vector chexports label*)))))

(define chi-external
 ; on output:
 ;   exports = list of directly exported identifiers
 ;   iface-vector = vector of resolved ids representing the (external) exports
 ;   chexports = procedure that checks that ids named by various flavors of export forms are bound
 ; while running:
 ;   expspec** = list of lists of export specs
 ;   iexport* = alist mapping identifiers to their indirectly exported identifiers
  (lambda (ribcage source-exp body r iexports-ht what src label*)
    (define defn-table (make-defn-table))
    (define return
      (lambda (mb* inits chexports expspec** iexport* impind? label*)
        (report-duplicates! defn-table source-exp)
        (unless (eq? what 'program)
          (if impind?
              (let ([all-my-ids (make-iexports-list (remq #f (map mbodit-id mb*)))])
                (define add-all!
                  (lambda (mb)
                    (cond
                      [(and (mbodit-meta? mb) (mbodit-id mb)) =>
                       (lambda (id)
                         (let ([label (id->label id empty-wrap)])
                           (unless label ($oops 'sc-expand "internal error: mb ~s id has no label" mb))
                           (let ([cell (eq-hashtable-cell iexports-ht label '())])
                             (set-cdr! cell (cons all-my-ids (cdr cell))))))])
                    (mbodit-case mb
                      [module (id label iface mb*) (for-each add-all! mb*)]
                      [else (void)])))
                (for-each add-all! mb*))
              (for-each
                (lambda (a)
                  (cond
                    [(id->label (car a) empty-wrap) =>
                     (lambda (label)
                       (let ([cell (eq-hashtable-cell iexports-ht label '())])
                         (set-cdr! cell (cons (make-iexports-list (cdr a)) (cdr cell)))))]))
                iexport*)))
        (let-values ([(exports exports-to-check iface-vector) (determine-exports what src expspec** r)])
          (values mb* inits exports iface-vector impind?
            (lambda ()
              (chexports)
              (check-exports! defn-table source-exp (apply append exports-to-check iexport*)))
            label*))))
    (let parse ([body body] [mb* '()] [inits '()] [chexports void] [meta-seen? #f] [expspec** '()] [iexport* '()] [impind? #f] [label* label*])
      (if (null? body)
          (return mb* inits chexports expspec** iexport* impind? label*)
          (let* ([fr (car body)] [e (frob-e fr)] [meta? (frob-meta? fr)])
            (let-values ([(type value e w ae) (syntax-type e r empty-wrap no-source ribcage)])
              (case type
                [(define-form)
                 (let-values ([(id rhs w ae) (parse-define e w ae)])
                   (let* ([id (wrap id w)]
                          [label (gen-global-label (id-sym-name id))])
                     (extend-ribcage! ribcage id label)
                     (unless (eq? (id->label id empty-wrap) label)
                      ; must be an enclosing local-syntax binding for id
                       (syntax-error (source-wrap e w ae)
                         "definition not permitted"))
                     (record-id! defn-table id label)
                     (cond
                       [meta?
                        (let ([b (make-binding 'meta-variable label)])
                          (extend-rho! r label b (fxlognot 1))
                         ; chi rhs after establishing lhs mapping to label to allow
                         ; recursive meta definitions.
                          (let ([exp (not-at-top (meta-chi rhs r w))])
                            (define-top-level-value-hook label (top-level-eval-hook exp))
                            (parse (cdr body)
                              (cons (mbodit-meta-define #t #f id label b exp) mb*)
                              inits chexports
                              #f expspec** iexport* impind? (cons label label*))))]
                       [else
                        (let ([b (make-binding 'dunno (make-frob (wrap rhs w) meta?))])
                          (extend-rho! r label b 0)
                          (parse (cdr body)
                            (cons (mbodit-define #f #f id label b) mb*)
                            inits chexports
                            #f expspec** iexport* impind? (cons label label*)))])))]
                [(define-syntax-form)
                 (let-values ([(id rhs w) (parse-define-syntax e w ae)])
                   (let* ([id (wrap id w)]
                          [label (gen-global-label (id-sym-name id))]
                          [exp (not-at-top (meta-chi rhs r w))])
                     (extend-ribcage! ribcage id label)
                     (unless (eq? (id->label id empty-wrap) label)
                      ; must be an enclosing local-syntax binding for id
                       (syntax-error (source-wrap e w ae)
                         "definition not permitted"))
                     (record-id! defn-table id label)
                     (let ([b (defer-or-eval-transformer 'define-syntax top-level-eval-hook exp)])
                       (extend-rho! r label b (fxlognot 0))
                       (parse (cdr body)
                         (cons (mbodit-define-syntax #t #f id label b exp) mb*)
                         inits chexports
                         #f expspec** iexport* impind? (cons label label*)))))]
                [(define-property-form)
                 (let-values ([(id key-id expr w) (parse-define-property e w ae)])
                   (let* ([id (wrap id w)]
                          [id-label/pl (id->label/pl id empty-wrap)]
                          [key-id-label (id->label key-id w)]
                          [prop-label (gen-global-label (id-sym-name id))])
                     (unless id-label/pl (syntax-error id "no visible binding for define-property id"))
                     (unless key-id-label (syntax-error (wrap key-id w) "no visible binding for define-property key"))
                     (let* ([id-label (label/pl->label id-label/pl)]
                            [id-label/pl (make-label/pl id-label (cons key-id-label prop-label) (label/pl->pl id-label/pl))])
                       (extend-ribcage! ribcage id id-label/pl)
                       (unless (eq? (id->label id empty-wrap) id-label)
                        ; must be an enclosing local-syntax binding for id
                         (syntax-error (source-wrap e w ae)
                           "definition not permitted"))
                       (record-property! defn-table id id-label)
                       (let* ([propvalexpr (not-at-top (meta-chi expr r w))]
                              [propval (top-level-eval-hook propvalexpr)]
                              [binding (make-binding 'property propval)])
                         (extend-rho! r prop-label binding (fxlognot 0))
                         (parse (cdr body)
                           (cons (mbodit-define-property #t #f (make-resolved-id (id-sym-name id) (id-marks id) id-label/pl)
                                   (cons key-id-label prop-label) propval propvalexpr)
                                 mb*)
                           inits chexports #f expspec** iexport* impind? (cons prop-label label*))))))]
                [($module-form)
                 (let* ([*ribcage (make-empty-ribcage)]
                        [*w (make-wrap (wrap-marks w) (cons *ribcage (wrap-subst w)))])
                   (let-values ([(orig id forms) (parse-module e w ae *w)])
                     (let-values ([(*mb* *inits *exports *iface-vector *impind? *chexports label*)
                                   (chi-external *ribcage orig
                                     (map (lambda (d) (make-frob d meta?)) forms)
                                     r iexports-ht 'module orig label*)])
                       (let ([iface (make-interface (wrap-marks (syntax-object-wrap id)) *iface-vector)]
                             [inits (append inits *inits)]
                             [label (gen-global-label (id-sym-name id))])
                         (extend-ribcage! ribcage id label)
                         (unless (eq? (id->label id empty-wrap) label)
                          ; must be an enclosing local-syntax binding for id
                           (syntax-error orig "definition not permitted"))
                         (record-id! defn-table id label)
                         (let ([b (make-binding '$module iface)])
                           (extend-rho! r label b (fxlognot 0))
                           (parse (cdr body)
                             (cons (mbodit-module #f #f id label iface *mb*) mb*)
                             inits
                             (lambda () (chexports) (*chexports))
                             #f expspec** iexport* impind? (cons label label*)))))))]
                [($import-form)
                 (let-values ([(orig impspec* only? std?) (parse-import e w ae)])
                   (let process-impspecs ([impspec* impspec*] [tid* '()])
                     (if (null? impspec*)
                         (when only? (for-each (lambda (tid) (extend-ribcage-barrier! ribcage tid)) tid*))
                         (let-values ([(mid tid imps) (determine-imports (car impspec*) r std?)])
                           (process-impspecs (cdr impspec*) (cons tid tid*))
                           (if (import-interface? imps)
                               (begin
                                 (extend-ribcage-subst! ribcage imps)
                                 (record-iface! defn-table imps))
                               (begin
                                 (for-each (lambda (id) (import-extend-ribcage! ribcage id)) imps)
                                 (for-each (lambda (id) (record-id! defn-table id (resolved-id->label/pl id))) imps)))))))
                 (parse (cdr body) mb* inits chexports #f expspec** iexport* impind? label*)]
                [(export-form)
                 (let ([expspec* (parse-export e w ae)])
                   (when (eq? what 'program)
                     (syntax-error (source-wrap e w ae) "export form outside of a module or library"))
                   (parse (cdr body) mb* inits chexports #f (cons expspec* expspec**) iexport* impind? label*))]
                [(indirect-export-form)
                 (let-values ([(id id*) (parse-indirect-export e w ae)])
                   (parse (cdr body) mb* inits chexports #f expspec** (cons (cons id id*) iexport*) impind? label*))]
                [(implicit-exports-form)
                 (let-values ([(impind?) (parse-implicit-exports e w ae)])
                   (when (eq? what 'program)
                     (syntax-error (source-wrap e w ae) "implicit-exports form outside of a module or library"))
                   (parse (cdr body) mb* inits chexports #f expspec** iexport* impind? label*))]
                [(alias-form)
                 (let-values ([(new-id old-id) (parse-alias e w ae)])
                   (let* ([new-id (wrap new-id w)]
                          [label/pl (id->label/pl old-id w)]
                          [label (label/pl->label label/pl)])
                     (unless label (displaced-lexical-error old-id "create an alias to" #f))
                     (extend-ribcage! ribcage new-id label/pl)
                     (unless (eq? (id->label new-id empty-wrap) label)
                      ; must be an enclosing local-syntax binding for new-id
                       (syntax-error (source-wrap e w ae)
                         "definition not permitted"))
                     (record-id! defn-table new-id label)
                     (parse (cdr body) mb* inits chexports
                       #f expspec** iexport* impind? label*)))]
                [(begin-form)
                 (parse (let f ([forms (parse-begin e w ae #t)])
                          (if (null? forms)
                              (cdr body)
                              (cons (make-frob (wrap (car forms) w) meta?)
                                    (f (cdr forms)))))
                   mb* inits chexports #f expspec** iexport* impind? label*)]
                [(eval-when-form)
                 (let-values ([(when-list forms) (parse-eval-when e w ae)])
                    (parse (if (memq 'eval when-list) ; mode set is implicitly (E)
                               (let f ([forms forms])
                                 (if (null? forms)
                                     (cdr body)
                                     (cons (make-frob (wrap (car forms) w) meta?)
                                           (f (cdr forms)))))
                               (cdr body))
                      mb* inits chexports #f expspec** iexport* impind? label*))]
                [(meta-form)
                 (parse (cons (make-frob (wrap (parse-meta e w ae) w) #t)
                              (cdr body))
                   mb* inits chexports #t expspec** iexport* impind? label*)]
                [(local-syntax-form)
                 (let-values ([(forms w ae new-label*) (chi-local-syntax value #t e r w ae)])
                   (parse (let f ([forms forms])
                            (if (null? forms)
                                (cdr body)
                                (cons (make-frob (wrap (car forms) w) meta?)
                                      (f (cdr forms)))))
                     mb* inits chexports #f expspec** iexport* impind? (append new-label* label*)))]
                [else ; found an init expression
                 (let ([e (source-wrap e w ae)])
                   (when meta-seen? (syntax-error e "invalid meta definition"))
                   (if (eq? what 'program)
                       (parse (cdr body)
                         (cons (mbodit-interleaved-init #f #f (make-frob e meta?)) mb*)
                         inits chexports #f expspec** iexport* impind? label*)
                       (let f ([body (cons (make-frob e meta?) (cdr body))] [mb* mb*])
                         (if (or (null? body) (not (frob-meta? (car body))))
                             (return mb* (append inits body) chexports expspec** iexport* impind? label*)
                             (let ([x (not-at-top (meta-chi-frob (car body) r))])
                               (top-level-eval-hook x)
                               (f (cdr body) (cons (mbodit-meta-eval #t #f x) mb*)))))))])))))))

(define update-mode-set
  (let ([table
         '((L (load . L) (compile . C) (visit . V) (revisit . R))
           (C (eval . C))
           (V (load . V) (compile . C) (visit . V))
           (R (load . R) (compile . C) (revisit . R))
           (E (eval . E)))])
    (lambda (when-list mode-set)
      (fold-left
        (lambda (mode-set m)
          (let ([row (cdr (assq m table))])
            (fold-left
              (lambda (mode-set s)
                (cond
                  [(assq s row) => (lambda (a) (cons (cdr a) mode-set))]
                  [else mode-set]))
              mode-set when-list)))
        '() mode-set))))

(define initial-mode-set
  (lambda (when-list compiling-a-file)
    (fold-left
      (lambda (mode-set s)
        (if compiling-a-file
            (case s
              [(compile) (cons 'C mode-set)]
              [(load) (cons 'L mode-set)]
              [(visit) (cons 'V mode-set)]
              [(revisit) (cons 'R mode-set)]
              [else mode-set])
            (case s
              [(eval) (cons 'E mode-set)]
              [else mode-set])))
      '() when-list)))

(define rt-eval/residualize
  (case-lambda
    [(rtem thunk)
     (let ([t #f])
       (rt-eval/residualize rtem
         (lambda () (unless t (set! t (thunk))) t)
         (lambda () (or t (thunk)))))]
    [(rtem eval-thunk residualize-thunk)
     (if (memq 'E rtem)
         (eval-thunk)
         (begin
           (when (memq 'C rtem) (top-level-eval-hook (eval-thunk)))
           (if (memq 'V rtem)
               (if (or (memq 'L rtem) (memq 'R rtem))
                   (residualize-thunk) ; visit-revisit
                   (build-visit-only (residualize-thunk)))
               (if (or (memq 'L rtem) (memq 'R rtem))
                   (build-revisit-only (residualize-thunk))
                   (build-void)))))]))

(define ct-eval/residualize
  (case-lambda
    [(ctem thunk)
     (let ([t #f])
       (ct-eval/residualize ctem
         (lambda ()
           (unless t (set! t (thunk)))
           (top-level-eval-hook t))
         (lambda () (or t (thunk)))))]
    [(ctem eval-thunk residualize-thunk)
     (if (memq 'E ctem)
         (begin (eval-thunk) (build-void))
         (begin
           (when (memq 'C ctem) (eval-thunk))
           (if (memq 'R ctem)
               (if (or (memq 'L ctem) (memq 'V ctem))
                   (residualize-thunk) ; visit-revisit
                   (build-revisit-only (residualize-thunk)))
               (if (or (memq 'L ctem) (memq 'V ctem))
                   (build-visit-only (residualize-thunk))
                   (build-void)))))]))

(define chi-frobs
  (lambda (frob* r)
   ; by processing init frobs from left-to-right, some "invalid context for define"
   ; are eliminated in favor of more useful unbound variable errors, e.g., in
   ; (library (a) (export x y) (import (rnrs)) (defnie x 3) (define y 5))
   ; this does not help module or lambda bodies, since the unbound error is not
   ; reported until run time.
    (define (maplr p ls)
      (if (null? ls)
          '()
          (let ([x (p (car ls))])
            (cons x (maplr p (cdr ls))))))
    (maplr (lambda (x) (chi (frob-e x) r empty-wrap)) frob*)))

(define chi-sequence
  (lambda (body r w ae)
    (build-sequence ae
      (let dobody ((body body))
        (if (null? body)
            '()
            (let ((first (chi (car body) r w)))
              (cons first (dobody (cdr body)))))))))

(define residualize-import-requirements
  (lambda (import* code)
    (build-sequence no-source
      `(,@(map (build-requirement '$import-library) import*)
         ,code))))

(define residualize-invoke-requirements
  (case-lambda
    [(code) (residualize-invoke-requirements '()
                                             (require-visit)
                                             (if (expand-omit-library-invocations)
                                                 '()
                                                 (require-invoke))
                                             code)]
    [(import* visit* invoke* code)
     (build-sequence no-source
       `(,@(map (build-requirement '$import-library) import*)
         ,@(map (build-requirement '$invoke-library) invoke*)
         ,@(map (build-requirement '$visit-library) visit*)
         ,code))]))

(define chi*
  (lambda (e w)
    (fluid-let ([require-invoke (library-collector #t)]
                [require-visit (library-collector #f)]
                [require-import (library-collector #f)])
     ; dropping import requirements, since there can be no top-level imports
      (residualize-invoke-requirements (chi e (make-rho) w)))))

(define meta-chi-frob
  (lambda (x r)
    (meta-chi (frob-e x) r empty-wrap)))

(define meta-chi
  (lambda (e r w)
    (parameterize ([meta-level (fx+ (meta-level) 1)])
      (chi e r w))))

(define chi
  (lambda (e r w)
    (let-values ([(type value e w ae) (syntax-type e r w no-source #f)])
      (chi-expr type value e r w ae))))

(define chi-expr
  (lambda (type value e r w ae)
    (case type
      ((lexical) (build-lexical-reference ae value))
      ((core) (value e r w ae))
      ((call)
       (chi-application
         (let-values ([(type value e w ae) (syntax-type (car e) r w no-source #f)])
           (case type
             [(library-global)
              (require-invoke (car value))
              (build-global-reference ae (cdr value) #t)]
             [else (chi-expr type value e r w ae)]))
         e r w ae))
      ((primitive) (build-primitive-reference ae value))
      ((begin-form) (chi-sequence (parse-begin e w ae #f) r w ae))
      ((constant) (build-data ae e))
      ((global) (build-global-reference ae value #f))
      ((immutable-global) (build-global-reference ae value #t))
      ((library-global)
       (require-invoke (car value))
       (build-global-reference ae (cdr value) #t))
      ((local-syntax-form)
       (let-values ([(forms w ae label*) (chi-local-syntax value #f e r w ae)])
         (let ([x (chi-sequence forms r w ae)])
           (for-each kill-local-label! label*)
           x)))
      ((library-meta-global)
       (if (fx> (meta-level) 0)
           (begin
             (require-visit (car value))
             (build-global-reference ae (cdr value) #f))
           (displaced-lexical-error (source-wrap e w ae) "reference" #f)))
      ((meta-variable)
       (if (fx> (meta-level) 0)
           (build-global-reference ae value #f)
           (displaced-lexical-error (source-wrap e w ae) "reference" #f)))
      ((eval-when-form)
       (let-values ([(when-list forms) (parse-eval-when e w ae)])
         (if (memq 'eval when-list) ; mode set is implicitly (E)
             (chi-sequence forms r w ae)
             (build-void))))
      ((meta-form)
       (syntax-error (source-wrap e w ae) "invalid context for meta definition"))
      ((define-form)
       (parse-define e w ae)
       (syntax-error (source-wrap e w ae) "invalid context for definition"))
      ((define-syntax-form)
       (parse-define-syntax e w ae)
       (syntax-error (source-wrap e w ae) "invalid context for definition"))
      ((define-property-form)
       (parse-define-property e w ae)
       (syntax-error (source-wrap e w ae) "invalid context for definition"))
      (($module-form)
       (let-values ([(orig . ignore) (parse-module e w ae w)])
         (syntax-error orig "invalid context for definition")))
      (($import-form)
       (let-values ([(orig . ignore) (parse-import e w ae)])
         (syntax-error orig "invalid context for definition")))
      ((export-form)
       (parse-export e w ae)
       (syntax-error (source-wrap e w ae) "invalid context for definition"))
      ((indirect-export-form)
       (parse-indirect-export e w ae)
       (syntax-error (source-wrap e w ae) "invalid context for definition"))
      ((implicit-exports-form)
       (parse-implicit-exports e w ae)
       (syntax-error (source-wrap e w ae) "invalid context for definition"))
      (($library-form)
       (let-values ([(orig . ignore) (parse-library e w ae w)])
         (syntax-error orig "invalid context for library form")))
      (($program-form)
       (let-values ([(orig . ignore) (parse-program e w ae w)])
         (syntax-error orig "invalid context for top-level-program form")))
      ((alias-form)
       (parse-alias e w ae)
       (syntax-error (source-wrap e w ae) "invalid context for definition"))
      ((syntax)
       (syntax-error (source-wrap e w ae)
         "reference to pattern variable outside syntax form"))
      ((displaced-lexical) (displaced-lexical-error (source-wrap e w ae) "reference" value))
      (else (syntax-error (source-wrap e w ae))))))

(define chi-application
  (lambda (x e r w ae)
    (syntax-case e ()
      ((e0 e1 ...)
       (build-application ae x
         (map (lambda (e) (chi e r w)) (syntax (e1 ...)))))
      (_ (syntax-error (source-wrap e w ae))))))

(define chi-set!
  (lambda (e r w ae rib)
    (syntax-case e ()
      ((_ id val)
       (id? (syntax id))
       (let ((b (lookup (id->label (syntax id) w) r)))
         (case (binding-type b)
           ((macro!)
            (let ((id (wrap (syntax id) w)) (val (wrap (syntax val) w)))
              (syntax-type (chi-macro (binding-value b)
                             `(,(syntax set!) ,id ,val)
                             r empty-wrap #f rib)
                r empty-wrap ae rib)))
           (else
            (values 'core
              (lambda (e r w ae)
               ; repeat lookup in case we were first expression (init) in
               ; module or lambda body.  we repeat id->label as well,
               ; which is necessary when inits may precede definitions,
               ; as in a top-level program or top-level begin
                (let ((val (chi (syntax val) r w)))
                  (let ((b (lookup (id->label #'id w) r)))
                    (case (binding-type b)
                      ((lexical) (build-lexical-assignment ae (binding-value b) val))
                      ((global) (build-global-assignment ae (binding-value b) val))
                      ((immutable-global) (syntax-error (wrap #'id w) "attempt to assign immutable variable"))
                      ((primitive)
                       (unless (eq? (subset-mode) 'system)
                         (syntax-error (wrap #'id w)
                           "attempt to assign immutable variable"))
                       (build-primitive-assignment ae (binding-value b) val))
                      ((library-global)
                       (syntax-error (wrap #'id w)
                         "attempt to assign immutable variable"))
                      ((library-meta-global)
                       (if (fx> (meta-level) 0)
                           (syntax-error (wrap #'id w) "attempt to assign immutable variable")
                           (displaced-lexical-error (source-wrap e w ae) "assign" #f)))
                      ((meta-variable)
                       (if (fx> (meta-level) 0)
                           (build-global-assignment ae (binding-value b) val)
                           (displaced-lexical-error (wrap (syntax id) w) "assign" #f)))
                      ((displaced-lexical)
                       (displaced-lexical-error (wrap (syntax id) w) "assign" (binding-value b)))
                      (else (syntax-error (source-wrap e w ae)))))))
              e w ae)))))
      (_ (syntax-error (source-wrap e w ae))))))

(define chi-macro
  (lambda (p e r w ae rib)
    (define rebuild-macro-output
     ; wraps (e.g., anti-marks, substitutions) are not generally pushed into
     ; records since there's no syntax-case pattern for unpeeling records,
     ; so it seems inconsistent to mark records here.
      (lambda (x m)
        (cond ((pair? x)
               (cons (rebuild-macro-output (car x) m)
                     (rebuild-macro-output (cdr x) m)))
              ((syntax-object? x)
               (let ((w (syntax-object-wrap x)))
                 (let ((ms (wrap-marks w)) (s (wrap-subst w)))
                   (make-syntax-object (syntax-object-expression x)
                     (if (and (pair? ms) (eq? (car ms) the-anti-mark))
                         (make-wrap (cdr ms) (cdr s))
                         (make-wrap (cons m ms)
                           (if rib
                               (cons rib (cons 'shift s))
                               (cons 'shift s))))))))
              ((vector? x)
               (let* ((n (vector-length x)) (v (make-vector n)))
                 (do ((i 0 (fx+ i 1)))
                     ((fx= i n) v)
                     (vector-set! v i
                       (rebuild-macro-output (vector-ref x i) m)))))
              ((box? x) (box (rebuild-macro-output (unbox x) m)))
              ((symbol? x)
               (syntax-error (source-wrap e w ae)
                 "encountered raw symbol "
                 (format "~s" x)
                 " in output of macro"))
              (else x))))
    (rebuild-macro-output
      (let ((out (p (source-wrap e (anti-mark w) ae))))
        (if (procedure? out)
            (out (rec rho
                   (case-lambda
                     [(id)
                      (unless (identifier? id)
                        (syntax-error id
                          "first argument to lookup procedure is not an identifier"))
                      (let ([b (lookup (id->label id empty-wrap) r)])
                        (case (binding-type b)
                          [(ctv) ($compile-time-value-value (binding-value b))]
                          [else #f]))]
                     [(id key-id)
                      (unless (identifier? id)
                        (syntax-error id
                          "first argument to lookup procedure is not an identifier"))
                      (unless (identifier? key-id)
                        (syntax-error key-id
                          "second argument to lookup procedure is not an identifier"))
                      (let-values ([(id-label/pl retry) (id->label/pl/retry id empty-wrap)])
                        (let ([key-label (id->label key-id empty-wrap)])
                          (unless id-label/pl (syntax-error id "no visible binding for property id"))
                          (unless key-label (syntax-error key-id "no visible binding for property key"))
                          (let loop ([id-label/pl id-label/pl] [retry retry])
                            (cond
                              [(assq key-label (label/pl->pl id-label/pl)) =>
                               (lambda (a)
                                 (let ([b (lookup* (cdr a) r)])
                                   (case (binding-type b)
                                     [(property) (binding-value b)]
                                     [else #f])))]
                              [else (let-values ([(new-id-label/pl retry) (retry)])
                                      (and new-id-label/pl
                                           (eq? (label/pl->label new-id-label/pl) (label/pl->label id-label/pl))
                                           (loop new-id-label/pl retry)))]))))])))
            out))
      (new-mark))))

(define chi-body
  (lambda (body outer-form r w)
    (let* ([ribcage (make-empty-ribcage)]
           [w (make-wrap (wrap-marks w) (cons ribcage (wrap-subst w)))]
           [body (map (lambda (x) (make-frob (wrap x w) #f)) body)])
      (let-values ([(exprs defn-table vars vals inits expspec** iexport* chexports label*)
                   ; while processing the definitions of a local body, the
                   ; only invoke requirements should be for references
                   ; made by the transformer expressions of local macros.
                   ; the libraries required by local macro transformers
                   ; must be invoked immediately and do not necessarily
                   ; need to be invoked when the program is run, so we
                   ; collect the requirements with "invoke now?" true
                   ; and don't bother to grab the recorded requirements
                    (fluid-let ([require-invoke (library-collector #t)]
                                [require-visit (library-collector #f)])
                      (chi-internal ribcage outer-form body r #f '()))])
        (when (null? exprs) (syntax-error outer-form "no expressions in body"))
        (let ([vals (chi-frobs (reverse vals) r)]
              [exprs (chi-frobs (append inits exprs) r)])
         ; verify exports are defined only after processing rhs, init, and
         ; body expressions so we get syntax, invalid define context, and
         ; other errors that might explain why exports are actually missing
          (chexports)
          (for-each kill-local-label! label*)
          (build-body no-source
            (reverse vars) vals
            (build-sequence no-source exprs)))))))

(define chi-internal
  ;; In processing the forms of the body, we create a new, empty wrap.
  ;; This wrap is augmented (destructively) each time we discover that
  ;; the next form is a definition.  This is done:
  ;;
  ;;   (1) to allow the first nondefinition form to be a call to
  ;;       one of the defined ids even if the id previously denoted a
  ;;       definition keyword or keyword for a macro expanding into a
  ;;       definition;
  ;;   (2) to prevent subsequent definition forms (but unfortunately
  ;;       not earlier ones) and the first nondefinition form from
  ;;       confusing one of the bound identifiers for an auxiliary
  ;;       keyword; and
  ;;   (3) so that we do not need to restart the expansion of the
  ;;       first nondefinition form, which is problematic anyway
  ;;       since it might be the first element of a begin that we
  ;;       have just spliced into the body (meaning if we restarted,
  ;;       we'd really need to restart with the begin or the macro
  ;;       call that expanded into the begin, and we'd have to give
  ;;       up allowing (begin <defn>+ <expr>+), which is itself
  ;;       problematic since we don't know if a begin contains only
  ;;       definitions until we've expanded it).
  ;;
  ;; Subforms of a begin, let-syntax, or letrec-syntax are spliced
  ;; into the body.
  ;;
  ;; outer-form is fully wrapped w/source
  (lambda (ribcage source-exp body r module? label*)
    (define defn-table (make-defn-table))
    (define return
      (lambda (exprs vars vals inits expspec** iexport* chexports label*)
        (report-duplicates! defn-table source-exp)
        (values exprs defn-table vars vals inits expspec** iexport* chexports label*)))
    (let parse ([body body] [vars '()] [vals '()] [inits '()] [expspec** '()] [iexport* '()] [chexports void] [meta-seen? #f] [label* label*])
      (if (null? body)
          (return body vars vals inits expspec** iexport* chexports label*)
          (let* ([fr (car body)] [e (frob-e fr)] [meta? (frob-meta? fr)])
            (let-values ([(type value e w ae) (syntax-type e r empty-wrap no-source ribcage)])
              (case type
                [(define-form)
                 (let-values ([(id rhs w ae) (parse-define e w ae)])
                   (let ((id (wrap id w)))
                     (cond
                       [meta?
                        (let ([sym (generate-id (id-sym-name id))])
                          (let ([label (make-local-label (make-binding 'meta-variable sym) (fxlognot (fx+ (meta-level) 1)))])
                            (record-id! defn-table id label)
                            (extend-ribcage! ribcage id label)
                         ; add meta bindings only to meta environment
                         ; so visible only to next higher level and beyond
                            (define-top-level-value-hook sym
                              (top-level-eval-hook (meta-chi rhs r w)))
                            (parse (cdr body) vars vals inits expspec** iexport* chexports #f (cons label label*))))]
                       [else
                        (let ([var (gen-var id)])
                          (let ([label (make-lexical-label var)])
                            (record-id! defn-table id label)
                            (extend-ribcage! ribcage id label)
                            (unless (eq? (id->label id empty-wrap) label)
                             ; must be an enclosing local-syntax binding for id
                              (syntax-error (source-wrap e w ae)
                                "definition not permitted"))
                            (parse (cdr body)
                              (cons var vars)
                              (cons (make-frob (wrap rhs w) #f) vals)
                              inits expspec** iexport* chexports
                              #f (cons label label*))))])))]
                [(define-syntax-form)
                 (let-values ([(id rhs w) (parse-define-syntax e w ae)])
                   (let ([id (wrap id w)]
                         [label (make-local-label
                                  (defer-or-eval-transformer 'define-syntax local-eval-hook
                                    (meta-chi rhs r w))
                                  (fxlognot (meta-level)))])
                     (record-id! defn-table id label)
                     (extend-ribcage! ribcage id label)
                     (unless (eq? (id->label id empty-wrap) label)
                      ; must be an enclosing local-syntax binding for id
                       (syntax-error (source-wrap e w ae)
                         "definition not permitted"))
                     (parse (cdr body)
                       vars vals inits expspec** iexport* chexports #f (cons label label*))))]
                [(begin-form)
                 (parse (let f ((forms (parse-begin e w ae #t)))
                          (if (null? forms)
                              (cdr body)
                              (cons (make-frob (wrap (car forms) w) meta?)
                                    (f (cdr forms)))))
                   vars vals inits expspec** iexport* chexports #f label*)]
                [(export-form)
                 (let ([expspec* (parse-export e w ae)])
                   (unless module?
                     (unless (null? expspec*)
                       (syntax-error (source-wrap e w ae) "nonempty export form outside of a module or library")))
                   (parse (cdr body) vars vals inits (cons expspec* expspec**) iexport* chexports #f label*))]
                [(indirect-export-form)
                 (let-values ([(id id*) (parse-indirect-export e w ae)])
                   (parse (cdr body) vars vals inits
                     expspec** (cons (cons id id*) iexport*)
                     chexports #f label*))]
                [($import-form)
                 (let-values ([(orig impspec* only? std?) (parse-import e w ae)])
                   (let process-impspecs ([impspec* impspec*] [tid* '()])
                     (if (null? impspec*)
                         (when only? (for-each (lambda (tid) (extend-ribcage-barrier! ribcage tid)) tid*))
                         (let-values ([(mid tid imps) (determine-imports (car impspec*) r std?)])
                           (process-impspecs (cdr impspec*) (cons tid tid*))
                           (if (import-interface? imps)
                               (begin
                                 (extend-ribcage-subst! ribcage imps)
                                 (record-iface! defn-table imps))
                               (begin
                                 (for-each (lambda (id) (import-extend-ribcage! ribcage id)) imps)
                                 (for-each (lambda (id) (record-id! defn-table id (resolved-id->label/pl id))) imps)))))))
                 (parse (cdr body) vars vals inits expspec** iexport* chexports #f label*)]
                [($module-form)
                 (let* ((*ribcage (make-empty-ribcage))
                        (*w (make-wrap (wrap-marks w) (cons *ribcage (wrap-subst w)))))
                   (let*-values ([(orig id forms) (parse-module e w ae *w)]
                                 [(*body *defn-table *vars *vals *inits *expspec** *iexport* *chexports label*)
                                   (chi-internal *ribcage orig
                                     (map (lambda (d) (make-frob d meta?)) forms)
                                     r #t label*)]
                                 [(exports exports-to-check iface-vector) (determine-exports 'module orig *expspec** r)])
                    ; valid bound ids checked already by chi-internal
                     (let ([iface (make-interface (wrap-marks (syntax-object-wrap id)) iface-vector)]
                           [vars (append *vars vars)]
                           [vals (append *vals vals)]
                           [inits (append inits *inits *body)])
                       (let ([label (make-local-label (make-binding '$module iface) (fxlognot (meta-level)))])
                         (record-id! defn-table id label)
                         (extend-ribcage! ribcage id label)
                         (unless (eq? (id->label id empty-wrap) label)
                          ; must be an enclosing local-syntax binding for id
                           (syntax-error orig "definition not permitted"))
                         (parse (cdr body)
                           vars vals inits expspec** iexport*
                           (lambda ()
                             (chexports)
                             (*chexports)
                             (check-exports! *defn-table source-exp (apply append exports-to-check *iexport*)))
                           #f (cons label label*))))))]
                [(implicit-exports-form)
                 (parse-implicit-exports e w ae)
                 (parse (cdr body) vars vals inits expspec** iexport* chexports #f label*)]
                [(define-property-form)
                 (let-values ([(id key-id expr w) (parse-define-property e w ae)])
                   (let* ([id (wrap id w)]
                          [id-label/pl (id->label/pl id empty-wrap)]
                          [key-id-label (id->label key-id w)]
                          [prop-label (make-local-label
                                        (make-binding 'property (local-eval-hook (meta-chi expr r w)))
                                        (fxlognot (meta-level)))])
                     (unless id-label/pl (syntax-error id "no visible binding for define-property id"))
                     (unless key-id-label (syntax-error (wrap key-id w) "no visible binding for define-property key"))
                     (let ([id-label (label/pl->label id-label/pl)])
                       (extend-ribcage! ribcage id
                         (make-label/pl id-label (cons key-id-label prop-label) (label/pl->pl id-label/pl)))
                       (unless (eq? (id->label id empty-wrap) id-label)
                        ; must be an enclosing local-syntax binding for id
                         (syntax-error (source-wrap e w ae)
                           "definition not permitted"))
                       (record-property! defn-table id id-label)
                       (parse (cdr body) vars vals inits expspec** iexport* chexports #f (cons prop-label label*)))))]
                [(alias-form)
                 (let-values ([(new-id old-id) (parse-alias e w ae)])
                   (let* ([new-id (wrap new-id w)]
                          [label/pl (id->label/pl old-id w)]
                          [label (label/pl->label label/pl)])
                     (unless label (displaced-lexical-error old-id "create an alias to" #f))
                     (extend-ribcage! ribcage new-id label/pl)
                     (unless (eq? (id->label new-id empty-wrap) label)
                      ; must be an enclosing local-syntax binding for new-id
                       (syntax-error (source-wrap e w ae)
                         "definition not permitted"))
                     (record-id! defn-table new-id label)
                     (parse (cdr body)
                       vars
                       vals
                       inits expspec** iexport* chexports
                       #f label*)))]
                [(eval-when-form)
                 (let-values ([(when-list forms) (parse-eval-when e w ae)])
                   (parse (if (memq 'eval when-list) ; mode set is implicitly (E)
                              (let f ((forms forms))
                                (if (null? forms)
                                    (cdr body)
                                    (cons (make-frob (wrap (car forms) w) meta?)
                                          (f (cdr forms)))))
                              (cdr body))
                     vars vals inits expspec** iexport* chexports #f label*))]
                [(meta-form)
                 (parse (cons (make-frob (wrap (parse-meta e w ae) w) #t)
                              (cdr body))
                   vars vals inits expspec** iexport* chexports #t label*)]
                [(local-syntax-form)
                 (let-values ([(forms w ae new-label*) (chi-local-syntax value #t e r w ae)])
                   (parse (let f ((forms forms))
                            (if (null? forms)
                                (cdr body)
                                (cons (make-frob (wrap (car forms) w) meta?)
                                      (f (cdr forms)))))
                     vars vals inits expspec** iexport* chexports #f (append new-label* label*)))]
                [($library-form)
                 (let-values ([(orig . ignore) (parse-library e w ae w)])
                   (syntax-error orig "invalid context for library form"))]
                [else ; found a non-definition
                 (when meta-seen? (syntax-error (source-wrap e w ae) "invalid meta definition"))
                 (let f ([body (cons (make-frob (source-wrap e w ae) meta?) (cdr body))])
                   (if (or (null? body) (not (frob-meta? (car body))))
                       (return body vars vals inits expspec** iexport* chexports label*)
                       (begin
                        ; expand meta inits for effect only
                         (top-level-eval-hook (meta-chi-frob (car body) r))
                         (f (cdr body)))))])))))))

(define parse-library
  (lambda (e w ae body-wrap)
    (syntax-case e ()
      [(_ orig (dir ... file) library-version uid form ...)
       (values #'orig (datum (dir ... file)) (datum library-version) (strip #'uid w) (wrap #'file w)
         (map (lambda (x) (wrap x body-wrap)) #'(form ...)))]
      [_ (syntax-error (source-wrap e w ae))])))

(define parse-program
  (lambda (e w ae body-wrap)
    (syntax-case e ()
      [(_ orig form ...)
       (values #'orig
         (syntax-case #'orig () [(k . stuff) (wrap #'k w)])
         (map (lambda (x) (wrap x body-wrap)) #'(form ...)))]
      [_ (syntax-error (source-wrap e w ae))])))

(define parse-module
  (lambda (e w ae *w)
    (syntax-case e ()
      [(_ orig mid form ...)
       (id? (syntax mid))
      ; mid receives old wrap so it won't be confused with id of same name
      ; defined within the module
       (values #'orig (wrap #'mid w) (map (lambda (x) (wrap x *w)) #'(form ...)))]
      [_ (syntax-error (source-wrap e w ae))])))

(define parse-import
  (lambda (e w ae)
    (syntax-case e ()
      [(_ orig (impspec ...) import-only? std?)
       (values #'orig #'(impspec ...) (strip #'import-only? w) (strip #'std? w))]
      [_ (syntax-error (source-wrap e w ae))])))

(define parse-export
  (lambda (e w ae)
    (syntax-case e ()
      [(_ ex ...) (map (lambda (x) (wrap x w)) #'(ex ...))]
      [_ (syntax-error (source-wrap e w ae))])))

(define parse-indirect-export
  (lambda (e w ae)
    (syntax-case e ()
      [(_ id id* ...)
       (and (id? #'id) (andmap id? #'(id* ...)))
       (values (wrap #'id w) (map (lambda (x) (wrap x w)) #'(id* ...)))]
      [_ (syntax-error (source-wrap e w ae))])))

(define parse-implicit-exports
  (lambda (e w ae)
    (syntax-case e ()
      [(_ #f) #f]
      [(_ #t) #t]
      [_ (syntax-error (source-wrap e w ae))])))

(module (determine-imports determine-exports)
  (define (module-exports imp-iface)
    (define vmap
      (lambda (fn v)
        (do ((i (fx- (vector-length v) 1) (fx- i 1))
             (ls '() (cons (fn (vector-ref v i)) ls)))
            ((fx< i 0) ls))))
    (let ([iface (get-indirect-interface (import-interface-interface imp-iface))]
          [new-marks (import-interface-new-marks imp-iface)])
      (vmap
        (if (null? new-marks)
            (lambda (id) (cons id id))
            (lambda (id)
              (let ([id (make-resolved-id
                          (id-sym-name id)
                          (join-marks new-marks (id-marks id))
                          (resolved-id->label/pl id))])
                (cons id id))))
        (interface-exports iface))))

  (define help-determine-imports
   ; returns an import-interface or a list of (old-id . new-id) pairs
   ; where new-id is a resolved id (resolved to old-id's label/pl)
    (lambda (impspec r std?)
     ; CSV7:
     ;   <import spec> -> <import set>
     ;   <import set> ->
     ;     id
     ;     (only <import set> <id>*)
     ;     (except <import set> <id>*)
     ;     (add-prefix <import set> <id>)
     ;     (drop-prefix <import set> <id>)
     ;     (rename <import set> (<to-id> <from-id>)*)
     ;     (alias <import set> (<to-id> <from-id>)*)
     ; R6RS:
     ;   <import spec> -> <import set> | (for <import set> <import level>*)
     ;   <import level> -> run | expand | (meta <exact integer>)
     ;   <import set> ->
     ;     <library ref>
     ;     (library <library ref>)
     ;     (only <import set> <id>*)
     ;     (except <import set> <id>*)
     ;     (prefix <import set> <id>)
     ;     (rename <import set> (<from-id> <to-id>)*)
     ;   <library ref> -> (<id> <id>*) | (<id> <id>* <version ref>)
     ;   <version ref> ->
     ;     (<sub-version ref>*)
     ;     (and <version ref>*)
     ;     (or <version ref>*)
     ;     (not <version ref>)
     ;   <sub-version ref> ->
     ;     (>= <sub-version>)
     ;     (<= <sub-version>)
     ;     (and <sub-version ref>*)
     ;     (or <sub-version ref>*)
     ;     (not <sub-version ref>)
     ; CSV8:
     ;   <import spec> -> <import set> | (for <import set> <import level>*)
     ;   <import level> -> run | expand | (meta <exact integer>)
     ;   <import set> ->
     ;     <module ref>
     ;     <library ref>
     ;     (library <library ref>)
     ;     (only <import set> <id>*)
     ;     (except <import set> <id>*)
     ;     (prefix <import set> <id>)
     ;     (add-prefix <import set> <id>)
     ;     (drop-prefix <import set> <id>)
     ;     (rename <import set> (<from-id> <to-id>)*)     ; incompatible change
     ;     (alias <import set> (<to-id> <from-id>)*)      ; incompatible change
     ;   <module ref> -> <id>
     ;   <library ref> -> (<id> <id>*) | (<id> <id>* <version ref>)
     ;   <version ref> ->
     ;     (<sub-version ref>* <sub-version ref>)
     ;     (and <version ref>*)
     ;     (or <version ref>*)
     ;     (not <version ref>)
     ;   <sub-version ref> ->
     ;     (>= <sub-version>)
     ;     (<= <sub-version>)
     ;     (and <sub-version ref>*)
     ;     (or <sub-version ref>*)
     ;     (not <sub-version ref>)
      (define (determine-module-imports what who mid tid)
        (let ([binding (lookup (id->label mid empty-wrap) r)])
          (case (binding-type binding)
            [($module)
             (let ([x (binding-value binding)])
               (define diff-marks
                 (lambda (m1 m2)
                   (let ([n1 (length m1)] [n2 (length m2)])
                     (let f ([n1 n1] [m1 m1])
                       (cond
                         [(> n1 n2) (cons (car m1) (f (- n1 1) (cdr m1)))]
                         [(equal? m1 m2) '()]
                         [else (syntax-error impspec (format "out-of-context ~a reference" what))])))))
               (values mid tid
                 (make-import-interface x
                   (diff-marks (id-marks tid) (interface-marks (get-indirect-interface x))))))]
            [else (syntax-error who (format "unknown ~a" what))])))
      (define (impset x)
        (syntax-case x ()
          [(?only *x id ...)
           (sym-kwd? ?only only)
           (begin
             (unless (andmap id? #'(id ...))
               (syntax-error x "invalid import set"))
             (let-values ([(mid tid imps) (impset #'*x)])
               (values mid tid
                 (if (import-interface? imps)
                     (let ([iface (get-indirect-interface (import-interface-interface imps))]
                           [new-marks (import-interface-new-marks imps)])
                       (let f ([id* #'(id ...)] [new-imps '()])
                         (if (null? id*)
                             new-imps
                             (let* ([id (car id*)] [sym (id-sym-name id)] [marks (id-marks id)])
                               (cond
                                 [(iface-id->label/pl sym marks iface new-marks) =>
                                  (lambda (label/pl)
                                    (f (cdr id*)
                                       (let ([id (make-resolved-id sym marks label/pl)])
                                         (cons (cons id id) new-imps))))]
                                 [else (syntax-error x (format "missing import for ~s" sym))])))))
                     (let f ([id* #'(id ...)] [new-imps '()])
                       (if (null? id*)
                           new-imps
                           (let ([id (car id*)])
                             (cond
                               [(find (lambda (a) (bound-id=? id (cdr a))) imps) =>
                                (lambda (a) (f (cdr id*) (cons a new-imps)))]
                               [else (syntax-error x (format "missing import for ~s" (id-sym-name id)))]))))))))]
          [(?except *x id ...)
           (sym-kwd? ?except except)
           (begin
             (unless (andmap id? #'(id ...))
               (syntax-error x "invalid import set"))
             (let-values ([(mid tid imps) (impset #'*x)])
               (let ([imps (if (import-interface? imps) (module-exports imps) imps)])
                 (values mid tid
                   (let f ([imps imps] [id* #'(id ...)] [new-imps '()])
                     (if (null? imps)
                         (if (null? id*)
                             new-imps
                             (syntax-error x (format "missing import for ~s" (id-sym-name (car id*)))))
                         (let* ([a (car imps)] [id (cdr a)])
                           (if (bound-id-member? id id*)
                               (f (cdr imps) (remp (lambda (x) (bound-id=? id x)) id*) new-imps)
                               (f (cdr imps) id* (cons a new-imps))))))))))]
          [(?prefix *x prefix-id)
           (if std? (sym-kwd? ?prefix prefix) (sym-kwd? ?prefix prefix add-prefix))
           (let ()
             (define prefix-add
               (lambda (prefix id)
                 (make-resolved-id
                   (string->symbol
                     (string-append prefix
                       (symbol->string (id-sym-name id))))
                   (id-marks id)
                   (resolved-id->label/pl id))))
             (unless (id? #'prefix-id) (syntax-error x "invalid import set"))
             (let-values ([(mid tid imps) (impset #'*x)])
               (let ([imps (if (import-interface? imps) (module-exports imps) imps)])
                 (values mid tid
                   (let ([prefix-str (symbol->string (id-sym-name #'prefix-id))])
                     (let f ([imps imps] [new-imps '()])
                       (if (null? imps)
                           new-imps
                           (let ([a (car imps)])
                             (f (cdr imps) (cons (cons (car a) (prefix-add prefix-str (cdr a))) new-imps))))))))))]
          [(?drop-prefix *x prefix-id)
           (and (not std?) (sym-kwd? ?drop-prefix drop-prefix))
           (let ()
             (define prefix-drop
               (lambda (prefix id)
                 (let ([s (symbol->string (id-sym-name id))])
                   (let ([np (string-length prefix)] [ns (string-length s)])
                     (unless (and (>= ns np) (string=? (substring s 0 np) prefix))
                       (syntax-error x (format "missing expected prefix on ~s" (id-sym-name id))))
                     (make-resolved-id
                       (string->symbol (substring s np ns))
                       (id-marks id)
                       (resolved-id->label/pl id))))))
             (unless (id? #'prefix-id) (syntax-error x "invalid import set"))
             (let-values ([(mid tid imps) (impset #'*x)])
               (let ([imps (if (import-interface? imps) (module-exports imps) imps)])
                 (values mid tid
                   (let ([prefix-str (symbol->string (id-sym-name #'prefix-id))])
                     (let f ([imps imps] [new-imps '()])
                       (if (null? imps)
                           new-imps
                           (let ([a (car imps)])
                             (f (cdr imps) (cons (cons (car a) (prefix-drop prefix-str (cdr a))) new-imps))))))))))]
          [(?rename *x [old-id new-id] ...)
           (sym-kwd? ?rename rename)
           (begin
             (unless (and (andmap id? #'(old-id ...)) (andmap id? #'(new-id ...)))
               (syntax-error x "invalid import set"))
             (let-values ([(mid tid imps) (impset #'*x)])
               (let ([imps (if (import-interface? imps) (module-exports imps) imps)])
                 (values mid tid
                   (let f ([imps imps] [o.n* #'((old-id . new-id) ...)] [new-imps '()])
                     (if (null? imps)
                         (if (null? o.n*)
                             new-imps
                             (syntax-error x (format "missing import for ~s" (id-sym-name (caar o.n*)))))
                         (let* ([a (car imps)] [id (cdr a)])
                           (cond
                             [(find (lambda (o.n) (bound-id=? id (car o.n))) o.n*) =>
                              (lambda (o.n)
                                (let ([new-id (make-resolved-id (id-sym-name (cdr o.n)) (id-marks id) (resolved-id->label/pl id))])
                                  (f (cdr imps) (remq o.n o.n*) (cons (cons (cdr a) new-id) new-imps))))]
                             [else (f (cdr imps) o.n* (cons a new-imps))]))))))))]
          [(?alias *x [old-id new-id] ...)
           (and (not std?) (sym-kwd? ?alias alias))
           (begin
             (unless (and (andmap id? #'(old-id ...)) (andmap id? #'(new-id ...)))
               (syntax-error x "invalid import set"))
             (let-values ([(mid tid imps) (impset #'*x)])
               (let ([imps (if (import-interface? imps) (module-exports imps) imps)])
                 (values mid tid
                   (let f ([imps imps] [o.n* #'((old-id . new-id) ...)] [new-imps '()])
                     (if (null? imps)
                         (if (null? o.n*)
                             new-imps
                             (syntax-error x (format "missing import for ~s" (id-sym-name (caar o.n*)))))
                         (let* ([a (car imps)] [id (cdr a)])
                           (cond
                             [(find (lambda (o.n) (bound-id=? id (car o.n))) o.n*) =>
                              (lambda (o.n)
                                (let ([new-id (make-resolved-id (id-sym-name (cdr o.n)) (id-marks id) (resolved-id->label/pl id))])
                                  (f imps (remq o.n o.n*) (cons (cons (cdr a) new-id) new-imps))))]
                             [else (f (cdr imps) o.n* (cons a new-imps))]))))))))]
          [mid
           (and (not std?) (id? #'mid))
           (determine-module-imports "module" #'mid #'mid #'mid)]
          [(?library-reference lr)
           (sym-kwd? ?library-reference library)
           (let-values ([(mid tid) (lookup-library #'lr)])
             (determine-module-imports "library" #'lr mid tid))]
          [lr (let-values ([(mid tid) (lookup-library #'lr)])
                (determine-module-imports "library" #'lr mid tid))]))
      (syntax-case impspec (for)
        [(?for *x level ...)
         (sym-kwd? ?for for)
         (begin
           (for-each
             (lambda (x)
               (unless (syntax-case x ()
                         [?id (sym-kwd? ?id run expand) #t]
                         [(?meta n)
                          (sym-kwd? ?meta meta)
                          (and (integer? (datum n)) (exact? (datum n)))]
                         [_ #f])
                 (syntax-error x "invalid import level")))
             #'(level ...))
           (impset #'*x))]
        [*x (impset #'*x)])))

  (define determine-imports
   ; returns an import-interface or a list of resolved identifiers
    (lambda (impspec r std?)
      (let-values ([(mid tid imps) (help-determine-imports impspec r std?)])
        (values mid tid
          (if (import-interface? imps)
              imps
              (map cdr imps))))))

  (define determine-exports
    (lambda (what src expspec** r)
      (define resolve&add-id
        (lambda (old new id*)
          (cond
            [(id->label/pl old empty-wrap) =>
             (lambda (label/pl)
               (add-id (make-resolved-id (id-sym-name new) (id-marks new) label/pl) id*))]
           ; leave id out...no matter, since chexports will error out at the appropriate time
            [else id*])))
      (define add-id
        (let ([ht (make-hashtable symbol-hash eq?)])
          (lambda (id id*)
            (let ([a (symbol-hashtable-cell ht (id-sym-name id) '())])
              (cond
                [(find (lambda (x) (bound-id=? id x)) (cdr a)) =>
                 (lambda (x)
                   (if (equal? (resolved-id->label/pl id) (resolved-id->label/pl x))
                       id*
                       (syntax-error src (format "attempt to export multiple bindings for ~s from ~a" (id-sym-name id) what))))]
                [else (set-cdr! a (cons id (cdr a))) (cons id id*)])))))
     ; tail-recur on expspec**, since it is given in reverse, but
     ; nontail-recur on each expspec*, since it is not.
     ; this maintains lexicographic order for exports-to-check
      (let g ([expspec** expspec**] [exports '()] [exports-to-check '()] [new-exports '()])
        (if (null? expspec**)
            (values exports exports-to-check (list->vector new-exports))
            (let-values ([(exports exports-to-check new-exports)
                          (let f ([expspec* (car expspec**)])
                            (if (null? expspec*)
                                (values exports exports-to-check new-exports)
                                (let-values ([(exports exports-to-check new-exports) (f (cdr expspec*))])
                                  (let ([x (car expspec*)])
                                    (syntax-case x ()
                                      [id
                                       (id? #'id)
                                       (values
                                         (cons #'id exports)
                                         (cons #'id exports-to-check)
                                         (resolve&add-id #'id #'id new-exports))]
                                      [(?rename (old-id new-id) ...)
                                       (and (sym-kwd? ?rename rename)
                                            (andmap id? #'(old-id ...))
                                            (andmap id? #'(new-id ...)))
                                       (values
                                         (append #'(old-id ...) exports)
                                         (append #'(old-id ...) exports-to-check)
                                         (fold-right resolve&add-id new-exports #'(old-id ...) #'(new-id ...)))]
                                      [(?import impspec ...)
                                       (sym-kwd? ?import import)
                                       (let process-impspecs ([impspec* #'(impspec ...)])
                                         (if (null? impspec*)
                                             (values exports exports-to-check new-exports)
                                             (let-values ([(_mid _tid imps) (help-determine-imports (car impspec*) r #f)]
                                                          [(exports exports-to-check new-exports) (process-impspecs (cdr impspec*))])
                                               (let ([imps (if (import-interface? imps) (module-exports imps) imps)])
                                                 (values
                                                  (append (map car imps) exports)
                                                  exports-to-check
                                                  (fold-right add-id new-exports (map cdr imps)))))))]
                                      [_ (syntax-error x "invalid export spec")])))))])
              (g (cdr expspec**) exports exports-to-check new-exports))))))
)

(define parse-define
  (lambda (e w ae)
    (syntax-case e ()
      [(_ name val) (id? #'name) (values #'name #'val w ae)]
      [(_ (name . args) e1 e2 ...)
       (and (id? #'name) (valid-bound-ids? (lambda-var-list #'args)))
       (values
         (wrap #'name w)
         (source-wrap (cons #'lambda (wrap #'(args e1 e2 ...) w)) empty-wrap ae)
         empty-wrap
         #f)]
      [(_ name)
       (id? #'name)
       (values (wrap #'name w) #'(void) empty-wrap ae)]
      [_ (syntax-error (source-wrap e w ae))])))

(define parse-define-syntax
  (lambda (e w ae)
    (syntax-case e ()
      ((_ (name id) e1 e2 ...)
       (and (id? (syntax name)) (id? (syntax id)))
       (values (wrap (syntax name) w)
               `(,(syntax lambda) ,(wrap (syntax (id)) w)
                   ,@(wrap (syntax (e1 e2 ...)) w))
               empty-wrap))
      ((_ name val)
       (id? (syntax name))
       (values (syntax name) (syntax val) w))
      (_ (syntax-error (source-wrap e w ae))))))

(define parse-define-property
  (lambda (e w ae)
    (syntax-case e ()
      ((_ name prop expr)
       (and (id? #'name) (id? #'prop))
       (values #'name #'prop #'expr w))
      (_ (syntax-error (source-wrap e w ae))))))

(define parse-meta
  (lambda (e w ae)
    (syntax-case e ()
      ((_ . form) (syntax form))
      (_ (syntax-error (source-wrap e w ae))))))

(define parse-eval-when
  (lambda (e w ae)
    (syntax-case e ()
      ((_ (x ...) e1 e2 ...)
       (values (chi-when-list (syntax (x ...)) w) (syntax (e1 e2 ...))))
      (_ (syntax-error (source-wrap e w ae))))))

(define parse-alias
  (lambda (e w ae)
    (syntax-case e ()
      ((_ new-id old-id)
       (and (id? (syntax new-id)) (id? (syntax old-id)))
       (values (syntax new-id) (syntax old-id)))
      (_ (syntax-error (source-wrap e w ae))))))

(define parse-begin
  (lambda (e w ae empty-okay?)
    (syntax-case e ()
      ((_) empty-okay? '())
      ((_ e1 e2 ...) (syntax (e1 e2 ...)))
      (_ (syntax-error (source-wrap e w ae))))))

(define chi-lambda-clause
  (lambda (e c r w)
    (syntax-case c ()
      (((id ...) e1 e2 ...)
       (let ((ids (syntax (id ...))))
         (if (not (valid-bound-ids? ids))
             (syntax-error e "invalid parameter list in")
             (let ([new-vars (map gen-var ids)])
               (let ([labels (map make-lexical-label new-vars)])
                 (let ([body (chi-body (syntax (e1 e2 ...))
                               e r (make-binding-wrap ids labels w))])
                   (map kill-local-label! labels)
                   (values new-vars body)))))))
      ((ids e1 e2 ...)
       (let ((old-ids (lambda-var-list (syntax ids))))
         (if (not (valid-bound-ids? old-ids))
             (syntax-error e "invalid parameter list in")
             (let ([new-vars (map gen-var old-ids)])
               (let ([labels (map make-lexical-label new-vars)])
                 (let ([body (chi-body (syntax (e1 e2 ...))
                               e r (make-binding-wrap old-ids labels w))])
                   (map kill-local-label! labels)
                   (values
                     (let f ((ls1 (cdr new-vars)) (ls2 (car new-vars)))
                       (if (null? ls1)
                           ls2
                           (f (cdr ls1) (cons (car ls1) ls2))))
                     body)))))))
      (_ (syntax-error e)))))

(define chi-local-syntax
  (lambda (rec? defn? e r w ae)
    (define (go ids vals body*)
      (if (not (valid-bound-ids? ids))
          (invalid-ids-error
            (map (lambda (x) (wrap x w)) ids)
            (source-wrap e w ae)
            "keyword")
          (let ([labels (map (lambda (id)
                               (make-local-label displaced-lexical-binding (fxlognot (meta-level))))
                          ids)])
            (let ([new-w (make-binding-wrap ids labels w)])
              (let ([b* (let ([w (if rec? new-w w)])
                         ; chi-body note re: require-invoke applies here too
                          (fluid-let ([require-invoke (library-collector #t)]
                                      [require-visit (library-collector #f)])
                            (map (lambda (x)
                                   (defer-or-eval-transformer (if rec? 'letrec-syntax 'let-syntax)
                                     local-eval-hook
                                     (meta-chi x r w)))
                                 vals)))])
                (for-each local-label-binding-set! labels b*)
                (values body* new-w ae labels))))))
    (syntax-case e ()
      [(_ ((id val) ...) e1 e2 ...)
       (go #'(id ...) #'(val ...) #'(e1 e2 ...))]
      [(_ ((id val) ...))
       defn?
       (go #'(id ...) #'(val ...) '())]
      [_ (syntax-error (source-wrap e w ae))])))

;;; lift the syntax-object out of the body...
(define ellipsis?
  (lambda (x)
    (and (nonsymbol-id? x)
         (free-id=? x (syntax (... ...))))))

;;; strips syntax-objects down to top-wrap; if top-wrap is layered directly
;;; on an annotation, strips the annotation as well.
;;; since only the head of a list is annotated by the reader, not each pair
;;; in the spine, we also check for pairs whose cars are annotated in case
;;; we've been passed the cdr of an annotated list.  we don't recur through
;;; all top-marked expressions to avoid traversing large or even cyclic
;;; structures

(define strip
  (lambda (x w)
    (if (top-marked? w)
        (if (annotation? x)
            (annotation-stripped x)
            (if (and (pair? x) (annotation? (car x)))
                (let f ([x x])
                  (cond
                    [(pair? x)
                     (cons (f (car x)) (f (cdr x)))]
                    [(annotation? x) (annotation-stripped x)]
                    [else x]))
                x))
        (let f ([x x])
          (cond
            [(syntax-object? x)
             (strip (syntax-object-expression x) (syntax-object-wrap x))]
           ; if we see an annotation before the top mark, it must be a floating
           ; annotation created by source-wrap
            [(annotation? x) (annotation-stripped x)]
            [(pair? x)
             (let ([a (f (car x))] [d (f (cdr x))])
               (if (and (eq? a (car x)) (eq? d (cdr x)))
                   x
                   (cons a d)))]
            [(vector? x)
             (let ([new (vector-map f x)])
               (define eq-elts?
                 (lambda (v1 v2)
                   (let f ([n (vector-length v1)])
                     (or (fx= n 0)
                         (let ([n (fx- n 1)])
                           (and (eq? (vector-ref v1 n) (vector-ref v2 n))
                                (f n)))))))
               (if (eq-elts? new x) x new))]
            [(box? x)
             (let ([old (unbox x)])
               (let ([new (f old)])
                 (if (eq? old new) x (box new))))]
            [else x])))))

;;; lexical variables

(define gen-var
  (lambda (id)
    (let ((id (if (syntax-object? id) (syntax-object-expression id) id)))
      (if (annotation? id)
          (build-lexical-var id (annotation-expression id))
          (build-lexical-var id id)))))

(define lambda-var-list
  (lambda (vars)
    (let lvl ((vars vars) (ls '()) (w empty-wrap))
       (cond
         ((pair? vars) (lvl (cdr vars) (cons (wrap (car vars) w) ls) w))
         ((id? vars) (cons (wrap vars w) ls))
         ((null? vars) ls)
         ((syntax-object? vars)
          (lvl (syntax-object-expression vars)
               ls
               (join-wraps w (syntax-object-wrap vars))))
         ((annotation? vars)
          (lvl (annotation-expression vars) ls w))
       ; include anything else to be caught by subsequent error
       ; checking
         (else (cons vars ls))))))

(define-threaded require-import
  (lambda (uid)
    ($oops 'sc-expand-internal "no one is collecting import requirements")))

(define-threaded require-include
  (lambda (path)
    ($oops 'sc-expand-internal "no one is collecting include requirements")))

(module (install-library install-library/ct-desc install-library/rt-desc
         install-library/ct-code install-library/rt-code uninstall-library
         create-library-uid load-library lookup-library)
  (module (search-loaded-libraries record-loaded-library delete-loaded-library list-loaded-libraries loaded-libraries-root)
    (module (make-root insert-path delete-path search-path list-paths)
      (define-record-type dir
        (fields (immutable name) (immutable dir*) (immutable file*))
        (nongenerative #{dir htcavk0jv3uhhtakfluarlapg-0})
        (sealed #t))
      (define-record-type file
        (fields (immutable name) (immutable lib))
        (nongenerative #{file htcavk0jv3uhhtakfluarlapg-1})
        (sealed #t))
      (define make-root (lambda () (make-dir "root" '() '())))
      (define insert-path
        (lambda (root path uid)
          (let f ([dot root] [path path])
            (cond
              [(null? (cdr path))
               (make-dir
                 (dir-name dot)
                 (dir-dir* dot)
                 (cons
                   (make-file (car path) uid)
                   (remp (lambda (x) (eq? (file-name x) (car path)))
                     (dir-file* dot))))]
              [(find (lambda (x) (eq? (dir-name x) (car path))) (dir-dir* dot)) =>
               (lambda (dir)
                 (make-dir
                   (dir-name dot)
                   (cons (f dir (cdr path)) (remq dir (dir-dir* dot)))
                   (dir-file* dot)))]
              [else
               (make-dir
                 (dir-name dot)
                 (cons (f (make-dir (car path) '() '()) (cdr path)) (dir-dir* dot))
                 (dir-file* dot))]))))
      (define delete-path
        (lambda (root path)
          (let f ([dot root] [path path])
            (cond
              [(null? (cdr path))
               (make-dir
                 (dir-name dot)
                 (dir-dir* dot)
                 (remp (lambda (x) (eq? (file-name x) (car path)))
                   (dir-file* dot)))]
              [(find (lambda (x) (eq? (dir-name x) (car path))) (dir-dir* dot)) =>
               (lambda (dir)
                 (make-dir
                   (dir-name dot)
                   (cons (f dir (cdr path)) (dir-dir* dot))
                   (dir-file* dot)))]))))
      (define search-path
        (lambda (root path)
          (let loop ([dot root] [path path])
            (cond
              [(null? (cdr path))
               (cond
                 [(find (lambda (x) (eq? (file-name x) (car path))) (dir-file* dot)) => file-lib]
                 [else #f])]
              [(find (lambda (x) (eq? (dir-name x) (car path))) (dir-dir* dot)) =>
               (lambda (dir) (loop dir (cdr path)))]
              [else #f]))))
      (define list-paths
        (lambda (root)
          (define (SubDir path)
            (lambda (dir name*)
              (Dir (cons (dir-name dir) path) dir name*)))
          (define (Dir path dir name*)
            (fold-right (File path)
              (fold-right (SubDir path) name* (dir-dir* dir))
              (dir-file* dir)))
          (define (File path)
            (lambda (file name*)
              (if (get-library-descriptor (file-lib file))
                  (cons (reverse (cons (file-name file) path)) name*)
                  name*)))
          (Dir '() root '()))))
    (define root (make-root))
    (define search-loaded-libraries
      (case-lambda
        [(path) (search-path root path)]
        [(root path) (search-path root path)]))
    (define delete-loaded-library
      (case-lambda
        [(path) (set! root (delete-path root path))]
        [(root path) (delete-path root path)]))
    (define record-loaded-library
      (case-lambda
        [(path uid) (set! root (insert-path root path uid))]
        [(root path uid) (insert-path root path uid)]))
    (define list-loaded-libraries
      (case-lambda
        [() (list-paths root)]
        [(root) (list-paths root)]))
    (define loaded-libraries-root
      (lambda () root))
    ;; for bootstrapping via "reboot.ss":
    (set! $loaded-libraries
      (case-lambda
        [() root]
        [(r) (set! root r)])))

  (define install-library/ct-desc
    (lambda (path version uid outfn importer visible? ctdesc)
      (with-tc-mutex
        (record-loaded-library path uid)
        (put-library-descriptor uid
          (let ([desc (get-library-descriptor uid)])
            (make-libdesc path version outfn (or (and desc (libdesc-importer desc)) importer) #f visible?
              ctdesc
              (and desc (libdesc-rtdesc desc))))))))

  (define install-library/rt-desc
    (lambda (path version uid outfn importer visible? rtdesc)
      (with-tc-mutex
        (record-loaded-library path uid)
        (put-library-descriptor uid
          (let ([desc (get-library-descriptor uid)])
            (make-libdesc path version outfn (or (and desc (libdesc-importer desc)) importer) #f visible?
              (and desc (libdesc-ctdesc desc))
              rtdesc))))))

  (define install-library
    (lambda (path uid desc)
      (with-tc-mutex
        (record-loaded-library path uid)
        (when desc (put-library-descriptor uid desc)))))

  (define-who install-library/ct-code
    (lambda (uid export-id* import-code visit-code)
      (let ([desc (get-library-descriptor uid)])
        (unless desc (sorry! who "unable to install visit code for non-existent library ~s" uid))
        (let ([ctdesc (libdesc-ctdesc desc)])
          (unless ctdesc (sorry! who "unable to install visit code for revisit-only library ~s" uid))
          (ctdesc-export-id*-set! ctdesc export-id*)
          (ctdesc-import-code-set! ctdesc import-code)
          (ctdesc-visit-code-set! ctdesc visit-code)))))

  (define-who install-library/rt-code
    (lambda (uid invoke-code)
      (let ([desc (get-library-descriptor uid)])
        (unless desc (sorry! who "unable to install invoke code for non-existent library ~s" uid))
        (let ([rtdesc (libdesc-rtdesc desc)])
          (unless rtdesc (sorry! who "unable to install invoke code for visit-only library ~s" uid))
          (rtdesc-invoke-code-set! rtdesc invoke-code)))))

  (define uninstall-library
    (lambda (path uid)
      (with-tc-mutex
        (rem-library-descriptor uid)
        (delete-loaded-library path))))

  (define create-library-uid
    (lambda (name)
      (syntax-case name ()
        [(dir-id ... file-id)
         (and (andmap id? #'(dir-id ...)) (id? #'file-id))
         (let ([uid (generate-id (datum file-id))])
           (values #'(dir-id ... file-id) '() (datum->syntax #'* uid)))]
        [(dir-id ... file-id (version ...))
         (andmap (lambda (x) (and (integer? x) (exact? x) (>= x 0))) (datum (version ...)))
         (let ([uid (generate-id (datum file-id))])
           (values #'(dir-id ... file-id) #'(version ...) (datum->syntax #'* uid)))]
        [_ (syntax-error name "invalid library name")])))

  (define library-search
    (lambda (who path dir* all-ext*)
      (let-values ([(src-path obj-path obj-exists?) ((library-search-handler) who path dir* all-ext*)])
        (unless (or (not src-path) (string? src-path))
          ($oops 'library-search-handler "returned invalid source-file path ~s" src-path))
        (unless (or (not obj-path) (string? obj-path))
          ($oops 'library-search-handler "returned invalid object-file path ~s" obj-path))
        (when (and obj-exists? (not obj-path))
          ($oops 'library-search-handler "claimed object file was found but returned no object-file path"))
        (values src-path obj-path obj-exists?))))

  (define internal-library-search
    (lambda (who path dir* all-ext*)
      (define-syntax with-message
        (syntax-rules ()
          [(_ msg e1 e2 ...)
           (begin
             (when (import-notify) (fprintf (console-output-port) "~s: ~a\n" who msg))
             e1 e2 ...)]))
      (define make-path
        (lambda (dir rpath ext)
          (if (or (string=? dir "") (string=? dir "."))
              (format "~a~a" rpath ext)
              (path-build dir (format "~a~a" rpath ext)))))
      (let ([rpath (fold-left (lambda (dir elem) (path-build dir (symbol->string elem))) (symbol->string (car path)) (cdr path))])
        (let dloop ([dir* (if (path-absolute? rpath)
                              (with-message (format "ignoring library-directories since ~s is absolute" rpath)
                                '(("" . "")))
                              dir*)])
          (if (null? dir*)
              (values #f #f #f)
              (let ([dir (car dir*)])
                (let src-loop ([ext* all-ext*])
                  (if (null? ext*)
                      (let obj-loop ([ext* all-ext*] [tried '()])
                        (if (null? ext*)
                            (dloop (cdr dir*))
                            (let ([ext (car ext*)])
                              (if (member (cdr ext) tried)
                                  (obj-loop (cdr ext*) tried)
                                  (let ([obj-path (make-path (cdr dir) rpath (cdr ext))])
                                    (if (file-exists? obj-path)
                                        (with-message (format "found object file ~s, continuing search for corresponding source file" obj-path)
                                          (values
                                            ; found object file...now see if we find source file in a source
                                            ; directory that's paired with the same object directory
                                            (let second-chance-dloop ([dir* (cdr dir*)])
                                              (if (null? dir*)
                                                  (with-message (format "did not find corresponding source file") #f)
                                                  (if (string=? (cdar dir*) (cdr dir))
                                                      (let second-chance-src-loop ([ext* all-ext*])
                                                        (if (null? ext*)
                                                            (second-chance-dloop (cdr dir*))
                                                            (if (string=? (cdar ext*) (cdr ext))
                                                                (let ([src-path (make-path (caar dir*) rpath (caar ext*))])
                                                                  (if (file-exists? src-path)
                                                                      (with-message (format "found corresponding source file ~s" src-path) src-path)
                                                                      (second-chance-src-loop (cdr ext*))))
                                                                (second-chance-src-loop (cdr ext*)))))
                                                      (second-chance-dloop (cdr dir*)))))
                                            obj-path
                                            #t))
                                        (with-message (format "did not find object file ~s" obj-path)
                                          (obj-loop (cdr ext*) (cons (cdr ext) tried)))))))))
                      (let ([ext (car ext*)])
                        (let ([src-path (make-path (car dir) rpath (car ext))])
                          (if (file-exists? src-path)
                              (with-message (format "found source file ~s" src-path)
                                (let ([obj-path (make-path (cdr dir) rpath (cdr ext))])
                                  (values src-path obj-path
                                    (cond
                                      [(equal? obj-path src-path) (with-message "source path and object path are the same" #t)]
                                      [(file-exists? obj-path) (with-message (format "found corresponding object file ~s" obj-path) #t)]
                                      [else (with-message (format "did not find corresponding object file ~s" obj-path) #f)]))))
                              (with-message (format "did not find source file ~s" src-path) (src-loop (cdr ext*))))))))))))))

  (define load-recompile-info
    (lambda (who fn)
      (let ([fn (let ([host-fn (format "~a.~s" (path-root fn) (machine-type))])
                  (if (file-exists? host-fn) host-fn fn))])
        (let ([ip ($open-file-input-port who fn)])
          (on-reset (close-port ip)
            (let ([fp (let ([start-pos (port-position ip)])
                        (if (and (eqv? (get-u8 ip) (char->integer #\#))
                                 (eqv? (get-u8 ip) (char->integer #\!))
                                 (let ([b (get-u8 ip)]) (or (eqv? b (char->integer #\space)) (eqv? b (char->integer #\/)))))
                            (let loop ([fp 3])
                              (let ([b (get-u8 ip)])
                                (if (eof-object? b)
                                    fp
                                    (let ([fp (+ fp 1)])
                                      (if (eqv? b (char->integer #\newline))
                                          fp
                                          (loop fp))))))
                            (begin (set-port-position! ip start-pos) 0)))])
              (if ($compiled-file-header? ip)
                  (let ([x (fasl-read ip)])
                    (close-port ip)
                    (unless (recompile-info? x) ($oops who "expected recompile info at start of ~s, found ~a" fn x))
                    x)
                  ($oops who "missing header for compiled file ~s" fn))))))))

  (define load-library
    (lambda (who path version-ref needed-uid importer-path ct? load-deps)
      (define-syntax with-message
        (syntax-rules ()
          [(_ msg e1 e2 ...)
           (begin
             (when (import-notify) (fprintf (console-output-port) "import: ~a\n" msg))
             e1 e2 ...)]))
      (define verify-uid
        (lambda (found-uid src-file-path)
          (when needed-uid
            (unless (eq? found-uid needed-uid)
              (let ([c ($make-recompile-condition importer-path)] [importer-path (or importer-path 'program)])
                (if src-file-path
                    ($oops/c who c
                      "loading ~a yielded a different compilation instance of ~s from that required by compiled ~s"
                      src-file-path
                      path
                      importer-path)
                    (let-values ([(outfn original-importer)
                                  (let ([desc (get-library-descriptor found-uid)])
                                    (if desc
                                        (values (libdesc-outfn desc) (libdesc-importer desc))
                                        (values #f #f)))])
                      ($oops/c who c
                        "compiled ~s requires a different compilation instance of ~s from the one previously ~:[compiled~;~:*loaded from ~a~]~@[ and originally imported by ~a~]"
                        importer-path
                        path
                        outfn
                        original-importer))))))))
      (define do-load-library
        (lambda (file-path situation)
          (parameterize ([source-directories (cons (path-parent file-path) (source-directories))])
            ($load-library file-path situation importer-path))
          (cond
            [(search-loaded-libraries path) =>
             (lambda (found-uid)
               (verify-version who path version-ref found-uid file-path file-path)
               (load-deps found-uid)
               (verify-uid found-uid file-path)
               found-uid)]
            [else ($oops who "loading ~a did not define library ~s" file-path path)])))
      (define do-compile-library
        (lambda (src-path obj-path)
          (parameterize ([source-directories (cons (path-parent src-path) (source-directories))])
            ((compile-library-handler) src-path obj-path))
          (cond
            [(search-loaded-libraries path) =>
             (lambda (found-uid)
               (verify-version who path version-ref found-uid obj-path src-path)
               (load-deps found-uid)
               (verify-uid found-uid src-path)
               found-uid)]
            [else ($oops who "compiling ~a did not define library ~s" src-path path)])))
      (define do-recompile-or-load-library
        (lambda (src-path obj-path)
          (let ([compiled? #f])
            (parameterize ([source-directories (cons (path-parent src-path) (source-directories))]
                           [compile-library-handler
                             (let ([clh (compile-library-handler)])
                               (lambda (src-path obj-path)
                                 (clh src-path obj-path)
                                 (set! compiled? #t)))])
              (maybe-compile-library src-path obj-path)
              (unless compiled?
                (with-message (format "no need to recompile, so loading ~s" obj-path)
                  ($load-library obj-path (if ct? 'visit 'revisit) importer-path))))
            (cond
              [(search-loaded-libraries path) =>
               (lambda (found-uid)
                 (verify-version who path version-ref found-uid obj-path src-path)
                 (load-deps found-uid)
                 (verify-uid found-uid src-path)
                 found-uid)]
              [else
                (if compiled?
                    ($oops who "compiling ~a did not define library ~s" src-path path)
                    ($oops who "loading ~a did not define library ~s" obj-path path))]))))
      (define do-load-library-src-or-obj
        (lambda (src-path obj-path)
          (define (load-source)
            (with-message "object file is out-of-date"
              (with-message (format "loading source file ~s" src-path)
                (do-load-library src-path 'load))))
          (let ([obj-path-mod-time (library-modification-time obj-path)])
            (if (time>=? obj-path-mod-time (library-modification-time src-path))
                ; NB: combine with $maybe-compile-file
                (let ([rcinfo (guard (c [else (with-message (with-output-to-string
                                                              (lambda ()
                                                                (display-string "failed to process object file: ")
                                                                (display-condition c)))
                                                #f)])
                                (load-recompile-info 'import obj-path))])
                  (if (and rcinfo
                           (parameterize ([source-directories (cons (path-parent src-path) (source-directories))])
                             (andmap
                               (lambda (x)
                                 ((guard (c [else (with-message (with-output-to-string
                                                                  (lambda ()
                                                                    (display-string "failed to find include file: ")
                                                                    (display-condition c)))
                                                    (lambda () #f))])
                                    (with-source-path 'import x
                                      (lambda (x)
                                        (lambda ()
                                          (and (file-exists? x)
                                               (time<=? (library-modification-time x) obj-path-mod-time))))))))
                               (recompile-info-include-req* rcinfo))))
                      ; NB: calling load-deps insures that we'll reload obj-path if one of
                      ; the deps has to be reloaded, but it will miss other libraries that might have
                      ; contributed to the generated code.  For example, if the source file imports
                      ; (a) and (b) but only (b) is one of the dependencies, we won't necessarily
                      ; reload if a.ss is newer than a.so.
                      (with-message "object file is not older"
                        (let ([found-uid (guard (c [(and ($recompile-condition? c) (eq? ($recompile-importer-path c) path))
                                                    (with-message (format "reloading ~s because a dependency has changed" src-path)
                                                      (parameterize ([source-directories (cons (path-parent src-path) (source-directories))])
                                                        ($load-library src-path 'load importer-path)))
                                                    (cond
                                                      [(search-loaded-libraries path) =>
                                                       (lambda (found-uid)
                                                         (verify-version who path version-ref found-uid obj-path src-path)
                                                         (load-deps found-uid)
                                                         found-uid)]
                                                      [else ($oops who "reloading ~a did not define library ~s" src-path path)])])
                                           (parameterize ([source-directories (cons (path-parent src-path) (source-directories))])
                                             (guard (c [(and (irritants-condition? c) (member obj-path (condition-irritants c)))
                                                        (with-message (with-output-to-string
                                                                        (lambda ()
                                                                          (display-string "failed to load object file: ")
                                                                          (display-condition c)))
                                                          ($oops/c who ($make-recompile-condition path)
                                                            "problem loading object file ~a ~s" obj-path c))])
                                               (let ([situation (if ct? 'visit 'revisit)])
                                                 (with-message (format "~sing object file ~s" situation obj-path)
                                                   ($load-library obj-path situation importer-path)))))
                                           (cond
                                             [(search-loaded-libraries path) =>
                                              (lambda (found-uid)
                                                (verify-version who path version-ref found-uid obj-path src-path)
                                                (load-deps found-uid)
                                                found-uid)]
                                             [else ($oops who "loading ~a did not define library ~s" obj-path path)]))])
                          (verify-uid found-uid src-path)
                          found-uid))
                      (load-source)))
                (load-source)))))
      ($pass-time 'load-library
        (lambda ()
          (cond
            [(search-loaded-libraries path) =>
             (lambda (found-uid)
               (verify-version who path version-ref found-uid #f #f)
               (verify-uid found-uid #f)
               (let ([desc (get-library-descriptor found-uid)])
                 (if ct?
                     (unless (libdesc-ctdesc desc)
                       (with-message (format "attempting to 'visit' previously 'revisited' ~s for library ~s compile-time info" (libdesc-outfn desc) path)
                         ($visit #f (libdesc-outfn desc) importer-path)))
                     (unless (libdesc-rtdesc desc)
                       (with-message (format "attempting to 'revisit' previously 'visited' ~s for library ~s run-time info" (libdesc-outfn desc) path)
                         ($revisit #f (libdesc-outfn desc) importer-path)))))
              ; need to call load-deps even if our library was already loaded,
              ; since we might, say, have previously loaded its invoke dependencies and
              ; now want to load its import dependencies
               (load-deps found-uid)
               found-uid)]
            [else
             (let-values ([(src-path obj-path obj-exists?) (library-search 'import path (library-directories) (library-extensions))])
               (if src-path
                   (if obj-exists?
                       (if (equal? obj-path src-path)
                           (with-message "source path and object path are the same"
                             (with-message (format "loading ~s" src-path)
                               (do-load-library src-path 'load)))
                           (if (and (compile-imported-libraries) $compiler-is-loaded?)
                               (do-recompile-or-load-library src-path obj-path)
                               (do-load-library-src-or-obj src-path obj-path)))
                       (if (and (compile-imported-libraries) $compiler-is-loaded?)
                           (with-message (format "compiling ~s to ~s" src-path obj-path)
                             (let f ([p obj-path])
                               (let ([p (path-parent p)])
                                 (unless (or (string=? p "") (file-exists? p))
                                   (f p)
                                   (with-message (format "creating subdirectory ~s" p) (mkdir p)))))
                             (do-compile-library src-path obj-path))
                           (with-message (format "loading source file ~s" src-path)
                             (do-load-library src-path 'load))))
                   (if obj-exists?
                       (let ([situation (if ct? 'visit 'revisit)])
                         (with-message (format "~sing object file ~s" situation obj-path)
                           (do-load-library obj-path situation)))
                       ($oops who "library ~s not found" path))))])))))

  (define version-okay?
    (lambda (version-ref version)
      (define sub-version-okay?
        (lambda (x sv)
          (syntax-case x (>= <= and or not)
            [(>= x) (>= sv #'x)]
            [(<= x) (<= sv #'x)]
            [(and x ...) (andmap (lambda (x) (sub-version-okay? x sv)) #'(x ...))]
            [(or x ...) (ormap (lambda (x) (sub-version-okay? x sv)) #'(x ...))]
            [(not x) (not (sub-version-okay? #'x sv))]
            [x (= sv #'x)])))
      (define version-okay?
        (lambda (x v)
          (syntax-case x (and or not)
            [(and x ...) (andmap (lambda (x) (version-okay? x v)) #'(x ...))]
            [(or x ...) (ormap (lambda (x) (version-okay? x v)) #'(x ...))]
            [(not x) (not (version-okay? #'x v))]
            [(x ...)
             (let loop ([x* #'(x ...)] [sv* v])
               (or (null? x*)
                   (and (not (null? sv*))
                        (sub-version-okay? (car x*) (car sv*))
                        (loop (cdr x*) (cdr sv*)))))])))
      (version-okay? version-ref version)))

  (define verify-version
    (lambda (who path version-ref found-uid file-path src-file-path)
      (let ([desc (get-library-descriptor found-uid)])
        (unless desc ($oops who "cyclic dependency involving import of library ~s" path))
        (let ([version (libdesc-version desc)])
          (unless (version-okay? version-ref version)
            (if src-file-path
                ($oops who "library ~s version mismatch: want ~s but found ~s at ~a" path version-ref version src-file-path)
                ($oops who "library ~s version mismatch: want ~s but ~s already loaded" path version-ref version)))))))

  (define version-ref?
    (lambda (x)
      (define sub-version?
        (lambda (x)
          (let ([x (syntax->datum x)])
            (and (integer? x) (exact? x) (fx>= x 0)))))
      (define sub-version-ref?
        (lambda (x)
          (syntax-case x ()
            [(?>= x) (sym-kwd? ?>= >=) (sub-version? #'x)]
            [(?<= x) (sym-kwd? ?<= <=) (sub-version? #'x)]
            [(?and x ...) (sym-kwd? ?and and) (andmap sub-version-ref? #'(x ...))]
            [(?or x ...) (sym-kwd? ?or or) (andmap sub-version-ref? #'(x ...))]
            [(?not x) (sym-kwd? ?not not) (sub-version-ref? #'x)]
            [x (sub-version? #'x)])))
      (syntax-case x ()
        [(?and x ...) (sym-kwd? ?and and) (andmap version-ref? #'(x ...))]
        [(?or x ...) (sym-kwd? ?or or) (andmap version-ref? #'(x ...))]
        [(?not x) (sym-kwd? ?not not) (version-ref? #'x)]
        [(x ...) (andmap sub-version-ref? #'(x ...))]
        [_ #f])))

  (define lookup-library
    (lambda (name)
      (define (do-lookup path tid version-ref)
        (cond
          [($import-library path version-ref #f) =>
           (lambda (uid)
             (require-import uid)
             (values (make-resolved-id uid (wrap-marks top-wrap) uid) tid))]
          [else (syntax-error name "unknown library")]))
      (syntax-case name ()
        [(dir-id ... file-id)
         (and (andmap id? #'(dir-id ...)) (id? #'file-id))
         (do-lookup (datum (dir-id ... file-id)) #'file-id '())]
        [(dir-id ... file-id version-ref)
         (and (andmap id? #'(dir-id ...)) (id? #'file-id) (version-ref? #'version-ref))
         (do-lookup (datum (dir-id ... file-id)) #'file-id (datum version-ref))]
        [_ (syntax-error name "invalid library reference")])))

  (define library-modification-time
    (lambda (fn)
      (if (eq? (library-timestamp-mode) 'modification-time)
          (file-modification-time fn)
          (make-time 'time-utc 0 0))))

  (set! import-notify
    ($make-thread-parameter #f
      (lambda (x) (and x #t))))

  (set! $library-search
    (lambda (who path dir* all-ext*)
      (library-search who path dir* all-ext*)))

  (set-who! default-library-search-handler
    (lambda (caller path dir* all-ext*)
      (define (string-pair? x) (and (pair? x) (string? (car x)) (string? (cdr x))))
      (unless (symbol? caller) ($oops who "~s is not a symbol" caller))
      (guard (c [else ($oops who "invalid library name ~s" path)])
        (unless (list? path) (raise #f))
        (let-values ([(path version uid) (create-library-uid path)])
          (void)))
      (unless (and (list? dir*) (andmap string-pair? dir*))
        ($oops who "invalid path list ~s" dir*))
      (unless (and (list? all-ext*) (andmap string-pair? all-ext*))
        ($oops who "invalid extension list ~s" all-ext*))
      (internal-library-search caller path dir* all-ext*)))

  (set-who! library-search-handler
    ($make-thread-parameter default-library-search-handler
      (lambda (x) (unless (procedure? x) ($oops who "~s is not a procedure" x)) x)))

  (set! library-list
    (lambda ()
      (list-loaded-libraries)))

  (set-who! library-timestamp-mode
    ($make-thread-parameter 'modification-time
      (lambda (x)
        (unless (or (eq? x 'modification-time)
                    (eq? x 'exists))
          ($oops who "~s is not a timestamp mode" x))
        x)))

  (set! expand-omit-library-invocations
    ($make-thread-parameter #f
      (lambda (v) (and v #t))))

  (set-who! verify-loadability
    (lambda (situation . input*)
      (define (parse-inputs input*)
        (let ([default-libdirs (library-directories)])
          (let loop ([input* input*] [rlibdirs* '()] [rfn* '()])
            (if (null? input*)
                (values (reverse rlibdirs*) (reverse rfn*))
                (let ([input (car input*)] [input* (cdr input*)])
                  (cond
                    [(string? input) (loop input* (cons default-libdirs rlibdirs*) (cons input rfn*))]
                    [(and (pair? input) (string? (car input)) (guard (c [else #f]) (parameterize ([library-directories (cdr input)]) #t)))
                     (loop input* (cons (cdr input) rlibdirs*) (cons (car input) rfn*))]
                    [else ($oops who "invalid input ~s: expected either a string or a pair of a string and a valid library-directories value" input)]))))))
      (define (get-lpinfo fn situation)
        (let ([fn (let ([host-fn (format "~a.~s" (path-root fn) (machine-type))])
                    (if (file-exists? host-fn) host-fn fn))])
          (let ([ip ($open-file-input-port who fn)])
            (on-reset (close-port ip)
              (let ([fp (let ([start-pos (port-position ip)])
                          (if (and (eqv? (get-u8 ip) (char->integer #\#))
                                   (eqv? (get-u8 ip) (char->integer #\!))
                                   (let ([b (get-u8 ip)]) (or (eqv? b (char->integer #\space)) (eqv? b (char->integer #\/)))))
                              (let loop ([fp 3])
                                (let ([b (get-u8 ip)])
                                  (if (eof-object? b)
                                      fp
                                      (let ([fp (+ fp 1)])
                                        (if (eqv? b (char->integer #\newline))
                                            fp
                                            (loop fp))))))
                              (begin (set-port-position! ip start-pos) 0)))])
                (unless ($compiled-file-header? ip) ($oops who "missing header for compiled file ~s" fn))
                (let ([x (fasl-read ip)])
                  (unless (recompile-info? x) ($oops who "expected recompile info at start of ~s, found ~a" fn x)))
                (let loop ([rlpinfo* '()])
                  (let ([x (fasl-read ip situation)])
                    (if (or (library-info? x) (program-info? x))
                        (loop (cons x rlpinfo*))
                        (begin (close-port ip) (reverse rlpinfo*))))))))))
      (unless (memq situation '(load visit revisit)) ($oops who "invalid situation ~s; should be one of load, visit, or revisit" situation))
      (let-values ([(libdirs* fn*) (parse-inputs input*)])
        (let ([root (loaded-libraries-root)] [uid-ht (make-eq-hashtable)])
          (define (check-ctdesc-libreqs! ctdesc importer)
            (unless (ctdesc-loaded-import-reqs ctdesc)
              (for-each (check-libreq! #t importer) (ctdesc-import-req* ctdesc))
              (ctdesc-loaded-import-reqs-set! ctdesc #t))
            (unless (ctdesc-loaded-visit-reqs ctdesc)
              (for-each (check-libreq! #t importer) (ctdesc-visit-visit-req* ctdesc))
              (for-each (check-libreq! #f importer) (ctdesc-visit-req* ctdesc))
              (ctdesc-loaded-visit-reqs-set! ctdesc #t)))
          (define (check-rtdesc-libreqs! rtdesc importer)
            (unless (rtdesc-loaded-invoke-reqs rtdesc)
              (for-each (check-libreq! #f importer) (rtdesc-invoke-req* rtdesc))
              (rtdesc-loaded-invoke-reqs-set! rtdesc #t)))
          (define (check-libreq! visit? importer)
            (lambda (libreq)
              (let ([path (libreq-path libreq)])
                (define (check-uid! found-uid obj-path)
                  (unless (eq? found-uid (libreq-uid libreq))
                    (if obj-path
                        ($oops who
                          "loading ~a yielded a different compilation instance of ~s from that required by ~a"
                          obj-path
                          path
                          importer)
                        (let-values ([(outfn original-importer)
                                      (let ([desc (get-library-descriptor found-uid)])
                                        (if desc
                                            (values (libdesc-outfn desc) (libdesc-importer desc))
                                            (values #f #f)))])
                          ($oops who
                            "~a requires a different compilation instance of ~s from the one previously ~:[compiled~;~:*loaded from ~a~]~@[ and originally imported by ~a~]"
                            importer
                            path
                            outfn
                            original-importer)))))
                (cond
                  [(search-loaded-libraries root path) =>
                   (lambda (found-uid)
                     (with-message (format "~s is already loaded...checking for compatibility" path)
                       (check-uid! found-uid #f)
                       (let ([desc (or (hashtable-ref uid-ht found-uid #f) (get-library-descriptor found-uid))])
                         (unless desc ($oops who "cyclic dependency involving import of library ~s" path))
                         (unless (libdesc-visible? desc)
                           ($oops who "attempting to ~:[invoke~;import or visit~] invisible library ~s" visit? path))
                         (if visit?
                             (cond
                               [(libdesc-ctdesc desc) => (lambda (ctdesc) (check-ctdesc-libreqs! ctdesc importer))]
                               [else
                                 (with-message "~s compile-time info for ~s has not yet been loaded...loading now"
                                   (check-fn! 'visit (libdesc-outfn desc) importer)
                                   (let ([desc (hashtable-ref uid-ht found-uid #f)])
                                     (unless (and desc (libdesc-ctdesc desc))
                                       ($oops who "visiting ~s does not define compile-time information for ~s" (libdesc-outfn desc) path))))])
                             (cond
                               [(libdesc-rtdesc desc) => (lambda (rtdesc) (check-rtdesc-libreqs! rtdesc importer))]
                               [else
                                 (with-message "~s run-time info for ~s has not yet been loaded...loading now"
                                   (check-fn! 'revisit (libdesc-outfn desc) importer)
                                   (let ([desc (hashtable-ref uid-ht found-uid #f)])
                                     (unless (and desc (libdesc-rtdesc desc))
                                       ($oops who "revisiting ~s does not define run-time information for ~s" (libdesc-outfn desc) path))))])))))]
                  [else
                   (let-values ([(src-path obj-path obj-exists?) (library-search who path (library-directories) (library-extensions))])
                     (unless obj-exists? ($oops who "cannot find object file for library ~s" path))
                     (check-fn! (if visit? 'visit 'revisit) obj-path importer)
                     (let ([found-uid (search-loaded-libraries root path)])
                       (unless found-uid ($oops who "loading ~s did not define library ~s" obj-path path))
                       (check-uid! found-uid obj-path)
                       (let ([desc (hashtable-ref uid-ht found-uid #f)])
                         (if visit?
                             (unless (and desc (libdesc-ctdesc desc))
                               ($oops who "visiting ~s does not define compile-time information for ~s" obj-path path))
                             (unless (and desc (libdesc-rtdesc desc))
                               ($oops who "revisiting ~s does not define run-time information for ~s" obj-path path))))))]))))
          (define (check-fn! situation fn importer)
            (with-message (format "checking ~aability of ~a" situation fn)
              ; register each of the libraries in the file before chasing any of the dependencies
              ; to handle out-of-order info records and whole programs or libraries that depend on a
              ; binary library which in turn depends on an embedded library.  this also more closely
              ; mirrors what happens when the file is actually loaded
              ((fold-left
                 (lambda (th lpinfo)
                   (cond
                     [(library/ct-info? lpinfo)
                      (with-message (format "found ~a import-req* = ~s, visit-visit-req* = ~s, visit-req* = ~s" fn
                                      (map libreq-path (library/ct-info-import-req* lpinfo))
                                      (map libreq-path (library/ct-info-visit-visit-req* lpinfo))
                                      (map libreq-path (library/ct-info-visit-req* lpinfo)))
                        (let ([ctdesc (make-ctdesc
                                        (library/ct-info-import-req* lpinfo)
                                        (library/ct-info-visit-visit-req* lpinfo)
                                        (library/ct-info-visit-req* lpinfo)
                                        #f #f '() 'loading 'loading)])
                          (let ([path (library-info-path lpinfo)] [uid (library-info-uid lpinfo)])
                            (set! root (record-loaded-library root path uid))
                            (hashtable-set! uid-ht uid
                              (let ([desc (or (hashtable-ref uid-ht uid #f) (get-library-descriptor uid))])
                                (make-libdesc path (library-info-version lpinfo) fn (or (and desc (libdesc-importer desc)) importer) #f #t
                                  ctdesc
                                  (and desc (libdesc-rtdesc desc))))))
                          (lambda () (th) (check-ctdesc-libreqs! ctdesc fn))))]
                     [(library/rt-info? lpinfo)
                      (with-message (format "found ~a invoke-req* = ~s" fn
                                      (map libreq-path (library/rt-info-invoke-req* lpinfo)))
                        (let ([rtdesc (make-rtdesc (library/rt-info-invoke-req* lpinfo) #f 'loading)])
                          (let ([path (library-info-path lpinfo)] [uid (library-info-uid lpinfo)])
                            (set! root (record-loaded-library root path uid))
                            (hashtable-set! uid-ht uid
                              (let ([desc (or (hashtable-ref uid-ht uid #f) (get-library-descriptor uid))])
                                (make-libdesc path (library-info-version lpinfo) fn (or (and desc (libdesc-importer desc)) importer) #f #t
                                  (and desc (libdesc-ctdesc desc))
                                  rtdesc))))
                          (lambda () (th) (check-rtdesc-libreqs! rtdesc fn))))]
                     [(program-info? lpinfo)
                      (with-message (format "found ~a invoke-req* = ~s" fn
                                      (map libreq-path (program-info-invoke-req* lpinfo)))
                        (lambda () (th) (for-each (check-libreq! #f fn) (program-info-invoke-req* lpinfo))))]
                     [else ($oops who "unexpected library/program info record ~s" lpinfo)]))
                 void
                 (get-lpinfo fn situation)))))
          (for-each (lambda (libdirs fn) (parameterize ([library-directories libdirs]) (check-fn! situation fn #f))) libdirs* fn*)))))

  (let ()
    (define maybe-get-lib
      (lambda (who libref)
        (syntax-case libref ()
          [(dir-id ... file-id)
           (and (andmap symbol? #'(dir-id ...)) (symbol? #'file-id))
           (cond
             [(search-loaded-libraries #'(dir-id ... file-id)) =>
              (lambda (uid) (and (get-library-descriptor uid) uid))]
             [else #f])]
          [(dir-id ... file-id version-ref)
           (and (andmap symbol? #'(dir-id ...)) (symbol? #'file-id) (version-ref? #'version-ref))
           (cond
             [(search-loaded-libraries #'(dir-id ... file-id)) =>
              (lambda (uid)
                (let ([libdesc (get-library-descriptor uid)])
                  (and libdesc (version-okay? #'version-ref (libdesc-version libdesc)) uid)))]
             [else #f])]
          [_ ($oops who "~s is not a valid library reference" libref)])))

    (define get-lib
      (lambda (who libref)
        (or (maybe-get-lib who libref)
            ($oops who "library ~s is not loaded" libref))))

    (set-who! library-exports
      (lambda (libref)
        (let* ([binding (lookup-global (get-lib who libref))]
               [iface
                (case (binding-type binding)
                 [($module) (get-indirect-interface (binding-value binding))]
                 [(global)
                  (let ([desc (get-library-descriptor (binding-value binding))])
                    (and desc (libdesc-visible? desc)
                         (cond
                          [(libdesc-import-code desc) =>
                           (lambda (import-code)
                             (guard (c [else #f])
                               (import-code 'get-iface)))]
                          [else #f])))]
                 [else #f])])
          (unless (interface? iface)
            ($oops who "unexpected binding ~s" binding))
          (let* ([exports (interface-exports iface)]
                 [n (vector-length exports)])
            (let loop ([i 0] [ls '()])
              (if (fx= i n)
                  ls
                  (loop
                    (fx+ i 1)
                    (let ([id (vector-ref exports i)])
                      (if (same-marks?
                            (id-marks id)
                            (wrap-marks top-wrap))
                          (cons (id-sym-name id) ls)
                          ls)))))))))

    (set-who! library-version
      (lambda (libref)
        (libdesc-version (get-library-descriptor (get-lib who libref)))))

    (set-who! library-object-filename
      (lambda (libref)
        (libdesc-outfn (get-library-descriptor (get-lib who libref)))))

    (set-who! $library-requirements-options (make-enumeration '(import visit@visit invoke@visit invoke)))
    (set-who! $make-library-requirements-options (enum-set-constructor $library-requirements-options))

    (set-who! library-requirements
      (rec library-requirements
        (case-lambda
          [(libref) (library-requirements libref (library-requirements-options import visit@visit invoke@visit invoke))]
          [(libref options)
           (define-syntax append-if
             (syntax-rules ()
               [(_ b e1 e2) (let ([ls e2]) (if b (append e1 e2) e2))]))
           (let ([desc (get-library-descriptor (get-lib who libref))])
             (unless (and (enum-set? options) (enum-set-subset? options $library-requirements-options))
               ($oops who "~s is not a library-requirements-options object" options))
             (let gather ([req* (append-if (enum-set-subset? (library-requirements-options import) options)
                                  (libdesc-import-req* desc)
                                  (append-if (enum-set-subset? (library-requirements-options visit@visit) options)
                                    (libdesc-visit-visit-req* desc)
                                    (append-if (enum-set-subset? (library-requirements-options invoke@visit) options)
                                      (libdesc-visit-req* desc)
                                      (append-if (enum-set-subset? (library-requirements-options invoke) options)
                                        (libdesc-invoke-req* desc)
                                        '()))))]
                          [uid* '()]
                          [name* '()])
               (if (null? req*)
                   name*
                   (let* ([req (car req*)] [uid (libreq-uid req)])
                     (if (memq uid uid*)
                         (gather (cdr req*) uid* name*)
                         (gather (cdr req*) (cons uid uid*)
                           (let ([path (libreq-path req)] [version (libreq-version req)])
                             (cons (if (null? version) path `(,@path ,version)) name*))))))))])))
    (set! $system-library?
      (lambda (libref)
        (cond
          [(maybe-get-lib '$system-library? libref) => (lambda (uid) (libdesc-system? (get-library-descriptor uid)))]
          [else #f]))))

  (let ()
    (define make-load-req
      (lambda (who loader path)
        (lambda (req)
          (loader who (libreq-path req) (libreq-version req) (libreq-uid req) path))))
    (define load-invoke-library
      (lambda (who path version-ref uid importer-path)
        (load-library who path version-ref uid importer-path #f
          (lambda (uid)
            (let ([desc (get-library-descriptor uid)])
              (unless (libdesc-rtdesc desc)
                ($oops who "loading ~a did not define run-time information for library ~s" (libdesc-outfn desc) path))
              (case (libdesc-loaded-invoke-reqs desc)
                [(#t) (void)]
                [(#f)
                 (libdesc-loaded-invoke-reqs-set! desc 'pending)
                 (on-reset (libdesc-loaded-invoke-reqs-set! desc #f)
                   (for-each (make-load-req who load-invoke-library path) (libdesc-invoke-req* desc)))
                 (libdesc-loaded-invoke-reqs-set! desc #t)]
                [(pending) ($oops who "cyclic dependency involving invocation of library ~s" (libdesc-path desc))]))))))
    (define load-visit-library
      (lambda (who path version-ref uid importer-path)
        (load-library #f path version-ref uid importer-path #t
          (lambda (uid)
            (let ([desc (get-library-descriptor uid)])
              (unless (libdesc-ctdesc desc)
                ($oops who "loading ~a did not define compile-time information for library ~s" (libdesc-outfn desc) path))
              (case (libdesc-loaded-visit-reqs desc)
                [(#t) (void)]
                [(#f)
                 (libdesc-loaded-visit-reqs-set! desc 'pending)
                 (on-reset (libdesc-loaded-visit-reqs-set! desc #f)
                   (for-each (make-load-req who load-visit-library path) (libdesc-visit-visit-req* desc))
                   (for-each (make-load-req who load-invoke-library path) (libdesc-visit-req* desc)))
                 (libdesc-loaded-visit-reqs-set! desc #t)]
                [(pending) ($oops who "cyclic dependency involving visit of library ~s" (libdesc-path desc))]))))))
    (define load-import-library
      (lambda (who path version-ref uid importer-path)
        (load-library #f path version-ref uid importer-path #t
          (lambda (uid)
            (let ([desc (get-library-descriptor uid)])
              (unless (libdesc-ctdesc desc)
                ($oops who "loading ~a did not define compile-time information for library ~s" (libdesc-outfn desc) path))
              (case (libdesc-loaded-import-reqs desc)
                [(#t) (void)]
                [(#f)
                 (libdesc-loaded-import-reqs-set! desc 'pending)
                 (on-reset (libdesc-loaded-import-reqs-set! desc #f)
                   (for-each (make-load-req who load-import-library path) (libdesc-import-req* desc)))
                 (libdesc-loaded-import-reqs-set! desc #t)]
                [(pending) ($oops who "cyclic dependency involving import of library ~s" (libdesc-path desc))]))))))
    (define import-library
      (lambda (uid)
        (cond
          [(get-library-descriptor uid) =>
           (lambda (desc)
             (unless (libdesc-visible? desc) ($oops #f "attempt to import invisible library ~s" (libdesc-path desc)))
             (cond
               [(libdesc-import-code desc) =>
                (lambda (p)
                  (when (eq? p 'loading)
                    ($oops #f "attempt to import library ~s while it is still being loaded" (libdesc-path desc)))
                  (libdesc-import-code-set! desc #f)
                  (on-reset (libdesc-import-code-set! desc p)
                    (for-each (lambda (req) (import-library (libreq-uid req))) (libdesc-import-req* desc))
                    (p)))]))]
          [else ($oops #f "library ~:s is not defined" uid)])))

    ; invoking or visiting a possibly unloaded library occurs in two separate steps:
    ;   1. load library and all dependencies first, recompiling or reloading if requested and required
    ;   2. invoke or visit the library and dependencies
    ; interleaving these steps would result in unnecessary visits and invokes if
    ; recompilation or reloading does occur
    (set! $invoke-library
      (lambda (path version-ref uid)
        (invoke-loaded-library (load-invoke-library #f path version-ref uid #f))))
    (set! $visit-library
      (lambda (path version-ref uid)
        (visit-loaded-library (load-visit-library #f path version-ref uid #f))))
    (set! $import-library
      (lambda (path version-ref uid)
        (let ([uid (load-import-library #f path version-ref uid #f)])
          (import-library uid)
          uid)))
    (set-who! invoke-library
      (lambda (name)
        (define (go path version-ref)
          (invoke-loaded-library (load-invoke-library who path version-ref #f #f)))
        (syntax-case name ()
          [(dir-id ... file-id)
           (and (andmap symbol? #'(dir-id ...)) (symbol? #'file-id))
           (go #'(dir-id ... file-id) '())]
          [(dir-id ... file-id version-ref)
           (and (andmap symbol? #'(dir-id ...)) (symbol? #'file-id) (version-ref? #'version-ref))
           (go #'(dir-id ... file-id) #'version-ref)]
          [_ ($oops who "invalid library reference ~s" name)])))
    (let ()
      (set! $maybe-compile-file
        (lambda (who ifn ofn handler)
          (define with-new-who
            (lambda (who th)
              (with-exception-handler
                (lambda (c)
                  (raise-continuable
                    (if (condition? c)
                        (apply condition (cons (make-who-condition who) (remp who-condition? (simple-conditions c))))
                        c)))
                th)))
          (define-syntax with-message
            (syntax-rules ()
              [(_ msg e1 e2 ...)
               (begin
                 (when (import-notify) (fprintf (console-output-port) "~s: ~a\n" who msg))
                 e1 e2 ...)]))
          (unless $compiler-is-loaded? ($oops '$maybe-compile-file "compiler is not loaded"))
          (if (file-exists? ofn)
              (let ([ofn-mod-time (library-modification-time ofn)])
                (if (time>=? ofn-mod-time (with-new-who who (lambda () (library-modification-time ifn))))
                    (with-message "object file is not older"
                      (let ([rcinfo (guard (c [else (with-message (with-output-to-string
                                                                    (lambda ()
                                                                      (display-string "failed to process object file: ")
                                                                      (display-condition c)))
                                                      #f)])
                                      (load-recompile-info who ofn))])
                        (if (and rcinfo
                                 (andmap
                                   (lambda (x)
                                     ((guard (c [else (with-message (with-output-to-string
                                                                      (lambda ()
                                                                        (display-string "failed to find include file: ")
                                                                        (display-condition c)))
                                                        (lambda () #f))])
                                        (with-source-path who x
                                          (lambda (x)
                                            (lambda ()
                                              (and (file-exists? x)
                                                   (time<=? (library-modification-time x) ofn-mod-time))))))))
                                   (recompile-info-include-req* rcinfo)))
                            (if (compile-imported-libraries)
                                (guard (c [(and ($recompile-condition? c) (eq? ($recompile-importer-path c) #f))
                                           (with-message (format "recompiling ~s because a dependency has changed" ifn)
                                             (handler ifn ofn))])
                                  (for-each (make-load-req who load-import-library #f) (recompile-info-import-req* rcinfo))
                                  #f)
                                (if (andmap
                                      (lambda (x)
                                        (let ([path (libreq-path x)])
                                          (cond
                                            [(search-loaded-libraries path) =>
                                             (lambda (found-uid)
                                               (verify-version who path (libreq-version x) found-uid #f #f)
                                               (eq? found-uid (libreq-uid x)))]
                                            [else
                                              (let-values ([(src-path obj-path obj-exists?) (library-search who path (library-directories) (library-extensions))])
                                                (and obj-exists?
                                                     (time<=? (library-modification-time obj-path) ofn-mod-time)))])))
                                      (recompile-info-import-req* rcinfo))
                                    #f
                                    (handler ifn ofn)))
                            (handler ifn ofn))))
                    (handler ifn ofn)))
              (handler ifn ofn)))))))

(set-who! $build-invoke-program
  (lambda (uid body)
    (build-primcall no-source 3 '$invoke-program
      (build-data no-source uid)
      (build-lambda no-source '() body))))

(set-who! $build-install-library/ct-code
  (lambda (uid export-id* import-code visit-code)
    (build-primcall no-source 3 '$install-library/ct-code
      (build-data no-source uid)
      (build-data no-source export-id*)
      import-code
      visit-code)))

(set-who! $build-install-library/rt-code
  (lambda (uid dl* db* dv* de* body)
    (build-primcall no-source 3 '$install-library/rt-code
      (build-data no-source uid)
      (build-lambda/lift-barrier no-source '()
        (build-library-body no-source dl* db* dv* de* body)))))

(let ()
  (define (parse-string s default-ls make-obj)
   ; "stuff^...", ^ is ; under windows : otherwise
   ; stuff -> src-dir^^src-dir | src-dir
   ; ends with ^, tail is default-ls, otherwise ()
    (let ([sep ($separator-character)]
          [n (string-length s)])
      (define (s0 i)
        (if (fx= i n)
            '()
            (if (char=? (string-ref s i) sep)
                (s1 (fx+ i 1))
                (s2 i (fx+ i 1)))))
      (define (s1 i) ; seen ^
        (if (fx= i n)
            default-ls
            (if (char=? (string-ref s i) sep)
                (s4 "" (fx+ i 1) (fx+ i 1))
                (cons (cons "" (make-obj "")) (s2 i (fx+ i 1))))))
      (define (s2 start i) ; parsing src-dir
        (if (fx= i n)
            (let ([src-dir (substring s start i)])
              (list (cons src-dir (make-obj src-dir))))
            (if (char=? (string-ref s i) sep)
                (s3 (substring s start i) (fx+ i 1))
                (s2 start (fx+ i 1)))))
      (define (s3 src-dir i) ; seen ^ after src-dir
        (if (fx= i n)
            (cons (cons src-dir (make-obj src-dir)) default-ls)
            (if (char=? (string-ref s i) sep)
                (s4 src-dir (fx+ i 1) (fx+ i 1))
                (cons (cons src-dir (make-obj src-dir))
                      (s2 i (fx+ i 1))))))
      (define (s4 src-dir start i) ; parsing obj-dir
        (if (fx= i n)
            (list (cons src-dir (substring s start i)))
            (if (char=? (string-ref s i) sep)
                (cons (cons src-dir (substring s start i)) (s5 (fx+ i 1)))
                (s4 src-dir start (fx+ i 1)))))
      (define (s5 i) ; seen ^ after obj-dir
        (if (fx= i n)
            default-ls
            (if (char=? (string-ref s i) sep)
                (s3 "" (fx+ i 1))
                (s2 i (fx+ i 1)))))
      (s0 0)))

  (define (parse-list who what ls make-obj)
    (let f ([ls ls])
      (if (null? ls)
          '()
          (let ([x (car ls)])
            (cond
              [(string? x) (cons (cons x (make-obj x)) (f (cdr ls)))]
              [(and (pair? x) (string? (car x)) (string? (cdr x)))
               (cons (cons (car x) (cdr x)) (f (cdr ls)))]
              [else ($oops who (format "invalid ~a element ~~s" what) x)])))))

  (set-who! library-directories
    (rec library-directories
      ($make-thread-parameter
        '(("." . "."))
        (lambda (x)
          (cond
            [(string? x) (parse-string x (library-directories) values)]
            [(list? x) (parse-list who "path-list" x values)]
            [else ($oops who "invalid path list ~s" x)])))))

  (set-who! library-extensions
    (rec library-extensions
      ($make-thread-parameter
        '((".chezscheme.sls" . ".chezscheme.so")
          (".ss" . ".so")
          (".sls" . ".so")
          (".scm" . ".so")
          (".sch" . ".so"))
        (lambda (x)
          (define default-obj-ext
            (lambda (src-ext)
              (string-append (path-root src-ext) ".so")))
          (cond
            [(string? x) (parse-string x (library-extensions) default-obj-ext)]
            [(list? x) (parse-list who "extension-list" x default-obj-ext)]
            [else ($oops who "invalid extension list ~s" x)]))))))

(set! $install-program-desc
  (lambda (pinfo)
    (put-program-descriptor (program-info-uid pinfo)
      (make-progdesc (program-info-invoke-req* pinfo)))))

(set! $install-library-clo-info
  (lambda (clo*)
    (for-each
      (lambda (p)
        (let ([box (get-clo-info (car p))])
          (if box
              (set-box! box
                (let merge ([new-a* (unbox (cdr p))] [a* (unbox box)])
                  (if (null? new-a*)
                      a*
                      (cons
                        (car new-a*)
                        (merge (cdr new-a*) (remp (lambda (a) (eq? (car a) (caar new-a*))) a*))))))
              (put-clo-info (car p) (cdr p)))))
      clo*)))

(set! $install-library/ct-desc
  (lambda (linfo/ct for-import? importer ofn)
    (let ([uid (library-info-uid linfo/ct)])
      (when for-import?
        (when (let ([desc (get-library-descriptor uid)]) (and desc (libdesc-ctdesc desc)))
          ($oops #f "attempting to re-install compile-time part of library ~s" (library-info-path linfo/ct))))
      (install-library/ct-desc (library-info-path linfo/ct) (library-info-version linfo/ct) uid ofn importer
        (library-info-visible? linfo/ct)
        (make-ctdesc
          (library/ct-info-import-req* linfo/ct)
          (library/ct-info-visit-visit-req* linfo/ct)
          (library/ct-info-visit-req* linfo/ct)
          #f #f '() 'loading 'loading)))))

(set! $install-library/rt-desc
  (lambda (linfo/rt for-import? importer ofn)
    (let ([uid (library-info-uid linfo/rt)])
      (when for-import?
        (when (let ([desc (get-library-descriptor uid)]) (and desc (libdesc-rtdesc desc)))
          ($oops #f "attempting to re-install run-time part of library ~s" (library-info-path linfo/rt))))
      (install-library/rt-desc (library-info-path linfo/rt) (library-info-version linfo/rt) uid ofn importer
        (library-info-visible? linfo/rt)
        (make-rtdesc (library/rt-info-invoke-req* linfo/rt) #f 'loading)))))

(set! $install-library/ct-code
  (lambda (uid export-id* import-code visit-code)
    (install-library/ct-code uid export-id* import-code visit-code)))

(set! $install-library/rt-code
  (lambda (uid invoke-code)
    (install-library/rt-code uid invoke-code)))

(set-who! $invoke-program
  (lambda (uid th)
    (let ([desc (get-program-descriptor uid)])
      (unless desc (sorry! who "unable to locate program descriptor for ~s" uid))
      (rem-program-descriptor uid)
      (for-each
        (lambda (req) ($invoke-library (libreq-path req) (libreq-version req) (libreq-uid req)))
        (progdesc-invoke-req* desc)))
    (th)))

(set! $mark-invoked!
 ; library must already have been loaded
  (lambda (uid)
    (cond
      [(get-library-descriptor uid) =>
       (lambda (desc) (libdesc-invoke-code-set! desc #f))]
      [else ($oops #f "library ~:s is not defined" uid)])))

(set! $mark-pending!
 ; library must already have been loaded
  (lambda (uid)
    (cond
      [(get-library-descriptor uid) =>
       (lambda (desc) (libdesc-invoke-code-set! desc 'pending))]
      [else ($oops #f "library ~:s is not defined" uid)])))

(set! $transformer->binding
  (lambda (x)
    (transformer->binding 'define-syntax x)))

; must precede global-extends

(let ()
  (define sc-put-module
    (lambda (exports token new-marks)
      (vector-for-each
        (lambda (id) (store-global-subst id token new-marks))
        exports)))
  (define (put-cte id binding token)
    (when token (store-global-subst id token '()))
    (let ((label (if (symbol? id) id (id->label id empty-wrap))))
      (put-global-definition-hook label
       ; global binding is assumed; if global pass #f to remove existing binding, if any
        (if (and (eq? (binding-type binding) 'global)
                 (eq? (binding-value binding) label))
            #f
            binding))))
  (set! $sc-put-cte
    (lambda (id binding top-token)
      (case (binding-type binding)
        [(do-alias) (when top-token (store-global-subst id top-token '()))]
        [(do-import)
         ; id is module id, binding-value is new-marks
         (let ([new-marks (binding-value binding)])
           (let ([b (lookup-global (id->label id empty-wrap))])
             (case (binding-type b)
               [($module)
                (let ([iface (get-indirect-interface (binding-value b))])
                  (when top-token
                    (sc-put-module (interface-exports iface) top-token new-marks)))]
               [else (syntax-error id "unknown module")])))]
        [(do-anonymous-import)
         ; id is #f, binding-value is vector of exported ids
         (when top-token
           (sc-put-module (binding-value binding) top-token '()))]
        [else (put-cte id binding top-token)])))
  (set! $sc-put-property!
    (lambda (id association propval top-token)
      (when top-token (store-global-subst id top-token '()))
      (put-global-definition-hook (cdr association) (make-binding 'property propval))))
)

(let ()
  (define-who install-system-library
    (lambda (path uid)
      (install-library path uid
        (make-libdesc path (if (eq? (car path) 'rnrs) '(6) '()) #f #f #t #t
          (make-ctdesc '() '() '() #t #t '() #f #f)
          (make-rtdesc '() #t #f)))))
  (set! $make-base-modules
    (lambda ()
      (let partition ((ls (oblist)) (r5rs-syntax '()) (r5rs '()) (ieee '()) (scheme '()) (system '()))
        (if (null? ls)
            (let ()
              (define put-module
                (lambda (sym export*)
                  ($set-top-level-value! sym (make-interface (wrap-marks top-wrap) (list->vector export*)))
                  (put-global-definition-hook sym (make-binding '$module sym))))
              (put-module '$system system)
              (put-module 'scheme scheme)
              (put-module 'ieee ieee)
              (put-module 'r5rs r5rs)
              (put-module 'r5rs-syntax r5rs-syntax)
              (install-system-library '(scheme) 'scheme)
              (install-system-library '(chezscheme) 'scheme)
              (install-system-library '(scheme csv7) '$chezscheme-csv7))
            (let* ((s (car ls)) (m ($sgetprop s '*flags* 0)))
              (define-syntax repartition
                (syntax-rules ()
                  [(_ id r5rs-syntax? r5rs? ieee? scheme?)
                   (partition (cdr ls)
                     (if r5rs-syntax? (cons id r5rs-syntax) r5rs-syntax)
                     (if r5rs? (cons id r5rs) r5rs)
                     (if ieee? (cons id ieee) ieee)
                     (if scheme? (cons id scheme) scheme)
                     (cons id system))]))
             ; copy imported library/module bindings to system module
              (cond
                [(cond
                   [(and (any-set? (prim-mask (or system system-keyword primitive keyword)) m)
                         (lookup-global-label s (wrap-marks top-wrap) '*system*)) =>
                    (lambda (label) (and (not (eq? label s)) label))]
                   [else #f]) =>
                 (lambda (label)
                   (let ([b (get-global-definition-hook label)])
                     (cond
                       [(not b)
                        ($set-top-level-value! s
                          ($top-level-value (binding-value label)))]
                       [(eq? (binding-type b) 'global)
                        ($set-top-level-value! s
                          ($top-level-value (binding-value b)))]
                       [else (put-global-definition-hook s b)])))])
             ; add system bindings to other modules as appropriate
              (cond
               [(any-set? (prim-mask (or keyword system-keyword)) m)
                 (let ([id (make-resolved-id s (wrap-marks top-wrap) s)])
                   (cond
                     [(any-set? (prim-mask keyword) m)
                      (store-global-subst id '*scheme* '())
                      (store-global-subst id '*top* '())
                      (cond
                        [(any-set? (prim-mask r5rs) m)
                         (let ([unprefixed-id (id->unprefixed-id id)])
                           (store-global-subst unprefixed-id '*r5rs* '())
                           (store-global-subst unprefixed-id '*r5rs-syntax* '())
                           (cond
                             [(any-set? (prim-mask ieee) m)
                              (store-global-subst unprefixed-id '*ieee* '())
                              (repartition id #t #t #t #t)]
                             [else (repartition id #t #t #f #t)]))]
                        [else (repartition id #f #f #f #t)])]
                     [else (repartition id #f #f #f #f)]))]
                [(any-set? (prim-mask (or primitive system)) m)
                 (put-global-definition-hook s (make-binding 'primitive s))
                 (let ([id (make-resolved-id s (wrap-marks top-wrap) s)])
                   (cond
                     [(any-set? (prim-mask primitive) m)
                      (store-global-subst id '*scheme* '())
                      (store-global-subst id '*top* '())
                      (cond
                        [(any-set? (prim-mask r5rs) m)
                         (let ([unprefixed-id (id->unprefixed-id id)])
                           (store-global-subst unprefixed-id '*r5rs* '())
                           (cond
                             [(any-set? (prim-mask ieee) m)
                              (store-global-subst unprefixed-id '*ieee* '())
                              (repartition id #f #t #t #t)]
                             [else (repartition id #f #t #f #t)]))]
                        [else (repartition id #f #f #f #t)])]
                     [else (repartition id #f #f #f #f)]))]
                [else (partition (cdr ls) r5rs-syntax r5rs ieee scheme system)]))))))
  (set! $make-rnrs-libraries
    (lambda ()
      (define make-library
        (lambda (path ventry)
          (let ([uid (string->symbol
                       (apply string-append
                         (let f ([sep "$"] [path path])
                           (if (null? path)
                               '()
                               (cons* sep (symbol->string (car path)) (f "-" (cdr path)))))))])
            ($set-top-level-value! uid
              (make-interface (wrap-marks top-wrap)
                (vector-map
                  (lambda (entry)
                    (let ([name (if (pair? entry) (car entry) entry)]
                          [prim (if (pair? entry) (cdr entry) entry)])
                      (make-resolved-id name (wrap-marks top-wrap) prim)))
                  ventry)))
            (put-global-definition-hook uid (make-binding '$module uid))
            (install-system-library path uid))))
      (define-syntax make-rnrs-libraries
        (lambda (x)
          (import priminfo)
          (define table '())
         ; sort vector of primitive names so boot files compare equal
          (let ([v-prim (vector-sort (lambda (x y) (string<=? (symbol->string x) (symbol->string y))) (primvec))])
            (vector-for-each
              (lambda (prim info)
                (let ([unprefixed (priminfo-unprefixed info)])
                  (for-each
                    (lambda (lib)
                      (cond
                        [(assoc lib table) => (lambda (a) (set-cdr! a (cons (cons unprefixed prim) (cdr a))))]
                        [else (set! table (cons (cons lib (list (cons unprefixed prim))) table))]))
                    (priminfo-libraries info))))
              v-prim (vector-map get-priminfo v-prim)))
            #`(vector-for-each make-library
                '#,(datum->syntax #'* (list->vector (map car table)))
                '#,(datum->syntax #'* (vector-map list->vector (list->vector (map cdr table)))))))
      make-rnrs-libraries)))

;;; core transformers

(global-extend 'local-syntax 'letrec-syntax #t)
(global-extend 'local-syntax 'let-syntax #f)

; (global-extend 'core 'transformer
;   (lambda (e r w ae)
;     (syntax-case e ()
;       ((_ id)
;        (id? (syntax id))
;        (let ((n (id->label (syntax id) w)))
;          (build-data no-source (lookup n r)))))))

(global-extend 'core 'fluid-let-syntax
  (lambda (e r w ae)
    (syntax-case e ()
      [(_ ((var val) ...) e1 e2 ...)
       (valid-bound-ids? (syntax (var ...)))
       (let ([label* (map (lambda (x) (id->label x w)) (syntax (var ...)))])
         (for-each
           (lambda (id n)
             (let ([b (lookup n r)])
               (case (binding-type b)
                 ((displaced-lexical) (displaced-lexical-error (wrap id w) "bind" (binding-value b))))))
           (syntax (var ...))
           label*)
         (let ([b* (map (lambda (x)
                          (defer-or-eval-transformer 'fluid-let-syntax
                            local-eval-hook
                            (meta-chi x r w)))
                        (syntax (val ...)))])
           (let f ([label* label*] [b* b*])
             (if (null? label*)
                 (chi-body (syntax (e1 e2 ...)) (source-wrap e w ae) r w)
                 (let ([label (car label*)] [b (car b*)])
                   (cond
                     [(if (symbol? label) (lookup-rho r label) label) =>
                      (lambda (label)
                        (let ([old-b (local-label-binding label)] [old-level (local-label-level label)])
                          (local-label-binding-set! label b)
                          (local-label-level-set! label (fxlognot (meta-level)))
                          (let ([body (f (cdr label*) (cdr b*))])
                            (local-label-binding-set! label old-b)
                            (local-label-level-set! label old-level)
                            body)))]
                     [else
                      (extend-rho! r label b (fxlognot (meta-level)))
                      (let ([body (f (cdr label*) (cdr b*))])
                        (retract-rho! r label)
                        body)]))))))]
      [_ (syntax-error (source-wrap e w ae))])))

(global-extend 'core 'quote
   (lambda (e r w ae)
      (syntax-case e ()
         ((_ e) (build-data ae (strip (syntax e) w)))
         (_ (syntax-error (source-wrap e w ae))))))

(global-extend 'core 'quote-syntax
  (lambda (e r w ae)
    (let ([e (source-wrap e w ae)])
      (syntax-case e ()
         ((_ e) (build-data no-source (syntax e)))
         (_ (syntax-error e))))))

(global-extend 'core 'syntax
  (let ()
    (define gen-syntax
      (lambda (src e r maps ellipsis? vec?)
        (if (id? e)
            (cond
              [(lookup-pattern-variable (id->label e empty-wrap) r) =>
               (lambda (var.lev)
                 (let-values ([(var maps) (gen-ref src (car var.lev) (cdr var.lev) maps)])
                   (values `(ref ,var) maps)))]
              [(ellipsis? e) (syntax-error src "misplaced ellipsis in syntax form")]
              [else (values `(quote ,e) maps)])
            (syntax-case e ()
              ((dots e)
               (ellipsis? (syntax dots))
               (if vec?
                   (syntax-error src "misplaced ellipsis in syntax template")
                   (gen-syntax src (syntax e) r maps (lambda (x) #f) #f)))
              ((x dots . y)
               ; this could be about a dozen lines of code, except that we
               ; choose to handle (syntax (x ... ...)) forms
               (ellipsis? (syntax dots))
               (let f ((y (syntax y))
                       (k (lambda (maps)
                            (let-values ([(x maps)
                                          (gen-syntax src (syntax x) r
                                            (cons '() maps) ellipsis? #f)])
                              (if (null? (car maps))
                                  (syntax-error src
                                    "extra ellipsis in syntax form")
                                  (values (gen-map x (car maps))
                                          (cdr maps)))))))
                 (syntax-case y ()
                   ((dots . y)
                    (ellipsis? (syntax dots))
                    (f (syntax y)
                       (lambda (maps)
                         (let-values ([(x maps) (k (cons '() maps))])
                           (if (null? (car maps))
                               (syntax-error src
                                 "extra ellipsis in syntax form")
                               (values (gen-mappend x (car maps))
                                       (cdr maps)))))))
                   (_ (let-values ([(y maps) (gen-syntax src y r maps ellipsis? vec?)])
                        (let-values ([(x maps) (k maps)])
                          (values (gen-append x y) maps)))))))
              ((x . y)
               (let-values ([(xnew maps) (gen-syntax src (syntax x) r maps ellipsis? #f)])
                 (let-values ([(ynew maps) (gen-syntax src (syntax y) r maps ellipsis? vec?)])
                   (values (gen-cons e (syntax x) (syntax y) xnew ynew)
                           maps))))
              (#(x1 x2 ...)
               (let ((ls (syntax (x1 x2 ...))))
                 (let-values ([(lsnew maps) (gen-syntax src ls r maps ellipsis? #t)])
                   (values (gen-vector e ls lsnew) maps))))
              (#&x
               (let-values ([(xnew maps) (gen-syntax src (syntax x) r maps ellipsis? #f)])
                 (values (gen-box e (syntax x) xnew) maps)))
              (_ (values `(quote ,e) maps))))))

    (define gen-ref
      (lambda (src var level maps)
        (if (fx= level 0)
            (values var maps)
            (if (null? maps)
                (syntax-error src (format "missing ellipsis for ~s in syntax form" var))
                (let-values ([(outer-var outer-maps) (gen-ref src var (fx- level 1) (cdr maps))])
                  (let ((b (assq outer-var (car maps))))
                    (if b
                        (values (cdr b) maps)
                        (let ((inner-var (gen-var 'tmp)))
                          (values inner-var
                                  (cons (cons (cons outer-var inner-var)
                                              (car maps))
                                        outer-maps))))))))))

    (define gen-append
      (lambda (x y)
        (if (equal? y '(quote ()))
            x
            `(append ,x ,y))))

    (define gen-mappend
      (lambda (e map-env)
        `(apply (primitive append) ,(gen-map e map-env))))

    (define gen-map
      (lambda (e map-env)
        (let ((formals (map cdr map-env))
              (actuals (map (lambda (x) `(ref ,(car x))) map-env)))
          (cond
            ((eq? (car e) 'ref)
             ; identity map equivalence:
             ; (map (lambda (x) x) y) == y
             (car actuals))
            ((andmap
                (lambda (x) (and (eq? (car x) 'ref) (memq (cadr x) formals)))
                (cdr e))
             ; eta map equivalence:
             ; (map (lambda (x ...) (f x ...)) y ...) == (map f y ...)
             `(map (primitive ,(car e))
                   ,@(map (let ((r (map cons formals actuals)))
                            (lambda (x) (cdr (assq (cadr x) r))))
                          (cdr e))))
            (else `(map (lambda ,formals ,e) ,@actuals))))))

   ; 12/12/00: semantic change: we now return original syntax object (e)
   ; if no pattern variables were found within, to avoid dropping
   ; source annotations prematurely.  the "syntax returns lists" for
   ; lists in its input guarantee counts only for substructure that
   ; contains pattern variables
   ; test with (define-syntax a (lambda (x) (list? (syntax (a b)))))
   ;           a => #f
    (define gen-cons
      (lambda (e x y xnew ynew)
        (case (car ynew)
          ((quote)
           (if (eq? (car xnew) 'quote)
               (let ([xnew (cadr xnew)] [ynew (cadr ynew)])
                 (if (and (eq? xnew x) (eq? ynew y))
                     `',e
                     `'(,xnew . ,ynew)))
               (if (eq? (cadr ynew) '()) `(list ,xnew) `(cons ,xnew ,ynew))))
          ((list) `(list ,xnew ,@(cdr ynew)))
          (else `(cons ,xnew ,ynew)))))

   ; test with (define-syntax a
   ;             (lambda (x)
   ;               (let ((x (syntax #(a b))))
   ;                 (and (vector? x)
   ;                      (not (eq? (vector-ref x 0) 'syntax-object))))))
   ;           a => #f
    (define gen-vector
      (lambda (e ls lsnew)
        (cond
          ((eq? (car lsnew) 'quote)
           (if (eq? (cadr lsnew) ls)
               `',e
               `(quote #(,@(cadr lsnew)))))
          ((eq? (car lsnew) 'list) `(vector ,@(cdr lsnew)))
          (else `(list->vector ,lsnew)))))

   ; test with (define-syntax a (lambda (x) (box? (syntax #&(a b)))))
   ;           a  => #f
    (define gen-box
      (lambda (e x xnew)
        (cond
          ((eq? (car xnew) 'quote)
           (if (eq? (cadr xnew) x)
               `',e
               `(quote #&,(cadr xnew))))
          (else `(box ,xnew)))))

    (define regen
      (lambda (x)
        (case (car x)
          ((ref) (build-lexical-reference no-source (cadr x)))
          ((primitive) (build-primref 3 (cadr x)))
          ((quote) (build-data no-source (cadr x)))
          ((lambda) (build-lambda no-source (cadr x) (regen (caddr x))))
          ((map) (let ((ls (map regen (cdr x))))
                   (if (fx= (length ls) 2)
                       (build-call no-source
                         (build-primref 3 'map)
                         ls)
                       (build-call no-source
                         (build-primref 3 '$map)
                         (cons (build-data #f 'syntax) ls)))))
          (else (build-call no-source
                  (build-primref 3 (car x))
                  (map regen (cdr x)))))))

    (lambda (e r w ae)
      (let ((e (source-wrap e w ae)))
        (syntax-case e ()
          ((_ x)
           (let-values ([(e maps) (gen-syntax e (syntax x) r '() ellipsis? #f)])
             (regen e)))
          (_ (syntax-error e)))))))

(global-extend 'core '$primitive
  (lambda (e r w ae)
    (define build
      (lambda (level x)
        (or (build-primref? ae level (strip x w))
            (syntax-error (source-wrap x w ae) "invalid primitive name"))))
    (syntax-case e ()
      [(_ x)
       (id? #'x)
       (build (fxmax (optimize-level) 2) #'x)]
      [(_ n x)
       (and (memv (strip #'n w) '(2 3)) (id? #'x))
       (build (strip #'n w) #'x)]
      [_ (syntax-error (source-wrap e w ae))])))

(global-extend 'core 'lambda
  (lambda (e r w ae)
    (syntax-case e ()
      ((_ . c)
       (let-values ([(vars body) (chi-lambda-clause (source-wrap e w ae) (syntax c) r w)])
         (build-lambda ae vars body))))))

(global-extend 'core '$lambda/lift-barrier
  (lambda (e r w ae)
    (syntax-case e ()
      ((_ . c)
       (let-values ([(vars body) (chi-lambda-clause (source-wrap e w ae) (syntax c) r w)])
         (build-lambda/lift-barrier ae vars body))))))

(global-extend 'core 'case-lambda
  (lambda (e r w ae)
    (syntax-case e ()
      ((_ libspec c1 ...)
       (and (eq? (subset-mode) 'system) (libspec? (strip (syntax libspec) w)))
       (build-library-case-lambda ae
         (strip (syntax libspec) w)
         (map (lambda (c) (with-values (chi-lambda-clause (source-wrap e w ae) c r w) list))
              (syntax (c1 ...)))))
      ((_ c1 ...)
       (build-case-lambda ae
         (map (lambda (c) (with-values (chi-lambda-clause (source-wrap e w ae) c r w) list))
              (syntax (c1 ...))))))))

(let ()
  (define letrec-transformer
    (lambda (build)
      (lambda (e r w ae)
        (syntax-case e ()
          ((_ ((id val) ...) e1 e2 ...)
           (let ((ids (syntax (id ...))))
             (if (not (valid-bound-ids? ids))
                 (invalid-ids-error (map (lambda (x) (wrap x w)) ids)
                   (source-wrap e w ae) "bound variable")
                 (let ([new-vars (map gen-var ids)])
                   (let ([labels (map make-lexical-label new-vars)])
                     (let ([w (make-binding-wrap ids labels w)])
                       (let ([x (build ae
                                  new-vars
                                  (map (lambda (x) (chi x r w)) (syntax (val ...)))
                                  (chi-body (syntax (e1 e2 ...)) (source-wrap e w ae) r w))])
                         (map kill-local-label! labels)
                         x)))))))
          (_ (syntax-error (source-wrap e w ae)))))))
  (global-extend 'core 'letrec (letrec-transformer build-letrec))
  (global-extend 'core 'letrec* (letrec-transformer build-letrec*)))

(global-extend 'core 'let
  (lambda (e r w ae)
    (let ([wrapped-form (source-wrap e w ae)])
      (syntax-case e ()
        [(_ ((id val) ...) e1 e2 ...)
         (let ([ids #'(id ...)])
           (if (null? ids)
               (build-profile ae (chi-body #'(e1 e2 ...) wrapped-form r w))
               (begin
                 (unless (valid-bound-ids? ids)
                   (invalid-ids-error (map (lambda (x) (wrap x w)) ids)
                     wrapped-form "bound variable"))
                 (let ([vars (map gen-var ids)])
                   (build-let ae
                     vars
                     (map (lambda (x) (chi x r w)) #'(val ...))
                     (let ([labels (map make-lexical-label vars)])
                       (let ([x (chi-body #'(e1 e2 ...) wrapped-form r
                                  (make-binding-wrap ids labels w))])
                         (map kill-local-label! labels)
                         x)))))))]
        [(_ f ((id val) ...) e1 e2 ...)
         (let ([id #'f] [ids #'(id ...)])
           (unless (id? id)
             (syntax-error wrapped-form
               (format "invalid bound variable ~s in" (strip id empty-wrap))))
           (unless (valid-bound-ids? ids)
             (invalid-ids-error (map (lambda (x) (wrap x w)) ids)
               wrapped-form "bound variable"))
           (let ([var (gen-var id)] [vars (map gen-var ids)])
             (build-call ae
               (build-letrec no-source
                 (list var)
                 (list (build-lambda no-source vars
                         (let ([label (make-lexical-label var)] [labels (map make-lexical-label vars)])
                           (let ([x (chi-body #'(e1 e2 ...) wrapped-form r
                                      (make-binding-wrap ids labels (make-binding-wrap (list id) (list label) w)))])
                             (kill-local-label! label)
                             (map kill-local-label! labels)
                             x))))
                 (build-lexical-reference no-source var))
               (map (lambda (x) (chi x r w)) #'(val ...)))))]
        [_ (syntax-error wrapped-form)]))))

(global-extend 'core 'let*
  (lambda (e r w ae)
    (let ([wrapped-form (source-wrap e w ae)])
      (syntax-case e ()
        [(_ ((id val) ...) e1 e2 ...)
         (let ([ids #'(id ...)])
           (for-each
             (lambda (id)
               (unless (id? id)
                 (syntax-error wrapped-form
                   (format "invalid bound variable ~s in" (strip id empty-wrap)))))
             ids)
           (let f ([ids ids] [vals #'(val ...)] [w w] [ae ae])
             (if (null? ids)
                 (build-profile ae (chi-body #'(e1 e2 ...) wrapped-form r w))
                 (let* ([id (car ids)] [var (gen-var id)])
                   (build-let ae
                     (list var)
                     (list (chi (car vals) r w))
                     (let ([label (make-lexical-label var)])
                       (let ([body (f (cdr ids) (cdr vals) (make-binding-wrap (list id) (list label) w) #f)])
                         (kill-local-label! label)
                         body)))))))]
        [_ (syntax-error wrapped-form)]))))

(global-extend 'core 'if
   (lambda (e r w ae)
      (syntax-case e ()
         ((_ test then)
          (build-conditional ae
             (chi (syntax test) r w)
             (chi (syntax then) r w)
             (build-void)))
         ((_ test then else)
          (build-conditional ae
             (chi (syntax test) r w)
             (chi (syntax then) r w)
             (chi (syntax else) r w)))
         (_ (syntax-error (source-wrap e w ae))))))

(global-extend 'core '$moi
  (lambda (e r w ae)
    (syntax-case e ()
      [(_) (build-moi)])))

(global-extend 'core '$foreign-procedure
  (lambda (e r w ae)
    (syntax-case e ()
      ((_ conv* foreign-name foreign-addr (arg ...) result)
       (build-foreign-procedure ae
         (strip (syntax conv*) w)
         (strip (syntax foreign-name) w)
         (chi (syntax foreign-addr) r w)
         (map (lambda (x) (strip x w)) (syntax (arg ...)))
         (strip (syntax result) w))))))

(global-extend 'core '$foreign-callable
  (lambda (e r w ae)
    (syntax-case e ()
      ((_ conv* proc (arg ...) result)
       (build-foreign-callable ae
         (strip (syntax conv*) w)
         (chi (syntax proc) r w)
         (map (lambda (x) (strip x w)) (syntax (arg ...)))
         (strip (syntax result) w))))))

(global-extend 'core 'pariah
  (lambda (e r w ae)
    (syntax-case e ()
      ((_ e1 e2 ...)
       (build-pariah ae (chi-sequence #'(e1 e2 ...) r w no-source))))))

(global-extend 'core 'profile
  (lambda (e r w ae)
    (syntax-case e ()
      [(_ src)
       (let ([src (datum src)])
         (unless (source-object? src) (syntax-error src "profile subform is not a source object"))
         (build-input-profile src))])))

(global-extend 'core '$begin-unsafe
  (lambda (e r w ae)
    (syntax-case e ()
      ((_ e1 e2 ...)
       (parameterize ([optimize-level 3])
         (chi-sequence #'(e1 e2 ...) r w no-source))))))

(global-extend 'set! 'set! '())

(global-extend 'alias 'alias '())
(global-extend 'begin 'begin '())

(global-extend '$library-key '$library '())
(global-extend '$program-key '$program '())
(global-extend '$module-key '$module '())
(global-extend '$import '$import '())
(global-extend 'export 'export '())
(global-extend 'indirect-export 'indirect-export '())
(global-extend 'implicit-exports 'implicit-exports '())

(global-extend 'define 'define '())

(global-extend 'define-syntax 'define-syntax '())

(global-extend 'define-property 'define-property '())

(global-extend 'eval-when 'eval-when '())

(global-extend 'meta 'meta '())

(let ()
  (define convert-pattern
    ; accepts pattern & keys
    ; returns syntax-dispatch pattern & ids
    (lambda (pattern keys)
      (define cvt*
        (lambda (p* n ids)
          (if (null? p*)
              (values '() ids)
              (let-values ([(y ids) (cvt* (cdr p*) n ids)])
                (let-values ([(x ids) (cvt (car p*) n ids)])
                  (values (cons x y) ids))))))
      (define cvt
        (lambda (p n ids)
          (if (id? p)
              (cond
                [(bound-id-member? p keys)
                 (values (vector 'free-id p) ids)]
                [(free-id=? p #'_) (values '_ ids)]
                [else (values 'any (cons (cons p n) ids))])
              (syntax-case p ()
                [(x dots)
                 (ellipsis? #'dots)
                 (let-values ([(p ids) (cvt #'x (fx+ n 1) ids)])
                   (values (if (eq? p 'any) 'each-any (vector 'each p))
                           ids))]
                [(x dots y ... . z)
                 (ellipsis? #'dots)
                 (let-values ([(z ids) (cvt #'z n ids)])
                   (let-values ([(y ids) (cvt* #'(y ...) n ids)])
                     (let-values ([(x ids) (cvt #'x (fx+ n 1) ids)])
                       (values `#(each+ ,x ,(reverse y) ,z) ids))))]
                [(x . y)
                 (let-values ([(y ids) (cvt #'y n ids)])
                   (let-values ([(x ids) (cvt #'x n ids)])
                     (values (cons x y) ids)))]
                [() (values '() ids)]
                [#(x ...)
                 (let-values ([(p ids) (cvt #'(x ...) n ids)])
                   (values (vector 'vector p) ids))]
                [#&x
                 (let-values ([(p ids) (cvt #'x n ids)])
                   (values (vector 'box p) ids))]
                [x (values (vector 'atom (strip p empty-wrap)) ids)]))))
      (cvt pattern 0 '())))

  (define build-dispatch-call
    (lambda (pvars template y r)
      (let ([ids (map car pvars)] [levels (map cdr pvars)])
        (let ([new-vars (map gen-var ids)])
          (let ([labels (map (lambda (var pvar)
                               (make-local-label (make-binding 'syntax `(,var . ,(cdr pvar))) (meta-level)))
                          new-vars pvars)])
            (let ([body (chi template r
                          (make-binding-wrap ids labels empty-wrap))])
              (map kill-local-label! labels)
              (build-primcall no-source 3 'apply
                (build-lambda no-source new-vars body)
                y)))))))

  (define gen-clause
    (lambda (who x keys clauses r pattern fender template)
      (let-values ([(p pvars) (convert-pattern pattern keys)])
        (cond
          [(not (distinct-bound-ids? (map car pvars)))
           (invalid-ids-error (map car pvars) pattern "pattern variable")]
          [(not (andmap (lambda (x) (not (ellipsis? (car x)))) pvars))
           (syntax-error pattern (format "misplaced ellipsis in ~s pattern" who))]
          [else
           (let ([y (gen-var 'tmp)])
             (build-let no-source
               (list y)
               (list (cond
                       [(eq? p 'any)
                        (build-primcall no-source 3 'list
                                        (build-lexical-reference no-source x))]
                       [(eq? p '_)
                        (build-data no-source '())]
                       [else
                        (build-primcall no-source 3 '$syntax-dispatch
                                        (build-lexical-reference no-source x)
                                        (build-data no-source p))]))
               (let-syntax ([y (identifier-syntax
                                 (build-lexical-reference no-source y))])
                 (build-conditional no-source
                   (syntax-case fender ()
                     (#t y)
                     (_ (build-conditional no-source
                          y
                          (build-dispatch-call pvars fender y r)
                          (build-data no-source #f))))
                   (build-dispatch-call pvars template y r)
                   (gen-syntax-case who x keys clauses r)))))]))))

  (define gen-syntax-case
    (lambda (who x keys clauses r)
      (if (null? clauses)
          (build-primcall no-source 3 'syntax-error
            (build-lexical-reference no-source x))
          (syntax-case (car clauses) ()
            [(pattern fender template)
             (if (and (eq? #'fender #t)
                      (id? #'pattern)
                      (not (bound-id-member? #'pattern keys))
                      (not (ellipsis? #'pattern)))
                 (if (free-id=? #'pattern #'_)
                     (chi #'template r empty-wrap)
                     (let ([var (gen-var #'pattern)])
                       (let ([label (make-local-label (make-binding 'syntax `(,var . 0)) (meta-level))])
                         (let ([body (chi #'template r
                                       (make-binding-wrap #'(pattern) (list label) empty-wrap))])
                           (kill-local-label! label)
                           (build-let no-source
                             (list var)
                             (list (build-lexical-reference no-source x))
                             body)))))
                 (gen-clause who x keys (cdr clauses) r
                   #'pattern #'fender #'template))]))))

  (define valid-literal?
    (lambda (x)
      (and (id? x)
           (not (ellipsis? x))
           (not (free-identifier=? x #'_)))))

  (global-extend 'core 'syntax-case
    (lambda (e r w ae)
      (define clause
        (lambda (y)
          (syntax-case y ()
            [(pattern template)
             #'(pattern #t template)]
            [(pattern fender template)
             #'(pattern fender template)]
            [_ (syntax-error y "invalid syntax-case clause")])))
      (let ([e (source-wrap e w ae)])
        (syntax-case e ()
          [(_ val (lit ...) cl ...)
           (if (andmap valid-literal? #'(lit ...))
               (let ([x (gen-var 'tmp)])
                 (build-let ae
                   (list x)
                   (list (chi #'val r empty-wrap))
                   (gen-syntax-case 'syntax-case x #'(lit ...)
                     (map clause #'(cl ...)) r)))
               (syntax-error e "invalid literals list in"))]))))

  (let ([marked-underscore
         (make-syntax-object '_
           (make-wrap
             (cons (new-mark) (wrap-marks top-wrap))
             (cons 'shift (wrap-subst top-wrap))))])
    (define syntax-rules-transformer
      (lambda (fender-okay?)
        (lambda (e r w ae)
          (define clause
            (lambda (y)
              (syntax-case y ()
                [((keyword . pattern) template)
                 (id? #'keyword)
                 #`((#,marked-underscore . pattern) #t #'template)]
                [((keyword . pattern) fender template)
                 (and fender-okay? (id? #'keyword))
                 #`((#,marked-underscore . pattern) fender #'template)]
                [_ (syntax-error y "invalid syntax-rules clause")])))
          (let ([e (source-wrap e w ae)])
            (syntax-case e ()
              [(_ (lit ...) cl ...)
               (andmap id? #'(lit ...))
               (if (andmap valid-literal? #'(lit ...))
                   (let ([x (gen-var 'tmp)])
                     (build-lambda no-source (list x)
                       (gen-syntax-case 'syntax-rules x #'(lit ...)
                         (map clause #'(cl ...)) r)))
                   (syntax-error e "invalid literals list in"))])))))
    (global-extend 'core 'r6rs:syntax-rules (syntax-rules-transformer #f))
    (global-extend 'core 'syntax-rules (syntax-rules-transformer #t))))

(global-extend 'macro 'module
  (lambda (x)
   ; export subform -> (ex ...)
   ; ex -> id | (id ex ...)
    (define parse-exports
      (lambda (ex*)
        (let f ([ex* ex*] [export* '()] [iexport* '()])
          (if (null? ex*)
              (values export* iexport*)
              (let-values ([(export* iexport*) (f (cdr ex*) export* iexport*)])
                (syntax-case (car ex*) ()
                  [id (identifier? #'id) (values (cons #'id export*) iexport*)]
                  [(id) (identifier? #'id) (values (cons #'id export*) iexport*)]
                  [(id ex ...)
                   (identifier? #'id)
                   (let-values ([(*export* iexport*) (f #'(ex ...) '() iexport*)])
                     (values
                       (cons #'id export*)
                       (cons (cons #'id *export*) iexport*)))]
                  [x (syntax-error #'x "invalid module export")]))))))
    (define module-form
      (lambda (mid ex* body*)
        (let-values ([(export* iexport*) (parse-exports ex*)])
          (with-syntax ([(ex ...) export*] [(iex ...) iexport*])
            #`($module #,x #,mid (export ex ...) (indirect-export . iex) ... #,@body*)))))
    (syntax-case x ()
      [(_ (e ...) d ...)
       #`(begin
           #,(module-form #'anon #'(e ...) #'(d ...))
           ($import #,x (anon) #f #f))]
      [(_ m (e ...) d ...)
       (identifier? #'m)
       (module-form #'m #'(e ...) #'(d ...))])))

(global-extend 'macro 'import
  (lambda (orig)
    (syntax-case orig ()
      [(_ im ...)
       #`($import #,orig (im ...) #f #f)])))

(global-extend 'macro 'import-only
  (lambda (orig)
    (syntax-case orig ()
      [(_ im ...)
       #`($import #,orig (im ...) #t #f)])))

(let ()
  (define check-std-export!
   ; make sure this looks like a plausible standard export form...more
   ; thorough check is done by determine-exports later
    (lambda (ex)
      (unless (syntax-case ex ()
                [id (identifier? #'id) #t]
                [(?rename (old new) ...) (sym-kwd? ?rename rename) #t]
                [_ #f])
        (syntax-error ex "invalid export spec"))))

  (global-extend 'macro 'library
    (lambda (orig)
      (syntax-case orig ()
        [(_ name exports imports form ...)
         (let-values ([(library-path library-version uid) (create-library-uid #'name)])
           (syntax-case #'exports ()
             [(?export ex ...)
              (symbolic-id=? #'?export 'export)
              (begin
                (for-each check-std-export! #'(ex ...))
                (syntax-case #'imports ()
                  [(?import im ...)
                   (symbolic-id=? #'?import 'import)
                   #`($library #,orig #,library-path #,library-version #,uid
                        (implicit-exports #t)
                        (export ex ...)
                        ($import #,orig (im ...) #f #t)
                        form ...)]
                  [_ (syntax-error #'imports "invalid library import subform")]))]
             [_ (syntax-error #'exports "invalid library export subform")]))])))
)

(global-extend 'macro 'top-level-program
  (lambda (orig)
    (syntax-case orig ()
      [(_ imports form ...)
       (syntax-case #'imports ()
         [(?import im ...)
          (symbolic-id=? #'?import 'import)
          #`($program #,orig ($import #,orig (im ...) #f #t) form ...)]
         [_ (syntax-error #'imports "invalid top-level program import subform")])])))

;;; To support eval-when, we maintain two mode sets:
;;;
;;; ctem (compile-time-expression mode)
;;;   determines whether/when to evaluate compile-time expressions such
;;;   as macro definitions, module definitions, and compile-time
;;;   registration of variable definitions
;;;
;;; rtem (run-time-expression mode)
;;;   determines whether/when to evaluate run-time expressions such
;;;   as the actual assignment performed by a variable definition or
;;;   arbitrary top-level expressions

;;; Possible modes in the mode set are:
;;;
;;; L (load): evaluate at load time.  implies V for compile-time
;;;     expressions and R for run-time expressions.
;;;
;;; C (compile): evaluate at compile (file) time
;;;
;;; E (eval): evaluate at evaluation (compile or interpret) time
;;;
;;; V (visit): evaluate at visit time
;;;
;;; R (revisit): evaluate at revisit time

;;; The mode set for the body of an eval-when is determined by
;;; translating each mode in the old mode set based on the situations
;;; present in the eval-when form and combining these into a set,
;;; using the following table.  See also update-mode-set.

;;;      load  compile  visit  revisit  eval
;;;
;;; L     L      C        V       R      -
;;;
;;; C     -      -        -       -      C
;;;
;;; V     V      C        V       -      -
;;;
;;; R     R      C        -       R      -
;;;
;;; E     -      -        -       -      E

;;; When we complete the expansion of a compile or run-time expression,
;;; the current ctem or rtem determines how the expression will be
;;; treated.  See ct-eval/residualize and rt-eval/residualize.

;;; Initial mode sets
;;;
;;; when compiling a file:
;;;
;;;     initial ctem: (L C)
;;;
;;;     initial rtem: (L)
;;;
;;; when not compiling a file:
;;;
;;;     initial ctem: (E)
;;;
;;;     initial rtem: (E)
;;;
;;; Assuming (eval-syntax-expanders-when) => (compile load eval)
;;;
;;; This means that top-level syntactic definitions are evaluated
;;; immediately after they are expanded, and the expanded definitions
;;; are also residualized into the object file if we are compiling
;;; a file.

;;; This structure can easily support eval-when/ct, which affects
;;; only ctem, and eval-when/rt, which affects only rtem.

(set! sc-expand
  (rec sc-expand
    (case-lambda
      ((x) (sc-expand x (if (eq? (subset-mode) 'system) ($system-environment) (interaction-environment)) #f #f))
      ((x env) (sc-expand x env #f #f #f))
      ((x env records?) (sc-expand x env records? #f #f))
      ((x env records? compiling-a-file) (sc-expand x env records? compiling-a-file #f))
      ((x env records? compiling-a-file outfn)
       (unless (env? env)
         ($oops 'sc-expand "~s is not an environment" env))
       (unless (not outfn)
         (unless (string? outfn)
           ($oops 'sc-expand "~s is not a string or #f" outfn)))
       (if (and (pair? x) (equal? (car x) noexpand))
           (cadr x)
           (let ((ctem (initial-mode-set (eval-syntax-expanders-when) compiling-a-file))
                 (rtem (initial-mode-set '(load eval) compiling-a-file)))
             (let ([x (at-top
                        (parameterize ([meta-level 0])
                          (chi-top* x
                            (env-wrap env)
                            ctem rtem
                            (env-top-ribcage env)
                            outfn)))])
               (if records? x ($uncprep x)))))))))

(set-who! $require-include
  (lambda (path)
    (unless (string? path) ($oops who "~s is not a string" path))
    (require-include path)))

(set-who! $require-libraries
  ($make-thread-parameter
    (case-lambda [() '()] [(ls) (void)])
    (lambda (x)
      (unless (procedure? x) ($oops who "~s is not a procedure" x))
      x)))

(record-writer (type-descriptor variable-transformer)
  (lambda (x p wr)
    (display "#<variable-transformer " p)
    (wr (variable-transformer-procedure x) p)
    (display ">" p)))

(record-writer (type-descriptor compile-time-value)
  (lambda (x p wr)
    (display "#<compile-time-value " p)
    (wr ($compile-time-value-value x) p)
    (display ">" p)))

(record-writer syntax-object-rtd ; from types.ss
  (lambda (x p wr)
    (define get-source
      (lambda (src)
        (call-with-values
          (lambda () ((current-locate-source-object-source) src #t #t))
          (case-lambda
            [() (let ([sfd (source-sfd src)] [fp (source-bfp src)])
                  (format "[char ~a of ~a]"
                          fp
                          (source-file-descriptor-name sfd)))]
            [(path line char)
             (format "[line ~a, char ~a of ~a]" line char path)]))))
    (display "#<syntax " p)
    (wr (syntax->datum x) p)
    (let f ([x x])
      (if (syntax-object? x)
          (f (syntax-object-expression x))
          (when (annotation? x)
            (display " " p)
            (display (get-source (annotation-source x)) p))))
    (display ">" p)))

(record-writer (type-descriptor env)
  (lambda (x p wr)
    (let ([token (top-ribcage-key (env-top-ribcage x))])
      (if (and (symbol? token) (not (gensym? token)))
          (begin
            (display "#<environment " p)
            (display (symbol->string token) p)
            (display ">" p))
          (display "#<environment>" p)))))

(set! $make-environment
  (lambda (token mutable?)
    (let ([top-ribcage (make-top-ribcage token mutable?)])
      (make-env
        top-ribcage
        (make-wrap
          (wrap-marks top-wrap)
          (cons top-ribcage (wrap-subst top-wrap)))))))

(set! environment?
  (lambda (x)
    (env? x)))

(let ()
  (define tlb?
    (lambda (sym env)
      (cond
        [(top-id-free-label sym (wrap-marks top-wrap) (env-top-ribcage env)) =>
         (lambda (label)
           (let ([b (lookup-global label)])
             (case (binding-type b)
               [(primitive) #t]
               [(global immutable-global) ($top-level-bound? (binding-value b))]
               [(library-global)
                (invoke-loaded-library (car (binding-value b)))
                ($top-level-bound? (cdr (binding-value b)))]
               [else #f])))]
        [else #f])))

  (set! top-level-bound?
    (case-lambda
      [(sym)
       (unless (symbol? sym)
         ($oops 'top-level-bound? "~s is not a symbol" sym))
       (tlb? sym (interaction-environment))]
      [(sym env)
       (unless (symbol? sym)
         ($oops 'top-level-bound? "~s is not a symbol" sym))
       (unless (environment? env)
         ($oops 'top-level-bound? "~s is not an environment" env))
       (tlb? sym env)])))

(let ()
  (define tlv
    (lambda (sym env)
      (cond
        [(top-id-free-label sym (wrap-marks top-wrap) (env-top-ribcage env)) =>
         (lambda (label)
           (let ([b (lookup-global label)])
             (case (binding-type b)
               [(primitive) (#3%$top-level-value (binding-value b))]
               [(global immutable-global) (#2%$top-level-value (binding-value b))]
               [(library-global)
                (invoke-loaded-library (car (binding-value b)))
                (#2%$top-level-value (cdr (binding-value b)))]
               [else ($oops 'top-level-value "~s is not a variable" sym)])))]
        [else ($oops #f "variable ~s is not bound" sym)])))

  (set! top-level-value
    (case-lambda
      [(sym)
       (unless (symbol? sym)
         ($oops 'top-level-value "~s is not a symbol" sym))
       (tlv sym (interaction-environment))]
      [(sym env)
       (unless (symbol? sym)
         ($oops 'top-level-value "~s is not a symbol" sym))
       (unless (environment? env)
         ($oops 'top-level-value "~s is not an environment" env))
       (tlv sym env)])))

(let ()
  (define stlv!
    (lambda (sym val env)
      (cond
        [(top-id-free-label sym (wrap-marks top-wrap) (env-top-ribcage env)) =>
         (lambda (label)
           (let ([b (lookup-global label)])
             (case (binding-type b)
               [(global) ($set-top-level-value! (binding-value b) val)]
               [(immutable-global) ($oops 'set-top-level-value! "cannot assign immutable variable ~s" sym)]
               [(primitive)
                (unless (eq? (subset-mode) 'system)
                  ($oops 'set-top-level-value! "cannot assign immutable variable ~s" sym))
                ($set-top-level-value! (binding-type b) val)]
               [(library-global)
                ($oops 'set-top-level-value! "cannot assign immutable variable ~s" sym)]
               [else ($oops 'set-top-level-value! "~s is not a variable" sym)])))]
        [else ($oops #f "variable ~s is not bound" sym)])))

  (set! set-top-level-value!
    (case-lambda
      [(sym val)
       (unless (symbol? sym)
         ($oops 'set-top-level-value! "~s is not a symbol" sym))
       (stlv! sym val (interaction-environment))]
      [(sym val env)
       (unless (symbol? sym)
         ($oops 'set-top-level-value! "~s is not a symbol" sym))
       (unless (environment? env)
         ($oops 'set-top-level-value! "~s is not an environment" env))
       (stlv! sym val env)])))

(let ()
  (define tlm?
    (lambda (sym env)
      (cond
        [(top-id-free-label sym (wrap-marks top-wrap) (env-top-ribcage env)) =>
         (lambda (label)
           (let ([b (lookup-global label)])
             (case (binding-type b)
               [(global) #t]
               [(primitive) (eq? (subset-mode) 'system)]
               [(library-global immutable-global) #f]
               [else ($oops 'top-level-mutable? "~s is not a variable" sym)])))]
        [else ($oops 'top-level-mutable? "variable ~s is not bound" sym)])))

  (set! top-level-mutable?
    (case-lambda
      [(sym)
       (unless (symbol? sym)
         ($oops 'top-level-mutable? "~s is not a symbol" sym))
       (tlm? sym (interaction-environment))]
      [(sym env)
       (unless (symbol? sym)
         ($oops 'top-level-mutable? "~s is not a symbol" sym))
       (unless (environment? env)
         ($oops 'top-level-mutable? "~s is not an environment" env))
       (tlm? sym env)])))

(let ()
  (define dtlv
    (lambda (sym val env)
      (let ([top-ribcage (env-top-ribcage env)])
        (unless (top-ribcage-mutable? top-ribcage)
          ($oops 'define-top-level-value "cannot modify immutable environment ~s" env))
        (let-values ([(label id) (top-id-bound-label sym (wrap-marks top-wrap) top-ribcage)])
         ; though implicit, we call sc-put-cte to clear out any previous binding
          ($sc-put-cte label (make-binding 'global label) #f)
          ($set-top-level-value! label val)))))

  (set! define-top-level-value
    (case-lambda
      [(sym val)
       (unless (symbol? sym)
         ($oops 'define-top-level-value "~s is not a symbol" sym))
       (dtlv sym val (interaction-environment))]
      [(sym val env)
       (unless (symbol? sym)
         ($oops 'define-top-level-value "~s is not a symbol" sym))
       (unless (environment? env)
         ($oops 'define-top-level-value "~s is not an environment" env))
       (dtlv sym val env)])))

(let ()
  (define who 'top-level-syntax)

  (define tls
    (lambda (sym env)
      (cond
        [(top-id-free-label sym (wrap-marks top-wrap) (env-top-ribcage env)) =>
         (lambda (label)
           (let ([b (lookup-global label)])
             (case (binding-type b)
               [(macro) (binding-value b)]
               [(macro!) ($make-variable-transformer (binding-value b))]
               [(ctv) (binding-value b)]
               [else (make-core-transformer b)])))]
        [else ($oops who "~s is not defined" sym)])))

  (set! top-level-syntax
    (case-lambda
      [(sym)
       (unless (symbol? sym)
         ($oops who "~s is not a symbol" sym))
       (tls sym (interaction-environment))]
      [(sym env)
       (unless (symbol? sym)
         ($oops who "~s is not a symbol" sym))
       (unless (environment? env)
         ($oops who "~s is not an environment" env))
       (tls sym env)])))

(let ()
  (define who 'top-level-syntax?)

  (define tls?
    (lambda (sym env)
      (and
        (top-id-free-label sym (wrap-marks top-wrap) (env-top-ribcage env))
        #t)))

  (set! top-level-syntax?
    (case-lambda
      [(sym)
       (unless (symbol? sym)
         ($oops who "~s is not a symbol" sym))
       (tls? sym (interaction-environment))]
      [(sym env)
       (unless (symbol? sym)
         ($oops who "~s is not a symbol" sym))
       (unless (environment? env)
         ($oops who "~s is not an environment" env))
       (tls? sym env)])))

(let ()
  (define who 'define-top-level-syntax)

  (define dtls
    (lambda (sym val env)
      (unless (symbol? sym) ($oops who "~s is not a symbol" sym))
      (let ([val (transformer->binding who val)])
        (let ([top-ribcage (env-top-ribcage env)])
          (unless (top-ribcage-mutable? top-ribcage)
            ($oops who "cannot modify immutable environment ~s" env))
          (let-values ([(label id) (top-id-bound-label sym (wrap-marks top-wrap) top-ribcage)])
            ($sc-put-cte label val #f))))))

  (set! define-top-level-syntax
    (case-lambda
      [(sym val)
       (dtls sym val (interaction-environment))]
      [(sym val env)
       (unless (environment? env) ($oops who "~s is not an environment" env))
       (dtls sym val env)])))

;; entry points for C to call
(set! $c-tlv
  (lambda (sym)
    (guard (c [#t ($unbound-object)])
      (top-level-value sym))))

(set! $c-stlv!
  (lambda (sym val)
    (guard (c [#t #f])
      (set-top-level-value! sym val)
      #t)))

(set-who! environment-mutable?
  (lambda (env)
    (unless (environment? env)
      ($oops who "~s is not an environment" env))
    (top-ribcage-mutable? (env-top-ribcage env))))

(set! environment-symbols
  (lambda (env) '()
    (unless (environment? env)
      ($oops 'environment-symbols "~s is not an environment" env))
    (let ([token (top-ribcage-key (env-top-ribcage env))])
      (let f ([ls (oblist)] [syms '()])
        (if (null? ls)
            syms
            (f (cdr ls)
               (let ([x (car ls)])
                 (if (cond
                       [(lookup-global-label x (wrap-marks top-wrap) token) =>
                        (lambda (label)
                          (or (get-global-definition-hook label)
                              ($top-level-bound? label)))]
                       [else #f])
                     (cons x syms)
                     syms))))))))

(set! copy-environment
  (let ()
    (define (copy-environment env mutable? syms)
      (let ([token (top-ribcage-key (env-top-ribcage env))]
            [new-token (gensym)])
        (for-each
          (lambda (sym)
            (cond
              [(lookup-global-label/pl sym (wrap-marks top-wrap) token) =>
               (lambda (label/pl)
                 (if (and (symbol? label/pl) (eq? label/pl (make-token:sym token sym)))
                     (let ([new-label (make-token:sym new-token sym)]
                           [b (get-global-definition-hook label/pl)])
                       (let ([id (make-resolved-id sym (wrap-marks top-wrap) new-label)])
                         (cond
                           [(or (not b)
                                (and (eq? (binding-type b) 'global)
                                     (eq? (binding-value b) label/pl)))
                            ($set-top-level-value! new-label (#3%$top-level-value label/pl))
                            ($sc-put-cte id (make-binding (if mutable? 'global 'immutable-global) new-label) new-token)]
                           [(and mutable? (eq? (binding-type b) 'immutable-global))
                            ($set-top-level-value! new-label (#3%$top-level-value (binding-value b)))
                            ($sc-put-cte id (make-binding 'global new-label) new-token)]
                           [else ($sc-put-cte id b new-token)])))
                     (store-global-subst (make-resolved-id sym (wrap-marks top-wrap) label/pl) new-token '())))]))
          syms)
        ($make-environment new-token mutable?)))
    (case-lambda
      [(env)
       (unless (environment? env)
         ($oops 'copy-environment "~s is not an environment" env))
       (copy-environment env #t (oblist))]
      [(env mutable?)
       (unless (environment? env)
         ($oops 'copy-environment "~s is not an environment" env))
       (copy-environment env mutable? (oblist))]
      [(env mutable? syms)
       (unless (environment? env)
         ($oops 'copy-environment "~s is not an environment" env))
       (unless (and (list? syms) (andmap symbol? syms))
         ($oops 'copy-environment "~s is not a list of symbols" syms))
       (copy-environment env mutable? syms)])))

(set! interaction-environment
  ($make-thread-parameter
    ($make-environment '*top* #t)
    (lambda (x)
      (unless (environment? x)
        ($oops 'interaction-environment "~s is not an environment" x))
      x)))

(set! environment
  (lambda import-spec*
    (define eval-import
      (lambda (orig env)
        (lambda (import-spec)
          (top-level-eval-hook
            (at-top
              (parameterize ([meta-level 0])
                (chi-top* `(,(wrap '$import (env-wrap ($system-environment))) ,orig
                            ,(wrap (list import-spec) (env-wrap env)) #f #f)
                  empty-wrap
                  (initial-mode-set '(eval) #f)
                  (initial-mode-set '(eval) #f)
                  (env-top-ribcage env)
                  #f)))))))
    (with-exception-handler
      (lambda (c)
        (raise-continuable
          (if (who-condition? c)
              c
              (condition (make-who-condition 'environment) c))))
      (lambda ()
        (let ([env ($make-environment (gensym) #t)])
          (for-each (eval-import (datum->syntax #'* (cons 'environment import-spec*)) env) import-spec*)
          (top-ribcage-mutable?-set! (env-top-ribcage env) #f)
          env)))))

(set-who! #(r6rs: eval)
  (lambda (x env)
    (unless (env? env)
      ($oops who "~s is not an environment" env))
    (top-level-eval-hook
      (not-at-top
        (parameterize ([meta-level 0])
          (chi* x (env-wrap env)))))))

(set! $real-sym-name
  (lambda (name env)
    (if (gensym? name)
        (let ([pretty-name (string->symbol (symbol->string name))])
          (if (eq? (lookup-global-label pretty-name (wrap-marks top-wrap)
                     (top-ribcage-key (env-top-ribcage env)))
                   name)
              pretty-name
              name))
        name)))

(set! scheme-environment
  (let ([r ($make-environment '*scheme* #f)])
    (lambda () r)))
(set! $system-environment
  (let ([r ($make-environment '*system* #f)])
    (lambda () r)))
(set! ieee-environment
  (let ([r ($make-environment '*ieee* #f)])
    (lambda () r)))
(set! null-environment
  (let ([r ($make-environment '*r5rs-syntax* #f)])
    (lambda (n)
      (unless (eq? n 5)
        ($oops 'null-environment "invalid report specifier ~s" n))
      r)))
(set! scheme-report-environment
  (let ([r ($make-environment '*r5rs* #f)])
    (lambda (n)
      (unless (eq? n 5)
        ($oops 'scheme-report-environment
          "invalid report specifier ~s"
          n))
      r)))

(set! $syntax-top-level?
  (lambda ()
    at-top-level?))

(set! $cte-optimization-info
  (lambda (sym)
    (let ([box (get-clo-info sym)])
      (if box (unbox box) '()))))

(set! identifier?
  (lambda (x)
    (nonsymbol-id? x)))

(let ()
  (define d->s
    (lambda (id datum who)
      (unless (nonsymbol-id? id) ($oops who "~s is not an identifier" id))
     ; no longer transferring annotation, since this can produce
     ; misleading profile output
      (make-syntax-object datum (syntax-object-wrap id))))
  (set-who! datum->syntax
    (lambda (id datum)
      (d->s id datum who)))
  (set-who! datum->syntax-object
    (lambda (id datum)
      (d->s id datum who))))

;; for bootstrapping via "reboot.ss":
(set! $datum->environment-syntax
  (lambda (sym env)
    (make-syntax-object sym (make-wrap (wrap-marks top-wrap)
                                       (cons (env-top-ribcage env)
                                             (wrap-subst top-wrap))))))

(set! syntax->list
  (lambda (orig-ls)
    (let f ([ls orig-ls])
      (syntax-case ls ()
        [() '()]
        [(x . r) (cons #'x (f #'r))]
        [_ ($oops 'syntax->list "invalid argument ~s" orig-ls)]))))

(set! syntax->vector
  (lambda (v)
    (syntax-case v ()
      [#(x ...) (apply vector (syntax->list #'(x ...)))]
      [_ ($oops 'syntax->vector "invalid argument ~s" v)])))

(set! syntax->datum
  ; accepts any object, since syntax objects may consist partially
  ; or entirely of unwrapped, nonsymbolic data
  (lambda (x)
    (strip x empty-wrap)))

(set! syntax-object->datum
  (lambda (x)
    (strip x empty-wrap)))

(let ()
  (define strip-outer
    (lambda (x)
      (cond
        [(syntax-object? x) (strip-outer (syntax-object-expression x))]
        [(annotation? x) (annotation-stripped x)]
        [else x])))
  (set-who! generate-temporaries
    (lambda (x)
      (define (gen-temp) (wrap (gensym) top-wrap))
      (let f ([fast x] [slow x])
        (let ([fast (strip-outer fast)])
          (cond
            [(null? fast) '()]
            [(pair? fast)
             (cons (gen-temp)
               (let ([fast (strip-outer (cdr fast))])
                 (cond
                   [(null? fast) '()]
                   [(pair? fast)
                    (cons (gen-temp)
                      (let ([slow (strip-outer slow)])
                        (if (eq? fast slow)
                            ($oops who "cyclic list structure ~s" x)
                            (if (pair? slow)
                                (f (cdr fast) (cdr slow))
                                ($oops who "improper list structure ~s" x)))))]
                   [else ($oops who "improper list structure ~s" x)])))]
            [else ($oops who "improper list structure ~s" x)]))))))

(set-who! free-identifier=?
  (lambda (x y)
    (unless (nonsymbol-id? x) ($oops who "~s is not an identifier" x))
    (unless (nonsymbol-id? y) ($oops who "~s is not an identifier" y))
    (free-id=? x y)))

(set-who! bound-identifier=?
  (lambda (x y)
    (unless (nonsymbol-id? x) ($oops who "~s is not an identifier" x))
    (unless (nonsymbol-id? y) ($oops who "~s is not an identifier" y))
    (bound-id=? x y)))

(set-who! literal-identifier=? ; now same as free-identifier=?
  (lambda (x y)
    (unless (nonsymbol-id? x) ($oops who "~s is not an identifier" x))
    (unless (nonsymbol-id? y) ($oops who "~s is not an identifier" y))
    (free-id=? x y)))

(set! $distinct-bound-ids?
  (lambda (ids)
    (distinct-bound-ids? ids)))

(set-who! make-variable-transformer
  (lambda (proc)
    (unless (procedure? proc) ($oops who "~s is not a procedure" proc))
    ($make-variable-transformer proc)))

(set-who! make-compile-time-value
  (lambda (x)
    ($make-compile-time-value x)))

(set-who! compile-time-value?
  (lambda (x)
    ($compile-time-value? x)))

(set-who! compile-time-value-value
  (lambda (x)
    (unless ($compile-time-value? x) ($oops who "~s is not a compile-time value" x))
    ($compile-time-value-value x)))

(set! $syntax->src
  (lambda (x)
    (let f ([x x] [n 0] [k (lambda () (values #f #t))])
      (cond
        [(annotation? x)
         (if (fxlogtest (annotation-flags x) (constant annotation-debug))
             (values (annotation-source x) (if (fx= n 0) #t 'near))
             (k))]
        [(syntax-object? x) (f (syntax-object-expression x) n k)]
        [(fx= n 3) (k)]
        [(pair? x) (f (car x) (fx+ n 1) (lambda () (f (cdr x) (fx+ n 1) k)))]
        [(vector? x) (if (fx= (vector-length x) 0) (k) (f (vector-ref x 0) (fx+ n 1) k))]
        [else (k)]))))

;;; syntax-dispatch expects an expression and a pattern.  If the expression
;;; matches the pattern a list of the matching expressions for each
;;; "any" is returned.  Otherwise, #f is returned.  (This use of #f will
;;; not work on r4rs implementations that violate the ieee requirement
;;; that #f and () be distinct.)

;;; The expression is matched with the pattern as follows:

;;; p in pattern:                        matches:
;;;   ()                                 empty list
;;;   any                                anything
;;;   (p1 . p2)                          pair (list)
;;;   #(free-id <key>)                   <key> with free-identifier=?
;;;   each-any                           any proper list
;;;   #(each p)                          (p*)
;;;   #(each+ p1 (p2_1 ...p2_n) p3)      (p1* (p2_n ... p2_1) . p3)
;;;   #(vector p)                        (list->vector p)
;;;   #(box p)                           (box p)
;;;   #(atom <object>)                   <object> with "equal?"

;;; Vector cops out to pair under assumption that vectors are rare.  If
;;; not, should convert to:
;;;   #(vector p)                        #(p*)

(let ()

(define match-each
  (lambda (e p w)
    (cond
      ((annotation? e)
       (match-each (annotation-expression e) p w))
      ((pair? e)
       (let ((first (match (car e) p w '())))
         (and first
              (let ((rest (match-each (cdr e) p w)))
                 (and rest (cons first rest))))))
      ((null? e) '())
      ((syntax-object? e)
       (match-each (syntax-object-expression e)
                   p
                   (join-wraps w (syntax-object-wrap e))))
      (else #f))))

(define match-each+
  (lambda (e x-pat y-pat z-pat w r)
    (let f ([e e] [w w])
      (cond
        [(pair? e)
         (let-values ([(xr* y-pat r) (f (cdr e) w)])
           (if r
               (if (null? y-pat)
                   (let ([xr (match (car e) x-pat w '())])
                     (if xr
                         (values (cons xr xr*) y-pat r)
                         (values #f #f #f)))
                   (values '() (cdr y-pat) (match (car e) (car y-pat) w r)))
               (values #f #f #f)))]
        [(annotation? e) (f (annotation-expression e) w)]
        [(syntax-object? e) (f (syntax-object-expression e)
                               (join-wraps w (syntax-object-wrap e)))]
        [else (values '() y-pat (match e z-pat w r))]))))

(define match-each-any
  (lambda (e w)
    (cond
      ((annotation? e)
       (match-each-any (annotation-expression e) w))
      ((pair? e)
       (let ((l (match-each-any (cdr e) w)))
         (and l (cons (wrap (car e) w) l))))
      ((null? e) '())
      ((syntax-object? e)
       (match-each-any (syntax-object-expression e)
                       (join-wraps w (syntax-object-wrap e))))
      (else #f))))

(define match-empty
  (lambda (p r)
    (cond
      ((null? p) r)
      ((eq? p '_) r)
      ((eq? p 'any) (cons '() r))
      ((pair? p) (match-empty (car p) (match-empty (cdr p) r)))
      ((eq? p 'each-any) (cons '() r))
      (else
       (case (vector-ref p 0)
         ((each) (match-empty (vector-ref p 1) r))
         ((each+) (match-empty (vector-ref p 1)
                    (match-empty (reverse (vector-ref p 2))
                      (match-empty (vector-ref p 3) r))))
         ((free-id atom) r)
         ((box) (match-empty (vector-ref p 1) r))
         ((vector) (match-empty (vector-ref p 1) r)))))))

(define combine
  (lambda (r* r)
    (if (null? (car r*))
        r
        (cons (map car r*) (combine (map cdr r*) r)))))

(define match*
  (lambda (e p w r)
    (cond
      ((null? p) (and (null? e) r))
      ((pair? p)
       (and (pair? e) (match (car e) (car p) w
                        (match (cdr e) (cdr p) w r))))
      ((eq? p 'each-any)
       (let ((l (match-each-any e w))) (and l (cons l r))))
      (else
       (case (vector-ref p 0)
         ((each)
          (if (null? e)
              (match-empty (vector-ref p 1) r)
              (let ((r* (match-each e (vector-ref p 1) w)))
                (and r* (combine r* r)))))
         ((free-id)
          (and (id? e)
            (let ([id (wrap e w)])
              (if (symbol? id)
                 ; someone's using syntax-case on a raw s-expression
                 ; and presumably wants symbolic comparison
                  (eq? id (id-sym-name (vector-ref p 1)))
                  (free-id=? id (vector-ref p 1))))
            r))
         ((each+)
          (let-values ([(xr* y-pat r)
                        (match-each+ e (vector-ref p 1) (vector-ref p 2)
                          (vector-ref p 3) w r)])
            (and r (null? y-pat)
              (if (null? xr*)
                  (match-empty (vector-ref p 1) r)
                  (combine xr* r)))))
         ((atom) (and (equal? (vector-ref p 1) (strip e w)) r))
         ((box) (and (box? e) (match (unbox e) (vector-ref p 1) w r)))
         ((vector)
          (and (vector? e)
               (match (vector->list e) (vector-ref p 1) w r))))))))

(define match
  (lambda (e p w r)
    (cond
      ((not r) #f)
      ((eq? p '_) r)
      ((eq? p 'any) (cons (wrap e w) r))
      ((syntax-object? e)
       (match*
         (unannotate (syntax-object-expression e))
         p
         (join-wraps w (syntax-object-wrap e))
         r))
      (else (match* (unannotate e) p w r)))))

(set! $syntax-dispatch
  (lambda (e p)
    (cond
      ((eq? p '_) '())
      ((eq? p 'any) (list e))
      ((syntax-object? e)
       (match* (unannotate (syntax-object-expression e))
         p (syntax-object-wrap e) '()))
      (else (match* (unannotate e) p empty-wrap '())))))

(set! $noexpand?
  (lambda (x)
    (and (pair? x) (equal? (car x) noexpand))))


(set! $build-library-exts build-library-exts)
))

(current-expand sc-expand)

(begin
;;; syntax-rules/syntax-case aux keywords
(define-syntax ...
  (lambda (x)
    (syntax-error x "misplaced aux keyword")))
(define-syntax _
  (lambda (x)
    (syntax-error x "misplaced aux keyword")))

;;; import aux keywords
(define-syntax only
  (lambda (x)
    (syntax-error x "misplaced aux keyword")))
(define-syntax except
  (lambda (x)
    (syntax-error x "misplaced aux keyword")))
(define-syntax add-prefix
  (lambda (x)
    (syntax-error x "misplaced aux keyword")))
(define-syntax drop-prefix
  (lambda (x)
    (syntax-error x "misplaced aux keyword")))
(define-syntax rename
  (lambda (x)
    (syntax-error x "misplaced aux keyword")))
#;(define-syntax alias ; already built-in
  (lambda (x)
    (syntax-error x "misplaced aux keyword")))

(define-syntax with-syntax
   (lambda (x)
      (syntax-case x ()
         ((_ () e1 e2 ...)
          (syntax (let () e1 e2 ...)))
         ((_ ((out in)) e1 e2 ...)
          (syntax (syntax-case in () (out (let () e1 e2 ...)))))
         ((_ ((out in) ...) e1 e2 ...)
          (syntax (syntax-case (list in ...) ()
                     ((out ...) (let () e1 e2 ...))))))))

(define-syntax with-implicit
  (syntax-rules ()
    [(_ (tid id ...) e1 e2 ...)
     (andmap identifier? (syntax (tid id ...)))
     (begin
       (unless (identifier? (syntax tid))
         (syntax-error (syntax tid) "non-identifier with-implicit template"))
       (with-syntax ([id (datum->syntax (syntax tid) 'id)] ...)
         e1 e2 ...))]))

(define-syntax datum
  (syntax-rules ()
    [(_ x) (syntax->datum (syntax x))]))

(define-syntax or
  (lambda (x)
    (syntax-case x ()
      [(_ e1 e2 ...)
       (let f ([e #'e1] [e* #'(e2 ...)])
         (if (null? e*)
             e
             #`(let ([t #,e]) (if t t #,(f (car e*) (cdr e*))))))]
      [(_) #'#f])))

(define-syntax and
  (lambda (x)
    (syntax-case x ()
      [(_ e1 e2 ...)
       (let f ([e #'e1] [e* #'(e2 ...)])
         (if (null? e*)
             e
             #`(if #,e #,(f (car e*) (cdr e*)) #f)))]
      [(_) #'#t])))

(define-syntax cond
  (lambda (x)
    (syntax-case x ()
      [(_ m1 m2 ...)
       (let f ([clause #'m1] [clauses #'(m2 ...)])
         (if (null? clauses)
             (syntax-case clause (else =>)
               [(else e1 e2 ...) #'(begin e1 e2 ...)]
               [(e0) #'(let ([t e0]) (if t t))]
              ; let for p below effectively forces e1 to evaluate to a
              ; procedure rather than a macro with one subform
               [(e0 => e1) #'(let ([t e0]) (if t ((let ([p e1]) p) t)))]
               [(e0 e1 e2 ...) #'(if e0 (begin e1 e2 ...))]
               [_ (syntax-error x)])
             (with-syntax ([rest (f (car clauses) (cdr clauses))])
               (syntax-case clause (else =>)
                 [(e0) #'(let ([t e0]) (if t t rest))]
                 [(e0 => e1) #'(let ([t e0]) (if t ((let ([p e1]) p) t) rest))]
                 [(e0 e1 e2 ...) #'(if e0 (begin e1 e2 ...) rest)]
                 [_ (syntax-error x)]))))])))

;;; cond aux keywords
(define-syntax else
  (lambda (x)
    (syntax-error x "misplaced aux keyword")))
(define-syntax =>
  (lambda (x)
    (syntax-error x "misplaced aux keyword")))

;; ========================================================================
;; The exclusive cond macro -- restricted cond, and clauses must be mutually exclusive.
;;
;; Uses profiling information to rearrange clauses in most likely to succeed order.
;; ========================================================================

(define-syntax exclusive-cond
  (lambda (x)
    (define (helper clause* els?)
      (let ([sort? ($profile-source-data?)])
        (define-record-type clause
          (nongenerative)
          (fields (immutable clause) (immutable weight))
          (protocol
            (lambda (new)
              (lambda (clause e)
                (new clause (and sort? (or (profile-query-weight e) -1.0)))))))
        (define (parse-clause clause)
          (syntax-case clause (=>)
            ; Can't figure out what to profile just yet, so we restrict
            ; exclusive-cond to not have this form
            #;[(e0) (make-clause clause #'e0)]
            [(e0 => e1) (make-clause clause #'e1)]
            [(e0 e1 e2 ...) (make-clause clause #'e1)]
            [_ (syntax-error clause "invalid exclusive-cond clause")]))
        (define (sort-em clause*)
          (if sort?
              (sort (lambda (cl1 cl2) (> (clause-weight cl1) (clause-weight cl2)))
                clause*)
              clause*))
        #`(cond #,@(map clause-clause (sort-em (map parse-clause clause*))) . #,els?)))
    (syntax-case x (else)
      [(_ m1 ... [else e1 e2 ...]) (helper #'(m1 ...) #'([else e1 e2 ...]))]
      [(_ m1 m2 ...) (helper #'(m1 m2 ...) #'())])))

(define-syntax do
   (lambda (orig-x)
      (syntax-case orig-x ()
         ((_ ((var init . step) ...) (e0 e1 ...) c ...)
          (with-syntax (((step ...)
                         (map (lambda (v s)
                                 (syntax-case s ()
                                    (() v)
                                    ((e) (syntax e))
                                    (_ (syntax-error orig-x))))
                              (syntax (var ...))
                              (syntax (step ...)))))
             (syntax-case (syntax (e1 ...)) ()
                (() (syntax (let do ((var init) ...)
                               (if (not e0)
                                   (begin c ... (do step ...))))))
                ((e1 e2 ...)
                 (syntax (let do ((var init) ...)
                            (if e0
                                (begin e1 e2 ...)
                                (begin c ... (do step ...))))))))))))

(define-syntax quasiquote
  (let ()
    (define (quasi p lev)
      (syntax-case p (unquote quasiquote)
        [(unquote p)
         (if (= lev 0)
             #'("value" p)
             (quasicons #'("quote" unquote) (quasi #'(p) (- lev 1))))]
        [(quasiquote p) (quasicons #'("quote" quasiquote) (quasi #'(p) (+ lev 1)))]
        [(p . q)
         (syntax-case #'p (unquote unquote-splicing)
           [(unquote p ...)
            (if (= lev 0)
                (quasilist* #'(("value" p) ...) (quasi #'q lev))
                (quasicons
                  (quasicons #'("quote" unquote) (quasi #'(p ...) (- lev 1)))
                  (quasi #'q lev)))]
           [(unquote-splicing p ...)
            (if (= lev 0)
                (quasiappend #'(("value" p) ...) (quasi #'q lev))
                (quasicons
                  (quasicons #'("quote" unquote-splicing) (quasi #'(p ...) (- lev 1)))
                  (quasi #'q lev)))]
           [_ (quasicons (quasi #'p lev) (quasi #'q lev))])]
        [#(x ...) (quasivector (vquasi #'(x ...) lev))]
        [#&x (quasibox (quasi #'x lev))]
        [p #'("quote" p)]))
    (define (vquasi p lev)
      (syntax-case p ()
        [(p . q)
         (syntax-case #'p (unquote unquote-splicing)
           [(unquote p ...)
            (if (= lev 0)
                (quasilist* #'(("value" p) ...) (vquasi #'q lev))
                (quasicons
                  (quasicons #'("quote" unquote) (quasi #'(p ...) (- lev 1)))
                  (vquasi #'q lev)))]
           [(unquote-splicing p ...)
            (if (= lev 0)
                (quasiappend #'(("value" p) ...) (vquasi #'q lev))
                (quasicons
                  (quasicons
                    #'("quote" unquote-splicing)
                    (quasi #'(p ...) (- lev 1)))
                  (vquasi #'q lev)))]
           [_ (quasicons (quasi #'p lev) (vquasi #'q lev))])]
        [() #'("quote" ())]))
    (define (quasicons x y)
      (with-syntax ([x x] [y y])
        (syntax-case #'y ()
          [("quote" dy)
           (syntax-case #'x ()
             [("quote" dx) #'("quote" (dx . dy))]
             [_ (if (null? #'dy) #'("list" x) #'("list*" x y))])]
          [("list" . stuff) #'("list" x . stuff)]
          [("list*" . stuff) #'("list*" x . stuff)]
          [_ #'("list*" x y)])))
    (define (quasiappend x y)
      (syntax-case y ()
        [("quote" ())
         (cond
           [(null? x) #'("quote" ())]
           [(null? (cdr x)) (car x)]
           [else (with-syntax ([(p ...) x]) #'("append" p ...))])]
        [_
         (cond
           [(null? x) y]
           [else (with-syntax ([(p ...) x] [y y]) #'("append" p ... y))])]))
    (define (quasilist* x y)
      (let f ((x x))
        (if (null? x)
            y
            (quasicons (car x) (f (cdr x))))))
    (define (quasivector x)
      (syntax-case x ()
        [("quote" (x ...)) #'("quote" #(x ...))]
        [_
         (let f ([y x] [k (lambda (ls) #`("vector" #,@ls))])
           (syntax-case y ()
             [("quote" (y ...)) (k #'(("quote" y) ...))]
             [("list" y ...) (k #'(y ...))]
             [("list*" y ... z) (f #'z (lambda (ls) (k (append #'(y ...) ls))))]
             [else #`("list->vector" #,x)]))]))
    (define (quasibox x)
      (syntax-case #'x ()
        [("quote" x) #'("quote" #&x)]
        [else #`("box" #,x)]))
    (define (emit x)
      (syntax-case x ()
        [("quote" x) #''x]
        [("list" x ...) #`(list #,@(map emit #'(x ...)))]
        [("list*" x y) #`(cons #,(emit #'x) #,(emit #'y))]
        [("list*" x ...) #`(list* #,@(map emit #'(x ...)))]
        [("append" x ...) #`(append #,@(map emit #'(x ...)))]
        [("vector" x ...) #`(vector #,@(map emit #'(x ...)))]
        [("list->vector" x) #`(list->vector #,(emit #'x))]
        [("box" x) #`(box #,(emit #'x))]
        [("value" x) #'x]))
    (lambda (x)
      (syntax-case x ()
       ; convert to intermediate language, combining introduced (but not
       ; unquoted source) quote expressions where possible and choosing
       ; optimal construction code otherwise, then emit Scheme code
       ; corresponding to the intermediate language forms.
        [(_ e) (emit (quasi #'e 0))]))))

;;; quasiquote aux keywords
(define-syntax unquote
  (lambda (x)
    (syntax-error x "misplaced aux keyword")))
(define-syntax unquote-splicing
  (lambda (x)
    (syntax-error x "misplaced aux keyword")))

(define-syntax quasisyntax
  (lambda (x)
    (define (qs q n b* k)
      (syntax-case q (quasisyntax unsyntax unsyntax-splicing)
        [(quasisyntax . d)
         (qs #'d (+ n 1) b*
           (lambda (b* dnew)
             (k b*
                (if (eq? dnew #'d)
                    q
                    (with-syntax ([d dnew]) #'(quasisyntax . d))))))]
        [(unsyntax . d)
         (not (= n 0))
         (qs #'d (- n 1) b*
           (lambda (b* dnew)
             (k b*
                (if (eq? dnew #'d)
                    q
                    (with-syntax ([d dnew]) #'(unsyntax . d))))))]
        [(unsyntax-splicing . d)
         (not (= n 0))
         (qs #'d (- n 1) b*
           (lambda (b* dnew)
             (k b*
                (if (eq? dnew #'d)
                    q
                    (with-syntax ([d dnew]) #'(unsyntax-splicing . d))))))]
        [(unsyntax q)
         (= n 0)
         (with-syntax ([(t) (generate-temporaries #'(q))])
           (k (cons #'[t q] b*) #'t))]
        [((unsyntax q ...) . d)
         (= n 0)
         (qs #'d n b*
           (lambda (b* dnew)
             (with-syntax ([(t ...) (generate-temporaries #'(q ...))])
               (k (append #'([t q] ...) b*)
                  (with-syntax ([d dnew]) #'(t ... . d))))))]
        [((unsyntax-splicing q ...) . d)
         (= n 0)
         (qs #'d n b*
           (lambda (b* dnew)
             (with-syntax ([(t ...) (generate-temporaries #'(q ...))])
               (k (append #'([(t (... ...)) q] ...) b*)
                  (with-syntax ([((m ...) ...) #'([t (... ...)] ...)])
                    (with-syntax ([d dnew]) #'(m ... ... . d)))))))]
        [(a . d)
         (qs #'a n b*
           (lambda (b* anew)
             (qs #'d n b*
               (lambda (b* dnew)
                 (k b*
                    (if (and (eq? anew #'a) (eq? dnew #'d))
                        q
                        (with-syntax ([a anew] [d dnew]) #'(a . d))))))))]
        [#(x ...)
         (vqs #'(x ...) n b*
           (lambda (b* xnew*)
             (k b*
                (if (let same? ([x* #'(x ...)] [xnew* xnew*])
                      (if (null? x*)
                          (null? xnew*)
                          (and (not (null? xnew*))
                               (eq? (car x*) (car xnew*))
                               (same? (cdr x*) (cdr xnew*)))))
                    q
                    (with-syntax ([(x ...) xnew*]) #'#(x ...))))))]
        [_ (k b* q)]))
    (define (vqs x* n b* k)
      (if (null? x*)
          (k b* '())
          (vqs (cdr x*) n b*
            (lambda (b* xnew*)
              (syntax-case (car x*) (unsyntax unsyntax-splicing)
                [(unsyntax q ...)
                 (= n 0)
                 (with-syntax ([(t ...) (generate-temporaries #'(q ...))])
                   (k (append #'([t q] ...) b*)
                      (append #'(t ...) xnew*)))]
                [(unsyntax-splicing q ...)
                 (= n 0)
                 (with-syntax ([(t ...) (generate-temporaries #'(q ...))])
                   (k (append #'([(t (... ...)) q] ...) b*)
                      (with-syntax ([((m ...) ...) #'([t (... ...)] ...)])
                        (append #'(m ... ...) xnew*))))]
                [_ (qs (car x*) n b*
                     (lambda (b* xnew)
                       (k b* (cons xnew xnew*))))])))))
    (syntax-case x ()
      [(_ x)
       (qs #'x 0 '()
         (lambda (b* xnew)
           (if (eq? xnew #'x)
               #'(syntax x)
               (with-syntax ([(b ...) b*] [x xnew])
                 #'(with-syntax (b ...) (syntax x))))))])))

;;; quasisyntax aux keywords
(define-syntax unsyntax
  (lambda (x)
    (syntax-error x "misplaced aux keyword")))
(define-syntax unsyntax-splicing
  (lambda (x)
    (syntax-error x "misplaced aux keyword")))

(define-syntax include
  (lambda (x)
    (define read-file
      (lambda (fn k)
        (with-source-path 'include fn
          (lambda (fn)
            (let* ([p ($open-file-input-port 'include fn)]
                   [sfd ($source-file-descriptor fn p)]
                   [p (transcoded-port p (current-transcoder))])
              (let ([do-read ($make-read p sfd 0)])
                (let f ()
                  (let ([x (do-read)])
                    (if (eof-object? x)
                        (begin (close-input-port p) '())
                        (cons (datum->syntax k x) (f)))))))))))
    (syntax-case x ()
      [(k filename)
       (string? (datum filename))
       (let ([fn (datum filename)])
         (with-syntax (((exp ...) (read-file fn #'k)))
           ($require-include fn)
           #'(begin exp ...)))])))

(define-syntax $case
  (lambda (x)
    (define-record-type clause
      (nongenerative)
      (fields (mutable keys) (immutable body)))
    (define parse-clause
      (lambda (atomic-keys?)
        (lambda (clause)
          (syntax-case clause ()
            ; a case clause eventually expands into an exclusive-cond clause.  the e1 e2 ... body
            ; structure must remain intact so exclusive-cond can use e1's profile count, if any,
            ; to determine the clause's position in the output.  but naively leaving e1 e2 ...
            ; in place results in case inappropriately supporting cond's => syntax, so we explicitly
            ; weed out uses of => here.
            [(k arrow e1 e2 ...)
             (and (identifier? #'arrow) (free-identifier=? #'arrow #'=>))
             (syntax-error #'arrow "misplaced aux keyword")]
            [((k ...) e1 e2 ...) (make-clause #'(k ...) #'(e1 e2 ...))]
            [(k e1 e2 ...) atomic-keys? (make-clause #'(k) #'(e1 e2 ...))]
            [_ (syntax-error clause "invalid case clause")]))))
    (define trim-keys!
      (let ([ht (make-hashtable equal-hash equal?)])
        (lambda (clause)
          ; remove keys already seen in the same or a previous clause.  we must remove
          ; keys seen in a previous clause before expanding to exclusive-cond, which
          ; might reorder clauses, and removing those in the same clause doesn't do any
          ; harm and might be beneficial if the compiler doesn't do it for us.
          (clause-keys-set! clause
            (let f ([keys (clause-keys clause)])
              (if (null? keys)
                  '()
                  (let ([key (car keys)])
                    (let ([datum-key (syntax->datum key)])
                      (if (hashtable-ref ht datum-key #f)
                          (f (cdr keys))
                          (begin
                            (hashtable-set! ht datum-key #t)
                            (cons key (f (cdr keys)))))))))))))
    (define helper
      (lambda (mem atomic-keys? key-expr clause* else*)
        (let ([clause* (map (parse-clause atomic-keys?) clause*)])
          (for-each trim-keys! clause*)
          #`(let ([t #,key-expr])
             (exclusive-cond
               #,@(map (lambda (clause)
                         ; the compiler reduces memv or member calls like those we produce here
                         ; to less expensive code (using memq or eqv? or eq?) when the elements
                         ; of the constant second argument (keys in this case) allow.
                         #`[(#,mem t '#,(clause-keys clause)) #,@(clause-body clause)])
                    ; we could remove keyless clauses here but don't because that would suppress
                    ; various compile-time errors in the clause body.  cp0 will optimize away the
                    ; code we produce for keyless clauses anyway.
                    clause*)
               #,@else*)))))
    (syntax-case x (else)
      [(_ mem atomic-keys? e clause ... [else e1 e2 ...])
       (helper #'mem (datum atomic-keys?) #'e #'(clause ...) #'([else e1 e2 ...]))]
      [(_ mem atomic-keys? e clause1 clause2 ...)
       (helper #'mem (datum atomic-keys?) #'e #'(clause1 clause2 ...) #'())])))

(define-syntax r6rs:case
  (syntax-rules ()
    [(_ e clause1 clause2 ...) ($case memv #f e clause1 clause2 ...)]))

(define-syntax case
  (syntax-rules ()
    [(_ e clause1 clause2 ...) ($case member #t e clause1 clause2 ...)]))

;;; case aux keywords
#;(define-syntax else ; defined above for cond
  (lambda (x)
    (syntax-error x "misplaced aux keyword")))

(define-syntax identifier-syntax
  (syntax-rules (set!)
    [(_ e)
     (lambda (x)
       (syntax-case x ()
         [id (identifier? (syntax id)) (syntax e)]
         [(_ x (... ...)) (syntax (e x (... ...)))]))]
    [(_ (id exp1) ((set! var val) exp2))
     (and (identifier? (syntax id)) (identifier? (syntax var)))
     (make-variable-transformer
       (lambda (x)
         (syntax-case x (set!)
           [(set! var val) (syntax exp2)]
           [(id x (... ...)) (syntax (exp1 x (... ...)))]
           [id (identifier? (syntax id)) (syntax exp1)])))]))

;;; identifier-syntax aux keywords
#;(define-syntax set! ; already built in
  (lambda (x)
    (syntax-error x "misplaced aux keyword")))

(define-syntax delay
  (lambda (x)
    (syntax-case x ()
      ((delay exp)
       (syntax ($make-promise (lambda () exp)))))))

(define-syntax define-structure
  (lambda (x)
    (define construct-name
      (lambda (template-identifier . args)
        (datum->syntax
          template-identifier
          (string->symbol
            (apply string-append
                   (map (lambda (x)
                          (if (string? x)
                              x
                              (symbol->string (syntax->datum x))))
                        args))))))
    (syntax-case x ()
      ((_ (name id1 ...))
       (andmap identifier? (syntax (name id1 ...)))
       (syntax (define-structure (name id1 ...) ())))
      ((_ (name id1 ...) ((id2 init) ...))
       (andmap identifier? (syntax (name id1 ... id2 ...)))
       (with-syntax
         ((constructor (construct-name (syntax name) "make-" (syntax name)))
          (predicate (construct-name (syntax name) (syntax name) "?"))
          ((access ...)
           (map (lambda (x) (construct-name x (syntax name) "-" x))
                (syntax (id1 ... id2 ...))))
          ((assign ...)
           (map (lambda (x)
                  (construct-name x "set-" (syntax name) "-" x "!"))
                (syntax (id1 ... id2 ...))))
          (structure-length
           (fx+ (length (syntax (id1 ... id2 ...))) 1))
          ((index ...)
           (let f ((i 1) (ids (syntax (id1 ... id2 ...))))
              (if (null? ids)
                  '()
                  (cons i (f (fx+ i 1) (cdr ids)))))))
         (syntax (begin
                   (define constructor
                     (lambda (id1 ...)
                       (let* ((id2 init) ...)
                         (vector 'name id1 ... id2 ...))))
                   (define predicate
                     (lambda (x)
                       (and (vector? x)
                            (#3%fx= (vector-length x) structure-length)
                            (eq? (vector-ref x 0) 'name))))
                   (define access
                     (lambda (x)
                       (vector-ref x index)))
                   ...
                   (define assign
                     (lambda (x update)
                       (vector-set! x index update)))
                   ...)))))))

(define-syntax critical-section
  (syntax-rules ()
    [(_ e1 e2 ...)
     (dynamic-wind
       disable-interrupts
       (lambda () e1 e2 ...)
       enable-interrupts)]))

(define-syntax with-interrupts-disabled
  (syntax-rules ()
    [(_ e1 e2 ...)
     (dynamic-wind
       disable-interrupts
       (lambda () e1 e2 ...)
       enable-interrupts)]))

(when-feature pthreads
(define-syntax with-mutex
  (syntax-rules ()
    ((_ m-expr e1 e2 ...)
     (let ([m m-expr])
       (dynamic-wind
          (lambda () (mutex-acquire m))
          (lambda () e1 e2 ...)
          (lambda () (mutex-release m)))))))
)

(define-syntax fluid-let
  (lambda (x)
    (syntax-case x ()
      [(_ () e1 e2 ...) #'(let () e1 e2 ...)]
      [(_ ((x v) ...) e1 e2 ...)
       (andmap identifier? #'(x ...))
       (with-syntax ([(y ...) (generate-temporaries #'(x ...))]
                     [(t ...) (generate-temporaries #'(x ...))])
         #'(let ([y v] ...)
             (let ([swap (lambda ()
                           (let ([t x] ...)
                             (set! x y)
                             ...
                             (set! y t)
                             ...))])
               (dynamic-wind #t swap (lambda () e1 e2 ...) swap))))])))

;(define-syntax let-values
;  (syntax-rules ()
;    [(_ () f1 f2 ...) (let () f1 f2 ...)]
;    [(_ ([fmls1 expr1] [fmls2 expr2] ...) f1 f2 ...)
;     (letrec-syntax ([lvhelp
;                      (...
;                        (syntax-rules ()
;                          [(_ (x1 . fmls) (x ...) (t ...) e m b)
;                           (lvhelp fmls (x ... x1) (t ... tmp) e m b)]
;                          [(_ () (x ...) (t ...) e m b)
;                           (call-with-values
;                             (lambda () e)
;                             (lambda (t ...)
;                               (let-values m (let ((x t) ...) . b))))]
;                          [(_ xr (x ...) (t ...) e m b)
;                           (call-with-values
;                             (lambda () e)
;                             (lambda (t ... . tmpr)
;                               (let-values m (let ((x t) ... (xr tmpr)) . b))))]))])
;       (lvhelp fmls1 () () expr1 ([fmls2 expr2] ...) (f1 f2 ...)))]))
(define-syntax let-values
  (lambda (x)
    (define check-duplicates!
      (lambda (ids)
        (unless (null? ids)
          (let loop ([ids ids])
            (let ([id (car ids)] [ids (cdr ids)])
              (unless (null? ids)
                (when (memp (lambda (id1) (bound-identifier=? id1 id)) ids)
                  (syntax-violation 'let-values "duplicate bound identifier" x id))
                (loop ids)))))))
    (define flatten-fmls
      (lambda (infmls)
        (let f ([fmls infmls])
          (syntax-case fmls ()
            [() '()]
            [id (identifier? #'id) (list #'id)]
            [(id . fmls) (identifier? #'id) (cons #'id (f #'fmls))]
            [_ (syntax-error infmls "invalid let-values left-hand side")]))))
    (define reconstitute-formals
      (lambda (fmls tmps)
        (syntax-case fmls ()
          [() '()]
          [id (identifier? #'id) (car tmps)]
          [(id . fmls)
           (cons (car tmps) (reconstitute-formals #'fmls (cdr tmps)))])))
    (define make-temp
      (lambda (id)
        ; return like-named gensym to make debugging easier
        (datum->syntax #'*
          (gensym (symbol->string (syntax->datum id))))))
    (define domvlet
      (lambda (bindings ids tmps body)
        (if (null? bindings)
            (begin
              (check-duplicates! ids)
              `((,#'lambda ,ids ,@body) ,@tmps))
            (syntax-case (car bindings) ()
              [(*fmls expr)
               (with-syntax ([*ids (flatten-fmls #'*fmls)])
                 (with-syntax ([*tmps (map make-temp #'*ids)])
                   (with-syntax ([body (domvlet (cdr bindings) (append #'*ids ids)
                                         (append #'*tmps tmps) body)]
                                 [*tfmls (reconstitute-formals #'*fmls #'*tmps)])
                     #`(call-with-values
                         (lambda () expr)
                         #,(if (or (= (optimize-level) 3) (identifier? #'*tfmls))
                               #'(lambda *tfmls body)
                               #`(case-lambda
                                   [*tfmls body]
                                   [args #,($make-source-oops #'let-values
                                             "incorrect number of values from rhs"
                                             #'expr)]))))))]))))
    (syntax-case x ()
      [(_ ((formals expr) ...) form1 form2 ...)
       (domvlet #'((formals expr) ...) '() '() #'(form1 form2 ...))])))

(define-syntax let*-values
  (lambda (x)
    (define check-duplicates!
      (lambda (ids)
        (unless (null? ids)
          (let loop ([ids ids])
            (let ([id (car ids)] [ids (cdr ids)])
              (unless (null? ids)
                (when (memp (lambda (id1) (bound-identifier=? id1 id)) ids)
                  (syntax-violation 'let-values "duplicate bound identifier" x id))
                (loop ids)))))))
    (define check-formals
      (lambda (infmls)
        (check-duplicates!
          (let f ([fmls infmls])
            (syntax-case fmls ()
              [() '()]
              [id (identifier? #'id) (list #'id)]
              [(id . fmls) (identifier? #'id) (cons #'id (f #'fmls))]
              [_ (syntax-error infmls "invalid let*-values left-hand side")])))))
    (define domvlet*
      (lambda (binding body)
        (syntax-case binding ()
          [(*fmls expr)
           #`(call-with-values
               (lambda () expr)
               #,(if (or (= (optimize-level) 3) (identifier? #'*fmls))
                     #`(lambda *fmls #,body)
                     #`(case-lambda
                         [*fmls #,body]
                         [args #,($make-source-oops #'let*-values
                                   "incorrect number of values from rhs"
                                   #'expr)])))])))
    (syntax-case x ()
      [(_ ((formals expr) ...) form1 form2 ...)
       (begin
         (for-each check-formals #'(formals ...))
         (let f ([bindings #'((formals expr) ...)])
           (if (null? bindings)
               #'(let () form1 form2 ...)
               (domvlet* (car bindings) (f (cdr bindings))))))])))

(define-syntax define-values
  (lambda (x)
    (define flatten-formals
      (lambda (infmls)
        (let f ([fmls infmls] [seenfmls '()])
          (syntax-case fmls ()
            [() (reverse seenfmls)]
            [id
             (identifier? #'id)
             (if (memp (lambda (x) (bound-identifier=? x #'id)) seenfmls)
                 (syntax-error infmls "duplicate variable in define-values left-hand side")
                 (cons #'id (reverse seenfmls)))]
            [(id . fmls)
             (identifier? #'id)
             (if (memp (lambda (x) (bound-identifier=? x #'id)) seenfmls)
                 (syntax-error infmls "duplicate variable in define-values left-hand side")
                 (f #'fmls (cons #'id seenfmls)))]
            [_ (syntax-error infmls "invalid define-values left-hand side")]))))
    (syntax-case x ()
      [(_ () expr)
       (if (= (optimize-level) 3)
           #'(define unused (begin expr (void)))
           #`(define unused
               (call-with-values
                 (lambda () expr)
                 (case-lambda
                   [() (void)]
                   [args #,($make-source-oops #'define-values
                             "incorrect number of values from rhs"
                             #'expr)]))))]
      [(_ (x) expr)
       (identifier? #'x)
       (if (= (optimize-level) 3)
           #'(define x expr)
           #`(define x
               (call-with-values
                 (lambda () expr)
                 (case-lambda
                   [(x) x]
                 [args #,($make-source-oops #'define-values
                           "incorrect number of values from rhs"
                           #'expr)]))))]
      [(_ formals expr)
       (with-syntax ([(ffml ...) (flatten-formals #'formals)])
         (with-syntax ([(i ...) (enumerate #'(ffml ...))])
           #`(begin
               (define t
                 (call-with-values
                   (lambda () expr)
                   (rec define-values-consumer
                     #,(if (or (= (optimize-level) 3) (identifier? #'formals))
                           #'(lambda formals (immutable-vector ffml ...))
                           #`(case-lambda
                               [formals (immutable-vector ffml ...)]
                               [args #,($make-source-oops #'define-values
                                         "incorrect number of values from rhs"
                                         #'expr)])))))
               (define ffml (vector-ref t i))
               ...)))])))

(define-syntax parameterize
  (lambda (x)
    (syntax-case x ()
      [(_ () e1 e2 ...) #'(let () e1 e2 ...)]
      [(_ ((x v) ...) e1 e2 ...)
       (with-syntax ([(p ...) (generate-temporaries #'(x ...))]
                     [(y ...) (generate-temporaries #'(x ...))]
                     [(t ...) (generate-temporaries #'(x ...))])
         #'(let ([p x] ... [y v] ...)
             (let ([swap (lambda ()
                           (let ([t (p)] ...)
                             (p y) ...
                             (set! y t) ...))])
               (dynamic-wind #t swap (lambda () e1 e2 ...) swap))))])))

(define-syntax with-continuation-mark
  (lambda (x)
    (syntax-case x ()
      [(_ key val body)
       #'(let ([k key]
               [v val])
           ($call-consuming-continuation-attachment
            '()
            (lambda (marks)
              ($call-setting-continuation-attachment
               ($update-mark marks k v)
               (lambda ()
                 body)))))])))

(define-syntax rec
  (lambda (x)
    (syntax-case x ()
      ((_ x v)
       (identifier? (syntax x))
       (syntax (letrec ((x v)) x))))))

(define-syntax record-case
  (let ()
    (define build-rc-body
      (lambda (p body)
        (syntax-case p ()
          ((id . p)
           (with-syntax ((body (build-rc-body (syntax p) body)))
             (syntax (let ((rec (cdr rec)))
                       (let ((id (car rec)))
                         body)))))
          (() (with-syntax ((body body))
                (syntax (begin . body))))
          (id
           (with-syntax ((body body))
             (syntax (let ((id (cdr rec))) . body)))))))

    (define-syntax build-clause
      (lambda (x)
        (syntax-case x ()
          ((_ tag keys idspec body rest)
           (syntax
             (with-syntax ((body (build-rc-body (syntax idspec) (syntax body))))
               (syntax (if (memv tag 'keys)
                           body
                           . rest))))))))
    (lambda (x)
      (syntax-case x ()
        ((_ e m1 m2 ...)
         (with-syntax
           ((body (let f ((clause (syntax m1)) (clauses (syntax (m2 ...))))
                    (if (null? clauses)
                        (syntax-case clause (else)
                          ((else e1 e2 ...) (syntax (begin e1 e2 ...)))
                          (((key ...) idspec e1 e2 ...)
                           (build-clause tag (key ...) idspec (e1 e2 ...) ()))
                          ((key idspec e1 e2 ...)
                           (build-clause tag (key) idspec (e1 e2 ...) ()))
                          (_ (syntax-error x)))
                        (with-syntax ((rest (f (car clauses) (cdr clauses))))
                          (syntax-case clause (else)
                            (((key ...) idspec e1 e2 ...)
                             (build-clause tag (key ...) idspec (e1 e2 ...)
                               (rest)))
                            ((key idspec e1 e2 ...)
                             (build-clause tag (key) idspec (e1 e2 ...) (rest)))
                            (_ (syntax-error x))))))))
           (syntax (let ((rec e))
                     (let ((tag (car rec)))
                       body)))))))))

;;; record-case aux keywords
#;(define-syntax else ; defined above for cond
  (lambda (x)
    (syntax-error x "misplaced aux keyword")))

(define-syntax time
   (syntax-rules ()
      ((_ e)
       ($as-time-goes-by 'e (lambda () e)))))

(define-syntax trace
  (lambda (x)
   (syntax-case x ()
      ((_ x ...)
       (andmap identifier? (syntax (x ...)))
       (syntax (#%$trace 'x ...))))))

(define-syntax trace-define
  (lambda (x)
    (syntax-case x ()
      ((_ name val)
       (identifier? (syntax name))
       (syntax (define name (#%$trace-closure 'name val))))
      ((_ (name . idspec) e1 e2 ...)
       (identifier? (syntax name))
       (syntax (define name (trace-lambda name idspec e1 e2 ...)))))))

(define-syntax trace-define-syntax
  (lambda (x)
    (syntax-case x ()
      ((_ (name id) e1 e2 ...)
       (and (identifier? (syntax name)) (identifier? (syntax id)))
       (syntax (trace-define-syntax name (lambda (id) e1 e2 ...))))
      ((_ name expr)
       (identifier? (syntax name))
       #'(define-syntax name
           (let ((tr expr))
             (if (or (procedure? tr)
                     (and (pair? tr)
                          (eq? (car tr) 'macro!)
                          (procedure? (cdr tr))))
                 (let ((proc (if (pair? tr) (cdr tr) tr)))
                   (let ((tproc (lambda (x)
                                  (lambda (r)
                                    (trace-let name ((y (syntax->datum x)))
                                      (let ([z (let ([z (proc x)])
                                                 (if (procedure? z)
                                                     (z r)
                                                     z))])
                                        (set! x z)
                                        (syntax->datum z)))
                                    x))))
                     (if (pair? tr) (cons (car tr) tproc) tproc)))
                 tr)))))))

(define-syntax trace-lambda
   (syntax-rules ()
      ((trace-lambda name idspec e1 e2 ...)
       (#%$trace-closure 'name (lambda idspec e1 e2 ...)))))

(define-syntax trace-case-lambda
   (syntax-rules ()
      ((trace-case-lambda name clause ...)
       (#%$trace-closure 'name (case-lambda clause ...)))))

(define-syntax trace-let
   (syntax-rules ()
      ((trace-let name ((x v) ...) e1 e2 ...)
       ((rec name (trace-lambda name (x ...) e1 e2 ...)) v ...))))

(define-syntax trace-do
   (lambda (orig-x)
      (syntax-case orig-x ()
         ((_ ((var init . step) ...) (e0 e1 ...) c ...)
          (with-syntax (((step ...)
                         (map (lambda (v s)
                                 (syntax-case s ()
                                    (() v)
                                    ((e) (syntax e))
                                    (_ (syntax-error orig-x))))
                              (syntax (var ...))
                              (syntax (step ...)))))
             (syntax-case (syntax (e1 ...)) ()
                (() (syntax (trace-let do ((var init) ...)
                               (if (not e0)
                                   (begin c ... (do step ...))))))
                ((e1 e2 ...)
                 (syntax (trace-let do ((var init) ...)
                            (if e0
                                (begin e1 e2 ...)
                                (begin c ... (do step ...))))))))))))

(define-syntax unless
   (syntax-rules ()
      ((_ e0 e1 e2 ...)
       (if (not e0) (begin e1 e2 ...)))))

(define-syntax untrace
  (lambda (x)
    (syntax-case x ()
      ((_ x ...)
       (andmap identifier? (syntax (x ...)))
       (syntax (#%$untrace 'x ...))))))

(define-syntax when
   (syntax-rules ()
      ((_ e0 e1 e2 ...)
       (if e0 (begin e1 e2 ...)))))

(define-syntax extend-syntax
  (lambda (x)
   (define id
      (lambda (name access control)
         (list name access control)))
   (define id-name car)
   (define id-access cadr)
   (define id-control caddr)

   (define loop
      (lambda ()
         (box '())))
   (define loop-ids unbox)
   (define loop-ids! set-box!)

   (define c...rs
      '((car caar . cdar)
        (cdr cadr . cddr)
        (caar caaar . cdaar)
        (cadr caadr . cdadr)
        (cdar cadar . cddar)
        (cddr caddr . cdddr)
        (caaar caaaar . cdaaar)
        (caadr caaadr . cdaadr)
        (cadar caadar . cdadar)
        (caddr caaddr . cdaddr)
        (cdaar cadaar . cddaar)
        (cdadr cadadr . cddadr)
        (cddar caddar . cdddar)
        (cdddr cadddr . cddddr)))

   (define add-car
      (lambda (access)
         (let ((x (and (pair? access) (assq (car access) c...rs))))
            (if x
                `(,(cadr x) ,@(cdr access))
                `(car ,access)))))

   (define add-cdr
      (lambda (access)
         (let ((x (and (pair? access) (assoc (car access) c...rs))))
            (if x
                `(,(cddr x) ,@(cdr access))
                `(cdr ,access)))))

   (define checkpat
      (lambda (keys pat exp)
         (let ((vars (let f ((x pat) (vars '()))
                        (cond
                           ((pair? x)
                            (if (and (pair? (cdr x))
                                     (eq? (cadr x) '...)
                                     (null? (cddr x)))
                                (f (car x) vars)
                                (f (car x) (f (cdr x) vars))))
                           ((symbol? x)
                            (cond
                               ((memq x keys) vars)
                               ((or (eq? x 'with) (eq? x '...))
                                ($oops 'extend-syntax
                                       "invalid context for ~s in ~s"
                                       x exp))
                               (else (cons x vars))))
                           (else vars)))))
            (let check-dups ([vars vars])
              (unless (null? vars)
                (when (memq (car vars) (cdr vars))
                  ($oops 'extend-syntax
                         "duplicate pattern variable name ~s in ~s"
                         (car vars)
                         exp))
                (check-dups (cdr vars)))))))

   (define parse
      (lambda (keys pat acc cntl ids)
         (cond
            ((symbol? pat)
             (if (memq pat keys)
                 ids
                 (cons (id pat acc cntl) ids)))
            ((pair? pat)
             (cons (id pat acc cntl)
                   (if (equal? (cdr pat) '(...))
                       (let ((x (gensym)))
                          (parse keys (car pat) x (id x acc cntl) ids))
                       (parse keys (car pat) (add-car acc) cntl
                          (parse keys (cdr pat) (add-cdr acc) cntl ids)))))
            (else ids))))

   (define pattern-variable?
      (lambda (sym ids)
         (memq sym (map id-name ids))))

   (define gen
      (lambda (keys exp ids loops qqlev)
         (cond
            ((lookup exp ids) =>
             (lambda (id)
                (add-control! (id-control id) loops)
                (list 'unquote (id-access id))))
            ((memq exp '(quasiquote unquote unquote-splicing))
             (list 'unquote (list 'quote exp)))
            ((not (pair? exp)) exp)
            (else
             (cond
                ((and ($syntax-match? '(quasiquote *) exp)
                      (not (pattern-variable? 'quasiquote ids)))
                 (list 'unquote
                       (list 'list
                             ''quasiquote
                             (make-quasi
                                (gen keys (cadr exp) ids loops
                                     (if (= qqlev 0) 0 (+ qqlev 1)))))))
                ((and ($syntax-match? '(* *) exp)
                      (memq (car exp) '(unquote unquote-splicing))
                      (not (pattern-variable? (car exp) ids)))
                 (if (= qqlev 1)
                     (list (car exp) (gen-quotes keys (cadr exp) ids loops))
                     (list 'unquote
                           `(list ',(car exp)
                                    ,(make-quasi
                                        (gen keys (cadr exp) ids loops
                                             (- qqlev 1)))))))
                ((and (eq? (car exp) 'with)
                      (not (pattern-variable? 'with ids)))
                 (unless ($syntax-match? '(with ((* *) ...) *) exp)
                    ($oops 'extend-syntax "invalid 'with' form ~s" exp))
                 (checkpat keys (map car (cadr exp)) exp)
                 (list 'unquote
                    (gen-with
                       keys
                       (map car (cadr exp))
                       (map cadr (cadr exp))
                       (caddr exp)
                       ids
                       loops)))
                ((and (pair? (cdr exp)) (eq? (cadr exp) '...))
                 (let ((x (loop)))
                    (gen-cons (list 'unquote-splicing
                                    (make-loop x (gen keys (car exp) ids
                                                      (cons x loops) qqlev)))
                              (gen keys (cddr exp) ids loops qqlev))))
                (else
                 (gen-cons (gen keys (car exp) ids loops qqlev)
                           (gen keys (cdr exp) ids loops qqlev))))))))

   (define gen-cons
      (lambda (head tail)
         (if (null? tail)
             (if ($syntax-match? '(unquote-splicing *) head)
                 (list 'unquote (cadr head))
                 (cons head tail))
             (if ($syntax-match? '(unquote *) tail)
                 (list head (list 'unquote-splicing (cadr tail)))
                 (cons head tail)))))

   (define gen-with
      (lambda (keys pats exps body ids loops)
         (let ((temps (map (lambda (x) (gensym)) pats)))
            `(let (,@(map (lambda (t e) `(,t ,(gen-quotes keys e ids loops)))
                          temps
                          exps))
                ,@(let f ((ps pats) (ts temps))
                     (if (null? ps)
                         (let f ((pats pats) (temps temps) (ids ids))
                            (if (null? pats)
                                `(,(make-quasi (gen keys body ids loops 0)))
                                (f (cdr pats)
                                   (cdr temps)
                                   (parse '() (car pats) (car temps) '() ids))))
                         (let ((m (match-pattern '() (car ps))))
                            (if (eq? m '*)
                                (f (cdr ps) (cdr ts))
                                `((unless (#%$syntax-match? ',m ,(car ts))
                                     (assertion-violationf
                                        ',(car keys)
                                        "~s does not fit 'with' pattern ~s"
                                        ,(car ts)
                                        ',(car ps)))
                                  ,@(f (cdr ps) (cdr ts)))))))))))

   (define gen-quotes
      (lambda (keys exp ids loops)
         (cond
            (($syntax-match? '(quote *) exp)
             (make-quasi (gen keys (cadr exp) ids loops 0)))
            (($syntax-match? '(quasiquote *) exp)
             (make-quasi (gen keys (cadr exp) ids loops 1)))
            ((pair? exp)
             (let f ((exp exp))
                (if (pair? exp)
                    (cons (gen-quotes keys (car exp) ids loops)
                          (f (cdr exp)))
                    (gen-quotes keys exp ids loops))))
            (else exp))))

   (define lookup
      (lambda (exp ids)
         (let loop ((ls ids))
            (cond
               ((null? ls) #f)
               ((equal? (id-name (car ls)) exp) (car ls))
               ((subexp? (id-name (car ls)) exp) #f)
               (else (loop (cdr ls)))))))

   (define subexp?
      (lambda (exp1 exp2)
         (and (symbol? exp1)
              (let f ((exp2 exp2))
                 (or (eq? exp1 exp2)
                     (and (pair? exp2)
                          (or (f (car exp2))
                              (f (cdr exp2)))))))))

   (define add-control!
      (lambda (id loops)
         (unless (null? id)
            (when (null? loops)
               ($oops 'extend-syntax "missing ellipsis in expansion"))
            (let ((x (loop-ids (car loops))))
               (unless (memq id x)
                  (loop-ids! (car loops) (cons id x))))
            (add-control! (id-control id) (cdr loops)))))

   (define make-loop
      (lambda (loop body)
         (let ((ids (loop-ids loop)))
            (when (null? ids)
               ($oops 'extend-syntax "extra ellipsis in expansion"))
            (cond
               ((equal? body (list 'unquote (id-name (car ids))))
                (id-access (car ids)))
               ((and (null? (cdr ids))
                     ($syntax-match? '(unquote (* *)) body)
                     (eq? (cadadr body) (id-name (car ids))))
                `(map ,(caadr body) ,(id-access (car ids))))
               (else
                `(map (lambda ,(map id-name ids) ,(make-quasi body))
                        ,@(map id-access ids)))))))

   (define match-pattern
      (lambda (keys pat)
         (cond
            ((symbol? pat)
             (if (memq pat keys)
                 (if (memq pat '(* \\ ...))
                     `(\\ ,pat)
                     pat)
                 '*))
            ((pair? pat)
             (if (and (pair? (cdr pat))
                      (eq? (cadr pat) '...)
                      (null? (cddr pat)))
                 `(,(match-pattern keys (car pat)) ...)
                 (cons (match-pattern keys (car pat))
                       (match-pattern keys (cdr pat)))))
            (else pat))))

   (define make-quasi
      (lambda (exp)
         (if (and (pair? exp) (eq? (car exp) 'unquote))
             (cadr exp)
             (list 'quasiquote exp))))

   (define match-check
      (lambda (keys pat x)
         `(#%$syntax-match? ',(match-pattern keys pat) ,x)))

   (define make-clause
      (lambda (keys cl x)
         (cond
            (($syntax-match? '(* * *) cl)
             (let ((pat (car cl)) (fender (cadr cl)) (exp (caddr cl)))
                (checkpat keys pat pat)
                (let ((ids (parse keys pat x '() '())))
                   `((and ,(match-check keys pat x)
                          ,(gen-quotes keys fender ids '()))
                     ,(make-quasi (gen keys exp ids '() 0))))))
            (($syntax-match? '(* *) cl)
             (let ((pat (car cl)) (exp (cadr cl)))
                (checkpat keys pat pat)
                (let ((ids (parse keys pat x '() '())))
                   `(,(match-check keys pat x)
                     ,(make-quasi (gen keys exp ids '() 0))))))
            (else
             ($oops 'extend-syntax "invalid clause ~s" cl)))))

   (define make-syntax
      (let ((x (gensym "x")))
         (lambda (keys clauses)
            (when (memq '... keys)
               ($oops 'extend-syntax
                       "invalid keyword ... in keyword list ~s"
                       keys))
            `(lambda (,x)
               (cond
                 ,@(map (lambda (cl) (make-clause keys cl x)) clauses)
                 (else (assertion-violationf ',(car keys) "invalid syntax ~s" ,x)))))))

   (syntax-case x ()
     ((k (key1 key2 ...) clause ...)
      (andmap identifier? (syntax (key1 key2 ...)))
      (with-syntax ((proc (datum->syntax (syntax k)
                            (make-syntax
                              (syntax->datum (syntax (key1 key2 ...)))
                              (syntax->datum (syntax (clause ...)))))))
        (syntax
          (define-syntax key1
            (lambda (x)
              (syntax-case x ()
                ((k1 . r)
                 (datum->syntax (syntax k1)
                   (proc (syntax->datum x)))))))))))))

(define $syntax-match?
  (rec $syntax-match?
    (lambda (pat exp)
      (cond
        [(not (pair? pat)) (or (eq? exp pat) (eq? pat '*))]
        [(eq? (car pat) '*)
         (if (and (pair? (cdr pat)) (eq? (cadr pat) '...))
             (let f ([lst exp])
               (or (and (pair? lst) (f (cdr lst))) (null? lst)))
             (and (pair? exp) ($syntax-match? (cdr pat) (cdr exp))))]
        [(and (eq? (car pat) '|\|) (pair? (cdr pat)))
         (eq? exp (cadr pat))]
        [(and (pair? (cdr pat)) (eq? (cadr pat) '...))
         (let ([pat (car pat)])
           (let f ([lst exp])
             (or (and (pair? lst)
                      ($syntax-match? pat (car lst))
                      (f (cdr lst)))
                 (null? lst))))]
        [else
         (and (pair? exp)
              ($syntax-match? (car pat) (car exp))
              ($syntax-match? (cdr pat) (cdr exp)))]))))

(define $fp-filter-type
  (lambda (type void-okay?)
   ; not the same as cmacros filter-type, which allows things like bigit
    (case type
      [(scheme-object double-float single-float
        integer-8 unsigned-8 integer-16 unsigned-16 integer-24 unsigned-24
        integer-32 unsigned-32 integer-40 unsigned-40 integer-48 unsigned-48
        integer-56 unsigned-56 integer-64 unsigned-64
        boolean stdbool fixnum char wchar u8* u16* u32* utf-8 utf-16le utf-16be utf-16
        utf-32le utf-32be utf-32) type]
      [(void) (and void-okay? type)]
      [(ptr) 'scheme-object]
      [(iptr)
       (constant-case ptr-bits
         [(32) 'integer-32]
         [(64) 'integer-64])]
      [(uptr)
       (constant-case ptr-bits
         [(32) 'unsigned-32]
         [(64) 'unsigned-64])]
      [(void*)
       (constant-case ptr-bits
         [(32) 'unsigned-32]
         [(64) 'unsigned-64])]
      [(int)
       (constant-case int-bits
         [(32) 'integer-32]
         [(64) 'integer-64])]
      [(unsigned unsigned-int)
       (constant-case int-bits
         [(32) 'unsigned-32]
         [(64) 'unsigned-64])]
      [(short)
       (constant-case short-bits
         [(16) 'integer-16]
         [(32) 'integer-32])]
      [(unsigned-short)
       (constant-case short-bits
         [(16) 'unsigned-16]
         [(32) 'unsigned-32])]
      [(long)
       (constant-case long-bits
         [(32) 'integer-32]
         [(64) 'integer-64])]
      [(unsigned-long)
       (constant-case long-bits
         [(32) 'unsigned-32]
         [(64) 'unsigned-64])]
      [(long-long)
       (constant-case long-long-bits
         [(64) 'integer-64])]
      [(unsigned-long-long)
       (constant-case long-long-bits
         [(64) 'unsigned-64])]
      [(size_t)
       (constant-case size_t-bits
         [(32) 'unsigned-32]
         [(64) 'unsigned-64])]
      [(ssize_t)
       (constant-case size_t-bits
         [(32) 'integer-32]
         [(64) 'integer-64])]
      [(ptrdiff_t)
       (constant-case ptrdiff_t-bits
         [(32) 'integer-32]
         [(64) 'integer-64])]
      [(wchar_t) 'wchar]
      [(float) 'single-float]
      [(double) 'double-float]
      [(string) 'utf-8]
      [(wstring)
       (constant-case wchar-bits
         [(16)
          (constant-case native-endianness
            [(little) 'utf-16le]
            [(big) 'utf-16be]
            [(unknown) 'utf-16])]
         [(32)
          (constant-case native-endianness
            [(little) 'utf-32le]
            [(big) 'utf-32be]
            [(unknown) 'utf-32])])]
      [else
       (and (or ($ftd? type) ($ftd-as-box? type))
            type)])))

(define $fp-type->pred
  (lambda (type)
    (cond
      [(assq type foreign-datatypes) =>
       (lambda (a)
         (apply
           (lambda (spec bytes pred) pred)
           a))]
      [else
       (case type
         [(boolean stdbool void) '(lambda (id) #t)]
         [(char) '(lambda (id) (and (char? id) (fx<= (char->integer id) #xff)))]
         [(wchar)
          (constant-case wchar-bits
            [(16) '(lambda (id) (and (char? id) (fx<= (char->integer id) #xffff)))]
            [(32) '(lambda (id) (char? id))])]
         [(utf-8 utf-16le utf-16be utf-16 utf32-le utf32-be utf-32)
          '(lambda (id) (or (not id) (string? id)))]
         [(u8* u16* u32*)
          '(lambda (id) (or (not id) (bytevector? id)))]
         [(fixnum) '(lambda (id) (fixnum? id))]
         [else ($oops '$fp-type->pred "unrecognized type ~s" type)])])))

(define $filter-conv
  (lambda (who conv* num-args)
    (define squawk
      (lambda (x)
        (syntax-error x (format "invalid ~s convention" who))))
    (define check-arg-count
      (lambda (n orig-c)
        (unless (<= n num-args)
          (syntax-error orig-c (format "invalid ~s convention with ~a arguments" who num-args)))))
    (let loop ([conv* conv*] [selected #f] [accum '()] [keep-accum '()])
      (cond
        [(null? conv*) (datum->syntax #'filter-conv keep-accum)]
        [else
         (let* ([orig-c (car conv*)]
                [c (syntax->datum orig-c)])
           (let-values ([(c select?)
                         (cond
                           [(not c) (values #f #f)]
                           [(eq? c '__collect_safe) (values 'adjust-active #f)]
                           [(eq? c '__varargs)
                            (check-arg-count 1 orig-c)
                            (values (cons 'varargs 1) #f)]
                           [(and (pair? c) (eq? (car c) '__varargs_after)
                                 (pair? (cdr c)) (null? (cddr c))
                                 (let ([i (cadr c)])
                                   (and (integer? i)
                                        (exact? i)
                                        (positive? i))))
                            (check-arg-count (cadr c) orig-c)
                            (values (cons 'varargs (cadr c)) #f)]
                           [else
                            (values
                             (case ($target-machine)
                               [(i3nt ti3nt)
                                (case c
                                  [(__stdcall) 'i3nt-stdcall]
                                  [(__cdecl) #f]
                                  [(__com) 'i3nt-com]
                                  [else (squawk orig-c)])]
                               [(ppcnt)
                                (case c
                                  [(__stdcall __cdecl) #f]
                                  [else (squawk orig-c)])]
                               [else (squawk orig-c)])
                             #t)])])
             (when (or (member c accum)
                       (and (pair? c) (ormap pair? accum)))
               (syntax-error orig-c (format "redundant ~s convention" who)))
             (when (and select? selected)
               (syntax-error orig-c (format "conflicting ~s convention" who)))
             (loop (cdr conv*) (if select? c selected) (cons c accum)
                   (if c
                       (cons c keep-accum)
                       keep-accum))))]))))

(define $make-foreign-procedure
  (lambda (who conv* foreign-name ?foreign-addr type* result-type)
    (let ([unsafe? (= (optimize-level) 3)])
      (define (check-strings-allowed)
        (when (memq 'adjust-active (syntax->datum conv*))
          ($oops who "string argument not allowed with __collect_safe procedure")))
      (define (check-floats-allowed pos)
        (let ([va-n (ormap (lambda (conv) (and (pair? conv) (eq? (car conv) 'varargs) (cdr conv)))
                           (syntax->datum conv*))])
          (when (and va-n (>= pos va-n))
            ($oops who "single-float varargs argument not allowed"))))
      (with-syntax ([conv* conv*]
                    [foreign-name foreign-name]
                    [?foreign-addr ?foreign-addr]
                    [(t ...) (generate-temporaries type*)])
        (with-syntax ([(((check ...) (actual ...) (arg ...)) ...)
                       (map
                         (lambda (type x pos)
                           (with-syntax ([x x])
                             (or (case type
                                   [(boolean)
                                    #`(()
                                       ((if x 1 0))
                                       (#,(constant-case int-bits
                                            [(32) #'integer-32]
                                            [(64) #'integer-64])))]
                                   [(stdbool)
                                    #`(()
                                       ((if x 1 0))
                                       (#,(constant-case stdbool-bits
                                            [(8) #'integer-8])))]
                                   [(char)
                                    #`(()
                                       (#,(if unsafe?
                                              #'(char->integer x)
                                              #'(or (and (char? x)
                                                         (let ([x (char->integer x)])
                                                           (and (fx<= x #xff) x)))
                                                    (err ($moi) x))))
                                       (unsigned-8))]
                                   [(wchar)
                                    (constant-case wchar-bits
                                      [(16) #`(()
                                               (#,(if unsafe?
                                                      #'(char->integer x)
                                                      #'(or (and (char? x)
                                                                 (let ([x (char->integer x)])
                                                                   (and (fx< x #xffff) x)))
                                                            (err ($moi) x))))
                                               (unsigned-16))]
                                      [(32) #`(()
                                               (#,(if unsafe?
                                                      #'(char->integer x)
                                                      #'(if (char? x)
                                                            (char->integer x)
                                                            (err ($moi) x))))
                                               (unsigned-32))])]
                                   [(utf-8)
                                    (check-strings-allowed)
                                    #`(()
                                       ((if (eq? x #f)
                                            x
                                            #,(if unsafe?
                                                  #'($fp-string->utf8 x)
                                                  #'(if (string? x)
                                                        ($fp-string->utf8 x)
                                                        (err ($moi) x)))))
                                       (u8*))]
                                   [(utf-16le)
                                    (check-strings-allowed)
                                    #`(()
                                       ((if (eq? x #f)
                                            x
                                            #,(if unsafe?
                                                  #'($fp-string->utf16 x 'little)
                                                  #'(if (string? x)
                                                        ($fp-string->utf16 x 'little)
                                                        (err ($moi) x)))))
                                       (u16*))]
                                   [(utf-16be)
                                    (check-strings-allowed)
                                    #`(()
                                       ((if (eq? x #f)
                                            x
                                            #,(if unsafe?
                                                  #'($fp-string->utf16 x 'big)
                                                  #'(if (string? x)
                                                        ($fp-string->utf16 x 'big)
                                                        (err ($moi) x)))))
                                       (u16*))]
                                   [(utf-16)
                                    (check-strings-allowed)
                                    #`(()
                                       ((if (eq? x #f)
                                            x
                                            #,(if unsafe?
                                                  #'($fp-string->utf16 x (native-endianness))
                                                  #'(if (string? x)
                                                        ($fp-string->utf16 x (native-endianness))
                                                        (err ($moi) x)))))
                                       (u16*))]
                                   [(utf-32le)
                                    (check-strings-allowed)
                                    #`(()
                                       ((if (eq? x #f)
                                            x
                                            #,(if unsafe?
                                                  #'($fp-string->utf32 x 'little)
                                                  #'(if (string? x)
                                                        ($fp-string->utf32 x 'little)
                                                        (err ($moi) x)))))
                                       (u32*))]
                                   [(utf-32be)
                                    (check-strings-allowed)
                                    #`(()
                                       ((if (eq? x #f)
                                            x
                                            #,(if unsafe?
                                                  #'($fp-string->utf32 x 'big)
                                                  #'(if (string? x)
                                                        ($fp-string->utf32 x 'big)
                                                        (err ($moi) x)))))
                                       (u32*))]
                                   [(utf-32)
                                    (check-strings-allowed)
                                    #`(()
                                       ((if (eq? x #f)
                                            x
                                            #,(if unsafe?
                                                  #'($fp-string->utf32 x (native-endianness))
                                                  #'(if (string? x)
                                                        ($fp-string->utf32 x (native-endianness))
                                                        (err ($moi) x)))))
                                       (u32*))]
                                   [(single-float)
                                    (check-floats-allowed pos)
                                    #f]
                                   [else #f])
                                 (if (or ($ftd? type) ($ftd-as-box? type))
                                     (let ([ftd (if ($ftd? type) type (unbox type))])
                                       #`(#,(if unsafe? #'() #`((unless (record? x '#,ftd) (err ($moi) x))))
                                          (x)
                                          (#,type)))
                                     (with-syntax ([pred (datum->syntax #'foreign-procedure ($fp-type->pred type))]
                                                   [type (datum->syntax #'foreign-procedure type)])
                                       #`(#,(if unsafe? #'() #'((unless (pred x) (err ($moi) x))))
                                          (x)
                                          (type)))))))
                         type* #'(t ...) (enumerate type*))]
                      [(result-filter result)
                       (case result-type
                         [(boolean) #`((lambda (x) (not (eq? x 0)))
                                       #,(constant-case int-bits
                                           [(32) #'integer-32]
                                           [(64) #'integer-64]))]
                         [(stdbool) #`((lambda (x) (not (eq? x 0)))
                                       #,(constant-case stdbool-bits
                                           [(8) #'integer-8]))]
                         [(char) #'((lambda (x) (#3%integer->char (#3%fxlogand x #xff)))
                                    unsigned-8)]
                         [(wchar) #`(integer->char
                                      #,(constant-case wchar-bits
                                          [(16) #'unsigned-16]
                                          [(32) #'unsigned-32]))]
                         [(utf-8) #'((lambda (x) (and x (utf8->string x))) u8*)]
                         [(utf-16le) #'((lambda (x) (and x (utf16->string x 'little #t))) u16*)]
                         [(utf-16be) #'((lambda (x) (and x (utf16->string x 'big #t))) u16*)]
                         [(utf-16) #'((lambda (x) (and x (utf16->string x (native-endianness) #t))) u16*)]
                         [(utf-32le) #'((lambda (x) (and x (utf32->string x 'little #t))) u32*)]
                         [(utf-32be) #'((lambda (x) (and x (utf32->string x 'big #t))) u32*)]
                         [(utf-32) #'((lambda (x) (and x (utf32->string x (native-endianness) #t))) u32*)]
                         [(integer-24) #`((lambda (x) (#,(constant-case ptr-bits [(32) #'mod0] [(64) #'fxmod0]) x #x1000000)) integer-32)]
                         [(unsigned-24) #`((lambda (x) (#,(constant-case ptr-bits [(32) #'mod] [(64) #'fxmod]) x #x1000000)) unsigned-32)]
                         [(integer-40) #`((lambda (x) (mod0 x #x10000000000)) integer-64)]
                         [(unsigned-40) #`((lambda (x) (mod x #x10000000000)) unsigned-64)]
                         [(integer-48) #`((lambda (x) (mod0 x #x1000000000000)) integer-64)]
                         [(unsigned-48) #`((lambda (x) (mod x #x1000000000000)) unsigned-64)]
                         [(integer-56) #`((lambda (x) (mod0 x #x100000000000000)) integer-64)]
                         [(unsigned-56) #`((lambda (x) (mod x #x100000000000000)) unsigned-64)]
                         [else
                          (cond
                            [($ftd-as-box? result-type)
                             ;; Return void, since an extra first argument receives the result,
                             ;; but tell `$foreign-procedure` that the result is actually an & form
                             #`((lambda (r) (void)) #,(datum->syntax #'foreign-procedure result-type))]
                            [else
                             #`(begin #,(datum->syntax #'foreign-procedure result-type))])])]
                      [([extra ...] [extra-arg ...] [extra-check ...])
                       ;; When the result type is `(& <ftype>)`, the `$foreign-procedure` result
                       ;; expects an extra argument as a `(* <ftype>)` that it uses to store the
                       ;; foreign-procedure result, and it returns void. The extra argument is made
                       ;; explicit for `$foreign-procedure`, and the return type is preserved as-is
                       ;; to let `$foreign-procedure` know that it needs to fill the first argument.
                       (cond
                         [($ftd-as-box? result-type)
                          #`([&-result]
                             [#,(unbox result-type)]
                             #,(if unsafe?
                                   #`[]
                                   #`[(unless (record? &-result '#,(unbox result-type)) (err ($moi) &-result))]))]
                         [else #'([] [] [])])])
          #`(let ([p ($foreign-procedure conv* foreign-name ?foreign-addr (extra-arg ... arg ... ...) result)]
                  #,@(if unsafe?
                         #'()
                         #'([err (lambda (who x)
                                   ($oops (or who foreign-name)
                                     "invalid foreign-procedure argument ~s"
                                     x))])))
              (lambda (extra ... t ...) extra-check ... check ... ... (result-filter (p extra ... actual ... ...)))))))))

(define-syntax foreign-procedure
  (lambda (x)
    (define filter-type
      (lambda (r x result?)
        (let ([what (if result? 'result 'argument)])
          (or ($fp-filter-type ($expand-fp-ftype 'foreign-procedure what r x) result?)
              (syntax-error x (format "invalid foreign-procedure ~s type specifier" what))))))
    (syntax-case x ()
      [(_ c ... ?name (arg ...) result)
       (lambda (r)
         ($make-foreign-procedure 'foreign-procedure
           ($filter-conv 'foreign-procedure #'(c ...) (length #'(arg ...)))
           (let ([x (datum ?name)]) (and (string? x) x))
           #'($foreign-entry ?name)
           (map (lambda (x) (filter-type r x #f)) #'(arg ...))
           (filter-type r #'result #t)))])))

(define $make-foreign-callable
  (lambda (who conv* ?proc type* result-type)
    (for-each (lambda (c)
                (when (eq? (syntax->datum c) 'i3nt-com)
                  ($oops who "unsupported convention ~s" c)))
              (syntax->list conv*))
    (let ([unsafe? (= (optimize-level) 3)])
      (define (check-strings-allowed)
        (when (memq 'adjust-active (syntax->datum conv*))
          ($oops who "string result not allowed with __collect_safe callable")))
      (define (check-floats-allowed pos)
        (let ([va-n (ormap (lambda (conv) (and (pair? conv) (eq? (car conv) 'varargs) (cdr conv)))
                           (syntax->datum conv*))])
          (when (and va-n (>= pos va-n))
            ($oops who "single-float argument not allowed for __varargs procedure"))))
      (with-syntax ([conv* conv*] [?proc ?proc])
        (with-syntax ([((actual (t ...) (arg ...)) ...)
                       (map
                        (lambda (type pos)
                           (or (case type
                                 [(boolean)
                                  (with-syntax ([(x) (generate-temporaries #'(*))])
                                    #`((not (eq? x 0))
                                       (x)
                                       (#,(constant-case int-bits
                                            [(32) #'integer-32]
                                            [(64) #'integer-64]))))]
                                 [(stdbool)
                                  (with-syntax ([(x) (generate-temporaries #'(*))])
                                    #`((not (eq? x 0))
                                       (x)
                                       (#,(constant-case stdbool-bits
                                            [(8) #'integer-8]))))]
                                 [(char)
                                  (with-syntax ([(x) (generate-temporaries #'(*))])
                                    #`((#3%integer->char (#3%fxlogand x #xff))
                                       (x)
                                       (unsigned-8)))]
                                 [(wchar)
                                  (with-syntax ([(x) (generate-temporaries #'(*))])
                                    #`((integer->char x)
                                       (x)
                                       (#,(constant-case wchar-bits
                                            [(16) #'unsigned-16]
                                            [(32) #'unsigned-32]))))]
                                 [(utf-8)
                                  (with-syntax ([(x) (generate-temporaries #'(*))])
                                    #`((and x (utf8->string x))
                                       (x)
                                       (u8*)))]
                                 [(utf-16le)
                                  (with-syntax ([(x) (generate-temporaries #'(*))])
                                    #`((and x (utf16->string x 'little #t))
                                       (x)
                                       (u16*)))]
                                 [(utf-16be)
                                  (with-syntax ([(x) (generate-temporaries #'(*))])
                                    #`((and x (utf16->string x 'big #t))
                                       (x)
                                       (u16*)))]
                                 [(utf-16)
                                  (with-syntax ([(x) (generate-temporaries #'(*))])
                                    #`((and x (utf16->string x (native-endianness) #t))
                                       (x)
                                       (u16*)))]
                                 [(utf-32le)
                                  (with-syntax ([(x) (generate-temporaries #'(*))])
                                    #`((and x (utf32->string x 'little #t))
                                       (x)
                                       (u32*)))]
                                 [(utf-32be)
                                  (with-syntax ([(x) (generate-temporaries #'(*))])
                                    #`((and x (utf32->string x 'big #t))
                                       (x)
                                       (u32*)))]
                                 [(utf-32)
                                  (with-syntax ([(x) (generate-temporaries #'(*))])
                                    #`((and x (utf32->string x (native-endianness) #t))
                                       (x)
                                       (u32*)))]
                                 [(integer-24)
                                  (with-syntax ([(x) (generate-temporaries #'(*))])
                                    #`((mod0 x #x1000000)
                                       (x)
                                       (integer-32)))]
                                 [(unsigned-24)
                                  (with-syntax ([(x) (generate-temporaries #'(*))])
                                    #`((mod x #x1000000)
                                       (x)
                                       (unsigned-32)))]
                                 [(integer-40)
                                  (with-syntax ([(x) (generate-temporaries #'(*))])
                                    #`((mod0 x #x10000000000)
                                       (x)
                                       (integer-64)))]
                                 [(unsigned-40)
                                  (with-syntax ([(x) (generate-temporaries #'(*))])
                                    #`((mod x #x10000000000)
                                       (x)
                                       (unsigned-64)))]
                                 [(integer-48)
                                  (with-syntax ([(x) (generate-temporaries #'(*))])
                                    #`((mod0 x #x1000000000000)
                                       (x)
                                       (integer-64)))]
                                 [(unsigned-48)
                                  (with-syntax ([(x) (generate-temporaries #'(*))])
                                    #`((mod x #x1000000000000)
                                       (x)
                                       (unsigned-64)))]
                                 [(integer-56)
                                  (with-syntax ([(x) (generate-temporaries #'(*))])
                                    #`((mod0 x #x100000000000000)
                                       (x)
                                       (integer-64)))]
                                 [(unsigned-56)
                                  (with-syntax ([(x) (generate-temporaries #'(*))])
                                    #`((mod x #x100000000000000)
                                       (x)
                                       (unsigned-64)))]
                                 [(single-float)
                                  (check-floats-allowed pos)
                                  #f]
                                 [else #f])
                               (with-syntax ([(x) (generate-temporaries #'(*))])
                                 #`(x (x) (#,(datum->syntax #'foreign-callable type))))))
                         type* (enumerate type*))]
                      [(result-filter result [extra-arg ...] [extra ...])
                       (case result-type
                         [(boolean) #`((lambda (x) (if x 1 0))
                                       #,(constant-case int-bits
                                           [(32) #'integer-32]
                                           [(64) #'integer-64])
                                       [] [])]
                         [(stdbool) #`((lambda (x) (if x 1 0))
                                       #,(constant-case stdbool-bits
                                           [(8) #'integer-8])
                                       [] [])]
                         [(char)
                          #`((lambda (x)
                               #,(if unsafe?
                                     #'(char->integer x)
                                     #'(or (and (char? x)
                                                (let ([x (char->integer x)])
                                                  (and (fx<= x #xff) x)))
                                           (err x))))
                             unsigned-8
                             [] [])]
                         [(wchar)
                          (constant-case wchar-bits
                            [(16) #`((lambda (x)
                                       #,(if unsafe?
                                             #'(char->integer x)
                                             #'(or (and (char? x)
                                                        (let ([x (char->integer x)])
                                                          (and (fx<= x #xffff) x)))
                                                   (err x))))
                                     unsigned-16
                                     [] [])]
                            [(32) #`((lambda (x)
                                       #,(if unsafe?
                                             #'(char->integer x)
                                             #'(if (char? x)
                                                   (char->integer x)
                                                   (err x))))
                                     unsigned-16
                                     [] [])])]
                         [(utf-8)
                          (check-strings-allowed)
                          #`((lambda (x)
                               (if (eq? x #f)
                                   x
                                   #,(if unsafe?
                                         #'($fp-string->utf8 x)
                                         #'(if (string? x)
                                               ($fp-string->utf8 x)
                                               (err x)))))
                             u8*
                             [] [])]
                         [(utf-16le)
                          (check-strings-allowed)
                          #`((lambda (x)
                               (if (eq? x #f)
                                   x
                                   #,(if unsafe?
                                         #'($fp-string->utf16 x 'little)
                                         #'(if (string? x)
                                               ($fp-string->utf16 x 'little)
                                               (err x)))))
                             u16*
                             [] [])]
                         [(utf-16be)
                          (check-strings-allowed)
                          #`((lambda (x)
                               (if (eq? x #f)
                                   x
                                   #,(if unsafe?
                                         #'($fp-string->utf16 x 'big)
                                         #'(if (string? x)
                                               ($fp-string->utf16 x 'big)
                                               (err x)))))
                             u16*
                             [] [])]
                         [(utf-16)
                          (check-strings-allowed)
                          #`((lambda (x)
                               (if (eq? x #f)
                                   x
                                   #,(if unsafe?
                                         #'($fp-string->utf16 x (native-endianness))
                                         #'(if (string? x)
                                               ($fp-string->utf16 x (native-endianness))
                                               (err x)))))
                             u16*
                             [] [])]
                         [(utf-32le)
                          (check-strings-allowed)
                          #`((lambda (x)
                               (if (eq? x #f)
                                   x
                                   #,(if unsafe?
                                         #'($fp-string->utf32 x 'little)
                                         #'(if (string? x)
                                               ($fp-string->utf32 x 'little)
                                               (err x)))))
                             u32*
                             [] [])]
                         [(utf-32be)
                          (check-strings-allowed)
                          #`((lambda (x)
                               (if (eq? x #f)
                                   x
                                   #,(if unsafe?
                                         #'($fp-string->utf32 x 'big)
                                         #'(if (string? x)
                                               ($fp-string->utf32 x 'big)
                                               (err x)))))
                             u32*
                             [] [])]
                         [(utf-32)
                          (check-strings-allowed)
                          #`((lambda (x)
                               (if (eq? x #f)
                                   x
                                   #,(if unsafe?
                                         #'($fp-string->utf32 x (native-endianness))
                                         #'(if (string? x)
                                               ($fp-string->utf32 x (native-endianness))
                                               (err x)))))
                             u32*
                             [] [])]
                         [else
                          (cond
                            [($ftd? result-type)
                             (with-syntax ([type (datum->syntax #'foreign-callable result-type)])
                               #`((lambda (x)
                                    #,@(if unsafe? #'() #'((unless (record? x 'type) (err x))))
                                    x)
                                  type
                                  [] []))]
                            [($ftd-as-box? result-type)
                             ;; callable receives an extra pointer argument to fill with the result;
                             ;; we add this type to `$foreign-callable` as an initial address argument,
                             ;; which may be actually provided by the caller or synthesized by the
                             ;; back end, depending on the type and architecture
                             (with-syntax ([type (datum->syntax #'foreign-callable result-type)]
                                           [ftd (datum->syntax #'foreign-callable (unbox result-type))])
                               #`((lambda (x) (void)) ; callable result is ignored
                                  type
                                  [ftd]
                                  [&-result]))]
                            [else
                             (with-syntax ([pred (datum->syntax #'foreign-callable ($fp-type->pred result-type))]
                                           [type (datum->syntax #'foreign-callable result-type)])
                               #`((lambda (x)
                                    #,@(if unsafe? #'() #'((unless (pred x) (err x))))
                                    x)
                                  type
                                  [] []))])])])
          ; use a gensym to avoid giving the procedure a confusing name
          (with-syntax ([p (datum->syntax #'foreign-callable (gensym))])
            #`($foreign-callable conv*
                (let ([p ?proc])
                  (define (err x)
                    ($oops 'foreign-callable
                      "invalid return value ~s from ~s"
                      x p))
                  #,@(if unsafe? #'() #'((unless (procedure? p) ($oops 'foreign-callable "~s is not a procedure" p))))
                  (lambda (extra ... t ... ...)
                    ($event-trap-check) ; ensure eventual `($event)` in the case of many short callbacks
                    (result-filter (p extra ... actual ...))))
                (extra-arg ... arg ... ...)
                result)))))))

(define-syntax foreign-callable
  (lambda (x)
    (define filter-type
      (lambda (r x result?)
        (let ([what (if result? 'result 'argument)])
          (or ($fp-filter-type ($expand-fp-ftype 'foreign-callable what r x) result?)
              (syntax-error x (format "invalid foreign-callable ~s type specifier" what))))))
    (syntax-case x ()
      [(_ c ... ?proc (arg ...) result)
       (lambda (r)
         ($make-foreign-callable 'foreign-callable
           ($filter-conv 'foreign-callable #'(c ...) (length #'(arg ...)))
           #'?proc
           (map (lambda (x) (filter-type r x #f)) #'(arg ...))
           (filter-type r #'result #t)))])))

(define-syntax meta-cond
  (lambda (x)
    (define (help clause*)
      (with-syntax ([([e0 e1 e2 ...] ...) clause*])
        (with-syntax ([(t ...) (generate-temporaries #'(e0 ...))])
          #'(let-syntax ([a (cond [e0 (syntax-rules () [(__ t ...) t])] ...)])
              (a (begin e1 e2 ...) ...)))))
    (syntax-case x (else)
      [(_ [e0 e1 e2 ...] ... [else ee1 ee2 ...])
       (help #'([e0 e1 e2 ...] ... [else ee1 ee2 ...]))]
      [(k [e0 e1 e2 ...] ...)
       (help #'([e0 e1 e2 ...] ... [else (void)]))])))

;;; meta-cond aux keywords
#;(define-syntax else ; defined above for cond
  (lambda (x)
    (syntax-error x "misplaced aux keyword")))

;;; (define-record name pname (field ...))
;;; (define-record name pname (field ...)
;;;   ((field init) ...))
;;; (define-record name pname (field ...)
;;;   ((field init) ...)
;;;   (option ...))
;;; name ::= id
;;; pname ::= <empty> | id
;;; field ::= id | (class type id)
;;; type ::= <empty> | supported record field type
;;; class ::= <empty> | immutable | mutable
;;; option ::= (prefix string)
;;;          | (predicate id)
;;;          | (constructor id)
;;;
;;; initialize fields containing non-ptr types to 0, then fill with
;;; $object-set!

(define-syntax type-descriptor
  (lambda (x)
    (syntax-case x ()
      [(_ name)
       (identifier? #'name)
       (lambda (r)
         (define info (r #'name))
         (cond
           [(and (pair? info) (eq? (car info) '#{record val9xfsq6oa12q4-a}))
            (with-syntax ([(rtd . stuff) (cdr info)])
              #''rtd)]
           [(and (pair? info) (eq? (car info) '#{r6rs-record vc7pishgmrh09qm-a}))
            (with-syntax ([(rtd rcd sealed? protocol?) (cdr info)])
              (if (record-type-descriptor? #'rtd) #''rtd #'rtd))]
           [else (syntax-error #'name "type-descriptor: unrecognized record")]))])))

(define-syntax record-type-descriptor
  (lambda (x)
    (syntax-case x ()
      [(_ name)
       (identifier? #'name)
       (lambda (r)
         (define info (r #'name))
         (cond
           [(and (pair? info) (eq? (car info) '#{record val9xfsq6oa12q4-a}))
            (with-syntax ([(rtd . stuff) (cdr info)])
              #''rtd)]
           [(and (pair? info) (eq? (car info) '#{r6rs-record vc7pishgmrh09qm-a}))
            (with-syntax ([(rtd rcd sealed? protocol?) (cdr info)])
              (if (record-type-descriptor? #'rtd) #''rtd #'rtd))]
           [else (syntax-error #'name "record-type-descriptor: unrecognized record")]))])))

(define-syntax record-constructor-descriptor
  (lambda (x)
    (syntax-case x ()
      [(_ name)
       (identifier? #'name)
       (lambda (r)
         (define info (r #'name))
         (cond
           [(and (pair? info) (eq? (car info) '#{r6rs-record vc7pishgmrh09qm-a}))
            (with-syntax ([(rtd rcd sealed? protocol?) (cdr info)])
              (if (record-constructor-descriptor? #'rcd) #''rcd #'rcd))]
           [(and (pair? info) (eq? (car info) '#{record val9xfsq6oa12q4-a}))
            (syntax-error #'name "no constructor descriptor for define-record record type")]
           [else (syntax-error #'name "record-constructor-descriptor: unrecognized record")]))])))

(define-syntax define-record
  (let ()
    (lambda (x)
      (define-syntactic-monad option cons-id pred-id pref-id)
      (define parse-options
        (option lambda (ols)
          (if (null? ols)
              (option values)
              (syntax-case (car ols) (constructor predicate prefix)
                [(prefix s)
                 (string? (datum s))
                 (option parse-options ((pref-id (datum s)))
                   (cdr ols))]
                [(predicate id)
                 (identifier? #'id)
                 (option parse-options ((pred-id #'id)) (cdr ols))]
                [(constructor id)
                 (identifier? #'id)
                 (option parse-options ((cons-id #'id)) (cdr ols))]))))
      (define record-name
        (lambda (x)
          (let ((x (syntax->datum x)))
            (if (gensym? x) x (symbol->string x)))))
      (define construct-name
        (lambda (template-identifier . args)
          (datum->syntax
            template-identifier
            (string->symbol
              (apply string-append
                     (map (lambda (x)
                            (if (string? x)
                                x
                                (symbol->string (syntax->datum x))))
                          args))))))
      (define field->id
        ; field -> id | ([class] [type] id)
        ; class -> immutable | mutable
        ; type -> scheme-object | double-float | ...
        (lambda (f)
          (define okay?
            (lambda (class type id)
              (and (identifier? id)
                   (or (not class)
                       (free-identifier=? class #'immutable)
                       (free-identifier=? class #'mutable))
                   (or (not type)
                       (memq (filter-foreign-type (syntax->datum type))
                             (record-datatype list))))))
          (syntax-case f ()
            [id (okay? #f #f #'id) #'id]
            [(id) (okay? #f #f #'id) #'id]
            [(class/type id)
             (or (okay? #'class/type #f #'id) (okay? #f #'class/type #'id))
             #'id]
            [(class type id) (okay? #'class #'type #'id) #'id]
            [_ (syntax-error f "invalid field specifier")])))
      (define disjoint?
        (lambda (sym*)
          (or (null? sym*)
              (and (not (memq (car sym*) (cdr sym*)))
                   (disjoint? (cdr sym*))))))
      (define do-define-record
        (lambda (src name prtd pid1s f1s pid2s f2s pinits inits opts)
          (define-syntax with (identifier-syntax with-syntax))
          (with-values
            (option parse-options
              ((cons-id #f) (pred-id #f) (pref-id #f))
              opts)
            (option lambda ()
              (with ([name name]
                     [((pinit ...) ...) pinits]
                     [(init ...) inits]
                     [(o ...) opts]
                     [((pid1 ...) ...) pid1s]
                     [((pid2 ...) ...) pid2s]
                     [(id1 ...) (map field->id f1s)]
                     [(id2 ...) (map field->id f2s)]
                     [primlev (if (= (optimize-level) 3) 3 2)]
                     [prefix (or pref-id (construct-name name name "-"))]
                     [constructor (or cons-id (construct-name name "make-" name))]
                     [predicate (or pred-id (construct-name name name "?"))])
                (unless (disjoint? (map syntax->datum #'(id1 ... id2 ...)))
                  (syntax-error src "duplicate field names in record definition"))
                (with ([rtd (if prtd
                                (make-record-type prtd (record-name #'name)
                                  (syntax->datum (append f1s f2s)))
                                (make-record-type (record-name #'name)
                                  (syntax->datum (append f1s f2s))))])
                  (let* ([pids (with ([((pid ...) ...) #'((pid1 ... pid2 ...) ...)])
                                 #'(pid ... ...))]
                         [allids (append pids #'(id1 ... id2 ...))]
                         [npids (length pids)])
                    (with ([((access ordinal) ...)
                            (map (lambda (id ordinal)
                                   (list (construct-name #'name #'prefix id)
                                         ordinal))
                              (list-tail allids npids)
                              (list-tail (enumerate allids) npids))]
                           [((assign !ordinal) ...)
                            (let f ([ids (list-tail allids npids)]
                                    [ordinal npids])
                              (if (null? ids)
                                  '()
                                  (if (csv7:record-field-mutable? #'rtd ordinal)
                                      (cons
                                        (list
                                          (construct-name #'name "set-" #'prefix (car ids) "!")
                                          ordinal)
                                        (f (cdr ids) (+ ordinal 1)))
                                      (f (cdr ids) (+ ordinal 1)))))])
                      #`(begin
                          (define-syntax name
                            (make-compile-time-value
                              `(#{record val9xfsq6oa12q4-a} rtd
                                 ,#'((pid1 ...) ... (id1 ...))
                                 ,#'((pid2 ...) ... (id2 ...))
                                 ,#'((((... ...) pinit) ...) ... (((... ...) init) ...)))))
                          (define constructor
                            (let ([rcons (($primitive primlev record-constructor) 'rtd)])
                              (lambda (pid1 ... ... id1 ...)
                               ; duplicating pinit code here
                                (let* ([pid2 pinit] ... ... [id2 init] ...)
                                  (rcons #,@allids)))))
                          (define predicate
                            (($primitive primlev record-predicate) 'rtd))
                          (define access
                            (($primitive primlev csv7:record-field-accessor) 'rtd ordinal)) ...
                          (define assign
                            (($primitive primlev csv7:record-field-mutator) 'rtd !ordinal)) ...)))))))))
      (define base-record
        (lambda (src name f1s f2s inits opts)
          (do-define-record src name #f '() f1s  '() f2s '() inits opts)))
      (define child-record
        (lambda (src name pname f1s f2s inits opts)
          (lambda (r)
            (define parent (r pname))
            (when (and (pair? parent) (eq? (car parent) '#{r6rs-record vc7pishgmrh09qm-a}))
              (syntax-error pname "cannot extend define-record-type parent"))
            (unless (and (pair? parent) (eq? (car parent) '#{record val9xfsq6oa12q4-a}))
              (syntax-error pname "unrecognized parent record"))
            (with-syntax ([(prtd ((pid1 ...) ...)
                                 ((pid2 ...) ...)
                                 ((pinit ...) ...))
                           (cdr parent)])
              (do-define-record x name #'prtd
                #'((pid1 ...) ...) f1s
                #'((pid2 ...) ...) f2s
                #'((pinit ...) ...) inits opts)))))
      (syntax-case x ()
        [(_ name (f1 ...))
         (identifier? #'name)
         (base-record x #'name #'(f1 ...) '() '() '())]
        [(_ name (f1 ...) ((f2 init) ...))
         (identifier? #'name)
         (base-record x #'name #'(f1 ...) #'(f2 ...) #'(init ...) '())]
        [(_ name (f1 ...) ((f2 init) ...) (o ...))
         (identifier? #'name)
         (base-record x #'name #'(f1 ...) #'(f2 ...) #'(init ...) #'(o ...))]
        [(_ name pname (f1 ...))
         (and (identifier? #'name) (identifier? #'pname))
         (child-record x #'name #'pname #'(f1 ...) '() '() '())]
        [(_ name pname (f1 ...) ((f2 init) ...))
         (and (identifier? #'name) (identifier? #'pname))
         (child-record x #'name #'pname #'(f1 ...) #'(f2 ...) #'(init ...) '())]
        [(_ name pname (f1 ...) ((f2 init) ...) (o ...))
         (and (identifier? #'name) (identifier? #'pname))
         (child-record x #'name #'pname #'(f1 ...) #'(f2 ...) #'(init ...)
           #'(o ...))]))))

;;; define-record aux keywords
(define-syntax prefix
  (lambda (x)
    (syntax-error x "misplaced aux keyword")))
(define-syntax predicate
  (lambda (x)
    (syntax-error x "misplaced aux keyword")))
(define-syntax constructor
  (lambda (x)
    (syntax-error x "misplaced aux keyword")))
(define-syntax immutable
  (lambda (x)
    (syntax-error x "misplaced aux keyword")))
(define-syntax mutable
  (lambda (x)
    (syntax-error x "misplaced aux keyword")))

(define require-nongenerative-clause
  ($make-thread-parameter #f
    (lambda (x) (and x #t))))

(define-syntax define-record-type
  (lambda (x)
    (lambda (env)
      (define construct-name
        (lambda (template-identifier . args)
          (datum->syntax
            template-identifier
            (string->symbol
              (apply string-append
                     (map (lambda (x)
                            (if (string? x)
                                x
                                (symbol->string (syntax->datum x))))
                          args))))))
      (define (do-define-record-type src name make-name pred-name clause*)
        (define-flags clause-key
          (fields        #b00000001)
          (parent        #b00000010)
          (protocol      #b00000100)
          (sealed        #b00001000)
          (opaque        #b00010000)
          (nongenerative #b00100000)
          (parent-rtd    #b01000000))
        (define-record-type field-desc
          (fields (immutable name) (immutable index) (immutable spec) (immutable accessor) (immutable mutator))
          (nongenerative)
          (sealed #t))
        (define-record-type parent-desc
          (fields (immutable rtd) (immutable rcd) (immutable protocol?))
          (nongenerative)
          (sealed #t))
        (define (parse-field x i)
          (syntax-case x (immutable mutable)
            [(immutable field-name accessor-name)
             (and (identifier? #'field-name) (identifier? #'accessor-name))
             (make-field-desc
               (datum field-name)
               i
               #'(immutable field-name)
               #'accessor-name
               #f)]
            [(mutable field-name accessor-name mutator-name)
             (and (identifier? #'field-name) (identifier? #'accessor-name) (identifier? #'mutator-name))
             (make-field-desc
               (datum field-name)
               i
               #'(mutable field-name)
               #'accessor-name
               #'mutator-name)]
            [(immutable field-name)
             (identifier? #'field-name)
             (make-field-desc (datum field-name) i x
               (construct-name name name "-" #'field-name)
               #f)]
            [(mutable field-name)
             (identifier? #'field-name)
             (make-field-desc (datum field-name) i x
               (construct-name name name "-" #'field-name)
               (construct-name name name "-" #'field-name "-set!"))]
            [field-name
             (identifier? #'field-name)
             (make-field-desc (datum field-name) i #'(immutable field-name)
               (construct-name name name "-" #'field-name)
               #f)]
            [_ (syntax-error x "invalid field specifier")]))
        (define-syntactic-monad Mclause %fields %parent %protocol
          %sealed? %opaque? %uid %prtd-expr %prcd-expr)
        (define parse-clauses
          (Mclause lambda (keys-seen clause*)
            (if (null? clause*)
                (Mclause values () keys-seen)
                (syntax-case (car clause*) (fields parent protocol sealed opaque nongenerative parent-rtd)
                  [(fields field ...)
                   (begin
                     (when (any-set? keys-seen (clause-key fields))
                       (syntax-error src "record definition has multiple fields clauses"))
                     (Mclause parse-clauses
                       ([%fields (let ([ls #'(field ...)])
                                   (map parse-field ls (enumerate ls)))])
                       (set-flags keys-seen (clause-key fields))
                       (cdr clause*)))]
                  [(parent pname)
                   (identifier? #'pname)
                   (begin
                     (when (any-set? keys-seen (clause-key parent))
                       (syntax-error src "record definition has multiple parent clauses"))
                     (when (any-set? keys-seen (clause-key parent-rtd))
                       (syntax-error src "record definition has both parent and parent-rtd clauses"))
                     (let ([info (env #'pname)])
                       (when (and (pair? info) (eq? (car info) '#{record val9xfsq6oa12q4-a}))
                         (syntax-error #'pname "cannot extend define-record parent"))
                       (unless (and (pair? info) (eq? (car info) '#{r6rs-record vc7pishgmrh09qm-a}))
                         (syntax-error #'pname "unrecognized parent record"))
                       (with-syntax ([(rtd rcd sealed? protocol?) (cdr info)])
                         (when #'sealed? (syntax-error #'pname "parent record type is sealed"))
                         (Mclause parse-clauses ([%parent (make-parent-desc #'rtd #'rcd #'protocol?)])
                           (set-flags keys-seen (clause-key parent))
                           (cdr clause*)))))]
                  [(protocol expr)
                   (begin
                     (when (any-set? keys-seen (clause-key protocol))
                       (syntax-error src "record definition has multiple protocol clauses"))
                     (Mclause parse-clauses ([%protocol #'expr])
                       (set-flags keys-seen (clause-key protocol))
                       (cdr clause*)))]
                  [(sealed expr)
                   (memq (datum expr) '(#t #f))
                   (begin
                     (when (any-set? keys-seen (clause-key sealed))
                       (syntax-error src "record definition has multiple sealed clauses"))
                     (Mclause parse-clauses ([%sealed? (datum expr)])
                       (set-flags keys-seen (clause-key sealed))
                       (cdr clause*)))]
                  [(opaque expr)
                   (memq (datum expr) '(#t #f))
                   (begin
                     (when (any-set? keys-seen (clause-key opaque))
                       (syntax-error src "record definition has multiple opaque clauses"))
                     (Mclause parse-clauses ([%opaque? (datum expr)])
                       (set-flags keys-seen (clause-key opaque))
                       (cdr clause*)))]
                  [(nongenerative)
                   (begin
                     (when (any-set? keys-seen (clause-key nongenerative))
                       (syntax-error src "record definition has multiple nongenerative clauses"))
                     (Mclause parse-clauses ([%uid (datum->syntax #'* ((current-generate-id) (syntax->datum name)))])
                       (set-flags keys-seen (clause-key nongenerative))
                       (cdr clause*)))]
                  [(nongenerative id)
                   (identifier? #'id)
                   (begin
                     (when (any-set? keys-seen (clause-key nongenerative))
                       (syntax-error src "record definition has multiple nongenerative clauses"))
                     (Mclause parse-clauses ([%uid #'id])
                       (set-flags keys-seen (clause-key nongenerative))
                       (cdr clause*)))]
                  [(nongenerative #f)
                   (begin
                     (when (any-set? keys-seen (clause-key nongenerative))
                       (syntax-error src "record definition has multiple nongenerative clauses"))
                     (Mclause parse-clauses ()
                       (set-flags keys-seen (clause-key nongenerative))
                       (cdr clause*)))]
                  [(parent-rtd prtd-expr pcd-expr)
                   (begin
                     (when (any-set? keys-seen (clause-key parent-rtd))
                       (syntax-error src "record definition has multiple parent-rtd clauses"))
                     (when (any-set? keys-seen (clause-key parent))
                       (syntax-error src "record definition has both parent and parent-rtd clauses"))
                     (Mclause parse-clauses
                       ([%prtd-expr #'prtd-expr] [%prcd-expr #'pcd-expr])
                       (set-flags keys-seen (clause-key parent-rtd))
                       (cdr clause*)))]
                  [_ (syntax-error (car clause*) "invalid define-record-type clause")]))))
        (define (quotify x) (if (and x (not (identifier? x))) #`'#,x x))
        (with-values
          (Mclause parse-clauses
            ([%fields '()]
             [%parent #f]
             [%protocol #f]
             [%sealed? #f]
             [%opaque? #f]
             [%uid #f]
             [%prtd-expr #f]
             [%prcd-expr #f])
            (clause-key (or))
            clause*)
          (Mclause lambda (keys-seen)
            (when (require-nongenerative-clause)
              (unless (any-set? keys-seen (clause-key nongenerative))
                (syntax-error src "missing nongenerative clause and require-nongenerative-clause is #t")))
            (unless %protocol
              (when (and %parent (parent-desc-protocol? %parent))
                (syntax-error src "no protocol supplied, but parent protocol was supplied")))
            (let ([%mutable-fields (filter field-desc-mutator %fields)])
              (with-syntax ([primlev (if (= (optimize-level) 3) 3 2)]
                            [(accessor-name ...) (map field-desc-accessor %fields)]
                            [(accessor-index ...) (map field-desc-index %fields)]
                            [(mutator-name ...) (map field-desc-mutator %mutable-fields)]
                            [(mutator-index ...) (map field-desc-index %mutable-fields)])
                (unless ($distinct-bound-ids? `(,make-name ,pred-name ,@#'(accessor-name ...) ,@#'(mutator-name ...)))
                  (syntax-error src "record definition would result in duplicates among the constructor, predicate, accessor, and mutator names"))
               ; construct rtd at expand time iff:
               ;  - uid is not #f or definition is at top level,
               ;  - %parent is #f or its rtd is not #f, and
               ;  - %prtd-expr is #f.
                (if (and (or %uid ($syntax-top-level?))
                         (or (not %parent) (record-type-descriptor? (parent-desc-rtd %parent)))
                         (not %prtd-expr))
                    (let ([rtd ($make-record-type-descriptor
                                 #!base-rtd
                                 (syntax->datum name)
                                 (and %parent (parent-desc-rtd %parent))
                                 (syntax->datum %uid)
                                 %sealed?
                                 %opaque?
                                 (list->vector
                                   (map (lambda (%field)
                                          (syntax->datum
                                            (field-desc-spec %field)))
                                        %fields))
                                 'define-record-type)])
                      (if %protocol
                          #`(begin
                              (define rcd
                                ($make-record-constructor-descriptor '#,rtd
                                  #,(quotify (and %parent (parent-desc-rcd %parent)))
                                  #,%protocol
                                  'define-record-type))
                              (define-syntax #,name
                                (make-compile-time-value
                                  `(#{r6rs-record vc7pishgmrh09qm-a} #,rtd ,#'rcd #,%sealed? #,#t)))
                              (indirect-export #,name rcd)
                              (define #,make-name (($primitive primlev r6rs:record-constructor) rcd))
                              (define #,pred-name (($primitive primlev record-predicate) '#,rtd))
                              (define accessor-name
                                (($primitive primlev record-accessor) '#,rtd accessor-index))
                                ...
                              (define mutator-name
                                (($primitive primlev record-mutator) '#,rtd mutator-index))
                              ...)
                          (let ([rcd (make-record-constructor-descriptor rtd
                                       (and %parent (parent-desc-rcd %parent))
                                       #f)])
                            #`(begin
                                (define-syntax #,name
                                  (make-compile-time-value
                                    `(#{r6rs-record vc7pishgmrh09qm-a} #,rtd #,rcd #,%sealed? #,#f)))
                                (define #,make-name (($primitive primlev r6rs:record-constructor) '#,rcd))
                                (define #,pred-name (($primitive primlev record-predicate) '#,rtd))
                                (define accessor-name
                                  (($primitive primlev record-accessor) '#,rtd accessor-index))
                                  ...
                                (define mutator-name
                                  (($primitive primlev record-mutator) '#,rtd mutator-index))
                                ...))))
                    #`(begin
                        (define rtd
                          ($make-record-type-descriptor
                            #!base-rtd
                            '#,name
                            #,(if %parent (quotify (parent-desc-rtd %parent)) %prtd-expr)
                            '#,%uid
                            #,%sealed?
                            #,%opaque?
                            '#,(list->vector (map field-desc-spec %fields))
                            'define-record-type))
                        (define rcd
                          ($make-record-constructor-descriptor rtd
                            #,(if %parent (quotify (parent-desc-rcd %parent)) %prcd-expr)
                            #,%protocol
                            'define-record-type))
                        (define-syntax #,name
                          (make-compile-time-value
                            `(#{r6rs-record vc7pishgmrh09qm-a} ,#'rtd ,#'rcd #,%sealed? #,(and %protocol #t))))
                        (indirect-export #,name rtd rcd)
                        (define #,make-name (($primitive primlev r6rs:record-constructor) rcd))
                        (define #,pred-name (($primitive primlev record-predicate) rtd))
                        (define accessor-name
                          (($primitive primlev record-accessor) rtd accessor-index))
                        ...
                        (define mutator-name
                          (($primitive primlev record-mutator) rtd mutator-index))
                        ...)))))))
      (syntax-case x ()
        [(_ name clause ...)
         (identifier? #'name)
         (do-define-record-type x #'name
           (construct-name #'name "make-" #'name)
           (construct-name #'name #'name "?")
           #'(clause ...))]
        [(_ (name make-name pred-name) clause ...)
         (and (identifier? #'name)
              (identifier? #'make-name)
              (identifier? #'pred-name))
         (do-define-record-type x #'name #'make-name
           #'pred-name #'(clause ...))]))))

;;; define-record-type aux keywords
(define-syntax fields
  (lambda (x)
    (syntax-error x "misplaced aux keyword")))
(define-syntax parent
  (lambda (x)
    (syntax-error x "misplaced aux keyword")))
(define-syntax protocol
  (lambda (x)
    (syntax-error x "misplaced aux keyword")))
(define-syntax sealed
  (lambda (x)
    (syntax-error x "misplaced aux keyword")))
(define-syntax opaque
  (lambda (x)
    (syntax-error x "misplaced aux keyword")))
(define-syntax nongenerative
  (lambda (x)
    (syntax-error x "misplaced aux keyword")))
(define-syntax parent-rtd
  (lambda (x)
    (syntax-error x "misplaced aux keyword")))
#;(define-syntax immutable ; defined above for define-record
  (lambda (x)
    (syntax-error x "misplaced aux keyword")))
#;(define-syntax mutable ; defined above for define-record
  (lambda (x)
    (syntax-error x "misplaced aux keyword")))

(define-syntax endianness
  (lambda (x)
    (syntax-case x ()
      [(_ x)
       (if (memq (datum x) '(big little))
           #''x
           (syntax-error #'x "invalid endianness"))])))

;;; syntactic interface to enumerations
(define-syntax define-enumeration
  (lambda (x)
    (syntax-case x ()
      [(_ type-name (symbol ...) constructor-syntax)
       (and (identifier? #'type-name)
            (for-all identifier? #'(symbol ...))
            (identifier? #'constructor-syntax))
       #'(module ((type-name expand-time-this-enum)
                  (constructor-syntax this-enum expand-time-this-enum))
           (define this-enum (make-enumeration '(symbol ...)))
           (define-syntax expand-time-this-enum
             (make-compile-time-value
               `(#{enum-set yb11fsqj62y93q3-a} . ,(make-enumeration '(symbol ...)))))
           (define-syntax type-name
             (lambda (x)
               (lambda (r)
                 (let ([expand-time-this-enum (cdr (r #'expand-time-this-enum))])
                   (syntax-case x ()
                     [(_ x)
                      (identifier? #'x)
                      (begin
                        (unless (enum-set-member? (datum x) expand-time-this-enum)
                          (syntax-error #'x
                            (format "universe of ~s does not include specified symbol"
                                    'type-name)))
                        #''x)])))))
           (define-syntax constructor-syntax
             (lambda (x)
               (lambda (r)
                 (let ([expand-time-this-enum (cdr (r #'expand-time-this-enum))])
                   (syntax-case x ()
                     [(_ args (... ...))
                      (for-all identifier? #'(args (... ...)))
                      (begin
                        (for-each
                          (lambda (x y)
                            (unless (enum-set-member? x expand-time-this-enum)
                              (syntax-error y
                                (format "universe of ~s does not include specified symbol"
                                        'type-name))))
                          (datum (args (... ...)))
                          #'(args (... ...)))
                        (with-syntax ([members
                                       ($enum-set-members
                                         ((enum-set-constructor expand-time-this-enum)
                                          (datum (args (... ...)))))])
                          #'($record (record-rtd this-enum) members)))]))))))])))

(define-syntax file-options
  (lambda (x)
    (syntax-case x ()
      [(_ id ...)
       (andmap identifier? #'(id ...))
       (begin
         (for-each
           (lambda (id)
             (unless (enum-set-member? (syntax->datum id) $file-options)
               (syntax-error id "invalid file option")))
           #'(id ...))
         (with-syntax ([members ($enum-set-members ($make-file-options (datum (id ...))))])
           #'($record (record-rtd $file-options) members)))])))

(define-syntax fasl-strip-options
  (lambda (x)
    (syntax-case x ()
      [(_ id ...)
       (andmap identifier? #'(id ...))
       (begin
         (for-each
           (lambda (id)
             (unless (enum-set-member? (syntax->datum id) $fasl-strip-options)
               (syntax-error id "invalid fasl strip option")))
           #'(id ...))
         (with-syntax ([members ($enum-set-members ($make-fasl-strip-options (datum (id ...))))])
           #'($record (record-rtd $fasl-strip-options) members)))])))

(define-syntax annotation-options
  (lambda (x)
    (syntax-case x ()
      [(_ id ...)
       (andmap identifier? #'(id ...))
       (begin
         (for-each
           (lambda (id)
             (unless (enum-set-member? (syntax->datum id) $annotation-options)
               (syntax-error id "invalid fasl strip option")))
           #'(id ...))
         (with-syntax ([members ($enum-set-members ($make-annotation-options (datum (id ...))))])
           #'($record (record-rtd $annotation-options) members)))])))

(define-syntax library-requirements-options
  (lambda (x)
    (syntax-case x ()
      [(_ id ...)
       (andmap identifier? #'(id ...))
       (begin
         (for-each
           (lambda (id)
             (unless (enum-set-member? (syntax->datum id) $library-requirements-options)
               (syntax-error id "invalid library requirements option")))
           #'(id ...))
         (with-syntax ([members ($enum-set-members ($make-library-requirements-options (datum (id ...))))])
           #'($record (record-rtd $library-requirements-options) members)))])))

(define-syntax buffer-mode
  (lambda (x)
    (syntax-case x ()
      [(_ x)
       (if (buffer-mode? (datum x))
           #''x
           (syntax-error #'x "invalid buffer mode"))])))

(define-syntax eol-style
  (lambda (x)
    (syntax-case x ()
      [(_ x)
       (if ($eol-style? (datum x))
           #''x
           (syntax-error #'x "invalid eol style"))])))

(define-syntax error-handling-mode
  (lambda (x)
    (syntax-case x ()
      [(_ x)
       (if ($error-handling-mode? (datum x))
           #''x
           (syntax-error #'x "invalid error handling mode"))])))

(define-syntax assert
  (lambda (x)
    (syntax-case x ()
      [(_ expr)
       #`(or expr #,($make-source-oops #f "failed assertion" #'expr))])))

(let ()
  (module types (source make-source source? source-sfd source-bfp source-efp
                 source-2d? make-source-2d source-2d-line source-2d-column
                 annotation make-annotation annotation? annotation-expression annotation-source annotation-stripped annotation-flags
                 make-source-file-descriptor source-file-descriptor source-file-descriptor? source-file-descriptor-name
                 source-file-descriptor-length source-file-descriptor-crc
                 syntax-object? syntax-object-expression
                 )
    (include "types.ss"))
  (import (prefix types %))
  (record-writer (type-descriptor %source-file-descriptor)
    (lambda (x p wr)
      (fprintf p "#<sfd ~a>" (%source-file-descriptor-name x))))
  (let ()
    (define prsource
      (lambda (x p)
        (fprintf p "~a[~s:~s]"
          (%source-file-descriptor-name (%source-sfd x))
          (%source-bfp x)
          (%source-efp x))))
    (record-writer (type-descriptor %source)
      (lambda (x p wr)
        (display-string "#<source " p)
        (prsource x p)
        (display-string ">" p)))
    (record-writer (type-descriptor %annotation)
      (lambda (x p wr)
        (display-string "#<annotation " p)
        (prsource (%annotation-source x) p)
        (display-string " " p)
        (wr (%annotation-stripped x) p)
        (display-string ">" p))))
  (set-who! make-source-object
    (case-lambda
     [(sfd bfp efp)
      (unless (%source-file-descriptor? sfd)
        ($oops who "~s is not a source file descriptor" sfd))
      (unless (if (fixnum? bfp) (fx>= bfp 0) (and (bignum? bfp) ($bigpositive? bfp)))
        ($oops who "~s is not an exact nonnegative integer" bfp))
      (unless (if (fixnum? efp) (fx>= efp 0) (and (bignum? efp) ($bigpositive? efp)))
        ($oops who "~s is not an exact nonnegative integer" efp))
      (unless (<= bfp efp)
        ($oops who "ending file position ~s is less than beginning file position ~s" efp bfp))
      (%make-source sfd bfp efp)]
     [(sfd bfp efp line column)
      (unless (%source-file-descriptor? sfd)
        ($oops who "~s is not a source file descriptor" sfd))
      (unless (if (fixnum? bfp) (fx>= bfp 0) (and (bignum? bfp) ($bigpositive? bfp)))
        ($oops who "~s is not an exact nonnegative integer" bfp))
      (unless (if (fixnum? efp) (fx>= efp 0) (and (bignum? efp) ($bigpositive? efp)))
        ($oops who "~s is not an exact nonnegative integer" efp))
      (unless (if (fixnum? line) (fx>= line 1) (and (bignum? line) ($bigpositive? line)))
        ($oops who "~s is not an exact positive integer" line))
      (unless (if (fixnum? column) (fx>= column 1) (and (bignum? column) ($bigpositive? column)))
        ($oops who "~s is not an exact positive integer" column))
      (unless (<= bfp efp)
        ($oops who "ending file position ~s is less than beginning file position ~s" efp bfp))
      (%make-source-2d sfd bfp efp line column)]))
  (set-who! current-make-source-object
    (case-lambda
     [() (or ($current-mso) make-source-object)]
     [(x)
      (unless (procedure? x) ($oops who "~s is not a procedure" x))
      ($current-mso (if (eq? x make-source-object) #f x))]))
  (set-who! source-object?
    (lambda (x)
      (%source? x)))
  (set-who! source-object-sfd
    (lambda (x)
      (unless (%source? x) ($oops who "~s is not a source object" x))
      (%source-sfd x)))
  (set-who! source-object-bfp
    (lambda (x)
      (unless (%source? x) ($oops who "~s is not a source object" x))
      (%source-bfp x)))
  (set-who! source-object-efp
    (lambda (x)
      (unless (%source? x) ($oops who "~s is not a source object" x))
      (%source-efp x)))
  (set-who! source-object-line
    (lambda (x)
      (cond
       [(%source-2d? x) (%source-2d-line x)]
       [(%source? x) #f]
       [else ($oops who "~s is not a source object" x)])))
  (set-who! source-object-column
    (lambda (x)
      (cond
       [(%source-2d? x) (%source-2d-column x)]
       [(%source? x) #f]
       [else ($oops who "~s is not a source object" x)])))
  (set-who! make-annotation
    (case-lambda
      [(expression source stripped)
       (unless (%source? source) ($oops who "~s is not a source object" source))
       (%make-annotation expression source stripped)]
      [(expression source stripped options)
       (unless (and (enum-set? options) (enum-set-subset? options $annotation-options))
         ($oops who "~s is not an annotation-options object" options))
       (unless (%source? source) ($oops who "~s is not a source object" source))
       (%make-annotation expression source stripped
         (fxlogor
           (if (enum-set-subset? (annotation-options debug) options)
               (constant annotation-debug)
               0)
           (if (enum-set-subset? (annotation-options profile) options)
               (constant annotation-profile)
               0)))]))
  (set-who! annotation?
    (lambda (x)
      (%annotation? x)))
  (set-who! annotation-source
    (lambda (x)
      (unless (%annotation? x) ($oops who "~s is not an annotation" x))
      (%annotation-source x)))
  (set-who! annotation-expression
    (lambda (x)
      (unless (%annotation? x) ($oops who "~s is not an annotation" x))
      (%annotation-expression x)))
  (set-who! annotation-stripped
    (lambda (x)
      (unless (%annotation? x) ($oops who "~s is not an annotation" x))
      (%annotation-stripped x)))
  (set-who! annotation-option-set
    (lambda (x)
      (unless (%annotation? x) ($oops who "~s is not an annotation" x))
      (let ([flags (%annotation-flags x)])
        (if (fxlogtest flags (constant annotation-debug))
            (if (fxlogtest flags (constant annotation-profile))
                (annotation-options debug profile)
                (annotation-options debug))
            (if (fxlogtest flags (constant annotation-profile))
                (annotation-options profile)
                (annotation-options))))))
  (set-who! make-source-file-descriptor
    (rec make-source-file-descriptor
      (case-lambda
        [(ifn bip) (make-source-file-descriptor ifn bip #f)]
        [(ifn bip reset?)
         (unless (and (input-port? bip) (binary-port? bip))
           ($oops who "~s is not a binary input port" bip))
         (when reset?
           (unless (and (port-has-port-position? bip) (port-has-set-port-position!? bip))
             ($oops who "~s does not support port-position and set-port-position!" bip)))
         ($source-file-descriptor ifn bip reset?)])))
  (set-who! source-file-descriptor
    (lambda (path checksum)
      (unless (if (fixnum? checksum) (fx>= checksum 0) (and (bignum? checksum) ($bigpositive? checksum)))
        ($oops who "~s is not an exact nonnegative integer" checksum))
      (%make-source-file-descriptor path (ash checksum -16) (logand checksum #xffff))))
  (set-who! source-file-descriptor?
    (lambda (x)
      (%source-file-descriptor? x)))
  (set-who! source-file-descriptor-path
    (lambda (x)
      (unless (%source-file-descriptor? x) ($oops who "~s is not a source-file descriptor" x))
      (%source-file-descriptor-name x)))
  (set-who! source-file-descriptor-checksum
    (lambda (x)
      (unless (%source-file-descriptor? x) ($oops who "~s is not a source-file descriptor" x))
      (logor
        (ash (%source-file-descriptor-length x) 16)
        (%source-file-descriptor-crc x))))
  (set-who! open-source-file
    (lambda (sfd)
      (unless (%source-file-descriptor? sfd) ($oops who "~s is not a source-file descriptor" sfd))
      ($open-source-file sfd)))
  (set-who! locate-source
    (rec locate-source
      (case-lambda
       [(sfd fp) (locate-source sfd fp #f)]
       [(sfd fp use-cache?)
        (unless (%source-file-descriptor? sfd) ($oops who "~s is not a source-file descriptor" sfd))
        (unless (if (fixnum? fp) (fx>= fp 0) (and (bignum? fp) ($bigpositive? fp)))
          ($oops who "~s is not an exact nonnegative integer" fp))
        ($locate-source sfd fp use-cache?)])))
  (set-who! locate-source-object-source
    (lambda (src start? cache?)
      (cond
       [(and start?
             (%source-2d? src))
        (values (%source-file-descriptor-name (%source-sfd src))
                (%source-2d-line src)
                (%source-2d-column src))]
       [(%source? src)
        ($locate-source (%source-sfd src)
                        (if start?
                            (%source-bfp src)
                            (%source-efp src))
                        cache?)]
       [else
        ($oops who "~s is not a source object" src)])))
  (set-who! current-locate-source-object-source
    ($make-thread-parameter
     locate-source-object-source
     (lambda (x)
       (unless (procedure? x) ($oops who "~s is not a procedure" x))
       x)))
  (set-who! syntax->annotation
    (lambda (x)
      (cond
        [(%annotation? x) x]
        [(%syntax-object? x)
         (let ([x (%syntax-object-expression x)])
           (and (%annotation? x) x))]
        [else #f]))))

(set-who! $annotation-options (make-enumeration '(debug profile)))
(set-who! $make-annotation-options (enum-set-constructor $annotation-options))
)
