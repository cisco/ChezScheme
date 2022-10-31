;;; expeditor.ss
;;; R. Kent Dybvig
;;; August 2007

;;; This code is based on David Boyer's command-line editor, which has the
;;; following copyright:
;;;
;;;   Copyright (c) 1989, 1993, 1994 C. David Boyer
;;;
;;;   Permission to copy this software, in whole or in part, to use this
;;;   software for any lawful purpose, and to redistribute this software is
;;;   granted subject to the restriction that all copies made of this software
;;;   must include this copyright notice in full.
;;;
;;; The present implementation retains some of the basic design but little
;;; of the original code.

;;; The expression editor module is organized into sections:
;;;
;;;  1. screen-management routines
;;;  2. exported parameters
;;;  3. eestate and pos record definitions
;;;  4. current entry management routines
;;;  5. the reader and prompt-and-reader
;;;  6. history management routines
;;;  7. key function definitions
;;;  8. key binding code
;;;
;;; Also contained within this file are a few system entry points:
;;; the $enable-expeditor and $expeditor-history-file parameters and
;;; the main entry point into the expression editor, $expeditor.

(when-feature expeditor

(define $enable-expeditor (make-parameter #f))
(define $expeditor-history-file
  (make-parameter #f
    (lambda (s)
      (cond
        [(not s) s]
        [(string? s)
         (if (string=? s "")
             (if-feature windows
               (cond
                 [(getenv "APPDATA") =>
                  (lambda (appdata)
                    (let ([dir (format "~a\\Chez Scheme" appdata)])
                      (unless (file-exists? dir)
                        (guard (c [#t (void)]) (mkdir dir)))
                      (format "~a\\History" dir)))]
                 [(getenv "HOME") =>
                  (lambda (home)
                    (format "~a\\.chezscheme_history" home))]
                 [else ".chezscheme_history"])
               "~/.chezscheme_history")
             s)]
        [else ($oops '$expeditor-history-file "~s is not #f or a string" s)]))))
          
(define $expeditor)

(module expression-editor
            (
            ; parameters
             ee-auto-indent ee-auto-paren-balance ee-common-identifiers
             ee-default-repeat ee-flash-parens ee-noisy
             ee-paren-flash-delay ee-history-limit ee-standard-indent
            ; establishing key bindings
             ee-bind-key ee-compose
            ; built-in operators
             ee-next-id-completion
             ee-next-id-completion/indent
             ee-id-completion ee-id-completion/indent
             ee-insert-self ee-command-repeat
             ee-history-bwd ee-history-fwd
             ee-history-fwd-prefix ee-history-bwd-prefix
             ee-history-fwd-contains ee-history-bwd-contains
             ee-newline ee-accept ee-newline/accept ee-open-line
             ee-indent ee-indent-all ee-backward-char
             ee-forward-char ee-next-line ee-previous-line
             ee-end-of-line ee-beginning-of-line
             ee-beginning-of-entry ee-end-of-entry
             ee-delete-to-eol ee-delete-line
             ee-delete-between-point-and-mark ee-set-mark
             ee-delete-entry ee-reset-entry ee-delete-sexp ee-backward-delete-sexp
             ee-redisplay ee-yank-kill-buffer ee-yank-selection
             ee-string-macro ee-eof ee-delete-char ee-eof/delete-char
             ee-backward-delete-char ee-insert-paren
             ee-flash-matching-delimiter ee-goto-matching-delimiter
             ee-exchange-point-and-mark ee-forward-sexp
             ee-backward-sexp ee-forward-word
             ee-backward-word ee-forward-page
             ee-backward-page ee-suspend-process
            )

(define-syntax assert*
  (syntax-rules ()
    [(_ expr ...)
     (begin (assert expr) ...)]))

(define-syntax on-error
  (syntax-rules ()
    [(on-error e0 e1 e2 ...)
     (guard (c [#t e0]) e1 e2 ...)]))

(define-syntax defopt
  (syntax-rules ()
    [(_ (p x ... [y e]) b1 b2 ...)
     (define p
       (case-lambda
         [(x ...) (p x ... e)]
         [(x ... y) b1 b2 ...]))]))

; screen initialization and manipulation routines

(module (init-screen raw-mode no-raw-mode
         screen-resize! screen-rows screen-cols
         ee-winch? ee-char-ready? ee-peek-char ee-read-char
         ee-write-char ee-display-string ee-flush
         move-cursor-up move-cursor-right move-cursor-left move-cursor-down
         scroll-reverse clear-eol clear-eos clear-screen
         carriage-return line-feed
         bell pause get-clipboard wait)
 ; screen state
  (define cols)
  (define rows)
  (define cursor-col)
  (define the-unread-char)
  (define winch)

 ; we use terminfo routines directly, rather than going through curses,
 ; because curses requires initscr(), which clears the screen, discarding
 ; the current context.  this is a shell, not a full-screen user interface.

  (define init-term (foreign-procedure "(cs)ee_init_term" (iptr iptr) boolean))
  (define $ee-read-char (foreign-procedure "(cs)ee_read_char" (boolean) scheme-object))
  (define $ee-write-char (foreign-procedure "(cs)ee_write_char" (wchar_t) void))
  (define ee-flush (foreign-procedure "(cs)ee_flush" () void))
  (define get-screen-size (foreign-procedure "(cs)ee_get_screen_size" () scheme-object))
  (define raw-mode (foreign-procedure "(cs)ee_raw" () void))
  (define no-raw-mode (foreign-procedure "(cs)ee_noraw" () void))
  (define enter-am-mode (foreign-procedure "(cs)ee_enter_am_mode" () void))
  (define exit-am-mode (foreign-procedure "(cs)ee_exit_am_mode" () void))
  (define nanosleep (foreign-procedure "(cs)ee_nanosleep" (unsigned-32 unsigned-32) void))
  (define pause (foreign-procedure "(cs)ee_pause" () void))
  (define get-clipboard (foreign-procedure "(cs)ee_get_clipboard" () scheme-object))

  (define move-cursor-up (foreign-procedure "(cs)ee_up" (integer-32) void))
  (define move-cursor-down (foreign-procedure "(cs)ee_down" (integer-32) void))
  (define $move-cursor-left (foreign-procedure "(cs)ee_left" (integer-32) void))
  (define $move-cursor-right (foreign-procedure "(cs)ee_right" (integer-32) void))
  (define clear-eol (foreign-procedure "(cs)ee_clr_eol" () void))
  (define clear-eos (foreign-procedure "(cs)ee_clr_eos" () void))
  (define $clear-screen (foreign-procedure "(cs)ee_clear_screen" () void))
  (define scroll-reverse (foreign-procedure "(cs)ee_scroll_reverse" (integer-32) void))
  (define bell (foreign-procedure "(cs)ee_bell" () void))
  (define $carriage-return (foreign-procedure "(cs)ee_carriage_return" () void))
  (define line-feed (foreign-procedure "(cs)ee_line_feed" () void))

  (define (screen-resize!)
    (let ([p (get-screen-size)])
      (set! rows (car p))
      (set! cols (cdr p))))

  (define (screen-rows) rows)
  (define (screen-cols) cols)

  (define (init-screen)
    (and (init-term -1 -1)
         (begin
           (set! cursor-col 0)
           (set! the-unread-char #f)
           (set! winch #f)
           #t)))

  (define (clear-screen)
    ($clear-screen)
    (set! cursor-col 0))

  (define (ee-winch?)
    (and (not the-unread-char)
      (if winch
          (begin (set! winch #f) #t)
          (begin
            (ee-flush)
            (let ([c ($ee-read-char #t)])
              (or (eq? c #t)
                  (begin (set! the-unread-char c) #f)))))))

  (define (ee-char-ready?)
    (if the-unread-char
        #t
        (let f ()
          (ee-flush)
          (let ([c ($ee-read-char #f)])
            (cond
              [(eq? c #f) #f]
              [(eq? c #t) (set! winch #t) (f)]
              [else (set! the-unread-char c) #t])))))

  (define (ee-read-char)
    (if the-unread-char
        (let ([c the-unread-char]) (set! the-unread-char #f) c)
        (let f ()
          (ee-flush)
          (let ([c ($ee-read-char #t)])
            (if (eq? c #t)
                (begin (set! winch #t) (f))
                c)))))

  (define (ee-peek-char)
    (or the-unread-char
        (let ([c (ee-read-char)])
          (set! the-unread-char c)
          c)))

 ; we assume that ee-write-char receives only characters that occupy one
 ; screen cell.  it should never be passed #\return, #\newline, or #\tab.
 ; furthermore, ee-write-char should never be used to write past the end
 ; of a screen line.
  (define (ee-write-char c)
    (set! cursor-col (fx+ cursor-col 1))
    (if (fx= cursor-col cols)
        (begin
          (exit-am-mode)
          ($ee-write-char c)
          (enter-am-mode))
        ($ee-write-char c)))

 ; comments regarding ee-write-char above apply also to ee-display-string
  (define (ee-display-string s)
    (let ([n (string-length s)])
      (do ([i 0 (fx+ i 1)])
          ((fx= i n))
        (ee-write-char (string-ref s i)))))

  (define (carriage-return)
    (set! cursor-col 0)
    ($carriage-return))

  (define (move-cursor-right n)
    (cond
      [(fx< (fx+ cursor-col n) cols)
       ($move-cursor-right n)
       (set! cursor-col (fx+ cursor-col n))]
      [else
       (move-cursor-down (quotient (fx+ cursor-col n) cols))
       (let ([new-cursor-col (remainder (fx+ cursor-col n) cols)])
         (if (fx>= new-cursor-col cursor-col)
             (move-cursor-right (fx- new-cursor-col cursor-col))
             (move-cursor-left (fx- cursor-col new-cursor-col))))]))

  (define (move-cursor-left n)
    (when (and (fx= cursor-col cols) (fx> n 0))
      (set! n (fx- n 1))
      (set! cursor-col (fx- cursor-col 1)))
    (cond
      [(fx<= n cursor-col)
       ($move-cursor-left n)
       (set! cursor-col (fx- cursor-col n))]
      [else
       (move-cursor-up (fx1+ (quotient (fx- n cursor-col 1) cols)))
       (let ([new-cursor-col (remainder
                               (fx- cols (remainder (fx- n cursor-col) cols))
                               cols)])
         (if (fx>= new-cursor-col cursor-col)
             (move-cursor-right (fx- new-cursor-col cursor-col))
             (move-cursor-left (fx- cursor-col new-cursor-col))))]))

  (define wait
    (lambda (ms)
      (unless (or (<= ms 0) (ee-char-ready?))
        (nanosleep 0 (* 10 1000 1000)) ; 10ms granularity is best we can assume
        (wait (- ms 10)))))
)

;;; parameters

(define ee-common-identifiers
  (make-parameter
   ; general theory: exclude short ids and ids that will come up early
   ; in an alphabetical search with short prefix.  include common ids that
   ; come up annoyingly late in such a search.
    '(append apply call/cc call-with-values define display display-string
      define-syntax define-record null? quote quotient reverse read-char
      substring string-ref string-length string? string=? string-set!
      syntax-case syntax-rules unless vector-ref vector-length vector?
      vector-set! vector)
    (lambda (x)
      (unless (and (list? x) (andmap symbol? x))
        ($oops 'ee-common-identifiers "~s is not a list of symbols" x))
      x)))

;;; default repeat value for ^U
(define ee-default-repeat
  (make-parameter 4
    (lambda (x)
      (unless (and (fixnum? x) (fxnonnegative? x))
        ($oops 'ee-default-repeat "~s is not an integer" x))
      x)))

(define ee-auto-indent (make-parameter #t (lambda (x) (and x #t))))

(define ee-auto-paren-balance (make-parameter #t (lambda (x) (and x #t))))

(define ee-flash-parens (make-parameter #t (lambda (x) (and x #t))))

;;; paren balance delay factor in milliseconds
(define ee-paren-flash-delay
  (make-parameter 100
    (lambda (x)
      (unless (and (fixnum? x) (fxnonnegative? x))
        ($oops 'ee-paren-flash-delay "~s is not an integer" x))
      x)))

;;; enable/disable bell
(define ee-noisy (make-parameter #f (lambda (x) (and x #t))))

;;; standard indent length
(define ee-standard-indent
  (make-parameter 2
    (lambda (x)
      (unless (and (fixnum? x) (fxnonnegative? x))
        ($oops 'ee-standard-indent "~s is not an integer" x))
      x)))

(define ee-history-limit
  (make-parameter 256
    (lambda (x)
      (unless (and (fixnum? x) (fxnonnegative? x))
        ($oops 'ee-history-length "~s is not a nonnegative fixnum" x))
      x)))

;;; eestate holds the state of the expression editor.

(define-record-type eestate
  (fields
    (mutable last-op)
    (mutable rt-last-op)
    (mutable prompt)
    (mutable repeat-count)
    (mutable killbuf)
    (mutable histnew)
    (mutable histbwd)
    (mutable histnow)
    (mutable histfwd)
    (mutable histkey)
    (mutable last-suffix*)
    (mutable cc?))
  (nongenerative)
  (sealed #t)
  (protocol
    (lambda (new)
      (lambda ()
        (new #f         ; last-op
             '(0 . 0)   ; rt-last-op
             ""         ; prompt
             1          ; repeat-count
             '()        ; killbuf
             0          ; histnew
             '()        ; histbwd
             ""         ; histnow
             '()        ; hisfwd
             ""         ; histkey
             '()        ; last-suffix*
             #f)))))     ; cc?

(module ()
  (record-writer (type-descriptor eestate)
    (lambda (x p wr)
      (display "#<ee-state>" p))))

;;; pos is used for two different but related purposes: for row, col
;;; positions and for row, physical-line positions.  see the comment
;;; about the entry top-line and bot-line fields below.

(module (make-pos pos? pos-row pos-col pos=? pos<? pos<=? pos>? pos>=? index->pos)
  (define-record-type pos
    (fields (immutable row) (immutable col))
    (nongenerative)
    (sealed #t))
  (define (pos=? p1 p2)
    (and (fx= (pos-row p1) (pos-row p2))
         (fx= (pos-col p1) (pos-col p2))))
  (define (pos<? p1 p2)
    (or (fx< (pos-row p1) (pos-row p2))
        (and (fx= (pos-row p1) (pos-row p2))
             (fx< (pos-col p1) (pos-col p2)))))
  (define (pos<=? p1 p2)
    (or (fx< (pos-row p1) (pos-row p2))
        (and (fx= (pos-row p1) (pos-row p2))
             (fx<= (pos-col p1) (pos-col p2)))))
  (define (pos>? p1 p2)
    (or (fx> (pos-row p1) (pos-row p2))
        (and (fx= (pos-row p1) (pos-row p2))
             (fx> (pos-col p1) (pos-col p2)))))
  (define (pos>=? p1 p2)
    (or (fx> (pos-row p1) (pos-row p2))
        (and (fx= (pos-row p1) (pos-row p2))
             (fx>= (pos-col p1) (pos-col p2)))))
  (define (index->pos s n r c)
   ; convert index in single-string representation of entry
   ; into pos.  r and c are row and col at which string
   ; starts in the entry
    (let f ([i 0] [r r] [c c])
      (if (fx= i n)
          (make-pos r c)
          (if (char=? (string-ref s i) #\newline)
              (f (fx+ i 1) (fx+ r 1) 0)
              (f (fx+ i 1) r (fx+ c 1))))))
  (record-writer (type-descriptor pos)
    (lambda (x p wr)
      (fprintf p "#<pos ~a ~a>" (pos-row x) (pos-col x))))
)

(define lpchar #\()
(define rpchar #\))
(define lbchar #\[)
(define rbchar #\])

(define beep
  (lambda (str . arg*)
    #;(with-output-to-file "/tmp/ee.log"
        (lambda () (apply printf str arg*) (newline))
        'append)
    (when (ee-noisy) (bell))))

(module (string->entry
         entry->string
         string->lines
        ; primtiive and derived record accessors and mutators: no ee argument
         entry-col
         entry-nsr
         entry-row
         entry-mark
         entry-point
         null-entry?
         entry-mark-set!
         entry-row-set!
         entry-col-set!
        ; normal entry procedures: first two arguments are ee and entry
         add-char
         beginning-of-line?
         clear-entry
         id-completions
         correct&flash-matching-delimiter
         yank-entry
         delete-char
         delete-forward
         delete-to-eol
         echo-entry
         end-of-line?
         find-matching-delimiter
         find-next-sexp-backward
         find-next-sexp-forward
         find-next-word
         find-previous-word
         first-line?
         flash
         goto
         handle-winch
         indent
         indent-all
         insert-string-before
         insert-strings-before
         join-rows
         last-line?
         last-line-displayed?
         move-bol
         move-down
         move-eoe
         move-eol
         move-left
         move-right
         move-up
         only-whitespace-left?
         page-down
         page-up
         redisplay
         should-auto-indent?)

  ; NB. top-line and bot-line aren't really positions.
  ; the row does identify the logical row, but the col identifies the
  ; physical row of the logical row, i.e., 0 for the first physical
  ; row, 1 for the second, etc.

  (define-record-type entry
    (fields
      (immutable lns)               ; logical lines
      (mutable row)                 ; point (logical cursor) row
      (mutable col)                 ; point (logical cursor) column
      (mutable screen-cols)         ; cached screen columns
      (mutable screen-rows)         ; cached screen rows
      (mutable top-line)            ; first displayed line
      (mutable bot-line)            ; last displayed line
      (mutable mark))               ; current mark pos
    (nongenerative)
    (sealed #t)
    (protocol
      (lambda (new)
        (lambda (lns)
          (new lns 0 0 (screen-cols) (screen-rows)
            (make-pos 0 0) (make-pos 0 0) #f)))))

  (module ()
    (record-writer (type-descriptor entry)
      (lambda (x p wr)
        (display "#<ee entry>" p))))

  (define (entry-point entry)
    (make-pos (entry-row entry) (entry-col entry)))

  ;;; an lns is a nonempty list of logical lines, each of which may span
  ;;; multiple screen lines.  each line consists of an integer that records
  ;;; the number of screen rows spanned along with a string containing
  ;;; the text of the line.  lines are implicitly separated by newlines; no
  ;;; newlines appear in the strings themselves.
  ;;;
  ;;; lns   := (ln ln ...)  ;;; list of "ln"s
  ;;; ln    := [nsr, str]
  ;;; nsr   := integer      ;;; number of screen rows occupied by the line
  ;;; str   := string       ;;; contents of the line

 ; arrange for nsr to be updated whenever str is changed
  (module (make-ln ln? ln-str ln-nsr ln-str-set! ln-nsr-set!)
    (define-record-type (ln make-ln ln?)
      (fields
        (mutable str ln-str $ln-str-set!)
        (mutable nsr ln-nsr ln-nsr-set!))
      (nongenerative)
      (sealed #t)
      (protocol
        (lambda (new)
          (lambda (ee str)
            (new str (str->nsr ee str))))))
    (define (ln-str-set! ee ln str)
      ($ln-str-set! ln str)
      (ln-nsr-set! ln (str->nsr ee str))))

 ; extract nsr or str from selected row of lns
  (define (lns->nsr lns row) (ln-nsr (list-ref lns row)))
  (define (lns->str lns row) (ln-str (list-ref lns row)))

 ; replace str in selected row of lns
  (define (lns->str! ee lns row str)
    (ln-str-set! ee (list-ref lns row) str))

  (define (lns->char lns row col)
    (let ([str (lns->str lns row)])
      (if (fx< col (string-length str))
          (string-ref str col)
          #f)))

  (define (yank-entry ee entry)
    (map ln-str (entry-lns entry)))

  (define (entry->string entry)
    (let* ([lns (entry-lns entry)] [n (length lns)])
      (let ([sop (open-output-string)])
        (let loop ([i 0] [sep ""])
          (unless (fx= i n)
            (fprintf sop "~a~a" sep (lns->str lns i))
            (loop (fx+ i 1) "\n")))
        (get-output-string sop))))

  (define (echo-entry ee entry tp)
    (display-string (eestate-prompt ee) tp)
    (let ([lns (entry-lns entry)])
      (fprintf tp "~a\n" (ln-str (car lns)))
      (for-each
        (let ([pad (phantom-prompt ee)])
          (lambda (ln) (fprintf tp "~a~a\n" pad (ln-str ln))))
        (cdr lns)))
    (flush-output-port tp))

  (define (string->lines s)
   ; break string into list of lines while expanding tabs
    (let ([n (string-length s)] [op (open-output-string)])
      (let f ([i 0] [col 0])
        (if (fx= i n)
            (list (get-output-string op))
            (let ([c (string-ref s i)])
              (case c
                [(#\newline)
                 (let ([line (get-output-string op)])
                   (cons line (f (fx+ i 1) 0)))]
                [(#\tab)
                 (do ([i (fx- 8 (fxmodulo col 8)) (fx- i 1)])
                     ((fx= i 0))
                   (write-char #\space op))
                 (f (fx+ i 1) 0)]
                [(#\return) (f (fx+ i 1) col)]
                [else (write-char c op) (f (fx+ i 1) (fx+ col 1))]))))))

  (define (string->entry ee s)
    (let ([ln* (map (lambda (str) (make-ln ee str)) (string->lines s))])
      (make-entry (if (null? ln*) (list (make-ln ee "")) ln*))))

  (define (null-entry? entry)
    (let ([lns (entry-lns entry)])
      (and (fx= (length lns) 1)
           (equal? (ln-str (car lns)) ""))))

  (define (entry-nsr entry) (apply fx+ (map ln-nsr (entry-lns entry))))

  ;;; split a logical row into a list of strings each of which will fit
  ;;; on a screen line, starting at logical column col.
  (define split-string
    (lambda (ee str col)
      (let ([str-len (string-length str)])
        (let f ([col col] [width (fx- (screen-cols) (col->screen-col ee col))])
          (if (fx< (fx- str-len col) width)
              (list (substring str col str-len))
              (cons (substring str col (fx+ col width))
                    (f (fx+ col width) (screen-cols))))))))

  (define (screen-lines-between ee entry toprow topoff nextrow nextoff)
   ; returns distance in physical screen lines between physical line
   ; topoff of toprow and nextoff of nextrow
    (let ([lns (entry-lns entry)])
      (let f ([i toprow] [off topoff] [lns (list-tail lns toprow)])
        (if (fx= i nextrow)
            (fx- nextoff off)
            (fx+ (fx- (ln-nsr (car lns)) off)
                 (f (fx+ i 1) 0 (cdr lns)))))))

  (define (str->nsr ee str)
    (fx+ (col->line-offset ee (string-length str)) 1))

  ;;; return the line offset based on the column and screen size
  ;;;                                      ||-offset=2 (prompt)
  ;;; example: if: col         = 15        vv
  ;;;              offset      = 2         -----------
  ;;;              scrn-cols   = 10       |> line-000| line-offset 0
  ;;;          then:                      |line-11111| line-offset 1
  ;;;              line-offset = 1                ^column = 15
  (define col->line-offset
    (lambda (ee col)
      (fxquotient (fx+ (string-length (eestate-prompt ee)) col) (screen-cols))))

  ;;; return the actual screen column based on the logical row column
  ;;; example: if: col         = 15        vv-offset=2 (prompt)
  ;;;              offset      = 2         -----------
  ;;;              scrn-cols   = 10       |> line-000| line-offset 0
  ;;;          then:                      |line-11111| line-offset 1
  ;;;              scrn-col    = 7                ^column = 15
  (define col->screen-col
    (lambda (ee col)
      (fxremainder (fx+ col (string-length (eestate-prompt ee))) (screen-cols))))

  (define (clear-entry ee entry)
   ; like clear-screen, but clears only from top line of entry
    (if (visible? ee entry 0 0)
        (begin
          (carriage-return)
          (move-cursor-up
            (let ([top-line (entry-top-line entry)])
              (screen-lines-between ee entry
                (pos-row top-line) (pos-col top-line)
                (entry-row entry) (col->line-offset ee (entry-col entry)))))
          (clear-eos))
        (clear-screen)))

  ;;; given bottom line displayed, determines top line that will fill
  ;;; the screen to the extent possible
  (defopt (calc-top-line-displayed entry last-row-pos [nrows (screen-rows)])
    (let ([lns (entry-lns entry)])
      (let loop ([n nrows]
                 [r (pos-row last-row-pos)]
                 [off (pos-col last-row-pos)])
        (if (or (fx= n 1) (and (fx= r 0) (fx= off 0)))
            (make-pos r off)
            (if (fx= off 0)
                (loop (fx- n 1) (fx- r 1) (fx- (lns->nsr lns (fx- r 1)) 1))
                (loop (fx- n 1) r (fx- off 1)))))))

  ;;; given first line displayed, determines bottom line that will fill
  ;;; the screen to the extent possible
  (defopt (calc-bot-line-displayed entry first-row-pos [nrows (screen-rows)])
    (let* ([lns (entry-lns entry)]
           [last-row (fx- (length lns) 1)]
           [last-off (fx- (lns->nsr lns last-row) 1)]
           [first-row (pos-row first-row-pos)])
      (let loop ([n nrows]
                 [r first-row]
                 [off (pos-col first-row-pos)]
                 [off-max (fx- (lns->nsr lns first-row) 1)])
        (if (or (fx= n 1) (and (fx= r last-row) (fx= off last-off)))
            (make-pos r off)
            (if (fx= off off-max)
                (loop (fx- n 1) (fx+ r 1) 0 (fx- (lns->nsr lns (fx+ r 1)) 1))
                (loop (fx- n 1) r (fx+ off 1) off-max))))))

 ; NB.  the macos x terminal app distinguishes between empty screen
 ; positions (e.g., after clr_eos or clr_eol) and screen positions filled
 ; with spaces.  attempts to move past and clear after the former result
 ; in strange behavior.  (For example, the sequence clr_eos, cursor_right,
 ; clr_eol, 'a', clr_eol, and 'b' doesn't print the b but does cause the
 ; terminal to send back some characters.  Using ' ' in place of the
 ; cursor_right works as expected.)  For this reason, we display spaces
 ; and avoid using move-cursor-right to pad the front of each row after
 ; the first, which gets the actual prompt.

  (define (phantom-prompt ee)
    (make-string (string-length (eestate-prompt ee)) #\space))

  (module (display-rest/goto)
    (define (display-rest-of-line ee entry row col clear?)
     ; display as much of the rest of row as will fit on the screen
      (let ([lns (entry-lns entry)] [bot-line (entry-bot-line entry)])
       ; n = number of lines to display beyond the first
        (let loop ([n (fx- (if (fx= row (pos-row bot-line))
                               (pos-col bot-line)
                               (fx- (lns->nsr lns row) 1))
                           (col->line-offset ee col))]
                   [str-lst (split-string ee (lns->str lns row) col)]
                   [new-col col])
          (when clear? (clear-eol))
          (let ([str (car str-lst)])
            (ee-display-string (car str-lst))
            (let ([new-col (fx+ new-col (string-length str))])
              (if (fx= n 0)
                  new-col
                  (begin
                    (carriage-return)
                    (line-feed)
                    (loop (fx- n 1) (cdr str-lst) new-col))))))))
  
    (define (display-rest-of-entry ee entry)
      (let ([row (entry-row entry)]
            [col (entry-col entry)]
            [bot-row (pos-row (entry-bot-line entry))])
        (let loop ([new-row row] [start-col col])
          (let ([new-col (display-rest-of-line ee entry new-row start-col #f)])
            (if (fx= new-row bot-row)
                (values new-row new-col)
                (begin
                  (carriage-return)
                  (line-feed)
                  (ee-display-string (phantom-prompt ee))
                  (loop (fx+ new-row 1) 0)))))))
  
    (define (display-rest/goto ee entry just-row? clear? to-row to-col)
     ; display rest of entry and go directly from there to (to-row, to-col)
     ; just-row? => only remainder of current logical row needed by displayed
     ; clear? => clear-eos or clear-eol needed
      (let-values ([(cur-row cur-col)
                    (if just-row?
                        (values
                          (entry-row entry)
                          (display-rest-of-line ee entry
                            (entry-row entry) (entry-col entry) clear?))
                        (begin
                          (entry-bot-line-set! entry
                            (calc-bot-line-displayed entry
                              (entry-top-line entry)))
                          (when clear? (clear-eos))
                          (display-rest-of-entry ee entry)))])
        (unless (and (fx= cur-row (entry-row entry))
                     (fx= cur-col (entry-col entry)))
          (entry-row-set! entry cur-row)
         ; if the last character written was in the last column of a screen
         ; line, move back one so that the cursor is pointing at that character
         ; to avoid returning a column value that would wrongly indicate that
         ; the cursor is at the start of the next screen line
          (if (and (fx> cur-col 0) (fx= (col->screen-col ee cur-col) 0))
              (begin
                (move-cursor-left 1)
                (entry-col-set! entry (fx- cur-col 1)))
              (entry-col-set! entry cur-col)))
        (goto ee entry (make-pos to-row to-col)))))

  (module (display-partial-entry)
    (define (display-partial-row ee row str start end)
     ; displays physical lines of str from start (inclusive) to end (inclusive)
     ; assumes cursor is at column zero of start line; leaves cursor at
     ; column zero of end line
      (let ([ls (list-tail (split-string ee str 0) start)])
        (when (fx= start 0)
          (ee-display-string
            (if (fx= row 0)
                (eestate-prompt ee)
                (phantom-prompt ee))))
        (ee-display-string (car ls))
        (carriage-return)
        (do ([i start (fx+ i 1)] [ls (cdr ls) (cdr ls)])
            ((fx= i end))
          (line-feed)
          (ee-display-string (car ls))
          (carriage-return))))

    (define (display-partial-entry ee entry toprow topoff botrow botoff)
     ; displays physical screen lines between physical line topoff of
     ; toprow (inclusive) and botoff of botrow (inclusive)
     ; assumes cursor is at column zero of first physical line to be displayed;
     ; leaves cursor at column zero of last line displayed
      (let ([lns (entry-lns entry)])
        (let loop ([r toprow] [start topoff] [lns (list-tail lns toprow)])
          (display-partial-row ee r (ln-str (car lns)) start
            (if (fx= r botrow) botoff (fx- (ln-nsr (car lns)) 1)))
          (unless (fx= r botrow)
            (line-feed)
            (loop (fx+ r 1) 0 (cdr lns)))))))

  (define (goto-backward ee entry new-row new-col)
    (assert* (fx>= new-row 0)
             (fx>= new-col 0)
             (fx<= new-col (string-length (lns->str (entry-lns entry) new-row))))
    (let* ([lns (entry-lns entry)]
           [row (entry-row entry)]
           [col (entry-col entry)]
           [top-line (entry-top-line entry)]
           [new-str (lns->str lns new-row)]
           [new-len (string-length new-str)]
           [new-row-offset (col->line-offset ee new-col)]
           [new-row-pos (make-pos new-row new-row-offset)]
           [new-bot-line (calc-bot-line-displayed entry new-row-pos)])
      (cond
       ; case 1: destination on screen, no scrolling necessary
       ;  ------------------
       ; | (define fact     | <--top-line
       ; |   (lambda (n)    | <--new-row
       ; |     (if (zero? n)| <--point
       ; |         1        |
       ; |         (* n (fac|
       ; |t (sub1 n))))))   | <--bot-line
       ; |                  |
       ;  ------------------
        [(pos>=? new-row-pos (entry-top-line entry))
         (move-cursor-up
           (screen-lines-between ee entry
             new-row new-row-offset
             (entry-row entry) (col->line-offset ee (entry-col entry))))
         (let ([screen-col (col->screen-col ee col)]
               [new-screen-col (col->screen-col ee new-col)])
           (cond
             [(fx> new-screen-col screen-col)
              (move-cursor-right (fx- new-screen-col screen-col))]
             [(fx< new-screen-col screen-col)
              (move-cursor-left (fx- screen-col new-screen-col))]))
         (entry-row-set! entry new-row)
         (entry-col-set! entry new-col)]

       ; case 2: a portion of the old screen overlaps the new screen.
       ;         we will scroll down and keep the overlap instead of
       ;         redrawing
       ;         + = new screen border
       ;         - = old-screen border
       ;  ++++++++++++++++++
       ; | (define f        | <--new-row        0         }extra-top-
       ; |   (lambda (n)    |                             }lines
       ;  ------------------
       ; |     (if (zero? n)| <--top-line       (2 . 0)
       ; |         1        | <--point          (row . col)
       ; |         (* n     | <--new-bot-line   (4 . 0)
       ;  ++++++++++++++++++
       ; |            (f    | <--bot-line       (5 . 0)
       ; |              (1- |
       ;  ------------------
       ;                  n))))))
        [(pos>? new-bot-line (entry-top-line entry))
        ; move cursor to physical column 0 of top screen line
         (move-cursor-up
           (screen-lines-between ee entry
             (pos-row top-line) (pos-col top-line)
             row (col->line-offset ee col)))
         (carriage-return)
         (let ([extra-top-lines
                (screen-lines-between ee entry
                  new-row new-row-offset
                  (pos-row top-line) (pos-col top-line))])
          ; reverse scroll to open up space at the top
          ; if we're not actually at the top of the physical display, e.g.,
          ; if we only partially displayed the entry after an error or tab-tab,
          ; we hope that this goes up a line and clears to end of line.  if
          ; this ever gives us problems, we'll have avoid getting into this
          ; case when less than a screenful of lines has been displayed.
           (scroll-reverse extra-top-lines)
          ; display the extra lines
           (let ([r (pos-row top-line)] [off (fx- (pos-col top-line) 1)])
             (if (fx>= off 0)
                 (display-partial-entry ee entry new-row new-row-offset r off)
                 (display-partial-entry ee entry new-row new-row-offset
                   (fx- r 1) (fx- (lns->nsr lns (fx- r 1)) 1))))
          ; move cursor back to top
           (move-cursor-up (fx- extra-top-lines 1)))
         (move-cursor-right (col->screen-col ee new-col))
         (entry-col-set! entry new-col)
         (entry-row-set! entry new-row)
         (entry-top-line-set! entry new-row-pos)
         (when (pos<? new-bot-line (entry-bot-line entry))
           (entry-bot-line-set! entry new-bot-line))]

       ; case 3: no overlap between old screen area and new screen
       ;         area. we will redraw the entire screen
       ; + = new screen border
       ; - = old-screen border
       ;  ++++++++++++++++++
       ; | (define f        | <--new-row        0
       ; |   (lambda (n)    |
       ; |     (if (zero? n)| <--new-bot-line   (4 . 0)
       ;  ++++++++++++++++++
       ;  ------------------
       ; |         1        | <--top-line       (2 . 0)
       ; |         (* n     |
       ; |            (f    | <--bot-line       (4 . 1)
       ;  ------------------
       ;                (1-
       ;                  n))))))
        [else
         (clear-screen)
         (display-partial-entry ee entry
           new-row new-row-offset
           (pos-row new-bot-line) (pos-col new-bot-line))
         (move-cursor-up
           (screen-lines-between ee entry
             new-row new-row-offset
             (pos-row new-bot-line) (pos-col new-bot-line)))
         (move-cursor-right (col->screen-col ee new-col))
         (entry-col-set! entry new-col)
         (entry-row-set! entry new-row)
         (entry-top-line-set! entry new-row-pos)
         (entry-bot-line-set! entry new-bot-line)])))

  (define (goto-forward ee entry new-row new-col)
    (assert* (fx< new-row (length (entry-lns entry)))
             (fx>= new-col 0)
             (fx<= new-col (string-length (lns->str (entry-lns entry) new-row))))
    (let* ([lns (entry-lns entry)]
           [row (entry-row entry)]
           [col (entry-col entry)]
           [bot-line (entry-bot-line entry)]
           [new-str (lns->str lns new-row)]
           [new-len (string-length new-str)]
           [new-row-offset (col->line-offset ee new-col)]
           [new-row-pos (make-pos new-row new-row-offset)]
           [new-top-line (calc-top-line-displayed entry new-row-pos)])
      (cond
       ; case 1: destination on screen, no scrolling necessary
       ;  ------------------
       ; | (define fact     | <--top-line
       ; |   (lambda (n)    | <--point
       ; |     (if (zero? n)| <--new-row
       ; |         1        |
       ; |         (* n (fac|
       ; |t (sub1 n))))))   | <--bot-line
       ; |                  |
       ;  ------------------
        [(pos<=? new-row-pos bot-line)
         (move-cursor-down
           (screen-lines-between ee entry
             row (col->line-offset ee col)
             new-row new-row-offset))
         (let ([screen-col (col->screen-col ee col)]
               [new-screen-col (col->screen-col ee new-col)])
           (cond
             [(fx> new-screen-col screen-col)
              (move-cursor-right (fx- new-screen-col screen-col))]
             [(fx< new-screen-col screen-col)
              (move-cursor-left (fx- screen-col new-screen-col))]))
         (entry-row-set! entry new-row)
         (entry-col-set! entry new-col)]

       ; case 2: a portion of the old screen overlaps the new screen.
       ;         we will scroll up and keep the overlap
       ;
       ;         + = new screen border
       ;         - = old-screen border
       ;  ------------------
       ; | (define f        | <--top-line       (0 . 0)
       ; |   (lambda (n)    |
       ;  ++++++++++++++++++
       ; |     (if (zero? n)| <--new-top-line               } scrn-
       ; |         1        | <--point          (row . col) } draw-
       ; |         (* n     | <--bot-line       (4 . 0)     } lines
       ;  ------------------
       ; |            (f    |
       ; |              (1- | <--new-row        6
       ;  ++++++++++++++++++
       ;                  n))))))
        [(pos>=? bot-line new-top-line)
        ; move cursor to physical col 0 of first line after old bot-line
         (move-cursor-down
           (screen-lines-between ee entry
             row (col->line-offset ee col)
             (pos-row bot-line) (pos-col bot-line)))
         (carriage-return)
         (line-feed)
         (let ([r (pos-row bot-line)] [off (fx+ (pos-col bot-line) 1)])
           (if (fx< off (lns->nsr lns r))
               (display-partial-entry ee entry r off
                 new-row new-row-offset)
               (display-partial-entry ee entry (fx+ r 1) 0
                 new-row new-row-offset)))
         (move-cursor-right (col->screen-col ee new-col))
         (entry-col-set! entry new-col)
         (entry-row-set! entry new-row)
         (when (pos>? new-top-line (entry-top-line entry))
           (entry-top-line-set! entry new-top-line))
         (entry-bot-line-set! entry new-row-pos)]

       ; case 3: no overlap between old screen area and new screen
       ;         area. we will redraw the entire screen
       ; + = new screen border
       ; - = old-screen border
       ;  ++++++++++++++++++
       ; | (define f        | <--top-line       (0 . 0)
       ; |   (lambda (n)    |
       ; |     (if (zero? n)| <--bot-line       (2 . 0)
       ;  ++++++++++++++++++
       ;  ------------------
       ; |         1        | <--new-top-line
       ; |         (* n     |
       ; |            (f    | <--new-row, new-row-offset
       ;  ------------------
       ;                (1-
       ;                  n))))))
        [else
         (clear-screen)
         (display-partial-entry ee entry
           (pos-row new-top-line) (pos-col new-top-line)
           new-row new-row-offset)
         (move-cursor-right (col->screen-col ee new-col))
         (entry-col-set! entry new-col)
         (entry-row-set! entry new-row)
         (entry-top-line-set! entry new-top-line)
         (entry-bot-line-set! entry new-row-pos)])))

  (define (goto ee entry p)
    (let ([new-row (pos-row p)] [new-col (pos-col p)])
      (assert* (fx< new-row (length (entry-lns entry)))
               (fx<= new-col (string-length (lns->str (entry-lns entry) new-row))))
      (if (or (fx< new-row (entry-row entry))
              (and (fx= new-row (entry-row entry))
                   (fx< new-col (entry-col entry))))
          (goto-backward ee entry new-row new-col)
          (goto-forward ee entry new-row new-col))))

  (defopt (move-up ee entry [n 1])
    (assert* (fx>= (fx- (entry-row entry) n) 0))
    (let ([new-row (fx- (entry-row entry) n)])
      (goto-backward ee entry new-row
        (fxmin (entry-col entry)
               (string-length (lns->str (entry-lns entry) new-row))))))

  (defopt (move-down ee entry [n 1])
    (assert* (fx< (fx+ (entry-row entry) n) (length (entry-lns entry))))
    (let ([new-row (fx+ (entry-row entry) n)])
      (goto-forward ee entry new-row
        (fxmin (entry-col entry)
               (string-length (lns->str (entry-lns entry) new-row))))))

  (defopt (move-left ee entry [n 1])
    (let ([new-col (fx- (entry-col entry) n)])
      (assert* (fx>= new-col 0))
      (goto-backward ee entry (entry-row entry) new-col)))

  (defopt (move-right ee entry [n 1])
    (let ([new-col (fx+ (entry-col entry) n)])
      (assert* (fx<= new-col (string-length (lns->str (entry-lns entry) (entry-row entry)))))
      (goto-forward ee entry (entry-row entry) new-col)))

  (define (page-down ee entry)
    (let* ([last-row (fx- (length (entry-lns entry)) 1)]
           [row (entry-row entry)]
           [col (entry-col entry)]
           [point-line-offset (col->line-offset ee col)]
           [top-line (entry-top-line entry)]
           [bot-line (entry-bot-line entry)]
           [n (screen-lines-between ee entry
                (pos-row top-line) (pos-col top-line)
                (pos-row bot-line) (pos-col bot-line))])
        (let f ([r (fxmin (fx+ row n) last-row)])
          (if (fx= r row)
              (unless (fx= r last-row)
                (goto-forward ee entry (fx+ r 1)
                  (fxmin col (string-length (lns->str (entry-lns entry) (fx+ r 1))))))
              (let ([c (fxmin col (string-length (lns->str (entry-lns entry) r)))])
                (if (<= (screen-lines-between ee entry
                          row point-line-offset
                          r (col->line-offset ee c))
                        n)
                    (goto-forward ee entry r c)
                    (f (fx- r 1))))))))

  (define (page-up ee entry)
    (let* ([row (entry-row entry)]
           [col (entry-col entry)]
           [point-line-offset (col->line-offset ee col)]
           [top-line (entry-top-line entry)]
           [bot-line (entry-bot-line entry)]
           [n (screen-lines-between ee entry
                (pos-row top-line) (pos-col top-line)
                (pos-row bot-line) (pos-col bot-line))])
        (let f ([r (max (fx- row n) 0)])
          (if (fx= r row)
              (unless (fx= r 0)
                (goto-backward ee entry (fx- r 1)
                  (fxmin col (string-length (lns->str (entry-lns entry) (fx- r 1))))))
              (let ([c (fxmin col (string-length (lns->str (entry-lns entry) r)))])
                (if (<= (screen-lines-between ee entry
                          r (col->line-offset ee c)
                          row point-line-offset)
                        n)
                    (goto-backward ee entry r c)
                    (f (fx+ r 1))))))))

  (define (move-eol ee entry)
    (move-right ee entry
      (fx- (string-length (lns->str (entry-lns entry) (entry-row entry)))
           (entry-col entry))))

  (define (move-bol ee entry)
    (move-left ee entry (entry-col entry)))

  (define (move-eoe ee entry)
    (let ([lns (entry-lns entry)])
      (let ([r (fx- (length lns) 1)])
        (goto-forward ee entry r (string-length (lns->str lns r))))))

  (define (move-to-col-pos ee entry new-col)
    (let ([col (entry-col entry)])
      (if (fx< new-col col)
          (move-left ee entry (fx- col new-col))
          (move-right ee entry (fx- new-col col)))))

  (define (adjust-mark/delete ee entry r1 c1 r2 c2)
    (let ([mark (entry-mark entry)])
      (when mark
        (let ([mrow (pos-row mark)] [mcol (pos-col mark)])
          (unless (or (fx< mrow r1) (and (fx= mrow r1) (fx< mcol c1)))
            (entry-mark-set! entry
              (and (not (or (fx< mrow r2) (and (fx= mrow r2) (fx< mcol c2))))
                   (make-pos
                     (fx- mrow (fx- r2 r1))
                     (if (fx= mrow r2) (fx+ c1 (fx- mcol c2)) mcol)))))))))

  (define (adjust-mark/insert ee entry r1 c1 r2 c2)
    (let ([mark (entry-mark entry)])
      (when mark
        (let ([mrow (pos-row mark)] [mcol (pos-col mark)])
          (unless (or (fx< mrow r1) (and (fx= mrow r1) (fx< mcol c1)))
            (entry-mark-set! entry
              (make-pos
                (fx+ mrow (fx- r2 r1))
                (if (fx= mrow r1) (fx+ c2 (fx- mcol c1)) mcol))))))))

  (define (delete-forward ee entry r2 c2)
   ; deletes from point, aka r1, c1 (inclusive) to r2, c2 (exclusive)
   ; and returns the deleted content as a list of strings
    (let ([r1 (entry-row entry)] [c1 (entry-col entry)])
      (assert* (or (fx< r1 r2) (and (fx= r1 r2) (fx<= c1 c2))))
      (adjust-mark/delete ee entry r1 c1 r2 c2)
      (if (fx= r1 r2)
          (let* ([ln (list-ref (entry-lns entry) r1)]
                 [s (ln-str ln)]
                 [old-nsr (ln-nsr ln)])
            (ln-str-set! ee ln
              (string-append
                (substring s 0 c1)
                (substring s c2 (string-length s))))
            (display-rest/goto ee entry (fx= (ln-nsr ln) old-nsr) #t r1 c1)
            (list (substring s c1 c2)))
          (let* ([lns (entry-lns entry)]
                 [ls1 (list-tail lns r1)]
                 [ls2 (list-tail ls1 (fx- r2 r1))]
                 [s1 (ln-str (car ls1))]
                 [s2 (ln-str (car ls2))])
            (ln-str-set! ee (car ls1)
              (string-append
                (substring s1 0 c1)
                (substring s2 c2 (string-length s2))))
            (let ([deleted
                   (cons (substring s1 c1 (string-length s1))
                         (let f ([ls (cdr ls1)])
                           (if (eq? ls ls2)
                               (list (substring s2 0 c2))
                               (cons (ln-str (car ls)) (f (cdr ls))))))])
              (set-cdr! ls1 (cdr ls2))
              (display-rest/goto ee entry #f #t r1 c1)
              deleted)))))

  (define (delete-char ee entry)
    (assert* (not (end-of-line? ee entry)))
    (let ([row (entry-row entry)] [col (entry-col entry)])
      (delete-forward ee entry row (fx+ col 1))))

  (define (delete-to-eol ee entry)
    (let ([row (entry-row entry)])
      (delete-forward ee entry row
        (string-length (lns->str (entry-lns entry) row)))))

  (define (join-rows ee entry)
    (assert* (end-of-line? ee entry) (not (last-line? ee entry)))
    (delete-forward ee entry (fx+ (entry-row entry) 1) 0))

  (define (insert-string-before ee entry new-str)
    (let* ([row (entry-row entry)]
           [col (entry-col entry)]
           [lns (entry-lns entry)]
           [ln (list-ref lns row)]
           [str (ln-str ln)]
           [str-len (string-length str)]
           [new-col (fx+ col (string-length new-str))]
           [nsr (ln-nsr ln)]
           [eoe? (end-of-entry? ee entry)])
      (ln-str-set! ee ln
        (string-append
          (substring str 0 col)
          new-str
          (substring str col (string-length str))))
      (let ([just-row? (fx= (ln-nsr ln) nsr)])
        (display-rest/goto ee entry just-row?
         ; avoid clear-eol/eos if insertion takes place at end of entry or
         ; if rewriting just the current row
          (and (not eoe?) (not just-row?))
          row new-col))
      (adjust-mark/insert ee entry row col row new-col)))

  (define (add-char ee entry c)
   ; add character after point, then move point forward one character
    (assert* (char? c))
    (insert-string-before ee entry (string c)))

  (define (insert-strings-before ee entry strs)
    (unless (null? strs)
      (if (fx= (length strs) 1)
          (insert-string-before ee entry (car strs))
          (let* ([row (entry-row entry)]
                 [col (entry-col entry)]
                 [lns (entry-lns entry)]
                 [ls (list-tail lns row)]
                 [ln (car ls)]
                 [point-str (ln-str ln)]
                 [eoe? (end-of-entry? ee entry)])
            (ln-str-set! ee ln
              (string-append (substring point-str 0 col) (car strs)))
            (set-cdr! ls
              (let f ([str (cadr strs)] [strs (cddr strs)])
                (if (null? strs)
                    (cons (make-ln ee
                            (string-append str
                              (substring point-str col
                                (string-length point-str))))
                          (cdr ls))
                    (cons (make-ln ee str)
                          (f (car strs) (cdr strs))))))
            (let* ([n (fx- (length strs) 1)]
                   [new-row (fx+ row n)]
                   [new-col (string-length (list-ref strs n))])
              (display-rest/goto ee entry #f (not eoe?) new-row new-col)
              (adjust-mark/insert ee entry row col new-row new-col))))))

  (define (first-line? ee entry) (fxzero? (entry-row entry)))

  (define (last-line? ee entry)
    (fx= (entry-row entry) (fx1- (length (entry-lns entry)))))

  (define (last-line-displayed? ee entry)
    (pos=? (make-pos (entry-row entry) (col->line-offset ee (entry-col entry)))
           (entry-bot-line entry)))

  (define (visible? ee entry row col)
    (let ([line (make-pos row (col->line-offset ee col))])
      (and (pos<=? (entry-top-line entry) line)
           (pos<=? line (entry-bot-line entry)))))

  (define (end-of-line? ee entry)
    (fx= (entry-col entry)
         (string-length (lns->str (entry-lns entry) (entry-row entry)))))

  (define (end-of-entry? ee entry)
    (and (fx= (entry-row entry) (fx- (length (entry-lns entry)) 1))
         (end-of-line? ee entry)))

  (define (beginning-of-line? ee entry) (fx= (entry-col entry) 0))

 ; returns #t iff only spaces and newlines are left after point
  (define (only-whitespace-left? ee entry)
    (let f ([ls (list-tail (entry-lns entry) (entry-row entry))]
            [col (entry-col entry)])
      (or (null? ls)
          (let* ([s (ln-str (car ls))] [n (string-length s)])
            (let g ([col col])
              (if (fx= col n)
                  (f (cdr ls) 0)
                  (and (char=? (string-ref s col) #\space)
                       (g (fx+ col 1)))))))))

  (define (handle-winch ee entry)
    (screen-resize!)
    (unless (and (fx= (screen-rows) (entry-screen-rows entry))
                 (fx= (screen-cols) (entry-screen-cols entry)))
      (clear-entry ee entry)
      (redisplay ee entry)))

  (module (redisplay)
    (define (set-screen-size! ee entry)
      (screen-resize!)
      (unless (and (fx= (entry-screen-cols entry) (screen-cols))
                   (fx= (entry-screen-rows entry) (screen-rows)))
        (for-each
          (lambda (ln) (ln-nsr-set! ln (str->nsr ee (ln-str ln))))
          (entry-lns entry))
        (entry-screen-cols-set! entry (screen-cols))
        (entry-screen-rows-set! entry (screen-rows))))

    (defopt (redisplay ee entry [nrows #f])
      (set-screen-size! ee entry)
      (let* ([nrows (or nrows (screen-rows))] ; want new screen-rows
             [row (entry-row entry)]
             [col (entry-col entry)]
             [point-line (make-pos row (col->line-offset ee col))])
        (entry-bot-line-set! entry
          (calc-bot-line-displayed entry (entry-top-line entry) nrows))
        (when (pos>? point-line (entry-bot-line entry))
          (entry-bot-line-set! entry point-line))
        (entry-top-line-set! entry
          (calc-top-line-displayed entry (entry-bot-line entry) nrows))
        (when (pos<? point-line (entry-top-line entry))
          (entry-top-line-set! entry point-line)
          (entry-bot-line-set! entry
            (calc-bot-line-displayed entry (entry-top-line entry) nrows)))
        (let ([top-line (entry-top-line entry)] [bot-line (entry-bot-line entry)])
          (display-partial-entry ee entry
            (pos-row top-line) (pos-col top-line)
            (pos-row bot-line) (pos-col bot-line))
          (move-cursor-up
            (screen-lines-between ee entry
              (pos-row point-line) (pos-col point-line)
              (pos-row bot-line) (pos-col bot-line)))
          (move-cursor-right (col->screen-col ee col))))))

  (define (flash ee entry mpos)
    (let ([point-pos (entry-point entry)])
      (cond
        [(visible? ee entry (pos-row mpos) (pos-col mpos))
         (goto ee entry mpos)
         (ee-flush)
         (wait (ee-paren-flash-delay))
         (goto ee entry point-pos)]
        [(pos<? mpos point-pos)
         (let ([nlines
                (screen-lines-between ee entry
                  (pos-row (entry-top-line entry))
                  (pos-col (entry-top-line entry))
                  (pos-row point-pos)
                  (col->line-offset ee (pos-col point-pos)))]
               [ncols (col->screen-col ee (pos-col point-pos))])
           (move-cursor-left ncols)
           (move-cursor-up nlines)
           (ee-flush)
           (wait (ee-paren-flash-delay))
           (move-cursor-down nlines)
           (move-cursor-right ncols))]
        [else
         (let ([nlines
                (screen-lines-between ee entry
                  (pos-row point-pos)
                  (col->line-offset ee (pos-col point-pos))
                  (pos-row (entry-bot-line entry))
                  (pos-col (entry-top-line entry)))]
               [ncols (col->screen-col ee (pos-col point-pos))])
           (move-cursor-left ncols)
           (move-cursor-down nlines)
           (ee-flush)
           (wait (ee-paren-flash-delay))
           (move-cursor-up nlines)
           (move-cursor-right ncols))])))

  (define (correct&flash-matching-delimiter ee entry)
    (define (expected left) (if (eqv? left lbchar) rbchar rpchar))
    (move-left ee entry 1) ; move over delim
    (let ([lns (entry-lns entry)])
      (let* ([row (entry-row entry)]
             [col (entry-col entry)]
             [str (lns->str lns row)]
             [c (string-ref str col)])
        (if (or (char=? c lpchar) (char=? c lbchar))
           ; don't correct close delimiter when inserting open delimiter
           ; since doing so often leads to surprising results
            (when (ee-flash-parens)
              (cond
                [(find-matching-delim-forward ee entry row col #f) =>
                 (lambda (mpos) (flash ee entry mpos))]))
            (cond
              [(find-matching-delim-backward ee entry row col
                 (ee-auto-paren-balance)) =>
               (lambda (mpos)
                 (let ([cexp (expected
                               (string-ref
                                 (lns->str lns (pos-row mpos))
                                 (pos-col mpos)))])
                   (unless (eqv? c cexp)
                     (string-set! str col cexp)
                     (ee-write-char cexp)
                     (move-cursor-left 1)))
                 (when (ee-flash-parens) (flash ee entry mpos)))]))))
    (move-right ee entry 1))

  (define (find-matching-delimiter ee entry)
    (let ([row (entry-row entry)]
          [col (entry-col entry)]
          [str (lns->str (entry-lns entry) (entry-row entry))])
      (and (fx< col (string-length str))
           (let ([c (string-ref str col)])
             (if (or (char=? c lpchar) (char=? c lbchar))
                 (find-matching-delim-forward ee entry row col #f)
                 (and (or (char=? c rpchar) (char=? c rbchar))
                      (find-matching-delim-backward ee entry row col #f)))))))

  (define (find-matching-delim-backward ee entry row col lax?)
    (let ([lns (entry-lns entry)])
     ; 1. create string representing current entry through row, col
     ; 2. search forward, stacking left/right delimiters and their indices
     ; 3. if matching delimiter found, convert string index to pos
      (let* ([s (let ([op (open-output-string)])
                  (let loop ([i 0] [sep ""])
                    (let ([str (lns->str lns i)])
                      (if (= i row)
                          (fprintf op "~a~a" sep (substring str 0 (fx+ col 1)))
                          (begin
                            (fprintf op "~a~a" sep str)
                            (loop (fx+ i 1) "\n")))))
                  (get-output-string op))]
             [ip (open-input-string s)])
        (let loop ([stack '()])
          (on-error (loop '())
            (let-values ([(type value start end) (read-token ip)])
              (case type
                [(atomic box dot insert mark quote) (loop stack)]
                [(lbrack record-brack)
                 (loop (cons (cons 'rbrack end) stack))]
                [(lparen vfxnparen vfxparen vflnparen vflparen vnparen vparen vu8nparen vu8paren vsparen vsnparen)
                 (loop (cons (cons 'rparen end) stack))]
                [(rbrack rparen)
                 (if (= end (string-length s))
                     (and (not (null? stack))
                          (or lax? (eq? (caar stack) type))
                          (index->pos s (fx- (cdar stack) 1) 0 0))
                     (if (and (not (null? stack)) (eq? (caar stack) type))
                         (loop (cdr stack))
                         (loop '())))]
                [(eof fasl) #f]
                [else
                 (warningf 'expeditor "unexpected token type ~s from read-token" type)
                 #f])))))))

  (define (find-matching-delim-forward ee entry row col lax?)
    (let ([lns (entry-lns entry)])
     ; should be sitting on left paren or bracket
      (assert* (or (char=? (lns->char lns row col) lpchar)
                   (char=? (lns->char lns row col) lbchar)))
     ; 1. create string representing current entry starting at col, row
     ; 2. search forward until matching delimiter, eof, or error
     ; 3. if matching delimiter found, convert string index to pos
      (let* ([s (let ([op (open-output-string)] [l-lns (length lns)])
                  (let ([s (lns->str lns row)])
                    (display (substring s col (string-length s)) op))
                  (let loop ([i (fx+ row 1)])
                    (unless (fx= i l-lns)
                      (fprintf op "\n~a" (lns->str lns i))
                      (loop (fx+ i 1))))
                  (get-output-string op))]
             [ip (open-input-string s)])
        (on-error #f
          (let loop ([stack '()])
            (let-values ([(type value start end) (read-token ip)])
              (case type
                [(atomic box dot insert mark quote) (loop stack)]
                [(lbrack record-brack)
                 (loop (cons 'rbrack stack))]
                [(lparen vfxnparen vfxparen vflnparen vflparen vnparen vparen vu8nparen vu8paren vsparen vsnparen)
                 (loop (cons 'rparen stack))]
                [(rbrack rparen)
                 (if (fx= (length stack) 1)
                     (and (or lax? (eq? (car stack) type))
                          (index->pos s start row col))
                     (and (eq? (car stack) type) (loop (cdr stack))))]
                [(eof fasl) #f]
                [else
                 (warningf 'expeditor "unexpected token type ~s from read-token" type)
                 #f])))))))

  (define (find-next-sexp-backward ee entry row col)
    (let* ([lns (entry-lns entry)]
           [s (let ([op (open-output-string)])
                (let loop ([i 0] [sep ""])
                  (let ([str (lns->str lns i)])
                    (if (= i row)
                        (fprintf op "~a~a" sep (substring str 0 col))
                        (begin
                          (fprintf op "~a~a" sep str)
                          (loop (fx+ i 1) "\n")))))
                (get-output-string op))]
           [ip (open-input-string s)])
      (on-error #f
        (let loop ([stack '()] [last-start 0])
          (let-values ([(type value start end) (read-token ip)])
            (case type
              [(atomic dot insert mark)
               (if (and (not (null? stack)) (eq? (caar stack) 'qubx))
                   (loop (cdr stack) (cdar stack))
                   (loop stack start))]
              [(box quote)
               (if (and (not (null? stack)) (eq? (caar stack) 'qubx))
                   (loop stack #f)
                   (loop (cons (cons 'qubx start) stack) #f))]
              [(eof) (and last-start (index->pos s last-start 0 0))]
              [(lbrack record-brack)
               (if (and (not (null? stack)) (eq? (caar stack) 'qubx))
                   (loop (cons (cons 'rbrack (cdar stack)) (cdr stack)) #f)
                   (loop (cons (cons 'rbrack start) stack) #f))]
              [(lparen vfxnparen vfxparen vflnparen vflparen vnparen vparen vu8nparen vu8paren vsparen vsnparen)
               (if (and (not (null? stack)) (eq? (caar stack) 'qubx))
                   (loop (cons (cons 'rparen (cdar stack)) (cdr stack)) #f)
                   (loop (cons (cons 'rparen start) stack) #f))]
              [(rbrack rparen)
               (if (and (not (null? stack)) (eq? (caar stack) type))
                   (loop (cdr stack) (cdar stack))
                   (loop '() #f))]
              [else
               (warningf 'expeditor "unexpected token type ~s from read-token" type)
               #f]))))))

  (define (find-next-sexp-forward ee entry row col ignore-whitespace?)
   ; ordinarily stops at first s-expression if it follows whitespace (or
   ; comments), but always moves to second if ignore-whitespace? is true
    (let* ([lns (entry-lns entry)]
           [s (let ([op (open-output-string)] [l-lns (length lns)])
                   (let ([s (lns->str lns row)])
                     (display (substring s col (string-length s)) op))
                   (let loop ([i (fx+ row 1)])
                     (unless (fx= i l-lns)
                       (fprintf op "\n~a" (lns->str lns i))
                       (loop (fx+ i 1))))
                   (get-output-string op))]
           [ip (open-input-string s)])
      (define (skip start)
        (index->pos s
          (on-error start
            (let-values ([(type value start end) (read-token ip)])
              start))
          row col))
      (on-error #f
        (let loop ([stack '()] [first? #t] [ignore? #f])
          (let-values ([(type value start end) (read-token ip)])
            (if (and first? (not ignore-whitespace?) (fx> start 0))
                (and (not ignore?) (index->pos s start row col))
                (case type
                  [(atomic dot insert mark)
                   (if (null? stack)
                       (and (not ignore?) (skip start))
                       (loop stack #f ignore?))]
                  [(box) (loop stack #f ignore?)]
                  [(quote)
                   (when (and ignore-whitespace?
                              (eq? value 'datum-comment)
                              (null? stack))
                     (loop '() #f #t))
                   (loop stack #f ignore?)]
                  [(eof fasl) #f]
                  [(lbrack record-brack) (loop (cons 'rbrack stack) #f ignore?)]
                  [(lparen vfxnparen vfxparen vflnparen vflparen vflnparen vflparen vnparen vparen
                           vu8nparen vu8paren vsparen vsnparen)
                   (loop (cons 'rparen stack) #f ignore?)]
                  [(rbrack rparen)
                   (and (not (null? stack))
                        (eq? (car stack) type)
                        (let ([stack (cdr stack)])
                          (if (null? stack)
                              (and (not ignore?) (skip start))
                              (loop stack #f ignore?))))]
                  [else
                   (warningf 'expeditor "unexpected token type ~s from read-token" type)
                   #f])))))))

  (module (find-next-word find-previous-word)
    (define separator?
      (lambda (c)
        (memq c '(#\space #\; #\( #\) #\[ #\] #\" #\' #\`))))

    (define (find-next-word ee entry row col)
     ; always returns a position
      (let ([lns (entry-lns entry)])
       ; skip past separators
        (let loop ([row row] [col col])
          (cond
            [(fx= col (string-length (lns->str lns row)))
             (if (fx= row (fx1- (length lns)))
                 (make-pos row col)
                 (loop (fx1+ row) 0))]
            [(separator? (lns->char lns row col))
             (loop row (fx1+ col))]
           ; now we are past initial separators, find next separator
            [else
             (let loop ([col col])
               (cond
                 [(or (fx= col (string-length (lns->str lns row)))
                      (separator? (lns->char lns row col)))
                  (make-pos row col)]
                 [else (loop (fx1+ col))]))]))))

    (define (find-previous-word ee entry row col)
     ; always returns a position
      (let ([lns (entry-lns entry)])
       ; skip past separators space (starts at char left of current)
        (let loop ([row row] [col col])
          (cond
            [(fx= col 0)
             (if (fx= row 0)
                 (make-pos row col)
                 (loop
                   (fx1- row)
                   (string-length (lns->str lns (fx1- row)))))]
            [(separator? (lns->char lns row (fx1- col)))
             (loop row (fx1- col))]
           ; now we are past initial separators, find next separator
            [else
             (let loop ([col col])
               (cond
                 [(or (fx= col 0)
                      (separator? (lns->char lns row (fx1- col))))
                  (make-pos row col)]
                 [else (loop (fx1- col))]))])))))

  (module (indent indent-all)
    (define (calc-indent ee entry row)
      (define (find-unmatched-left-delim row)
        (let* ([ln (list-ref (entry-lns entry) row)]
               [s (ln-str ln)])
          (ln-str-set! ee ln (string rpchar))
          (let ([pos (find-matching-delim-backward ee entry row 0 #t)])
            (ln-str-set! ee ln s)
            pos)))
      (let ([lns (entry-lns entry)])
        (cond
          [(find-unmatched-left-delim row) =>
           (lambda (mpos)
             (let ([mrow (pos-row mpos)] [mcol (pos-col mpos)])
               (or
                  ; if some intervening line has same unmatched left
                  ; delimiter, use its indentation
                   (let f ([xrow (fx- row 1)])
                     (and (not (fx= xrow mrow))
                          (cond
                            [(find-unmatched-left-delim xrow) =>
                             (lambda (xmpos)
                               (if (pos=? xmpos mpos)
                                   (current-indent lns xrow)
                                   (f (fx- xrow 1))))]
                            [else (f (fx- xrow 1))])))
                  ; otherwise, if left paren is followed by a symbol,
                  ; indent under second item or use standard indent if
                  ; second item is too far out or not present
                   (let ([ip (open-input-string
                               (let ([s (lns->str lns mrow)])
                                 (substring s mcol (string-length s))))])
                     (on-error #f
                       (and (char=? (read-char ip) lpchar)
                         (let-values ([(t1 v1 s1 e1) (read-token ip)])
                           (and (and (eq? t1 'atomic) (symbol? v1))
                                (let-values ([(t2 v2 s2 e2) (read-token ip)])
                                  (if (and (not (eq? t2 'eof))
                                           (fx< s2 6)
                                          ; use standard indent for let and rec
                                           (not (memq v1 '(let rec))))
                                      (fx+ mcol s2)
                                      (fx+ mcol (ee-standard-indent)))))))))
                  ; otherwise, indent one space in.  this handles, among
                  ; other things, bracketed let bindings and cond clauses.
                   (fx+ mcol 1))))]
          [else 0])))

    (define current-indent
      (lambda (lns row)
        (let* ([s (lns->str lns row)]
               [n (string-length s)])
          (let f ([i 0])
            (if (and (fx< i n) (char=? (string-ref s i) #\space))
                (f (fx+ i 1))
                i)))))

    (define (indent-row! ee entry row n)
      (cond
        [(fx< n 0)
         (adjust-mark/delete ee entry row 0 row (fx- n))
         (let ([lns (entry-lns entry)])
           (lns->str! ee lns row
             (let ([s (lns->str lns row)])
               (substring s (fx- n) (string-length s)))))]
        [(fx> n 0)
         (adjust-mark/insert ee entry row 0 row n)
         (let ([lns (entry-lns entry)])
           (lns->str! ee lns row
             (string-append
               (make-string n #\space)
               (lns->str lns row))))]))

    (define (indent ee entry)
      (let* ([row (entry-row entry)]
             [lns (entry-lns entry)]
             [n (fx- (calc-indent ee entry row) (current-indent lns row))])
        (unless (fx= n 0)
          (let* ([ln (list-ref lns row)]
                 [nsr (ln-nsr ln)]
                 [eoe? (end-of-entry? ee entry)])
            (indent-row! ee entry row n)
            (move-bol ee entry)
            (let ([just-row? (fx= (ln-nsr ln) nsr)])
              (display-rest/goto ee entry just-row?
               ; avoid clear-eol/eos if inserting and either at end of entry or
               ; rewriting just the current row
                (or (fx< n 0) (and (not eoe?) (not just-row?)))
                row (fxmax (fx+ (entry-col entry) n) 0)))))))

    (define (indent-all ee entry)
      (let* ([lns (entry-lns entry)]
             [row (entry-row entry)]
             [col (entry-col entry)]
             [top-line (entry-top-line entry)]
             [point-ln (list-ref lns row)]
             [point-strlen (string-length (ln-str point-ln))]
             [lines-to-top ; compute before we muck with indentation
              (screen-lines-between ee entry
                (pos-row top-line) (pos-col top-line)
                row (col->line-offset ee col))])
        (let loop ([ls lns] [i 0] [firstmod (length lns)] [lastmod -1])
          (if (null? ls)
              (unless (and (fx< lastmod (pos-row top-line))
                           (fx> firstmod (pos-row (entry-bot-line entry))))
               ; move to first physical column of first displayed line
                (move-cursor-up lines-to-top)
                (carriage-return)
                (clear-eos)
                (entry-col-set! entry
                  (fxmax 0
                    (fx+ col
                         (fx- (string-length (ln-str point-ln))
                              point-strlen))))
                (redisplay ee entry))
              (let ([n (fx- (calc-indent ee entry i) (current-indent lns i))])
                (if (fx= n 0)
                    (loop (cdr ls) (fx+ i 1) firstmod lastmod)
                    (begin
                      (indent-row! ee entry i n)
                      (loop (cdr ls) (fx+ i 1)
                        (fxmin i firstmod)
                        (fxmax i lastmod)))))))))
  )

  (define (id-completions ee entry)
    (define (idstring<? prefix)
      (let ([common (ee-common-identifiers)]
            [scheme-syms (environment-symbols (scheme-environment))])
        (lambda (s1 s2)
          (let ([x1 (string->symbol (string-append prefix s1))]
                [x2 (string->symbol (string-append prefix s2))])
            ; prefer common
            (let ([m1 (memq x1 common)] [m2 (memq x2 common)])
              (if m1
                  (or (not m2) (< (length m2) (length m1)))
                  (and (not m2)
                       ; prefer user-defined
                       (let ([u1 (not (memq x1 scheme-syms))]
                             [u2 (not (memq x2 scheme-syms))])
                         (if u1
                             (or (not u2) (string<? s1 s2))
                             (and (not u2) (string<? s1 s2)))))))))))
    (define (completion str1 str2)
      (let ([n1 (string-length str1)] [n2 (string-length str2)])
        (and (fx>= n2 n1)
             (string=? (substring str2 0 n1) str1)
             (substring str2 n1 n2))))
    (define (fn-completions prefix)
      (values prefix
        (sort string<?
          (fold-left
            (let ([last (path-last prefix)])
              (lambda (suffix* s)
                (cond
                  [(completion last s) =>
                   (lambda (suffix) 
                     (cons (if (file-directory? (string-append prefix suffix))
                               (string-append suffix (string (directory-separator)))
                               suffix)
                       suffix*))]
                  [else suffix*])))
            '()
            (on-error '()
              (directory-list
                (let ([dir (path-parent prefix)])
                  (if (string=? dir "") "." dir))))))))
    (let loop ([c 0])
      (if (fx>= c (entry-col entry))
          (values #f '())
          (let ([s (let ([s (lns->str (entry-lns entry) (entry-row entry))])
                     (substring s c (string-length s)))])
            ((on-error
               (lambda ()
                 (if (and (fx> (string-length s) 0) (char=? (string-ref s 0) #\"))
                     (fn-completions (substring s 1 (string-length s)))
                     (loop (fx+ c 1))))
               (let-values ([(type value start end) (read-token (open-input-string s))])
                 (lambda ()
                   (cond
                     [(and (fx= (fx+ c end) (entry-col entry))
                           (eq? type 'atomic)
                           (symbol? value))
                      (let ([prefix (symbol->string value)])
                        (values prefix
                          (sort (idstring<? prefix)
                            (fold-left (lambda (suffix* x)
                                         (cond
                                           [(completion prefix (symbol->string x)) =>
                                            (lambda (suffix) (cons suffix suffix*))]
                                           [else suffix*]))
                              '() (environment-symbols (interaction-environment))))))]
                     [(and (fx= (fx+ c end -1) (entry-col entry))
                           (eq? type 'atomic)
                           (string? value))
                      (fn-completions value)]
                     [else (loop (fx+ c end))])))))))))

  (define (should-auto-indent? ee)
    (and (ee-auto-indent)
        ; don't autoindent if the characters are coming so fast that we're
        ; probably dealing with paste input
         (> (- (real-time) (car (eestate-rt-last-op ee))) 50)))
)

(module (ee-read)
  (define (accept ee entry kf)
    (let* ([str (entry->string entry)]
           [sip (open-input-string str)])
      (define (fail c)
        (define (report sop)
          (cond
            [(and (message-condition? c)
                  (irritants-condition? c)
                  (equal? (condition-message c) "~? at char ~a of ~s")
                  (let ([irritants (condition-irritants c)])
                    (and (list? irritants)
                         (fx= (length irritants) 4)
                         irritants))) =>
             (lambda (irritants)
               (apply
                 (lambda (?msg ?args fp ip)
                   (fprintf sop "read: ~?" ?msg ?args)
                   (let ([pos (index->pos str fp 0 0)])
                     (entry-row-set! entry (pos-row pos))
                     (entry-col-set! entry (pos-col pos))))
                 irritants))]
            [else (display-condition c sop)]))
       ; clear entry before report has a chance to muck with point position
        (clear-entry ee entry)
        (ee-display-string (make-string (screen-cols) #\-))
        (carriage-return)
        (line-feed)
        (let* ([s (let ([sop (open-output-string)])
                    (report sop)
                    (get-output-string sop))]
               [n (string-length s)])
          (let loop ([i 0] [msg-lines 0])
            (if (= i n)
                (begin
                  (unless (fx< (screen-rows) 3)
                    (ee-display-string (make-string (screen-cols) #\-))
                    (carriage-return)
                    (line-feed))
                  (redisplay ee entry (max (fx- (screen-rows) msg-lines 2) 1)))
                (let ([m (min (fx+ i (screen-cols)) n)])
                  (ee-display-string (substring s i m))
                  (when (fx< (screen-rows) 2) (wait 2000))
                  (carriage-return)
                  (line-feed)
                  (loop m (fx+ msg-lines 1))))))
        (kf))
      (define (succeed result)
        (move-eoe ee entry)
        (no-raw-mode)
        (ee-write-char #\newline)
        (ee-flush)
        (update-history! ee entry)
       ; skip close delimiters, whitespace, and comments, then
       ; save remainder of entry, if any, as histnow
        (eestate-histnow-set! ee
          (substring str
            (let skip ([fp (file-position sip)])
              (on-error fp
                (let-values ([(type value start end) (read-token sip)])
                  (case type
                    [(rparen rbrack) (skip end)]
                    [else start]))))
            (string-length str)))
        (eestate-last-op-set! ee #f)
       ; inform encapsulated transcript port(s) if any
        (let loop ([op (console-output-port)])
          (when ($xscript-port? op)
            (let-values ([(ip op xp) ($constituent-ports op)])
              (unless (port-closed? xp) (echo-entry ee entry xp))
              (loop op))))
        result)
      ((guard (c [#t (lambda () (fail c))])
         (let ([x (read sip)])
           (lambda () (succeed x)))))))

  (define (dispatch ee entry table)
    (if (ee-winch?)
        (begin
          (handle-winch ee entry)
          (dispatch ee entry table))
        (let ([c (ee-read-char)])
          (let ([x (if (eof-object? c)
                       (lambda (ee entry c) #f)
                       (hashtable-ref table c ee-insert-self))])
            (cond
              [(procedure? x)
               (let ([n (eestate-repeat-count ee)])
                 (eestate-repeat-count-set! ee 1)
                 (if (= n 0)
                     (dispatch ee entry base-dispatch-table)
                     (let loop ([n n] [entry entry])
                       (cond
                         [(x ee entry c) =>
                          (lambda (entry)
                            (if (> n 1)
                                (loop (- n 1) entry)
                                (begin
                                  (eestate-rt-last-op-set! ee
                                    (cons (cdr (eestate-rt-last-op ee))
                                          (real-time)))
                                  (eestate-last-op-set! ee x)
                                  (dispatch ee entry base-dispatch-table))))]
                         [else
                          (accept ee entry
                            (lambda ()
                              (dispatch ee entry base-dispatch-table)))]))))]
              [(dispatch-table? x) (dispatch ee entry x)]
              [else
               (eestate-repeat-count-set! ee 1)
               (eestate-last-op-set! ee #f)
               (beep "unbound key")
               (dispatch ee entry base-dispatch-table)])))))

  (define (ee-read ee)
    (screen-resize!)
    (let ([entry (let ([s (eestate-histnow ee)])
                  ; set to "" so that entry will appear modified if nonempty,
                  ; i.e., if a partial entry is left over from last read
                   (eestate-histnow-set! ee "")
                   (string->entry ee s))])
      (raw-mode)
      (carriage-return)
      (redisplay ee entry)
      (move-eol ee entry)
      (guard (c [#t (carriage-return)
                    (line-feed)
                    (clear-eos)
                    (ee-flush)
                    (no-raw-mode)
                    (ee-display-string
                      (call-with-string-output-port
                        (lambda (p)
                          (display-condition c p))))
                    (ee-write-char #\newline)
                    (update-history! ee entry)
                    (void)])
        (dispatch ee entry base-dispatch-table)))))

(define (ee-prompt-and-read ee n)
  (unless (and (integer? n) (>= n 0))
    ($oops 'ee-prompt-and-read
      "nesting level ~s is not a positive integer"
      n))
  (if (and (let f ([ip (console-input-port)])
             (or (eq? ip #%$console-input-port)
                 (and ($xscript-port? ip)
                   (let-values ([(ip op xp) ($constituent-ports ip)])
                     (f ip)))))
           (let f ([op (console-output-port)])
             (or (eq? op #%$console-output-port)
                 (and ($xscript-port? op)
                   (let-values ([(ip op xp) ($constituent-ports op)])
                     (f op))))))
      (begin
       ; fresh-line doesn't take into account output written to the console
       ; through some other port or external means, so this might not emit a
       ; fresh line when one is needed, but the user can always redisplay
        (fresh-line (console-output-port))
        (flush-output-port (console-output-port))
        (eestate-prompt-set! ee
          (let ([wps (waiter-prompt-string)])
            (if (string=? wps "")
                ""
                (string-append
                  (apply string-append (make-list n wps))
                  " "))))
        (ee-read ee))
      (default-prompt-and-read n)))

;;; history functions

(module (history-search-bwd history-search-fwd
         update-history! history-fast-forward! entry-modified?
         ee-save-history ee-load-history)
  (define search
    (lambda (ee pred? get-bwd set-bwd! get-fwd set-fwd!)
      (let loop ([bwd (get-bwd ee)]
                 [now (eestate-histnow ee)]
                 [fwd (get-fwd ee)])
        (and (not (null? bwd))
             (let ([s (car bwd)])
               (if (pred? s)
                   (begin
                     (set-bwd! ee (cdr bwd))
                     (eestate-histnow-set! ee s)
                     (set-fwd! ee (cons now fwd))
                     s)
                   (loop (cdr bwd) s (cons now fwd))))))))

  (define history-search-bwd
    (lambda (ee pred?)
      (search ee pred? eestate-histbwd eestate-histbwd-set!
        eestate-histfwd eestate-histfwd-set!)))

  (define history-search-fwd
    (lambda (ee pred?)
      (search ee pred? eestate-histfwd eestate-histfwd-set!
        eestate-histbwd eestate-histbwd-set!)))

  (define history->list
    (lambda (ee)
      (cdr `(,@(reverse (eestate-histfwd ee))
             ,(eestate-histnow ee)
             ,@(eestate-histbwd ee)))))

  (define trim-history
    (lambda (ls)
      (let ([n (ee-history-limit)])
        (if (> (length ls) n)
            (list-head ls n)
            ls))))

  (define update-history!
    (lambda (ee entry)
      (define (all-whitespace? s)
        (let ([n (string-length s)])
          (let f ([i 0])
            (or (fx= i n)
                (and (memv (string-ref s i) '(#\space #\newline))
                     (f (fx+ i 1)))))))
      (let ([s (entry->string entry)] [ls (history->list ee)])
        (eestate-histbwd-set! ee
          (if (or (all-whitespace? s)
                  (and (not (null? ls))
                       (equal? s (car ls))))
              ls
              (begin
                (eestate-histnew-set! ee (fx+ (eestate-histnew ee) 1))
                (trim-history (cons s ls))))))
      (eestate-histnow-set! ee "")
      (eestate-histfwd-set! ee '())))

  (define history-fast-forward!
    (lambda (ee)
      (eestate-histbwd-set! ee (history->list ee))
      (eestate-histnow-set! ee "")
      (eestate-histfwd-set! ee '())))

  (define (entry-modified? ee entry)
    (not (string=? (entry->string entry) (eestate-histnow ee))))

  (module (ee-save-history ee-load-history)
    (define read-history
      (lambda (ip)
        (on-error #f
          (let loop ([ls '()])
            (let ([x (read ip)])
              (if (eof-object? x)
                  ls
                  (begin
                    (unless (string? x) ($oops #f "oops"))
                    (loop (cons x ls)))))))))

    (define ee-save-history
      (lambda (ee filename)
        (unless (string? filename)
          ($oops 'ee-save "~s is not a string" filename))
        (let* ([iop ($open-file-input/output-port 'expeditor filename
                      (file-options exclusive no-fail no-truncate)
                      (buffer-mode block)
                      (make-transcoder (utf-8-codec)))]
               [ls (let ([curls (history->list ee)])
                     (cond
                       [(read-history iop) =>
                        (lambda (savls)
                          (trim-history
                            (append
                              (list-head curls (eestate-histnew ee))
                              savls)))]
                       [else curls]))])
          (truncate-file iop)
          (fprintf iop "~
            ;;; This file contains a saved history for the (Petite) Chez Scheme~@
            ;;; expression editor.  The history is represented as a sequence of~@
            ;;; strings, each representing a history entry, with the most recent~@
            ;;; entries listed last.~@
            ~@
            ;;; Exit each Scheme session running the expression editor before~@
            ;;; saving changes so they aren't wiped out when the session ends.\n\n")
          (for-each (lambda (s) (fprintf iop "~s\n" s)) (reverse ls))
          (close-port iop))))

    (define ee-load-history
      (lambda (ee filename)
        (unless (string? filename)
          ($oops 'ee-load-history "~s is not a string" filename))
        (let* ([iop ($open-file-input/output-port 'expeditor filename
                      (file-options exclusive no-fail no-truncate)
                      (buffer-mode block)
                      (make-transcoder (utf-8-codec)))]
               [ls (read-history iop)])
          (close-port iop)
          (unless ls
            ($oops 'ee-load-history "missing or malformed history file ~s"
              filename))
          (eestate-histnew-set! ee 0)
          (eestate-histbwd-set! ee ls)
          (eestate-histnow-set! ee "")
          (eestate-histfwd-set! ee '())))))
)

;;; editing functions

(module (ee-next-id-completion ee-next-id-completion/indent)
  (define complete
    (lambda (ee entry suffix*)
      (eestate-last-suffix*-set! ee suffix*)
      (if (null? suffix*)
          (beep "id-completion: no completion found")
          (insert-string-before ee entry (car suffix*)))))

  (define next-completion
    (lambda (ee entry)
      (if (fx<= (length (eestate-last-suffix* ee)) 1)
          (beep "id-completion: no completion found")
          (let ([suffix (car (eestate-last-suffix* ee))])
            (let ([n (string-length suffix)])
              (move-left ee entry n)
              (delete-forward ee entry (entry-row entry) (fx+ (entry-col entry) n)))
            (complete ee entry
              (append (cdr (eestate-last-suffix* ee)) (list suffix)))))))

  (define ee-next-id-completion
    (lambda (ee entry c)
      (if (eq? (eestate-last-op ee) ee-next-id-completion)
          (next-completion ee entry)
          (let-values ([(prefix suffix*) (id-completions ee entry)])
            (if prefix
                (complete ee entry suffix*)
                (begin
                  (eestate-last-suffix*-set! ee '())
                  (beep "id-completion: no identifier to complete")))))
      entry))

  (define ee-next-id-completion/indent
    (lambda (ee entry c)
      (cond
        [(and (eq? (eestate-last-op ee) ee-next-id-completion/indent)
              (eestate-cc? ee))
         (next-completion ee entry)
         entry]
        [(and (or (eq? (eestate-last-op ee) ee-insert-self)
                  (eq? (eestate-last-op ee) ee-next-id-completion/indent))
              (let-values ([(prefix suffix*) (id-completions ee entry)])
                (and prefix suffix*))) =>
         (lambda (suffix*)
           (eestate-cc?-set! ee #t)
           (complete ee entry suffix*)
           entry)]
        [else
         (eestate-cc?-set! ee #f)
         (eestate-last-suffix*-set! ee '())
         (ee-indent ee entry c)])))
)
  
(module (ee-id-completion ee-id-completion/indent)
  (define (display-completions prefix suffix*)
    (let* ([s* (map (lambda (suffix) (string-append prefix suffix)) suffix*)]
           [width (fx+ (apply fxmax (map string-length s*)) 2)]
           [tcols (fxmax 1 (fxquotient (screen-cols) width))]
           [trows (fxquotient (length s*) tcols)]
           [nlong (fxremainder (length s*) tcols)])
      (define (display-row v last)
        (let loop ([j 0])
          (let ([s (vector-ref v j)])
            (if (fx= j last)
                (ee-display-string s)
                (begin
                  (ee-display-string (format "~va" width s))
                  (loop (fx+ j 1))))))
        (carriage-return)
        (line-feed))
      (let ([v (make-vector (if (fx= nlong 0) trows (fx+ trows 1)))])
        (do ([i 0 (fx+ i 1)])
            ((fx= i (vector-length v)))
          (vector-set! v i (make-vector tcols #f)))
        (let f ([s* s*] [i 0] [j 0] [nlong nlong])
          (unless (null? s*)
            (if (fx= i (if (fx> nlong 0) (fx+ trows 1) trows))
                (f s* 0 (fx+ j 1) (fx- nlong 1))
                (begin
                  (vector-set! (vector-ref v i) j (car s*))
                  (f (cdr s*) (fx+ i 1) j nlong)))))
        (do ([i 0 (fx+ i 1)])
            ((fx= i trows))
          (display-row (vector-ref v i) (fx- tcols 1)))
        (unless (fx= nlong 0)
          (display-row (vector-ref v trows) (fx- nlong 1)))
        (if (fx= nlong 0) trows (fx+ trows 1)))))

  (define (common-prefix s*)
    (let outer ([s1 (car s*)] [s* (cdr s*)])
      (if (null? s*)
          s1
          (let ([s2 (car s*)])
            (let ([n1 (string-length s1)] [n2 (string-length s2)])
              (let inner ([i 0])
                (if (or (fx= i n1)
                        (fx= i n2)
                        (not (char=? (string-ref s1 i) (string-ref s2 i))))
                    (outer (substring s1 0 i) (cdr s*))
                    (inner (fx+ i 1)))))))))

  (define ee-id-completion
    (lambda (ee entry c)
      (let-values ([(prefix suffix*) (id-completions ee entry)])
        (if prefix
            (if (not (null? suffix*))
                (if (eq? (eestate-last-op ee) ee-id-completion)
                    (begin
                      (clear-entry ee entry)
                      (ee-display-string (make-string (screen-cols) #\-))
                      (carriage-return)
                      (line-feed)
                      (let ([nrows (display-completions prefix suffix*)])
                        (ee-display-string (make-string (screen-cols) #\-))
                        (carriage-return)
                        (line-feed)
                        (redisplay ee entry (max (fx- (screen-rows) nrows 1) 1))))
                    (insert-string-before ee entry (common-prefix suffix*)))
                (beep "id-completion: no completions found"))
            (beep "id-completion: no identifier to complete")))
      entry))

  (define ee-id-completion/indent
    (lambda (ee entry c)
      (cond
        [(and (eq? (eestate-last-op ee) ee-id-completion/indent)
              (eestate-cc? ee))
         (let-values ([(prefix suffix*) (id-completions ee entry)])
           (if (not (null? suffix*))
               (begin
                 (clear-entry ee entry)
                 (ee-display-string (make-string (screen-cols) #\-))
                 (carriage-return)
                 (line-feed)
                 (let ([nrows (display-completions prefix suffix*)])
                   (ee-display-string (make-string (screen-cols) #\-))
                   (carriage-return)
                   (line-feed)
                   (redisplay ee entry (max (fx- (screen-rows) nrows 1) 1))))
               (beep "id-completion: no completions found")))
         entry]
        [(and (or (eq? (eestate-last-op ee) ee-insert-self)
                  (eq? (eestate-last-op ee) ee-id-completion/indent))
              (let-values ([(prefix suffix*) (id-completions ee entry)])
                (and prefix suffix*))) =>
         (lambda (suffix*)
           (eestate-cc?-set! ee #t)
           (if (not (null? suffix*))
               (insert-string-before ee entry (common-prefix suffix*))
               (beep "id-completion: no completions found"))
           entry)]
        [else
         (eestate-cc?-set! ee #f)
         (ee-indent ee entry c)])))
)

(define ee-insert-self
  (lambda (ee entry c)
    (add-char ee entry c)
    entry))

(define ee-command-repeat
  (lambda (ee entry c)
    (define (digit-value c) (char- c #\0))
    (eestate-repeat-count-set! ee
      (let ([c (ee-peek-char)])
        (if (and (not (eof-object? c)) (char-numeric? c))
            (let loop ([n (digit-value (ee-read-char))])
              (let ([c (ee-peek-char)])
                (if (and (not (eof-object? c)) (char-numeric? c))
                    (loop (+ (* n 10) (digit-value (ee-read-char))))
                    n)))
            (ee-default-repeat))))
    entry))

(module (ee-history-bwd ee-history-fwd
         ee-history-bwd-prefix ee-history-fwd-prefix
         ee-history-bwd-contains ee-history-fwd-contains)
  (define contains?
    (lambda (key str)
      (let ([key-len (string-length key)]
            [str-len (string-length str)])
        (let loop ([idx 0])
          (cond
            [(fx> key-len (fx- str-len idx)) #f]
            [(string=? key (substring str idx (fx+ idx key-len))) #t]
            [else (loop (add1 idx))])))))

  (define prefix?
    (lambda (key str)
      (let ([nkey (string-length key)] [nstr (string-length str)])
       ; if key doesn't start with space, skip leading spaces in str
        (let ([i (if (or (fx= nkey 0) (char=? (string-ref key 0) #\space))
                     0
                     (let f ([i 0])
                       (if (or (fx= i nstr) (not (char=? (string-ref str i) #\space)))
                           i
                           (f (fx+ i 1)))))])
          (let ([n (fx+ nkey i)])
            (and (fx<= n nstr)
                 (string=? key (substring str i n))))))))

  (define new-entry
    (lambda (ee entry s)
      (clear-entry ee entry)
      (let ([entry (string->entry ee s)])
        (redisplay ee entry 1)
        (move-eol ee entry)
        entry)))

  (define ee-history-bwd
    (lambda (ee entry c)
      (cond
        [(and (not (null-entry? entry)) (entry-modified? ee entry))
         (beep "cannot leave nonempty modified entry")
         entry]
        [(history-search-bwd ee (lambda (s) #t)) =>
         (lambda (s)
          ; clear histkey when null as favor to search commands
           (when (null-entry? entry) (eestate-histkey-set! ee ""))
           (new-entry ee entry s))]
        [else
         (beep "invalid history movement")
         entry])))

  (define ee-history-fwd
    (lambda (ee entry c)
      (cond
        [(and (not (null-entry? entry)) (entry-modified? ee entry))
         (beep "cannot leave nonempty modified entry")
         entry]
        [(history-search-fwd ee (lambda (s) #t)) =>
         (lambda (s)
          ; clear histkey when null as favor to search commands
           (when (null-entry? entry) (eestate-histkey-set! ee ""))
           (new-entry ee entry s))]
        [else
         (beep "invalid history movement")
         entry])))

  (define history-search-bwd-key
    (lambda (ee entry match?)
      (if (or (entry-modified? ee entry) (null-entry? entry))
          (begin
            (history-fast-forward! ee)
            (eestate-histkey-set! ee (entry->string entry))
            (cond
              [(history-search-bwd ee
                 (lambda (s) (match? (eestate-histkey ee) s))) =>
               (lambda (s) (new-entry ee entry s))]
              [else
               (beep "invalid history movement")
               entry]))
         ; if nonempty and unmodified, we must already have moved via one
         ; of the history commands, so eestate-histkey should be valid
          (cond
            [(history-search-bwd ee
               (lambda (s) (match? (eestate-histkey ee) s))) =>
             (lambda (s) (new-entry ee entry s))]
            [else
             (beep "invalid history movement")
             entry]))))

  (define history-search-fwd-key
   ; similar to history-search-bwd-key but "finds" key at forward extreme
    (lambda (ee entry match?)
      (if (or (entry-modified? ee entry) (null-entry? entry))
          (begin
            (history-fast-forward! ee)
            (eestate-histkey-set! ee (entry->string entry))
            (cond
              [(history-search-fwd ee
                 (lambda (s) (prefix? (eestate-histkey ee) s))) =>
               (lambda (s) (new-entry ee entry s))]
              [else
               (beep "invalid history movement")
               entry]))
         ; if nonempty and unmodified, we must already have moved via one
         ; of the history commands, so eestate-histkey should be valid
          (cond
            [(history-search-fwd ee
               (lambda (s) (match? (eestate-histkey ee) s))) =>
             (lambda (s) (new-entry ee entry s))]
            [else
             (let ([entry (new-entry ee entry (eestate-histkey ee))])
               (history-fast-forward! ee)
               entry)]))))

  (define ee-history-fwd-prefix
    (lambda (ee entry c)
      (history-search-fwd-key ee entry prefix?)))

  (define ee-history-bwd-prefix
    (lambda (ee entry c)
      (history-search-bwd-key ee entry prefix?)))

  (define ee-history-fwd-contains
    (lambda (ee entry c)
      (history-search-fwd-key ee entry contains?)))

  (define ee-history-bwd-contains
    (lambda (ee entry c)
      (history-search-bwd-key ee entry contains?)))
)

(define ee-newline/accept
  (lambda (ee entry c)
    (cond
      [(null-entry? entry) entry]
     ; #f tells ee-read to return expr
      [(and (find-next-sexp-forward ee entry 0 0 #t)
            (only-whitespace-left? ee entry))
       (let loop ()
         (delete-to-eol ee entry)
         (unless (last-line? ee entry)
           (join-rows ee entry)
           (loop)))
       #f]
      [else
       (insert-strings-before ee entry '("" ""))
       (when (should-auto-indent? ee) (indent ee entry))
       entry])))

(define ee-newline
  (lambda (ee entry c)
    (cond
      [(null-entry? entry) entry]
      [else
       (insert-strings-before ee entry '("" ""))
       (when (should-auto-indent? ee) (indent ee entry))
       entry])))

(define ee-accept
  (lambda (ee entry c)
   ; force ee-read to attempt read even if not at end of expr and not balanced
    (on-error #f
      (let ([sip (open-input-string (entry->string entry))])
        (let loop ()
          (let-values ([(type value start end) (read-token sip)])
            (cond
              [(eq? type 'eof)
              ; entry contains only whitespace and comments.  pretend to accept
              ; but don't really, or ee-read will return eof, causing cafe to exit
               (update-history! ee entry)
               (move-eoe ee entry)
               (no-raw-mode)
               (ee-write-char #\newline)
               (ee-flush)
               (raw-mode)
               (let ([entry (string->entry ee "")])
                 (redisplay ee entry)
                 entry)]
              [(and (eq? type 'quote) (eq? value 'datum-comment))
               (read sip)
               (loop)]
              [else #f])))))))

(define ee-open-line
  (lambda (ee entry c)
    (let ([point (entry-point entry)])
      (insert-strings-before ee entry '("" ""))
      (when (should-auto-indent? ee) (indent ee entry))
      (goto ee entry point)
      entry)))

(define ee-indent
  (lambda (ee entry c)
    (indent ee entry)
    entry))

(define ee-indent-all
  (lambda (ee entry c)
    (indent-all ee entry)
    entry))

(define ee-backward-char
  (lambda (ee entry c)
    (if (beginning-of-line? ee entry)
        (unless (first-line? ee entry)
          (move-up ee entry)
          (move-eol ee entry))
        (move-left ee entry))
    entry))

(define ee-forward-char
  (lambda (ee entry c)
    (if (end-of-line? ee entry)
        (unless (last-line? ee entry)
          (move-down ee entry)
          (move-bol ee entry))
        (move-right ee entry))
    entry))

(define ee-next-line
  (lambda (ee entry c)
    (if (last-line? ee entry)
        (ee-history-fwd ee entry c)
        (begin
          (move-down ee entry)
          entry))))

(define ee-previous-line
  (lambda (ee entry c)
    (if (first-line? ee entry)
        (ee-history-bwd ee entry c)
        (begin
          (move-up ee entry)
          entry))))

(define ee-end-of-line
  (lambda (ee entry c)
    (move-eol ee entry)
    entry))

(define ee-beginning-of-line
  (lambda (ee entry c)
    (move-bol ee entry)
    entry))

(define ee-beginning-of-entry
  (lambda (ee entry c)
    (goto ee entry (make-pos 0 0))
    entry))

(define ee-end-of-entry
  (lambda (ee entry c)
    (move-eoe ee entry)
    entry))

(define ee-delete-to-eol
  (lambda (ee entry c)
    (if (end-of-line? ee entry)
        (unless (last-line? ee entry)
          (join-rows ee entry)
          (eestate-killbuf-set! ee
            (if (eq? (eestate-last-op ee) ee-delete-to-eol)
                (append (eestate-killbuf ee) '(""))
                '(""))))
        (eestate-killbuf-set! ee
          (let ([killbuf (delete-to-eol ee entry)])
            (if (eq? (eestate-last-op ee) ee-delete-to-eol)
               ; last addition must have been ("") representing newline
                (append (reverse (cdr (reverse (eestate-killbuf ee))))
                        killbuf)
                killbuf))))
    entry))

(define ee-delete-line
  (lambda (ee entry c)
    (if (and (first-line? ee entry)
             (not (last-line? ee entry))
             (last-line-displayed? ee entry))
        (ee-delete-entry ee entry c)
        (begin
          (move-bol ee entry)
          (let ([killbuf (delete-to-eol ee entry)])
             (unless (equal? killbuf '(""))
               (eestate-killbuf-set! ee killbuf)))
          entry))))

(define ee-delete-between-point-and-mark
  (lambda (ee entry c)
    (let ([point (entry-point entry)] [mark (entry-mark entry)])
      (if mark
          (unless (pos=? mark point)
            (eestate-killbuf-set! ee
              (if (pos<? mark (entry-point entry))
                  (begin
                    (goto ee entry mark)
                    (delete-forward ee entry (pos-row point) (pos-col point)))
                  (delete-forward ee entry (pos-row mark) (pos-col mark)))))
          (beep "mark not set")))
    entry))

(define ee-set-mark
  (lambda (ee entry c)
    (entry-mark-set! entry (entry-point entry))
    entry))

(define ee-delete-entry
  (lambda (ee entry c)
    (unless (null-entry? entry)
      (eestate-killbuf-set! ee (yank-entry ee entry)))
    (clear-entry ee entry)
    (let ([entry (string->entry ee "")])
      (redisplay ee entry)
      entry)))

(define ee-reset-entry
  (lambda (ee entry c)
    (history-fast-forward! ee)
    (ee-delete-entry ee entry c)))

(define ee-delete-sexp
  (lambda (ee entry c)
    (let ([pos (find-next-sexp-forward ee entry
                 (entry-row entry) (entry-col entry) #f)])
      (if pos
          (eestate-killbuf-set! ee
            (delete-forward ee entry (pos-row pos) (pos-col pos)))
          (beep "end of s-expression not found")))
    entry))

(define ee-backward-delete-sexp
  (lambda (ee entry c)
    (let ([row (entry-row entry)] [col (entry-col entry)])
      (let ([pos (find-next-sexp-backward ee entry row col)])
        (if pos
            (begin
              (goto ee entry pos)
              (eestate-killbuf-set! ee (delete-forward ee entry row col)))
          (beep "start of s-expression not found"))))
    entry))

(define ee-redisplay
  (lambda (ee entry c)
    (if (eq? (eestate-last-op ee) ee-redisplay)
        (clear-screen)
        (clear-entry ee entry))
    (redisplay ee entry)
    entry))

(define ee-yank-kill-buffer
  (lambda (ee entry c)
    (insert-strings-before ee entry (eestate-killbuf ee))
    entry))

(define ee-yank-selection
  (lambda (ee entry c)
    (insert-strings-before ee entry
      (string->lines
        (let* ([s (get-clipboard)]
               [n (fx- (string-length s) 1)])
          (if (and (fx>= n 0) (char=? (string-ref s n) #\newline))
              (substring s 0 n)
              s))))
    entry))

(define ee-string-macro
  (lambda (str)
    (lambda (ee entry c)
      (insert-string-before ee entry str)
      entry)))

(define ee-eof
  (lambda (ee entry c)
    (cond
      [(null-entry? entry) #f]
      [else (beep "eof ignored except in null entry")])))

(define ee-delete-char
  (lambda (ee entry c)
    (cond
      [(end-of-line? ee entry)
       (unless (last-line? ee entry) (join-rows ee entry))
       entry]
      [else (delete-char ee entry) entry])))

(define ee-eof/delete-char
  (lambda (ee entry c)
    (cond
      [(null-entry? entry)
       (if (eq? (eestate-last-op ee) ee-eof/delete-char)
           entry     ; assume attempt to continue deleting chars
           #f)]
      [(end-of-line? ee entry)
       (unless (last-line? ee entry) (join-rows ee entry))
       entry]
      [else (delete-char ee entry) entry])))

(define ee-backward-delete-char
  (lambda (ee entry c)
    (if (beginning-of-line? ee entry)
        (unless (first-line? ee entry)
          (move-up ee entry)
          (move-eol ee entry)
          (join-rows ee entry))
        (begin
          (move-left ee entry)
          (delete-char ee entry)))
    entry))

(define ee-insert-paren
  (lambda (ee entry c)
    (add-char ee entry c)
    (when (or (ee-flash-parens) (ee-auto-paren-balance))
      (correct&flash-matching-delimiter ee entry))
    entry))

(define ee-goto-matching-delimiter
  (lambda (ee entry c)
    (let ([pos (find-matching-delimiter ee entry)])
      (if pos
          (goto ee entry pos)
          (beep "matching delimiter not found")))
    entry))

(define ee-flash-matching-delimiter
  (lambda (ee entry c)
    (let ([pos (find-matching-delimiter ee entry)])
      (if pos
          (flash ee entry pos)
          (beep "matching delimiter not found")))
    entry))

(define ee-exchange-point-and-mark
  (lambda (ee entry c)
    (let ([mark (entry-mark entry)])
      (if mark
          (begin
            (entry-mark-set! entry (entry-point entry))
            (goto ee entry mark))
          (beep "mark not set")))
    entry))

(define ee-forward-sexp
  (lambda (ee entry c)
    (let ([pos (find-next-sexp-forward ee entry
                 (entry-row entry) (entry-col entry) #f)])
      (if pos
          (goto ee entry pos)
          (beep "end of s-expression not found")))
    entry))

(define ee-backward-sexp
  (lambda (ee entry c)
    (let ([pos (find-next-sexp-backward ee entry
                 (entry-row entry) (entry-col entry))])
      (if pos
          (goto ee entry pos)
          (beep "start of s-expression not found")))
    entry))

(define ee-forward-word
  (lambda (ee entry c)
    (goto ee entry
      (find-next-word ee entry
        (entry-row entry)
        (entry-col entry)))
    entry))

(define ee-backward-word
  (lambda (ee entry c)
    (goto ee entry
      (find-previous-word ee entry
        (entry-row entry)
        (entry-col entry)))
    entry))

(define ee-forward-page
  (lambda (ee entry c)
    (page-down ee entry)
    entry))

(define ee-backward-page
  (lambda (ee entry c)
    (page-up ee entry)
    entry))

(define ee-suspend-process
  (lambda (ee entry c)
    (carriage-return)
    (line-feed)
    (clear-eos)
    (ee-flush)
    (no-raw-mode)
    (pause)
    (raw-mode)
    (carriage-return)
    (clear-eos)
    (redisplay ee entry)
    entry))

(define (ee-compose . p*)
  (rec ee-composition
    (lambda (ee entry c)
      (let f ([p* p*] [entry entry])
        (if (null? p*)
            entry
            (let ([entry ((car p*) ee entry c)])
              (and entry (f (cdr p*) entry))))))))

;;; key bindings

;;; (ee-bind-key key ee-xxx)

;;; key must evaluate to a <key>, where:
;;;
;;; <key> = <char> | <key-string>
;;;
;;; <key-string> -> "<key-char>+"
;;; <key-char> ->
;;;   \e             escape character
;;;   ^x             control is applied to character x
;;;   \\             backslash
;;;   \^             caret
;;;   <plain-char>   any character other than \ or ^
;;;
;;; <char> examples:
;;;
;;;   input key   description  byte sequence
;;;   ---------   -----------  -------------
;;;   #\a         letter 'a'   97
;;;   #\^         caret        94
;;;
;;; <key-string> examples:
;;;
;;;   input key   contents  description  byte sequence
;;;   ---------   --------  -----------  -------------
;;;   "\\ex"      \ex       Esc-x        27 120
;;;   "^a"        ^a        Ctrl-A       1
;;;   "\\\\"      \\        backslash    92
;;;   "\\^"       \^        caret        94
;;;   "a"         a         letter 'a'   97

(module (dispatch-table? base-dispatch-table ee-bind-key)
  (define make-dispatch-table
    (lambda ()
      (make-eqv-hashtable 256)))

  (define dispatch-table? hashtable?)

  (define ee-bind-key
    (lambda (key proc)
      (unless (or (char? key)
                  (and (string? key) (fx> (string-length key) 0)))
        ($oops 'ee-bind-key "~s is not a valid key (character or nonempty string)" key))
      (unless (procedure? proc)
        ($oops 'ee-bind-key "~s is not a procedure" proc))

      (if (string? key)
          (let* ([n (string-length key)])
            (define (s0 table i)
              (let ([c (string-ref key i)])
                (case c
                  [(#\\) (s-backslash table (fx+ i 1))]
                  [(#\^) (s-caret table (fx+ i 1))]
                  [else (s-lookup table (fx+ i 1) c)])))
            (define (s-backslash table i)
              (when (fx= i n)
                ($oops 'ee-bind-key
                  "malformed key ~s (nothing following \\)"
                  key))
              (let ([c (string-ref key i)])
                (case c
                  [(#\e) (s-lookup table (fx+ i 1) #\esc)]
                  [(#\\ #\^) (s-lookup table (fx+ i 1) c)]
                  [else ($oops 'ee-bind-key
                         "malformed key ~s (unexpected character following \\)"
                         key)])))
            (define (s-caret table i)
              (define (^char c)
                (integer->char (fxlogand (char->integer c) #b11111)))
              (when (fx= i n)
                ($oops 'ee-bind-key
                  "malformed key ~s (nothing following ^)"
                  key))
              (s-lookup table (fx+ i 1) (^char (string-ref key i))))
            (define (s-lookup table i key)
              (let ([x (hashtable-ref table key #f)])
                (cond
                  [(fx= i n)
                   (when (dispatch-table? x)
                     (warningf 'ee-bind-key
                       "definition for key ~s disables its use as a prefix"
                       key))
                   (hashtable-set! table key proc)]
                  [(dispatch-table? x) (s0 x i)]
                  [else
                   (when (procedure? x)
                     (warningf 'ee-bind-key
                       "definition for key ~s disables its use as a prefix"
                       key))
                   (let ([x (make-dispatch-table)])
                     (hashtable-set! table key x)
                     (s0 x i))])))
            (s0 base-dispatch-table 0))
          (begin
            (when (dispatch-table? (hashtable-ref base-dispatch-table key #f))
              (warningf 'ee-bind-key
                "definition for key ~s disables its use as a prefix"
                key))
            (hashtable-set! base-dispatch-table key proc)))))

  (define base-dispatch-table (make-dispatch-table))

 ; set up self-insertion for space and all printing characters
  (for-each
    (lambda (c) (ee-bind-key c ee-insert-self))
    (string->list " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~"))
)

(let ([ebk ee-bind-key])
 ; newline operations
  (ebk #\return   ee-newline/accept)                  ; Enter, ^M
  (ebk "^J"       ee-accept)                          ; ^J
  (ebk "^O"       ee-open-line)                       ; ^O

 ; indenting operations
  (ebk "\\e\t"    ee-indent)                          ; Esc-Tab
  (ebk "\\eq"     ee-indent-all)                      ; Esc-q
  (ebk "\\eQ"     ee-indent-all)                      ; Esc-Q
  (ebk "\\e^Q"    ee-indent-all)                      ; Esc-^Q

 ; command completion
  (ebk "\t"       ee-id-completion/indent)            ; Tab
  (ebk "^R"       ee-next-id-completion)              ; ^R

 ; cursor movement keys
  (ebk "^B"       ee-backward-char)                   ; ^B
  (ebk "\\e[D"    ee-backward-char)                   ; Left       ; ]
  (ebk "^F"       ee-forward-char)                    ; ^F
  (ebk "\\e[C"    ee-forward-char)                    ; Right      ; ]
  (ebk "^N"       ee-next-line)                       ; ^N
  (ebk "\\e[B"    ee-next-line)                       ; Down
  (ebk "^P"       ee-previous-line)                   ; ^P
  (ebk "\\e[A"    ee-previous-line)                   ; Up

  (ebk "\\ef"     ee-forward-word)                    ; Esc-f
  (ebk "\\eF"     ee-forward-word)                    ; Esc-F
  (ebk "\\e^F"    ee-forward-sexp)                    ; Esc-^F
  (ebk "\\eb"     ee-backward-word)                   ; Esc-b
  (ebk "\\eB"     ee-backward-word)                   ; Esc-B
  (ebk "\\e^B"    ee-backward-sexp)                   ; Esc-^B

  (ebk "^X^X"     ee-exchange-point-and-mark)         ; ^X^X
  (ebk "^X["      ee-backward-page)                   ; ^X[
  (ebk "^X]"      ee-forward-page)                    ; ^X]
  (ebk "\\e[5~"   ee-backward-page)                   ; Page-Up
  (ebk "\\e[6~"   ee-forward-page)                    ; Page-Down

  (ebk "^E"       ee-end-of-line)                     ; ^E
  (ebk "\\e[F"    ee-end-of-line)                     ; End key
 ; terminals are supposed to default to "normal" (aka "cursor") rather than
 ; "application" mode and in normal mode send ANSI \\e[F and \\e[H for End
 ; and Home.  although gnome terminal apparently starts in normal mode, it
 ; sends the application-mode sequences for this.  we capitulate reluctantly,
 ; since by defining Esc-OF and Esc-OH to do End and Home we prevent people
 ; from binding Esc-O by itself to a command.
  (ebk "\\eOF"    ee-end-of-line)                     ; End key (gnome terminal)
  (ebk "\\e[4~"   ee-end-of-line)                     ; End key (cygwin)
  (ebk "^A"       ee-beginning-of-line)               ; ^A
  (ebk "\\e[H"    ee-beginning-of-line)               ; Home key
  (ebk "\\eOH"    ee-beginning-of-line)               ; Home key (gnome terminal)
  (ebk "\\e[1~"   ee-beginning-of-line)               ; Home key (cygwin)
  (ebk "\\e<"     ee-beginning-of-entry)              ; Esc-<
  (ebk "\\e>"     ee-end-of-entry)                    ; Esc->      ; [[
  (ebk "\\e]"     ee-goto-matching-delimiter)         ; Esc-]
  (ebk #\(        ee-insert-paren)                    ; (
  (ebk #\)        ee-insert-paren)                    ; )
  (ebk #\[        ee-insert-paren)                    ; [
  (ebk #\]        ee-insert-paren)                    ; ]
  (ebk "^]"       ee-flash-matching-delimiter)        ; ^]

 ; destructive functions
  (ebk "^U"       ee-delete-line)                     ; ^U
  (ebk "^K"       ee-delete-to-eol)                   ; ^K
  (ebk "\\ek"     ee-delete-to-eol)                   ; Esc-k
  (ebk "^W"       ee-delete-between-point-and-mark)   ; ^W
  (ebk "^G"       ee-delete-entry)                    ; ^G
  (ebk "^C"       ee-reset-entry)                     ; ^C
  (ebk "\\e^K"    ee-delete-sexp)                     ; Esc-^K
  (ebk "\\e\\e[3~" ee-delete-sexp)                    ; Esc-Delete
  (ebk "\\e\177"  ee-backward-delete-sexp)            ; Esc-Backspace
  (ebk "\\e^H"    ee-backward-delete-sexp)            ; Esc-^H
  (ebk "^V"       ee-yank-selection)                  ; ^V
  (ebk "^Y"       ee-yank-kill-buffer)                ; ^Y
  (ebk "^D"       ee-eof/delete-char)                 ; ^D
  (ebk #\rubout   ee-backward-delete-char)            ; Backspace (<--)
  (ebk "\\e[3~"   ee-delete-char)                     ; Delete 
  (ebk "^H"       ee-backward-delete-char)            ; ^H
  (ebk "^@"       ee-set-mark)                        ; ^@ (or ^Space)
  (ebk "^^"       ee-set-mark)                        ; ^^

 ; display functions
  (ebk "^L"       ee-redisplay)                       ; ^L

 ; string macros
  (ebk "\\ed"     (ee-string-macro "(define "))       ; Esc-d   ; )
  (ebk "\\el"     (ee-string-macro "(lambda "))       ; Esc-l   ; )

 ; history keys
  (ebk "\\e^P"    ee-history-bwd)                     ; Esc-^P
  (ebk "\\e\\e[A" ee-history-bwd)                     ; Esc-Up
  (ebk "\\e^N"    ee-history-fwd)                     ; Esc-^N
  (ebk "\\e\\e[B" ee-history-fwd)                     ; Esc-Down
  (ebk "\\ep"     ee-history-bwd-prefix)              ; Esc-p
  (ebk "\\eP"     ee-history-bwd-contains)            ; Esc-P
  (ebk "\\en"     ee-history-fwd-prefix)              ; Esc-n
  (ebk "\\eN"     ee-history-fwd-contains)            ; Esc-N

 ; misc
  (ebk "\\e^U"   ee-command-repeat)                   ; Esc-^U
  (ebk "^Z"      ee-suspend-process)                  ; ^Z
)

(set! $expeditor
  (lambda (thunk)
    (let ([ee #f])
      (define (expeditor-prompt-and-read n)
        (if (cond
              [(eestate? ee) #t]
              [(eq? ee 'failed) #f]
              [(init-screen)
               (set! ee (make-eestate))
               (let ([histfile ($expeditor-history-file)])
                 (when histfile
                   (on-error (void) (ee-load-history ee histfile))))
               #t]
              [else (set! ee 'failed) #f])
            (ee-prompt-and-read ee n)
            (default-prompt-and-read n)))
      (let-values ([val* (parameterize ([waiter-prompt-and-read expeditor-prompt-and-read])
                           (thunk))])
        (when (eestate? ee)
          (let ([histfile ($expeditor-history-file)])
            (when histfile
              (on-error (void) (ee-save-history ee histfile)))))
        (apply values val*)))))
)

) ; when-feature expeditor
