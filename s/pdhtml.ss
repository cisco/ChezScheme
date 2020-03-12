;;; pdhtml.ss
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

;;; NOTES:
;;; - fixed bug in define-tags: moved (void) end of text ... to start
;;;
;;; - to change palette to use white background with colorized text:
;;;   (profile-palette
;;;     (vector-map
;;;       (lambda (p) (cons "white" (car p)))
;;;       (profile-palette)))

;;;  profile-dump-html suggestions from Oscar:
;;;
;;;  We could probably build a table mapping source regions to procedure names
;;;  in enough cases to actually be useful.  If so, showing procedure name instead
;;;  of line/char position would help the user get a high-level perspective on the
;;;  profile results.  Right now the user has to synthesize that perspective by
;;;  remembering where each link led.
;;;
;;;  Within the file view window, it would be nice to have a way to scan quickly
;;;  through the hot spots within that file (we have some obscenely large source
;;;  files at work).  Perhaps you could reprise the profile spectrum horizontally
;;;  in a short frame at the top of the window and rig it so that dragging, scroll
;;;  wheel, or clicking on a color cycles through the regions tagged with that col>
;;;
;;;  With a large range of profile counts to compress into a fairly small
;;;  spectrum, it might be nice if there were a way to zoom into a range by
;;;  clicking on the legend, either in the overview window or the file window.
;;;  Reallocating the color map could be confusing with multiple windows open,
;;;  but perhaps there's some javascript way to rig all the other colors to
;;;  desaturate when you zoom into a range in one window.  Perhaps intensity
;;;  could be used to show the sub-ranges in varying shades of the main legend
;;;  color.
;;;
;;;  I notice that the profile annotations on the when expressions start at the te>
;;;  expression rather than the start of the when.  Yet the if expression annotati>
;;;  starts at the beginning of the if expression and extends to the closing paren.
;;;  Not sure if that made any sense, basically I'm trying to say that the "(when"
;;;  itself (and closing paren) isn't colored the same as the test part.
;;;  I don't remember exactly how we handled source annotations during wrapping and
;;;  unwrapping, but it seems offhand that it might make sense to wrap the input
;;;  source annotation around the transformer output so that the source info for t>
;;;  when expression is transferred to the generated if expression.

(begin
(let ()
  (include "types.ss")
  (module (make-tracker tracker-profile-ct)
    (define-record-type tracker
      (nongenerative)
      (fields profile-ct)))
  (define-record-type cc
    (nongenerative)
    (fields (mutable cookie) (mutable total) (mutable current) (mutable preceding)))
  (define-record-type (source-table $make-source-table $source-table?)
    (nongenerative)
    (sealed #t)
    (opaque #t)
    (fields ht)
    (protocol
      (lambda (new)
        (lambda ()
          (define sfd-hash
            (lambda (sfd)
              (source-file-descriptor-crc sfd)))
          (define sfd=?
            (lambda (sfd1 sfd2)
              (and (fx= (source-file-descriptor-crc sfd1) (source-file-descriptor-crc sfd2))
                   (= (source-file-descriptor-length sfd1) (source-file-descriptor-length sfd2))
                   (string=? (source-file-descriptor-name sfd1) (source-file-descriptor-name sfd2)))))
          (new (make-hashtable sfd-hash sfd=?))))))
  (define *local-profile-trackers* '())
  (define op+ car)
  (define op- cdr)
  (define count+ (constant-case ptr-bits [(32) +] [(64) fx+]))
  (define count- (constant-case ptr-bits [(32) -] [(64) fx-]))
  (define count< (constant-case ptr-bits [(32) <] [(64) fx<]))
  (define get-counter-list (foreign-procedure "(cs)s_profile_counters" () ptr))
  (define release-counters (foreign-procedure "(cs)s_profile_release_counters" () ptr))

  (define rblock-count
    (lambda (rblock)
      (let sum ((op (rblock-op rblock)))
        (if (profile-counter? op)
            (profile-counter-count op)
            ; using #3%fold-left in case the #2% versions are profiled
            (#3%fold-left
              (lambda (a op) (count- a (sum op)))
              (#3%fold-left (lambda (a op) (count+ a (sum op))) 0 (op+ op))
              (op- op))))))

  (define profile-counts
    ; like profile-dump but returns ((count . (src ...)) ...)
    (case-lambda
      [() (profile-counts (get-counter-list))]
      [(counter*)
       ; disabiling interrupts so we don't sum part of the counters for a block before
       ; an interrupt and the remaining counters after the interrupt, which can lead
       ; to inaccurate (and possibly negative) counts.  we could disable interrupts just
       ; around the body of rblock-count to shorten the windows during which interrupts
       ; are disabled, but doing it here incurs less overhead
       (with-interrupts-disabled
         (fold-left
           (lambda (r x)
             (fold-left
               (lambda (r rblock)
                 (cons (cons (rblock-count rblock) (rblock-srecs rblock)) r))
               r (cdr x)))
           '() counter*))]))

  (define (snapshot who uncleared-count* cleared-count*)
    (lambda (tracker)
      (define cookie (cons 'vanilla 'wafer))
      ; set current corresponding to each src to a total of its counts
      (let ([incr-current
              (lambda (count.src*)
                (let ([count (car count.src*)])
                  (for-each
                    (lambda (src)
                      (let ([a ($source-table-cell (tracker-profile-ct tracker) src #f)])
                        (when (count< count 0) (errorf who "negative profile count ~s for ~s" count src))
                        (let ([cc (cdr a)])
                          (if cc
                              (if (eq? (cc-cookie cc) cookie)
                                  (cc-current-set! cc (count+ (cc-current cc) count))
                                  (begin
                                    (cc-cookie-set! cc cookie)
                                    (cc-current-set! cc count)))
                              (set-cdr! a (make-cc cookie 0 count 0))))))
                    (cdr count.src*))))])
        (for-each incr-current uncleared-count*)
        (for-each incr-current cleared-count*))
      ; then increment total of each affected cc by the delta between current and preceding
      (source-table-for-each
        (lambda (src cc)
          (when (eq? (cc-cookie cc) cookie)
            (let ([current (cc-current cc)])
              (let ([delta (count- current (cc-preceding cc))])
                (unless (eqv? delta 0)
                  (when (count< delta 0) (errorf who "total profile count for ~s dropped from ~s to ~s" src (cc-preceding cc) current))
                  (cc-total-set! cc (count+ (cc-total cc) delta))
                  (cc-preceding-set! cc current))))))
        (tracker-profile-ct tracker))
      ; then reduce preceding by cleared counts
      (for-each
        (lambda (count.src*)
          (let ([count (car count.src*)])
            (for-each
              (lambda (src)
                (let ([a ($source-table-cell (tracker-profile-ct tracker) src #f)])
                  (let ([cc (cdr a)])
                    (if cc
                        (cc-preceding-set! cc (count- (cc-preceding cc) count))
                        (set-cdr! a (make-cc cookie 0 0 0))))))
              (cdr count.src*))))
        cleared-count*)))

  (define adjust-trackers!
    (lambda (who uncleared-counter* cleared-counter*)
      (let ([local-tracker* *local-profile-trackers*])
        (unless (null? local-tracker*)
          (let ([uncleared-count* (profile-counts uncleared-counter*)]
                [cleared-count* (profile-counts cleared-counter*)])
            (let ([snapshot (snapshot who uncleared-count* cleared-count*)])
              (for-each snapshot local-tracker*)))))))

  (define $source-table-contains?
    (lambda (st src)
      (let ([src-ht (hashtable-ref (source-table-ht st) (source-sfd src) #f)])
        (and src-ht (hashtable-contains? src-ht src)))))

  (define $source-table-ref
    (lambda (st src default)
      (let ([src-ht (hashtable-ref (source-table-ht st) (source-sfd src) #f)])
        (if src-ht (hashtable-ref src-ht src default) default))))

  (define $source-table-cell
    (lambda (st src default)
      (define same-sfd-src-hash
        (lambda (src)
          (source-bfp src)))
      (define same-sfd-src=?
        (lambda (src1 src2)
          (and (= (source-bfp src1) (source-bfp src2))
               (= (source-efp src1) (source-efp src2)))))
      (let ([src-ht (let ([a (hashtable-cell (source-table-ht st) (source-sfd src) #f)])
                      (or (cdr a)
                          (let ([src-ht (make-hashtable same-sfd-src-hash same-sfd-src=?)])
                            (set-cdr! a src-ht)
                            src-ht)))])
        (hashtable-cell src-ht src default))))

  (define $source-table-delete!
    (lambda (st src)
      (let ([ht (source-table-ht st)] [sfd (source-sfd src)])
        (let ([src-ht (hashtable-ref ht sfd #f)])
          (when src-ht
            (hashtable-delete! src-ht src)
            (when (fx= (hashtable-size src-ht) 0)
              (hashtable-delete! ht sfd)))))))

  (define source-table-for-each
    (lambda (p st)
      (vector-for-each
        (lambda (src-ht)
          (let-values ([(vsrc vcount) (hashtable-entries src-ht)])
            (vector-for-each p vsrc vcount)))
        (hashtable-values (source-table-ht st)))))

  (set-who! profile-clear
    (lambda ()
      (define clear-links
        (lambda (op)
          (if (profile-counter? op)
              (profile-counter-count-set! op 0)
              (begin
                (for-each clear-links (op+ op))
                (for-each clear-links (op- op))))))
      (let ([counter* (get-counter-list)])
        (adjust-trackers! who '() counter*)
        (for-each
          (lambda (x)
            (for-each
              (lambda (node) (clear-links (rblock-op node)))
              (cdr x)))
          counter*))))

  (set-who! profile-release-counters
    (lambda ()
      ; release-counters prunes out (and hands back) the released counters
      (let* ([dropped-counter* (release-counters)]
             [kept-counter* (get-counter-list)])
        (adjust-trackers! who kept-counter* dropped-counter*))))

  (set-who! profile-dump
    ; like profile-counts but returns ((src . count) ...), which requires more allocation
    ; profile-dump could use profile-counts but that would require even more allocation
    (lambda ()
      ; could disable interrupts just around each call to rblock-count, but doing it here incurs less overhead
      (with-interrupts-disabled
        (fold-left
          (lambda (r x)
            (fold-left
              (lambda (r rblock)
                (let ([count (rblock-count rblock)])
                  (fold-left
                    (lambda (r src)
                      (cons (cons src count) r))
                    r (rblock-srecs rblock))))
              r (cdr x)))
          '() (get-counter-list)))))

  (set-who! make-source-table
    (lambda ()
      ($make-source-table)))

  (set-who! source-table?
    (lambda (x)
      ($source-table? x)))

  (set-who! source-table-size
    (lambda (st)
      (unless ($source-table? st) ($oops who "~s is not a source table" st))
      (let ([vsrc-ht (hashtable-values (source-table-ht st))])
        (let ([n (vector-length vsrc-ht)])
          (do ([i 0 (fx+ i 1)] [size 0 (fx+ size (hashtable-size (vector-ref vsrc-ht i)))])
            ((fx= i n) size))))))

  (set-who! source-table-contains?
    (lambda (st src)
      (unless ($source-table? st) ($oops who "~s is not a source table" st))
      (unless (source? src) ($oops who "~s is not a source object" src))
      ($source-table-contains? st src)))

  (set-who! source-table-ref
    (lambda (st src default)
      (unless ($source-table? st) ($oops who "~s is not a source table" st))
      (unless (source? src) ($oops who "~s is not a source object" src))
      ($source-table-ref st src default)))

  (set-who! source-table-set!
    (lambda (st src val)
      (unless ($source-table? st) ($oops who "~s is not a source table" st))
      (unless (source? src) ($oops who "~s is not a source object" src))
      (set-cdr! ($source-table-cell st src #f) val)))

  (set-who! source-table-delete!
    (lambda (st src)
      (unless ($source-table? st) ($oops who "~s is not a source table" st))
      (unless (source? src) ($oops who "~s is not a source object" src))
      ($source-table-delete! st src)))

  (set-who! source-table-cell
    (lambda (st src default)
      (unless ($source-table? st) ($oops who "~s is not a source table" st))
      (unless (source? src) ($oops who "~s is not a source object" src))
      ($source-table-cell st src default)))

  (set-who! source-table-dump
    (lambda (st)
      (unless ($source-table? st) ($oops who "~s is not a source table" st))
      (let* ([vsrc-ht (hashtable-values (source-table-ht st))]
             [n (vector-length vsrc-ht)])
        (do ([i 0 (fx+ i 1)]
             [dumpit* '()
               (let-values ([(vsrc vcount) (hashtable-entries (vector-ref vsrc-ht i))])
                 (let ([n (vector-length vsrc)])
                   (do ([i 0 (fx+ i 1)]
                        [dumpit* dumpit*
                          (cons (cons (vector-ref vsrc i) (vector-ref vcount i)) dumpit*)])
                     ((fx= i n) dumpit*))))])
          ((fx= i n) dumpit*)))))

  (set-who! put-source-table
    (lambda (op st)
      (unless (and (output-port? op) (textual-port? op)) ($oops who "~s is not a textual output port" op))
      (unless ($source-table? st) ($oops who "~s is not a source table" st))
      (fprintf op "(source-table")
      (let-values ([(vsfd vsrc-ht) (hashtable-entries (source-table-ht st))])
        (vector-for-each
          (lambda (sfd src-ht)
            (let-values ([(vsrc vval) (hashtable-entries src-ht)])
              (let ([n (vector-length vsrc)])
                (unless (fx= n 0)
                  (fprintf op "\n (file ~s ~s"
                    (source-file-descriptor-name sfd)
                    (source-file-descriptor-checksum sfd))
                  (let ([v (vector-sort (lambda (x1 x2) (< (vector-ref x1 0) (vector-ref x2 0)))
                              (vector-map (lambda (src val) (vector (source-bfp src) (source-efp src) val)) vsrc vval))])
                    (let loop ([i 0] [last-bfp 0])
                      (unless (fx= i n)
                        (let ([x (vector-ref v i)])
                          (let ([bfp (vector-ref x 0)] [efp (vector-ref x 1)] [val (vector-ref x 2)])
                            (let ([offset (- bfp last-bfp)] [len (- efp bfp)])
                              (fprintf op " (~s ~s ~s)" offset len val))
                            (loop (fx+ i 1) bfp))))))
                  (fprintf op ")")))))
          vsfd vsrc-ht))
      (fprintf op ")\n")))

  (set-who! get-source-table!
    (rec get-source-table!
      (case-lambda
        [(ip st) (get-source-table! ip st #f)]
        [(ip st combine)
         (define (nnint? x) (and (integer? x) (exact? x) (nonnegative? x)))
         (define (token-oops what bfp)
           (if bfp
               ($oops who "expected ~a at file position ~s of ~s" what bfp ip)
               ($oops who "malformed source table reading from ~a" ip)))
         (define (next-token expected-type expected-value? what)
           (let-values ([(type val bfp efp) (read-token ip)])
             (unless (and (eq? type expected-type) (expected-value? val)) (token-oops what bfp))
             val))
         (unless (and (input-port? ip) (textual-port? ip)) ($oops who "~s is not a textual input port" ip))
         (unless ($source-table? st) ($oops who "~s is not a source table" st))
         (unless (or (not combine) (procedure? combine)) ($oops who "~s is not a procedure" combine))
         (next-token 'lparen not "open parenthesis")
         (next-token 'atomic (lambda (x) (eq? x 'source-table)) "identifier 'source-table'")
         (let file-loop ()
           (let-values ([(type val bfp efp) (read-token ip)])
             (unless (eq? type 'rparen)
               (unless (eq? type 'lparen) (token-oops "open parenthesis" bfp))
               (next-token 'atomic (lambda (x) (eq? x 'file)) "identifier 'file'")
               (let* ([path (next-token 'atomic string? "string")]
                      [checksum (next-token 'atomic nnint? "checksum")])
                 (let ([sfd (#%source-file-descriptor path checksum)])
                   (let entry-loop ([last-bfp 0])
                     (let-values ([(type val bfp efp) (read-token ip)])
                       (unless (eq? type 'rparen)
                         (unless (eq? type 'lparen) (token-oops "open parenthesis" bfp))
                         (let* ([bfp (+ last-bfp (next-token 'atomic nnint? "file position"))]
                                [efp (+ bfp (next-token 'atomic nnint? "file position"))]
                                [val (get-datum ip)])
                           (next-token 'rparen not "close parenthesis")
                           (let ([a ($source-table-cell st (make-source-object sfd bfp efp) #f)])
                             (set-cdr! a
                               (if (and (cdr a) combine)
                                   (combine (cdr a) val)
                                   val)))
                           (entry-loop bfp)))))))
               (file-loop))))])))

  (set-who! with-profile-tracker
    (rec with-profile-tracker
      (case-lambda
        [(thunk) (with-profile-tracker #f thunk)]
        [(include-existing-counts? thunk)
         (define extract-covered-entries
           (lambda (profile-ct) 
             (let ([covered-ct ($make-source-table)])
               (source-table-for-each
                 (lambda (src cc)
                   (let ([count (cc-total cc)])
                     (unless (eqv? count 0)
                       ($source-table-cell covered-ct src count))))
                 profile-ct)
               covered-ct)))
         (unless (procedure? thunk) ($oops who "~s is not a procedure" thunk))
         (let* ([profile-ct ($make-source-table)]
                [tracker (make-tracker profile-ct)])
           (unless include-existing-counts?
             ; set preceding corresponding to each src to a total of its dumpit counts
             ; set total to zero, since we don't want to count anything from before
             (for-each
               (lambda (count.src*)
                 (let ([count (car count.src*)])
                   (for-each
                     (lambda (src)
                       (let ([a ($source-table-cell profile-ct src #f)])
                         (let ([cc (cdr a)])
                           (if cc
                               (cc-preceding-set! cc (count+ (cc-preceding cc) count))
                               (set-cdr! a (make-cc #f 0 0 count))))))
                     (cdr count.src*))))
               (profile-counts)))
           ; register for possible adjustment by profile-clear and profile-release-counters
           (let-values ([v* (fluid-let ([*local-profile-trackers* (cons tracker *local-profile-trackers*)]) (thunk))])
             ; increment the recorded counts by the now current counts.
             ((snapshot who (profile-counts) '()) tracker)
             (apply values (extract-covered-entries profile-ct) v*)))]))))

(let ()
  (include "types.ss")

  (define check-dump
    (lambda (who x)
      (unless (and (list? x)
                   (andmap (lambda (x)
                             (and (pair? x)
                                  (source-object? (car x))
                                  (let ([x (cdr x)])
                                    (and (integer? x) (exact? x)))))
                     x))
        ($oops  who "invalid dump ~s" x))))

  (define-record-type filedata
    (fields
      (immutable sfd)
      (immutable ip)
      (mutable entry*)
     ; remaining fields are ignored by profile-dump-list
      (mutable max-count)
      (mutable ci)
      (mutable htmlpath)
      (mutable htmlfn)
      (mutable winid))
    (nongenerative)
    (sealed #t)
    (protocol
      (lambda (new)
        (lambda (sfd ip)
          (new sfd ip '() #f #f #f #f #f)))))

  (define-record-type entrydata
    (fields
      (immutable fdata)
      (immutable bfp)
      (immutable efp)
      (mutable count)
      (mutable line)
      (mutable char)
     ; ci is ignored by profile-dump-list
      (mutable ci))
    (nongenerative)
    (sealed #t)
    (protocol
      (lambda (new)
        (lambda (fdata bfp efp count)
          (new fdata bfp efp count #f #f #f)))))

  (define (gather-filedata who warn? dumpit*)
   ; returns list of fdata records, each holding a list of entries
   ; the entries are sorted based on their (unique) bfps
    (let ([fdata-ht (make-hashtable
                      (lambda (x) (source-file-descriptor-crc x))
                      (lambda (x y)
                       ; there's no way to make this foolproof, so we identify paths with
                       ; same crc, length, and last component.  this can cause problems
                       ; only if two copies of the same file are loaded and used.
                        (or (eq? x y)
                            (and (= (source-file-descriptor-crc x)
                                    (source-file-descriptor-crc y))
                                 (= (source-file-descriptor-length x)
                                    (source-file-descriptor-length y))
                                 (string=?
                                   (path-last (source-file-descriptor-name x))
                                   (path-last (source-file-descriptor-name y)))))))])
      (define (open-source sfd)
        (cond
          [(hashtable-ref fdata-ht sfd #f)]
          [($open-source-file sfd) =>
           (lambda (ip)
             (let ([fdata (make-filedata sfd ip)])
               (hashtable-set! fdata-ht sfd fdata)
               fdata))]
          [else
           (when warn?
             (warningf who
               "unmodified source file ~s not found in source directories"
               (source-file-descriptor-name sfd)))
           (let ([fdata (make-filedata sfd #f)])
             (hashtable-set! fdata-ht sfd fdata)
             fdata)]))
      (for-each
        (lambda (dumpit)
          (let ([source (car dumpit)])
            (assert (source? source))
            (let ([bfp (source-bfp source)])
              (when (>= bfp 0) ; weed out block-profiling entries, whose bfps are negative
                (let ([fdata (open-source (source-sfd source))])
                  (filedata-entry*-set! fdata
                    (cons (make-entrydata fdata bfp (source-efp source) (cdr dumpit))
                      (filedata-entry* fdata))))))))
        dumpit*)
      (let ([fdatav (hashtable-values fdata-ht)])
        (vector-for-each
          (lambda (fdata)
            (let ([entry* (sort (lambda (x y)
                                  (or (> (entrydata-bfp x) (entrydata-bfp y))
                                      (and (= (entrydata-bfp x) (entrydata-bfp y))
                                           (< (entrydata-efp x) (entrydata-efp y)))))
                                (filedata-entry* fdata))])
              #;(assert (not (null? entry*)))
              (let loop ([entry (car entry*)] [entry* (cdr entry*)] [new-entry* '()])
                (if (null? entry*)
                    (filedata-entry*-set! fdata (cons entry new-entry*))
                    (if (and (= (entrydata-bfp (car entry*)) (entrydata-bfp entry))
                             (= (entrydata-efp (car entry*)) (entrydata-efp entry)))
                        (begin
                          (entrydata-count-set! entry
                            (+ (entrydata-count entry)
                               (entrydata-count (car entry*))))
                          (loop entry (cdr entry*) new-entry*))
                        (loop (car entry*) (cdr entry*) (cons entry new-entry*)))))))
          fdatav)
        (vector->list fdatav))))

  (let ()
    (define (scan-file fdata)
      (let ([ip (filedata-ip fdata)] [line 1] [char 1])
        (define (read-until bfp next)
          (let loop ([bfp bfp])
            (unless (= bfp next)
              (cond
                [(eqv? (read-char ip) #\newline)
                 (set! line (+ line 1))
                 (set! char 1)]
                [else (set! char (+ char 1))])
              (loop (+ bfp 1)))))
        (let ([entry* (filedata-entry* fdata)]) ; already sorted by gather-filedata
          (let f ([bfp 0] [entry* entry*])
            (unless (null? entry*)
              (let ([entry (car entry*)] [entry* (cdr entry*)])
                (let ([next (entrydata-bfp entry)])
                  (read-until bfp next)
                  (entrydata-line-set! entry line)
                  (entrydata-char-set! entry char)
                  (f next entry*))))))))

    (set-who! profile-dump-list
     ; return list of lists of:
     ;   - count
     ;   - path      ; current if line and char are not #f
     ;   - bfp
     ;   - efp
     ;   - line      ; may be #f
     ;   - char      ; may be #f
      (rec profile-dump-list
        (case-lambda
          [() (profile-dump-list #t)]
          [(warn?) (profile-dump-list warn? (profile-dump))]
          [(warn? dumpit*)
           (check-dump who dumpit*)
           (let ([fdata* (gather-filedata who warn? dumpit*)])
             (for-each scan-file (remp (lambda (x) (not (filedata-ip x))) fdata*))
             (let ([ls (map (lambda (entry)
                              (let ([fdata (entrydata-fdata entry)])
                                (list
                                  (entrydata-count entry)
                                  (cond
                                    [(filedata-ip fdata) => port-name]
                                    [else (source-file-descriptor-name
                                            (filedata-sfd fdata))])
                                  (entrydata-bfp entry)
                                  (entrydata-efp entry)
                                  (entrydata-line entry)
                                  (entrydata-char entry))))
                         (sort
                           (lambda (x y) (> (entrydata-count x) (entrydata-count y)))
                           (apply append (map filedata-entry* fdata*))))])
               (for-each
                 (lambda (fdata) (cond [(filedata-ip fdata) => close-input-port]))
                 fdata*)
               ls))]))))

  (let ()
    (define-record-type profilit
      (nongenerative #{profilit iw9f7z5ovg4jjetsvw5m0-2})
      (sealed #t)
      (fields sfd bfp efp weight))
    (define make-profile-database
      (lambda ()
        (make-hashtable
          source-file-descriptor-crc
          (lambda (x y)
            (or (eq? x y)
                (and (= (source-file-descriptor-crc x)
                        (source-file-descriptor-crc y))
                     (= (source-file-descriptor-length x)
                        (source-file-descriptor-length y))
                     (string=?
                       (path-last (source-file-descriptor-name x))
                       (path-last (source-file-descriptor-name y)))))))))

    (define profile-database #f)
    (define profile-source-data? #f)
    (define profile-block-data? #f)
    (define update-sfd!
      (lambda (cell sfd)
        ; if the recorded sfd is the same but not eq, it's likely from an earlier session.
        ; overwrite so remaining hashtable equality-procedure checks are more likely to
        ; succeed at the eq? check
        (unless (eq? (car cell) sfd)
          (set-car! cell sfd))))
    (set-who! profile-clear-database
      (lambda ()
        (set! profile-database #f)))
    (set-who! profile-dump-data
      (rec profile-dump-data
        (case-lambda
          [(ofn) (profile-dump-data ofn (profile-dump))]
          [(ofn dumpit*)
           (check-dump who dumpit*)
           (let ([op ($open-file-output-port who ofn (file-options replace))])
             (on-reset (delete-file ofn #f)
               (on-reset (close-port op)
                 (let* ([dump dumpit*] [max-count (inexact (fold-left max 1 (map cdr dump)))])
                   (for-each
                     (lambda (dumpit)
                       (let ([source (car dumpit)] [count (cdr dumpit)])
                         (fasl-write
                           (make-profilit (source-sfd source) (source-bfp source) (source-efp source)
                             ; compute weight as % of max count
                             (fl/ (inexact count) max-count))
                           op)))
                     dump)))
               (close-port op)))])))
    (set! $profile-source-data? (lambda () profile-source-data?))
    (set! $profile-block-data? (lambda () profile-block-data?))
    (set-who! profile-load-data
      (lambda ifn*
        (define populate!
          (lambda (x)
            (unless (profilit? x) ($oops who "invalid profile data element ~s" x))
            (unless profile-database (set! profile-database (make-profile-database)))
            (let ([ht (let* ([sfd (profilit-sfd x)]
                             [cell (hashtable-cell profile-database sfd #f)])
                        (update-sfd! cell sfd)
                        (or (cdr cell)
                            (let ([ht (make-hashtable values =)])
                              (set-cdr! cell ht)
                              ht)))])
              ; each ht entry is an alist mapping efp -> (weight . n) where n is
              ; the number of contributing entries so far for this sfd, bfp, and efp.
              ; n is used to compute the average weight of the contributing entries.
              (let ([bfp.alist (hashtable-cell ht (profilit-bfp x) '())])
                (cond
                  [(assv (profilit-efp x) (cdr bfp.alist)) =>
                   (lambda (a)
                     (let ([weight.n (cdr a)])
                       (let ([weight (car weight.n)] [n (cdr weight.n)])
                         (let ([new-n (fl+ n 1.0)])
                           (set-car! weight.n (fl/ (fl+ (* weight n) (profilit-weight x)) new-n))
                           (set-cdr! weight.n new-n)))))]
                  [else (set-cdr! bfp.alist (cons (cons* (profilit-efp x) (profilit-weight x) 1.0) (cdr bfp.alist)))])))
            (if (fxnegative? (profilit-bfp x))
                (set! profile-block-data? #t)
                (set! profile-source-data? #t))))
        (define (load-file ifn)
          (let ([ip ($open-file-input-port who ifn)])
            (on-reset (close-port ip)
              (let f ()
                (let ([x (fasl-read ip)])
                  (unless (eof-object? x)
                    (with-tc-mutex (populate! x))
                    (f)))))
            (close-port ip)))
        (for-each
          (lambda (ifn)
            (unless (string? ifn) ($oops who "~s is not a string" ifn)))
          ifn*)
        (for-each load-file ifn*)))
    (set! $profile-show-database
      (lambda ()
        (when profile-database
          (let-values ([(sfd* ht*) (hashtable-entries profile-database)])
            (vector-for-each
              (lambda (sfd ht)
                (printf "~a:\n" (source-file-descriptor-name sfd))
                (let-values ([(bfp* alist*) (hashtable-entries ht)])
                  (vector-for-each
                    (lambda (bfp alist)
                      (for-each
                        (lambda (a) (printf "  ~s, ~s: ~s\n" bfp (car a) (cadr a)))
                        alist))
                    bfp* alist*)))
              sfd* ht*)))))
    (set! profile-query-weight
      (lambda (x)
        (define src->weight
          (lambda (src)
            (cond
              [(and profile-database
                    (let* ([sfd (source-object-sfd src)]
                           [ht (hashtable-ref profile-database sfd #f)])
                      (and ht
                           (begin
                             ; could do just one lookup if we had a nondestructive variant of
                             ; hashtable-cell to call above
                             (update-sfd! (hashtable-cell profile-database sfd #f) sfd)
                             ht)))) =>
               (lambda (ht)
                 (let ([alist (hashtable-ref ht (source-object-bfp src) '())])
                   (cond
                     [(assv (source-object-efp src) alist) => cadr]
                     [(and (fxnegative? (source-object-bfp src)) (not (null? alist)))
                      ($oops #f "block-profiling info is out-of-date for ~s"
                        (source-file-descriptor-name (source-object-sfd src)))]
                     ; no info for given bfp, efp...assume dead code and return 0
                     [else 0.0])))]
              ; no info for given sfd...assume not profiled and return #f
              [else #f])))
        (if (source? x)
            (src->weight x)
            (let ([x (syntax->annotation x)])
              (if (annotation? x)
                  (src->weight (annotation-source x))
                  #f))))))

  (let ()
    ;;; The following copyright notice goes with the %html module.

    ;;; Copyright (c) 2005 R. Kent Dybvig

    ;;; Permission is hereby granted, free of charge, to any person obtaining a
    ;;; copy of this software and associated documentation files (the "Software"),
    ;;; to deal in the Software without restriction, including without limitation
    ;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
    ;;; and/or sell copies of the Software, and to permit persons to whom the
    ;;; Software is furnished to do so, subject to the following conditions:

    ;;; The above copyright notice and this permission notice shall be included in
    ;;; all copies or substantial portions of the Software.

    ;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
    ;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
    ;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
    ;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
    ;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
    ;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
    ;;; DEALINGS IN THE SOFTWARE.

    (module %html ((<html> <*> attribute $tag)
                   (<head> <*> attribute $tag)
                   (<body> <*> attribute $tag)
                   (<script> <*> attribute $tag)
                   (<style> <*> attribute $tag)
                   (<title> <*> attribute $tag)
                   (<base> <*> attribute $tag)
                   (<link> <*> attribute $tag)
                   (<meta> <*> attribute $tag)
                   (<address> <*> attribute $tag)
                   (<blockquote> <*> attribute $tag)
                   (<del> <*> attribute $tag)
                   (<div> <*> attribute $tag)
                   (<h1> <*> attribute $tag)
                   (<h2> <*> attribute $tag)
                   (<h3> <*> attribute $tag)
                   (<h4> <*> attribute $tag)
                   (<h5> <*> attribute $tag)
                   (<h6> <*> attribute $tag)
                   (<ins> <*> attribute $tag)
                   (<noscript> <*> attribute $tag)
                   (<p> <*> attribute $tag)
                   (<pre> <*> attribute $tag)
                   (<hr> <*> attribute $tag)
                   (<dd> <*> attribute $tag)
                   (<dl> <*> attribute $tag)
                   (<dt> <*> attribute $tag)
                   (<li> <*> attribute $tag)
                   (<ol> <*> attribute $tag)
                   (<ul> <*> attribute $tag)
                   (<table> <*> attribute $tag)
                   (<caption> <*> attribute $tag)
                   (<colgroup> <*> attribute $tag)
                   (<thead> <*> attribute $tag)
                   (<tfoot> <*> attribute $tag)
                   (<tbody> <*> attribute $tag)
                   (<tr> <*> attribute $tag)
                   (<td> <*> attribute $tag)
                   (<th> <*> attribute $tag)
                   (<col> <*> attribute $tag)
                   (<form> <*> attribute $tag)
                   (<button> <*> attribute $tag)
                   (<fieldset> <*> attribute $tag)
                   (<legend> <*> attribute $tag)
                   (<label> <*> attribute $tag)
                   (<select> <*> attribute $tag)
                   (<optgroup> <*> attribute $tag)
                   (<option> <*> attribute $tag)
                   (<textarea> <*> attribute $tag)
                   (<input> <*> attribute $tag)
                   (<a> <*> attribute $tag)
                   (<bdo> <*> attribute $tag)
                   (<map> <*> attribute $tag)
                   (<object> <*> attribute $tag)
                   (<q> <*> attribute $tag)
                   (<span> <*> attribute $tag)
                   (<sub> <*> attribute $tag)
                   (<sup> <*> attribute $tag)
                   (<br> <*> attribute $tag)
                   (<img> <*> attribute $tag)
                   (<area> <*> attribute $tag)
                   (<param> <*> attribute $tag)
                   (<abbr> <*> attribute $tag)
                   (<acronym> <*> attribute $tag)
                   (<cite> <*> attribute $tag)
                   (<code> <*> attribute $tag)
                   (<dfn> <*> attribute $tag)
                   (<em> <*> attribute $tag)
                   (<kbd> <*> attribute $tag)
                   (<samp> <*> attribute $tag)
                   (<strong> <*> attribute $tag)
                   (<var> <*> attribute $tag)
                   (<b> <*> attribute $tag)
                   (<big> <*> attribute $tag)
                   (<i> <*> attribute $tag)
                   (<small> <*> attribute $tag)
                   (<tt> <*> attribute $tag)
                   <doctype>
                   html-text nbsp encode-url-parameter flush-html-output)
      (define $tag
        (lambda (tag attributes text end-tag)
          (define (simple-value? s)
            (define (simple-char? c)
              (or (char<=? #\0 c #\9)
                  (char<=? #\a c #\z)
                  (char<=? #\A c #\Z)
                  (char=? c #\-)
                  (char=? c #\.)))
            (let ([n (string-length s)])
              (and (fx> n 0)
                   (let f ([i (fx- n 1)])
                     (and (simple-char? (string-ref s i))
                          (or (fx= i 0) (f (fx- i 1))))))))
          (printf "<~a" tag)
          (for-each
            (lambda (a)
              (if (pair? a)
                  (let ([value (let ([s (cdr a)])
                                 (if (string? s)
                                     s
                                     (format "~a" (cdr a))))])
                    (if (simple-value? value)
                        (printf " ~a=~a" (car a) value)
                        (let ([n (string-length value)])
                          (printf " ~a=\"" (car a))
                          (do ([i 0 (fx+ i 1)])
                              ((fx= i n) (write-char #\"))
                            (display
                              (let ([c (string-ref value i)])
                                (if (char=? c #\")
                                    "&quot;"
                                    (html-text-char c))))))))
                  (printf " ~a" a)))
            attributes)
          (printf ">")
          (cond
            [end-tag (let-values ([v* (text)])
                       (printf "</~a>" tag)
                       (apply values v*))]
            [else (text)])))
      (meta define <*>
        (lambda (id)
          (datum->syntax-object id
            (string->symbol
              (string-append "<" (symbol->string (syntax-object->datum id)) ">")))))
      (meta define (attribute x)
        (syntax-case x ()
          [(a v) (identifier? #'a) #'(cons 'a v)]
          [a (identifier? #'a) #''a]
          [else (syntax-error x "improper attribute")]))
      (define-syntax define-tags
        (lambda (x)
          (syntax-case x ()
            [(_ tag ...)
             (with-syntax ([(<tag> ...) (map <*> (syntax->list #'(tag ...)))])
               #'(begin
                   (define-syntax <tag>
                     (lambda (x)
                       (syntax-case x ()
                         [(_ (attr (... ...)) text (... ...))
                          (with-syntax ([(attr (... ...))
                                         (map attribute
                                              (syntax->list #'(attr (... ...))))])
                            #'($tag 'tag (list attr (... ...))
                                (lambda () (void) text (... ...)) #t))])))
                   ...))])))
      (define-syntax define-endless-tags
        (lambda (x)
          (syntax-case x ()
            [(_ tag ...)
             (with-syntax ([(<tag> ...) (map <*> (syntax->list #'(tag ...)))])
               #'(begin
                   (define-syntax <tag>
                     (lambda (x)
                       (syntax-case x ()
                         [(_) #'($tag 'tag '() (lambda () "") #f)]
                         [(_ (attr (... ...)))
                          (with-syntax ([(attr (... ...))
                                         (map attribute
                                           (syntax->list #'(attr (... ...))))])
                            #'($tag 'tag (list attr (... ...))
                                (lambda () "") #f))])))
                   ...))])))

     ; top-level
      (define-tags html head body)

     ; head
      (define-tags script style title) ; script also special inline
      (define-endless-tags base link meta)

     ; block-level generic
     ; del and ins are also phrase
      (define-tags address blockquote del div h1 h2 h3 h4 h5 h6 ins noscript p pre)
      (define-endless-tags hr)

     ; lists
      (define-tags dd dl dt li ol ul)

     ; tables
      (define-tags table caption colgroup thead tfoot tbody tr td th)
      (define-endless-tags col)

     ; forms
      (define-tags form button fieldset legend label select optgroup option textarea)
      (define-endless-tags input)

     ; special inline
      (define-tags a bdo map object q span sub sup)
      (define-endless-tags br img area param)

     ; phrase
      (define-tags abbr acronym cite code dfn em kbd samp strong var)

     ; font-style
      (define-tags b big i small tt)

     ; pseudo tags
      (define (<doctype>)
        (printf "<!DOCTYPE html>\n"))

     ;;; other helpers
      (define (html-text-char c)
        (case c
          [(#\<) "&lt;"]
          [(#\>) "&gt;"]
          [(#\&) "&amp;"]
          [(#\return) ""]
          [else c]))

      (define (html-text fmt . args)
        (let ([s (apply format fmt args)])
          (let ([n (string-length s)])
            (do ([i 0 (fx+ i 1)])
                ((fx= i n))
              (display (html-text-char (string-ref s i)))))))

      (define (nbsp) (display-string "&nbsp;"))

      (define encode-url-parameter
        (let ()
          (define get-encoding
            (let ([encoding (make-vector 256)])
              (do ([i 0 (fx+ i 1)])
                  ((fx= i 256))
                (let ([c (integer->char i)])
                  (cond
                    [(or (char<=? #\a c #\z)
                         (char<=? #\A c #\Z)
                         (char<=? #\0 c #\9)
                         (memv c '(#\$ #\- #\_ #\. #\+ #\! #\* #\' #\( #\) #\,)))
                     (vector-set! encoding i c)]
                    [(char=? c #\space) (vector-set! encoding i #\+)]
                    [else (vector-set! encoding i (format "%~(~2,'0x~)" i))])))
              (lambda (c)
                (let ([n (char->integer c)])
                  (if (fx< n 256)
                      (vector-ref encoding c)
                      ($oops 'encode-url-parameter "cannot encode non-latin-1 character ~s" c))))))
          (lambda (s)
            (define (string-insert! s1 i1 s2 n2)
              (do ([i2 0 (fx+ i2 1)] [i1 i1 (fx+ i1 1)])
                  ((fx= i2 n2))
                (string-set! s1 i1 (string-ref s2 i2))))
            (let ([n (string-length s)])
              (let f ([i 0] [j 0])
                (if (fx= i n)
                    (make-string j)
                    (let ([x (get-encoding (string-ref s i))])
                      (if (char? x)
                          (let ([s (f (fx+ i 1) (fx+ j 1))])
                            (string-set! s j x)
                            s)
                          (let ([xn (string-length x)])
                            (let ([s (f (fx+ i 1) (fx+ j xn))])
                              (string-insert! s j x xn)
                              s))))))))))

      (define (flush-html-output) (flush-output-port))
    )
    (import %html)

    (define (assign-colors ncolors fdata*)
     ; assign highest color to entries whose counts are within X% of maximum
     ; count, where X = 100/ncolors, then recur without assigned color or
     ; entries to which it is assigned
     ; NB: color 0 is for unprofiled code, and color 1 is for unexecuted code
      (let loop ([entry* (sort (lambda (x y)
                                 (> (entrydata-count x) (entrydata-count y)))
                               (apply append (map filedata-entry* fdata*)))]
                 [ci (- ncolors 1)])
        (unless (null? entry*)
          (let ([limit (if (= ci 1)
                           -1
                           (let ([max-count (entrydata-count (car entry*))])
                             (truncate (* max-count (- 1 (/ 1 (- ci 1)))))))])
            (let loop2 ([entry* entry*])
              (unless (null? entry*)
                (let ([entry (car entry*)])
                  (if (<= (entrydata-count entry) limit)
                      (loop entry* (- ci 1))
                      (let ([fdata (entrydata-fdata entry)])
                        (unless (filedata-ci fdata)
                          (filedata-ci-set! fdata ci))
                        (entrydata-ci-set! entry ci)
                        (loop2 (cdr entry*)))))))))))

    (define-syntax with-html-file
      (syntax-rules ()
        [(_ who palette ?path title body1 body2 ...)
         (let ([path ?path])
           (let ([op ($open-file-output-port who path
                       (file-options replace)
                       (buffer-mode block)
                       (current-transcoder))])
             (on-reset (delete-file path #f)
               (on-reset (close-port op)
                 (parameterize ([current-output-port op])
                   (<doctype>)
                   (<html> ()
                     (newline)
                     (<head> ()
                       (newline)
                       (<meta> ([http-equiv "Content-Type"]
                                [content "text/html;charset=utf-8"]))
                       (newline)
                       (<title> () (html-text "~a" title))
                       (newline)
                       (display-style-with-palette palette)
                       (newline))
                     (newline)
                     (let () body1 body2 ...)
                     (newline))))
               (close-port op))))]))

    (define (display-file who palette fdata)
      (let ([ip (filedata-ip fdata)] [line 1] [char 1])
        (define (copy-all)
          (html-text "~a"
            (with-output-to-string
              (rec f
                (lambda ()
                  (let ([c (read-char ip)])
                    (unless (eof-object? c)
                      (write-char c)
                      (f))))))))
        (define (read-space imax)
          (with-output-to-string
            (lambda ()
              (let f ([imax imax])
                (unless (= imax 0)
                  (let ([c (peek-char ip)])
                    (when (memv c '(#\space #\tab))
                      (read-char ip)
                      (set! char (+ char 1))
                      (write-char c)
                      (f (- imax 1)))))))))
        (define (read-to-eol imax)
          (with-output-to-string
            (lambda ()
              (let f ([imax imax])
                (unless (= imax 0)
                  (let ([c (peek-char ip)])
                    (unless (or (eof-object? c) (char=? c #\newline))
                      (read-char ip)
                      (set! char (+ char 1))
                      (write-char c)
                      (f (- imax 1)))))))))
        (define (copy-until bfp next ci title)
          (let loop ([bfp bfp])
            (unless (= bfp next)
              (let ([s (read-to-eol (- next bfp))])
                (let ([n (string-length s)])
                  (when (> n 0)
                    (if ci
                        (<span> ([class (color-class ci)] [title title])
                          (html-text "~a" s))
                        (html-text "~a" s)))
                  (let ([bfp (+ bfp n)])
                    (unless (= bfp next)
                     ; next character must be newline, if not eof
                      (when (eof-object? (read-char ip))
                        ($oops who
                          "unexpected end-of-file on ~s"
                          ip))
                      (let ([bfp (+ bfp 1)])
                        (newline)
                        (set! line (+ line 1))
                        (set! char 1)
                        (let ([s (read-space (- next bfp))])
                          (let ([n (string-length s)])
                            (when (> n 0) (display s))
                            (loop (+ bfp n))))))))))))
        (define-syntax with-line-numbers
          (syntax-rules ()
            [(_ e1 e2 ...)
             (let ([th (lambda () e1 e2 ...)])
               (cond
                 [(profile-line-number-color) =>
                  (lambda (color)
                    (define line-count
                      (let loop ([n 0] [bol? #t])
                        (let ([c (read-char ip)])
                          (if (eof-object? c)
                              (begin (set-port-position! ip 0) n)
                              (loop (if bol? (+ n 1) n) (char=? c #\newline))))))
                    (<table> ()
                      (<tr> ()
                        (<td> ([style (format "color: ~a; font-weight: bold; padding-right: 1rem; text-align: right" color)])
                          (<pre> ()
                            (unless (fx= line-count 0)
                              (newline)
                              (let loop ([i 1])
                                (<span> ([id (format "line~d" i)]) (html-text "~s\n" i))
                                (unless (fx= i line-count) (loop (fx+ i 1)))))))
                        (<td> () (th)))))]
                 [else (th)]))]))
        (with-html-file who palette (filedata-htmlpath fdata) (port-name ip)
          (<body> ([class (color-class 0)])
            (newline)
            (<h1> ([style "margin-bottom: 1rem"])
              (html-text "~a" (port-name ip)) (<span> ([style "opacity: 0.5"]) (html-text " on ~a" (date-and-time))))
            (newline)
            (with-line-numbers
              (<pre> ()
                (newline)
                (let ([entry* (filedata-entry* fdata)]) ; already sorted by gather-filedata
                  (let f ([bfp 0] [entry* entry*] [efp #f] [ci #f] [title ""])
                    (cond
                      [(and (null? entry*) (not efp)) (copy-all)]
                      [(and (not (null? entry*))
                            (or (not efp) (< (entrydata-bfp (car entry*)) efp)))
                       (let ([entry (car entry*)] [entry* (cdr entry*)])
                         (let ([next (entrydata-bfp entry)])
                           (copy-until bfp next ci title)
                           (entrydata-line-set! entry line)
                           (entrydata-char-set! entry char)
                           (let-values ([(bfp entry*)
                                         (f next entry*
                                            (entrydata-efp entry)
                                            (entrydata-ci entry)
                                            (format "line ~d char ~d count ~:d" line char (entrydata-count entry)))])
                             (f bfp entry* efp ci title))))]
                      [else
                       (copy-until bfp efp ci title)
                       (values efp entry*)])))))
            (newline)))))

    (define color-class
      (lambda (ci)
        (format "pc~s" ci)))

    (define (display-style-with-palette palette)
      (<style> ([type "text/css"])
        (newline)

        ;; CSS Reset Styling

        ;; See https://perishablepress.com/a-killer-collection-of-global-css-reset-styles/ for an overview
        ;; of CSS resets.
        ;;
        ;; See http://code.stephenmorley.org/html-and-css/fixing-browsers-broken-monospace-font-handling/
        ;; for an explanation of "font-family: monospace, monospace;" and the following "font-size: 1rem;".
        ;;
        (printf "* {")
        (printf " border: 0;")
        (printf " margin: 0;")
        (printf " outline: 0;")
        (printf " padding: 0;")
        (printf " vertical-align: baseline;")
        (printf " }\n")
        (printf "code, kbd, pre, samp {")
        (printf " font-family: monospace, monospace;")
        (printf " font-size: 1rem;")
        (printf " }\n")
        (printf "html {")
        (printf " -moz-osx-font-smoothing: grayscale;")
        (printf " -webkit-font-smoothing: antialiased;")
        (printf " }\n")
        (printf "table {")
        (printf " border-collapse: collapse;")
        (printf " border-spacing: 0;")
        (printf " }\n")

        ;; CSS Base Styling

        (printf "body {")
        (printf " padding: 1rem;")
        (printf " }\n")
        (printf "h1, h2, h3, h4 {")
        (printf " line-height: 1.25;")
        (printf " margin-bottom: 0.5rem;")
        (printf " }\n")
        (printf "h1 {")
        (printf " font-size: 1.296rem;")
        (printf " }\n")
        (printf "h2 {")
        (printf " font-size: 1.215rem;")
        (printf " }\n")
        (printf "h3 {")
        (printf " font-size: 1.138rem;")
        (printf " }\n")
        (printf "h4 {")
        (printf " font-size: 1.067rem;")
        (printf " }\n")
        (printf "html {")
        (printf " font-family: monospace, monospace;")
        (printf " font-size: 1rem;")
        (printf " }\n")
        (printf "p {")
        (printf " margin-bottom: 1.25rem;")
        (printf " }\n")

        ;; CSS Profile Styling

        (do ([ci 0 (fx+ ci 1)])
            ((fx= ci (vector-length palette)))
          (let ([color (vector-ref palette ci)])
            (printf ".~a { background-color: ~a; color: ~a; white-space: nowrap; }\n"
              (color-class ci) (car color) (cdr color))))))

    (define (safe-prefix name name*)
      (define (prefix? prefix str)
        (let ([n (string-length prefix)])
          (and (fx<= n (string-length str))
               (string=? prefix (substring str 0 n)))))
      (define (digit+? s i n)
        (and (fx< i n)
             (let ([n (fx- n 1)])
               (let loop ([i i])
                 (and (char-numeric? (string-ref s i))
                      (or (fx= i n) (loop (fx+ i 1))))))))
      (define (okay? prefix)
        (let loop ([name* name*])
          (or (null? name*)
              (let ([next-name (car name*)])
                (or (not (prefix? name next-name))
                    (and (or (not (prefix? prefix next-name))
                             (not (digit+? next-name
                                    (string-length prefix)
                                    (string-length next-name))))
                         (loop (cdr name*))))))))
      (let try ([prefix name])
        (let ([prefix (format "~a-" prefix)])
          (if (okay? prefix)
              prefix
              (try prefix)))))

    (define (readable-number n)
      (cond
        [(>= n 1000000000) (format "~~~sB" (quotient n 1000000000))]
        [(>= n 1000000) (format "~~~sM" (quotient n 1000000))]
        [(>= n 1000) (format "~~~sK" (quotient n 1000))]
        [else (format "~a" n)]))

    (set-who! profile-dump-html
      (rec profile-dump-html
        (case-lambda
          [() (profile-dump-html "")]
          [(path-prefix) (profile-dump-html path-prefix (profile-dump))]
          [(path-prefix dumpit*)
           (unless (string? path-prefix)
             ($oops who "~s is not a string" path-prefix))
           (check-dump who dumpit*)
           (let ([palette (profile-palette)])
             (let ([fdata* (gather-filedata who #f dumpit*)])
               (when (null? fdata*)
                 ($oops who "no profiled code found"))
               (for-each
                 (lambda (fdata)
                   (filedata-max-count-set! fdata
                     (apply max
                       (map entrydata-count
                         (filedata-entry* fdata)))))
                 fdata*)
              ; assign unique html pathnames to fdatas with ips
               (let ([fdata*
                      (sort
                        (lambda (x y)
                          (let ([xpath (path-last (port-name (filedata-ip x)))]
                                [ypath (path-last (port-name (filedata-ip y)))])
                            (or (string<? xpath ypath)
                                (and (string=? xpath ypath)
                                     (< (source-file-descriptor-crc (filedata-sfd x))
                                        (source-file-descriptor-crc (filedata-sfd x)))))))
                        (remp (lambda (x) (not (filedata-ip x))) fdata*))])
                 (for-each
                   (lambda (fdata i htmlpath)
                     (filedata-htmlpath-set! fdata htmlpath)
                     (filedata-htmlfn-set! fdata (path-last htmlpath))
                     (filedata-winid-set! fdata (format "win~s" i)))
                   fdata*
                   (enumerate fdata*)
                   (let f ([name* (map (lambda (fdata)
                                         (path-last (port-name (filedata-ip fdata))))
                                       fdata*)]
                           [last-name #f])
                     (if (null? name*)
                         '()
                         (let ([name (car name*)])
                           (if (equal? name last-name)
                               (let ([prefix (safe-prefix name name*)])
                                 (let g ([name* (cdr name*)] [i 0])
                                   (cons (format "~a~a~s.html" path-prefix prefix i)
                                         (if (and (not (null? name*))
                                                  (string=? (car name*) name))
                                             (g (cdr name*) (+ i 1))
                                             (f name* name)))))
                               (cons (format "~a~a.html" path-prefix name)
                                     (f (cdr name*) name))))))))
               (assign-colors (vector-length palette) fdata*)
               (with-html-file who palette (format "~aprofile.html" path-prefix) "Profile Output"
                 (<body> ([class (color-class 0)])
                   (newline)
                   (<h1> ([style "margin-bottom: 1rem"])
                     (html-text "Profile Output") (<span> ([style "opacity: 0.5"]) (html-text " on ~a" (date-and-time))))
                   (newline)
                   (<table> ()
                     (<tr> ()
                       (newline)
                       (<td> ([style "vertical-align: top"])
                         (<h2> ([style "margin-bottom: 0.25rem"])
                           (html-text "Legend"))
                         (newline)
                         (<table> ([style "margin-bottom: 1rem"])
                           (newline)
                           (let* ([n (vector-length palette)] [v (make-vector n #f)])
                             (for-each
                               (lambda (fdata)
                                 (for-each
                                   (lambda (entry)
                                     (let ([ci (entrydata-ci entry)]
                                           [count (entrydata-count entry)])
                                       (vector-set! v ci
                                         (let ([p (vector-ref v ci)])
                                           (if p
                                               (cons (min (car p) count)
                                                     (max (cdr p) count))
                                               (cons count count))))))
                                   (filedata-entry* fdata)))
                               fdata*)
                             (do ([ci (- n 1) (- ci 1)])
                                 ((= ci 0))
                               (let ([p (vector-ref v ci)])
                                 (when p
                                   (<tr> ()
                                     (<td> ([class (color-class ci)]
                                            [style "padding: 0.5rem"])
                                       (let ([smin (readable-number (car p))]
                                             [smax (readable-number (cdr p))])
                                         (if (string=? smin smax)
                                             (html-text "executed ~a time~p"
                                               smin (car p))
                                             (html-text "executed ~a-~a times"
                                               smin smax)))))
                                   (newline))))))
                         (newline)
                         (<h2> ([style "margin-bottom: 0.25rem"])
                           (html-text "Files"))
                         (newline)
                         (<table> ([style "margin-bottom: 1rem"])
                           (newline)
                           (for-each
                             (lambda (fdata)
                               (let ([ip (filedata-ip fdata)])
                                 (<tr> ()
                                   (<td> ([class (color-class (filedata-ci fdata))]
                                          [style "padding: 0.5rem"])
                                     (if ip
                                         (<a> ([href (filedata-htmlfn fdata)]
                                               [target (filedata-winid fdata)]
                                               [class (color-class (filedata-ci fdata))])
                                           (html-text "~a" (port-name (filedata-ip fdata))))
                                         (html-text "~a"
                                           (source-file-descriptor-name (filedata-sfd fdata))))))
                                 (newline)
                                 (when ip (display-file who palette fdata))))
                             (sort
                               (lambda (x y)
                                 (> (filedata-max-count x)
                                    (filedata-max-count y)))
                               fdata*))))
                       (newline)
                       (<td> ([style "width: 10rem"]))
                       (newline)
                       (<td> ([style "vertical-align: top"])
                         (<h2> ([style "margin-bottom: 0.25rem"])
                           (html-text "Hot Spots"))
                         (newline)
                         (<table> ([style "margin-bottom: 1rem"])
                           (newline)
                           (let loop ([entry*
                                       (sort
                                         (lambda (x y)
                                           (or (> (entrydata-count x) (entrydata-count y))
                                               (and (= (entrydata-count x) (entrydata-count y))
                                                    (let ([fn1 (filedata-htmlfn (entrydata-fdata x))]
                                                          [fn2 (filedata-htmlfn (entrydata-fdata y))])
                                                      (and fn1 fn2
                                                           (or (string<? fn1 fn2)
                                                               (and (string=? fn1 fn2)
                                                                    (let ([line1 (entrydata-line x)]
                                                                          [line2 (entrydata-line y)])
                                                                    (and line1 line2 (< line1 line2))))))))))
                                         (apply append (map filedata-entry* fdata*)))]
                                      [last-htmlfn #f]
                                      [last-count #f]
                                      [last-line #f])
                             (unless (or (null? entry*) (= (entrydata-count (car entry*)) 0))
                               (let* ([entry (car entry*)]
                                      [count (entrydata-count entry)]
                                      [line (entrydata-line entry)]
                                      [fdata (entrydata-fdata entry)]
                                      [htmlfn (filedata-htmlfn fdata)])
                                 (unless (and htmlfn last-htmlfn
                                              (string=? htmlfn last-htmlfn)
                                              (= count last-count)
                                              (= line last-line))
                                   (<tr> ()
                                     (<td> ([class (color-class (entrydata-ci entry))]
                                            [style "padding: 0.5rem"])
                                       (cond
                                         [(filedata-ip fdata) =>
                                          (lambda (ip)
                                          (let ([url (format "~a#line~d"
                                                          (filedata-htmlfn fdata)
                                                        line)])
                                              (<a> ([href url]
                                                    [target (filedata-winid fdata)]
                                                    [style "text-decoration: underline"]
                                                    [class (color-class (entrydata-ci entry))])
                                              (html-text "~a line ~s (~:d)"
                                                  (port-name ip) (entrydata-line entry)
                                                (entrydata-count entry)))))]
                                         [else
                                          (html-text "~a char ~s (~:d)"
                                            (source-file-descriptor-name (filedata-sfd fdata))
                                            (entrydata-bfp entry)
                                            (entrydata-count entry))])))
                                   (newline))
                                 (loop (cdr entry*) htmlfn count line)))))
                         (newline))
                       (newline)))
                   (newline)))
               (for-each
                 (lambda (fdata) (cond [(filedata-ip fdata) => close-input-port]))
                 fdata*)))]))))

  (set-who! profile-palette
    (make-parameter
     ; color background with appropriate white or black foreground
      '#(("#111111" . "white")  ; black (for unprofiled code)
         ("#607D8B" . "white")  ; gray  (for unexecuted code)
         ("#9C27B0" . "black")  ; purple
         ("#673AB7" . "white")  ; dark purple
         ("#3F51B5" . "white")  ; dark blue
         ("#2196F3" . "black")  ; medium blue
         ("#00BCD4" . "black")  ; aqua
         ("#4CAF50" . "black")  ; green
         ("#CDDC39" . "black")  ; yellow green
         ("#FFEB3B" . "black")  ; yellow
         ("#FFC107" . "black")  ; dark yellow
         ("#FF9800" . "black")  ; orange
         ("#F44336" . "white")) ; red
      (lambda (palette)
        (unless (and (vector? palette)
                     (andmap
                       (lambda (x)
                         (and (pair? x) (string? (car x)) (string? (cdr x))))
                       (vector->list palette)))
          ($oops who "invalid palette ~s" palette))
        (unless (fx> (vector-length palette) 2)
          ($oops who "palette ~s has too few entries" palette))
        palette)))

  (set-who! profile-line-number-color
    (make-parameter "#666666"
      (lambda (color)
        (unless (or (eq? color #f) (string? color)) ($oops who "~s is not a string or #f" color))
        color)))
)
)
