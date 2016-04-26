;;; rsa.ss
;;; Bruce T. Smith, University of North Carolina at Chapel Hill
;;; (circa 1984)

;;; Updated for Chez Scheme Version 7, May 2005

;;; This is  a toy example of an RSA public-key encryption system.   It
;;; is possible  to create users who register their public  keys with a
;;; center and hide  their private keys.   Then, it is possible to have
;;; the  users exchange messages.   To a limited extent one can look at
;;; the intermediate steps of the process by using encrypt and decrypt.
;;; The encrypted messages are represented by lists of numbers.

;;; Example session:

#|
> (make-user bonzo)
Registered with Center
User: bonzo
Base: 152024296883113044375867034718782727467
Encryption exponent: 7
> (make-user bobo)
Registered with Center
User: bobo
Base: 244692569127295893294157219042233636899
Encryption exponent: 5
> (make-user tiger)
Registered with Center
User: tiger
Base: 138555414233087084786368622588289286073
Encryption exponent: 7
> (show-center)

User: tiger
Base: 138555414233087084786368622588289286073
Encryption exponent: 7

User: bobo
Base: 244692569127295893294157219042233636899
Encryption exponent: 5

User: bonzo
Base: 152024296883113044375867034718782727467
Encryption exponent: 7
> (send "hi there" bonzo bobo)
"hi there"
> (send "hi there to you" bobo bonzo)
"hi there to you"
> (decrypt (encrypt "hi there" bonzo bobo) tiger)
" #z  R4WN Zbb   E8J"
|#

;;; Implementation:

(module ((make-user user) show-center encrypt decrypt send)

;;; (make-user name) creates a user with the chosen name.  When it
;;; creates the user, it tells him what his name is.  He will use
;;; this when registering with the center.

(define-syntax make-user
  (syntax-rules ()
    [(_ uid)
     (begin (define uid (user 'uid)) (uid 'register))]))

;;; (encrypt mesg u1 u2) causes user 1 to encrypt mesg using the public
;;; keys for user 2.

(define-syntax encrypt
  (syntax-rules ()
    [(_ mesg u1 u2) ((u1 'send) mesg 'u2)]))

;;; (decrypt number-list u) causes the user to decrypt the list of
;;; numbers using his private key.

(define-syntax decrypt
  (syntax-rules ()
    [(_ numbers u) ((u 'receive) numbers)]))

;;; (send mesg u1 u2) this combines the functions 'encrypt' and 'decrypt',
;;; calling on user 1 to encrypt the message for user 2 and calling on
;;; user 2 to decrypt the message.

(define-syntax send
  (syntax-rules ()
    [(_ mesg u1 u2) (decrypt (encrypt mesg u1 u2) u2)]))

;;; A user is capable of the following:
;;;   -        choosing public and private keys and registering with the center
;;;   - revealing his public and private keys
;;;   -        retrieving user's private keys from the center and encrypting a
;;;        message for that user
;;;   -        decrypting a message with his private key

(define user
  (lambda (name)
    (let* ([low (expt 2 63)]     ; low, high = bounds on p and q
           [high (* 2 low)]
           [p 0]                 ; p,q = two large, probable primes
           [q 0]
           [n 0]                 ; n = p * q, base for modulo arithmetic
           [phi 0]               ; phi = lcm(p-1,q-1), not quite the Euler phi function,
                                 ;        but it will serve for our purposes
           [e 0]                 ; e = exponent for encryption
           [d 0])                ; d = exponent for decryption
      (lambda (request)
        (case request
          ;; choose keys and register with the center
          [register
           (set! p (find-prime low high))
           (set! q
             (let loop ([q1 (find-prime low high)])
               (if (= 1 (gcd p q1))
                   q1
                   (loop (find-prime low high)))))
           (set! n (* p q))
           (set! phi
              (/ (* (1- p) (1- q))
                 (gcd (1- p) (1- q))))
           (set! e
             (do ([i 3 (+ 2 i)])
                 ((= 1 (gcd i phi)) i)))
           (set! d (mod-inverse e phi))
           (register-center (cons name (list n e)))
           (printf "Registered with Center~%")
           (printf "User: ~s~%" name)
           (printf "Base: ~d~%" n)
           (printf "Encryption exponent: ~d~%" e)]

          ;; divulge your keys-- you should resist doing this...
          [show-all
           (printf "p = ~d ; q = ~d~%" p q)
           (printf "n = ~d~%" n)
           (printf "phi = ~d~%" (* (1- p) (1- q)))
           (printf "e = ~d ; d = ~d~%" e d)]

          ;; get u's public key from the center and encode
          ;; a message for him
          [send
           (lambda (mesg u)
             (let* ([public (request-center u)]
                    [base (car public)]
                    [exponent (cadr public)]
                    [mesg-list (string->numbers mesg base)])
               (map (lambda (x) (expt-mod x exponent base))
                    mesg-list)))]

          ;; decrypt a message with your private key
          [receive
           (lambda (crypt-mesg)
             (let ([mesg-list (map (lambda (x) (expt-mod x d n)) crypt-mesg)])
               (numbers->string mesg-list)))])))))

;;; The center maintains the list of public keys.  It can register
;;; new users, provide the public keys for any particular user, or
;;; display the whole public file.

(module (register-center request-center show-center)
  (define public-keys '())
  (define register-center
    (lambda (entry)
      (set! public-keys
        (cons entry
              (remq (assq (car entry) public-keys) public-keys)))))
  (define request-center
    (lambda (u)
      (let ([a (assoc u public-keys)])
        (when (null? a)
          (error 'request-center
            "User ~s not registered in center"
            u))
        (cdr a))))
  (define show-center
    (lambda ()
      (for-each
        (lambda (entry)
          (printf "~%User: ~s~%" (car entry))
          (printf "Base: ~s~%" (cadr entry))
          (printf "Encryption exponent: ~s~%" (caddr entry)))
        public-keys)))
)

;;; string->numbers encodes a string as a list of numbers
;;; numbers->string decodes a string from a list of numbers

;;; string->numbers and numbers->string are defined with respect to
;;; an alphabet.  Any characters in the alphabet are translated into
;;; integers---their regular ascii codes.  Any characters outside
;;; the alphabet cause an error during encoding.  An invalid code
;;; during decoding is translated to a space.

(module (string->numbers numbers->string)
  (define first-code 32)
  (define last-code 126)
  (define alphabet
    ; printed form of the characters, indexed by their ascii codes
    (let ([alpha (make-string 128 #\space)])
      (do ([i first-code (1+ i)])
          ((= i last-code) alpha)
        (string-set! alpha i (integer->char i)))))

  (define string->integer
    (lambda (str)
      (let ([ln (string-length str)])
        (let loop ([i 0] [m 0])
          (if (= i ln)
              m
              (let* ([c (string-ref str i)] [code (char->integer c)])
                (when (or (< code first-code) (>= code last-code))
                  (error 'rsa "Illegal character ~s" c))
                (loop (1+ i) (+ code (* m 128)))))))))

  (define integer->string
    (lambda (n)
      (list->string
        (map (lambda (n) (string-ref alphabet n))
             (let loop ([m n] [lst '()])
               (if (zero? m)
                   lst
                   (loop (quotient m 128)
                         (cons (remainder m 128) lst))))))))

  ; turn a string into a list of numbers, each no larger than base
  (define string->numbers
    (lambda (str base)
      (letrec ([block-size
                (do ([i -1 (1+ i)] [m 1 (* m 128)]) ((>= m base) i))]
               [substring-list
                (lambda (str)
                  (let ([ln (string-length str)])
                    (if (>= block-size ln)
                        (list str)
                        (cons (substring str 0 block-size)
                              (substring-list
                                (substring str block-size ln))))))])
        (map string->integer (substring-list str)))))

  ; turn a list of numbers into a string
  (define numbers->string
    (lambda (lst)
      (letrec ([reduce
                (lambda (f l)
                  (if (null? (cdr l))
                      (car l)
                      (f (car l) (reduce f (cdr l)))))])
        (reduce
          string-append
          (map (lambda (x) (integer->string x)) lst)))))
)

;;; find-prime finds a probable prime between two given arguments.
;;; find-prime uses a cheap but fairly dependable test for primality
;;; for large numbers, by first weeding out multiples of first 200
;;; primes, then applies Fermat's theorem with base 2.

(module (find-prime)
  (define product-of-primes
    ; compute product of first n primes, n > 0
    (lambda (n)
      (let loop ([n (1- n)] [p 2] [i 3])
        (cond
          [(zero? n) p]
          [(= 1 (gcd i p)) (loop (1- n) (* p i) (+ i 2))]
          [else (loop n p (+ i 2))]))))
  (define prod-first-200-primes (product-of-primes 200))
  (define probable-prime
    ; first check is quick, and weeds out most non-primes
    ; second check is slower, but weeds out almost all non-primes
    (lambda (p)
      (and (= 1 (gcd p prod-first-200-primes))
           (= 1 (expt-mod 2 (1- p) p)))))
  (define find-prime
    ; find probable prime in range low to high (inclusive)
    (lambda (low high)
      (let ([guess
             (lambda (low high)
               (let ([g (+ low (random (1+ (- high low))))])
                 (if (odd? g) g (1+ g))))])
        (let loop ([g (guess low high)])
          (cond
            ; start over if already too high
            [(> g high) (loop (guess low high))]
            ; if guess is probably prime, return
            [(probable-prime g) g]
            ; don't bother with even guesses
            [else (loop (+ 2 g))])))))
)

;;; mod-inverse finds the multiplicative inverse of x mod b, if it exists

(module (mod-inverse)
  (define gcdx
    ; extended Euclid's gcd algorithm, x <= y
    (lambda (x y)
      (let loop ([x x] [y y] [u1 1] [u2 0] [v1 0] [v2 1])
        (if (zero? y)
            (list x u1 v1)
            (let ([q (quotient x y)] [r (remainder x y)])
              (loop y r u2 (- u1 (* q u2)) v2 (- v1 (* q v2))))))))

  (define mod-inverse
    (lambda (x b)
      (let* ([x1 (modulo x b)] [g (gcdx x1 b)])
        (unless (= (car g) 1)
          (error 'mod-inverse "~d and ~d not relatively prime" x b))
        (modulo (cadr g) b))))
)
)
