;;; rabbit

;;; The rabbit program highlights the use of continuations and
;;; timer interrupts to perform thread scheduling.  The scheduler
;;; maintains a thread queue and operating system primitives for
;;; dispatching and thread creation.  The queue is only visible
;;; to the operating system kernel and all accesses are performed
;;; with the timer off to prevent corruption.

;;; (thread exp) will create a thread out of exp and place it in
;;; the thread queue.  you may do this for as many threads as
;;; you like.  (dispatch) starts the threads going.  If the
;;; thread queue ever becomes empty, dispatch exits.  Threads
;;; may create other threads.

;;; The rabbit function creates a thread that spawns two offspring
;;; and dies.  Each thread has a generation number associated with
;;; it.  The generation number of each rabbit is one lower than that
;;; of it's parent; rabbits in generation 0 are sterile.

;;; load the queue datatype -- might need a fuller pathname
(load "queue.ss")

;;; swap-time determines the number of timer ticks in a time slice
(define swap-time
   (make-parameter
      100
      (lambda (x)
         (unless (and (integer? x) (positive? x))
            (error 'swap-time "~s is not a positive integer" x))
         x)))

(define dispatch #f)
(define thread #f)

(let ([pq (queue)])
   (set! dispatch
      (lambda ()
         (unless (pq 'empty?)
             ; the thread queue holds continuations---grab one and invoke it
             (let ([next (pq 'get)])
                (set-timer (swap-time))
                (next #f)))))
    (set! thread
       (lambda (thunk)
          (call/cc
             (lambda (return)
                (call/cc
                   (lambda (k)
                      ; turn off the timer while accessing the queue
                      (let ([time-left (set-timer 0)])
                         ; put the thread on the queue
                         (pq 'put k)
                         (set-timer time-left)
                         ; get out of here
                         (return #f))))
                ; the first time through we will return before getting
                ; here.  the second time is when a thread is first
                ; dispatched from the thread queue.
                (thunk)
                (set-timer 0)
                (dispatch)))))
    (timer-interrupt-handler
       (lambda ()
          (printf "swapping~%")
          (call/cc
             (lambda (l)
                ; place the continuation of the interrupt on the queue
                (pq 'put l)
                (dispatch))))))


;;; *delay-max* gives the maximum random delay before a rabbit
;;; reaches child-bearing age.
(define *delay-max* 10000)

(define rabbit
   (lambda (n)
      (thread
         (lambda ()
            (printf "~s~%" n)
            (unless (zero? n)
               (do ([i (random *delay-max*) (1- i)]) ((zero? i)))
               (rabbit (1- n))
               (rabbit (1- n)))))))

;;; try:
;;;        (rabbit 3)
;;;        (rabbit 5)
;;;        (dispatch)
