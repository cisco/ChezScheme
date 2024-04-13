/* schsig.c
 * Copyright 1984-2017 Cisco Systems, Inc.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#include "system.h"
#include <setjmp.h>

/* locally defined functions */
static void split(ptr k, ptr *s);
static void reset_scheme(void);
static NORETURN void do_error(iptr type, const char *who, const char *s, ptr args);
static void handle_call_error(ptr tc, iptr type, ptr x);
static void init_signal_handlers(void);
static void keyboard_interrupt(ptr tc);

static void (*register_modified_signal)(int);

ptr S_get_scheme_arg(ptr tc, iptr n) {

    if (n <= asm_arg_reg_cnt) return REGARG(tc, n);
    else return FRAME(tc, n - asm_arg_reg_cnt);
}

void S_put_scheme_arg(ptr tc, iptr n, ptr x) {

    if (n <= asm_arg_reg_cnt) REGARG(tc, n) = x;
    else FRAME(tc, n - asm_arg_reg_cnt) = x;
}

void S_promote_to_multishot(ptr k) {
    while (CONTLENGTH(k) != CONTCLENGTH(k)) {
        CONTLENGTH(k) = CONTCLENGTH(k);
        k = CONTLINK(k);
    }
}

/* k must be is a multi-shot continuation, and s (the split point)
 * must be strictly between the base and end of k's stack segment. */
static void split(ptr k, ptr *s) {
    iptr m, n;
    seginfo *si;
    ISPC spc;

  /* set m to size of lower piece, n to size of upper piece */
    m = (uptr)TO_PTR(s) - (uptr)CONTSTACK(k);
    n = CONTCLENGTH(k) - m;

    si = SegInfo(ptr_get_segment(k));
    spc = si->space;
    if (spc != space_new) spc = space_continuation; /* to avoid space_count_pure */

  /* insert a new continuation between k and link(k) */
    CONTLINK(k) = S_mkcontinuation(spc,
                                 si->generation,
                                 CLOSENTRY(k),
                                 CONTSTACK(k),
                                 m, m,
                                 CONTLINK(k),
                                 *s,
                                 Snil,
                                 Sfalse);
    CONTLENGTH(k) = CONTCLENGTH(k) = n;
    CONTSTACK(k) = TO_PTR(s);
    *s = TO_PTR(DOUNDERFLOW);
}

/* We may come in to S_split_and_resize with a multi-shot continuation whose
 * stack segment exceeds the copy bound or is too large to fit along
 * with the return values in the current stack.  We may also come in to
 * S_split_and_resize with a one-shot continuation for which all of the
 * above is true and for which there is insufficient space between the
 * top frame and the end of the stack.  If we have to split a 1-shot, we
 * promote it to multi-shot; doing otherwise is too much trouble.  */
void S_split_and_resize(void) {
    ptr tc = get_thread_context();
    ptr k; iptr value_count; iptr n;

  /* cp = continuation, ac0 = return value count */
    k = CP(tc);
    value_count = (iptr)AC0(tc);

    if (CONTCLENGTH(k) > underflow_limit) {
        iptr frame_size;
        ptr *front_stack_ptr, *end_stack_ptr, *split_point, *guard;

        front_stack_ptr = TO_VOIDP(CONTSTACK(k));
        end_stack_ptr = TO_VOIDP((uptr)TO_PTR(front_stack_ptr) + CONTCLENGTH(k));

        guard = TO_VOIDP((uptr)TO_PTR(end_stack_ptr) - underflow_limit);

      /* set split point to base of top frame */
        frame_size = ENTRYFRAMESIZE(CONTRET(k));
        split_point = TO_VOIDP((uptr)TO_PTR(end_stack_ptr) - frame_size);

      /* split only if we have more than one frame */
        if (split_point != front_stack_ptr) {
          /* walk the stack to set split_point at first frame above guard */
          /* note that first frame may have put us below the guard already */
            for (;;) {
                ptr *p;
                frame_size = ENTRYFRAMESIZE(*split_point);
                p = TO_VOIDP((uptr)TO_PTR(split_point) - frame_size);
                if (p < guard) break;
                split_point = p;
            }

          /* promote to multi-shot if necessary */
            S_promote_to_multishot(k);

          /* split */
            split(k, split_point);
        }
    }

  /* make sure the stack is big enough to hold continuation
   * this is conservative: really need stack-base + clength <= esp
   * and clength + size(values) < stack-size; also, size may include
   * argument register values */
    n = CONTCLENGTH(k) + (value_count * sizeof(ptr)) + stack_slop;
    if (n >= SCHEMESTACKSIZE(tc))
       S_reset_scheme_stack(tc, n);
}

iptr S_continuation_depth(ptr k) {
    iptr n, frame_size; ptr *stack_base, *stack_ptr;

    n = 0;
  /* terminate on shot 1-shot, which could be null_continuation */
    while (CONTLENGTH(k) != scaled_shot_1_shot_flag) {
        stack_base = TO_VOIDP(CONTSTACK(k));
        frame_size = ENTRYFRAMESIZE(CONTRET(k));
        stack_ptr = TO_VOIDP((uptr)TO_PTR(stack_base) + CONTCLENGTH(k));
        for (;;) {
            stack_ptr = TO_VOIDP((uptr)TO_PTR(stack_ptr) - frame_size);
            n += 1;
            if (stack_ptr == stack_base) break;
            frame_size = ENTRYFRAMESIZE(*stack_ptr);
        }
        k = CONTLINK(k);
    }
    return n;
}

ptr S_single_continuation(ptr k, iptr n) {
    iptr frame_size; ptr *stack_base, *stack_top, *stack_ptr;

  /* bug out on shot 1-shots, which could be null_continuation */
    while (CONTLENGTH(k) != scaled_shot_1_shot_flag) {
        stack_base = TO_VOIDP(CONTSTACK(k));
        stack_top = TO_VOIDP((uptr)TO_PTR(stack_base) + CONTCLENGTH(k));
        stack_ptr = stack_top;
        frame_size = ENTRYFRAMESIZE(CONTRET(k));
        for (;;) {
            if (n == 0) {
              /* promote to multi-shot if necessary, even if we don't end
               * up in split, since inspector assumes multi-shot */
                S_promote_to_multishot(k);

                if (stack_ptr != stack_top) {
                    split(k, stack_ptr);
                    k = CONTLINK(k);
                }

                stack_ptr = TO_VOIDP((uptr)TO_PTR(stack_ptr) - frame_size);
                if (stack_ptr != stack_base)
                    split(k, stack_ptr);

                return k;
            } else {
                n -= 1;
                stack_ptr = TO_VOIDP((uptr)TO_PTR(stack_ptr) - frame_size);
                if (stack_ptr == stack_base) break;
                frame_size = ENTRYFRAMESIZE(*stack_ptr);
            }
        }
        k = CONTLINK(k);
    }

    return Sfalse;
}

void S_handle_overflow() {
    ptr tc = get_thread_context();

 /* default frame size is enough */
    S_overflow(tc, 0);
}

void S_handle_overflood() {
    ptr tc = get_thread_context();

 /* xp points to where esp needs to be */
    S_overflow(tc, ((ptr *)TO_VOIDP(XP(tc)) - (ptr *)TO_VOIDP(SFP(tc)))*sizeof(ptr));
}

void S_handle_apply_overflood() {
    ptr tc = get_thread_context();

 /* ac0 contains the argument count for the called procedure */
 /* could reduce request by default frame size and number of arg registers */
 /* the "+ 1" is for the return address slot */
    S_overflow(tc, ((iptr)AC0(tc) + 1) * sizeof(ptr));
}

/* allocates a new stack
 * --the old stack below the sfp is turned into a continuation
 * --the old stack above the sfp is copied to the new stack
 * --return address must be in first frame location
 * --scheme registers are preserved or reset
 * frame_request is how much (in bytes) to increase the default frame size
 */
void S_overflow(ptr tc, iptr frame_request) {
    ptr *sfp;
    iptr above_split_size, sfp_offset;
    ptr *split_point, *guard, *other_guard;
    iptr split_stack_length, split_stack_clength;
    ptr nuate;

    sfp = TO_VOIDP(SFP(tc));
    nuate = SYMVAL(S_G.nuate_id);
    if (!Scodep(nuate)) {
        S_error_abort("overflow: nuate not yet defined");
    }

    guard = TO_VOIDP((uptr)TO_PTR(sfp) - underflow_limit);
  /* leave at least stack_slop headroom in the old stack to reduce the need for return-point overflow checks */
    other_guard = TO_VOIDP((uptr)SCHEMESTACK(tc) + (uptr)SCHEMESTACKSIZE(tc) - (uptr)TO_PTR(stack_slop));
    if ((uptr)TO_PTR(other_guard) < (uptr)TO_PTR(guard)) guard = other_guard;

  /* split only if old stack contains more than underflow_limit bytes */
    if (guard > (ptr *)TO_VOIDP(SCHEMESTACK(tc))) {
        iptr frame_size;

      /* set split point to base of the frame below the current one */
        frame_size = ENTRYFRAMESIZE(*sfp);
        split_point = TO_VOIDP((uptr)TO_PTR(sfp) - frame_size);

      /* split only if we have more than one frame */
        if (split_point != TO_VOIDP(SCHEMESTACK(tc))) {
          /* walk the stack to set split_point at first frame above guard */
          /* note that first frame may have put us below the guard already */
            for (;;) {
                ptr *p;

                frame_size = ENTRYFRAMESIZE(*split_point);
                p = TO_VOIDP((uptr)TO_PTR(split_point) - frame_size);
                if (p < guard) break;
                split_point = p;
            }

            split_stack_clength = (uptr)TO_PTR(split_point) - (uptr)SCHEMESTACK(tc);

          /* promote to multi-shot if current stack is shrimpy */
            if (SCHEMESTACKSIZE(tc) < default_stack_size / 4) {
                split_stack_length = split_stack_clength;
                S_promote_to_multishot(STACKLINK(tc));
            } else {
                split_stack_length = SCHEMESTACKSIZE(tc);
            }

          /* create a continuation */
            STACKLINK(tc) = S_mkcontinuation(space_new,
                                        0,
                                        CODEENTRYPOINT(nuate),
                                        SCHEMESTACK(tc),
                                        split_stack_length,
                                        split_stack_clength,
                                        STACKLINK(tc),
                                        *split_point,
                                        Snil,
                                        Sfalse);

          /* overwrite old return address with dounderflow */
              *split_point = TO_PTR(DOUNDERFLOW);
        }
    } else {
        split_point = TO_VOIDP(SCHEMESTACK(tc));
    }

    above_split_size = SCHEMESTACKSIZE(tc) - ((uptr)TO_PTR(split_point) - (uptr)SCHEMESTACK(tc));

  /* allocate a new stack, retaining same relative sfp */
    sfp_offset = (uptr)TO_PTR(sfp) - (uptr)TO_PTR(split_point);
    S_reset_scheme_stack(tc, above_split_size + frame_request);
    SFP(tc) = (ptr)((uptr)SCHEMESTACK(tc) + sfp_offset);

  /* copy up everything above the split point.  we don't know where the
     current frame ends, so we copy through the end of the old stack */
    {ptr *p, *q; iptr n;
     p = TO_VOIDP(SCHEMESTACK(tc));
     q = split_point;
     for (n = above_split_size; n != 0; n -= sizeof(ptr)) *p++ = *q++;
    }
}

void S_error_abort(const char *s) {
    fprintf(stderr, "%s\n", s);
    S_abnormal_exit();
}

void S_abnormal_exit() {
  S_abnormal_exit_proc();
  fprintf(stderr, "abnormal_exit procedure did not exit\n");
  abort();
}

static void reset_scheme() {
    ptr tc = get_thread_context();

    alloc_mutex_acquire();
   /* eap should always be up-to-date now that we write-through to the tc
      when making any changes to eap when eap is a real register */
    S_scan_dirty(TO_VOIDP(EAP(tc)), TO_VOIDP(REAL_EAP(tc)));
    S_reset_allocation_pointer(tc);
    S_reset_scheme_stack(tc, stack_slop);
    alloc_mutex_release();
    FRAME(tc,0) = TO_PTR(DOUNDERFLOW);
    S_maybe_fire_collector(THREAD_GC(tc));
}

/* error_resets occur with the system in an unknown state,
 * thus we must reset with no opportunity for debugging
 */

void S_error_reset(const char *s) {

    if (!S_errors_to_console) reset_scheme();
    do_error(ERROR_RESET, "", s, Snil);
}

void S_error(const char *who, const char *s) {
    do_error(ERROR_OTHER, who, s, Snil);
}

void S_error1(const char *who, const char *s, ptr x) {
    do_error(ERROR_OTHER, who, s, LIST1(x));
}

void S_error2(const char *who, const char *s, ptr x, ptr y) {
    do_error(ERROR_OTHER, who, s, LIST2(x,y));
}

void S_error3(const char *who, const char *s, ptr x, ptr y, ptr z) {
    do_error(ERROR_OTHER, who, s, LIST3(x,y,z));
}

void S_boot_error(ptr who, ptr msg, ptr args) {
  printf("error caught before error-handing subsystem initialized\n"); 
  printf("who: ");
  S_prin1(who);
  printf("\nmsg: ");
  S_prin1(msg);
  printf("\nargs: ");
  S_prin1(args);
  printf("\n");
  fflush(stdout);
  S_abnormal_exit();
}

static void do_error(iptr type, const char *who, const char *s, ptr args) {
    ptr tc = get_thread_context();

    if (S_errors_to_console || tc == (ptr)0 || CCHAIN(tc) == Snil) {
        if (strlen(who) == 0)
          printf("Error: %s\n", s);
        else
          printf("Error in %s: %s\n", who, s);
        S_prin1(args); putchar('\n');
        fflush(stdout);
        S_abnormal_exit();
    }

    args = Scons(FIX(type),
                 Scons((strlen(who) == 0 ? Sfalse : Sstring_utf8(who,-1)),
                       Scons(Sstring_utf8(s, -1), args)));

#ifdef PTHREADS
    while (S_mutex_is_owner(&S_alloc_mutex) && (S_alloc_mutex_depth > 0)) {
      S_alloc_mutex_depth -= 1;
      S_mutex_release(&S_alloc_mutex);
    }
    while (S_mutex_is_owner(&S_tc_mutex) && (S_tc_mutex_depth > 0)) {
      S_tc_mutex_depth -= 1;
      S_mutex_release(&S_tc_mutex);
    }
#endif /* PTHREADS */

    /* in case error is during fasl read: */
    S_thread_end_code_write(tc, static_generation, 0, NULL, 0);

    TRAP(tc) = (ptr)1;
    AC0(tc) = (ptr)1;
    CP(tc) = S_symbol_value(S_G.error_id);
    S_put_scheme_arg(tc, 1, args);
    LONGJMP(TO_VOIDP(CAAR(CCHAIN(tc))), -1);
}

static void handle_call_error(ptr tc, iptr type, ptr x) {
    ptr p, arg1;
    iptr argcnt;

    argcnt = (iptr)AC0(tc);
    arg1 = argcnt == 0 ? Snil : S_get_scheme_arg(tc, 1);
    p = Scons(FIX(type), Scons(FIX(argcnt), Scons(x, Scons(arg1, Snil))));

    if (S_errors_to_console) {
        printf("Call error: ");
        S_prin1(p); putchar('\n'); fflush(stdout);
        S_abnormal_exit();
    }

    CP(tc) = S_symbol_value(S_G.error_id);
    S_put_scheme_arg(tc, 1, p);
    AC0(tc) = (ptr)(argcnt==0 ? 1 : argcnt);
    TRAP(tc) = (ptr)1;         /* Why is this here? */
}

void S_handle_docall_error() {
    ptr tc = get_thread_context();

    AC0(tc) = (ptr)0;
    handle_call_error(tc, ERROR_CALL_NONPROCEDURE, CP(tc));
}

void S_handle_arg_error() {
    ptr tc = get_thread_context();

    handle_call_error(tc, ERROR_CALL_ARGUMENT_COUNT, CP(tc));
}

void S_handle_nonprocedure_symbol() {
    ptr tc = get_thread_context();
    ptr s;

    s = XP(tc);
    handle_call_error(tc,
                      (SYMVAL(s) == sunbound ?
                                ERROR_CALL_UNBOUND :
                                ERROR_CALL_NONPROCEDURE_SYMBOL),
                      s);
}

void S_handle_values_error(void) {
    ptr tc = get_thread_context();

    handle_call_error(tc, ERROR_VALUES, Sfalse);
}

void S_handle_mvlet_error(void) {
    ptr tc = get_thread_context();

    handle_call_error(tc, ERROR_MVLET, Sfalse);
}

void S_handle_event_detour() {
    ptr tc = get_thread_context();
    ptr resume_proc = CP(tc);
    ptr resume_args = Snil;
    iptr argcnt, stack_avail, i;

    argcnt = (iptr)AC0(tc);
    stack_avail = (((uptr)ESP(tc) - (uptr)SFP(tc)) >> log2_ptr_bytes) - 1;

    if (argcnt < (stack_avail + asm_arg_reg_cnt)) {
      /* Avoid allocation by passing arguments directly. The compiler
         will only use `detour-event` when the expected number is
         small enough to avoid allocation (unless the function expected
         to allocate a list of arguments, anyway). */
      for (i = argcnt; i > 0; i--)
        S_put_scheme_arg(tc, i+1, S_get_scheme_arg(tc, i));
      S_put_scheme_arg(tc, 1, resume_proc);
      CP(tc) = S_symbol_value(S_G.event_and_resume_id);
      AC0(tc) = (ptr)(argcnt+1);
    } else {
      /* We're assuming that either at least one argument can go in a
         register or stack slop will save us. */
      for (i = argcnt; i > 0; i--)
        resume_args = Scons(S_get_scheme_arg(tc, i), resume_args);
      resume_args = Scons(resume_proc, resume_args);
 
      CP(tc) = S_symbol_value(S_G.event_and_resume_star_id);
      S_put_scheme_arg(tc, 1, resume_args);
      AC0(tc) = (ptr)1;
    }
}

static void keyboard_interrupt(ptr tc) {
  KEYBOARDINTERRUPTPENDING(tc) = Strue;
  SOMETHINGPENDING(tc) = Strue;
}

/* used in printf below
static uptr list_length(ptr ls) {
  uptr i = 0;
  while (ls != Snil) { ls = Scdr(ls); i += 1; }
  return i;
}
*/

void S_fire_collector(void) {
  ptr crp_id = S_G.collect_request_pending_id;

/*  printf("firing collector!\n"); fflush(stdout); */

  if (!Sboolean_value(S_symbol_racy_value(crp_id))) {
    ptr ls;

/*    printf("really firing collector!\n"); fflush(stdout); */

    tc_mutex_acquire();
   /* check again in case some other thread beat us to the punch */
    if (!Sboolean_value(S_symbol_value(crp_id))) {
/* printf("firing collector nthreads = %d\n", list_length(S_threads)); fflush(stdout); */
      S_set_symbol_value(crp_id, Strue);
      for (ls = S_threads; ls != Snil; ls = Scdr(ls))
        SOMETHINGPENDING(THREADTC(Scar(ls))) = Strue;
    }
    tc_mutex_release();
  }
}

void S_noncontinuable_interrupt(void) {
  ptr tc = get_thread_context();

  reset_scheme();
  KEYBOARDINTERRUPTPENDING(tc) = Sfalse;
  do_error(ERROR_NONCONTINUABLE_INTERRUPT,"","",Snil);
}

void Sscheme_register_signal_registerer(void (*registerer)(int)) {
  register_modified_signal = registerer;
}

#ifdef WIN32
ptr S_dequeue_scheme_signals(UNUSED ptr tc) {
  return Snil;
}

ptr S_allocate_scheme_signal_queue() {
  return (ptr)0;
}

void S_register_scheme_signal(UNUSED iptr sig) {
  S_error("register_scheme_signal", "unsupported in this version");
}

/* code courtesy Bob Burger, burgerrg@sagian.com
   We cannot call noncontinuable_interrupt, because we are not allowed
   to perform a longjmp inside a signal handler; instead, we don't
   handle the signal, which will cause the process to terminate.
*/

static BOOL WINAPI handle_signal(DWORD dwCtrlType) {
  switch (dwCtrlType) {
    case CTRL_C_EVENT:
    case CTRL_BREAK_EVENT: {
#ifdef PTHREADS
     /* get_thread_context() always returns 0, so assume main thread */
      ptr tc = TO_PTR(S_G.thread_context);
#else
      ptr tc = get_thread_context();
#endif
      if (!THREAD_GC(tc)->during_alloc && Sboolean_value(KEYBOARDINTERRUPTPENDING(tc)))
        return(FALSE);
      keyboard_interrupt(tc);
      return(TRUE);
    }
  }
  return(FALSE);
}

#if defined(_M_ARM64) && !defined(PORTABLE_BYTECODE)
static LONG WINAPI fault_handler(LPEXCEPTION_POINTERS e) {
  if (e->ExceptionRecord->ExceptionCode == EXCEPTION_ACCESS_VIOLATION) {
    ptr tc = get_thread_context();
    if (THREAD_GC(tc)->during_alloc)
      S_error_abort("nonrecoverable invalid memory reference");
    else
      S_error_reset("invalid memory reference");
  }
  return EXCEPTION_CONTINUE_SEARCH;
}
#endif

static void init_signal_handlers(void) {
  SetConsoleCtrlHandler(handle_signal, TRUE);
#if defined(_M_ARM64) && !defined(PORTABLE_BYTECODE)
  /* On Arm64, the absence of unwind info means that the `__try`...`__catch`
     in "scheme.c" doesn't get a chance to handle exceptions. */
  AddVectoredExceptionHandler(TRUE, fault_handler);
#endif
}
#else /* WIN32 */

#include <signal.h>

static void handle_signal(INT sig, siginfo_t *si, void *data);
static IBOOL enqueue_scheme_signal(ptr tc, INT sig);
static ptr allocate_scheme_signal_queue(void);
static void forward_signal_to_scheme(INT sig);

#define RESET_SIGNAL {\
    sigset_t set;\
    sigemptyset(&set);\
    sigaddset(&set, sig);\
    sigprocmask(SIG_UNBLOCK,&set,(sigset_t *)0);\
}

/* we buffer up to SIGNALQUEUESIZE - 1 unhandled signals, then start dropping them. */
#define SIGNALQUEUESIZE 64
static IBOOL scheme_signals_registered;

/* we use a simple queue for pending signals.  signals are enqueued only by the
   C signal handler and dequeued only by the Scheme event handler.  since the signal
   handler and event handler run in the same thread, there's no need for locks
   or write barriers. */

struct signal_queue {
  INT head;
  INT tail;
  INT data[SIGNALQUEUESIZE];
};

static IBOOL enqueue_scheme_signal(ptr tc, INT sig) {
  struct signal_queue *queue = TO_VOIDP(SIGNALINTERRUPTQUEUE(tc));
  /* ignore the signal if we failed to allocate the queue */
  if (queue == NULL) return 0;
  INT tail = queue->tail;
  INT next_tail = tail + 1;
  if (next_tail == SIGNALQUEUESIZE) next_tail = 0;
  /* ignore the signal if the queue is full */
  if (next_tail == queue->head) return 0;
  queue->data[tail] = sig;
  queue->tail = next_tail;
  return 1;
}

ptr S_dequeue_scheme_signals(ptr tc) {
  ptr ls = Snil;
  struct signal_queue *queue = TO_VOIDP(SIGNALINTERRUPTQUEUE(tc));
  if (queue == NULL) return ls;
  INT head = queue->head;
  INT tail = queue->tail;
  INT i = tail;
  while (i != head) {
    if (i == 0) i = SIGNALQUEUESIZE;
    i -= 1;
    ls = Scons(Sfixnum(queue->data[i]), ls);
  }
  queue->head = tail;
  return ls;
}

static void forward_signal_to_scheme(INT sig) {
  ptr tc = get_thread_context();

#ifdef PTHREADS
  /* deliver signals to the main thread, only; depending
     on the threads that are running, `tc` might even be NULL */
  if (tc != TO_PTR(&S_G.thread_context)) {
    pthread_kill(S_main_thread_id, sig);
    RESET_SIGNAL
    return;
  }
#endif

  if (enqueue_scheme_signal(tc, sig)) {
    SIGNALINTERRUPTPENDING(tc) = Strue;
    SOMETHINGPENDING(tc) = Strue;
  }
  RESET_SIGNAL
}

static ptr allocate_scheme_signal_queue(void) {
  /* silently fail to allocate space for signals if malloc returns NULL */
  struct signal_queue *queue = malloc(sizeof(struct signal_queue));
  if (queue != (struct signal_queue *)0) {
    queue->head = queue->tail = 0;
  }
  return TO_PTR(queue);
}

ptr S_allocate_scheme_signal_queue(void) {
  return scheme_signals_registered ? allocate_scheme_signal_queue() : (ptr)0;
}

void S_register_scheme_signal(iptr sig) {
    struct sigaction act;

    tc_mutex_acquire();
    if (!scheme_signals_registered) {
      ptr ls;
      scheme_signals_registered = 1;
      for (ls = S_threads; ls != Snil; ls = Scdr(ls)) {
        SIGNALINTERRUPTQUEUE(THREADTC(Scar(ls))) = S_allocate_scheme_signal_queue();
      }
    }
    tc_mutex_release();

    sigfillset(&act.sa_mask);
    act.sa_flags = 0;
    act.sa_handler = forward_signal_to_scheme;
    sigaction(sig, &act, (struct sigaction *)0);
}

static void handle_signal(INT sig, UNUSED siginfo_t *si, UNUSED void *data) {
/* printf("handle_signal(%d) for tc %x\n", sig, UNFIX(get_thread_context())); fflush(stdout); */
  /* check for particular signals */
    switch (sig) {
        case SIGINT: {
            ptr tc = get_thread_context();
           /* disable keyboard interrupts in subordinate threads until we think
             of something more clever to do with them */
            if (tc == TO_PTR(&S_G.thread_context)) {
              if (!THREAD_GC(tc)->during_alloc && Sboolean_value(KEYBOARDINTERRUPTPENDING(tc))) {
               /* this is a no-no, but the only other options are to ignore
                  the signal or to kill the process */
                RESET_SIGNAL
                S_noncontinuable_interrupt();
              }
              keyboard_interrupt(tc);
            }
            RESET_SIGNAL
            break;
        }
#ifdef SIGQUIT
        case SIGQUIT:
            RESET_SIGNAL
            S_abnormal_exit();
	    break;	/* Pacify compilers treating fallthrough warnings as errors */
#endif /* SIGQUIT */
        case SIGILL:
            RESET_SIGNAL
            S_error_reset("illegal instruction");
	    break;	/* Pacify compilers treating fallthrough warnings as errors */
        case SIGFPE:
            RESET_SIGNAL
            S_error_reset("arithmetic overflow");
	    break;	/* Pacify compilers treating fallthrough warnings as errors */
#ifdef SIGBUS
        case SIGBUS:
#endif /* SIGBUS */
        case SIGSEGV:
          {
            ptr tc = get_thread_context();
            RESET_SIGNAL
            if (THREAD_GC(tc)->during_alloc)
                S_error_abort("nonrecoverable invalid memory reference");
            else
                S_error_reset("invalid memory reference");
          }
	    break;
        default:
            RESET_SIGNAL
            S_error_reset("unexpected signal");
	    break;
    }
}

static void no_op_register(UNUSED int sigid) {
}

#define SIGACTION(id, act_p, old_p) (register_modified_signal(id), sigaction(id, act_p, old_p))

static void init_signal_handlers(void) {
    struct sigaction act;

    if (register_modified_signal == NULL)
      register_modified_signal = no_op_register;

    sigemptyset(&act.sa_mask);

  /* drop pending keyboard interrupts */
    act.sa_flags = 0;
    act.sa_handler = SIG_IGN;
    SIGACTION(SIGINT, &act, (struct sigaction *)0);

  /* ignore broken pipe signals */
    act.sa_flags = 0;
    act.sa_handler = SIG_IGN;
    SIGACTION(SIGPIPE, &act, (struct sigaction *)0);

  /* set up to catch SIGINT w/no system call restart */
#ifdef SA_INTERRUPT
    act.sa_flags = SA_INTERRUPT|SA_SIGINFO;
#else
    act.sa_flags = SA_SIGINFO;
#endif /* SA_INTERRUPT */
    act.sa_sigaction = handle_signal;
    SIGACTION(SIGINT, &act, (struct sigaction *)0);
#ifdef BSDI
    siginterrupt(SIGINT, 1);
#endif

  /* set up to catch selected signals */
    act.sa_flags = SA_SIGINFO;
    act.sa_sigaction = handle_signal;
#ifdef SA_RESTART
    act.sa_flags |= SA_RESTART;
#endif /* SA_RESTART */
#ifdef SIGQUIT
    SIGACTION(SIGQUIT, &act, (struct sigaction *)0);
#endif /* SIGQUIT */
    SIGACTION(SIGILL, &act, (struct sigaction *)0);
    SIGACTION(SIGFPE, &act, (struct sigaction *)0);
#ifdef SIGBUS
    SIGACTION(SIGBUS, &act, (struct sigaction *)0);
#endif /* SIGBUS */
    SIGACTION(SIGSEGV, &act, (struct sigaction *)0);
}

#endif /* WIN32 */

void S_schsig_init(void) {
    if (S_boot_time) {
        ptr p;
        ptr tc = get_thread_context();

        S_protect(&S_G.nuate_id);
        S_G.nuate_id = S_intern((const unsigned char *)"$nuate");
        S_set_symbol_value(S_G.nuate_id, FIX(0));

        S_protect(&S_G.null_continuation_id);
        S_G.null_continuation_id = S_intern((const unsigned char *)"$null-continuation");

        S_protect(&S_G.collect_request_pending_id);
        S_G.collect_request_pending_id = S_intern((const unsigned char *)"$collect-request-pending");

        S_thread_start_code_write(tc, 0, 0, NULL, 0);
        p = S_code(tc, type_code | (code_flag_continuation << code_flags_offset), 0);
        CODERELOC(p) = S_relocation_table(0);
        CODENAME(p) = Sfalse;
        CODEARITYMASK(p) = FIX(0);
        CODEFREE(p) = 0;
        CODEINFO(p) = Sfalse;
        CODEPINFOS(p) = Snil;
        S_thread_end_code_write(tc, 0, 0, NULL, 0);

        S_set_symbol_value(S_G.null_continuation_id,
            S_mkcontinuation(space_new,
                           0,
                           CODEENTRYPOINT(p),
                           FIX(0),
                           scaled_shot_1_shot_flag, scaled_shot_1_shot_flag,
                           FIX(0),
                           FIX(0),
                           Snil,
                           Snil));

        S_protect(&S_G.error_id);
        S_G.error_id = S_intern((const unsigned char *)"$c-error");

        S_protect(&S_G.event_and_resume_id);
        S_G.event_and_resume_id = S_intern((const unsigned char *)"$event-and-resume");

        S_protect(&S_G.event_and_resume_star_id);
        S_G.event_and_resume_star_id = S_intern((const unsigned char *)"$event-and-resume*");

#ifndef WIN32
        scheme_signals_registered = 0;
#endif
    }


    S_set_symbol_value(S_G.collect_request_pending_id, Sfalse);

    init_signal_handlers();
}
