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
static void S_promote_to_multishot PROTO((ptr k));
static void split PROTO((ptr k, ptr *s));
static void reset_scheme PROTO((void));
static void do_error PROTO((iptr type, const char *who, const char *s, ptr args));
static void handle_call_error PROTO((ptr tc, iptr type, ptr x));
static void init_signal_handlers PROTO((void));
static void keyboard_interrupt PROTO((ptr tc));

ptr S_get_scheme_arg(tc, n) ptr tc; iptr n; {

    if (n <= asm_arg_reg_cnt) return REGARG(tc, n);
    else return FRAME(tc, n - asm_arg_reg_cnt);
}

void S_put_scheme_arg(tc, n, x) ptr tc; iptr n; ptr x; {

    if (n <= asm_arg_reg_cnt) REGARG(tc, n) = x;
    else FRAME(tc, n - asm_arg_reg_cnt) = x;
}

static void S_promote_to_multishot(k) ptr k; {
    while (CONTLENGTH(k) != CONTCLENGTH(k)) {
        CONTLENGTH(k) = CONTCLENGTH(k);
        k = CONTLINK(k);
    }
}

/* k must be is a multi-shot continuation, and s (the split point)
 * must be strictly between the base and end of k's stack segment. */
static void split(k, s) ptr k; ptr *s; {
    iptr m, n;
    seginfo *si;

    tc_mutex_acquire()
  /* set m to size of lower piece, n to size of upper piece */
    m = (uptr)s - (uptr)CONTSTACK(k);
    n = CONTCLENGTH(k) - m;

    si = SegInfo(ptr_get_segment(k));
  /* insert a new continuation between k and link(k) */
    CONTLINK(k) = S_mkcontinuation(si->space,
                                 si->generation,
                                 CLOSENTRY(k),
                                 CONTSTACK(k),
                                 m, m,
                                 CONTLINK(k),
                                 *s,
                                 Snil);
    CONTLENGTH(k) = CONTCLENGTH(k) = n;
    CONTSTACK(k) = (ptr)s;
    *s = (ptr)DOUNDERFLOW;
    tc_mutex_release()
}

/* We may come in to S_split_and_resize with a multi-shot contination whose
 * stack segment exceeds the copy bound or is too large to fit along
 * with the return values in the current stack.  We may also come in to
 * S_split_and_resize with a one-shot continuation for which all of the
 * above is true and for which there is insufficient space between the
 * top frame and the end of the stack.  If we have to split a 1-shot, we
 * promote it to multi-shot; doing otherwise is too much trouble.  */
void S_split_and_resize() {
    ptr tc = get_thread_context();
    ptr k; iptr value_count; iptr n;

  /* cp = continuation, ac0 = return value count */
    k = CP(tc);
    value_count = (iptr)AC0(tc);

    if (CONTCLENGTH(k) > underflow_limit) {
        iptr frame_size;
        ptr *front_stack_ptr, *end_stack_ptr, *split_point, *guard;

        front_stack_ptr = (ptr *)CONTSTACK(k);
        end_stack_ptr = (ptr *)((uptr)front_stack_ptr + CONTCLENGTH(k));

        guard = (ptr *)((uptr)end_stack_ptr - underflow_limit);

      /* set split point to base of top frame */
        frame_size = ENTRYFRAMESIZE(CONTRET(k));
        split_point = (ptr *)((uptr)end_stack_ptr - frame_size);

      /* split only if we have more than one frame */
        if (split_point != front_stack_ptr) {
          /* walk the stack to set split_point at first frame above guard */
          /* note that first frame may have put us below the guard already */
            for (;;) {
                ptr *p;
                frame_size = ENTRYFRAMESIZE(*split_point);
                p = (ptr *)((uptr)split_point - frame_size);
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
    if (n >= SCHEMESTACKSIZE(tc)) {
       tc_mutex_acquire()
       S_reset_scheme_stack(tc, n);
       tc_mutex_release()
    }
}

iptr S_continuation_depth(k) ptr k; {
    iptr n, frame_size; ptr *stack_base, *stack_ptr;

    n = 0;
  /* terminate on shot 1-shot, which could be null_continuation */
    while (CONTLENGTH(k) != scaled_shot_1_shot_flag) {
        stack_base = (ptr *)CONTSTACK(k);
        frame_size = ENTRYFRAMESIZE(CONTRET(k));
        stack_ptr = (ptr *)((uptr)stack_base + CONTCLENGTH(k));
        for (;;) {
            stack_ptr = (ptr *)((uptr)stack_ptr - frame_size);
            n += 1;
            if (stack_ptr == stack_base) break;
            frame_size = ENTRYFRAMESIZE(*stack_ptr);
        }
        k = CONTLINK(k);
    }
    return n;
}

ptr S_single_continuation(k, n) ptr k; iptr n; {
    iptr frame_size; ptr *stack_base, *stack_top, *stack_ptr;

  /* bug out on shot 1-shots, which could be null_continuation */
    while (CONTLENGTH(k) != scaled_shot_1_shot_flag) {
        stack_base = (ptr *)CONTSTACK(k);
        stack_top = (ptr *)((uptr)stack_base + CONTCLENGTH(k));
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

                stack_ptr = (ptr *)((uptr)stack_ptr - frame_size);
                if (stack_ptr != stack_base)
                    split(k, stack_ptr);

                return k;
            } else {
                n -= 1;
                stack_ptr = (ptr *)((uptr)stack_ptr - frame_size);
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
    S_overflow(tc, ((ptr *)XP(tc) - (ptr *)SFP(tc))*sizeof(ptr));
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
void S_overflow(tc, frame_request) ptr tc; iptr frame_request; {
    ptr *sfp;
    iptr above_split_size, sfp_offset;
    ptr *split_point, *guard, *other_guard;
    iptr split_stack_length, split_stack_clength;
    ptr nuate;

    sfp = (ptr *)SFP(tc);
    nuate = SYMVAL(S_G.nuate_id);
    if (!Scodep(nuate)) {
        S_error_abort("overflow: nuate not yet defined");
    }

    guard = (ptr *)((uptr)sfp - underflow_limit);
  /* leave at least stack_slop headroom in the old stack to reduce the need for return-point overflow checks */
    other_guard = (ptr *)((uptr)SCHEMESTACK(tc) + (uptr)SCHEMESTACKSIZE(tc) - (uptr)stack_slop);
    if ((uptr)other_guard < (uptr)guard) guard = other_guard;

  /* split only if old stack contains more than underflow_limit bytes */
    if (guard > (ptr *)SCHEMESTACK(tc)) {
        iptr frame_size;

      /* set split point to base of the frame below the current one */
        frame_size = ENTRYFRAMESIZE(*sfp);
        split_point = (ptr *)((uptr)sfp - frame_size);

      /* split only if we have more than one frame */
        if (split_point != (ptr *)SCHEMESTACK(tc)) {
          /* walk the stack to set split_point at first frame above guard */
          /* note that first frame may have put us below the guard already */
            for (;;) {
                ptr *p;

                frame_size = ENTRYFRAMESIZE(*split_point);
                p = (ptr *)((uptr)split_point - frame_size);
                if (p < guard) break;
                split_point = p;
            }

            split_stack_clength = (uptr)split_point - (uptr)SCHEMESTACK(tc);

          /* promote to multi-shot if current stack is shrimpy */
            if (SCHEMESTACKSIZE(tc) < default_stack_size / 4) {
                split_stack_length = split_stack_clength;
                S_promote_to_multishot(STACKLINK(tc));
            } else {
                split_stack_length = SCHEMESTACKSIZE(tc);
            }

          /* create a continuation */
            tc_mutex_acquire()
            STACKLINK(tc) = S_mkcontinuation(space_new,
                                        0,
                                        CODEENTRYPOINT(nuate),
                                        SCHEMESTACK(tc),
                                        split_stack_length,
                                        split_stack_clength,
                                        STACKLINK(tc),
                                        *split_point,
                                        Snil);
            tc_mutex_release()

          /* overwrite old return address with dounderflow */
            *split_point = (ptr)DOUNDERFLOW;
        }
    } else {
        split_point = (ptr *)SCHEMESTACK(tc);
    }

    above_split_size = SCHEMESTACKSIZE(tc) - ((uptr)split_point - (uptr)SCHEMESTACK(tc));

  /* allocate a new stack, retaining same relative sfp */
    sfp_offset = (uptr)sfp - (uptr)split_point;
    tc_mutex_acquire()
    S_reset_scheme_stack(tc, above_split_size + frame_request);
    tc_mutex_release()
    SFP(tc) = (ptr)((uptr)SCHEMESTACK(tc) + sfp_offset);

  /* copy up everything above the split point.  we don't know where the
     current frame ends, so we copy through the end of the old stack */
    {ptr *p, *q; iptr n;
     p = (ptr *)SCHEMESTACK(tc);
     q = split_point;
     for (n = above_split_size; n != 0; n -= sizeof(ptr)) *p++ = *q++;
    }
}

void S_error_abort(s) const char *s; {
    fprintf(stderr, "%s\n", s);
    S_abnormal_exit();
}

void S_abnormal_exit() {
  S_abnormal_exit_proc();
  fprintf(stderr, "abnormal_exit proedure did not exit\n");
  exit(1);
}

static void reset_scheme() {
    ptr tc = get_thread_context();

    tc_mutex_acquire()
   /* eap should always be up-to-date now that we write-through to the tc
      when making any changes to eap when eap is a real register */
    S_scan_dirty((ptr **)EAP(tc), (ptr **)REAL_EAP(tc));
    S_reset_allocation_pointer(tc);
    S_reset_scheme_stack(tc, stack_slop);
    FRAME(tc,0) = (ptr)DOUNDERFLOW;
    tc_mutex_release()
}

/* error_resets occur with the system in an unknown state,
 * thus we must reset with no opportunity for debugging
 */

void S_error_reset(s) const char *s; {

    if (!S_errors_to_console) reset_scheme();
    do_error(ERROR_RESET, "", s, Snil);
}

void S_error(who, s) const char *who, *s; {
    do_error(ERROR_OTHER, who, s, Snil);
}

void S_error1(who, s, x) const char *who, *s; ptr x; {
    do_error(ERROR_OTHER, who, s, LIST1(x));
}

void S_error2(who, s, x, y) const char *who, *s; ptr x, y; {
    do_error(ERROR_OTHER, who, s, LIST2(x,y));
}

void S_error3(who, s, x, y, z) const char *who, *s; ptr x, y, z; {
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

static void do_error(type, who, s, args) iptr type; const char *who, *s; ptr args; {
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
                 Scons((strlen(who) == 0 ? Sfalse : S_string(who,-1)),
                       Scons(S_string(s, -1), args)));

#ifdef PTHREADS
    while (S_tc_mutex_depth > 0) {
      S_mutex_release(&S_tc_mutex);
      S_tc_mutex_depth -= 1;
    }
#endif /* PTHREADS */
    
    TRAP(tc) = (ptr)1;
    AC0(tc) = (ptr)1;
    CP(tc) = S_symbol_value(S_G.error_id);
    S_put_scheme_arg(tc, 1, args);
    LONGJMP(CAAR(CCHAIN(tc)), -1);
}

static void handle_call_error(tc, type, x) ptr tc; iptr type; ptr x; {
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

void S_handle_values_error() {
    ptr tc = get_thread_context();

    handle_call_error(tc, ERROR_VALUES, Sfalse);
}

void S_handle_mvlet_error() {
    ptr tc = get_thread_context();

    handle_call_error(tc, ERROR_MVLET, Sfalse);
}

static void keyboard_interrupt(ptr tc) {
  KEYBOARDINTERRUPTPENDING(tc) = Strue;
  SOMETHINGPENDING(tc) = Strue;
}

/* used in printf below
static uptr list_length(ls) ptr ls; {
  uptr i = 0;
  while (ls != Snil) { ls = Scdr(ls); i += 1; }
  return i;
}
*/

void S_fire_collector() {
  ptr crp_id = S_G.collect_request_pending_id;

/*  printf("firing collector!\n"); fflush(stdout); */

  if (!Sboolean_value(S_symbol_value(crp_id))) {
    ptr ls;

/*    printf("really firing collector!\n"); fflush(stdout); */

    tc_mutex_acquire()
   /* check again in case some other thread beat us to the punch */
    if (!Sboolean_value(S_symbol_value(crp_id))) {
/* printf("firing collector nthreads = %d\n", list_length(S_threads)); fflush(stdout); */
      S_set_symbol_value(crp_id, Strue);
      for (ls = S_threads; ls != Snil; ls = Scdr(ls))
        SOMETHINGPENDING(THREADTC(Scar(ls))) = Strue;
    }
    tc_mutex_release()
  }
}

void S_noncontinuable_interrupt() {
  ptr tc = get_thread_context();

  reset_scheme();
  KEYBOARDINTERRUPTPENDING(tc) = Sfalse;
  do_error(ERROR_NONCONTINUABLE_INTERRUPT,"","",Snil);
}

#ifdef WIN32
/* code courtesy Bob Burger, burgerrg@sagian.com
   We cannot call noncontinuable_interrupt, because we are not allowed
   to perform a longjmp inside a signal handler; instead, we don't
   handle the signal, which will cause the process to terminate.
*/

void S_register_scheme_signal(sig) iptr sig; {
    S_error("register_scheme_signal", "unsupported in this version");
}

static BOOL WINAPI handle_signal(DWORD dwCtrlType) {
  switch (dwCtrlType) {
    case CTRL_C_EVENT:
    case CTRL_BREAK_EVENT: {
#ifdef PTHREADS
     /* get_thread_context() always returns 0, so assume main thread */
      ptr tc = S_G.thread_context;
#else
      ptr tc = get_thread_context();
#endif
      if (!S_pants_down && Sboolean_value(KEYBOARDINTERRUPTPENDING(tc)))
        return(FALSE);
      keyboard_interrupt(tc);
      return(TRUE);
    }
  }
  return(FALSE);
}

static void init_signal_handlers() {
  SetConsoleCtrlHandler(handle_signal, TRUE);
}
#else /* WIN32 */

#include <signal.h>

static void handle_signal PROTO((INT sig, siginfo_t *si, void *data));
static void forward_signal_to_scheme PROTO((INT sig));

#define RESET_SIGNAL {\
    sigset_t set;\
    sigemptyset(&set);\
    sigaddset(&set, sig);\
    sigprocmask(SIG_UNBLOCK,&set,(sigset_t *)0);\
}

static void forward_signal_to_scheme(sig) INT sig; {
  ptr tc = get_thread_context();

  SIGNALINTERRUPTPENDING(tc) = Sfixnum(sig);
  SOMETHINGPENDING(tc) = Strue;
  RESET_SIGNAL
}

void S_register_scheme_signal(sig) iptr sig; {
    struct sigaction act;

    sigemptyset(&act.sa_mask);
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
            if (tc == S_G.thread_context) {
              if (!S_pants_down && Sboolean_value(KEYBOARDINTERRUPTPENDING(tc))) {
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
#endif /* SIGQUIT */
        case SIGILL:
            RESET_SIGNAL
            S_error_reset("illegal instruction");
        case SIGFPE:
            RESET_SIGNAL
            S_error_reset("arithmetic overflow");
#ifdef SIGBUS
        case SIGBUS:
#endif /* SIGBUS */
        case SIGSEGV:
            RESET_SIGNAL
            if (S_pants_down)
                S_error_abort("nonrecoverable invalid memory reference");
            else
                S_error_reset("invalid memory reference");
        default:
            RESET_SIGNAL
            S_error_reset("unexpected signal");
    }
}

static void init_signal_handlers() {
    struct sigaction act;

    sigemptyset(&act.sa_mask);

  /* drop pending keyboard interrupts */
    act.sa_flags = 0;
    act.sa_handler = SIG_IGN;
    sigaction(SIGINT, &act, (struct sigaction *)0);

  /* ignore broken pipe signals */
    act.sa_flags = 0;
    act.sa_handler = SIG_IGN;
    sigaction(SIGPIPE, &act, (struct sigaction *)0);

  /* set up to catch SIGINT w/no system call restart */
#ifdef SA_INTERRUPT
    act.sa_flags = SA_INTERRUPT|SA_SIGINFO;
#else
    act.sa_flags = SA_SIGINFO;
#endif /* SA_INTERRUPT */
    act.sa_sigaction = handle_signal;
    sigaction(SIGINT, &act, (struct sigaction *)0);
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
    sigaction(SIGQUIT, &act, (struct sigaction *)0);
#endif /* SIGQUIT */
    sigaction(SIGILL, &act, (struct sigaction *)0);
    sigaction(SIGFPE, &act, (struct sigaction *)0);
#ifdef SIGBUS
    sigaction(SIGBUS, &act, (struct sigaction *)0);
#endif /* SIGBUS */
    sigaction(SIGSEGV, &act, (struct sigaction *)0);
}

#endif /* WIN32 */

void S_schsig_init() {
    if (S_boot_time) {
        ptr p;

        S_protect(&S_G.nuate_id);
        S_G.nuate_id = S_intern((const unsigned char *)"$nuate");
        S_set_symbol_value(S_G.nuate_id, FIX(0));

        S_protect(&S_G.null_continuation_id);
        S_G.null_continuation_id = S_intern((const unsigned char *)"$null-continuation");

        S_protect(&S_G.collect_request_pending_id);
        S_G.collect_request_pending_id = S_intern((const unsigned char *)"$collect-request-pending");

        p = S_code(get_thread_context(), type_code | (code_flag_continuation << code_flags_offset), 0);
        CODERELOC(p) = S_relocation_table(0);
        CODENAME(p) = Sfalse;
        CODEARITYMASK(p) = FIX(0);
        CODEFREE(p) = 0;
        CODEINFO(p) = Sfalse;
        CODEPINFOS(p) = Snil;

        S_set_symbol_value(S_G.null_continuation_id,
            S_mkcontinuation(space_new,
                           0,
                           CODEENTRYPOINT(p),
                           FIX(0),
                           scaled_shot_1_shot_flag, scaled_shot_1_shot_flag,
                           FIX(0),
                           FIX(0),
                           Snil));

        S_protect(&S_G.error_id);
        S_G.error_id = S_intern((const unsigned char *)"$c-error");
    }


    S_pants_down = 0;
    S_set_symbol_value(S_G.collect_request_pending_id, Sfalse);

    init_signal_handlers();
}
