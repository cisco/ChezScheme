/* thread.c
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

/* locally defined functions */
#ifdef PTHREADS
static s_thread_rv_t start_thread PROTO((void *tc));
static IBOOL destroy_thread PROTO((ptr tc));
#endif

void S_thread_init() {
  if (S_boot_time) {
    S_protect(&S_G.threadno);
    S_G.threadno = FIX(0);

#ifdef PTHREADS
   /* this is also reset in scheme.c after heap restoration */
    s_thread_mutex_init(&S_tc_mutex.pmutex);
    S_tc_mutex.owner = s_thread_self();
    S_tc_mutex.count = 0;
    s_thread_cond_init(&S_collect_cond);
    S_tc_mutex_depth = 0;
#endif /* PTHREADS */
  }
}

/* this needs to be reworked.  currently, S_create_thread_object is
   called from main to create the base thread, from fork_thread when
   there is already an active current thread, and from S_activate_thread
   when there is no current thread.  we have to avoid thread-local
   allocation in at least the latter case, so we call vector_in and
   cons_in and arrange for S_thread to use find_room rather than
   thread_find_room.  scheme.c does part of the initialization of the
   base thread (e.g., parameters, current input/output ports) in one
   or more places. */
ptr S_create_thread_object(who, p_tc) const char *who; ptr p_tc; {
  ptr thread, tc;
  INT i;

  tc_mutex_acquire()

  if (S_threads == Snil) {
    tc = (ptr)S_G.thread_context;
  } else { /* clone parent */
    ptr p_v = PARAMETERS(p_tc);
    iptr i, n = Svector_length(p_v);
   /* use S_vector_in to avoid thread-local allocation */
    ptr v = S_vector_in(space_new, 0, n);

    tc = (ptr)malloc(size_tc);
    if (tc == (ptr)0)
      S_error(who, "unable to malloc thread data structure");
    memcpy((void *)tc, (void *)p_tc, size_tc);

    for (i = 0; i < n; i += 1)
      INITVECTIT(v, i) = Svector_ref(p_v, i);
    
    PARAMETERS(tc) = v;
    CODERANGESTOFLUSH(tc) = Snil;
  }

 /* override nonclonable tc fields */
  THREADNO(tc) = S_G.threadno;
  S_G.threadno = S_add(S_G.threadno, FIX(1));

  CCHAIN(tc) = Snil;

  WINDERS(tc) = Snil;
  STACKLINK(tc) = SYMVAL(S_G.null_continuation_id);
  STACKCACHE(tc) = Snil;

 /* S_reset_scheme_stack initializes stack, size, esp, and sfp */
  S_reset_scheme_stack(tc, stack_slop);
  FRAME(tc,0) = (ptr)&CODEIT(S_G.dummy_code_object,size_rp_header);

 /* S_reset_allocation_pointer initializes ap and eap */
  S_reset_allocation_pointer(tc);
  RANDOMSEED(tc) = most_positive_fixnum < 0xffffffff ? most_positive_fixnum : 0xffffffff;
  X(tc) = Y(tc) = U(tc) = V(tc) = W(tc) = FIX(0);

  TIMERTICKS(tc) = Sfalse;
  DISABLECOUNT(tc) = Sfixnum(0);
  SIGNALINTERRUPTPENDING(tc) = Sfalse;
  KEYBOARDINTERRUPTPENDING(tc) = Sfalse;

  TARGETMACHINE(tc) = S_intern((const unsigned char *)MACHINE_TYPE);

 /* choosing not to clone virtual registers */
  for (i = 0 ; i < virtual_register_count ; i += 1) {
    VIRTREG(tc, i) = FIX(0);
  }

 /* S_thread had better not do thread-local allocation */
  thread = S_thread(tc);

 /* use S_cons_in to avoid thread-local allocation */
  S_threads = S_cons_in(space_new, 0, thread, S_threads);
  S_nthreads += 1;
  SETSYMVAL(S_G.active_threads_id,
   FIX(UNFIX(SYMVAL(S_G.active_threads_id)) + 1));
  ACTIVE(tc) = 1;

 /* collect request is only thing that can be pending for new thread.
    must do this after we're on the thread list in case the cons
    adding us onto the thread list set collect-request-pending */
  SOMETHINGPENDING(tc) = SYMVAL(S_G.collect_request_pending_id);

  GUARDIANENTRIES(tc) = Snil;

  tc_mutex_release()

  return thread;
}

#ifdef PTHREADS
IBOOL Sactivate_thread() { /* create or reactivate current thread */
  ptr tc = get_thread_context();

  if (tc == (ptr)0) { /* thread created by someone else */
    ptr thread;

   /* borrow base thread for now */
    thread = S_create_thread_object("Sactivate_thread", S_G.thread_context);
    s_thread_setspecific(S_tc_key, (ptr)THREADTC(thread));
    return 1;
  } else {
    reactivate_thread(tc)
    return 0;
  }
}

int S_activate_thread() { /* Like Sactivate_thread(), but returns a mode to revert the effect */
  ptr tc = get_thread_context();

  if (tc == (ptr)0) {
    Sactivate_thread();
    return unactivate_mode_destroy;
  } else if (!ACTIVE(tc)) {
    reactivate_thread(tc);
    return unactivate_mode_deactivate;
  } else
    return unactivate_mode_noop;
}

void S_unactivate_thread(int mode) { /* Reverts a previous S_activate_thread() effect */
  switch (mode) {
  case unactivate_mode_deactivate:
    Sdeactivate_thread();
    break;
  case unactivate_mode_destroy:
    Sdestroy_thread();
    break;
  case unactivate_mode_noop:
  default:
    break;
  }
}

void Sdeactivate_thread() { /* deactivate current thread */
  ptr tc = get_thread_context();
  if (tc != (ptr)0) deactivate_thread(tc)
}

int Sdestroy_thread() { /* destroy current thread */
  ptr tc = get_thread_context();
  if (tc != (ptr)0 && destroy_thread(tc)) {
    s_thread_setspecific(S_tc_key, 0);
    return 1;
  }
  return 0;
}

static IBOOL destroy_thread(tc) ptr tc; {
  ptr *ls; IBOOL status;

  status = 0;
  tc_mutex_acquire()
  ls = &S_threads;
  while (*ls != Snil) {
    ptr thread = Scar(*ls);
    if (THREADTC(thread) == (uptr)tc) {
      *ls = Scdr(*ls);
      S_nthreads -= 1;

     /* process remembered set before dropping allocation area */
      S_scan_dirty((ptr **)EAP(tc), (ptr **)REAL_EAP(tc));

     /* process guardian entries */
      {
	ptr target, ges, obj, next; seginfo *si;
	target = S_G.guardians[0];
	for (ges = GUARDIANENTRIES(tc); ges != Snil; ges = next) {
	  obj = GUARDIANOBJ(ges);
	  next = GUARDIANNEXT(ges);
	  if (!IMMEDIATE(obj) && (si = MaybeSegInfo(ptr_get_segment(obj))) != NULL && si->generation != static_generation) {
	    INITGUARDIANNEXT(ges) = target;
	    target = ges;
	  }
	}
	S_G.guardians[0] = target;
      }

     /* deactivate thread */
      if (ACTIVE(tc)) {
        SETSYMVAL(S_G.active_threads_id,
         FIX(UNFIX(SYMVAL(S_G.active_threads_id)) - 1));
        if (Sboolean_value(SYMVAL(S_G.collect_request_pending_id))
            && SYMVAL(S_G.active_threads_id) == FIX(0)) {
          s_thread_cond_signal(&S_collect_cond);
        }
      }

      free((void *)THREADTC(thread));
      THREADTC(thread) = 0; /* mark it dead */
      status = 1;
      break;
    }
    ls = &Scdr(*ls);
  }
  tc_mutex_release()
  return status;
}

ptr S_fork_thread(thunk) ptr thunk; {
  ptr thread;
  int status;

  /* pass the current thread's context as the parent thread */
  thread = S_create_thread_object("fork-thread", get_thread_context());
  CP(THREADTC(thread)) = thunk;

  if ((status = s_thread_create(start_thread, (void *)THREADTC(thread))) != 0) {
    destroy_thread((ptr)THREADTC(thread));
    S_error1("fork-thread", "failed: ~a", S_strerror(status));
  }

  return thread;
}

static s_thread_rv_t start_thread(p) void *p; {
  ptr tc = (ptr)p; ptr cp;

  s_thread_setspecific(S_tc_key, tc);

  cp = CP(tc);
  CP(tc) = Svoid; /* should hold calling code object, which we don't have */
  TRAP(tc) = (ptr)default_timer_ticks;
  Scall0(cp);
 /* caution: calling into Scheme may result into a collection, so we
    can't access any Scheme objects, e.g., cp, after this point.  But tc
    is static, so we can access it. */

 /* find and destroy our thread */
  destroy_thread(tc);
  s_thread_setspecific(S_tc_key, (ptr)0);

  s_thread_return;
}


scheme_mutex_t *S_make_mutex() {
  scheme_mutex_t *m;

  m = (scheme_mutex_t *)malloc(sizeof(scheme_mutex_t));

  if (m == (scheme_mutex_t *)0)
    S_error("make-mutex", "unable to malloc mutex");
  s_thread_mutex_init(&m->pmutex);
  m->owner = s_thread_self();
  m->count = 0;

  return m;
}

void S_mutex_free(m) scheme_mutex_t *m; {
  s_thread_mutex_destroy(&m->pmutex);
  free(m);
}

void S_mutex_acquire(m) scheme_mutex_t *m; {
  s_thread_t self = s_thread_self();
  iptr count;
  INT status;

  if ((count = m->count) > 0 && s_thread_equal(m->owner, self)) {
    if (count == most_positive_fixnum)
      S_error1("mutex-acquire", "recursion limit exceeded for ~s", m);
    m->count = count + 1;
    return;
  }
    
  if ((status = s_thread_mutex_lock(&m->pmutex)) != 0)
    S_error1("mutex-acquire", "failed: ~a", S_strerror(status));
  m->owner = self;
  m->count = 1;
}

INT S_mutex_tryacquire(m) scheme_mutex_t *m; {
  s_thread_t self = s_thread_self();
  iptr count;
  INT status;

  if ((count = m->count) > 0 && s_thread_equal(m->owner, self)) {
    if (count == most_positive_fixnum)
      S_error1("mutex-acquire", "recursion limit exceeded for ~s", m);
    m->count = count + 1;
    return 0;
  }

  status = s_thread_mutex_trylock(&m->pmutex);
  if (status == 0) {
    m->owner = self;
    m->count = 1;
  } else if (status != EBUSY) {
    S_error1("mutex-acquire", "failed: ~a", S_strerror(status));
  }
  return status;
}

void S_mutex_release(m) scheme_mutex_t *m; {
  s_thread_t self = s_thread_self();
  iptr count;
  INT status;

  if ((count = m->count) == 0 || !s_thread_equal(m->owner, self))
    S_error1("mutex-release", "thread does not own mutex ~s", m);

  if ((m->count = count - 1) == 0)
    if ((status = s_thread_mutex_unlock(&m->pmutex)) != 0)
      S_error1("mutex-release", "failed: ~a", S_strerror(status));
}

s_thread_cond_t *S_make_condition() {
  s_thread_cond_t *c;

  c = (s_thread_cond_t *)malloc(sizeof(s_thread_cond_t));
  if (c == (s_thread_cond_t *)0)
    S_error("make-condition", "unable to malloc condition");
  s_thread_cond_init(c);
  return c;
}

void S_condition_free(c) s_thread_cond_t *c; {
  s_thread_cond_destroy(c);
  free(c);
}

#ifdef FEATURE_WINDOWS

static inline int s_thread_cond_timedwait(s_thread_cond_t *cond, s_thread_mutex_t *mutex, int typeno, long sec, long nsec) {
  if (typeno == time_utc) {
    struct timespec now;
    S_gettime(time_utc, &now);
    sec -= (long)now.tv_sec;
    nsec -= now.tv_nsec;
    if (nsec < 0) {
      sec -= 1;
      nsec += 1000000000;
    }
  }
  if (sec < 0) {
    sec = 0;
    nsec = 0;
  }
  if (SleepConditionVariableCS(cond, mutex, sec*1000 + nsec/1000000)) {
    return 0;
  } else if (GetLastError() == ERROR_TIMEOUT) {
    return ETIMEDOUT;
  } else {
    return EINVAL;
  }
}

#else /* FEATURE_WINDOWS */

static inline int s_thread_cond_timedwait(s_thread_cond_t *cond, s_thread_mutex_t *mutex, int typeno, long sec, long nsec) {
  struct timespec t;
  if (typeno == time_duration) {
    struct timespec now;
    S_gettime(time_utc, &now);
    t.tv_sec = now.tv_sec + sec;
    t.tv_nsec = now.tv_nsec + nsec;
    if (t.tv_nsec >= 1000000000) {
      t.tv_sec += 1;
      t.tv_nsec -= 1000000000;
    }
  } else {
    t.tv_sec = sec;
    t.tv_nsec = nsec;
  }
  return pthread_cond_timedwait(cond, mutex, &t);
}

#endif /* FEATURE_WINDOWS */

#define Srecord_ref(x,i) (((ptr *)((uptr)(x)+record_data_disp))[i])

IBOOL S_condition_wait(c, m, t) s_thread_cond_t *c; scheme_mutex_t *m; ptr t; {
  ptr tc = get_thread_context();
  s_thread_t self = s_thread_self();
  iptr count;
  INT typeno;
  long sec;
  long nsec;
  INT status;

  if ((count = m->count) == 0 || !s_thread_equal(m->owner, self))
    S_error1("condition-wait", "thread does not own mutex ~s", m);

  if (count != 1)
    S_error1("condition-wait", "mutex ~s is recursively locked", m);

  if (t != Sfalse) {
    /* Keep in sync with ts record in s/date.ss */
    typeno = Sinteger32_value(Srecord_ref(t,0));
    sec = Sinteger32_value(Scar(Srecord_ref(t,1)));
    nsec = Sinteger32_value(Scdr(Srecord_ref(t,1)));
  }

  if (c == &S_collect_cond || DISABLECOUNT(tc) == 0) {
    deactivate_thread(tc)
  }

  m->count = 0;
  status = (t == Sfalse) ? s_thread_cond_wait(c, &m->pmutex) :
    s_thread_cond_timedwait(c, &m->pmutex, typeno, sec, nsec);
  m->owner = self;
  m->count = 1;

  if (c == &S_collect_cond || DISABLECOUNT(tc) == 0) {
    reactivate_thread(tc)
  }

  if (status == 0) {
    return 1;
  } else if (status == ETIMEDOUT) {
    return 0;
  } else {
    S_error1("condition-wait", "failed: ~a", S_strerror(status));
    return 0;
  }
}
#endif /* PTHREADS */

