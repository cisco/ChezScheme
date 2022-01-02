/* thread.h
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

#ifdef FEATURE_PTHREADS
#ifdef FEATURE_WINDOWS

#include <process.h>
#include <time.h>

/* learned from http://locklessinc.com/articles/pthreads_on_windows/ which
 * Windows API types and functions to use to support mutexes and condition
 * variables.  there's much more information there if we ever need a more
 * complete implementation of pthreads functionality.
 */

typedef DWORD s_thread_t;
typedef DWORD s_thread_key_t;
typedef CRITICAL_SECTION s_thread_mutex_t;
typedef CONDITION_VARIABLE s_thread_cond_t;
typedef void s_thread_rv_t;
#define s_thread_return return
#define s_thread_self() GetCurrentThreadId()
#define s_thread_equal(t1, t2) ((t1) == (t2))
/* CreateThread description says to use _beginthread if thread uses the C library */
#define s_thread_create(start_routine, arg) (_beginthread(start_routine, 0, arg) == (uintptr_t)-1 ? EAGAIN : 0)
#define s_thread_key_create(key) ((*key = TlsAlloc()) == TLS_OUT_OF_INDEXES ? EAGAIN : 0)
#define s_thread_key_delete(key) (TlsFree(key) == 0 ? EINVAL : 0)
#define s_thread_getspecific(key) TlsGetValue(key)
#define s_thread_setspecific(key, value) (TlsSetValue(key, (void *)value) == 0 ? EINVAL : 0)
#define s_thread_mutex_init(mutex) InitializeCriticalSection(mutex)
#define s_thread_mutex_lock(mutex) (EnterCriticalSection(mutex), 0)
#define s_thread_mutex_unlock(mutex) (LeaveCriticalSection(mutex), 0)
#define s_thread_mutex_trylock(mutex) (TryEnterCriticalSection(mutex) ? 0 : EBUSY)
#define s_thread_mutex_destroy(mutex) DeleteCriticalSection(mutex)
#define s_thread_cond_init(cond) InitializeConditionVariable(cond)
#define s_thread_cond_signal(cond) WakeConditionVariable(cond)
#define s_thread_cond_broadcast(cond) WakeAllConditionVariable(cond)
#define s_thread_cond_wait(cond, mutex) (SleepConditionVariableCS(cond, mutex, INFINITE) == 0 ? EINVAL : 0)
#define s_thread_cond_destroy(cond) /* empty */

#else /* FEATURE_WINDOWS */

#include <pthread.h>

typedef pthread_t s_thread_t;
typedef pthread_key_t s_thread_key_t;
typedef pthread_mutex_t s_thread_mutex_t;
typedef pthread_cond_t s_thread_cond_t;
typedef void *s_thread_rv_t;
#define s_thread_return return NULL
#define s_thread_self() pthread_self()
#define s_thread_equal(t1, t2) pthread_equal(t1, t2)
static inline int s_thread_create(void *(* start_routine)(void *), void *arg) {
  pthread_attr_t attr; size_t stacksize; pthread_t thread; int status;

  /* The minimum stack with musl can be as small as 80k, which is
     small enough that faslin could overflow (despite the depth limit
     imposed when writing a fasl file). Make sure we have a more
     typical amount of space to work with. */
# define WANT_MIN_STACK_SIZE (512 * 1024)

  pthread_attr_init(&attr);
  pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
  pthread_attr_getstacksize(&attr, &stacksize);
  if (stacksize < WANT_MIN_STACK_SIZE)
    pthread_attr_setstacksize(&attr, WANT_MIN_STACK_SIZE);
  status = pthread_create(&thread, &attr, start_routine, arg);
  pthread_attr_destroy(&attr);
  return status;
}
#define s_thread_key_create(key) pthread_key_create(key, NULL)
#define s_thread_key_delete(key) pthread_key_delete(key)
#define s_thread_getspecific(key) pthread_getspecific(key)
#define s_thread_setspecific(key, value) pthread_setspecific(key, value)
#define s_thread_mutex_init(mutex) pthread_mutex_init(mutex, NULL)
#define s_thread_mutex_lock(mutex) pthread_mutex_lock(mutex)
#define s_thread_mutex_unlock(mutex) pthread_mutex_unlock(mutex)
#define s_thread_mutex_trylock(mutex) pthread_mutex_trylock(mutex)
#define s_thread_mutex_destroy(mutex) pthread_mutex_destroy(mutex)
#define s_thread_cond_init(cond) pthread_cond_init(cond, NULL)
#define s_thread_cond_signal(cond) pthread_cond_signal(cond)
#define s_thread_cond_broadcast(cond) pthread_cond_broadcast(cond)
#define s_thread_cond_wait(cond, mutex) pthread_cond_wait(cond, mutex)
#define s_thread_cond_destroy(cond) pthread_cond_destroy(cond)

#endif /* FEATURE_WINDOWS */
#endif /* FEATURE_PTHREADS */
