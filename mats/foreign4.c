/* foreign4.c
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

#include <stdio.h>
#include <stdlib.h>

#ifdef _WIN32
# include <Windows.h>
# define SCHEME_IMPORT
#endif

#include "scheme.h"
#undef EXPORT

#ifdef FEATURE_PTHREADS
# ifdef _WIN32
#  include <process.h>
# else
#  include <pthread.h>
# endif
#endif

typedef signed char i8;
typedef unsigned char u8;
typedef unsigned short u16;
#ifdef _WIN32
typedef __int64 i64;
# define EXPORT extern __declspec (dllexport)
#else
typedef long long i64;
# define EXPORT
#endif

/* To help make sure that argument and result handling doesn't
   read or write too far, try to provide functions that allocate
   a structure at the end of a memory page (where the next page is
   likely to be unmapped) */

#if defined(__linux__) || (defined(__APPLE__) && defined(__MACH__))

# include <stdlib.h>
# include <sys/mman.h>
# include <unistd.h>
# include <inttypes.h>

EXPORT void *malloc_at_boundary(int sz)
{
  intptr_t alloc_size = getpagesize();
  char *p;
  p = mmap(NULL, 2 * alloc_size, PROT_READ | PROT_WRITE, MAP_ANON | MAP_PRIVATE, -1, 0);
  mprotect(p + alloc_size, alloc_size, PROT_NONE);
  return p + alloc_size - sz;
}

EXPORT void free_at_boundary(void *p)
{
  intptr_t alloc_size = getpagesize();
  munmap((void *)(((intptr_t)p) & ~(alloc_size-1)), 2 * alloc_size);
}

#elif defined(_WIN32)

EXPORT void *malloc_at_boundary(int sz)
{
  SYSTEM_INFO si;
  char *p;
  DWORD dummy;
  GetSystemInfo(&si);
  p = VirtualAlloc(NULL, 2 * si.dwPageSize, MEM_COMMIT | MEM_RESERVE, PAGE_READWRITE);
  VirtualProtect(p + si.dwPageSize, si.dwPageSize, PAGE_NOACCESS, &dummy);
  return p + si.dwPageSize - sz;
}

EXPORT void free_at_boundary(void *p)
{
  SYSTEM_INFO si;
  GetSystemInfo(&si);
  VirtualFree((void *)(((intptr_t)p) & ~(si.dwPageSize-1)), 0, MEM_RELEASE);
}

#else

EXPORT void *malloc_at_boundary(int sz)
{
  return malloc(sz);
}
 
EXPORT void free_at_boundary(void *p)
{
  free(p);
}

#endif

#ifdef FEATURE_PTHREADS

#if defined(_WIN32)
# define os_thread_t unsigned
# define os_thread_create(addr, proc, arg) (((*(addr)) = _beginthread((void(*)(void*))proc, 0, arg)) == -1)
# define os_thread_join(t) WaitForSingleObject((HANDLE)(intptr_t)(t), INFINITE)
#else
# define os_thread_t pthread_t
# define os_thread_create(addr, proc, arg) pthread_create(addr, NULL, proc, proc_and_arg)
# define os_thread_join(t) pthread_join(t, NULL)
#endif

typedef struct in_thread_args_t {
  double (*proc)(double arg);
  double arg;
  int n_times;
} in_thread_args_t;

static void *in_thread(void *_proc_and_arg)
{
  in_thread_args_t *proc_and_arg = _proc_and_arg;
  int i;

  for (i = 0; i < proc_and_arg->n_times; i++) {
    proc_and_arg->arg = proc_and_arg->proc(proc_and_arg->arg);
  }

  return NULL;
}

EXPORT double call_in_unknown_thread(double (*proc)(double arg), double arg,
                                     int n_times,
                                     int do_fork, int do_deactivate) {
  os_thread_t t;
  in_thread_args_t *proc_and_arg = malloc(sizeof(in_thread_args_t));

  proc_and_arg->proc = proc;
  proc_and_arg->arg = arg;
  proc_and_arg->n_times = n_times;

  if (do_fork) {
    if (do_deactivate) Sdeactivate_thread();
    if (!os_thread_create(&t, in_thread, proc_and_arg)) {
      os_thread_join(t);
    }
    if (do_deactivate) Sactivate_thread();
  } else {
    in_thread(proc_and_arg);
  }

  arg = proc_and_arg->arg;
  free(proc_and_arg);

  return arg;
}

typedef struct in_one_thread_args_t {
  int (*proc)(int);
  int arg;
} in_one_thread_args_t;

static void *in_one_thread(void *_proc_and_arg)
{
  in_one_thread_args_t *proc_and_arg = _proc_and_arg;
  int i;

  Sactivate_thread();

  proc_and_arg->arg = proc_and_arg->proc(proc_and_arg->arg);

  Sdestroy_thread();

  return NULL;
}

EXPORT int call_in_many_unknown_threads(int (*proc)(int), int arg,
                                        int n_threads) {
  os_thread_t t;
  in_one_thread_args_t *proc_and_arg = malloc(sizeof(in_one_thread_args_t));

  proc_and_arg->proc = proc;
  proc_and_arg->arg = arg;

  while (n_threads-- > 0) {
    if (!os_thread_create(&t, in_one_thread, proc_and_arg)) {
      os_thread_join(t);
    }
  }

  arg = proc_and_arg->arg;
  free(proc_and_arg);

  return arg;
}
#endif /* FEATURE_PTHREADS */

EXPORT unsigned spin_a_while(int amt, unsigned a, unsigned b)
{
  int i;

  /* A loop that the compiler is unlikely to optimize away */
  for (i = 0; i < amt; i++) {
    a = a + b;
    b = b + a;
  }

  return a;
}

#define GEN(ts, init, sum)                                              \
  EXPORT ts f4_get_ ## ts () {                                          \
    ts r = init;                                                        \
    return r;                                                           \
  }                                                                     \
  EXPORT double f4_sum_ ## ts (ts v) {                                  \
    return sum(v);                                                      \
  }                                                                     \
  EXPORT double f4_sum_two_ ## ts (ts v1, ts v2) {                      \
    return sum(v1) + sum(v2);                                           \
  }                                                                     \
  EXPORT double f4_sum_pre_double_ ## ts (double v0, ts v) {            \
    return v0 + sum(v);                                                 \
  }                                                                     \
  EXPORT double f4_sum_pre_double_double_ ## ts (double v0, double v1, ts v) { \
    return v0 + v1 + sum(v);                                            \
  }                                                                     \
  EXPORT double f4_sum_pre_double_double_double_double_ ## ts (double v0, double v1, double v2, double v3, ts v) { \
    return v0 + v1 + v2 + v3 + sum(v);                                  \
  }                                                                     \
  EXPORT double f4_sum_pre_double_double_double_double_double_double_double_double_ ## ts \
  (double v0, double v1, double v2, double v3, double v4, double v5, double v6, double v7, ts v) { \
    return v0 + v1 + v2 + v3 + v4 + v5 + v6 + v7 + sum(v);              \
  }                                                                     \
  EXPORT double f4_sum_ ## ts ## _post_double (ts v, double v0) {       \
    return v0 + sum(v);                                                 \
  }                                                                     \
  EXPORT double f4_sum_pre_int_ ## ts (int v0, ts v) {                  \
    return (double)v0 + sum(v);                                         \
  }                                                                     \
  EXPORT double f4_sum_pre_int_int_ ## ts (int v0, int v1, ts v) {      \
    return (double)v0 + (double)v1 + sum(v);                            \
  }                                                                     \
  EXPORT double f4_sum_pre_int_int_int_int_ ## ts (int v0, int v1, int v2, int v3, ts v) { \
    return (double)v0 + (double)v1 + (double)v2 + (double)v3 + sum(v);  \
  }                                                                     \
  EXPORT double f4_sum_pre_int_int_int_int_int_int_ ## ts (int v0, int v1, int v2, int v3, int v4, int v5, ts v) { \
    return (double)v0 + (double)v1 + (double)v2 + (double)v3 + (double)v4 + (double)v5 + sum(v); \
  }                                                                     \
  EXPORT double f4_sum_ ## ts ## _post_int (ts v, int v0) {             \
    return (double)v0 + sum(v);                                         \
  }                                                                     \
  EXPORT double f4_cb_send_ ## ts (double (*cb)(ts)) {                  \
    ts r = init;                                                        \
    return cb(r) + 1.0;							\
  }                                                                     \
  EXPORT double f4_cb_send_two_ ## ts (double (*cb)(ts, ts)) {          \
    ts r1 = init;                                                       \
    ts r2 = init;                                                       \
    return cb(r1, r2) + 1.0;                                            \
  }                                                                     \
  EXPORT double f4_cb_send_pre_int_ ## ts (double (*cb)(int, ts)) {     \
    ts r = init;                                                        \
    return cb(8, r) + 1.0;                                              \
  }                                                                     \
  EXPORT double f4_cb_send_pre_int_int_ ## ts (double (*cb)(int, int, ts)) { \
    ts r = init;                                                        \
    return cb(8, 9, r) + 1.0;                                           \
  }                                                                     \
  EXPORT double f4_cb_send_pre_int_int_int_int_ ## ts (double (*cb)(int, int, int, int, ts)) { \
    ts r = init;                                                        \
    return cb(8, 9, 10, 11, r) + 1.0;                                   \
  }                                                                     \
  EXPORT double f4_cb_send_pre_int_int_int_int_int_int_ ## ts (double (*cb)(int, int, int, int, int, int, ts)) { \
    ts r = init;                                                        \
    return cb(8, 9, 10, 11, 12, 13, r) + 1.0;                           \
  }                                                                     \
  EXPORT double f4_cb_send_pre_double_ ## ts (double (*cb)(double, ts)) { \
    ts r = init;                                                        \
    return cb(8.25, r) + 1.0;                                           \
  }                                                                     \
  EXPORT double f4_cb_send_pre_double_double_ ## ts (double (*cb)(double, double, ts)) { \
    ts r = init;                                                        \
    return cb(8.25, 9.25, r) + 1.0;                                     \
  }                                                                     \
  EXPORT double f4_cb_send_pre_double_double_double_double_ ## ts (double (*cb)(double, double, double, double, ts)) { \
    ts r = init;                                                        \
    return cb(8.25, 9.25, 10.25, 11.25, r) + 1.0;                       \
  }                                                                     \
  EXPORT double f4_cb_send_pre_double_double_double_double_double_double_double_double_ ## ts \
  (double (*cb)(double, double, double, double, double, double, double, double, ts)) { \
    ts r = init;                                                        \
    return cb(8.25, 9.25, 10.25, 11.25, 12.25, 13.25, 14.25, 15.25, r) + 1.0; \
  }                                                                     \
  EXPORT double f4_sum_cb_ ## ts (ts (*cb)()) {                         \
    ts v = cb();                                                        \
    return sum(v);                                                      \
  }

#define TO_DOUBLE(x) ((double)(x))
GEN(i8, -11, TO_DOUBLE)
GEN(u8, 129, TO_DOUBLE)
GEN(short, -22, TO_DOUBLE)
GEN(u16, 33022, TO_DOUBLE)
GEN(long, 33, TO_DOUBLE)
GEN(int, 44, TO_DOUBLE)
GEN(i64, 49, TO_DOUBLE)
GEN(float, 55.0, TO_DOUBLE)
GEN(double, 66.0, TO_DOUBLE)

/* Some ABIs treat a struct containing a single field different that
   just the field */
#define GEN_1(t1, v1)                                                   \
  typedef struct struct_ ## t1 { t1 x; } struct_ ## t1;                 \
  static double _f4_sum_struct_ ## t1 (struct_ ## t1 v) {               \
    return (double)v.x;                                                 \
  }                                                                     \
  static struct_ ## t1 init_struct_ ## t1 = { v1 };                     \
  GEN(struct_ ## t1, init_struct_ ## t1, _f4_sum_struct_ ## t1)

GEN_1(i8, -12)
GEN_1(u8, 212)
GEN_1(short, -23)
GEN_1(u16, 33023)
GEN_1(long, 34)
GEN_1(int, 45)
GEN_1(i64, 48)
GEN_1(float, 56.0)
GEN_1(double, 67.0)

#define GEN_2(t1, t2, v1, v2)                                           \
  typedef struct struct_ ## t1 ## _ ## t2 { t1 x; t2 y; } struct_ ## t1 ## _ ## t2; \
  static double _f4_sum_struct_ ## t1 ## _ ## t2 (struct_ ## t1 ## _ ## t2 v) { \
    return (double)v.x + (double)v.y;                                   \
  }                                                                     \
  static struct_ ## t1 ## _ ## t2 init_struct_ ## t1 ## _ ## t2 = { v1, v2 }; \
  GEN(struct_ ## t1 ## _ ## t2, init_struct_ ## t1 ## _ ## t2, _f4_sum_struct_ ## t1 ## _ ## t2)

#define GEN_2_SET(t, x)                         \
  GEN_2(t, i8, 1+x, 10)                         \
  GEN_2(t, short, 2+x, 20)                      \
  GEN_2(t, long, 3+x, 30)                       \
  GEN_2(t, i64, 5+x, 50)                        \
  GEN_2(short, t, 6, 60+x)                      \
  GEN_2(long, t, 7, 70+x)                       \
  GEN_2(i64, t, 9, 90+x)                        \
  GEN_2(i8, t, 10, 100+x)

GEN_2_SET(int, 0)
GEN_2_SET(float, 0.5)
GEN_2_SET(double, 0.25)

GEN_2(int, int, 4, 40)
GEN_2(float, float, 4.5, 40.5)
GEN_2(double, double, 4.25, 40.25)

#define GEN_3(t1, t2, t3, v1, v2, v3)                                   \
  typedef struct struct_ ## t1 ## _ ## t2 ## _ ## t3 { t1 x; t2 y; t3 z; } struct_ ## t1 ## _ ## t2 ## _ ## t3; \
  static double _f4_sum_struct_ ## t1 ## _ ## t2 ## _ ## t3 (struct_ ## t1 ## _ ## t2 ## _ ## t3 v) { \
    return (double)v.x + (double)v.y + (double)v.z;                     \
  }                                                                     \
  static struct_ ## t1 ## _ ## t2 ## _ ## t3 init_struct_ ## t1 ## _ ## t2 ## _ ## t3 = { v1, v2, v3 }; \
  GEN(struct_ ## t1 ## _ ## t2 ## _ ## t3, init_struct_ ## t1 ## _ ## t2 ## _ ## t3, _f4_sum_struct_ ## t1 ## _ ## t2 ## _ ## t3)

#define GEN_3_SET(t, x)                           \
  GEN_3(t, i8, int, 1+x, 10, 100)                 \
  GEN_3(t, short, int, 2+x, 20, 200)              \
  GEN_3(t, long, int, 3+x, 30, 300)               \
  GEN_3(t, i64, int, 5+x, 50, 500)                \
  GEN_3(short, t, int, 6, 60+x, 600)              \
  GEN_3(long, t, int, 7, 70+x, 700)               \
  GEN_3(i64, t, int, 9, 90+x, 900)                \
  GEN_3(i8, t, int, 10, 100+x, 1000)

GEN_3_SET(int, 0)
GEN_3_SET(float, 0.5)
GEN_3_SET(double, 0.25)

GEN_3(i8, i8, i8, 4, 38, 127)
GEN_3(short, short, short, 4, 39, 399)
GEN_3(int, int, int, 4, 40, 400)
GEN_3(float, float, float, 4.5, 40.5, 400.5)
GEN_3(double, double, double, 4.25, 40.25, 400.25)

typedef struct struct_i8_i8_i8_i8_i8 { i8 x, y, z, w, q; } struct_i8_i8_i8_i8_i8;
static double _f4_sum_struct_i8_i8_i8_i8_i8 (struct_i8_i8_i8_i8_i8 v) {
  return (double)v.x + (double)v.y + (double)v.z + (double)v.w + (double)v.q;
}
static struct struct_i8_i8_i8_i8_i8 init_struct_i8_i8_i8_i8_i8 = { 1, 2, 3, 4, 5 };
GEN(struct_i8_i8_i8_i8_i8, init_struct_i8_i8_i8_i8_i8, _f4_sum_struct_i8_i8_i8_i8_i8)

typedef struct struct_i8_i8_i8_i8_i8_i8_i8 { i8 x, y, z, w, q, r, s; } struct_i8_i8_i8_i8_i8_i8_i8;
static double _f4_sum_struct_i8_i8_i8_i8_i8_i8_i8 (struct struct_i8_i8_i8_i8_i8_i8_i8 v) {
  return (double)v.x + (double)v.y + (double)v.z + (double)v.w + (double)v.q + (double)v.r + (double)v.s;
}
static struct struct_i8_i8_i8_i8_i8_i8_i8 init_struct_i8_i8_i8_i8_i8_i8_i8 = { 1, 2, 3, 4, 5, 6, 7 };
GEN(struct_i8_i8_i8_i8_i8_i8_i8, init_struct_i8_i8_i8_i8_i8_i8_i8, _f4_sum_struct_i8_i8_i8_i8_i8_i8_i8)

/* Some ABIs treat a union containing a single field different that
   just the field */
#define GEN_U1(t1, v1)                                                \
  typedef union union_ ## t1 { t1 x; } union_ ## t1;                  \
  static double _f4_sum_union_ ## t1 (union_ ## t1 v) {               \
    return (double)v.x;                                               \
  }                                                                   \
  static union_ ## t1 init_union_ ## t1 = { v1 };                     \
  GEN(union_ ## t1, init_union_ ## t1, _f4_sum_union_ ## t1)

GEN_U1(i8, -17)
GEN_U1(u8, 217)
GEN_U1(short, -27)
GEN_U1(u16, 33027)
GEN_U1(long, 37)
GEN_U1(int, 47)
GEN_U1(i64, 49)
GEN_U1(float, 57.0)
GEN_U1(double, 77.0)

#define GEN_U2(t1, t2, v1)                                              \
  typedef union union_ ## t1 ## _ ## t2 { t1 x; t2 y; } union_ ## t1 ## _ ## t2; \
  static double _f4_sum_union_ ## t1 ## _ ## t2 (union_ ## t1 ## _ ## t2 v) { \
    return (double)v.x;                                                 \
  }                                                                     \
  static union_ ## t1 ## _ ## t2 init_union_ ## t1 ## _ ## t2 = { v1 }; \
  GEN(union_ ## t1 ## _ ## t2, init_union_ ## t1 ## _ ## t2, _f4_sum_union_ ## t1 ## _ ## t2)

GEN_U2(i8, int, 18)
GEN_U2(short, int, 28)
GEN_U2(long, int, 38)
GEN_U2(int, int, 48)
GEN_U2(i64, int, 43)
GEN_U2(float, int, 58.0)
GEN_U2(double, int, 68.0)

typedef struct struct_uniondoubledouble_double {
  union { double d1; double d2; } ds;
  double d3;
} struct_uniondoubledouble_double;
static double _f4_sum_struct_uniondoubledouble_double (struct_uniondoubledouble_double v) {
  return v.ds.d1 + v.d3;
}
static struct_uniondoubledouble_double init_struct_uniondoubledouble_double = { { 99.0 }, -12.5};
GEN(struct_uniondoubledouble_double, init_struct_uniondoubledouble_double, _f4_sum_struct_uniondoubledouble_double)
