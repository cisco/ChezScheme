/* foreign3.c
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

#ifndef WIN32
#include <string.h>
#endif

#ifdef _WIN32
#  define SCHEME_IMPORT
#  include "scheme.h"
#  undef EXPORT
#  define EXPORT extern __declspec (dllexport)
#else
#include "scheme.h"
#endif

EXPORT int chk_data(void) {
    static char c[10]="ABCDEFGH";

    return('A' == c[0] && 'B' == c[1] && 'C' == c[2] && 'D' == c[3] &&
           'E' == c[4] && 'F' == c[5] && 'G' == c[6] && 'H' == c[7]);
}

EXPORT int chk_bss(void) {
    static int j[2000];
    int i;

    for (i=0; i<2000; i++) if (j[i] != 0) break;

    return i == 2000;
}

EXPORT int chk_malloc(void) {
    int *j, i;

    j = (int *)malloc(2000 * sizeof(int));

    for (i=0; i<2000; i++) j[i] = 0;

    for (i=0; i<2000; i++) if (j[i] != 0) break;

    free(j);

    return i == 2000;
}

EXPORT float sxstos(float x, float y) {
    return x * y;
}

EXPORT float singlesum12(float x1, float x2, float x3, float x4,
                         float x5, float x6, float x7, float x8,
                         float x9, float x10, float x11, float x12) {
    return x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12;
}

/* these are taken from SYSTEM V Application Binary Interface
 * MIPS Processor Supplement, 1991
 * page 3-21
 */   

EXPORT double d1d2(double d1, double d2) {
    return d1 + d2;
}
EXPORT double s1s2(float s1, float s2) {
    return s1 + s2;
}
EXPORT double s1d1(float s1, double d1) {
    return s1 + d1;
}
EXPORT double d1s1(double d1, float s1) {
    return d1 + s1;
}
EXPORT double n1n2n3n4(int n1, int n2, int n3, int n4) {
    return (double)(n1 + n2 + n3 + n4);
}
EXPORT double d1n1d2(double d1, int n1, double d2) {
    return d1 + n1 + d2;
}
EXPORT double d1n1n2(double d1, int n1, int n2) {
    return d1 + n1 + n2;
}
EXPORT double s1n1n2(float s1, int n1, int n2) {
    return s1 + n1 + n2;
}
EXPORT double n1n2n3d1(int n1, int n2, int n3, double d1) {
    return n1 + n2 + n3 + d1;
}
EXPORT double n1n2n3s1(int n1, int n2, int n3, float s1) {
    return n1 + n2 + n3 + s1;
}
EXPORT double n1n2d1(int n1, int n2, double d1) {
    return n1 + n2 + d1;
}
EXPORT double n1d1(int n1, double d1) {
    return n1 + d1;
}
EXPORT double s1s2s3s4(float s1, float s2, float s3, float s4) {
    return s1 + s2 + s3 + s4;
}
EXPORT double s1n1s2n2(float s1, int n1, float s2, int n2) {
    return s1 + n1 + s2 + n2;
}
EXPORT double d1s1s2(double d1, float s1, float s2) {
    return d1 + s1 + s2;
}
EXPORT double s1s2d1(float s1, float s2, double d1) {
    return s1 + s2 + d1;
}
EXPORT double n1s1n2s2(int n1, float s1, int n2, float s2) {
    return n1 + s1 + n2 + s2;
}
EXPORT double n1s1n2n3(int n1, float s1, int n2, int n3) {
    return n1 + s1 + n2 + n3;
}
EXPORT double n1n2s1n3(int n1, int n2, float s1, int n3) {
    return n1 + n2 + s1 + n3;
}

/* a few more for good measure */
EXPORT double d1d2s1s2(double d1, double d2, float s1, float s2) {
    return d1 + d2 + s1 + s2;
}
EXPORT double d1d2n1n2(double d1, double d2, int n1, int n2) {
    return d1 + d2 + n1 + n2;
}
EXPORT double s1d1s2s3(float s1, double d1, float s2, float s3) {
    return s1 + d1 + s2 + s3;
}

/* support for testing foreign-callable */
EXPORT ptr Sinvoke2(ptr code, ptr x1, iptr x2) {
    return (*((ptr (*)(ptr, iptr))Sforeign_callable_entry_point(code)))(x1, x2);
}

EXPORT ptr Sargtest(iptr f, int x1, int x2, iptr x3, double x4, float x5, char *x6) {
    return (*((ptr (*)(int, int, iptr, double, float, char *))f))(x1, x2, x3, x4, x5, x6);
}

EXPORT ptr Sargtest2(iptr f, short x1, int x2, char x3, double x4, short x5, char x6) {
    return (*((ptr (*)(short, int, char, double, short, char))f))(x1, x2, x3, x4, x5, x6);
}

EXPORT int Srvtest_int32(ptr code, ptr x1) {
    return (*((int (*)(ptr))Sforeign_callable_entry_point(code)))(x1);
}

EXPORT unsigned Srvtest_uns32(ptr code, ptr x1) {
    return (*((unsigned (*)(ptr))Sforeign_callable_entry_point(code)))(x1);
}

EXPORT float Srvtest_single(ptr code, ptr x1) {
    return (*((float (*)(ptr))Sforeign_callable_entry_point(code)))(x1);
}

EXPORT double Srvtest_double(ptr code, ptr x1) {
    return (*((double (*)(ptr))Sforeign_callable_entry_point(code)))(x1);
}

EXPORT char Srvtest_char(ptr code, ptr x1) {
    return (*((char (*)(ptr))Sforeign_callable_entry_point(code)))(x1);
}

#ifdef WIN32
EXPORT int __stdcall sum_stdcall(int a, int b) {
    return a + b;
}

EXPORT ptr Sinvoke2_stdcall(ptr code, ptr x1, iptr x2) {
    return (*((ptr (__stdcall *)(ptr, iptr))Sforeign_callable_entry_point(code)))(x1, x2);
}

typedef int (__stdcall *comfunc) (void *, int);
typedef struct { comfunc *vtable; int data; } com_instance_t;

static comfunc com_vtable[2];
static com_instance_t com_instance;

extern int __stdcall com_method0(void *inst, int val) {
  return ((com_instance_t *)inst)->data = val;
}

extern int __stdcall com_method1(void *inst, int val) {
  return val * 2 + ((com_instance_t *)inst)->data;
}

EXPORT com_instance_t *get_com_instance(void) {
  com_instance.vtable = com_vtable;
  com_vtable[0] = com_method0;
  com_vtable[1] = com_method1;
  com_instance.data = -31;
  return &com_instance;
}
#endif /* WIN32 */

/* foreign_callable example adapted from foreign.stex */
typedef void (*CB)(char);

static CB callbacks[256];

EXPORT void cb_init(void) {
   int i;

   for (i = 0; i < 256; i += 1)
       callbacks[i] = (CB)0;
}

EXPORT void register_callback(char c, iptr cb) {
    callbacks[(int)c] = (CB)cb;
}

EXPORT void event_loop(char *s) {
    char buf[10];
    CB f; char c;

  /* create a local copy, since s points into an unlocked Scheme string */
    strncpy(buf, s, 9);
    buf[9] = '0';
    s = buf;
    for (;;) {
        c = *s++;
        if (c == 0) break;
        f = callbacks[(int)c];
        if (f != (CB)0) f(c);
    }
}

EXPORT void call_twice(void (*foo)(int), int x, int y) {
  foo(x);
  foo(y);
}

EXPORT void unlock_callback(int (* f)(int)) {
  Sunlock_object(Sforeign_callable_code_object(f));
}

EXPORT int call_and_unlock(int (* f)(int), int arg) {
  int ans = f(arg);
  Sunlock_object(Sforeign_callable_code_object(f));
  return ans;
}

EXPORT void init_lock (uptr *u) {
  INITLOCK(u);
}

EXPORT void spinlock (uptr *u) {
  SPINLOCK(u);
}

EXPORT void unlock (uptr *u) {
  UNLOCK(u);
}

EXPORT int locked_incr (uptr *u) {
  int ret;
  LOCKED_INCR(u, ret);
  return ret;
}

EXPORT int locked_decr (uptr *u) {
  int ret;
  LOCKED_DECR(u, ret);
  return ret;
}
