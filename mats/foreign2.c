/* foreign2.c
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
#include <wchar.h>

#ifdef _WIN32
#  define SCHEME_IMPORT
#  include "scheme.h"
#  undef EXPORT
#  define EXPORT extern __declspec (dllexport)
#else
#include "scheme.h"
#endif

EXPORT int testten(int x0,int x1,int x2,int x3,int x4,int x5,int x6,int x7,int x8,int x9) {
   return  1 * x0 +
           2 * x1 +
           3 * x2 +
           5 * x3 +
           7 * x4 +
          11 * x5 +
          13 * x6 +
          17 * x7 +
          19 * x8 +
          23 * x9;
}

EXPORT double flsum8(double x1,double x2,double x3,double x4,double x5,double x6,double x7,double x8) {
    return (x1+x2+x3+x4+x5+x6+x7+x8);
}

EXPORT double sparcfltest(int x1,int x2,int x3,int x4,int x5,double x6,int x7,double x8) {
    return (x1+x2+x3+x4+x5+x6+x7+x8);
}

EXPORT double mipsfltest1(int x1,int x2,double x3) {
    return (x1+x2+x3);
}

EXPORT double mipsfltest2(int x1,double x2,double x3) {
    return (x1+x2+x3);
}

EXPORT double ppcfltest(int x1,double x2,int x3,double x4,int x5,double x6,int x7,double x8,double x9,double x10,double x11,double x12,double x13,double x14,double x15,double x16,double x17,double x18,double x19) {
    return x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14+x15+x16+x17+x18+x19;
}

EXPORT double ppcfltest2(int x1, double x2, int x3, double x4, int x5, long long x5_5, double x6, int x7, double x8, long long x8_5, int x8_75, double x9, double x10, double x11, double x12, double x13, float x14, double x15, int x15_5, double x16, int x16_5, long long x17, double x18, int x18_5, double x19) {
    return x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14+x15+x16+x17+x18+x19 + x5_5 + x8_5 + x8_75 + x15_5 + x16_5 + x18_5;
}

typedef char i8;
typedef unsigned char u8;
typedef short i16;
typedef unsigned short u16;
typedef int i32;
typedef unsigned int u32;
#ifdef _WIN32
typedef __int64 i64;
typedef unsigned __int64 u64;
typedef __int64 LONGLONG;
typedef unsigned __int64 UNSIGNED_LONGLONG;
#else
typedef long long i64;
typedef unsigned long long u64;
typedef long long LONGLONG;
typedef unsigned long long UNSIGNED_LONGLONG;
#endif
typedef float single_float;
typedef double double_float;

EXPORT int check_types(int Bchar, int Bwchar, int Bshort, int Bint, int Blong, int Blonglong, int Bfloat, int Bdouble, int Bvoid_star) {
  int succ = 1;
  if (sizeof(i8) != 1) {
    fprintf(stderr,"sizeof(i8) [%ld] != 1\n", (long)sizeof(i8));
    succ = 0;
  }
  if (sizeof(u8) != 1) {
    fprintf(stderr,"sizeof(u8) [%ld] != 1\n", (long)sizeof(u8));
    succ = 0;
  }
  if (sizeof(i16) != 2) {
    fprintf(stderr,"sizeof(i16) [%ld] != 2\n", (long)sizeof(i16));
    succ = 0;
  }
  if (sizeof(u16) != 2) {
    fprintf(stderr,"sizeof(u16) [%ld] != 2\n", (long)sizeof(u16));
    succ = 0;
  }
  if (sizeof(i32) != 4) {
    fprintf(stderr,"sizeof(i32) [%ld] != 4\n", (long)sizeof(i32));
    succ = 0;
  }
  if (sizeof(u32) != 4) {
    fprintf(stderr,"sizeof(u32) [%ld] != 4\n", (long)sizeof(u32));
    succ = 0;
  }
  if (sizeof(i64) != 8) {
    fprintf(stderr,"sizeof(i64) [%ld] != 8\n", (long)sizeof(i64));
    succ = 0;
  }
  if (sizeof(u64) != 8) {
    fprintf(stderr,"sizeof(u64) [%ld] != 8\n", (long)sizeof(u64));
    succ = 0;
  }
  if (sizeof(single_float) != 4) {
    fprintf(stderr,"sizeof(single_float) [%ld] != 4\n", (long)sizeof(single_float));
    succ = 0;
  }
  if (sizeof(double_float) != 8) {
    fprintf(stderr,"sizeof(double_float) [%ld] != 8\n", (long)sizeof(double_float));
    succ = 0;
  }
  if (sizeof(char) != Bchar) {
    fprintf(stderr,"sizeof(char) [%ld] != %ld\n", (long)sizeof(char), (long)Bchar);
    succ = 0;
  }
  if (sizeof(wchar_t) != Bwchar) {
    fprintf(stderr,"sizeof(wchar_t) [%ld] != %ld\n", (long)sizeof(wchar_t), (long)Bwchar);
    succ = 0;
  }
  if (sizeof(short) != Bshort) {
    fprintf(stderr,"sizeof(short) [%ld] != %ld\n", (long)sizeof(short), (long)Bshort);
    succ = 0;
  }
  if (sizeof(int) != Bint) {
    fprintf(stderr,"sizeof(int) [%ld] != %ld\n", (long)sizeof(int), (long)Bint);
    succ = 0;
  }
  if (sizeof(long) != Blong) {
    fprintf(stderr,"sizeof(long) [%ld] != %ld\n", (long)sizeof(long), (long)Blong);
    succ = 0;
  }
  if (sizeof(long long) != Blonglong) {
    fprintf(stderr,"sizeof(long long) [%ld] != %ld\n", (long)sizeof(long long), (long)Blong);
    succ = 0;
  }
  if (sizeof(float) != Bfloat) {
    fprintf(stderr,"sizeof(float) [%ld] != %ld\n", (long)sizeof(float), (long)Bfloat);
    succ = 0;
  }
  if (sizeof(double) != Bdouble) {
    fprintf(stderr,"sizeof(double) [%ld] != %ld\n", (long)sizeof(double), (long)Bdouble);
    succ = 0;
  }
  if (sizeof(void *) != Bvoid_star) {
    fprintf(stderr,"sizeof(void *) [%ld] != %ld\n", (long)sizeof(void *), (long)Bvoid_star);
    succ = 0;
  }
  return succ;
}

EXPORT i8 i8_to_i8(i8 x, int k) {
  return x + k;
}

EXPORT u8 u8_to_u8(u8 x, int k) {
  return x + k;
}

EXPORT i8 call_i8(ptr code, i8 x, int m, int k) {
  return (*((i8 (*) (i8))Sforeign_callable_entry_point(code)))(x + m) + k;
}

EXPORT u8 call_u8(ptr code, u8 x, int m, int k) {
  return (*((u8 (*) (u8))Sforeign_callable_entry_point(code)))(x + m) + k;
}

EXPORT i16 i16_to_i16(i16 x, int k) {
  return x + k;
}

EXPORT u16 u16_to_u16(u16 x, int k) {
  return x + k;
}

EXPORT i16 call_i16(ptr code, i16 x, int m, int k) {
  return (*((i16 (*) (i16))Sforeign_callable_entry_point(code)))(x + m) + k;
}

EXPORT u16 call_u16(ptr code, u16 x, int m, int k) {
  return (*((u16 (*) (u16))Sforeign_callable_entry_point(code)))(x + m) + k;
}

EXPORT i32 i32_to_i32(i32 x, int k) {
  return x + k;
}

EXPORT u32 u32_to_u32(u32 x, int k) {
  return x + k;
}

EXPORT i32 call_i32(ptr code, i32 x, int m, int k) {
  return (*((i32 (*) (i32))Sforeign_callable_entry_point(code)))(x + m) + k;
}

EXPORT u32 call_u32(ptr code, u32 x, int m, int k) {
  return (*((u32 (*) (u32))Sforeign_callable_entry_point(code)))(x + m) + k;
}

EXPORT i64 i64_to_i64(u64 x, int k) {
  return x + k;
}

EXPORT u64 u64_to_u64(u64 x, int k) {
  return x + k;
}

EXPORT i64 call_i64(ptr code, i64 x, int m, int k) {
  return (*((i64 (*) (i64))Sforeign_callable_entry_point(code)))(x + m) + k;
}

EXPORT u64 call_u64(ptr code, u64 x, int m, int k) {
  return (*((u64 (*) (u64))Sforeign_callable_entry_point(code)))(x + m) + k;
}

EXPORT single_float sf_to_sf(single_float x) {
  return x + 1;
}

EXPORT single_float call_sf(ptr code, single_float x, int m, int k) {
  return (*((single_float (*) (single_float))Sforeign_callable_entry_point(code)))(x + m) + k;
}

EXPORT double_float df_to_df(double_float x) {
  return x + 1;
}

EXPORT double_float call_df(ptr code, double_float x, int m, int k) {
  return (*((double_float (*) (double_float))Sforeign_callable_entry_point(code)))(x + m) + k;
}

EXPORT u8 *u8_star_to_u8_star(u8 *s) {
  return s == (u8 *)0 ? (u8 *)0 : s + 1;
}

EXPORT u8 *call_u8_star(ptr code, u8 *s) {
  return (*((u8 *(*) (u8 *))Sforeign_callable_entry_point(code)))(s + 1) + 1;
}

EXPORT u16 *u16_star_to_u16_star(u16 *s) {
  return s == (u16 *)0 ? (u16 *)0 : s + 1;
}

EXPORT u16 *call_u16_star(ptr code, u16 *s) {
  return (*((u16 *(*) (u16 *))Sforeign_callable_entry_point(code)))(s + 1) + 1;
}

EXPORT u32 *u32_star_to_u32_star(u32 *s) {
  return s == (u32 *)0 ? (u32 *)0 : s + 1;
}

EXPORT u32 *call_u32_star(ptr code, u32 *s) {
  return (*((u32 *(*) (u32 *))Sforeign_callable_entry_point(code)))(s + 1) + 1;
}

EXPORT char *char_star_to_char_star(char *s) {
  return s == (char *)0 ? (char *)0 : s + 1;
}

EXPORT char *call_string(ptr code, char *s) {
  return (*((char *(*) (char *))Sforeign_callable_entry_point(code)))(s + 1) + 1;
}

EXPORT wchar_t *wchar_star_to_wchar_star(wchar_t *s) {
  return s == (wchar_t *)0 ? (wchar_t *)0 : s + 1;
}

EXPORT wchar_t *call_wstring(ptr code, wchar_t *s) {
  return (*((wchar_t *(*) (wchar_t *))Sforeign_callable_entry_point(code)))(s + 1) + 1;
}

EXPORT char char_to_char(char x) {
  return x - 0x20;
}

EXPORT char call_char(ptr code, char x, int m, int k) {
  return (*((char (*) (char))Sforeign_callable_entry_point(code)))(x + m) + k;
}

EXPORT wchar_t wchar_to_wchar(wchar_t x) {
  return x - 0x20;
}

EXPORT wchar_t call_wchar(ptr code, wchar_t x, int m, int k) {
  return (*((wchar_t (*) (wchar_t))Sforeign_callable_entry_point(code)))(x + m) + k;
}

EXPORT short short_to_short(short x, int k) {
  return x + k;
}

EXPORT unsigned short unsigned_short_to_unsigned_short(unsigned short x, int k) {
  return x + k;
}

EXPORT short call_short(ptr code, short x, int m, int k) {
  return (*((short (*) (short))Sforeign_callable_entry_point(code)))(x + m) + k;
}

EXPORT unsigned short call_unsigned_short(ptr code, unsigned short x, int m, int k) {
  return (*((unsigned short (*) (unsigned short))Sforeign_callable_entry_point(code)))(x + m) + k;
}

EXPORT int int_to_int(int x, int k) {
  return x + k;
}

EXPORT unsigned unsigned_to_unsigned(int x, int k) {
  return x + k;
}

EXPORT int call_int(ptr code, int x, int m, int k) {
  return (*((int (*) (int))Sforeign_callable_entry_point(code)))(x + m) + k;
}

EXPORT unsigned call_unsigned(ptr code, unsigned x, int m, int k) {
  return (*((unsigned (*) (unsigned))Sforeign_callable_entry_point(code)))(x + m) + k;
}

EXPORT long long_to_long(long x, int k) {
  return x + k;
}

EXPORT unsigned long unsigned_long_to_unsigned_long(unsigned long x, int k) {
  return x + k;
}

EXPORT long call_long(ptr code, long x, int m, int k) {
  return (*((long (*) (long))Sforeign_callable_entry_point(code)))(x + m) + k;
}

EXPORT unsigned long call_unsigned_long(ptr code, unsigned long x, int m, int k) {
  return (*((unsigned long (*) (unsigned long))Sforeign_callable_entry_point(code)))(x + m) + k;
}

EXPORT LONGLONG long_long_to_long_long(LONGLONG x, int k) {
  return x + k;
}

EXPORT UNSIGNED_LONGLONG unsigned_long_long_to_unsigned_long_long(UNSIGNED_LONGLONG x, int k) {
  return x + k;
}

EXPORT LONGLONG call_long_long(ptr code, LONGLONG x, int m, int k) {
  return (*((LONGLONG (*) (LONGLONG))Sforeign_callable_entry_point(code)))(x + m) + k;
}

EXPORT UNSIGNED_LONGLONG call_unsigned_long_long(ptr code, UNSIGNED_LONGLONG x, int m, int k) {
  return (*((UNSIGNED_LONGLONG (*) (UNSIGNED_LONGLONG))Sforeign_callable_entry_point(code)))(x + m) + k;
}

EXPORT iptr iptr_to_iptr(iptr x, int k) {
  return x + k;
}

EXPORT iptr uptr_to_uptr(uptr x, int k) {
  return x + k;
}

EXPORT iptr call_iptr(ptr code, iptr x, int m, int k) {
  return (*((iptr (*) (iptr))Sforeign_callable_entry_point(code)))(x + m) + k;
}

EXPORT iptr call_uptr(ptr code, uptr x, int m, int k) {
  return (*((uptr (*) (uptr))Sforeign_callable_entry_point(code)))(x + m) + k;
}

EXPORT float float_to_float(float x) {
  return x + 1;
}

EXPORT float call_float(ptr code, float x, int m, int k) {
  return (*((float (*) (float))Sforeign_callable_entry_point(code)))(x + m) + k;
}

EXPORT double double_to_double(double x) {
  return x + 1;
}

EXPORT double call_double(ptr code, double x, int m, int k) {
  return (*((double (*) (double))Sforeign_callable_entry_point(code)))(x + m) + k;
}

EXPORT u64 u32xu32_to_u64(u32 x, u32 y) {
  return (u64)x << 32 | (u64)y;
}

EXPORT i64 i32xu32_to_i64(i32 x, u32 y) {
  return (i64)((u64)x << 32 | (u64)y);
}

EXPORT i64 call_i32xu32_to_i64(ptr code, i32 x, u32 y, int k) {
  i64 q = (*((i64 (*) (i32, u32))Sforeign_callable_entry_point(code)))(x, y);
  return q + k;
}

EXPORT u64 ufoo64a(u64 a, u64 b, u64 c, u64 d, u64 e, u64 f, u64 g) {
  return (a - b) + (c - d) + (e - f) + g;
}

EXPORT u64 ufoo64b(u32 x, u64 a, u64 b, u64 c, u64 d, u64 e, u64 f, u64 g) {
  return (u64)x + (a - b) + (c - d) + (e - f) + g;
}

EXPORT i64 ifoo64a(i64 a, i64 b, i64 c, i64 d, i64 e, i64 f, i64 g) {
  return (a - b) + (c - d) + (e - f) + g;
}

EXPORT i64 ifoo64b(i32 x, i64 a, i64 b, i64 c, i64 d, i64 e, i64 f, i64 g) {
  return (i64)x + (a - b) + (c - d) + (e - f) + g;
}

EXPORT void call_many_times(void (*f)(iptr))
{
  int x;
  iptr a = 1, b = 3, c = 5, d = 7;
  iptr e = 1, g = 3, h = 5, i = 7;
  iptr j = 1, k = 3, l = 5, m = 7;
  iptr big = (((iptr)1) << ((8 * sizeof(iptr)) - 2));

  /* The intent of the loop is to convince the C compiler to store
     something in the same register used for CP (so, compile with
     optimization). */
  for (x = 0; x < 1000000; x++) {
    f(big|(a+e+j));
    a = b; b = c; c = d; d = e;
    e = g; g = h; h = i; i = j;
    j = k+2; k = l+2; l = m+2; m = m+2;
  }
}

EXPORT void call_many_times_bv(void (*f)(char *s))
{
  /* make this sensible as u8*, u16*, and u32* */
  char buf[8] = { 1, 2, 3, 4, 0, 0, 0, 0 };
  int x;

  for (x = 0; x < 1000000; x++) {
    buf[0] = (x & 63) + 1;
    f(buf);
  }
}

typedef void (*many_arg_callback_t)(int i, const char* s1, const char* s2, const char* s3,
                                    const char* s4, int i2, const char* s6, const char* s7, int i3);
EXPORT void call_with_many_args(many_arg_callback_t callback)
{
    callback(0, "this", "is", "working", "just", 1, "fine", "or does it?", 2);
}
