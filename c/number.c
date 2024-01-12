/* number.c
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

/*
***
 * assumptions:
 *   bigits are unsigned
 *   uptr is either one or two bigits wide
***
*/

#include "system.h"

/* locally defined functions */
static ptr copy_normalize(ptr tc, const bigit *p, iptr len, IBOOL sign);
static IBOOL abs_big_lt(ptr x, ptr y, iptr xl, iptr yl);
static IBOOL abs_big_eq(ptr x, ptr y, iptr xl, iptr yl);
static ptr big_negate(ptr tc, ptr x);
static ptr big_add_pos(ptr tc, ptr x, ptr y, iptr xl, iptr yl, IBOOL sign);
static ptr big_add_neg(ptr tc, ptr x, ptr y, iptr xl, iptr yl, IBOOL sign);
static ptr big_add(ptr tc, ptr x, ptr y, iptr xl, iptr yl, IBOOL xs, IBOOL ys);
static ptr big_mul(ptr tc, ptr x, ptr y, iptr xl, iptr yl, IBOOL sign);
static void big_short_trunc(ptr tc, ptr x, bigit s, iptr xl, IBOOL qs, IBOOL rs, ptr *q, ptr *r);
static void big_trunc(ptr tc, ptr x, ptr y, iptr xl, iptr yl, IBOOL qs, IBOOL rs, ptr *q, ptr *r);
static INT normalize(bigit *xp, bigit *yp, iptr xl, iptr yl);
static bigit quotient_digit(ptr tc, bigit *xp, bigit *yp, iptr yl);
static bigit qhat(bigit *xp, bigit *yp);
static ptr big_short_gcd(ptr tc, ptr x, bigit y, iptr xl);
static ptr big_gcd(ptr tc, ptr x, ptr y, iptr xl, iptr yl);
static ptr s_big_ash(ptr tc, bigit *xp, iptr xl, IBOOL sign, iptr cnt);
static double big_short_floatify(ptr tc, ptr x, bigit s, iptr xl, IBOOL sign);
static double big_floatify(ptr tc, ptr x, ptr y, iptr xl, iptr yl, IBOOL sign);
static double floatify_normalize(bigit *p, iptr e, IBOOL sign, IBOOL sticky);
static double floatify_ratnum(ptr tc, ptr x);
static ptr big_logbitp(iptr n, ptr x, iptr xl, IBOOL xs);
static ptr big_logbit0(ptr tc, ptr origx, iptr n, ptr x, iptr xl, IBOOL xs);
static ptr big_logbit1(ptr tc, ptr origx, iptr n, ptr x, iptr xl, IBOOL xs);
static ptr big_logtest(ptr x, ptr y, iptr xl, iptr yl, IBOOL xs, IBOOL ys);
static ptr big_logand(ptr tc, ptr x, ptr y, iptr xl, iptr yl, IBOOL xs, IBOOL ys);
static ptr big_logor(ptr tc, ptr x, ptr y, iptr xl, iptr yl, IBOOL xs, IBOOL ys);
static ptr big_logxor(ptr tc, ptr x, ptr y, iptr xl, iptr yl, IBOOL xs, IBOOL ys);

/* use w/o trailing semicolon */
#define PREPARE_BIGNUM(tc,x,l)\
 {if (x == FIX(0) || BIGLEN(x) < (l)) x = S_bignum(tc, (l)*2, 0);}

#define bigit_mask (~(bigit)0)

#define IBIGIT_TO_BIGNUM(tc,B,x,cnt,sign) {\
  ibigit _i_ = x;\
  PREPARE_BIGNUM(tc, B, 1)\
  *cnt = 1;\
  BIGIT(B,0) = (*sign = (_i_ < 0)) ? -_i_ : _i_;\
}

#define UBIGIT_TO_BIGNUM(tc,B,u,cnt) {\
  PREPARE_BIGNUM(tc, B, 1)\
  *cnt = 1;\
  BIGIT(B,0) = u;\
}

#define IBIGITBIGIT_TO_BIGNUM(tc,B,x,cnt,sign) {\
  ibigitbigit _i_ = x; bigitbigit _u_; bigitbigit _b_;\
  PREPARE_BIGNUM(tc, B, 2)\
  _u_ = (*sign = (_i_ < 0)) ? -_i_ : _i_;\
  if ((_b_ = (_u_ & (bigitbigit)bigit_mask)) == _u_) {\
    *cnt = 1;\
    BIGIT(B,0) = (bigit)_u_;\
  } else {\
    *cnt = 2;\
    BIGIT(B,0) = (bigit)(_u_ >> bigit_bits);\
    BIGIT(B,1) = (bigit)_b_;\
  }\
}

#define UBIGITBIGIT_TO_BIGNUM(tc,B,x,cnt) {\
  bigitbigit _u_ = x; bigitbigit _b_;\
  PREPARE_BIGNUM(tc, B, 2)\
  if ((_b_ = (_u_ & (bigitbigit)bigit_mask)) == _u_) {\
    *cnt = 1;\
    BIGIT(B,0) = (bigit)_u_;\
  } else {\
    *cnt = 2;\
    BIGIT(B,0) = (bigit)(_u_ >> bigit_bits);\
    BIGIT(B,1) = (bigit)_b_;\
  }\
}

#define U32_bigits (32 / bigit_bits)

#if (U32_bigits == 1)
#define I32_TO_BIGNUM(tc,B,x,cnt,sign) IBIGIT_TO_BIGNUM(tc,B,x,cnt,sign)
#define U32_TO_BIGNUM(tc,B,x,cnt) UBIGIT_TO_BIGNUM(tc,B,x,cnt)
#endif

#if (U32_bigits == 2)
#define I32_TO_BIGNUM(tc,B,x,cnt,sign) IBIGITBIGIT_TO_BIGNUM(tc,B,x,cnt,sign)
#define U32_TO_BIGNUM(tc,B,x,cnt) UBIGITBIGIT_TO_BIGNUM(tc,B,x,cnt)
#endif

#define U64_bigits (64 / bigit_bits)

#if (U64_bigits == 2)
#define I64_TO_BIGNUM(tc,B,x,cnt,sign) IBIGITBIGIT_TO_BIGNUM(tc,B,x,cnt,sign)
#define U64_TO_BIGNUM(tc,B,x,cnt) UBIGITBIGIT_TO_BIGNUM(tc,B,x,cnt)
#endif

#if (U64_bigits == 4)
#error see v7.4 number.c for U64_TO_BIGNUM w/U64_bigits == 4
#endif

#define ptr_bigits (ptr_bits / bigit_bits)

#if (ptr_bigits == 1)
#define IPTR_TO_BIGNUM(tc,B,x,cnt,sign) IBIGIT_TO_BIGNUM(tc,B,x,cnt,sign)
#define UPTR_TO_BIGNUM(tc,B,x,cnt) UBIGIT_TO_BIGNUM(tc,B,x,cnt)
#endif

#if (ptr_bigits == 2)
#define IPTR_TO_BIGNUM(tc,B,x,cnt,sign) IBIGITBIGIT_TO_BIGNUM(tc,B,x,cnt,sign)
#define UPTR_TO_BIGNUM(tc,B,x,cnt) UBIGITBIGIT_TO_BIGNUM(tc,B,x,cnt)
#endif

#define FIXNUM_TO_BIGNUM(tc,B,p,cnt,sign) IPTR_TO_BIGNUM(tc,B,UNFIX(p),cnt,sign)

ptr S_normalize_bignum(ptr x) {
  uptr n = BIGIT(x, 0); iptr len = BIGLEN(x); IBOOL sign = BIGSIGN(x);
 
#if (ptr_bigits == 1)
  if (len == 1) {
    if (sign) {
      if (n <= -most_negative_fixnum) return FIX(-(iptr)n);
    } else {
      if (n <= most_positive_fixnum) return FIX(n);
    }
  }
#elif (ptr_bigits == 2)
  if (len == 1)
    return sign ? FIX(-(iptr)n) : FIX(n);
  else if (len == 2) {
    n = (n << bigit_bits) | BIGIT(x, 1);
    if (sign) {
     /* avoid -most-negative-fixnum to avoid bogus Sun compiler warning */
      if (n <= most_positive_fixnum+1) return FIX(-(iptr)n);
    } else {
      if (n <= most_positive_fixnum) return FIX(n);
    }
  }
#endif

  return x;
}

static ptr copy_normalize(ptr tc, const bigit *p, iptr len, IBOOL sign) {
  bigit *p1; uptr n; ptr b;

  for (;;) {
    if ((n = *p) != 0)
      break;
    else if (--len == 0)
      return FIX(0);
    else p++;
  }

#if (ptr_bigits == 1)
  if (len == 1) {
    if (sign) {
      if (n <= -most_negative_fixnum) return FIX(-(iptr)n);
    } else {
      if (n <= most_positive_fixnum) return FIX(n);
    }
  }
#elif (ptr_bigits == 2)
  if (len == 1)
    return sign ? FIX(-(iptr)n) : FIX(n);
  else if (len == 2) {
    n = (n << bigit_bits) | *(p+1);
    if (sign) {
     /* avoid -most-negative-fixnum to avoid bogus Sun compiler warning */
      if (n <= most_positive_fixnum+1) return FIX(-(iptr)n);
    } else {
      if (n <= most_positive_fixnum) return FIX(n);
    }
  }
#endif

  b = S_bignum(tc, len, sign);
  for (p1 = &BIGIT(b, 0); len--;) *p1++ = *p++;
  return b;
}

#define RETURN_STRY_OK(expr) { *val = expr; return 1; }
#define RETURN_STRY_ERROR(why) { if (reason) *reason = why; return 0; }

/* -2^(b-1) <= x <= 2^b-1, where b = number of bits in a uptr */
IBOOL Stry_integer_value(ptr x, iptr *val, const char** reason) {
  if (Sfixnump(x)) RETURN_STRY_OK(UNFIX(x))

  if (Sbignump(x)) {
    iptr xl; uptr u;

    if ((xl = BIGLEN(x)) > ptr_bigits) RETURN_STRY_ERROR("~s is out of range")

    u = BIGIT(x,0);

#if (ptr_bigits == 2)
    if (xl == 2) u = (u << bigit_bits) | BIGIT(x,1);
#endif

    if (!BIGSIGN(x)) RETURN_STRY_OK((iptr)u)
    if (u < ((uptr)1 << (ptr_bits - 1))) RETURN_STRY_OK(-(iptr)u)
    if (u > ((uptr)1 << (ptr_bits - 1))) RETURN_STRY_ERROR("~s is out of range")
#if (fixnum_bits > 32)
    RETURN_STRY_OK((iptr)0x8000000000000000)
#else
    RETURN_STRY_OK((iptr)0x80000000)
#endif
  }

  RETURN_STRY_ERROR("~s is not an integer")
}

iptr Sinteger_value(ptr x) {
  iptr result;
  const char* reason;
  if (Stry_integer_value(x, &result, &reason)) return result;
  S_error1("Sinteger_value", reason, x);
}

/* -2^31 <= x <= 2^32-1 */
IBOOL Stry_integer32_value(ptr x, Sint32_t* val, const char** reason) {
#if (fixnum_bits > 32)
  if (Sfixnump(x)) {
    iptr n = UNFIX(x);
    if (n < 0) {
      Sint32_t m = (Sint32_t)n;
      if ((iptr)m == UNFIX(x)) RETURN_STRY_OK(m)
    } else {
      Suint32_t m = (Suint32_t)n;
      if ((uptr)m == (uptr)UNFIX(x)) RETURN_STRY_OK((Sint32_t)m)
    }
    RETURN_STRY_ERROR("~s is out of range")
  }
  if (Sbignump(x)) RETURN_STRY_ERROR("~s is out of range")
#else /* (fixnum_bits > 32) */
  if (Sfixnump(x)) RETURN_STRY_OK(UNFIX(x))

  if (Sbignump(x)) {
    iptr xl; Suint32_t u;

    if ((xl = BIGLEN(x)) > U32_bigits) RETURN_STRY_ERROR("~s is out of range")

    u = BIGIT(x,0);

#if (U32_bigits == 2)
    if (xl == 2) u = (u << bigit_bits) | BIGIT(x,1);
#endif

    if (!BIGSIGN(x)) RETURN_STRY_OK((Sint32_t)u)
    if (u < ((Suint32_t)1 << 31)) RETURN_STRY_OK(-(Sint32_t)u)
    if (u > ((Suint32_t)1 << 31)) RETURN_STRY_ERROR("~s is out of range")
    RETURN_STRY_OK((Sint32_t)0x80000000)
  }
#endif /* (fixnum_bits > 32) */

  RETURN_STRY_ERROR("~s is not an integer")
}

Sint32_t Sinteger32_value(ptr x) {
  Sint32_t result;
  const char* reason;
  if (Stry_integer32_value(x, &result, &reason)) return result;
  S_error1("Sinteger32_value", reason, x);
}

/* -2^63 <= x <= 2^64-1 */
IBOOL Stry_integer64_value(ptr x, Sint64_t *val, const char** reason) {
  if (Sfixnump(x)) RETURN_STRY_OK(UNFIX(x))

  if (Sbignump(x)) {
    iptr xl; Suint64_t u;

    if ((xl = BIGLEN(x)) > U64_bigits) RETURN_STRY_ERROR("~s is out of range")

    u = BIGIT(x,0);

#if (U64_bigits == 2)
    if (xl == 2) u = (u << bigit_bits) | BIGIT(x,1);
#endif

    if (!BIGSIGN(x)) RETURN_STRY_OK((Sint64_t)u)
    if (u < ((Suint64_t)1 << 63)) RETURN_STRY_OK(-(Sint64_t)u)
    if (u > ((Suint64_t)1 << 63)) RETURN_STRY_ERROR("~s is out of range")
    RETURN_STRY_OK((Sint64_t)0x8000000000000000)
  }

  RETURN_STRY_ERROR("~s is not an integer")
}

I64 S_int64_value(char *who, ptr x) {
  Sint64_t result;
  const char* reason;
  if (Stry_integer64_value(x, &result, &reason)) return result;
  S_error1(who, reason, x);
}

Sint64_t Sinteger64_value(ptr x) {
  return S_int64_value("Sinteger64_value", x);
}

IBOOL Stry_unsigned_value(ptr x, uptr* val, const char** reason) {
  iptr tmp;
  if (Stry_integer_value(x, &tmp, reason)) {
    *val = (uptr)tmp;
    return 1;
  }
  return 0;
}

IBOOL Stry_unsigned32_value(ptr x, Suint32_t* val, const char** reason) {
  Sint32_t tmp;
  if (Stry_integer32_value(x, &tmp, reason)) {
    *val = (Suint32_t)tmp;
    return 1;
  }
  return 0;
}

IBOOL Stry_unsigned64_value(ptr x, Suint64_t* val, const char** reason) {
  Sint64_t tmp;
  if (Stry_integer64_value(x, &tmp, reason)) {
    *val = (Suint64_t)tmp;
    return 1;
  }
  return 0;
}

ptr Sunsigned(uptr u) { /* convert arg to Scheme integer */
  if (u <= most_positive_fixnum)
    return FIX(u);
  else {
    ptr x = FIX(0); iptr xl;
    UPTR_TO_BIGNUM(get_thread_context(), x, u, &xl)
    SETBIGLENANDSIGN(x, xl, 0);
    return x;
  }
}

ptr Sinteger(iptr i) { /* convert arg to Scheme integer */
  if (FIXRANGE(i))
    return FIX(i);
  else {
    ptr x = FIX(0); iptr xl; IBOOL xs;
    IPTR_TO_BIGNUM(get_thread_context(), x, i, &xl, &xs)
    SETBIGLENANDSIGN(x, xl, xs);
    return x;
  }
}

ptr Sunsigned32(Suint32_t u) { /* convert arg to Scheme integer */
#if (fixnum_bits > 32)
  return FIX((uptr)u);
#else
  if (u <= most_positive_fixnum)
    return FIX((uptr)u);
  else {
    ptr x = FIX(0); iptr xl;
    U32_TO_BIGNUM(get_thread_context(), x, u, &xl)
    SETBIGLENANDSIGN(x, xl, 0);
    return x;
  }
#endif
}

ptr Sinteger32(Sint32_t i) { /* convert arg to Scheme integer */
#if (fixnum_bits > 32)
  return FIX((iptr)i);
#else
  if (i >= most_negative_fixnum && i <= most_positive_fixnum)
    return FIX((iptr)i);
  else {
    ptr x = FIX(0); iptr xl; IBOOL xs;
    I32_TO_BIGNUM(get_thread_context(), x, i, &xl, &xs)
    SETBIGLENANDSIGN(x, xl, xs);
    return x;
  }
#endif
}

ptr Sunsigned64(Suint64_t u) { /* convert arg to Scheme integer */
  if (u <= most_positive_fixnum)
    return FIX((uptr)u);
  else {
    ptr x = FIX(0); iptr xl;
    U64_TO_BIGNUM(get_thread_context(), x, u, &xl)
    SETBIGLENANDSIGN(x, xl, 0);
    return x;
  }
}

ptr Sinteger64(Sint64_t i) { /* convert arg to Scheme integer */
  if (i >= most_negative_fixnum && i <= most_positive_fixnum)
    return FIX((iptr)i);
  else {
    ptr x = FIX(0); iptr xl; IBOOL xs;
    I64_TO_BIGNUM(get_thread_context(), x, i, &xl, &xs)
    SETBIGLENANDSIGN(x, xl, xs);
    return x;
  }
}

/* extended arithmetic macros: use w/o trailing semicolon */
#define ELSH(n,x,k) { /* undefined when n == 0 */\
  INT _n_ = (INT)(n); bigit _b_ = *(x), _newk_ = _b_>>(bigit_bits-_n_);\
  *(x) = _b_<<_n_ | *(k);\
  *(k) = _newk_;}

#define ERSH(n,x,k) { /* undefined when n == 0 */\
  INT _n_ = (INT)(n); bigit _b_ = *(x), _newk_ = _b_<<(bigit_bits-_n_);\
  *(x) = _b_>>_n_ | *(k);\
  *(k) = _newk_;}

#define ERSH2(n,x,y,k) { /* undefined when n == 0 */\
  INT _n_ = (INT)(n); bigit _b_ = (x), _newk_ = _b_<<(bigit_bits-_n_);\
  *(y) = _b_>>_n_ | *(k);\
  *(k) = _newk_;}

#define EADDC(a1, a2, sum, k) {\
  bigit _tmp1_, _tmp2_, _tmpk_;\
  _tmp1_ = (a1);\
  _tmp2_ = _tmp1_ + (a2);\
  _tmpk_ = _tmp2_ < _tmp1_;\
  _tmp1_ = _tmp2_ + *(k);\
  *k = _tmpk_ + (_tmp1_ < _tmp2_);\
  *sum = _tmp1_;}

#define ESUBC(s1, s2, diff, b) {\
  bigit _tmp1_, _tmp2_, tmpb;\
  _tmp1_ = (s1);\
  _tmp2_ = _tmp1_ - (s2);\
  tmpb = _tmp2_ > _tmp1_;\
  _tmp1_ = _tmp2_ - *(b);\
  *b = tmpb + (_tmp1_ > _tmp2_);\
  *diff = _tmp1_;}

/* bigit x bigit -> bigitbigit */
#define EMUL(m1,m2,a1,low,high) {\
  bigitbigit _tmp_;\
  _tmp_ = (bigitbigit)m1 * m2 + a1;\
  *low = (bigit)(_tmp_ & (bigitbigit)bigit_mask);\
  *high = (bigit)(_tmp_ >> bigit_bits);}

/* bigitbigit / bigit -> bigit */
#define EDIV(high,low,divr,quo,rem) {\
  bigit _tmpr_; bigitbigit _tmp_;\
  _tmp_ = ((bigitbigit)high << bigit_bits) | low;\
  _tmpr_ = divr;\
  *quo = (bigit)(_tmp_ / _tmpr_);\
  *rem = (bigit)(_tmp_ % _tmpr_);}

/*
***
comparison
***
*/

IBOOL S_big_lt(ptr x, ptr y) {
  if (BIGSIGN(x))
    if (BIGSIGN(y))
      return abs_big_lt(y, x, BIGLEN(y), BIGLEN(x)); /* both negative */
    else
      return 1; /* x negative, y positive */
  else
    if (BIGSIGN(y))
      return 0; /* x positive, y negative */
    else
      return abs_big_lt(x, y, BIGLEN(x), BIGLEN(y)); /* both positive */
}

IBOOL S_big_eq(ptr x, ptr y) {
  return (BIGSIGN(x) == BIGSIGN(y)) && abs_big_eq(x, y, BIGLEN(x), BIGLEN(y));
}

static IBOOL abs_big_lt(ptr x, ptr y, iptr xl, iptr yl) {
  if (xl != yl)
    return xl < yl;
  else {
    bigit *xp, *yp;

    for (xp = &BIGIT(x,0), yp = &BIGIT(y,0); xl-- > 0; xp++, yp++)
      if (*xp != *yp) return (*xp < *yp);

    return 0;
  }
}

static IBOOL abs_big_eq(ptr x, ptr y, iptr xl, iptr yl) {
  if (xl != yl)
    return 0;
  else {
    bigit *xp, *yp;

    for (xp = &BIGIT(x,0), yp = &BIGIT(y,0); xl-- > 0; xp++, yp++)
      if (*xp != *yp) return 0;

    return 1;
  }
}

/*
***
addition/subtraction
***
*/

static ptr big_negate(ptr tc, ptr x) {
  return copy_normalize(tc, &BIGIT(x,0),BIGLEN(x),!BIGSIGN(x));
}

ptr S_big_negate(ptr x) {
  return big_negate(get_thread_context(), x);
}

/* assumptions: BIGLEN(x) >= BIGLEN(y) */
static ptr big_add_pos(ptr tc, ptr x, ptr y, iptr xl, iptr yl, IBOOL sign) {
  iptr i;
  bigit *xp, *yp, *zp;
  bigit k = 0;

  PREPARE_BIGNUM(tc, W(tc),xl+1)

  xp = &BIGIT(x,xl-1); yp = &BIGIT(y,yl-1); zp = &BIGIT(W(tc),xl);

  for (i = yl; i-- > 0; )
    EADDC(*xp--, *yp--, zp--, &k)
  for (i = xl - yl; k != 0 && i-- > 0; )
    EADDC(*xp--, 0, zp--, &k)
  for (; i-- > 0; )
    *zp-- = *xp--;

  *zp = k;

  return copy_normalize(tc, zp,xl+1,sign);
}

/* assumptions: x >= y */
static ptr big_add_neg(ptr tc, ptr x, ptr y, iptr xl, iptr yl, IBOOL sign) {
  iptr i;
  bigit *xp, *yp, *zp;
  bigit b = 0;

  PREPARE_BIGNUM(tc, W(tc),xl)

  xp = &BIGIT(x,xl-1); yp = &BIGIT(y,yl-1); zp = &BIGIT(W(tc),xl-1);

  for (i = yl; i-- > 0; )
    ESUBC(*xp--, *yp--, zp--, &b)
  for (i = xl-yl; b != 0 && i-- > 0; )
    ESUBC(*xp--, 0, zp--, &b)
  for (; i-- > 0; )
    *zp-- = *xp--;

  return copy_normalize(tc, zp+1,xl,sign);
}

static ptr big_add(ptr tc, ptr x, ptr y, iptr xl, iptr yl, IBOOL xs, IBOOL ys) {
  if (xs == ys)
    if (xl < yl)
      return big_add_pos(tc, y, x, yl, xl, xs);
    else
      return big_add_pos(tc, x, y, xl, yl, xs);
  else
    if (abs_big_lt(x, y, xl, yl))
      return big_add_neg(tc, y, x, yl, xl, ys);
    else
      return big_add_neg(tc, x, y, xl, yl, xs);
}

/* arguments must be integers, fixnums or bignums */
ptr S_add(ptr x, ptr y) {
  ptr tc = get_thread_context();

  if (Sfixnump(x)) {
    if (Sfixnump(y)) {
      iptr n = UNFIX(x) + UNFIX(y);
      return FIXRANGE(n) ? FIX(n) : Sinteger(n);
    } else {
      iptr xl; IBOOL xs;
      FIXNUM_TO_BIGNUM(tc,X(tc),x,&xl,&xs)
      return big_add(tc, X(tc), y, xl, BIGLEN(y), xs, BIGSIGN(y));
    }
  } else {
    if (Sfixnump(y)) {
      iptr yl; IBOOL ys;
      FIXNUM_TO_BIGNUM(tc,Y(tc),y,&yl,&ys)
      return big_add(tc, x, Y(tc), BIGLEN(x), yl, BIGSIGN(x), ys);
    } else {
      return big_add(tc, x, y, BIGLEN(x), BIGLEN(y), BIGSIGN(x), BIGSIGN(y));
    }
  }
}

/* arguments must be integers, fixnums or bignums */
ptr S_sub(ptr x, ptr y) {
  ptr tc = get_thread_context();

  if (Sfixnump(x)) {
    if (Sfixnump(y)) {
      iptr n = UNFIX(x) - UNFIX(y);
      return FIXRANGE(n) ? FIX(n) : Sinteger(n);
    } else {
      iptr xl; IBOOL xs;
      FIXNUM_TO_BIGNUM(tc,X(tc),x,&xl,&xs)
      return big_add(tc, X(tc), y, xl, BIGLEN(y), xs, !BIGSIGN(y));
    }
  } else {
    if (Sfixnump(y)) {
      iptr yl; IBOOL ys;
      FIXNUM_TO_BIGNUM(tc,Y(tc),y,&yl,&ys)
      return big_add(tc, x, Y(tc), BIGLEN(x), yl, BIGSIGN(x), !ys);
    } else {
      return big_add(tc, x, y, BIGLEN(x), BIGLEN(y), BIGSIGN(x), !BIGSIGN(y));
    }
  }
}

/*
***
multiplication
***
*/

static ptr big_mul(ptr tc, ptr x, ptr y, iptr xl, iptr yl, IBOOL sign) {
  iptr xi, yi;
  bigit *xp, *yp, *zp, *zpa;
  bigit k, k1, prod;

  PREPARE_BIGNUM(tc, W(tc),xl+yl)
  for (xi = xl, zp = &BIGIT(W(tc),xl+yl-1); xi-- > 0; ) *zp-- = 0;

  /* account for nested loop: */
  USE_TRAP_FUEL(tc, xl * yl);

  for (yi=yl,yp= &BIGIT(y,yl-1),zp= &BIGIT(W(tc),xl+yl-1); yi-- > 0; yp--, zp--)
    if (*yp == 0)
      *(zp-xl) = 0;
    else {
      for (xi=xl,k=0,zpa=zp,xp= &BIGIT(x,xl-1); xi-- > 0; xp--,zpa--) {
        EMUL(*xp, *yp, *zpa, &prod, &k1)
        EADDC(prod, 0, zpa, &k)
        k += k1;
      }
      *zpa = k;
    }

  return copy_normalize(tc, &BIGIT(W(tc),0),xl+yl,sign);
}

/* SHORTRANGE is -floor(sqrt(most_positive_fixnum))..floor(sqrt(most_positive_fixnum)).
   We don't use sqrt because it rounds up for fixnum_bits = 61 */
#if (fixnum_bits == 30)
#define SHORTRANGE(x) (-23170 <= (x) && (x) <= 23170)
#elif (fixnum_bits == 61)
#define SHORTRANGE(x) (-0x3FFFFFFF <= (x) && (x) <= 0x3FFFFFFF)
#endif

ptr S_mul(ptr x, ptr y) {
  ptr tc = get_thread_context();

  iptr xl, yl; IBOOL xs, ys;

  if (Sfixnump(x)) {
    if (Sfixnump(y)) {
      iptr xn = UNFIX(x);
      iptr yn = UNFIX(y);
      if (SHORTRANGE(xn) && SHORTRANGE(yn))
        return FIX(xn * yn);
      else {
        FIXNUM_TO_BIGNUM(tc, X(tc),x,&xl,&xs) x = X(tc);
        FIXNUM_TO_BIGNUM(tc, Y(tc),y,&yl,&ys) y = Y(tc);
      }
    } else {
      FIXNUM_TO_BIGNUM(tc,X(tc),x,&xl,&xs) x = X(tc);
      yl = BIGLEN(y); ys = BIGSIGN(y);
    }
  } else {
    if (Sfixnump(y)) {
      xl = BIGLEN(x); xs = BIGSIGN(x);
      FIXNUM_TO_BIGNUM(tc,Y(tc),y,&yl,&ys) y = Y(tc);
    } else {
      xl = BIGLEN(x); xs = BIGSIGN(x);
      yl = BIGLEN(y); ys = BIGSIGN(y);
    }
  }
  return big_mul(tc, x, y, xl, yl, xs ^ ys);
}

/*
***
division
***
*/

/* arguments must be integers (fixnums or bignums), y must be nonzero */
ptr S_div(ptr x, ptr y) {
  ptr g, n, d;
  ptr tc = get_thread_context();

  g = S_gcd(x,y);
  if (Sfixnump(y) ? UNFIX(y) < 0 : BIGSIGN(y)) {
    g = Sfixnump(g) ? Sinteger(-UNFIX(g)) : big_negate(tc, g);
  }

  S_trunc_rem(tc, x, g, &n, (ptr *)NULL);
  S_trunc_rem(tc, y, g, &d, (ptr *)NULL);

  return S_rational(n, d);
}

ptr S_trunc(ptr x, ptr y) {
  ptr q;
  S_trunc_rem(get_thread_context(), x, y, &q, (ptr *)NULL);
  return q;
}

ptr S_rem(ptr x, ptr y) {
  ptr r;
  S_trunc_rem(get_thread_context(), x, y, (ptr *)NULL, &r);
  return r;
}

/* arguments must be integers (fixnums or bignums), y must be nonzero */
void S_trunc_rem(ptr tc, ptr origx, ptr y, ptr *q, ptr *r) {
  iptr xl, yl; IBOOL xs, ys; ptr x = origx;

  if (Sfixnump(x)) {
    if (Sfixnump(y)) {
      if (x == FIX(most_negative_fixnum) && y == FIX(-1)) {
        iptr m = most_negative_fixnum /* pull out to avoid bogus Sun C warning */;
        if (q != NULL) *q = Sinteger(-m);
        if (r != NULL) *r = FIX(0);
        return;
      } else {
        if (q != NULL) *q = FIX((iptr)x / (iptr)y);
        if (r != NULL) *r = (ptr)((iptr)x % (iptr)y);
        return;
      }
    } else {
      FIXNUM_TO_BIGNUM(tc, X(tc),x,&xl,&xs) x = X(tc);
      yl = BIGLEN(y); ys = BIGSIGN(y);
    }
  } else {
    if (Sfixnump(y)) {
      xl = BIGLEN(x); xs = BIGSIGN(x);
      FIXNUM_TO_BIGNUM(tc, Y(tc),y,&yl,&ys) y = Y(tc);
    } else {
      xl = BIGLEN(x); xs = BIGSIGN(x);
      yl = BIGLEN(y); ys = BIGSIGN(y);
    }
  }

  if (xl < yl) {
    if (q != (ptr *)NULL) *q = FIX(0);
    if (r != (ptr *)NULL) *r = origx;
  } else if (yl == 1) /* must have two bigits for full algorithm */
    big_short_trunc(tc, x, BIGIT(y,0), xl, xs^ys, xs, q, r);
  else
    big_trunc(tc, x, y, xl, yl, xs^ys, xs, q, r);
}

/* sparc C compiler barfs w/o full declaration */
static void big_short_trunc(ptr tc, ptr x, bigit s, iptr xl, IBOOL qs, IBOOL rs, ptr *q, ptr *r) {
  iptr i;
  bigit *xp, *zp;
  bigit k;

  PREPARE_BIGNUM(tc, W(tc),xl)

  for (i = xl, k = 0, xp = &BIGIT(x,0), zp = &BIGIT(W(tc),0); i-- > 0; )
    EDIV(k, *xp++, s, zp++, &k)

  if (q != (ptr *)NULL) *q = copy_normalize(tc, &BIGIT(W(tc),0),xl,qs);
  if (r != (ptr *)NULL) *r = copy_normalize(tc, &k,1,rs);
}

static void big_trunc(ptr tc, ptr x, ptr y, iptr xl, iptr yl, IBOOL qs, IBOOL rs, ptr *q, ptr *r) {
  iptr i;
  bigit *p, *xp, *yp;
  iptr m = xl-yl+1;
  INT d;
  bigit k;

  PREPARE_BIGNUM(tc, U(tc), xl+1)
  for (i = xl, xp = &BIGIT(U(tc),xl+1), p = &BIGIT(x,xl); i-- > 0;) *--xp = *--p;
  *--xp = 0;

  PREPARE_BIGNUM(tc, V(tc), yl)
  for (i = yl, yp = &BIGIT(V(tc),yl), p = &BIGIT(y,yl); i-- > 0;) *--yp = *--p;

  d = normalize(xp, yp, xl, yl);

  if (q == (ptr *)NULL) {
    for (i = m; i-- > 0 ; xp++) (void) quotient_digit(tc, xp, yp, yl);
  } else {
    PREPARE_BIGNUM(tc, W(tc),m)
    p = &BIGIT(W(tc),0);
    for (i = m; i-- > 0 ; xp++) *p++ = quotient_digit(tc, xp, yp, yl);
    *q = copy_normalize(tc, &BIGIT(W(tc),0),m,qs);
  }

  if (r != (ptr *)NULL) {
   /* unnormalize the remainder */
    if (d != 0) {
      for (i = yl, p = xp, k = 0; i-- > 0; p++) ERSH(d,p,&k)
    }
    *r = copy_normalize(tc, xp, yl, rs);
  }
}

static INT normalize(bigit *xp, bigit *yp, iptr xl, iptr yl) {
  iptr i;
  bigit *p, k, b;
  INT shft;

  for (shft = bigit_bits-1, b = *yp; b >>= 1; shft -= 1);

  if (shft != 0) {
    for (i = yl, p = yp+yl-1, k = 0; i-- > 0; p--) ELSH(shft,p,&k)
    for (i = xl, p = xp+xl, k = 0; i-- > 0; p--) ELSH(shft,p,&k)
    *xp = k;
  }

  return shft;
}

static bigit quotient_digit(ptr tc, bigit *xp, bigit *yp, iptr yl) {
  bigit *p1, *p2, q, k, b, prod;
  iptr i;

  /* this function is called in loops, so use fuel every time */
  USE_TRAP_FUEL(tc, yl);

  q = qhat(xp, yp);

  for (i = yl, p1 = xp+yl, p2 = yp+yl-1, k = 0, b = 0; i-- > 0; p1--, p2--) {
    EMUL(*p2, q, k, &prod, &k)
    ESUBC(*p1, prod, p1, &b)
  }

  ESUBC(*p1, k, p1, &b)

  if (b != 0) {
    for (i = yl, p1 = xp+yl, p2 = yp+yl-1, k = 0; i-- > 0; p1--, p2--) {
      EADDC(*p2, *p1, p1, &k)
    }
    EADDC(0,*p1,p1,&k)
    q--;
  }

  return q;
}

static bigit qhat(bigit *xp, bigit *yp) {
  bigit q, r, high, low, k;

  k = 0;

  if (*xp == *yp) {
    q = bigit_mask;
    EADDC(*(xp+1), *yp, &r, &k)
  } else {
    EDIV(*xp, *(xp+1), *yp, &q, &r)
  }

  for (; k == 0; q--) {
    EMUL(*(yp+1), q, 0, &low, &high)
    if (high < r || (high == r && low <= *(xp+2))) break;
    EADDC(r, *yp, &r, &k)
  }

  return q;
}

/*
***
gcd
***
*/

static ptr uptr_gcd(uptr x, uptr y) {
  uptr r;

  while (y != 0) {
    r = x % y;
    x = y;
    y = r;
  }

  return Sunsigned(x);
}

/* sparc C compiler barfs w/o full declaration */
static ptr big_short_gcd(ptr tc, ptr x, bigit y, iptr xl) {
  bigit *xp;
  iptr i;
  bigit r, q;

  if (y == 0) return BIGSIGN(x) ? big_negate(tc, x) : x;

  USE_TRAP_FUEL(tc, xl);

  for (i = xl, r = 0, xp = &BIGIT(x,0); i-- > 0; )
    EDIV(r, *xp++, y, &q, &r)

  return uptr_gcd((uptr)y,(uptr)r);
}

static ptr big_gcd(ptr tc, ptr x, ptr y, iptr xl, iptr yl) {
  iptr i;
  INT shft, asc;
  bigit *p, *xp, *yp, k, b;

 /* Copy x to scratch bignum, with a leading zero */
  PREPARE_BIGNUM(tc, U(tc),xl+1)
  xp = &BIGIT(U(tc),xl+1);
  for (i = xl, p = &BIGIT(x,xl); i-- > 0; ) *--xp = *--p;
  *--xp = 0;                /* leave xp pointing at leading 0-bigit */

 /* Copy y to scratch bignum, with a leading zero */
  PREPARE_BIGNUM(tc, V(tc),yl+1)
  yp = &BIGIT(V(tc),yl+1);
  for (i = yl, p = &BIGIT(y,yl); i-- > 0; ) *--yp = *--p;
  *(yp-1) = 0;        /* leave yp pointing just after leading 0-bigit */

 /* initialize aggregate shift count (asc) */
  asc = 0;

  for (;;) {
   /* find number of leading zeros in first bigit of y */
    for (shft = bigit_bits - 1, b = *yp; b >>= 1; shft--);

   /* find directed distance to shift and new asc */
    if (asc+shft >= bigit_bits) shft -= bigit_bits;
    asc += shft;

    /* account for nested loops: */
    USE_TRAP_FUEL(tc, xl + yl);

   /* shift left or right; adjust lengths, xp and yp */
    if (shft < 0) {                /* shift right */
      for (i = yl--, p = yp++, k = 0; i-- > 0; p++) ERSH(-shft,p,&k)
      for (i = xl+1, p = xp, k = 0; i-- > 0; p++) ERSH(-shft,p,&k)
     /* don't need two leading zeros */
      if (*(xp+1) == 0) xp++, xl--;
     /* we have shrunk y, so test the length here */
      if (yl == 1) break;
    } else if (shft > 0) {         /* left shift */
      for (i=yl, p=yp+yl-1, k=0; i-- > 0; p--) ELSH(shft,p,&k)
      for (i=xl+1, p=xp+xl, k=0; i-- > 0; p--) ELSH(shft,p,&k)
    }

   /* destructive remainder x = x rem y */
    for (i = xl-yl+1; i-- > 0; xp++) (void) quotient_digit(tc, xp, yp, yl);

   /* strip leading zero bigits.  remainder is at most yl bigits long */
    for (i = yl ; *xp == 0 && i > 0; xp++, i--);

   /* swap x and y */
    p  = yp;        /* leading bigit of y */
    yp = xp;        /* remainder */
    xp = p-1;       /* new dividend w/leading zero */
    xl = yl;
    yl = i;

   /* may have lopped off all or all but one bigit of the remainder */
    if (yl <= 1) break;
  }

 /* point xp after the leading zero */
  xp += 1;

 /* if y is already zero, shift x and leave */
  if (yl == 0) {
    if (asc != 0) {
      for (i = xl, p = xp, k = 0; i-- > 0; p++) ERSH(asc,p,&k)
    }
    return copy_normalize(tc, xp,xl,0);
  } else {
    bigit d, r;

    d = *yp;
    for (r = 0; xl-- > 0; xp++) EDIV(r, *xp, d, xp, &r)
    return uptr_gcd((uptr)(d>>asc), (uptr)(r>>asc));
  }
}

ptr S_gcd(ptr x, ptr y) {
  ptr tc = get_thread_context();
  iptr xl, yl; IBOOL xs, ys;

  if (Sfixnump(x))
    if (Sfixnump(y)) {
      iptr xi = UNFIX(x), yi = UNFIX(y);
      if (xi < 0) xi = -xi;
      if (yi < 0) yi = -yi;
      return xi >= yi ?
               uptr_gcd((uptr)xi, (uptr)yi) :
               uptr_gcd((uptr)yi, (uptr)xi);
    } else {
      FIXNUM_TO_BIGNUM(tc, X(tc),x,&xl,&xs) x = X(tc);
      yl = BIGLEN(y); ys = BIGSIGN(y);
    }
  else
    if (Sfixnump(y)) {
      xl = BIGLEN(x); xs = BIGSIGN(x);
      FIXNUM_TO_BIGNUM(tc, Y(tc),y,&yl,&ys) y = Y(tc);
    } else {
      xl = BIGLEN(x); xs = BIGSIGN(x);
      yl = BIGLEN(y); ys = BIGSIGN(y);
    }

  if (xl == 1)
    if (yl == 1) {
      uptr xu = BIGIT(x,0), yu = BIGIT(y,0);
      return xu >= yu ? uptr_gcd(xu, yu) : uptr_gcd(yu, xu);
    } else
      return big_short_gcd(tc, y, BIGIT(x,0), yl);
  else
    if (yl == 1)
      return big_short_gcd(tc, x, BIGIT(y,0), xl);
    else
      if (abs_big_lt(x, y, xl, yl))
        return big_gcd(tc, y, x, yl, xl);
      else
        return big_gcd(tc, x, y, xl, yl);
}

/*
***
floating-point operations
***
*/

#ifdef IEEE_DOUBLE
/* exponent stored + 1024, hidden bit to left of decimal point */
# define bias 1023
# define bitstoright 52
# define m1mask 0xf
# ifdef WIN32
#  define hidden_bit 0x10000000000000
# else
#  define hidden_bit 0x10000000000000ULL
# endif
# ifdef LITTLE_ENDIAN_IEEE_DOUBLE
struct dblflt {
    UINT m4: 16;
    UINT m3: 16;
    UINT m2: 16;
    UINT m1: 4;
    UINT e: 11;
    UINT sign: 1;
};
# else
struct dblflt {
    UINT sign: 1;
    UINT e: 11;
    UINT m1: 4;
    UINT m2: 16;
    UINT m3: 16;
    UINT m4: 16;
};
# endif
#endif

double S_random_double(U32 m1, U32 m2, U32 m3, U32 m4, double scale) {
 /* helper for s_fldouble in prim5.c */
  union dxunion {
    double d;
    struct dblflt x;
  } dx;

  dx.x.m1 = m1 >> 16 & m1mask;
  dx.x.m2 = m2 >> 16;
  dx.x.m3 = m3 >> 16;
  dx.x.m4 = m4 >> 16;
  dx.x.sign = 0;
  dx.x.e = bias;
  return (dx.d - 1.0) * scale;
}

/* number of quotient bigits to guarantee at least 64 bits */
/* +2 since first bigit may be zero and second may not be full */
#define enough (64 / bigit_bits + 2)

/* sparc C compiler barfs w/o full declaration */
static double big_short_floatify(ptr tc, ptr x, bigit s, iptr xl, IBOOL sign) {
  iptr i;
  bigit *xp, *zp, k;

  PREPARE_BIGNUM(tc, W(tc),enough+1)

 /* compute only as much of quotient as we need */
  for (i = 0, k = 0, xp = &BIGIT(x,0), zp = &BIGIT(W(tc),0); i < enough; i++)
    if (i < xl)
      EDIV(k, *xp++, s, zp++, &k)
    else
      EDIV(k, 0, s, zp++, &k)

 /* then see if there's a bit set somewhere beyond */
  while (k == 0 && i++ < xl) k = *xp++;

  return floatify_normalize(&BIGIT(W(tc),0), xl*bigit_bits, sign, k != 0);
}

static double big_floatify(ptr tc, ptr x, ptr y, iptr xl, iptr yl, IBOOL sign) {
  iptr i, ul;
  bigit *p, *xp, *yp, k;

 /* copy x to U(tc), scaling with added zero bigits as necessary */
  ul = xl < yl + enough-1 ? yl + enough-1 : xl;
  PREPARE_BIGNUM(tc, U(tc), ul+1)
  for (i = ul - xl, xp = &BIGIT(U(tc),ul+1); i-- > 0;) *--xp = 0;
  for (i = xl, p = &BIGIT(x,xl); i-- > 0;) *--xp = *--p;
  *--xp = 0;

 /* copy y to V(tc) */
  PREPARE_BIGNUM(tc, V(tc), yl)
  for (i = yl, yp = &BIGIT(V(tc),yl), p = &BIGIT(y,yl); i-- > 0;) *--yp = *--p;

  (void) normalize(xp, yp, ul, yl);

  PREPARE_BIGNUM(tc, W(tc),4)
  p = &BIGIT(W(tc),0);

 /* compute 'enough' bigits of the quotient */
  for (i = enough; i-- > 0; xp++) *p++ = quotient_digit(tc, xp, yp, yl);

 /* set k if remainder is nonzero */
  k = 0;
  for (i = ul + 1, xp = &BIGIT(U(tc),ul); k == 0 && i-- > 0; xp--) k = *xp;

  return floatify_normalize(&BIGIT(W(tc),0), (xl-yl+1)*bigit_bits, sign, k != 0);
}

/* come in with exactly 'enough' bigits */
static double floatify_normalize(bigit *p, iptr e, IBOOL sign, IBOOL sticky) {
  /* *p: first bigit; e: exponent; sign: sign; sticky: sticky bit */
  union dxunion {
    double d;
    struct dblflt x;
  } dx;
  bigit mhigh;
  U64 mlow;
  IBOOL cutbit = 0;
  INT n;
 
  /* shift in what we need, plus at least one bit */
  mhigh = 0; mlow = 0; n = enough;
  while (mhigh == 0 && mlow < hidden_bit * 2) {
    mhigh = (bigit)(mlow >> (64-bigit_bits));
    mlow = (mlow << bigit_bits) | *p++;
    n -= 1;
    e -= bigit_bits;
  }

  /* back up to align high bit on hidden bit, setting cut bit to last loser */
  do {
    sticky = sticky || cutbit;
    cutbit = (bigit)(mlow & 1);
    mlow = (U64)mhigh << 63 | mlow >> 1;
    mhigh = mhigh >> 1;
    e = e + 1;
  } while (mhigh != 0 || mlow >= hidden_bit * 2);

  e = e + bitstoright + bias;

  /* back up further if denormalized */
  if (e <= 0) {
    for (;;) {
      sticky = sticky || cutbit;
      cutbit = (bigit)(mlow & 1);
      mlow = mlow >> 1;
      if (e == 0 || mlow == 0) break;
      e = e + 1;
    }
  }

  if (e < 0) {
    e = 0; /* NB: e < 0 => mlow == 0 */
  } else {
    /* round up if necessary */
    if (cutbit) {
      IBOOL round;
      /* cutbit = 1 => at least half way to next number.  round up if odd or
         if there are any bits set to the right of cutbit */
      round = (mlow & 1) || sticky;
      while (!round && n-- > 0) round = *p++ != 0;
      if (round) {
        mlow += 1;
        if (e == 0 && mlow == hidden_bit) {
          e = 1; /* squeaking into lowest normalized spot */
        } else if (mlow == hidden_bit * 2) {
          /* don't bother with mlow = mlow >> 1 since hidden bit and up are ignored after this */
          e += 1;
        }
      }
    }

    if (e > 2046) { /* infinity */
      e = 2047;
      mlow = 0;
    }
  }

  /* fill in the fields */
  dx.x.sign = sign;
  dx.x.e = (UINT)e;
  dx.x.m1 = (UINT)(mlow >> 48 & m1mask);
  dx.x.m2 = (UINT)(mlow >> 32 & 0xffff);
  dx.x.m3 = (UINT)(mlow >> 16 & 0xffff);
  dx.x.m4 = (UINT)(mlow & 0xffff);

  return dx.d;
}

static double floatify_ratnum(ptr tc, ptr p) {
  ptr x, y; iptr xl, yl; IBOOL xs;

  x = RATNUM(p); y = RATDEN(p);

  if (fixnum_bits <= bitstoright && Sfixnump(x) && Sfixnump(y))
    return (double)UNFIX(x) / (double)UNFIX(y);

 /* make sure we are dealing with bignums */
  if (Sfixnump(x)) {
    FIXNUM_TO_BIGNUM(tc,X(tc),x,&xl,&xs)
    x = X(tc);
  } else {
    xl = BIGLEN(x);
    xs = BIGSIGN(x);
  }

  if (Sfixnump(y)) {
    IBOOL ys;
    FIXNUM_TO_BIGNUM(tc,Y(tc),y,&yl,&ys)
    y = Y(tc);
  } else {
    yl = BIGLEN(y);
  }

 /* need second bignum to be at least two bigits for full algorithm */
  if (yl == 1)
    return big_short_floatify(tc, x, BIGIT(y,0), xl, xs);
  else
    return big_floatify(tc, x, y, xl, yl, xs);
}

double S_floatify(ptr x) {
  ptr tc = get_thread_context();

  if (Sflonump(x)) return FLODAT(x);
  else if (Sfixnump(x)) return (double)UNFIX(x);
  else if (Sbignump(x)) return big_short_floatify(tc, x, 1, BIGLEN(x), BIGSIGN(x));
  else if (Sratnump(x)) return floatify_ratnum(tc, x);
  else S_error1("", "~s is not a real number", x);

  return 0.0 /* not reached */;
}

#ifdef IEEE_DOUBLE
ptr S_decode_float(double d) {
  union dxunion {
    double d;
    struct dblflt x;
  } dx;
  IBOOL s; INT e; U64 m;
  ptr x, p;

 /* pick apart the fields */
  dx.d = d;
  s = dx.x.sign;
  e = dx.x.e;
  m = (U64)dx.x.m1 << 48 | (U64)dx.x.m2 << 32 | (U64)dx.x.m3 << 16 | (U64)dx.x.m4;
  if (e != 0) {
    e = e - bias - bitstoright;
    m |= hidden_bit;
  } else if (m != 0) {
   /* denormalized */
    e = 1 - bias - bitstoright;
  }

 /* compute significand */
  if (m <= most_positive_fixnum)
    x = FIX((uptr)m);
  else {
    iptr xl;
    x = FIX(0);
    U64_TO_BIGNUM(get_thread_context(), x, m, &xl)
    SETBIGLENANDSIGN(x, xl, 0);
  }

 /* construct return vector */
  p = S_vector(3);
  INITVECTIT(p,0) = x;
  INITVECTIT(p, 1) = FIX(e);
  INITVECTIT(p, 2) = s ? FIX(-1) : FIX(1);
  return p;
}
#endif

/*
***
logical operations
***
*/

static ptr s_big_ash(ptr tc, bigit *xp, iptr xl, IBOOL sign, iptr cnt) {
  iptr i;
  bigit *p1, *p2, k;

  if (cnt < 0) { /* shift to the right */
    iptr whole_bigits;
   
    /* decrement length to shift by whole bigits */
    if ((xl -= (whole_bigits = (cnt = -cnt) / bigit_bits)) <= 0) return sign ? FIX(-1) : FIX(0);
    cnt -= whole_bigits * bigit_bits;

    /* shift by remaining count to scratch bignum, tracking bits shifted off to the right;
       prepare a bignum one large than probably needed, in case we have to deal with a
       carry bit when rounding down for a negative number */
    PREPARE_BIGNUM(tc, W(tc),xl+1)
    p1 = &BIGIT(W(tc), 0);
    p2 = xp;
    k = 0;
    i = xl;
    if (cnt == 0) {
      do { *p1++ = *p2++; } while (--i > 0);
    } else {
      do { ERSH2(cnt,*p2,p1,&k); p1++; p2++; } while (--i > 0);
    }

    if (sign) {
      if (k == 0) {
        /* check for one bits in the shifted-off bigits, looking */
        /* from both ends in an attempt to get out more quickly for what */
        /* seem like the most likely patterns.  of course, there might */
        /* be no one bits (in which case this won't help) or they might be */
        /* only in the middle (in which case this will be slower) */
        p2 = (p1 = xp + xl) + whole_bigits;
        while (p1 != p2) {
          if ((k = *p1++) || p1 == p2 || (k = *--p2)) break;
        }
      }

      /* round down negative numbers by incrementing the magnitude if any
         one bits were shifted off to the right */
      if (k) {
        p1 = &BIGIT(W(tc), xl - 1);
        for (i = xl, k = 1; k != 0 && i-- > 0; p1 -= 1)
          EADDC(0, *p1, p1, &k)
        if (k) {
          /* add carry bit back; we prepared a large enough bignum,
             and since of all the middle are zero, we don't have to reshift */
          BIGIT(W(tc), xl) = 0;
          BIGIT(W(tc), 0) = 1;
          xl++;
        }
      }
    }

    return copy_normalize(tc, &BIGIT(W(tc), 0), xl, sign);
  } else { /* shift to the left */
    iptr xlplus, newxl;

   /* determine how many zero bigits to add on the end */
    xlplus = 0;
    while (cnt >= bigit_bits) {
      xlplus += 1;
      cnt -= bigit_bits;
    }

   /* maximum total length includes +1 for shift out of top bigit */
    newxl = xl + xlplus + 1;

    PREPARE_BIGNUM(tc, W(tc),newxl)

   /* fill bigits to right with zero */
    for (i = xlplus, p1 = &BIGIT(W(tc), newxl); i-- > 0; ) *--p1 = 0;

   /* shift to the left */
    for (i = xl, p2 = xp + xl, k = 0; i-- > 0; ) {
      *--p1 = *--p2;
      if (cnt != 0) ELSH(cnt, p1, &k);
    }
    *--p1 = k;

    return copy_normalize(tc, p1, newxl, sign);
  }
}

/* x is a bignum or fixnum, n is a fixnum */
ptr S_ash(ptr x, ptr n) {
  ptr tc = get_thread_context();
  iptr cnt = UNFIX(n);

  if (Sfixnump(x)) {
   /* when we get here with a fixnum, we've done what we could in Scheme
      code to avoid use of bignums, so go straight to it.  it's difficult to
      do much here anyway since semantics of signed >> are undefined in C */
    iptr xl; IBOOL xs;

    FIXNUM_TO_BIGNUM(tc,X(tc),x,&xl,&xs);
    return s_big_ash(tc, &BIGIT(X(tc),0), xl, xs, cnt);
  } else
    return s_big_ash(tc, &BIGIT(x,0), BIGLEN(x), BIGSIGN(x), cnt);
}

/* x is a bignum */
ptr S_integer_length(ptr x) {
  iptr a; bigit b;

  if (BIGSIGN(x)) x = S_sub(FIX(-1), x);

  b = BIGIT(x, 0);
  a = 1;
  while (b >>= 1) a += 1;

  return S_add(S_mul(FIX(BIGLEN(x) - 1), FIX(bigit_bits)), FIX(a));
}

/* x is a bignum */
ptr S_big_first_bit_set(ptr x) {
  iptr xl = BIGLEN(x);
  bigit *xp = &BIGIT(x, xl);
  bigit b;
  iptr zbigits = 0;
  INT zbits = 0;

 /* first bit set in signed magnitude is same as for two's complement,
    since if x ends with k zeros, ~x+1 also ends with k zeros. */
  while ((b = *--xp) == 0) zbigits += 1;
  while ((b & 1) == 0) { zbits += 1; b >>= 1; }
  return S_add(S_mul(FIX(zbigits), FIX(bigit_bits)), FIX(zbits));
}

/* assumes fxstart - fxend > 0 */
ptr S_big_positive_bit_field(ptr x, ptr fxstart, ptr fxend) {
  ptr tc = get_thread_context();
  bigit *xp = &BIGIT(x, 0);
  iptr start = UNFIX(fxstart), end = UNFIX(fxend), xl = BIGLEN(x);
  iptr wl, bigits, i;
  bigit *p1, *p2, k;
  uptr bits, maskbits;

 /* shift by whole bigits by decrementing length */
  bigits = (unsigned)start / bigit_bits;
  xl -= bigits;
  if (xl <= 0) return FIX(0);
  bits = (unsigned)bigits * bigit_bits;
  start -= bits;
  end -= bits;

 /* compute maximum length of result */
  bigits = (unsigned)end / bigit_bits;
  if (xl <= bigits) {
    wl = xl;
    maskbits = 0;
  } else {
    end -= (unsigned)bigits * bigit_bits;
    if (end != 0) {
      wl = bigits + 1;
      maskbits = bigit_bits - end;
    } else {
      wl = bigits;
      maskbits = 0;
    }
  }

 /* copy to scratch bignum */
  PREPARE_BIGNUM(tc, W(tc),wl)
  p1 = &BIGIT(W(tc), wl);
  for (i = wl, p2 = xp + xl; i-- > 0; ) *--p1 = *--p2;

 /* kill unwanted bits at the top of the first bigit */
  if (maskbits != 0) *p1 = (*p1 << maskbits) >> maskbits;

 /* shift by remaining start bits */
  if (start != 0) {
    k = 0;
    for (i = wl; i > 0; i -= 1, p1 += 1) ERSH(start,p1,&k)
  }

  return copy_normalize(tc, &BIGIT(W(tc), 0), wl, 0);
}

/* returns a lower bound on the number of trailing 0 bits in the
   binary representation; the result plus bigit_bits-1 is an
   upper bound: */
ptr S_big_trailing_zero_bits(ptr x) {
  bigit *xp = &BIGIT(x, 0);
  iptr xl = BIGLEN(x), i;

  for (i = xl; i-- > 0; ) {
    if (xp[i] != 0)
      break;
  }

  i = (xl - 1) - i;
  i *= bigit_bits;

  return FIX(i);
}

/* logical operations simulate two's complement operations using the
   following general strategy:

   1. break into cases based on signs of operands

   2. convert negative operands to two's complement

   3. operate

   4. convert negative results to two's complement and set sign bit.
      sign of result is known based on signs of operands

   simplifications are made where possible to reduce number of operations.

   # = 2's complement; #x = ~x + 1 = ~(x - 1) if x > 0
*/

ptr S_logand(ptr x, ptr y) {
  ptr tc = get_thread_context();

  if (Sfixnump(x)) {
    if (Sfixnump(y)) {
      return (ptr)((iptr)x & (iptr)y);
    } else {
      iptr xl; IBOOL xs;
      FIXNUM_TO_BIGNUM(tc,X(tc),x,&xl,&xs)
      return big_logand(tc, y, X(tc), BIGLEN(y), xl, BIGSIGN(y), xs);
    }
  } else {
    if (Sfixnump(y)) {
      iptr yl; IBOOL ys;
      FIXNUM_TO_BIGNUM(tc,Y(tc),y,&yl,&ys)
      return big_logand(tc, x, Y(tc), BIGLEN(x), yl, BIGSIGN(x), ys);
    } else {
      if (BIGLEN(x) >= BIGLEN(y))
        return big_logand(tc, x, y, BIGLEN(x), BIGLEN(y), BIGSIGN(x), BIGSIGN(y));
      else
        return big_logand(tc, y, x, BIGLEN(y), BIGLEN(x), BIGSIGN(y), BIGSIGN(x));
    }
  }
}

/* logand on signed-magnitude bignums
   # = 2's complement; #x = ~x + 1 = ~(x - 1) if x > 0
     s&(x,y)   = x&y                       know result >= 0
     s&(x,-y)  = x&#y                      know result >= 0
               = x&~(y-1)
     s&(-x,y)  = s&(y,-x)
     s&(-x,-y) = -(#(#x&#y))               know result < 0
               = -(~(~(x-1)&~(y-1))+1)
               = -(((x-1)|(y-1))+1)        de morgan's law
*/

/* assumes xl >= yl */
static ptr big_logand(ptr tc, ptr x, ptr y, iptr xl, iptr yl, IBOOL xs, IBOOL ys) {
  iptr i;
  bigit *xp, *yp, *zp;

  if (xs == 0) {
    if (ys == 0) {
      PREPARE_BIGNUM(tc, W(tc),yl);
      xp = &BIGIT(x,xl); yp = &BIGIT(y,yl); zp = &BIGIT(W(tc),yl);
      for (i = yl; i > 0; i -= 1) *--zp = *--xp & *--yp;
      return copy_normalize(tc, zp, yl, 0);
    } else {
      bigit yb;

      PREPARE_BIGNUM(tc, W(tc),xl);
      xp = &BIGIT(x,xl); yp = &BIGIT(y,yl); zp = &BIGIT(W(tc),xl);
      yb = 1;
      for (i = yl; i > 0; i -= 1) {
        bigit t1 = *--yp, t2 = t1 - yb;
        yb = t2 > t1;
        *--zp = *--xp & ~t2;
      }
     /* yb must be 0, since high-order bigit >= 1.  effectively, this
        means ~t2 would be all 1's from here on out. */
      for (i = xl - yl; i > 0; i -= 1) *--zp = *--xp;
      return copy_normalize(tc, zp, xl, 0);
    }
  } else {
    if (ys == 0) {
      bigit xb;

      PREPARE_BIGNUM(tc, W(tc),yl);
      xp = &BIGIT(x,xl); yp = &BIGIT(y,yl); zp = &BIGIT(W(tc),yl);
      xb = 1;
      for (i = yl; i > 0; i -= 1) {
        bigit t1 = *--xp, t2 = t1 - xb;
        xb = t2 > t1;
        *--zp = *--yp & ~t2;
      }
      return copy_normalize(tc, zp, yl, 0);
    } else {
      bigit xb, yb, k;

      PREPARE_BIGNUM(tc, W(tc),xl+1);
      xp = &BIGIT(x,xl); yp = &BIGIT(y,yl); zp = &BIGIT(W(tc),xl+1);
      k = yb = xb = 1;
      for (i = yl; i > 0; i -= 1) {
        bigit x1, x2, y1, y2, z1, z2;
        x1 = *--xp; x2 = x1 - xb; xb = x2 > x1;
        y1 = *--yp; y2 = y1 - yb; yb = y2 > y1;
        z1 = x2 | y2;
        z2 = z1 + k; k = z2 < z1;
        *--zp = z2;
      }
      for (i = xl - yl; i > 0; i -= 1) {
        bigit x1, x2, z1, z2;
        x1 = *--xp; x2 = x1 - xb; xb = x2 > x1;
        z1 = x2;
        z2 = z1 + k; k = z2 < z1;
        *--zp = z2;
      }
      *--zp = k;
      return copy_normalize(tc, zp, xl+1, 1);
    }
  }
}

/* logtest is like logand but returns a boolean value */

ptr S_logtest(ptr x, ptr y) {
  ptr tc = get_thread_context();

  if (Sfixnump(x)) {
    if (Sfixnump(y)) {
      return Sboolean((iptr)x & (iptr)y);
    } else {
      iptr xl; IBOOL xs;
      FIXNUM_TO_BIGNUM(tc,X(tc),x,&xl,&xs)
      return big_logtest(y, X(tc), BIGLEN(y), xl, BIGSIGN(y), xs);
    }
  } else {
    if (Sfixnump(y)) {
      iptr yl; IBOOL ys;
      FIXNUM_TO_BIGNUM(tc,Y(tc),y,&yl,&ys)
      return big_logtest(x, Y(tc), BIGLEN(x), yl, BIGSIGN(x), ys);
    } else {
      if (BIGLEN(x) >= BIGLEN(y))
        return big_logtest(x, y, BIGLEN(x), BIGLEN(y), BIGSIGN(x), BIGSIGN(y));
      else
        return big_logtest(y, x, BIGLEN(y), BIGLEN(x), BIGSIGN(y), BIGSIGN(x));
    }
  }
}

/* essentially the same logic as big_logand, but just produces true iff
   logand would return a nonzero value */

/* assumes xl >= yl */
static ptr big_logtest(ptr x, ptr y, iptr xl, iptr yl, IBOOL xs, IBOOL ys) {
  iptr i;
  bigit *xp, *yp;

  if (xs == 0) {
    if (ys == 0) {
      xp = &BIGIT(x,xl); yp = &BIGIT(y,yl);
      for (i = yl; i > 0; i -= 1) if (*--xp & *--yp) return Strue;
      return Sfalse;
    } else {
      bigit yb;

      if (xl > yl) return Strue;
      xp = &BIGIT(x,xl); yp = &BIGIT(y,yl);
      yb = 1; i = yl;
      for (;;) {
        bigit t1 = *--yp, t2 = t1 - yb;
        if (*--xp & ~t2) return Strue;
        if (--i == 0) return Sfalse;
        yb = t2 > t1;
      }
    }
  } else {
    if (ys == 0) {
      bigit xb;

      xp = &BIGIT(x,xl); yp = &BIGIT(y,yl);
      xb = 1; i = yl;
      for (;;) {
        bigit t1 = *--xp, t2 = t1 - xb;
        if (*--yp & ~t2) return Strue;
        if (--i == 0) return Sfalse;
        xb = t2 > t1;
      }
    } else {
     /* logand of two negative bignums is always nonzero */
      return Strue;
    }
  }
}

/* k must be a nonnegative fixnum.  x may be a bignum or fixnum */
ptr S_logbitp(ptr k, ptr x) {
  uptr n = UNFIX(k);

  if (Sfixnump(x)) {
    if (n >= fixnum_bits)
      return Sboolean((iptr)x < 0);
    else
      return Sboolean((iptr)x & ((iptr)FIX(1) << n));
  } else {
    return big_logbitp(n, x, BIGLEN(x), BIGSIGN(x));
  }
}

/* %ac0 must hold a nonnegative fixnum.  %ts must hold a bignum.  Changes %ts */
void S_bignum_mask_test(void) {
  ptr tc = get_thread_context();
  iptr n = (iptr)AC0(tc);
  ptr x = TS(tc);

  TS(tc) = big_logbitp(n, x, BIGLEN(x), BIGSIGN(x));
}

/* similar logic to big_logand */

static ptr big_logbitp(iptr n, ptr x, iptr xl, IBOOL xs) {
  iptr i;
  bigit *xp;

  if (xs == 0) {
    i = xl - (n / bigit_bits + 1);
    if (i < 0) return Sfalse;

    n = n % bigit_bits;
    return Sboolean(BIGIT(x,i) & ((bigit)1 << n));
  } else {
    bigit xb;

   /* get out quick when 2^n has more bigits than x */
    if (n / bigit_bits >= xl) return Strue;

    xp = &BIGIT(x,xl); xb = 1;
    for (i = xl; ; i -= 1) {
      bigit t1 = *--xp, t2 = t1 - xb;
      if (n < bigit_bits) return Sboolean(~t2 & (1 << n));
      xb = t2 > t1;
      n -= bigit_bits;
    }
  }
}

/* k must be a nonnegative fixnum.  x may be a bignum or fixnum */
ptr S_logbit0(ptr k, ptr x) {
  ptr tc = get_thread_context();
  iptr n = UNFIX(k);

  if (Sfixnump(x)) {
    if (n < fixnum_bits - 1) {
      return FIX(UNFIX(x) & ~(1 << n));
    } else {
      iptr xl; IBOOL xs;

      FIXNUM_TO_BIGNUM(tc,X(tc),x,&xl,&xs);
      return big_logbit0(tc, x, n, X(tc), xl, xs);
    }
  } else {
    return big_logbit0(tc, x, n, x, BIGLEN(x), BIGSIGN(x));
  }
}

/* logbit0 on signed-magnitude bignums
     y = 1 << n
     s&(x,~y) = x&~y                       know result >= 0
     s&(-x,~y) = -#(#x&~y)                 know result < 0
               = -(~(~(x-1)&~y)+1)
               = -(((x-1)|y)+1)
*/

/* adapted from big_logor algorithm */
static ptr big_logbit0(ptr tc, ptr origx, iptr n, ptr x, iptr xl, IBOOL xs) {
  iptr i;
  bigit *xp, *zp;
  iptr yl = (n / bigit_bits) + 1;

  if (xs == 0) {
    if (yl > xl) {
     /* we'd just be clearing a bit that's already (virtually) cleared */
      return origx;
    } else {
      PREPARE_BIGNUM(tc, W(tc),xl);
      xp = &BIGIT(x,xl); zp = &BIGIT(W(tc),xl);
      for (;;) {
        if (n < bigit_bits) break;
        *--zp = *--xp;
        n -= bigit_bits;
      }
      *--zp = *--xp & ~(1 << n);
      for (i = xl - yl; i > 0; i -= 1) *--zp = *--xp;
      return copy_normalize(tc, zp,xl,0);
    }
  } else {
    bigit xb, k, x1, x2, z1, z2;
    iptr zl = (yl > xl ? yl : xl) + 1;

    PREPARE_BIGNUM(tc, W(tc),zl);
    xp = &BIGIT(x,xl); zp = &BIGIT(W(tc),zl);
    k = xb = 1;
    i = xl;
    for (;;) {
      if (i > 0) { x1 = *--xp; i -= 1; } else x1 = 0;
      x2 = x1 - xb; xb = x2 > x1;
      if (n < bigit_bits) break;
      z1 = x2; z2 = z1 + k; k = z2 < z1;
      *--zp = z2;
      n -= bigit_bits;
    }
    z1 = x2 | (1 << n); z2 = z1 + k; k = z2 < z1;
    *--zp = z2;
    for (; i > 0; i -= 1) {
      x1 = *--xp; x2 = x1 - xb; xb = x2 > x1;
      z1 = x2; z2 = z1 + k; k = z2 < z1;
      *--zp = z2;
    }
    *--zp = k;
    return copy_normalize(tc, zp, zl, 1);
  }
}

/* k must be a nonnegative fixnum.  x may be a bignum or fixnum */
ptr S_logbit1(ptr k, ptr x) {
  ptr tc = get_thread_context();
  iptr n = UNFIX(k);

  if (Sfixnump(x)) {
    if (n < fixnum_bits - 1) {
      return FIX(UNFIX(x) | ((uptr)1 << n));
    } else {
      iptr xl; IBOOL xs;

      FIXNUM_TO_BIGNUM(tc,X(tc),x,&xl,&xs);
      return big_logbit1(tc, x, n, X(tc), xl, xs);
    }
  } else {
    return big_logbit1(tc, x, n, x, BIGLEN(x), BIGSIGN(x));
  }
}

/* adapted from big_logor algorithm */
static ptr big_logbit1(ptr tc, ptr origx, iptr n, ptr x, iptr xl, IBOOL xs) {
  iptr i;
  bigit *xp, *zp;
  iptr yl = (n / bigit_bits) + 1;

  if (xs == 0) {
    bigit x1;
    iptr zl = yl > xl ? yl : xl;

    PREPARE_BIGNUM(tc, W(tc),zl);
    xp = &BIGIT(x,xl); zp = &BIGIT(W(tc),zl);

    i = xl;
    for (;;) {
      if (i > 0) { x1 = *--xp; i -= 1; } else x1 = 0;
      if (n < bigit_bits) break;
      *--zp = x1;
      n -= bigit_bits;
    }
    *--zp = x1 | ((bigit)1 << n);
    for (; i > 0; i -= 1) *--zp = *--xp;
    return copy_normalize(tc, zp, zl, 0);
  } else if (yl > xl) {
   /* we'd just be setting a bit that's already (virtually) set */
    return origx;
  } else { /* xl >= yl */
    bigit xb, k, x1, x2, z1, z2;
    iptr zl = xl + 1;

    PREPARE_BIGNUM(tc, W(tc),zl);
    xp = &BIGIT(x,xl); zp = &BIGIT(W(tc),zl);
    k = xb = 1;
    for (;;) {
      x1 = *--xp; x2 = x1 - xb; xb = x2 > x1;
      if (n < bigit_bits) break;
      z1 = x2;
      z2 = z1 + k; k = z2 < z1;
      *--zp = z2;
      n -= bigit_bits;
    }
    z1 = x2 & ~(1 << n);
    z2 = z1 + k; k = z2 < z1;
    *--zp = z2;
    for (i = xl - yl; i > 0; i -= 1) {
      x1 = *--xp; x2 = x1 - xb; xb = x2 > x1;
      z1 = x2;
      z2 = z1 + k; k = z2 < z1;
      *--zp = z2;
    }
    *--zp = k;
    return copy_normalize(tc, zp, zl, 1);
  }
}

ptr S_logor(ptr x, ptr y) {
  ptr tc = get_thread_context();

  if (Sfixnump(x)) {
    if (Sfixnump(y)) {
      return (ptr)((iptr)x | (iptr)(y));
    } else {
      iptr xl; IBOOL xs;
      FIXNUM_TO_BIGNUM(tc,X(tc),x,&xl,&xs)
      return big_logor(tc, y, X(tc), BIGLEN(y), xl, BIGSIGN(y), xs);
    }
  } else {
    if (Sfixnump(y)) {
      iptr yl; IBOOL ys;
      FIXNUM_TO_BIGNUM(tc,Y(tc),y,&yl,&ys)
      return big_logor(tc, x, Y(tc), BIGLEN(x), yl, BIGSIGN(x), ys);
    } else {
      if (BIGLEN(x) >= BIGLEN(y))
        return big_logor(tc, x, y, BIGLEN(x), BIGLEN(y), BIGSIGN(x), BIGSIGN(y));
      else
        return big_logor(tc, y, x, BIGLEN(y), BIGLEN(x), BIGSIGN(y), BIGSIGN(x));
    }
  }
}

/* logor on signed-magnitude bignums
     s|(x,y)   = x|y                       know result >= 0
     s|(x,-y)  = -(#(x|#y))                know result < 0
               = -(~(x|~(y-1))+1)
               = -(((y-1)&~x)+1)
     s|(-x,y)  = -(((x-1)&~y)+1)
     s|(-x,-y) = -(#(#x|#y))               know result < 0
               = -(~(~(x-1)|~(y-1))+1)
               = -(((x-1)&(y-1))+1)        de morgan's law
*/

/* assumes xl >= yl */
static ptr big_logor(ptr tc, ptr x, ptr y, iptr xl, iptr yl, IBOOL xs, IBOOL ys) {
  iptr i;
  bigit *xp, *yp, *zp;

  if (xs == 0) {
    if (ys == 0) {
      PREPARE_BIGNUM(tc, W(tc),xl);
      xp = &BIGIT(x,xl); yp = &BIGIT(y,yl); zp = &BIGIT(W(tc),xl);
      for (i = yl; i > 0; i -= 1) *--zp = *--xp | *--yp;
      for (i = xl - yl; i > 0; i -= 1) *--zp = *--xp;
      return copy_normalize(tc, zp, xl, 0);
    } else {
      bigit yb, k;

      PREPARE_BIGNUM(tc, W(tc),yl+1);
      xp = &BIGIT(x,xl); yp = &BIGIT(y,yl); zp = &BIGIT(W(tc),yl+1);
      k = yb = 1;
      for (i = yl; i > 0; i -= 1) {
        bigit y1, y2, z1, z2;
        y1 = *--yp; y2 = y1 - yb; yb = y2 > y1;
        z1 = y2 & ~*--xp;
        z2 = z1 + k; k = z2 < z1;
        *--zp = z2;
      }
      *--zp = k;
      return copy_normalize(tc, zp, yl+1, 1);
    }
  } else {
    if (ys == 0) {
      bigit xb, k;

      PREPARE_BIGNUM(tc, W(tc),xl+1);
      xp = &BIGIT(x,xl); yp = &BIGIT(y,yl); zp = &BIGIT(W(tc),xl+1);
      k = xb = 1;
      for (i = yl; i > 0; i -= 1) {
        bigit x1, x2, z1, z2;
        x1 = *--xp; x2 = x1 - xb; xb = x2 > x1;
        z1 = x2 & ~*--yp;
        z2 = z1 + k; k = z2 < z1;
        *--zp = z2;
      }
      for (i = xl - yl; i > 0; i -= 1) {
        bigit x1, x2, z1, z2;
        x1 = *--xp; x2 = x1 - xb; xb = x2 > x1;
        z1 = x2;
        z2 = z1 + k; k = z2 < z1;
        *--zp = z2;
      }
      *--zp = k;
      return copy_normalize(tc, zp, xl+1, 1);
    } else {
      bigit xb, yb, k;

      PREPARE_BIGNUM(tc, W(tc),yl+1);
      xp = &BIGIT(x,xl); yp = &BIGIT(y,yl); zp = &BIGIT(W(tc),yl+1);
      k = yb = xb = 1;
      for (i = yl; i > 0; i -= 1) {
        bigit x1, x2, y1, y2, z1, z2;
        x1 = *--xp; x2 = x1 - xb; xb = x2 > x1;
        y1 = *--yp; y2 = y1 - yb; yb = y2 > y1;
        z1 = x2 & y2;
        z2 = z1 + k; k = z2 < z1;
        *--zp = z2;
      }
      *--zp = k;
      return copy_normalize(tc, zp, yl+1, 1);
    }
  }
}

ptr S_logxor(ptr x, ptr y) {
  ptr tc = get_thread_context();

  if (Sfixnump(x)) {
    if (Sfixnump(y)) {
      return (ptr)((iptr)x ^ (iptr)(y));
    } else {
      iptr xl; IBOOL xs;
      FIXNUM_TO_BIGNUM(tc,X(tc),x,&xl,&xs)
      return big_logxor(tc, y, X(tc), BIGLEN(y), xl, BIGSIGN(y), xs);
    }
  } else {
    if (Sfixnump(y)) {
      iptr yl; IBOOL ys;
      FIXNUM_TO_BIGNUM(tc,Y(tc),y,&yl,&ys)
      return big_logxor(tc, x, Y(tc), BIGLEN(x), yl, BIGSIGN(x), ys);
    } else {
      if (BIGLEN(x) >= BIGLEN(y))
        return big_logxor(tc, x, y, BIGLEN(x), BIGLEN(y), BIGSIGN(x), BIGSIGN(y));
      else
        return big_logxor(tc, y, x, BIGLEN(y), BIGLEN(x), BIGSIGN(y), BIGSIGN(x));
    }
  }
}

/* logxor on signed-magnitude bignums
     s^(x,y)   = x^y                       know result >= 0
     s^(x,-y)  = -(#(x^#y))                know result < 0
               = -(~(x^~(y-1))+1)
               = -((x^(y-1))+1)            since ~(a^~b) = a^b
     s^(-x,y)  = -((y^(x-1))+1)
     s^(-x,-y) = #x^#y                     know result >= 0
               = ~(x-1)^~(y-1)
               = (x-1)^(y-1)               since ~a^~b = a^b
*/

/* assumes xl >= yl */
static ptr big_logxor(ptr tc, ptr x, ptr y, iptr xl, iptr yl, IBOOL xs, IBOOL ys) {
  iptr i;
  bigit *xp, *yp, *zp;

  if (xs == 0) {
    if (ys == 0) {
      PREPARE_BIGNUM(tc, W(tc),xl);
      xp = &BIGIT(x,xl); yp = &BIGIT(y,yl); zp = &BIGIT(W(tc),xl);
      for (i = yl; i > 0; i -= 1) *--zp = *--xp ^ *--yp;
      for (i = xl - yl; i > 0; i -= 1) *--zp = *--xp;
      return copy_normalize(tc, zp, xl, 0);
    } else {
      bigit yb, k;

      PREPARE_BIGNUM(tc, W(tc),xl+1);
      xp = &BIGIT(x,xl); yp = &BIGIT(y,yl); zp = &BIGIT(W(tc),xl+1);
      k = yb = 1;
      for (i = yl; i > 0; i -= 1) {
        bigit y1, y2, z1, z2;
        y1 = *--yp; y2 = y1 - yb; yb = y2 > y1;
        z1 = *--xp ^ y2;
        z2 = z1 + k; k = z2 < z1;
        *--zp = z2;
      }
      for (i = xl - yl; i > 0; i -= 1) {
        bigit z1, z2;
        z1 = *--xp;
        z2 = z1 + k; k = z2 < z1;
        *--zp = z2;
      }
      *--zp = k;
      return copy_normalize(tc, zp, xl+1, 1);
    }
  } else {
    if (ys == 0) {
      bigit xb, k;

      PREPARE_BIGNUM(tc, W(tc),xl+1);
      xp = &BIGIT(x,xl); yp = &BIGIT(y,yl); zp = &BIGIT(W(tc),xl+1);
      k = xb = 1;
      for (i = yl; i > 0; i -= 1) {
        bigit x1, x2, z1, z2;
        x1 = *--xp; x2 = x1 - xb; xb = x2 > x1;
        z1 = *--yp ^ x2;
        z2 = z1 + k; k = z2 < z1;
        *--zp = z2;
      }
      for (i = xl - yl; i > 0; i -= 1) {
        bigit x1, x2, z1, z2;
        x1 = *--xp; x2 = x1 - xb; xb = x2 > x1;
        z1 = x2;
        z2 = z1 + k; k = z2 < z1;
        *--zp = z2;
      }
      *--zp = k;
      return copy_normalize(tc, zp, xl+1, 1);
    } else {
      bigit xb, yb;

      PREPARE_BIGNUM(tc, W(tc),xl);
      xp = &BIGIT(x,xl); yp = &BIGIT(y,yl); zp = &BIGIT(W(tc),xl);
      yb = xb = 1;
      for (i = yl; i > 0; i -= 1) {
        bigit x1, x2, y1, y2;
        x1 = *--xp; x2 = x1 - xb; xb = x2 > x1;
        y1 = *--yp; y2 = y1 - yb; yb = y2 > y1;
        *--zp = x2 ^ y2;
      }
      for (i = xl - yl; i > 0; i -= 1) {
        bigit x1, x2;
        x1 = *--xp; x2 = x1 - xb; xb = x2 > x1;
        *--zp = x2;
      }
      return copy_normalize(tc, zp, xl, 0);
    }
  }
}

/* lognot on signed-magnitude bignums:
     s~(x)  = -#~x
            = -(~~x+1)
            = -(x+1)
     s~(-x) = ~#x
            = ~~(x-1)
            = x-1
   therefore:
     (define (lognot x)
       (if (< x 0)
           (- (- x) 1)
           (- (+ x 1))))
   simplifying:
     (define (lognot x) (- -1 x))
*/

ptr S_lognot(ptr x) {
  if (Sfixnump(x)) {
    return FIX(~UNFIX(x));
  } else {
    return S_sub(FIX(-1), x);
  }
}

void S_number_init(void) {
  if ((int)(hidden_bit >> 22) != 0x40000000) {
    fprintf(stderr, "hidden_bit >> 22 = %x\n", (int)(hidden_bit >> 22));
    S_abnormal_exit();
  }
}
