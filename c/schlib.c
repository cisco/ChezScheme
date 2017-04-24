/* schlib.c
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
static ptr S_call PROTO((ptr tc, ptr cp, iptr argcnt));

/* Sinteger_value is in number.c */

/* Sinteger32_value is in number.c */

/* Sinteger64_value is in number.c */

void Sset_box(x, y) ptr x, y; {
    SETBOXREF(x, y);
}

void Sset_car(x, y) ptr x, y; {
    SETCAR(x, y);
}

void Sset_cdr(x, y) ptr x, y; {
    SETCDR(x, y);
}

void Svector_set(x, i, y) ptr x; iptr i; ptr y; {
    SETVECTIT(x, i, y);
}

/* Scons is in alloc.c */

ptr Sstring_to_symbol(s) const char *s; {
    return S_intern((const unsigned char *)s);
}

ptr Ssymbol_to_string(x) ptr x; {
  ptr name = SYMNAME(x);
  if (Sstringp(name))
    return name;
  else if (Spairp(name))
    return Scdr(name);
  else
   /* don't have access to prefix or count, and can't handle arbitrary
      prefixes anyway, so always punt */
    return S_string("gensym", -1);
}

/* Sflonum is in alloc.c */

ptr Smake_vector(n, x) iptr n; ptr x; {
    ptr p; iptr i;

    p = S_vector(n);
    for (i = 0; i < n; i += 1) INITVECTIT(p, i) = x;
    return p;
}

ptr Smake_fxvector(n, x) iptr n; ptr x; {
    ptr p; iptr i;

    p = S_fxvector(n);
    for (i = 0; i < n; i += 1) Sfxvector_set(p, i, x);
    return p;
}

ptr Smake_bytevector(n, x) iptr n; int x; {
    ptr p; iptr i;

    p = S_bytevector(n);
    for (i = 0; i < n; i += 1) Sbytevector_u8_set(p, i, (octet)x);
    return p;
}

ptr Smake_string(n, c) iptr n; int c; {
    ptr p; iptr i;

    p = S_string((char *)NULL, n);
    for (i = 0; i < n; i += 1) Sstring_set(p, i, c);
    return p;
}

ptr Smake_uninitialized_string(n) iptr n; {
    return S_string((char *)NULL, n);
}

ptr Sstring(s) const char *s; {
    return S_string(s, -1);
}

ptr Sstring_of_length(s, n) const char *s; iptr n; {
    return S_string(s, n);
}

/* Sbox is in alloc.c */

/* Sinteger is in number.c */

/* Sunsigned is in number.c */

/* Sunsigned32 is in number.c */

/* Sunsigned64 is in number.c */

ptr Stop_level_value(x) ptr x; {
  ptr tc = get_thread_context();
  IBOOL enabled = (DISABLECOUNT(tc) == 0);
  if (enabled) DISABLECOUNT(tc) = FIX(UNFIX(DISABLECOUNT(tc)) + 1);
  x = Scall1(S_symbol_value(Sstring_to_symbol("$c-tlv")), x);
  if (enabled) DISABLECOUNT(tc) = FIX(UNFIX(DISABLECOUNT(tc)) - 1);
  return x;
}

void Sset_top_level_value(x, y) ptr x, y; {
  ptr tc = get_thread_context();
  IBOOL enabled = (DISABLECOUNT(tc) == 0);
  if (enabled) DISABLECOUNT(tc) = FIX(UNFIX(DISABLECOUNT(tc)) + 1);
  Scall2(S_symbol_value(Sstring_to_symbol("$c-stlv!")), x, y);
  if (enabled) DISABLECOUNT(tc) = FIX(UNFIX(DISABLECOUNT(tc)) - 1);
}

#include <setjmp.h>

/* consider rewriting these to avoid multiple calls to get_thread_context */
ptr Scall0(cp) ptr cp; {
    ptr tc = get_thread_context();
    S_initframe(tc,0);
    return S_call(tc, cp, 0);
}

ptr Scall1(cp, x1) ptr cp, x1; {
    ptr tc = get_thread_context();
    S_initframe(tc, 1);
    S_put_arg(tc, 1, x1);
    return S_call(tc, cp, 1);
}

ptr Scall2(cp, x1, x2) ptr cp, x1, x2; {
    ptr tc = get_thread_context();
    S_initframe(tc, 2);
    S_put_arg(tc, 1, x1);
    S_put_arg(tc, 2, x2);
    return S_call(tc, cp, 2);
}

ptr Scall3(cp, x1, x2, x3) ptr cp, x1, x2, x3; {
    ptr tc = get_thread_context();
    S_initframe(tc, 3);
    S_put_arg(tc, 1, x1);
    S_put_arg(tc, 2, x2);
    S_put_arg(tc, 3, x3);
    return S_call(tc, cp, 3);
}

void Sinitframe(n) iptr n; {
    ptr tc = get_thread_context();
    S_initframe(tc, n);
}

void S_initframe(tc, n) ptr tc; iptr n; {
  /* check for and handle stack overflow */
    if ((ptr *)SFP(tc) + n + 2 > (ptr *)ESP(tc))
        S_overflow(tc, (n+2)*sizeof(ptr));

  /* intermediate frame contains old RA + cchain */;
    SFP(tc) = (ptr)((ptr *)SFP(tc) + 2);
}

void Sput_arg(i, x) iptr i; ptr x; {
    ptr tc = get_thread_context();
    S_put_arg(tc, i, x);
}

void S_put_arg(tc, i, x) ptr tc; iptr i; ptr x; {
    if (i <= asm_arg_reg_cnt)
        REGARG(tc, i) = x;
    else
        FRAME(tc, i - asm_arg_reg_cnt) = x;
}

ptr Scall(cp, argcnt) ptr cp; iptr argcnt; {
    ptr tc = get_thread_context();
    return S_call(tc, cp, argcnt);
}

static ptr S_call(tc, cp, argcnt) ptr tc; ptr cp; iptr argcnt; {
    AC0(tc) = (ptr)argcnt;
    AC1(tc) = cp;
    S_call_help(tc, 1);
    return AC0(tc);
}

/* args are set up, argcnt in ac0, closure in ac1 */
void S_call_help(tc, singlep) ptr tc; IBOOL singlep; {
  /* declaring code volatile should be unnecessary, but it quiets gcc */
    void *jb; volatile ptr code;

  /* lock caller's code object, since his return address is sitting in
     the C stack and we may end up in a garbage collection */
    code = CP(tc);
    if (Sprocedurep(code)) code = CLOSCODE(code);
    Slock_object(code);

    CP(tc) = AC1(tc);

    jb = CREATEJMPBUF();
    if (jb == NULL)
      S_error_abort("unable to allocate memory for jump buffer");
    FRAME(tc, -1) = CCHAIN(tc) = Scons(Scons(jb, code), CCHAIN(tc));

    switch (SETJMP(jb)) {
        case 0: /* first time */
            S_generic_invoke(tc, S_G.invoke_code_object);
            S_error_abort("S_generic_invoke return");
            break;
        case -1: /* error */
            S_generic_invoke(tc, S_G.error_invoke_code_object);
            S_error_abort("S_generic_invoke return");
            break;
        case 1: { /* normal return */
            ptr yp = CCHAIN(tc);
            FREEJMPBUF(CAAR(yp));
            CCHAIN(tc) = Scdr(yp);
            break;
        }
        default:
            S_error_abort("unexpected SETJMP return value");
            break;
    }

  /* verify single return value */
    if (singlep && (iptr)AC1(tc) != 1)
        S_error1("", "returned ~s values to single value return context",
                   FIX((iptr)AC1(tc)));

  /* restore caller to cp so that we can lock it again another day.  we
     restore the code object rather than the original closure, as the
     closure may have been relocated or reclaimed by now */
    CP(tc) = code;
}

void S_call_void() {
    ptr tc = get_thread_context();
    S_call_help(tc, 0);
}

ptr S_call_ptr() {
    ptr tc = get_thread_context();
    S_call_help(tc, 1);
    return AC0(tc);
}

iptr S_call_fixnum() {
    ptr tc = get_thread_context();
    S_call_help(tc, 1);
    return Sfixnum_value(AC0(tc));
}

I32 S_call_int32() {
    ptr tc = get_thread_context();
    S_call_help(tc, 1);
    return (I32)Sinteger_value(AC0(tc));
}

U32 S_call_uns32() {
    ptr tc = get_thread_context();
    S_call_help(tc, 1);
    return (U32)Sinteger_value(AC0(tc));
}

I64 S_call_int64() {
    ptr tc = get_thread_context();
    S_call_help(tc, 1);
    return S_int64_value("foreign-callable", AC0(tc));
}

U64 S_call_uns64() {
    ptr tc = get_thread_context();
    S_call_help(tc, 1);
    return S_int64_value("foreign-callable", AC0(tc));
}

double S_call_double() {
    ptr tc = get_thread_context();
    S_call_help(tc, 1);
    return Sflonum_value(AC0(tc));
}

float S_call_single() {
    ptr tc = get_thread_context();
    S_call_help(tc, 1);
    return (float)Sflonum_value(AC0(tc));
}

U8 *S_call_bytevector() {
    ptr tc = get_thread_context();
    S_call_help(tc, 1);
    return (U8 *)&BVIT(AC0(tc),0);
}

uptr S_call_fptr() {
    ptr tc = get_thread_context();
    S_call_help(tc, 1);
    return (uptr)RECORDINSTIT(AC0(tc),0);
}

/* cchain = ((jb . co) ...) */
void S_return() {
    ptr tc = get_thread_context();
    ptr xp, yp;

    SFP(tc) = (ptr)((ptr *)SFP(tc) - 2);

  /* grab saved cchain */
    yp = FRAME(tc, 1);

  /* verify saved cchain is sublist of current cchain */
    for (xp = CCHAIN(tc); xp != yp; xp = Scdr(xp))
        if (xp == Snil)
            S_error("", "attempt to return to stale foreign context");

  /* error checks are done; now unlock affected code objects */
    for (xp = CCHAIN(tc); ; xp = Scdr(xp)) {
        Sunlock_object(CDAR(xp));
        if (xp == yp) break;
        FREEJMPBUF(CAAR(xp));
    }

  /* reset cchain and return via longjmp */
    CCHAIN(tc) = yp;
    LONGJMP(CAAR(yp), 1);
}
