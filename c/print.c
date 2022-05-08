/* print.c
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
static void pimmediate(ptr x);
static void pbox(ptr x);
static void pclo(ptr x);
static void pcode(ptr x);
static void pcons(ptr x);
static void pfile(ptr x);
static void pinexactnum(ptr x);
static IBOOL exact_real_negativep(ptr x);
static void pexactnum(ptr x);
static void prat(ptr x);
static void pchar(ptr x);
static void pstr(ptr x);
static void psym(ptr x);
static void pvec(ptr x);
static void pfxvector(ptr x);
static void pbytevector(ptr x);
static void pflonum(ptr x);
static void pfixnum(ptr x);
static void pbignum(ptr x);
static void wrint(ptr x);

void S_print_init(void) {}

void S_prin1(ptr x) {
    if (Simmediatep(x)) pimmediate(x);
    else if (Spairp(x)) pcons(x);
    else if (Ssymbolp(x)) psym(x);
    else if (Sfixnump(x)) pfixnum(x);
    else if (Sbignump(x)) pbignum(x);
    else if (Sstringp(x)) pstr(x);
    else if (Sratnump(x)) prat(x);
    else if (Sflonump(x)) (void) pflonum(x);
    else if (Sinexactnump(x)) pinexactnum(x);
    else if (Sexactnump(x)) pexactnum(x);
    else if (Svectorp(x)) pvec(x);
    else if (Sfxvectorp(x)) pfxvector(x);
    else if (Sbytevectorp(x)) pbytevector(x);
    else if (Sboxp(x)) pbox(x);
    else if (Sprocedurep(x)) pclo(x);
    else if (Scodep(x)) pcode(x);
    else if (Sportp(x)) pfile(x);
    else if (Srecordp(x)) printf("#<record>");
    else printf("#<garbage>");
    fflush(stdout);
}


static void pimmediate(ptr x) {
    if (Scharp(x)) pchar(x);
    else if (x == Snil) printf("()");
    else if (x == Strue) printf("#t");
    else if (x == Sfalse) printf("#f");
    else if (x == Seof_object) printf("#!eof");
    else if (x == Sbwp_object) printf("#!bwp");
    else if (x == sunbound) printf("#<unbound>");
    else if (x == Svoid) printf("#<void>");
    else printf("#<garbage>");
}

static void pbox(ptr x) {
    printf("#&");
    S_prin1(Sunbox(x));
}

static void pclo(UNUSED ptr x) {
  if (CODETYPE(CLOSCODE(x)) & (code_flag_continuation << code_flags_offset))
    printf("#<continuation>");
  else
    printf("#<procedure>");
}

static void pcode(UNUSED ptr x) {
    printf("#<code>");
}

static void pcons(ptr x) {
    putchar('(');
    while (1) {
        S_prin1(Scar(x));
        x = Scdr(x);
        if (!Spairp(x)) break;
        putchar(' ');
    }
    if (x!=Snil) {
        printf(" . ");
        S_prin1(x);
    }
    putchar(')');
}


static void pfile(UNUSED ptr x) {
    printf("#<port>");
}

static void pinexactnum(ptr x) {
    S_prin1(TYPE(&INEXACTNUM_REAL_PART(x),type_flonum));
    if (INEXACTNUM_IMAG_PART(x) >= 0.0) putchar('+');
    S_prin1(TYPE(&INEXACTNUM_IMAG_PART(x),type_flonum));
    putchar('i');
}

static IBOOL exact_real_negativep(ptr x) {
  if (Sratnump(x)) x = RATNUM(x);
  return Sfixnump(x) ? UNFIX(x) < 0 : BIGSIGN(x);
}

static void pexactnum(ptr x) {
    S_prin1(EXACTNUM_REAL_PART(x));
    if (!exact_real_negativep(EXACTNUM_IMAG_PART(x))) putchar('+');
    S_prin1(EXACTNUM_IMAG_PART(x));
    putchar('i');
}

static void prat(ptr x) {
    wrint(RATNUM(x));
    putchar('/');
    wrint(RATDEN(x));
}

static void pchar(ptr x) {
  int k = Schar_value(x);
  if (k >= 256) k = '?';
  printf("#\\");
  putchar(k);
}

static void pstr(ptr x) {
  iptr i, n = Sstring_length(x);

  putchar('"');
  for (i = 0; i < n; i += 1) {
    int k = Sstring_ref(x, i);
    if (k >= 256) k = '?';
    if ((k == '\\') || (k == '"')) putchar('\\');
    putchar(k);
  }
  putchar('"');
}

static void display_string(ptr x) {
  iptr i, n = Sstring_length(x);

  for (i = 0; i < n; i += 1) {
    int k = Sstring_ref(x, i);
    if (k >= 256) k = '?';
    putchar(k);
  }
}

static void psym(ptr x) {
  ptr name = SYMNAME(x);
  if (Sstringp(name)) {
    display_string(name);
  } else if (Spairp(name)) {
    if (Scar(name) != Sfalse) {
      printf("#{");
      display_string(Scdr(name));
      printf(" ");
      display_string(Scar(name));
      printf("}");
    } else {
      printf("#<gensym ");
      display_string(Scdr(name));
      printf(">");
    }
  } else {
    printf("#<gensym>");
  }
}

static void pvec(ptr x) {
    iptr n;

    putchar('#');
    n = Svector_length(x);
    wrint(FIX(n));
    putchar('(');
    if (n != 0) {
        iptr i = 0;

        while (1) {
            S_prin1(Svector_ref(x, i));
            if (++i == n) break;
            putchar(' ');
        }
    }
    putchar(')');
}

static void pfxvector(ptr x) {
    iptr n;

    putchar('#');
    n = Sfxvector_length(x);
    wrint(FIX(n));
    printf("vfx(");
    if (n != 0) {
        iptr i = 0;

        while (1) {
            pfixnum(Sfxvector_ref(x, i));
            if (++i == n) break;
            putchar(' ');
        }
    }
    putchar(')');
}

static void pbytevector(ptr x) {
    iptr n;

    putchar('#');
    n = Sbytevector_length(x);
    wrint(FIX(n));
    printf("vu8(");
    if (n != 0) {
        iptr i = 0;

        while (1) {
            pfixnum(FIX(Sbytevector_u8_ref(x, i)));
            if (++i == n) break;
            putchar(' ');
        }
    }
    putchar(')');
}

static void pflonum(ptr x) {
  char buf[256], *s;

 /* use snprintf to get it in a string */
  (void) snprintf(buf, 256, "%.16g",FLODAT(x));

 /* print the silly thing */
  printf("%s", buf);

 /* add .0 if it looks like an integer */
  s = buf;
  while (*s != 'E' && *s != 'e' && *s != '.')
    if (*s++ == 0) {
      printf(".0");
      break;
    }
}

static void pfixnum(ptr x) {
  if (UNFIX(x) < 0) {
    putchar('-');
    x = S_sub(FIX(0), x);
  }
  wrint(x);
}

static void pbignum(ptr x) {
  if (BIGSIGN(x)) {
    putchar('-');
    x = S_sub(FIX(0), x);
  }
  wrint(x);
}

static void wrint(ptr x) {
  ptr q, r;

  S_trunc_rem(get_thread_context(), x, FIX(10), &q, &r);
  if (q != 0) wrint(q);
  putchar((INT)UNFIX(r) + '0');
}
