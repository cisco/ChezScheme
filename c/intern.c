/* intern.c
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
static void oblist_insert(ptr sym, iptr idx, IGEN g);
static iptr hash(const unsigned char *s, iptr n);
static iptr hash_sc(const string_char *s, iptr n);
static iptr hash_uname(const string_char *s, iptr n);
static ptr mkstring(const string_char *s, iptr n);

/* list of some primes to use for oblist sizes */
#if (ptr_bits == 32)
static iptr oblist_lengths[] = {
  1031,
  2053,
  4099,
  8209,
  16411,
  32771,
  65537,
  131101,
  262147,
  524309,
  1048583,
  2097169,
  4194319,
  8388617,
  16777259,
  33554467,
  67108879,
  134217757,
  268435459,
  536870923,
  1073741827,
  0};
#endif
#if (ptr_bits == 64)
static iptr oblist_lengths[] = {
  1031,
  2053,
  4099,
  8209,
  16411,
  32771,
  65537,
  131101,
  262147,
  524309,
  1048583,
  2097169,
  4194319,
  8388617,
  16777259,
  33554467,
  67108879,
  134217757,
  268435459,
  536870923,
  1073741827,
  2147483659,
  4294967311,
  8589934609,
  17179869209,
  34359738421,
  68719476767,
  137438953481,
  274877906951,
  549755813911,
  1099511627791,
  2199023255579,
  4398046511119,
  8796093022237,
  17592186044423,
  35184372088891,
  70368744177679,
  140737488355333,
  281474976710677,
  562949953421381,
  1125899906842679,
  2251799813685269,
  4503599627370517,
  9007199254740997,
  18014398509482143,
  36028797018963971,
  72057594037928017,
  144115188075855881,
  288230376151711813,
  576460752303423619,
  1152921504606847009,
  2305843009213693967,
  4611686018427388039,
  0};
#endif

void S_intern_init(void) {
    IGEN g;

    if (!S_boot_time) return;

    S_G.oblist_length_pointer = &oblist_lengths[3];
    S_G.oblist_length = *S_G.oblist_length_pointer;
    S_G.oblist_count = 0;
    S_G.oblist = S_getmem(S_G.oblist_length * sizeof(bucket *), 1);
    for (g = 0; g < static_generation; g += 1) S_G.buckets_of_generation[g] = NULL;
}

static void oblist_insert(ptr sym, iptr idx, IGEN g) {
  bucket *b, *oldb, **pb;

  find_room(g == 0 ? space_new : space_data, g, typemod, sizeof(bucket), b);
  b->sym = sym;
  if (g == 0) {
    b->next = S_G.oblist[idx];
    S_G.oblist[idx] = b;
  } else {
    for (pb = &S_G.oblist[idx]; (oldb = *pb) != NULL && SegmentGeneration(addr_get_segment(oldb)) < g; pb = &oldb->next);
    b->next = oldb;
    *pb = b;
  }

  if (g != static_generation) {
    bucket_list *bl;
    find_room(g == 0 ? space_new : space_data, g, typemod, sizeof(bucket_list), bl);
    bl->car = b;
    bl->cdr = S_G.buckets_of_generation[g];
    S_G.buckets_of_generation[g] = bl;
  }

  S_G.oblist_count += 1;
}

void S_resize_oblist(void) {
  bucket **new_oblist, *b, *oldb, **pb, *bnext;
  iptr *new_oblist_length_pointer, new_oblist_length, i, idx;
  ptr sym;
  IGEN g;

  new_oblist_length_pointer = S_G.oblist_length_pointer;

  if (S_G.oblist_count < S_G.oblist_length) {
    while (new_oblist_length_pointer != &oblist_lengths[0] && *(new_oblist_length_pointer - 1) >= S_G.oblist_count) {
      new_oblist_length_pointer -= 1;
    }
  } else if (S_G.oblist_count > S_G.oblist_length) {
    while (*(new_oblist_length_pointer + 1) != 0 && *(new_oblist_length_pointer + 1) <= S_G.oblist_count) {
      new_oblist_length_pointer += 1;
    }
  }

  if (new_oblist_length_pointer == S_G.oblist_length_pointer) return;

  new_oblist_length = *new_oblist_length_pointer;
  new_oblist = S_getmem(new_oblist_length * sizeof(bucket *), 1);

  for (i = 0; i < S_G.oblist_length; i += 1) {
    for (b = S_G.oblist[i]; b != NULL; b = bnext) {
      bnext = b->next;
      sym = b->sym;
      idx = UNFIX(SYMHASH(sym)) % new_oblist_length;
      g = GENERATION(sym);

      for (pb = &new_oblist[idx]; (oldb = *pb) != NULL && SegmentGeneration(addr_get_segment(oldb)) < g; pb = &oldb->next);
      b->next = oldb;
      *pb = b;
    }
  }

  S_freemem(S_G.oblist, S_G.oblist_length * sizeof(bucket *));
  S_G.bytesof[static_generation][countof_oblist] += (new_oblist_length - S_G.oblist_length) * sizeof(bucket *);

  S_G.oblist_length_pointer = new_oblist_length_pointer;
  S_G.oblist_length = new_oblist_length;
  S_G.oblist = new_oblist;
}

/* hash function: multiplier weights each character, h = n factors in the length */
#define multiplier 3

static iptr hash(const unsigned char *s, iptr n) {
  iptr h = n + 401887359;
  while (n--) h = h * multiplier + *s++;
  return h & most_positive_fixnum;
}

static iptr hash_sc(const string_char *s, iptr n) {
  iptr h = n + 401887359;
  while (n--) h = h * multiplier + Schar_value(*s++);
  return h & most_positive_fixnum;
}

static iptr hash_uname(const string_char *s, iptr n) {
  /* attempting to get dissimilar hash codes for gensyms created in the same session */
  iptr i = n, h = 0; iptr pos = 1; int d, c;

  while (i-- > 0) {
    if ((c = Schar_value(s[i])) == '-') {
      if (pos <= 10) break;
      return (h + 523658599) & most_positive_fixnum;
    }
    d = c - '0';
    if (d < 0 || d > 9) break;
    h += d * pos;
    pos *= 10;
  }

  return hash_sc(s, n);
}

static ptr mkstring(const string_char *s, iptr n) {
  iptr i;
  ptr str = S_string(NULL, n);
  for (i = 0; i != n; i += 1) STRIT(str, i) = s[i];
  return str;
}

/* handles single-byte characters, implicit length */
ptr S_intern(const unsigned char *s) {
  iptr n = strlen((const char *)s);
  iptr hc = hash(s, n);
  iptr idx = hc % S_G.oblist_length;
  ptr sym;
  bucket *b;

  tc_mutex_acquire()

  b = S_G.oblist[idx];
  while (b != NULL) {
    sym = b->sym;
    if (!GENSYMP(sym)) {
       ptr str = SYMNAME(sym);
       if (Sstring_length(str) == n) {
          iptr i;
          for (i = 0; ; i += 1) {
            if (i == n) {
               tc_mutex_release()
               return sym;
            }
            if (Sstring_ref(str, i) != s[i]) break;
          }
       }
    }
    b = b->next;
  }

  sym = S_symbol(S_string((const char *)s, n));
  INITSYMHASH(sym) = FIX(hc);
  oblist_insert(sym, idx, 0);

  tc_mutex_release()
  return sym;
}

/* handles string_chars, explicit length */
ptr S_intern_sc(const string_char *name, iptr n, ptr name_str) {
  iptr hc = hash_sc(name, n);
  iptr idx = hc % S_G.oblist_length;
  ptr sym;
  bucket *b;

  tc_mutex_acquire()

  b = S_G.oblist[idx];
  while (b != NULL) {
    sym = b->sym;
    if (!GENSYMP(sym)) {
       ptr str = SYMNAME(sym);
       if (Sstring_length(str) == n) {
          iptr i;
          for (i = 0; ; i += 1) {
            if (i == n) {
               tc_mutex_release()
               return sym;
            }
            if (STRIT(str, i) != name[i]) break;
          }
       }
    }
    b = b->next;
  }

  /* if (name_str == Sfalse) */ name_str = mkstring(name, n);
  sym = S_symbol(name_str);
  INITSYMHASH(sym) = FIX(hc);
  oblist_insert(sym, idx, 0);

  tc_mutex_release()
  return sym;
}

ptr S_intern3(const string_char *pname, iptr plen, const string_char *uname, iptr ulen, ptr pname_str, ptr uname_str) {
  iptr hc = hash_uname(uname, ulen);
  iptr idx = hc % S_G.oblist_length;
  ptr sym;
  bucket *b;

  tc_mutex_acquire()

  b = S_G.oblist[idx];
  while (b != NULL) {
    sym = b->sym;
    if (GENSYMP(sym)) {
       ptr str = Scar(SYMNAME(sym));
       if (Sstring_length(str) == ulen) {
          iptr i;
          for (i = 0; ; i += 1) {
            if (i == ulen) {
               tc_mutex_release()
               return sym;
            }
            if (STRIT(str, i) != uname[i]) break;
          }
       }
    }
    b = b->next;
  }

  if (pname_str == Sfalse) pname_str = mkstring(pname, plen);
  if (uname_str == Sfalse) uname_str = mkstring(uname, ulen);
  sym = S_symbol(Scons(uname_str, pname_str));
  INITSYMHASH(sym) = FIX(hc);
  oblist_insert(sym, idx, 0);

  tc_mutex_release()
  return sym;
}

void S_intern_gensym(ptr sym) {
  ptr uname_str = Scar(SYMNAME(sym));
  const string_char *uname = &STRIT(uname_str, 0);
  iptr ulen = Sstring_length(uname_str);
  iptr hc = hash_uname(uname, ulen);
  iptr idx = hc % S_G.oblist_length;
  bucket *b;

  tc_mutex_acquire()

  b = S_G.oblist[idx];
  while (b != NULL) {
    ptr x = b->sym;
    if (GENSYMP(x)) {
       ptr str = Scar(SYMNAME(x));
       if (Sstring_length(str) == ulen) {
          iptr i;
          for (i = 0; ; i += 1) {
            if (i == ulen) {
               tc_mutex_release()
               S_error1("intern-gensym", "unique name ~s already interned", uname_str);
            }
            if (Sstring_ref(str, i) != uname[i]) break;
          }
       }
    }
    b = b->next;
  }

  INITSYMHASH(sym) = FIX(hc);
  oblist_insert(sym, idx, GENERATION(sym));

  tc_mutex_release()
}

/* retrofit existing symbols once nonprocedure_code is available */
void S_retrofit_nonprocedure_code(void) {
  ptr npc, sym, val; bucket_list *bl;

  npc = S_G.nonprocedure_code;

  /* assuming this happens early, before collector has been called, so need look only for generation 0 symbols */
  for (bl = S_G.buckets_of_generation[0]; bl != NULL; bl = bl->cdr) {
    sym = bl->car->sym;
    val = SYMVAL(sym);
    SETSYMCODE(sym, Sprocedurep(val) ? CLOSCODE(val) : npc);
  }
}
