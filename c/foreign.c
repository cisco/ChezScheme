/* foreign.c
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

#define debug(y) /* (void)printf(y) *//* uncomment printf for debug */
/* #define UNLINK(x) 0 *//* uncomment #define to preserve temp files */


#include "system.h"

/* we can now return arbitrary values (aligned or not)
 * since the garbage collector ignores addresses outside of the heap
 * or within foreign segments */
#define ptr_to_addr(p) ((void *)p)
#define addr_to_ptr(a) ((ptr)a)

/* buckets should be prime */
#define buckets 457
#define multiplier 3

#define ptrhash(x) ((uptr)x % buckets)

#ifdef LOAD_SHARED_OBJECT
#if defined(HPUX)
#include <dl.h>
#define dlopen(path,flags) (void *)shl_load(path, BIND_IMMEDIATE, 0L)
#define s_dlerror() Sstring_utf8(strerror(errno), -1)
#elif defined(WIN32)
#define dlopen(path,flags) S_ntdlopen(path)
#define dlsym(h,s) S_ntdlsym(h,s)
#define s_dlerror() S_ntdlerror()
#else
#include <dlfcn.h>
#define s_dlerror() Sstring_utf8(dlerror(), -1)
#ifndef RTLD_NOW
#define RTLD_NOW 2
#endif /* RTLD_NOW */
#endif /* machine types */
#endif /* LOAD_SHARED_OBJECT */

/* locally defined functions */
static iptr symhash PROTO((const char *s));
static ptr lookup_static PROTO((const char *s));
static ptr lookup_dynamic PROTO((const char *s, ptr tbl));
static ptr lookup PROTO((const char *s));
static ptr remove_foreign_entry PROTO((const char *s));
static void *lookup_foreign_entry PROTO((const char *s));
static ptr foreign_entries PROTO((void));
static ptr foreign_static_table PROTO((void));
static ptr foreign_dynamic_table PROTO((void));
static ptr bvstring PROTO((const char *s));

#ifdef LOAD_SHARED_OBJECT
static void load_shared_object PROTO((const char *path));
#endif /* LOAD_SHARED_OBJECT */

#ifdef HPUX
void *proc2entry(void *f, ptr name) {
   if (((uptr)f & 2) == 0)
      if (name == NULL)
         S_error("Sforeign_symbol", "invalid entry");
      else
         S_error1("Sforeign_symbol", "invalid entry for ~s", name);
   return (void *)((uptr)f & ~0x3);
}
#endif /* HPUX */

static ptr bvstring(const char *s) {
  iptr n = strlen(s) + 1;
  ptr x = S_bytevector(n);
  memcpy(&BVIT(x, 0), s, n);
  return x;
}

/* multiplier weights each character, h = n factors in the length */
static iptr symhash(s) const char *s; {
  iptr n, h;

  h = n = strlen(s);
  while (n--) h = h * multiplier + *s++;
  return (h & 0x7fffffff) % buckets;
}

static ptr lookup_static(s) const char *s; {
  iptr b; ptr p;

  b = symhash(s);
  for (p = Svector_ref(S_G.foreign_static, b); p != Snil; p = Scdr(p))
    if (strcmp(s, (char *)&BVIT(Scar(Scar(p)),0)) == 0)
       return Scdr(Scar(p));

  return addr_to_ptr(0);
}

#ifdef LOAD_SHARED_OBJECT
#define LOOKUP_DYNAMIC
static ptr lookup_dynamic(s, tbl) const char *s; ptr tbl; {
    ptr p;

    for (p = tbl; p != Snil; p = Scdr(p)) {
#ifdef HPUX
        (void *)value = (void *)0; /* assignment to prevent compiler warning */
        shl_t handle = (shl_t)ptr_to_addr(Scar(p));

        if (shl_findsym(&handle, s, TYPE_PROCEDURE, (void *)&value) == 0)
           return addr_to_ptr(proc2entry(value, NULL));
#else /* HPUX */
        void *value;

        value = dlsym(ptr_to_addr(Scar(p)), s);
        if (value != (void *)0) return addr_to_ptr(value);
#endif /* HPUX */
    }

    return addr_to_ptr(0);
}
#endif /* LOAD_SHARED_OBJECT */

static ptr lookup(s) const char *s; {
    iptr b; ptr p;

#ifdef LOOKUP_DYNAMIC
    ptr x;

    x = lookup_dynamic(s, S_foreign_dynamic);
    if (x == addr_to_ptr(0))
#endif /* LOOKUP_DYNAMIC */

    x = lookup_static(s);
    if (x == addr_to_ptr(0)) return x;

    tc_mutex_acquire()

    b = ptrhash(x);
    for (p = Svector_ref(S_G.foreign_names, b); p != Snil; p = Scdr(p)) {
      if (Scar(Scar(p)) == x) {
        SETCDR(Scar(p),bvstring(s));
        goto quit;
      }
    }
    SETVECTIT(S_G.foreign_names, b, Scons(Scons(addr_to_ptr(x),bvstring(s)),
                                      Svector_ref(S_G.foreign_names, b)));

quit:
    tc_mutex_release()
    return x;
}

void Sforeign_symbol(s, v) const char *s; void *v; {
    iptr b; ptr x;

    tc_mutex_acquire()

#ifdef HPUX
    v = proc2entry(v,name);
#endif

    if ((x = lookup(s)) == addr_to_ptr(0)) {
        b = symhash(s);
        SETVECTIT(S_G.foreign_static, b, Scons(Scons(bvstring(s), addr_to_ptr(v)),
                                          Svector_ref(S_G.foreign_static, b)));
    } else if (ptr_to_addr(x) != v)
        S_error1("Sforeign_symbol", "duplicate symbol entry for ~s", Sstring_utf8(s, -1));

    tc_mutex_release()
}

/* like Sforeign_symbol except it silently redefines the symbol
   if it's already in S_G.foreign_static */
void Sregister_symbol(s, v) const char* s; void *v; {
  iptr b; ptr p;

  tc_mutex_acquire()

  b = symhash(s);
  for (p = Svector_ref(S_G.foreign_static, b); p != Snil; p = Scdr(p))
    if (strcmp(s, (char *)&BVIT(Scar(Scar(p)),0)) == 0) {
       INITCDR(Scar(p)) = addr_to_ptr(v);
       goto quit;
    }
  SETVECTIT(S_G.foreign_static, b, Scons(Scons(bvstring(s), addr_to_ptr(v)),
                                      Svector_ref(S_G.foreign_static, b)));

 quit:
  tc_mutex_release()
}

static ptr remove_foreign_entry(s) const char *s; {
    iptr b;
    ptr tbl, p1, p2;

    tc_mutex_acquire()

    b = symhash(s);
    tbl = S_G.foreign_static;
    p1 = Snil;
    p2 = Svector_ref(tbl, b);
    for (; p2 != Snil; p1 = p2, p2 = Scdr(p2)) {
        if (strcmp(s, (char *)&BVIT(Scar(Scar(p2)), 0)) == 0) {
            if (p1 == Snil) {
                SETVECTIT(tbl, b, Scdr(p2))
            } else {
                SETCDR(p1, Scdr(p2))
            }
            tc_mutex_release()
            return Strue;
        }
    }
    tc_mutex_release()
    return Sfalse;
}

#ifdef LOAD_SHARED_OBJECT
static void load_shared_object(path) const char *path; {
    void *handle;

    tc_mutex_acquire()

    handle = dlopen(path, RTLD_NOW);
    if (handle == (void *)NULL)
        S_error2("", "(while loading ~a) ~a", Sstring_utf8(path, -1), s_dlerror());
    S_foreign_dynamic = Scons(addr_to_ptr(handle), S_foreign_dynamic);

    tc_mutex_release()

    return;
}
#endif /* LOAD_SHARED_OBJECT */

void S_foreign_entry() {
    ptr tc = get_thread_context();
    ptr name, x, bvname;
    iptr i, n;

    name = AC0(tc);
    if (Sfixnump(name) || Sbignump(name)) {
        AC0(tc) = (ptr)Sinteger_value(name);
        return;
    }

    if (!(Sstringp(name))) {
        S_error1("foreign-procedure", "invalid foreign procedure handle ~s", name);
    }

    n = Sstring_length(name);
    bvname = S_bytevector(n + 1);
    for (i = 0; i != n; i += 1) {
      int k = Sstring_ref(name, i);
      if (k >= 256) k = '?';
      BVIT(bvname, i) = k;
    }
    BVIT(bvname, n) = 0;

    if ((x = lookup((char *)&BVIT(bvname, 0))) == addr_to_ptr(0)) {
        S_error1("foreign-procedure", "no entry for ~s", name);
    }

    AC0(tc) = x;
}

static void *lookup_foreign_entry(s) const char *s; {
  return ptr_to_addr(lookup(s));
}

static ptr foreign_entries() {
    iptr b; ptr p, entries;

    entries = Snil;

    for (b = 0; b < buckets; b++)
        for (p = Svector_ref(S_G.foreign_static, b); p != Snil; p = Scdr(p))
            entries = Scons(Sstring_utf8((char *)&BVIT(Scar(Scar(p)), 0), -1), entries);

    return entries;
}

static ptr foreign_static_table() { return S_G.foreign_static; }
#ifdef LOAD_SHARED_OBJECT
static ptr foreign_dynamic_table() { return S_foreign_dynamic; }
#else
static ptr foreign_dynamic_table() { return Sfalse; }
#endif /* LOAD_SHARED_OBJECT */

static octet *foreign_address_name(ptr addr) {
  iptr b; ptr p;

  b = ptrhash(addr);
  for (p = Svector_ref(S_G.foreign_names, b); p != Snil; p = Scdr(p))
    if (Scar(Scar(p)) == (ptr)addr)
       return &BVIT(Scdr(Scar(p)),0);

  return NULL;
}

void S_foreign_init() {
  if (S_boot_time) {
    S_protect(&S_G.foreign_static);
    S_G.foreign_static = S_vector(buckets);
    {iptr i; for (i = 0; i < buckets; i++) INITVECTIT(S_G.foreign_static,i) = Snil;}

    S_protect(&S_G.foreign_names);
    S_G.foreign_names = S_vector(buckets);
    {iptr i; for (i = 0; i < buckets; i++) INITVECTIT(S_G.foreign_names,i) = Snil;}

#ifdef LOAD_SHARED_OBJECT
    S_protect(&S_foreign_dynamic);
    S_foreign_dynamic = Snil;
    Sforeign_symbol("(cs)load_shared_object", (void *)load_shared_object);
#endif /* LOAD_SHARED_OBJECT */

    Sforeign_symbol("(cs)lookup_foreign_entry", (void *)lookup_foreign_entry);
    Sforeign_symbol("(cs)remove_foreign_entry", (void *)remove_foreign_entry);
    Sforeign_symbol("(cs)foreign_entries", (void *)foreign_entries);
    Sforeign_symbol("(cs)foreign_static_table", (void *)foreign_static_table);
    Sforeign_symbol("(cs)foreign_dynamic_table", (void *)foreign_dynamic_table);
    Sforeign_symbol("(cs)foreign_address_name", (void *)foreign_address_name);
  }

#ifdef LOAD_SHARED_OBJECT
  S_foreign_dynamic = Snil;
#endif /* LOAD_SHARED_OBJECT */
}
