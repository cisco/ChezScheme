/* prim.c
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
static void install_library_entry PROTO((ptr n, ptr x));
static void scheme_install_library_entry PROTO((void));
static void create_library_entry_vector PROTO((void));
static void install_c_entry PROTO((iptr i, ptr x));
static void create_c_entry_vector PROTO((void));
static void s_instantiate_code_object PROTO((void));
static void s_link_code_object PROTO((ptr co, ptr objs));
static IBOOL s_check_heap_enabledp PROTO((void));
static void s_enable_check_heap PROTO((IBOOL b));
static uptr s_check_heap_errors PROTO((void));

static void install_library_entry(n, x) ptr n, x; {
    if (!Sfixnump(n) || UNFIX(n) < 0 || UNFIX(n) >= library_entry_vector_size)
        S_error1("$install-library-entry", "invalid index ~s", n);
    if (!Sprocedurep(x) && !Scodep(x))
        S_error2("$install-library-entry", "invalid entry ~s for ~s", x, n);
    if (Svector_ref(S_G.library_entry_vector, UNFIX(n)) != Sfalse) {
        printf("$install-library-entry: overwriting entry for %ld\n", (long)UNFIX(n));
        fflush(stdout);
    }
    SETVECTIT(S_G.library_entry_vector, UNFIX(n), x);
    if (n == FIX(library_nonprocedure_code)) {
        S_G.nonprocedure_code = x;
        S_retrofit_nonprocedure_code();
    }
}

ptr S_lookup_library_entry(n, errorp) iptr n; IBOOL errorp; {
    ptr p;

    if (n < 0 || n >= library_entry_vector_size)
        S_error1("$lookup-library-entry", "invalid index ~s", FIX(n));
    p = Svector_ref(S_G.library_entry_vector, n);
    if (p == Sfalse && errorp)
        S_error1("$lookup-library-entry", "entry ~s uninitialized", FIX(n));
    return p;
}

static void scheme_install_library_entry() {
    ptr tc = get_thread_context();
    install_library_entry(S_get_scheme_arg(tc, 1), S_get_scheme_arg(tc, 2));
}

static void create_library_entry_vector() {
    iptr i;

    S_protect(&S_G.library_entry_vector);
    S_G.library_entry_vector = S_vector(library_entry_vector_size);
    for (i = 0; i < library_entry_vector_size; i++)
        INITVECTIT(S_G.library_entry_vector, i) = Sfalse;
}

#ifdef HPUX
#define proc2ptr(x) int2ptr((iptr)(x))
ptr int2ptr(iptr f)
{
   if ((f & 2) == 0)
      S_error("proc2ptr", "invalid C procedure");
   return (ptr)(f & ~0x3);
}
#else /* HPUX */
#define proc2ptr(x) (ptr)(iptr)(x)
#endif /* HPUX */

static void install_c_entry(i, x) iptr i; ptr x; {
    if (i < 0 || i >= c_entry_vector_size)
        S_error1("install_c_entry", "invalid index ~s", FIX(i));
    if (Svector_ref(S_G.c_entry_vector, i) != Sfalse)
        S_error1("install_c_entry", "duplicate entry for ~s", FIX(i));
    SETVECTIT(S_G.c_entry_vector, i, x);
}

ptr S_lookup_c_entry(i) iptr i; {
   ptr x;

   if (i < 0 || i >= c_entry_vector_size)
       S_error1("lookup_c_entry", "invalid index ~s", FIX(i));
   if ((x = Svector_ref(S_G.c_entry_vector, i)) == Sfalse)
       S_error1("lookup_c_entry", "uninitialized entry ~s", FIX(i));
   return x;
}

static ptr s_get_thread_context() {
  return get_thread_context();
}

static void create_c_entry_vector() {
    INT i;

    S_protect(&S_G.c_entry_vector);
    S_G.c_entry_vector = S_vector(c_entry_vector_size);

    for (i = 0; i < c_entry_vector_size; i++)
        INITVECTIT(S_G.c_entry_vector, i) = Sfalse;

    install_c_entry(CENTRY_thread_context, proc2ptr(S_G.thread_context));
    install_c_entry(CENTRY_get_thread_context, proc2ptr(s_get_thread_context));
    install_c_entry(CENTRY_handle_apply_overflood, proc2ptr(S_handle_apply_overflood));
    install_c_entry(CENTRY_handle_docall_error, proc2ptr(S_handle_docall_error));
    install_c_entry(CENTRY_handle_overflow, proc2ptr(S_handle_overflow));
    install_c_entry(CENTRY_handle_overflood, proc2ptr(S_handle_overflood));
    install_c_entry(CENTRY_handle_nonprocedure_symbol, proc2ptr(S_handle_nonprocedure_symbol));
    install_c_entry(CENTRY_thread_list, (ptr)&S_threads);
    install_c_entry(CENTRY_split_and_resize, proc2ptr(S_split_and_resize));
#ifdef PTHREADS
    install_c_entry(CENTRY_raw_collect_cond, (ptr)&S_collect_cond);
    install_c_entry(CENTRY_raw_tc_mutex, (ptr)&S_tc_mutex);
    install_c_entry(CENTRY_activate_thread, proc2ptr(S_activate_thread));
    install_c_entry(CENTRY_deactivate_thread, proc2ptr(Sdeactivate_thread));
    install_c_entry(CENTRY_unactivate_thread, proc2ptr(S_unactivate_thread));
#endif /* PTHREADS */
    install_c_entry(CENTRY_handle_values_error, proc2ptr(S_handle_values_error));
    install_c_entry(CENTRY_handle_mvlet_error, proc2ptr(S_handle_mvlet_error));
    install_c_entry(CENTRY_handle_arg_error, proc2ptr(S_handle_arg_error));
    install_c_entry(CENTRY_foreign_entry, proc2ptr(S_foreign_entry));
    install_c_entry(CENTRY_install_library_entry, proc2ptr(scheme_install_library_entry));
    install_c_entry(CENTRY_get_more_room, proc2ptr(S_get_more_room));
    install_c_entry(CENTRY_scan_remembered_set, proc2ptr(S_scan_remembered_set));
    install_c_entry(CENTRY_instantiate_code_object, proc2ptr(s_instantiate_code_object));
    install_c_entry(CENTRY_Sreturn, proc2ptr(S_return));
    install_c_entry(CENTRY_Scall_one_result, proc2ptr(S_call_one_result));
    install_c_entry(CENTRY_Scall_any_results, proc2ptr(S_call_any_results));

    for (i = 0; i < c_entry_vector_size; i++) {
#ifndef PTHREADS
      if (i == CENTRY_raw_collect_cond || i == CENTRY_raw_tc_mutex
          || i == CENTRY_activate_thread || i == CENTRY_deactivate_thread
          || i == CENTRY_unactivate_thread)
        continue;
#endif /* NOT PTHREADS */
      if (Svector_ref(S_G.c_entry_vector, i) == Sfalse) {
        fprintf(stderr, "c_entry_vector entry %d is uninitialized\n", i);
        S_abnormal_exit();
      }
    }
}

void S_prim_init() {
    if (!S_boot_time) return;

    create_library_entry_vector();
    create_c_entry_vector();

    Sforeign_symbol("(cs)fixedpathp", (void *)S_fixedpathp);
    Sforeign_symbol("(cs)bytes_allocated", (void *)S_compute_bytes_allocated);
    Sforeign_symbol("(cs)curmembytes", (void *)S_curmembytes);
    Sforeign_symbol("(cs)maxmembytes", (void *)S_maxmembytes);
    Sforeign_symbol("(cs)resetmaxmembytes", (void *)S_resetmaxmembytes);
    Sforeign_symbol("(cs)do_gc", (void *)S_do_gc);
    Sforeign_symbol("(cs)check_heap_enabledp", (void *)s_check_heap_enabledp);
    Sforeign_symbol("(cs)enable_check_heap", (void *)s_enable_check_heap);
    Sforeign_symbol("(cs)check_heap_errors", (void *)s_check_heap_errors);
    Sforeign_symbol("(cs)lookup_library_entry", (void *)S_lookup_library_entry);
    Sforeign_symbol("(cs)link_code_object", (void *)s_link_code_object);
    Sforeign_symbol("(cs)lookup_c_entry", (void *)S_lookup_c_entry);
    Sforeign_symbol("(cs)lock_object", (void *)Slock_object);
    Sforeign_symbol("(cs)unlock_object", (void *)Sunlock_object);
    Sforeign_symbol("(cs)locked_objectp", (void *)Slocked_objectp);
    Sforeign_symbol("(cs)locked_objects", (void *)S_locked_objects);
    Sforeign_symbol("(cs)maxgen", (void *)S_maxgen);
    Sforeign_symbol("(cs)set_maxgen", (void *)S_set_maxgen);
    Sforeign_symbol("(cs)minfreegen", (void *)S_minfreegen);
    Sforeign_symbol("(cs)set_minfreegen", (void *)S_set_minfreegen);
    Sforeign_symbol("(cs)enable_object_counts", (void *)S_enable_object_counts);
    Sforeign_symbol("(cs)set_enable_object_counts", (void *)S_set_enable_object_counts);
    Sforeign_symbol("(cs)object_counts", (void *)S_object_counts);
}

static void s_instantiate_code_object() {
    ptr tc = get_thread_context();
    ptr old, cookie, proc;
    ptr new, oldreloc, newreloc;
    uptr a, m, n;
    iptr i, size;

    old = S_get_scheme_arg(tc, 1);
    cookie = S_get_scheme_arg(tc, 2);
    proc = S_get_scheme_arg(tc, 3);

    tc_mutex_acquire()
    new = S_code(tc, CODETYPE(old), CODELEN(old));
    tc_mutex_release()

    oldreloc = CODERELOC(old);
    size = RELOCSIZE(oldreloc);
    newreloc = S_relocation_table(size);
    RELOCCODE(newreloc) = new;
    for (i = 0; i < size; i += 1) RELOCIT(newreloc, i) = RELOCIT(oldreloc, i);

    CODERELOC(new) = newreloc;
    CODENAME(new) = CODENAME(old);
    CODEARITYMASK(new) = CODEARITYMASK(old);
    CODEFREE(new) = CODEFREE(old);
    CODEINFO(new) = CODEINFO(old);
    CODEPINFOS(new) = CODEPINFOS(old);

    for (i = 0; i < CODELEN(old); i++) CODEIT(new,i) = CODEIT(old,i);

    m = RELOCSIZE(newreloc);
    a = 0;
    n = 0;
    while (n < m) {
        uptr entry, item_off, code_off; ptr obj;
        entry = RELOCIT(newreloc, n); n += 1;
        if (RELOC_EXTENDED_FORMAT(entry)) {
            item_off = RELOCIT(newreloc, n); n += 1;
            code_off = RELOCIT(newreloc, n); n += 1;
        } else {
            item_off = RELOC_ITEM_OFFSET(entry);
            code_off = RELOC_CODE_OFFSET(entry);
        }
        a += code_off;
        obj = S_get_code_obj(RELOC_TYPE(entry), old, a, item_off);

      /* we've seen the enemy, and he is us */
        if (obj == old) obj = new;

      /* if we find our cookie, insert proc; otherwise, insert the object
         into new to get proper adjustment of relative addresses */
        if (obj == cookie)
           S_set_code_obj("fcallable", RELOC_TYPE(entry), new, a, proc, item_off);
        else
           S_set_code_obj("fcallable", RELOC_TYPE(entry), new, a, obj, item_off);
    }
    S_flush_instruction_cache(tc);

    AC0(tc) = new;
}

static void s_link_code_object(co, objs) ptr co, objs; {
    ptr t; uptr a, m, n;

    t = CODERELOC(co);
    m = RELOCSIZE(t);
    a = 0;
    n = 0;
    while (n < m) {
        uptr entry, item_off, code_off;
        entry = RELOCIT(t, n); n += 1;
        if (RELOC_EXTENDED_FORMAT(entry)) {
            item_off = RELOCIT(t, n); n += 1;
            code_off = RELOCIT(t, n); n += 1;
        } else {
            item_off = RELOC_ITEM_OFFSET(entry);
            code_off = RELOC_CODE_OFFSET(entry);
        }
        a += code_off;
        S_set_code_obj("gc", RELOC_TYPE(entry), co, a, Scar(objs), item_off);
        objs = Scdr(objs);
    }
}

static INT s_check_heap_enabledp(void) {
  return S_checkheap;
}

static void s_enable_check_heap(IBOOL b) {
  S_checkheap = b;
}

static uptr s_check_heap_errors(void) {
  return S_checkheap_errors;
}
