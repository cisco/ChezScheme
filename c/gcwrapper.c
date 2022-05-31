/* gcwrapper.c
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
#include "popcount.h"

/* locally defined functions */
static void segment_tell(uptr seg);
static void check_heap_dirty_msg(char *msg, ptr *x);
static IBOOL dirty_listedp(seginfo *x, IGEN from_g, IGEN to_g);
static void check_dirty_space(ISPC s);
static void check_dirty(void);
static void check_locked_object(ptr p, IBOOL locked, IGEN g, IBOOL aftergc, IGEN mcg);

static IBOOL checkheap_noisy;

void S_gc_init(void) {
  IGEN g; INT i;

  S_checkheap = 0; /* 0 for disabled, 1 for enabled */
  S_checkheap_errors = 0; /* count of errors detected by checkheap */
  checkheap_noisy = 0; /* 0 for error output only; 1 for more noisy output */
  S_G.prcgeneration = static_generation;

  if (S_checkheap) {
    printf(checkheap_noisy ? "NB: check_heap is enabled and noisy\n" : "NB: check_heap_is_enabled\n");
    fflush(stdout);
  }

#ifndef WIN32
  for (g = 0; g <= static_generation; g++) {
    S_child_processes[g] = Snil;
  }
#endif /* WIN32 */

  if (!S_boot_time) return;

  for (g = 0; g <= static_generation; g++) {
    S_G.guardians[g] = Snil;
    S_G.locked_objects[g] = Snil;
    S_G.unlocked_objects[g] = Snil;
  }
  S_G.max_nonstatic_generation = 
    S_G.new_max_nonstatic_generation = 
      S_G.min_free_gen = 
        S_G.new_min_free_gen =
          S_G.min_mark_gen = default_max_nonstatic_generation;

  for (g = 0; g <= static_generation; g += 1) {
    for (i = 0; i < countof_types; i += 1) {
      S_G.countof[g][i] = 0;
      S_G.bytesof[g][i] = 0;
    }
    S_G.gctimestamp[g] = 0;
    S_G.rtds_with_counts[g] = Snil;
  }

  S_G.countof[static_generation][countof_oblist] += 1;
  S_G.bytesof[static_generation][countof_oblist] += S_G.oblist_length * sizeof(bucket *);

  S_protect(&S_G.static_id);
  S_G.static_id = S_intern((const unsigned char *)"static");

  S_protect(&S_G.countof_names);
  S_G.countof_names = S_vector(countof_types);
  for (i = 0; i < countof_types; i += 1) {
    INITVECTIT(S_G.countof_names, i) = FIX(0);
    S_G.countof_size[i] = 0;
  }
  INITVECTIT(S_G.countof_names, countof_pair) = S_intern((const unsigned char *)"pair");
    S_G.countof_size[countof_pair] = size_pair;
  INITVECTIT(S_G.countof_names, countof_symbol) = S_intern((const unsigned char *)"symbol");
    S_G.countof_size[countof_symbol] = size_symbol;
  INITVECTIT(S_G.countof_names, countof_flonum) = S_intern((const unsigned char *)"flonum");
    S_G.countof_size[countof_flonum] = size_flonum;
  INITVECTIT(S_G.countof_names, countof_closure) = S_intern((const unsigned char *)"procedure");
    S_G.countof_size[countof_closure] = 0;
  INITVECTIT(S_G.countof_names, countof_continuation) = S_intern((const unsigned char *)"continuation");
    S_G.countof_size[countof_continuation] = size_continuation;
  INITVECTIT(S_G.countof_names, countof_bignum) = S_intern((const unsigned char *)"bignum");
    S_G.countof_size[countof_bignum] = 0;
  INITVECTIT(S_G.countof_names, countof_ratnum) = S_intern((const unsigned char *)"ratnum");
    S_G.countof_size[countof_ratnum] = size_ratnum;
  INITVECTIT(S_G.countof_names, countof_inexactnum) = S_intern((const unsigned char *)"inexactnum");
    S_G.countof_size[countof_inexactnum] = size_inexactnum;
  INITVECTIT(S_G.countof_names, countof_exactnum) = S_intern((const unsigned char *)"exactnum");
    S_G.countof_size[countof_exactnum] = size_exactnum;
  INITVECTIT(S_G.countof_names, countof_box) = S_intern((const unsigned char *)"box");
    S_G.countof_size[countof_box] = size_box;
  INITVECTIT(S_G.countof_names, countof_port) = S_intern((const unsigned char *)"port");
    S_G.countof_size[countof_port] = size_port;
  INITVECTIT(S_G.countof_names, countof_code) = S_intern((const unsigned char *)"code");
    S_G.countof_size[countof_code] = 0;
  INITVECTIT(S_G.countof_names, countof_thread) = S_intern((const unsigned char *)"thread");
    S_G.countof_size[countof_thread] = size_thread;
  INITVECTIT(S_G.countof_names, countof_tlc) = S_intern((const unsigned char *)"tlc");
    S_G.countof_size[countof_tlc] = size_tlc;
  INITVECTIT(S_G.countof_names, countof_rtd_counts) = S_intern((const unsigned char *)"rtd-counts");
    S_G.countof_size[countof_rtd_counts] = size_rtd_counts;
  INITVECTIT(S_G.countof_names, countof_stack) = S_intern((const unsigned char *)"stack");
    S_G.countof_size[countof_stack] = 0;
  INITVECTIT(S_G.countof_names, countof_relocation_table) = S_intern((const unsigned char *)"reloc-table");
    S_G.countof_size[countof_relocation_table] = 0;
  INITVECTIT(S_G.countof_names, countof_weakpair) = S_intern((const unsigned char *)"weakpair");
    S_G.countof_size[countof_weakpair] = size_pair;
  INITVECTIT(S_G.countof_names, countof_vector) = S_intern((const unsigned char *)"vector");
    S_G.countof_size[countof_vector] = 0;
  INITVECTIT(S_G.countof_names, countof_string) = S_intern((const unsigned char *)"string");
    S_G.countof_size[countof_string] = 0;
  INITVECTIT(S_G.countof_names, countof_fxvector) = S_intern((const unsigned char *)"fxvector");
    S_G.countof_size[countof_fxvector] = 0;
  INITVECTIT(S_G.countof_names, countof_flvector) = S_intern((const unsigned char *)"flvector");
    S_G.countof_size[countof_flvector] = 0;
  INITVECTIT(S_G.countof_names, countof_bytevector) = S_intern((const unsigned char *)"bytevector");
    S_G.countof_size[countof_bytevector] = 0;
  INITVECTIT(S_G.countof_names, countof_locked) = S_intern((const unsigned char *)"locked");
    S_G.countof_size[countof_locked] = 0;
  INITVECTIT(S_G.countof_names, countof_guardian) = S_intern((const unsigned char *)"guardian");
    S_G.countof_size[countof_guardian] = size_guardian_entry;
  INITVECTIT(S_G.countof_names, countof_oblist) = S_intern((const unsigned char *)"oblist");
    S_G.countof_size[countof_guardian] = 0;
  INITVECTIT(S_G.countof_names, countof_ephemeron) = S_intern((const unsigned char *)"ephemeron");
    S_G.countof_size[countof_ephemeron] = size_ephemeron;
  INITVECTIT(S_G.countof_names, countof_stencil_vector) = S_intern((const unsigned char *)"stencil-vector");
    S_G.countof_size[countof_stencil_vector] = 0;
  INITVECTIT(S_G.countof_names, countof_record) = S_intern((const unsigned char *)"record");
    S_G.countof_size[countof_record] = 0;
  INITVECTIT(S_G.countof_names, countof_phantom) = S_intern((const unsigned char *)"phantom");
    S_G.countof_size[countof_phantom] = 0;
  for (i = 0; i < countof_types; i += 1) {
    if (Svector_ref(S_G.countof_names, i) == FIX(0)) {
      fprintf(stderr, "uninitialized countof_name at index %d\n", i);
      S_abnormal_exit();
    }
  }
}

IGEN S_maxgen(void) {
  return S_G.new_max_nonstatic_generation;
}

void S_set_maxgen(IGEN g) {
  if (g < 0 || g >= static_generation) {
    fprintf(stderr, "invalid maxgen %d\n", g);
    S_abnormal_exit();
  }
  if (S_G.new_min_free_gen == S_G.new_max_nonstatic_generation || S_G.new_min_free_gen > g) {
    S_G.new_min_free_gen = g;
  }
  S_G.new_max_nonstatic_generation = g;
}

IGEN S_minfreegen(void) {
  return S_G.new_min_free_gen;
}

void S_set_minfreegen(IGEN g) {
  S_G.new_min_free_gen = g;
  if (S_G.new_max_nonstatic_generation == S_G.max_nonstatic_generation) {
    S_G.min_free_gen = g;
  }
}

IGEN S_minmarkgen(void) {
  return S_G.min_mark_gen;
}

void S_set_minmarkgen(IGEN g) {
  S_G.min_mark_gen = g;
}

void S_immobilize_object(ptr x) {
  seginfo *si;

  if (FIXMEDIATE(x))
    si = NULL;
  else
    si = MaybeSegInfo(ptr_get_segment(x));
 
  if ((si != NULL) && (si->generation != static_generation)) {
    tc_mutex_acquire();

    /* Try a little to to support cancellation of segment-level
     * immobilzation --- but we don't try too hard */
    if (si->must_mark < MUST_MARK_INFINITY) {
      si->must_mark++;
      if (si->generation == 0)
        S_G.must_mark_gen0 = 1;
    }

    /* Note: for `space_new`, `must_mark` doesn't really mean all
       objects must be marked; only those in the locked list must be
       marked. Non-locked objects on `space_new` cannot be immobilized. */

    tc_mutex_release();
  }
}

void S_mobilize_object(ptr x) {
  seginfo *si;

  if (FIXMEDIATE(x))
    si = NULL;
  else
    si = MaybeSegInfo(ptr_get_segment(x));

  if ((si != NULL) && (si->generation != static_generation)) {
    tc_mutex_acquire();

    if (si->must_mark == 0)
      S_error_abort("S_mobilize_object(): object was definitely not immobilzed");

    /* See S_immobilize_object() about this vague try at canceling immobilation: */
    if (si->must_mark < MUST_MARK_INFINITY)
      --si->must_mark;
  
    tc_mutex_release();
  }
}

static IBOOL memqp(ptr x, ptr ls) {
  for (;;) {
    if (ls == Snil) return 0;
    if (Scar(ls) == x) return 1;
    ls = Scdr(ls);
  }
}

static IBOOL remove_first_nomorep(ptr x, ptr *pls, IBOOL look) {
  ptr ls;

  for (;;) {
    ls = *pls;
    if (ls == Snil) break;
    if (Scar(ls) == x) {
      ls = Scdr(ls);
      *pls = ls;
      if (look) return !memqp(x, ls);
      break;
    }
    pls = &Scdr(ls);
  }

 /* must return 0 if we don't look for more */
  return 0;
}

IBOOL Slocked_objectp(ptr x) {
  seginfo *si; IGEN g; IBOOL ans; ptr ls;

  if (FIXMEDIATE(x) || (si = MaybeSegInfo(ptr_get_segment(x))) == NULL || (g = si->generation) == static_generation) return 1;

  tc_mutex_acquire();

  ans = 0;
  for (ls = S_G.locked_objects[g]; ls != Snil; ls = Scdr(ls)) {
    if (x == Scar(ls)) {
      ans = 1;
      break;
    }
  }

  tc_mutex_release();

  return ans;
}

ptr S_locked_objects(void) {
  IGEN g; ptr ans; ptr ls;

  tc_mutex_acquire();

  ans = Snil;
  for (g = 0; g <= static_generation; INCRGEN(g)) {
    for (ls = S_G.locked_objects[g]; ls != Snil; ls = Scdr(ls)) {
      ans = Scons(Scar(ls), ans);
    }
  }

  tc_mutex_release();

  return ans;
}

void Slock_object(ptr x) {
  seginfo *si; IGEN g;

 /* weed out pointers that won't be relocated */
  if (!FIXMEDIATE(x) && (si = MaybeSegInfo(ptr_get_segment(x))) != NULL && (g = si->generation) != static_generation) {
    ptr tc = get_thread_context();
    tc_mutex_acquire();
    THREAD_GC(tc)->during_alloc += 1;
    /* immobilize */
    if (si->must_mark < MUST_MARK_INFINITY) {
      si->must_mark++;
      if (si->generation == 0)
        S_G.must_mark_gen0 = 1;
    }
   /* add x to locked list. remove from unlocked list */
    S_G.locked_objects[g] = S_cons_in(tc, (g == 0 ? space_new : space_impure), g, x, S_G.locked_objects[g]);
    if (S_G.enable_object_counts) {
      if (g != 0) S_G.countof[g][countof_pair] += 1;
    }
    (void)remove_first_nomorep(x, &S_G.unlocked_objects[g], 0);
    THREAD_GC(tc)->during_alloc -= 1;
    tc_mutex_release();
  }
}

void Sunlock_object(ptr x) {
  seginfo *si; IGEN g;

  if (!FIXMEDIATE(x) && (si = MaybeSegInfo(ptr_get_segment(x))) != NULL && (g = si->generation) != static_generation) {
    ptr tc = get_thread_context();
    tc_mutex_acquire();
    THREAD_GC(tc)->during_alloc += 1;
    /* mobilize, if we haven't lost track */
    if (si->must_mark < MUST_MARK_INFINITY)
      --si->must_mark;
   /* remove first occurrence of x from locked list. if there are no
      others, add x to unlocked list */
    if (remove_first_nomorep(x, &S_G.locked_objects[g], (si->space == space_new) && (si->generation > 0))) {
      S_G.unlocked_objects[g] = S_cons_in(tc, (g == 0 ? space_new : space_impure), g, x, S_G.unlocked_objects[g]);
      if (S_G.enable_object_counts) {
        if (g != 0) S_G.countof[g][countof_pair] += 1;
      }
    }
    THREAD_GC(tc)->during_alloc -= 1;
    tc_mutex_release();
  }
}

ptr s_help_unregister_guardian(ptr *pls, ptr tconc, ptr result) {
  ptr rep, ls;
  while ((ls = *pls) != Snil) {
    if (GUARDIANTCONC(ls) == tconc) {
      result = Scons(((rep = GUARDIANREP(ls)) == ftype_guardian_rep ? GUARDIANOBJ(ls) : rep), result);
      *pls = ls = GUARDIANNEXT(ls);
    } else {
      ls = *(pls = &GUARDIANNEXT(ls));
    }
  }
  return result;
}

ptr S_unregister_guardian(ptr tconc) {
  ptr result, tc; IGEN g;
  tc_mutex_acquire();
  tc = get_thread_context();
  /* in the interest of thread safety, gather entries only in the current thread, ignoring any others */
  result = s_help_unregister_guardian(&GUARDIANENTRIES(tc), tconc, Snil);
  /* plus, of course, any already known to the storage-management system */
  for (g = 0; g <= static_generation; INCRGEN(g)) {
    result = s_help_unregister_guardian(&S_G.guardians[g], tconc, result);
  }
  tc_mutex_release();
  return result;
}

#ifndef WIN32
void S_register_child_process(INT child) {
  tc_mutex_acquire();
  S_child_processes[0] = Scons(FIX(child), S_child_processes[0]);
  tc_mutex_release();
}
#endif /* WIN32 */

IBOOL S_enable_object_counts(void) {
  return S_G.enable_object_counts;
}

void S_set_enable_object_counts(IBOOL eoc) {
  S_G.enable_object_counts = eoc;
}

ptr S_object_counts(void) {
  IGEN grtd, g; ptr ls; iptr i; ptr outer_alist;

  tc_mutex_acquire();

  outer_alist = Snil;

 /* add rtds w/nonozero counts to the alist */
  for (grtd = 0; grtd <= static_generation; INCRGEN(grtd)) {
    for (ls = S_G.rtds_with_counts[grtd]; ls != Snil; ls = Scdr(ls)) {
      ptr rtd = Scar(ls);
      ptr counts = RECORDDESCCOUNTS(rtd);
      IGEN g;
      uptr size = size_record_inst(UNFIX(RECORDDESCSIZE(rtd)));
      ptr inner_alist = Snil;

      S_fixup_counts(counts);
      for (g = 0; g <= static_generation; INCRGEN(g)) {
        uptr count = RTDCOUNTSIT(counts, g); IGEN gcurrent = g;
        if (g == S_G.new_max_nonstatic_generation) {
          while (g < S_G.max_nonstatic_generation) {
            g += 1;
            count += RTDCOUNTSIT(counts, g);
          }
        }
        if (count != 0) inner_alist = Scons(Scons((gcurrent == static_generation ? S_G.static_id : FIX(gcurrent)), Scons(Sunsigned(count), Sunsigned(count * size))), inner_alist);
      }
      if (inner_alist != Snil) outer_alist = Scons(Scons(rtd, inner_alist), outer_alist);
    }
  }

 /* add primary types w/nonozero counts to the alist */
  for (i = 0 ; i < countof_types; i += 1) {
    if (i != countof_record) { /* covered by rtd-specific counts */
      ptr inner_alist = Snil;
      for (g = 0; g <= static_generation; INCRGEN(g)) {
        IGEN gcurrent = g;
        uptr count = S_G.countof[g][i];
        uptr bytes = S_G.bytesof[g][i];

        if (g == S_G.new_max_nonstatic_generation) {
          while (g < S_G.max_nonstatic_generation) {
            g += 1;
            /* NB: S_G.max_nonstatic_generation + 1 <= static_generation, but coverity complains about overrun */
            /* coverity[overrun-buffer-val] */
            count += S_G.countof[g][i];
            /* coverity[overrun-buffer-val] */
            bytes += S_G.bytesof[g][i];
          }
        }

        if (count != 0) {
          if (bytes == 0) bytes = count * S_G.countof_size[i];
          inner_alist = Scons(Scons((gcurrent == static_generation ? S_G.static_id : FIX(gcurrent)), Scons(Sunsigned(count), Sunsigned(bytes))), inner_alist);
        }
      }
      if (inner_alist != Snil) outer_alist = Scons(Scons(Svector_ref(S_G.countof_names, i), inner_alist), outer_alist);
    }
  }

  tc_mutex_release();

  return outer_alist;
}

IBOOL S_enable_object_backreferences(void) {
  return S_G.enable_object_backreferences;
}

void S_set_enable_object_backreferences(IBOOL eoc) {
  S_G.enable_object_backreferences = eoc;
}

ptr S_object_backreferences(void) {
  IGEN g; ptr ls = Snil;

  tc_mutex_acquire();

  for (g = S_G.max_nonstatic_generation+1; g--; )
    ls = Scons(S_G.gcbackreference[g], ls);

  tc_mutex_release();

  return ls;
}

seginfo *S_ptr_seginfo(ptr p) {
  return MaybeSegInfo(ptr_get_segment(p)); 
}

/* Scompact_heap().  Compact into as few O/S chunks as possible and
 * move objects into static generation
 */
void Scompact_heap(void) {
  ptr tc = get_thread_context();
  IBOOL eoc = S_G.enable_object_counts;
  THREAD_GC(tc)->during_alloc += 1;
  S_G.enable_object_counts = 1;
  S_gc_oce(tc, S_G.max_nonstatic_generation, static_generation, static_generation, Sfalse);
  S_G.enable_object_counts = eoc;
  THREAD_GC(tc)->during_alloc -= 1;
}

/* S_check_heap checks for various kinds of heap consistency
   It currently checks for:
       dangling references in space_impure (generation > 0) and space_pure
       extra dirty bits
       missing dirty bits

   Some additional things it should check for but doesn't:
       correct dirty bytes, following sweep_dirty conventions
       dangling references in in space_code and space_continuation
       dirty bits set for non-impure segments outside of generation zero
       proper chaining of segments of a space and generation:
          chains contain all and only the appropriate segments

   If noisy is nonzero, additional comments may be included in the output
*/

#ifdef __MINGW32__
# include <inttypes.h>
# define PHtx "%" PRIxPTR
# define Ptd "%" PRIdPTR
#else
# define PHtx "%#tx"
# define Ptd "%td"
#endif

static void segment_tell(uptr seg) {
  seginfo *si;
  ISPC s, s1;
  static char *spacename[max_space+1] = { alloc_space_names };

  printf("segment "PHtx"", (ptrdiff_t)seg);
  if ((si = MaybeSegInfo(seg)) == NULL) {
    printf(" out of heap bounds\n");
  } else {
    printf(" si=%p", si);
    printf(" generation=%d", si->generation);
    s = si->space;
    s1 = si->space;
    if (s1 < 0 || s1 > max_space)
      printf(" space-bogus (%d)", s);
    else {
      printf(" space-%s", spacename[s1]);
      if (si->old_space) printf(" oldspace");
      if (si->must_mark) printf(" mustmark");
      if (si->marked_mask) printf(" marked");
    }
    printf("\n");
  }
  fflush(stdout);
}

void S_ptr_tell(ptr p) {
  segment_tell(ptr_get_segment(p));
}

void S_addr_tell(ptr p) {
  segment_tell(addr_get_segment(p));
}

static int maybe_inexactnum_marked(ptr p, seginfo *psi) {
  /* test for possible flonum within marked inexactnum */
  uptr real_delta = (uptr)TO_PTR(&INEXACTNUM_REAL_PART(TYPE((ptr)0, type_typed_object)));
  uptr imag_delta = (uptr)TO_PTR(&INEXACTNUM_IMAG_PART(TYPE((ptr)0, type_typed_object)));
  ptr maybe_pr = TYPE((uptr)UNTYPE(p, type_flonum) - real_delta, type_typed_object);
  ptr maybe_pi = TYPE((uptr)UNTYPE(p, type_flonum) - imag_delta, type_typed_object);

  if ((MaybeSegInfo(ptr_get_segment(maybe_pr)) == psi)
      && (psi->marked_mask[segment_bitmap_byte(maybe_pr)] & segment_bitmap_bit(maybe_pr))) {
    return 1;
  }

  if ((MaybeSegInfo(ptr_get_segment(maybe_pi)) == psi)
      && (psi->marked_mask[segment_bitmap_byte(maybe_pi)] & segment_bitmap_bit(maybe_pi))) {
    return 1;
  }

  return 0;
}

static void check_pointer(ptr *pp, IBOOL address_is_meaningful, IBOOL is_reference, ptr base, uptr seg, ISPC s, IBOOL aftergc) {
  ptr p = *pp;

  if (is_reference)
    p = S_maybe_reference_to_object(p);

  if (!FIXMEDIATE(p)) {
    seginfo *psi = MaybeSegInfo(ptr_get_segment(p));
    if (psi != NULL) {
      if ((psi->space == space_empty)
          || psi->old_space
          || (psi->marked_mask && !(psi->marked_mask[segment_bitmap_byte(p)] & segment_bitmap_bit(p))
              /* corner case: a continuation in space_count_pure can refer to code via CLOSENTRY
                 where the entry point doesn't have a mark bit: */
              && !((s == space_count_pure) && (psi->space == space_code))
              /* another corner case: a flonum might be inside a marked inexactnum */
              && !(Sflonump(p)
                   && ((psi->space == space_data) || (psi->space == space_new))
                   && maybe_inexactnum_marked(p, psi)))) {
        S_checkheap_errors += 1;
        printf("!!! dangling reference at %s"PHtx" to "PHtx"%s\n",
               (address_is_meaningful ? "" : "insideof "),
               (ptrdiff_t)(address_is_meaningful ? pp : TO_VOIDP(base)),
               (ptrdiff_t)p, (aftergc ? " after gc" : ""));
        printf("from: "); segment_tell(seg);
        printf("to:   "); segment_tell(ptr_get_segment(p));
        {
          ptr l;
          for (l = S_G.locked_objects[psi->generation]; l != Snil; l = Scdr(l))
            if (Scar(l) == p)
              printf(" in locked\n");
          for (l = S_G.unlocked_objects[psi->generation]; l != Snil; l = Scdr(l))
            if (Scar(l) == p)
              printf(" in unlocked\n");
        }
      }

      if (address_is_meaningful) {
        seginfo *ppsi = MaybeSegInfo(addr_get_segment(TO_PTR(pp)));
        if ((ppsi != NULL)
            && (ppsi->generation > psi->generation)
            /* space_data includes stacks, which are always swept */
            && (ppsi->space != space_data)) {
          uptr card = (uptr)TO_PTR(pp) >> card_offset_bits;
          uptr cardno = card & ((1 << segment_card_offset_bits) - 1);
          if (psi->generation < ppsi->dirty_bytes[cardno]) {
            S_checkheap_errors += 1;
            check_heap_dirty_msg("!!! INVALID", pp);
          }
        }
      }
    }
  }
}

static void check_bignum(ptr p) {
  if (!Sbignump(p))
    printf("!!! not a bignum %p\n", TO_VOIDP(p));
}

#include "heapcheck.inc"

#ifdef PTHREADS

static ptr *find_nl(ptr *pp1, ISPC s, IGEN g) {
  seginfo *si;
  thread_gc *tgc;

  si = SegInfo(addr_get_segment(TO_PTR(pp1)));
  tgc = si->creator;
  return TO_VOIDP(tgc->next_loc[g][s]);
}


# define FIND_NL(pp1, pp2, s, g) find_nl(pp1, s, g)

#else

/* no `creator` field, so we search the slow way */
static ptr *find_nl(ptr *pp1, ptr *pp2, ISPC s, IGEN g) {
  ptr *nl, ls;

  for (ls = S_threads; ls != Snil; ls = Scdr(ls)) {
    ptr t_tc = (ptr)THREADTC(Scar(ls));
    nl = TO_VOIDP(THREAD_GC(t_tc)->next_loc[g][s]);
    if (pp1 <= nl && nl < pp2)
      return nl;
  }

  return NULL;
}
# define FIND_NL(pp1, pp2, s, g) find_nl(pp1, pp2, s, g)

#endif

static void check_heap_dirty_msg(char *msg, ptr *x) {
    INT d; seginfo *si;

    si = SegInfo(addr_get_segment(TO_PTR(x)));
    d = (INT)(((uptr)TO_PTR(x) >> card_offset_bits) & ((1 << segment_card_offset_bits) - 1));
    printf("%s dirty byte %d found in segment "PHtx", card %d at "PHtx"\n", msg, si->dirty_bytes[d], (ptrdiff_t)(si->number), d, (ptrdiff_t)x);
    printf("from "); segment_tell(addr_get_segment(TO_PTR(x)));
    printf("to   "); segment_tell(ptr_get_segment(*x));
}

void S_check_heap(IBOOL aftergc, IGEN mcg) {
  uptr seg; INT d; ISPC s; IGEN g; IDIRTYBYTE dirty; IBOOL found_eos; IGEN pg;
  ptr p, *pp1, *pp2, *nl;
  iptr i, for_code;
  uptr empty_segments = 0;
  uptr used_segments = 0;
  uptr static_segments = 0;
  uptr nonstatic_segments = 0;

  check_dirty();

  {
    ptr ls;
    for (ls = S_threads; ls != Snil; ls = Scdr(ls)) {
      ptr t_tc = (ptr)THREADTC(Scar(ls));
      for (s = 0; s <= max_real_space; s += 1) {
        for (g = 0; g <= static_generation; INCRGEN(g)) {
          thread_gc *tgc = THREAD_GC(t_tc);
          if ((tgc->next_loc[g][s] == (ptr)0) != (tgc->base_loc[g][s] == (ptr)0)) {
            S_checkheap_errors += 1;
            printf("!!! inconsistent thread NEXT %p and BASE %p\n",
                   TO_VOIDP(tgc->next_loc[g][s]), TO_VOIDP(tgc->base_loc[g][s]));
          }
        }
      }
    }
  }

  for (for_code = 0; for_code < 2; for_code++) {
    for (i = PARTIAL_CHUNK_POOLS; i >= -1; i -= 1) {
      chunkinfo *chunk = (i == -1
                          ? (for_code ? S_code_chunks_full : S_chunks_full)
                          : (for_code ? S_code_chunks[i] : S_chunks[i]));
      while (chunk != NULL) {
        seginfo *si = chunk->unused_segs;
        iptr count = 0;
        while(si) {
          count += 1;
          if (si->space != space_empty) {
            S_checkheap_errors += 1;
            printf("!!! unused segment has unexpected space\n");
          }
          si = si->next;
        }
        if ((chunk->segs - count) != chunk->nused_segs) {
          S_checkheap_errors += 1;
          printf("!!! unexpected used segs count "Ptd" with "Ptd" total segs and "Ptd" segs on the unused list\n",
                 (ptrdiff_t)chunk->nused_segs, (ptrdiff_t)chunk->segs, (ptrdiff_t)count);
        }
        used_segments += chunk->nused_segs;
        empty_segments += count;
        chunk = chunk->next;
      }
    }
  }

  for (s = 0; s <= max_real_space; s += 1) {
    seginfo *si;
    for (g = 0; g <= S_G.max_nonstatic_generation; INCRGEN(g)) {
      for (si = S_G.occupied_segments[g][s]; si != NULL; si = si->next) {
        if (si->generation != g) {
          S_checkheap_errors += 1;
          printf("!!! segment in wrong occupied_segments list\n");
        }
        nonstatic_segments += 1;
      }
    }
    for (si = S_G.occupied_segments[static_generation][s]; si != NULL; si = si->next) {
      static_segments += 1;
    }
  }

  if (used_segments != nonstatic_segments + static_segments) {
    S_checkheap_errors += 1;
    printf("!!! found "PHtx" used segments and "PHtx" occupied segments\n",
        (ptrdiff_t)used_segments,
        (ptrdiff_t)(nonstatic_segments + static_segments));
  }

  if (S_G.number_of_nonstatic_segments != nonstatic_segments) {
    S_checkheap_errors += 1;
    printf("!!! S_G.number_of_nonstatic_segments "PHtx" is different from occupied number "PHtx"\n",
        (ptrdiff_t)S_G.number_of_nonstatic_segments,
        (ptrdiff_t)nonstatic_segments);
  }

  if (S_G.number_of_empty_segments != empty_segments) {
    S_checkheap_errors += 1;
    printf("!!! S_G.number_of_empty_segments "PHtx" is different from unused number "PHtx"\n",
        (ptrdiff_t)S_G.number_of_empty_segments,
        (ptrdiff_t)empty_segments);
  }

  for (for_code = 0; for_code < 2; for_code++) {
    for (i = PARTIAL_CHUNK_POOLS; i >= -1; i -= 1) {
      chunkinfo *chunk = (i == -1
                          ? (for_code ? S_code_chunks_full : S_chunks_full)
                          : (for_code ? S_code_chunks[i] : S_chunks[i]));
      while (chunk != NULL) {
        uptr nsegs; seginfo *si;
        for (si = &chunk->sis[0], nsegs = chunk->segs; nsegs != 0; nsegs -= 1, si += 1) {
          seginfo *recorded_si; uptr recorded_seg;
          if ((seg = si->number) != (recorded_seg = (chunk->base + chunk->segs - nsegs))) {
            S_checkheap_errors += 1;
            printf("!!! recorded segment number "PHtx" differs from actual segment number "PHtx"", (ptrdiff_t)seg, (ptrdiff_t)recorded_seg);
          }
          if ((recorded_si = SegInfo(seg)) != si) {
            S_checkheap_errors += 1;
            printf("!!! recorded segment "PHtx" seginfo "PHtx" differs from actual seginfo "PHtx"", (ptrdiff_t)seg, (ptrdiff_t)recorded_si, (ptrdiff_t)si);
          }
          s = si->space;
          g = si->generation;

          if (si->use_marks)
            printf("!!! use_marks set on generation %d segment "PHtx"\n", g, (ptrdiff_t)seg);

          if (s == space_new) {
            if (g != 0 && !si->marked_mask) {
              S_checkheap_errors += 1;
              printf("!!! unexpected generation %d segment "PHtx" in space_new\n", g, (ptrdiff_t)seg);
            }
          } else if (s == space_impure || s == space_symbol || s == space_pure || s == space_weakpair || s == space_ephemeron
                     || s == space_immobile_impure || s == space_count_pure || s == space_count_impure || s == space_closure
                     || s == space_pure_typed_object || s == space_continuation || s == space_port || s == space_code
                     || s == space_impure_record || s == space_impure_typed_object || s == space_reference_array) {
            ptr start;
          
            /* check for dangling references */
            pp1 = TO_VOIDP(build_ptr(seg, 0));
            pp2 = TO_VOIDP(build_ptr(seg + 1, 0));

            nl = FIND_NL(pp1, pp2, s, g);
            if (pp1 <= nl && nl < pp2) pp2 = nl;

            if (s == space_pure_typed_object || s == space_port || s == space_code
                || s == space_impure_record || s == space_impure_typed_object) {
              /* only check this segment for objects that start on it */
              uptr before_seg = seg;

              /* Back up over segments for the same space and generation: */
              while (1) {
                seginfo *before_si = MaybeSegInfo(before_seg-1);
                if (!before_si
                    || (before_si->space != si->space)
                    || (before_si->generation != si->generation)
                    || ((before_si->marked_mask == NULL) != (si->marked_mask == NULL)))
                  break;
                before_seg--;
              }

              /* Move forward to reach `seg` again: */
              start = build_ptr(before_seg, 0);
              while (before_seg != seg) {
                ptr *before_pp2, *before_nl;

                before_pp2 = TO_VOIDP(build_ptr(before_seg + 1, 0));
                if ((ptr *)TO_VOIDP(start) > before_pp2) {
                  /* skipped to a further segment */
                  before_seg++;
                } else {
                  before_nl = FIND_NL(TO_VOIDP(start), before_pp2, s, g);
                  if (((ptr*)TO_VOIDP(start)) <= before_nl && before_nl < before_pp2) {
                    /* this segment ends, so move to next segment */
                    before_seg++;
                    if (s == space_code) {
                      /* in the case of code, it's possible for a whole segment to
                         go unused if a large code object didn't fit; give up, just in case */
                      start = build_ptr(seg+1, 0);
                    } else {
                      start = build_ptr(before_seg, 0);
                    }
                  } else {
                    seginfo *before_si = MaybeSegInfo(before_seg);
                    while (((ptr *)TO_VOIDP(start)) < before_pp2) {
                      if (before_si->marked_mask) {
                        if (before_si->marked_mask[segment_bitmap_byte(start)] & segment_bitmap_bit(start)) {
                          start = (ptr)((uptr)start + size_object(TYPE(start, type_typed_object)));
                        } else {
                          /* skip past unmarked */
                          start = (ptr)((uptr)start + byte_alignment);
                        }
                      } else {
                        if (*(ptr *)TO_VOIDP(start) == forward_marker) {
                          /* this segment ends, so move to next segment */
                          if (s == space_code) {
                            start = build_ptr(seg+1, 0);
                          } else {
                            start = build_ptr(before_seg+1, 0);
                          }
                        } else {
                          start = (ptr)((uptr)start + size_object(TYPE(start, type_typed_object)));
                        }
                      }
                    }
                    before_seg++;
                  }
                }
              }

              if (((ptr *)TO_VOIDP(start)) >= pp2) {
                /* previous object extended past the segment */
              }  else {
                pp1 = TO_VOIDP(start);
                while (pp1 < pp2) {
                  if (si->marked_mask) {
                    if (si->marked_mask[segment_bitmap_byte(TO_PTR(pp1))] & segment_bitmap_bit(TO_PTR(pp1))) {
                      p = TYPE(TO_PTR(pp1), type_typed_object);
                      check_object(p, seg, s, aftergc);
                      pp1 = TO_VOIDP((ptr)((uptr)TO_PTR(pp1) + size_object(p)));
                    } else {
                      /* skip past unmarked */
                      pp1 = TO_VOIDP((ptr)((uptr)TO_PTR(pp1) + byte_alignment));
                    }
                  } else {
                    if (*pp1 == forward_marker)
                      break;
                    else {
                      p = TYPE(TO_PTR(pp1), type_typed_object);
                      check_object(p, seg, s, aftergc);
                      pp1 = TO_VOIDP((ptr)((uptr)TO_PTR(pp1) + size_object(p)));
                    }
                  }
                }
              }
            } else if (s == space_continuation) {
              while (pp1 < pp2) {
                if (*pp1 == forward_marker)
                  break;
                if (!si->marked_mask || (si->marked_mask[segment_bitmap_byte(TO_PTR(pp1))] & segment_bitmap_bit(TO_PTR(pp1)))) {
                  p = TYPE(TO_PTR(pp1), type_closure);
                  check_object(p, seg, s, aftergc);
                }
                pp1 = TO_VOIDP((ptr)((uptr)TO_PTR(pp1) + size_continuation));
              }
            } else {
              while (pp1 < pp2) {
                if (!si->marked_mask || (si->marked_mask[segment_bitmap_byte(TO_PTR(pp1))] & segment_bitmap_bit(TO_PTR(pp1)))) {
                  int a;
                  for (a = 0; (a < ptr_alignment) && (pp1 < pp2); a++) {
#define             in_ephemeron_pair_part(pp1, seg) ((((uptr)TO_PTR(pp1) - (uptr)build_ptr(seg, 0)) % size_ephemeron) < size_pair)
                    if ((s == space_ephemeron) && !in_ephemeron_pair_part(pp1, seg)) {
                      /* skip non-pair part of ephemeron */
                    } else {
                      p = *pp1;
                      if (!si->marked_mask && (p == forward_marker)) {
                        pp1 = pp2; /* break out of outer loop */
                        break;
                      } else {
                        check_pointer(pp1, 1, (s == space_reference_array), (ptr)0, seg, s, aftergc);
                      }
                    }
                    pp1 += 1;
                  }
                } else
                  pp1 += ptr_alignment;
              }
            }

            /* further verify that dirty bits are set appropriately; only handles some spaces
               to make sure that the dirty byte is not unnecessarily approximate, but we have also
               checked dirty bytes already via `check_pointer` */
            if (s == space_impure || s == space_symbol || s == space_weakpair || s == space_ephemeron
                || s == space_immobile_impure || s == space_closure) {
              found_eos = 0;
              pp2 = pp1 = TO_VOIDP(build_ptr(seg, 0));
              for (d = 0; d < cards_per_segment; d += 1) {
                if (found_eos) {
                  if (si->dirty_bytes[d] != 0xff) {
                    S_checkheap_errors += 1;
                    printf("!!! Dirty byte set past end-of-segment for segment "PHtx", card %d\n", (ptrdiff_t)seg, d);
                    segment_tell(seg);
                  }
                  continue;
                }

                pp2 += bytes_per_card / sizeof(ptr);
                if (pp1 <= nl && nl < pp2) {
                  found_eos = 1;
                  pp2 = nl;
                }

#ifdef DEBUG
                printf("pp1 = "PHtx", pp2 = "PHtx", nl = "PHtx"\n", (ptrdiff_t)pp1, (ptrdiff_t)pp2, (ptrdiff_t)nl);
                fflush(stdout);
#endif

                dirty = 0xff;
                while (pp1 < pp2) {
                  if (!si->marked_mask || (si->marked_mask[segment_bitmap_byte(TO_PTR(pp1))] & segment_bitmap_bit(TO_PTR(pp1)))) {
                    int a;
                    for (a = 0; (a < ptr_alignment) && (pp1 < pp2); a++) {
                      if ((s == space_ephemeron) && !in_ephemeron_pair_part(pp1, seg)) {
                        /* skip non-pair part of ephemeron */
                      } else {
                        p = *pp1;
                      
                        if (p == forward_marker) {
                          found_eos = 1;
                          pp1 = pp2;
                          break;
                        } else if (!FIXMEDIATE(p)) {
                          seginfo *psi = MaybeSegInfo(ptr_get_segment(p));
                          if ((psi != NULL) && ((pg = psi->generation) < g)) {
                            if (pg < dirty) dirty = pg;
                            if (si->dirty_bytes[d] > pg) {
                              S_checkheap_errors += 1;
                              check_heap_dirty_msg("!!! INVALID", pp1);
                            } else if (checkheap_noisy)
                              check_heap_dirty_msg("... ", pp1);
                          }
                        }
                      }
                      pp1 += 1;
                    }
                  } else {
                    pp1 += ptr_alignment;
                  }
                }
              
                if (checkheap_noisy && si->dirty_bytes[d] < dirty) {
                  /* sweep_dirty won't sweep, and update dirty byte, for
                     cards with dirty pointers to segments older than the
                     maximum copied generation, so we can get legitimate
                     conservative dirty bytes even after gc */
                  printf("... Conservative dirty byte %x (%x) %sfor segment "PHtx" card %d ",
                         si->dirty_bytes[d], dirty,
                         (aftergc ? "after gc " : ""),
                         (ptrdiff_t)seg, d);
                  segment_tell(seg);
                }
              }
            } else {
              /* at least check that no dirty bytes are set beyond the end of the segment */
              if (pp2 < (ptr *)TO_VOIDP(build_ptr(seg + 1, 0))) {
                uptr card = (uptr)TO_PTR(pp2) >> card_offset_bits;
                int d = (int)(card & ((1 << segment_card_offset_bits) - 1));

                for (d++; d < cards_per_segment; d++) {
                  if (si->dirty_bytes[d] != 0xff) {
                    S_checkheap_errors += 1;
                    printf("!!! Dirty byte set past end-of-segment for segment "PHtx", card %d\n", (ptrdiff_t)seg, d);
                    segment_tell(seg);
                  }
                }
              }
            }
          }
          if (aftergc
              && (s != space_empty)
              && (g == 0
                  || (s != space_new && s != space_impure && s != space_symbol && s != space_port && s != space_weakpair && s != space_ephemeron
                      && s != space_impure_record && s != space_impure_typed_object
                      && s != space_immobile_impure && s != space_count_impure && s != space_closure && s != space_reference_array))) {
            for (d = 0; d < cards_per_segment; d += 1) {
              if (si->dirty_bytes[d] != 0xff) {
                S_checkheap_errors += 1;
                printf("!!! Unnecessary dirty byte %x (%x) after gc for segment "PHtx" card %d ",
                       si->dirty_bytes[d], 0xff, (ptrdiff_t)(si->number), d);
                segment_tell(seg);
              }
            }
          }
        }
        chunk = chunk->next;
      }
    }
  }

  {
    for (g = 0; g <= S_G.max_nonstatic_generation; INCRGEN(g)) {
      ptr l;
      for (l = S_G.locked_objects[g]; l != Snil; l = Scdr(l))
        check_locked_object(Scar(l), 1, g, aftergc, mcg);
      for (l = S_G.unlocked_objects[g]; l != Snil; l = Scdr(l))
        check_locked_object(Scar(l), 0, g, aftergc, mcg);
    }
  }

  if (S_checkheap_errors) {
    printf("heap check failed%s\n", (aftergc ? " after gc" : ""));
    abort();
  }
}

static IBOOL dirty_listedp(seginfo *x, IGEN from_g, IGEN to_g) {
  seginfo *si = DirtySegments(from_g, to_g);
  while (si != NULL) {
    if (si == x) return 1;
    si = si->dirty_next;
  }
  return 0;
}

static void check_dirty_space(ISPC s) {
  IGEN from_g, to_g, min_to_g; INT d; seginfo *si;

  for (from_g = 0; from_g <= static_generation; from_g += 1) {
    for (si = S_G.occupied_segments[from_g][s]; si != NULL; si = si->next) {
      min_to_g = 0xff;
      for (d = 0; d < cards_per_segment; d += 1) {
        to_g = si->dirty_bytes[d];
        if (to_g != 0xff) {
          if (to_g < min_to_g) min_to_g = to_g;
          if (from_g == 0) {
            S_checkheap_errors += 1;
            printf("!!! (check_dirty): space %d, generation %d segment "PHtx" card %d is marked dirty\n", s, from_g, (ptrdiff_t)(si->number), d);
          }
        }
      }
      if (min_to_g != si->min_dirty_byte) {
        S_checkheap_errors += 1;
        printf("!!! (check_dirty): space %d, generation %d segment "PHtx" min_dirty_byte is %d while actual min is %d\n",  s, from_g, (ptrdiff_t)(si->number), si->min_dirty_byte, min_to_g);
        segment_tell(si->number);
      } else if (min_to_g != 0xff) {
        if (!dirty_listedp(si, from_g, min_to_g)) {
          S_checkheap_errors += 1;
          printf("!!! (check_dirty): space %d, generation %d segment "PHtx" is marked dirty but not in dirty-segment list\n", s, from_g, (ptrdiff_t)(si->number));
          segment_tell(si->number);
        }
      }
    }
  }
}

static void check_dirty(void) {
  IGEN from_g, to_g; seginfo *si;

  for (from_g = 1; from_g <= static_generation; from_g = from_g == S_G.max_nonstatic_generation ? static_generation : from_g + 1) {
    for (to_g = 0; (from_g == static_generation) ? (to_g <= S_G.max_nonstatic_generation) : (to_g < from_g); to_g += 1) {
      si = DirtySegments(from_g, to_g);
      if (from_g > S_G.max_nonstatic_generation && from_g != static_generation) {
        if (si != NULL) {
          S_checkheap_errors += 1;
          printf("!!! (check_dirty): unexpected nonempty from-generation %d, to-generation %d dirty segment list\n", from_g, to_g);
        }
      } else {
        while (si != NULL) {
          ISPC s = si->space;
          IGEN g = si->generation;
          IGEN mingval = si->min_dirty_byte;
          if (g != from_g) {
            S_checkheap_errors += 1;
            printf("!!! (check_dirty): generation %d segment "PHtx" in %d -> %d dirty list\n", g, (ptrdiff_t)(si->number), from_g, to_g);
          }
          if (mingval != to_g) {
            S_checkheap_errors += 1;
            printf("!!! (check_dirty): dirty byte = %d for segment "PHtx" in %d -> %d dirty list\n", mingval, (ptrdiff_t)(si->number), from_g, to_g);
          }
          if (s != space_new && s != space_impure && s != space_count_impure && s != space_symbol && s != space_port
              && s != space_impure_record && s != space_impure_typed_object && s != space_immobile_impure 
              && s != space_weakpair && s != space_ephemeron && s != space_closure && s != space_reference_array) {
            S_checkheap_errors += 1;
            printf("!!! (check_dirty): unexpected space %d for dirty segment "PHtx"\n", s, (ptrdiff_t)(si->number));
          }
          si = si->dirty_next;
        }
      }
    }
  }

  check_dirty_space(space_impure);
  check_dirty_space(space_symbol);
  check_dirty_space(space_port);
  check_dirty_space(space_impure_record);
  check_dirty_space(space_weakpair);
  check_dirty_space(space_ephemeron);
  check_dirty_space(space_immobile_impure);
  check_dirty_space(space_reference_array);

  fflush(stdout);
}

static void check_locked_object(ptr p, IBOOL locked, IGEN g, IBOOL aftergc, IGEN mcg)
{
  const char *what = (locked ? "locked" : "unlocked");
  seginfo *psi = MaybeSegInfo(ptr_get_segment(p));
  if (!psi) {
    S_checkheap_errors += 1;
    printf("!!! generation %d %s object has no segment: %p\n", g, what, TO_VOIDP(p));
  } else {
    if (psi->generation != g) {
      S_checkheap_errors += 1;
      printf("!!! generation %d %s object in generation %d segment: %p\n", g, what, psi->generation, TO_VOIDP(p));
    }
    if (!psi->must_mark && locked) {
      S_checkheap_errors += 1;
      printf("!!! generation %d %s object not on must-mark page: %p\n", g, what, TO_VOIDP(p));
    }
    if (!psi->marked_mask) {
      if (aftergc && (psi->generation <= mcg)) {
        S_checkheap_errors += 1;
        printf("!!! %s object not in marked segment: %p\n", what, TO_VOIDP(p));
        printf(" in: "); segment_tell(psi->number);
      }
    } else if (!(psi->marked_mask[segment_bitmap_byte(p)] & segment_bitmap_bit(p))) {
      S_checkheap_errors += 1;
      printf("!!! generation %d %s object not marked: %p\n", g, what, TO_VOIDP(p));
    }
  }
}

void S_fixup_counts(ptr counts) {
  IGEN g; U64 timestamp;

  timestamp = RTDCOUNTSTIMESTAMP(counts);
  for (g = 0; g <= static_generation; INCRGEN(g)) {
    if (timestamp >= S_G.gctimestamp[g]) break;
    RTDCOUNTSIT(counts, g) = 0;
  }
  RTDCOUNTSTIMESTAMP(counts) = S_G.gctimestamp[0];
}

ptr S_do_gc(IGEN max_cg, IGEN min_tg, IGEN max_tg, ptr count_roots) {
  ptr tc = get_thread_context();
  ptr code, result;

  code = CP(tc);
  if (Sprocedurep(code)) code = CLOSCODE(code);
  Slock_object(code);

 /* Scheme side grabs mutex before calling S_do_gc */
  THREAD_GC(tc)->during_alloc += 1;

  if (S_G.new_max_nonstatic_generation > S_G.max_nonstatic_generation) {
    S_G.min_free_gen = S_G.new_min_free_gen;
    S_G.max_nonstatic_generation = S_G.new_max_nonstatic_generation;
  }

  if (max_tg == max_cg && max_cg == S_G.new_max_nonstatic_generation && max_cg < S_G.max_nonstatic_generation) {
    IGEN new_g, old_g, from_g, to_g; ISPC s; seginfo *si, *nextsi, *tail;
   /* reducing max_nonstatic_generation */
    new_g = S_G.new_max_nonstatic_generation;
    old_g = S_G.max_nonstatic_generation;
   /* first, collect everything to old_g, ignoring min_tg */
    result = S_gc(tc, old_g, old_g, old_g, count_roots);
   /* now transfer old_g info to new_g, and clear old_g info */
    S_G.bytes_of_generation[new_g] = S_G.bytes_of_generation[old_g]; S_G.bytes_of_generation[old_g] = 0;
    for (s = 0; s <= max_real_space; s += 1) {
      S_G.bytes_of_space[new_g][s] = S_G.bytes_of_space[old_g][s]; S_G.bytes_of_space[old_g][s] = 0;
      S_G.occupied_segments[new_g][s] = S_G.occupied_segments[old_g][s]; S_G.occupied_segments[old_g][s] = NULL;
      for (si = S_G.occupied_segments[new_g][s]; si != NULL; si = si->next) {
        si->generation = new_g;
      }
      {
        ptr ls;
        for (ls = S_threads; ls != Snil; ls = Scdr(ls)) {
          ptr t_tc = (ptr)THREADTC(Scar(ls));
          thread_gc *tgc = THREAD_GC(t_tc);
          tgc->base_loc[new_g][s] = tgc->base_loc[old_g][s]; tgc->base_loc[old_g][s] = (ptr)0;
          tgc->next_loc[new_g][s] = tgc->next_loc[old_g][s]; tgc->next_loc[old_g][s] = (ptr)0;
          tgc->bytes_left[new_g][s] = tgc->bytes_left[old_g][s]; tgc->bytes_left[old_g][s] = 0;
          tgc->sweep_loc[new_g][s] = tgc->sweep_loc[old_g][s]; tgc->sweep_loc[old_g][s] = 0;
          tgc->sweep_next[new_g][s] = tgc->sweep_next[old_g][s]; tgc->sweep_next[old_g][s] = NULL;
        }
      }
    }
    S_G.guardians[new_g] = S_G.guardians[old_g]; S_G.guardians[old_g] = Snil;
    S_G.locked_objects[new_g] = S_G.locked_objects[old_g]; S_G.locked_objects[old_g] = Snil;
    S_G.unlocked_objects[new_g] = S_G.unlocked_objects[old_g]; S_G.unlocked_objects[old_g] = Snil;
    S_G.buckets_of_generation[new_g] = S_G.buckets_of_generation[old_g]; S_G.buckets_of_generation[old_g] = NULL;
    if (S_G.enable_object_counts) {
      INT i; ptr ls;
      for (i = 0; i < countof_types; i += 1) {
        S_G.countof[new_g][i] = S_G.countof[old_g][i]; S_G.countof[old_g][i] = 0;
        S_G.bytesof[new_g][i] = S_G.bytesof[old_g][i]; S_G.bytesof[old_g][i] = 0;
      }
      S_G.rtds_with_counts[new_g] = S_G.rtds_with_counts[old_g]; S_G.rtds_with_counts[old_g] = Snil;
      for (ls = S_G.rtds_with_counts[new_g]; ls != Snil; ls = Scdr(ls)) {
        ptr counts = RECORDDESCCOUNTS(Scar(ls));
        RTDCOUNTSIT(counts, new_g) = RTDCOUNTSIT(counts, old_g); RTDCOUNTSIT(counts, old_g) = 0;
      }
      for (ls = S_G.rtds_with_counts[static_generation]; ls != Snil; ls = Scdr(ls)) {
        ptr counts = RECORDDESCCOUNTS(Scar(ls));
        RTDCOUNTSIT(counts, new_g) = RTDCOUNTSIT(counts, old_g); RTDCOUNTSIT(counts, old_g) = 0;
      }
    }
#ifndef WIN32
    S_child_processes[new_g] = S_child_processes[old_g];
#endif

    /* change old_g dirty bytes in static generation to new_g; splice list of old_g
       seginfos onto front of new_g seginfos */
    for (from_g = 1; from_g <= static_generation; INCRGEN(from_g)) {
      for (to_g = 0; (from_g == static_generation) ? (to_g <= S_G.max_nonstatic_generation) : (to_g < from_g); to_g += 1) {
        if ((si = DirtySegments(from_g, to_g)) != NULL) {
          if (from_g == old_g) {
            DirtySegments(from_g, to_g) = NULL;
            DirtySegments(new_g, to_g) = si;
            si->dirty_prev = &DirtySegments(new_g, to_g);
          } else if (from_g == static_generation) {
            if (to_g == old_g) {
              DirtySegments(from_g, to_g) = NULL;
              tail = DirtySegments(from_g, new_g);
              DirtySegments(from_g, new_g) = si;
              si->dirty_prev = &DirtySegments(from_g, new_g);
              for (;;) {
                INT d;
                si->min_dirty_byte = new_g;
                for (d = 0; d < cards_per_segment; d += 1) {
                  if (si->dirty_bytes[d] == old_g) si->dirty_bytes[d] = new_g;
                }
                nextsi = si->dirty_next;
                if (nextsi == NULL) break;
                si = nextsi;
              }
              if (tail != NULL) tail->dirty_prev = &si->dirty_next;
              si->dirty_next = tail;
            } else {
              do {
                INT d;
                for (d = 0; d < cards_per_segment; d += 1) {
                  if (si->dirty_bytes[d] == old_g) si->dirty_bytes[d] = new_g;
                }
                si = si->dirty_next;
              } while (si != NULL);
            }
          } else {
            S_error_abort("S_do_gc(gc): unexpected nonempty dirty segment list");
          }
        }
      }
    }

   /* tell profile_release_counters to scan only through new_g */
    if (S_G.prcgeneration == old_g) S_G.prcgeneration = new_g;

   /* finally reset max_nonstatic_generation */
    S_G.min_free_gen = S_G.new_min_free_gen;
    S_G.max_nonstatic_generation = new_g;
  } else {
    result = S_gc(tc, max_cg, min_tg, max_tg, count_roots);
  }

 /* eagerly give collecting thread, the only one guaranteed to be
    active, a fresh allocation area.  the other threads have to trap
    to get_more_room if and when they awake and try to allocate */
  S_reset_allocation_pointer(tc);

  THREAD_GC(tc)->during_alloc -= 1;

  Sunlock_object(code);

  return result;
}

ptr S_gc(ptr tc, IGEN max_cg, IGEN min_tg, IGEN max_tg, ptr count_roots) {
  if (min_tg == static_generation
      || S_G.enable_object_counts || S_G.enable_object_backreferences
      || (count_roots != Sfalse)) {
    if (S_G.enable_object_backreferences) min_tg = max_tg;
    return S_gc_oce(tc, max_cg, min_tg, max_tg, count_roots);
#if defined(PTHREADS)
  } else if ((S_collect_waiting_threads != 0)
             || (Spairp(S_threads)
                 && Spairp(Scdr(S_threads))
                 && (S_num_preserve_ownership_threads > 0))) {
    return S_gc_par(tc, max_cg, min_tg, max_tg, Sfalse);
#endif
  } else if (max_cg == 0 && min_tg == 1 && max_tg == 1
           && !S_G.must_mark_gen0 && S_G.locked_objects[0] == Snil
           && (S_G.min_mark_gen > 0)) {
    S_gc_011(tc);
    return Svoid;
  } else {
    return S_gc_ocd(tc, max_cg, min_tg, max_tg, Sfalse);
  }
}
