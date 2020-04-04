/* gc.c
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
#include "sort.h"
#ifndef WIN32
#include <sys/wait.h>
#endif /* WIN32 */
#include "popcount.h"

#define enable_object_counts do_not_use_enable_object_counts_in_this_file_use_ifdef_ENABLE_OBJECT_COUNTS_instead

/* locally defined functions */
static uptr list_length PROTO((ptr ls));
static ptr copy_list PROTO((ptr ls, IGEN tg));
static ptr dosort PROTO((ptr ls, uptr n));
static ptr domerge PROTO((ptr l1, ptr l2));
static ptr copy PROTO((ptr pp, seginfo *si));
static void sweep PROTO((ptr tc, ptr p));
static void sweep_in_old PROTO((ptr tc, ptr p));
static IBOOL object_directly_refers_to_self PROTO((ptr p));
static ptr copy_stack PROTO((ptr old, iptr *length, iptr clength));
static void resweep_weak_pairs PROTO((IGEN g));
static void forward_or_bwp PROTO((ptr *pp, ptr p));
static void sweep_generation PROTO((ptr tc, IGEN g));
static uptr size_object PROTO((ptr p));
static iptr sweep_typed_object PROTO((ptr tc, ptr p));
static void sweep_symbol PROTO((ptr p));
static void sweep_port PROTO((ptr p));
static void sweep_thread PROTO((ptr p));
static void sweep_continuation PROTO((ptr p));
static void sweep_record PROTO((ptr x));
static IGEN sweep_dirty_record PROTO((ptr x, IGEN tg, IGEN youngest));
static IGEN sweep_dirty_port PROTO((ptr x, IGEN tg, IGEN youngest));
static IGEN sweep_dirty_symbol PROTO((ptr x, IGEN tg, IGEN youngest));
static void sweep_code_object PROTO((ptr tc, ptr co));
static void record_dirty_segment PROTO((IGEN from_g, IGEN to_g, seginfo *si));
static void sweep_dirty PROTO((void));
static IGEN sweep_dirty_intersecting PROTO((ptr lst, ptr *pp, ptr *ppend, IGEN tg, IGEN youngest));
static IGEN sweep_dirty_bytes PROTO((ptr *pp, ptr *ppend, ptr *pu, ptr *puend, IGEN tg, IGEN youngest));
static void resweep_dirty_weak_pairs PROTO((void));
static void add_pending_guardian PROTO((ptr gdn, ptr tconc));
static void add_trigger_guardians_to_recheck PROTO((ptr ls));
static void add_ephemeron_to_pending PROTO((ptr p));
static void add_trigger_ephemerons_to_repending PROTO((ptr p));
static void check_triggers PROTO((seginfo *si));
static void check_ephemeron PROTO((ptr pe, int add_to_trigger));
static void check_pending_ephemerons PROTO(());
static int check_dirty_ephemeron PROTO((ptr pe, int tg, int youngest));
static void clear_trigger_ephemerons PROTO(());
static void sanitize_locked_segment PROTO((seginfo *si));

#ifdef ENABLE_OBJECT_COUNTS
static uptr total_size_so_far();
#endif

#ifdef ENABLE_MEASURE
static void init_measure(IGEN min_gen, IGEN max_gen);
static void finish_measure();
static void measure(ptr p);
static IBOOL flush_measure_stack();
static void init_measure_mask(seginfo *si);
static void init_counting_mask(seginfo *si);
static void push_measure(ptr p);
static void measure_add_stack_size(ptr stack, uptr size);
static void add_ephemeron_to_pending_measure(ptr pe);
static void add_trigger_ephemerons_to_pending_measure(ptr pe);
static void check_ephemeron_measure(ptr pe);
static void check_pending_measure_ephemerons();
#endif

#define OLDSPACE(x) (SPACE(x) & space_old)

/* #define DEBUG */

/* initialized and used each gc cycle.  any others should be defined in globals.h */
static IBOOL change;
static IGEN target_generation;
static IGEN max_copied_generation;
static ptr sweep_loc[max_real_space+1];
static ptr orig_next_loc[max_real_space+1];
static ptr tlcs_to_rehash;
static ptr conts_to_promote;
static ptr recheck_guardians_ls;

#ifdef ENABLE_OBJECT_COUNTS
static int measure_all_enabled;
static uptr count_root_bytes;
# define COUNTING_OR(e) 1
#else
# define COUNTING_OR(e) e
#endif

#ifdef ENABLE_MEASURE
static uptr measure_total; /* updated by `measure` */
static IGEN min_measure_generation, max_measure_generation;
static ptr *measure_stack_start, *measure_stack, *measure_stack_limit;
static ptr measured_seginfos;
static ptr pending_measure_ephemerons;
#endif

#ifdef ENABLE_BACKREFERENCE
static ptr sweep_from;
# define BACKREFERENCES_ENABLED S_G.enable_object_backreferences
# define SET_SWEEP_FROM(p) if (S_G.enable_object_backreferences) sweep_from = p
# define WITH_TOP_BACKREFERENCE(v, e) SET_SWEEP_FROM(v); e; SET_SWEEP_FROM(Sfalse)
# define SET_BACKREFERENCE(p) sweep_from = p;
# define PUSH_BACKREFERENCE(p) ptr old_sweep_from = sweep_from; SET_SWEEP_FROM(p);
# define POP_BACKREFERENCE() SET_SWEEP_FROM(old_sweep_from);
# define ADD_BACKREFERENCE_FROM(p, from_p)             \
  { IGEN tg = target_generation; \
    if ((S_G.enable_object_backreferences) && (target_generation < static_generation)) \
      S_G.gcbackreference[tg] = S_cons_in(space_impure, tg, \
                                          S_cons_in(space_impure, tg, p, from_p), \
                                          S_G.gcbackreference[tg]); }
# define ADD_BACKREFERENCE(p) ADD_BACKREFERENCE_FROM(p, sweep_from)
#else
# define BACKREFERENCES_ENABLED 0
# define WITH_TOP_BACKREFERENCE(v, e) e
# define SET_BACKREFERENCE(p)
# define PUSH_BACKREFERENCE(p)
# define POP_BACKREFERENCE()
# define ADD_BACKREFERENCE(p)
# define ADD_BACKREFERENCE_FROM(p, from_p)
#endif

/* Values for a guardian entry's `pending` field when it's added to a
   seginfo's pending list: */
enum {
  GUARDIAN_PENDING_HOLD,
  GUARDIAN_PENDING_FINAL
};

static ptr copy_list(ptr ls, IGEN tg) {
  ptr ls2 = Snil;
  for (; ls != Snil; ls = Scdr(ls))
    ls2 = S_cons_in(space_impure, tg, Scar(ls), ls2);
  return ls2;
}

#define CARLT(x, y) (Scar(x) < Scar(y))
mkmergesort(dosort, domerge, ptr, Snil, CARLT, INITCDR)

uptr list_length(ptr ls) {
  uptr i = 0;
  while (ls != Snil) { ls = Scdr(ls); i += 1; }
  return i;
}

#ifdef PRESERVE_FLONUM_EQ

static void flonum_set_forwarded(ptr p, seginfo *si) {
  if (!si->forwarded_flonums) {
    ptr ff;
    find_room(space_data, 0, typemod, ptr_align(segment_bitmap_bytes), ff);
    memset(ff, 0, segment_bitmap_bytes);
    si->forwarded_flonums = ff;
  }
  {
    uptr byte = segment_bitmap_byte(p);
    uptr bit = segment_bitmap_bit(p);
    si->forwarded_flonums[byte] |= bit;
  }
}

static int flonum_is_forwarded_p(ptr p, seginfo *si) {
  if (!si->forwarded_flonums)
    return 0;
  else {
    uptr delta = (uptr)UNTYPE(p, type_flonum) - (uptr)build_ptr(si->number, 0);
    delta >>= log2_ptr_bytes;
    return si->forwarded_flonums[delta >> 3] & (1 << (delta & 0x7));
  }
}

# define FLONUM_FWDADDRESS(p) *(ptr*)(UNTYPE(p, type_flonum))

# define FORWARDEDP(p, si) ((TYPEBITS(p) == type_flonum) ? flonum_is_forwarded_p(p, si) : (FWDMARKER(p) == forward_marker))
# define GET_FWDADDRESS(p) ((TYPEBITS(p) == type_flonum) ? FLONUM_FWDADDRESS(p) : FWDADDRESS(p))
#else
# define FORWARDEDP(p, si) (FWDMARKER(p) == forward_marker && TYPEBITS(p) != type_flonum)
# define GET_FWDADDRESS(p) FWDADDRESS(p)
#endif

#define relocate(ppp) {\
    ptr PP;\
    PP = *ppp;\
    relocate_help(ppp, PP)\
}

/* optimization of:
 * relocate(ppp)
 * if (GENERATION(*ppp) < youngest)
 *   youngest = GENERATION(*ppp);
 */
#define relocate_dirty(ppp,tg,youngest) {\
  ptr PP = *ppp; seginfo *SI;\
  if (!IMMEDIATE(PP) && (SI = MaybeSegInfo(ptr_get_segment(PP))) != NULL) {\
    if (SI->space & space_old) {\
      relocate_help_help(ppp, PP, SI)\
      youngest = tg;\
    } else {\
      IGEN pg;\
      if (youngest != tg && (pg = SI->generation) < youngest) {\
        youngest = pg;\
      }\
    }\
  }\
}

#define relocate_help(ppp, pp) {\
  seginfo *SI; \
  if (!IMMEDIATE(pp) && (SI = MaybeSegInfo(ptr_get_segment(pp))) != NULL && COUNTING_OR(SI->space & space_old)) \
    relocate_help_help(ppp, pp, SI)\
}

#define relocate_help_help(ppp, pp, si) {       \
  if (FORWARDEDP(pp, si)) \
    *ppp = GET_FWDADDRESS(pp); \
  else\
    *ppp = copy(pp, si);\
}

#define locked(si, p) (si->locked_mask && (si->locked_mask[segment_bitmap_byte(p)] & segment_bitmap_bit(p)))

#ifdef ENABLE_OBJECT_COUNTS
# define is_counting_root(si, p) (si->counting_mask && (si->counting_mask[segment_bitmap_byte(p)] & segment_bitmap_bit(p)))
#endif

FORCEINLINE void check_triggers(seginfo *si) {
  /* Registering ephemerons and guardians to recheck at the
     granularity of a segment means that the worst-case complexity of
     GC is quadratic in the number of objects that fit into a segment
     (but that only happens if the objects are ephemeron keys that are
     reachable just through a chain via the value field of the same
     ephemerons). */
  if (si->has_triggers) {
    if (si->trigger_ephemerons) {
      add_trigger_ephemerons_to_repending(si->trigger_ephemerons);
      si->trigger_ephemerons = NULL;
    }
    if (si->trigger_guardians) {
      add_trigger_guardians_to_recheck(si->trigger_guardians);
      si->trigger_guardians = NULL;
    }
    si->has_triggers = 0;
  }
}

#ifndef ENABLE_OBJECT_COUNTS
# include "gc-ocd.inc"
#else
# include "gc-oce.inc"
#endif

/* sweep_in_old() is like sweep(), but the goal is to sweep the
   object's content without copying the object itself, so we're sweep
   an object while it's still in old space. If an object refers back
   to itself, naively sweeping might copy the object while we're
   trying to sweep the old copy, which interacts badly with the words
   set to a forwarding marker and pointer. To handle that problem,
   sweep_in_old() is allowed to copy the object, since the object
   is going to get copied anyway. */
static void sweep_in_old(ptr tc, ptr p) {
  /* Detect all the cases when we need to give up on in-place
     sweeping: */
  if (object_directly_refers_to_self(p)) {
    relocate(&p)
    return;
  }

  /* We've determined that `p` won't refer immediately back to itself,
     so it's ok to use sweep(). */
  sweep(tc, p);
}

static ptr copy_stack(old, length, clength) ptr old; iptr *length, clength; {
  iptr n, m; ptr new;

  /* Don't copy non-oldspace stacks, since we may be sweeping a locked
     continuation that is older than target_generation.  Doing so would
     be a waste of work anyway. */
  if (!OLDSPACE(old)) return old;

  /* reduce headroom created for excessively large frames (typically resulting from apply with long lists) */
  if ((n = *length) != clength && n > default_stack_size && n > (m = clength + one_shot_headroom)) {
    *length = n = m;
  }

  n = ptr_align(n);
#ifdef ENABLE_OBJECT_COUNTS
  S_G.countof[target_generation][countof_stack] += 1;
  S_G.bytesof[target_generation][countof_stack] += n;
#endif /* ENABLE_OBJECT_COUNTS */
  find_room(space_data, target_generation, typemod, n, new);
  n = ptr_align(clength);
 /* warning: stack may have been left non-double-aligned by split_and_resize */
  memcpy_aligned(new, old, n);

 /* also returning possibly updated value in *length */
  return new;
}

#define NONSTATICINHEAP(si, x) (!IMMEDIATE(x) && (si = MaybeSegInfo(ptr_get_segment(x))) != NULL && si->generation != static_generation)
#define ALWAYSTRUE(si, x) (si = SegInfo(ptr_get_segment(x)), 1)
#define partition_guardians(LS, FILTER) { \
  ptr ls; seginfo *si;\
  for (ls = LS; ls != Snil; ls = next) { \
    obj = GUARDIANOBJ(ls); \
    next = GUARDIANNEXT(ls); \
 \
    if (FILTER(si, obj)) { \
      if (!(si->space & space_old) || locked(si, obj)) { \
        INITGUARDIANNEXT(ls) = pend_hold_ls; \
        pend_hold_ls = ls; \
      } else if (FORWARDEDP(obj, si)) { \
        INITGUARDIANOBJ(ls) = GET_FWDADDRESS(obj); \
        INITGUARDIANNEXT(ls) = pend_hold_ls; \
        pend_hold_ls = ls; \
      } else { \
        seginfo *t_si; \
        tconc = GUARDIANTCONC(ls); \
        t_si = SegInfo(ptr_get_segment(tconc)); \
        if (!(t_si->space & space_old) || locked(t_si, tconc)) { \
          INITGUARDIANNEXT(ls) = final_ls; \
          final_ls = ls; \
        } else if (FWDMARKER(tconc) == forward_marker) { \
          INITGUARDIANTCONC(ls) = FWDADDRESS(tconc); \
          INITGUARDIANNEXT(ls) = final_ls; \
          final_ls = ls; \
        } else { \
          INITGUARDIANNEXT(ls) = pend_final_ls; \
          pend_final_ls = ls; \
        } \
      } \
    } \
  } \
}

typedef struct count_root_t {
  ptr p;
  IBOOL weak;
} count_root_t;

ptr GCENTRY(ptr tc, IGEN mcg, IGEN tg, ptr count_roots_ls) {
    IGEN g; ISPC s;
    seginfo *oldspacesegments, *si, *nextsi;
    ptr ls, younger_locked_objects;
    bucket_pointer_list *buckets_to_rebuild;
#ifdef ENABLE_OBJECT_COUNTS
    ptr count_roots_counts = Snil;
    iptr count_roots_len;
    count_root_t *count_roots;
#endif

   /* flush instruction cache: effectively clear_code_mod but safer */
    for (ls = S_threads; ls != Snil; ls = Scdr(ls)) {
      ptr tc = (ptr)THREADTC(Scar(ls));
      S_flush_instruction_cache(tc);
    }

    tlcs_to_rehash = Snil;
    conts_to_promote = Snil;

    for (ls = S_threads; ls != Snil; ls = Scdr(ls)) {
      ptr tc = (ptr)THREADTC(Scar(ls));
      S_scan_dirty((ptr **)EAP(tc), (ptr **)REAL_EAP(tc));
      EAP(tc) = REAL_EAP(tc) = AP(tc) = (ptr)0;
    }

   /* perform after ScanDirty */
    if (S_checkheap) S_check_heap(0);

#ifdef DEBUG
(void)printf("mcg = %x;  go? ", mcg); (void)fflush(stdout); (void)getc(stdin);
#endif

    target_generation = tg;
    max_copied_generation = mcg;

  /* set up generations to be copied */
    for (s = 0; s <= max_real_space; s++)
        for (g = 0; g <= mcg; g++) {
            S_G.base_loc[s][g] = FIX(0);
            S_G.first_loc[s][g] = FIX(0);
            S_G.next_loc[s][g] = FIX(0);
            S_G.bytes_left[s][g] = 0;
            S_G.bytes_of_space[s][g] = 0;
        }

  /* reset phantom size in generations to be copied */
    for (g = 0; g <= mcg; g++) {
      S_G.phantom_sizes[g] = 0;
    }

  /* set up target generation sweep_loc and orig_next_loc pointers */
    for (s = 0; s <= max_real_space; s++)
      orig_next_loc[s] = sweep_loc[s] = S_G.next_loc[s][tg];

  /* mark segments from which objects are to be copied */
    oldspacesegments = (seginfo *)NULL;
    for (s = 0; s <= max_real_space; s += 1) {
      for (g = 0; g <= mcg; g += 1) {
        for (si = S_G.occupied_segments[s][g]; si != NULL; si = nextsi) {
          nextsi = si->next;
          si->next = oldspacesegments;
          oldspacesegments = si;
          si->space = s | space_old; /* NB: implicitly clearing space_locked */
        }
        S_G.occupied_segments[s][g] = NULL;
      }
    }

#ifdef ENABLE_OBJECT_COUNTS
   /* clear object counts & bytes for copied generations; bump timestamp */
   {INT i;
    for (g = 0; g <= mcg; g += 1) {
      for (i = 0; i < countof_types; i += 1) {
        S_G.countof[g][i] = 0;
        S_G.bytesof[g][i] = 0;
      }
      if (g == 0) {
        S_G.gctimestamp[g] += 1;
      } else {
        S_G.gctimestamp[g] = S_G.gctimestamp[0];
      }
    }
   }
#endif /* ENABLE_OBJECT_COUNTS */

   /* Clear any backreference lists for copied generations */
   for (g = 0; g <= mcg; g += 1) {
      S_G.gcbackreference[g] = Snil;
   }

   SET_BACKREFERENCE(Sfalse) /* #f => root or locked */

#ifdef ENABLE_OBJECT_COUNTS
  /* set flag on count_roots objects so they get copied to space_count_root */
     if (count_roots_ls != Sfalse) {
       iptr i;

       count_roots_len = list_length(count_roots_ls);
       find_room(space_data, 0, typemod, ptr_align(count_roots_len*sizeof(count_root_t)), count_roots);

       for (ls = count_roots_ls, i = 0; ls != Snil; ls = Scdr(ls), i++) {
         ptr p = Scar(ls);
         if (IMMEDIATE(p)) {
           count_roots[i].p = p;
           count_roots[i].weak = 0;
         } else {
           seginfo *ls_si = SegInfo(ptr_get_segment(ls));
           seginfo *si = SegInfo(ptr_get_segment(p));

           if (!si->counting_mask)
             init_counting_mask(si);

           si->counting_mask[segment_bitmap_byte(p)] |= segment_bitmap_bit(p);

           count_roots[i].p = p;
           count_roots[i].weak = (((ls_si->space & ~(space_old|space_locked)) == space_weakpair)
                                  || ((ls_si->space & ~(space_old|space_locked)) == space_ephemeron));
         }
       }
     } else {
       count_roots_len = 0;
       count_roots = NULL;
     }
#endif
     
    /* pre-collection handling of locked objects. */

     /* set up locked-object masks */
     younger_locked_objects = Snil;
     for (si = oldspacesegments; si != NULL; si = si->next) {
       if (si->locked_objects != Snil) {
         find_room(space_data, 0, typemod, ptr_align(segment_bitmap_bytes), si->locked_mask);
         memset(si->locked_mask, 0, segment_bitmap_bytes);

         ls = copy_list(si->locked_objects, tg);
         si->locked_objects = ls;
         
         while (ls != Snil) {
           ptr p = Scar(ls);
           uptr byte = segment_bitmap_byte(p);
           uptr bit = segment_bitmap_bit(p);
           if (!(si->locked_mask[byte] & bit)) {
             si->locked_mask[byte] |= bit;
             younger_locked_objects = S_cons_in(space_new, 0, p, younger_locked_objects);
           }
           ls = Scdr(ls);
         }
       }
       si->unlocked_objects = Snil;
     }

#ifdef ENABLE_OBJECT_COUNTS
  /* sweep count_roots in order and accumulate counts */
     if (count_roots_len > 0) {
       ptr prev = NULL; uptr prev_total = total_size_so_far();
       iptr i;

# ifdef ENABLE_MEASURE
       init_measure(tg+1, static_generation);
# endif

       for (i = 0; i < count_roots_len; i++) {
         uptr total;
         ptr p = count_roots[i].p;
         if (IMMEDIATE(p)) {
           /* nothing to do */
         } else {
           seginfo *si = SegInfo(ptr_get_segment(p));

           si->counting_mask[segment_bitmap_byte(p)] -= segment_bitmap_bit(p);

           if (!(si->space & space_old) || FORWARDEDP(p, si) || locked(si, p)
               || !count_roots[i].weak) {
             /* reached or older; sweep transitively */
             relocate(&p)
             if ((si->space & ~(space_old|space_locked)) != space_ephemeron) /* not ok to resweep ephemeron */
               sweep(tc, p);
             ADD_BACKREFERENCE(p)
             sweep_generation(tc, tg);
# ifdef ENABLE_MEASURE
             while (flush_measure_stack()) {
               sweep_generation(tc, tg);
             }
# endif

             /* now count this object's size, if we have deferred it before */
             si = SegInfo(ptr_get_segment(p));
             if ((si->space == space_count_pure) || (si->space == space_count_impure))
               count_root_bytes -= size_object(p);
           }
         }

         total = total_size_so_far();
         p = S_cons_in(space_new, 0, FIX(total-prev_total), Snil);
         if (prev != NULL)
           Scdr(prev) = p;
         else
           count_roots_counts = p;
         prev = p;
         prev_total = total;
       }

# ifdef ENABLE_MEASURE
       finish_measure();
# endif

       /* clear `counting_mask`s */
       for (i = 0; i < count_roots_len; i++) {
         ptr p = count_roots[i].p;
         if (!IMMEDIATE(p)) {
           seginfo *si = SegInfo(ptr_get_segment(p));
           si->counting_mask = NULL;
         }
       }
     }
#endif

   /* sweep younger locked objects */
     for (ls = younger_locked_objects; ls != Snil; ls = Scdr(ls)) {
       ptr x = Scar(ls);
       sweep(tc, x);
       ADD_BACKREFERENCE(x)
      }

  /* sweep non-oldspace threads, since any thread may have an active stack */
    for (ls = S_threads; ls != Snil; ls = Scdr(ls)) {
      ptr thread;

    /* someone may have their paws on the list */
      if (FWDMARKER(ls) == forward_marker) ls = FWDADDRESS(ls);

      thread = Scar(ls);
      if (!OLDSPACE(thread)) sweep_thread(thread);
    }
    relocate(&S_threads)

  /* relocate nonempty oldspace symbols and set up list of buckets to rebuild later */
    buckets_to_rebuild = NULL;
    for (g = 0; g <= mcg; g += 1) {
      bucket_list *bl, *blnext; bucket *b; bucket_pointer_list *bpl; bucket **oblist_cell; ptr sym; iptr idx;
      for (bl = S_G.buckets_of_generation[g]; bl != NULL; bl = blnext) {
        blnext = bl->cdr;
        b = bl->car;
        /* mark this bucket old for the rebuilding loop */
        b->next = (bucket *)((uptr)b->next | 1);
        sym = b->sym;
        idx = UNFIX(SYMHASH(sym)) % S_G.oblist_length;
        oblist_cell = &S_G.oblist[idx];
        if (!((uptr)*oblist_cell & 1)) {
          /* mark this bucket in the set */
          *oblist_cell = (bucket *)((uptr)*oblist_cell | 1);
          /* repurpose the bucket list element for the list of buckets to rebuild later */
          /* idiot_checks verifies these have the same size */
          bpl = (bucket_pointer_list *)bl;
          bpl->car = oblist_cell;
          bpl->cdr = buckets_to_rebuild;
          buckets_to_rebuild = bpl;
        }
        if (FWDMARKER(sym) != forward_marker &&
            /* coordinate with alloc.c */
            (SYMVAL(sym) != sunbound || SYMPLIST(sym) != Snil || SYMSPLIST(sym) != Snil))
          (void)copy(sym, SegInfo(ptr_get_segment(sym)));
      }
      S_G.buckets_of_generation[g] = NULL;
    }

  /* relocate the protected C pointers */
    {uptr i;
     for (i = 0; i < S_G.protect_next; i++)
         relocate(S_G.protected[i])
    }

  /* sweep areas marked dirty by assignments into older generations */
    sweep_dirty();

    sweep_generation(tc, tg);

  /* handle guardians */
    {   ptr hold_ls, pend_hold_ls, final_ls, pend_final_ls, maybe_final_ordered_ls;
        ptr obj, rep, tconc, next;
        IBOOL do_ordered = 0;

      /* move each entry in guardian lists into one of:
       *   pend_hold_ls     if obj accessible
       *   final_ls         if obj not accessible and tconc accessible
       *   pend_final_ls    if obj not accessible and tconc not accessible
       * When a pend_hold_ls or pend_final_ls entry is tconc is
       * determined to be accessible, then it moves to hold_ls or
       * final_ls. When an entry in pend_hold_ls or pend_final_ls can't
       * be moved to final_ls or hold_ls, the entry moves into a
       * seginfo's trigger list (to avoid quadratic-time processing of
       * guardians). When the trigger fires, the entry is added to
       * recheck_guardians_ls, which is sorted back into pend_hold_ls
       * and pend_final_ls for another iteration.
       * Ordered and unordered guardian entries start out together;
       * when final_ls is processed, ordered entries are delayed by
       * moving them into maybe_final_ordered_ls, which is split back
       * into final_ls and pend_hold_ls after all unordered entries
       * have been handled. */
        pend_hold_ls = final_ls = pend_final_ls = maybe_final_ordered_ls = Snil;
        recheck_guardians_ls = Snil;

        for (ls = S_threads; ls != Snil; ls = Scdr(ls)) {
          ptr tc = (ptr)THREADTC(Scar(ls));
          partition_guardians(GUARDIANENTRIES(tc), NONSTATICINHEAP);
          GUARDIANENTRIES(tc) = Snil;
        }

        for (g = 0; g <= mcg; g += 1) {
          partition_guardians(S_G.guardians[g], ALWAYSTRUE);
          S_G.guardians[g] = Snil;
        }

       /* invariants after partition_guardians:
        * for entry in pend_hold_ls, obj is !OLDSPACE or locked
        * for entry in final_ls, obj is OLDSPACE and !locked
        * for entry in final_ls, tconc is !OLDSPACE or locked
        * for entry in pend_final_ls, obj and tconc are OLDSPACE and !locked
        */

        hold_ls = S_G.guardians[tg];
        while (1) {
            IBOOL relocate_rep = final_ls != Snil;

          /* relocate & add the final objects to their tconcs */
            ls = final_ls; final_ls = Snil;
            for (; ls != Snil; ls = next) {
                ptr old_end, new_end;

                next = GUARDIANNEXT(ls);

                rep = GUARDIANREP(ls);
              /* ftype_guardian_rep is a marker for reference-counted ftype pointer */
                if (rep == ftype_guardian_rep) {
                  int b; uptr *addr;
                  rep = GUARDIANOBJ(ls);
                  if (FWDMARKER(rep) == forward_marker) rep = FWDADDRESS(rep);
                /* Caution: Building in assumption about shape of an ftype pointer */
                  addr = RECORDINSTIT(rep, 0);
                  LOCKED_DECR(addr, b);
                  if (!b) continue;
                }

                if (!do_ordered && (GUARDIANORDERED(ls) == Strue)) {
                  /* Sweep from the representative, but don't copy the
                     representative itself; if the object stays uncopied by
                     the end, then the entry is really final, and we copy the
                     representative only at that point; crucially, the
                     representative can't itself be a tconc, so we
                     won't discover any new tconcs at that point. */
                  ptr obj = GUARDIANOBJ(ls);
                  if (FORWARDEDP(obj, SegInfo(ptr_get_segment(obj)))) {
                    /* Object is reachable, so we might as well move
                       this one to the hold list --- via pend_hold_ls, which
                       leads to a copy to move to hold_ls */
                    INITGUARDIANNEXT(ls) = pend_hold_ls;
                    pend_hold_ls = ls;
                  } else {
                    seginfo *si;
                    if (!IMMEDIATE(rep) && (si = MaybeSegInfo(ptr_get_segment(rep))) != NULL && (si->space & space_old) && !locked(si, rep)) {
                      PUSH_BACKREFERENCE(rep)
                      sweep_in_old(tc, rep);
                      POP_BACKREFERENCE()
                    }
                    INITGUARDIANNEXT(ls) = maybe_final_ordered_ls;
                    maybe_final_ordered_ls = ls;
                  }
                } else {
                /* if tconc was old it's been forwarded */
                  tconc = GUARDIANTCONC(ls);
                  
                  WITH_TOP_BACKREFERENCE(tconc, relocate(&rep));

                  old_end = Scdr(tconc);
                  new_end = S_cons_in(space_impure, 0, FIX(0), FIX(0));
#ifdef ENABLE_OBJECT_COUNTS
                  S_G.countof[tg][countof_pair] += 1;
#endif /* ENABLE_OBJECT_COUNTS */
                  
                  SETCAR(old_end,rep);
                  SETCDR(old_end,new_end);
                  SETCDR(tconc,new_end);
                }
            }

            /* discard static pend_hold_ls entries */
            if (tg != static_generation) {
                /* copy each entry in pend_hold_ls into hold_ls if tconc accessible */
                ls = pend_hold_ls; pend_hold_ls = Snil;
                for ( ; ls != Snil; ls = next) {
                    ptr p;
                    seginfo *t_si;
                    
                    tconc = GUARDIANTCONC(ls); next = GUARDIANNEXT(ls); 

                    t_si = SegInfo(ptr_get_segment(tconc));

                    if ((t_si->space & space_old) && !locked(t_si, tconc)) {
                        if (FWDMARKER(tconc) == forward_marker)
                            tconc = FWDADDRESS(tconc);
                        else {
                            INITGUARDIANPENDING(ls) = FIX(GUARDIAN_PENDING_HOLD);
                            add_pending_guardian(ls, tconc);
                            continue;
                        }
                    }

                    rep = GUARDIANREP(ls);
                    WITH_TOP_BACKREFERENCE(tconc, relocate(&rep));
                    relocate_rep = 1;

#ifdef ENABLE_OBJECT_COUNTS
                    S_G.countof[tg][countof_guardian] += 1;
#endif /* ENABLE_OBJECT_COUNTS */
                    /* In backreference mode, we rely on sweep of the guardian
                       entry not registering any backreferences. Otherwise,
                       bogus pair pointers would get created. */
                    find_room(space_pure, tg, typemod, size_guardian_entry, p);
                    INITGUARDIANOBJ(p) = GUARDIANOBJ(ls);
                    INITGUARDIANREP(p) = rep;
                    INITGUARDIANTCONC(p) = tconc;
                    INITGUARDIANNEXT(p) = hold_ls;
                    INITGUARDIANORDERED(p) = GUARDIANORDERED(ls);
                    INITGUARDIANPENDING(p) = FIX(0);
                    hold_ls = p;
                }
            }

            if (!relocate_rep && !do_ordered && maybe_final_ordered_ls != Snil) {
              /* Switch to finishing up ordered. Move all maybe-final
                 ordered entries to final_ls and pend_hold_ls */
              do_ordered = relocate_rep = 1;
              ls = maybe_final_ordered_ls; maybe_final_ordered_ls = Snil;
              for (; ls != Snil; ls = next) {
                ptr obj = GUARDIANOBJ(ls);
                next = GUARDIANNEXT(ls);
                if (FORWARDEDP(obj, SegInfo(ptr_get_segment(obj)))) {
                  /* Will defintely move to hold_ls, but the entry
                     must be copied to move from pend_hold_ls to
                     hold_ls: */
                  INITGUARDIANNEXT(ls) = pend_hold_ls;
                  pend_hold_ls = ls;
                } else {
                  INITGUARDIANNEXT(ls) = final_ls;
                  final_ls = ls;
                }
              }
            }

            if (!relocate_rep) break;

            sweep_generation(tc, tg);

            ls = recheck_guardians_ls; recheck_guardians_ls = Snil;
            for ( ; ls != Snil; ls = next) {
              next = GUARDIANNEXT(ls);
              if (GUARDIANPENDING(ls) == FIX(GUARDIAN_PENDING_HOLD)) {
                INITGUARDIANNEXT(ls) = pend_hold_ls;
                pend_hold_ls = ls;
              } else {
                INITGUARDIANNEXT(ls) = pend_final_ls;
                pend_final_ls = ls;
              }
            }
            
          /* move each entry in pend_final_ls into one of:
           *   final_ls         if tconc forwarded
           *   pend_final_ls    if tconc not forwarded
           * where the output pend_final_ls coresponds to pending in a segment */
            ls = pend_final_ls; pend_final_ls = Snil;
            for ( ; ls != Snil; ls = next) {
                tconc = GUARDIANTCONC(ls); next = GUARDIANNEXT(ls);

                if (FWDMARKER(tconc) == forward_marker) {
                    INITGUARDIANTCONC(ls) = FWDADDRESS(tconc);
                    INITGUARDIANNEXT(ls) = final_ls;
                    final_ls = ls;
                } else {
                    INITGUARDIANPENDING(ls) = FIX(GUARDIAN_PENDING_FINAL);
                    add_pending_guardian(ls, tconc);
                }
            }
        }

        S_G.guardians[tg] = hold_ls;
    }

  /* handle weak pairs */
    resweep_dirty_weak_pairs();
    resweep_weak_pairs(tg);

   /* still-pending ephemerons all go to bwp */
    clear_trigger_ephemerons();

   /* forward car fields of locked oldspace weak pairs */
    for (ls = younger_locked_objects; ls != Snil; ls = Scdr(ls)) {
      ptr x = Scar(ls);
      if (Spairp(x) && (SPACE(x) & ~(space_old|space_locked)) == space_weakpair) {
        forward_or_bwp(&INITCAR(x), Scar(x));
      }
    }

   /* post-gc oblist handling.  rebuild old buckets in the target generation, pruning unforwarded symbols */
    { bucket_list *bl, *blnext; bucket *b, *bnext; bucket_pointer_list *bpl; bucket **pb;
      ptr sym; seginfo *si;
      bl = tg == static_generation ? NULL : S_G.buckets_of_generation[tg];
      for (bpl = buckets_to_rebuild; bpl != NULL; bpl = bpl->cdr) {
        pb = bpl->car;
        for (b = (bucket *)((uptr)*pb - 1); b != NULL && ((uptr)(b->next) & 1); b = bnext) {
          bnext = (bucket *)((uptr)(b->next) - 1);
          sym = b->sym;
          si = SegInfo(ptr_get_segment(sym));
          if (locked(si, sym) || (FWDMARKER(sym) == forward_marker && ((sym = FWDADDRESS(sym)) || 1))) {
            find_room(space_data, tg, typemod, sizeof(bucket), b);
#ifdef ENABLE_OBJECT_COUNTS
            S_G.countof[tg][countof_oblist] += 1;
            S_G.bytesof[tg][countof_oblist] += sizeof(bucket);
#endif /* ENABLE_OBJECT_COUNTS */
            b->sym = sym;
            *pb = b;
            pb = &b->next;
            if (tg != static_generation) {
              blnext = bl;
              find_room(space_data, tg, typemod, sizeof(bucket_list), bl);
#ifdef ENABLE_OBJECT_COUNTS
              S_G.countof[tg][countof_oblist] += 1;
              S_G.bytesof[tg][countof_oblist] += sizeof(bucket_list);
#endif /* ENABLE_OBJECT_COUNTS */
              bl->cdr = blnext;
              bl->car = b;
            }
          } else {
            S_G.oblist_count -= 1;
          }
        }
        *pb = b;
      }
      if (tg != static_generation) S_G.buckets_of_generation[tg] = bl;
    }

  /* rebuild rtds_with_counts lists, dropping otherwise inaccessible rtds */
    { IGEN g; ptr ls, p, newls = tg == mcg ? Snil : S_G.rtds_with_counts[tg]; seginfo *si;
      for (g = 0; g <= mcg; g += 1) {
        for (ls = S_G.rtds_with_counts[g], S_G.rtds_with_counts[g] = Snil; ls != Snil; ls = Scdr(ls)) {
          p = Scar(ls);
          si = SegInfo(ptr_get_segment(p));
          if (!(si->space & space_old) || locked(si, p)) {
            newls = S_cons_in(space_impure, tg, p, newls);
            S_G.countof[tg][countof_pair] += 1;
          } else if (FWDMARKER(p) == forward_marker) {
            newls = S_cons_in(space_impure, tg, FWDADDRESS(p), newls);
            S_G.countof[tg][countof_pair] += 1;
          }
        }
      }
      S_G.rtds_with_counts[tg] = newls;
    }

#ifndef WIN32
  /* rebuild child_process list, reaping any that have died and refusing
     to promote into the static generation. */
    {
      ptr old_ls, new_ls; IGEN gtmp, cpgen;
      cpgen = tg == static_generation ? S_G.max_nonstatic_generation : tg;
      new_ls = cpgen <= mcg ? Snil : S_child_processes[cpgen];
      for (gtmp = 0; gtmp <= mcg; gtmp += 1) {
        for (old_ls = S_child_processes[gtmp]; old_ls != Snil; old_ls = Scdr(old_ls)) {
          INT pid = UNFIX(Scar(old_ls)), status, retpid;
          retpid = waitpid(pid, &status, WNOHANG);
          if (retpid == 0 || (retpid == pid && !(WIFEXITED(status) || WIFSIGNALED(status)))) {
            new_ls = S_cons_in(space_impure, cpgen, FIX(pid), new_ls);
#ifdef ENABLE_OBJECT_COUNTS
            S_G.countof[cpgen][countof_pair] += 1;
#endif /* ENABLE_OBJECT_COUNTS */
          }
        }
        S_child_processes[gtmp] = Snil;
      }
      S_child_processes[cpgen] = new_ls;
    }
#endif /* WIN32 */

   /* post-collection handling of locked objects.  This must come after
      any use of relocate. */
    for (ls = younger_locked_objects; ls != Snil; ls = Scdr(ls)) {
        ptr x = Scar(ls);
        ptr a1, a2; uptr seg; uptr n;

        /* promote the segment(s) containing x to the target generation.
           reset the space_old bit to prevent the segments from being
           reclaimed; sanitize the segments to support sweeping by
           sweep_dirty (since the segments may contain a mix of objects,
           many of which have been discarded). */
        n = size_object(x);
#ifdef ENABLE_OBJECT_COUNTS
        S_G.countof[target_generation][countof_locked] += 1;
        S_G.bytesof[target_generation][countof_locked] += n;
#endif /* ENABLE_OBJECT_COUNTS */

        a1 = UNTYPE_ANY(x);
        a2 = (ptr)((uptr)a1 + n - 1);
        for (seg = addr_get_segment(a1); seg <= addr_get_segment(a2); seg += 1) {
          seginfo *si = SegInfo(seg);
          if (!(si->space  & space_locked)) {
            si->generation = tg;
            si->space = (si->space & ~space_old) | space_locked;
            sanitize_locked_segment(si);
          }
          si->locked_mask = NULL; /* really only need to clear the first one */
        }
    }

  /* move old space segments to empty space */
    for (si = oldspacesegments; si != NULL; si = nextsi) {
      nextsi = si->next;
      s = si->space;
      if (s & space_locked) {
        /* note: the oldspace bit is cleared above for locked objects */
        s &= ~space_locked;
        g = si->generation;
        if (g == static_generation) S_G.number_of_nonstatic_segments -= 1;
        si->next = S_G.occupied_segments[s][g];
        S_G.occupied_segments[s][g] = si;
        si->trigger_ephemerons = NULL;
#ifdef PRESERVE_FLONUM_EQ
        /* any flonums forwarded won't be reference anymore */
        si->forwarded_flonums = NULL;
#endif
      } else {
        chunkinfo *chunk = si->chunk;
        if (si->generation != static_generation) S_G.number_of_nonstatic_segments -= 1;
        S_G.number_of_empty_segments += 1;
        si->space = space_empty;
        si->next = chunk->unused_segs;
        chunk->unused_segs = si;
#ifdef WIPECLEAN
        memset((void *)build_ptr(si->number,0), 0xc7, bytes_per_segment);
#endif
        if ((chunk->nused_segs -= 1) == 0) {
          if (chunk->bytes != (minimum_segment_request + 1) * bytes_per_segment) {
            /* release oversize chunks back to the O/S immediately to avoid allocating
             * small stuff into them and thereby invite fragmentation */
            S_free_chunk(chunk);
          } else {
            S_move_to_chunk_list(chunk, &S_chunks[PARTIAL_CHUNK_POOLS]);
          }
        } else {
          S_move_to_chunk_list(chunk, &S_chunks[PARTIAL_CHUNK_POOLS-1]);
        }
      }
    }

    if (mcg >= S_G.min_free_gen) S_free_chunks();

    S_flush_instruction_cache(tc);

    if (S_checkheap) S_check_heap(1);

   /* post-collection rehashing of tlcs.
      must come after any use of relocate.
      logically comes after gc is entirely complete */
    while (tlcs_to_rehash != Snil) {
      ptr b, next; uptr old_idx, new_idx;
      ptr tlc = Scar(tlcs_to_rehash);
      ptr ht = TLCHT(tlc);
      ptr vec = PTRFIELD(ht,eq_hashtable_vec_disp);
      uptr veclen = Svector_length(vec);
      ptr key = Scar(TLCKEYVAL(tlc));
  
     /* scan to end of bucket to find the index */
      for (b = TLCNEXT(tlc); !Sfixnump(b); b = TLCNEXT(b));
      old_idx = UNFIX(b);

      if (key == Sbwp_object && PTRFIELD(ht,eq_hashtable_subtype_disp) != FIX(eq_hashtable_subtype_normal)) {
       /* remove tlc */
        b = Svector_ref(vec, old_idx);
        if (b == tlc) {
          SETVECTIT(vec, old_idx, TLCNEXT(b));
        } else {
          for (;;) { next = TLCNEXT(b); if (next == tlc) break; b = next; }
          SETTLCNEXT(b,TLCNEXT(next));
        }
        INITTLCNEXT(tlc) = Sfalse;
        INITPTRFIELD(ht,eq_hashtable_size_disp) = FIX(UNFIX(PTRFIELD(ht,eq_hashtable_size_disp)) - 1);
      } else if ((new_idx = ((uptr)key >> primary_type_bits) & (veclen - 1)) != old_idx) {
       /* remove tlc from old bucket */
        b = Svector_ref(vec, old_idx);
        if (b == tlc) {
          SETVECTIT(vec, old_idx, TLCNEXT(b));
        } else {
          for (;;) { next = TLCNEXT(b); if (next == tlc) break; b = next; }
          SETTLCNEXT(b,TLCNEXT(next));
        }
       /* and add to new bucket */
        SETTLCNEXT(tlc, Svector_ref(vec, new_idx));
        SETVECTIT(vec, new_idx, tlc);
      }
      tlcs_to_rehash = Scdr(tlcs_to_rehash);
    }

    /* Promote opportunistic 1-shot continuations, because we can no
       longer cached one and we can no longer reliably fuse the stack
       back. */
    while (conts_to_promote != Snil) {
      S_promote_to_multishot(CONTLINK(Scar(conts_to_promote)));
      conts_to_promote = Scdr(conts_to_promote);
    }

    S_resize_oblist();

    /* tell profile_release_counters to look for bwp'd counters at least through tg */
    if (S_G.prcgeneration < tg) S_G.prcgeneration = tg;

    if (count_roots_ls != Sfalse) {
#ifdef ENABLE_OBJECT_COUNTS
      return count_roots_counts;
#else
      return Snil;
#endif
    } else
      return Svoid;
}

#define sweep_space(s, body)\
    slp = &sweep_loc[s];\
    nlp = &S_G.next_loc[s][g];\
    if (*slp == 0) *slp = S_G.first_loc[s][g];\
    pp = (ptr *)*slp;\
    while (pp != (nl = (ptr *)*nlp))\
        do\
            if ((p = *pp) == forward_marker)\
                pp = (ptr *)*(pp + 1);\
            else\
                body\
        while (pp != nl);\
    *slp = (ptr)pp;

static void resweep_weak_pairs(g) IGEN g; {
    ptr *slp, *nlp; ptr *pp, p, *nl;

    sweep_loc[space_weakpair] = S_G.first_loc[space_weakpair][g];
    sweep_space(space_weakpair, {
        forward_or_bwp(pp, p);
        pp += 2;
    })
}

static void forward_or_bwp(pp, p) ptr *pp; ptr p; {
  seginfo *si;
 /* adapted from relocate */
  if (!IMMEDIATE(p) && (si = MaybeSegInfo(ptr_get_segment(p))) != NULL && si->space & space_old && !locked(si, p)) {
    if (FORWARDEDP(p, si)) {
      *pp = GET_FWDADDRESS(p);
    } else {
      *pp = Sbwp_object;
    }
  }
}

static void sweep_generation(tc, g) ptr tc; IGEN g; {
  ptr *slp, *nlp; ptr *pp, p, *nl;
  
  do {
    change = 0;
    sweep_space(space_impure, {
        SET_BACKREFERENCE(TYPE((ptr)pp, type_pair)) /* only pairs put here in backreference mode */
        relocate_help(pp, p)
        p = *(pp += 1);
        relocate_help(pp, p)
        pp += 1;
    })
    SET_BACKREFERENCE(Sfalse)

    sweep_space(space_symbol, {
        p = TYPE((ptr)pp, type_symbol);
        sweep_symbol(p);
        pp += size_symbol / sizeof(ptr);
    })

    sweep_space(space_port, {
        p = TYPE((ptr)pp, type_typed_object);
        sweep_port(p);
        pp += size_port / sizeof(ptr);
    })

    sweep_space(space_weakpair, {
        SET_BACKREFERENCE(TYPE((ptr)pp, type_pair))
        p = *(pp += 1);
        relocate_help(pp, p)
        pp += 1;
    })
    SET_BACKREFERENCE(Sfalse)

    sweep_space(space_ephemeron, {
        p = TYPE((ptr)pp, type_pair);
        add_ephemeron_to_pending(p);
        pp += size_ephemeron / sizeof(ptr);
    })
      
    sweep_space(space_pure, {
        SET_BACKREFERENCE(TYPE((ptr)pp, type_pair)) /* only pairs put here in backreference mode */
        relocate_help(pp, p)
        p = *(pp += 1);
        relocate_help(pp, p)
        pp += 1;
    })
    SET_BACKREFERENCE(Sfalse)

    sweep_space(space_continuation, {
        p = TYPE((ptr)pp, type_closure);
        sweep_continuation(p);
        pp += size_continuation / sizeof(ptr);
    })

    sweep_space(space_pure_typed_object, {
        p = TYPE((ptr)pp, type_typed_object);
        pp = (ptr *)((uptr)pp + sweep_typed_object(tc, p));
    })

    sweep_space(space_code, {
        p = TYPE((ptr)pp, type_typed_object);
        sweep_code_object(tc, p);
        pp += size_code(CODELEN(p)) / sizeof(ptr);
    })

    sweep_space(space_impure_record, {
        p = TYPE((ptr)pp, type_typed_object);
        sweep_record(p);
        pp = (ptr *)((iptr)pp +
               size_record_inst(UNFIX(RECORDDESCSIZE(RECORDINSTTYPE(p)))));
    })

    /* space used only as needed for backreferences: */
    sweep_space(space_impure_typed_object, {
        p = TYPE((ptr)pp, type_typed_object);
        pp = (ptr *)((uptr)pp + sweep_typed_object(tc, p));
    })

    /* space used only as needed for backreferences: */
    sweep_space(space_closure, {
        p = TYPE((ptr)pp, type_closure);
        sweep(tc, p);
        pp = (ptr *)((uptr)pp + size_object(p));
    })

    /* don't sweep from space_count_pure or space_count_impure */

    /* Waiting until sweeping doesn't trigger a change reduces the
       chance that an ephemeron must be reigistered as a
       segment-specific trigger or gets triggered for recheck, but
       it doesn't change the worst-case complexity. */
    if (!change)
      check_pending_ephemerons();
  } while (change);
}

static iptr sweep_typed_object(tc, p) ptr tc; ptr p; {
  ptr tf = TYPEFIELD(p);

  if (TYPEP(tf, mask_record, type_record)) {
    sweep_record(p);
    return size_record_inst(UNFIX(RECORDDESCSIZE(RECORDINSTTYPE(p))));
  } else if (TYPEP(tf, mask_thread, type_thread)) {
    sweep_thread(p);
    return size_thread;
  } else {
    /* We get here only if backreference mode pushed other typed objects into
       a typed space or if an object is a counting root */
    sweep(tc, p);
    return size_object(p);
  }
}

typedef struct _weakseginfo {
  seginfo *si;
  IGEN youngest[cards_per_segment];
  struct _weakseginfo *next;
} weakseginfo;

static weakseginfo *weaksegments_to_resweep;

static void record_dirty_segment(IGEN from_g, IGEN to_g, seginfo *si) {
  if (si->min_dirty_byte != 0xff) {
    S_error_abort("record_dirty(gc): unexpected mutation while sweeping");
  }

  if (to_g < from_g) {
    seginfo *oldfirst = DirtySegments(from_g, to_g);
    DirtySegments(from_g, to_g) = si;
    si->dirty_prev = &DirtySegments(from_g, to_g);
    si->dirty_next = oldfirst;
    if (oldfirst != NULL) oldfirst->dirty_prev = &si->dirty_next;
    si->min_dirty_byte = to_g;
  }
}

#define SIMPLE_DIRTY_SPACE_P(s) (((s) == space_weakpair) || ((s) == space_ephemeron) || ((s) == space_symbol) || ((s) == space_port))

static void sanitize_locked_segment(seginfo *si) {
  /* If `si` is for weak pairs, ephemeron pairs, or other things that
     are guaranteed to stay on a single segment, make the segment safe
     for handling by `sweep_dirty`, where memory not occupied by
     objects in `si->locked_objects` is "zeroed" out in a way that it
     can be traversed. This is merely convenient and efficient for
     some kinds of segments, but it's required for weak and ephemeron
     pairs. */
  ISPC s = si->space & ~space_locked;

  if (SIMPLE_DIRTY_SPACE_P(s)) {
    ptr ls;
    ptr *pp, *ppend;

    /* Sort locked objects */
    si->locked_objects = ls = dosort(si->locked_objects, list_length(si->locked_objects));

    pp = build_ptr(si->number, 0);
    ppend = (ptr *)((uptr)pp + bytes_per_segment);

    /* Zero out unused memory */
    while (pp < ppend) {
      if ((ls != Snil) && (pp == UNTYPE_ANY(Scar(ls)))) {
        ptr a = Scar(ls);
        pp = (ptr *)((uptr)pp + size_object(Scar(ls)));
        while ((ls != Snil) && (Scar(ls) == a))
          ls = Scdr(ls);
      } else {
        *pp = FIX(0);
        pp++;
      }
    }
  }
}

static void sweep_dirty(void) {
  IGEN tg, mcg, youngest, min_youngest;
  ptr *pp, *ppend, *nl;
  uptr seg, d;
  ISPC s;
  IGEN from_g, to_g;
  seginfo *dirty_si, *nextsi;
  IBOOL check_locked;

  PUSH_BACKREFERENCE(Snil) /* '() => from unspecified old object */

  tg = target_generation;
  mcg = max_copied_generation;
  weaksegments_to_resweep = NULL;

  /* clear dirty segment lists for copied generations */
  for (from_g = 1; from_g <= mcg; from_g += 1) {
    for (to_g = 0; to_g < from_g; to_g += 1) {
      DirtySegments(from_g, to_g) = NULL;
    }
  }

  /* NB: could have problems if a card is moved from some current or to-be-swept (from_g, to_g) to some previously
     swept list due to a dirty_set while we sweep.  believe this can't happen as of 6/14/2013.  if it can, it
     might be sufficient to process the lists in reverse order. */
  for (from_g = mcg + 1; from_g <= static_generation; INCRGEN(from_g)) {
    for (to_g = 0; to_g <= mcg; to_g += 1) {
      for (dirty_si = DirtySegments(from_g, to_g), DirtySegments(from_g, to_g) = NULL; dirty_si != NULL; dirty_si = nextsi) {
        nextsi = dirty_si->dirty_next;
        seg = dirty_si->number;
        s = dirty_si->space;

        /* reset min dirty byte so we can detect if byte is set while card is swept */
        dirty_si->min_dirty_byte = 0xff;

        check_locked = 0;
        if (s & space_locked) {
          s &= ~space_locked;
          if (!SIMPLE_DIRTY_SPACE_P(s)) {
            /* Only consider cards that intersect with
               `locked_objects`, `unlocked_objects`, or a
               segment-spanning object from a preceding page */
            check_locked = 1;
          }
        }

        min_youngest = 0xff;
        nl = from_g == tg ? (ptr *)orig_next_loc[s] : (ptr *)S_G.next_loc[s][from_g];
        ppend = build_ptr(seg, 0);

        if (s == space_weakpair) {
          weakseginfo *next = weaksegments_to_resweep;
          find_room(space_data, 0, typemod, sizeof(weakseginfo), weaksegments_to_resweep);
          weaksegments_to_resweep->si = dirty_si;
          weaksegments_to_resweep->next = next;
        }

        d = 0;
        while (d < cards_per_segment) {
          uptr dend = d + sizeof(iptr);
          iptr *dp = (iptr *)(dirty_si->dirty_bytes + d);
          /* check sizeof(iptr) bytes at a time for 0xff */
          if (*dp == -1) {
            pp = ppend;
            ppend += bytes_per_card;
            if (pp <= nl && nl < ppend) ppend = nl;
            d = dend;
          } else {
            while (d < dend) {
              pp = ppend;
              ppend += bytes_per_card / sizeof(ptr);
              if (pp <= nl && nl < ppend) ppend = nl;

              if (dirty_si->dirty_bytes[d] <= mcg) {
                /* assume we won't find any wrong-way pointers */
                youngest = 0xff;

                if (check_locked) {
                  /* Look only at bytes that intersect with a locked or unlocked object */
                  ptr backp;
                  seginfo *prev_si;

                  youngest = sweep_dirty_intersecting(dirty_si->locked_objects, pp, ppend, tg, youngest);
                  youngest = sweep_dirty_intersecting(dirty_si->unlocked_objects, pp, ppend, tg, youngest);

                  /* Look for previous segment that might have locked objects running into this one */
                  backp = (ptr)((uptr)build_ptr(seg, 0) - ptr_bytes);
                  while (1) {
                    ISPC s2;
                    prev_si = MaybeSegInfo(ptr_get_segment(backp));
                    if (!prev_si) break;
                    s2 = prev_si->space;
                    if (!(s2 & space_locked)) break;
                    s2 &= ~space_locked;
                    if (SIMPLE_DIRTY_SPACE_P(s2)) break;
                    if ((prev_si->locked_objects != Snil) || (prev_si->unlocked_objects != Snil)) {
                      youngest = sweep_dirty_intersecting(prev_si->locked_objects, pp, ppend, tg, youngest);
                      youngest = sweep_dirty_intersecting(prev_si->unlocked_objects, pp, ppend, tg, youngest);
                      break;
                    } else {
                      backp = (ptr)(((uptr)backp) - bytes_per_segment);
                    }
                  }
                } else if ((s == space_impure)
                           || (s == space_impure_typed_object) || (s == space_count_impure)
                           || (s == space_closure)) {
                  while (pp < ppend && *pp != forward_marker) {
                    /* handle two pointers at a time */
                    relocate_dirty(pp,tg,youngest)
                    pp += 1;
                    relocate_dirty(pp,tg,youngest)
                    pp += 1;
                  }
                } else if (s == space_symbol) {
                  /* old symbols cannot overlap segment boundaries
                     since any object that spans multiple
                     generations begins at the start of a segment,
                     and symbols are much smaller (we assume)
                     than the segment size. */
                  pp = (ptr *)build_ptr(seg,0) +
                    ((pp - (ptr *)build_ptr(seg,0)) /
                     (size_symbol / sizeof(ptr))) *
                    (size_symbol / sizeof(ptr));

                  while (pp < ppend && *pp != forward_marker) { /* might overshoot card by part of a symbol.  no harm. */
                    ptr p = TYPE((ptr)pp, type_symbol);

                    youngest = sweep_dirty_symbol(p, tg, youngest);

                    pp += size_symbol / sizeof(ptr);
                  }
                } else if (s == space_port) {
                  /* old ports cannot overlap segment boundaries
                     since any object that spans multiple
                     generations begins at the start of a segment,
                     and ports are much smaller (we assume)
                     than the segment size. */
                  pp = (ptr *)build_ptr(seg,0) +
                    ((pp - (ptr *)build_ptr(seg,0)) /
                     (size_port / sizeof(ptr))) *
                    (size_port / sizeof(ptr));

                  while (pp < ppend && *pp != forward_marker) { /* might overshoot card by part of a port.  no harm. */
                    ptr p = TYPE((ptr)pp, type_typed_object);

                    youngest = sweep_dirty_port(p, tg, youngest);

                    pp += size_port / sizeof(ptr);
                  }
                } else if (s == space_impure_record) { /* abandon hope all ye who enter here */
                  uptr j; ptr p, pnext; seginfo *si;

                  /* synchronize on first record that overlaps the dirty
                     area, then relocate any mutable pointers in that
                     record and those that follow within the dirty area. */

                  /* find first segment of group of like segments */
                  j = seg - 1;
                  while ((si = MaybeSegInfo(j)) != NULL &&
                      si->space == s &&
                      si->generation == from_g)
                    j -= 1;
                  j += 1;

                  /* now find first record in segment seg */
                  /* we count on following fact: if an object spans two
                     or more segments, then he starts at the beginning
                     of a segment */
                  for (;;) {
                    p = TYPE(build_ptr(j,0),type_typed_object);
                    pnext = (ptr)((iptr)p +
                        size_record_inst(UNFIX(RECORDDESCSIZE(
                              RECORDINSTTYPE(p)))));
                    if (ptr_get_segment(pnext) >= seg) break;
                    j = ptr_get_segment(pnext) + 1;
                  }

                  /* now find first within dirty area */
                  while ((ptr *)UNTYPE(pnext, type_typed_object) <= pp) {
                    p = pnext;
                    pnext = (ptr)((iptr)p +
                        size_record_inst(UNFIX(RECORDDESCSIZE(
                              RECORDINSTTYPE(p)))));
                  }

                  /* now sweep */
                  while ((ptr *)UNTYPE(p, type_typed_object) < ppend) {
                    /* quit on end of segment */
                    if (FWDMARKER(p) == forward_marker) break;

                    youngest = sweep_dirty_record(p, tg, youngest);
                    p = (ptr)((iptr)p +
                        size_record_inst(UNFIX(RECORDDESCSIZE(
                              RECORDINSTTYPE(p)))));
                  }
                } else if (s == space_weakpair) {
                  while (pp < ppend && *pp != forward_marker) {
                    /* skip car field and handle cdr field */
                    pp += 1;
                    relocate_dirty(pp, tg, youngest)
                    pp += 1;
                  }
                } else if (s == space_ephemeron) {
                  while (pp < ppend && *pp != forward_marker) {
                    ptr p = TYPE((ptr)pp, type_pair);
                    youngest = check_dirty_ephemeron(p, tg, youngest);
                    pp += size_ephemeron / sizeof(ptr);
                  }
                } else {
                  S_error_abort("sweep_dirty(gc): unexpected space");
                }

                if (s == space_weakpair) {
                  weaksegments_to_resweep->youngest[d] = youngest;
                } else {
                  dirty_si->dirty_bytes[d] = youngest < from_g ? youngest : 0xff;
                }
                if (youngest < min_youngest) min_youngest = youngest;
              } else {
                if (dirty_si->dirty_bytes[d] < min_youngest) min_youngest = dirty_si->dirty_bytes[d];
              }
              d += 1;
            }
          }
        }
        if (s != space_weakpair) {
          record_dirty_segment(from_g, min_youngest, dirty_si);
        }
      }
    }
  }

  POP_BACKREFERENCE()
}

IGEN sweep_dirty_intersecting(ptr lst, ptr *pp, ptr *ppend, IGEN tg, IGEN youngest)
{
  ptr p, *pu, *puend;

  for (; lst != Snil; lst = Scdr(lst)) {
    p = (ptr *)Scar(lst);

    pu = (ptr*)UNTYPE_ANY(p);
    puend = (ptr*)((uptr)pu + size_object(p));

    if (((pu >= pp) && (pu < ppend))
        || ((puend >= pp) && (puend < ppend))
        || ((pu <= pp) && (puend >= ppend))) {
      /* Overlaps, so sweep */
      ITYPE t = TYPEBITS(p);

      if (t == type_pair) {
        youngest = sweep_dirty_bytes(pp, ppend, pu, puend, tg, youngest);
      } else if (t == type_closure) {
        ptr code;

        code = CLOSCODE(p);
        if (CODETYPE(code) & (code_flag_mutable_closure << code_flags_offset)) {
          youngest = sweep_dirty_bytes(pp, ppend, pu, puend, tg, youngest);
        }
      } else if (t == type_symbol) {
        youngest = sweep_dirty_symbol(p, tg, youngest);
      } else if (t == type_flonum) {
        /* nothing to sweep */
      } else {
        ptr tf = TYPEFIELD(p);
        if (TYPEP(tf, mask_vector, type_vector)
            || TYPEP(tf, mask_stencil_vector, type_stencil_vector)
            || TYPEP(tf, mask_box, type_box)
            || ((iptr)tf == type_tlc)) {
          /* impure objects */
          youngest = sweep_dirty_bytes(pp, ppend, pu, puend, tg, youngest);
        } else if (TYPEP(tf, mask_string, type_string)
                   || TYPEP(tf, mask_bytevector, type_bytevector)
                   || TYPEP(tf, mask_fxvector, type_fxvector)) {
          /* nothing to sweep */;
        } else if (TYPEP(tf, mask_record, type_record)) {
          youngest = sweep_dirty_record(p, tg, youngest);
        } else if (((iptr)tf == type_ratnum)
                   || ((iptr)tf == type_exactnum)
                   || TYPEP(tf, mask_bignum, type_bignum)) {
          /* immutable */
        } else if (TYPEP(tf, mask_port, type_port)) {
          youngest = sweep_dirty_port(p, tg, youngest);
        } else if (TYPEP(tf, mask_code, type_code)) {
          /* immutable */
        } else if (((iptr)tf == type_rtd_counts)
                   || ((iptr)tf == type_phantom)) {
          /* nothing to sweep */;
        } else {
          S_error_abort("sweep_dirty_intersection(gc): unexpected type");
        }
      }
    }
  }

  return youngest;
}

IGEN sweep_dirty_bytes(ptr *pp, ptr *ppend, ptr *pu, ptr *puend, IGEN tg, IGEN youngest)
{
  if (pu < pp) pu = pp;
  if (puend > ppend) puend = ppend;

  while (pu < puend) {
    relocate_dirty(pu,tg,youngest)
    pu += 1;
  }

  return youngest;
}

static void resweep_dirty_weak_pairs() {
  weakseginfo *ls;
  ptr *pp, *ppend, *nl, p;
  IGEN from_g, min_youngest, youngest, tg, mcg, pg;
  uptr d;

  tg = target_generation;
  mcg = max_copied_generation;

  for (ls = weaksegments_to_resweep; ls != NULL; ls = ls->next) {
    seginfo *dirty_si = ls->si;
    from_g = dirty_si->generation;
    nl = from_g == tg ? (ptr *)orig_next_loc[space_weakpair] : (ptr *)S_G.next_loc[space_weakpair][from_g];
    ppend = build_ptr(dirty_si->number, 0);
    min_youngest = 0xff;
    d = 0;
    while (d < cards_per_segment) {
      uptr dend = d + sizeof(iptr);
      iptr *dp = (iptr *)(dirty_si->dirty_bytes + d);
      /* check sizeof(iptr) bytes at a time for 0xff */
      if (*dp == -1) {
        d = dend;
        ppend += bytes_per_card;
      } else {
        while (d < dend) {
          pp = ppend;
          ppend += bytes_per_card / sizeof(ptr);
          if (pp <= nl && nl < ppend) ppend = nl;
          if (dirty_si->dirty_bytes[d] <= mcg) {
            youngest = ls->youngest[d];
            while (pp < ppend) {
              p = *pp;
              seginfo *si;

              /* handle car field */
              if (!IMMEDIATE(p) && (si = MaybeSegInfo(ptr_get_segment(p))) != NULL) {
                if (si->space & space_old) {
                  if (locked(si, p)) {
                    youngest = tg;
                  } else if (FORWARDEDP(p, si)) {
                    *pp = FWDADDRESS(p);
                    youngest = tg;
                  } else {
                    *pp = Sbwp_object;
                  }
                } else {
                  if (youngest != tg && (pg = si->generation) < youngest)
                    youngest = pg;
                }
              }

              /* skip cdr field */
              pp += 2;
            }

            dirty_si->dirty_bytes[d] = youngest < from_g ? youngest : 0xff;
            if (youngest < min_youngest) min_youngest = youngest;
          } else {
            if (dirty_si->dirty_bytes[d] < min_youngest) min_youngest = dirty_si->dirty_bytes[d];
          }
          d += 1;
        }
      }
    }
    record_dirty_segment(from_g, min_youngest, dirty_si);
  }
}

static void add_pending_guardian(ptr gdn, ptr tconc)
{
  seginfo *si = SegInfo(ptr_get_segment(tconc));
  INITGUARDIANNEXT(gdn) = si->trigger_guardians;
  si->trigger_guardians = gdn;
  si->has_triggers = 1;
}

static void add_trigger_guardians_to_recheck(ptr ls)
{
  ptr last = ls, next = GUARDIANNEXT(ls);
  while (next != NULL) {
    last = next;
    next = GUARDIANNEXT(next);
  }
  INITGUARDIANNEXT(last) = recheck_guardians_ls;
  recheck_guardians_ls = ls;
}

static ptr pending_ephemerons = NULL;
/* Ephemerons that we haven't looked at, chained through `next`. */

static ptr trigger_ephemerons = NULL;
/* Ephemerons that we've checked and added to segment triggers,
   chained through `next`. Ephemerons attached to a segment are
   chained through `trigger-next`. A #t in `trigger-next` means that
   the ephemeron has been processed, so we don't need to remove it
   from the trigger list in a segment. */

static ptr repending_ephemerons = NULL;
/* Ephemerons in `trigger_ephemerons` that we need to inspect again,
   removed from the triggering segment and chained here through
   `trigger-next`. */

static void add_ephemeron_to_pending(ptr pe) {
  /* We could call check_ephemeron directly here, but the indirection
     through `pending_ephemerons` can dramatically decrease the number
     of times that we have to trigger re-checking, especially since
     check_pending_pehemerons() is run only after all other sweep
     opportunities are exhausted. */
  EPHEMERONNEXT(pe) = pending_ephemerons;
  pending_ephemerons = pe;
}

static void add_trigger_ephemerons_to_repending(ptr pe) {
  ptr last_pe = pe, next_pe = EPHEMERONTRIGGERNEXT(pe);
  while (next_pe != NULL) {
    last_pe = next_pe;
    next_pe = EPHEMERONTRIGGERNEXT(next_pe);
  }
  EPHEMERONTRIGGERNEXT(last_pe) = repending_ephemerons;
  repending_ephemerons = pe;
}

static void check_ephemeron(ptr pe, int add_to_trigger) {
  ptr p;
  seginfo *si;
  PUSH_BACKREFERENCE(pe);

  p = Scar(pe);
  if (!IMMEDIATE(p) && (si = MaybeSegInfo(ptr_get_segment(p))) != NULL && si->space & space_old && !locked(si, p)) {
    if (FORWARDEDP(p, si)) {
      INITCAR(pe) = FWDADDRESS(p);
      relocate(&INITCDR(pe))
      if (!add_to_trigger)
        EPHEMERONTRIGGERNEXT(pe) = Strue; /* in trigger list, #t means "done" */
    } else {
      /* Not reached, so far; install as trigger */
      EPHEMERONTRIGGERNEXT(pe) = si->trigger_ephemerons;
      si->trigger_ephemerons = pe;
      si->has_triggers = 1;
      if (add_to_trigger) {
        EPHEMERONNEXT(pe) = trigger_ephemerons;
        trigger_ephemerons = pe;
      }
    }
  } else {
    relocate(&INITCDR(pe))
  }

  POP_BACKREFERENCE();
}

static void check_pending_ephemerons() {
  ptr pe, next_pe;

  pe = pending_ephemerons;
  pending_ephemerons = NULL;
  while (pe != NULL) {
    next_pe = EPHEMERONNEXT(pe);
    check_ephemeron(pe, 1);
    pe = next_pe;
  }

  pe = repending_ephemerons;
  repending_ephemerons = NULL;
  while (pe != NULL) {
    next_pe = EPHEMERONTRIGGERNEXT(pe);
    check_ephemeron(pe, 0);
    pe = next_pe;
  }
}

/* Like check_ephemeron(), but for a dirty, old-generation
   ephemeron (that was not yet added to the pending list), so we can
   be less pessimistic than setting `youngest` to the target
   generation: */
static int check_dirty_ephemeron(ptr pe, int tg, int youngest) {
  ptr p;
  seginfo *si;
  PUSH_BACKREFERENCE(pe);
 
  p = Scar(pe);
  if (!IMMEDIATE(p) && (si = MaybeSegInfo(ptr_get_segment(p))) != NULL) {
    if (si->space & space_old && !locked(si, p)) {
      if (FORWARDEDP(p, si)) {
        INITCAR(pe) = GET_FWDADDRESS(p);
        relocate(&INITCDR(pe))
        youngest = tg;
      } else {
        /* Not reached, so far; add to pending list */
        add_ephemeron_to_pending(pe);
        /* Make the consistent (but pessimistic w.r.t. to wrong-way
           pointers) assumption that the key will stay live and move
           to the target generation. That assumption covers the value
           part, too, since it can't end up younger than the target
           generation. */
        youngest = tg;
      }
    } else {
      int pg;
      if ((pg = si->generation) < youngest)
        youngest = pg;
      relocate_dirty(&INITCDR(pe), tg, youngest)
    }
  } else {
    /* Non-collectable key means that the value determines
       `youngest`: */
    relocate_dirty(&INITCDR(pe), tg, youngest)
  }

  POP_BACKREFERENCE()

  return youngest;
}

static void clear_trigger_ephemerons() {
  ptr pe;

  if (pending_ephemerons != NULL)
    S_error_abort("clear_trigger_ephemerons(gc): non-empty pending list");

  pe = trigger_ephemerons;
  trigger_ephemerons = NULL;
  while (pe != NULL) {
    if (EPHEMERONTRIGGERNEXT(pe) == Strue) {
      /* The ephemeron was triggered and retains its key and value */
    } else {
      /* Key never became reachable, so clear key and value */
      INITCAR(pe) = Sbwp_object;
      INITCDR(pe) = Sbwp_object;
    }
    pe = EPHEMERONNEXT(pe);
  }
}

#ifdef ENABLE_OBJECT_COUNTS
static uptr total_size_so_far() {
  IGEN g;
  int i;
  uptr total = 0;

  for (g = 0; g <= static_generation; g += 1) {
    for (i = 0; i < countof_types; i += 1) {
      uptr bytes;
      bytes = S_G.bytesof[g][i];
      if (bytes == 0) bytes = S_G.countof[g][i] * S_G.countof_size[i];
      total += bytes;
    }
    total += S_G.phantom_sizes[g];
  }

  return total - count_root_bytes;
}
#endif

/* **************************************** */

#ifdef ENABLE_MEASURE

static void init_measure(IGEN min_gen, IGEN max_gen) {
  uptr init_stack_len = 1024;

  min_measure_generation = min_gen;
  max_measure_generation = max_gen;
  
  find_room(space_data, 0, typemod, init_stack_len, measure_stack_start);
  measure_stack = (ptr *)measure_stack_start;
  measure_stack_limit = (ptr *)((uptr)measure_stack_start + init_stack_len);

  measured_seginfos = Snil;

  measure_all_enabled = 1;
}

static void finish_measure() {
  ptr ls;

  for (ls = measured_seginfos; ls != Snil; ls = Scdr(ls)) {
    seginfo *si = (seginfo *)Scar(ls);
    si->measured_mask = NULL;
    si->trigger_ephemerons = NULL;
  }

  measure_all_enabled = 0;
}

static void init_counting_mask(seginfo *si) {
  find_room(space_data, 0, typemod, ptr_align(segment_bitmap_bytes), si->counting_mask);
  memset(si->counting_mask, 0, segment_bitmap_bytes);
}

static void init_measure_mask(seginfo *si) {
  find_room(space_data, 0, typemod, ptr_align(segment_bitmap_bytes), si->measured_mask);
  memset(si->measured_mask, 0, segment_bitmap_bytes);
  
  measured_seginfos = S_cons_in(space_new, 0, (ptr)si, measured_seginfos);
}

#define measure_unreached(si, p) \
  (!si->measured_mask \
   || !(si->measured_mask[segment_bitmap_byte(p)] & segment_bitmap_bit(p)))

#define measure_mask_set(mm, si, p) \
  mm[segment_bitmap_byte(p)] |= segment_bitmap_bit(p)
#define measure_mask_unset(mm, si, p) \
  mm[segment_bitmap_byte(p)] -= segment_bitmap_bit(p)

static void push_measure(ptr p)
{
  seginfo *si = MaybeSegInfo(ptr_get_segment(p));

  if (!si)
    return;

  if (si->space & space_old) {
    /* We must be in a GC--measure fusion, so switch back to GC */
    if (!locked(si, p)) {
      relocate(&p)
      return;
    }
  }

  if (si->generation > max_measure_generation)
    return;
  else if (si->generation < min_measure_generation) {
    /* this only happens in fusion mode, too; si must be a new segment */
    return;
  } else {
    uptr byte = segment_bitmap_byte(p);
    uptr bit = segment_bitmap_bit(p);

    if (!si->measured_mask)
      init_measure_mask(si);
    else if (si->measured_mask[byte] & bit)
      return;

    si->measured_mask[byte] |= bit;
  }

  if (si->trigger_ephemerons) {
    add_trigger_ephemerons_to_pending_measure(si->trigger_ephemerons);
    si->trigger_ephemerons = NULL;
  }

  if (measure_stack == measure_stack_limit) {
    uptr sz = ptr_bytes * (measure_stack_limit - measure_stack_start);
    uptr new_sz = 2*sz;
    ptr new_measure_stack;
    find_room(space_data, 0, typemod, ptr_align(new_sz), new_measure_stack);
    memcpy(new_measure_stack, measure_stack_start, sz);
    measure_stack_start = (ptr *)new_measure_stack;
    measure_stack_limit = (ptr *)((uptr)new_measure_stack + new_sz);
    measure_stack = (ptr *)((uptr)new_measure_stack + sz);
  }
  
  *(measure_stack++) = p;
}

static void measure_add_stack_size(ptr stack, uptr size) {
  seginfo *si = SegInfo(ptr_get_segment(stack));
  if (!(si->space & space_old)
      && (si->generation <= max_measure_generation)
      && (si->generation >= min_measure_generation))
    measure_total += size;
}

static void add_ephemeron_to_pending_measure(ptr pe) {
  EPHEMERONNEXT(pe) = pending_measure_ephemerons;
  pending_measure_ephemerons = pe;
}

static void add_trigger_ephemerons_to_pending_measure(ptr pe) {
  ptr last_pe = pe, next_pe = EPHEMERONNEXT(pe);

  while (next_pe != NULL) {
    last_pe = next_pe;
    next_pe = EPHEMERONNEXT(next_pe);
  }
  EPHEMERONNEXT(last_pe) = pending_measure_ephemerons;
  pending_measure_ephemerons = pe;
}

static void check_ephemeron_measure(ptr pe) {
  ptr p;
  seginfo *si;

  p = Scar(pe);
  if (!IMMEDIATE(p) && (si = MaybeSegInfo(ptr_get_segment(p))) != NULL
      && (si->generation <= max_measure_generation)
      && (si->generation >= min_measure_generation)
      && (!(si->space & space_old) || !FORWARDEDP(p, si))
      && (measure_unreached(si, p)
          || (si->counting_mask
              && (si->counting_mask[segment_bitmap_byte(p)] & segment_bitmap_bit(p))))) {
    /* Not reached, so far; install as trigger */
    EPHEMERONNEXT(pe) = si->trigger_ephemerons;
    si->trigger_ephemerons = pe;
    if (!si->measured_mask)
      init_measure_mask(si); /* so triggers are cleared at end */
    return;
  }

  p = Scdr(pe);
  if (!IMMEDIATE(p))
    push_measure(p);
}

static void check_pending_measure_ephemerons() {
  ptr pe, next_pe;

  pe = pending_measure_ephemerons;
  pending_measure_ephemerons = NULL;
  while (pe != NULL) {
    next_pe = EPHEMERONNEXT(pe);
    check_ephemeron_measure(pe);
    pe = next_pe;
  }
}

void gc_measure_one(ptr p) {
  seginfo *si = SegInfo(ptr_get_segment(p));

  if (si->trigger_ephemerons) {
    add_trigger_ephemerons_to_pending_measure(si->trigger_ephemerons);
    si->trigger_ephemerons = NULL;
  }
  
  measure(p);

  (void)flush_measure_stack();
}

IBOOL flush_measure_stack() {
  if ((measure_stack <= measure_stack_start)
      && !pending_measure_ephemerons)
    return 0;
  
  while (1) {
    while (measure_stack > measure_stack_start)
      measure(*(--measure_stack));

    if (!pending_measure_ephemerons)
      break;
    check_pending_measure_ephemerons();
  }

  return 1;
}

ptr S_count_size_increments(ptr ls, IGEN generation) {
  ptr l, totals = Snil, totals_prev = NULL;

  tc_mutex_acquire();

  init_measure(0, generation);

  for (l = ls; l != Snil; l = Scdr(l)) {
    ptr p = Scar(l);
    if (!IMMEDIATE(p)) {
      seginfo *si = si = SegInfo(ptr_get_segment(p));

      if (!si->measured_mask)
        init_measure_mask(si);
      measure_mask_set(si->measured_mask, si, p);

      if (!si->counting_mask)
        init_counting_mask(si);
      measure_mask_set(si->counting_mask, si, p);
    }
  }

  for (l = ls; l != Snil; l = Scdr(l)) {
    ptr p = Scar(l);

    measure_total = 0;

    if (!IMMEDIATE(p)) {
      seginfo *si = si = SegInfo(ptr_get_segment(p));
      measure_mask_unset(si->counting_mask, si, p);
      gc_measure_one(p);
    }

    p = Scons(FIX(measure_total), Snil);
    if (totals_prev)
      Scdr(totals_prev) = p;
    else
      totals = p;
    totals_prev = p;
  }

  for (l = ls; l != Snil; l = Scdr(l)) {
    ptr p = Scar(l);
    if (!IMMEDIATE(p)) {
      seginfo *si = si = SegInfo(ptr_get_segment(p));
      si->counting_mask = NULL;
    }
  }

  finish_measure();

  tc_mutex_release();

  return totals;
}

#endif
