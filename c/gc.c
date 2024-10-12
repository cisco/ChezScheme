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

#include "sort.h"
#ifndef WIN32
#include <sys/wait.h>
#endif /* WIN32 */
#include "popcount.h"
#include <assert.h>

/* 
   GC Implementation
   -----------------

   The copying, sweeping, and marking operations that depend on
   object's shape are mostly implemented in "mkgc.ss". That script
   generates "gc-ocd.inc" (for modes where object counting and
   backpointers are disabled), "gc-oce.inc", and "gc-par.inc". The
   rest of the implementation here can still depend on representation
   details, though, especially for pairs, weak pairs, and ephemerons.

   GC Copying versus Marking
   -------------------------

   Generations range from 0 to `S_G.max_nonstatic_generation` plus a
   static generation. After an object moves to the static generation,
   it doesn't move anymore. In the case of code objects, relocations
   may be discarded when the code object moves into a static
   generation.

   For the most part, collecting generations 0 through MAX_CG (= max
   copied generation) to MIN_TG to MAX_TG (= target generation) means
   copying objects from old segments into fresh segments generations
   MIN_TG through MAX_TG. Note that MAX_TG is either the same as or
   one larger than MAX_CG. For objects in generation 0 through MAX_CG,
   the target generation is either one more than the current
   generation or it's MIN_TG.

   Objects might be marked [and swept] instead of copied [and swept]
   as triggered by two possibilities: one or more objects on the
   source segment are immobile (subsumes locked) or MAX_CG == MAX_TG
   and the object is on a MAX_CG segment that hasn't been discovered as
   sparse by a previous marking (non-copying) pass. Segments with
   marked objects are promoted to the target generation.

   As a special case, locking on `space_new` does not mark all objects
   on that segment, because dirty-write handling cannot deal with
   `space_new`; only locked objects stay on the old segment in that
   case, and they have to be marked by looking at a list of locked
   objects.

   During a collection, the `old_space` flag is set on a segment if
   objects are being copied out of it or marked on it; that is,
   `old_space` is set if the segment starts out in one of the
   generations 0 through mgc. If a segment is being marked instead of
   copied, the `use_marks` bit is also set; note that the bit will not
   be set for a `space_new` segment, and locked objects in that space
   will be specially marked.

   Marking an object means setting a bit in `marked_mask`, which is
   allocated as needed. Any segments that ends up with a non-NULL
   `marked_mask` is kept in its new generation at the end of
   collection. If a marked object spans multiple segments, then
   `masked_mask` is created across all of the segments. It's possible
   for a segment to end up with `marked_mask` even though `use_marks`
   was not set: an marked object spanned into the segment, or it's a
   `space_new` segment with locked objects; in that case, other
   objects will be copied out of the segment, because `use_marks` is
   how relocation decides whether to copy or mark.

   If an object is copied, then its first word is set to
   `forward_marker` and its second word is set to the new address.
   Obviously, that doesn't happen if an object is marked. So, to test
   whether an object has been reached:

   * the object must be in an `old_space` segment, otherwise it counts
     as reached because it's in a generation older than MAX_CG;

   * the object either starts with `forward_marker` or its mark bit is
     set (and those are mutually exclusive).

   Besides the one bit for the start of an object in the mark mask,
   extra bits for the object content may be set as well. Those extra
   bits tell the dirty-object sweeper which words in a previously
   marked page should be swept and which should be skipped, so the
   extra bits are only needed for impure objects in certain kinds of
   spaces. Only every alternate word needs to be marked that way, so
   half of the mark bits are usually irrelevant; the exception is that
   flonums can be between normal object-start positions, so those mark
   bits can matter, at least if we're preserving `eq?` on flonums (but
   the bits are not relevant to dirty-object sweeping, since flonums
   don't have pointer fields).

   It's ok to sweep an object multiple times, but that's to be be
   avoided if possible.

   Pending Ephemerons and Guardians
   --------------------------------

   Ephemerons and guardians act as a kind of "and": an object stays
   reachable only if some other object (besides the the
   ephemeron/guardian itself) is reachable or not. Instead of
   rechecking all guardians and ephemerons constantly, the collector
   queues pending guardians and ephemerons on the segment where the
   relevant object lives. If any object on that segment is discovered
   to be reachable (i.e., copied or marked), the guardian/ephemeron is
   put into a list of things to check again.

   Parallel Collection
   -------------------

   Parallel mode runs `sweep_generation` concurrently in multiple
   sweeper threads. It relies on a number of invariants:

    * There are no attempts to take tc_mutex during sweeping. To the
      degree that locking is needed (e.g., to allocate new segments),
      the allocation mutex is used. No other locks can be taken while
      that one is held.

      Along similar lines, get_thread_context() must not be used,
      because the sweepers threads are not the same as Scheme threads,
      and a sweeper thread may temporarily adapt a different Scheme
      thread context.

    * To copy from or mark on a segment, a sweeper must own the
      segment. A sweeper during sweeping may encounter a "remote"
      reference to a segment that it doesn't own; in that case, it
      registers the object containing the remote reference to be
      re-swept by the sweeper that owns the target of the reference.

      A segment is owned by the thread that originally allocated it.
      When a GC starts, for old-space segments that are owned by
      threads that do no have a corresponding sweeper, the segment is
      moved to the main collecting thread's ownership.

      Note that copying and marking are constrained so that they don't
      have to recursively copy or mark. In some cases, this property
      is achieved by not caring whether a reference goes to an old
      copy or unmarked object; for example, a record type's size field
      will be the same in both places, so either copy can be used to
      determine a record size of copying. A record type's parent field
      would not be available, however, since it can get overwritten
      with forwarding information.

    * An object that is marked does not count as "remote".

      Sweepers might attempt to access marked-object information at
      the same time that it is being updated by the owning sweeper.
      It's ok if the non-owning sweepers get stale information;
      they'll just send the referencing object to the owning thread
      for re-sweeping. A write fence ensures that non-owning sweepers
      do not inspect mark-bitmap bits that have not been initialized.

    * Normally, a sweeper that encounters a remote reference can
      continue sweeping and eventually register the remote re-sweep.
      An object is swept by only one sweeper at a time; if multiple
      remote references to different sweepers are discovered in an
      object, it is sent to only one of the remote sweepers, and that
      sweeper will eventually send on the object to the other sweeper.
      At worst, each object is swept N times for N sweepers.

      In rare cases, a sweeper cannot fully process an object, because
      doing so would require inspecting a remote object. For example,
      a record type's pointer mask or a stack frame's live-pointer
      mask can be a bignum, and the bignum might be remote. In those
      cases, the object might have to be sent back to the original
      sweeper, and so on. In the worst case, the object can be swept
      more than N times ---- but, again, this case rarely happens at
      all, and sweeping more than N times is very unlikely.

    * In counting/backtrace/measure mode, "parallel" collection can be
      used to preserve object ownership, but no extra sweeper threads
      are used. So, it is not really parallel, and counting and
      backtrace operations do not need locks.

      Counting needs to copy or mark a record-type or object-count
      object as part of a copy or mark operation, which is otherwise
      not allowed (but ok with counting, since it's not actually in
      parallel). For that purpose, `relocate_pure_in_owner`
      temporarily switches to the owning thread.

*/

/* locally defined functions */
static IGEN copy(thread_gc *tgc, ptr pp, seginfo *si, ptr *dest);
static IGEN mark_object(thread_gc *tgc, ptr pp, seginfo *si);
static void sweep(thread_gc *tgc, ptr p, IGEN from_g);
static void sweep_in_old(thread_gc *tgc, ptr p);
static void sweep_object_in_old(thread_gc *tgc, ptr p);
static IBOOL object_directly_refers_to_self(ptr p);
static ptr copy_stack(thread_gc *tgc, ptr old, iptr *length, iptr clength);
static void resweep_weak_pairs(thread_gc *tgc, seginfo *oldweakspacesegments);
static void forward_or_bwp(thread_gc *tgc, IGEN from_g, ptr *pp, ptr p);
static void sweep_generation(thread_gc *tgc);
static iptr sweep_from_stack(thread_gc *tgc);
static void enlarge_stack(thread_gc *tgc, ptr *stack, ptr *stack_start, ptr *stack_limit, uptr grow_at_least);
static uptr size_object(ptr p);
static iptr sweep_typed_object(thread_gc *tgc, ptr p, IGEN from_g);
static void sweep_symbol(thread_gc *tgc, ptr p, IGEN from_g);
static void sweep_port(thread_gc *tgc, ptr p, IGEN from_g);
static void sweep_thread(thread_gc *tgc, ptr p);
static void sweep_continuation(thread_gc *tgc, ptr p, IGEN from_g);
static void sweep_record(thread_gc *tgc, ptr x, IGEN from_g);
static IGEN sweep_dirty_record(thread_gc *tgc, ptr x, IGEN youngest);
static IGEN sweep_dirty_port(thread_gc *tgc, ptr x, IGEN youngest);
static IGEN sweep_dirty_symbol(thread_gc *tgc, ptr x, IGEN youngest);
static void sweep_code_object(thread_gc *tgc, ptr co, IGEN from_g);
static void record_dirty_segment(IGEN from_g, IGEN to_g, seginfo *si);
static void setup_sweep_dirty(thread_gc *tgc);
static uptr sweep_dirty_segments(thread_gc *tgc, seginfo **dirty_segments);
static void resweep_dirty_weak_pairs(thread_gc *tgc);
static void mark_untyped_data_object(thread_gc *tgc, ptr p, uptr len, seginfo *si);
static void add_pending_guardian(ptr gdn, ptr tconc);
static void add_trigger_guardians_to_recheck(ptr ls);
static void add_ephemeron_to_pending(thread_gc *tgc, ptr p);
static void add_trigger_ephemerons_to_pending(thread_gc *tgc, ptr p);
static void check_triggers(thread_gc *tgc, seginfo *si);
static void check_ephemeron(thread_gc *tgc, ptr pe);
static void check_pending_ephemerons(thread_gc *tgc);
static int check_dirty_ephemeron(thread_gc *tgc, ptr pe, int youngest);
static void finish_pending_ephemerons(thread_gc *tgc, seginfo *si);
static void init_fully_marked_mask(thread_gc *tgc, IGEN g);
static void copy_and_clear_list_bits(thread_gc *tgc, seginfo *oldspacesegments);

#ifdef ENABLE_OBJECT_COUNTS
static uptr total_size_so_far();
static uptr list_length(ptr ls);
#endif
static uptr target_generation_space_so_far(thread_gc *tgc);

#ifdef ENABLE_MEASURE
static void init_measure(thread_gc *tgc, IGEN min_gen, IGEN max_gen);
static void finish_measure();
static void measure(thread_gc *tgc, ptr p);
static void flush_measure_stack(thread_gc *tgc);
static void init_measure_mask(thread_gc *tgc, seginfo *si);
static void init_counting_mask(thread_gc *tgc, seginfo *si);
static void push_measure(thread_gc *tgc, ptr p);
static void measure_add_stack_size(ptr stack, uptr size);
static void add_ephemeron_to_pending_measure(thread_gc *tgc, ptr pe);
static void add_trigger_ephemerons_to_pending_measure(ptr pe);
static void check_ephemeron_measure(thread_gc *tgc, ptr pe);
static void check_pending_measure_ephemerons(thread_gc *tgc);
#endif

#ifdef ENABLE_PARALLEL
/* # define ENABLE_TIMING */
#endif

#ifdef ENABLE_TIMING
#include <sys/time.h>
static uptr get_real_time () {
  struct timeval now;
  gettimeofday(&now, NULL);
  return ((uptr) now.tv_sec) * 1000 + ((uptr) now.tv_usec) / 1000;
}
static uptr get_cpu_time () {
  struct timespec now;
  clock_gettime(CLOCK_THREAD_CPUTIME_ID, &now);
  return ((uptr) now.tv_sec) * 1000 + ((uptr) now.tv_nsec) / 1000000;
}
# define GET_REAL_TIME(x) uptr x = get_real_time()
# define GET_CPU_TIME(x) uptr x = get_cpu_time()
# define ACCUM_REAL_TIME(a, y, x) uptr y = get_real_time() - x; a += y
# define ACCUM_CPU_TIME(a, y, x) uptr y = get_cpu_time() - x; a += y
# define REPORT_TIME(e) e
static uptr collect_accum, all_accum, par_accum;
# define COUNT_SWEPT_BYTES(start, end) num_swept_bytes += ((uptr)TO_PTR(end) - (uptr)TO_PTR(start))
# define ADJUST_COUNTER(e) e
#else
# define GET_REAL_TIME(x) do { } while (0)
# define GET_CPU_TIME(x) do { } while (0)
# define ACCUM_REAL_TIME(a, y, x) do { } while (0)
# define ACCUM_CPU_TIME(a, y, x) do { } while (0)
# define REPORT_TIME(e) do { } while (0)
# define COUNT_SWEPT_BYTES(start, end) do { } while (0)
# define ADJUST_COUNTER(e) do { } while (0)
#endif

#if defined(MIN_TG) && defined(MAX_TG)
# if MIN_TG == MAX_TG
#  define NO_DIRTY_NEWSPACE_POINTERS
# endif
#endif

/* #define DEBUG */

/* initialized and used each gc cycle.  any others should be defined in globals.h */
static ptr tlcs_to_rehash;
static ptr conts_to_promote;
static ptr recheck_guardians_ls;
static seginfo *resweep_weak_segments;

#ifdef ENABLE_OBJECT_COUNTS
static int measure_all_enabled;
static uptr count_root_bytes;
#endif

/* max_cg: maximum copied generation, i.e., maximum generation subject to collection.  max_cg >= 0 && max_cg <= static_generation.
 * min_tg: minimum target generation.  max_tg == 0 ? min_tg == 0 : min_tg > 0 && min_tg <= max_tg;
 * max_tg: maximum target generation.  max_tg == max_cg || max_tg == max_cg + 1.
 * Objects in generation g are collected into generation MIN(max_tg, MAX(min_tg, g+1)).
 */
#if defined(MAX_CG) && defined(MIN_TG) && defined(MAX_TG)
#else
static IGEN MAX_CG, MIN_TG, MAX_TG;
#endif

#if defined(MIN_TG) && defined(MAX_TG) && (MIN_TG == MAX_TG)
# define TARGET_GENERATION(si) MIN_TG
# define compute_target_generation(g) MIN_TG 
# define CONSTANT_TARGET_GENERATION
#else
# define TARGET_GENERATION(si) si->generation
FORCEINLINE IGEN compute_target_generation(IGEN g) {
  return g == MAX_TG ? g : g < MIN_TG ? MIN_TG : g + 1;
}
#endif

static octet *fully_marked_mask[static_generation+1];

static const int sweep_stack_min_size = 256;

#define push_sweep(p) do {                                              \
    if (tgc->sweep_stack == tgc->sweep_stack_limit)                     \
      enlarge_stack(tgc, &tgc->sweep_stack, &tgc->sweep_stack_start, &tgc->sweep_stack_limit, ptr_bytes); \
    *(ptr *)TO_VOIDP(tgc->sweep_stack) = p;                             \
    tgc->sweep_stack = (ptr)((uptr)tgc->sweep_stack + ptr_bytes);       \
  } while (0)

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
# define SET_BACKREFERENCE(p) sweep_from = p
# define PUSH_BACKREFERENCE(p) ptr old_sweep_from = sweep_from; SET_SWEEP_FROM(p);
# define POP_BACKREFERENCE() SET_SWEEP_FROM(old_sweep_from);
# define ADD_BACKREFERENCE_FROM(p, from_p, tg) do { \
    IGEN TG = tg;                                                       \
    if ((S_G.enable_object_backreferences) && (TG < static_generation)) \
      S_G.gcbackreference[TG] = S_cons_in(tgc->tc, space_impure, TG,       \
                                          S_cons_in(tgc->tc, space_impure, TG, p, from_p), \
                                          S_G.gcbackreference[TG]);     \
  } while (0)
# define ADD_BACKREFERENCE(p, tg) ADD_BACKREFERENCE_FROM(p, sweep_from, tg)
#else
# define BACKREFERENCES_ENABLED 0
# define WITH_TOP_BACKREFERENCE(v, e) e
# define SET_BACKREFERENCE(p) do { } while (0)
# define PUSH_BACKREFERENCE(p)
# define POP_BACKREFERENCE()
# define ADD_BACKREFERENCE_FROM(p, from_p, from_g)
# define ADD_BACKREFERENCE(p, from_g)
#endif

#if !defined(PTHREADS)
# undef ENABLE_PARALLEL
#endif

#ifdef ENABLE_PARALLEL

static int in_parallel_sweepers = 0;

#ifdef USE_PAR_SWEEPERS_WORKAROUND
FORCEINLINE int get_in_parallel_sweepers() { return in_parallel_sweepers; }
# define PAR_SWEEPERS_WORKAROUND() int in_parallel_sweepers = get_in_parallel_sweepers()
#endif

#define HAS_SWEEPER_WRT(t_tc, tc) 1

# define GC_MUTEX_ACQUIRE() alloc_mutex_acquire()
# define GC_MUTEX_RELEASE() alloc_mutex_release()

/* shadows `tgc` binding in context: */
# define BLOCK_SET_THREAD(a_tgc) thread_gc *tgc = a_tgc

# define SEGMENT_IS_LOCAL(si, p) (((si)->creator == tgc) || marked(si, p) || !in_parallel_sweepers)
# define FLUSH_REMOTE_BLOCK thread_gc *remote_tgc = NULL;
# define RECORD_REMOTE(si) remote_tgc = si->creator
# define FLUSH_REMOTE(tgc, p) do {                            \
    if (remote_tgc != NULL)                                   \
      push_remote_sweep(tgc, p, remote_tgc);                  \
  } while (0)
# define ASSERT_EMPTY_FLUSH_REMOTE() do {                            \
    if (remote_tgc != NULL) S_error_abort("non-empty remote flush"); \
  } while (0);

static void setup_sweepers(thread_gc *tgc);
static void run_sweepers(void);
static void teardown_sweepers(void);
# define parallel_sweep_generation(tgc) run_sweepers()
# define parallel_sweep_dirty_and_generation(tgc) run_sweepers()

static void push_remote_sweep(thread_gc *tgc, ptr p, thread_gc *remote_tgc);
static void send_and_receive_remote_sweeps(thread_gc *tgc);

#define SWEEPER_NONE             0
#define SWEEPER_READY            1
#define SWEEPER_SWEEPING         2
#define SWEEPER_WAITING_FOR_WORK 3

typedef struct {
  int status;
  s_thread_cond_t done_cond, work_cond;
  thread_gc *first_tgc, *last_tgc;

  iptr num_swept_bytes;

#ifdef ENABLE_TIMING
  int remotes_sent, remotes_received;
  uptr step, sweep_accum;
#endif
} gc_sweeper;

static gc_sweeper sweepers[maximum_parallel_collect_threads+1];
static int num_sweepers;

# define PARALLEL_UNUSED    UNUSED
# define NO_PARALLEL_UNUSED /* empty */

#else

#define HAS_SWEEPER_WRT(t_tc, tc) (t_tc == tc)

# define GC_MUTEX_ACQUIRE() do { } while (0)
# define GC_MUTEX_RELEASE() do { } while (0)

# define BLOCK_SET_THREAD(a_tgc) do { } while (0)

# define SEGMENT_IS_LOCAL(si, p) 1
# define FLUSH_REMOTE_BLOCK /* empty */
# define RECORD_REMOTE(si) do { } while (0)
# define FLUSH_REMOTE(tgc, p) do { } while (0)
# define ASSERT_EMPTY_FLUSH_REMOTE() do { } while (0)

# define setup_sweepers(tgc) do { } while (0)
# define parallel_sweep_generation(tgc) do { sweep_generation(tgc); } while (0)
# define parallel_sweep_dirty_and_generation(tgc) do { sweep_dirty(tgc); sweep_generation(tgc); } while (0)
# define send_and_receive_remote_sweeps(tgc) do { } while (0)
# define teardown_sweepers() do { } while (0)
static void sweep_dirty(thread_gc *tgc);

# define PARALLEL_UNUSED    /* empty */
# define NO_PARALLEL_UNUSED UNUSED

#endif

#ifndef PAR_SWEEPERS_WORKAROUND
# define PAR_SWEEPERS_WORKAROUND() do { } while (0)
#endif

#define SWEEP_NO_CHANGE        0
#define SWEEP_CHANGE_PROGRESS  1

#if ptr_alignment == 2
# define record_full_marked_mask 0x55
# define record_high_marked_bit  0x40
# define mask_bits_to_list_bits_mask(m) ((m) | ((m) << 1))
#elif ptr_alignment == 1
# define record_full_marked_mask 0xFF
# define record_high_marked_bit  0x80
# define mask_bits_to_list_bits_mask(m) (m)
#endif

#define segment_sufficiently_compact_bytes ((bytes_per_segment * 3) / 4)
#define chunk_sufficiently_compact(nsegs) ((nsegs) >> 2)
  
/* Values for a guardian entry's `pending` field when it's added to a
   seginfo's pending list: */
enum {
  GUARDIAN_PENDING_HOLD,
  GUARDIAN_PENDING_FINAL
};

#ifdef ENABLE_OBJECT_COUNTS
uptr list_length(ptr ls) {
  uptr i = 0;
  while (ls != Snil) { ls = Scdr(ls); i += 1; }
  return i;
}
#endif

#define init_mask(tgc, dest, tg, init) do {                             \
    octet *MASK;                                                        \
    find_gc_room_voidp(tgc, space_data, tg, ptr_align(segment_bitmap_bytes), MASK); \
    memset(MASK, init, segment_bitmap_bytes);                           \
    STORE_FENCE();                                                      \
    dest = MASK;                                                        \
    tgc->bitmask_overhead[tg] += ptr_align(segment_bitmap_bytes);       \
  } while (0)

#define marked(si, p) (si->marked_mask && (si->marked_mask[segment_bitmap_byte(p)] & segment_bitmap_bit(p)))

#ifdef NO_NEWSPACE_MARKS
# define new_marked(si, p) 0
# define CAN_MARK_AND(x) 0
#else
# define new_marked(si, p) marked(si, p)
# define CAN_MARK_AND(x) x
#endif

static void init_fully_marked_mask(thread_gc *tgc, IGEN g) {
  GC_MUTEX_ACQUIRE();
  if (!fully_marked_mask[g]) {
    init_mask(tgc, fully_marked_mask[g], g, 0xFF);
  }
  GC_MUTEX_RELEASE();
}

#ifdef PRESERVE_FLONUM_EQ

static void flonum_set_forwarded(thread_gc *tgc, ptr p, seginfo *si) {
  if (!si->forwarded_flonums)
    init_mask(tgc, si->forwarded_flonums, 0, 0);
  si->forwarded_flonums[segment_bitmap_byte(p)] |= segment_bitmap_bit(p);
}

static int flonum_is_forwarded_p(ptr p, seginfo *si) {
  if (!si->forwarded_flonums)
    return 0;
  else
    return si->forwarded_flonums[segment_bitmap_byte(p)] & segment_bitmap_bit(p);
}

# define FLONUM_FWDADDRESS(p) *(ptr*)TO_VOIDP(UNTYPE(p, type_flonum))

# define FORWARDEDP(p, si) ((TYPEBITS(p) == type_flonum) ? flonum_is_forwarded_p(p, si) : (FWDMARKER(p) == forward_marker))
# define GET_FWDADDRESS(p) ((TYPEBITS(p) == type_flonum) ? FLONUM_FWDADDRESS(p) : FWDADDRESS(p))
#else
# define FORWARDEDP(p, si) (FWDMARKER(p) == forward_marker && TYPEBITS(p) != type_flonum)
# define GET_FWDADDRESS(p) FWDADDRESS(p)
#endif

#ifdef ENABLE_OBJECT_COUNTS
# define ELSE_MEASURE_NONOLDSPACE(p) \
  else if (measure_all_enabled)      \
    push_measure(tgc, p);
#else
# define ELSE_MEASURE_NONOLDSPACE(p) /* empty */
#endif

/* use relocate_pure for newspace fields that can't point to younger
   objects or where there's no need to track generations */

#define relocate_pure(ppp) do {                 \
    ptr* PPP = ppp; ptr PP = *PPP;              \
    relocate_pure_help(PPP, PP);                \
  } while (0)

#define relocate_pure_help(ppp, pp) do {     \
    seginfo *SI;                             \
    if (!FIXMEDIATE(pp) && (SI = MaybeSegInfo(ptr_get_segment(pp))) != NULL) {  \
      if (SI->old_space)                      \
        relocate_pure_help_help(ppp, pp, SI); \
      ELSE_MEASURE_NONOLDSPACE(pp)            \
    }                                         \
  } while (0)

#define relocate_pure_help_help(ppp, pp, si) do {    \
    if (SEGMENT_IS_LOCAL(si, pp)) {                  \
      if (FORWARDEDP(pp, si))                        \
        *ppp = GET_FWDADDRESS(pp);                   \
      else if (!new_marked(si, pp))                  \
        mark_or_copy_pure(ppp, pp, si);              \
    } else                                           \
      RECORD_REMOTE(si);                             \
  } while (0)

#define relocate_code(pp, si) do {              \
    if (si->old_space) {                        \
      if (SEGMENT_IS_LOCAL(si, pp)) {           \
        if (FWDMARKER(pp) == forward_marker)    \
          pp = GET_FWDADDRESS(pp);              \
        else if (!new_marked(si, pp))           \
          mark_or_copy_pure(&pp, pp, si);       \
      } else                                    \
        RECORD_REMOTE(si);                      \
    } ELSE_MEASURE_NONOLDSPACE(pp)              \
  } while (0)

#define mark_or_copy_pure(dest, p, si) do {   \
    if (CAN_MARK_AND(si->use_marks))          \
      (void)mark_object(tgc, p, si);          \
    else                                      \
      (void)copy(tgc, p, si, dest);           \
  } while (0)

#define relocate_pure_now(ppp) do {           \
    FLUSH_REMOTE_BLOCK                        \
    relocate_pure(ppp);                       \
    ASSERT_EMPTY_FLUSH_REMOTE();              \
  } while (0)

#if defined(ENABLE_PARALLEL) && defined(ENABLE_OBJECT_COUNTS)
static void do_relocate_pure_in_owner(thread_gc *tgc, ptr *ppp) {
  seginfo *si;
  ptr pp = *ppp;
  if (!FIXMEDIATE(pp)
      && (si = MaybeSegInfo(ptr_get_segment(pp))) != NULL
      && si->old_space) {
    BLOCK_SET_THREAD(si->creator);
    relocate_pure_now(ppp);
  } else {
    relocate_pure_now(ppp);
  }
}
# define relocate_pure_in_owner(ppp) do_relocate_pure_in_owner(tgc, ppp)
#else
# define relocate_pure_in_owner(pp) relocate_pure(pp)
#endif

/* use relocate_impure for newspace fields that can point to younger objects */

#ifdef NO_DIRTY_NEWSPACE_POINTERS

# define relocate_impure_help(PPP, PP, FROM_G) do {(void)FROM_G; relocate_pure_help(PPP, PP);} while (0)
# define relocate_impure(PPP, FROM_G) do {(void)FROM_G; relocate_pure(PPP);} while (0)

# define NO_DIRTY_NEWSPACE_UNUSED    UNUSED

#else /* !NO_DIRTY_NEWSPACE_POINTERS */

#define relocate_impure(ppp, from_g) do {                       \
    ptr* PPP = ppp; ptr PP = *PPP; IGEN FROM_G = from_g;        \
    relocate_impure_help(PPP, PP, FROM_G);                      \
  } while (0)

#define relocate_impure_help(ppp, pp, from_g) do {                      \
    seginfo *SI;                                                        \
    if (!FIXMEDIATE(pp) && (SI = MaybeSegInfo(ptr_get_segment(pp))) != NULL) { \
      if (SI->old_space)                                                \
        relocate_impure_help_help(ppp, pp, from_g, SI);                 \
      ELSE_MEASURE_NONOLDSPACE(pp)                                      \
    }                                                                   \
  } while (0)

#define relocate_impure_help_help(ppp, pp, from_g, si) do {             \
    IGEN __to_g;                                                        \
    if (SEGMENT_IS_LOCAL(si, pp)) {                                     \
      if (FORWARDEDP(pp, si)) {                                         \
        *ppp = GET_FWDADDRESS(pp);                                      \
        __to_g = TARGET_GENERATION(si);                                 \
      } else if (!new_marked(si, pp)) {                                 \
        mark_or_copy_impure(__to_g, ppp, pp, from_g, si);               \
      } else {                                                          \
        __to_g = TARGET_GENERATION(si);                                 \
      }                                                                 \
      if (__to_g < from_g) S_record_new_dirty_card(tgc, ppp, __to_g);   \
    } else                                                              \
      RECORD_REMOTE(si);                                                \
  } while (0)

#define mark_or_copy_impure(to_g, dest, p, from_g, si) do {      \
    if (CAN_MARK_AND(si->use_marks))                             \
      to_g = mark_object(tgc, p, si);                            \
    else                                                         \
      to_g = copy(tgc, p, si, dest);                             \
  } while (0)

# define NO_DIRTY_NEWSPACE_UNUSED    /* empty */

#endif /* !NO_DIRTY_NEWSPACE_POINTERS */

#define relocate_dirty(PPP, YOUNGEST) do {                              \
    seginfo *_si; ptr *_ppp = PPP, _pp = *_ppp; IGEN _pg;               \
    if (!FIXMEDIATE(_pp) && (_si = MaybeSegInfo(ptr_get_segment(_pp))) != NULL) { \
      if (!_si->old_space) {                                            \
        _pg = _si->generation;                                          \
      } else {                                                          \
        if (SEGMENT_IS_LOCAL(_si, _pp)) {                               \
          if (FORWARDEDP(_pp, _si)) {                                   \
            *_ppp = GET_FWDADDRESS(_pp);                                \
            _pg = TARGET_GENERATION(_si);                               \
          } else if (new_marked(_si, _pp)) {                            \
            _pg = TARGET_GENERATION(_si);                               \
          } else if (CAN_MARK_AND(_si->use_marks)) {                    \
            _pg = mark_object(tgc, _pp, _si);                           \
          } else {                                                      \
            _pg = copy(tgc, _pp, _si, _ppp);                            \
          }                                                             \
        } else {                                                        \
          RECORD_REMOTE(_si);                                           \
          _pg = 0xff;                                                   \
        }                                                               \
      }                                                                 \
      if (_pg < YOUNGEST) YOUNGEST = _pg;                               \
    }                                                                   \
  } while (0)

#define relocate_reference(ppp, from_g) do {                    \
    ptr* rPPP = ppp; ptr rPP = *rPPP;                           \
    if (!FOREIGN_REFERENCEP(rPP)) {                             \
      *rPPP = S_reference_to_object(rPP);                       \
      relocate_impure(rPPP, from_g);                            \
      *rPPP = S_object_to_reference(*rPPP);                     \
    }                                                           \
  } while (0)

#define relocate_reference_dirty(ppp, YOUNGEST) do {            \
    ptr* rPPP = ppp;                                            \
    if (!FOREIGN_REFERENCEP(*rPPP)) {                           \
      *rPPP = S_reference_to_object(*rPPP);                     \
      relocate_dirty(rPPP, YOUNGEST);                           \
      *rPPP = S_object_to_reference(*rPPP);                     \
    }                                                           \
  } while (0)

#ifdef ENABLE_OBJECT_COUNTS
# define is_counting_root(si, p) (si->counting_mask && (si->counting_mask[segment_bitmap_byte(p)] & segment_bitmap_bit(p)))
#endif

# define relocate_indirect(p) do { \
    ptr _P = p;                    \
    relocate_pure(&_P);            \
  } while (0)

# define relocate_reference_indirect(p) do {   \
    ptr _P = p;                                \
    if (!FOREIGN_REFERENCEP(_P)) {             \
      _P = S_reference_to_object(_P);          \
      relocate_pure(&_P);                      \
    }                                          \
  } while (0)

FORCEINLINE void check_triggers(thread_gc *tgc, seginfo *si) {
  /* Registering ephemerons and guardians to recheck at the
     granularity of a segment means that the worst-case complexity of
     GC is quadratic in the number of objects that fit into a segment
     (but that only happens if the objects are ephemeron keys that are
     reachable just through a chain via the value field of the same
     ephemerons). */
  if (si->has_triggers) {
    if (si->trigger_ephemerons) {
      add_trigger_ephemerons_to_pending(tgc, si->trigger_ephemerons);
      si->trigger_ephemerons = 0;
    }
    if (si->trigger_guardians) {
      add_trigger_guardians_to_recheck(si->trigger_guardians);
      si->trigger_guardians = 0;
    }
    si->has_triggers = 0;
  }
}

#if defined(ENABLE_OBJECT_COUNTS)
# include "gc-oce.inc"
#elif defined(ENABLE_PARALLEL)
# include "gc-par.inc"
#else
# include "gc-ocd.inc"
#endif

/* sweep_in_old() is like sweep(), but the goal is to sweep the
   object's content without copying the object itself, so we're sweep
   an object while it's still in old space. If an object refers back
   to itself, naively sweeping might copy the object while we're
   trying to sweep the old copy, which interacts badly with the words
   set to a forwarding marker and pointer. To handle that problem,
   sweep_in_old() is allowed to copy the object, since the object
   is going to get copied anyway. */
static void sweep_in_old(thread_gc *tgc, ptr p) {
  /* Detect all the cases when we need to give up on in-place
     sweeping: */
  if (object_directly_refers_to_self(p)) {
    relocate_pure_now(&p);
    return;
  }

  /* We've determined that `p` won't refer immediately back to itself,
     so it's ok to sweep(), but only update `p` for pure relocations;
     impure oness must that will happen later, after `p` is
     potentially copied, so the card updates will be right. */
  sweep_object_in_old(tgc, p);
}

static void sweep_dirty_object_if_space_new(thread_gc *tgc, ptr p) {
  seginfo *si = SegInfo(ptr_get_segment(p));
  if (si->space == space_new)
    (void)sweep_dirty_object(tgc, p, 0);
}

static ptr copy_stack(thread_gc *tgc, ptr old, iptr *length, iptr clength) {
  iptr n, m; ptr new; IGEN newg;
  seginfo *si = SegInfo(ptr_get_segment(old));

  /* Don't copy non-oldspace stacks, since we may be sweeping a
     continuation that is older than target_generation.  Doing so would
     be a waste of work anyway. */
  if (!si->old_space) return old;

  newg = TARGET_GENERATION(si);

  n = *length;

#ifndef NO_NEWSPACE_MARKS
  if (si->use_marks) {
    if (!marked(si, old)) {
      mark_untyped_data_object(tgc, old, n, si);
    
#ifdef ENABLE_OBJECT_COUNTS
      S_G.countof[newg][countof_stack] += 1;
      S_G.bytesof[newg][countof_stack] += n;
#endif
    }

    return old;
  }
#endif

  /* reduce headroom created for excessively large frames (typically resulting from apply with long lists) */
  if (n != clength && n > default_stack_size && n > (m = clength + one_shot_headroom)) {
    *length = n = m;
  }

  n = ptr_align(n);
#ifdef ENABLE_OBJECT_COUNTS
  S_G.countof[newg][countof_stack] += 1;
  S_G.bytesof[newg][countof_stack] += n;
#endif /* ENABLE_OBJECT_COUNTS */

  if (n == 0) {
    return (ptr)0;
  } else {
    find_gc_room(tgc, space_data, newg, type_untyped, n, new);
    n = ptr_align(clength);
    /* warning: stack may have been left non-double-aligned by split_and_resize */
    memcpy_aligned(TO_VOIDP(new), TO_VOIDP(old), n);

    /* also returning possibly updated value in *length */
    return new;
  }
}

#define NONSTATICINHEAP(si, x) (!FIXMEDIATE(x) && (si = MaybeSegInfo(ptr_get_segment(x))) != NULL && si->generation != static_generation)
#define ALWAYSTRUE(si, x) (si = SegInfo(ptr_get_segment(x)), 1)
#define partition_guardians(LS, FILTER) do {                    \
    ptr ls; seginfo *si;                                        \
    for (ls = LS; ls != Snil; ls = next) {                      \
      obj = GUARDIANOBJ(ls);                                    \
      next = GUARDIANNEXT(ls);                                  \
      if (FILTER(si, obj)) {                                    \
        if (!si->old_space || new_marked(si, obj)) {            \
          INITGUARDIANNEXT(ls) = pend_hold_ls;                  \
          pend_hold_ls = ls;                                    \
        } else if (FORWARDEDP(obj, si)) {                       \
          INITGUARDIANOBJ(ls) = GET_FWDADDRESS(obj);            \
          INITGUARDIANNEXT(ls) = pend_hold_ls;                  \
          pend_hold_ls = ls;                                    \
        } else {                                                \
          seginfo *t_si;                                        \
          tconc = GUARDIANTCONC(ls);                            \
          t_si = SegInfo(ptr_get_segment(tconc));               \
          if (!t_si->old_space || new_marked(t_si, tconc)) {    \
            INITGUARDIANNEXT(ls) = final_ls;                    \
            final_ls = ls;                                      \
          } else if (FWDMARKER(tconc) == forward_marker) {      \
            INITGUARDIANTCONC(ls) = FWDADDRESS(tconc);          \
            INITGUARDIANNEXT(ls) = final_ls;                    \
            final_ls = ls;                                      \
          } else {                                              \
            INITGUARDIANNEXT(ls) = pend_final_ls;               \
            pend_final_ls = ls;                                 \
          }                                                     \
        }                                                       \
      }                                                         \
    }                                                           \
  } while (0)

typedef struct count_root_t {
  ptr p;
  IBOOL weak;
} count_root_t;

ptr GCENTRY(ptr tc, ptr count_roots_ls) {
    thread_gc *tgc = THREAD_GC(tc);
    IGEN g; ISPC s;
    seginfo *oldspacesegments, *oldweakspacesegments, *si, *nextsi;
    ptr ls;
    bucket_pointer_list *buckets_to_rebuild;
    uptr pre_finalization_size, pre_phantom_bytes;
#ifdef ENABLE_OBJECT_COUNTS
    ptr count_roots_counts = Snil;
    iptr count_roots_len;
    count_root_t *count_roots;
#endif

    GET_REAL_TIME(astart);

    S_thread_start_code_write(tc, MAX_TG, 0, NULL, 0);

   /* flush instruction cache: effectively clear_code_mod but safer */
    for (ls = S_threads; ls != Snil; ls = Scdr(ls)) {
      ptr t_tc = (ptr)THREADTC(Scar(ls));
      S_flush_instruction_cache(t_tc);
    }

    tlcs_to_rehash = Snil;
    conts_to_promote = Snil;
#ifndef NO_DIRTY_NEWSPACE_POINTERS
    S_G.new_dirty_cards = NULL;
#endif /* !NO_DIRTY_NEWSPACE_POINTERS */
    S_G.must_mark_gen0 = 0;

    setup_sweepers(tgc); /* maps  threads to sweepers */

    for (ls = S_threads; ls != Snil; ls = Scdr(ls)) {
      ptr t_tc = (ptr)THREADTC(Scar(ls));
      thread_gc *t_tgc = THREAD_GC(t_tc);
      S_scan_dirty(TO_VOIDP(EAP(t_tc)), TO_VOIDP(REAL_EAP(t_tc)));
      EAP(t_tc) = REAL_EAP(t_tc) = AP(t_tc) = (ptr)0;

      /* clear thread-local allocation: */
      for (g = 0; g <= MAX_CG; g++) {
        for (s = 0; s <= max_real_space; s++) {
          if (t_tgc->base_loc[g][s]) {
            /* We close off, instead of just setting BASELOC to 0,
               in case the page ends up getting marked, in which
               case a terminator mark needed. */
            S_close_off_thread_local_segment(t_tc, s, g);
          }
        }
      }

      if (!HAS_SWEEPER_WRT(t_tc, tc)) {
        /* close off any current allocation in MAX_TG, and ensure that
           end-of-segment markers are otherwise set (in case that's
           needed for dirty-byte sweeping) */
        for (s = 0; s <= max_real_space; s++) {
          if (t_tgc->base_loc[MAX_TG][s])
            S_close_off_thread_local_segment(t_tc, s, MAX_TG);
          for (g = MAX_TG + 1; g <= static_generation; g++) {
            ptr old = t_tgc->next_loc[g][s];
            if (old != (ptr)0)
              *(ptr*)TO_VOIDP(old) = forward_marker;
          }
        }
      } else {
        /* set up context for sweeping --- effectively remembering the current
           allocation state so anything new is recognized as needing sweeping */
        t_tgc->sweep_stack_start = t_tgc->sweep_stack = t_tgc->sweep_stack_limit = (ptr)0;
        t_tgc->send_remote_sweep_stack_start = t_tgc->send_remote_sweep_stack = t_tgc->send_remote_sweep_stack_limit = (ptr)0;
        t_tgc->receive_remote_sweep_stack_start = t_tgc->receive_remote_sweep_stack = t_tgc->receive_remote_sweep_stack_limit = (ptr)0;
        t_tgc->bitmask_overhead[0] = 0;
        for (g = MIN_TG; g <= MAX_TG; g++)
          t_tgc->bitmask_overhead[g] = 0;
        for (s = 0; s <= max_real_space; s++) {
          /* need to save `next_loc` to ensure that dirty sweeping
             doesn't overshoot into newly allocated objects */
          t_tgc->orig_next_loc[s] = t_tgc->next_loc[MAX_TG][s];
          t_tgc->sweep_loc[MAX_TG][s] = t_tgc->next_loc[MAX_TG][s];
          for (g = MIN_TG; g <= MAX_TG; g++)
            t_tgc->sweep_next[g][s] = NULL;
        }
      }
    }

   /* perform after ScanDirty */
    if (S_checkheap) S_check_heap(0, MAX_CG);

#ifdef DEBUG
(void)printf("max_cg = %x;  go? ", MAX_CG); (void)fflush(stdout); (void)getc(stdin);
#endif

    resweep_weak_segments = NULL;
    for (g = MIN_TG; g <= MAX_TG; g++) fully_marked_mask[g] = NULL;

  /* set up generations to be copied */
    for (g = 0; g <= MAX_CG; g++) {
      S_G.bytes_of_generation[g] = 0;
      for (s = 0; s <= max_real_space; s++) {
        S_G.bytes_of_space[g][s] = 0;
        S_G.bitmask_overhead[g] = 0;
      }
    }

  /* reset phantom size in generations to be copied, even if counting is not otherwise enabled */
    pre_phantom_bytes = 0;
    for (g = 0; g <= MAX_CG; g++) {
      pre_phantom_bytes += S_G.bytesof[g][countof_phantom];
      S_G.bytesof[g][countof_phantom] = 0;
    }
    for (g = MIN_TG; g <= MAX_TG; g++) {
      pre_phantom_bytes += S_G.bytesof[g][countof_phantom];
    }

  /* mark segments from which objects are to be copied or marked */
    oldspacesegments = oldweakspacesegments = (seginfo *)NULL;
    for (g = 0; g <= MAX_CG; g += 1) {
      IBOOL maybe_mark = ((g >= S_G.min_mark_gen) && (g >= MIN_TG));
      for (s = 0; s <= max_real_space; s += 1) {
        seginfo *saved;

        if (s == space_weakpair) {
          saved = oldspacesegments;
          oldspacesegments = oldweakspacesegments;
        } else
          saved = NULL;

        for (si = S_G.occupied_segments[g][s]; si != NULL; si = nextsi) {
          nextsi = si->next;
          si->next = oldspacesegments;
          oldspacesegments = si;
          si->old_space = 1;
          /* update generation now, both to compute the target generation,
             and so that any updated dirty references will record the correct
             new generation; also used for a check in S_dirty_set */
          si->generation = compute_target_generation(si->generation);
          if (si->must_mark
              || (maybe_mark
                  && (!si->marked_mask
                      || (si->marked_count >= segment_sufficiently_compact_bytes))
                  && (si->chunk->nused_segs >= chunk_sufficiently_compact(si->chunk->segs)))) {
            if (s != space_new) /* only lock-based marking is allowed on space_new */
              si->use_marks = 1;
          }
          si->marked_mask = NULL; /* clear old mark bits, if any */
          si->marked_count = 0;
          si->min_dirty_byte = 0; /* prevent registering as dirty while GCing */
#ifdef ENABLE_PARALLEL
          if (!si->creator->tc) si->creator = tgc;
#endif
        }
        S_G.occupied_segments[g][s] = NULL;

        if (s == space_weakpair) {
          oldweakspacesegments = oldspacesegments;
          oldspacesegments = saved;
        }
      }
    }
    if (oldweakspacesegments) {
      /* make oldweakspacesegments a prefix of weakspacesegments */
      seginfo *p;
      for (p = oldweakspacesegments; p->next; p = p->next);
      p->next = oldspacesegments;
      oldspacesegments = oldweakspacesegments;
    }

#ifdef ENABLE_OBJECT_COUNTS
   /* clear object counts & bytes for copied generations; bump timestamp */
   {INT i;
    for (g = 0; g <= MAX_CG; g += 1) {
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
   for (g = 0; g <= MAX_CG; g += 1) {
      S_G.gcbackreference[g] = Snil;
   }

   SET_BACKREFERENCE(Sfalse); /* #f => root */

    /* Set mark bit for any locked object in `space_new`. Don't sweep until
       after handling counting roots. Note that the segment won't have
       `use_marks` set, so non-locked objects will be copied out. */
     for (g = 0; g <= MAX_CG; g += 1) {
       IGEN tg = compute_target_generation(g);
       for (ls = S_G.locked_objects[g]; ls != Snil; ls = Scdr(ls)) {
         ptr p = Scar(ls);
         seginfo *si = SegInfo(ptr_get_segment(p));
         if (si->space == space_new) {
           if (!si->marked_mask)
             init_mask(tgc, si->marked_mask, tg, 0);
           si->marked_mask[segment_bitmap_byte(p)] |= segment_bitmap_bit(p);
         }
       }
     }
   
#ifdef ENABLE_OBJECT_COUNTS
  /* set flag on count_roots objects so they get copied to space_count_root */
     if (count_roots_ls != Sfalse) {
       iptr i;

       count_roots_len = list_length(count_roots_ls);
       find_gc_room_voidp(tgc, space_data, 0, ptr_align(count_roots_len*sizeof(count_root_t)), count_roots);

       for (ls = count_roots_ls, i = 0; ls != Snil; ls = Scdr(ls), i++) {
         ptr p = Scar(ls);
         if (FIXMEDIATE(p)) {
           count_roots[i].p = p;
           count_roots[i].weak = 0;
         } else {
           seginfo *ls_si = SegInfo(ptr_get_segment(ls));
           seginfo *si = SegInfo(ptr_get_segment(p));

           if (!si->counting_mask)
             init_counting_mask(tgc, si);

           si->counting_mask[segment_bitmap_byte(p)] |= segment_bitmap_bit(p);

           count_roots[i].p = p;
           count_roots[i].weak = ((ls_si->space == space_weakpair)
                                  || (ls_si->space == space_ephemeron));
         }
       }
     } else {
       count_roots_len = 0;
       count_roots = NULL;
     }
#endif

#ifdef ENABLE_OBJECT_COUNTS
  /* sweep count_roots in order and accumulate counts */
     if (count_roots_len > 0) {
       ptr prev = 0; uptr prev_total = total_size_so_far();
       iptr i;

# ifdef ENABLE_MEASURE
       init_measure(tgc, MAX_TG+1, static_generation);
# endif

       for (i = 0; i < count_roots_len; i++) {
         uptr total;
         ptr p = count_roots[i].p;
         if (FIXMEDIATE(p)) {
           /* nothing to do */
         } else {
           seginfo *si = SegInfo(ptr_get_segment(p));

           if (si->counting_mask[segment_bitmap_byte(p)] & segment_bitmap_bit(p)) {
             si->counting_mask[segment_bitmap_byte(p)] -= segment_bitmap_bit(p);

             if (!si->old_space || FORWARDEDP(p, si) || marked(si, p)
                 || !count_roots[i].weak) {
               /* reached or older; sweep transitively */
#ifdef ENABLE_PARALLEL
               if (si->creator->tc == 0) si->creator = tgc;
#endif
               {
                 BLOCK_SET_THREAD(si->creator);
                 relocate_pure_now(&p);
                 push_sweep(p);
               }
               ADD_BACKREFERENCE(p, si->generation);

               parallel_sweep_generation(tgc);

               /* now count this object's size, if we have deferred it before */
               si = SegInfo(ptr_get_segment(p));
               if ((si->space == space_count_pure) || (si->space == space_count_impure))
                 count_root_bytes -= size_object(p);
             }
           } else {
             /* must have been already counted by being earlier in the list */
           }
         }

         total = total_size_so_far();
         p = S_cons_in(tc, space_new, 0, FIX(total-prev_total), Snil);
         if (prev != 0)
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
         if (!FIXMEDIATE(p)) {
           seginfo *si = SegInfo(ptr_get_segment(p));
           si->counting_mask = NULL;
         }
       }
     }
#endif

    /* Gather and mark all younger locked objects.
       Any object on a `space_new` segment is already marked, but still
       needs to be swept. */
    {
       for (g = MAX_CG; g >= 0; g -= 1) {
         ptr locked_objects;
         IGEN tg = compute_target_generation(g);
         ls = S_G.locked_objects[g];
         S_G.locked_objects[g] = Snil;
         S_G.unlocked_objects[g] = Snil;
         locked_objects = S_G.locked_objects[tg];
         for (; ls != Snil; ls = Scdr(ls)) {
           ptr p = Scar(ls);
           seginfo *si = SegInfo(ptr_get_segment(p));
           if (si->space == space_new) {
             /* Retract the mark bit and mark properly, so anything that needs
                to happen with marking will happen. */
             if (!marked(si, p))
               S_error_abort("space_new locked object should have a mark bit set");
             si->marked_mask[segment_bitmap_byte(p)] -= segment_bitmap_bit(p);
             mark_object(tgc, p, si);
           }
           /* non-`space_new` objects will be swept via new pair */
           locked_objects = S_cons_in(tc, space_impure, tg, p, locked_objects);
#ifdef ENABLE_OBJECT_COUNTS
           S_G.countof[tg][countof_pair] += 1;
           S_G.countof[tg][countof_locked] += 1;
           S_G.bytesof[tg][countof_locked] += size_object(p);
#endif /* ENABLE_OBJECT_COUNTS */
         }
         S_G.locked_objects[tg] = locked_objects;
       }
    }

  /* for each thread with a sweeper, sweep in that thread's context to
     make sure the sweeper will perform that thread's work; otherwise,
     sweep non-oldspace threads, since any thread may have an active
     stack */
    for (ls = S_threads; ls != Snil; ls = Scdr(ls)) {
      ptr thread;

    /* someone may have their paws on the list */
      if (FWDMARKER(ls) == forward_marker) ls = FWDADDRESS(ls);

      thread = Scar(ls);

#ifdef ENABLE_PARALLEL
      {
        ptr t_tc = (ptr)THREADTC(thread);
        BLOCK_SET_THREAD(THREAD_GC(t_tc)); /* switches mark/sweep to thread */
        if (!OLDSPACE(thread)) {
          /* remember to sweep in sweeper thread */
          push_sweep(thread);
        } else {
          /* relocate now, then sweeping will happen in sweeper thread */
          relocate_pure_now(&thread);
        }
      }
#else
      if (!OLDSPACE(thread))
        sweep_thread(tgc, thread);
#endif
    }

    relocate_pure_now(&S_threads);

    GET_REAL_TIME(start);

  /* relocate nonempty oldspace symbols and set up list of buckets to rebuild later */
    buckets_to_rebuild = NULL;
    for (g = 0; g <= MAX_CG; g += 1) {
      bucket_list *bl, *blnext; bucket *b; bucket_pointer_list *bpl; bucket **oblist_cell; ptr sym; iptr idx;
      for (bl = S_G.buckets_of_generation[g]; bl != NULL; bl = blnext) {
        blnext = bl->cdr;
        b = bl->car;
        /* mark this bucket old for the rebuilding loop */
        b->next = TO_VOIDP((uptr)TO_PTR(b->next) | 1);
        sym = b->sym;
        idx = UNFIX(SYMHASH(sym)) % S_G.oblist_length;
        oblist_cell = &S_G.oblist[idx];
        if (!((uptr)TO_PTR(*oblist_cell) & 1)) {
          /* mark this bucket in the set */
          *oblist_cell = TO_VOIDP((uptr)TO_PTR(*oblist_cell) | 1);
          /* repurpose the bucket list element for the list of buckets to rebuild later */
          /* idiot_checks verifies these have the same size */
          bpl = (bucket_pointer_list *)bl;
          bpl->car = oblist_cell;
          bpl->cdr = buckets_to_rebuild;
          buckets_to_rebuild = bpl;
        }
        if (FWDMARKER(sym) != forward_marker &&
            /* coordinate with alloc.c */
            (SYMVAL(sym) != sunbound || SYMPLIST(sym) != Snil || SYMSPLIST(sym) != Snil)) {
          seginfo *sym_si = SegInfo(ptr_get_segment(sym));
          BLOCK_SET_THREAD(sym_si->creator); /* use symbol's creator thread context */
          if (!new_marked(sym_si, sym))
            mark_or_copy_pure(&sym, sym, sym_si);
        }
      }
      S_G.buckets_of_generation[g] = NULL;
    }

  /* relocate the protected C pointers */
    {uptr i;
     for (i = 0; i < S_G.protect_next; i++)
       relocate_pure_now(S_G.protected[i]);
    }

  /* sweep older locked and unlocked objects that are on `space_new` segments,
     because we can't find dirty writes there */
    for (g = MAX_CG + 1; g <= static_generation; INCRGEN(g)) {
      for (ls = S_G.locked_objects[g]; ls != Snil; ls = Scdr(ls))
        sweep_dirty_object_if_space_new(tgc, Scar(ls));
      for (ls = S_G.unlocked_objects[g]; ls != Snil; ls = Scdr(ls))
        sweep_dirty_object_if_space_new(tgc, Scar(ls));
    }

  /* prepare to sweep areas marked dirty by assignments into older generations */
    setup_sweep_dirty(tgc);

    parallel_sweep_dirty_and_generation(tgc);

    teardown_sweepers();

    pre_finalization_size = target_generation_space_so_far(tgc);

  /* handle guardians */
    {   ptr pend_hold_ls, final_ls, pend_final_ls, maybe_final_ordered_ls;
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

        for (g = 0; g <= MAX_CG; g += 1) {
          partition_guardians(S_G.guardians[g], ALWAYSTRUE);
          S_G.guardians[g] = Snil;
        }

       /* invariants after partition_guardians:
        * for entry in pend_hold_ls, obj is !OLDSPACE
        * for entry in final_ls, obj is OLDSPACE
        * for entry in final_ls, tconc is !OLDSPACE
        * for entry in pend_final_ls, obj and tconc are OLDSPACE
        */

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
                  INT b; iptr *addr;
                  rep = GUARDIANOBJ(ls);
                  if (FWDMARKER(rep) == forward_marker) rep = FWDADDRESS(rep);
                /* Caution: Building in assumption about shape of an ftype pointer */
                  addr = TO_VOIDP(RECORDINSTIT(rep, 0));
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
                  seginfo *o_si = SegInfo(ptr_get_segment(obj));
                  if (FORWARDEDP(obj, o_si) || new_marked(o_si, obj)) {
                    /* Object is reachable, so we might as well move
                       this one to the hold list --- via pend_hold_ls, which
                       leads to a copy to move to hold_ls */
                    INITGUARDIANNEXT(ls) = pend_hold_ls;
                    pend_hold_ls = ls;
                  } else {
                    seginfo *si;
                    if (!FIXMEDIATE(rep) && (si = MaybeSegInfo(ptr_get_segment(rep))) != NULL && si->old_space) {
                      /* mark things reachable from `rep`, but not `rep` itself, unless
                         `rep` is immediately reachable from itself */
                      PUSH_BACKREFERENCE(ls)
                      sweep_in_old(tgc, rep);
                      POP_BACKREFERENCE()
                    }
                    INITGUARDIANNEXT(ls) = maybe_final_ordered_ls;
                    maybe_final_ordered_ls = ls;
                  }
                } else {
                /* if tconc was old it's been forwarded */
                  tconc = GUARDIANTCONC(ls);

                  WITH_TOP_BACKREFERENCE(tconc, relocate_pure_now(&rep));

                  old_end = Scdr(tconc);
                  new_end = S_cons_in(tc, space_impure, 0, FIX(0), FIX(0));
#ifdef ENABLE_OBJECT_COUNTS
                  S_G.countof[0][countof_pair] += 1;
#endif /* ENABLE_OBJECT_COUNTS */

                  /* These assignments may trigger card marking or additions to `new_dirty_cards`: */
                  SETCAR(old_end,rep);
                  SETCDR(old_end,new_end);
                  SETCDR(tconc,new_end);
                }
            }

          /* copy each entry in pend_hold_ls into hold_ls if tconc accessible */
            ls = pend_hold_ls; pend_hold_ls = Snil;
            for ( ; ls != Snil; ls = next) {
              ptr p;
              seginfo *t_si;
#ifdef CONSTANT_TARGET_GENERATION
              g = MAX_TG;
#else
              seginfo *g_si;
              g_si = SegInfo(ptr_get_segment(ls));
              g = TARGET_GENERATION(g_si);
#endif
              
              next = GUARDIANNEXT(ls); 

              /* discard static pend_hold_ls entries */
              if (g == static_generation) continue;
              
              tconc = GUARDIANTCONC(ls);

              t_si = SegInfo(ptr_get_segment(tconc));
              
              if (t_si->old_space && !new_marked(t_si, tconc)) {
                if (FWDMARKER(tconc) == forward_marker)
                  tconc = FWDADDRESS(tconc);
                else {
                  INITGUARDIANPENDING(ls) = FIX(GUARDIAN_PENDING_HOLD);
                  add_pending_guardian(ls, tconc);
                  continue;
                }
              }
              
              rep = GUARDIANREP(ls);
              WITH_TOP_BACKREFERENCE(tconc, relocate_pure_now(&rep));
              relocate_rep = 1;

#ifdef ENABLE_OBJECT_COUNTS
                S_G.countof[g][countof_guardian] += 1;
#endif /* ENABLE_OBJECT_COUNTS */

                /* In backreference mode, we rely on sweep of the guardian
                   entry not registering any backreferences. Otherwise,
                   bogus pair pointers would get created. */
                find_gc_room(tgc, space_pure, g, type_untyped, size_guardian_entry, p);
                INITGUARDIANOBJ(p) = GUARDIANOBJ(ls);
                INITGUARDIANREP(p) = rep;
                INITGUARDIANTCONC(p) = tconc;
                INITGUARDIANNEXT(p) = S_G.guardians[g];
                INITGUARDIANORDERED(p) = GUARDIANORDERED(ls);
                INITGUARDIANPENDING(p) = FIX(0);
                S_G.guardians[g] = p;
            }

            if (!relocate_rep && !do_ordered && maybe_final_ordered_ls != Snil) {
              /* Switch to finishing up ordered. Move all maybe-final
                 ordered entries to final_ls and pend_hold_ls */
              do_ordered = relocate_rep = 1;
              ls = maybe_final_ordered_ls; maybe_final_ordered_ls = Snil;
              for (; ls != Snil; ls = next) {
                ptr obj = GUARDIANOBJ(ls);
                seginfo *o_si = SegInfo(ptr_get_segment(obj));
                next = GUARDIANNEXT(ls);
                if (FORWARDEDP(obj, o_si) || new_marked(o_si, obj)) {
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

            sweep_generation(tgc);

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
           *   final_ls         if tconc forwarded or marked
           *   pend_final_ls    if tconc not forwarded or marked
           * where the output pend_final_ls coresponds to pending in a segment */
            ls = pend_final_ls; pend_final_ls = Snil;
            for ( ; ls != Snil; ls = next) {
                tconc = GUARDIANTCONC(ls); next = GUARDIANNEXT(ls);

                if (FWDMARKER(tconc) == forward_marker) {
                    INITGUARDIANTCONC(ls) = FWDADDRESS(tconc);
                    INITGUARDIANNEXT(ls) = final_ls;
                    final_ls = ls;
                } else {
#ifndef NO_NEWSPACE_MARKS
                  seginfo *t_si = SegInfo(ptr_get_segment(tconc));
#endif
                  if (new_marked(t_si, tconc)) {
                    INITGUARDIANNEXT(ls) = final_ls;
                    final_ls = ls;
                  } else {
                    INITGUARDIANPENDING(ls) = FIX(GUARDIAN_PENDING_FINAL);
                    add_pending_guardian(ls, tconc);
                  }
                }
            }
        }
    }

    S_G.bytes_finalized = target_generation_space_so_far(tgc) - pre_finalization_size;
    {
      iptr post_phantom_bytes = 0;
      for (g = MIN_TG; g <= MAX_TG; g++) {
        post_phantom_bytes += S_G.bytesof[g][countof_phantom];
      }
      S_adjustmembytes(post_phantom_bytes - pre_phantom_bytes);
    }

  /* handle weak pairs */
    resweep_dirty_weak_pairs(tgc);
    resweep_weak_pairs(tgc, oldweakspacesegments);

   /* still-pending ephemerons all go to bwp */
    finish_pending_ephemerons(tgc, oldspacesegments);

    ACCUM_REAL_TIME(collect_accum, step, start);
    REPORT_TIME(fprintf(stderr, "%d coll  +%ld ms  %ld ms  [real time]\n",
                        MAX_CG, step, collect_accum));

   /* post-gc oblist handling.  rebuild old buckets in the target generation, pruning unforwarded symbols */
    { bucket_list *bl; bucket *b, *bnext; bucket_pointer_list *bpl; bucket **pb; ptr sym;
      for (bpl = buckets_to_rebuild; bpl != NULL; bpl = bpl->cdr) {
        pb = bpl->car;
        for (b = TO_VOIDP((uptr)TO_PTR(*pb) - 1); b != NULL && ((uptr)TO_PTR(b->next) & 1); b = bnext) {
          bnext = TO_VOIDP((uptr)TO_PTR(b->next) - 1);
          sym = b->sym;
          si = SegInfo(ptr_get_segment(sym));
          if (new_marked(si, sym) || (FWDMARKER(sym) == forward_marker && ((sym = FWDADDRESS(sym)) || 1))) {
            IGEN g = si->generation;
            find_gc_room_voidp(tgc, space_data, g, ptr_align(sizeof(bucket)), b);
#ifdef ENABLE_OBJECT_COUNTS
            S_G.countof[g][countof_oblist] += 1;
            S_G.bytesof[g][countof_oblist] += sizeof(bucket);
#endif /* ENABLE_OBJECT_COUNTS */
            b->sym = sym;
            *pb = b;
            pb = &b->next;
            if (g != static_generation) {
              find_gc_room_voidp(tgc, space_data, g, ptr_align(sizeof(bucket_list)), bl);
#ifdef ENABLE_OBJECT_COUNTS
              S_G.countof[g][countof_oblist] += 1;
              S_G.bytesof[g][countof_oblist] += sizeof(bucket_list);
#endif /* ENABLE_OBJECT_COUNTS */
              bl->car = b;
              bl->cdr = S_G.buckets_of_generation[g];
              S_G.buckets_of_generation[g] = bl;
            }
          } else {
            S_G.oblist_count -= 1;
          }
        }
        *pb = b;
      }
    }

  /* rebuild rtds_with_counts lists, dropping otherwise inaccessible rtds */
    { IGEN g, newg; ptr ls, p; seginfo *si;
      for (g = MAX_CG; g >= 0; g -= 1) {
        for (ls = S_G.rtds_with_counts[g], S_G.rtds_with_counts[g] = Snil; ls != Snil; ls = Scdr(ls)) {
          p = Scar(ls);
          si = SegInfo(ptr_get_segment(p));
          if (!si->old_space || new_marked(si, p)) {
            newg = TARGET_GENERATION(si);
            S_G.rtds_with_counts[newg] = S_cons_in(tc, space_impure, newg, p, S_G.rtds_with_counts[newg]);
#ifdef ENABLE_OBJECT_COUNTS
            S_G.countof[newg][countof_pair] += 1;
#endif
          } else if (FWDMARKER(p) == forward_marker) {
            p = FWDADDRESS(p);
            newg = GENERATION(p);
            S_G.rtds_with_counts[newg] = S_cons_in(tc, space_impure, newg, p, S_G.rtds_with_counts[newg]);
#ifdef ENABLE_OBJECT_COUNTS
            S_G.countof[newg][countof_pair] += 1;
#endif
          }
        }
      }
    }

#ifndef WIN32
  /* rebuild child_process list, reaping any that have died and refusing
     to promote into the static generation. */
    { IGEN g, newg; ptr ls, newls;
      for (g = MAX_CG; g >= 0; g -= 1) {
        newg = compute_target_generation(g);
        if (newg == static_generation) newg = S_G.max_nonstatic_generation;
        newls = newg == g ? Snil : S_child_processes[newg];
        for (ls = S_child_processes[g], S_child_processes[g] = Snil; ls != Snil; ls = Scdr(ls)) {
          INT pid = UNFIX(Scar(ls)), status, retpid;
          retpid = waitpid(pid, &status, WNOHANG);
          if (retpid == 0 || (retpid == pid && !(WIFEXITED(status) || WIFSIGNALED(status)))) {
            newls = S_cons_in(tc, space_impure, newg, FIX(pid), newls);
#ifdef ENABLE_OBJECT_COUNTS
            S_G.countof[newg][countof_pair] += 1;
#endif /* ENABLE_OBJECT_COUNTS */
          }
        }
        S_child_processes[newg] = newls;
      }
    }
#endif /* WIN32 */

    copy_and_clear_list_bits(tgc, oldspacesegments);

  /* move copied old space segments to empty space, and promote
     marked old space segments to the target generation */
    for (si = oldspacesegments; si != NULL; si = nextsi) {
      nextsi = si->next;
      si->old_space = 0;
      si->use_marks = 0;
      if (si->marked_mask != NULL) {
        IGEN tg;
        si->min_dirty_byte = 0xff;
        if (si->space != space_data) {
          int d;
          for (d = 0; d < cards_per_segment; d += sizeof(ptr)) {
            iptr *dp = (iptr *)(si->dirty_bytes + d);
            /* fill sizeof(iptr) bytes at a time with 0xff */
            *dp = -1;
          }
        }
        tg = si->generation;
        if (tg == static_generation) S_G.number_of_nonstatic_segments -= 1;
        s = si->space;
        si->next = S_G.occupied_segments[tg][s];
        S_G.occupied_segments[tg][s] = si;
        S_G.bytes_of_space[tg][s] += si->marked_count;
        si->trigger_guardians = 0;
#ifdef PRESERVE_FLONUM_EQ
        si->forwarded_flonums = 0;
#endif
      } else {
        IBOOL for_code = (si->space == space_code);
        chunkinfo *chunk = si->chunk, **chunks = (for_code ? S_code_chunks : S_chunks);
        S_G.number_of_nonstatic_segments -= 1;
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
            S_free_chunk(chunk, for_code);
          } else {
            S_move_to_chunk_list(chunk, &chunks[PARTIAL_CHUNK_POOLS]);
          }
        } else {
          S_move_to_chunk_list(chunk, &chunks[PARTIAL_CHUNK_POOLS-1]);
        }
      }
    }

    S_G.g0_bytes_after_last_gc = S_G.bytes_of_generation[0];

    if (MAX_CG >= S_G.min_free_gen) S_free_chunks();

    S_flush_instruction_cache(tc);
    S_thread_end_code_write(tc, MAX_TG, 0, NULL, 0);

#ifndef NO_DIRTY_NEWSPACE_POINTERS
    /* mark dirty those newspace cards to which we've added wrong-way pointers */
    { dirtycardinfo *ndc;
      for (ndc = S_G.new_dirty_cards; ndc != NULL; ndc = ndc->next)
        S_mark_card_dirty(ndc->card, ndc->youngest);
    }
#endif /* !NO_DIRTY_NEWSPACE_POINTERS */

    if (S_checkheap) S_check_heap(1, MAX_CG);

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
      } else if ((new_idx = eq_hash(key) & (veclen - 1)) != old_idx) {
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
       longer cache one and we can no longer reliably fuse the stack
       back. */
    while (conts_to_promote != Snil) {
      S_promote_to_multishot(CONTLINK(Scar(conts_to_promote)));
      conts_to_promote = Scdr(conts_to_promote);
    }

    S_resize_oblist();

    /* tell profile_release_counters to look for bwp'd counters at least through max_tg */
    if (S_G.prcgeneration < MAX_TG) S_G.prcgeneration = MAX_TG;

    if (tgc->sweep_stack_start != tgc->sweep_stack)
      S_error_abort("gc: sweep stack ended non-empty");

    S_G.bitmask_overhead[0] += tgc->bitmask_overhead[0];
    tgc->bitmask_overhead[0] = 0;
    for (g = MIN_TG; g <= MAX_TG; g++)
      S_G.bitmask_overhead[g] += tgc->bitmask_overhead[g];

    tgc->queued_fire = 0;

    ACCUM_REAL_TIME(all_accum, astep, astart);
    REPORT_TIME(fprintf(stderr, "%d all   +%ld ms  %ld ms  [real time]\n", MAX_CG, astep, all_accum));

    if (count_roots_ls != Sfalse) {
#ifdef ENABLE_OBJECT_COUNTS
      return count_roots_counts;
#else
      return Snil;
#endif
    } else
      return Svoid;
}

#ifdef ENABLE_PARALLEL

static void push_remote_sweep(thread_gc *tgc, ptr p, thread_gc *remote_tgc) {
  if (tgc->send_remote_sweep_stack == tgc->send_remote_sweep_stack_limit)
    enlarge_stack(tgc,
                  &tgc->send_remote_sweep_stack,
                  &tgc->send_remote_sweep_stack_start,
                  &tgc->send_remote_sweep_stack_limit,
                  2 * ptr_bytes);
  ((ptr *)TO_VOIDP(tgc->send_remote_sweep_stack))[0] = p;
  ((ptr *)TO_VOIDP(tgc->send_remote_sweep_stack))[1] = TO_PTR(remote_tgc);
  tgc->send_remote_sweep_stack = (ptr)((uptr)tgc->send_remote_sweep_stack + 2 * ptr_bytes);
  tgc->sweep_change = SWEEP_CHANGE_PROGRESS;
}

#endif

#define sweep_space(s, from_g, body) do {                               \
    sweep_space_segments(s, from_g, body);                              \
    sweep_space_bump_range(s, from_g, body);                            \
  } while (0)

#define sweep_space_segments(s, from_g, body) do {                      \
    while ((si = (seginfo *)TO_VOIDP(tgc->sweep_next[from_g][s])) != NULL) { \
      tgc->sweep_next[from_g][s] = si->sweep_next;                      \
      pp = TO_VOIDP(si->sweep_start);                                   \
      while ((p = *pp) != forward_marker) {                             \
        do body while (0);                                              \
      }                                                                 \
      COUNT_SWEPT_BYTES(si->sweep_start, pp);                           \
      save_resweep(s, si);                                              \
    }                                                                   \
  } while (0)

#define sweep_space_bump_range(s, from_g, body) do {                    \
    slp = &tgc->sweep_loc[from_g][s];                                   \
    nlp = &tgc->next_loc[from_g][s];                                    \
    while ((sl = TO_VOIDP(*slp)) != (nl = TO_VOIDP(*nlp))) {            \
      *slp = TO_PTR(nl);                                                \
      pp = sl;                                                          \
      while (pp != nl) {                                                \
        p = *pp;                                                        \
        do body while (0);                                              \
      }                                                                 \
      COUNT_SWEPT_BYTES(sl, nl);                                        \
    }                                                                   \
  } while (0)

#define save_resweep(s, si) do {                  \
    if (s == space_weakpair) {                    \
      GC_MUTEX_ACQUIRE();                         \
      si->sweep_next = resweep_weak_segments;     \
      resweep_weak_segments = si;                 \
      GC_MUTEX_RELEASE();                         \
    }                                             \
  } while (0)

static void resweep_weak_pairs(thread_gc *tgc, seginfo *oldweakspacesegments) {
    IGEN from_g;
    ptr *pp, p, *nl, ls;
    seginfo *si;

    for (ls = S_threads; ls != Snil; ls = Scdr(ls)) {
      thread_gc *s_tgc = THREAD_GC(THREADTC(Scar(ls)));
      for (from_g = MIN_TG; from_g <= MAX_TG; from_g += 1) {
        /* By starting from `base_loc`, we may needlessly sweep pairs in `MAX_TG`
           that were allocated before the GC, but that's ok. Could consult
           `orig_next_loc` to detect that case. */
        pp = TO_VOIDP(s_tgc->base_loc[from_g][space_weakpair]);
        nl = TO_VOIDP(s_tgc->next_loc[from_g][space_weakpair]);
        while (pp != nl) {
          p = *pp;
          forward_or_bwp(tgc, from_g, pp, p);
          pp += 2;
        }
      }
    }

   for (si = resweep_weak_segments; si != NULL; si = si->sweep_next) {
     pp = TO_VOIDP(build_ptr(si->number, 0));
     while ((p = *pp) != forward_marker) {
       forward_or_bwp(tgc, si->generation, pp, p);
       pp += 2;
     }
   }

   for (si = oldweakspacesegments; si != NULL; si = si->next) {
     if (si->space != space_weakpair)
       break;
     if (si->marked_mask) {
       uptr i;
       for (i = 0; i < segment_bitmap_bytes; i++) {
         int mask = si->marked_mask[i];
         if (mask != 0) {
           /* Assuming 4 pairs per 8 words */
           pp = TO_VOIDP(build_ptr(si->number, (i << (log2_ptr_bytes+3))));
           if (mask & 0x1)
             forward_or_bwp(tgc, si->generation, pp, *pp);
           pp += 2;
           if (mask & 0x4)
             forward_or_bwp(tgc, si->generation, pp, *pp);
           pp += 2;
           if (mask & 0x10)
             forward_or_bwp(tgc, si->generation, pp, *pp);
           pp += 2;
           if (mask & 0x40)
             forward_or_bwp(tgc, si->generation, pp, *pp);
         }
       }
     }
   }
}

static void forward_or_bwp(NO_DIRTY_NEWSPACE_UNUSED thread_gc *tgc, NO_DIRTY_NEWSPACE_UNUSED IGEN from_g,
                           ptr *pp, ptr p) {
  seginfo *si;
#ifndef NO_DIRTY_NEWSPACE_POINTERS
  /* adapted from relocate_impure_help_help */
  if (!FIXMEDIATE(p) && (si = MaybeSegInfo(ptr_get_segment(p))) != NULL) {
    IGEN __to_g;
    if (!si->old_space || new_marked(si, p)) {
      __to_g = TARGET_GENERATION(si);
    } else if (FORWARDEDP(p, si)) {
      *pp = GET_FWDADDRESS(p);
      __to_g = TARGET_GENERATION(si);
    } else {
      *pp = Sbwp_object;
      __to_g = static_generation;
    }
    if (__to_g < from_g) S_record_new_dirty_card(tgc, pp, __to_g);
  }
#else
  if (!FIXMEDIATE(p) && (si = MaybeSegInfo(ptr_get_segment(p))) != NULL && si->old_space && !new_marked(si, p)) {
    if (FORWARDEDP(p, si)) {
      *pp = GET_FWDADDRESS(p);
    } else {
      *pp = Sbwp_object;
    }
  }
#endif
}

static iptr sweep_generation_pass(thread_gc *tgc) {
  ptr *slp, *nlp; ptr *pp, *ppn, p, *nl, *sl; IGEN from_g;
  seginfo *si;
  iptr num_swept_bytes = 0;

  do {
    tgc->sweep_change = SWEEP_NO_CHANGE;

    num_swept_bytes += sweep_from_stack(tgc);

    for (from_g = MIN_TG; from_g <= MAX_TG; from_g += 1) {

      sweep_space(space_impure, from_g, {
          /* only pairs in these spaces in backreference mode */
          FLUSH_REMOTE_BLOCK
          SET_BACKREFERENCE(TYPE(TO_PTR(pp), type_pair));
          relocate_impure_help(pp, p, from_g);
          ppn = pp + 1;
          p = *ppn;
          relocate_impure_help(ppn, p, from_g);
          FLUSH_REMOTE(tgc, TYPE(TO_PTR(pp), type_pair)); /* can always treat as a pair to sweep words */
          pp = ppn + 1;
        });
      SET_BACKREFERENCE(Sfalse);

      sweep_space(space_symbol, from_g, {
          p = TYPE(TO_PTR(pp), type_symbol);
          sweep_symbol(tgc, p, from_g);
          pp += size_symbol / sizeof(ptr);
        });
      
      sweep_space(space_port, from_g, {
          p = TYPE(TO_PTR(pp), type_typed_object);
          sweep_port(tgc, p, from_g);
          pp += size_port / sizeof(ptr);
        });

      sweep_space(space_weakpair, from_g, {
          FLUSH_REMOTE_BLOCK
          SET_BACKREFERENCE(TYPE(TO_PTR(pp), type_pair));
          ppn = pp + 1;
          p = *ppn;
          relocate_impure_help(ppn, p, from_g);
          FLUSH_REMOTE(tgc, TYPE(TO_PTR(pp), type_pair));
          pp = ppn + 1;
        });
      SET_BACKREFERENCE(Sfalse);

      sweep_space(space_ephemeron, from_g, {
          p = TYPE(TO_PTR(pp), type_pair);
          add_ephemeron_to_pending(tgc, p);
          pp += size_ephemeron / sizeof(ptr);
        });

      sweep_space(space_pure, from_g, {
          FLUSH_REMOTE_BLOCK
          SET_BACKREFERENCE(TYPE(TO_PTR(pp), type_pair)); /* only pairs put here in backreference mode */
          relocate_impure_help(pp, p, from_g);
          ppn = pp + 1;
          p = *ppn;
          relocate_impure_help(ppn, p, from_g);
          FLUSH_REMOTE(tgc, TYPE(TO_PTR(pp), type_pair));
          pp = ppn + 1;
        });
      SET_BACKREFERENCE(Sfalse);

      sweep_space(space_continuation, from_g, {
          p = TYPE(TO_PTR(pp), type_closure);
          sweep_continuation(tgc, p, from_g);
          pp += size_continuation / sizeof(ptr);
        });

      sweep_space(space_pure_typed_object, from_g, {
          p = TYPE(TO_PTR(pp), type_typed_object);
          pp = TO_VOIDP(((uptr)TO_PTR(pp) + sweep_typed_object(tgc, p, from_g)));
        });

      sweep_space(space_code, from_g, {
          p = TYPE(TO_PTR(pp), type_typed_object);
          sweep_code_object(tgc, p, from_g);
          pp += size_code(CODELEN(p)) / sizeof(ptr);
        });

      sweep_space(space_impure_record, from_g, {
          p = TYPE(TO_PTR(pp), type_typed_object);
          sweep_record(tgc, p, from_g);
          pp = TO_VOIDP((iptr)TO_PTR(pp) +
                        size_record_inst(UNFIX(RECORDDESCSIZE(RECORDINSTTYPE(p)))));
        });

      /* space used only as needed for backreferences: */
      sweep_space(space_impure_typed_object, from_g, {
          p = TYPE(TO_PTR(pp), type_typed_object);
          pp = TO_VOIDP((uptr)TO_PTR(pp) + sweep_typed_object(tgc, p, from_g));
        });

      /* space used only as needed for backreferences: */
      sweep_space(space_closure, from_g, {
          p = TYPE(TO_PTR(pp), type_closure);
          sweep(tgc, p, from_g);
          pp = TO_VOIDP((uptr)TO_PTR(pp) + size_object(p));
        });

      sweep_space(space_reference_array, from_g, {
          p = TYPE(TO_PTR(pp), type_typed_object);
          pp = TO_VOIDP((uptr)TO_PTR(pp) + sweep_typed_object(tgc, p, from_g));
        });

    }

    /* May add to the sweep stack: */
    send_and_receive_remote_sweeps(tgc);

    /* Waiting until sweeping doesn't trigger a change reduces the
       chance that an ephemeron must be registered as a
       segment-specific trigger or gets triggered for recheck, but
       it doesn't change the worst-case complexity. */
    if (tgc->sweep_change == SWEEP_NO_CHANGE)
      check_pending_ephemerons(tgc);

# ifdef ENABLE_MEASURE
    if ((tgc->sweep_change == SWEEP_NO_CHANGE)
        && measure_all_enabled) {
      flush_measure_stack(tgc);
    }
# endif
  } while (tgc->sweep_change == SWEEP_CHANGE_PROGRESS);

  return num_swept_bytes;
}

static void sweep_generation(thread_gc *tgc) {
  sweep_generation_pass(tgc);
}

void enlarge_stack(thread_gc *tgc, ptr *stack, ptr *stack_start, ptr *stack_limit, uptr grow_at_least) {
  uptr sz = ((uptr)*stack - (uptr)*stack_start);
  uptr new_sz = 2 * ((sz == 0) ? (uptr)sweep_stack_min_size : sz);
  ptr new_stack;
  if (new_sz - sz < grow_at_least) new_sz += grow_at_least;
  find_gc_room(tgc, space_data, 0, type_untyped, ptr_align(new_sz), new_stack);
  if (sz != 0)
    memcpy(TO_VOIDP(new_stack), TO_VOIDP(*stack_start), sz);
  tgc->bitmask_overhead[0] += ptr_align(new_sz);
  *stack_start = new_stack;
  *stack_limit = (ptr)((uptr)new_stack + new_sz);
  *stack = (ptr)((uptr)new_stack + sz);
}

iptr sweep_from_stack(thread_gc *tgc) {
  iptr num_swept_bytes = 0;

  if (tgc->sweep_stack > tgc->sweep_stack_start) {
    while (tgc->sweep_stack > tgc->sweep_stack_start) {
      ptr p;
      seginfo *si;
      tgc->sweep_stack = (ptr)((uptr)tgc->sweep_stack - ptr_bytes);
      p = *(ptr *)TO_VOIDP(tgc->sweep_stack);
      /* Room for improvement: `si->generation` is needed only for
         objects that have impure fields. */
      si = SegInfo(ptr_get_segment(p));
      sweep(tgc, p, si->generation);
      COUNT_SWEPT_BYTES(0, size_object(p));
    }
  }

  return num_swept_bytes;
}
 
static iptr sweep_typed_object(thread_gc *tgc, ptr p, IGEN from_g) {
  ptr tf = TYPEFIELD(p);

  if (TYPEP(tf, mask_record, type_record)) {
    sweep_record(tgc, p, from_g);
    return size_record_inst(UNFIX(RECORDDESCSIZE(RECORDINSTTYPE(p))));
  } else if (TYPEP(tf, mask_thread, type_thread)) {
    sweep_thread(tgc, p);
    return size_thread;
  } else {
    /* We get here only if backreference mode pushed other typed objects into
       a typed space or if an object is a counting root */
    sweep(tgc, p, from_g);
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
    seginfo *oldfirst;
    GC_MUTEX_ACQUIRE();
    oldfirst = DirtySegments(from_g, to_g);
    DirtySegments(from_g, to_g) = si;
    si->dirty_prev = &DirtySegments(from_g, to_g);
    si->dirty_next = oldfirst;
    if (oldfirst != NULL) oldfirst->dirty_prev = &si->dirty_next;
    si->min_dirty_byte = to_g;
    GC_MUTEX_RELEASE();
  }
}

static void add_weaksegments_to_resweep(weakseginfo *segs, weakseginfo *last_seg) {
  if (segs != NULL) {
    GC_MUTEX_ACQUIRE();
    last_seg->next = weaksegments_to_resweep;
    weaksegments_to_resweep = segs;
    GC_MUTEX_RELEASE();
  }
}

static void setup_sweep_dirty(NO_PARALLEL_UNUSED thread_gc *tgc) {
  IGEN from_g, to_g;

  weaksegments_to_resweep = NULL;

  /* clear dirty segment lists for copied generations */
  for (from_g = 1; from_g <= MAX_CG; from_g += 1) {
    for (to_g = 0; to_g < from_g; to_g += 1) {
      DirtySegments(from_g, to_g) = NULL;
    }
  }

#ifdef ENABLE_PARALLEL
  /* Move dirty-segment information to the right thread */
  for (from_g = MAX_CG + 1; from_g <= static_generation; INCRGEN(from_g)) {
    for (to_g = 0; to_g <= MAX_CG; to_g += 1) {
      seginfo *dirty_si, *nextsi;
      dirty_si = DirtySegments(from_g, to_g);
      DirtySegments(from_g, to_g) = NULL;
      for (; dirty_si != NULL; dirty_si = nextsi) {
        thread_gc *d_tgc;
        ISPC s;
        
        nextsi = dirty_si->dirty_next;
        s = dirty_si->space;

        if (s == space_new) {
          /* Must be a space that has only locked objects, which we sweeep every time */
          continue;
        }

        d_tgc = dirty_si->creator;
        if (d_tgc->tc == (ptr)0) d_tgc = tgc;

        dirty_si->dirty_next = DirtySegmentsAt(d_tgc->dirty_segments, from_g, to_g);
        DirtySegmentsAt(d_tgc->dirty_segments, from_g, to_g) = dirty_si;
      }
    }
  }
#endif
}

static uptr sweep_dirty_segments(thread_gc *tgc, seginfo **dirty_segments) {
  IGEN youngest, min_youngest;
  ptr *pp, *ppn, *ppend, *nl, start;
  uptr seg, d;
  ISPC s;
  IGEN from_g, to_g;
  seginfo *dirty_si, *nextsi;
  uptr num_swept_bytes = 0;
  weakseginfo *local_weaksegments_to_resweep = NULL, *last_local_weaksegments_to_resweep = NULL;

  PUSH_BACKREFERENCE(Snil) /* '() => from unspecified old object */

  /* no new dirty registrations should happen while sweeping */
  for (from_g = MAX_CG + 1; from_g <= static_generation; INCRGEN(from_g)) {
    for (to_g = 0; to_g <= MAX_CG; to_g += 1) {
      dirty_si = DirtySegmentsAt(dirty_segments, from_g, to_g);
      DirtySegmentsAt(dirty_segments, from_g, to_g) = NULL;
      for (; dirty_si != NULL; dirty_si = nextsi) {
        nextsi = dirty_si->dirty_next;
        seg = dirty_si->number;
        s = dirty_si->space;

        /* reset min dirty byte so we can detect if byte is set while card is swept */
        dirty_si->min_dirty_byte = 0xff;

#ifndef ENABLE_PARALLEL
        if (s == space_new) {
          /* Must be a space that has only locked objects, which we sweeep every time */
          continue;
        }
#endif

        if (s == space_weakpair) {
          weakseginfo *next = local_weaksegments_to_resweep;
          find_gc_room_voidp(tgc, space_data, 0, ptr_align(sizeof(weakseginfo)), local_weaksegments_to_resweep);
          tgc->bitmask_overhead[0] += ptr_align(sizeof(weakseginfo));
          local_weaksegments_to_resweep->si = dirty_si;
          local_weaksegments_to_resweep->next = next;
          if (next == NULL)
            last_local_weaksegments_to_resweep = local_weaksegments_to_resweep;
        }
  
        min_youngest = 0xff;
        start = build_ptr(seg, 0);
        ppend = TO_VOIDP(start);

        /* The original allocation pointer may be relevant as the
           ending point. We assume that thread-local regions for all
           threads without a sweeper are terminated and won't get new
           allocations while dirty sweeping runs, while all
           allocations for a thread with a sweeper will be only using
           that tc, and no allocation happens for a non-target generation. */
        if (from_g == MAX_TG)
          nl = TO_VOIDP(tgc->orig_next_loc[s]);
        else
          nl = TO_VOIDP(tgc->next_loc[from_g][s]);

        d = 0;
        while (d < cards_per_segment) {
          uptr dend = d + sizeof(iptr);
          iptr *dp = (iptr *)(dirty_si->dirty_bytes + d);
          /* check sizeof(iptr) bytes at a time for 0xff */
          if (*dp == -1) {
            pp = ppend;
            ppend += bytes_per_card;
            d = dend;
          } else {
            while (d < dend) {
              pp = ppend;
              ppend += bytes_per_card / sizeof(ptr);
              if (pp <= nl && nl < ppend) ppend = nl;

              if (dirty_si->dirty_bytes[d] <= MAX_CG) {
                /* start out with assumption that we won't find any wrong-way pointers */
                youngest = 0xff;

                COUNT_SWEPT_BYTES(pp, ppend);

                if ((s == space_impure) || (s == space_immobile_impure)
                    || (s == space_impure_typed_object) || (s == space_count_impure)
                    || (s == space_closure)) {
                  if (dirty_si->marked_mask) {
                    while (pp < ppend) {
                      /* handle two pointers at a time */
                      if (marked(dirty_si, TO_PTR(pp))) {
                        FLUSH_REMOTE_BLOCK
                        relocate_dirty(pp, youngest);
                        ppn = pp + 1;
                        relocate_dirty(ppn, youngest);
                        FLUSH_REMOTE(tgc, TYPE(TO_PTR(pp), type_pair));
                        pp = ppn + 1;
                      } else {
                        pp += 2;
                      }
                    }
                  } else {
                    while (pp < ppend && *pp != forward_marker) {
                      /* handle two pointers at a time */
                      FLUSH_REMOTE_BLOCK
                      relocate_dirty(pp, youngest);
                      ppn = pp + 1;
                      relocate_dirty(ppn, youngest);
                      FLUSH_REMOTE(tgc, TYPE(TO_PTR(pp), type_pair));
                      pp = ppn + 1;
                    }
                  }
                } else if (s == space_symbol) {
                  /* old symbols cannot overlap segment boundaries
                     since any object that spans multiple
                     segments begins at the start of a segment,
                     and symbols are much smaller (we assume)
                     than the segment size. */
                  pp = (ptr *)TO_VOIDP(build_ptr(seg,0)) +
                    ((pp - (ptr *)TO_VOIDP(build_ptr(seg,0))) /
                     (size_symbol / sizeof(ptr))) *
                    (size_symbol / sizeof(ptr));

                  /* might overshoot card by part of a symbol.  no harm. */
                  while (pp < ppend && (dirty_si->marked_mask || (*pp != forward_marker))) {
                    ptr p = TYPE(TO_PTR(pp), type_symbol);

                    if (!dirty_si->marked_mask || marked(dirty_si, p))
                      youngest = sweep_dirty_symbol(tgc, p, youngest);

                    pp += size_symbol / sizeof(ptr);
                  }
                } else if (s == space_port) {
                  /* old ports cannot overlap segment boundaries
                     since any object that spans multiple
                     segments begins at the start of a segment,
                     and ports are much smaller (we assume)
                     than the segment size. */
                  pp = (ptr *)TO_VOIDP(build_ptr(seg,0)) +
                    ((pp - (ptr *)TO_VOIDP(build_ptr(seg,0))) /
                     (size_port / sizeof(ptr))) *
                    (size_port / sizeof(ptr));

                  /* might overshoot card by part of a port.  no harm. */
                  while (pp < ppend && (dirty_si->marked_mask || (*pp != forward_marker))) {
                    ptr p = TYPE(TO_PTR(pp), type_typed_object);

                    if (!dirty_si->marked_mask || marked(dirty_si, p))
                      youngest = sweep_dirty_port(tgc, p, youngest);

                    pp += size_port / sizeof(ptr);
                  }
                } else if (s == space_impure_record) { /* abandon hope all ye who enter here */
                  ptr p;
                  if (dirty_si->marked_mask) {
                    /* To get to the start of a record, move backward as long as bytes
                       are marked and segment space+generation+marked is the same. */
                    uptr byte = segment_bitmap_byte(TO_PTR(pp));
                    uptr bit = segment_bitmap_bit(TO_PTR(pp));
                    uptr at_seg = seg;
                    seginfo *si = dirty_si;

                    while (si->marked_mask[byte] & (bit >> ptr_alignment))
                      bit >>= ptr_alignment;
                    if (bit == 1) {
                      /* try previous byte(s) */
                      while (1) {
                        if (byte == 0) {
                          seginfo *prev_si = MaybeSegInfo(at_seg-1);
                          if (prev_si
                              && (prev_si->space == si->space)
                              && (prev_si->generation == si->generation)
                              && prev_si->marked_mask
                              /* object can only continue from the previous segment
                                 if that segment is fully marked (including last words) */
                              && (prev_si->marked_mask[segment_bitmap_bytes-1] == record_full_marked_mask)) {
                            /* maybe the object continues from the previous segment, although
                               we don't really know... */
                            at_seg -= 1;
                            si = prev_si;
                            byte = segment_bitmap_bytes-1;
                          } else {
                            /* object does not continue from the previous segment */
                            break;
                          }
                        } else {
                          if (si->marked_mask[byte-1] == record_full_marked_mask) {
                            /* next byte is full, so keep looking */
                            byte--;
                          } else if (si->marked_mask[byte-1] & record_high_marked_bit) {
                            /* next byte continues, but is not full, so we can start
                               there */
                            if (at_seg != seg) {
                              /* in fact, we can start at the beginning of the
                                 next segment, because that segment's
                                 first object cannot start on this segment */
                              at_seg++;
                              byte = 0;
                              si = SegInfo(at_seg);
                            } else {
                              byte--;
                              bit = record_high_marked_bit;
                              /* find bit contiguous with highest bit */
                              while (si->marked_mask[byte] & (bit >> ptr_alignment))
                                bit >>= ptr_alignment;
                            }
                            break;
                          } else {
                            /* next byte is empty, so don't go there */
                            break;
                          }
                        }
                      }
                    }

                    /* `bit` and `byte` refer to a non-0 mark bit that must be
                       the start of an object */
                    p = build_ptr(at_seg, (byte << (log2_ptr_bytes+3)));
                    while (bit > ptr_alignment) {
                      p = (ptr)((uptr)p + byte_alignment);
                      bit >>= ptr_alignment;
                    }
                    p = TYPE(p, type_typed_object);

                    /* now sweep, but watch out for unmarked holes in the dirty region */
                    while ((ptr *)TO_VOIDP(UNTYPE(p, type_typed_object)) < ppend) {
                      seginfo *si = SegInfo(ptr_get_segment(p));
                      if (!marked(si, p)) {
                        /* skip unmarked words */
                        p = (ptr)((uptr)p + byte_alignment);
                      } else {
                        youngest = sweep_dirty_record(tgc, p, youngest);
                        p = (ptr)((iptr)p +
                            size_record_inst(UNFIX(RECORDDESCSIZE(
                                  RECORDINSTTYPE(p)))));
                      }
                    }
                  } else {
                    uptr j; ptr pnext; seginfo *si;

                    /* synchronize on first record that overlaps the dirty
                       area, then relocate any mutable pointers in that
                       record and those that follow within the dirty area. */

                    /* find first segment of group of like segments */
                    j = seg - 1;
                    while ((si = MaybeSegInfo(j)) != NULL &&
                           si->space == s &&
                           si->generation == from_g &&
                           !si->marked_mask)
                      j -= 1;
                    j += 1;

                    /* now find first record in segment seg */
                    /* we count on following fact: if an object spans two
                       or more segments, then it starts at the beginning
                       of a segment */
                    for (;;) {
                      p = TYPE(build_ptr(j,0),type_typed_object);
                      pnext = (ptr)((iptr)p +
                                    size_record_inst(UNFIX(RECORDDESCSIZE(RECORDINSTTYPE(p)))));
                      if (ptr_get_segment(pnext) >= seg) break;
                      j = ptr_get_segment(pnext) + 1;
                    }

                    /* now find first within dirty area */
                    while ((ptr *)TO_VOIDP(UNTYPE(pnext, type_typed_object)) <= pp) {
                      p = pnext;
                      pnext = (ptr)((iptr)p +
                                    size_record_inst(UNFIX(RECORDDESCSIZE(RECORDINSTTYPE(p)))));
                    }

                    /* now sweep */
                    while ((ptr *)TO_VOIDP(UNTYPE(p, type_typed_object)) < ppend) {
                      /* quit on end of segment */
                      if (FWDMARKER(p) == forward_marker) break;

                      youngest = sweep_dirty_record(tgc, p, youngest);
                      p = (ptr)((iptr)p +
                          size_record_inst(UNFIX(RECORDDESCSIZE(
                                RECORDINSTTYPE(p)))));
                    }
                  }
                } else if (s == space_weakpair) {
                  while (pp < ppend && (dirty_si->marked_mask || (*pp != forward_marker))) {
                    /* skip car field and handle cdr field */
                    if (!dirty_si->marked_mask || marked(dirty_si, TO_PTR(pp))) {
                      FLUSH_REMOTE_BLOCK
                      ptr *ppn = pp + 1;
                      relocate_dirty(ppn, youngest);
                      FLUSH_REMOTE(tgc, TYPE(TO_PTR(pp), type_pair));
                      pp = ppn + 1;
                    } else {
                      pp += 2;
                    }
                  }
                } else if (s == space_ephemeron) {
                  while (pp < ppend && (dirty_si->marked_mask || (*pp != forward_marker))) {
                    ptr p = TYPE(TO_PTR(pp), type_pair);
                    if (!dirty_si->marked_mask || marked(dirty_si, p))
                      youngest = check_dirty_ephemeron(tgc, p, youngest);
                    pp += size_ephemeron / sizeof(ptr);
                  }
                } else if (s == space_reference_array) {
                  /* the same as space_impure and others above, but for object references */
                  if (dirty_si->marked_mask) {
                    while (pp < ppend) {
                      /* handle two pointers at a time */
                      if (marked(dirty_si, TO_PTR(pp))) {
                        FLUSH_REMOTE_BLOCK
                        relocate_reference_dirty(pp, youngest);
                        ppn = pp + 1;
                        relocate_reference_dirty(ppn, youngest);
                        FLUSH_REMOTE(tgc, TYPE(TO_PTR(pp), type_pair)); /* can treat as a pair for resweep */
                        pp = ppn + 1;
                      } else {
                        pp += 2;
                      }
                    }
                  } else {
                    while (pp < ppend && *pp != forward_marker) {
                      /* handle two pointers at a time */
                      FLUSH_REMOTE_BLOCK
                      relocate_reference_dirty(pp, youngest);
                      ppn = pp + 1;
                      relocate_reference_dirty(ppn, youngest);
                      FLUSH_REMOTE(tgc, TYPE(TO_PTR(pp), type_pair));
                      pp = ppn + 1;
                    }
                  }
                } else {
                  S_error_abort("sweep_dirty(gc): unexpected space");
                }

                if (s == space_weakpair) {
                  local_weaksegments_to_resweep->youngest[d] = youngest;
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

  add_weaksegments_to_resweep(local_weaksegments_to_resweep, last_local_weaksegments_to_resweep);

  POP_BACKREFERENCE()

  return num_swept_bytes;
}

#ifndef ENABLE_PARALLEL
static void sweep_dirty(thread_gc *tgc) {
  (void)sweep_dirty_segments(tgc, S_G.dirty_segments);
}
#endif

static void resweep_dirty_weak_pairs(thread_gc *tgc) {
  weakseginfo *ls;
  ptr *pp, *ppend, p;
  IGEN from_g, min_youngest, youngest;
  uptr d;

  /* Make sure terminator is in place for allocation areas relevant to this thread */
  for (from_g = MIN_TG; from_g <= static_generation; from_g++) {
    ptr old;
    old = tgc->next_loc[from_g][space_weakpair];
    if (old != (ptr)0)
      *(ptr*)TO_VOIDP(old) = forward_marker;
  }

  for (ls = weaksegments_to_resweep; ls != NULL; ls = ls->next) {
    seginfo *dirty_si = ls->si;
    from_g = dirty_si->generation;
    ppend = TO_VOIDP(build_ptr(dirty_si->number, 0));
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
          if (dirty_si->dirty_bytes[d] <= MAX_CG) {
            youngest = ls->youngest[d];
            while (pp < ppend) {
              if (!dirty_si->marked_mask && *pp == forward_marker)
                break;
              if (!dirty_si->marked_mask || marked(dirty_si, TO_PTR(pp))) {
                p = *pp;
                seginfo *si;

                /* handle car field */
                if (!FIXMEDIATE(p) && (si = MaybeSegInfo(ptr_get_segment(p))) != NULL) {
                  if (si->old_space) {
                    if (new_marked(si, p)) {
                      youngest = TARGET_GENERATION(si);
                    } else if (FORWARDEDP(p, si)) {
                      IGEN newpg;
                      *pp = GET_FWDADDRESS(p);
                      newpg = TARGET_GENERATION(si);
                      if (newpg < youngest) youngest = newpg;
                    } else {
                      *pp = Sbwp_object;
                    }
                  } else {
                    IGEN pg = si->generation;
                    if (pg < youngest) youngest = pg;
                  }
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
  ptr last = ls, next;

  GC_MUTEX_ACQUIRE();

  next = GUARDIANNEXT(ls);
  while (next != 0) {
    last = next;
    next = GUARDIANNEXT(next);
  }
  INITGUARDIANNEXT(last) = recheck_guardians_ls;
  recheck_guardians_ls = ls;

  GC_MUTEX_RELEASE();
}

static void ephemeron_remove(ptr pe) {
  ptr next = EPHEMERONNEXT(pe);
  *((ptr *)TO_VOIDP(EPHEMERONPREVREF(pe))) = next;
  if (next)
    EPHEMERONPREVREF(next) = EPHEMERONPREVREF(pe);
  EPHEMERONPREVREF(pe) = 0;
  EPHEMERONNEXT(pe) = 0;
}

static void ephemeron_add(ptr *first, ptr pe) {
  ptr last_pe = pe, next_pe = EPHEMERONNEXT(pe), next;
  while (next_pe != 0) {
    last_pe = next_pe;
    next_pe = EPHEMERONNEXT(next_pe);
  }
  next = *first;
  *first = pe;
  EPHEMERONPREVREF(pe) = TO_PTR(first);
  EPHEMERONNEXT(last_pe) = next;
  if (next)
    EPHEMERONPREVREF(next) = TO_PTR(&EPHEMERONNEXT(last_pe));
}

static void add_ephemeron_to_pending(thread_gc *tgc, ptr pe) {
  /* We could call check_ephemeron directly here, but the indirection
     through `PENDINGEPHEMERONS` can dramatically decrease the number
     of times that we have to trigger re-checking, especially since
     check_pending_pehemerons() is run only after all other sweep
     opportunities are exhausted. */
  if (EPHEMERONPREVREF(pe)) ephemeron_remove(pe);
  ephemeron_add(&tgc->pending_ephemerons, pe);
}

static void add_trigger_ephemerons_to_pending(thread_gc *tgc, ptr pe) {
  ephemeron_add(&tgc->pending_ephemerons, pe);
}

static void check_ephemeron(thread_gc *tgc, ptr pe) {
  FLUSH_REMOTE_BLOCK
  ptr p;
  seginfo *si;
  IGEN from_g;
  PUSH_BACKREFERENCE(pe);

  EPHEMERONNEXT(pe) = 0;
  EPHEMERONPREVREF(pe) = 0;
  
  from_g = GENERATION(pe);
  
  p = Scar(pe);
  if (!FIXMEDIATE(p) && (si = MaybeSegInfo(ptr_get_segment(p))) != NULL && si->old_space) {
    if (SEGMENT_IS_LOCAL(si, p)) {
      if (new_marked(si, p)) {
#ifndef NO_DIRTY_NEWSPACE_POINTERS
        IGEN tg = TARGET_GENERATION(si);
        if (tg < from_g) S_record_new_dirty_card(tgc, &INITCAR(pe), tg);
#endif
        relocate_impure(&INITCDR(pe), from_g);
      } else if (FORWARDEDP(p, si)) {
#ifndef NO_DIRTY_NEWSPACE_POINTERS
        IGEN tg = TARGET_GENERATION(si);
        if (tg < from_g) S_record_new_dirty_card(tgc, &INITCAR(pe), tg);
#endif
        INITCAR(pe) = GET_FWDADDRESS(p);
        relocate_impure(&INITCDR(pe), from_g);
      } else {
        /* Not reached, so far; install as trigger */
        ephemeron_add(&si->trigger_ephemerons, pe);
        si->has_triggers = 1;
      }
    } else {
      RECORD_REMOTE(si);
    }
  } else {
    relocate_impure(&INITCDR(pe), from_g);
  }

  FLUSH_REMOTE(tgc, pe);

  POP_BACKREFERENCE();
}

static void check_pending_ephemerons(thread_gc *tgc) {
  ptr pe, next_pe;

  pe = tgc->pending_ephemerons;
  tgc->pending_ephemerons = 0;

  while (pe != 0) {
    next_pe = EPHEMERONNEXT(pe);
    check_ephemeron(tgc, pe);
    pe = next_pe;
  }

  
}

/* Like check_ephemeron(), but for a dirty, old-generation
   ephemeron (that was not yet added to the pending list), so we can
   be less pessimistic than setting `youngest` to the target
   generation: */
static IGEN check_dirty_ephemeron(thread_gc *tgc, ptr pe, IGEN youngest) {
  FLUSH_REMOTE_BLOCK
  ptr p;
  seginfo *si;
  IGEN pg;
  PUSH_BACKREFERENCE(pe);
 
  p = Scar(pe);
  if (!FIXMEDIATE(p) && (si = MaybeSegInfo(ptr_get_segment(p))) != NULL) {
    if (si->old_space) {
      if (SEGMENT_IS_LOCAL(si, p)) {
        if (new_marked(si, p)) {
          relocate_dirty(&INITCDR(pe), youngest);
        } else if (FORWARDEDP(p, si)) {
          INITCAR(pe) = GET_FWDADDRESS(p);
          relocate_dirty(&INITCDR(pe), youngest);
        } else {
          /* Not reached, so far; add to pending list */
          add_ephemeron_to_pending(tgc, pe);

          /* Make the consistent (but pessimistic w.r.t. to wrong-way
             pointers) assumption that the key will stay live and move
             to the target generation. That assumption covers the value
             part, too, since it can't end up younger than the target
             generation. */
          if (youngest != MIN_TG && (pg = TARGET_GENERATION(si)) < youngest)
            youngest = pg;
        }
      } else {
        RECORD_REMOTE(si);
        FLUSH_REMOTE(tgc, pe);
        return youngest;
      }
    } else {
      if (youngest != MIN_TG && (pg = si->generation) < youngest)
        youngest = pg;
      relocate_dirty(&INITCDR(pe), youngest);
    }
  } else {
    /* Non-collectable key means that the value determines
       `youngest`: */
    relocate_dirty(&INITCDR(pe), youngest);
  }

  FLUSH_REMOTE(tgc, pe);

  POP_BACKREFERENCE()

  return youngest;
}

static void finish_pending_ephemerons(thread_gc *tgc, seginfo *si) {
  /* Any ephemeron still in a trigger list is an ephemeron
     whose key was not reached. */
  if (tgc->pending_ephemerons != 0)
    S_error_abort("clear_trigger_ephemerons(gc): non-empty pending list");

  for (; si != NULL; si = si->next) {
    if (si->trigger_ephemerons) {
      ptr pe, next_pe;
      for (pe = si->trigger_ephemerons; pe != 0; pe = next_pe) {
        INITCAR(pe) = Sbwp_object;
        INITCDR(pe) = Sbwp_object;
        next_pe = EPHEMERONNEXT(pe);
        EPHEMERONPREVREF(pe) = 0;
        EPHEMERONNEXT(pe) = 0;
      }
      si->trigger_ephemerons = 0;
    }
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
  }

  return total - count_root_bytes;
}
#endif

static uptr target_generation_space_so_far(thread_gc *tgc) {
  IGEN g;
  ISPC s;
  uptr sz = 0;

  for (g = MIN_TG; g <= MAX_TG; g++) {
    sz += S_G.bytesof[g][countof_phantom];
    
    for (s = 0; s <= max_real_space; s++) {
      sz += S_G.bytes_of_space[g][s];
      if (tgc->next_loc[g][s] != FIX(0))
        sz += (uptr)tgc->next_loc[g][s] - (uptr)tgc->base_loc[g][s];
    }
  }

  return sz;
}

void copy_and_clear_list_bits(thread_gc *tgc, seginfo *oldspacesegments) {
  seginfo *si;
  int i;

  /* Update bits that are used by `list-assuming-immutable?`. */

  for (si = oldspacesegments; si != NULL; si = si->next) {
    if (si->list_bits) {
      if ((si->generation == 0) && !si->marked_mask) {
        /* drop generation-0 bits, because probably the relevant pairs
           were short-lived, and it's ok to recompute them if needed */
      } else {
        if (si->marked_mask) {
          /* Besides marking or copying `si->list_bits`, clear bits
             where there's no corresponding mark bit, so we don't try to
             check forwarding in a future GC */
          seginfo *bits_si = SegInfo(ptr_get_segment(TO_PTR(si->list_bits)));
        
          if (bits_si->old_space) {
            if (bits_si->use_marks) {
              if (!bits_si->marked_mask)
                init_mask(tgc, bits_si->marked_mask, bits_si->generation, 0);
              bits_si->marked_mask[segment_bitmap_byte(TO_PTR(si->list_bits))] |= segment_bitmap_bit(TO_PTR(si->list_bits));
            } else {
              octet *copied_bits;
              find_gc_room_voidp(tgc, space_data, bits_si->generation, ptr_align(segment_bitmap_bytes), copied_bits);
              memcpy_aligned(copied_bits, si->list_bits, segment_bitmap_bytes);
              si->list_bits = copied_bits;
              S_G.bitmask_overhead[bits_si->generation] += ptr_align(segment_bitmap_bytes);
            }
          }

          for (i = 0; i < segment_bitmap_bytes; i++) {
            int m = si->marked_mask[i];
            si->list_bits[i] &= mask_bits_to_list_bits_mask(m);
          }
        }

        if (si->use_marks) {
          /* No forwarding possible from this segment */
        } else {
          /* For forwarded pointers, copy over list bits */
          for (i = 0; i < segment_bitmap_bytes; i++) {
            if (si->list_bits[i]) {
              int bitpos;
              for (bitpos = 0; bitpos < 8; bitpos += ptr_alignment) {
                int bits = si->list_bits[i] & (list_bits_mask << bitpos);
                if (bits != 0) {
                  ptr p = build_ptr(si->number, ((i << (log2_ptr_bytes+3)) + (bitpos << log2_ptr_bytes)));
                  if (FWDMARKER(p) == forward_marker) {
                    ptr new_p = FWDADDRESS(p);
                    seginfo *new_si = SegInfo(ptr_get_segment(new_p));
                    if (!new_si->list_bits)
                      init_mask(tgc, new_si->list_bits, new_si->generation, 0);
                    bits >>= bitpos;
                    new_si->list_bits[segment_bitmap_byte(new_p)] |= segment_bitmap_bits(new_p, bits);
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

/* **************************************** */

#ifdef ENABLE_PARALLEL

static int sweep_mutex_initialized = 0;
static s_thread_mutex_t sweep_mutex;
static s_thread_cond_t sweep_cond;

static int num_running_sweepers;

static IBOOL sweeper_started(int i, IBOOL start_new);
static void run_sweeper(gc_sweeper *sweeper);

static void assign_sweeper(int n, thread_gc *t_tgc) {
  if (sweepers[n].last_tgc == NULL) {
    sweepers[n].first_tgc = t_tgc;
    sweepers[n].last_tgc = t_tgc;
  } else {
    sweepers[n].last_tgc->next = t_tgc;
    sweepers[n].last_tgc = t_tgc;
  }
  t_tgc->next = NULL;
  t_tgc->sweeper = n;
}

#if defined(ENABLE_OBJECT_COUNTS)
# define MAX_SWEEPERS 0
#else
# define MAX_SWEEPERS maximum_parallel_collect_threads
#endif

static void setup_sweepers(thread_gc *tgc) {
  int i, n, next = 0;
  ptr ls;

  assign_sweeper(main_sweeper_index, tgc);

  /* assign a tc for each sweeper to run in parallel */
  for (n = 0, i = 0; (n < MAX_SWEEPERS) && (i < S_collect_waiting_threads); i++) {
    if ((i < MAX_SWEEPERS) && (S_collect_waiting_tcs[i] != (ptr)0)) {
      if (sweeper_started(n, 1)) {
        assign_sweeper(n, THREAD_GC(S_collect_waiting_tcs[i]));
        n++;
      } else
        break;
    }
  }

  next = n;

  /* map remaining threads to existing sweepers */
  for (ls = S_threads; ls != Snil; ls = Scdr(ls)) {
    thread_gc *t_tgc = THREAD_GC(THREADTC(Scar(ls)));
    t_tgc->during_alloc += 1;
    if ((t_tgc != tgc) && (t_tgc->sweeper == main_sweeper_index)) {
      if ((n < MAX_SWEEPERS) && sweeper_started(n, 0)) {
        assign_sweeper(n, t_tgc);
        n++;
        next = n;
      } else {
        if (next == n)
          next = main_sweeper_index;

        assign_sweeper(next, t_tgc);

        if (next == main_sweeper_index)
          next = 0;
        else
          next++;
      }
    }
  }

  num_sweepers = n;

  for (i = 0; i <= num_sweepers; i++) {
    int idx = ((i == num_sweepers) ? main_sweeper_index : i);
    sweepers[idx].num_swept_bytes = 0;
    ADJUST_COUNTER(sweepers[idx].remotes_sent = 0);
    ADJUST_COUNTER(sweepers[idx].remotes_received = 0);
  }
}

static s_thread_rv_t start_sweeper(void *_sweeper) {
  gc_sweeper *sweeper = _sweeper;

#if !defined(WRITE_XOR_EXECUTE_CODE)
  S_thread_start_code_write((ptr)0, static_generation, 0, NULL, 0); /* never ended */
#endif

  (void)s_thread_mutex_lock(&sweep_mutex);
  while (1) {
    while (sweeper->status != SWEEPER_SWEEPING) {
      s_thread_cond_wait(&sweep_cond, &sweep_mutex);
    }
    (void)s_thread_mutex_unlock(&sweep_mutex);

    run_sweeper(sweeper);

    (void)s_thread_mutex_lock(&sweep_mutex);

    s_thread_cond_signal(&sweeper->done_cond);
    sweeper->status = SWEEPER_READY;
  }

  s_thread_return;
}

static IBOOL sweeper_started(int i, IBOOL start_new) {
  if (!sweep_mutex_initialized) {
    s_thread_mutex_init(&sweep_mutex);
    s_thread_cond_init(&sweep_cond);
    s_thread_cond_init(&sweepers[main_sweeper_index].work_cond);
    sweep_mutex_initialized = 1;
  }

  if (sweepers[i].status == SWEEPER_NONE) {
    int status;

    if (!start_new)
      return 0;

    sweepers[i].status = SWEEPER_READY;
    s_thread_cond_init(&sweepers[i].done_cond);
    s_thread_cond_init(&sweepers[i].work_cond);
    
    if ((status = s_thread_create(start_sweeper, &sweepers[i])) != 0) {
      /* eror creating a thread; just go with as many as we have */
      sweepers[i].status = SWEEPER_NONE;
      s_thread_cond_destroy(&sweepers[i].done_cond);
      return 0;
    }
  }

  return 1;
}

static void run_sweepers(void) {
  int i;

  in_parallel_sweepers = 1;

  /* start other sweepers */
  (void)s_thread_mutex_lock(&sweep_mutex);
  for (i = 0; i < num_sweepers + 1; i++) {
    int idx = ((i == num_sweepers) ? main_sweeper_index : i);
    sweepers[idx].status = SWEEPER_SWEEPING;
    num_running_sweepers++;
  }
  s_thread_cond_broadcast(&sweep_cond);
  (void)s_thread_mutex_unlock(&sweep_mutex);
  
  /* sweep in the main thread */
  run_sweeper(&sweepers[main_sweeper_index]);
  
  /* wait for other sweepers and clean up each tgc */
  (void)s_thread_mutex_lock(&sweep_mutex);
  for (i = 0; i < num_sweepers; i++) {
    while (sweepers[i].status != SWEEPER_READY)
      s_thread_cond_wait(&sweepers[i].done_cond, &sweep_mutex);
  }
  (void)s_thread_mutex_unlock(&sweep_mutex);

  in_parallel_sweepers = 0;
}

static void teardown_sweepers(void) {
  thread_gc *t_tgc;
  int i;

  REPORT_TIME(fprintf(stderr, "------\n"));
  for (i = 0; i <= num_sweepers; i++) {
    int idx = ((i == num_sweepers) ? main_sweeper_index : i);

    for (t_tgc = sweepers[idx].first_tgc; t_tgc != NULL; t_tgc = t_tgc->next) {
      IGEN g;
      S_G.bitmask_overhead[0] += t_tgc->bitmask_overhead[0];
      t_tgc->bitmask_overhead[0] = 0;
      for (g = MIN_TG; g <= MAX_TG; g++) {
        S_G.bitmask_overhead[g] += t_tgc->bitmask_overhead[g];
        t_tgc->bitmask_overhead[g] = 0; /* needed to avoid double add for main_sweeper_index */
      }
      S_flush_instruction_cache(t_tgc->tc);
      t_tgc->sweeper = main_sweeper_index;
      t_tgc->queued_fire = 0;
      t_tgc->during_alloc -= 1;
    }
    
    REPORT_TIME(fprintf(stderr, "%d swpr  +%ld ms  %ld ms  %ld bytes  %d sent %d received\n",
                        MAX_CG, sweepers[idx].step, sweepers[idx].sweep_accum, sweepers[idx].num_swept_bytes,
                        sweepers[idx].remotes_sent,
                        sweepers[idx].remotes_received));

    sweepers[idx].first_tgc = sweepers[idx].last_tgc = NULL;
  }
}

static void run_sweeper(gc_sweeper *sweeper) {
  iptr num_swept_bytes = 0;
  thread_gc *tgc;

  GET_CPU_TIME(start);

  for (tgc = sweeper->first_tgc; tgc != NULL; tgc = tgc->next) {
    num_swept_bytes += sweep_dirty_segments(tgc, tgc->dirty_segments);
    num_swept_bytes += sweep_generation_pass(tgc);
  }
  
  (void)s_thread_mutex_lock(&sweep_mutex);
  --num_running_sweepers;
  while (1) {
    IBOOL any_ranges = 0;
    for (tgc = sweeper->first_tgc; tgc != NULL; tgc = tgc->next) {
      if (tgc->receive_remote_sweep_stack != tgc->receive_remote_sweep_stack_start) {
        any_ranges = 1;
        break;
      }
    }

    if ((num_running_sweepers == 0) && !any_ranges) {
      /* everyone is done */
      int i, they = main_sweeper_index;
      for (i = -1; i < num_sweepers; i++) {
        s_thread_cond_signal(&sweepers[they].work_cond);
        they = i + 1;
      }
      (void)s_thread_mutex_unlock(&sweep_mutex);
      break;
    } else {
      /* wait for work */
      if (any_ranges) {
        /* some work appeared since we last checked */
        num_running_sweepers++;
      } else {
        sweeper->status = SWEEPER_WAITING_FOR_WORK;
        s_thread_cond_wait(&sweeper->work_cond, &sweep_mutex);
      }
      if (sweeper->status != SWEEPER_WAITING_FOR_WORK) {
        /* got work; num_running_sweepers was incremented, too */
        (void)s_thread_mutex_unlock(&sweep_mutex);

        for (tgc = sweeper->first_tgc; tgc != NULL; tgc = tgc->next) {
          num_swept_bytes += sweep_generation_pass(tgc);
        }
 
        (void)s_thread_mutex_lock(&sweep_mutex);
        --num_running_sweepers;
      } else if (num_running_sweepers == 0) {
        /* other sweeper noticed that everyone is done */
        (void)s_thread_mutex_unlock(&sweep_mutex);
        break;
      } else {
        /* not clear why we were awoken, so just go around again */
      }
    }
  }

  ACCUM_CPU_TIME(sweeper->sweep_accum, step, start);
  ADJUST_COUNTER(sweeper->step = step);

  sweeper->num_swept_bytes += num_swept_bytes;
}

static void send_and_receive_remote_sweeps(thread_gc *tgc) {
  (void)s_thread_mutex_lock(&sweep_mutex);

  /* Send objects to remote sweepers */
  while (tgc->send_remote_sweep_stack > tgc->send_remote_sweep_stack_start) {
    thread_gc *r_tgc;    
    ptr p;

    tgc->send_remote_sweep_stack = (ptr)((uptr)tgc->send_remote_sweep_stack - (2 * ptr_bytes));
    p = ((ptr *)TO_VOIDP(tgc->send_remote_sweep_stack))[0];
    r_tgc = TO_VOIDP(((ptr *)TO_VOIDP(tgc->send_remote_sweep_stack))[1]);

    if (r_tgc->receive_remote_sweep_stack == r_tgc->receive_remote_sweep_stack_limit)
      enlarge_stack(tgc,
                    &r_tgc->receive_remote_sweep_stack,
                    &r_tgc->receive_remote_sweep_stack_start,
                    &r_tgc->receive_remote_sweep_stack_limit,
                    ptr_bytes);
    
    *(ptr *)TO_VOIDP(r_tgc->receive_remote_sweep_stack) = p;
    r_tgc->receive_remote_sweep_stack = (ptr)((uptr)r_tgc->receive_remote_sweep_stack + ptr_bytes);

    if (sweepers[r_tgc->sweeper].status == SWEEPER_WAITING_FOR_WORK) {
      num_running_sweepers++;
      sweepers[r_tgc->sweeper].status = SWEEPER_SWEEPING;
      s_thread_cond_signal(&sweepers[r_tgc->sweeper].work_cond);
    }

    ADJUST_COUNTER(sweepers[tgc->sweeper].remotes_sent++);
  }

  /* Received objects from remote sweepers, moving to sweep stack: */
  if (tgc->receive_remote_sweep_stack != tgc->receive_remote_sweep_stack_start) {
    iptr len = (uptr)tgc->receive_remote_sweep_stack - (uptr)tgc->receive_remote_sweep_stack_start;
    
    tgc->sweep_change = SWEEP_CHANGE_PROGRESS;

    if (((uptr)tgc->sweep_stack + len) > (uptr)tgc->sweep_stack_limit)
      enlarge_stack(tgc, &tgc->sweep_stack, &tgc->sweep_stack_start, &tgc->sweep_stack_limit, len);

    memcpy(TO_VOIDP(tgc->sweep_stack), TO_VOIDP(tgc->receive_remote_sweep_stack_start), len);
    tgc->sweep_stack = (ptr)((uptr)tgc->sweep_stack + len);
    if ((uptr)tgc->sweep_stack > (uptr)tgc->sweep_stack_limit)
      abort();
    tgc->receive_remote_sweep_stack = tgc->receive_remote_sweep_stack_start;

    ADJUST_COUNTER(sweepers[tgc->sweeper].remotes_received += (len / ptr_bytes));
  }

  (void)s_thread_mutex_unlock(&sweep_mutex);
}

#endif

/* **************************************** */

#ifdef ENABLE_MEASURE

static void init_measure(thread_gc *tgc, IGEN min_gen, IGEN max_gen) {
  uptr init_stack_len = 1024;

  min_measure_generation = min_gen;
  max_measure_generation = max_gen;
  
  find_gc_room_voidp(tgc, space_data, 0, ptr_align(init_stack_len), measure_stack_start);
  S_G.bitmask_overhead[0] += ptr_align(init_stack_len);
  measure_stack = TO_VOIDP(measure_stack_start);
  measure_stack_limit = TO_VOIDP((uptr)TO_PTR(measure_stack_start) + init_stack_len);

  measured_seginfos = Snil;

  measure_all_enabled = 1;
}

static void finish_measure() {
  ptr ls;

  for (ls = measured_seginfos; ls != Snil; ls = Scdr(ls)) {
    ptr pe, next_pe;
    seginfo *si = TO_VOIDP(Scar(ls));
    si->measured_mask = NULL;
    for (pe = si->trigger_ephemerons; pe != 0; pe = next_pe) {
      next_pe = EPHEMERONNEXT(pe);
      EPHEMERONPREVREF(pe) = 0;
      EPHEMERONNEXT(pe) = 0;
    }
    si->trigger_ephemerons = 0;
  }

  measure_all_enabled = 0;
}

static void init_counting_mask(thread_gc *tgc, seginfo *si) {
  init_mask(tgc, si->counting_mask, 0, 0);
}

static void init_measure_mask(thread_gc *tgc, seginfo *si) {
  init_mask(tgc, si->measured_mask, 0, 0);
  measured_seginfos = S_cons_in(tgc->tc, space_new, 0, TO_PTR(si), measured_seginfos);
}

#define measure_unreached(si, p) \
  (!si->measured_mask \
   || !(si->measured_mask[segment_bitmap_byte(p)] & segment_bitmap_bit(p)))

#define measure_mask_is_set(mm, si, p) \
  (mm[segment_bitmap_byte(p)] & segment_bitmap_bit(p))
#define measure_mask_set(mm, si, p) \
  do { mm[segment_bitmap_byte(p)] |= segment_bitmap_bit(p); } while (0)
#define measure_mask_unset(mm, si, p) \
  do { mm[segment_bitmap_byte(p)] -= segment_bitmap_bit(p); } while (0)

static void push_measure(thread_gc *tgc, ptr p)
{
  seginfo *si = MaybeSegInfo(ptr_get_segment(p));

  if (!si)
    return;

  if (si->old_space) {
    /* We must be in a GC--measure fusion, so switch back to GC */
    FLUSH_REMOTE_BLOCK
    BLOCK_SET_THREAD(si->creator);
    relocate_pure_help_help(&p, p, si);
    ASSERT_EMPTY_FLUSH_REMOTE();
    return;
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
      init_measure_mask(tgc, si);
    else if (si->measured_mask[byte] & bit)
      return;

    si->measured_mask[byte] |= bit;
  }

  if (si->trigger_ephemerons) {
    add_trigger_ephemerons_to_pending_measure(si->trigger_ephemerons);
    si->trigger_ephemerons = 0;
  }

  if (measure_stack == measure_stack_limit) {
    uptr sz = ptr_bytes * (measure_stack_limit - measure_stack_start);
    uptr new_sz = 2*sz;
    ptr *new_measure_stack;
    find_gc_room_voidp(tgc, space_data, 0, ptr_align(new_sz), new_measure_stack);
    S_G.bitmask_overhead[0] += ptr_align(new_sz);
    memcpy(new_measure_stack, measure_stack_start, sz);
    measure_stack_start = new_measure_stack;
    measure_stack_limit = TO_VOIDP((uptr)TO_PTR(new_measure_stack) + new_sz);
    measure_stack = TO_VOIDP((uptr)TO_PTR(new_measure_stack) + sz);
  }
  
  *(measure_stack++) = p;
}

static void measure_add_stack_size(ptr stack, uptr size) {
  seginfo *si = SegInfo(ptr_get_segment(stack));
  if (!(si->old_space)
      && (si->generation <= max_measure_generation)
      && (si->generation >= min_measure_generation))
    measure_total += size;
}

static void add_ephemeron_to_pending_measure(thread_gc *tgc, ptr pe) {
  /* If we're in hybrid mode and the key in `pe` is in the
     old space, then we need to use the regular pending list
     instead of the measure-specific one */
  seginfo *si;
  ptr p = Scar(pe);

  if (!FIXMEDIATE(p) && (si = MaybeSegInfo(ptr_get_segment(p))) != NULL && si->old_space)
    add_ephemeron_to_pending(tgc, pe);
  else {
    if (EPHEMERONPREVREF(pe))
      S_error_abort("add_ephemeron_to_pending_measure: ephemeron is in some list");
    ephemeron_add(&pending_measure_ephemerons, pe);
  }
}

static void add_trigger_ephemerons_to_pending_measure(ptr pe) {
  ephemeron_add(&pending_measure_ephemerons, pe);
}

static void check_ephemeron_measure(thread_gc *tgc, ptr pe) {
  ptr p;
  seginfo *si;

  EPHEMERONPREVREF(pe) = 0;
  EPHEMERONNEXT(pe) = 0;

  p = Scar(pe);
  if (!FIXMEDIATE(p) && (si = MaybeSegInfo(ptr_get_segment(p))) != NULL
      && (si->generation <= max_measure_generation)
      && (si->generation >= min_measure_generation)
      && (!(si->old_space) || !FORWARDEDP(p, si))
      && (measure_unreached(si, p)
          || (si->counting_mask
              && (si->counting_mask[segment_bitmap_byte(p)] & segment_bitmap_bit(p))))) {
    /* Not reached, so far; install as trigger */
    ephemeron_add(&si->trigger_ephemerons, pe);
    if (!si->measured_mask)
      init_measure_mask(tgc, si); /* so triggers are cleared at end */
    return;
  }

  p = Scdr(pe);
  if (!FIXMEDIATE(p))
    push_measure(tgc, p);
}

static void check_pending_measure_ephemerons(thread_gc *tgc) {
  ptr pe, next_pe;

  pe = pending_measure_ephemerons;
  pending_measure_ephemerons = 0;
  while (pe != 0) {
    next_pe = EPHEMERONNEXT(pe);
    check_ephemeron_measure(tgc, pe);
    pe = next_pe;
  }
}

void gc_measure_one(thread_gc *tgc, ptr p) {
  seginfo *si = SegInfo(ptr_get_segment(p));

  if (si->trigger_ephemerons) {
    add_trigger_ephemerons_to_pending_measure(si->trigger_ephemerons);
    si->trigger_ephemerons = 0;
  }
  
  measure(tgc, p);

  flush_measure_stack(tgc);
}

void flush_measure_stack(thread_gc *tgc) {
  if ((measure_stack <= measure_stack_start)
      && !pending_measure_ephemerons)
    return;

  tgc->sweep_change = SWEEP_CHANGE_PROGRESS;

  while (1) {
    while (measure_stack > measure_stack_start)
      measure(tgc, *(--measure_stack));

    if (!pending_measure_ephemerons)
      break;
    check_pending_measure_ephemerons(tgc);
  }
}

ptr S_count_size_increments(ptr ls, IGEN generation) {
  ptr l, totals = Snil, totals_prev = 0;
  ptr tc = get_thread_context();
  thread_gc *tgc = THREAD_GC(tc);

  /* caller acquires mutex and ensures that this is the only thread */

  init_measure(tgc, 0, generation);

  for (l = ls; l != Snil; l = Scdr(l)) {
    ptr p = Scar(l);
    if (!FIXMEDIATE(p)) {
      seginfo *si = SegInfo(ptr_get_segment(p));

      if (!si->measured_mask)
        init_measure_mask(tgc, si);
      measure_mask_set(si->measured_mask, si, p);

      if (!si->counting_mask)
        init_counting_mask(tgc, si);
      measure_mask_set(si->counting_mask, si, p);
    }
  }

  for (l = ls; l != Snil; l = Scdr(l)) {
    ptr p = Scar(l);

    measure_total = 0;

    if (!FIXMEDIATE(p)) {
      seginfo *si = SegInfo(ptr_get_segment(p));
      if (measure_mask_is_set(si->counting_mask, si, p)) {
        measure_mask_unset(si->counting_mask, si, p);
        gc_measure_one(tgc, p);
      }
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
    if (!FIXMEDIATE(p)) {
      seginfo *si = SegInfo(ptr_get_segment(p));
      si->counting_mask = NULL;
    }
  }

  finish_measure();

  return totals;
}

#endif
