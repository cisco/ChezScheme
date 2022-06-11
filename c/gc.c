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

#define enable_object_counts do_not_use_enable_object_counts_in_this_file_use_ifdef_ENABLE_OBJECT_COUNTS_instead

#if defined(MIN_TG) && defined(MAX_TG)
#if MIN_TG == MAX_TG
#define NO_DIRTY_NEWSPACE_POINTERS
#endif
#endif

#if defined(MAX_CG) && defined(MIN_TG) && defined(MAX_TG)
#define FORMAL_CTGS
#define ONLY_FORMAL_CTGS void
#define ACTUAL_CTGS
#define ONLY_ACTUAL_CTGS
#define DECLARE_CTGS(max_cg, min_tg, max_tg) IGEN max_cg = MAX_CG, min_tg = MIN_TG, max_tg = MAX_TG
#define GCENTRY_PROTO(tcdecl, max_cgdecl, min_tgdecl, max_tgdecl) (tcdecl)
#else
#define FORMAL_CTGS , UINT _ctgs
#define ONLY_FORMAL_CTGS UINT _ctgs
#define ACTUAL_CTGS , _ctgs
#define ONLY_ACTUAL_CTGS _ctgs
#define DECLARE_CTGS(max_cg, min_tg, max_tg) UINT _ctgs = (((UINT)min_tg << 16) | ((UINT)max_cg << 8) | (UINT)max_tg)
#define MAX_CG ((INT)((_ctgs >> 8) & 0xff))
#define MIN_TG ((INT)(_ctgs >> 16))
#define MAX_TG ((INT)(_ctgs & 0xff))
#define GCENTRY_PROTO(tcdecl, max_cgdecl, min_tgdecl, max_tgdecl) (tcdecl, max_cgdecl, min_tgdecl, max_tgdecl)
#endif

/* locally defined functions */
#ifndef NO_DIRTY_NEWSPACE_POINTERS
static void record_new_dirty_card(ptr *ppp, IGEN to_g);
#endif /* !NO_DIRTY_NEWSPACE_POINTERS */
#ifndef NO_LOCKED_OLDSPACE_OBJECTS
static ptr append_bang(ptr ls1, ptr ls2);
static uptr count_unique(ptr ls);
static uptr list_length(ptr ls);
static ptr dosort(ptr ls, uptr n);
static ptr domerge(ptr l1, ptr l2);
static IBOOL search_locked(ptr p);
#endif /* !NO_LOCKED_OLDSPACE_OBJECTS */
static IGEN copy(ptr pp, seginfo *si, ptr *ppp FORMAL_CTGS);
static void sweep_locked_ptrs(ptr *p, iptr n FORMAL_CTGS);
static void sweep_locked(ptr tc, ptr p, IBOOL sweep_pure FORMAL_CTGS);
static ptr copy_stack(ptr old, iptr *length, iptr clength FORMAL_CTGS);
static void resweep_weak_pairs(ONLY_FORMAL_CTGS);
static void forward_or_bwp(ptr *pp, ptr p);
static void sweep_generation(ptr tc FORMAL_CTGS);
#ifndef NO_LOCKED_OLDSPACE_OBJECTS
static iptr size_object(ptr p);
#endif /* !NO_LOCKED_OLDSPACE_OBJECTS */
static iptr sweep_typed_object(ptr p, IGEN from_g FORMAL_CTGS);
static void sweep_symbol(ptr p, IGEN from_g FORMAL_CTGS);
static void sweep_port(ptr p, IGEN from_g FORMAL_CTGS);
static void sweep_thread(ptr p FORMAL_CTGS);
static void sweep_continuation(ptr p FORMAL_CTGS);
static void sweep_stack(uptr base, uptr size, uptr ret FORMAL_CTGS);
static void sweep_record(ptr x, IGEN from_g FORMAL_CTGS);
static IGEN sweep_dirty_record(ptr x, IGEN youngest FORMAL_CTGS);
static void sweep_code_object(ptr tc, ptr co FORMAL_CTGS);
static void record_dirty_segment(IGEN from_g, IGEN to_g, seginfo *si);
static void sweep_dirty(ONLY_FORMAL_CTGS);
static void resweep_dirty_weak_pairs(ONLY_FORMAL_CTGS);
static void add_ephemeron_to_pending(ptr p);
static void add_trigger_ephemerons_to_repending(ptr p);
static void check_trigger_ephemerons(seginfo *si);
static void check_ephemeron(ptr pe, IBOOL add_to_trigger FORMAL_CTGS);
static void check_pending_ephemerons(ONLY_FORMAL_CTGS);
static IGEN check_dirty_ephemeron(ptr pe, IGEN youngest FORMAL_CTGS);
static void clear_trigger_ephemerons();

#define OLDSPACE(x) (SPACE(x) & space_old)

/* #define DEBUG */

/* initialized and used each gc cycle.  any others should be defined in globals.h */
static IBOOL change;
static ptr sweep_loc[static_generation+1][max_real_space+1];
static ptr orig_next_loc[static_generation+1][max_real_space+1];
#ifndef NO_LOCKED_OLDSPACE_OBJECTS
static ptr sorted_locked_objects;
#endif /* !NO_LOCKED_OLDSPACE_OBJECTS */
static ptr tlcs_to_rehash;

#ifndef compute_target_generation
FORCEINLINE IGEN compute_target_generation(IGEN g FORMAL_CTGS) {
  return g == MAX_TG ? g : g < MIN_TG ? MIN_TG : g + 1;
}
#endif /* !compute_target_generation */

/* rkd 2020/06/16: had the relocate routines more nicely coded with FORCEINLINE.
   unfortunately, the llvm-compiled gc ran much (10-20%) slower on my mac. */
#define relocate_return_addr(PCP) do {\
  ptr *_pcp = PCP;\
  seginfo *_si;\
  ptr _cp = *_pcp;\
  if ((_si = SegInfo(ptr_get_segment(_cp)))->space & space_old) {\
    iptr _co = ENTRYOFFSET(_cp) + ((uptr)_cp - (uptr)&ENTRYOFFSET(_cp));\
    ptr _pp = (ptr)((uptr)_cp - _co);\
    if (FWDMARKER(_pp) == forward_marker)\
      _pp = FWDADDRESS(_pp);\
    else\
      (void) copy(_pp, _si, &_pp ACTUAL_CTGS);\
    *_pcp = (ptr)((uptr)_pp + _co);\
  }\
} while (0)

/* use relocate_dirty for oldspace fields that might hold pointers to younger objects */
#define relocate_dirty(PPP, YOUNGEST) do {\
  seginfo *_si; ptr *_ppp = PPP, _pp = *_ppp; IGEN _pg;\
  if (!IMMEDIATE(_pp) && (_si = MaybeSegInfo(ptr_get_segment(_pp))) != NULL) {\
    if (!(_si->space & space_old)) {\
      _pg = _si->generation;\
    } else if (FWDMARKER(_pp) == forward_marker && TYPEBITS(_pp) != type_flonum) {\
      *_ppp = FWDADDRESS(_pp);\
      _pg = compute_target_generation(_si->generation ACTUAL_CTGS);\
    } else {\
      _pg = copy(_pp, _si, _ppp ACTUAL_CTGS);\
    }\
    if (_pg < YOUNGEST) YOUNGEST = _pg;\
  }\
} while (0)

/* use relocate_pure for newspace fields that can't point to younger objects */
#define relocate_pure_help(PPP, PP) do {\
  ptr *__ppp = PPP, __pp = PP; seginfo *__si;\
  if (!IMMEDIATE(__pp) && (__si = MaybeSegInfo(ptr_get_segment(__pp))) != NULL && (__si->space & space_old)) {\
    if (FWDMARKER(__pp) == forward_marker && TYPEBITS(__pp) != type_flonum) {\
      *__ppp = FWDADDRESS(__pp);\
    } else {\
      (void) copy(__pp, __si, __ppp ACTUAL_CTGS);\
    }\
  }\
} while (0)

#define relocate_pure(PPP) do {\
  ptr *_ppp = PPP; relocate_pure_help(_ppp, *_ppp);\
} while (0)

/* use relocate_impure for newspace fields that can point to younger objects */
#ifdef NO_DIRTY_NEWSPACE_POINTERS
#define relocate_impure_help(PPP, PP, FROM_G) do {(void)FROM_G; relocate_pure_help(PPP, PP);} while (0)
#define relocate_impure(PPP, FROM_G) do {(void)FROM_G; relocate_pure(PPP);} while (0)
#else /* !NO_DIRTY_NEWSPACE_POINTERS */
/* the initialization of __to_g to 0 below shouldn't be necessary, but gcc 7.5.0 complains without it */
#define relocate_impure_help(PPP, PP, FROM_G) do {\
  ptr *__ppp = PPP, __pp = PP; IGEN __from_g = FROM_G;\
  seginfo *__si; IGEN __to_g = 0;\
  if (!IMMEDIATE(__pp) && (__si = MaybeSegInfo(ptr_get_segment(__pp))) != NULL && (__si->space & space_old)) {\
    if (FWDMARKER(__pp) == forward_marker && TYPEBITS(__pp) != type_flonum ?\
         (*__ppp = FWDADDRESS(__pp), (__from_g > 1 && (__to_g = compute_target_generation(__si->generation ACTUAL_CTGS)) < __from_g)) :\
         ((__to_g = copy(__pp, __si, __ppp ACTUAL_CTGS)) < __from_g)) {\
      record_new_dirty_card(__ppp, __to_g);\
    }\
  }\
} while (0)

#define relocate_impure(PPP, FROM_G) do {\
  ptr *_ppp = PPP; relocate_impure_help(_ppp, *_ppp, FROM_G);\
} while (0)
#endif /* !NO_DIRTY_NEWSPACE_POINTERS */

#ifndef NO_DIRTY_NEWSPACE_POINTERS
typedef struct _dirtycardinfo {
  uptr card;
  IGEN youngest;
  struct _dirtycardinfo *next;
} dirtycardinfo;

static dirtycardinfo *new_dirty_cards;

static void record_new_dirty_card(ptr *ppp, IGEN to_g) {
  uptr card = (uptr)ppp >> card_offset_bits;

  dirtycardinfo *ndc = new_dirty_cards;
  if (ndc != NULL && ndc->card == card) {
    if (to_g < ndc->youngest) ndc->youngest = to_g;
  } else {
    dirtycardinfo *next = ndc;
    find_room(space_new, 0, typemod, ptr_align(sizeof(dirtycardinfo)), ndc);
    ndc->card = card;
    ndc->youngest = to_g;
    ndc->next = next;
    new_dirty_cards = ndc;
  }
}
#endif

/* rkd 2015/06/05: tried to use sse instructions.  abandoned the code
   because the collector ran slower */
#define copy_ptrs(ty, p1, p2, n) {\
  ptr *Q1, *Q2, *Q1END;\
  Q1 = (ptr *)UNTYPE((p1),ty);\
  Q2 = (ptr *)UNTYPE((p2),ty);\
  Q1END = (ptr *)((uptr)Q1 + n);\
  while (Q1 != Q1END) *Q1++ = *Q2++;}

#ifdef NO_LOCKED_OLDSPACE_OBJECTS
#define locked(p) 0
#else /* !NO_LOCKED_OLDSPACE_OBJECTS */
/* MAXPTR is used to pad the sorted_locked_object vector.  The pad value must be greater than any heap address */
#define MAXPTR ((ptr)-1)

static ptr append_bang(ptr ls1, ptr ls2) { /* assumes ls2 pairs are older than ls1 pairs, or that we don't care */
  if (ls2 == Snil) {
    return ls1;
  } else if (ls1 == Snil) {
    return ls2;
  } else {
    ptr this = ls1, next;
    while ((next = Scdr(this)) != Snil) this = next;
    INITCDR(this) = ls2;
    return ls1;
  }
}

static uptr count_unique(ptr ls) { /* assumes ls is sorted and nonempty */
  uptr i = 1; ptr x = Scar(ls), y;
  while ((ls = Scdr(ls)) != Snil) {
    if ((y = Scar(ls)) != x) {
      i += 1;
      x = y;
    }
  }
  return i;
}

#define CARLT(x, y) (Scar(x) < Scar(y))
mkmergesort(dosort, domerge, ptr, Snil, CARLT, INITCDR)

uptr list_length(ptr ls) {
  uptr i = 0;
  while (ls != Snil) { ls = Scdr(ls); i += 1; }
  return i;
}

static IBOOL search_locked(ptr p) {
  uptr k; ptr v, *vp, x;
  v = sorted_locked_objects;
  k = Svector_length(v);
  vp = &INITVECTIT(v, 0);
  for (;;) {
    k >>= 1;
    if ((x = vp[k]) == p) return 1;
    if (k == 0) return 0;
    if (x < p) vp += k + 1;
  }
}

#define locked(p) (sorted_locked_objects != FIX(0) && search_locked(p))
#endif /* !NO_LOCKED_OLDSPACE_OBJECTS */

FORCEINLINE void check_trigger_ephemerons(seginfo *si) {
  /* Registering ephemerons to recheck at the granularity of a segment
     means that the worst-case complexity of GC is quadratic in the
     number of objects that fit into a segment (but that only happens
     if the objects are ephemeron keys that are reachable just through
     a chain via the value field of the same ephemerons). */
  if (si->trigger_ephemerons) {
    add_trigger_ephemerons_to_repending(si->trigger_ephemerons);
    si->trigger_ephemerons = NULL;
  }
}

static IGEN copy(ptr pp, seginfo *si, ptr *ppp FORMAL_CTGS) {
    ptr p, tf; ITYPE t;
    IGEN newg = compute_target_generation(si->generation ACTUAL_CTGS);

#ifndef NO_LOCKED_OLDSPACE_OBJECTS
    if (locked(pp)) { *ppp = pp; return newg; }
#endif /* !NO_LOCKED_OLDSPACE_OBJECTS */

    change = 1;

    check_trigger_ephemerons(si);

    if ((t = TYPEBITS(pp)) == type_typed_object) {
      tf = TYPEFIELD(pp);
      if (TYPEP(tf, mask_record, type_record)) {
          ptr rtd; iptr n; ISPC s;

        /* relocate to make sure we aren't using an oldspace descriptor
           that has been overwritten by a forwarding marker, but don't loop
           on tag-reflexive base descriptor */
          if ((rtd = tf) != pp) relocate_pure(&rtd);

          n = size_record_inst(UNFIX(RECORDDESCSIZE(rtd)));

#ifdef ENABLE_OBJECT_COUNTS
          { ptr counts; IGEN g;
            counts = RECORDDESCCOUNTS(rtd);
            if (counts == Sfalse) {
              IGEN grtd = rtd == pp ? newg : GENERATION(rtd);
              S_G.countof[grtd][countof_rtd_counts] += 1;
             /* allocate counts struct in same generation as rtd.  initialize timestamp & counts */
              find_room(space_data, grtd, type_typed_object, size_rtd_counts, counts);
              RTDCOUNTSTYPE(counts) = type_rtd_counts;
              RTDCOUNTSTIMESTAMP(counts) = S_G.gctimestamp[0];
              for (g = 0; g <= static_generation; g += 1) RTDCOUNTSIT(counts, g) = 0;
              RECORDDESCCOUNTS(rtd) = counts;
              S_G.rtds_with_counts[grtd] = S_cons_in((grtd == 0 ? space_new : space_impure), grtd, rtd, S_G.rtds_with_counts[grtd]);
              S_G.countof[grtd][countof_pair] += 1;
            } else {
              relocate_pure(&counts);
              RECORDDESCCOUNTS(rtd) = counts;
              if (RTDCOUNTSTIMESTAMP(counts) != S_G.gctimestamp[0]) S_fixup_counts(counts);
            }
            RTDCOUNTSIT(counts, newg) += 1;
          }
#endif /* ENABLE_OBJECT_COUNTS */

        /* if the rtd is the only pointer and is immutable, put the record
           into space data.  if the record contains only pointers, put it
           into space_pure or space_impure.  otherwise put it into
           space_pure_typed_object or space_impure_record.  we could put all
           records into space_{pure,impure}_record or even into
           space_impure_record, but by picking the target space more
           carefully we may reduce fragmentation and sweeping cost */
          s = RECORDDESCPM(rtd) == FIX(1) && RECORDDESCMPM(rtd) == FIX(0) ?
                  space_data :
                  RECORDDESCPM(rtd) == FIX(-1) ?
                      RECORDDESCMPM(rtd) == FIX(0) ?
                          space_pure :
                          space_impure :
                      RECORDDESCMPM(rtd) == FIX(0) ?
                          space_pure_typed_object :
                          space_impure_record;

          find_room(s, newg, type_typed_object, n, p);
          copy_ptrs(type_typed_object, p, pp, n);

        /* overwrite type field with forwarded descriptor */
          RECORDINSTTYPE(p) = rtd == pp ? p : rtd;

        /* pad if necessary */
          if (s == space_pure || s == space_impure) {
              iptr m = unaligned_size_record_inst(UNFIX(RECORDDESCSIZE(rtd)));
              if (m != n)
                  *((ptr *)((uptr)UNTYPE(p,type_typed_object) + m)) = FIX(0);
          }
      } else if (TYPEP(tf, mask_vector, type_vector)) {
          iptr len, n;
          len = Svector_length(pp);
          n = size_vector(len);
#ifdef ENABLE_OBJECT_COUNTS
          S_G.countof[newg][countof_vector] += 1;
          S_G.bytesof[newg][countof_vector] += n;
#endif /* ENABLE_OBJECT_COUNTS */
        /* assumes vector lengths look like fixnums; if not, vectors will need their own space */
          if ((uptr)tf & vector_immutable_flag) {
            find_room(space_pure, newg, type_typed_object, n, p);
          } else {
            find_room(space_impure, newg, type_typed_object, n, p);
          }
          copy_ptrs(type_typed_object, p, pp, n);
        /* pad if necessary */
          if ((len & 1) == 0) INITVECTIT(p, len) = FIX(0);
      } else if (TYPEP(tf, mask_string, type_string)) {
          iptr n;
          n = size_string(Sstring_length(pp));
#ifdef ENABLE_OBJECT_COUNTS
          S_G.countof[newg][countof_string] += 1;
          S_G.bytesof[newg][countof_string] += n;
#endif /* ENABLE_OBJECT_COUNTS */
          find_room(space_data, newg, type_typed_object, n, p);
          copy_ptrs(type_typed_object, p, pp, n);
      } else if (TYPEP(tf, mask_bytevector, type_bytevector)) {
          iptr n;
          n = size_bytevector(Sbytevector_length(pp));
#ifdef ENABLE_OBJECT_COUNTS
          S_G.countof[newg][countof_bytevector] += 1;
          S_G.bytesof[newg][countof_bytevector] += n;
#endif /* ENABLE_OBJECT_COUNTS */
          find_room(space_data, newg, type_typed_object, n, p);
          copy_ptrs(type_typed_object, p, pp, n);
      } else if ((iptr)tf == type_tlc) {
          ptr keyval, next;

#ifdef ENABLE_OBJECT_COUNTS
          S_G.countof[newg][countof_tlc] += 1;
#endif /* ENABLE_OBJECT_COUNTS */
          find_room(space_impure, newg, type_typed_object, size_tlc, p);
          TLCTYPE(p) = type_tlc;
          INITTLCKEYVAL(p) = keyval = TLCKEYVAL(pp);
          INITTLCHT(p) = TLCHT(pp);
          INITTLCNEXT(p) = next = TLCNEXT(pp);

        /* if next isn't false and keyval is old, add tlc to a list of tlcs
         * to process later.  determining if keyval is old is a (conservative)
         * approximation to determining if key is old.  we can't easily
         * determine if key is old, since keyval might or might not have been
         * swept already.  NB: assuming keyvals are always pairs. */
          if (next != Sfalse && SPACE(keyval) & space_old)
            tlcs_to_rehash = S_cons_in(space_new, 0, p, tlcs_to_rehash);
      } else if (TYPEP(tf, mask_box, type_box)) {
#ifdef ENABLE_OBJECT_COUNTS
          S_G.countof[newg][countof_box] += 1;
#endif /* ENABLE_OBJECT_COUNTS */
          if ((uptr)tf == type_immutable_box) {
            find_room(space_pure, newg, type_typed_object, size_box, p);
          } else {
            find_room(space_impure, newg, type_typed_object, size_box, p);
          }
          BOXTYPE(p) = (iptr)tf;
          INITBOXREF(p) = Sunbox(pp);
      } else if (TYPEP(tf, mask_fxvector, type_fxvector)) {
          iptr n;
          n = size_fxvector(Sfxvector_length(pp));
#ifdef ENABLE_OBJECT_COUNTS
          S_G.countof[newg][countof_fxvector] += 1;
          S_G.bytesof[newg][countof_fxvector] += n;
#endif /* ENABLE_OBJECT_COUNTS */
          find_room(space_data, newg, type_typed_object, n, p);
          copy_ptrs(type_typed_object, p, pp, n);
      } else if ((iptr)tf == type_ratnum) {
        /* not recursive: place in space_data and relocate fields immediately */
#ifdef ENABLE_OBJECT_COUNTS
          S_G.countof[newg][countof_ratnum] += 1;
#endif /* ENABLE_OBJECT_COUNTS */
          find_room(space_data, newg,
                      type_typed_object, size_ratnum, p);
          RATTYPE(p) = type_ratnum;
          RATNUM(p) = RATNUM(pp);
          RATDEN(p) = RATDEN(pp);
          relocate_pure(&RATNUM(p));
          relocate_pure(&RATDEN(p));
      } else if ((iptr)tf == type_exactnum) {
        /* not recursive: place in space_data and relocate fields immediately */
#ifdef ENABLE_OBJECT_COUNTS
          S_G.countof[newg][countof_exactnum] += 1;
#endif /* ENABLE_OBJECT_COUNTS */
          find_room(space_data, newg,
                      type_typed_object, size_exactnum, p);
          EXACTNUM_TYPE(p) = type_exactnum;
          EXACTNUM_REAL_PART(p) = EXACTNUM_REAL_PART(pp);
          EXACTNUM_IMAG_PART(p) = EXACTNUM_IMAG_PART(pp);
          relocate_pure(&EXACTNUM_REAL_PART(p));
          relocate_pure(&EXACTNUM_IMAG_PART(p));
      } else if ((iptr)tf == type_inexactnum) {
#ifdef ENABLE_OBJECT_COUNTS
          S_G.countof[newg][countof_inexactnum] += 1;
#endif /* ENABLE_OBJECT_COUNTS */
          find_room(space_data, newg,
                      type_typed_object, size_inexactnum, p);
          INEXACTNUM_TYPE(p) = type_inexactnum;
          INEXACTNUM_REAL_PART(p) = INEXACTNUM_REAL_PART(pp);
          INEXACTNUM_IMAG_PART(p) = INEXACTNUM_IMAG_PART(pp);
      } else if (TYPEP(tf, mask_bignum, type_bignum)) {
          iptr n;
          n = size_bignum(BIGLEN(pp));
#ifdef ENABLE_OBJECT_COUNTS
          S_G.countof[newg][countof_bignum] += 1;
          S_G.bytesof[newg][countof_bignum] += n;
#endif /* ENABLE_OBJECT_COUNTS */
          find_room(space_data, newg, type_typed_object, n, p);
          copy_ptrs(type_typed_object, p, pp, n);
      } else if (TYPEP(tf, mask_port, type_port)) {
#ifdef ENABLE_OBJECT_COUNTS
          S_G.countof[newg][countof_port] += 1;
#endif /* ENABLE_OBJECT_COUNTS */
          find_room(space_port, newg, type_typed_object, size_port, p);
          PORTTYPE(p) = PORTTYPE(pp);
          PORTHANDLER(p) = PORTHANDLER(pp);
          PORTNAME(p) = PORTNAME(pp);
          PORTINFO(p) = PORTINFO(pp);
          PORTOCNT(p) = PORTOCNT(pp);
          PORTICNT(p) = PORTICNT(pp);
          PORTOBUF(p) = PORTOBUF(pp);
          PORTOLAST(p) = PORTOLAST(pp);
          PORTIBUF(p) = PORTIBUF(pp);
          PORTILAST(p) = PORTILAST(pp);
      } else if (TYPEP(tf, mask_code, type_code)) {
          iptr n;
          n = size_code(CODELEN(pp));
#ifdef ENABLE_OBJECT_COUNTS
          S_G.countof[newg][countof_code] += 1;
          S_G.bytesof[newg][countof_code] += n;
#endif /* ENABLE_OBJECT_COUNTS */
          find_room(space_code, newg, type_typed_object, n, p);
          copy_ptrs(type_typed_object, p, pp, n);
      } else if ((iptr)tf == type_thread) {
#ifdef ENABLE_OBJECT_COUNTS
          S_G.countof[newg][countof_thread] += 1;
#endif /* ENABLE_OBJECT_COUNTS */
          find_room(space_pure_typed_object, newg,
                      type_typed_object, size_thread, p);
          TYPEFIELD(p) = (ptr)type_thread;
          THREADTC(p) = THREADTC(pp); /* static */
      } else if ((iptr)tf == type_rtd_counts) {
#ifdef ENABLE_OBJECT_COUNTS
          S_G.countof[newg][countof_rtd_counts] += 1;
#endif /* ENABLE_OBJECT_COUNTS */
          find_room(space_data, newg, type_typed_object, size_rtd_counts, p);
          copy_ptrs(type_typed_object, p, pp, size_rtd_counts);
      } else {
          S_error_abort("copy(gc): illegal type");
          return newg /* not reached */;
      }
    } else if (t == type_pair) {
      if (si->space == (space_ephemeron | space_old)) {
#ifdef ENABLE_OBJECT_COUNTS
        S_G.countof[newg][countof_ephemeron] += 1;
#endif /* ENABLE_OBJECT_COUNTS */
        find_room(space_ephemeron, newg, type_pair, size_ephemeron, p);
        INITCAR(p) = Scar(pp);
        INITCDR(p) = Scdr(pp);
      } else {
        ptr qq = Scdr(pp); ptr q;
        if (qq != pp && TYPEBITS(qq) == type_pair && ptr_get_segment(qq) == ptr_get_segment(pp) && FWDMARKER(qq) != forward_marker && !locked(qq)) {
          if (si->space == (space_weakpair | space_old)) {
#ifdef ENABLE_OBJECT_COUNTS
            S_G.countof[newg][countof_weakpair] += 2;
#endif /* ENABLE_OBJECT_COUNTS */
            find_room(space_weakpair, newg, type_pair, 2 * size_pair, p);
          } else {
#ifdef ENABLE_OBJECT_COUNTS
            S_G.countof[newg][countof_pair] += 2;
#endif /* ENABLE_OBJECT_COUNTS */
            find_room(space_impure, newg, type_pair, 2 * size_pair, p);
          }
          q = (ptr)((uptr)p + size_pair);
          INITCAR(p) = Scar(pp);
          INITCDR(p) = q;
          INITCAR(q) = Scar(qq);
          INITCDR(q) = Scdr(qq);
          FWDMARKER(qq) = forward_marker;
          FWDADDRESS(qq) = q;
        } else {
          if (si->space == (space_weakpair | space_old)) {
#ifdef ENABLE_OBJECT_COUNTS
            S_G.countof[newg][countof_weakpair] += 1;
#endif /* ENABLE_OBJECT_COUNTS */
            find_room(space_weakpair, newg, type_pair, size_pair, p);
          } else {
#ifdef ENABLE_OBJECT_COUNTS
            S_G.countof[newg][countof_pair] += 1;
#endif /* ENABLE_OBJECT_COUNTS */
            find_room(space_impure, newg, type_pair, size_pair, p);
          }
          INITCAR(p) = Scar(pp);
          INITCDR(p) = qq;
        }
      }
    } else if (t == type_closure) {
        ptr code;

      /* relocate before accessing code type field, which otherwise might
         be a forwarding marker */
        code = CLOSCODE(pp);
        relocate_pure(&code);
        if (CODETYPE(code) & (code_flag_continuation << code_flags_offset)) {
#ifdef ENABLE_OBJECT_COUNTS
            S_G.countof[newg][countof_continuation] += 1;
#endif /* ENABLE_OBJECT_COUNTS */
            find_room(space_continuation, newg,
                        type_closure, size_continuation, p);
            SETCLOSCODE(p,code);
          /* don't promote one-shots */
            CONTLENGTH(p) = CONTLENGTH(pp);
            CONTCLENGTH(p) = CONTCLENGTH(pp);
            CONTWINDERS(p) = CONTWINDERS(pp);
            if (CONTLENGTH(p) != scaled_shot_1_shot_flag) {
                CONTLINK(p) = CONTLINK(pp);
                CONTRET(p) = CONTRET(pp);
                CONTSTACK(p) = CONTSTACK(pp);
            }
        } else {
            iptr len, n;
            len = CLOSLEN(pp);
            n = size_closure(len);
#ifdef ENABLE_OBJECT_COUNTS
            S_G.countof[newg][countof_closure] += 1;
            S_G.bytesof[newg][countof_closure] += n;
#endif /* ENABLE_OBJECT_COUNTS */
            find_room(space_pure, newg, type_closure, n, p);
            copy_ptrs(type_closure, p, pp, n);
            SETCLOSCODE(p,code);
         /* pad if necessary */
            if ((len & 1) == 0) CLOSIT(p, len) = FIX(0);
        }
    } else if (t == type_symbol) {
#ifdef ENABLE_OBJECT_COUNTS
        S_G.countof[newg][countof_symbol] += 1;
#endif /* ENABLE_OBJECT_COUNTS */
        find_room(space_symbol, newg, type_symbol, size_symbol, p);
        INITSYMVAL(p) = SYMVAL(pp);
        INITSYMPVAL(p) = SYMPVAL(pp);
        INITSYMPLIST(p) = SYMPLIST(pp);
        INITSYMSPLIST(p) = SYMSPLIST(pp);
        INITSYMNAME(p) = SYMNAME(pp);
        INITSYMHASH(p) = SYMHASH(pp);
    } else if (t == type_flonum) {
#ifdef ENABLE_OBJECT_COUNTS
        S_G.countof[newg][countof_flonum] += 1;
#endif /* ENABLE_OBJECT_COUNTS */
        find_room(space_data, newg, type_flonum, size_flonum, p);
        FLODAT(p) = FLODAT(pp);
      /* no room for forwarding address, so let 'em be duplicated */
        *ppp = p;
        return newg;
    } else {
      S_error_abort("copy(gc): illegal type");
      return newg /* not reached */;
    }

    FWDMARKER(pp) = forward_marker;
    FWDADDRESS(pp) = p;

    *ppp = p;
    return newg;
}

static void sweep_locked_ptrs(ptr *pp, iptr n FORMAL_CTGS) {
  ptr *end = pp + n;

  while (pp != end) {
    relocate_pure(pp);
    pp += 1;
  }
}

static void sweep_locked(ptr tc, ptr p, IBOOL sweep_pure FORMAL_CTGS) {
  ptr tf; ITYPE t;

  if ((t = TYPEBITS(p)) == type_pair) {
    ISPC s = SPACE(p) & ~(space_locked | space_old);
    if (s == space_ephemeron)
      add_ephemeron_to_pending(p);
    else {
      if (s != space_weakpair) {
        relocate_pure(&INITCAR(p));
      }
      relocate_pure(&INITCDR(p));
    }
  } else if (t == type_closure) {
    if (sweep_pure) {
      ptr code;

      code = CLOSCODE(p);
      relocate_pure(&code);
      SETCLOSCODE(p,code);
      if (CODETYPE(code) & (code_flag_continuation << code_flags_offset))
        sweep_continuation(p ACTUAL_CTGS);
      else
        sweep_locked_ptrs(&CLOSIT(p, 0), CLOSLEN(p) ACTUAL_CTGS);
    }
  } else if (t == type_symbol) {
    sweep_symbol(p, 0 ACTUAL_CTGS);
  } else if (t == type_flonum) {
    /* nothing to sweep */;
 /* typed objects */
  } else if (tf = TYPEFIELD(p), TYPEP(tf, mask_vector, type_vector)) {
    sweep_locked_ptrs(&INITVECTIT(p, 0), Svector_length(p) ACTUAL_CTGS);
  } else if (TYPEP(tf, mask_string, type_string) || TYPEP(tf, mask_bytevector, type_bytevector) || TYPEP(tf, mask_fxvector, type_fxvector)) {
    /* nothing to sweep */;
  } else if (TYPEP(tf, mask_record, type_record)) {
    relocate_pure(&RECORDINSTTYPE(p));
    if (sweep_pure || RECORDDESCMPM(RECORDINSTTYPE(p)) != FIX(0)) {
      sweep_record(p, 0 ACTUAL_CTGS);
    }
  } else if (TYPEP(tf, mask_box, type_box)) {
    relocate_pure(&INITBOXREF(p));
  } else if ((iptr)tf == type_ratnum) {
    if (sweep_pure) {
      relocate_pure(&RATNUM(p));
      relocate_pure(&RATDEN(p));
    }
  } else if ((iptr)tf == type_exactnum) {
    if (sweep_pure) {
      relocate_pure(&EXACTNUM_REAL_PART(p));
      relocate_pure(&EXACTNUM_IMAG_PART(p));
    }
  } else if ((iptr)tf == type_inexactnum) {
    /* nothing to sweep */;
  } else if (TYPEP(tf, mask_bignum, type_bignum)) {
    /* nothing to sweep */;
  } else if (TYPEP(tf, mask_port, type_port)) {
    sweep_port(p, 0 ACTUAL_CTGS);
  } else if (TYPEP(tf, mask_code, type_code)) {
    if (sweep_pure) {
      sweep_code_object(tc, p ACTUAL_CTGS);
    }
  } else if ((iptr)tf == type_thread) {
    sweep_thread(p ACTUAL_CTGS);
  } else if ((iptr)tf == type_rtd_counts) {
    /* nothing to sweep */;
  } else {
    S_error_abort("sweep_locked(gc): illegal type");
  }
}

static ptr copy_stack(ptr old, iptr *length, iptr clength FORMAL_CTGS) {
  iptr n, m; ptr new; IGEN newg;

  /* Don't copy non-oldspace stacks, since we may be sweeping a locked
     continuation.  Doing so would be a waste of work anyway. */
  if (!OLDSPACE(old)) return old;

  newg = compute_target_generation(GENERATION(old) ACTUAL_CTGS);

  /* reduce headroom created for excessively large frames (typically resulting from apply with long lists) */
  if ((n = *length) != clength && n > default_stack_size && n > (m = clength + one_shot_headroom)) {
    *length = n = m;
  }

  n = ptr_align(n);
#ifdef ENABLE_OBJECT_COUNTS
  S_G.countof[newg][countof_stack] += 1;
  S_G.bytesof[newg][countof_stack] += n;
#endif /* ENABLE_OBJECT_COUNTS */
  find_room(space_data, newg, typemod, n, new);
  n = ptr_align(clength);
 /* warning: stack may have been left non-double-aligned by split_and_resize */
  copy_ptrs(typemod, new, old, n);

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
      if (!(si->space & space_old) || locked(obj) || ((FWDMARKER(obj) == forward_marker && TYPEBITS(obj) != type_flonum) && (INITGUARDIANOBJ(ls) = FWDADDRESS(obj), 1))) { \
        INITGUARDIANNEXT(ls) = pend_hold_ls; \
        pend_hold_ls = ls; \
      } else { \
        tconc = GUARDIANTCONC(ls); \
        if (!OLDSPACE(tconc) || locked(tconc) || ((FWDMARKER(tconc) == forward_marker) && (INITGUARDIANTCONC(ls) = FWDADDRESS(tconc), 1))) { \
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

/* tc: thread context
 * max_cg: maximum copied generation, i.e., maximum generation subject to collection.  max_cg >= 0 && max_cg <= 255.
 * min_tg: minimum target generation.  max_tg == 0 ? min_tg == 0 : min_tg > 0 && min_tg <= max_tg;
 * max_tg: maximum target generation.  max_tg == max_cg || max_tg == max_cg + 1.
 * Objects in generation g are collected into generation MIN(max_tg, MAX(min_tg, g+1)).
 */
void GCENTRY GCENTRY_PROTO(ptr tc, IGEN max_cg, IGEN min_tg, IGEN max_tg) {
    IGEN g; ISPC s;
    seginfo *oldspacesegments, *si, *nextsi;
    ptr ls;
    bucket_pointer_list *buckets_to_rebuild;
#ifndef NO_LOCKED_OLDSPACE_OBJECTS
    ptr locked_oldspace_objects;
#endif /* !NO_LOCKED_OLDSPACE_OBJECTS */
    DECLARE_CTGS(max_cg, min_tg, max_tg);

   /* flush instruction cache: effectively clear_code_mod but safer */
    for (ls = S_threads; ls != Snil; ls = Scdr(ls)) {
      ptr tc = (ptr)THREADTC(Scar(ls));
      S_flush_instruction_cache(tc);
    }

    tlcs_to_rehash = Snil;
#ifndef NO_DIRTY_NEWSPACE_POINTERS
    new_dirty_cards = NULL;
#endif /* !NO_DIRTY_NEWSPACE_POINTERS */

    for (ls = S_threads; ls != Snil; ls = Scdr(ls)) {
      ptr tc = (ptr)THREADTC(Scar(ls));
      S_scan_dirty((ptr **)EAP(tc), (ptr **)REAL_EAP(tc));
      EAP(tc) = REAL_EAP(tc) = AP(tc) = (ptr)0;
    }

   /* perform after ScanDirty */
    if (S_checkheap) S_check_heap(0);

#ifdef DEBUG
(void)printf("max_cg = %x;  go? ", max_cg); (void)fflush(stdout); (void)getc(stdin);
#endif

  /* set up generations to be copied */
    for (g = 0; g <= max_cg; g++) {
      S_G.bytes_of_generation[g] = 0;
      for (s = 0; s <= max_real_space; s++) {
        S_G.base_loc[g][s] = FIX(0);
        S_G.first_loc[g][s] = FIX(0);
        S_G.next_loc[g][s] = FIX(0);
        S_G.bytes_left[g][s] = 0;
        S_G.bytes_of_space[g][s] = 0;
      }
    }

  /* set up target generation sweep_loc and orig_next_loc pointers */
    for (g = min_tg; g <= max_tg; g += 1) {
      for (s = 0; s <= max_real_space; s++) {
        /* for all but max_tg (and max_tg as well, if max_tg == max_cg), this
           will set orig_net_loc and sweep_loc to 0 */
        orig_next_loc[g][s] = sweep_loc[g][s] = S_G.next_loc[g][s];
      }
    }

  /* mark segments from which objects are to be copied */
    oldspacesegments = (seginfo *)NULL;
    for (g = 0; g <= max_cg; g += 1) {
      for (s = 0; s <= max_real_space; s += 1) {
        for (si = S_G.occupied_segments[g][s]; si != NULL; si = nextsi) {
          nextsi = si->next;
          si->next = oldspacesegments;
          oldspacesegments = si;
          si->space = s | space_old; /* NB: implicitly clearing space_locked */
        }
        S_G.occupied_segments[g][s] = NULL;
      }
    }

#ifdef ENABLE_OBJECT_COUNTS
   /* clear object counts & bytes for copied generations; bump timestamp */
   {INT i;
    for (g = 0; g <= max_cg; g += 1) {
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

    /* pre-collection handling of locked objects. */

#ifndef NO_LOCKED_OLDSPACE_OBJECTS
    /* create a single sorted_locked_object vector for all copied generations
     * to accelerate the search for locked objects in copy().  copy wants
     * a vector of some size n=2^k-1 so it doesn't have to check bounds */
    ls = Snil;
    /* note: append_bang and dosort reuse pairs, which can result in older
     * objects pointing to newer ones...but we don't care since they are all
     * oldspace and going away after this collection. */
    for (g = 0; g <= max_cg; g += 1) {
      ls = append_bang(S_G.locked_objects[g], ls);
      S_G.locked_objects[g] = Snil;
      S_G.unlocked_objects[g] = Snil;
    }
    if (ls == Snil) {
      sorted_locked_objects = FIX(0);
      locked_oldspace_objects = Snil;
    } else {
      ptr v, x, y; uptr i, n;

      /* dosort is destructive, so have to store the result back */
      locked_oldspace_objects = ls = dosort(ls, list_length(ls));

      /* create vector of smallest size n=2^k-1 that will fit all of
         the list's unique elements */
      i = count_unique(ls);
      for (n = 1; n < i; n = (n << 1) | 1);
      sorted_locked_objects = v = S_vector_in(space_new, 0, n);

      /* copy list elements in, skipping duplicates */
      INITVECTIT(v,0) = x = Scar(ls);
      i = 1;
      while ((ls = Scdr(ls)) != Snil) {
        if ((y = Scar(ls)) != x) {
          INITVECTIT(v, i) = x = y;
          i += 1;
        }
      }

      /* fill remaining slots with largest ptr value */
      while (i < n) { INITVECTIT(v, i) = MAXPTR; i += 1; }
    }
#endif /* !NO_LOCKED_OLDSPACE_OBJECTS */

    /* sweep older locked and unlocked objects */
    for (g = max_cg + 1; g <= static_generation; INCRGEN(g)) {
      for (ls = S_G.locked_objects[g]; ls != Snil; ls = Scdr(ls))
        sweep_locked(tc, Scar(ls), 0 ACTUAL_CTGS);
      for (ls = S_G.unlocked_objects[g]; ls != Snil; ls = Scdr(ls))
        sweep_locked(tc, Scar(ls), 0 ACTUAL_CTGS);
    }

#ifndef NO_LOCKED_OLDSPACE_OBJECTS
    /* sweep younger locked objects, working from sorted vector to avoid redundant sweeping of duplicates */
    if (sorted_locked_objects != FIX(0)) {
      uptr i; ptr x, v, *vp;
      v = sorted_locked_objects;
      i = Svector_length(v);
      x = *(vp = &INITVECTIT(v, 0));
      do sweep_locked(tc, x, 1 ACTUAL_CTGS); while (--i != 0 && (x = *++vp) != MAXPTR);
    }
#endif /* !NO_LOCKED_OLDSPACE_OBJECTS */

  /* sweep non-oldspace threads, since any thread may have an active stack */
    for (ls = S_threads; ls != Snil; ls = Scdr(ls)) {
      ptr thread;

    /* someone may have their paws on the list */
      if (FWDMARKER(ls) == forward_marker) ls = FWDADDRESS(ls);

      thread = Scar(ls);
      if (!OLDSPACE(thread)) sweep_thread(thread ACTUAL_CTGS);
    }
    relocate_pure(&S_threads);

  /* relocate nonempty oldspace symbols and set up list of buckets to rebuild later */
    buckets_to_rebuild = NULL;
    for (g = 0; g <= max_cg; g += 1) {
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
            (SYMVAL(sym) != sunbound || SYMPLIST(sym) != Snil || SYMSPLIST(sym) != Snil)) {
          ptr ignore;
          copy(sym, SegInfo(ptr_get_segment(sym)), &ignore ACTUAL_CTGS);
        }
      }
      S_G.buckets_of_generation[g] = NULL;
    }

  /* relocate the protected C pointers */
    {uptr i;
     for (i = 0; i < S_G.protect_next; i++)
         relocate_pure(S_G.protected[i]);
    }

  /* sweep areas marked dirty by assignments into older generations */
    sweep_dirty(ONLY_ACTUAL_CTGS);

    sweep_generation(tc ACTUAL_CTGS);

  /* handle guardians */
    {   ptr pend_hold_ls, final_ls, pend_final_ls;
        ptr obj, rep, tconc, next;

      /* move each entry in guardian lists into one of:
       *   pend_hold_ls     if obj accessible
       *   final_ls         if obj not accessible and tconc accessible
       *   pend_final_ls    if obj not accessible and tconc not accessible */
        pend_hold_ls = final_ls = pend_final_ls = Snil;

        for (ls = S_threads; ls != Snil; ls = Scdr(ls)) {
          ptr tc = (ptr)THREADTC(Scar(ls));
          partition_guardians(GUARDIANENTRIES(tc), NONSTATICINHEAP);
          GUARDIANENTRIES(tc) = Snil;
        }

        for (g = 0; g <= max_cg; g += 1) {
          partition_guardians(S_G.guardians[g], ALWAYSTRUE);
          S_G.guardians[g] = Snil;
        }

       /* invariants after partition_guardians:
        * for entry in pend_hold_ls, obj is !OLDSPACE or locked
        * for entry in final_ls, obj is OLDSPACE and !locked
        * for entry in final_ls, tconc is !OLDSPACE or locked
        * for entry in pend_final_ls, obj and tconc are OLDSPACE and !locked
        */

        while (1) {
            IBOOL relocate_rep = final_ls != Snil;

          /* relocate & add the final objects to their tconcs */
            for (ls = final_ls; ls != Snil; ls = GUARDIANNEXT(ls)) {
                ptr old_end, new_end;

                rep = GUARDIANREP(ls);
              /* ftype_guardian_rep is a marker for reference-counted ftype pointer */
                if (rep == ftype_guardian_rep) {
                  INT b; uptr *addr;
                  rep = GUARDIANOBJ(ls);
                  if (FWDMARKER(rep) == forward_marker) rep = FWDADDRESS(rep);
                /* Caution: Building in assumption about shape of an ftype pointer */
                  addr = RECORDINSTIT(rep, 0);
                  LOCKED_DECR(addr, b);
                  if (!b) continue;
                }

                relocate_pure(&rep);

              /* if tconc was old it's been forwarded */
                tconc = GUARDIANTCONC(ls);

                old_end = Scdr(tconc);
                new_end = S_cons_in(space_impure, 0, FIX(0), FIX(0));
#ifdef ENABLE_OBJECT_COUNTS
                S_G.countof[0][countof_pair] += 1;
#endif /* ENABLE_OBJECT_COUNTS */

                SETCAR(old_end,rep);
                SETCDR(old_end,new_end);
                SETCDR(tconc,new_end);
            }

            /* copy each entry in pend_hold_ls into its target generation if tconc accessible */
            ls = pend_hold_ls; pend_hold_ls = Snil;
            for ( ; ls != Snil; ls = next) {
                ptr p;

                next = GUARDIANNEXT(ls);

                /* discard static pend_hold_ls entries */
                g = compute_target_generation(GENERATION(ls) ACTUAL_CTGS);
                if (g == static_generation) continue;

                tconc = GUARDIANTCONC(ls);
        
                if (OLDSPACE(tconc) && !locked(tconc)) {
                    if (FWDMARKER(tconc) == forward_marker)
                        tconc = FWDADDRESS(tconc);
                    else {
                        INITGUARDIANNEXT(ls) = pend_hold_ls;
                        pend_hold_ls = ls;
                        continue;
                    }
                }
    
                rep = GUARDIANREP(ls);
                relocate_pure(&rep);
                relocate_rep = 1;

#ifdef ENABLE_OBJECT_COUNTS
                S_G.countof[g][countof_guardian] += 1;
#endif /* ENABLE_OBJECT_COUNTS */
                find_room(space_pure, g, typemod, size_guardian_entry, p);
                INITGUARDIANOBJ(p) = GUARDIANOBJ(ls);
                INITGUARDIANREP(p) = rep;
                INITGUARDIANTCONC(p) = tconc;
                INITGUARDIANNEXT(p) = S_G.guardians[g];
                S_G.guardians[g] = p;
            }

            if (!relocate_rep) break;

            sweep_generation(tc ACTUAL_CTGS);

          /* move each entry in pend_final_ls into one of:
           *   final_ls         if tconc forwarded
           *   pend_final_ls    if tconc not forwarded */
            ls = pend_final_ls; final_ls = pend_final_ls = Snil;
            for ( ; ls != Snil; ls = next) {
                tconc = GUARDIANTCONC(ls); next = GUARDIANNEXT(ls);

                if (FWDMARKER(tconc) == forward_marker) {
                    INITGUARDIANTCONC(ls) = FWDADDRESS(tconc);
                    INITGUARDIANNEXT(ls) = final_ls;
                    final_ls = ls;
                } else {
                    INITGUARDIANNEXT(ls) = pend_final_ls;
                    pend_final_ls = ls;
                }
            }
        }
    }

  /* handle weak pairs */
    resweep_dirty_weak_pairs(ONLY_ACTUAL_CTGS);
    resweep_weak_pairs(ONLY_ACTUAL_CTGS);

   /* still-pending ephemerons all go to bwp */
    clear_trigger_ephemerons();

   /* forward car fields of locked and unlocked older weak pairs */
    for (g = max_cg + 1; g <= static_generation; INCRGEN(g)) {
      for (ls = S_G.locked_objects[g]; ls != Snil; ls = Scdr(ls)) {
        ptr x = Scar(ls);
        if (Spairp(x) && (SPACE(x) & ~(space_old|space_locked)) == space_weakpair)
          forward_or_bwp(&INITCAR(x), Scar(x));
      }
      for (ls = S_G.unlocked_objects[g]; ls != Snil; ls = Scdr(ls)) {
        ptr x = Scar(ls);
        if (Spairp(x) && (SPACE(x) & ~(space_old|space_locked)) == space_weakpair)
          forward_or_bwp(&INITCAR(x), Scar(x));
      }
    }

#ifndef NO_LOCKED_OLDSPACE_OBJECTS
   /* forward car fields of locked oldspace weak pairs */
    if (sorted_locked_objects != FIX(0)) {
      uptr i; ptr x, v, *vp;
      v = sorted_locked_objects;
      i = Svector_length(v);
      x = *(vp = &INITVECTIT(v, 0));
      do {
        if (Spairp(x) && (SPACE(x) & ~(space_old|space_locked)) == space_weakpair) {
          forward_or_bwp(&INITCAR(x), Scar(x));
        }
      } while (--i != 0 && (x = *++vp) != MAXPTR);
    }
#endif /* !NO_LOCKED_OLDSPACE_OBJECTS */

   /* post-gc oblist handling.  rebuild old buckets in the target generation, pruning unforwarded symbols */
    { bucket_list *bl; bucket *b, *bnext; bucket_pointer_list *bpl; bucket **pb; ptr sym;
      for (bpl = buckets_to_rebuild; bpl != NULL; bpl = bpl->cdr) {
        pb = bpl->car;
        for (b = (bucket *)((uptr)*pb - 1); b != NULL && ((uptr)(b->next) & 1); b = bnext) {
          bnext = (bucket *)((uptr)(b->next) - 1);
          sym = b->sym;
          if (locked(sym) || (FWDMARKER(sym) == forward_marker && ((sym = FWDADDRESS(sym)) || 1))) {
            IGEN g = GENERATION(sym);
            find_room(space_data, g, typemod, sizeof(bucket), b);
#ifdef ENABLE_OBJECT_COUNTS
            S_G.countof[g][countof_oblist] += 1;
            S_G.bytesof[g][countof_oblist] += sizeof(bucket);
#endif /* ENABLE_OBJECT_COUNTS */
            b->sym = sym;
            *pb = b;
            pb = &b->next;
            if (g != static_generation) {
              find_room(space_data, g, typemod, sizeof(bucket_list), bl);
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
    { IGEN g, newg; ptr ls, lsls, p;
      for (g = 0, lsls = Snil; g <= max_cg; g += 1) {
        lsls = S_cons_in(space_new, 0, S_G.rtds_with_counts[g], lsls);
        S_G.rtds_with_counts[g] = Snil;
      }
      for (; lsls != Snil; lsls = Scdr(lsls)) {
        for (ls = Scar(lsls); ls != Snil; ls = Scdr(ls)) {
          p = Scar(ls);
          if (!OLDSPACE(p) || locked(p) || (FWDMARKER(p) == forward_marker && (p = FWDADDRESS(p), 1))) {
            newg = GENERATION(p);
#ifdef ENABLE_OBJECT_COUNTS
            S_G.countof[newg][countof_pair] += 1;
#endif /* ENABLE_OBJECT_COUNTS */
            S_G.rtds_with_counts[newg] = S_cons_in(space_impure, newg, p, S_G.rtds_with_counts[newg]);
          }
        }
      }
    }

#ifndef WIN32
  /* rebuild child_process list, reaping any that have died and refusing
     to promote into the static generation. */
    { IGEN g, newg; ptr ls, newls;
      for (g = max_cg; g >= 0; g -= 1) {
        newg = compute_target_generation(g ACTUAL_CTGS);
        if (newg == static_generation) newg = S_G.max_nonstatic_generation;
        newls = newg == g ? Snil : S_child_processes[newg];
        for (ls = S_child_processes[g], S_child_processes[g] = Snil; ls != Snil; ls = Scdr(ls)) {
          INT pid = UNFIX(Scar(ls)), status, retpid;
          retpid = waitpid(pid, &status, WNOHANG);
          if (retpid == 0 || (retpid == pid && !(WIFEXITED(status) || WIFSIGNALED(status)))) {
            newls = S_cons_in(space_impure, newg, FIX(pid), newls);
#ifdef ENABLE_OBJECT_COUNTS
            S_G.countof[newg][countof_pair] += 1;
#endif /* ENABLE_OBJECT_COUNTS */
          }
        }
        S_child_processes[newg] = newls;
      }
    }
#endif /* WIN32 */

#ifndef NO_LOCKED_OLDSPACE_OBJECTS
   /* post-collection handling of locked objects.  This must come after
      any use of relocate or any other use of sorted_locked_objects */
    if (sorted_locked_objects != FIX(0)) {
      ptr ls, x, v, *vp; iptr i; uptr last_seg = 0, addr, seg, n; IGEN newg = 0;

      v = sorted_locked_objects;

      /* work from sorted vector to avoid redundant processing of duplicates */
      i = Svector_length(v);
      x = *(vp = &INITVECTIT(v, 0));
      do {
        /* promote the segment(s) containing x to the target generation.
           reset the space_old bit to prevent the segments from being
           reclaimed; set the locked bit to prevent sweeping by
           sweep_dirty (since the segments may contain a mix of objects,
           many of which have been discarded). */
        addr = (uptr)UNTYPE_ANY(x);
        if ((seg = addr_get_segment(addr)) == last_seg) {
          /* the generation has already been updated on this segment, and newg is still valid.
             this isn't just an optimization.  if we recompute newg based on the already updated
             generation, we could get the wrong result.  good thing the vector is sorted. */
          seg += 1;
        } else {
          newg = compute_target_generation(GENERATION(x) ACTUAL_CTGS);
        }

        n = size_object(x);

#ifdef ENABLE_OBJECT_COUNTS
        S_G.countof[newg][countof_locked] += 1;
        S_G.bytesof[newg][countof_locked] += n;
#endif /* ENABLE_OBJECT_COUNTS */

        addr += n - 1;
        last_seg = addr_get_segment(addr);
        while (seg <= last_seg) {
          seginfo *si = SegInfo(seg);
          si->generation = newg;
          si->space = (si->space & ~space_old) | space_locked;
          seg += 1;
        }
      } while (--i != 0 && (x = *++vp) != MAXPTR);

      /* add every object, including duplicates, to target-generation list(s).  we do so
         even when newg == static_generation so we can keep track of static objects that need to
         be swept at the start of collection.  (we could weed out pure static objects.) */
      for (newg = min_tg; newg < max_tg; newg += 1) S_G.locked_objects[newg] = Snil;
      if (max_tg == max_cg) S_G.locked_objects[max_cg] = Snil;
      for (ls = locked_oldspace_objects; ls != Snil; ls = Scdr(ls)) {
        x = Scar(ls);
        newg = GENERATION(x);
        S_G.locked_objects[newg] = S_cons_in(space_impure, newg, x, S_G.locked_objects[newg]);
#ifdef ENABLE_OBJECT_COUNTS
        S_G.countof[newg][countof_pair] += 1;
#endif /* ENABLE_OBJECT_COUNTS */
      }
    }
#endif /* !NO_LOCKED_OLDSPACE_OBJECTS */

  /* move old space segments to empty space */
    for (si = oldspacesegments; si != NULL; si = nextsi) {
      nextsi = si->next;
      s = si->space;
      if (s & space_locked) {
        /* note: the oldspace bit is cleared above for locked objects */
        s &= ~space_locked;
        g = si->generation;
        if (g == static_generation) S_G.number_of_nonstatic_segments -= 1;
        si->next = S_G.occupied_segments[g][s];
        S_G.occupied_segments[g][s] = si;
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

    S_G.g0_bytes_after_last_gc = S_G.bytes_of_generation[0];

    if (max_cg >= S_G.min_free_gen) S_free_chunks();

    S_flush_instruction_cache(tc);

#ifndef NO_DIRTY_NEWSPACE_POINTERS
    /* mark dirty those newspace cards to which we've added wrong-way pointers */
    { dirtycardinfo *ndc;
      for (ndc = new_dirty_cards; ndc != NULL; ndc = ndc->next)
        S_mark_card_dirty(ndc->card, ndc->youngest);
    }
#endif /* !NO_DIRTY_NEWSPACE_POINTERS */

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

    S_resize_oblist();

    /* tell profile_release_counters to look for bwp'd counters at least through max_tg */
    if (S_G.prcgeneration < max_tg) S_G.prcgeneration = max_tg;
}

#define sweep_space(s, from_g, body) {\
  slp = &sweep_loc[from_g][s];\
  nlp = &S_G.next_loc[from_g][s];\
  if (*slp == 0) *slp = S_G.first_loc[from_g][s];\
  pp = (ptr *)*slp;\
  while (pp != (nl = (ptr *)*nlp))\
      do\
          if ((p = *pp) == forward_marker)\
              pp = (ptr *)*(pp + 1);\
          else\
              body\
      while (pp != nl);\
  *slp = (ptr)pp; \
}

static void resweep_weak_pairs(ONLY_FORMAL_CTGS) {
    IGEN from_g; ptr *slp, *nlp; ptr *pp, p, *nl;

    for (from_g = MIN_TG; from_g <= MAX_TG; from_g += 1) {
      sweep_loc[from_g][space_weakpair] = orig_next_loc[from_g][space_weakpair];
      sweep_space(space_weakpair, from_g, {
          forward_or_bwp(pp, p);
          pp += 2;
      })
    }
}

static void forward_or_bwp(ptr *pp, ptr p) {
  seginfo *si;
 /* adapted from relocate */
  if (!IMMEDIATE(p) && (si = MaybeSegInfo(ptr_get_segment(p))) != NULL && si->space & space_old && !locked(p)) {
    if (FWDMARKER(p) == forward_marker && TYPEBITS(p) != type_flonum) {
      *pp = FWDADDRESS(p);
    } else {
      *pp = Sbwp_object;
    }
  }
}

static void sweep_generation(ptr tc FORMAL_CTGS) {
  IGEN from_g; ptr *slp, *nlp; ptr *pp, p, *nl;

  do {
    change = 0;
    for (from_g = MIN_TG; from_g <= MAX_TG; from_g += 1) {
      sweep_space(space_impure, from_g, {
          relocate_impure_help(pp, p, from_g);
          p = *(pp += 1);
          relocate_impure_help(pp, p, from_g);
          pp += 1;
      })

      sweep_space(space_symbol, from_g, {
          p = TYPE((ptr)pp, type_symbol);
          sweep_symbol(p, from_g ACTUAL_CTGS);
          pp += size_symbol / sizeof(ptr);
      })
  
      sweep_space(space_port, from_g, {
          p = TYPE((ptr)pp, type_typed_object);
          sweep_port(p, from_g ACTUAL_CTGS);
          pp += size_port / sizeof(ptr);
      })
  
      sweep_space(space_weakpair, from_g, {
          p = *(pp += 1);
          relocate_impure_help(pp, p, from_g);
          pp += 1;
      })
  
      sweep_space(space_ephemeron, from_g, {
          p = TYPE((ptr)pp, type_pair);
          add_ephemeron_to_pending(p);
          pp += size_ephemeron / sizeof(ptr);
      })
        
      sweep_space(space_pure, from_g, {
          relocate_pure_help(pp, p);
          p = *(pp += 1);
          relocate_pure_help(pp, p);
          pp += 1;
      })
  
      sweep_space(space_continuation, from_g, {
          p = TYPE((ptr)pp, type_closure);
          sweep_continuation(p ACTUAL_CTGS);
          pp += size_continuation / sizeof(ptr);
      })
  
      sweep_space(space_pure_typed_object, from_g, {
          p = TYPE((ptr)pp, type_typed_object);
          pp = (ptr *)((uptr)pp + sweep_typed_object(p, from_g ACTUAL_CTGS));
      })

      sweep_space(space_code, from_g, {
          p = TYPE((ptr)pp, type_typed_object);
          sweep_code_object(tc, p ACTUAL_CTGS);
          pp += size_code(CODELEN(p)) / sizeof(ptr);
      })
  
      sweep_space(space_impure_record, from_g, {
          p = TYPE((ptr)pp, type_typed_object);
          sweep_record(p, from_g ACTUAL_CTGS);
          pp = (ptr *)((iptr)pp +
                 size_record_inst(UNFIX(RECORDDESCSIZE(RECORDINSTTYPE(p)))));
      })
    }

    /* Waiting until sweeping doesn't trigger a change reduces the
       chance that an ephemeron must be registered as a
       segment-specific trigger or gets triggered for recheck, but
       it doesn't change the worst-case complexity. */
    if (!change)
      check_pending_ephemerons(ONLY_ACTUAL_CTGS);
  } while (change);
}

#ifndef NO_LOCKED_OLDSPACE_OBJECTS
static iptr size_object(ptr p) {
    ITYPE t; ptr tf;

    if ((t = TYPEBITS(p)) == type_pair) {
        seginfo *si;
        if ((si = MaybeSegInfo(ptr_get_segment(p))) != NULL && (si->space & ~(space_locked | space_old)) == space_ephemeron)
            return size_ephemeron;
        else
            return size_pair;
    } else if (t == type_closure) {
        ptr code = CLOSCODE(p);
        if (CODETYPE(code) & (code_flag_continuation << code_flags_offset))
            return size_continuation;
        else
            return size_closure(CLOSLEN(p));
    } else if (t == type_symbol) {
        return size_symbol;
    } else if (t == type_flonum) {
        return size_flonum;
  /* typed objects */
    } else if (tf = TYPEFIELD(p), TYPEP(tf, mask_vector, type_vector)) {
        return size_vector(Svector_length(p));
    } else if (TYPEP(tf, mask_string, type_string)) {
        return size_string(Sstring_length(p));
    } else if (TYPEP(tf, mask_bytevector, type_bytevector)) {
        return size_bytevector(Sbytevector_length(p));
    } else if (TYPEP(tf, mask_record, type_record)) {
        return size_record_inst(UNFIX(RECORDDESCSIZE(tf)));
    } else if (TYPEP(tf, mask_fxvector, type_fxvector)) {
        return size_fxvector(Sfxvector_length(p));
    } else if (TYPEP(tf, mask_box, type_box)) {
        return size_box;
    } else if ((iptr)tf == type_ratnum) {
        return size_ratnum;
    } else if ((iptr)tf == type_exactnum) {
        return size_exactnum;
    } else if ((iptr)tf == type_inexactnum) {
        return size_inexactnum;
    } else if (TYPEP(tf, mask_bignum, type_bignum)) {
        return size_bignum(BIGLEN(p));
    } else if (TYPEP(tf, mask_port, type_port)) {
        return size_port;
    } else if (TYPEP(tf, mask_code, type_code)) {
        return size_code(CODELEN(p));
    } else if ((iptr)tf == type_thread) {
        return size_thread;
    } else if ((iptr)tf == type_rtd_counts) {
        return size_rtd_counts;
    } else {
        S_error_abort("size_object(gc): illegal type");
        return 0 /* not reached */;
    }
}
#endif /* !NO_LOCKED_OLDSPACE_OBJECTS */

static iptr sweep_typed_object(ptr p, IGEN from_g FORMAL_CTGS) {
  ptr tf = TYPEFIELD(p);

  if (TYPEP(tf, mask_record, type_record)) {
    sweep_record(p, from_g ACTUAL_CTGS);
    return size_record_inst(UNFIX(RECORDDESCSIZE(RECORDINSTTYPE(p))));
  } else if (TYPEP(tf, mask_thread, type_thread)) {
    sweep_thread(p ACTUAL_CTGS);
    return size_thread;
  } else {
      S_error_abort("sweep_typed_object(gc): unexpected type");
      return 0 /* not reached */;
  }
}

static void sweep_symbol(ptr p, IGEN from_g FORMAL_CTGS) {
  ptr val, code;

  relocate_impure(&INITSYMVAL(p), from_g);
  val = SYMVAL(p);
  code = Sprocedurep(val) ? CLOSCODE(val) : SYMCODE(p);
  relocate_pure(&code);
  INITSYMCODE(p,code);
  relocate_impure(&INITSYMPLIST(p), from_g);
  relocate_impure(&INITSYMSPLIST(p), from_g);
  relocate_impure(&INITSYMNAME(p), from_g);
  relocate_impure(&INITSYMHASH(p), from_g);
}

static void sweep_port(ptr p, IGEN from_g FORMAL_CTGS) {
  relocate_impure(&PORTHANDLER(p), from_g);
  relocate_impure(&PORTINFO(p), from_g);
  relocate_impure(&PORTNAME(p), from_g);

  if (PORTTYPE(p) & PORT_FLAG_OUTPUT) {
    iptr n = (iptr)PORTOLAST(p) - (iptr)PORTOBUF(p);
    relocate_impure(&PORTOBUF(p), from_g);
    PORTOLAST(p) = (ptr)((iptr)PORTOBUF(p) + n);
  }

  if (PORTTYPE(p) & PORT_FLAG_INPUT) {
    iptr n = (iptr)PORTILAST(p) - (iptr)PORTIBUF(p);
    relocate_impure(&PORTIBUF(p), from_g);
    PORTILAST(p) = (ptr)((iptr)PORTIBUF(p) + n);
  }
}

static void sweep_thread(ptr p FORMAL_CTGS) {
  ptr tc = (ptr)THREADTC(p);
  INT i;

  if (tc != (ptr)0) {
    ptr old_stack = SCHEMESTACK(tc);
    if (OLDSPACE(old_stack)) {
      iptr clength = (uptr)SFP(tc) - (uptr)old_stack;
     /* include SFP[0], which contains the return address */
      SCHEMESTACK(tc) = copy_stack(old_stack, &SCHEMESTACKSIZE(tc), clength + sizeof(ptr) ACTUAL_CTGS);
      SFP(tc) = (ptr)((uptr)SCHEMESTACK(tc) + clength);
      ESP(tc) = (ptr)((uptr)SCHEMESTACK(tc) + SCHEMESTACKSIZE(tc) - stack_slop);
    }
    STACKCACHE(tc) = Snil;
    relocate_pure(&CCHAIN(tc));
    /* U32 RANDOMSEED(tc) */
    /* I32 ACTIVE(tc) */
    relocate_pure(&STACKLINK(tc));
    /* iptr SCHEMESTACKSIZE */
    relocate_pure(&WINDERS(tc));
    relocate_return_addr(&FRAME(tc,0));
    sweep_stack((uptr)SCHEMESTACK(tc), (uptr)SFP(tc), (uptr)FRAME(tc,0) ACTUAL_CTGS);
    U(tc) = V(tc) = W(tc) = X(tc) = Y(tc) = 0;
    /* immediate SOMETHINGPENDING(tc) */
    /* immediate TIMERTICKS */
    /* immediate DISABLE_COUNT */
    /* immediate SIGNALINTERRUPTPENDING */
    /* void* SIGNALINTERRUPTQUEUE(tc) */
    /* immediate KEYBOARDINTERRUPTPENDING */
    relocate_pure(&THREADNO(tc));
    relocate_pure(&CURRENTINPUT(tc));
    relocate_pure(&CURRENTOUTPUT(tc));
    relocate_pure(&CURRENTERROR(tc));
    /* immediate BLOCKCOUNTER */
    relocate_pure(&SFD(tc));
    relocate_pure(&CURRENTMSO(tc));
    relocate_pure(&TARGETMACHINE(tc));
    relocate_pure(&FXLENGTHBV(tc));
    relocate_pure(&FXFIRSTBITSETBV(tc));
    relocate_pure(&NULLIMMUTABLEVECTOR(tc));
    relocate_pure(&NULLIMMUTABLEFXVECTOR(tc));
    relocate_pure(&NULLIMMUTABLEBYTEVECTOR(tc));
    relocate_pure(&NULLIMMUTABLESTRING(tc));
    /* immediate METALEVEL */
    relocate_pure(&COMPILEPROFILE(tc));
    /* immediate GENERATEINSPECTORINFORMATION */
    /* immediate GENERATEPROFILEFORMS */
    /* immediate OPTIMIZELEVEL */
    relocate_pure(&SUBSETMODE(tc));
    /* immediate SUPPRESSPRIMITIVEINLINING */
    relocate_pure(&DEFAULTRECORDEQUALPROCEDURE(tc));
    relocate_pure(&DEFAULTRECORDHASHPROCEDURE(tc));
    relocate_pure(&COMPRESSFORMAT(tc));
    relocate_pure(&COMPRESSLEVEL(tc));
    /* void* LZ4OUTBUFFER(tc) */
    /* U64 INSTRCOUNTER(tc) */
    /* U64 ALLOCCOUNTER(tc) */
    relocate_pure(&PARAMETERS(tc));
    for (i = 0 ; i < virtual_register_count ; i += 1) {
      relocate_pure(&VIRTREG(tc, i));
    }
    DSTBV(tc) = SRCBV(tc) = Sfalse;
  }
}

static void sweep_continuation(ptr p FORMAL_CTGS) {
  relocate_pure(&CONTWINDERS(p));

 /* bug out for shot 1-shot continuations */
  if (CONTLENGTH(p) == scaled_shot_1_shot_flag) return;

  if (OLDSPACE(CONTSTACK(p)))
    CONTSTACK(p) = copy_stack(CONTSTACK(p), &CONTLENGTH(p), CONTCLENGTH(p) ACTUAL_CTGS);

  relocate_pure(&CONTLINK(p));
  relocate_return_addr(&CONTRET(p));

 /* use CLENGTH to avoid sweeping unoccupied portion of one-shots */
  sweep_stack((uptr)CONTSTACK(p), (uptr)CONTSTACK(p) + CONTCLENGTH(p), (uptr)CONTRET(p) ACTUAL_CTGS);
}

/* assumes stack has already been copied to newspace */
static void sweep_stack(uptr base, uptr fp, uptr ret FORMAL_CTGS) {
  ptr *pp; iptr oldret;
  ptr num;

  while (fp != base) {
    if (fp < base)
      S_error_abort("sweep_stack(gc): malformed stack");
    fp = fp - ENTRYFRAMESIZE(ret);
    pp = (ptr *)fp;

    oldret = ret;
    ret = (iptr)(*pp);
    relocate_return_addr(pp);

    num = ENTRYLIVEMASK(oldret);
    if (Sfixnump(num)) {
      uptr mask = UNFIX(num);
      while (mask != 0) {
        pp += 1;
        if (mask & 0x0001) relocate_pure(pp);
        mask >>= 1;
      }
    } else {
      iptr index;

      relocate_pure(&ENTRYLIVEMASK(oldret));
      num = ENTRYLIVEMASK(oldret);
      index = BIGLEN(num);
      while (index-- != 0) {
        INT bits = bigit_bits;
        bigit mask = BIGIT(num,index);
        while (bits-- > 0) {
          pp += 1;
          if (mask & 1) relocate_pure(pp);
          mask >>= 1;
        }
      }
    }
  }
}

static void sweep_record(ptr x, IGEN from_g FORMAL_CTGS) {
    ptr *pp; ptr num; ptr rtd;

  /* record-type descriptor was forwarded in copy */
    rtd = RECORDINSTTYPE(x);
    num = RECORDDESCPM(rtd);
    pp = &RECORDINSTIT(x,0);

  /* sweep cells for which bit in pm is set; quit when pm == 0. */
    if (Sfixnump(num)) {
       /* ignore bit for already forwarded rtd */
        uptr mask = (uptr)UNFIX(num) >> 1;
        if (mask == (uptr)-1 >> 1) {
            ptr *ppend = (ptr *)((uptr)pp + UNFIX(RECORDDESCSIZE(rtd))) - 1;
            while (pp < ppend) {
                relocate_impure(pp, from_g);
                pp += 1;
            }
        } else {
            while (mask != 0) {
                if (mask & 1) { relocate_impure(pp, from_g); }
                mask >>= 1;
                pp += 1;
            }
        }
    } else {
        iptr index; bigit mask; INT bits;

       /* bignum pointer mask may have been forwarded */
        relocate_pure(&RECORDDESCPM(rtd));
        num = RECORDDESCPM(rtd);
        index = BIGLEN(num) - 1;
       /* ignore bit for already forwarded rtd */
        mask = BIGIT(num,index) >> 1;
        bits = bigit_bits - 1;
        for (;;) {
            do {
                if (mask & 1) { relocate_impure(pp, from_g); }
                mask >>= 1;
                pp += 1;
            } while (--bits > 0);
            if (index-- == 0) break;
            mask = BIGIT(num,index);
            bits = bigit_bits;
        }
    }
}

static IGEN sweep_dirty_record(ptr x, IGEN youngest FORMAL_CTGS) {
    ptr *pp; ptr num; ptr rtd;

   /* warning: assuming rtd is immutable */
    rtd = RECORDINSTTYPE(x);

   /* warning: assuming MPM field is immutable */
    num = RECORDDESCMPM(rtd);
    pp = &RECORDINSTIT(x,0);

  /* sweep cells for which bit in mpm is set */
    if (Sfixnump(num)) {
       /* ignore bit for assumed immutable rtd */
        uptr mask = (uptr)UNFIX(num) >> 1;
        while (mask != 0) {
            if (mask & 1) relocate_dirty(pp, youngest);
            mask >>= 1;
            pp += 1;
        }
    } else {
        iptr index; bigit mask; INT bits;

        index = BIGLEN(num) - 1;
       /* ignore bit for assumed immutable rtd */
        mask = BIGIT(num,index) >> 1;
        bits = bigit_bits - 1;
        for (;;) {
            do {
                if (mask & 1) relocate_dirty(pp, youngest);
                mask >>= 1;
                pp += 1;
            } while (--bits > 0);
            if (index-- == 0) break;
            mask = BIGIT(num,index);
            bits = bigit_bits;
        }
    }

    return youngest;
}

static void sweep_code_object(ptr tc, ptr co FORMAL_CTGS) {
    ptr t, oldco; iptr a, m, n;

#ifdef DEBUG
    if ((CODETYPE(co) & mask_code) != type_code) {
      (void)printf("unexpected type %x sweeping code object %p\n", CODETYPE(co), co);
      (void)fflush(stdout);
    }
#endif

    relocate_pure(&CODENAME(co));
    relocate_pure(&CODEARITYMASK(co));
    relocate_pure(&CODEINFO(co));
    relocate_pure(&CODEPINFOS(co));

    t = CODERELOC(co);
    m = RELOCSIZE(t);
    oldco = RELOCCODE(t);
    a = 0;
    n = 0;
    while (n < m) {
        uptr entry, item_off, code_off; ptr obj;
        entry = RELOCIT(t, n); n += 1;
        if (RELOC_EXTENDED_FORMAT(entry)) {
            item_off = RELOCIT(t, n); n += 1;
            code_off = RELOCIT(t, n); n += 1;
        } else {
            item_off = RELOC_ITEM_OFFSET(entry);
            code_off = RELOC_CODE_OFFSET(entry);
        }
        a += code_off;
        obj = S_get_code_obj(RELOC_TYPE(entry), oldco, a, item_off);
        relocate_pure(&obj);
        S_set_code_obj("gc", RELOC_TYPE(entry), co, a, obj, item_off);
    }

    /* Don't copy non-oldspace relocation tables, since we may be
       sweeping a locked code object that is older than max_target_generation
       Doing so would be a waste of work anyway. */
    if (OLDSPACE(t)) {
      IGEN newg = compute_target_generation(GENERATION(t) ACTUAL_CTGS);
      if (newg == static_generation && !S_G.retain_static_relocation && (CODETYPE(co) & (code_flag_template << code_flags_offset)) == 0) {
        CODERELOC(co) = (ptr)0;
      } else {
        ptr oldt = t;
        n = size_reloc_table(RELOCSIZE(oldt));
#ifdef ENABLE_OBJECT_COUNTS
        S_G.countof[newg][countof_relocation_table] += 1;
        S_G.bytesof[newg][countof_relocation_table] += n;
#endif /* ENABLE_OBJECT_COUNTS */
        find_room(space_data, newg, typemod, n, t);
        copy_ptrs(typemod, t, oldt, n);
        RELOCCODE(t) = co;
        CODERELOC(co) = t;
      }
    } else {
      RELOCCODE(t) = co;
    }

    S_record_code_mod(tc, (uptr)&CODEIT(co,0), (uptr)CODELEN(co));
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

static void sweep_dirty(ONLY_FORMAL_CTGS) {
  IGEN youngest, min_youngest;
  ptr *pp, *ppend, *nl;
  uptr seg, d;
  ISPC s;
  IGEN from_g, to_g;
  seginfo *dirty_si, *nextsi;

  weaksegments_to_resweep = NULL;

  /* clear dirty segment lists for copied generations */
  for (from_g = 1; from_g <= MAX_CG; from_g += 1) {
    for (to_g = 0; to_g < from_g; to_g += 1) {
      DirtySegments(from_g, to_g) = NULL;
    }
  }

  /* NB: could have problems if a card is moved from some current or to-be-swept (from_g, to_g) to some previously
     swept list due to a dirty_set while we sweep.  believe this can't happen as of 6/14/2013.  if it can, it
     might be sufficient to process the lists in reverse order. */
  for (from_g = MAX_CG + 1; from_g <= static_generation; INCRGEN(from_g)) {
    for (to_g = 0; to_g <= MAX_CG; to_g += 1) {
      for (dirty_si = DirtySegments(from_g, to_g), DirtySegments(from_g, to_g) = NULL; dirty_si != NULL; dirty_si = nextsi) {
        nextsi = dirty_si->dirty_next;
        seg = dirty_si->number;
        s = dirty_si->space;

        if (s & space_locked) continue;

        /* reset min dirty byte so we can detect if byte is set while card is swept */
        dirty_si->min_dirty_byte = 0xff;

        min_youngest = 0xff;
        nl = from_g == MAX_TG ? (ptr *)orig_next_loc[from_g][s] : (ptr *)S_G.next_loc[from_g][s];
        ppend = build_ptr(seg, 0);

        if (s == space_weakpair) {
          weakseginfo *next = weaksegments_to_resweep;
          find_room(space_data, 0, typemod, ptr_align(sizeof(weakseginfo)), weaksegments_to_resweep);
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

              if (dirty_si->dirty_bytes[d] <= MAX_CG) {
                /* assume we won't find any wrong-way pointers */
                youngest = 0xff;

                if (s == space_impure) {
                  while (pp < ppend && *pp != forward_marker) {
                    /* handle two pointers at a time */
                    relocate_dirty(pp, youngest);
                    pp += 1;
                    relocate_dirty(pp, youngest);
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
                    ptr p, val, code;

                    p = TYPE((ptr)pp, type_symbol);

                    val = SYMVAL(p);
                    relocate_dirty(&val, youngest);
                    INITSYMVAL(p) = val;
                    code = Sprocedurep(val) ? CLOSCODE(val) : SYMCODE(p);
                    relocate_dirty(&code, youngest);
                    INITSYMCODE(p,code);
                    relocate_dirty(&INITSYMPLIST(p), youngest);
                    relocate_dirty(&INITSYMSPLIST(p), youngest);
                    relocate_dirty(&INITSYMNAME(p), youngest);
                    relocate_dirty(&INITSYMHASH(p), youngest);

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

                    relocate_dirty(&PORTHANDLER(p), youngest);
                    relocate_dirty(&PORTINFO(p), youngest);
                    relocate_dirty(&PORTNAME(p), youngest);

                    if (PORTTYPE(p) & PORT_FLAG_OUTPUT) {
                      iptr n = (iptr)PORTOLAST(p) - (iptr)PORTOBUF(p);
                      relocate_dirty(&PORTOBUF(p), youngest);
                      PORTOLAST(p) = (ptr)((iptr)PORTOBUF(p) + n);
                    }

                    if (PORTTYPE(p) & PORT_FLAG_INPUT) {
                      iptr n = (iptr)PORTILAST(p) - (iptr)PORTIBUF(p);
                      relocate_dirty(&PORTIBUF(p), youngest);
                      PORTILAST(p) = (ptr)((iptr)PORTIBUF(p) + n);
                    }

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

                    youngest = sweep_dirty_record(p, youngest ACTUAL_CTGS);
                    p = (ptr)((iptr)p +
                        size_record_inst(UNFIX(RECORDDESCSIZE(
                              RECORDINSTTYPE(p)))));
                  }
                } else if (s == space_weakpair) {
                  while (pp < ppend && *pp != forward_marker) {
                    /* skip car field and handle cdr field */
                    pp += 1;
                    relocate_dirty(pp, youngest);
                    pp += 1;
                  }
                } else if (s == space_ephemeron) {
                  while (pp < ppend && *pp != forward_marker) {
                    ptr p = TYPE((ptr)pp, type_pair);
                    youngest = check_dirty_ephemeron(p, youngest ACTUAL_CTGS);
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
}

static void resweep_dirty_weak_pairs(ONLY_FORMAL_CTGS) {
  weakseginfo *ls;
  ptr *pp, *ppend, *nl, p;
  IGEN from_g, min_youngest, youngest, pg, newpg;
  uptr d;

  for (ls = weaksegments_to_resweep; ls != NULL; ls = ls->next) {
    seginfo *dirty_si = ls->si;
    from_g = dirty_si->generation;
    nl = from_g == MAX_TG ? (ptr *)orig_next_loc[from_g][space_weakpair] : (ptr *)S_G.next_loc[from_g][space_weakpair];
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
          if (dirty_si->dirty_bytes[d] <= MAX_CG) {
            youngest = ls->youngest[d];
            while (pp < ppend) {
              p = *pp;
              seginfo *si;

              /* handle car field */
              if (!IMMEDIATE(p) && (si = MaybeSegInfo(ptr_get_segment(p))) != NULL) {
                pg = si->generation;
                newpg = compute_target_generation(pg ACTUAL_CTGS);
                if (si->space & space_old) {
                  if (locked(p)) {
                    if (newpg < youngest) youngest = newpg;
                  } else if (FWDMARKER(p) == forward_marker && TYPEBITS(p) != type_flonum) {
                    *pp = FWDADDRESS(p);
                    if (newpg < youngest) youngest = newpg;
                  } else {
                    *pp = Sbwp_object;
                  }
                } else {
                  if (pg < youngest) youngest = pg;
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

static void check_ephemeron(ptr pe, IBOOL add_to_trigger FORMAL_CTGS) {
  ptr p;
  seginfo *si;
  IGEN from_g = GENERATION(pe);

  p = Scar(pe);
  if (!IMMEDIATE(p) && (si = MaybeSegInfo(ptr_get_segment(p))) != NULL && si->space & space_old && !locked(p)) {
    if (FWDMARKER(p) == forward_marker && TYPEBITS(p) != type_flonum) {
#ifndef NO_DIRTY_NEWSPACE_POINTERS
      IGEN pg = compute_target_generation(si->generation ACTUAL_CTGS);
      if (pg < from_g) record_new_dirty_card(&INITCAR(pe), pg);
#endif
      INITCAR(pe) = FWDADDRESS(p);
      relocate_impure(&INITCDR(pe), from_g);
      if (!add_to_trigger)
        EPHEMERONTRIGGERNEXT(pe) = Strue; /* in trigger list, #t means "done" */
    } else {
      /* Not reached, so far; install as trigger */
      EPHEMERONTRIGGERNEXT(pe) = si->trigger_ephemerons;
      si->trigger_ephemerons = pe;
      if (add_to_trigger) {
        EPHEMERONNEXT(pe) = trigger_ephemerons;
        trigger_ephemerons = pe;
      }
    }
  } else {
    relocate_impure(&INITCDR(pe), from_g);
  }
}

static void check_pending_ephemerons(ONLY_FORMAL_CTGS) {
  ptr pe, next_pe;

  pe = pending_ephemerons;
  pending_ephemerons = NULL;
  while (pe != NULL) {
    next_pe = EPHEMERONNEXT(pe);
    check_ephemeron(pe, 1 ACTUAL_CTGS);
    pe = next_pe;
  }

  pe = repending_ephemerons;
  repending_ephemerons = NULL;
  while (pe != NULL) {
    next_pe = EPHEMERONTRIGGERNEXT(pe);
    check_ephemeron(pe, 0 ACTUAL_CTGS);
    pe = next_pe;
  }
}

/* Like check_ephemeron(), but for a dirty, old-generation
   ephemeron (that was not yet added to the pending list), so we can
   be less pessimistic than setting `youngest` to the target
   generation: */
static IGEN check_dirty_ephemeron(ptr pe, IGEN youngest FORMAL_CTGS) {
  ptr p;
  seginfo *si;
  IGEN pg;

  p = Scar(pe);
  if (!IMMEDIATE(p) && (si = MaybeSegInfo(ptr_get_segment(p))) != NULL) {
    if (si->space & space_old && !locked(p)) {
      if (FWDMARKER(p) == forward_marker && TYPEBITS(p) != type_flonum) {
        INITCAR(pe) = FWDADDRESS(p);
        if (youngest != MIN_TG && (pg = compute_target_generation(si->generation ACTUAL_CTGS)) < youngest)
          youngest = pg;
        relocate_dirty(&INITCDR(pe), youngest);
      } else {
        /* Not reached, so far; add to pending list */
        add_ephemeron_to_pending(pe);
        /* Make the consistent (but pessimistic w.r.t. to wrong-way
           pointers) assumption that the key will stay live and move
           to the target generation. That assumption covers the value
           part, too, since it can't end up younger than the target
           generation. */
        if (youngest != MIN_TG && (pg = compute_target_generation(si->generation ACTUAL_CTGS)) < youngest)
          youngest = pg;
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

  return youngest;
}

static void clear_trigger_ephemerons(void) {
  ptr pe;

  if (pending_ephemerons != NULL)
    S_error_abort("clear_trigger_ephemerons(gc): non-empty pending list");

  pe = trigger_ephemerons;
  trigger_ephemerons = NULL;
  while (pe != NULL) {
    if (EPHEMERONTRIGGERNEXT(pe) == Strue) {
      /* The ephemeron was triggered and retains its key and value */
    } else {
      seginfo *si;
      ptr p = Scar(pe);
      /* Key never became reachable, so clear key and value */
      INITCAR(pe) = Sbwp_object;
      INITCDR(pe) = Sbwp_object;

      /* Remove trigger */
      si = SegInfo(ptr_get_segment(p));
      si->trigger_ephemerons = NULL;
    }
    pe = EPHEMERONNEXT(pe);
  }
}
