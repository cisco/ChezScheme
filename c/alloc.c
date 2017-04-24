/* alloc.c
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
static void maybe_fire_collector PROTO((void));

void S_alloc_init() {
    ISPC s; IGEN g; UINT i;

    if (S_boot_time) {
      /* reset the allocation tables */
        for (s = 0; s <= max_real_space; s++) {
            for (g = 0; g <= static_generation; g++) {
                S_G.base_loc[s][g] = FIX(0);
                S_G.first_loc[s][g] = FIX(0);
                S_G.next_loc[s][g] = FIX(0);
                S_G.bytes_left[s][g] = 0;
                S_G.bytes_of_space[s][g] = 0;
            }
        }

        /* initialize the dirty-segment lists. */
        for (i = 0; i < DIRTY_SEGMENT_LISTS; i += 1) {
          S_G.dirty_segments[i] = NULL;
        }

        S_G.collect_trip_bytes = default_collect_trip_bytes;

       /* set to final value in prim.c when known */
        S_protect(&S_G.nonprocedure_code);
        S_G.nonprocedure_code = FIX(0);

        S_protect(&S_G.null_vector);
        find_room(space_new, 0, type_typed_object, size_vector(0), S_G.null_vector);
        VECTTYPE(S_G.null_vector) = (0 << vector_length_offset) | type_vector;

        S_protect(&S_G.null_fxvector);
        find_room(space_new, 0, type_typed_object, size_fxvector(0), S_G.null_fxvector);
        FXVECTOR_TYPE(S_G.null_fxvector) = (0 << fxvector_length_offset) | type_fxvector;

        S_protect(&S_G.null_bytevector);
        find_room(space_new, 0, type_typed_object, size_bytevector(0), S_G.null_bytevector);
        BYTEVECTOR_TYPE(S_G.null_bytevector) = (0 << bytevector_length_offset) | type_bytevector;

        S_protect(&S_G.null_string);
        find_room(space_new, 0, type_typed_object, size_string(0), S_G.null_string);
        STRTYPE(S_G.null_string) = (0 << string_length_offset) | type_string;
    }
}

void S_protect(p) ptr *p; {
    if (S_G.protect_next > max_protected)
        S_error_abort("max_protected constant too small");
    *p = snil;
    S_G.protected[S_G.protect_next++] = p;
}

/* S_reset_scheme_stack is always called with mutex */
void S_reset_scheme_stack(tc, n) ptr tc; iptr n; {
    ptr *x; iptr m;

  /* we allow less than one_shot_headroom here for no truly justifyable
     reason */
    n = ptr_align(n + (one_shot_headroom >> 1));

    x = &STACKCACHE(tc);
    for (;;) {
        if (*x == snil) {
            if (n < default_stack_size) n = default_stack_size;
          /* stacks are untyped objects */
            find_room(space_new, 0, typemod, n, SCHEMESTACK(tc));
            break;
        }
        if ((m = CACHEDSTACKSIZE(*x)) >= n) {
            n = m;
            SCHEMESTACK(tc) = *x;
/* if we decide to leave KEEPSMALLPUPPIES undefined permanently, we should
   rewrite this code to remove the indirect on x */
/* #define KEEPSMALLPUPPIES */
#ifdef KEEPSMALLPUPPIES
            *x = CACHEDSTACKLINK(*x);
#else
            STACKCACHE(tc) = CACHEDSTACKLINK(*x);
#endif
            break;
        }
        x = &CACHEDSTACKLINK(*x);
    }
    SCHEMESTACKSIZE(tc) = n;
    ESP(tc) = (ptr)((uptr)SCHEMESTACK(tc) + n - stack_slop);
    SFP(tc) = (ptr)SCHEMESTACK(tc);
}

ptr S_compute_bytes_allocated(xg, xs) ptr xg; ptr xs; {
  ptr tc = get_thread_context();
  ISPC s, smax, smin; IGEN g, gmax, gmin;
  uptr n;

  gmin = (IGEN)UNFIX(xg);
  if (gmin < 0) {
    gmin = 0;
    gmax = static_generation;
  } else if (gmin == S_G.new_max_nonstatic_generation) {
   /* include virtual inhabitents too */
    gmax = S_G.max_nonstatic_generation;
  } else {
    gmax = gmin;
  }

  smin = (ISPC)(UNFIX(xs));
  smax = smin < 0 ? max_real_space : smin;
  smin = smin < 0 ? 0 : smin;

  n = 0;

  g = gmin;
  while (g <= gmax) {
    for (s = smin; s <= smax; s++) {
     /* add in bytes previously recorded */
      n += S_G.bytes_of_space[s][g];
     /* add in bytes in active segments */
      if (S_G.next_loc[s][g] != FIX(0))
        n += (char *)S_G.next_loc[s][g] - (char *)S_G.base_loc[s][g];
    }
    if (g == S_G.max_nonstatic_generation)
      g = static_generation;
    else
      g += 1;
  }

 /* subtract off bytes not allocated */
  if (gmin == 0 && smin <= space_new && space_new <= smax)
      n -= (uptr)REAL_EAP(tc) - (uptr)AP(tc);

  return Sunsigned(n);
}

ptr S_thread_get_more_room(t, n) iptr t; iptr n; {
   ptr x;
   tc_mutex_acquire()
   find_room(space_new, 0, t, n, x);
   tc_mutex_release()
   return x;
}

static void maybe_fire_collector() {
  ISPC s;
  uptr bytes, fudge;

  bytes = 0;

  for (s = 0; s <= max_real_space; s += 1) {
   /* bytes already accounted for */
    bytes += S_G.bytes_of_space[s][0];
   /* bytes in current block of segments */
    if (S_G.next_loc[s][0] != FIX(0))
      bytes += (char *)S_G.next_loc[s][0] - (char *)S_G.base_loc[s][0];
  }

 /* arbitrary fudge factor to account for space we may not be using yet
    arbitrary because:
      - we assume each thread has not yet used half it's allocation area
      - we assume each thread has not yet used half its stack
      - some threads' stacks may not be as much as the default size
 */
  fudge = (default_stack_size / 2) + S_nthreads * (bytes_per_segment / 2);
  bytes = bytes > fudge ? bytes - fudge : 0;

  if (bytes >= S_G.collect_trip_bytes)
    S_fire_collector();
}

/* find_more_room
 * S_find_more_room is called from the macro find_room when
 * the current segment is too full to fit the allocation.
 *
 * A forward_marker followed by a pointer to
 * the newly obtained segment is placed at next_loc to show
 * gc where the end of this segment is and where the next
 * segment of this type resides.  Allocation occurs from the
 * beginning of the newly obtained segment.  The need for the
 * eos marker explains the (2 * ptr_bytes) byte factor in
 * S_find_more_room.
 */
/* S_find_more_room is always called with mutex */
ptr S_find_more_room(s, g, n, old) ISPC s; IGEN g; iptr n; ptr old; {
  iptr nsegs, seg;
  ptr new;

  S_pants_down += 1;

  nsegs = (uptr)(n + 2 * ptr_bytes + bytes_per_segment - 1) >> segment_offset_bits;

 /* block requests to minimize fragmentation and improve cache locality */
  if (s == space_code && nsegs < 16) nsegs = 16;

  seg = S_find_segments(s, g, nsegs);
  new = build_ptr(seg, 0);

  if (old == FIX(0)) {
   /* first object of this space */
    S_G.first_loc[s][g] = new;
  } else {
   /* increment bytes_allocated by the closed-off partial segment */
    S_G.bytes_of_space[s][g] += (char *)old - (char *)S_G.base_loc[s][g];
   /* lay down an end-of-segment marker */
    *(ptr*)old = forward_marker;
    *((ptr*)old + 1) = new;
  }

 /* base address of current block of segments to track amount of allocation */
  S_G.base_loc[s][g] = new;

  S_G.next_loc[s][g] = (ptr)((uptr)new + n);
  S_G.bytes_left[s][g] = (nsegs * bytes_per_segment - n) - 2 * ptr_bytes;

  if (g == 0) maybe_fire_collector();

  S_pants_down -= 1;
  return new;
}

/* S_reset_allocation_pointer is always called with mutex */
/* We always allocate exactly one segment for the allocation area, since
   we can get into hot water with formerly locked objects, specifically
   symbols and impure records, that cross segment boundaries.  This allows
   us to maintain the invariant that no object crosses a segment boundary
   unless it starts on a segment boundary (and is thus at least one
   segment long).  NB.  This invariant does not apply to code objects
   since we grab large blocks of segments for them.
*/

void S_reset_allocation_pointer(tc) ptr tc; {
  iptr seg;

  S_pants_down += 1;

  seg = S_find_segments(space_new, 0, 1);

  /* NB: if allocate_segments didn't already ensure we don't use the last segment
     of memory, we'd have to reject it here so cp2-alloc can avoid a carry check for
     small allocation requests, using something like this:

     if (seg == (((uptr)1 << (ptr_bits - segment_offset_bits)) - 1))
       seg = S_find_segments(space_new, 0, 1);
  */

  S_G.bytes_of_space[space_new][0] += bytes_per_segment;

  maybe_fire_collector();

  AP(tc) = build_ptr(seg, 0);
  REAL_EAP(tc) = EAP(tc) = (ptr)((uptr)AP(tc) + bytes_per_segment);

  S_pants_down -= 1;
}


FORCEINLINE void mark_segment_dirty(seginfo *si, IGEN from_g) {
  IGEN to_g = si->min_dirty_byte;
  if (to_g != 0) {
    seginfo **pointer_to_first, *oldfirst;
    if (to_g != 0xff) {
      seginfo *next = si->dirty_next, **prev = si->dirty_prev;
      /* presently on some other list, so remove */
      *prev = next;
      if (next != NULL) next->dirty_prev = prev;
    }
    oldfirst = *(pointer_to_first = &DirtySegments(from_g, 0));
    *pointer_to_first = si;
    si->dirty_prev = pointer_to_first;
    si->dirty_next = oldfirst;
    if (oldfirst != NULL) oldfirst->dirty_prev = &si->dirty_next;
    si->min_dirty_byte = 0;
  }
}

void S_dirty_set(ptr *loc, ptr x) {
  *loc = x;
  if (!Sfixnump(x)) {
    seginfo *si = SegInfo(addr_get_segment(loc));
    IGEN from_g = si->generation;
    if (from_g != 0) {
      si->dirty_bytes[((uptr)loc >> card_offset_bits) & ((1 << segment_card_offset_bits) - 1)] = 0;
      mark_segment_dirty(si, from_g);
    }
  }
}

/* scan remembered set from P to ENDP, transfering to dirty vector */
void S_scan_dirty(ptr **p, ptr **endp) {
  uptr this, last;
 
  last = 0;

  while (p < endp) {
    ptr *loc = *p;
   /* whether building s directory or running UXLB code, the most
      common situations are that *loc is a fixnum, this == last, or loc
      is in generation 0. the generated code no longer adds elements
      to the remembered set if the RHS val is a fixnum.  the other
      checks we do here.  we don't bother looking for *loc being an
      immediate or outside the heap, nor for the generation of *loc
      being the same or older than the generation of loc, since these
      don't seem to weed out many dirty writes, and we don't want to
      waste time here on fruitless memory reads and comparisions */
    if ((this = (uptr)loc >> card_offset_bits) != last) {
      seginfo *si = SegInfo(addr_get_segment(loc));
      IGEN from_g = si->generation;
      if (from_g != 0) {
        si->dirty_bytes[((uptr)loc >> card_offset_bits) & ((1 << segment_card_offset_bits) - 1)] = 0;
        if (this >> segment_card_offset_bits != last >> segment_card_offset_bits) mark_segment_dirty(si, from_g);
      }
      last = this;
    }
    p += 1;
  }
}

/* S_scan_remembered_set is called from generated machine code when there
 * is insufficient room for a remembered set addition.
 */

void S_scan_remembered_set() {
  ptr tc = get_thread_context();
  uptr ap, eap, real_eap;

  tc_mutex_acquire()

  ap = (uptr)AP(tc);
  eap = (uptr)EAP(tc);
  real_eap = (uptr)REAL_EAP(tc);

  S_scan_dirty((ptr **)eap, (ptr **)real_eap);
  eap = real_eap;

  if (eap - ap > alloc_waste_maximum) {
    AP(tc) = (ptr)ap;
    EAP(tc) = (ptr)eap;
  } else {
    S_G.bytes_of_space[space_new][0] -= eap - ap;
    S_reset_allocation_pointer(tc);
  }

  tc_mutex_release()
}

/* S_get_more_room is called from genereated machine code when there is
 * insufficient room for an allocation.  ap has already been incremented
 * by the size of the object and xp is a (typed) pointer to the value of
 * ap before the allocation attempt.  xp must be set to a new object of
 * the appropriate type and size.
 */

void S_get_more_room() {
  ptr tc = get_thread_context();
  ptr xp; uptr ap, eap, real_eap, type, size;

  tc_mutex_acquire()

  ap = (uptr)AP(tc);
  eap = (uptr)EAP(tc);
  real_eap = (uptr)REAL_EAP(tc);

  xp = XP(tc);
  if ((type = TYPEBITS(xp)) == 0) type = typemod;
  size = ap - (iptr)UNTYPE(xp,type);
  ap -= size;

  S_scan_dirty((ptr **)eap, (ptr **)real_eap);
  eap = real_eap;

  if (eap - ap >= size) {
    XP(tc) = TYPE(ap, type);
    ap += size;
    if (eap - ap > alloc_waste_maximum) {
      AP(tc) = (ptr)ap;
      EAP(tc) = (ptr)eap;
    } else {
      S_G.bytes_of_space[space_new][0] -= eap - ap;
      S_reset_allocation_pointer(tc);
    }
  } else if (eap - ap > alloc_waste_maximum) {
    AP(tc) = (ptr)ap;
    EAP(tc) = (ptr)eap;
    find_room(space_new, 0, type, size, XP(tc));
  } else {
    S_G.bytes_of_space[space_new][0] -= eap - ap;
    S_reset_allocation_pointer(tc);
    ap = (uptr)AP(tc);
    if (size + alloc_waste_maximum <= (uptr)EAP(tc) - ap) {
      XP(tc) = TYPE(ap, type);
      AP(tc) = (ptr)(ap + size);
    } else {
      find_room(space_new, 0, type, size, XP(tc));
    }
  }

  tc_mutex_release()
}

/* S_cons_in is always called with mutex */
ptr S_cons_in(s, g, car, cdr) ISPC s; IGEN g; ptr car, cdr; {
    ptr p;

    find_room(s, g, type_pair, size_pair, p);
    INITCAR(p) = car;
    INITCDR(p) = cdr;
    return p;
}

ptr Scons(car, cdr) ptr car, cdr; {
    ptr tc = get_thread_context();
    ptr p;

    thread_find_room(tc, type_pair, size_pair, p);
    INITCAR(p) = car;
    INITCDR(p) = cdr;
    return p;
}

ptr Sbox(ref) ptr ref; {
    ptr tc = get_thread_context();
    ptr p;

    thread_find_room(tc, type_typed_object, size_box, p);
    BOXTYPE(p) = type_box;
    INITBOXREF(p) = ref;
    return p;
}

ptr S_symbol(name) ptr name; {
    ptr tc = get_thread_context();
    ptr p;

    thread_find_room(tc, type_symbol, size_symbol, p);
  /* changes here should be reflected in the oblist collection code in gc.c */
    INITSYMVAL(p) = sunbound;
    INITSYMCODE(p,S_G.nonprocedure_code);
    INITSYMPLIST(p) = snil;
    INITSYMSPLIST(p) = snil;
    INITSYMNAME(p) = name;
    INITSYMHASH(p) = Sfalse;
    return p;
}

ptr S_rational(n, d) ptr n, d; {
    if (d == FIX(1)) return n;
    else {
        ptr tc = get_thread_context();
        ptr p;

        thread_find_room(tc, type_typed_object, size_ratnum, p);
        RATTYPE(p) = type_ratnum;
        RATNUM(p) = n;
        RATDEN(p) = d;
        return p;
    }
}

ptr S_tlc(ptr keyval, ptr ht, ptr next) {
    ptr tc = get_thread_context();
    ptr p;

    thread_find_room(tc, type_typed_object, size_tlc, p);
    TLCTYPE(p) = type_tlc;
    INITTLCKEYVAL(p) = keyval;
    INITTLCHT(p) = ht;
    INITTLCNEXT(p) = next;
    return p;
}

/* S_vector_in is always called with mutex */
ptr S_vector_in(s, g, n) ISPC s; IGEN g; iptr n; {
    ptr p; iptr d;

    if (n == 0) return S_G.null_vector;

    if ((uptr)n >= maximum_vector_length)
        S_error("", "invalid vector size request");

    d = size_vector(n);
   /* S_vector_in always called with mutex */
    find_room(s, g, type_typed_object, d, p);
    VECTTYPE(p) = (n << vector_length_offset) | type_vector;
    return p;
}

ptr S_vector(n) iptr n; {
    ptr tc;
    ptr p; iptr d;

    if (n == 0) return S_G.null_vector;

    if ((uptr)n >= maximum_vector_length)
        S_error("", "invalid vector size request");

    tc = get_thread_context();

    d = size_vector(n);
    thread_find_room(tc, type_typed_object, d, p);
    VECTTYPE(p) = (n << vector_length_offset) | type_vector;
    return p;
}

ptr S_fxvector(n) iptr n; {
    ptr tc;
    ptr p; iptr d;

    if (n == 0) return S_G.null_fxvector;

    if ((uptr)n > (uptr)maximum_fxvector_length)
        S_error("", "invalid fxvector size request");

    tc = get_thread_context();

    d = size_fxvector(n);
    thread_find_room(tc, type_typed_object, d, p);
    FXVECTOR_TYPE(p) = (n << fxvector_length_offset) | type_fxvector;
    return p;
}

ptr S_bytevector(n) iptr n; {
    ptr tc;
    ptr p; iptr d;

    if (n == 0) return S_G.null_bytevector;

    if ((uptr)n > (uptr)maximum_bytevector_length)
        S_error("", "invalid bytevector size request");

    tc = get_thread_context();

    d = size_bytevector(n);
    thread_find_room(tc, type_typed_object, d, p);
    BYTEVECTOR_TYPE(p) = (n << bytevector_length_offset) | type_bytevector;
    return p;
}

ptr S_null_immutable_vector() {
  ptr v;
  find_room(space_new, 0, type_typed_object, size_vector(0), v);
  VECTTYPE(v) = (0 << vector_length_offset) | type_vector | vector_immutable_flag;
  return v;
}

ptr S_null_immutable_fxvector() {
  ptr v;
  find_room(space_new, 0, type_typed_object, size_fxvector(0), v);
  VECTTYPE(v) = (0 << fxvector_length_offset) | type_fxvector | fxvector_immutable_flag;
  return v;
}

ptr S_null_immutable_bytevector() {
  ptr v;
  find_room(space_new, 0, type_typed_object, size_bytevector(0), v);
  VECTTYPE(v) = (0 << bytevector_length_offset) | type_bytevector | bytevector_immutable_flag;
  return v;
}

ptr S_null_immutable_string() {
  ptr v;
  find_room(space_new, 0, type_typed_object, size_string(0), v);
  VECTTYPE(v) = (0 << string_length_offset) | type_string | string_immutable_flag;
  return v;
}

ptr S_record(n) iptr n; {
    ptr tc = get_thread_context();
    ptr p;

    thread_find_room(tc, type_typed_object, n, p);
    return p;
}

ptr S_closure(cod, n) ptr cod; iptr n; {
    ptr tc = get_thread_context();
    ptr p; iptr d;

    d = size_closure(n);
    thread_find_room(tc, type_closure, d, p);
    CLOSENTRY(p) = cod;
    return p;
}

/* S_mkcontinuation is always called with mutex */
ptr S_mkcontinuation(s, g, nuate, stack, length, clength, link, ret, winders)
        ISPC s; IGEN g; ptr nuate; ptr stack; iptr length; iptr clength; ptr link;
        ptr ret; ptr winders; {
    ptr p;

    find_room(s, g, type_closure, size_continuation, p);
    CLOSENTRY(p) = nuate;
    CONTSTACK(p) = stack;
    CONTLENGTH(p) = length;
    CONTCLENGTH(p) = clength;
    CONTLINK(p) = link;
    CONTRET(p) = ret;
    CONTWINDERS(p) = winders;
    return p;
}

ptr Sflonum(x) double x; {
    ptr tc = get_thread_context();
    ptr p;

    thread_find_room(tc, type_flonum, size_flonum, p);
    FLODAT(p) = x;
    return p;
}

ptr S_inexactnum(rp, ip) double rp, ip; {
    ptr tc = get_thread_context();
    ptr p;

    thread_find_room(tc, type_typed_object, size_inexactnum, p);
    INEXACTNUM_TYPE(p) = type_inexactnum;
    INEXACTNUM_REAL_PART(p) = rp;
    INEXACTNUM_IMAG_PART(p) = ip;
    return p;
}

/* S_thread is always called with mutex */
ptr S_thread(xtc) ptr xtc; {
    ptr p;

   /* don't use thread_find_room since we may be building the current thread */
    find_room(space_new, 0, type_typed_object, size_thread, p);
    TYPEFIELD(p) = (ptr)type_thread;
    THREADTC(p) = (uptr)xtc;
    return p;
}

ptr S_exactnum(a, b) ptr a, b; {
    ptr tc = get_thread_context();
    ptr p;

    thread_find_room(tc, type_typed_object, size_exactnum, p);
    EXACTNUM_TYPE(p) = type_exactnum;
    EXACTNUM_REAL_PART(p) = a;
    EXACTNUM_IMAG_PART(p) = b;
    return p;
}

ptr S_ifile(icount, name, fd, info, flags, ilast, ibuf)
        iptr flags, icount; char *ilast; iptr fd; ptr name, ibuf, info; {
    ptr tc = get_thread_context();
    ptr p;

    thread_find_room(tc, type_typed_object, size_port, p);
    PORTTYPE(p) = flags | type_port;
    PORTNAME(p) = name;
  /* PORTHANDLER is really a ptr only when PORTTYPE & PORT_FLAG_PROC_HANDLER is true */
    PORTHANDLER(p) = (ptr)fd;
    PORTINFO(p) = info;
    PORTICNT(p) = icount;
    PORTILAST(p) = (ptr)ilast;
    PORTIBUF(p) = ibuf;
  /* leave output buffer and last uninitialized for input only ports */
    PORTOCNT(p) = 0;
    return p;
}

ptr S_ofile(ocount, name, fd, info, flags, olast, obuf)
        iptr flags, ocount; char *olast; iptr fd; ptr name, obuf, info; {
    ptr tc = get_thread_context();
    ptr p;

    thread_find_room(tc, type_typed_object, size_port, p);
    PORTTYPE(p) = flags | type_port;
    PORTNAME(p) = name;
  /* PORTHANDLER is really a ptr only when PORTTYPE & PORT_FLAG_PROC_HANDLER is true */
    PORTHANDLER(p) = (ptr)fd;
    PORTINFO(p) = info;
    PORTOCNT(p) = ocount;
    PORTOLAST(p) = (ptr)olast;
    PORTOBUF(p) = obuf;
  /* leave input buffer and last uninitialized for output only ports */
    PORTICNT(p) = 0;
    return p;
}

ptr S_iofile(icount, ocount, name, fd, info, flags, ilast, ibuf, olast, obuf)
        iptr flags, icount, ocount; char *ilast, *olast; iptr fd; ptr name, ibuf, obuf, info; {
    ptr tc = get_thread_context();
    ptr p;

    thread_find_room(tc, type_typed_object, size_port, p);
    PORTTYPE(p) = flags | type_port;
    PORTNAME(p) = name;
  /* PORTHANDLER is really a ptr only when PORTTYPE & PORT_FLAG_PROC_HANDLER is true */
    PORTHANDLER(p) = (ptr)fd;
    PORTINFO(p) = info;
    PORTICNT(p) = icount;
    PORTILAST(p) = (ptr)ilast;
    PORTIBUF(p) = ibuf;
    PORTOCNT(p) = ocount;
    PORTOLAST(p) = (ptr)olast;
    PORTOBUF(p) = obuf;
    return p;
}

/* S_string returns a new string of length n.  If s is not NULL, it is
 * copied into the new string.  If n < 0, then s must be non-NULL,
 * and the length of s (by strlen) determines the length of the string */
ptr S_string(s, n) const char *s; iptr n; {
    ptr tc;
    ptr p; iptr d;
    iptr i;

    if (n < 0) n = strlen(s);

    if (n == 0) return S_G.null_string;

    if ((uptr)n > (uptr)maximum_string_length)
        S_error("", "invalid string size request");

    tc = get_thread_context();

    d = size_string(n);
    thread_find_room(tc, type_typed_object, d, p);
    STRTYPE(p) = (n << string_length_offset) | type_string;

  /* fill the string with valid characters */
    i = 0;

  /* first copy input string, if any */
    if (s != (char *)NULL) {
      while (i != n && *s != 0) {
        Sstring_set(p, i, *s++);
        i += 1;
      }
    }

  /* fill remaining slots with nul */
    while (i != n) {
      Sstring_set(p, i, 0);
      i += 1;
    }

    return p;
}

ptr S_bignum(n, sign) iptr n; IBOOL sign; {
    ptr tc = get_thread_context();
    ptr p; iptr d;

    if ((uptr)n > (uptr)maximum_bignum_length)
        S_error("", "invalid bignum size request");

    d = size_bignum(n);
    thread_find_room(tc, type_typed_object, d, p);
    BIGTYPE(p) = (uptr)n << bignum_length_offset | sign << bignum_sign_offset | type_bignum;
    return p;
}

/* S_code is always called with mutex */
ptr S_code(tc, type, n) ptr tc; iptr type, n; {
    ptr p; iptr d;

    d = size_code(n);
    find_room(space_code, 0, type_typed_object, d, p);
    CODETYPE(p) = type;
    CODELEN(p) = n;
  /* we record the code modification here, even though we haven't
     even started modifying the code yet, since we always create
     and fill the code object within a critical section. */
    S_record_code_mod(tc, (uptr)&CODEIT(p,0), (uptr)n);
    return p;
}

ptr S_relocation_table(n) iptr n; {
    ptr tc = get_thread_context();
    ptr p; iptr d;

    d = size_reloc_table(n);
    thread_find_room(tc, typemod, d, p);
    RELOCSIZE(p) = n;
    return p;
}
