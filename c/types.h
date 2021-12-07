/* types.h
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

/* C datatypes (mostly defined in equates.h or scheme.h)
 *    ptr: scheme object: (void *) on most platforms
 *   uptr: unsigned integer sizeof(uptr) == sizeof(ptr): typically unsigned long
 *   iptr: signed integer sizeof(uptr) == sizeof(ptr): typically long
 *     I8: 8-bit signed integer: typically char
 *    I16: 16-bit signed integer: typically short
 *    I32: 32-bit signed integer: typically int
 *    U32: 32-bit unsigned integer: typically unsigned int
 *    I64: 64-bit signed integer: typically long long
 *    U64: 64-bit unsigned integer: typically unsigned long long
 *  bigit: unsigned integer sizeof(bigit)*8 == bigit_bits
 *  bigit: unsigned integer sizeof(bigit)*8 == bigit_bits
 */

#if (bigit_bits == 32)
typedef U32 bigit;
typedef U64 bigitbigit;
typedef I32 ibigit;
typedef I64 ibigitbigit;
#endif

/* C signed/unsigned conventions:
 *  signed/unsigned distinction is felt in comparisons with zero, right
 *  shifts, multiplies, and divides.
 *
 *  general philosophy is to avoid surprises by using signed quantities,
 *  with a few exceptions.
 *
 *  use unsigned whenever shifting right.  ANSI C >> is undefined for
 *  negative numbers.  if arithmetic shift is desired, divide by the
 *  appropriate power of two and hope that the C compiler generates a
 *  shift instruction.
 *
 *  cast to uptr for ptr address computations.  this is really necessary
 *  only when shifting addresses, but we do it all the time since
 *  addresses are inherently unsigned values.
 *
 *  however, use signed (usually iptr) for lengths and array indices.
 *  this allows base cases like i < 0 when working backward from the end
 *  to the front of an array.  using uptr would give a slightly larger
 *  range in theory, but not in practice.
 */

/* documentary names for ints and unsigned ints */
typedef int INT;            /* honest-to-goodness C int */
typedef unsigned int UINT;  /* honest-to-goodness C unsigned int */
typedef int ITYPE;          /* ptr types */
typedef int ISPC;           /* storage manager spaces */
typedef int IGEN;           /* storage manager generations */
typedef int IDIRTYBYTE;     /* storage manager dirty bytes */
typedef int IBOOL;          /* int used exclusively as a boolean */
typedef int ICHAR;          /* int used exclusively as a character */
typedef int IFASLCODE;      /* fasl type codes */

#if (BUFSIZ < 4096)
#define SBUFSIZ 4096
#else
#define SBUFSIZ BUFSIZ
#endif

/* inline allocation --- mutex required */
/* find room allocates n bytes in space s and generation g into
 * destination x, tagged with ty, punting to find_more_room if
 * no space is left in the current segment.  n is assumed to be
 * an integral multiple of the object alignment. */
#define find_room(s, g, t, n, x) {\
    ptr X = S_G.next_loc[g][s];\
    S_G.next_loc[g][s] = (ptr)((uptr)X + (n));\
    if ((S_G.bytes_left[g][s] -= (n)) < 0) X = S_find_more_room(s, g, n, X);\
    (x) = TYPE(X, t);\
}

/* thread-local inline allocation --- no mutex required */
/* thread_find_room allocates n bytes in the local allocation area of
 * the thread (hence space new, generation zero) into destination x, tagged
 * with type t, punting to find_more_room if no space is left in the current
 * allocation area.  n is assumed to be an integral multiple of the object
 * alignment. */
#define thread_find_room(tc, t, n, x) {\
  ptr _tc = tc;\
  uptr _ap = (uptr)AP(_tc);\
  if ((uptr)n > ((uptr)EAP(_tc) - _ap)) {\
    (x) = S_get_more_room_help(_tc, _ap, t, n);\
  } else {\
    (x) = TYPE(_ap,t);\
    AP(_tc) = (ptr)(_ap + n);\
  }\
}

/* size of protected array used to store roots for the garbage collector */
#define max_protected 100

#define build_ptr(s,o) ((ptr)(((uptr)(s) << segment_offset_bits) | (uptr)(o)))
#define addr_get_segment(p) ((uptr)(p) >> segment_offset_bits)
#define ptr_get_segment(p) (((uptr)(p) + typemod - 1) >> segment_offset_bits)

#define SPACE(p) SegmentSpace(ptr_get_segment(p))
#define GENERATION(p) SegmentGeneration(ptr_get_segment(p))

#define ptr_align(size) (((size)+byte_alignment-1) & ~(byte_alignment-1))

typedef struct _seginfo {
  unsigned char space;                      /* space the segment is in */
  unsigned char generation;                 /* generation the segment is in */
  unsigned char sorted;                     /* sorted indicator---possibly to be incorporated into space flags? */
  octet min_dirty_byte;                     /* dirty byte for full segment, effectively min(dirty_bytes) */
  uptr number;                              /* the segment number */
  struct _chunkinfo *chunk;                 /* the chunk this segment belongs to */
  struct _seginfo *next;                    /* pointer to the next seginfo (used in occupied_segments and unused_segs */
  struct _seginfo **dirty_prev;             /* pointer to the next pointer on the previous seginfo in the DirtySegments list */
  struct _seginfo *dirty_next;              /* pointer to the next seginfo on the DirtySegments list */
  ptr trigger_ephemerons;                   /* ephemerons to re-check if object in segment is copied out */
  octet dirty_bytes[cards_per_segment];     /* one dirty byte per card */
} seginfo;

typedef struct _chunkinfo {
  void *addr;                               /* chunk starting address */
  iptr base;                                /* first segment */
  iptr bytes;                               /* size in bytes */
  iptr segs;                                /* size in segments */
  iptr nused_segs;                          /* number of segments currently in used use */ 
  struct _chunkinfo **prev;                 /* pointer to previous chunk's next */
  struct _chunkinfo *next;                  /* next chunk */
  struct _seginfo *unused_segs;             /* list of unused segments */
  struct _seginfo sis[0];                   /* one seginfo per segment */
} chunkinfo;

#ifdef segment_t2_bits
typedef struct _t1table {
  seginfo *t1[1<<segment_t1_bits];         /* table first to reduce access cost */
  iptr refcount;                           /* refcount last, since it's rarely accessed */
} t1table;
#ifdef segment_t3_bits
typedef struct _t2table {
  t1table *t2[1<<segment_t2_bits];         /* table first to reduce access cost */
  iptr refcount;                           /* refcount last, since it's rarely accessed */
} t2table;
#endif /* segment_t3_bits */
#endif /* segment_t2_bits */

/* CHUNK_POOLS determines the number of bins into which find_segment sorts chunks with
   varying lengths of empty segment chains.  it must be at least 1. */
#define PARTIAL_CHUNK_POOLS 8

/* dirty list table is conceptually a two-dimensional gen x gen table,
   but we use only the to_g entries for 0..from_g - 1.  say
   static_generation were 5 instead of 255, we don't need the 'X'
   entries in the table below, and they would clutter up our cache lines:

                     to_g
          0     1     2     3     4     5
       +-----+-----+-----+-----+-----+-----+
    0  |  X  |  X  |  X  |  X  |  X  |  X  |
       +-----+-----+-----+-----+-----+-----+
    1  |     |  X  |  X  |  X  |  X  |  X  |
       +-----+-----+-----+-----+-----+-----+
    2  |     |     |  X  |  X  |  X  |  X  |
       +-----+-----+-----+-----+-----+-----+
    3  |     |     |     |  X  |  X  |  X  |
       +-----+-----+-----+-----+-----+-----+
    4  |     |     |     |     |  X  |  X  |
       +-----+-----+-----+-----+-----+-----+
    5  |     |     |     |     |     |  X  |
       +-----+-----+-----+-----+-----+-----+

   so we create a vector instead of a matrix and roll our own version
   of row-major order.

       +-----+-----+-----+-----+----
       | 1,0 | 2,0 | 2,1 | 3,0 | ...
       +-----+-----+-----+-----+----

   any entry from_g, to_g can be found at from_g*(from_g-1)/2+to_g.
*/

#define DIRTY_SEGMENT_INDEX(from_g, to_g) ((((unsigned)((from_g)*((from_g)-1)))>>1)+to_g)
#define DIRTY_SEGMENT_LISTS DIRTY_SEGMENT_INDEX(static_generation, static_generation)

#define DirtySegments(from_g, to_g) S_G.dirty_segments[DIRTY_SEGMENT_INDEX(from_g, to_g)]

/* oblist */

typedef struct _bucket {
  ptr sym;
  struct _bucket *next;
} bucket;

typedef struct _bucket_list {
  struct _bucket *car;
  struct _bucket_list *cdr;
} bucket_list;

typedef struct _bucket_pointer_list {
  struct _bucket **car;
  struct _bucket_pointer_list *cdr;
} bucket_pointer_list;

/* size macros for variable-sized objects */

#define size_vector(n) ptr_align(header_size_vector + (n)*ptr_bytes)
#define size_closure(n) ptr_align(header_size_closure + (n)*ptr_bytes)
#define size_string(n) ptr_align(header_size_string + (n)*string_char_bytes)
#define size_fxvector(n) ptr_align(header_size_fxvector + (n)*ptr_bytes)
#define size_bytevector(n) ptr_align(header_size_bytevector + (n))
#define size_bignum(n) ptr_align(header_size_bignum + (n)*bigit_bytes)
#define size_code(n) ptr_align(header_size_code + (n))
#define size_reloc_table(n) ptr_align(header_size_reloc_table + (n)*ptr_bytes)
#define size_record_inst(n) ptr_align(n)
#define unaligned_size_record_inst(n) (n)

/* type tagging macros */

#define TYPE(x,type) ((ptr)((iptr)(x) - typemod + (type)))
#define UNTYPE(x,type) ((ptr)((iptr)(x) + typemod - (type)))
#define UNTYPE_ANY(x) ((ptr)(((iptr)(x) + (typemod - 1)) & ~(typemod - 1)))
#define TYPEBITS(x) ((iptr)(x) & (typemod - 1))
#define TYPEFIELD(x) (*(ptr *)UNTYPE(x, type_typed_object))

#define FIX(x) Sfixnum(x)
#define UNFIX(x) Sfixnum_value(x)

#define TYPEP(x,mask,type) (((iptr)(x) & (mask)) == (type))

/* reloc fields */
#define RELOC_EXTENDED_FORMAT(x) ((x)&reloc_extended_format)
#define RELOC_TYPE(x) (((x)>>reloc_type_offset)&reloc_type_mask)
#define RELOC_CODE_OFFSET(x) (((x)>>reloc_code_offset_offset)&reloc_code_offset_mask)
#define RELOC_ITEM_OFFSET(x) (((x)>>reloc_item_offset_offset)&reloc_item_offset_mask)
#define MAKE_SHORT_RELOC(ty,co,io) (((ty)<<reloc_type_offset)|((co)<<reloc_code_offset_offset)|((io)<<reloc_item_offset_offset))

/* derived type predicates */

#define GENSYMP(x) (Ssymbolp(x) && (!Sstringp(SYMNAME(x))))
#define FIXRANGE(x) ((uptr)((x) - most_negative_fixnum) <= (uptr)(most_positive_fixnum - most_negative_fixnum))
/* this breaks gcc 2.96
#define FIXRANGE(x) (Sfixnum_value(Sfixnum(x)) == x)
*/

#define DIRTYSET(lhs,rhs) S_dirty_set(lhs, rhs);

/* derived accessors/constructors */
#define FWDMARKER(p) FORWARDMARKER((uptr)UNTYPE_ANY(p))
#define FWDADDRESS(p) FORWARDADDRESS((uptr)UNTYPE_ANY(p))

#define ENTRYFRAMESIZE(x) RPHEADERFRAMESIZE((uptr)(x) - size_rp_header)
#define ENTRYOFFSET(x) RPHEADERTOPLINK((uptr)(x) - size_rp_header)
#define ENTRYLIVEMASK(x) RPHEADERLIVEMASK((uptr)(x) - size_rp_header)

#define PORTFD(x) ((iptr)PORTHANDLER(x))
#define PORTGZFILE(x) ((gzFile)(PORTHANDLER(x)))

#define CAAR(x) Scar(Scar(x))
#define CADR(x) Scar(Scdr(x))
#define CDAR(x) Scdr(Scar(x))
#define LIST1(x) Scons(x, Snil)
#define LIST2(x,y) Scons(x, LIST1(y))
#define LIST3(x,y,z) Scons(x, LIST2(y, z))
#define LIST4(x,y,z,w) Scons(x, LIST3(y, z, w))

#define REGARG(tc,i) ARGREG(tc,(i)-1)
#define FRAME(tc,i) (((ptr *)SFP(tc))[i])

#ifdef PTHREADS
typedef struct {
  volatile s_thread_t owner;
  volatile uptr count;
  s_thread_mutex_t pmutex;
} scheme_mutex_t;

#define get_thread_context() (ptr)s_thread_getspecific(S_tc_key)
/* deactivate thread prepares the thread for a possible collection.
   if it's the last active thread, it signals one of the threads
   waiting on the collect condition, if any, so that a collection
   can proceed.  if we happen to be the collecting thread, the active
   thread count is zero, in which case we don't signal.  collection
   is not permitted to happen when interrupts are disabled, so we
   don't let anything happen in that case. */
#define deactivate_thread(tc) {\
  if (ACTIVE(tc)) {\
    ptr code;\
    tc_mutex_acquire()\
    code = CP(tc);\
    if (Sprocedurep(code)) CP(tc) = code = CLOSCODE(code);\
    Slock_object(code);\
    SETSYMVAL(S_G.active_threads_id,\
     FIX(UNFIX(SYMVAL(S_G.active_threads_id)) - 1));\
    if (Sboolean_value(SYMVAL(S_G.collect_request_pending_id))\
        && SYMVAL(S_G.active_threads_id) == FIX(0)) {\
      s_thread_cond_signal(&S_collect_cond);\
    }\
    ACTIVE(tc) = 0;\
    tc_mutex_release()\
  }\
}
#define reactivate_thread(tc) {\
  if (!ACTIVE(tc)) {\
    tc_mutex_acquire()\
    SETSYMVAL(S_G.active_threads_id,\
     FIX(UNFIX(SYMVAL(S_G.active_threads_id)) + 1));\
    Sunlock_object(CP(tc));\
    ACTIVE(tc) = 1;\
    tc_mutex_release()\
  }\
}
/* S_tc_mutex_depth records the number of nested mutex acquires in
   C code on tc_mutex.  it is used by do_error to release tc_mutex
   the appropriate number of times.
*/
#define tc_mutex_acquire() {\
  S_mutex_acquire(&S_tc_mutex);\
  S_tc_mutex_depth += 1;\
}
#define tc_mutex_release() {\
  S_tc_mutex_depth -= 1;\
  S_mutex_release(&S_tc_mutex);\
}
#else
#define get_thread_context() (ptr)S_G.thread_context
#define deactivate_thread(tc) {}
#define reactivate_thread(tc) {}
#define tc_mutex_acquire() {}
#define tc_mutex_release() {}
#endif

#ifdef __MINGW32__
/* With MinGW on 64-bit Windows, setjmp/longjmp is not reliable. Using
   __builtin_setjmp/__builtin_longjmp is reliable, but
   __builtin_longjmp requires 1 as its second argument. So, allocate
   room in the buffer for a return value. */
# define JMPBUF_RET(jb) (*(int *)((char *)(jb)+sizeof(jmp_buf)))
# define CREATEJMPBUF() malloc(sizeof(jmp_buf)+sizeof(int))
# define FREEJMPBUF(jb) free(jb)
# define SETJMP(jb) (JMPBUF_RET(jb) = 0, __builtin_setjmp(jb), JMPBUF_RET(jb))
# define LONGJMP(jb,n) (JMPBUF_RET(jb) = n, __builtin_longjmp(jb, 1))
#else
# ifdef _WIN64
#  define CREATEJMPBUF() malloc(256)
#  define SETJMP(jb) S_setjmp(jb)
#  define LONGJMP(jb,n) S_longjmp(jb, n)
# else
/* assuming malloc will give us required alignment */
#  define CREATEJMPBUF() malloc(sizeof(jmp_buf))
#  define SETJMP(jb) _setjmp(jb)
#  define LONGJMP(jb,n) _longjmp(jb, n)
# endif
# define FREEJMPBUF(jb) free(jb)
#endif

#define DOUNDERFLOW\
 &CODEIT(CLOSCODE(S_lookup_library_entry(library_dounderflow, 1)),size_rp_header)

#define HEAP_VERSION_LENGTH 16
#define HEAP_MACHID_LENGTH 16
#define HEAP_STAMP_LENGTH 16

/* keep MAKE_FD in sync with io.ss make-fd */
#define MAKE_FD(fd) Sinteger(fd)
#define GET_FD(file) ((INT)Sinteger_value(file))

#define PTRFIELD(x,disp) (*(ptr *)((uptr)(x)+disp))
#define INITPTRFIELD(x,disp) (*(ptr *)((uptr)(x)+disp))
#define SETPTRFIELD(x,disp,y) DIRTYSET(((ptr *)((uptr)(x)+disp)),(y))

#define INCRGEN(g) (g = g == S_G.max_nonstatic_generation ? static_generation : g+1)
#define IMMEDIATE(x) (Sfixnump(x) || Simmediatep(x))
