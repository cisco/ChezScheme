/* segment.c
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

/*
Low-level Memory management strategy:
  * use getmem-allocated multiple-segment chunks of memory
  * maintain getmem-allocated list of chunks
  * maintain getmem-allocated segment info and dirty vector tables
  * after each collection, run through the list of chunks.  If all
    segments in a chunk are empty, the chunk is a candidate for return
    to the O/S.  Return (freemem) as many chunks as possible without going
    below a user-defined threshold of empty segments (determined as a
    multiple of the occupied nonstatic segments).  Bias return to the
    most recently allocated chunks.
  * getmem/freemem may be implemented with malloc/free; we use them
    relatively infrequently so performance isn't an issue.
*/

#define debug(x) ;
/* #define debug(x) {x; fflush(stdout);} */

#include "system.h"
#include "sort.h"
#include <sys/types.h>

static void out_of_memory(void);
static void initialize_seginfo(seginfo *si, thread_gc *creator, ISPC s, IGEN g);
static seginfo *allocate_segments(uptr nreq, IBOOL for_code);
static void expand_segment_table(uptr base, uptr end, seginfo *si);
static void contract_segment_table(uptr base, uptr end);
static void add_to_chunk_list(chunkinfo *chunk, chunkinfo **pchunk_list);
static seginfo *sort_seginfo(seginfo *si, uptr n);
static seginfo *merge_seginfo(seginfo *si1, seginfo *si2);

#if defined(WRITE_XOR_EXECUTE_CODE)
static void enable_code_write(ptr tc, IGEN maxg, IBOOL on, IBOOL current, void *hint, uptr hint_len);
#endif

void S_segment_init(void) {
  IGEN g; ISPC s; int i;

  if (!S_boot_time) return;

  S_chunks_full = NULL;
  S_code_chunks_full = NULL;
  for (i = PARTIAL_CHUNK_POOLS; i >= 0; i -= 1) {
    S_chunks[i] = NULL;
    S_code_chunks[i] = NULL;
  }
  for (g = 0; g <= static_generation; g++) {
    for (s = 0; s <= max_real_space; s++) {
      S_G.occupied_segments[g][s] = NULL;
    }
  }
  S_G.number_of_nonstatic_segments = 0;
  S_G.number_of_empty_segments = 0;

#ifndef PORTABLE_BYTECODE
  if (seginfo_space_disp != offsetof(seginfo, space))
    S_error_abort("seginfo_space_disp is wrong");
  if (seginfo_generation_disp != offsetof(seginfo, generation))
    S_error_abort("seginfo_generation_disp is wrong");
  if (seginfo_list_bits_disp != offsetof(seginfo, list_bits))
    S_error_abort("seginfo_list_bits_disp is wrong");
#endif
}

static uptr membytes = 0;
static uptr maxmembytes = 0;

static void out_of_memory(void) {
  (void) fprintf(stderr,"out of memory\n");
  S_abnormal_exit();
}

#if defined(USE_MMAP)
static int for_code_succeeded = 0;
static void w_and_x_problem(void) {
  (void) fprintf(stderr,
                 "allocation failed for code; maybe write+execute permission is not allowed\n");
  S_abnormal_exit();
}
#endif

#if defined(USE_MALLOC)
void *S_getmem(iptr bytes, IBOOL zerofill, UNUSED IBOOL for_code) {
  void *addr;

  if ((addr = malloc(bytes)) == (void *)0) out_of_memory();

  debug(printf("getmem(%p) -> %p\n", TO_VOIDP(bytes), addr))
  if ((membytes += bytes) > maxmembytes) maxmembytes = membytes;
  if (zerofill) memset(addr, 0, bytes);
  return addr;
}

void S_freemem(void *addr, iptr bytes, UNUSED IBOOL for_code) {
  debug(printf("freemem(%p, %p)\n", addr, TO_VOIDP(bytes)))
  free(addr);
  membytes -= bytes;
}
#endif

#if defined(USE_VIRTUAL_ALLOC)
# include <winbase.h>
void *S_getmem(iptr bytes, IBOOL zerofill, IBOOL for_code) {
  void *addr;

  if ((uptr)bytes < S_pagesize) {
    if ((addr = malloc(bytes)) == (void *)0) out_of_memory();
    debug(printf("getmem malloc(%p) -> %p\n", TO_VOIDP(bytes), addr))
    if ((membytes += bytes) > maxmembytes) maxmembytes = membytes;
    if (zerofill) memset(addr, 0, bytes);
  } else {
    uptr n = S_pagesize - 1; iptr p_bytes = (iptr)(((uptr)bytes + n) & ~n);
    int perm = (for_code ? PAGE_EXECUTE_READWRITE : PAGE_READWRITE);
    if ((addr = VirtualAlloc((void *)0, (SIZE_T)p_bytes, MEM_COMMIT, perm)) == (void *)0) out_of_memory();
    if ((membytes += p_bytes) > maxmembytes) maxmembytes = membytes;    
    debug(printf("getmem VirtualAlloc(%p => %p) -> %p\n", TO_VOIDP(bytes), TO_VOIDP(p_bytes), addr))
  }

  return addr;
}

void S_freemem(void *addr, iptr bytes, UNUSED IBOOL for_code) {
  if ((uptr)bytes < S_pagesize) {
    debug(printf("freemem free(%p, %p)\n", addr, bytes))
    membytes -= bytes;
    free(addr);
  } else {
    uptr n = S_pagesize - 1; iptr p_bytes = (iptr)(((uptr)bytes + n) & ~n);
    debug(printf("freemem VirtualFree(%p, %p => %p)\n", addr, bytes, p_bytes))
    membytes -= p_bytes;
    VirtualFree(addr, 0, MEM_RELEASE);
  }
}
#endif

#if defined(USE_MMAP)
#include <sys/mman.h>
#ifndef MAP_ANONYMOUS
# define MAP_ANONYMOUS MAP_ANON
#endif
#ifdef PORTABLE_BYTECODE
# undef S_PROT_CODE
# define S_PROT_CODE (PROT_WRITE | PROT_READ)
#endif
void *S_getmem(iptr bytes, IBOOL zerofill, IBOOL for_code) {
  void *addr;

  if ((uptr)bytes < S_pagesize) {
    if ((addr = malloc(bytes)) == (void *)0) out_of_memory();
    debug(printf("getmem malloc(%p) -> %p\n", TO_VOIDP(bytes), addr))
    if ((membytes += bytes) > maxmembytes) maxmembytes = membytes;
    if (zerofill) memset(addr, 0, bytes);
  } else {
    uptr n = S_pagesize - 1; iptr p_bytes = (iptr)(((uptr)bytes + n) & ~n);
    int perm = (for_code ? S_PROT_CODE : (PROT_WRITE | PROT_READ));
    int flags = (MAP_PRIVATE | MAP_ANONYMOUS) | (for_code ? S_MAP_CODE : 0);
#ifdef MAP_32BIT
    /* try for first 2GB of the memory space first of x86_64 so that we have a
       better chance of having short jump instructions */
    if ((addr = mmap(NULL, p_bytes, perm, flags|MAP_32BIT, -1, 0)) == (void *)-1) {
#endif
      if ((addr = mmap(NULL, p_bytes, perm, flags, -1, 0)) == (void *)-1) {
        if (for_code && !for_code_succeeded)
          w_and_x_problem();
        out_of_memory();
        debug(printf("getmem mmap(%p) -> %p\n", TO_VOIDP(bytes), addr))
      }
#ifdef MAP_32BIT
    }
#endif
    if (for_code) for_code_succeeded = 1;
    if ((membytes += p_bytes) > maxmembytes) maxmembytes = membytes;
    debug(printf("getmem mmap(%p => %p) -> %p\n", TO_VOIDP(bytes), TO_VOIDP(p_bytes), addr))
  }

  return addr;
}

void S_freemem(void *addr, iptr bytes, UNUSED IBOOL for_code) {
  if ((uptr)bytes < S_pagesize) {
    debug(printf("freemem free(%p, %p)\n", addr, TO_VOIDP(bytes)))
    free(addr);
    membytes -= bytes;
  } else {
    uptr n = S_pagesize - 1; iptr p_bytes = (iptr)(((uptr)bytes + n) & ~n);
    debug(printf("freemem munmap(%p, %p => %p)\n", addr, TO_VOIDP(bytes), TO_VOIDP(p_bytes)))
    munmap(addr, p_bytes);
    membytes -= p_bytes;
  }
}
#endif

void S_move_to_chunk_list(chunkinfo *chunk, chunkinfo **pchunk_list) {
  if ((*chunk->prev = chunk->next) != NULL) chunk->next->prev = chunk->prev;
  add_to_chunk_list(chunk, pchunk_list);
}

static void add_to_chunk_list(chunkinfo *chunk, chunkinfo **pchunk_list) {
  if ((chunk->next = *pchunk_list) != NULL) (*pchunk_list)->prev = &chunk->next;
  chunk->prev = pchunk_list;
  *pchunk_list = chunk;
}

#define SEGLT(x, y) ((x)->number < (y)->number)
#define SEGCDR(x) ((x)->next)
mkmergesort(sort_seginfo, merge_seginfo, seginfo *, NULL, SEGLT, SEGCDR)

static void sort_chunk_unused_segments(chunkinfo *chunk) {
  seginfo *si, *nextsi, *sorted, *unsorted; uptr n;

  /* bail out early if we find the unused segments list is already sorted */
  if ((unsorted = chunk->unused_segs)->sorted) return;

  /* find the sorted tail so we can just sort in the unsorted ones */
  si = unsorted;
  n = 1;
  for (;;) {
    si->sorted = 1;
    if ((nextsi = si->next) == NULL || nextsi->sorted) {
      sorted = nextsi;
      si->next = NULL;
      break;
    }
    si = nextsi;
    n += 1;
  }

  sorted = merge_seginfo(sort_seginfo(unsorted, n), sorted);

  chunk->unused_segs = sorted;
}

static INT find_index(iptr n) {
  UINT index = (UINT)(((uptr)n >> 2) + 1);

  return (index < PARTIAL_CHUNK_POOLS-1) ? index : PARTIAL_CHUNK_POOLS-1;
}

static void initialize_seginfo(seginfo *si, NO_THREADS_UNUSED thread_gc *creator, ISPC s, IGEN g) {
  INT d;

  si->space = s;
  si->generation = g;
  si->sorted = 0;
  si->old_space = 0;
  si->use_marks = 0;
  si->must_mark = 0;
#ifdef PTHREADS
  si->creator = creator;
#endif
  si->list_bits = NULL;
  si->min_dirty_byte = 0xff;
  for (d = 0; d < cards_per_segment; d += sizeof(ptr)) {
    iptr *dp = (iptr *)(si->dirty_bytes + d);
    /* fill sizeof(iptr) bytes at a time with 0xff */
    *dp = -1;
  }
  si->has_triggers = 0;
  si->trigger_ephemerons = 0;
  si->trigger_guardians = 0;
  si->marked_mask = NULL;
#ifdef PRESERVE_FLONUM_EQ
  si->forwarded_flonums = NULL;
#endif
  si->counting_mask = NULL;
  si->measured_mask = NULL;
  si->sweep_next = NULL;
#if defined(WRITE_XOR_EXECUTE_CODE)
  si->sweep_bytes = 0;
#endif
}

/* allocation mutex must be held */
iptr S_find_segments(thread_gc *creator, ISPC s, IGEN g, iptr n) {
  chunkinfo *chunk, *nextchunk, **chunks;
  seginfo *si, *nextsi, **prevsi;
  iptr nunused_segs, j;
  INT i, loser_index;
  IBOOL for_code = ((s == space_code));

  if (g != static_generation) S_G.number_of_nonstatic_segments += n;

  debug(printf("attempting to find %ld segments for space %d, generation %d\n", n, s, g))

  chunks = (for_code ? S_code_chunks : S_chunks);

  if (n == 1) {
    for (i = 0; i <= PARTIAL_CHUNK_POOLS; i++) {
      chunk = chunks[i];
      if (chunk != NULL) {
        si = chunk->unused_segs;
        chunk->unused_segs = si->next;

        if (chunk->unused_segs == NULL) {
          S_move_to_chunk_list(chunk, (for_code ? &S_code_chunks_full : &S_chunks_full));
        } else if (i == PARTIAL_CHUNK_POOLS) {
          S_move_to_chunk_list(chunk, &chunks[PARTIAL_CHUNK_POOLS-1]);
        }

        chunk->nused_segs += 1;
        initialize_seginfo(si, creator, s, g);
        si->next = S_G.occupied_segments[g][s];
        S_G.occupied_segments[g][s] = si;
        S_G.number_of_empty_segments -= 1;
        return si->number;
      }
    }
  } else {
    loser_index = (n == 2) ? 0 : find_index(n-1);
    for (i = find_index(n); i <= PARTIAL_CHUNK_POOLS; i += 1) {
      chunk = chunks[i];
      while (chunk != NULL) {
        if (n < (nunused_segs = (chunk->segs - chunk->nused_segs))) {
          sort_chunk_unused_segments(chunk);
          si = chunk->unused_segs;
          prevsi = &chunk->unused_segs;
          while (nunused_segs >= n) {
            nextsi = si;
            j = n - 1;
            for (;;) {
              nunused_segs -= 1;
              if (nextsi->number + 1 != nextsi->next->number) {
                si = nextsi->next;
                prevsi = &nextsi->next;
                break;
              }
              nextsi = nextsi->next;
              if (--j == 0) {
                *prevsi = nextsi->next;
                if (chunk->unused_segs == NULL) {
                  S_move_to_chunk_list(chunk, (for_code ? &S_code_chunks_full : &S_chunks_full));
                } else if (i == PARTIAL_CHUNK_POOLS) {
                  S_move_to_chunk_list(chunk, &chunks[PARTIAL_CHUNK_POOLS-1]);
                }
                chunk->nused_segs += n;
                nextsi->next = S_G.occupied_segments[g][s];
                S_G.occupied_segments[g][s] = si;
                for (j = n, nextsi = si; j > 0; j -= 1, nextsi = nextsi->next) {
                  initialize_seginfo(nextsi, creator, s, g);
                }
                S_G.number_of_empty_segments -= n;
                return si->number;
              }
            }
          }
        }
        nextchunk = chunk->next;
        if (i != loser_index && i != PARTIAL_CHUNK_POOLS) {
          S_move_to_chunk_list(chunk, &chunks[loser_index]);
        }
        chunk = nextchunk;
      }
    }
  }

  /* we couldn't find space, so ask for more */
  si = allocate_segments(n, for_code);
  for (nextsi = si, i = 0; i < n; i += 1, nextsi += 1) {
    initialize_seginfo(nextsi, creator, s, g);
    /* add segment to appropriate list of occupied segments */
    nextsi->next = S_G.occupied_segments[g][s];
    S_G.occupied_segments[g][s] = nextsi;
  }

  /* preemptively mark a huge allocation as immobile, since
     we don't want the GC to ever copy it */
  if (n > 128)
    si->must_mark = MUST_MARK_INFINITY;

  return si->number;
}

/* allocate_segments(n)
 * allocates a group of n contiguous fresh segments, returning the
 * segment number of the first segment of the group.
 */
static seginfo *allocate_segments(uptr nreq, UNUSED IBOOL for_code) {
  uptr nact, bytes, base; void *addr;
  iptr i;
  chunkinfo *chunk; seginfo *si;

  nact = nreq < minimum_segment_request ? minimum_segment_request : nreq;

  bytes = (nact + 1) * bytes_per_segment;
  addr = S_getmem(bytes, 0, for_code);
  debug(printf("allocate_segments addr = %p\n", addr))

    base = addr_get_segment((uptr)TO_PTR(addr) + bytes_per_segment - 1);
  /* if the base of the first segment is the same as the base of the chunk, and
     the last segment isn't the last segment in memory (which could cause 'next' and 'end'
     pointers to wrap), we've actually got nact + 1 usable segments in this chunk */
  if (build_ptr(base, 0) == TO_PTR(addr) && base + nact != ((uptr)1 << (ptr_bits - segment_offset_bits)) - 1)
    nact += 1;

  chunk = S_getmem(sizeof(chunkinfo) + sizeof(seginfo) * nact, 0, 0);
  debug(printf("allocate_segments chunk = %p\n", chunk))
  chunk->addr = addr;
  chunk->base = base;
  chunk->bytes = bytes;
  chunk->segs = nact;
  chunk->nused_segs = nreq;
  chunk->unused_segs = NULL;

  expand_segment_table(base, base + nact, &chunk->sis[0]);

  /* initialize seginfos */
  for (i = nact - 1; i >= 0; i -= 1) {
    si = &chunk->sis[i];
    si->chunk = chunk;
    si->number = i + base;
    if (i >= (iptr)nreq) {
      si->space = space_empty;
      si->generation = 0;
      si->sorted = 1; /* inserting in reverse order, so emptys are always sorted */
      si->old_space = 0;
      si->use_marks = 0;
      si->must_mark = 0;
      si->next = chunk->unused_segs;
      chunk->unused_segs = si;
    }
  }

 /* account for trailing empty segments */
  if (nact > nreq) {
    S_G.number_of_empty_segments += nact - nreq;
    add_to_chunk_list(chunk, &((for_code ? S_code_chunks : S_chunks)[PARTIAL_CHUNK_POOLS-1]));
  } else {
    add_to_chunk_list(chunk, (for_code ? &S_code_chunks_full : &S_chunks_full));
  }

  return &chunk->sis[0];
}

void S_free_chunk(chunkinfo *chunk, IBOOL for_code) {
  chunkinfo *nextchunk = chunk->next;
  contract_segment_table(chunk->base, chunk->base + chunk->segs);
  S_G.number_of_empty_segments -= chunk->segs;
  *chunk->prev = nextchunk;
  if (nextchunk != NULL) nextchunk->prev = chunk->prev;
  S_freemem(chunk->addr, chunk->bytes, for_code);
  S_freemem(chunk, sizeof(chunkinfo) + sizeof(seginfo) * chunk->segs, for_code);
}

/* retain approximately heap-reserve-ratio segments for every
 * nonempty nonstatic segment. */
void S_free_chunks(void) {
  iptr ntofree;
  chunkinfo *chunk, *code_chunk, *nextchunk= NULL, *code_nextchunk = NULL;

  ntofree = S_G.number_of_empty_segments -
    (iptr)(Sflonum_value(SYMVAL(S_G.heap_reserve_ratio_id)) * S_G.number_of_nonstatic_segments);

  for (chunk = S_chunks[PARTIAL_CHUNK_POOLS], code_chunk = S_code_chunks[PARTIAL_CHUNK_POOLS];
       ntofree > 0 && ((chunk != NULL) || (code_chunk != NULL));
       chunk = nextchunk, code_chunk = code_nextchunk) {
    if (chunk) {
      nextchunk = chunk->next;
      ntofree -= chunk->segs;
      S_free_chunk(chunk, 0);
    }
    if (code_chunk) {
      code_nextchunk = code_chunk->next;
      ntofree -= code_chunk->segs;
      S_free_chunk(code_chunk, 1);
    }
  }
}

uptr S_curmembytes(void) {
  return membytes;
}

uptr S_maxmembytes(void) {
  return maxmembytes;
}

void S_resetmaxmembytes(void) {
  maxmembytes = membytes;
}

void S_adjustmembytes(iptr amt) {
  if ((membytes += amt) > maxmembytes) maxmembytes = membytes;
}

static void expand_segment_table(uptr base, uptr end, seginfo *si) {
#ifdef segment_t2_bits
#ifdef segment_t3_bits
  t2table *t2i;
#endif
  t1table **t2, *t1i; uptr n;
#endif
  seginfo **t1, **t1end;

#ifdef segment_t2_bits
  while (base != end) {
#ifdef segment_t3_bits
    if ((t2i = S_segment_info[SEGMENT_T3_IDX(base)]) == NULL) {
      S_segment_info[SEGMENT_T3_IDX(base)] = t2i = (t2table *)S_getmem(sizeof(t2table), 1, 0);
    }
    t2 = t2i->t2;
#else
    t2 = S_segment_info;
#endif
    if ((t1i = t2[SEGMENT_T2_IDX(base)]) == NULL) {
      t2[SEGMENT_T2_IDX(base)] = t1i = (t1table *)S_getmem(sizeof(t1table), 1, 0);
#ifdef segment_t3_bits
      t2i->refcount += 1;
#endif
    }
    t1 = t1i->t1 + SEGMENT_T1_IDX(base);
    t1end = t1 + end - base < t1i->t1 + SEGMENT_T1_SIZE ? t1 + end - base : t1i->t1 + SEGMENT_T1_SIZE;
    n = t1end - t1;
    t1i->refcount += n;

    while (t1 < t1end) *t1++ = si++;
    base += n;
  }
#else
  t1 = S_segment_info + SEGMENT_T1_IDX(base);
  t1end = t1 + end - base;
  while (t1 < t1end) *t1++ = si++;
#endif
}

static void contract_segment_table(uptr base, uptr end) {
#ifdef segment_t2_bits
#ifdef segment_t3_bits
  t2table *t2i;
#endif
  t1table **t2, *t1i; uptr n;
#endif
  seginfo **t1, **t1end;

#ifdef segment_t2_bits
  while (base != end) {
#ifdef segment_t3_bits
    t2i = S_segment_info[SEGMENT_T3_IDX(base)];
    t2 = t2i->t2;
#else
    t2 = S_segment_info;
#endif
    t1i = t2[SEGMENT_T2_IDX(base)];
    t1 = t1i->t1 + SEGMENT_T1_IDX(base);
    t1end = t1 + end - base < t1i->t1 + SEGMENT_T1_SIZE ? t1 + end - base : t1i->t1 + SEGMENT_T1_SIZE;
    n = t1end - t1;
    if ((t1i->refcount -= n) == 0) {
      S_freemem((void *)t1i, sizeof(t1table), 0);
#ifdef segment_t3_bits
      if ((t2i->refcount -= 1) == 0) {
        S_freemem((void *)t2i, sizeof(t2table), 0);
        S_segment_info[SEGMENT_T3_IDX(base)] = NULL;
      } else {
        S_segment_info[SEGMENT_T3_IDX(base)]->t2[SEGMENT_T2_IDX(base)] = NULL;
      }
#else
      S_segment_info[SEGMENT_T2_IDX(base)] = NULL;
#endif
    } else {
      while (t1 < t1end) *t1++ = NULL;
    }
    base += n;
  }
#else
  t1 = S_segment_info + SEGMENT_T1_IDX(base);
  t1end = t1 + end - base;
  while (t1 < t1end) *t1++ = NULL;
#endif
}

/* Bracket all writes to `space_code` memory with calls to
   `S_thread_start_code_write` and `S_thread_end_code_write'.

   On a platform where a page cannot be both writable and executable
   at the same time (a.k.a. W^X), AND assuming that the disposition is
   thread-specific, the bracketing functions disable execution of the
   code's memory while enabling writing.

   A process-wide W^X disposition seems incompatible with the Chez
   Scheme rule that a foreign thread is allowed to invoke a callback
   (as long as the callback is immobile/locked) at any time --- even,
   say, while Scheme is collecting garbage and needs to write to
   executable pages.  However, on platforms where W^X is enforced
   (eg. iOS), we provide a best-effort implementation that flips pages
   between W and X for the minimal set of segments possible (depending
   on the context) in an effort to minimize the chances of a page
   being flipped while a thread is executing code off of it.
*/

void S_thread_start_code_write(WX_UNUSED ptr tc, WX_UNUSED IGEN maxg, WX_UNUSED IBOOL current,
                               WX_UNUSED void *hint, WX_UNUSED uptr hint_len) {
#if defined(WRITE_XOR_EXECUTE_CODE)
  enable_code_write(tc, maxg, 1, current, hint, hint_len);
#else
  S_ENABLE_CODE_WRITE(1);
#endif
}

void S_thread_end_code_write(WX_UNUSED ptr tc, WX_UNUSED IGEN maxg, WX_UNUSED IBOOL current,
                             WX_UNUSED void *hint, WX_UNUSED uptr hint_len) {
#if defined(WRITE_XOR_EXECUTE_CODE)
  enable_code_write(tc, maxg, 0, current, hint, hint_len);
#else
  S_ENABLE_CODE_WRITE(0);
#endif
}

#if defined(WRITE_XOR_EXECUTE_CODE)
# if defined(PTHREADS)
static IBOOL is_unused_seg(chunkinfo *chunk, seginfo *si) {
  uptr number;
  if (si->creator == NULL) {
    /* If the seginfo doesn't have a creator, then it's unused so we
       can skip the search. */
    return 1;
  }
  number = si->number;
  si = chunk->unused_segs;
  while (si != NULL) {
    if (si->number == number) {
      return 1;
    }
    si = si->next;
  }
  return 0;
}
# endif

static void enable_code_write(ptr tc, IGEN maxg, IBOOL on, IBOOL current, void *hint, uptr hint_len) {
  thread_gc *tgc;
  chunkinfo *chunk;
  seginfo *sip;
  iptr i, bytes;
  void *addr;
  INT flags = (on ? PROT_WRITE : PROT_EXEC) | PROT_READ;

  /* Flip only the segment hinted at by the caller. */
  if (maxg == 0 && hint != NULL) {
    uptr seg, start_seg, end_seg;
    start_seg = addr_get_segment(TO_PTR(hint));
    end_seg = addr_get_segment((uptr)TO_PTR(hint) + hint_len - 1);
    for (seg = start_seg; seg <= end_seg; seg++) {
      addr = TO_VOIDP(build_ptr(seg, 0));
      if (mprotect(addr, bytes_per_segment, flags) != 0) {
        S_error_abort("bad hint to enable_code_write");
      }
    }
    return;
  }

  /* Flip only the current allocation segments. */
  tgc = THREAD_GC(tc);
  if (maxg == 0 && current) {
    addr = TO_VOIDP(tgc->base_loc[0][space_code]);
    if (addr == NULL) {
      return;
    }
    bytes = ((char*)tgc->next_loc[0][space_code] - (char*)tgc->base_loc[0][space_code]
             + tgc->bytes_left[0][space_code] + allocation_segment_tail_padding);
    if (mprotect(addr, bytes, flags) != 0) {
      S_error_abort("failed to protect current allocation segments");
    }
    /* If disabling writes, turn on exec for recently-allocated
       segments in addition to the current segments. Clears the
       current sweep_next chain so must not be used durring
       collection. */
    if (!on) {
      while ((sip = tgc->sweep_next[0][space_code]) != NULL) {
        tgc->sweep_next[0][space_code] = sip->sweep_next;
        addr = TO_VOIDP(sip->sweep_start);
        bytes = sip->sweep_bytes;
        if (mprotect(addr, bytes, flags) != 0) {
          S_error_abort("failed to protect recent allocation segments");
        }
      }
    }
    return;
  }

  for (i = 0; i <= PARTIAL_CHUNK_POOLS; i++) {
    chunk = S_code_chunks[i];
    while (chunk != NULL) {
      addr = chunk->addr;
# if defined(PTHREADS)
      bytes = 0;
      if (chunk->nused_segs == 0) {
        /* None of the segments in the chunk are used so flip the bits
           for all of them in one go. */
        bytes = chunk->bytes;
      } else {
        /* Flip bits for whole runs of segs that are either unused or
           whose generation is within the range [0, maxg]. */
        int j;
        for (j = 0; j < chunk->segs; j++) {
          seginfo si = chunk->sis[j];
          /* When maxg is 0, limit the search to unused segments and
             segments that belong to the current thread. */
          if ((maxg == 0 && si.generation == 0 && si.creator == tgc) ||
              (maxg != 0 && si.generation <= maxg) ||
              (is_unused_seg(chunk, &si))) {
            bytes += bytes_per_segment;
          } else {
            if (bytes > 0) {
              debug(printf("mprotect flags=%d from=%p to=%p maxg=%d (interrupted)\n", flags, addr, TO_VOIDP((char *)addr + bytes), maxg))
              if (mprotect(addr, bytes, flags) != 0) {
                S_error_abort("mprotect failed");
              }
            }

            addr = TO_VOIDP((char *)chunk->addr + (j + 1) * bytes_per_segment);
            bytes = 0;
          }
        }
      }
# else
      bytes = chunk->bytes;
# endif
      if (bytes > 0) {
        debug(printf("mprotect flags=%d from=%p to=%p maxg=%d\n", flags, addr, TO_VOIDP((char *)addr + bytes), maxg))
        if (mprotect(addr, bytes, flags) != 0) {
          S_error_abort("mprotect failed");
        }
      }

      chunk = chunk->next;
    }
  }
}
#endif
