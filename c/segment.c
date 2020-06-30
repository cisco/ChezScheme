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

static void out_of_memory PROTO((void));
static void initialize_seginfo PROTO((seginfo *si, ISPC s, IGEN g));
static seginfo *allocate_segments PROTO((uptr nreq));
static void expand_segment_table PROTO((uptr base, uptr end, seginfo *si));
static void contract_segment_table PROTO((uptr base, uptr end));
static void add_to_chunk_list PROTO((chunkinfo *chunk, chunkinfo **pchunk_list));
static seginfo *sort_seginfo PROTO((seginfo *si, uptr n));
static seginfo *merge_seginfo PROTO((seginfo *si1, seginfo *si2));

void S_segment_init() {
  IGEN g; ISPC s; int i;

  if (!S_boot_time) return;

  S_chunks_full = NULL;
  for (i = PARTIAL_CHUNK_POOLS; i >= 0; i -= 1) S_chunks[i] = NULL;
  for (g = 0; g <= static_generation; g++) {
    for (s = 0; s <= max_real_space; s++) {
      S_G.occupied_segments[g][s] = NULL;
    }
  }
  S_G.number_of_nonstatic_segments = 0;
  S_G.number_of_empty_segments = 0;
}

static uptr membytes = 0;
static uptr maxmembytes = 0;

static void out_of_memory(void) {
  (void) fprintf(stderr,"out of memory\n");
  S_abnormal_exit();
}

#if defined(USE_MALLOC)
void *S_getmem(iptr bytes, IBOOL zerofill) {
  void *addr;

  if ((addr = malloc(bytes)) == (void *)0) out_of_memory();

  debug(printf("getmem(%p) -> %p\n", bytes, addr))
  if ((membytes += bytes) > maxmembytes) maxmembytes = membytes;
  if (zerofill) memset(addr, 0, bytes);
  return addr;
}

void S_freemem(void *addr, iptr bytes) {
  debug(printf("freemem(%p, %p)\n", addr, bytes))
  free(addr);
  membytes -= bytes;
}
#endif

#if defined(USE_VIRTUAL_ALLOC)
#include <winbase.h>
void *S_getmem(iptr bytes, IBOOL zerofill) {
  void *addr;

  if ((uptr)bytes < S_pagesize) {
    if ((addr = malloc(bytes)) == (void *)0) out_of_memory();
    debug(printf("getmem malloc(%p) -> %p\n", bytes, addr))
    if ((membytes += bytes) > maxmembytes) maxmembytes = membytes;
    if (zerofill) memset(addr, 0, bytes);
  } else {
    uptr n = S_pagesize - 1; iptr p_bytes = (iptr)(((uptr)bytes + n) & ~n);
    if ((addr = VirtualAlloc((void *)0, (SIZE_T)p_bytes, MEM_COMMIT, PAGE_EXECUTE_READWRITE)) == (void *)0) out_of_memory();
    if ((membytes += p_bytes) > maxmembytes) maxmembytes = membytes;
    debug(printf("getmem VirtualAlloc(%p => %p) -> %p\n", bytes, p_bytes, addr))
  }

  return addr;
}

void S_freemem(void *addr, iptr bytes) {
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
#define MAP_ANONYMOUS MAP_ANON
#endif
void *S_getmem(iptr bytes, IBOOL zerofill) {
  void *addr;

  if ((uptr)bytes < S_pagesize) {
    if ((addr = malloc(bytes)) == (void *)0) out_of_memory();
    debug(printf("getmem malloc(%p) -> %p\n", bytes, addr))
    if ((membytes += bytes) > maxmembytes) maxmembytes = membytes;
    if (zerofill) memset(addr, 0, bytes);
  } else {
    uptr n = S_pagesize - 1; iptr p_bytes = (iptr)(((uptr)bytes + n) & ~n);
#ifdef MAP_32BIT
    /* try for first 2GB of the memory space first of x86_64 so that we have a
       better chance of having short jump instructions */
    if ((addr = mmap(NULL, p_bytes, PROT_EXEC|PROT_WRITE|PROT_READ, MAP_PRIVATE|MAP_ANONYMOUS|MAP_32BIT, -1, 0)) == (void *)-1) {
#endif
      if ((addr = mmap(NULL, p_bytes, PROT_EXEC|PROT_WRITE|PROT_READ, MAP_PRIVATE|MAP_ANONYMOUS, -1, 0)) == (void *)-1) {
        out_of_memory();
        debug(printf("getmem mmap(%p) -> %p\n", bytes, addr))
      }
#ifdef MAP_32BIT
    }
#endif
    if ((membytes += p_bytes) > maxmembytes) maxmembytes = membytes;
    debug(printf("getmem mmap(%p => %p) -> %p\n", bytes, p_bytes, addr))
  }

  return addr;
}

void S_freemem(void *addr, iptr bytes) {
  if ((uptr)bytes < S_pagesize) {
    debug(printf("freemem free(%p, %p)\n", addr, bytes))
    free(addr);
    membytes -= bytes;
  } else {
    uptr n = S_pagesize - 1; iptr p_bytes = (iptr)(((uptr)bytes + n) & ~n);
    debug(printf("freemem munmap(%p, %p => %p)\n", addr, bytes, p_bytes))
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
  INT index = (INT)((n >> 2) + 1);

  return (index < PARTIAL_CHUNK_POOLS-1) ? index : PARTIAL_CHUNK_POOLS-1;
}

static void initialize_seginfo(seginfo *si, ISPC s, IGEN g) {
  INT d;

  si->space = s;
  si->generation = g;
  si->sorted = 0;
  si->min_dirty_byte = 0xff;
  si->trigger_ephemerons = NULL;
  for (d = 0; d < cards_per_segment; d += sizeof(ptr)) {
    iptr *dp = (iptr *)(si->dirty_bytes + d);
    /* fill sizeof(iptr) bytes at a time with 0xff */
    *dp = -1;
  }
}

iptr S_find_segments(s, g, n) ISPC s; IGEN g; iptr n; {
  chunkinfo *chunk, *nextchunk;
  seginfo *si, *nextsi, **prevsi;
  iptr nunused_segs, j;
  INT i, loser_index;

  if (g != static_generation) S_G.number_of_nonstatic_segments += n;

  debug(printf("attempting to find %d segments for space %d, generation %d\n", n, s, g))

  if (n == 1) {
    for (i = 0; i <= PARTIAL_CHUNK_POOLS; i++) {
      chunk = S_chunks[i];
      if (chunk != NULL) {
        si = chunk->unused_segs;
        chunk->unused_segs = si->next;

        if (chunk->unused_segs == NULL) {
          S_move_to_chunk_list(chunk, &S_chunks_full);
        } else if (i == PARTIAL_CHUNK_POOLS) {
          S_move_to_chunk_list(chunk, &S_chunks[PARTIAL_CHUNK_POOLS-1]);
        }

        chunk->nused_segs += 1;
        initialize_seginfo(si, s, g);
        si->next = S_G.occupied_segments[g][s];
        S_G.occupied_segments[g][s] = si;
        S_G.number_of_empty_segments -= 1;
        return si->number;
      }
    }
  } else {
    loser_index = (n == 2) ? 0 : find_index(n-1);
    for (i = find_index(n); i <= PARTIAL_CHUNK_POOLS; i += 1) {
      chunk = S_chunks[i];
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
                  S_move_to_chunk_list(chunk, &S_chunks_full);
                } else if (i == PARTIAL_CHUNK_POOLS) {
                  S_move_to_chunk_list(chunk, &S_chunks[PARTIAL_CHUNK_POOLS-1]);
                }
                chunk->nused_segs += n;
                nextsi->next = S_G.occupied_segments[g][s];
                S_G.occupied_segments[g][s] = si;
                for (j = n, nextsi = si; j > 0; j -= 1, nextsi = nextsi->next) {
                  initialize_seginfo(nextsi, s, g);
                }
                S_G.number_of_empty_segments -= n;
                return si->number;
              }
            }
          }
        }
        nextchunk = chunk->next;
        if (i != loser_index && i != PARTIAL_CHUNK_POOLS) {
          S_move_to_chunk_list(chunk, &S_chunks[loser_index]);
        }
        chunk = nextchunk;
      }
    }
  }

  /* we couldn't find space, so ask for more */
  si = allocate_segments(n);
  for (nextsi = si; n > 0; n -= 1, nextsi += 1) {
    initialize_seginfo(nextsi, s, g);
    /* add segment to appropriate list of occupied segments */
    nextsi->next = S_G.occupied_segments[g][s];
    S_G.occupied_segments[g][s] = nextsi;
  }
  return si->number;
}

/* allocate_segments(n)
 * allocates a group of n contiguous fresh segments, returning the
 * segment number of the first segment of the group.
 */
static seginfo *allocate_segments(nreq) uptr nreq; {
  uptr nact, bytes, base; void *addr;
  iptr i;
  chunkinfo *chunk; seginfo *si;

  nact = nreq < minimum_segment_request ? minimum_segment_request : nreq;

  bytes = (nact + 1) * bytes_per_segment;
  addr = S_getmem(bytes, 0);
  debug(printf("allocate_segments addr = %p\n", addr))

  base = addr_get_segment((uptr)addr + bytes_per_segment - 1);
  /* if the base of the first segment is the same as the base of the chunk, and
     the last segment isn't the last segment in memory (which could cause 'next' and 'end'
     pointers to wrap), we've actually got nact + 1 usable segments in this chunk */
  if (build_ptr(base, 0) == addr && base + nact != ((uptr)1 << (ptr_bits - segment_offset_bits)) - 1)
    nact += 1;

  chunk = S_getmem(sizeof(chunkinfo) + sizeof(seginfo) * nact, 0);
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
      si->next = chunk->unused_segs;
      chunk->unused_segs = si;
    }
  }

 /* account for trailing empty segments */
  if (nact > nreq) {
    S_G.number_of_empty_segments += nact - nreq;
    add_to_chunk_list(chunk, &S_chunks[PARTIAL_CHUNK_POOLS-1]);
  } else {
    add_to_chunk_list(chunk, &S_chunks_full);
  }

  return &chunk->sis[0];
}

void S_free_chunk(chunkinfo *chunk) {
  chunkinfo *nextchunk = chunk->next;
  contract_segment_table(chunk->base, chunk->base + chunk->segs);
  S_G.number_of_empty_segments -= chunk->segs;
  *chunk->prev = nextchunk;
  if (nextchunk != NULL) nextchunk->prev = chunk->prev;
  S_freemem(chunk->addr, chunk->bytes);
  S_freemem(chunk, sizeof(chunkinfo) + sizeof(seginfo) * chunk->segs);
}

/* retain approximately heap-reserve-ratio segments for every
 * nonempty nonstatic segment. */
void S_free_chunks(void) {
  iptr ntofree;
  chunkinfo *chunk, *nextchunk;

  ntofree = S_G.number_of_empty_segments -
    (iptr)(Sflonum_value(SYMVAL(S_G.heap_reserve_ratio_id)) * S_G.number_of_nonstatic_segments);

  for (chunk = S_chunks[PARTIAL_CHUNK_POOLS]; ntofree > 0 && chunk != NULL; chunk = nextchunk) {
    nextchunk = chunk->next;
    ntofree -= chunk->segs;
    S_free_chunk(chunk);
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
      S_segment_info[SEGMENT_T3_IDX(base)] = t2i = (t2table *)S_getmem(sizeof(t2table), 1);
    }
    t2 = t2i->t2;
#else
    t2 = S_segment_info;
#endif
    if ((t1i = t2[SEGMENT_T2_IDX(base)]) == NULL) {
      t2[SEGMENT_T2_IDX(base)] = t1i = (t1table *)S_getmem(sizeof(t1table), 1);
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
      S_freemem((void *)t1i, sizeof(t1table));
#ifdef segment_t3_bits
      if ((t2i->refcount -= 1) == 0) {
        S_freemem((void *)t2i, sizeof(t2table));
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
