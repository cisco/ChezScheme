/* ppc32.c
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

#include <sys/types.h>
#include <sys/mman.h>

/* NB: when sysconf isn't helpful, hardcoding data max cache line size from PowerMac G4.
 * NB: this may cause illegal instruction error on machines with smaller cache line sizes. Also, it
 * NB: will make invalidating the caches slower on machines with larger cache line sizes. */
#define DEFAULT_L1_MAX_CACHE_LINE_SIZE 32

static int l1_dcache_line_size, l1_icache_line_size, l1_max_cache_line_size;

/* flushcache_max_gap is the maximum gap between unmerged chunks of memory to be flushed */
INT S_flushcache_max_gap(void) {
  return l1_max_cache_line_size;
}

void S_doflush(uptr start, uptr end) {
  uptr i;

#ifdef DEBUG
  printf("  doflush(%x, %x)\n", start, end); fflush(stdout);
#endif

  start &= ~(l1_max_cache_line_size - 1);
  end = (end + l1_max_cache_line_size - 1) & ~(l1_max_cache_line_size - 1);

  for(i = start; i < end; i += l1_dcache_line_size) {
    __asm__ __volatile__ ("dcbst 0, %0" :: "r" (i));
  }
  __asm__ __volatile__ ("sync");

  for(i = start; i < end; i += l1_icache_line_size) {
    __asm__ __volatile__ ("icbi 0, %0" :: "r" (i));
  }
  __asm__ __volatile__ ("sync ; isync");
}

void S_machine_init(void) {
#if defined(__linux__)
  if ((l1_dcache_line_size = sysconf(_SC_LEVEL1_DCACHE_LINESIZE)) <= 0) {
    l1_dcache_line_size = DEFAULT_L1_MAX_CACHE_LINE_SIZE;
  }
  if ((l1_icache_line_size = sysconf(_SC_LEVEL1_ICACHE_LINESIZE)) <= 0) {
    l1_icache_line_size = DEFAULT_L1_MAX_CACHE_LINE_SIZE;
  }
#else
  l1_dcache_line_size = DEFAULT_L1_MAX_CACHE_LINE_SIZE;
  l1_icache_line_size = DEFAULT_L1_MAX_CACHE_LINE_SIZE;
#endif
  l1_max_cache_line_size = l1_dcache_line_size > l1_icache_line_size ? l1_dcache_line_size : l1_icache_line_size;
}
