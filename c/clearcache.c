/* clearcache.c
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

/* Implements cache flushing for typical architectures using mostly
   __clear_cache() and Linux facilities (when available). */

#include "system.h"

#include <sys/types.h>
#include <sys/mman.h>

#ifdef TARGET_OS_IPHONE
# include <libkern/OSCacheControl.h>
#endif

/* we don't count on having the right value for correctness,
 * but the right value will give maximum efficiency */
#define DEFAULT_L1_MAX_CACHE_LINE_SIZE 32

static int l1_max_cache_line_size;

/* flushcache_max_gap is the maximum gap between unmerged chunks of memory to be flushed */
INT S_flushcache_max_gap(void) {
  return l1_max_cache_line_size;
}

void S_doflush(uptr start, uptr end) {
#ifdef DEBUG
  printf("  doflush(%x, %x)\n", start, end); fflush(stdout);
#endif

#ifdef TARGET_OS_IPHONE
  sys_icache_invalidate((void *)start, (char *)end-(char *)start);
#else
  __clear_cache((char *)start, (char *)end);
# if defined(__clang__) && defined(__aarch64__) && !defined(__APPLE__)
  /* Seem to need an extra combination of barriers here to make up for
     something in Clang's __clear_cache() */
  asm volatile ("dsb ish\n\t"
                "isb"
                : : : "memory");
# endif
#endif
}

void S_machine_init(void) {
  int l1_dcache_line_size, l1_icache_line_size;

#if defined(__linux__) && defined(_SC_LEVEL1_DCACHE_LINESIZE)
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
