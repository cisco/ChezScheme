/* arm32le.c
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

  __clear_cache((char *)start, (char *)end);
}

void S_machine_init(void) {
  int l1_dcache_line_size, l1_icache_line_size;

#ifdef _SC_LEVEL1_DCACHE_LINESIZE
  if ((l1_dcache_line_size = sysconf(_SC_LEVEL1_DCACHE_LINESIZE)) <= 0)
#endif
    l1_dcache_line_size = DEFAULT_L1_MAX_CACHE_LINE_SIZE;
#ifdef _SC_LEVEL1_ICACHE_LINESIZE
  if ((l1_icache_line_size = sysconf(_SC_LEVEL1_ICACHE_LINESIZE)) <= 0)
#endif
    l1_icache_line_size = DEFAULT_L1_MAX_CACHE_LINE_SIZE;
  l1_max_cache_line_size = l1_dcache_line_size > l1_icache_line_size ? l1_dcache_line_size : l1_icache_line_size;
}
