#include "system.h"

#include <sys/types.h>
#include <sys/mman.h>
#include <unistd.h>

#define DEFAULT_L1_MAX_CACHE_LINE_SIZE 32

static int l1_max_cache_line_size;

/* flushcache_max_gap is the maximum gap between unmerged chunks of memory to be flushed */
INT S_flushcache_max_gap(void) {
  return l1_max_cache_line_size;
}

#ifdef FLUSHCACHE
//oops, no S_flushcache_max_gap or S_doflush

void S_doflush(uptr start, uptr end)
{
    __clear_cache((char *)start, (char *)end);
}
#endif /* FLUSHCACHE */

void S_machine_init() {
  int l1_dcache_line_size, l1_icache_line_size;

  if ((l1_dcache_line_size = sysconf(_SC_LEVEL1_DCACHE_LINESIZE)) <= 0) {
    l1_dcache_line_size = DEFAULT_L1_MAX_CACHE_LINE_SIZE;
  }
  if ((l1_icache_line_size = sysconf(_SC_LEVEL1_ICACHE_LINESIZE)) <= 0) {
    l1_icache_line_size = DEFAULT_L1_MAX_CACHE_LINE_SIZE;
  }
  l1_max_cache_line_size = l1_dcache_line_size > l1_icache_line_size ? l1_dcache_line_size : l1_icache_line_size;
}
