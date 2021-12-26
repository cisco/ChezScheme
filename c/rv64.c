#include "system.h"

#include <sys/types.h>
#include <sys/mman.h>

#ifdef FLUSHCACHE
oops, no S_flushcache_max_gap or S_doflush
#endif /* FLUSHCACHE */

void S_machine_init() {}
