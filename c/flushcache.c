/* flushcache.c
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

#ifdef FLUSHCACHE
typedef struct {
  uptr start;
  uptr end;
} mod_range;

#define mod_range_start(x) (((mod_range *)&BVIT(x,0))->start)
#define mod_range_end(x) (((mod_range *)&BVIT(x,0))->end)

static uptr max_gap;

static ptr make_mod_range(ptr tc, uptr start, uptr end);

static ptr make_mod_range(ptr tc, uptr start, uptr end) {
  ptr bv = S_bytevector2(tc, sizeof(mod_range), space_new);
  mod_range_start(bv) = start;
  mod_range_end(bv) = end;
  return bv;
}

/* we record info per thread so flush in one prematurely for another doesn't prevent
   the other from doing its own flush...and also since it's not clear that flushing in one
   actually syncs caches across cores & processors */

void S_record_code_mod(ptr tc, uptr addr, uptr bytes) {
  uptr end = addr + bytes;
  ptr ls = CODERANGESTOFLUSH(tc);

  if (ls != Snil) {
    ptr last_mod = Scar(ls);
    uptr last_end = mod_range_end(last_mod);
    if (addr > last_end && addr - last_end < max_gap) {
#ifdef DEBUG
      printf("  record_code_mod merging %x %x and %x %x\n", mod_range_start(last_mod), last_end, addr, end); fflush(stdout);
#endif
      mod_range_end(last_mod) = end;
      return;
    }
  }

#ifdef DEBUG
      printf("  record_code_mod new range %x to %x\n", addr, end); fflush(stdout);
#endif
  CODERANGESTOFLUSH(tc) = S_cons_in(tc, space_new, 0, make_mod_range(tc, addr, end), ls);
  return;
}

extern void S_flush_instruction_cache(ptr tc) {
  ptr ls;

  for (ls = CODERANGESTOFLUSH(tc); ls != Snil; ls = Scdr(ls)) {
    S_doflush(mod_range_start(Scar(ls)), mod_range_end(Scar(ls)));
  }
  CODERANGESTOFLUSH(tc) = Snil;
}

extern void S_flushcache_init(void) {
  if (S_boot_time) {
    max_gap = S_flushcache_max_gap();
    if (max_gap < (uptr)(code_data_disp + byte_alignment)) {
      max_gap = (uptr)(code_data_disp + byte_alignment);
    }
  }
}
#else /* FLUSHCACHE */

extern void S_record_code_mod(UNUSED ptr tc, UNUSED uptr addr, UNUSED uptr bytes) {}
extern void S_flush_instruction_cache(UNUSED ptr tc) {}
extern void S_flushcache_init(void) { return; }

#endif /* FLUSHCACHE */
