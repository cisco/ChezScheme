/* gc-par.c
 */

#define GCENTRY S_gc_par_entry
#define ENABLE_PARALLEL
#include "gc.c"

ptr S_gc_par(ptr tc, IGEN max_cg, IGEN min_tg, IGEN max_tg, ptr count_roots) {
  MAX_CG = max_cg;
  MIN_TG = min_tg;
  MAX_TG = max_tg;

  return S_gc_par_entry(tc, count_roots); 
}
