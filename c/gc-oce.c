/* gc-oce.c
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

#define GCENTRY S_gc_oce_entry
#define ENABLE_OBJECT_COUNTS
#define ENABLE_BACKREFERENCE
#define ENABLE_MEASURE
/* #define ENABLE_PARALLEL - could enable to preserve owenrship/creator information */
#include "gc.c"

ptr S_gc_oce(ptr tc, IGEN max_cg, IGEN min_tg, IGEN max_tg, ptr count_roots) {
  MAX_CG = max_cg;
  MIN_TG = min_tg;
  MAX_TG = max_tg;

  return S_gc_oce_entry(tc, count_roots); 
}
