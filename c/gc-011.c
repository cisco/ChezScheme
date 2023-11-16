/* gc-011.c
 * Copyright 1984-2020 Cisco Systems, Inc.
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

#define GCENTRY S_gc_011_entry
#define MAX_CG 0
#define MIN_TG 1
#define MAX_TG 1
#define NO_NEWSPACE_MARKS
#include "gc.c"

void S_gc_011(ptr tc) {
  (void)S_gc_011_entry(tc, Sfalse);
}
