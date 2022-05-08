/* symbol.c
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

ptr S_symbol_value(ptr sym) {
  if (SYMVAL(sym) == sunbound)
    S_error1("","~s is not bound", sym);
  return SYMVAL(sym);
}

void S_set_symbol_value(ptr sym, ptr val) {
  SETSYMVAL(sym, val);
  SETSYMCODE(sym, Sprocedurep(val) ? CLOSCODE(val) : S_G.nonprocedure_code);
}
