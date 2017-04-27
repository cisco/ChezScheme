/* sort.h
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

#define mkmergesort(sort, merge, type, nil, lt, cdr)\
type sort(type ls, uptr len) {\
  if (len == 1) {\
    cdr(ls) = nil;\
    return ls;\
  } else {\
    uptr head_len, i; type tail;\
    head_len = len >> 1;\
    for (tail = ls, i = head_len; i != 0; i -= 1) tail = cdr(tail);\
    return merge(sort(ls, head_len), sort(tail, len - head_len));\
  }\
}\
type merge(type ls1, type ls2) {\
  type p; type *pp = &p;\
  for (;;) {\
    if (ls1 == nil) { *pp = ls2; break; }\
    if (ls2 == nil) { *pp = ls1; break; }\
    if (lt(ls2, ls1))\
      { *pp = ls2; pp = &cdr(ls2); ls2 = cdr(ls2); }\
    else\
      { *pp = ls1; pp = &cdr(ls1); ls1 = cdr(ls1); }\
  }\
  return p;\
}
