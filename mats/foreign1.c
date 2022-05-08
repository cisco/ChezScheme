/* foreign1.c
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

#ifdef _WIN32
#  define SCHEME_IMPORT
#  include "scheme.h"
#  undef EXPORT
#  define EXPORT extern __declspec (dllexport)
#else
#include "scheme.h"
#endif

EXPORT int id(int x) {
   return x;
}

EXPORT int idid(int x) {
    return id(id(x));
}

EXPORT int ididid(int x) {
    return idid(id(x));
}

EXPORT unsigned int iduns(unsigned int x) {
   return x;
}

EXPORT iptr idiptr(iptr x) {
   return x;
}

EXPORT iptr idiptr_addr(void) {
   return (iptr)&idiptr;
}

EXPORT double float_id(double x) {
   return x;
}

#ifdef _WIN32
#include <stdlib.h>

EXPORT int windows_strcpy(char *dst, char *src) {
  return strcpy(dst, src);
}

EXPORT int windows_strcmp(char *dst, char *src) {
  return strcmp(dst, src);
}

EXPORT void *windows_malloc(long n) {
  return malloc(n);
}

EXPORT void windows_free(void *x) {
  free(x);
}
#endif
