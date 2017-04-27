/* crepl.c
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

/*
This is a variant of main.c that implements a Scheme repl in C.
It's not at all useful, but it highlights how to invoke Scheme
without going through Sscheme_start.
  
Test in a workarea's examples subdirectory with:

( cd ../c ; ln -sf ../examples/crepl.c . )
( cd ../c ; make mainsrc=crepl.c )
sh -c 'SCHEMEHEAPDIRS=../boot/%m ../bin/scheme'
 */

#include "scheme.h"
#include <stdio.h>
#include <stdlib.h>

#define CALL0(who) Scall0(Stop_level_value(Sstring_to_symbol(who)))
#define CALL1(who, arg) Scall1(Stop_level_value(Sstring_to_symbol(who)), arg)

static void custom_init(void) {}

int main(int argc, char *argv[]) {
  int n, new_argc = 1, ignoreflags = 0;
  ptr p;

  Sscheme_init(NULL);

 /* process command-line arguments, registering boot and heap files */
  for (n = 1; n < argc; n += 1) {
    if (!ignoreflags && *argv[n] == '-') {
      switch (*(argv[n]+1)) {
        case '-': /* pass through remaining options */
          if (*(argv[n]+2) != 0) break;
          ignoreflags = 1;
          continue;
        case 'b': /* boot option, expects boot file pathname */
          if (*(argv[n]+2) != 0) break;
          if (++n == argc) {
            (void) fprintf(stderr,"\n-b option requires argument\n");
            exit(1);
          }
          Sregister_boot_file(argv[n]);
          continue;
        default:
          break;
      }
    }
    argv[new_argc++] = argv[n];
  }

 /* must call Sscheme_heap after registering boot and heap files
  * Sscheme_heap() completes the initialization of the Scheme system
  * and loads the boot or heap files.  Before loading boot files,
  * it calls custom_init(). */
  Sbuild_heap(argv[0], custom_init);

  for (;;) {
      CALL1("display", Sstring("* "));
      p = CALL0("read");
      if (Seof_objectp(p)) break;
      p = CALL1("eval", p);
      if (p != Svoid) CALL1("pretty-print", p);
  }
  CALL0("newline");

 /* must call Scheme_deinit after saving the heap and before exiting */
  Sscheme_deinit();

  exit(0);
}
