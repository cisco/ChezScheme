/* main.c
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

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "scheme.h"
#include "config.h"

/****
  CUSTOM_INIT may be defined as a function with the signature shown to
  perform boot-time initialization, e.g., registering foreign symbols.
****/
#ifndef CUSTOM_INIT
#define CUSTOM_INIT ((void (*)(void))0)
#endif /* CUSTOM_INIT */

/****
  ABNORMAL_EXIT may be defined as a function with the signature shown to
  take some action, such as printing a special error message or performing
  a nonlocal exit with longjmp, when the Scheme system exits abnormally,
  i.e., when an unrecoverable error occurs.  If left null, the default
  is to call exit(1).
****/
#ifndef ABNORMAL_EXIT
#define ABNORMAL_EXIT ((void (*)(void))0)
#endif /* ABNORMAL_EXIT */

#ifndef SCHEME_SCRIPT
#define SCHEME_SCRIPT "scheme-script"
#endif

static const char *path_last(const char *p) {
  const char *s;
#ifdef WIN32
  char c;
  if (((c = *p) >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z'))
    if (*(p + 1) == ':')
      p += 2;

  for (s = p; *s != 0; s += 1)
    if ((c = *s) == '/' || c == '\\') p = ++s;
#else /* WIN32 */
  for (s = p; *s != 0; s += 1) if (*s == '/') p = ++s;
#endif /* WIN32 */
  return p;
}

#if defined(WIN32) && !defined(__MINGW32__)
#define GETENV Sgetenv
#define GETENV_FREE free
int wmain(int argc, wchar_t* wargv[], wchar_t* wenvp[]) {
  const char** argv = (char**)malloc((argc + 1) * sizeof(char*));
  int i;
  for (i = 0; i < argc; i++) {
    wchar_t* warg = wargv[i];
    if (NULL == (argv[i] = Swide_to_utf8(warg))) {
      fprintf_s(stderr, "Invalid argument: %S\n", warg);
      exit(1);
    }
  }
  argv[argc] = NULL;
#else /* WIN32 */
#define GETENV getenv
#define GETENV_FREE (void)
int main(int argc, const char *argv[]) {
#endif /* WIN32 */
  int n, new_argc = 1;
#ifdef SAVEDHEAPS
  int compact = 1, savefile_level = 0;
  const char *savefile = (char *)0;
#endif /* SAVEDHEAPS */
  const char *execpath = argv[0];
  const char *scriptfile = (char *)0;
  const char *programfile = (char *)0;
  const char *libdirs = (char *)0;
  const char *libexts = (char *)0;
  int status;
  const char *arg;
  int quiet = 0;
  int eoc = 0;
  int optlevel = 0;
  int debug_on_exception = 0;
  int import_notify = 0;
  int compile_imported_libraries = 0;
  int exists_timestamp_mode = 0;
#ifdef FEATURE_EXPEDITOR
  int expeditor_enable = 1;
  const char *expeditor_history_file = "";  /* use "" for default location */
#endif /* FEATURE_EXPEDITOR */

  if (strcmp(Skernel_version(), VERSION) != 0) {
    (void) fprintf(stderr, "unexpected shared library version %s for %s version %s\n", Skernel_version(), execpath, VERSION);
    exit(1);
  }

  Sscheme_init(ABNORMAL_EXIT);

  if (strcmp(path_last(execpath), SCHEME_SCRIPT) == 0) {
    if (argc < 2) {
      (void) fprintf(stderr,"%s requires program-path argument\n", execpath);
      exit(1);
    }
    argv[0] = programfile = argv[1];
    n = 1;
    while (++n < argc) argv[new_argc++] = argv[n];
  } else {
   /* process command-line arguments, registering boot and heap files */
    for (n = 1; n < argc; n += 1) {
      arg = argv[n];
      if (strcmp(arg,"--") == 0) {
        while (++n < argc) argv[new_argc++] = argv[n];
      } else if (strcmp(arg,"-b") == 0 || strcmp(arg,"--boot") == 0) {
        if (++n == argc) {
          (void) fprintf(stderr,"%s requires argument\n", arg);
          exit(1);
        }
        Sregister_boot_executable_relative_file(execpath, argv[n]);
      } else if (strcmp(arg,"-B") == 0 || strcmp(arg,"--Boot") == 0) {
        if (++n == argc) {
          (void) fprintf(stderr,"%s requires argument\n", arg);
          exit(1);
        }
        Sregister_boot_relative_file(argv[n]);
      } else if (strcmp(arg,"--eedisable") == 0) {
  #ifdef FEATURE_EXPEDITOR
        expeditor_enable = 0;
  #endif /* FEATURE_EXPEDITOR */
      } else if (strcmp(arg,"--eehistory") == 0) {
        if (++n == argc) {
          (void) fprintf(stderr,"%s requires argument\n", arg);
          exit(1);
        }
  #ifdef FEATURE_EXPEDITOR
        if (strcmp(argv[n], "off") == 0)
          expeditor_history_file = (char *)0;
        else
          expeditor_history_file = argv[n];
  #endif /* FEATURE_EXPEDITOR */
      } else if (strcmp(arg,"-q") == 0 || strcmp(arg,"--quiet") == 0) {
        quiet = 1;
      } else if (strcmp(arg,"--retain-static-relocation") == 0) {
        Sretain_static_relocation();
      } else if (strcmp(arg,"--enable-object-counts") == 0) {
        eoc = 1;
#ifdef SAVEDHEAPS
      } else if (strcmp(arg,"-c") == 0 || strcmp(arg,"--compact") == 0) {
        compact = !compact;
      } else if (strcmp(arg,"-h") == 0 || strcmp(arg,"--heap") == 0) {
        if (++n == argc) {
          (void) fprintf(stderr,"%s requires argument\n", arg);
          exit(1);
        }
        Sregister_heap_file(argv[n]);
      } else if (strncmp(arg,"-s",2) == 0 &&
                 (savefile_level = -2,
                  *(arg+2) == 0 ||
                  *(arg+3) == 0 &&
                  ((savefile_level = *(arg+2) - '+' - 1) == -1 ||
                    (savefile_level = *(arg+2) - '0') >= 0 &&
                     savefile_level <= 9)) ||
                 strncmp(arg,"--saveheap",10) == 0 &&
                 (savefile_level = -2,
                  *(arg+10) == 0 ||
                  *(arg+11) == 0 &&
                  ((savefile_level = *(arg+2) - '+' - 1) == -1 ||
                    (savefile_level = *(arg+10) - '0') >= 0 &&
                     savefile_level <= 9))) {
        if (++n == argc) {
          (void) fprintf(stderr,"%s requires argument\n", arg);
          exit(1);
        }
        savefile = argv[n];
#else /* SAVEDHEAPS */
      } else if (strcmp(arg,"-c") == 0 || strcmp(arg,"--compact") == 0) {
        fprintf(stderr, "-c and --compact options are not presently supported\n");
        exit(1);
      } else if (strcmp(arg,"-h") == 0 || strcmp(arg,"--heap") == 0) {
        fprintf(stderr, "-h and --heap options are not presently supported\n");
        exit(1);
      } else if (strncmp(arg,"-s",2) == 0 || strncmp(arg,"--saveheap",10) == 0) {
        fprintf(stderr, "-s and --saveheap options are not presently supported\n");
        exit(1);
#endif /* SAVEDHEAPS */
      } else if (strcmp(arg,"--script") == 0) {
        if (++n == argc) {
          (void) fprintf(stderr,"%s requires argument\n", arg);
          exit(1);
        }
        scriptfile = argv[n];
        while (++n < argc) argv[new_argc++] = argv[n];
      } else if (strcmp(arg,"--optimize-level") == 0) {
        const char *nextarg;
        if (++n == argc) {
          (void) fprintf(stderr,"%s requires argument\n", arg);
          exit(1);
        }
        nextarg = argv[n];
        if (strcmp(nextarg,"0") == 0)
          optlevel = 0;
        else if (strcmp(nextarg,"1") == 0)
          optlevel = 1;
        else if (strcmp(nextarg,"2") == 0)
          optlevel = 2;
        else if (strcmp(nextarg,"3") == 0)
          optlevel = 3;
        else {
          (void) fprintf(stderr,"invalid optimize-level %s\n", nextarg);
          exit(1);
        }
      } else if (strcmp(arg,"--debug-on-exception") == 0) {
        debug_on_exception = 1;
      } else if (strcmp(arg,"--import-notify") == 0) {
        import_notify = 1;
      } else if (strcmp(arg,"--libexts") == 0) {
        if (++n == argc) {
          (void) fprintf(stderr,"%s requires argument\n", arg);
          exit(1);
        }
        libexts = argv[n];
      } else if (strcmp(arg,"--libdirs") == 0) {
        if (++n == argc) {
          (void) fprintf(stderr,"%s requires argument\n", arg);
          exit(1);
        }
        libdirs = argv[n];
      } else if (strcmp(arg,"--compile-imported-libraries") == 0) {
        compile_imported_libraries = 1;
      } else if (strcmp(arg,"--disable-library-timestamps") == 0) {
        exists_timestamp_mode = 1;
      } else if (strcmp(arg,"--program") == 0) {
        if (++n == argc) {
          (void) fprintf(stderr,"%s requires argument\n", arg);
          exit(1);
        }
        programfile = argv[n];
        while (++n < argc) argv[new_argc++] = argv[n];
      } else if (strcmp(arg,"--help") == 0) {
        fprintf(stderr,"usage: %s [options and files]\n", execpath);
        fprintf(stderr,"options:\n");
        fprintf(stderr,"  -q, --quiet                             suppress greeting and prompt\n");
        fprintf(stderr,"  --script <path>                         run as shell script\n");
        fprintf(stderr,"  --program <path>                        run rnrs program as shell script\n");
#ifdef WIN32
#define sep ";"
#else
#define sep ":"
#endif
        fprintf(stderr,"  --libdirs <dir>%s...                     set library directories\n", sep);
        fprintf(stderr,"  --libexts <ext>%s...                     set library extensions\n", sep);
        fprintf(stderr,"  --compile-imported-libraries            compile libraries before loading\n");
        fprintf(stderr,"  --import-notify                         enable import search messages\n");
        fprintf(stderr,"  --optimize-level <0 | 1 | 2 | 3>        set optimize-level\n");
        fprintf(stderr,"  --debug-on-exception                    on uncaught exception, call debug\n");
        fprintf(stderr,"  --eedisable                             disable expression editor\n");
        fprintf(stderr,"  --eehistory <off | path>                expression-editor history file\n");
        fprintf(stderr,"  --enable-object-counts                  have collector maintain object counts\n");
        fprintf(stderr,"  --retain-static-relocation              keep reloc info for compute-size, etc.\n");
        fprintf(stderr,"  -b <path>, --boot <path>                load boot file\n");
//        fprintf(stderr,"  -c, --compact                           toggle compaction flag\n");
//        fprintf(stderr,"  -h <path>, --heap <path>                load heap file\n");
//        fprintf(stderr,"  -s[<n>] <path>, --saveheap[<n>] <path>  save heap file\n");
        fprintf(stderr,"  --verbose                               trace boot/heap search process\n");
        fprintf(stderr,"  --version                               print version and exit\n");
        fprintf(stderr,"  --help                                  print help and exit\n");
        fprintf(stderr,"  --                                      pass through remaining args\n");
        exit(0);
      } else if (strcmp(arg,"--verbose") == 0) {
        Sset_verbose(1);     
      } else if (strcmp(arg,"--version") == 0) {
        fprintf(stderr,"%s\n", VERSION);
        exit(0);
      } else {
        argv[new_argc++] = arg;
      }
    }
  }

 /* must call Sbuild_heap after registering boot and heap files.
  * Sbuild_heap() completes the initialization of the Scheme system
  * and loads the boot or heap files.  If no boot or heap files have
  * been registered, the first argument to Sbuild_heap must be a
  * non-null path string; in this case, Sbuild_heap looks for
  * a heap or boot file named <name>.boot, where <name> is the last
  * component of the path.  If no heap files are loaded and
  * CUSTOM_INIT is non-null, Sbuild_heap calls CUSTOM_INIT just
  * prior to loading the boot file(s). */
  Sbuild_heap(execpath, CUSTOM_INIT);

#define CALL0(who) Scall0(Stop_level_value(Sstring_to_symbol(who)))
#define CALL1(who, arg) Scall1(Stop_level_value(Sstring_to_symbol(who)), arg)
#ifdef FunCRepl
  {
    ptr p;

    for (;;) {
        CALL1("display", Sstring("* "));
        p = CALL0("read");
        if (Seof_objectp(p)) break;
        p = CALL1("eval", p);
        if (p != Svoid) CALL1("pretty-print", p);
    }
    CALL0("newline");
    status = 0;
  }
#else /* FunCRepl */
  if (quiet) {
    CALL1("suppress-greeting", Strue);
    CALL1("waiter-prompt-string", Sstring(""));
  }
  if (eoc) {
    CALL1("enable-object-counts", Strue);
  }
  if (optlevel != 0) {
    CALL1("optimize-level", Sinteger(optlevel));
  }
  if (debug_on_exception != 0) {
    CALL1("debug-on-exception", Strue);
  }
  if (import_notify != 0) {
    CALL1("import-notify", Strue);
  }
  if (libdirs == 0) {
    char *cslibdirs = GETENV("CHEZSCHEMELIBDIRS");
    if (cslibdirs != 0) {
      CALL1("library-directories", Sstring_utf8(cslibdirs, -1));
      GETENV_FREE(cslibdirs);
    }
  } else {
    CALL1("library-directories", Sstring_utf8(libdirs, -1));
  }
  if (libexts == 0) {
    char *cslibexts = GETENV("CHEZSCHEMELIBEXTS");
    if (cslibexts != 0) {
      CALL1("library-extensions", Sstring_utf8(cslibexts, -1));
      GETENV_FREE(cslibexts);
    }
  } else {
    CALL1("library-extensions", Sstring_utf8(libexts, -1));
  }
  if (compile_imported_libraries != 0) {
    CALL1("compile-imported-libraries", Strue);
  }
  if (exists_timestamp_mode != 0) {
    CALL1("library-timestamp-mode", Sstring_to_symbol("exists"));
  }
#ifdef FEATURE_EXPEDITOR
 /* Senable_expeditor must be called before Scheme_start/Scheme_script (if at all) */
  if (!quiet && expeditor_enable) Senable_expeditor(expeditor_history_file);
#endif /* FEATURE_EXPEDITOR */

  if (scriptfile != (char *)0)
   /* Sscheme_script invokes the value of the scheme-script parameter */
    status = Sscheme_script(scriptfile, new_argc, argv);
  else if (programfile != (char *)0)
   /* Sscheme_program invokes the value of the scheme-program parameter */
    status = Sscheme_program(programfile, new_argc, argv);
  else {
   /* Sscheme_start invokes the value of the scheme-start parameter */
    status = Sscheme_start(new_argc, argv);
  }
#endif /* FunCRepl */

#ifdef SAVEDHEAPS
  if (status == 0 && savefile != (char *)0) {
      if (compact) Scompact_heap();
      Ssave_heap(savefile, savefile_level);
  }
#endif /* SAVEDHEAPS */

 /* must call Scheme_deinit after saving the heap and before exiting */
  Sscheme_deinit();

  exit(status);
}
