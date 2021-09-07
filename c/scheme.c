/* scheme.c
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
#include "config.h"
#include <setjmp.h>
#include <limits.h>
#ifdef WIN32
#include <io.h>
#else
#include <sys/time.h>
#endif
#include <fcntl.h>
#include <stddef.h>

#ifndef O_BINARY
#define O_BINARY 0
#endif /* O_BINARY */

static INT boot_count;
static IBOOL verbose;

typedef enum { UNINITIALIZED, BOOTING, RUNNING, DEINITIALIZED } heap_state;
static heap_state current_state = UNINITIALIZED;

/***************************************************************************/
/* INITIALIZATION SUPPORT */

/* locally defined functions */
static void main_init PROTO((void));
static void idiot_checks PROTO((void));
static INT run_script PROTO((const char *who, const char *scriptfile, INT argc, const char *argv[], IBOOL programp));

extern void scheme_include(void);
  
static void main_init() {
    ptr tc = get_thread_context();
    ptr p;
    INT i;

  /* create dependency for linker */
    scheme_statics();

  /* force thread inline allocation to go through find_room until ready */
    AP(tc) = (ptr)0;
    EAP(tc) = (ptr)0;
    REAL_EAP(tc) = (ptr)0;
  /* set up dummy CP so locking in read/write/Scall won't choke */
    CP(tc) = Svoid;
    CODERANGESTOFLUSH(tc) = Snil;

    if (S_boot_time) S_G.protect_next = 0;

    S_segment_init();
    S_alloc_init();
    S_thread_init();
    S_intern_init();
    S_gc_init();
    S_number_init();
    S_schsig_init();
    S_new_io_init();
    S_print_init();
    S_stats_init();
    S_foreign_init();
    S_prim_init();
    S_prim5_init();
    S_fasl_init();
    S_machine_init();
    S_flushcache_init(); /* must come after S_machine_init(); */
#ifdef FEATURE_EXPEDITOR
    S_expeditor_init();
#endif /* FEATURE_EXPEDITOR */

    if (!S_boot_time) return;

    S_protect(&S_G.profile_counters);
    S_G.profile_counters = Snil;

    FXLENGTHBV(tc) = p = S_bytevector(256);
    for (i = 0; i < 256; i += 1) {
      BVIT(p, i) =
       (iptr)FIX(i & 0x80 ? 8 : i & 0x40 ? 7 : i & 0x20 ? 6 : i & 0x10 ? 5 :
                 i & 0x08 ? 4 : i & 0x04 ? 3 : i & 0x02 ? 2 : i & 0x01 ? 1 : 0);
    }

    FXFIRSTBITSETBV(tc) = p = S_bytevector(256);
    for (i = 0; i < 256; i += 1) {
      BVIT(p, i) =
       (iptr)FIX(i & 0x01 ? 0 : i & 0x02 ? 1 : i & 0x04 ? 2 : i & 0x08 ? 3 :
                 i & 0x10 ? 4 : i & 0x20 ? 5 : i & 0x40 ? 6 : i & 0x80 ? 7 : 0);
    }

    NULLIMMUTABLEVECTOR(tc) = S_null_immutable_vector();
    NULLIMMUTABLEFXVECTOR(tc) = S_null_immutable_fxvector();
    NULLIMMUTABLEBYTEVECTOR(tc) = S_null_immutable_bytevector();
    NULLIMMUTABLESTRING(tc) = S_null_immutable_string();

    PARAMETERS(tc) = S_G.null_vector;
    for (i = 0 ; i < virtual_register_count ; i += 1) {
      VIRTREG(tc, i) = FIX(0);
    }

    p = S_code(tc, type_code, size_rp_header);
    CODERELOC(p) = S_relocation_table(0);
    CODENAME(p) = Sfalse;
    CODEARITYMASK(p) = FIX(0);
    CODEFREE(p) = 0;
    CODEINFO(p) = Sfalse;
    CODEPINFOS(p) = Snil;
    RPHEADERFRAMESIZE(&CODEIT(p, 0)) = 0;
    RPHEADERLIVEMASK(&CODEIT(p, 0)) = 0;
    RPHEADERTOPLINK(&CODEIT(p, 0)) =
       (uptr)&RPHEADERTOPLINK(&CODEIT(p, 0)) - (uptr)p;
    S_protect(&S_G.dummy_code_object);
    S_G.dummy_code_object = p;

    S_protect(&S_G.error_invoke_code_object);
    S_G.error_invoke_code_object = Snil;
    S_protect(&S_G.invoke_code_object);
    S_G.invoke_code_object = Snil;

    S_protect(&S_G.active_threads_id);
    S_G.active_threads_id = S_intern((const unsigned char *)"$active-threads");
    S_set_symbol_value(S_G.active_threads_id, FIX(0));

    S_protect(&S_G.heap_reserve_ratio_id);
    S_G.heap_reserve_ratio_id = S_intern((const unsigned char *)"$heap-reserve-ratio");
    SETSYMVAL(S_G.heap_reserve_ratio_id, Sflonum(default_heap_reserve_ratio));

    S_protect(&S_G.scheme_version_id);
    S_G.scheme_version_id = S_intern((const unsigned char *)"$scheme-version");
    S_protect(&S_G.make_load_binary_id);
    S_G.make_load_binary_id = S_intern((const unsigned char *)"$make-load-binary");
    S_protect(&S_G.load_binary);
    S_G.load_binary = Sfalse;
}

static ptr fixtest = FIX(-1);

static void idiot_checks() {
  IBOOL oops = 0;

  if (bytes_per_segment < S_pagesize) {
    fprintf(stderr, "bytes_per_segment (%x) < S_pagesize (%lx)\n",
              bytes_per_segment, (long)S_pagesize);
    oops = 1;
  }
  if (sizeof(iptr) != sizeof(ptr)) {
    fprintf(stderr, "sizeof(iptr) [%ld] != sizeof(ptr) [%ld]\n",
              (long)sizeof(iptr), (long)sizeof(ptr));
    oops = 1;
  }
  if (sizeof(uptr) != sizeof(ptr)) {
    fprintf(stderr, "sizeof(uptr) [%ld] != sizeof(ptr) [%ld]\n",
              (long)sizeof(uptr), (long)sizeof(ptr));
    oops = 1;
  }
  if (sizeof(ptr) * 8 != ptr_bits) {
    fprintf(stderr, "sizeof(ptr) * 8 [%ld] != ptr_bits [%d]\n",
              (long)sizeof(ptr), ptr_bits);
    oops = 1;
  }
  if (sizeof(int) * 8 != int_bits) {
    fprintf(stderr, "sizeof(int) * 8 [%ld] != int_bits [%d]\n",
              (long)sizeof(int), int_bits);
    oops = 1;
  }
  if (sizeof(short) * 8 != short_bits) {
    fprintf(stderr, "sizeof(short) * 8 [%ld] != short_bits [%d]\n",
              (long)sizeof(short), short_bits);
    oops = 1;
  }
  if (sizeof(long) * 8 != long_bits) {
    fprintf(stderr, "sizeof(long) * 8 [%ld] != long_bits [%d]\n",
              (long)sizeof(long), long_bits);
    oops = 1;
  }
#ifndef WIN32
  if (sizeof(long long) * 8 != long_long_bits) {
    fprintf(stderr, "sizeof(long long) * 8 [%ld] != long_long_bits [%d]\n",
              (long)sizeof(long long), long_long_bits);
    oops = 1;
  }
#endif
  if (sizeof(wchar_t) * 8 != wchar_bits) {
    fprintf(stderr, "sizeof(wchar_t) * 8 [%ld] != wchar_bits [%d]\n",
              (long)sizeof(wchar_t), wchar_bits);
    oops = 1;
  }
  if (sizeof(size_t) * 8 != size_t_bits) {
    fprintf(stderr, "sizeof(size_t) * 8 [%ld] != size_t_bits [%d]\n",
              (long)sizeof(size_t), size_t_bits);
    oops = 1;
  }
#ifndef WIN32
  if (sizeof(ssize_t) * 8 != size_t_bits) {
    fprintf(stderr, "sizeof(ssize_t) * 8 [%ld] != size_t_bits [%d]\n",
              (long)sizeof(ssize_t), size_t_bits);
    oops = 1;
  }
#endif
  if (sizeof(ptrdiff_t) * 8 != ptrdiff_t_bits) {
    fprintf(stderr, "sizeof(ptrdiff_t) * 8 [%ld] != ptrdiff_t_bits [%d]\n",
              (long)sizeof(ptrdiff_t), ptrdiff_t_bits);
    oops = 1;
  }
  if (sizeof(time_t) * 8 != time_t_bits) {
    fprintf(stderr, "sizeof(time_t) * 8 [%ld] != time_t_bits [%d]\n",
              (long)sizeof(time_t), time_t_bits);
    oops = 1;
  }
  if (sizeof(bigit) * 8 != bigit_bits) {
    fprintf(stderr, "sizeof(bigit) * 8 [%ld] != bigit_bits [%d]\n",
              (long)sizeof(bigit), bigit_bits);
    oops = 1;
  }
  if (sizeof(bigitbigit) != 2 * sizeof(bigit)) {
    fprintf(stderr, "sizeof(bigitbigit) [%ld] != sizeof(bigit) [%ld] * 2\n",
              (long)sizeof(bigitbigit), (long)sizeof(bigit));
    oops = 1;
  }
  if (sizeof(char) != 1) {
    fprintf(stderr, "sizeof(char) [%ld] != 1\n", (long)sizeof(char));
    oops = 1;
  }
  if (sizeof(I8) != 1) {
    fprintf(stderr, "sizeof(I8) [%ld] != 1\n", (long)sizeof(I8));
    oops = 1;
  }
  if (sizeof(U8) != 1) {
    fprintf(stderr, "sizeof(U8) [%ld] != 1\n", (long)sizeof(U8));
    oops = 1;
  }
  if (sizeof(I16) != 2) {
    fprintf(stderr, "sizeof(I16) [%ld] != 2\n", (long)sizeof(I16));
    oops = 1;
  }
  if (sizeof(U16) != 2) {
    fprintf(stderr, "sizeof(U16) [%ld] != 2\n", (long)sizeof(U16));
    oops = 1;
  }
  if (sizeof(I32) != 4) {
    fprintf(stderr, "sizeof(I32) [%ld] != 4\n", (long)sizeof(I32));
    oops = 1;
  }
  if (sizeof(U32) != 4) {
    fprintf(stderr, "sizeof(U32) [%ld] != 4\n", (long)sizeof(U32));
    oops = 1;
  }
  if (sizeof(I64) != 8) {
    fprintf(stderr, "sizeof(I64) [%ld] != 8\n", (long)sizeof(I64));
    oops = 1;
  }
  if (sizeof(U64) != 8) {
    fprintf(stderr, "sizeof(U64) [%ld] != 8\n", (long)sizeof(U64));
    oops = 1;
  }
  if (sizeof(string_char) != string_char_bytes) {
    fprintf(stderr, "sizeof(string_char) [%ld] != string_char_bytes [%d]\n", (long)sizeof(string_char), string_char_bytes);
    oops = 1;
  }
  if (UNFIX(fixtest) != -1) {
    fprintf(stderr, "UNFIX operation failed\n");
    oops = 1;
  }
  if (strlen(VERSION)+1 > HEAP_VERSION_LENGTH) {
    fprintf(stderr, "insufficient space for version in heap header\n");
    oops = 1;
  }
  if (strlen(MACHINE_TYPE)+1 > HEAP_MACHID_LENGTH) {
    fprintf(stderr, "insufficient space for machine id in heap header\n");
    oops = 1;
  }
#define big 0
#define little 1
  if (native_endianness == big) {
    uptr x[1];
    *x = 1;
    if (*(char *)x != 0) {
      fprintf(stderr, "endianness claimed to be big, appears to be little\n");
      oops = 1;
    }
  } else {
    uptr x[1];
    *x = 1;
    if (*(char *)x == 0) {
      fprintf(stderr, "endianness claimed to be little, appears to be big\n");
      oops = 1;
    }
  }

  if (sizeof(bucket_pointer_list) != sizeof(bucket_list)) {
    /* gc repurposes bucket_lists for bucket_pointer lists, so they'd better have the same size */
    fprintf(stderr, "bucket_pointer_list and bucket_list have different sizes\n");
    oops = 1;
  }

  if ((cards_per_segment & (sizeof(iptr) - 1)) != 0) {
    /* gc sometimes processes dirty bytes sizeof(iptr) bytes at a time */
    fprintf(stderr, "cards_per_segment is not a multiple of sizeof(iptr)\n");
    oops = 1;
  }
  if (((uptr)(&((seginfo *)0)->dirty_bytes[0]) & (sizeof(iptr) - 1)) != 0) {
    /* gc sometimes processes dirty bytes sizeof(iptr) bytes at a time */
    fprintf(stderr, "dirty_bytes[0] is not iptr-aligned wrt to seginfo struct\n");
    oops = 1;
  }
  if (!Sfixnump(type_vector | ~mask_vector)) {
    /* gc counts on vector type/length looking like a fixnum, so it can put vectors in space_impure */
    fprintf(stderr, "vector type/length field does not look like a fixnum\n");
    oops = 1;
  }

  if (oops) S_abnormal_exit();
}

/***************************************************************************/
/* SUPPORT FOR CALLING INTO SCHEME */

/* locally defined functions */
static ptr boot_call PROTO((ptr tc, ptr p, INT n));
static void check_ap PROTO((ptr tc));

/* arguments and ac0 set up */
static ptr boot_call(tc, p, n) ptr tc; ptr p; INT n; {
    AC1(tc) = p;
    CP(tc) = Svoid; /* don't have calling code object */

    AC0(tc) = (ptr)(uptr)n;
    S_call_help(tc, 0, 0);
    check_ap(tc);

    CP(tc) = Svoid; /* leave clean so direct Scall won't choke */

    switch ((iptr)AC1(tc)) {
        case 1:
            p = AC0(tc);
            break;
        case 0:
            p = Svoid;
            break;
        default:
            p = S_get_scheme_arg(tc, 1);
            break;
    }
    return p;
}

static void check_ap(tc) ptr tc; {
    if ((uptr)AP(tc) & (byte_alignment - 1)) {
        (void) fprintf(stderr, "ap is not double word aligned\n");
        S_abnormal_exit();
    }
    if ((ptr *)AP(tc) > (ptr *)EAP(tc)) {
        (void) fprintf(stderr, "ap is greater than eap\n");
        S_abnormal_exit();
    }
}

void S_generic_invoke(tc, code) ptr tc; ptr code; {
#if defined(PPCAIX)
    struct {caddr_t entry, toc, static_link;} hdr;
    hdr.entry = (caddr_t)&CODEIT(code,0);
    hdr.toc = (caddr_t)0;
    hdr.static_link = (caddr_t)0;
    (*((void (*) PROTO((ptr)))(void *)&hdr))(tc);
#elif defined(PPCNT)
  /* under NT, function headers contain no static link */
    struct {I32 entry, toc;} hdr;
    typedef void (*ugly)(ptr);
    ugly p;
    hdr.entry = (I32)&CODEIT(code,0);
    hdr.toc = (I32)0;
  /* MSVC++ bombs with internal compiler error if we don't split this up */
    p = (ugly)&hdr;
    p(tc);
#elif defined(PARISC)
    struct {I32 entry, env;} hdr;
    typedef void (*ugly)(ptr);
    ugly p;
    hdr.entry = (I32)&CODEIT(code,0);
    hdr.env = (I32)0;
    p = (ugly)((I32)&hdr + 2);
    p(tc);
#elif defined(WIN32) && !defined(__MINGW32__)
    __try {
      (*((void (*) PROTO((ptr)))(void *)&CODEIT(code,0)))(tc);
    }
    __except(GetExceptionCode() == EXCEPTION_ACCESS_VIOLATION ?
             EXCEPTION_EXECUTE_HANDLER : EXCEPTION_CONTINUE_SEARCH)
    {
        if (S_pants_down)
            S_error_abort("nonrecoverable invalid memory reference");
        else
            S_error_reset("invalid memory reference");
    }
#else
    (*((void (*) PROTO((ptr)))(void *)&CODEIT(code,0)))(tc);
#endif
}

/***************************************************************************/
/* MISCELLANEOUS HELPERS */

/* locally defined functions */
static IBOOL next_path PROTO((char *path, const char *name, const char *ext, const char **sp, const char **dsp));
static const char *path_last PROTO((const char *path));
static char *get_defaultheapdirs PROTO((void));

static const char *path_last(p) const char *p; {
  const char *s;
#ifdef WIN32
  char c;

  if ((c = *p) >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z')
    if (*(p + 1) == ':')
      p += 2;
#endif

  for (s = p; *s != 0; s += 1)
    if (DIRMARKERP(*s)) p = ++s;
  return p;
}

#ifdef WIN32
#ifndef DEFAULT_HEAP_PATH
/* by default, look in executable directory or in parallel boot directory */
#define DEFAULT_HEAP_PATH "%x;%x\\..\\..\\boot\\%m"
#endif
#define SEARCHPATHSEP ';'
#define PATHSEP '\\'

static char *get_defaultheapdirs() {
  char *result;
  wchar_t buf[PATH_MAX];
  DWORD len = sizeof(buf);
  if (ERROR_SUCCESS != RegGetValueW(HKEY_LOCAL_MACHINE, L"Software\\Chez Scheme\\csv" VERSION, L"HeapSearchPath", RRF_RT_REG_SZ, NULL, buf, &len))
    return DEFAULT_HEAP_PATH;
  else if ((result = Swide_to_utf8(buf)))
    return result;
  else
    return DEFAULT_HEAP_PATH;
}
#else /* not WIN32: */
#define SEARCHPATHSEP ':'
#define PATHSEP '/'
#ifndef DEFAULT_HEAP_PATH
#define DEFAULT_HEAP_PATH "/usr/lib/csv%v/%m:/usr/local/lib/csv%v/%m"
#endif

static char *get_defaultheapdirs() {
  return DEFAULT_HEAP_PATH;
}
#endif /* WIN32 */

/* next_path isolates the next entry in the two-part search path sp/dsp,
 * leaving the full path with name affixed in path and *sp / *dsp pointing
 * past the current entry.  it returns 1 on success and 0 if at the end of
 * the search path.  path should be a pointer to an unoccupied buffer
 * PATH_MAX characters long.  either or both of sp/dsp may be empty,
 * but neither may be null, i.e., (char *)0. */
static IBOOL next_path(path, name, ext, sp, dsp) char *path; const char *name, *ext, **sp, **dsp; {
  char *p;
  const char *s, *t;

#define setp(c) if (p >= path + PATH_MAX) { fprintf(stderr, "search path entry too long\n"); S_abnormal_exit(); } else *p++ = (c)
  for (;;) {
    s = *sp;
    p = path;
  /* copy first searchpath entry into path, substituting MACHINE_TYPE for %m,
   * VERSION for %v, % for %%, and : (; windows) for %: (%; windows) */
    while (*s != 0 && *s != SEARCHPATHSEP) {
      switch (*s) {
        case '%':
          s += 1;
          switch (*s) {
#ifdef WIN32
            case 'x': {
              wchar_t exepath[PATH_MAX]; DWORD n;
              s += 1;
              n = GetModuleFileNameW(NULL, exepath, PATH_MAX);
              if (n == 0 || (n == PATH_MAX && GetLastError() == ERROR_INSUFFICIENT_BUFFER)) {
                fprintf(stderr, "warning: executable path is too long; ignoring %%x\n");
              } else {
                char *tstart;
                const char *tend;
                tstart = Swide_to_utf8(exepath);
                t = tstart;
                tend = path_last(t);
                if (tend != t) tend -= 1; /* back up to directory separator */
                while (t != tend) setp(*t++);
                free(tstart);
              }
              break;
            }
#endif
            case 'm':
              s += 1;
              t = MACHINE_TYPE;
              while (*t != 0) setp(*t++);
              break;
            case 'v':
              s += 1;
              t = VERSION;
              while (*t != 0) setp(*t++);
              break;
            case '%':
            case SEARCHPATHSEP:
              setp(*s++);
              break;
            default:
              fprintf(stderr, "warning: ignoring extra %% in search path\n");
              break;
          }
          break;
        default:
          setp(*s++);
          break;
      }
    }

  /* unless entry was null, append name and ext onto path and return true with
   * updated path, sp, and possibly dsp */
    if (s != *sp) {
      if ((p > path) && !DIRMARKERP(*(p - 1))) { setp(PATHSEP); }
      t = name;
      while (*t != 0) setp(*t++);
      t = ext;
      while (*t != 0) setp(*t++);
      setp(0);
      *sp = s;
      return 1;
    }

  /* if current segment is empty, move to next segment.  if next segment
   * is empty, return false */
    if (*s == 0) {
      if (*(*sp = *dsp) == 0) return 0;
      *dsp = "";
    } else {
      *sp = s + 1;
    }
  }
#undef setp
}

/***************************************************************************/
/* BOOT FILES */

typedef struct {
  INT fd;
  char path[PATH_MAX];
} boot_desc;

#define MAX_BOOT_FILES 10
static boot_desc bd[MAX_BOOT_FILES];

/* locally defined functions */
static char get_u8 PROTO((INT fd));
static uptr get_uptr PROTO((INT fd, uptr *pn));
static INT get_string PROTO((INT fd, char *s, iptr max, INT *c));
static IBOOL find_boot PROTO((const char *name, const char *ext, int fd, IBOOL errorp));
static void load PROTO((ptr tc, iptr n, IBOOL base));
static void check_boot_file_state PROTO((const char *who));

static IBOOL find_boot(name, ext, fd, errorp) const char *name, *ext; int fd; IBOOL errorp; {
  char pathbuf[PATH_MAX], buf[PATH_MAX];
  uptr n = 0;
  INT c;
  const char *path;
  char *expandedpath;

  if ((fd != -1) || S_fixedpathp(name)) {
    if (strlen(name) >= PATH_MAX) {
      fprintf(stderr, "boot-file path is too long %s\n", name);
      S_abnormal_exit();
    }

    path = name;

    if (fd == -1) {
      expandedpath = S_malloc_pathname(path);
      fd = OPEN(expandedpath, O_BINARY|O_RDONLY, 0);
      free(expandedpath);
    }

    if (fd == -1) {
      if (errorp) {
        fprintf(stderr, "cannot open boot file %s\n", path);
        S_abnormal_exit();
      } else {
        if (verbose) fprintf(stderr, "trying %s...cannot open\n", path);
        return 0;
      }
    }
    if (verbose) fprintf(stderr, "trying %s...opened\n", path);

   /* check for magic number */
    if (get_u8(fd) != fasl_type_header ||
        get_u8(fd) != 0 ||
        get_u8(fd) != 0 ||
        get_u8(fd) != 0 ||
        get_u8(fd) != 'c' ||
        get_u8(fd) != 'h' ||
        get_u8(fd) != 'e' ||
        get_u8(fd) != 'z') {
      fprintf(stderr, "malformed fasl-object header in %s\n", path);
      S_abnormal_exit();
    }

   /* check version */
    if (get_uptr(fd, &n) != 0) {
      fprintf(stderr, "unexpected end of file on %s\n", path);
      CLOSE(fd);
      S_abnormal_exit();
    }

    if (n != scheme_version) {
      fprintf(stderr, "%s is for Version %s; ", path, S_format_scheme_version(n));
     /* use separate fprintf since S_format_scheme_version returns static string */
      fprintf(stderr, "need Version %s\n", S_format_scheme_version(scheme_version));
      CLOSE(fd);
      S_abnormal_exit();
    }

   /* check machine type */
    if (get_uptr(fd, &n) != 0) {
      fprintf(stderr, "unexpected end of file on %s\n", path);
      CLOSE(fd);
      S_abnormal_exit();
    }

    if (n != machine_type) {
      fprintf(stderr, "%s is for machine-type %s; need machine-type %s\n", path,
              S_lookup_machine_type(n), S_lookup_machine_type(machine_type));
      CLOSE(fd);
      S_abnormal_exit();
    }
  } else {
    const char *sp = Sschemeheapdirs;
    const char *dsp = Sdefaultheapdirs;

    path = pathbuf;
    for (;;) {
      if (!next_path(pathbuf, name, ext, &sp, &dsp)) {
        if (errorp) {
          fprintf(stderr, "cannot find compatible boot file %s%s in search path:\n  \"%s%s\"\n",
                  name, ext,
                  Sschemeheapdirs, Sdefaultheapdirs);
          S_abnormal_exit();
        } else {
          if (verbose) fprintf(stderr, "no compatible %s%s found\n", name, ext);
          return 0;
        }
      }

      expandedpath = S_malloc_pathname(path);
      fd = OPEN(expandedpath, O_BINARY|O_RDONLY, 0);
      free(expandedpath);
      if (fd == -1) {
        if (verbose) fprintf(stderr, "trying %s...cannot open\n", path);
        continue;
      }

      if (verbose) fprintf(stderr, "trying %s...opened\n", path);

     /* check for magic number */
      if (get_u8(fd) != fasl_type_header ||
          get_u8(fd) != 0 ||
          get_u8(fd) != 0 ||
          get_u8(fd) != 0 ||
          get_u8(fd) != 'c' ||
          get_u8(fd) != 'h' ||
          get_u8(fd) != 'e' ||
          get_u8(fd) != 'z') {
        if (verbose) fprintf(stderr, "malformed fasl-object header in %s\n", path);
        CLOSE(fd);
        continue;
      }

     /* check version */
      if (get_uptr(fd, &n) != 0) {
        if (verbose) fprintf(stderr, "unexpected end of file on %s\n", path);
        CLOSE(fd);
        continue;
      }

      if (n != scheme_version) {
        if (verbose) {
          fprintf(stderr, "%s is for Version %s; ", path, S_format_scheme_version(n));
         /* use separate fprintf since S_format_scheme_version returns static string */
          fprintf(stderr, "need Version %s\n", S_format_scheme_version(scheme_version));
        }
        CLOSE(fd);
        continue;
      }

     /* check machine type */
      if (get_uptr(fd, &n) != 0) {
        if (verbose) fprintf(stderr, "unexpected end of file on %s\n", path);
        CLOSE(fd);
        continue;
      }

      if (n != machine_type) {
        if (verbose)
          fprintf(stderr, "%s is for machine-type %s; need machine-type %s\n", path,
                  S_lookup_machine_type(n), S_lookup_machine_type(machine_type));
        CLOSE(fd);
        continue;
      }

      break;
    }
  }

  if (verbose) fprintf(stderr, "version and machine type check\n");

  if (get_u8(fd) != '(') {  /* ) */
    fprintf(stderr, "malformed boot file %s\n", path);
    CLOSE(fd);
    S_abnormal_exit();
  }

  /* ( */
  if ((c = get_u8(fd)) == ')') {
    if (boot_count != 0) {
      fprintf(stderr, "base boot file %s must come before other boot files\n", path);
      CLOSE(fd);
      S_abnormal_exit();
    }
  } else {
    if (boot_count == 0) {
      for (;;) {
       /* try to load heap or boot file this boot file requires */
        if (get_string(fd, buf, PATH_MAX, &c) != 0) {
          fprintf(stderr, "unexpected end of file on %s\n", path);
          CLOSE(fd);
          S_abnormal_exit();
        }
        if (find_boot(buf, ".boot", -1, 0)) break;
        if (c == ')') {
          char *sep; char *wastebuf[8];
          fprintf(stderr, "cannot find subordinate boot file");
          if (LSEEK(fd, 0, SEEK_SET) != 0 || READ(fd, wastebuf, 8) != 8) { /* attempt to rewind and read magic number */
            fprintf(stderr, "---retry with verbose flag for more information\n");
            CLOSE(fd);
            S_abnormal_exit();
          }
          (void) get_uptr(fd, &n); /* version */
          (void) get_uptr(fd, &n); /* machine type */
          (void) get_u8(fd);        /* open paren */
          c = get_u8(fd);
          for (sep = " "; ; sep = "or ") {
            if (c == ')') break;
            (void) get_string(fd, buf, PATH_MAX, &c);
            fprintf(stderr, "%s%s.boot ", sep, buf);
          }
          fprintf(stderr, "required by %s\n", path);
          CLOSE(fd);
          S_abnormal_exit();
        }
      }
    }

   /* skip to end of header */
    while (c != ')') {
      if (c < 0) {
        fprintf(stderr, "malformed boot file %s\n", path);
        CLOSE(fd);
        S_abnormal_exit();
      }
      c = get_u8(fd);
    }
  }

  if (boot_count >= MAX_BOOT_FILES) {
    fprintf(stderr, "exceeded maximum number of boot files (%d)\n", MAX_BOOT_FILES);
    S_abnormal_exit();
  }

  bd[boot_count].fd = fd;
  strcpy(bd[boot_count].path, path);
  boot_count += 1;

  return 1;
}

static char get_u8(INT fd) {
  char buf[1];
  if (READ(fd, &buf, 1) != 1) return -1;
  return buf[0];
}

static uptr get_uptr(INT fd, uptr *pn) {
  uptr n, m; int c; octet k;

  if ((c = get_u8(fd)) < 0) return -1;
  k = (octet)c;
  n = k >> 1;
  while (k & 1) {
    if ((c = get_u8(fd)) < 0) return -1;
    k = (octet)c;
    m = n << 7;
    if (m >> 7 != n) return -1;
    n = m | (k >> 1);
  }
  *pn = n;
  return 0;
}

static INT get_string(fd, s, max, c) INT fd; char *s; iptr max; INT *c; {
  while (max-- > 0) {
    if (*c < 0) return -1;
    if (*c == ' ' || *c == ')') {
      if (*c == ' ') *c = get_u8(fd);
      *s = 0;
      return 0;
    }
    *s++ = *c;
    *c = get_u8(fd);
  }
  return -1;
}

static IBOOL loadecho = 0;
#define LOADSKIP 0

static int set_load_binary(iptr n) {
  if (!Ssymbolp(SYMVAL(S_G.scheme_version_id))) return 0; // set by back.ss
  ptr make_load_binary = SYMVAL(S_G.make_load_binary_id);
  if (Sprocedurep(make_load_binary)) {
    S_G.load_binary = Scall1(make_load_binary, Sstring_utf8(bd[n].path, -1));
    return 1;
  }
  return 0;
}

static void load(tc, n, base) ptr tc; iptr n; IBOOL base; {
  ptr x; iptr i;

  if (base) {
    S_G.error_invoke_code_object = S_boot_read(bd[n].fd, bd[n].path);
    if (!Scodep(S_G.error_invoke_code_object)) {
      (void) fprintf(stderr, "first object on boot file not code object\n");
      S_abnormal_exit();
    }

    S_G.invoke_code_object = S_boot_read(bd[n].fd, bd[n].path);
    if (!Scodep(S_G.invoke_code_object)) {
      (void) fprintf(stderr, "second object on boot file not code object\n");
      S_abnormal_exit();
    }
    S_G.base_rtd = S_boot_read(bd[n].fd, bd[n].path);
    if (!Srecordp(S_G.base_rtd)) {
      S_abnormal_exit();
    }
  }

  i = 0;
  while (i++ < LOADSKIP && S_boot_read(bd[n].fd, bd[n].path) != Seof_object);

  while ((x = S_boot_read(bd[n].fd, bd[n].path)) != Seof_object) {
    if (loadecho) {
      printf("%ld: ", (long)i);
      fflush(stdout);
    }
    if (Sprocedurep(x)) {
      S_initframe(tc, 0);
      x = boot_call(tc, x, 0);
    } else if (Sprocedurep(S_G.load_binary) || set_load_binary(n)) {
      S_initframe(tc, 1);
      S_put_arg(tc, 1, x);
      x = boot_call(tc, S_G.load_binary, 1);
    }
    if (loadecho) {
      S_prin1(x);
      putchar('\n');
      fflush(stdout);
    }
    i += 1;
  }

  S_G.load_binary = Sfalse;
  CLOSE(bd[n].fd);
}

/***************************************************************************/
/* HEAP FILES */

#ifdef DEBUG
#define debug(x) {x}
#else
#define debug(x)
#endif

#include <fcntl.h>
#include <sys/types.h>

#ifdef WIN32
#include <io.h>
#endif /* WIN32 */

#ifdef MMAP_HEAP
#include <sys/mman.h>
#endif

#ifndef O_BINARY
#define O_BINARY 0
#endif /* O_BINARY */

#define check(expr,path) {if ((INT)(expr) < 0) {perror(path); S_abnormal_exit();}}

/***************************************************************************/
/* EXPORTED ROUTINES */

const char *Skernel_version(void) {
  return VERSION;
}

extern void Sset_verbose(v) INT v; {
  verbose = v;
}

extern void Sretain_static_relocation(void) {
  S_G.retain_static_relocation = 1;
}

#if defined(CHECK_FOR_ROSETTA)
#include <sys/sysctl.h>
int is_rosetta = 0;
static void init_rosetta_check() {
  int val = 0;
  size_t size = sizeof(val);
  if (sysctlbyname("sysctl.proc_translated", &val, &size, NULL, 0) != 0) {
    if (errno == ENOENT) {
         is_rosetta = 0;
    } else {
      perror("checking to see if running under Rosetta");
      // if for some reason we can't tell whether we are running under Rosetta or not,
      // default to the safer choice.  It doesn't impact correctness to do the Rosetta
      // workarounds when they are not needed.
      is_rosetta = 1;
    }
  }
  is_rosetta = val;
}
#endif

#ifdef ITEST
#include "itest.c"
#endif

static void default_abnormal_exit(void) {
  exit(1);
}

extern void Sscheme_init(abnormal_exit) void (*abnormal_exit) PROTO((void)); {
  S_abnormal_exit_proc = abnormal_exit ? abnormal_exit : default_abnormal_exit;
  S_errors_to_console = 1;

 /* set before idiot checks */
  S_pagesize = GETPAGESIZE();

  idiot_checks();
#if defined(CHECK_FOR_ROSETTA)
  init_rosetta_check();
#endif
  
  switch (current_state) {
    case RUNNING:
      fprintf(stderr, "error (Sscheme_init): call Sscheme_deinit first to terminate\n");
      S_abnormal_exit();
    case BOOTING:
      fprintf(stderr, "error (Sscheme_init): already initialized\n");
      S_abnormal_exit();
    case UNINITIALIZED:
    case DEINITIALIZED:
      break;
  }
  current_state = BOOTING;

  S_G.retain_static_relocation = 0;
  S_G.enable_object_counts = 0;

  boot_count = 0;

#ifdef WIN32
  Sschemeheapdirs = Sgetenv("SCHEMEHEAPDIRS");
#else
  Sschemeheapdirs = getenv("SCHEMEHEAPDIRS");
#endif
  if (Sschemeheapdirs == (char *)0) {
    Sschemeheapdirs = "";
    if ((Sdefaultheapdirs = get_defaultheapdirs()) == (char *)0) Sdefaultheapdirs = "";
  } else if (*Sschemeheapdirs != 0 && Sschemeheapdirs[strlen(Sschemeheapdirs)-1] == SEARCHPATHSEP) {
      if ((Sdefaultheapdirs = get_defaultheapdirs()) == (char *)0) Sdefaultheapdirs = "";
  } else {
      Sdefaultheapdirs = "";
  }

#ifdef PTHREADS
  {
    int status;
    if ((status = s_thread_key_create(&S_tc_key)) != 0)
      S_error_abort(strerror(status));
    s_thread_setspecific(S_tc_key, S_G.thread_context);
  }
#endif

#ifdef ITEST
  S_boot_time = 1;
  main_init();

  bignum_test();
  exit(0);
#endif
}

static void check_boot_file_state(const char *who) {
  switch (current_state) {
    case UNINITIALIZED:
    case DEINITIALIZED:
      fprintf(stderr, "error (%s): uninitialized; call Sscheme_init first\n", who);
      if (current_state == UNINITIALIZED) exit(1); else S_abnormal_exit();
    case RUNNING:
      fprintf(stderr, "error (%s): already running\n", who);
      S_abnormal_exit();
    case BOOTING:
      break;
  }
}

extern void Sregister_boot_file(name) const char *name; {
  check_boot_file_state("Sregister_boot_file");
  find_boot(name, "", -1, 1);
}

extern void Sregister_boot_file_fd(name, fd) const char *name; int fd; {
  check_boot_file_state("Sregister_boot_file_fd");
  find_boot(name, "", fd, 1);
}

extern void Sregister_heap_file(UNUSED const char *path) {
  fprintf(stderr, "Sregister_heap_file: saved heap files are not presently supported\n");
  S_abnormal_exit();
}

extern void Sbuild_heap(kernel, custom_init) const char *kernel; void (*custom_init) PROTO((void)); {
  ptr tc = Svoid; /* initialize to make gcc happy */
  ptr p;

  switch (current_state) {
    case UNINITIALIZED:
    case DEINITIALIZED:
      fprintf(stderr, "error (Sbuild_heap): uninitialized; call Sscheme_init first\n");
      if (current_state == UNINITIALIZED) exit(1); else S_abnormal_exit();
    case RUNNING:
      fprintf(stderr, "error (Sbuild_heap): already running\n");
      S_abnormal_exit();
    case BOOTING:
      break;
  }
  current_state = RUNNING;

  S_boot_time = 1;

  if (boot_count == 0) {
    const char *name;

    if (!kernel) {
      fprintf(stderr, "no boot file or executable name specified\n");
      S_abnormal_exit();
    }

    name = path_last(kernel);
    if (strlen(name) >= PATH_MAX) {
      fprintf(stderr, "executable name too long: %s\n", name);
      S_abnormal_exit();
    }

#ifdef WIN32
    { /* strip off trailing .exe, if any */
      static char buf[PATH_MAX];
      iptr n;

      n = strlen(name) - 4;
      if (n >= 0 && (_stricmp(name + n, ".exe") == 0)) {
        strcpy(buf, name);
        buf[n] = 0;
        name = buf;
      }
    }
#endif

    if (!find_boot(name, ".boot", -1, 0)) {
      fprintf(stderr, "cannot find compatible %s.boot in search path\n  \"%s%s\"\n",
              name,
              Sschemeheapdirs, Sdefaultheapdirs);
      S_abnormal_exit();
    }
  }

  if (boot_count != 0) {
    INT i = 0;

    main_init();
    if (custom_init) custom_init();

    S_threads = Snil;
    S_nthreads = 0;
    S_set_symbol_value(S_G.active_threads_id, FIX(0));
    /* pass a parent tc of Svoid, since this call establishes the initial
     * thread context and hence there is no parent thread context.  */
    tc = (ptr)THREADTC(S_create_thread_object("startup", tc));
#ifdef PTHREADS
    s_thread_setspecific(S_tc_key, tc);
#endif

    /* #scheme-init enables interrupts */
    TRAP(tc) = (ptr)most_positive_fixnum;
    DISABLECOUNT(tc) = Sfixnum(1);
    COMPRESSFORMAT(tc) = FIX(COMPRESS_LZ4);
    COMPRESSLEVEL(tc) = FIX(COMPRESS_MEDIUM);

    load(tc, i++, 1);
    S_boot_time = 0;

    while (i < boot_count) load(tc, i++, 0);
  }

  if (boot_count != 0) Scompact_heap();

 /* complete the initialization on the Scheme side */
  p = S_symbol_value(S_intern((const unsigned char *)"$scheme-init"));
  if (!Sprocedurep(p)) {
      (void) fprintf(stderr,"\n$scheme-init is not bound to a procedure\n");
      S_abnormal_exit();
  }

  S_initframe(tc, 0);
  (void)boot_call(tc, p, 0);

 /* should be okay to invoke Scheme's error handler now */
  S_errors_to_console = 0;
}

extern void Senable_expeditor(history_file) const char *history_file; {
  Scall1(S_symbol_value(Sstring_to_symbol("$enable-expeditor")), Strue);
  if (history_file != (const char *)0)
    Scall1(S_symbol_value(Sstring_to_symbol("$expeditor-history-file")),
           Sstring_utf8(history_file, -1));
}

extern INT Sscheme_start(argc, argv) INT argc; const char *argv[]; {
  ptr tc = get_thread_context();
  ptr arglist, p; INT i;

  switch (current_state) {
    case UNINITIALIZED:
    case DEINITIALIZED:
      fprintf(stderr, "error (Sscheme_start): uninitialized; call Sscheme_init and Sbuild_heap first\n");
      if (current_state == UNINITIALIZED) exit(1); else S_abnormal_exit();
    case BOOTING:
      fprintf(stderr, "error (Sscheme_start): no heap built yet; call Sbuild_heap first\n");
      S_abnormal_exit();
    case RUNNING:
      break;
  }

  arglist = Snil;
  for (i = argc - 1; i > 0; i -= 1)
    arglist = Scons(Sstring_utf8(argv[i], -1), arglist);

  p = S_symbol_value(S_intern((const unsigned char *)"$scheme"));
  if (!Sprocedurep(p)) {
    (void) fprintf(stderr,"\n$scheme is not bound to a procedure\n");
    S_abnormal_exit();
  }

  S_initframe(tc, 1);
  S_put_arg(tc, 1, arglist);
  p = boot_call(tc, p, 1);

  if (S_integer_valuep(p)) return (INT)Sinteger_value(p);
  return p == Svoid ? 0 : 1;
}

static INT run_script(const char *who, const char *scriptfile, INT argc, const char *argv[], IBOOL programp) {
  ptr tc = get_thread_context();
  ptr arglist, p; INT i;

  switch (current_state) {
    case UNINITIALIZED:
    case DEINITIALIZED:
      fprintf(stderr, "error (%s): uninitialized; call Sscheme_init and Sbuild_heap first\n", who);
      if (current_state == UNINITIALIZED) exit(1); else S_abnormal_exit();
    case BOOTING:
      fprintf(stderr, "error (%s): no heap built yet; call Sbuild_heap first\n", who);
      S_abnormal_exit();
    case RUNNING:
      break;
  }

  arglist = Snil;
  for (i = argc - 1; i > 0; i -= 1)
    arglist = Scons(Sstring_utf8(argv[i], -1), arglist);

  p = S_symbol_value(S_intern((const unsigned char *)"$script"));
  if (!Sprocedurep(p)) {
    (void) fprintf(stderr,"\n$script is not bound to a procedure\n");
    S_abnormal_exit();
  }

  S_initframe(tc, 3);
  S_put_arg(tc, 1, Sboolean(programp));
  S_put_arg(tc, 2, Sstring_utf8(scriptfile, -1));
  S_put_arg(tc, 3, arglist);
  p = boot_call(tc, p, 3);

  if (S_integer_valuep(p)) return (INT)Sinteger_value(p);
  return p == Svoid ? 0 : 1;
}

extern INT Sscheme_script(scriptfile, argc, argv) const char *scriptfile; INT argc; const char *argv[]; {
  return run_script("Sscheme_script", scriptfile, argc, argv, 0);
}

extern INT Sscheme_program(programfile, argc, argv) const char *programfile; INT argc; const char *argv[]; {
  return run_script("Sscheme_program", programfile, argc, argv, 1);
}

extern void Ssave_heap(UNUSED const char *path, UNUSED INT level) {
  fprintf(stderr, "Ssave_heap: saved heap files are not presently supported\n");
  S_abnormal_exit();
}

extern void Sscheme_deinit() {
  ptr p, tc = get_thread_context();

  switch (current_state) {
    case UNINITIALIZED:
    case DEINITIALIZED:
      fprintf(stderr, "error (Sscheme_deinit): not yet initialized or running\n");
      if (current_state == UNINITIALIZED) exit(1); else S_abnormal_exit();
    case BOOTING:
      fprintf(stderr, "error (Sscheme_deinit): not yet running\n");
      S_abnormal_exit();
    case RUNNING:
      break;
  }

  p = S_symbol_value(S_intern((const unsigned char *)"$close-files"));
  S_initframe(tc, 0);
  boot_call(tc, p, 0);

  S_errors_to_console = 1;
  current_state = DEINITIALIZED;
}
