/*
  This file is meant to be standalone and suitable for use in
  programs other than Chez Scheme.

  Defines

     self_exe_t get_process_executable_path(const char *exec_file);

  which takes argv[0] as supplied to `main` and returns an improved
  representation of the containing executable. Not all platforms use
  the `exec_file` argument; in particular, it is not used on Windows
  or macOS. At worst, on Unix, uses `PATH` to convert `exec_file`
  into a path.

  The result type `self_exe_t` is normally `char *`, but on Windows
  it's `wchar_t *` when `SELF_EXE_WINDOWS_AS_UTF16` is defined. In
  all cases, the result is `malloc`ed, so it can be `free`d.

  If `SELF_EXE_MAIN` is defined, then `main` is defined to call
  and print the result from `get_process_executable_path`, which
  is useful for testing.

  Parts of the implementaiton here are from the LLVM Project under
  the Apache License v2.0 with LLVM Exceptions.
*/

/* `undef`ed below if not needed */
#define USE_GENERIC_GET_SELF_PATH

#include <stdlib.h>
#include <string.h>

#ifndef WIN32
# if defined(_MSC_VER) || defined(__MINGW32__)
#  define WIN32
# endif
#endif

#ifdef WIN32
# include <windows.h>
#endif

#if defined(WIN32) && defined(SELF_EXE_WINDOWS_AS_UTF16)
typedef wchar_t *self_exe_t;
#else
typedef char *self_exe_t;
#endif

#if defined(__linux__) || defined(__GNU__)
# define USE_PROC_SELF_EXE_FILESYSTEM_PATH "/proc/self/exe"
#endif

/* From LLVM: (but removed OpenBSD, which doesn't have "/proc") */
#if defined(__NetBSD__) || defined(__minix) || \
    defined(__DragonFly__) || defined(__FreeBSD_kernel__) || defined(_AIX)
# define USE_PROC_SELF_EXE_FILESYSTEM_PATH "/proc/curproc/file"
#endif

#if defined(USE_PROC_SELF_EXE_FILESYSTEM_PATH)
# include <errno.h>
# include <unistd.h>
# define GENERIC_GET_SELF_PATH_NAME generic_get_self_path
static char *GENERIC_GET_SELF_PATH_NAME(const char *exec_file);
static char *get_process_executable_path(const char *exec_file)
{
  char *s;
  ssize_t len, blen = 256;

  s = malloc(blen);

  while (1) {
    len = readlink(USE_PROC_SELF_EXE_FILESYSTEM_PATH, s, blen-1);
    if (len == (blen-1)) {
      free(s);
      blen *= 2;
      s = malloc(blen);
    } else if (len < 0) {
      /* possibly in a chroot environment where "/proc" is not
         available, so fall back to generic approach: */
      free(s);
      return generic_get_self_path(exec_file);
    } else
      break;
  }
  s[len] = 0;

#if defined(__GNU__)
  /* From LLVM comments: */
  /* [...] on GNU/Hurd, /proc/self/exe is a symlink to the path that was used to start
     the program, and not the eventual binary file. Therefore, call realpath [...] */
  {
    char *link_path;
    link_path = realpath(s, NULL);
    if (link_path) {
      free(s);
      return link_path;
    }
  }
#endif

  return s;
}
#endif

#if defined(__FreeBSD__)
# include <osreldate.h>
# if __FreeBSD_version >= 1300057
#  include <sys/auxv.h>
#  include <limits.h>
# else
#  include <machine/elf.h>
extern char **environ;
# endif
# define GENERIC_GET_SELF_PATH_NAME generic_get_self_path
static char *GENERIC_GET_SELF_PATH_NAME(const char *exec_file);
static char *get_process_executable_path(const char *exec_file)
{
  /* From LLVM, including most comments: */

  /* On FreeBSD if the exec path specified in ELF auxiliary vectors is
     preferred, if available.  /proc/curproc/file and the KERN_PROC_PATHNAME
     sysctl may not return the desired path if there are multiple hardlinks
     to the file. */
#if __FreeBSD_version >= 1300057
  char exe_path[PATH_MAX];
  if (elf_aux_info(AT_EXECPATH, exe_path, sizeof(exe_path)) == 0) {
    char *link_path;
    link_path = realpath(exe_path, NULL);
    if (link_path)
      return link_path;
  }
#else
  /* elf_aux_info(AT_EXECPATH, ... is not available in all supported versions,
     fall back to finding the ELF auxiliary vectors after the process's
     environment. */
  char **p = environ;
  while (*p++ != 0)
    ;
  /* Iterate through auxiliary vectors for AT_EXECPATH. */
  for (Elf_Auxinfo *aux = (Elf_Auxinfo *)p; aux->a_type != AT_NULL; aux++) {
    if (aux->a_type == AT_EXECPATH) {
      char *link_path;
      link_path = realpath((char *)aux->a_un.a_ptr, NULL);
      if (link_path)
        return link_path;
    }
  }
#endif
  /* Fallback: */
  return generic_get_self_path(exec_file);
}
#endif

#if defined(__APPLE__) && defined(__MACH__)
static char *get_process_executable_path(const char *exec_file)
{
  char buf[1024], *s;
  uint32_t size = sizeof(buf);
  int r;

  r = _NSGetExecutablePath(buf, &size);
  if (!r)
    return strdup(buf);
  else {
    s = malloc(size);
    r = _NSGetExecutablePath(s, &size);
    if (!r)
      return s;
    return strdup(exec_file);
  }
}
# undef USE_GENERIC_GET_SELF_PATH
#endif

#ifdef WIN32
self_exe_t get_process_executable_path(const char *exec_file)
{
  wchar_t *path;
  DWORD r, sz = 1024;

  while (1) {
    path = (wchar_t *)malloc(sz * sizeof(wchar_t));
    r = GetModuleFileNameW(NULL, path, sz);
    if ((r == sz)
        && (GetLastError() == ERROR_INSUFFICIENT_BUFFER)) {
      free(path);
      sz = 2 * sz;
    } else
      break;
  }

#ifndef SELF_EXE_WINDOWS_AS_UTF16
  {
    char *r;
    int len;
    len = WideCharToMultiByte(CP_UTF8, 0, path, -1, NULL, 0, NULL, NULL);
    r = malloc(len);
    len = WideCharToMultiByte(CP_UTF8, 0, path, -1, r, len, NULL, NULL);
    free(path);
    return r;
  }
#else
  return path;
#endif
}
# undef USE_GENERIC_GET_SELF_PATH
#endif

#if defined(USE_GENERIC_GET_SELF_PATH) || defined(USE_EXE_LOOKUP_VIA_PATH)

/* Get executable path via argv[0] and the `PATH` environment variable */

# include <errno.h>
# include <unistd.h>

static int has_slash(const char *s)
{
  while (*s) {
    if (s[0] == '/')
      return 1;
    s++;
  }
  return 0;
}

static char *do_path_append(const char *s1, int l1, const char *s2)
{
  int l2;
  char *s;

  l2 = strlen(s2);

  s  = (char *)malloc(l1 + l2 + 2);

  memcpy(s, s1, l1);
  if (s[l1 - 1] != '/') {
    s[l1++] = '/';
  }

  memcpy(s + l1, s2, l2);
  s[l1 + l2] = 0;

  return s;
}

static char *path_append(const char *s1, const char *s2)
{
  return do_path_append(s1, strlen(s1), s2);
}

static char *copy_string(const char *s1)
{
  if (!s1) return NULL;
  return strdup(s1);
}

static int executable_exists(const char *path)
{
  return (access(path, X_OK) == 0);
}

static char *get_current_directory()
{
  char *dir;

  dir = getcwd(NULL, 0);

  if (dir == NULL) {
    /* Probably an old system where you have to allocate space yourself */
    char *s;
    int len = 256;

    s = malloc(len);
    while (1) {
      dir = getcwd(s, len);
      if (dir != NULL)
        break;
      if (errno == ERANGE) {
        free(s);
        len *= 2;
        s = malloc(len);
      } else
        break;
    }

    if (dir == NULL) {
      /* Still failed, so give up with "." as the path */
      s[0] = '.';
      s[1] = 0;
      dir = s;
    }
  }

  return dir;
}

static char *lookup_exe_via_path(const char *exec_file)
{
  if (exec_file[0] == '/') {
    /* Absolute path */
    return copy_string(exec_file);
  } else if (has_slash(exec_file)) {
    /* Relative path with a directory: */
    char *dir, *r;
    dir = get_current_directory();
    r = path_append(dir, exec_file);
    free(dir);
    return r;
  } else {
    /* We have to find the executable by searching PATH: */
    char *path = copy_string(getenv("PATH")), *p, *m, *saved_path = path;
    int more;

    if (!path) {
      path = "";
    }

    while (1) {
      /* Try each element of path: */
      for (p = path; *p && (*p != ':'); p++) { }
      if (*p) {
	*p = 0;
	more = 1;
      } else
	more = 0;

      if (!*path)
	break;

      m = path_append(path, exec_file);

      if (executable_exists(m)) {
	if (m[0] != '/') {
          char *old_m = m;
	  m = path_append(get_current_directory(), m);
          free(old_m);
        }
        if (saved_path) free(saved_path);
	return m;
      }
      free(m);

      if (more)
	path = p + 1;
      else
	break;
    }

    if (saved_path) free(saved_path);

    return copy_string(exec_file);
  }
}
#endif

#ifdef USE_GENERIC_GET_SELF_PATH
# ifndef GENERIC_GET_SELF_PATH_NAME
#  define GENERIC_GET_SELF_PATH_NAME get_process_executable_path
# endif
static char *GENERIC_GET_SELF_PATH_NAME(const char *exec_file)
{
  return lookup_exe_via_path(exec_file);
}
#endif

char *S_get_process_executable_path(const char *exec_file)
{
  return get_process_executable_path(exec_file);
}

#ifdef SELF_EXE_MAIN
# include <stdio.h>
int main(int argc, char **argv)
{
  printf("%s\n", get_process_executable_path(argv[0]));
}
#endif
