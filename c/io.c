/* io.c
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
#include <sys/types.h>
#include <sys/stat.h>
#include <limits.h>
#ifdef WIN32
#include <io.h>
#include <shlobj.h>
#if !defined(__MINGW32__)
#pragma comment(lib, "shell32.lib")
#endif
#else /* WIN32 */
#include <sys/file.h>
#include <dirent.h>
#include <pwd.h>
#endif /* WIN32 */

/* locally defined functions */
#ifdef WIN32
static ptr s_wstring_to_bytevector(const wchar_t *s);
#else
static ptr s_string_to_bytevector(const char *s);
# define WIN32_UNUSED
#endif

#ifdef WIN32
# define WIN32_UNUSED UNUSED
#else
# define WIN32_UNUSED
#endif

/* raises an exception if insufficient space cannot be malloc'd.
   otherwise returns a freshly allocated version of inpath with ~ (home directory)
   prefix expanded, if possible */
char *S_malloc_pathname(const char *inpath) {
  char *outpath; const char *ip;

#ifdef WIN32
  if (*inpath == '~' && (*(ip = inpath + 1) == 0 || DIRMARKERP(*ip))) {
    wchar_t* homew;
    if (SUCCEEDED(SHGetKnownFolderPath(&FOLDERID_Profile, 0, NULL, &homew))) {
      char *home = Swide_to_utf8(homew);
      CoTaskMemFree(homew);
      if (NULL != home) {
        size_t n1, n2;
        n1 = strlen(home);
        n2 = strlen(ip) + 1;
        if ((outpath = malloc(n1 + n2)) == NULL) {
          free(home);
          S_error("expand_pathname", "malloc failed");
        }
        memcpy(outpath, home, n1);
        memcpy(outpath + n1, ip, n2);
        free(home);
        return outpath;
      }
    }
  }
#else /* WIN32 */
  if (*inpath == '~') {
    const char *dir; size_t n1, n2; struct passwd *pwent;
    if (*(ip = inpath + 1) == 0 || DIRMARKERP(*ip)) {
      if ((dir = getenv("HOME")) == NULL)
        if ((pwent = getpwuid(getuid())) != NULL)
          dir = pwent->pw_dir;
    } else {
      char *userbuf; const char *user_start = ip;
      do { ip += 1; } while (*ip != 0 && !DIRMARKERP(*ip));
      if ((userbuf = malloc(ip - user_start + 1)) == NULL) S_error("expand_pathname", "malloc failed");
      memcpy(userbuf, user_start, ip - user_start);
      userbuf[ip - user_start] = 0;
      dir = (pwent = getpwnam(userbuf)) != NULL ? pwent->pw_dir : NULL;
      free(userbuf);
    }
    if (dir != NULL) {
      n1 = strlen(dir);
      n2 = strlen(ip) + 1;
      if ((outpath = malloc(n1 + n2)) == NULL) S_error("expand_pathname", "malloc failed");
      memcpy(outpath, dir, n1);
      memcpy(outpath + n1, ip, n2);
      return outpath;
    }
  }
#endif /* WIN32 */

  /* if no ~ or tilde dir can't be found, copy inpath */
  {
    size_t n = strlen(inpath) + 1;
    if ((outpath = (char *)malloc(n)) == NULL) S_error("expand_pathname", "malloc failed");
    memcpy(outpath, inpath, n);
    return outpath;
  }
}

#ifdef WIN32
wchar_t *S_malloc_wide_pathname(const char *inpath) {
  char *path = S_malloc_pathname(inpath);
  wchar_t *wpath = Sutf8_to_wide(path);
  free(path);
  return wpath;
}
#endif

IBOOL S_fixedpathp(const char *inpath) {
  char c; IBOOL res; char *path;

  path = S_malloc_pathname(inpath);
  res = (c = *path) == 0
        || DIRMARKERP(c)
#ifdef WIN32
        || ((*(path + 1) == ':') && ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')))
#endif
        || ((c == '.')
            && ((c = *(path + 1)) == 0
                || DIRMARKERP(c)
                || (c == '.' && ((c = *(path + 2)) == 0 || DIRMARKERP(c)))));
  free(path);
  return res;
}

IBOOL S_file_existsp(const char *inpath, WIN32_UNUSED IBOOL followp) {
#ifdef WIN32
  wchar_t *wpath; IBOOL res;
  WIN32_FILE_ATTRIBUTE_DATA filedata;
  
  if ((wpath = S_malloc_wide_pathname(inpath)) == NULL) {
    return 0;
  } else {
    res = GetFileAttributesExW(wpath, GetFileExInfoStandard, &filedata);
    free(wpath);
    return res;
  }
#else /* WIN32 */
  struct STATBUF statbuf; char *path; IBOOL res;

  path = S_malloc_pathname(inpath);
  res = (followp ? STAT(path, &statbuf) : LSTAT(path, &statbuf)) == 0;
  free(path);
  return res;
#endif /* WIN32 */
}

IBOOL S_file_regularp(const char *inpath, WIN32_UNUSED IBOOL followp) {
#ifdef WIN32
  wchar_t *wpath; IBOOL res;
  WIN32_FILE_ATTRIBUTE_DATA filedata;
  
  if ((wpath = S_malloc_wide_pathname(inpath)) == NULL) {
    return 0;
  } else {
    res = GetFileAttributesExW(wpath, GetFileExInfoStandard, &filedata)
          && (filedata.dwFileAttributes & (FILE_ATTRIBUTE_DEVICE | FILE_ATTRIBUTE_DIRECTORY | FILE_ATTRIBUTE_REPARSE_POINT)) == 0;
    free(wpath);
    return res;
  }
#else /* WIN32 */
  struct STATBUF statbuf; char *path; IBOOL res;

  path = S_malloc_pathname(inpath);
  res = (followp ? STAT(path, &statbuf) : LSTAT(path, &statbuf)) == 0
        && (statbuf.st_mode & S_IFMT) == S_IFREG;
  free(path);
  return res;
#endif /* WIN32 */
}

IBOOL S_file_directoryp(const char *inpath, WIN32_UNUSED IBOOL followp) {
#ifdef WIN32
  wchar_t *wpath; IBOOL res;
  WIN32_FILE_ATTRIBUTE_DATA filedata;
  
  if ((wpath = S_malloc_wide_pathname(inpath)) == NULL) {
    return 0;
  } else {
    res = GetFileAttributesExW(wpath, GetFileExInfoStandard, &filedata)
          && filedata.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY;
    free(wpath);
    return res;
  }
#else /* WIN32 */
  struct STATBUF statbuf; char *path; IBOOL res;

  path = S_malloc_pathname(inpath);
  res = (followp ? STAT(path, &statbuf) : LSTAT(path, &statbuf)) == 0
        && (statbuf.st_mode & S_IFMT) == S_IFDIR;
  free(path);
  return res;
#endif /* WIN32 */
}

IBOOL S_file_symbolic_linkp(const char *inpath) {
#ifdef WIN32
  wchar_t *wpath; IBOOL res;
  WIN32_FILE_ATTRIBUTE_DATA filedata;
  
  if ((wpath = S_malloc_wide_pathname(inpath)) == NULL) {
    return 0;
  } else {
    res = GetFileAttributesExW(wpath, GetFileExInfoStandard, &filedata)
          && filedata.dwFileAttributes & FILE_ATTRIBUTE_REPARSE_POINT;
    free(wpath);
    return res;
  }
#else /* WIN32 */
  struct STATBUF statbuf; char *path; IBOOL res;

  path = S_malloc_pathname(inpath);
  res = LSTAT(path, &statbuf) == 0 && (statbuf.st_mode & S_IFMT) == S_IFLNK;
  free(path);
  return res;
#endif /* WIN32 */
}

#ifdef WIN32
static ptr s_wstring_to_bytevector(const wchar_t *s) {
  iptr n; ptr bv;
  if ((n = wcslen(s)) == 0) return S_G.null_bytevector;
  n *= sizeof(wchar_t);
  bv = S_bytevector(n);
  memcpy(&BVIT(bv,0), s, n);
  return bv;
}

ptr S_find_files(const char *wildpath) {
  wchar_t *wwildpath;
  intptr_t handle;
  struct _wfinddata_t fileinfo;

  if ((wwildpath = S_malloc_wide_pathname(wildpath)) == NULL)
    return S_LastErrorString();

  if ((handle = _wfindfirst(wwildpath, &fileinfo)) == (intptr_t)-1) {
    free(wwildpath);
    return S_strerror(errno);
  } else {
    ptr ls = Snil;
    do {
      ls = Scons(s_wstring_to_bytevector(fileinfo.name), ls);
    } while (_wfindnext(handle, &fileinfo) == 0);
    _findclose(handle);
    free(wwildpath);
    return ls;
  }
}
#else /* WIN32 */
static ptr s_string_to_bytevector(const char *s) {
  iptr n; ptr bv;
  if ((n = strlen(s)) == 0) return S_G.null_bytevector;
  bv = S_bytevector(n);
  memcpy(&BVIT(bv,0), s, n);
  return bv;
}

ptr S_directory_list(const char *inpath) {
  char *path; DIR *dirp;

  path = S_malloc_pathname(inpath);
  if ((dirp = opendir(path)) == (DIR *)0) {
    free(path);
    return S_strerror(errno);
  } else {
    struct dirent *dep; ptr ls = Snil;

    while ((dep = readdir(dirp)) != (struct dirent *)0)
      ls = Scons(s_string_to_bytevector(dep->d_name), ls);
    closedir(dirp);
    free(path);
    return ls;
  }
}
#endif /* WIN32 */
