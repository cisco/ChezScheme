/* io.c
 * Copyright 1984-2016 Cisco Systems, Inc.
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
#else /* WIN32 */
#include <sys/file.h>
#include <dirent.h>
#include <pwd.h>
#endif /* WIN32 */

/* locally defined functions */
#ifdef WIN32
static ptr s_wstring_to_bytevector PROTO((const wchar_t *s));
#else
static ptr s_string_to_bytevector PROTO((const char *s));
#endif

#ifdef WIN32
/* Warning: returns pointer to static string */
const char *S_homedir() {
  static char home[PATH_MAX];
  const char *homedrive = getenv("HOMEDRIVE");
  const char *homepath = getenv("HOMEPATH");

  if (snprintf(home, PATH_MAX, "%s%s", homedrive, homepath) < PATH_MAX) return home;
  return NULL;
}
#else
/* Warning: returns pointer to static string */
const char *S_homedir() {
  const char *home;
  static struct passwd *pwent;

  if ((home = getenv("HOME"))) return home;
  if ((pwent = getpwuid(getuid()))) return pwent->pw_dir;
  return NULL;
}
#endif

const char *S_pathname_impl(const char *inpath, char *buffer) {
  if (*inpath != '~') { return inpath; }
  else {
#define setp(c) if (p > buffer + PATH_MAX) return NULL; else *p++ = (c)
    static char path[PATH_MAX];
    char *p;
    const char *ip, *dir;

    if (buffer == NULL) buffer = path;

    ip = inpath + 1;
    if (*ip == 0 || DIRMARKERP(*ip)) {
      if (!(dir = S_homedir())) return NULL;
    } else {
#ifdef WIN32
      return inpath;
#else
      struct passwd *pwent;
      p = buffer;
      while (*ip != 0 && !DIRMARKERP(*ip)) setp(*ip++);
      setp(0);
      if (!(pwent = getpwnam(buffer)) || !(dir = pwent->pw_dir))
        return NULL;
#endif /* WIN32 */
    }

    p = buffer;
    while (*dir != 0) setp(*dir++);
    while (*ip != 0) setp(*ip++);
    setp(0);
    return buffer;
#undef setp
  }
}

/* Warning: may return pointer to static string */
const char *S_pathname(const char *who, const char *inpath,
                       IBOOL errorp,  char *buffer) {
  const char *path = S_pathname_impl(inpath, buffer);
  if (path != NULL) return path;
  if (errorp) S_error1(who, "unable to expand path name ~s", Sstring(inpath));
  return inpath;
}

IBOOL S_fixedpathp(p) const char *p; {
  char c;

  p = S_pathname("", p, 0, (char *)0);

  if ((c = *p) == 0 || DIRMARKERP(c)) return 1;
  if (c == '.')
    return (c = *++p) == 0
           || DIRMARKERP(c)
           || (c == '.' && ((c = *++p) == 0 || DIRMARKERP(c)));
#ifdef WIN32
  if (*++p == ':') return c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z';
#endif
  return 0;
}

IBOOL S_file_existsp(path, followp) const char *path; IBOOL followp; {
#ifdef WIN32
  wchar_t wpath[PATH_MAX];
  WIN32_FILE_ATTRIBUTE_DATA filedata;
  
  path = S_pathname("file-exists?", path, 1, (char *)0);
  if (MultiByteToWideChar(CP_UTF8,0,path,-1,wpath,PATH_MAX) == 0)
    return 0;

  return GetFileAttributesExW(wpath, GetFileExInfoStandard, &filedata);
#else /* WIN32 */
  struct STATBUF statbuf;
  const char *expandedpath = S_pathname("file-exists?", path, 0, (char *)0);

  return (followp ?
           STAT(expandedpath, &statbuf) :
           LSTAT(expandedpath, &statbuf)) == 0;
#endif /* WIN32 */
}

IBOOL S_file_regularp(path, followp) const char *path; IBOOL followp; {
#ifdef WIN32
  wchar_t wpath[PATH_MAX];
  WIN32_FILE_ATTRIBUTE_DATA filedata;
  
  path = S_pathname("file-regular?", path, 1, (char *)0);
  if (MultiByteToWideChar(CP_UTF8,0,path,-1,wpath,PATH_MAX) == 0)
    return 0;
  if (!GetFileAttributesExW(wpath, GetFileExInfoStandard, &filedata))
    return 0;

  return (filedata.dwFileAttributes & (FILE_ATTRIBUTE_DEVICE | FILE_ATTRIBUTE_DIRECTORY | FILE_ATTRIBUTE_REPARSE_POINT)) == 0;
#else /* WIN32 */
  struct STATBUF statbuf;
  const char *expandedpath = S_pathname("file-regular?", path, 0, (char *)0);

  return (followp ?
           STAT(expandedpath, &statbuf) :
           LSTAT(expandedpath, &statbuf)) == 0
           && (statbuf.st_mode & S_IFMT) == S_IFREG;
#endif /* WIN32 */
}

IBOOL S_file_directoryp(path, followp) const char *path; IBOOL followp; {
#ifdef WIN32
  wchar_t wpath[PATH_MAX];
  WIN32_FILE_ATTRIBUTE_DATA filedata;
  
  path = S_pathname("file-directory?", path, 1, (char *)0);
  if (MultiByteToWideChar(CP_UTF8,0,path,-1,wpath,PATH_MAX) == 0)
    return 0;
  if (!GetFileAttributesExW(wpath, GetFileExInfoStandard, &filedata))
    return 0;

  return filedata.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY;
#else /* WIN32 */
  struct STATBUF statbuf;
  const char *expandedpath = S_pathname("file-directory?", path, 0, (char *)0);

  return (followp ?
           STAT(expandedpath, &statbuf) :
           LSTAT(expandedpath, &statbuf)) == 0
           && (statbuf.st_mode & S_IFMT) == S_IFDIR;
#endif /* WIN32 */
}

IBOOL S_file_symbolic_linkp(const char *path) {
#ifdef WIN32
  wchar_t wpath[PATH_MAX];
  WIN32_FILE_ATTRIBUTE_DATA filedata;
  
  path = S_pathname("file-symbolic-link?", path, 1, (char *)0);
  if (MultiByteToWideChar(CP_UTF8,0,path,-1,wpath,PATH_MAX) == 0)
    return 0;
  if (!GetFileAttributesExW(wpath, GetFileExInfoStandard, &filedata))
    return 0;

  return filedata.dwFileAttributes & FILE_ATTRIBUTE_REPARSE_POINT;
#else /* WIN32 */
  struct STATBUF statbuf;
  const char *expandedpath = S_pathname("file-symbolic-link?", path, 0, (char *)0);

  return (LSTAT(expandedpath, &statbuf) == 0)
            && (statbuf.st_mode & S_IFMT) == S_IFLNK;
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
  wchar_t wwildpath[PATH_MAX];
  intptr_t handle;
  struct _wfinddata_t fileinfo;

  wildpath = S_pathname("directory-list", wildpath, 1, (char *)0);
  if (MultiByteToWideChar(CP_UTF8,0,wildpath,-1,wwildpath,PATH_MAX) == 0)
    return S_LastErrorString();

  if ((handle = _wfindfirst(wwildpath, &fileinfo)) == (intptr_t)-1)
    return S_strerror(errno);
  else {
    ptr ls = Snil;
    do {
      ls = Scons(s_wstring_to_bytevector(fileinfo.name), ls);
    } while (_wfindnext(handle, &fileinfo) == 0);
    _findclose(handle);
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

ptr S_directory_list(const char *path) {
  DIR *dirp; struct dirent *dep; ptr ls = Snil;

  path = S_pathname("directory-list", path, 1, (char *)0);

  if ((dirp = opendir(path)) == (DIR *)0)
    return S_strerror(errno);

  while ((dep = readdir(dirp)) != (struct dirent *)0)
    ls = Scons(s_string_to_bytevector(dep->d_name), ls);

  closedir(dirp);
  return ls;
}
#endif /* WIN32 */
