/* windows.c
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

/* much of the following code courtesy of Bob Burger, burgerrg@sagian.com */

#include "system.h"
#include <objbase.h>
#include <io.h>
#include <sys/stat.h>

static ptr s_ErrorString(DWORD dwMessageId);
static IUnknown *s_CreateInstance(CLSID *pCLSID, IID *iid);
static ptr s_GetRegistry(char *s);
static void s_PutRegistry(char *s, char *val);
static void s_RemoveRegistry(char *s);

void S_machine_init() {
    Sregister_symbol("(com)CreateInstance", (void *)s_CreateInstance);
    Sregister_symbol("(windows)GetRegistry", (void *)s_GetRegistry);
    Sregister_symbol("(windows)PutRegistry", (void *)s_PutRegistry);
    Sregister_symbol("(windows)RemoveRegistry", (void *)s_RemoveRegistry);
    Sregister_symbol("(windows)ErrorString", (void *)s_ErrorString);
}

INT S_getpagesize() {
  SYSTEM_INFO si;
  GetSystemInfo(&si);
  return si.dwPageSize;
}

void *S_ntdlopen(const char *path) {
    return (void *)LoadLibrary(path);
}

void *S_ntdlsym(void *h, const char *s) {
    return (void *)GetProcAddress(h, s);
}

/* S_ntdlerror courtesy of Bob Burger, burgerrg@sagian.com */
char *S_ntdlerror(void) {
    static char s[80];
    INT n;

    n = FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, NULL, GetLastError(),
                      0, (LPTSTR)s, 80, NULL);
    if (n == 0) return "unable to load library";
  /* Strip trailing period, newline & return when present */
    if (n >= 3 && s[n-3] == '.') s[n-3] = 0;
    return s;
}

#ifdef FLUSHCACHE
oops, no S_flushcache_max_gap or S_doflush
#endif /* FLUSHCACHE */

static int strncasecmp(const char *s1, const char *s2, int n) {
  while (n > 0) {
    char c1 = *s1; char c2 = *s2;

    if (c1 == 0) return c2 == 0 ? 0 : -1;
    if (c2 == 0) return 1;

    c1 = tolower(c1);
    c2 = tolower(c2);

    if (c1 != c2) return c1 < c2 ? -1 : 1;
    n -= 1; s1 += 1; s2 += 1;
  }
  return 0;
}

static void SplitRegistryKey(char *who, char *wholekey, HKEY *key, char **subkey, char **last) {
  char c, *s;

 /* Determine the base key */
  if (strncasecmp(wholekey, "HKEY_CLASSES_ROOT\\", 18) == 0) {
    *key = HKEY_CLASSES_ROOT;
    *subkey = wholekey+18;
  } else if (strncasecmp(wholekey, "HKEY_CURRENT_USER\\", 18) == 0) {
    *key = HKEY_CURRENT_USER;
    *subkey = wholekey+18;
  } else if (strncasecmp(wholekey, "HKEY_LOCAL_MACHINE\\", 19) == 0) {
    *key = HKEY_LOCAL_MACHINE;
    *subkey = wholekey+19;
  } else if (strncasecmp(wholekey, "HKEY_USERS\\", 11) == 0) {
    *key = HKEY_USERS;
    *subkey = wholekey+11;
  } else if (strncasecmp(wholekey, "HKEY_CURRENT_CONFIG\\", 20) == 0) {
    *key = HKEY_CURRENT_CONFIG;
    *subkey = wholekey+20;
  } else if (strncasecmp(wholekey, "HKEY_DYN_DATA\\", 14) == 0) {
    *key = HKEY_DYN_DATA;
    *subkey = wholekey+14;
  } else
    S_error1(who, "invalid registry key ~s", Sstring(wholekey));

  for (*last = s = *subkey, c = *s; c != '\0'; c = *++s)
    if (c == '\\') *last = s;
}

/* could commonize portions of next two routines, but they're short.
 * the first version takes a char * and returns the result in a buffer
 * of fixed size.  the second takes a char * and returns the result
 * in a scheme string of the necessary size.  the first returns
 * (char *)0 on failure; the second returns Sfalse. */
extern char *S_GetRegistry(char *buf, int bufsize, char *s) {
  HKEY key, result;
  char *subkey, *last;
  DWORD rc, type, size;

  SplitRegistryKey("get-registry", s, &key, &subkey, &last);

 /* open the key */
  if (last == subkey) {
    rc = RegOpenKeyEx(key, "", 0, KEY_QUERY_VALUE, &result);
  } else {
    *last = '\0'; /* Truncate subkey at backslash */
    rc = RegOpenKeyEx(key, subkey, 0, KEY_QUERY_VALUE, &result);
    *last++ = '\\'; /* Restore backslash */
  }
  if (rc != ERROR_SUCCESS) return (char *)0;

 /* grab the data */
  size = bufsize - 1;  /* leave room for trailing nul */
  rc = RegQueryValueEx(result, last, NULL, &type, buf, &size);
  RegCloseKey(result);
  buf[bufsize-1] = 0;  /* nul may be missing if buffer just large enough */

  return rc != ERROR_SUCCESS ? (char *)0 : buf;
}

static ptr s_GetRegistry(char *s) {
  HKEY key, result;
  char *subkey, *last;
  DWORD rc, type, size;
  ptr ans;

  SplitRegistryKey("get-registry", s, &key, &subkey, &last);

 /* open the key */
  if (last == subkey) {
    rc = RegOpenKeyEx(key, "", 0, KEY_QUERY_VALUE, &result);
  } else {
    *last = '\0'; /* Truncate subkey at backslash */
    rc = RegOpenKeyEx(key, subkey, 0, KEY_QUERY_VALUE, &result);
    *last++ = '\\'; /* Restore backslash */
  }
  if (rc != ERROR_SUCCESS) return Sfalse;

 /* Get the size of the value */
  rc = RegQueryValueEx(result, last, NULL, &type, NULL, &size);
  if (rc != ERROR_SUCCESS) {
    RegCloseKey(result);
    return Sfalse;
  }

 /* Allocate a Scheme bytevector of the proper size */
  ans = S_bytevector(size);

 /* Load up the bytevector */
  rc = RegQueryValueEx(result, last, NULL, &type, &BVIT(ans,0), &size);
  RegCloseKey(result);
  if (rc != ERROR_SUCCESS) return Sfalse;

 /* discard unwanted terminating null byte, if present */
  if ((type == REG_SZ) || (type == REG_EXPAND_SZ))
    BYTEVECTOR_TYPE(ans) = ((size-1) << bytevector_length_offset) | type_bytevector;

  return ans;
}

static void s_PutRegistry(char *s, char *val) {
  HKEY key, result;
  char *subkey, *last;
  DWORD rc, qrc, type, size;

  SplitRegistryKey("put-registry!", s, &key, &subkey, &last);

 /* create/open the key */
  if (last == subkey) {
    rc = RegCreateKey(key, "", &result);
  } else {
    *last = '\0'; /* Truncate subkey at backslash */
    rc = RegCreateKey(key, subkey, &result);
    *last++ = '\\'; /* Restore backslash */
  }

 /* lookup type for key (if it exists), if not assume REG_SZ */
  if (rc == ERROR_SUCCESS) {
    qrc = RegQueryValueEx(result, last, NULL, &type, NULL, &size);
    if (qrc != ERROR_SUCCESS) type = REG_SZ;
  }

  if (rc == ERROR_SUCCESS) {
    size_t n = strlen(val)+1;
#if (size_t_bits > 32)
    if ((DWORD)n != n)  {
      RegCloseKey(result);
      S_error2("put-registry!", "cannot set ~a (~a)", Sstring(s), Sstring("too long"));
    }
#endif
   /* set the value */
    rc = RegSetValueEx(result, last, 0, type, val, (DWORD)n);
    RegCloseKey(result);
  }

  if (rc != ERROR_SUCCESS)
    S_error2("put-registry!", "cannot set ~a (~a)", Sstring(s),
      rc == ERROR_FILE_NOT_FOUND ? Sstring("not found") : s_ErrorString(rc));
}

static void s_RemoveRegistry(char *s) {
  HKEY key, result;
  char *subkey, *last;
  DWORD rc;

  SplitRegistryKey("remove-registry!", s, &key, &subkey, &last);

 /* open the key */
  if (last == subkey) {
    rc = RegOpenKeyEx(key, "", 0, KEY_ALL_ACCESS, &result);
  } else {
    *last = '\0'; /* Truncate subkey at backslash */
    rc = RegOpenKeyEx(key, subkey, 0, KEY_ALL_ACCESS, &result);
    *last++ = '\\'; /* Restore backslash */
  }
  if (rc == ERROR_SUCCESS) {
   /* delete the value */
    rc = RegDeleteValue(result, last);
    if (rc == ERROR_FILE_NOT_FOUND)
     /* value by given name not found; try deleting as key */
      rc = RegDeleteKey(result, last);
    RegCloseKey(result);
  }

  if (rc != ERROR_SUCCESS)
    S_error2("remove-registry!", "cannot remove ~a (~a)", Sstring(s),
      rc == ERROR_FILE_NOT_FOUND ? Sstring("not found") :
      rc == ERROR_ACCESS_DENIED ? Sstring("insufficient permission or subkeys exist") :
      s_ErrorString(rc));
}

static IUnknown *s_CreateInstance(CLSID *pCLSID, IID *iid) {
  IUnknown *pIface;
  HRESULT hr;

  hr = CoCreateInstance(pCLSID,
                        NULL,
                        CLSCTX_INPROC_SERVER,
                        iid,
                        (void **)&pIface);
  if (SUCCEEDED(hr)) {
     return (IUnknown *)pIface;
  } else {
     S_error1("", "unable to create instance: ~s", s_ErrorString(hr));
     return (IUnknown *)0 /* not reached */;
  }
}

static ptr s_ErrorString(DWORD dwMessageId) {
    char *lpMsgBuf;
    DWORD len;
    ptr result;

    len = FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS,
                        NULL, dwMessageId, MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), (LPTSTR)&lpMsgBuf, 0, NULL);
    /* If FormatMessage fails, use the error code in hexadecimal. */
    if (len == 0) {
#define HEXERRBUFSIZ ((sizeof(dwMessageId) * 2) + 3)
        char hexerrbuf[HEXERRBUFSIZ];
        snprintf(hexerrbuf, HEXERRBUFSIZ, "0x%x", dwMessageId);
        return Sstring(hexerrbuf);
#undef HEXERRBUFSIZ
    }
    /* Otherwise remove trailing newlines & returns and strip trailing period. */
    while (len > 0) {
        char c = lpMsgBuf[len - 1];
        if (c == '\n' || c == '\r')
            len--;
        else if (c == '.') {
            len--;
            break;
        }
        else break;
    }
    result = Sstring_of_length(lpMsgBuf, len);
    LocalFree(lpMsgBuf);
    return result;
}

ptr S_LastErrorString(void) {
  return s_ErrorString(GetLastError());
}

#ifdef CHAFF
int S_windows_open_exclusive(char *who, char *path, int flags) {
  HANDLE hfile;
  int fd;
  DWORD access = 0;
  DWORD crdisp = 0;

 /* could implement this later with more difficulty */
  if ((flags & (O_TRUNC|O_CREAT)) == (O_TRUNC|O_CREAT))
    S_error("open_exclusive", "O_TRUNC|O_CREAT not supported");

  if (flags & O_RDWR) access |= GENERIC_READ|GENERIC_WRITE;
  if (flags & O_RDONLY) access |= GENERIC_READ;
  if (flags & O_WRONLY) access |= GENERIC_WRITE;

  if (flags & O_CREAT) crdisp = OPEN_ALWAYS;
  if (flags & O_TRUNC) crdisp = TRUNCATE_EXISTING;
  
  hfile = CreateFile(path, access, 0, (SECURITY_ATTRIBUTES *)0,
                     crdisp, FILE_ATTRIBUTE_NORMAL, (HANDLE)0);
  if (hfile == INVALID_HANDLE_VALUE)
    S_error1(who, "~a", s_ErrorString(GetLastError()));

  flags &= O_RDONLY|O_WRONLY|O_RDWR|O_APPEND;
  fd = _open_osfhandle((long)hfile, flags);
  if (fd == -1) S_error(who, "open_osfhandle failed");

  return fd;
}
#endif

#include <Winbase.h>

/* primitive version of flock compatible with Windows 95/98/ME.  A better
   version could be implemented for Windows NT/2000/XP using LockFileEx. */
int S_windows_flock(int fd, int operation) {
  HANDLE hfile = (HANDLE)_get_osfhandle(fd);

  switch (operation) {
    case LOCK_EX|LOCK_NB:
      if (LockFile(hfile, 0, 0, 0x0fffffff, 0)) return 0;
      errno = EWOULDBLOCK;
      return -1;
    case LOCK_EX:
      while (LockFile(hfile, 0, 0, 0x0fffffff, 0) == 0) Sleep(10);
      return 0;
    case LOCK_SH:
    case LOCK_SH|LOCK_NB:
      S_error("flock", "shared locks unsupported");
      return -1;
    case LOCK_UN:
    case LOCK_UN|LOCK_NB:
      UnlockFile(hfile, 0, 0, 0x0fffffff, 0);
      return 0;
    default:
      errno = EINVAL;
      return -1;
  }
}

int S_windows_chdir(const char *pathname) {
  wchar_t wpathname[PATH_MAX];
  if (MultiByteToWideChar(CP_UTF8,0,pathname,-1,wpathname,PATH_MAX) == 0)
    return _chdir(pathname);
  else
    return _wchdir(wpathname);
}

int S_windows_chmod(const char *pathname, int mode) {
  wchar_t wpathname[PATH_MAX];
  if (MultiByteToWideChar(CP_UTF8,0,pathname,-1,wpathname,PATH_MAX) == 0)
    return _chmod(pathname, mode);
  else
    return _wchmod(wpathname, mode);
}

int S_windows_mkdir(const char *pathname) {
  wchar_t wpathname[PATH_MAX];
  if (MultiByteToWideChar(CP_UTF8,0,pathname,-1,wpathname,PATH_MAX) == 0)
    return _mkdir(pathname);
  else
    return _wmkdir(wpathname);
}

int S_windows_open(const char *pathname, int flags, int mode) {
  wchar_t wpathname[PATH_MAX];
  if (MultiByteToWideChar(CP_UTF8,0,pathname,-1,wpathname,PATH_MAX) == 0)
    return _open(pathname,flags, mode);
  else
    return _wopen(wpathname,flags,mode);
}

int S_windows_rename(const char *oldpathname, const char *newpathname) {
  wchar_t woldpathname[PATH_MAX], wnewpathname[PATH_MAX];
  if (MultiByteToWideChar(CP_UTF8,0,oldpathname,-1,woldpathname,PATH_MAX) == 0 ||
        MultiByteToWideChar(CP_UTF8,0,newpathname,-1,wnewpathname,PATH_MAX) == 0)
    return rename(oldpathname, newpathname);
  else
    return _wrename(woldpathname, wnewpathname);
}

int S_windows_rmdir(const char *pathname) {
  wchar_t wpathname[PATH_MAX];
  if (MultiByteToWideChar(CP_UTF8,0,pathname,-1,wpathname,PATH_MAX) == 0)
    return _rmdir(pathname);
  else
    return _wrmdir(wpathname);
}

int S_windows_stat64(const char *pathname, struct STATBUF *buffer) {
  wchar_t wpathname[PATH_MAX];
  if (MultiByteToWideChar(CP_UTF8,0,pathname,-1,wpathname,PATH_MAX) == 0)
    return _stat64(pathname, buffer);
  else
    return _wstat64(wpathname, buffer);
}

int S_windows_system(const char *command) {
  wchar_t wcommand[PATH_MAX];
  if (MultiByteToWideChar(CP_UTF8,0,command,-1,wcommand,PATH_MAX) == 0)
    return system(command);
  else
    return _wsystem(wcommand);
}

int S_windows_unlink(const char *pathname) {
  wchar_t wpathname[PATH_MAX];
  if (MultiByteToWideChar(CP_UTF8,0,pathname,-1,wpathname,PATH_MAX) == 0)
    return _unlink(pathname);
  else
    return _wunlink(wpathname);
}

char *S_windows_getcwd(char *buffer, int maxlen) {
  wchar_t wbuffer[PATH_MAX];
  if (_wgetcwd(wbuffer, PATH_MAX) == NULL) return NULL;
  if (WideCharToMultiByte(CP_UTF8,0,wbuffer,-1,buffer,PATH_MAX,NULL,NULL) == 0) {
    switch (GetLastError()) {
      case ERROR_INSUFFICIENT_BUFFER:
        errno = ERANGE;
        break;
      default:
        errno = EINVAL;
        break;
    }
    return NULL;
  } else
    return buffer;
}
