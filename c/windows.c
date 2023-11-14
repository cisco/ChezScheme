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
#include <psapi.h>
#include <io.h>
#include <sys/stat.h>

static ptr s_ErrorStringImp(DWORD dwMessageId, const char *lpcDefault);
static ptr s_ErrorString(DWORD dwMessageId);
static IUnknown *s_CreateInstance(CLSID *pCLSID, IID *iid);
static ptr s_GetRegistry(wchar_t *s);
static void s_PutRegistry(wchar_t *s, wchar_t *val);
static void s_RemoveRegistry(wchar_t *s);

void S_machine_init(void) {
    Sregister_symbol("(com)CreateInstance", (void *)s_CreateInstance);
    Sregister_symbol("(windows)GetRegistry", (void *)s_GetRegistry);
    Sregister_symbol("(windows)PutRegistry", (void *)s_PutRegistry);
    Sregister_symbol("(windows)RemoveRegistry", (void *)s_RemoveRegistry);
    Sregister_symbol("(windows)ErrorString", (void *)s_ErrorString);
}

INT S_getpagesize(void) {
  SYSTEM_INFO si;
  GetSystemInfo(&si);
  return si.dwPageSize;
}

void *S_ntdlopen(const char *path) {
  wchar_t *pathw = Sutf8_to_wide(path);
  void *r = (void *)LoadLibraryW(pathw);
  free(pathw);
  return r;
}

HMODULE *S_enum_process_modules(void) {
    DWORD cur_num_bytes = 1024;
    DWORD req_num_bytes;
    HMODULE *modules = malloc(cur_num_bytes);

    if (!modules)
        return NULL;

    for (;;) {
        if (!EnumProcessModules(GetCurrentProcess(), modules, cur_num_bytes, &req_num_bytes))
            return NULL;
        req_num_bytes += sizeof *modules; // for sentinel NULL value
        if (req_num_bytes <= cur_num_bytes)
            break;
        HMODULE *new_mod = realloc(modules, req_num_bytes);
        if (!new_mod) {
            free(modules);
            return NULL;
        }

        modules = new_mod;
        cur_num_bytes = req_num_bytes;
    }

    const size_t numel = req_num_bytes/sizeof *modules;
    modules[numel - 1] = NULL;

    return modules;
}

void *S_ntdlsym(void *h, const char *s) {
    return (void *)GetProcAddress(h, s);
}

/* Initial version of S_ntdlerror courtesy of Bob Burger
 * Modifications by James-Adam Renquinha Henri, jarhmander@gmail.com */
ptr S_ntdlerror(void) {
    return s_ErrorStringImp(GetLastError(), "unable to load library");
}

#ifdef FLUSHCACHE
void S_doflush(uptr start, uptr end) {
  FlushInstructionCache(GetCurrentProcess(), TO_VOIDP(start), end - start);
}

INT S_flushcache_max_gap(void) {
  return 32;
}
#endif

static void SplitRegistryKey(char *who, wchar_t *wholekey, HKEY *key, wchar_t **subkey, wchar_t **last) {
  wchar_t c, *s;

 /* Determine the base key */
  if (_wcsnicmp(wholekey, L"HKEY_CLASSES_ROOT\\", 18) == 0) {
    *key = HKEY_CLASSES_ROOT;
    *subkey = wholekey+18;
  } else if (_wcsnicmp(wholekey, L"HKEY_CURRENT_USER\\", 18) == 0) {
    *key = HKEY_CURRENT_USER;
    *subkey = wholekey+18;
  } else if (_wcsnicmp(wholekey, L"HKEY_LOCAL_MACHINE\\", 19) == 0) {
    *key = HKEY_LOCAL_MACHINE;
    *subkey = wholekey+19;
  } else if (_wcsnicmp(wholekey, L"HKEY_USERS\\", 11) == 0) {
    *key = HKEY_USERS;
    *subkey = wholekey+11;
  } else if (_wcsnicmp(wholekey, L"HKEY_CURRENT_CONFIG\\", 20) == 0) {
    *key = HKEY_CURRENT_CONFIG;
    *subkey = wholekey+20;
  } else if (_wcsnicmp(wholekey, L"HKEY_DYN_DATA\\", 14) == 0) {
    *key = HKEY_DYN_DATA;
    *subkey = wholekey+14;
  } else {
    char *wholekey_utf8 = Swide_to_utf8(wholekey);
    ptr wholekey_scheme = Sstring_utf8(wholekey_utf8, -1);
    free(wholekey_utf8);
    S_error1(who, "invalid registry key ~s", wholekey_scheme);
  }

  for (*last = s = *subkey, c = *s; c != '\0'; c = *++s)
    if (c == '\\') *last = s;
}

static ptr s_GetRegistry(wchar_t *s) {
  HKEY key, result;
  wchar_t *subkey, *last;
  DWORD rc, type, size;
  ptr ans;

  SplitRegistryKey("get-registry", s, &key, &subkey, &last);

 /* open the key */
  if (last == subkey) {
    rc = RegOpenKeyExW(key, L"", 0, KEY_QUERY_VALUE, &result);
  } else {
    *last = '\0'; /* Truncate subkey at backslash */
    rc = RegOpenKeyExW(key, subkey, 0, KEY_QUERY_VALUE, &result);
    *last++ = '\\'; /* Restore backslash */
  }
  if (rc != ERROR_SUCCESS) return Sfalse;

 /* Get the size of the value */
  rc = RegQueryValueExW(result, last, NULL, &type, NULL, &size);
  if (rc != ERROR_SUCCESS) {
    RegCloseKey(result);
    return Sfalse;
  }

 /* Allocate a Scheme bytevector of the proper size */
  ans = S_bytevector(size);

 /* Load up the bytevector */
  rc = RegQueryValueExW(result, last, NULL, &type, &BVIT(ans,0), &size);
  RegCloseKey(result);
  if (rc != ERROR_SUCCESS) return Sfalse;

 /* discard unwanted terminating null character, if present */
  if (((type == REG_SZ) || (type == REG_EXPAND_SZ)) &&
      (size >= 2) &&
      (*(wchar_t*)(&BVIT(ans, size-2)) == 0))
    BYTEVECTOR_TYPE(ans) = ((size-2) << bytevector_length_offset) | type_bytevector;

  return ans;
}

static void s_PutRegistry(wchar_t *s, wchar_t *val) {
  HKEY key, result;
  wchar_t *subkey, *last;
  DWORD rc, type;
  size_t n = (wcslen(val) + 1) * sizeof(wchar_t);
#if (size_t_bits > 32)
  if ((DWORD)n != n)  { 
    char *s_utf8 = Swide_to_utf8(s);
    ptr s_scheme = Sstring_utf8(s_utf8, -1);
    free(s_utf8);
    S_error2("put-registry!", "cannot set ~a (~a)", s_scheme, Sstring("too long"));
  }
#endif

  SplitRegistryKey("put-registry!", s, &key, &subkey, &last);

 /* create/open the key */
  if (last == subkey) {
    rc = RegCreateKeyExW(key, L"", 0, NULL, 0, KEY_SET_VALUE, NULL, &result, NULL);
  } else {
    *last = '\0'; /* Truncate subkey at backslash */
    rc = RegCreateKeyExW(key, subkey, 0, NULL, 0, KEY_SET_VALUE, NULL, &result, NULL);
    *last++ = '\\'; /* Restore backslash */
  }

  if (rc == ERROR_SUCCESS) {
   /* lookup type for key (if it exists), if not assume REG_SZ */
    if (ERROR_SUCCESS != RegQueryValueExW(result, last, NULL, &type, NULL, NULL))
      type = REG_SZ;

   /* set the value */
    rc = RegSetValueExW(result, last, 0, type, (const BYTE*)val, (DWORD)n);
    RegCloseKey(result);
  }

  if (rc != ERROR_SUCCESS) {
    char *s_utf8 = Swide_to_utf8(s);
    ptr s_scheme = Sstring_utf8(s_utf8, -1);
    free(s_utf8);
    S_error2("put-registry!", "cannot set ~a (~a)", s_scheme,
      rc == ERROR_FILE_NOT_FOUND ? Sstring("not found") : s_ErrorString(rc));
  }
}


static void s_RemoveRegistry(wchar_t *s) {
  HKEY key, result;
  wchar_t *subkey, *last;
  DWORD rc;

  SplitRegistryKey("remove-registry!", s, &key, &subkey, &last);

 /* open the key */
  if (last == subkey) {
    rc = RegOpenKeyExW(key, L"", 0, KEY_ALL_ACCESS, &result);
  } else {
    *last = '\0'; /* Truncate subkey at backslash */
    rc = RegOpenKeyExW(key, subkey, 0, KEY_ALL_ACCESS, &result);
    *last++ = '\\'; /* Restore backslash */
  }
  if (rc == ERROR_SUCCESS) {
   /* delete the value */
    rc = RegDeleteValueW(result, last);
    if (rc == ERROR_FILE_NOT_FOUND)
     /* value by given name not found; try deleting as key */
      rc = RegDeleteKeyW(result, last);
    RegCloseKey(result);
  }

  if (rc != ERROR_SUCCESS) {
    char *s_utf8 = Swide_to_utf8(s);
    ptr s_scheme = Sstring_utf8(s_utf8, -1);
    free(s_utf8);
    S_error2("remove-registry!", "cannot remove ~a (~a)", s_scheme,
      rc == ERROR_FILE_NOT_FOUND ? Sstring("not found") :
      rc == ERROR_ACCESS_DENIED ? Sstring("insufficient permission or subkeys exist") :
      s_ErrorString(rc));
  }
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
    return s_ErrorStringImp(dwMessageId, NULL);
}

static ptr s_ErrorStringImp(DWORD dwMessageId, const char *lpcDefault) {
    wchar_t *lpMsgBuf;
    DWORD len;
    char *u8str;
    ptr result;

    len = FormatMessageW(FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS,
                         NULL, dwMessageId, MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), (LPWSTR)&lpMsgBuf, 0, NULL);
    /* If FormatMessage fails... */
    if (len == 0) {
        if (lpcDefault) {
            /* ... use the default string if provided... */
            return Sstring_utf8(lpcDefault, -1);
        } else {
            /* ...otherwise, use the error code in hexadecimal. */
            char buf[(sizeof(dwMessageId) * 2) + 3];
            int n = snprintf(buf, sizeof(buf), "0x%lx", dwMessageId);
            if ((unsigned)n < sizeof(buf))
                return Sstring_utf8(buf, n);
            else
                return Sstring("??");
        }
    }
    /* Otherwise remove trailing newlines & returns and strip trailing period, if present. */
    while (len > 0) {
        wchar_t c = lpMsgBuf[len - 1];
        if (c == L'\n' || c == '\r')
            len--;
        else if (c == L'.') {
            len--;
            break;
        }
        else break;
    }
    lpMsgBuf[len] = 0;
    u8str = Swide_to_utf8(lpMsgBuf);
    LocalFree(lpMsgBuf);
    result = Sstring_utf8(u8str, -1);
    free(u8str);
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

#include <winbase.h>

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
  else {
    int rc;
    if (!(rc = _wrmdir(wpathname))) {
      // Spin loop until Windows deletes the directory.
      int n;
      for (n = 1000; n > 0; n--) {
        if (_wrmdir(wpathname) && (errno == ENOENT)) break;
      }
      return 0;
    }
    return rc;
  }
}

int S_windows_stat64(const char *pathname, struct STATBUF *buffer) {
  wchar_t wpathname[PATH_MAX];
  int len = MultiByteToWideChar(CP_UTF8,0,pathname,-1,wpathname,PATH_MAX);

# ifdef __MINGW32__
  /* MinGW _wstat64 does not want path separators at the end, except for 
     a drive: */
  while ((len > 2)
	 && ((wpathname[len-2] == '/')
	     || (wpathname[len-2] == '\\'))
	 && (wpathname[len-3] != ':')) {
    wpathname[(--len)-1] = 0;
  }
# endif

  if (len == 0)
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
  else {
    int rc;
    if (!(rc = _wunlink(wpathname))) {
      // Spin loop until Windows deletes the file.
      int n;
      for (n = 1000; n > 0; n--) {
        if (_wunlink(wpathname) && (errno == ENOENT)) break;
      }
      return 0;
    }
    return rc;
  }
}

char *S_windows_getcwd(char *buffer, UNUSED int maxlen) {
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

char *Swide_to_utf8(const wchar_t *arg) {
  int len = WideCharToMultiByte(CP_UTF8, 0, arg, -1, NULL, 0, NULL, NULL);
  if (0 == len) return NULL;
  char* arg8 = (char*)malloc(len * sizeof(char));
  if (0 == WideCharToMultiByte(CP_UTF8, 0, arg, -1, arg8, len, NULL, NULL)) {
    free(arg8);
    return NULL;
  }
  return arg8;
}

wchar_t *Sutf8_to_wide(const char *arg) {
  int len = MultiByteToWideChar(CP_UTF8, 0, arg, -1, NULL, 0);
  if (0 == len) return NULL;
  wchar_t* argw = (wchar_t*)malloc(len * sizeof(wchar_t));
  if (0 == MultiByteToWideChar(CP_UTF8, 0, arg, -1, argw, len)) {
    free(argw);
    return NULL;
  }
  return argw;
}

char *Sgetenv(const char *name) {
  wchar_t* wname;
  DWORD n;
  wchar_t buffer[256];
  wname = Sutf8_to_wide(name);
  if (NULL == wname) return NULL;
  n = GetEnvironmentVariableW(wname, buffer, 256);
  if (n == 0) {
    free(wname);
    return NULL;
  } else if (n <= 256) {
    free(wname);
    return Swide_to_utf8(buffer);
  } else {
    wchar_t* value = (wchar_t*)malloc(n * sizeof(wchar_t));
    if (0 == GetEnvironmentVariableW(wname, value, n)) {
      free(wname);
      free(value);
      return NULL;
    } else {
      char* result = Swide_to_utf8(value);
      free(wname);
      free(value);
      return result;
    }
  }
}
