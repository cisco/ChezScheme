/* version.h
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

#include "config.h"

#if (machine_type == machine_type_arm32le || machine_type == machine_type_tarm32le || machine_type == machine_type_arm64le || machine_type == machine_type_tarm64le)
#if (machine_type == machine_type_tarm32le || machine_type == machine_type_tarm64le)
#define PTHREADS
#endif
#define NOBLOCK O_NONBLOCK
#define LOAD_SHARED_OBJECT
#define USE_MMAP
#define MMAP_HEAP
#define IEEE_DOUBLE
#define LITTLE_ENDIAN_IEEE_DOUBLE
#define LDEXP
#define ARCHYPERBOLIC
#define GETPAGESIZE() getpagesize()
typedef char *memcpy_t;
#define MAKE_NAN(x) { x = 0.0; x = x / x; }
#define GETWD(x) getcwd((x),PATH_MAX)
typedef int tputsputcchar;
#define LOCKF
#define DIRMARKERP(c) ((c) == '/')
#define FLUSHCACHE
#ifndef DISABLE_X11
#define LIBX11 "libX11.so"
#endif
#define LSEEK lseek64
#define OFF_T off64_t
#define _LARGEFILE64_SOURCE
#define SECATIME(sb) (sb).st_atim.tv_sec
#define SECCTIME(sb) (sb).st_ctim.tv_sec
#define SECMTIME(sb) (sb).st_mtim.tv_sec
#define NSECATIME(sb) (sb).st_atim.tv_nsec
#define NSECCTIME(sb) (sb).st_ctim.tv_nsec
#define NSECMTIME(sb) (sb).st_mtim.tv_nsec
#define ICONV_INBUF_TYPE char **
#define UNUSED __attribute__((__unused__))
#endif

#if (machine_type == machine_type_ppc32le || machine_type == machine_type_tppc32le || machine_type == machine_type_ppc64le || machine_type == machine_type_tppc64le)
#if (machine_type == machine_type_tppc32le || machine_type == machine_type_tppc64le)
#define PTHREADS
#endif
#define NOBLOCK O_NONBLOCK
#define LOAD_SHARED_OBJECT
#define USE_MMAP
#define MMAP_HEAP
#define IEEE_DOUBLE
#define LDEXP
#define ARCHYPERBOLIC
#define GETPAGESIZE() getpagesize()
typedef char *memcpy_t;
#define MAKE_NAN(x) { x = 0.0; x = x / x; }
#define GETWD(x) getcwd((x),PATH_MAX)
typedef int tputsputcchar;
#define LOCKF
#define DIRMARKERP(c) ((c) == '/')
#define FLUSHCACHE
#ifndef DISABLE_X11
#define LIBX11 "libX11.so"
#endif
#define LSEEK lseek64
#define OFF_T off64_t
#define _LARGEFILE64_SOURCE
#define SECATIME(sb) (sb).st_atim.tv_sec
#define SECCTIME(sb) (sb).st_ctim.tv_sec
#define SECMTIME(sb) (sb).st_mtim.tv_sec
#define NSECATIME(sb) (sb).st_atim.tv_nsec
#define NSECCTIME(sb) (sb).st_ctim.tv_nsec
#define NSECMTIME(sb) (sb).st_mtim.tv_nsec
#define ICONV_INBUF_TYPE char **
#define UNUSED __attribute__((__unused__))
#endif

#if (machine_type == machine_type_i3le || machine_type == machine_type_ti3le || machine_type == machine_type_a6le || machine_type == machine_type_ta6le)
#if (machine_type == machine_type_ti3le || machine_type == machine_type_ta6le)
#define PTHREADS
#endif
#define NOBLOCK O_NONBLOCK
#define LOAD_SHARED_OBJECT
#define USE_MMAP
#define MMAP_HEAP
#define IEEE_DOUBLE
#define LITTLE_ENDIAN_IEEE_DOUBLE
#define LDEXP
#define ARCHYPERBOLIC
#define GETPAGESIZE() getpagesize()
typedef char *memcpy_t;
#define MAKE_NAN(x) { x = 0.0; x = x / x; }
#define GETWD(x) getcwd((x),PATH_MAX)
typedef int tputsputcchar;
#define LOCKF
#define DIRMARKERP(c) ((c) == '/')
#ifndef DISABLE_X11
#define LIBX11 "libX11.so"
#endif
#define LSEEK lseek64
#define OFF_T off64_t
#define _LARGEFILE64_SOURCE
#define SECATIME(sb) (sb).st_atim.tv_sec
#define SECCTIME(sb) (sb).st_ctim.tv_sec
#define SECMTIME(sb) (sb).st_mtim.tv_sec
#define NSECATIME(sb) (sb).st_atim.tv_nsec
#define NSECCTIME(sb) (sb).st_ctim.tv_nsec
#define NSECMTIME(sb) (sb).st_mtim.tv_nsec
#define ICONV_INBUF_TYPE char **
#define UNUSED __attribute__((__unused__))
#endif

#if (machine_type == machine_type_i3fb || machine_type == machine_type_ti3fb || machine_type == machine_type_a6fb || machine_type == machine_type_ta6fb)
#if (machine_type == machine_type_ti3fb || machine_type == machine_type_ta6fb)
#define PTHREADS
#endif
#define NOBLOCK O_NONBLOCK
#define LOAD_SHARED_OBJECT
#define USE_MMAP
#define MMAP_HEAP
#define IEEE_DOUBLE
#define LITTLE_ENDIAN_IEEE_DOUBLE
#define LDEXP
#define ARCHYPERBOLIC
#define GETPAGESIZE() getpagesize()
typedef char *memcpy_t;
#define MAKE_NAN(x) { x = 0.0; x = x / x; }
#define GETWD(x) getcwd((x),PATH_MAX)
typedef int tputsputcchar;
#define LOCKF
#define DIRMARKERP(c) ((c) == '/')
#ifndef DISABLE_X11
#define LIBX11 "libX11.so"
#endif
#define SECATIME(sb) (sb).st_atimespec.tv_sec
#define SECCTIME(sb) (sb).st_ctimespec.tv_sec
#define SECMTIME(sb) (sb).st_mtimespec.tv_sec
#define NSECATIME(sb) (sb).st_atimespec.tv_nsec
#define NSECCTIME(sb) (sb).st_ctimespec.tv_nsec
#define NSECMTIME(sb) (sb).st_mtimespec.tv_nsec
#define ICONV_INBUF_TYPE char **
#define UNUSED __attribute__((__unused__))
#define USE_OSSP_UUID
#endif

#if (machine_type == machine_type_i3nb || machine_type == machine_type_ti3nb || machine_type == machine_type_a6nb || machine_type == machine_type_ta6nb)
#if (machine_type == machine_type_ti3nb || machine_type == machine_type_ta6nb)
#define NETBSD
#define PTHREADS
#endif
#define NOBLOCK O_NONBLOCK
#define LOAD_SHARED_OBJECT
#define USE_MMAP
#define MMAP_HEAP
#define IEEE_DOUBLE
#define LITTLE_ENDIAN_IEEE_DOUBLE
#define LDEXP
#define ARCHYPERBOLIC
#define GETPAGESIZE() getpagesize()
typedef char *memcpy_t;
struct timespec;
#define MAKE_NAN(x) { x = 0.0; x = x / x; }
#define GETWD(x) getcwd((x),PATH_MAX)
typedef int tputsputcchar;
#define LOCKF
#define DIRMARKERP(c) ((c) == '/')
#ifndef DISABLE_X11
#define LIBX11 "libX11.so"
#endif
#define SECATIME(sb) (sb).st_atimespec.tv_sec
#define SECCTIME(sb) (sb).st_ctimespec.tv_sec
#define SECMTIME(sb) (sb).st_mtimespec.tv_sec
#define NSECATIME(sb) (sb).st_atimespec.tv_nsec
#define NSECCTIME(sb) (sb).st_ctimespec.tv_nsec
#define NSECMTIME(sb) (sb).st_mtimespec.tv_nsec
#define ICONV_INBUF_TYPE const char **
#define UNUSED __attribute__((__unused__))
#define USE_NETBSD_UUID
#define USE_MBRTOWC_L
#endif

#if (machine_type == machine_type_i3nt || machine_type == machine_type_ti3nt || machine_type == machine_type_a6nt || machine_type == machine_type_ta6nt)
#if (machine_type == machine_type_ti3nt || machine_type == machine_type_ta6nt)
#define PTHREADS
#endif
#define GETPAGESIZE() S_getpagesize()
#define GETWD(x) GETCWD(x, _MAX_PATH)
#define IEEE_DOUBLE
#define LITTLE_ENDIAN_IEEE_DOUBLE
#define LOAD_SHARED_OBJECT
#define USE_VIRTUAL_ALLOC
#define NAN_INCLUDE <math.h>
#define MAKE_NAN(x) { x = sqrt(-1.0); }
#ifndef PATH_MAX
# define PATH_MAX _MAX_PATH
#endif
typedef char *memcpy_t;
struct timespec;
#ifndef __MINGW32__
# define _setjmp setjmp
# define _longjmp longjmp
# define ftruncate _chsize_s
#endif
#define LOCK_SH 1
#define LOCK_EX 2
#define LOCK_NB 4
#define LOCK_UN 8
#define FLOCK S_windows_flock
#define DIRMARKERP(c) ((c) == '/' || (c) == '\\')
#define CHDIR S_windows_chdir
#define CHMOD S_windows_chmod
#define CLOSE _close
#define DUP _dup
#define FILENO _fileno
#define FSTAT _fstat64
#define GETCWD S_windows_getcwd
#define GETPID _getpid
#define HYPOT _hypot
#define LSEEK _lseeki64
#define LSTAT S_windows_stat64
#define OFF_T __int64
#define OPEN S_windows_open
#define READ _read
#define RENAME S_windows_rename
#define RMDIR S_windows_rmdir
#define STAT S_windows_stat64
#define STATBUF _stat64
#define SYSTEM S_windows_system
#define UNLINK S_windows_unlink
#define WRITE _write
#define SECATIME(sb) (sb).st_atime
#define SECCTIME(sb) (sb).st_ctime
#define SECMTIME(sb) (sb).st_mtime
#define NSECATIME(sb) 0
#define NSECCTIME(sb) 0
#define NSECMTIME(sb) 0
#define ICONV_INBUF_TYPE char **
struct timespec;
#define UNUSED
#endif

#if (machine_type == machine_type_i3ob || machine_type == machine_type_ti3ob || machine_type == machine_type_a6ob || machine_type == machine_type_ta6ob)
#if (machine_type == machine_type_ti3ob || machine_type == machine_type_ta6ob)
#define PTHREADS
#endif
#define NOBLOCK O_NONBLOCK
#define LOAD_SHARED_OBJECT
#define USE_MMAP
#define MMAP_HEAP
#define IEEE_DOUBLE
#define LITTLE_ENDIAN_IEEE_DOUBLE
#define LDEXP
#define ARCHYPERBOLIC
#define GETPAGESIZE() getpagesize()
typedef char *memcpy_t;
struct timespec;
#define MAKE_NAN(x) { x = 0.0; x = x / x; }
#define GETWD(x) getcwd((x),PATH_MAX)
typedef int tputsputcchar;
#define LOCKF
#define DIRMARKERP(c) ((c) == '/')
#ifndef DISABLE_X11
#define LIBX11 "libX11.so"
#endif
#define SECATIME(sb) (sb).st_atimespec.tv_sec
#define SECCTIME(sb) (sb).st_ctimespec.tv_sec
#define SECMTIME(sb) (sb).st_mtimespec.tv_sec
#define NSECATIME(sb) (sb).st_atimespec.tv_nsec
#define NSECCTIME(sb) (sb).st_ctimespec.tv_nsec
#define NSECMTIME(sb) (sb).st_mtimespec.tv_nsec
#define ICONV_INBUF_TYPE char **
#define UNUSED __attribute__((__unused__))
#define USE_OSSP_UUID
#endif

#if (machine_type == machine_type_i3osx || machine_type == machine_type_ti3osx || machine_type == machine_type_a6osx || machine_type == machine_type_ta6osx)
#if (machine_type == machine_type_ti3osx || machine_type == machine_type_ta6osx)
#define PTHREADS
#endif
#if (machine_type == machine_type_a6osx || machine_type == machine_type_ta6osx)
#ifndef NO_ROSETTA_CHECK
#define CHECK_FOR_ROSETTA
extern int is_rosetta;
#endif
#endif
#define MACOSX
#define NOBLOCK O_NONBLOCK
#define LOAD_SHARED_OBJECT
#define USE_MMAP
#define MMAP_HEAP
#define IEEE_DOUBLE
#define LITTLE_ENDIAN_IEEE_DOUBLE
#define LDEXP
#define ARCHYPERBOLIC
#define GETPAGESIZE() getpagesize()
typedef char *memcpy_t;
#define MAKE_NAN(x) { x = 0.0; x = x / x; }
#define GETWD(x) getcwd((x),PATH_MAX)
typedef int tputsputcchar;
#define LOCKF
#define DIRMARKERP(c) ((c) == '/')
#ifndef DISABLE_X11
#define LIBX11 "/usr/X11R6/lib/libX11.dylib"
#endif
#define _DARWIN_USE_64_BIT_INODE
#define SECATIME(sb) (sb).st_atimespec.tv_sec
#define SECCTIME(sb) (sb).st_ctimespec.tv_sec
#define SECMTIME(sb) (sb).st_mtimespec.tv_sec
#define NSECATIME(sb) (sb).st_atimespec.tv_nsec
#define NSECCTIME(sb) (sb).st_ctimespec.tv_nsec
#define NSECMTIME(sb) (sb).st_mtimespec.tv_nsec
#define ICONV_INBUF_TYPE char **
#define UNUSED __attribute__((__unused__))
#endif

#if (machine_type == machine_type_i3qnx || machine_type == machine_type_ti3qnx)
#if (machine_type == machine_type_ti3qnx)
#define PTHREADS
#endif
#define NOBLOCK O_NONBLOCK
#define LOAD_SHARED_OBJECT
#define USE_MMAP
#define MMAP_HEAP
#define IEEE_DOUBLE
#define LITTLE_ENDIAN_IEEE_DOUBLE
#define LDEXP
#define ARCHYPERBOLIC
#define GETPAGESIZE() getpagesize()
typedef char *memcpy_t;
#define MAKE_NAN(x) { x = 0.0; x = x / x; }
#define GETWD(x) getcwd((x),PATH_MAX)
typedef int tputsputcchar;
#define LOCKF
#define DIRMARKERP(c) ((c) == '/')
#define LSEEK lseek64
#define OFF_T off64_t
#define _LARGEFILE64_SOURCE
#define SECATIME(sb) (sb).st_atime
#define SECCTIME(sb) (sb).st_ctime
#define SECMTIME(sb) (sb).st_mtime
#define NSECATIME(sb) 0
#define NSECCTIME(sb) 0
#define NSECMTIME(sb) 0
#define ICONV_INBUF_TYPE char **
#define NOFILE 256
#define UNUSED
#endif

#if (machine_type == machine_type_i3s2 || machine_type == machine_type_ti3s2 || machine_type == machine_type_a6s2 || machine_type == machine_type_ta6s2)
#if (machine_type == machine_type_ti3s2 || machine_type == machine_type_ta6s2)
#define PTHREADS
#endif
#define NOBLOCK O_NONBLOCK
#define LOAD_SHARED_OBJECT
#define USE_MMAP
#define MMAP_HEAP
#define IEEE_DOUBLE
#define LITTLE_ENDIAN_IEEE_DOUBLE
#define LDEXP
#define ARCHYPERBOLIC
#define LOG1P
#define DEFINE_MATHERR
#define GETPAGESIZE() getpagesize()
typedef char *memcpy_t;
#define MAKE_NAN(x) { x = 0.0; x = x / x; }
#define _setjmp setjmp
#define _longjmp longjmp
typedef char tputsputcchar;
#define LOCKF
#define DIRMARKERP(c) ((c) == '/')
#ifndef DISABLE_X11
#define LIBX11 "libX11.so"
#endif
#define SECATIME(sb) (sb).st_atim.tv_sec
#define SECCTIME(sb) (sb).st_ctim.tv_sec
#define SECMTIME(sb) (sb).st_mtim.tv_sec
#define NSECATIME(sb) (sb).st_atim.tv_nsec
#define NSECCTIME(sb) (sb).st_ctim.tv_nsec
#define NSECMTIME(sb) (sb).st_mtim.tv_nsec
#define ICONV_INBUF_TYPE const char **
#define UNUSED __attribute__((__unused__))
#endif

/* defaults */

#ifndef CHDIR
# define CHDIR chdir
#endif
#ifndef CHMOD
# define CHMOD chmod
#endif
#ifndef CLOSE
# define CLOSE close
#endif
#ifndef DUP
# define DUP dup
#endif
#ifndef FILENO
# define FILENO fileno
#endif
#ifndef FSTAT
# define FSTAT fstat
#endif
#ifndef GETPID
# define GETPID getpid
#endif
#ifndef HYPOT
# define HYPOT hypot
#endif
#ifndef OFF_T
# define OFF_T off_t
#endif
#ifndef LSEEK
# define LSEEK lseek
#endif
#ifndef LSTAT
# define LSTAT lstat
#endif
#ifndef OPEN
# define OPEN open
#endif
#ifndef READ
# define READ read
#endif
#ifndef RENAME
# define RENAME rename
#endif
#ifndef RMDIR
# define RMDIR rmdir
#endif
#ifndef STAT
# define STAT stat
#endif
#ifndef STATBUF
# define STATBUF stat
#endif
#ifndef SYSTEM
# define SYSTEM system
#endif
#ifndef UNLINK
# define UNLINK unlink
#endif
#ifndef WRITE
# define WRITE write
#endif
