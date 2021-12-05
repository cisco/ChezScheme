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

#ifndef EXPEDITOR_EXTERNAL_USE
# include "config.h"
#endif

#if defined(scheme_feature_pthreads)
# define PTHREADS
#endif

/*****************************************/
/* Architectures                         */

#if (defined(__powerpc__) || defined(__POWERPC__)) && !defined(__powerpc64__)
# define PORTABLE_BYTECODE_BIGENDIAN
# define BIG_ENDIAN_IEEE_DOUBLE
# define FLUSHCACHE
#endif

#if (defined(__arm__) || defined(__arm64__) || defined(__aarch64__))
# define FLUSHCACHE
#endif

#if (machine_type == machine_type_pb)
# undef FLUSHCACHE
#else
# undef PORTABLE_BYTECODE_BIGENDIAN
#endif

/*****************************************/
/* Operating systems                     */

#if defined(__linux__)
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
#ifndef __ANDROID__
# define LOCKF
#endif
#define DIRMARKERP(c) ((c) == '/')
#ifndef DISABLE_X11
# define LIBX11 "libX11.so"
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
#ifdef __ANDROID__
# define NOFILE 256
# define NO_USELOCALE
#endif
#define UNUSED __attribute__((__unused__))
#endif

#if defined(__FreeBSD__) || defined(__FreeBSD_kernel__)
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

#if defined(__NetBSD__)
#ifdef PTHREADS
# define NETBSD
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

#if defined(_MSC_VER) || defined(__MINGW32__)
#define GETPAGESIZE() S_getpagesize()
#define GETWD(x) GETCWD(x, _MAX_PATH)
#define IEEE_DOUBLE
#define LOAD_SHARED_OBJECT
#define USE_VIRTUAL_ALLOC
#define NAN_INCLUDE <math.h>
#define MAKE_NAN(x) { x = sqrt(-1.0); }
#define ARCHYPERBOLIC
#ifndef PATH_MAX
# define PATH_MAX _MAX_PATH
#endif
typedef char *memcpy_t;
struct timespec;
#ifndef __MINGW32__
# define _setjmp setjmp
# define _longjmp longjmp
#endif
#ifndef __MINGW32__
#define ftruncate _chsize_s
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
#if defined(__MINGW32__)
# define UNUSED __attribute__((__unused__))
#else
# define UNUSED
#endif
#if defined(__MINGW32__) && (machine_type == machine_type_ti3nt || machine_type == machine_type_i3nt)
#define time_t __time64_t
#define GET_TIME _time64
#endif
#if defined(_WIN64) && !defined(__MINGW32__)
# define PROVIDE_WINDOWS_UNWIND_INFO
#endif
#endif

#if defined(__OpenBSD__) && !defined(__Bitrig__)
#define NOBLOCK O_NONBLOCK
#define LOAD_SHARED_OBJECT
#define USE_MMAP
#define MMAP_HEAP
#define IEEE_DOUBLE
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

#if defined(__APPLE__)
#define MACOSX
#define NOBLOCK O_NONBLOCK
#define LOAD_SHARED_OBJECT
#define USE_MMAP
#define MMAP_HEAP
#define IEEE_DOUBLE
/* for both iPhone and iPhoneSimulator */
#if defined(TARGET_OS_IPHONE)
# define SYSTEM(s) ((void)s, -1)
# define WRITE_XOR_EXECUTE_CODE
#endif
#if defined(__arm64__)
# if !defined(WRITE_XOR_EXECUTE_CODE)
#  define S_MAP_CODE MAP_JIT
#  define S_ENABLE_CODE_WRITE(on) pthread_jit_write_protect_np(!(on))
# endif
# define CANNOT_READ_DIRECTLY_INTO_CODE
# include <pthread.h>
#elif defined(__x86_64__)
/* needed to run under Rosetta2 on ARM Mac OS: */
# define CANNOT_READ_DIRECTLY_INTO_CODE
#endif
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
# define DISABLE_X11
#endif
#ifndef DISABLE_X11
# define LIBX11 "/usr/X11R6/lib/libX11.dylib"
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

#if defined(__QNX__)
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

#if defined(sun)
#define NOBLOCK O_NONBLOCK
#define LOAD_SHARED_OBJECT
#define USE_MMAP
#define MMAP_HEAP
#define IEEE_DOUBLE
#define LDEXP
#define ARCHYPERBOLIC
#define LOG1P
#define DEFINE_MATHERR
#define NO_USELOCALE
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

/*****************************************/
/* Defaults and derived                  */

#ifndef BIG_ENDIAN_IEEE_DOUBLE
# define LITTLE_ENDIAN_IEEE_DOUBLE
#endif

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

#ifndef S_PROT_CODE
# ifdef WRITE_XOR_EXECUTE_CODE
#  define S_PROT_CODE (PROT_WRITE | PROT_READ)
# else
#  define S_PROT_CODE (PROT_READ | PROT_WRITE | PROT_EXEC)
# endif
#endif
#ifndef S_MAP_CODE
# define S_MAP_CODE 0
#endif
#ifndef S_ENABLE_CODE_WRITE
# define S_ENABLE_CODE_WRITE(on) do { } while (0)
#endif

/* WX_UNUSED indicates that an argument is used only for
   WRITE_XOR_EXECUTE_CODE mode */
#ifndef WX_UNUSED
# ifdef WRITE_XOR_EXECUTE_CODE
#  define WX_UNUSED
# else
#  define WX_UNUSED UNUSED
# endif
#endif

#ifdef PTHREADS
# define NO_THREADS_UNUSED /* empty */
#else
# define NO_THREADS_UNUSED UNUSED
#endif

#if defined(PROVIDE_WINDOWS_UNWIND_INFO)
# define UNUSED_UNLESS_UNWIND /* empty */
#else
# define UNUSED_UNLESS_UNWIND UNUSED
#endif

#if defined(__has_feature)
# if __has_feature(thread_sanitizer)
#  define NO_THREAD_SANITIZE __attribute__((no_sanitize("thread")))
#  define IMPLICIT_ATOMIC_AS_EXPLICIT
# endif
#endif
#ifndef NO_THREAD_SANITIZE
# define NO_THREAD_SANITIZE /* empty */
#endif

/* Use "/dev/urandom" everywhere except Windows */
#define USE_DEV_URANDOM_UUID
