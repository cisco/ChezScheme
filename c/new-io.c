/* new-io.c
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
#else /* WIN32 */
#include <sys/file.h>
#include <dirent.h>
#include <pwd.h>
#endif /* WIN32 */
#include <fcntl.h>
#include "zlib.h"

/* !!! UNLESS you enjoy spending endless days tracking down race conditions
   !!! involving the garbage collector, please note: DEACTIVATE and
   !!! REACTIVATE or LOCKandDEACTIVATE and REACTIVATEandLOCK should be used
   !!! around operations that can block.  While deactivated, the process
   !!! MUST NOT touch any unlocked Scheme objects (ptrs) or allocate any
   !!! new Scheme objects.  It helps to bracket only small pieces of code
   !!! with DEACTIVATE/REACTIVATE or LOCKandDEACTIVATE/REACTIVATE_and_LOCK. */
#ifdef PTHREADS
/* assume the scheme wrapper has us in a critical section */
#define DEACTIVATE(tc) if (DISABLECOUNT(tc) == FIX(1)) { deactivate_thread(tc); }
#define REACTIVATE(tc) if (DISABLECOUNT(tc) == FIX(1)) { reactivate_thread(tc); }
#define LOCKandDEACTIVATE(tc,bv) if (DISABLECOUNT(tc) == FIX(1)) { Slock_object(bv); deactivate_thread(tc); }
#define REACTIVATEandUNLOCK(tc,bv) if (DISABLECOUNT(tc) == FIX(1)) { reactivate_thread(tc); Sunlock_object(bv); }
#else /* PTHREADS */
#define DEACTIVATE(tc)
#define REACTIVATE(tc)
#define LOCKandDEACTIVATE(tc,bv)
#define REACTIVATEandUNLOCK(tc,bv)
#endif /* PTHREADS */

/* locally defined functions */
static ptr new_open_output_fd_helper PROTO((const char *filename, INT mode,
             INT flags, INT no_create, INT no_fail, INT no_truncate,
             INT append, INT lock, INT replace, INT compressed));
static INT lockfile PROTO((INT fd));
static ptr make_gzxfile PROTO((int fd, gzFile file));

/*
 not_ok_is_fatal: !ok definitely implies error, so ignore gzerror
 ok: whether the result of body seems to be ok
 flag: will be set when an error is detected and cleared if no error
 fd: the gzFile object to call gzerror on
 body: the operation we are checking the error on
*/
#ifdef EINTR
/* like FD_EINTR_GUARD and GZ_EINTR_GUARD but ignores EINTR.
   used for calls to close so we don't close a file descriptor that
   might already have been reallocated by a different thread */
#define FD_GUARD(ok,flag,body)                          \
  do { body;                                            \
    flag = !(ok) && errno != EINTR;                     \
  } while (0)
#define GZ_GUARD(not_ok_is_fatal,ok,flag,fd,body) \
  do { body;                                            \
    if (ok) { flag = 0; }                               \
    else {                                              \
      INT errnum;                                       \
      gzerror((fd),&errnum);                            \
      gzclearerr((fd));                                 \
      if (errnum == Z_ERRNO) {                          \
        flag = errno != EINTR;                          \
      } else {                                          \
        flag = not_ok_is_fatal || errnum != Z_OK;       \
        errno = 0;                                      \
      }                                                 \
    }                                                   \
  } while (0)
/* like FD_GUARD and GZ_GUARD but spins on EINTR */
#define FD_EINTR_GUARD(ok,flag,body)                    \
  do { body;                                            \
    if (ok) { flag = 0; break; }                        \
    else if (errno != EINTR) { flag = 1; break; }       \
  } while (1)
#define GZ_EINTR_GUARD(not_ok_is_fatal,ok,flag,fd,body) \
  do { body;                                            \
    if (ok) { flag = 0; break; }                        \
    else {                                              \
      INT errnum;                                       \
      gzerror((fd),&errnum);                            \
      gzclearerr((fd));                                 \
      if (errnum == Z_ERRNO) {                          \
        if (errno != EINTR) { flag = 1; break; }        \
      } else {                                          \
        flag = not_ok_is_fatal || errnum != Z_OK;       \
        errno = 0;                                      \
        break;                                          \
      }                                                 \
    }                                                   \
  } while (1)
#else /* EINTR */
#define FD_GUARD(ok,flag,body) do { body; flag = !(ok); } while (0)
#define GZ_GUARD(not_ok_is_fatal,ok,flag,fd,body)       \
  do { body;                                            \
    if (ok) { flag = 0; }                               \
    else {                                              \
      INT errnum;                                       \
      gzerror((fd),&errnum);                            \
      gzclearerr((fd));                                 \
      if (errnum == Z_ERRNO) { flag = 1; }              \
      else {                                            \
        flag = not_ok_is_fatal || errnum != Z_OK;       \
        errno = 0;                                      \
      }                                                 \
    }                                                   \
  } while (0)
#define FD_EINTR_GUARD FD_GUARD
#define GZ_EINTR_GUARD GZ_GUARD
#endif /* EINTR */

#ifndef O_BINARY
#define O_BINARY 0
#endif /* O_BINARY */


/* These functions are intended for use immediately upon opening
 * (lockfile) fd.  They need to be redesigned for general-purpose 
 * locking. */
#ifdef FLOCK
static INT lockfile(INT fd) { return FLOCK(fd, LOCK_EX); }
#endif
#ifdef LOCKF
static INT lockfile(INT fd) { return lockf(fd, F_LOCK, (off_t)0); }
#endif

/* work around missing zlib API operation to extract a gzFile's fd */
typedef struct {
  int fd;
  gzFile file;
} gzxfile;
#define gzxfile_fd(x) (((gzxfile *)&BVIT(x,0))->fd)
#define gzxfile_gzfile(x) (((gzxfile *)&BVIT(x,0))->file)
static ptr make_gzxfile(int fd, gzFile file) {
  ptr bv;

  bv = S_bytevector(sizeof(gzxfile));
  gzxfile_fd(bv) = fd;
  gzxfile_gzfile(bv) = file;
  return bv;
}
INT S_gzxfile_fd(ptr x) {
  return gzxfile_fd(x);
}
gzFile S_gzxfile_gzfile(ptr x) {
  return gzxfile_gzfile(x);
}

ptr S_new_open_input_fd(const char *infilename, IBOOL compressed) {
  char *filename;
  INT saved_errno = 0;
  INT fd, dupfd, error, result, ok, flag;
  gzFile file;
#ifdef PTHREADS
  ptr tc = get_thread_context();
#endif

  filename = S_malloc_pathname(infilename);

  /* NB: don't use infilename, which might point into a Scheme string, after this point */
  DEACTIVATE(tc)
  FD_EINTR_GUARD(fd>=0, error, fd=OPEN(filename,O_BINARY|O_RDONLY,0));
  saved_errno = errno;
  REACTIVATE(tc)

  /* NB: don't use free'd filename after this point */
  free(filename);

  if (error) {
    ptr str = S_strerror(saved_errno);
    switch (saved_errno) {
      case EACCES:
        return Scons(FIX(OPEN_ERROR_PROTECTION), str);
      case ENOENT:
        return Scons(FIX(OPEN_ERROR_EXISTSNOT), str);
      default:
        return Scons(FIX(OPEN_ERROR_OTHER), str);
    }
  }

  if (!compressed) {
    return MAKE_FD(fd);
  }
    
  if ((dupfd = DUP(fd)) == -1) {
    ptr str = S_strerror(errno);
    FD_GUARD(result == 0, error, result = CLOSE(fd));
    return Scons(FIX(OPEN_ERROR_OTHER), str);
  }
  
  if ((file = gzdopen(dupfd, "rb")) == Z_NULL) {
    FD_GUARD(result == 0, error, result = CLOSE(fd));
    FD_GUARD(result == 0, error, result = CLOSE(dupfd));
    return Scons(FIX(OPEN_ERROR_OTHER), Sstring("unable to allocate compression state (too many open files?)"));
  }

  DEACTIVATE(tc)
  compressed = !gzdirect(file);
  REACTIVATE(tc)

  if (compressed) {
    FD_GUARD(result == 0, error, result = CLOSE(fd));
    /* box indicates gzip'd */
    return Sbox(make_gzxfile(dupfd, file));
  }

  GZ_GUARD(1, ok == 0 || ok == Z_BUF_ERROR, flag, file, ok = gzclose(file));
  if (flag) {} /* make the compiler happy */
  if (LSEEK(fd, 0, SEEK_SET) != 0) { /* gzdirect does not leave fd at position 0 */
    FD_GUARD(result == 0, error, result = CLOSE(fd));
    return Scons(FIX(OPEN_ERROR_OTHER),Sstring("unable to reset after reading header bytes"));
  }
  return MAKE_FD(fd);
}

ptr S_compress_input_fd(INT fd, I64 pos) {
  INT dupfd, error, result, ok, flag; IBOOL compressed;
  gzFile file;
#ifdef PTHREADS
  ptr tc = get_thread_context();
#endif

  if ((dupfd = DUP(fd)) == -1) {
    return S_strerror(errno);
  }
  
  if ((file = gzdopen(dupfd, "rb")) == Z_NULL) {
    FD_GUARD(result == 0, error, result = CLOSE(dupfd));
    return Sstring("unable to allocate compression state (too many open files?)");
  }
  
  DEACTIVATE(tc)
  compressed = !gzdirect(file);
  REACTIVATE(tc)

  if (compressed) {
    FD_GUARD(result == 0, error, result = CLOSE(fd));
    if (error) {} /* make the compiler happy */
    return Sbox(make_gzxfile(dupfd, file));
  }

  GZ_GUARD(1, ok == 0 || ok == Z_BUF_ERROR, flag, file, ok = gzclose(file));
  if (flag) {} /* make the compiler happy */
  if (LSEEK(fd, pos, SEEK_SET) != pos) { /* gzdirect does not leave fd at same position */
    return Sstring("unable to reset after reading header bytes");
  }
  return MAKE_FD(fd);
}

ptr S_compress_output_fd(INT fd) {
  gzFile file;

#ifdef WIN32
  if ((file = gzdopen(fd, "wb")) == Z_NULL) {
#else
  if ((file = gzdopen(fd, (fcntl(fd, F_GETFL) & O_APPEND) ? "ab" : "wb")) == Z_NULL) {
#endif
    return Sstring("unable to allocate compression state (too many open files?)");
  }

  return Sbox(make_gzxfile(fd, file));
}

static ptr new_open_output_fd_helper(
  const char *infilename, INT mode, INT flags,
  IBOOL no_create, IBOOL no_fail, IBOOL no_truncate,
  IBOOL append, IBOOL lock, IBOOL replace, IBOOL compressed) {
  char *filename;
  INT saved_errno = 0;
  iptr error;
  INT fd, result;
#ifdef PTHREADS
  ptr tc = get_thread_context();
#endif

  flags |=
    (no_create ? 0 : O_CREAT) |
    ((no_fail || no_create) ? 0 : O_EXCL) |
    (no_truncate ? 0 : O_TRUNC) |
    ((!append) ? 0 : O_APPEND);

  filename = S_malloc_pathname(infilename);
  
  if (replace && UNLINK(filename) != 0 && errno != ENOENT) {
    ptr str = S_strerror(errno);
    switch (errno) {
      case EACCES:
        return Scons(FIX(OPEN_ERROR_PROTECTION), str);
      default:
        return Scons(FIX(OPEN_ERROR_OTHER), str);
    }
  }
  
  /* NB: don't use infilename, which might point into a Scheme string, after this point */
  DEACTIVATE(tc)
  FD_EINTR_GUARD(fd >= 0, error, fd = OPEN(filename, flags, mode));
  saved_errno = errno;
  REACTIVATE(tc)

  /* NB: don't use free'd filename after this point */
  free(filename);

  if (error) {
    ptr str = S_strerror(saved_errno);
    switch (saved_errno) {
      case EACCES:
        return Scons(FIX(OPEN_ERROR_PROTECTION), str);
      case EEXIST:
        return Scons(FIX(OPEN_ERROR_EXISTS), str);
      case ENOENT:
        return Scons(FIX(OPEN_ERROR_EXISTSNOT), str);
      default:
        return Scons(FIX(OPEN_ERROR_OTHER), str);
    }
  }
  
  if (lock) {
    DEACTIVATE(tc)
    error = lockfile(fd);
    saved_errno = errno;
    REACTIVATE(tc)
    if (error) {
      FD_GUARD(result == 0, error, result = CLOSE(fd));
      return Scons(FIX(OPEN_ERROR_OTHER), S_strerror(saved_errno));
    }
  }
  
  if (!compressed) {
    return MAKE_FD(fd);
  }
  
  gzFile file = gzdopen(fd, append ? "ab" : "wb");
  if (file == Z_NULL) {
    FD_GUARD(result == 0, error, result = CLOSE(fd));
    return Scons(FIX(OPEN_ERROR_OTHER), Sstring("unable to allocate compression state"));
  }

  return make_gzxfile(fd, file);
}

ptr S_new_open_output_fd(
  const char *filename, INT mode,
  IBOOL no_create, IBOOL no_fail, IBOOL no_truncate,
  IBOOL append, IBOOL lock, IBOOL replace, IBOOL compressed) {
  return new_open_output_fd_helper(
    filename, mode, O_BINARY | O_WRONLY,
    no_create, no_fail, no_truncate,
    append, lock, replace, compressed);
}

ptr S_new_open_input_output_fd(
  const char *filename, INT mode,
  IBOOL no_create, IBOOL no_fail, IBOOL no_truncate,
  IBOOL append, IBOOL lock, IBOOL replace, IBOOL compressed) {
  if (compressed)
    return Sstring("compressed input/output files not supported");
  else
    return new_open_output_fd_helper(
      filename, mode, O_BINARY | O_RDWR,
      no_create, no_fail, no_truncate,
      append, lock, replace, compressed);
}

ptr S_close_fd(ptr file, IBOOL gzflag) {
  INT saved_errno = 0;
  INT ok, flag;
  INT fd = gzflag ? 0 : GET_FD(file);
  gzFile gzfile = gzflag ? gzxfile_gzfile(file) : NULL;
#ifdef PTHREADS
  ptr tc = get_thread_context();
#endif

 /* refuse to close stdin, stdout, and stderr fds */
  if (!gzflag && fd <= 2) return Strue;

 /* file is not locked; do not reference after deactivating thread! */
  file = (ptr)-1;

 /* NOTE: close automatically releases locks so we don't to call unlock*/
  DEACTIVATE(tc)
  if (!gzflag) {
    FD_GUARD(ok == 0, flag, ok = CLOSE(fd));
  } else {
   /* zlib 1.2.1 returns Z_BUF_ERROR when closing an empty file opened for reading */
    GZ_GUARD(1, ok == 0 || ok == Z_BUF_ERROR, flag, gzfile, ok = gzclose(gzfile));
  }
  saved_errno = errno;
  REACTIVATE(tc)

  if (!flag) {
    return Strue;
  }

  if (gzflag && saved_errno == 0) {
    return Sstring("compression failed");
  }

  return S_strerror(saved_errno);
}

#define GZ_IO_SIZE_T unsigned int

#ifdef WIN32
#define IO_SIZE_T unsigned int
#else /* WIN32 */
#define IO_SIZE_T size_t
#endif /* WIN32 */

/* Returns string on error, #!eof on end-of-file and integer-count otherwise */
ptr S_bytevector_read(ptr file, ptr bv, iptr start, iptr count, IBOOL gzflag) {
  INT saved_errno = 0;
  ptr tc = get_thread_context();
  iptr m, flag = 0;
  INT fd = gzflag ? 0 : GET_FD(file);
  gzFile gzfile = gzflag ? gzxfile_gzfile(file) : NULL;

 /* file is not locked; do not reference after deactivating thread! */
  file = (ptr)-1;

#if (iptr_bits > 32)
  if ((WIN32 || gzflag) && (unsigned int)count != count) count = 0xffffffff;
#endif

  LOCKandDEACTIVATE(tc, bv)
#ifdef WIN32
  if (!gzflag && fd == 0) {
    DWORD error_code;
    SetConsoleCtrlHandler(NULL, TRUE);
    SetLastError(0);
    m = _read(0, &BVIT(bv,start), (IO_SIZE_T)count);
    error_code = GetLastError();
    SetConsoleCtrlHandler(NULL, FALSE);
    if (m == 0 && error_code == 0x3e3) {
      KEYBOARDINTERRUPTPENDING(tc) = Strue;
      SOMETHINGPENDING(tc) = Strue;
    }
  } else
#endif /* WIN32 */
  {
    if (!gzflag) {
      FD_EINTR_GUARD(
        m >= 0 || Sboolean_value(KEYBOARDINTERRUPTPENDING(tc)), flag,
        m = READ(fd,&BVIT(bv,start),(IO_SIZE_T)count));
    } else {
      GZ_EINTR_GUARD(
        1, m >= 0 || Sboolean_value(KEYBOARDINTERRUPTPENDING(tc)),
        flag, gzfile,
        m = gzread(gzfile, &BVIT(bv,start), (GZ_IO_SIZE_T)count));
    }
  }
  saved_errno = errno;
  REACTIVATEandUNLOCK(tc, bv)

  if (Sboolean_value(KEYBOARDINTERRUPTPENDING(tc))) {
    return Sstring("interrupt");
  }

  if (!flag) {
    return m == 0 ? Seof_object : FIX(m);
  }

  if (saved_errno == EAGAIN) {
    return FIX(0);
  }

  return S_strerror(saved_errno);
}

/* Returns:
    string on error, including if not supported,
    n when read,
    0 on non-blocking and
    #!eof otherwise */
ptr S_bytevector_read_nb(ptr file, ptr bv, iptr start, iptr count, IBOOL gzflag) {
#ifdef WIN32
  HANDLE h;

 /* assume compressed files are always ready */
  if (gzflag) return FIX(1);

  if ((h = (HANDLE)_get_osfhandle(GET_FD(file))) != INVALID_HANDLE_VALUE) {
    switch (GetFileType(h)) {
      case FILE_TYPE_CHAR:
       /* if h is hStdin, PeekConsoleInput can tell us if a key down event
          is waiting, but if it's not a newline, we can't be sure that
          a read will succeed.  so PeekConsoleInput is basically useless
          for our purposes. */
        break;
      case FILE_TYPE_PIPE: {
        DWORD bytes;
        if (PeekNamedPipe(h, NULL, 0, NULL, &bytes, NULL) && bytes == 0) return FIX(0);
       /* try the read on error or if bytes > 0 */
        return S_bytevector_read(file, bv, start, count, gzflag);
      }
      default: {
        if (WaitForSingleObject(h, 0) == WAIT_TIMEOUT) return FIX(0);
       /* try the read on error or if bytes > 0 */
        return S_bytevector_read(file, bv, start, count, gzflag);
      }
    }
  }

  return Sstring("cannot determine ready status");
#else /* WIN32 */
  INT fcntl_flags;
  ptr result;
  INT fd;

 /* assume compressed files are always ready */
  if (gzflag) return FIX(1);

  fd = GET_FD(file);

 /* set NOBLOCK for nonblocking read */
  fcntl_flags = fcntl(fd, F_GETFL, 0);
  if (!(fcntl_flags & NOBLOCK)) (void) fcntl(fd, F_SETFL, fcntl_flags | NOBLOCK);

  result = S_bytevector_read(file, bv, start, count, gzflag);

 /* reset NOBLOCK for normal blocking read */
  if (!(fcntl_flags & NOBLOCK)) (void) fcntl(fd, F_SETFL, fcntl_flags);

  return result;
#endif /* WIN32 */
}

ptr S_bytevector_write(ptr file, ptr bv, iptr start, iptr count, IBOOL gzflag) {
  iptr i, s, c;
  ptr tc = get_thread_context();
  INT flag = 0, saved_errno = 0;
  INT fd = gzflag ? 0 : GET_FD(file);
  gzFile gzfile = gzflag ? gzxfile_gzfile(file) : NULL;

  for (s = start, c = count; c > 0; s += i, c -= i) {
    iptr cx = c;

#if (iptr_bits > 32)
  if ((WIN32 || gzflag) && (unsigned int)cx != cx) cx = 0xffffffff;
#endif

  /* if we could know that fd is nonblocking, we wouldn't need to deactivate.
     we could test ioctl, but some other thread could change it before we actually
     get around to writing. */
    LOCKandDEACTIVATE(tc, bv)
    if (gzflag) {
     /* strangely, gzwrite returns 0 on error */
      GZ_EINTR_GUARD(
        i < 0, i > 0 || Sboolean_value(KEYBOARDINTERRUPTPENDING(tc)),
        flag, gzfile,
        i = gzwrite(gzfile, &BVIT(bv,s), (GZ_IO_SIZE_T)cx));
    } else {
      FD_EINTR_GUARD(i >= 0 || Sboolean_value(KEYBOARDINTERRUPTPENDING(tc)),
                     flag, i = WRITE(fd, &BVIT(bv,s), (IO_SIZE_T)cx));
    }
    saved_errno = errno;
    REACTIVATEandUNLOCK(tc, bv)

    if (flag) {
      if (saved_errno == EAGAIN) { flag = 0; }
      break;
    }

   /* we escape from loop if keyboard interrupt is pending, but this won't
      do much good until we fix up the interrupt protocol to guarantee
      that the interrupt handler is actually called */
    if (Sboolean_value(KEYBOARDINTERRUPTPENDING(tc))) {
      if (i >= 0) s += i;
      break;
    }
  }

  if (!flag) {
    return FIX(s - start);
  }

  if (saved_errno == EAGAIN) {
    return FIX(0);
  }

  if (gzflag && saved_errno == 0) {
    return Sstring("compression failed");
  }

  return S_strerror(saved_errno);
}

/* S_put_byte is a simplified version of S_bytevector_write for writing one
   byte on unbuffered ports */
ptr S_put_byte(ptr file, INT byte, IBOOL gzflag) {
  iptr i;
  ptr tc = get_thread_context();
  INT flag = 0, saved_errno = 0;
  INT fd = gzflag ? 0 : GET_FD(file);
  gzFile gzfile = gzflag ? gzxfile_gzfile(file) : NULL;
  octet buf[1];

  buf[0] = (octet)byte;

  DEACTIVATE(tc)
  if (gzflag) {
   /* strangely, gzwrite returns 0 on error */
    GZ_EINTR_GUARD(
      i < 0, i > 0 || Sboolean_value(KEYBOARDINTERRUPTPENDING(tc)),
      flag, gzfile,
      i = gzwrite(gzfile, buf, 1));
  } else {
    FD_EINTR_GUARD(i >= 0 || Sboolean_value(KEYBOARDINTERRUPTPENDING(tc)),
                   flag, i = WRITE(fd, buf, 1));
  }
  saved_errno = errno;
  REACTIVATE(tc)

  if (flag) {
    if (saved_errno == EAGAIN) { flag = 0; }
  }

  if (!flag) {
    return FIX(i);
  }

  if (saved_errno == EAGAIN) {
    return FIX(0);
  }

  if (gzflag && saved_errno == 0) {
    return Sstring("compression failed");
  }

  return S_strerror(saved_errno);
}

ptr S_get_fd_pos(ptr file, IBOOL gzflag) {
  errno = 0;
  if (gzflag) {
    z_off_t offset = gzseek(gzxfile_gzfile(file), 0, SEEK_CUR);
    if (offset != -1) return Sinteger64(offset);
  } else {
    OFF_T offset = LSEEK(GET_FD(file), 0, SEEK_CUR);
    if (offset != -1) return Sinteger64(offset);
  }
  if (gzflag && errno == 0) return Sstring("compression failed");
  return S_strerror(errno);
}

/* assume wrapper ensures 0 <= pos <= 2^63-1 */
ptr S_set_fd_pos(ptr file, ptr pos, IBOOL gzflag) {
  I64 offset64 = S_int64_value("set-file-position", pos);

  if (gzflag) {
    z_off_t offset = (z_off_t)offset64;
    if (sizeof(z_off_t) != sizeof(I64))
      if (offset != offset64) return Sstring("invalid position");
    errno = 0;
    if (gzseek(gzxfile_gzfile(file),offset,SEEK_SET) == offset) return Strue;
    if (errno == 0) return Sstring("compression failed");
    return S_strerror(errno);
  } else {
    OFF_T offset = (OFF_T)offset64;
    if (sizeof(OFF_T) != sizeof(I64))
      if (offset != offset64) return Sstring("invalid position");
    if (LSEEK(GET_FD(file), offset, SEEK_SET) == offset) return Strue;
    return S_strerror(errno);
  }
}

ptr S_get_fd_non_blocking(ptr file, IBOOL gzflag) {
#ifdef WIN32
  return Sfalse;
#else /* WIN32 */
  INT fcntl_flags;

  if (gzflag) return Sfalse;

  fcntl_flags = fcntl(GET_FD(file), F_GETFL, 0);

  if (fcntl_flags == -1) {
    return S_strerror(errno);
  }

  return Sboolean(NOBLOCK & fcntl_flags);
#endif /* WIN32 */
}

ptr S_set_fd_non_blocking(ptr file, IBOOL x, IBOOL gzflag) {
#ifdef WIN32
  return Sstring("unsupported");
#else /* WIN32 */
  iptr fd;
  INT fcntl_flags;

  if (gzflag) {
    if (x) return Sstring("Compressed non-blocking ports not supported");
    else return Strue;
  }

  fd = GET_FD(file);
  fcntl_flags = fcntl(fd, F_GETFL, 0);

  if (fcntl_flags == -1) {
    return S_strerror(errno);
  }

  if (x) {
    if (fcntl_flags & NOBLOCK) {
      return Strue;
    }
    if (0 == fcntl(fd, F_SETFL, fcntl_flags | NOBLOCK)) {
      return Strue;
    }
    return S_strerror(errno);
  } else {
    if (!(fcntl_flags & NOBLOCK)) {
      return Strue;
    }
    if (0 == fcntl(fd, F_SETFL, fcntl_flags & ~NOBLOCK)) {
      return Strue;
    }
    return S_strerror(errno);
  }
#endif /* WIN32 */
}

ptr S_get_fd_length(ptr file, IBOOL gzflag) {
  struct STATBUF statbuf;

  if (gzflag) return Sstring("Not supported on compressed files");

  if (FSTAT(GET_FD(file), &statbuf) == 0) {
    return Sinteger64(statbuf.st_size);
  }

  return S_strerror(errno);    
}

ptr S_set_fd_length(ptr file, ptr length, IBOOL gzflag) {
  INT fd, ok, flag = 0;
  I64 len64; off_t len;
#ifdef PTHREADS
  ptr tc = get_thread_context();
#endif

  if (gzflag) return Sstring("Not supported on compressed files");

  len64 = S_int64_value("set-file-length", length);
  len = (off_t)len64;
  if (sizeof(off_t) != sizeof(I64))
    if (len != len64) return Sstring("invalid length");

  fd = GET_FD(file);
  DEACTIVATE(tc)
  FD_EINTR_GUARD(ok == 0, flag, ok = ftruncate(fd, len));
  REACTIVATE(tc)

  return flag ? S_strerror(errno) : Strue;
}

void S_new_io_init() {
  if (S_boot_time) {
    S_set_symbol_value(S_intern((const unsigned char *)"$c-bufsiz"), Sinteger(SBUFSIZ));
  }
#ifdef WIN32
 /* transcoder, if any, does its own cr, lf translations */
  _setmode(_fileno(stdin), O_BINARY);
  _setmode(_fileno(stdout), O_BINARY);
  _setmode(_fileno(stderr), O_BINARY);
#endif /* WIN32 */
}
