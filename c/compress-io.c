/* compress-io.c
 * Copyright 1984-2019 Cisco Systems, Inc.
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

/* Dispatch to zlib or LZ4 */

#include "system.h"
#include "zlib.h"
#include "lz4.h"
#include "lz4frame.h"
#include "lz4hc.h"
#include <fcntl.h>
#include <errno.h>

#ifdef WIN32
#include <io.h>
# define WIN32_IZE(id) _ ## id
# define GLZ_O_BINARY O_BINARY
#else
# define WIN32_IZE(id) id
# define GLZ_O_BINARY 0
#endif

/* the value of LZ4_OUTPUT_PORT_IN_BUFFER_SIZE was determined
   through experimentation on an intel linux server and an intel
   osx laptop.  smaller sizes result in significantly worse compression
   of object files, and larger sizes don't have much beneficial effect.
   don't increase the output-port in-buffer size unless you're sure
   it reduces object-file size or reduces compression time
   significantly.  don't decrease it unless you're sure it doesn't
   increase object-file size significantly.  one buffer of size
   LZ4_OUTPUT_PORT_IN_BUFFER_SIZE is allocated per lz4-compressed
   output port.  another buffer of a closely related size is allocated
   per thread. */
#define LZ4_OUTPUT_PORT_IN_BUFFER_SIZE (1 << 18)

/* the values we choose for LZ4_INPUT_PORT_IN_BUFFER_SIZE and
   LZ4_INPUT_PORT_OUT_BUFFER_SIZE don't seem to make much difference
   in decompression speed, so we keep them fairly small.  one buffer
   of size LZ4_INPUT_PORT_IN_BUFFER_SIZE and one buffer of size
   LZ4_INPUT_PORT_OUT_BUFFER_SIZE are allocated per lz4-compressed
   input port. */
#define LZ4_INPUT_PORT_IN_BUFFER_SIZE (1 << 12)
#define LZ4_INPUT_PORT_OUT_BUFFER_SIZE (1 << 14)

typedef struct lz4File_out_r {
  LZ4F_preferences_t preferences;
  INT fd;
  INT out_buffer_size;
  INT in_pos;
  INT err;
  size_t stream_pos;
  char in_buffer[LZ4_OUTPUT_PORT_IN_BUFFER_SIZE];
} lz4File_out;

typedef struct lz4File_in_r {
  INT fd;
  LZ4F_dctx *dctx;
  INT in_pos, in_len, out_pos, out_len;
  INT frame_ended;
  INT err;
  size_t stream_pos;
  off_t init_pos;
  char in_buffer[LZ4_INPUT_PORT_IN_BUFFER_SIZE];
  char out_buffer[LZ4_INPUT_PORT_OUT_BUFFER_SIZE];
} lz4File_in;

typedef struct sized_buffer_r {
  INT size;
  char buffer[0];
} sized_buffer;

/* local functions */
static glzFile glzdopen_output_gz(INT fd, INT compress_level);
static glzFile glzdopen_output_lz4(INT fd, INT compress_level);
static glzFile glzdopen_input_gz(INT fd);
static glzFile glzdopen_input_lz4(INT fd, off_t init_pos);
static INT glzread_lz4(lz4File_in *lz4, void *buffer, UINT count);
static INT glzemit_lz4(lz4File_out *lz4, void *buffer, UINT count);
static INT glzwrite_lz4(lz4File_out *lz4, void *buffer, UINT count);

INT S_zlib_compress_level(INT compress_level) {
  switch (compress_level) {
    case COMPRESS_MIN:
    case COMPRESS_LOW:
      return Z_BEST_SPEED;
    case COMPRESS_MEDIUM:
      return (Z_BEST_SPEED + Z_BEST_COMPRESSION) / 2;
    case COMPRESS_HIGH:
      return (Z_BEST_SPEED + (3 * Z_BEST_COMPRESSION)) / 4;
    case COMPRESS_MAX:
      return Z_BEST_COMPRESSION;
    default:
      S_error1("S_zlib_compress_level", "unexpected compress level ~s", Sinteger(compress_level));
      return 0;
  }
}

static glzFile glzdopen_output_gz(INT fd, INT compress_level) {
  gzFile gz;
  glzFile glz;
  INT as_append;
  INT level;

#ifdef WIN32
  as_append = 0;
#else
  as_append = fcntl(fd, F_GETFL) & O_APPEND;
#endif

  if ((gz = gzdopen(fd, as_append ? "ab" : "wb")) == Z_NULL) return Z_NULL;

  level = S_zlib_compress_level(compress_level);

  gzsetparams(gz, level, Z_DEFAULT_STRATEGY);

  if ((glz = malloc(sizeof(struct glzFile_r))) == NULL) {
    (void)gzclose(gz);
    return Z_NULL;
  }
  glz->fd = fd;
  glz->inputp = 0;
  glz->format = COMPRESS_GZIP;
  glz->gz = gz;
  return glz;
}

INT S_lz4_compress_level(INT compress_level) {
  switch (compress_level) {
    case COMPRESS_MIN:
    case COMPRESS_LOW:
      return 1;
    case COMPRESS_MEDIUM:
      return LZ4HC_CLEVEL_MIN;
    case COMPRESS_HIGH:
      return (LZ4HC_CLEVEL_MIN + LZ4HC_CLEVEL_MAX) / 2;
    case COMPRESS_MAX:
      return LZ4HC_CLEVEL_MAX;
    default:
      S_error1("S_lz4_compress_level", "unexpected compress level ~s", Sinteger(compress_level));
      return 0;
  }
}

static glzFile glzdopen_output_lz4(INT fd, INT compress_level) {
  glzFile glz;
  lz4File_out *lz4;
  INT level;

  level = S_lz4_compress_level(compress_level);

  if ((lz4 = malloc(sizeof(lz4File_out))) == NULL) return Z_NULL;
  memset(&lz4->preferences, 0, sizeof(LZ4F_preferences_t));
  lz4->preferences.compressionLevel = level;
  lz4->fd = fd;
  lz4->out_buffer_size = (INT)LZ4F_compressFrameBound(LZ4_OUTPUT_PORT_IN_BUFFER_SIZE, &lz4->preferences);
  lz4->in_pos = 0;
  lz4->err = 0;
  lz4->stream_pos = 0;

  if ((glz = malloc(sizeof(struct glzFile_r))) == NULL) {
    free(lz4);
    return Z_NULL;
  }
  glz->fd = fd;
  glz->inputp = 0;
  glz->format = COMPRESS_LZ4;
  glz->lz4_out = lz4;
  return glz;
}

glzFile S_glzdopen_output(INT fd, INT compress_format, INT compress_level) {
  switch (compress_format) {
    case COMPRESS_GZIP:
      return glzdopen_output_gz(fd, compress_level);
    case COMPRESS_LZ4:
      return glzdopen_output_lz4(fd, compress_level);
    default:
      S_error1("glzdopen_output", "unexpected compress format ~s", Sinteger(compress_format));
      return Z_NULL;
  }
}

static glzFile glzdopen_input_gz(INT fd) {
  gzFile gz;
  glzFile glz;

  if ((gz = gzdopen(fd, "rb")) == Z_NULL) return Z_NULL;
  
  if ((glz = malloc(sizeof(struct glzFile_r))) == NULL) {
    (void)gzclose(gz);
    return Z_NULL;
  }
  glz->fd = fd;
  glz->inputp = 1;
  glz->format = COMPRESS_GZIP;
  glz->gz = gz;
  return glz;
}

static glzFile glzdopen_input_lz4(INT fd, off_t init_pos) {
  glzFile glz;
  LZ4F_dctx *dctx;
  LZ4F_errorCode_t r;
  lz4File_in *lz4;

  r = LZ4F_createDecompressionContext(&dctx, LZ4F_VERSION);
  if (LZ4F_isError(r))
    return Z_NULL;

  if ((lz4 = malloc(sizeof(lz4File_in))) == NULL) {
    (void)LZ4F_freeDecompressionContext(dctx);
    return Z_NULL;
  }
  lz4->fd = fd;
  lz4->dctx = dctx;
  lz4->in_pos = 0;
  lz4->in_len = 0;
  lz4->out_len = 0;
  lz4->out_pos = 0;
  lz4->frame_ended = 0;
  lz4->err = 0;
  lz4->stream_pos = 0;
  lz4->init_pos = init_pos;

  if ((glz = malloc(sizeof(struct glzFile_r))) == NULL) {
    (void)LZ4F_freeDecompressionContext(lz4->dctx);
    free(lz4);
    return Z_NULL;
  }
  glz->fd = fd;
  glz->inputp = 1;
  glz->format = COMPRESS_LZ4;
  glz->lz4_in = lz4;
  return glz;
}

glzFile S_glzdopen_input(INT fd) {
  INT r, pos = 0;
  unsigned char buffer[4];
  off_t init_pos;

  /* check for LZ4 magic number, otherwise defer to gzdopen */

  if ((init_pos = WIN32_IZE(lseek)(fd, 0, SEEK_CUR)) == -1) return Z_NULL;

  while (pos < 4) {
    r = WIN32_IZE(read)(fd, (char*)buffer + pos, 4 - pos);
    if (r == 0)
      break;
    else if (r > 0)
      pos += r;
#ifdef EINTR
    else if (errno == EINTR)
      continue;
#endif
    else
      break; /* error reading */
  }

  if (pos > 0) {
    if (WIN32_IZE(lseek)(fd, init_pos, SEEK_SET) == -1) return Z_NULL;
  }

  if ((pos == 4)
      && (buffer[0] == 0x04)
      && (buffer[1] == 0x22)
      && (buffer[2] == 0x4d)
      && (buffer[3] == 0x18))
    return glzdopen_input_lz4(fd, init_pos);

  return glzdopen_input_gz(fd);
}

glzFile S_glzopen_input(const char *path) {
  INT fd;

  fd = WIN32_IZE(open)(path, O_RDONLY | GLZ_O_BINARY);

  if (fd == -1)
    return Z_NULL;
  else
    return S_glzdopen_input(fd);
}

#ifdef WIN32
glzFile S_glzopen_input_w(const wchar_t *path) {
  INT fd;

  fd = _wopen(path, O_RDONLY | GLZ_O_BINARY);

  if (fd == -1)
    return Z_NULL;
  else
    return S_glzdopen_input(fd);
}
#endif

IBOOL S_glzdirect(glzFile glz) {
  if (glz->format == COMPRESS_GZIP)
    return gzdirect(glz->gz);
  else
    return 0;
}

INT S_glzclose(glzFile glz) {
  INT r = Z_OK, saved_errno = 0;
  switch (glz->format) {
    case COMPRESS_GZIP:
      r = gzclose(glz->gz);
      break;
    case COMPRESS_LZ4:
      if (glz->inputp) {
        lz4File_in *lz4 = glz->lz4_in;
        while (1) {
          INT r = WIN32_IZE(close)(lz4->fd);
#ifdef EINTR
          if (r < 0 && errno == EINTR) continue;
#endif
	  if (r == 0) { saved_errno = errno; }
          break;
        }
        (void)LZ4F_freeDecompressionContext(lz4->dctx);
        free(lz4);
      } else {
        lz4File_out *lz4 = glz->lz4_out;
        if (lz4->in_pos != 0) {
          r = glzemit_lz4(lz4, lz4->in_buffer, lz4->in_pos);
	  if (r >= 0) r = Z_OK; else { r = Z_ERRNO; saved_errno = errno; }
        }
        while (1) {
          int r1 = WIN32_IZE(close)(lz4->fd);
#ifdef EINTR
          if (r1 < 0 && errno == EINTR) continue;
#endif
	  if (r == Z_OK && r1 < 0) { r = Z_ERRNO; saved_errno = errno; }
          break;
        }
        free(lz4);
      }
      break;
    default:
      S_error1("S_glzclose", "unexpected compress format ~s", Sinteger(glz->format));
  }
  free(glz);
  if (saved_errno) errno = saved_errno;
  return r;
}

static INT glzread_lz4(lz4File_in *lz4, void *buffer, UINT count) {
  while (lz4->out_pos == lz4->out_len) {
    INT in_avail;

    in_avail = lz4->in_len - lz4->in_pos;
    if (!in_avail) {
      while (1) {
        in_avail = WIN32_IZE(read)(lz4->fd, lz4->in_buffer, LZ4_INPUT_PORT_IN_BUFFER_SIZE);
        if (in_avail >= 0) {
          lz4->in_len = in_avail;
          lz4->in_pos = 0;
          break;
#ifdef EINTR
        } else if (errno == EINTR) {
          /* try again */
#endif
        } else {
          lz4->err = Z_ERRNO;
          return -1;
        }
      }
    }

    if (in_avail > 0) {
      size_t amt, out_len = LZ4_INPUT_PORT_OUT_BUFFER_SIZE, in_len = in_avail;

      /* For a large enough result buffer, try to decompress directly
         to that buffer: */
      if (count >= (out_len >> 1)) {
        size_t direct_out_len = count;

        if (lz4->frame_ended && lz4->in_buffer[lz4->in_pos] == 0)
          return 0; /* count 0 after frame as stream terminator */

        amt = LZ4F_decompress(lz4->dctx,
                              buffer, &direct_out_len,
                              lz4->in_buffer + lz4->in_pos, &in_len,
                              NULL);
        lz4->frame_ended = (amt == 0);

        if (LZ4F_isError(amt)) {
          lz4->err = Z_STREAM_ERROR;
          return -1;
        }

        lz4->in_pos += (INT)in_len;

        if (direct_out_len) {
          lz4->stream_pos += direct_out_len;
          return (INT)direct_out_len;
        }

        in_len = in_avail - in_len;
      }

      if (in_len > 0) {
        if (lz4->frame_ended && lz4->in_buffer[lz4->in_pos] == 0)
          return 0; /* count 0 after frame as stream terminator */

        amt = LZ4F_decompress(lz4->dctx,
                              lz4->out_buffer, &out_len,
                              lz4->in_buffer + lz4->in_pos, &in_len,
                              NULL);
        lz4->frame_ended = (amt == 0);

        if (LZ4F_isError(amt)) {
          lz4->err = Z_STREAM_ERROR;
          return -1;
        }

        lz4->in_pos += (INT)in_len;
        lz4->out_len = (INT)out_len;
        lz4->out_pos = 0;
      }
    } else {
      /* EOF on read */
      break;
    }
  }

  if (lz4->out_pos < lz4->out_len) {
    UINT amt = lz4->out_len - lz4->out_pos;
    if (amt > count) amt = count;
    memcpy(buffer, lz4->out_buffer + lz4->out_pos, amt);
    lz4->out_pos += amt;
    lz4->stream_pos += amt;
    return amt;
  }

  return 0;
}

INT S_glzread(glzFile glz, void *buffer, UINT count) {
  switch (glz->format) {
    case COMPRESS_GZIP:
      return gzread(glz->gz, buffer, count);
    case COMPRESS_LZ4:
      return glzread_lz4(glz->lz4_in, buffer, count);
    default:
      S_error1("S_glzread", "unexpected compress format ~s", Sinteger(glz->format));
      return -1;
  }
}

static INT glzemit_lz4(lz4File_out *lz4, void *buffer, UINT count) {
  ptr tc = get_thread_context();
  sized_buffer *cached_out_buffer;
  char *out_buffer;
  INT out_len, out_pos;
  INT r = 0;

  /* allocate one out_buffer (per thread) since we don't need one for each file.
     the buffer is freed by destroy_thread. */
  if ((cached_out_buffer = TO_VOIDP(LZ4OUTBUFFER(tc))) == NULL || cached_out_buffer->size < lz4->out_buffer_size) {
    if (cached_out_buffer != NULL) free(cached_out_buffer);
    if ((LZ4OUTBUFFER(tc) = TO_PTR(cached_out_buffer = malloc(sizeof(sized_buffer) + lz4->out_buffer_size))) == 0) return -1;
    cached_out_buffer->size = lz4->out_buffer_size;
  }
  out_buffer = cached_out_buffer->buffer;

  out_len = (INT)LZ4F_compressFrame(out_buffer, lz4->out_buffer_size,
                                    buffer, count,
                                    &lz4->preferences);
  if (LZ4F_isError(out_len)) {
    lz4->err = Z_STREAM_ERROR;
    return -1;
  }

  out_pos = 0;
  while (out_pos < out_len) {
    r = WIN32_IZE(write)(lz4->fd, out_buffer + out_pos, out_len - out_pos);
    if (r >= 0)
      out_pos += r;
#ifdef EINTR
    else if (errno == EINTR)
      continue;
#endif
    else
      break;
  }

  return r;
}

static INT glzwrite_lz4(lz4File_out *lz4, void *buffer, UINT count) {
  UINT amt; INT r;

  if ((amt = LZ4_OUTPUT_PORT_IN_BUFFER_SIZE - lz4->in_pos) > count) amt = count;

  if (amt == LZ4_OUTPUT_PORT_IN_BUFFER_SIZE) {
    /* full buffer coming from input...skip the memcpy */
    if ((r = glzemit_lz4(lz4, buffer, LZ4_OUTPUT_PORT_IN_BUFFER_SIZE)) < 0) return 0;
  } else {
    memcpy(lz4->in_buffer + lz4->in_pos, buffer, amt);
    if ((lz4->in_pos += amt) == LZ4_OUTPUT_PORT_IN_BUFFER_SIZE) {
      lz4->in_pos = 0;
      if ((r = glzemit_lz4(lz4, lz4->in_buffer, LZ4_OUTPUT_PORT_IN_BUFFER_SIZE)) < 0) return 0;
    }
  }

  lz4->stream_pos += amt;
  return amt;
}

INT S_glzwrite(glzFile glz, void *buffer, UINT count) {
  switch (glz->format) {
    case COMPRESS_GZIP:
      return gzwrite(glz->gz, buffer, count);
    case COMPRESS_LZ4:
      return glzwrite_lz4(glz->lz4_out, buffer, count);
    default:
      S_error1("S_glzwrite", "unexpected compress format ~s", Sinteger(glz->format));
      return -1;
  }
}

long S_glzseek(glzFile glz, long offset, INT whence) {
  switch (glz->format) {
    case COMPRESS_GZIP:
      return gzseek(glz->gz, offset, whence);
    case COMPRESS_LZ4:
      if (glz->inputp) {
        lz4File_in *lz4 = glz->lz4_in;
        if (whence == SEEK_CUR)
          offset += (long)lz4->stream_pos;
        if (offset < 0)
          offset = 0;
        if ((size_t)offset < lz4->stream_pos) {
          /* rewind and read from start */
          if (WIN32_IZE(lseek)(lz4->fd, lz4->init_pos, SEEK_SET) < 0) {
            lz4->err = Z_ERRNO;
            return -1;
          }
          LZ4F_resetDecompressionContext(lz4->dctx);
          lz4->in_pos = 0;
          lz4->in_len = 0;
          lz4->out_len = 0;
          lz4->out_pos = 0;
          lz4->err = 0;
          lz4->stream_pos = 0;
        }
        while ((size_t)offset > lz4->stream_pos) {
          static char buffer[1024];
          size_t amt = (size_t)offset - lz4->stream_pos;
          if (amt > sizeof(buffer)) amt = sizeof(buffer);
          if (glzread_lz4(lz4, buffer, (UINT)amt) < 0)
            return -1;
        }
        return (long)lz4->stream_pos;
      } else {
        lz4File_out *lz4 = glz->lz4_out;
        if (whence == SEEK_CUR)
          offset += (long)lz4->stream_pos;
        if (offset >= 0) {
          while ((size_t)offset > lz4->stream_pos) {
            size_t amt = (size_t)offset - lz4->stream_pos;
            if (amt > 8) amt = 8;
            if (glzwrite_lz4(lz4, "\0\0\0\0\0\0\0\0", (UINT)amt) < 0)
              return -1;
          }
        }
        return (long)lz4->stream_pos;
      }
    default:
      S_error1("S_glzseek", "unexpected compress format ~s", Sinteger(glz->format));
      return -1;
  }
}

INT S_glzgetc(glzFile glz) {
  switch (glz->format) {
    case COMPRESS_GZIP:
      return gzgetc(glz->gz);
    case COMPRESS_LZ4:
      {
        unsigned char buffer[1];
        INT r;
        r = S_glzread(glz, buffer, 1);
        if (r == 1)
          return buffer[0];
        else
          return -1;
      }
    default:
      S_error1("S_glzgetc", "unexpected compress format ~s", Sinteger(glz->format));
      return -1;
  }
}

INT S_glzungetc(INT c, glzFile glz) {
  switch (glz->format) {
    case COMPRESS_GZIP:
      return gzungetc(c, glz->gz);
    case COMPRESS_LZ4:
      {
        lz4File_in *lz4 = glz->lz4_in;
        if (lz4->out_len == 0)
          lz4->out_len = lz4->out_pos = 1;
        if (lz4->out_pos) {
          lz4->out_pos--;
          lz4->out_buffer[lz4->out_pos] = c;
          lz4->stream_pos--;
          return c;
        } else {
          /* support ungetc only just after a getc, in which case there
             should have been room */
          return -1;
        }
      }
    default:
      S_error1("S_glzungetc", "unexpected compress format ~s", Sinteger(glz->format));
      return -1;
  }
}

INT S_glzrewind(glzFile glz) {
  return S_glzseek(glz, 0, SEEK_SET);
}

void S_glzerror(glzFile glz, INT *errnum) {
  switch (glz->format) {
    case COMPRESS_GZIP:
      (void)gzerror(glz->gz, errnum);
      break;
    case COMPRESS_LZ4:
      if (glz->inputp)
        *errnum = glz->lz4_in->err;
      else
        *errnum = glz->lz4_out->err;
      break;
    default:
      S_error1("S_glzerror", "unexpected compress format ~s", Sinteger(glz->format));
      *errnum = 0;
  }
}

void S_glzclearerr(glzFile glz) {
  switch (glz->format) {
    case COMPRESS_GZIP:
      gzclearerr(glz->gz);
      break;
    case COMPRESS_LZ4:
      if (glz->inputp)
        glz->lz4_in->err = 0;
      else
        glz->lz4_out->err = 0;
      break;
    default:
      S_error1("S_glzerror", "unexpected compress format ~s", Sinteger(glz->format));
  }
}
