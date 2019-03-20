/* system.h
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

/* Dispatch to zlib or LZ4 */

#include "system.h"
#include "lz4.h"
#include "lz4frame.h"
#include <fcntl.h>
#include <errno.h>

#ifdef WIN32
# define WIN32_IZE(id) _ ## id
# define GLZ_O_BINARY O_BINARY
#else
# define WIN32_IZE(id) id
# define GLZ_O_BINARY 0
#endif

enum {
      is_gz,
      is_lz4_write,
      is_lz4_read
};

typedef struct lz4File_out {
  int fd;
  void *in_buffer, *out_buffer;
  int in_pos, out_len, out_pos;
  int err;
  size_t stream_pos;
} lz4File_out;

typedef struct lz4File_in {
  int fd;
  LZ4F_dctx *dctx;
  void *in_buffer, *out_buffer;
  int in_pos, in_len, out_pos, out_len;
  int err;
  size_t stream_pos;
  off_t init_pos;
} lz4File_in;

#define USE_LZ4_BUFFER_SIZE (1 << 14)

static glzFile glzdopen_lz4_pos(int fd, const char *mode, off_t init_pos);

glzFile glzdopen_gz(int fd, const char *mode) {
  gzFile gz;

  if ((gz = gzdopen(fd, mode)) == Z_NULL) {
    return Z_NULL;
  } else {
    glzFile glz = malloc(sizeof(struct glzFile_r));
    glz->mode = is_gz;
    glz->gz = gz;
    return glz;
  }
}

glzFile glzdopen_lz4(int fd, const char *mode) {
  return glzdopen_lz4_pos(fd, mode, 0);
}

static glzFile glzdopen_lz4_pos(int fd, const char *mode, off_t init_pos) {
  glzFile glz = malloc(sizeof(struct glzFile_r));

  if (mode[0] == 'r') {
    LZ4F_dctx *dctx;
    LZ4F_errorCode_t r;
    lz4File_in *lz4;
  
    r = LZ4F_createDecompressionContext(&dctx, LZ4F_VERSION);
    if (LZ4F_isError(r))
      return Z_NULL;

    lz4 = malloc(sizeof(lz4File_in));
    lz4->fd = fd;
    lz4->dctx = dctx;
    lz4->in_buffer = malloc(USE_LZ4_BUFFER_SIZE);
    lz4->out_buffer = malloc(USE_LZ4_BUFFER_SIZE);
    lz4->in_pos = 0;
    lz4->in_len = 0;
    lz4->out_len = 0;
    lz4->out_pos = 0;
    lz4->err = 0;
    lz4->stream_pos = 0;
    lz4->init_pos = init_pos;

    glz->mode = is_lz4_read;
    glz->lz4 = (struct lz4File *)lz4;
  } else {
    lz4File_out *lz4 = malloc(sizeof(lz4File_out));
    
    lz4->fd = fd;
    lz4->in_buffer = malloc(USE_LZ4_BUFFER_SIZE);
    lz4->out_buffer = malloc(LZ4F_compressFrameBound(USE_LZ4_BUFFER_SIZE, NULL));
    lz4->in_pos = 0;
    lz4->out_len = 0;
    lz4->out_pos = 0;
    lz4->err = 0;
    lz4->stream_pos = 0;

    glz->mode = is_lz4_write;
    glz->lz4 = (struct lz4File *)lz4;
  }

  return glz;
}

glzFile glzdopen(int fd, const char *mode) {
  if (mode[0] == 'r') {
    int r, pos = 0;
    unsigned char buffer[4];
    off_t init_pos;

    /* check for LZ4 magic number, otherwise defer to gzdopen */

    init_pos = WIN32_IZE(lseek)(fd, 0, SEEK_CUR);

    while (pos < 4) {
      r = WIN32_IZE(read)(fd, (char*)buffer + pos, 4 - pos);
      if (r == 0)
        break;
      else if (r > 0)
        pos += r;
#ifdef EINTR
      else if (r == EINTR)
        r = 0;
#endif
      else
        break; /* error reading */
    }

    if (pos > 0)
      WIN32_IZE(lseek)(fd, init_pos, SEEK_SET);

    if ((pos == 4)
        && (buffer[0] == 0x04)
        && (buffer[1] == 0x22)
        && (buffer[2] == 0x4d)
        && (buffer[3] == 0x18))
      return glzdopen_lz4_pos(fd, mode, init_pos);

    return glzdopen_gz(fd, mode);
  } else
    return glzdopen_gz(fd, mode);
}

/* currently assumes read mode: */
glzFile glzopen(const char *path, const char *mode) {
  int fd;

  fd = WIN32_IZE(open)(path, O_RDONLY | GLZ_O_BINARY);

  if (fd == -1)
    return Z_NULL;
  else
    return glzdopen(fd, mode);
}

#ifdef WIN32
/* currently assumes read mode: */
glzFile glzopen_w(wchar_t *path, const char *mode) {
  int fd;

  fd = _wopen(path, O_RDONLY | GLZ_O_BINARY);

  if (fd == -1)
    return Z_NULL;
  else
    return glzdopen(fd, mode);
}
#endif

int glzdirect(glzFile file) {
  if (file->mode == is_gz)
    return gzdirect(file->gz);
  else
    return 0;
}

int glzclose(glzFile file) {
  if (file->mode == is_gz) {
    int r;
    r = gzclose(file->gz);
    if (r != Z_OK)
      return r;
  } else if (file->mode == is_lz4_write) {
    lz4File_out *lz4 = (lz4File_out *)file->lz4;
    glzwrite(file, NULL /* => flush */, 0);
    while (1) {
      int r = WIN32_IZE(close)(lz4->fd);
      if (r == 0)
        break;
#ifdef EINTR
      else if (r == EINTR)
        r = 0;
#endif
      else
        return r;
    }
    free(lz4->in_buffer);
    free(lz4->out_buffer);
  } else {
    lz4File_in *lz4 = (lz4File_in *)file->lz4;
    while (1) {
      int r = WIN32_IZE(close)(lz4->fd);
      if (r == 0)
        break;
#ifdef EINTR
      else if (r == EINTR)
        r = 0;
#endif
      else
        return r;
    }
    (void)LZ4F_freeDecompressionContext(lz4->dctx);
    free(lz4->in_buffer);
    free(lz4->out_buffer);
  }
  free(file);
  return Z_OK;
}

int glzread(glzFile file, void *buffer, unsigned int count) {
  if (file->mode == is_gz) {
    return gzread(file->gz, buffer, count);
  } else {
    lz4File_in *lz4 = (lz4File_in *)file->lz4;

    while (lz4->out_pos == lz4->out_len) {
      int in_avail;

      in_avail = lz4->in_len - lz4->in_pos;
      if (!in_avail) {
        while (1) {
          in_avail = WIN32_IZE(read)(lz4->fd, (char*)lz4->in_buffer, USE_LZ4_BUFFER_SIZE);
          if (in_avail >= 0) {
            lz4->in_len = in_avail;
            lz4->in_pos = 0;
            break;
#ifdef EINTR
          } else if (in_avail == EINTR) {
            /* try again */
#endif
          } else {
            lz4->err = errno;
            return -1;
          }
        }
      }

      if (in_avail > 0) {
        size_t amt, out_len = USE_LZ4_BUFFER_SIZE, in_len = in_avail;

        /* For a larger enough result buffer, try to decompress directly
           to that buffer: */
        if (count >= (out_len >> 1)) {
          size_t direct_out_len = count;
          amt = LZ4F_decompress(lz4->dctx,
                                buffer, &direct_out_len,
                                (char *)lz4->in_buffer + lz4->in_pos, &in_len,
                                NULL);
        
          if (LZ4F_isError(amt)) {
            lz4->err = amt;
            return -1;
          }

          lz4->in_pos += in_len;

          if (direct_out_len) {
            lz4->stream_pos += direct_out_len;
            return direct_out_len;
          }

          in_len = in_avail - in_len;
        }

        if (in_len > 0) {
          amt = LZ4F_decompress(lz4->dctx,
                                lz4->out_buffer, &out_len,
                                (char *)lz4->in_buffer + lz4->in_pos, &in_len,
                                NULL);
        
          if (LZ4F_isError(amt)) {
            lz4->err = amt;
            return -1;
          }
          
          lz4->in_pos += in_len;
          lz4->out_len = out_len;
          lz4->out_pos = 0;
        }
      } else {
        /* EOF on read */
        break;
      }
    }

    if (lz4->out_pos < lz4->out_len) {
      unsigned amt = lz4->out_len - lz4->out_pos;
      if (amt > count) amt = count;
      memcpy(buffer, (char *)lz4->out_buffer + lz4->out_pos, amt);
      lz4->out_pos += amt;
      lz4->stream_pos += amt;
      return amt;
    }

    return 0;
  }
}

int glzwrite(glzFile file, void *buffer, unsigned int count) {
  if (file->mode == is_gz)
    return gzwrite(file->gz, buffer, count);
  else {
    lz4File_out *lz4 = (lz4File_out *)file->lz4;

    if ((lz4->in_pos == USE_LZ4_BUFFER_SIZE)
        || ((lz4->in_pos > 0) && !buffer)) {
      size_t out_len;

      out_len = LZ4F_compressFrame(lz4->out_buffer, LZ4F_compressFrameBound(USE_LZ4_BUFFER_SIZE, NULL),
                                   lz4->in_buffer, lz4->in_pos,
                                   NULL);
      if (LZ4F_isError(out_len)) {
        lz4->err = out_len;
        return -1;
      }

      lz4->in_pos = 0;
      lz4->out_len = out_len;
      lz4->out_pos = 0;
    }

    while (lz4->out_pos < lz4->out_len) {
      int r = WIN32_IZE(write)(lz4->fd, (char*)lz4->out_buffer + lz4->out_pos, lz4->out_len - lz4->out_pos);
      if (r >= 0)
        lz4->out_pos += r;
#ifdef EINTR
      else if (r == EINTR)
        lz4->out_pos += 0; /* try again */
#endif
      else {
        lz4->err = errno;
        return r;
      }
    }

    {
      unsigned int amt = (USE_LZ4_BUFFER_SIZE - lz4->in_pos);

      if (count < amt)
        amt = count;

      memcpy((char *)lz4->in_buffer + lz4->in_pos, buffer, amt);
      lz4->in_pos += amt;
      lz4->stream_pos += amt;

      return amt;
    }
  }
}

long glzseek(glzFile file, long offset, int whence) {
  if (file->mode == is_gz)
    return gzseek(file->gz, offset, whence);
  else if (file->mode == is_lz4_write) {
    lz4File_out *lz4 = (lz4File_out *)file->lz4;
    if (whence == SEEK_CUR)
      offset += lz4->stream_pos;
    if (offset >= 0) {
      while ((size_t)offset > lz4->stream_pos) {
        size_t amt = (size_t)offset - lz4->stream_pos;
        if (amt > 8) amt = 8;
        if (glzwrite(file, "\0\0\0\0\0\0\0\0", amt) < 0)
          return -1;
      }
    }
    return lz4->stream_pos;
  } else if (file->mode == is_lz4_read) {
    lz4File_in *lz4 = (lz4File_in *)file->lz4;
    if (whence == SEEK_CUR)
      offset += lz4->stream_pos;
    if (offset < 0)
      offset = 0;
    if ((size_t)offset < lz4->stream_pos) {
      /* rewind and read from start */
      if (WIN32_IZE(lseek)(lz4->fd, lz4->init_pos, SEEK_SET) < 0) {
        lz4->err = errno;
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
      char buffer[32];
      size_t amt = (size_t)offset - lz4->stream_pos;
      if (amt > sizeof(buffer)) amt = sizeof(buffer);
      if (glzread(file, buffer, amt) < 0)
        return -1;
    }
    return lz4->stream_pos;
  } else
    return 0;
}

int glzgetc(glzFile file) {
  if (file->mode == is_gz)
    return gzgetc(file->gz);
  else {
    unsigned char buffer[1];
    int r;
    r = glzread(file, buffer, 1);
    if (r == 1)
      return buffer[0];
    return -1;
  }
}
  
int glzungetc(int c, glzFile file) {
  if (file->mode == is_gz)
    return gzungetc(c, file->gz);
  else if (file->mode == is_lz4_read) {
    lz4File_in *lz4 = (lz4File_in *)file->lz4;
    if (lz4->out_len == 0)
      lz4->out_len = lz4->out_pos = 1;
    if (lz4->out_pos) {
      lz4->out_pos--;
      ((unsigned char *)lz4->out_buffer)[lz4->out_pos] = c;
      lz4->stream_pos--;
      return c;
    } else {
      /* support ungetc only just after a getc, in which case there
         should have been room */
      return -1;
    }
  } else
    return -1;
}
  
int glzrewind(glzFile file) {
  return glzseek(file, 0, SEEK_SET);
}

void glzerror(glzFile file, int *errnum)
{
  if (file->mode == is_gz)
    (void)gzerror(file->gz, errnum);
  else if (file->mode == is_lz4_write)
    *errnum = ((lz4File_out *)file->lz4)->err;
  else if (file->mode == is_lz4_read)
    *errnum = ((lz4File_in *)file->lz4)->err;
  else
    *errnum = 0;
}

void glzclearerr(glzFile file)
{
  if (file->mode == is_gz)
    gzclearerr(file->gz);
  else if (file->mode == is_lz4_write)
    ((lz4File_out *)file->lz4)->err = 0;
  else if (file->mode == is_lz4_read)
    ((lz4File_in *)file->lz4)->err = 0;
}
