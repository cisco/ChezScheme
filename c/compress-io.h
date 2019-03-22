#include "zlib.h"

struct lz4File;

typedef struct glzFile_r {
  int mode;
  union {
    gzFile gz;
    struct lz4File *lz4;
  };
} *glzFile;

glzFile glzdopen_gz(int fd, const char *mode);
glzFile glzdopen_lz4(int fd, const char *mode);
glzFile glzdopen(int fd, const char *mode);
glzFile glzopen(const char *path, const char *mode);
#ifdef WIN32
glzFile glzopen_w(wchar_t *path, const char *mode);
#endif
int glzdirect(glzFile file);
int glzclose(glzFile file);

int glzread(glzFile file, void *buffer, unsigned int count);
int glzwrite(glzFile file, void *buffer, unsigned int count);
long glzseek(glzFile file, long offset, int whence);
int glzgetc(glzFile file);
int glzungetc(int c, glzFile file);
int glzrewind(glzFile file);

void glzerror(glzFile file, int *errnum);
void glzclearerr(glzFile fdfile);
