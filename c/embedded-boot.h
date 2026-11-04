#ifndef CHEZSCHEME_EMBEDDED_BOOT_H
#define CHEZSCHEME_EMBEDDED_BOOT_H

/*
 * This header uses Chez's iptr type.
 *
 * Include scheme.h or system.h before including this file.
 */

typedef enum {
    S_EMBEDDED_BOOT_INVALID  = -2,
    S_EMBEDDED_BOOT_IO_ERROR = -1,
    S_EMBEDDED_BOOT_NONE     =  0,
    S_EMBEDDED_BOOT_FOUND    =  1
} S_embedded_boot_status;

typedef struct {
    int fd;
    iptr offset;
    iptr length;
} S_embedded_boot;

/*
 * Inspect the running executable for an appended Chez boot image.
 *
 * On S_EMBEDDED_BOOT_FOUND:
 *
 *   boot->fd     is open and owned by the caller
 *   boot->offset is the byte offset of the embedded boot
 *   boot->length is the exact boot length
 *
 * The caller should normally transfer ownership of boot->fd to Chez:
 *
 *   Sregister_boot_file_fd_region(
 *       "<embedded>",
 *       boot->fd,
 *       boot->offset,
 *       boot->length,
 *       1);
 *
 * On every result other than S_EMBEDDED_BOOT_FOUND:
 *
 *   boot->fd     == -1
 *   boot->offset == 0
 *   boot->length == 0
 */
S_embedded_boot_status
S_find_embedded_boot(
    const char *execpath,
    S_embedded_boot *boot);

const char *
S_embedded_boot_status_message(
    S_embedded_boot_status status);

#endif
