#include "system.h"
#include "embedded-boot.h"

#include <errno.h>
#include <fcntl.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#ifdef WIN32
#include <io.h>
#endif

#ifndef O_BINARY
#define O_BINARY 0
#endif

#define EMBEDDED_BOOT_TRAILER_SIZE 32
#define EMBEDDED_BOOT_VERSION 1

#define EMBEDDED_BOOT_VERSION_OFFSET  8
#define EMBEDDED_BOOT_FLAGS_OFFSET    12
#define EMBEDDED_BOOT_LENGTH_OFFSET   16
#define EMBEDDED_BOOT_RESERVED_OFFSET 24

static const unsigned char embedded_boot_magic[8] = {
    'C', 'H', 'E', 'Z', 'B', 'O', 'O', 'T'
};


static uint32_t
read_u32le(const unsigned char *p)
{
    return ((uint32_t)p[0])
         | ((uint32_t)p[1] << 8)
         | ((uint32_t)p[2] << 16)
         | ((uint32_t)p[3] << 24);
}


static uint64_t
read_u64le(const unsigned char *p)
{
    return ((uint64_t)p[0])
         | ((uint64_t)p[1] << 8)
         | ((uint64_t)p[2] << 16)
         | ((uint64_t)p[3] << 24)
         | ((uint64_t)p[4] << 32)
         | ((uint64_t)p[5] << 40)
         | ((uint64_t)p[6] << 48)
         | ((uint64_t)p[7] << 56);
}


/*
 * Read exactly len bytes.
 *
 * READ maps to read() on Unix and _read() on Windows through Chez's
 * platform layer. Retry interrupted reads.
 */
static int
read_exact(
    int fd,
    unsigned char *buf,
    unsigned int len)
{
    while (len != 0) {
        int n = (int)READ(fd, buf, len);

        if (n > 0) {
            buf += n;
            len -= (unsigned int)n;
            continue;
        }

        if (n < 0 && errno == EINTR)
            continue;

        return 0;
    }

    return 1;
}


/*
 * Convert an unsigned 64-bit trailer field to Chez's iptr without
 * truncation.
 */
static int
u64_to_iptr(
    uint64_t value,
    iptr *result)
{
    iptr n = (iptr)value;

    if (n < 0)
        return 0;

    if ((uint64_t)n != value)
        return 0;

    *result = n;
    return 1;
}


/*
 * Convert a platform-native file offset to iptr without truncation.
 *
 * Chez defines OFF_T appropriately per platform:
 *
 *   Windows      __int64
 *   Linux        off64_t
 *   macOS/BSD    off_t
 */
static int
off_t_to_iptr(
    OFF_T value,
    iptr *result)
{
    iptr n;

    if (value < (OFF_T)0)
        return 0;

    n = (iptr)value;

    if (n < 0)
        return 0;

    if ((OFF_T)n != value)
        return 0;

    *result = n;
    return 1;
}


const char *
S_embedded_boot_status_message(
    S_embedded_boot_status status)
{
    switch (status) {
    case S_EMBEDDED_BOOT_FOUND:
        return "embedded Chez boot image found";

    case S_EMBEDDED_BOOT_NONE:
        return "no embedded Chez boot image";

    case S_EMBEDDED_BOOT_IO_ERROR:
        return "unable to read embedded Chez boot metadata";

    case S_EMBEDDED_BOOT_INVALID:
        return "invalid embedded Chez boot metadata";

    default:
        return "unknown embedded Chez boot status";
    }
}


S_embedded_boot_status
S_find_embedded_boot(
    const char *execpath,
    S_embedded_boot *boot)
{
    char *resolved_path;
    int fd;

    OFF_T end;
    OFF_T trailer_offset;
    OFF_T boot_offset;

    unsigned char trailer[EMBEDDED_BOOT_TRAILER_SIZE];

    uint32_t version;
    uint32_t flags;

    uint64_t boot_length;
    uint64_t reserved;
    uint64_t payload_length;
    uint64_t boot_offset_u64;

    iptr chez_boot_offset;
    iptr chez_boot_length;


    /*
     * Maintain a strong result invariant:
     *
     * if FOUND is not returned, no descriptor is owned by the caller.
     */
    boot->fd = -1;
    boot->offset = 0;
    boot->length = 0;


    /*
     * Resolve argv[0] to the actual running executable.
     *
     * Failure here should not change normal Chez behavior. An ordinary
     * Chez launcher can continue with its existing boot discovery.
     */
    resolved_path =
        S_get_process_executable_path(execpath);

    if (resolved_path == NULL)
        return S_EMBEDDED_BOOT_NONE;


    /*
     * OPEN is Chez's portable open abstraction:
     *
     *   Windows: S_windows_open, including UTF-16 pathname support
     *   Unix:    open
     */
    fd = OPEN(
        resolved_path,
        O_RDONLY | O_BINARY,
        0);

    free(resolved_path);

    if (fd < 0)
        return S_EMBEDDED_BOOT_NONE;


    /*
     * LSEEK/OFF_T are already large-file safe in Chez:
     *
     *   MSVC/MinGW: _lseeki64 / __int64
     *   Linux:      lseek64 / off64_t
     *   macOS/BSD:  lseek / off_t
     */
    end = LSEEK(
        fd,
        (OFF_T)0,
        SEEK_END);

    if (end < (OFF_T)0) {
        CLOSE(fd);
        return S_EMBEDDED_BOOT_IO_ERROR;
    }


    /*
     * Too small to contain the 32-byte trailer means this is simply not
     * an embedded executable.
     */
    if (end < (OFF_T)EMBEDDED_BOOT_TRAILER_SIZE) {
        CLOSE(fd);
        return S_EMBEDDED_BOOT_NONE;
    }


    trailer_offset =
        end - (OFF_T)EMBEDDED_BOOT_TRAILER_SIZE;

    if (LSEEK(
            fd,
            trailer_offset,
            SEEK_SET)
        != trailer_offset) {
        CLOSE(fd);
        return S_EMBEDDED_BOOT_IO_ERROR;
    }


    if (!read_exact(
            fd,
            trailer,
            EMBEDDED_BOOT_TRAILER_SIZE)) {
        CLOSE(fd);
        return S_EMBEDDED_BOOT_IO_ERROR;
    }


    /*
     * No magic means this is an ordinary Chez executable.
     */
    if (memcmp(
            trailer,
            embedded_boot_magic,
            sizeof embedded_boot_magic)
        != 0) {
        CLOSE(fd);
        return S_EMBEDDED_BOOT_NONE;
    }


    version =
        read_u32le(
            trailer + EMBEDDED_BOOT_VERSION_OFFSET);

    flags =
        read_u32le(
            trailer + EMBEDDED_BOOT_FLAGS_OFFSET);

    boot_length =
        read_u64le(
            trailer + EMBEDDED_BOOT_LENGTH_OFFSET);

    reserved =
        read_u64le(
            trailer + EMBEDDED_BOOT_RESERVED_OFFSET);


    /*
     * Version 1 has no optional flags or extension fields.
     *
     * Once CHEZBOOT magic is present, malformed metadata is considered a
     * hard error instead of falling back to ordinary external boot lookup.
     */
    if (version != EMBEDDED_BOOT_VERSION
        || flags != 0
        || boot_length == 0
        || reserved != 0) {
        CLOSE(fd);
        return S_EMBEDDED_BOOT_INVALID;
    }


    /*
     * The boot immediately precedes the trailer:
     *
     *   [launcher][boot][32-byte trailer]
     */
    payload_length =
        (uint64_t)end
        - (uint64_t)EMBEDDED_BOOT_TRAILER_SIZE;

    if (boot_length > payload_length) {
        CLOSE(fd);
        return S_EMBEDDED_BOOT_INVALID;
    }


    boot_offset_u64 =
        payload_length - boot_length;


    /*
     * Verify that the computed offset fits in OFF_T exactly.
     */
    boot_offset =
        (OFF_T)boot_offset_u64;

    if (boot_offset < (OFF_T)0
        || (uint64_t)boot_offset != boot_offset_u64) {
        CLOSE(fd);
        return S_EMBEDDED_BOOT_INVALID;
    }


    /*
     * Sregister_boot_file_fd_region currently takes iptr values for
     * offset and length. Refuse values that cannot be represented exactly.
     */
    if (!off_t_to_iptr(
            boot_offset,
            &chez_boot_offset)
        || !u64_to_iptr(
            boot_length,
            &chez_boot_length)) {
        CLOSE(fd);
        return S_EMBEDDED_BOOT_INVALID;
    }


    /*
     * Success.
     *
     * Do not close fd. Ownership transfers to the caller.
     */
    boot->fd = fd;
    boot->offset = chez_boot_offset;
    boot->length = chez_boot_length;

    return S_EMBEDDED_BOOT_FOUND;
}
