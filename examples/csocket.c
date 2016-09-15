/*/ csocket.c
R. Kent Dybvig May 1998
Updated by Jamie Taylor, Sept 2016
Public Domain
/*/

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <string.h>
#include <errno.h>
#include <signal.h>
#include <sys/ioctl.h>
#include <stdio.h>
#include <unistd.h>

/* c_write attempts to write the entire buffer, pushing through
   interrupts, socket delays, and partial-buffer writes */
int c_write(int fd, char *buf, ssize_t start, ssize_t n) {
    ssize_t i, m;

    buf += start;
    m = n;
    while (m > 0) {
        if ((i = write(fd, buf, m)) < 0) {
            if (errno != EAGAIN && errno != EINTR)
                return i;
        } else {
            m -= i;
            buf += i;
        }
    }
    return n;
}

/* c_read pushes through interrupts and socket delays */
int c_read(int fd, char *buf, size_t start, size_t n) {
    int i;

    buf += start;
    for (;;) {
        i = read(fd, buf, n);
        if (i >= 0) return i;
        if (errno != EAGAIN && errno != EINTR) return -1;
    }
}

/* bytes_ready(fd) returns true if there are bytes available
   to be read from the socket identified by fd */
int bytes_ready(int fd) {
    int n;

    (void) ioctl(fd, FIONREAD, &n);
    return n;
}

/* socket support */

/* do_socket() creates a new AF_UNIX socket */
int do_socket(void) {

    return socket(AF_UNIX, SOCK_STREAM, 0);
}

/* do_bind(s, name) binds name to the socket s */
int do_bind(int s, char *name) {
    struct sockaddr_un sun;
    int length;

    sun.sun_family = AF_UNIX;
    (void) strcpy(sun.sun_path, name);
    length = sizeof(sun.sun_family) + sizeof(sun.sun_path);

    return bind(s, (struct sockaddr*)(&sun), length);
}

/* do_accept accepts a connection on socket s */
int do_accept(int s) {
    struct sockaddr_un sun;
    socklen_t length;

    length = sizeof(sun.sun_family) + sizeof(sun.sun_path);

    return accept(s, (struct sockaddr*)(&sun), &length);
}

/* do_connect initiates a socket connection */
int do_connect(int s, char *name) {
    struct sockaddr_un sun;
    int length;

    sun.sun_family = AF_UNIX;
    (void) strcpy(sun.sun_path, name);
    length = sizeof(sun.sun_family) + sizeof(sun.sun_path);

    return connect(s, (struct sockaddr*)(&sun), length);
}

/* get_error returns the operating system's error status */
char* get_error(void) {
    extern int errno;
    return strerror(errno);
}
