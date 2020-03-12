/* expeditor.c
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

#ifdef FEATURE_EXPEDITOR

/* locally defined functions */
static IBOOL s_ee_init_term(void);
static ptr s_ee_read_char(IBOOL blockp);
static void s_ee_write_char(wchar_t c);
static void s_ee_flush(void);
static ptr s_ee_get_screen_size(void);
static void s_ee_raw(void);
static void s_ee_noraw(void);
static void s_ee_enter_am_mode(void);
static void s_ee_exit_am_mode(void);
static void s_ee_pause(void);
static void s_ee_nanosleep(U32 secs, U32 nanosecs);
static ptr s_ee_get_clipboard(void);
static void s_ee_up(I32);
static void s_ee_down(I32);
static void s_ee_left(I32);
static void s_ee_right(I32);
static void s_ee_clear_eol(void);
static void s_ee_clear_eos(void);
static void s_ee_clear_screen(void);
static void s_ee_scroll_reverse(I32);
static void s_ee_bell(void);
static void s_ee_carriage_return(void);
static void s_ee_line_feed(void);

static INT init_status = -1;

#ifdef WIN32

#include <objbase.h>
#include <io.h>

static HANDLE hStdout, hStdin; 
static DWORD InMode, OutMode;

static IBOOL s_ee_init_term(void) {
  CONSOLE_SCREEN_BUFFER_INFO csbiInfo; 

  if (init_status != -1) return init_status;

  init_status =
     (hStdin = GetStdHandle(STD_INPUT_HANDLE)) != INVALID_HANDLE_VALUE
     && (hStdout = GetStdHandle(STD_OUTPUT_HANDLE)) != INVALID_HANDLE_VALUE
     && GetConsoleScreenBufferInfo(hStdout, &csbiInfo)
     && GetConsoleMode(hStdin, &InMode)
     && GetConsoleMode(hStdout, &OutMode);

  return init_status;
}

/* returns char, eof, #t (winched), or #f (nothing ready), the latter
   only if blockp is false */
static ptr s_ee_read_char(IBOOL blockp) {
  DWORD cNumRead;
  INPUT_RECORD irInBuf[1]; 
#ifdef PTHREADS
  ptr tc;
#endif /* PTHREADS */
  BOOL succ;
  static wchar_t buf[10];
  static int bufidx = 0;
  static int buflen = 0;
  static int rptcnt = 0;

  for (;;) {
    if (buflen != 0) {
      int i = bufidx++;
      if (bufidx == buflen) {
        bufidx = 0;
        if (--rptcnt == 0) buflen = 0;
      }
      return Schar(buf[i]);
    }
   
    if (!blockp) {
       DWORD NumberOfEvents;
       if (!GetNumberOfConsoleInputEvents(hStdin, &NumberOfEvents))
         S_error1("expeditor", "error getting console input: ~a",
                    S_LastErrorString());
       if (NumberOfEvents == 0) return Sfalse;
    }

#ifdef PTHREADS
    tc = get_thread_context();
    if (DISABLECOUNT(tc) == FIX(0)) {
        deactivate_thread(tc);
        succ = ReadConsoleInputW(hStdin, irInBuf, 1, &cNumRead);
        reactivate_thread(tc);
    } else {
        succ = ReadConsoleInputW(hStdin, irInBuf, 1, &cNumRead);
    }
#else /* PTHREADS */
    succ = ReadConsoleInputW(hStdin, irInBuf, 1, &cNumRead);
#endif /* PTHREADS */


    if (!succ)
      S_error1("expeditor", "error getting console info: ~a",
                 S_LastErrorString());
  
    if (cNumRead == 0) return Seof_object;
  
    switch(irInBuf[0].EventType) { 
      case KEY_EVENT: {
        KEY_EVENT_RECORD ker = irInBuf[0].Event.KeyEvent; 
        rptcnt = ker.wRepeatCount;
        if (ker.bKeyDown) {
          wchar_t c;

          if (c = ker.uChar.UnicodeChar) {
            /* translate ^<space> to nul */
            if (c == 0x20 && (ker.dwControlKeyState & (LEFT_CTRL_PRESSED|RIGHT_CTRL_PRESSED)))
              buf[0] = 0;
            else
              buf[0] = c;
            buflen = 1;
          } else {
            switch (ker.wVirtualKeyCode) {
              case VK_DELETE:
                buf[0] = '\033';
                buf[1] = '[';
                buf[2] = '3';
                buf[3] = '~';
                buflen = 4;
                break;
              case VK_NEXT: /* page-down */
                buf[0] = '\033';
                buf[1] = '[';
                buf[2] = '6';
                buf[3] = '~';
                buflen = 4;
                break;
              case VK_PRIOR: /* page-up */
                buf[0] = '\033';
                buf[1] = '[';
                buf[2] = '5';
                buf[3] = '~';
                buflen = 4;
                break;
              case VK_END:
                buf[0] = '\033';
                buf[1] = '[';
                buf[2] = 'F';
                buflen = 3;
                break;
              case VK_HOME:
                buf[0] = '\033';
                buf[1] = '[';
                buf[2] = 'H';
                buflen = 3;
                break;
              case VK_LEFT:
                buf[0] = '\033';
                buf[1] = '[';
                buf[2] = 'D';
                buflen = 3;
                break;
              case VK_UP:
                buf[0] = '\033';
                buf[1] = '[';
                buf[2] = 'A';
                buflen = 3;
                break;
              case VK_RIGHT:
                buf[0] = '\033';
                buf[1] = '[';
                buf[2] = 'C';
                buflen = 3;
                break;
              case VK_DOWN:
                buf[0] = '\033';
                buf[1] = '[';
                buf[2] = 'B';
                buflen = 3;
                break;
             /* translate ^@ to nul */
              case 0x32:
                if (ker.dwControlKeyState & (LEFT_CTRL_PRESSED|RIGHT_CTRL_PRESSED)) {
                  buf[0] = 0;
                  buflen = 1;
                }
                break;
              default:
                break;
            }
          }
        }
        break;
      }
  
    /* this tells us when the buffer size changes, but nothing comes through
       when the window size changes. */
      case WINDOW_BUFFER_SIZE_EVENT: // scrn buf. resizing 
        return Strue;
  
      default: 
        break; 
    }
  }
}

/* probably need write-char too */

static ptr s_ee_get_screen_size(void) {
  CONSOLE_SCREEN_BUFFER_INFO csbiInfo; 

  if (!GetConsoleScreenBufferInfo(hStdout, &csbiInfo))
    S_error1("expeditor", "error getting console info: ~a",
               S_LastErrorString());

  return Scons(Sinteger(csbiInfo.srWindow.Bottom - csbiInfo.srWindow.Top + 1),
               Sinteger(csbiInfo.srWindow.Right - csbiInfo.srWindow.Left + 1));
}

static void s_ee_raw(void) {
 /* see http://msdn2.microsoft.com/en-us/library/ms686033.aspx */
  if (!SetConsoleMode(hStdin, ENABLE_WINDOW_INPUT)
        || !SetConsoleMode(hStdout, 0))
    S_error1("expeditor", "error setting console mode: ~a",
               S_LastErrorString());
}

static void s_ee_noraw(void) {
  if (!SetConsoleMode(hStdin, InMode) || !SetConsoleMode(hStdout, OutMode))
    S_error1("expeditor", "error setting console mode: ~a",
               S_LastErrorString());
}

static void s_ee_enter_am_mode(void) { return; }

static void s_ee_exit_am_mode(void) { return; }

static void s_ee_pause(void) { return; }

static void s_ee_nanosleep(U32 secs, U32 nanosecs) {
  Sleep((secs * 1000) + (nanosecs / 1000000));
}

static void s_ee_up(I32 n) {
  CONSOLE_SCREEN_BUFFER_INFO csbiInfo; 
  COORD cursor_pos;

  fflush(stdout);

  if (!GetConsoleScreenBufferInfo(hStdout, &csbiInfo))
    S_error1("expeditor", "error getting console info: ~a",
               S_LastErrorString());
  
  cursor_pos.X = csbiInfo.dwCursorPosition.X;
  cursor_pos.Y = csbiInfo.dwCursorPosition.Y - n;

 /* ignore error, which can occur only if someone else screwed with screen */
  SetConsoleCursorPosition(hStdout, cursor_pos);
}

static void s_ee_down(I32 n) {
  CONSOLE_SCREEN_BUFFER_INFO csbiInfo; 
  COORD cursor_pos;

  fflush(stdout);

  if (!GetConsoleScreenBufferInfo(hStdout, &csbiInfo))
    S_error1("expeditor", "error getting console info: ~a",
               S_LastErrorString());
  
  cursor_pos.X = csbiInfo.dwCursorPosition.X;
  cursor_pos.Y = csbiInfo.dwCursorPosition.Y + n;

 /* ignore error, which can occur only if someone else screwed with screen */
  SetConsoleCursorPosition(hStdout, cursor_pos);
}

static void s_ee_left(I32 n) {
  CONSOLE_SCREEN_BUFFER_INFO csbiInfo; 
  COORD cursor_pos;

  fflush(stdout);

  if (!GetConsoleScreenBufferInfo(hStdout, &csbiInfo))
    S_error1("expeditor", "error getting console info: ~a",
               S_LastErrorString());
  
  cursor_pos.X = csbiInfo.dwCursorPosition.X - n;
  cursor_pos.Y = csbiInfo.dwCursorPosition.Y;

 /* ignore error, which can occur only if someone else screwed with screen */
  SetConsoleCursorPosition(hStdout, cursor_pos);
}

static void s_ee_right(I32 n) {
  CONSOLE_SCREEN_BUFFER_INFO csbiInfo; 
  COORD cursor_pos;

  fflush(stdout);

  if (!GetConsoleScreenBufferInfo(hStdout, &csbiInfo))
    S_error1("expeditor", "error getting console info: ~a",
               S_LastErrorString());

  cursor_pos.X = csbiInfo.dwCursorPosition.X + n;
  cursor_pos.Y = csbiInfo.dwCursorPosition.Y;

 /* ignore error, which can occur only if someone else screwed with screen */
  SetConsoleCursorPosition(hStdout, cursor_pos);
}

static void s_ee_clear_eol(void) {
  CONSOLE_SCREEN_BUFFER_INFO csbiInfo; 
  DWORD ntowrite, numwritten;

  fflush(stdout);

  if (!GetConsoleScreenBufferInfo(hStdout, &csbiInfo))
    S_error1("expeditor", "error getting console info: ~a",
               S_LastErrorString());

  ntowrite = csbiInfo.dwSize.X - csbiInfo.dwCursorPosition.X;

  if (!FillConsoleOutputCharacter(hStdout, (TCHAR)' ',
         ntowrite, csbiInfo.dwCursorPosition, &numwritten))
    S_error1("expeditor", "error clearing section of screen buffer: ~a",
               S_LastErrorString());

  if (!FillConsoleOutputAttribute(hStdout, csbiInfo.wAttributes,
         ntowrite, csbiInfo.dwCursorPosition, &numwritten))
    S_error1("expeditor", "error setting attributes in section of screen buffer: ~a",
               S_LastErrorString());
}

static void s_ee_clear_eos(void) {
  CONSOLE_SCREEN_BUFFER_INFO csbiInfo; 
  DWORD ntowrite, numwritten;

  fflush(stdout);

  if (!GetConsoleScreenBufferInfo(hStdout, &csbiInfo))
    S_error1("expeditor", "error getting console info: ~a",
               S_LastErrorString());

  ntowrite = (csbiInfo.dwSize.X - csbiInfo.dwCursorPosition.X) +
             (csbiInfo.dwSize.X * 
               (csbiInfo.dwSize.Y - csbiInfo.dwCursorPosition.Y - 1));

  if (!FillConsoleOutputCharacter(hStdout, (TCHAR)' ',
         ntowrite, csbiInfo.dwCursorPosition, &numwritten))
    S_error1("expeditor", "error clearing section of screen buffer: ~a",
               S_LastErrorString());

  if (!FillConsoleOutputAttribute(hStdout, csbiInfo.wAttributes,
         ntowrite, csbiInfo.dwCursorPosition, &numwritten))
    S_error1("expeditor", "error setting attributes in section of screen buffer: ~a",
               S_LastErrorString());
}

static void s_ee_clear_screen(void) {
  CONSOLE_SCREEN_BUFFER_INFO csbiInfo; 
  COORD cursor_pos;
  DWORD ntowrite, numwritten;

  fflush(stdout);

  if (!GetConsoleScreenBufferInfo(hStdout, &csbiInfo))
    S_error1("expeditor", "error getting console info: ~a",
               S_LastErrorString());

  cursor_pos.X = 0;
  cursor_pos.Y = csbiInfo.srWindow.Top;
  SetConsoleCursorPosition(hStdout, cursor_pos);

  ntowrite = csbiInfo.dwSize.X * (csbiInfo.dwSize.Y - cursor_pos.Y);

  if (!FillConsoleOutputCharacter(hStdout, (TCHAR)' ',
         ntowrite, cursor_pos, &numwritten))
    S_error1("expeditor", "error clearing section of screen buffer: ~a",
               S_LastErrorString());

  if (!FillConsoleOutputAttribute(hStdout, csbiInfo.wAttributes,
         ntowrite, cursor_pos, &numwritten))
    S_error1("expeditor", "error setting attributes in section of screen buffer: ~a",
               S_LastErrorString());
}

static void s_ee_scroll_reverse(I32 n) {
  CONSOLE_SCREEN_BUFFER_INFO csbiInfo; 

  fflush(stdout);

  if (!GetConsoleScreenBufferInfo(hStdout, &csbiInfo))
    S_error1("expeditor", "error getting console info: ~a",
               S_LastErrorString());

  if (csbiInfo.dwCursorPosition.Y - n < 0) {
    SMALL_RECT rect;
    COORD dest;
    CHAR_INFO fill;

   /* set fill to blank so new top lines will be cleared */
    fill.Attributes = csbiInfo.wAttributes;
    fill.Char.AsciiChar = (char)' ';

   /* move lines 0 through N-n-1 down to lines n through N-1 */
    rect.Top = 0;
    rect.Bottom = csbiInfo.dwSize.Y - n - 1;
    rect.Left = 0;
    rect.Right = csbiInfo.dwSize.X - 1;
    dest.X = 0;
    dest.Y = n;
    if (!ScrollConsoleScreenBuffer(hStdout, &rect, (SMALL_RECT *)0, dest, &fill))
      S_error1("expeditor", "error scrolling screen buffer: ~a",
                 S_LastErrorString());
  } else {
    COORD cursor_pos; DWORD numwritten;

    cursor_pos.X = csbiInfo.dwCursorPosition.X;
    cursor_pos.Y = csbiInfo.dwCursorPosition.Y - n;
    SetConsoleCursorPosition(hStdout, cursor_pos);

    if (!FillConsoleOutputCharacter(hStdout, (TCHAR)' ',
           csbiInfo.dwSize.X * n, cursor_pos, &numwritten))
      S_error1("expeditor", "error clearing section of screen buffer: ~a",
                 S_LastErrorString());

    if (!FillConsoleOutputAttribute(hStdout, csbiInfo.wAttributes,
           csbiInfo.dwSize.X * n, cursor_pos, &numwritten))
      S_error1("expeditor", "error setting attributes in section of screen buffer: ~a",
                 S_LastErrorString());
  }
}

static void s_ee_bell(void) {
  MessageBeep(MB_OK);
}

static void s_ee_carriage_return(void) {
  CONSOLE_SCREEN_BUFFER_INFO csbiInfo; 
  COORD cursor_pos;

  fflush(stdout);

  if (!GetConsoleScreenBufferInfo(hStdout, &csbiInfo))
    S_error1("expeditor", "error getting console info: ~a",
               S_LastErrorString());
  
  cursor_pos.X = 0;
  cursor_pos.Y = csbiInfo.dwCursorPosition.Y;

  SetConsoleCursorPosition(hStdout, cursor_pos);
}

static void s_ee_line_feed(void) {
  CONSOLE_SCREEN_BUFFER_INFO csbiInfo; 

  fflush(stdout);

  if (!GetConsoleScreenBufferInfo(hStdout, &csbiInfo))
    S_error1("expeditor", "error getting console info: ~a",
               S_LastErrorString());

  if (csbiInfo.dwCursorPosition.Y == (csbiInfo.dwSize.Y - 1)) {
    SMALL_RECT rect;
    COORD dest;
    CHAR_INFO fill;

   /* set fill to blank so new bottom line will be cleared */
    fill.Attributes = csbiInfo.wAttributes;
    fill.Char.AsciiChar = (char)' ';

   /* move lines 1 through N-1 up to lines 0 through N-2 */
    rect.Top = 1;
    rect.Bottom = csbiInfo.dwSize.Y - 1;
    rect.Left = 0;
    rect.Right = csbiInfo.dwSize.X - 1;
    dest.X = 0;
    dest.Y = 0;
    if (!ScrollConsoleScreenBuffer(hStdout, &rect, (SMALL_RECT *)0, dest, &fill))
      S_error1("expeditor", "error scrolling screen buffer: ~a",
                 S_LastErrorString());
  } else {
    COORD cursor_pos;

    cursor_pos.X = csbiInfo.dwCursorPosition.X;
    cursor_pos.Y = csbiInfo.dwCursorPosition.Y + 1;
    SetConsoleCursorPosition(hStdout, cursor_pos);
  }
}

static ptr s_ee_get_clipboard(void) {
  ptr x = S_G.null_string;

  if (OpenClipboard((HWND)0)) {
    HANDLE h = GetClipboardData(CF_UNICODETEXT);
    if (h != NULL) {
      wchar_t *w = (wchar_t*)GlobalLock(h);
      if (w != NULL) {
        char *s = Swide_to_utf8(w);
        x = Sstring_utf8(s, -1);
        free(s);
        GlobalUnlock(h);
      }
    }
    CloseClipboard();
  }

  return x;
}

static void s_ee_write_char(wchar_t c) {
  DWORD n;
  WriteConsoleW(hStdout, &c, 1, &n, NULL);
}

#else /* WIN32 */
#include <limits.h>
#ifdef DISABLE_CURSES
#include "nocurses.h"
#elif defined(SOLARIS)
#define NCURSES_CONST
#define CHTYPE int
#include </usr/include/curses.h>
#include </usr/include/term.h>
#elif defined(NETBSD)
#include <ncurses.h>
#include <ncurses/term.h>
#else /* NETBSD */
#include <curses.h>
#include <term.h>
#endif /* SOLARIS */
#include <termios.h>
#include <signal.h>
#include <time.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <wchar.h>
#include <locale.h>
#if !defined(__GLIBC__) && !defined(__OpenBSD__) && !defined(__NetBSD__)
#include <xlocale.h>
#endif

#if defined(TIOCGWINSZ) && defined(SIGWINCH) && defined(EINTR)
#define HANDLE_SIGWINCH
#endif

#ifdef USE_MBRTOWC_L
static locale_t the_locale;
static locale_t uselocale_alt(locale_t l) {
  locale_t old = the_locale;
  the_locale = l;
  return old;
}
# define uselocale(v) uselocale_alt(v)
# define mbrtowc(pwc, s, n, ps) mbrtowc_l(pwc, s, n, ps, the_locale)
#endif

/* locally defined functions */
static int eeputc(tputsputcchar c);
#ifdef HANDLE_SIGWINCH
static void handle_sigwinch(int sig);
#endif

#ifdef HANDLE_SIGWINCH
static IBOOL winched = 0;
static void handle_sigwinch(UNUSED int sig) {
  winched = 1; 
}
#endif

#define STDIN_FD 0
#define STDOUT_FD 1

static IBOOL disable_auto_margin = 0, avoid_last_column = 0;
static locale_t term_locale;
static mbstate_t term_in_mbs;
static mbstate_t term_out_mbs;

static IBOOL s_ee_init_term(void) {
  int errret;

  if (init_status != -1) return init_status;

  if (isatty(STDIN_FD)
      && isatty(STDOUT_FD)
      && setupterm(NULL, STDOUT_FD, &errret) != ERR
/* assuming here and in our optproc definitions later that the names of
   missing capabilities are set to NULL, although this does not appear
   to be documented */
      && cursor_up
      && cursor_down
      && cursor_left
      && cursor_right
      && clr_eol
      && clr_eos
      && clear_screen
      && scroll_reverse
      && carriage_return) {
    if (auto_right_margin) {
     /* terminal automatically wraps.  safest to disable if possible */
      if (exit_am_mode && enter_am_mode) {
        disable_auto_margin = 1;
        avoid_last_column = 0;
     /* can't disable automatic margins, but eat_newline_glitch is set.
        may still be okay, since we never write past the last column,
        and the automatic newline should be emitted only if we do.  but
        see hack in s_ee_enter_am_mode */
      } else if (eat_newline_glitch) {
        disable_auto_margin = 0;
        avoid_last_column = 0;
      } else {
        disable_auto_margin = 0;
        avoid_last_column = 1;
      }
    } else {
      disable_auto_margin = 0;
      avoid_last_column = 0;
    }

#ifdef HANDLE_SIGWINCH
    struct sigaction act;

    sigemptyset(&act.sa_mask);

    act.sa_flags = 0;
    act.sa_handler = handle_sigwinch;
    sigaction(SIGWINCH, &act, (struct sigaction *)0);
#endif

    term_locale = newlocale(LC_ALL_MASK, "", NULL);
    memset(&term_out_mbs, 0, sizeof(term_out_mbs));
    memset(&term_in_mbs, 0, sizeof(term_in_mbs));

    init_status = 1;
  } else {
    init_status = 0;
  }

  return init_status;
}

/* returns char, eof, #t (winched), or #f (nothing ready), the latter
   only if blockp is false */
static ptr s_ee_read_char(IBOOL blockp) {
  ptr msg; int fd = STDIN_FD; int n; char buf[1]; wchar_t wch; size_t sz;
  locale_t old_locale;
#ifdef PTHREADS
  ptr tc = get_thread_context();
#endif

  do {
#ifdef HANDLE_SIGWINCH
    if (winched) { winched = 0; return Strue; }
#endif
#ifdef PTHREADS
    if (!blockp || DISABLECOUNT(tc) == FIX(0)) {
      fcntl(fd, F_SETFL, fcntl(fd, F_GETFL, 0) | NOBLOCK);
      n = READ(fd, buf, 1);
      fcntl(fd, F_SETFL, fcntl(fd, F_GETFL, 0) & ~NOBLOCK);
      if (n < 0 && errno == EWOULDBLOCK) {
        if (!blockp) return Sfalse;
        deactivate_thread(tc);
        n = READ(fd, buf, 1);
        reactivate_thread(tc);
      }
    } else {
      n = READ(fd, buf, 1);
    }
#else /* PTHREADS */
    if (!blockp) {
      fcntl(fd, F_SETFL, fcntl(fd, F_GETFL, 0) | NOBLOCK);
      n = READ(fd, buf, 1);
      fcntl(fd, F_SETFL, fcntl(fd, F_GETFL, 0) & ~NOBLOCK);
      if (n < 0 && errno == EWOULDBLOCK) return Sfalse;
    } else {
      n = READ(fd, buf, 1);
    }
#endif /* PTHREADS */

    if (n == 1) {
      if (buf[0] == '\0') {
        return Schar('\0');
      } else {
        old_locale = uselocale(term_locale);
        sz = mbrtowc(&wch, buf, 1, &term_out_mbs);
        uselocale(old_locale);
        if (sz == 1) {
          return Schar(wch);
        }
      }
    }

  } while ((n < 0 && errno == EINTR) || (n == 1 && sz == (size_t)-2));

  if (n == 0) return Seof_object;

  msg = S_strerror(errno);
  S_error1("expeditor", "error reading from console: ~a", msg);

  memset(&term_out_mbs, 0, sizeof(term_out_mbs));
  return Svoid;
}

/* returns a pair of positive integers */
static ptr s_ee_get_screen_size(void) {
  static INT ee_rows = 0;
  static INT ee_cols = 0;

#ifdef TIOCGWINSZ
  struct winsize ws;
  if (ioctl(STDOUT_FD, TIOCGWINSZ, &ws) == 0) {
    if (ws.ws_row > 0) ee_rows = ws.ws_row;
    if (ws.ws_col > 0) ee_cols = ws.ws_col;
  }
#ifdef MACOSX
  static IBOOL tried_resize = 0;
 /* attempt to work around 10.6 tty driver / xterm bug */
  if (ee_rows == 0 && ee_cols == 0 && !tried_resize) {
    system("exec /usr/X11/bin/resize >& /dev/null");
    tried_resize = 1;
    return s_ee_get_screen_size();
  }
#endif /* MACOSX */
#endif /* TIOCGWINSZ */

  if (ee_rows == 0) {
    char *s, *endp;
    if ((s = getenv("LINES")) != NULL) {
      INT n = (int)strtol(s, &endp, 10);
      if (n > 0 && *endp == '\0') ee_rows = n;
    }
    if (ee_rows == 0) ee_rows = lines > 0 ? lines : 24;
  }

  if (ee_cols == 0) {
    char *s, *endp;
    if ((s = getenv("COLUMNS")) != NULL) {
      INT n = (int)strtol(s, &endp, 10);
      if (n > 0 && *endp == '\0') ee_cols = n;
    }
    if (ee_cols == 0) ee_cols = columns > 0 ? columns : 80;
  }

  return Scons(Sinteger(ee_rows), Sinteger(ee_cols > 1 && avoid_last_column ? ee_cols - 1 : ee_cols));
}

static int eeputc(tputsputcchar c) {
  return putchar(c);
}

static struct termios orig_termios;

static void s_ee_raw(void) {
  struct termios new_termios;
  while (tcgetattr(STDIN_FD, &orig_termios) != 0) {
    if (errno != EINTR) {
      ptr msg = S_strerror(errno);
      if (msg != Sfalse)
        S_error1("expeditor", "error entering raw mode: ~a", msg);
      else
        S_error("expeditor", "error entering raw mode");
    }
  }
  new_termios = orig_termios;

 /* essentially want "stty raw -echo".  the appropriate flags to accomplish
    this were determined by studying the gnu/linux stty and termios man
    pages, with particular attention to the cfmakeraw function. */
  new_termios.c_iflag &= ~(IGNBRK|BRKINT|PARMRK|INPCK|ISTRIP
                            |INLCR|IGNCR|ICRNL|IXON);
  new_termios.c_oflag &= ~(OPOST);
  new_termios.c_lflag &= ~(ISIG|ICANON|ECHO|IEXTEN);
  new_termios.c_cflag &= ~(CSIZE|PARENB);
  new_termios.c_cflag |= CS8;
  new_termios.c_cc[VMIN] = 1;
  new_termios.c_cc[VTIME] = 0;

  while (tcsetattr(STDIN_FD, TCSADRAIN, &new_termios) != 0) {
    if (errno != EINTR) {
      ptr msg = S_strerror(errno);
      if (msg != Sfalse)
        S_error1("expeditor", "error entering raw mode: ~a", msg);
      else
        S_error("expeditor", "error entering raw mode");
    }
  }
}

static void s_ee_noraw(void) {
  while (tcsetattr(STDIN_FD, TCSADRAIN, &orig_termios) != 0) {
    if (errno != EINTR) {
      ptr msg = S_strerror(errno);
      if (msg != Sfalse)
        S_error1("expeditor", "error leaving raw mode: ~a", msg);
      else
        S_error("expeditor", "error leaving raw mode");
    }
  }
}

static void s_ee_enter_am_mode(void) {
  if (disable_auto_margin) {
    tputs(enter_am_mode, 1, eeputc);
   /* flush to minimize time span when automatic margins are disabled */
    fflush(stdout);
  } else if (eat_newline_glitch) {
   /* hack: try to prevent terminal from eating subsequent cr or lf.
      assumes we've just written to last column.  probably works only
      for vt100 interpretation of eat_newline_glitch/xn/xenl flag. */
    tputs(cursor_left, 1, eeputc);
    tputs(cursor_right, 1, eeputc);
  }
}

static void s_ee_exit_am_mode(void) {
  if (disable_auto_margin) {
    tputs(exit_am_mode, 1, eeputc);
  }
}

static void s_ee_pause(void) { /* used to handle ^Z */
  fflush(stdout);
  kill(0, SIGTSTP);
}

static void s_ee_nanosleep(U32 secs, U32 nanosecs) {
  struct timespec ts;
  ts.tv_sec = secs;
  ts.tv_nsec = nanosecs;
  nanosleep(&ts, (struct timespec *)0);
}

static void s_ee_up(I32 n) {
  while (n--) tputs(cursor_up, 1, eeputc);
}

static void s_ee_down(I32 n) {
  while (n--) tputs(cursor_down, 1, eeputc);
}

static void s_ee_left(I32 n) {
  while (n--) tputs(cursor_left, 1, eeputc);
}

static void s_ee_right(I32 n) {
  while (n--) tputs(cursor_right, 1, eeputc);
}

static void s_ee_clear_eol(void) {
  tputs(clr_eol, 1, eeputc);
}

static void s_ee_clear_eos(void) {
  tputs(clr_eos, 1, eeputc);
}

static void s_ee_clear_screen(void) {
  tputs(clear_screen, 1, eeputc);
}

static void s_ee_scroll_reverse(I32 n) {
 /* moving up from an entry that was only partially displayed,
    scroll-reverse may be called when cursor isn't at the top line of
    the screen, in which case we hope it will move up by one line.
    in this case, which we have no way of distinguishing from the normal
    case, scroll-reverse needs to clear the line explicitly */
  while (n--) {
    tputs(scroll_reverse, 1, eeputc);
    tputs(clr_eol, 1, eeputc);
  }
}

static void s_ee_bell(void) {
  tputs(bell, 1, eeputc);
}

static void s_ee_carriage_return(void) {
  tputs(carriage_return, 1, eeputc);
}

/* move-line-down doesn't scroll the screen when performed on the last
   line on the freebsd and openbsd consoles.  the official way to scroll
   the screen is to use scroll-forward (ind), but ind is defined only
   at the bottom left corner of the screen, and we don't always know
   where the bottom of the screen actually is.  so we write a line-feed
   (newline) character and hope that will do the job. */
static void s_ee_line_feed(void) {
  putchar(0x0a);
}

#ifdef LIBX11
#include <dlfcn.h>
#include <X11/Xlib.h>
#include <X11/Xatom.h>
#include <sys/select.h>
#endif /* LIBX11 */

static ptr s_ee_get_clipboard(void) {
#ifdef LIBX11
  static enum {UNINITIALIZED, INITIALIZED, FAILED} status = UNINITIALIZED;
  static int (*pXConvertSelection)(Display *, Atom, Atom, Atom, Window, Time);
  static int (*pXPending)(Display *display);
  static int (*pXNextEvent)(Display *, XEvent *);
  static int (*pXGetWindowProperty)(Display *, Window, Atom, long, long, Bool, Atom, Atom *, int *, unsigned long *, unsigned long *, unsigned char **);
  static int (*pXFree)(void *);

  static Display *D;
  static Window R, W;
#endif /* LIBX11 */

  ptr p = S_G.null_string;

#ifdef LIBX11
  if (status == UNINITIALIZED) {
    char *display_name;
    void *handle;
    Display *(*pXOpenDisplay)(char *);
    Window (*pXDefaultRootWindow)(Display *);
    Window (*pXCreateSimpleWindow)(Display *, Window, int, int, unsigned int, unsigned int, unsigned int, unsigned long, unsigned long);

    status = (display_name = getenv("DISPLAY"))
          && (handle = dlopen(LIBX11, RTLD_NOW))
          && (pXOpenDisplay = (Display *(*)(char *display_name))dlsym(handle, "XOpenDisplay"))
          && (pXDefaultRootWindow = (Window (*)(Display *))dlsym(handle, "XDefaultRootWindow"))
          && (pXCreateSimpleWindow = (Window (*)(Display *, Window, int, int, unsigned int, unsigned int, unsigned int, unsigned long, unsigned long))dlsym(handle, "XCreateSimpleWindow"))
          && (pXConvertSelection = (int (*)(Display *, Atom, Atom, Atom, Window, Time))dlsym(handle, "XConvertSelection"))
          && (pXPending = (int (*)(Display *display))dlsym(handle, "XPending"))
          && (pXNextEvent = (int (*)(Display *, XEvent *))dlsym(handle, "XNextEvent"))
          && (pXGetWindowProperty = (int (*)(Display *, Window, Atom, long, long, Bool, Atom, Atom *, int *, unsigned long *, unsigned long *, unsigned char **))dlsym(handle, "XGetWindowProperty"))
          && (pXFree = (int (*)(void *))dlsym(handle, "XFree"))
          && (D = pXOpenDisplay(display_name))
          && (R = pXDefaultRootWindow(D))
          && (W = pXCreateSimpleWindow(D, R, 0, 0, 1, 1, 0, 0, 0))
       ? INITIALIZED : FAILED;
  }

  if (status == INITIALIZED) {
    XEvent XE;
    Window W2; Atom P;
    Atom type;
    int format;
    unsigned long items, bytes, ignore_bytes;
    unsigned char *buf;
    int timeout;

   /* flush late arrivals from previous requests, if any */
    while (pXPending(D)) pXNextEvent(D, &XE);

    pXConvertSelection(D, XA_PRIMARY, XA_STRING, XA_STRING, W, CurrentTime);

   /* mini event loop to catch response, if any */
    timeout = 20; /* wait two seconds, 100ms at a time */
    for (;;) {
      if (pXPending(D)) {
        pXNextEvent(D, &XE);
        if (XE.type == SelectionNotify) {
          if (XE.xselection.property == None) {
            W2 = R;
            P = XA_CUT_BUFFER0;
          } else {
            W2 = XE.xselection.requestor;
            P = XE.xselection.property;
          }

          if (pXGetWindowProperty(D, W2, P, 0, 0, 0, AnyPropertyType,
                  &type, &format, &items, &bytes, &buf) == Success
              && type == XA_STRING
              && format == 8) {
            pXFree(buf);
            if (pXGetWindowProperty(D, W2, P, 0, bytes, 0, AnyPropertyType,
                  &type, &format, &items, &ignore_bytes, &buf) == Success
                && type == XA_STRING
                && format == 8) {
              p = S_string((char *)buf, (iptr)bytes);
            }
          }

          pXFree(buf);

          break;
        }
      } else {
        int xfd;
        fd_set rfds;
        struct timeval tv;

        if (timeout == 0) break;
        xfd = ConnectionNumber(D);
        FD_ZERO(&rfds);
        FD_SET(xfd, &rfds);
        tv.tv_sec = 0;
        tv.tv_usec = 100*1000;
        select(xfd+1, &rfds, NULL, NULL, &tv);
        timeout -= 1;
      }
    }
  }
#endif /* LIBX11 */

#ifdef MACOSX
#define PBPASTEBUFSIZE 1024
  if (p == S_G.null_string) {
    char buf[PBPASTEBUFSIZE];
    FILE *f = popen("/usr/bin/pbpaste", "r");
    iptr i, n, m;
    char *s;

    for (;;) {
      ptr newp;
      n = fread(buf, 1, PBPASTEBUFSIZE, f);
      if (n == 0) break;
      n += (m = Sstring_length(p));
      newp = S_string(NULL, n);
      for (i = 0; i != m; i += 1) Sstring_set(newp, i, Sstring_ref(p, i));
      for (s = buf; i != n; i += 1, s += 1)
        Sstring_set(newp, i, *s);
      p = newp;
    }

    fclose(f);
  }
#endif /* MACOSX */

  return p;
}

static void s_ee_write_char(wchar_t wch) {
  locale_t old; char buf[MB_LEN_MAX]; size_t n;

  old = uselocale(term_locale);
  n = wcrtomb(buf, wch, &term_in_mbs);
  if (n == (size_t)-1) {
    putchar('?');
  } else {
    fwrite(buf, 1, n, stdout);
  }
  uselocale(old);
}

#endif /* WIN32 */

static void s_ee_flush(void) {
  fflush(stdout);
}

void S_expeditor_init(void) {
  Sforeign_symbol("(cs)ee_init_term", (void *)s_ee_init_term);
  Sforeign_symbol("(cs)ee_read_char", (void *)s_ee_read_char);
  Sforeign_symbol("(cs)ee_write_char", (void *)s_ee_write_char);
  Sforeign_symbol("(cs)ee_flush", (void *)s_ee_flush);
  Sforeign_symbol("(cs)ee_get_screen_size", (void *)s_ee_get_screen_size);
  Sforeign_symbol("(cs)ee_raw", (void *)s_ee_raw);
  Sforeign_symbol("(cs)ee_noraw", (void *)s_ee_noraw);
  Sforeign_symbol("(cs)ee_enter_am_mode", (void *)s_ee_enter_am_mode);
  Sforeign_symbol("(cs)ee_exit_am_mode", (void *)s_ee_exit_am_mode);
  Sforeign_symbol("(cs)ee_pause", (void *)s_ee_pause);
  Sforeign_symbol("(cs)ee_nanosleep", (void *)s_ee_nanosleep);
  Sforeign_symbol("(cs)ee_get_clipboard", (void *)s_ee_get_clipboard);
  Sforeign_symbol("(cs)ee_up", (void *)s_ee_up);
  Sforeign_symbol("(cs)ee_down", (void *)s_ee_down);
  Sforeign_symbol("(cs)ee_left", (void *)s_ee_left);
  Sforeign_symbol("(cs)ee_right", (void *)s_ee_right);
  Sforeign_symbol("(cs)ee_clr_eol", (void *)s_ee_clear_eol);
  Sforeign_symbol("(cs)ee_clr_eos", (void *)s_ee_clear_eos);
  Sforeign_symbol("(cs)ee_clear_screen", (void *)s_ee_clear_screen);
  Sforeign_symbol("(cs)ee_scroll_reverse", (void *)s_ee_scroll_reverse);
  Sforeign_symbol("(cs)ee_bell", (void *)s_ee_bell);
  Sforeign_symbol("(cs)ee_carriage_return", (void *)s_ee_carriage_return);
  Sforeign_symbol("(cs)ee_line_feed", (void *)s_ee_line_feed);
}

#endif /* FEATURE_EXPEDITOR */
