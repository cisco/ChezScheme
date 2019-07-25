#ifndef ERR
# define ERR               -1
#endif

#define setupterm(a, b, e) (*(e) = 0, ERR)
#define tputs(c, x, f)     (f(c))

#define lines              0
#define columns            0

#define cursor_left        0
#define cursor_right       0
#define cursor_up          0
#define cursor_down        0
#define enter_am_mode      0
#define exit_am_mode       0
#define clr_eos            0
#define clr_eol            0
#define clear_screen       0
#define carriage_return    0
#define bell               0
#define scroll_reverse     0
#define auto_right_margin  0
#define eat_newline_glitch 0
