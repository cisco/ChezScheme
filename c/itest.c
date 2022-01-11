/* itest.c
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

#define r_EOF 0
#define r_LPAREN 1
#define r_RPAREN 2
#define r_CONST 3

static INT digit_value(ICHAR c, INT r) {
  switch (r) {
    case 2:
      if ('0' <= c && c <= '1') return c - '0';
      break;
    case 8:
      if ('0' <= c && c <= '8') return c - '0';
      break;
    case 10:
      if ('0' <= c && c <= '9') return c - '0';
      break;
    case 16:
      if ('0' <= c && c <= '9') return c - '0';
      if ('a' <= c && c <= 'f') return c - 'a';
      if ('A' <= c && c <= 'F') return c - 'A';
    default:
      break;
  }
  return -1;
}

static INT read_int(ptr *v, ptr n, INT r, IBOOL sign) {
  INT i, c;

  for (;;) {
    if ((i = digit_value((c = getchar()), r)) == -1) {
      ungetc(c, stdin);
      break;
    }
    n = S_add(S_mul(n, FIX(r)), FIX(i));
  }
  *v = sign ? S_sub(FIX(0), n) : n;
  return r_CONST;
}

static INT read_token(ptr *v) {
  ICHAR c = getchar();
  switch (c) {
    case SEOF: return r_EOF;
    case '\n':
    case ' ': return read_token(v);
    case ';':
      for (;;) {
        switch (getchar()) {
          case SEOF:
           return r_EOF;
          case '\n':
           return read_token(v);
          default:
           break;
        }
      }
    case '(': return r_LPAREN;
    case ')': return r_RPAREN;
    case '#': {
      ICHAR c = getchar();
      INT r = 10;
      switch (c) {
        case 'x':
          r = 16;
        case 'o':
          if (r == 0) r = 8;
        case 'b':
          if (r == 10) r = 2;
        case 'd': {
          INT i;
          IBOOL sign = 0;
          c = getchar();
          if (c == '+')
            c = getchar();
          else if (c == '-') {
            sign = 1;
            c = getchar();
          }

          if ((i = digit_value(c, r)) != -1)
            return read_int(v, FIX(i), r, sign);
        }
        default:
          printf("malformed hash prefix ignored\n");
          return read_token(v);
      }
    }
    case '+':
    case '-': {
      INT i, c2;
      if ((i = digit_value((c2 = getchar()), 10)) == -1) {
        ungetc(c2, stdin);
      } else {
        return read_int(v, FIX(i), 10, c == '-');
      }
    }
    case '*':
    case '/':
    case 'q':
    case 'r':
    case 'g':
    case '=':
    case '<':
    case 'f':
    case 'c':
    case 'd':
      *v = Schar(c);
      return r_CONST;
    default: {
      INT i;
      if ((i = digit_value(c, 10)) != -1)
        return read_int(v, FIX(i), 10, 0);
      }
      break;
  }
  printf("invalid character %d ignored\n", c);
  return read_token(v);
}

static ptr readx(INT t, ptr v);

static ptr read_list(void) {
  INT t; ptr v, x;

  t = read_token(&v);
  if (t == r_RPAREN) return Snil;
  x = readx(t, v);
  return Scons(x, read_list());
}

static ptr readx(INT t, ptr v) {

  switch (t) {
    case r_EOF:
      printf("unexpected EOF\n");
      exit(1);
    case r_LPAREN: return read_list();
    case r_RPAREN:
      printf("unexpected right paren ignored\n");
      t = read_token(&v);
      return readx(t, v);
    case r_CONST: return v;
    default:
      printf("invalid token %d\n", t);
      exit(1);
  }
}

static ptr read_top(void) {
  INT t; ptr v;

  t = read_token(&v);
  switch (t) {
    case r_EOF: return Seof_object;
    case r_RPAREN: return read_top();
    default: return readx(t, v);
  }
}

static ptr eval(ptr x);

#define First(x) eval(Scar(Scdr(x)))
#define Second(x) eval(Scar(Scdr(Scdr(x))))

static ptr eval(ptr x) {
  if (Spairp(x)) {
    switch (Schar_value(Scar(x))) {
      case '+': return S_add(First(x), Second(x));
      case '-': return S_sub(First(x), Second(x));
      case '*': return S_mul(First(x), Second(x));
      case '/': return S_div(First(x), Second(x));
      case 'q': return S_trunc(First(x), Second(x));
      case 'r': return S_rem(First(x), Second(x));
      case 'g': return S_gcd(First(x), Second(x));
      case '=': {
        ptr x1 = First(x), x2 = Second(x);
        if (Sfixnump(x1) && Sfixnump(x2))
          return Sboolean(x1 == x2);
        else if (Sbignump(x1) && Sbignump(x2))
          return Sboolean(S_big_eq(x1, x2));
        else return Sfalse;
      }
      case '<': {
        ptr x1 = First(x), x2 = Second(x);
        if (Sfixnump(x1))
          if (Sfixnump(x2))
            return Sboolean(x1 < x2);
          else
            return Sboolean(!BIGSIGN(x2));
        else
          if (Sfixnump(x2))
            return Sboolean(BIGSIGN(x1));
          else
            return Sboolean(S_big_lt(x1, x2));
      }
      case 'f': return Sflonum(S_floatify(First(x)));
      case 'c':
        S_gc(get_thread_context(), UNFIX(First(x)),UNFIX(Second(x)));
        return Svoid;
      case 'd': return S_decode_float(Sflonum_value(First(x)));
      default:
        S_prin1(x);
        putchar('\n');
        printf("unrecognized operator, returning zero\n");
        return FIX(0);
    }
  } else
    return x;
}

#undef PROMPT
#undef NOISY
static void bignum_test(void) {
  ptr x;
  for (;;) {
#ifdef PROMPT
    putchar('*');
    putchar(' ');
#endif
    x = read_top();
    if (x == Seof_object) { putchar('\n'); exit(0); }
#ifdef NOISY
    S_prin1(x);
    putchar('\n');
#endif
    x = eval(x);
    S_prin1(x);
    putchar('\n');
  }
}
