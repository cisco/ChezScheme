#include "system.h"

/*
  Based on

   Implementation of SRFI-27 core generator in C for Racket.
   dvanhorn@cs.uvm.edu

  and

   54-BIT (double) IMPLEMENTATION IN C OF THE "MRG32K3A" GENERATOR
   ===============================================================

   Sebastian.Egner@philips.com, Mar-2002, in ANSI-C and Scheme 48 0.57

   This code is a C-implementation of Pierre L'Ecuyer's MRG32k3a generator.
   The code uses (double)-arithmetics, assuming that it covers the range
   {-2^53..2^53-1} exactly (!). The code of the generator is based on the
   L'Ecuyer's own implementation of the generator. Please refer to the
   file 'mrg32k3a.scm' for more information about the method.
*/

/* Representation is arecord with 6 `double` fields: */

#define RECORDINSTDBLA(x) ((double *)TO_VOIDP((uptr)TO_PTR(&RECORDINSTIT(x, 0)) + (max_float_alignment - ptr_bytes)))

#define RANDSTATEX10(x) (RECORDINSTDBLA(x)[0])
#define RANDSTATEX11(x) (RECORDINSTDBLA(x)[1])
#define RANDSTATEX12(x) (RECORDINSTDBLA(x)[2])
#define RANDSTATEX20(x) (RECORDINSTDBLA(x)[3])
#define RANDSTATEX21(x) (RECORDINSTDBLA(x)[4])
#define RANDSTATEX22(x) (RECORDINSTDBLA(x)[5])

/* The Generator
   =============
*/

/* moduli of the components */
#define Im1 0xffffff2f
#define Im2 0xffffa6bb
#define m1 4294967087.0
#define m2 4294944443.0

/* recursion coefficients of the components */
#define a12  1403580.0
#define a13n  810728.0
#define a21   527612.0
#define a23n 1370589.0

/* normalization factor 1/(m1 + 1) */
#define norm 2.328306549295728e-10

/* the actual generator */

static double mrg32k3a(ptr s) { /* (double), in {0..m1-1} */
  double x10, x20, y;
  iptr   k10, k20;

  /* component 1 */
  x10  = a12*(RANDSTATEX11(s)) - a13n*(RANDSTATEX12(s));
  k10  = (iptr)(x10 / m1);
  x10 -= k10 * m1;
  if (x10 < 0.0)
    x10 += m1;
  RANDSTATEX12(s) = RANDSTATEX11(s);
  RANDSTATEX11(s) = RANDSTATEX10(s);
  RANDSTATEX10(s) = x10;

  /* component 2 */
  x20  = a21*(RANDSTATEX20(s)) - a23n*(RANDSTATEX22(s));
  k20  = (iptr)(x20 / m2);
  x20 -= k20 * m2;
  if (x20 < 0.0)
    x20 += m2;
  RANDSTATEX22(s) = RANDSTATEX21(s);
  RANDSTATEX21(s) = RANDSTATEX20(s);
  RANDSTATEX20(s) = x20;

  /* combination of component */
  y = x10 - x20;
  if (y < 0.0)
    y += m1;
  return y;
}

/**************************************************/

/* The number `n` must be no more than 4294967087 */
uptr S_random_state_next_integer(ptr s, uptr n)
{
  double  x, q, qn, xq;

  /* generate result in {0..n-1} using the rejection method */
  q  = (double)( (uptr)(m1 / (double)n) );
  qn = q * n;
  do {
    x = mrg32k3a(s);
  } while (x >= qn);
  xq = x / q;

  /* return result */
  return (uptr)xq;
}

double S_random_state_next_double(ptr s)
{
  double  x;
  x = mrg32k3a(s);
  return (x + 1.0) * norm;
}

/**************************************************/

static UINT _random_m(UINT *_x)
{
  UINT x, y;
  x = *_x;
  y = x & 0xFFFF;
  x = (30903 * y) + (x >> 16);
  *_x = x;
  return y;
}

static int _random_n(UINT *_x, int n)
{
  return ((_random_m(_x) << 16) + _random_m(_x)) % n;
}

static void sch_srand_half(UINT x, ptr s)
{
  /* Due to integer overflow, this doesn't match the Scheme implementation!
     We use "int" instead of "long" to make the overflow consistent
     across platforms (since "long" is sometimes 64 bits). */
  UINT z;
  z = _random_n(&x, Im1-1);
  RANDSTATEX10(s) = (double)(1 + (((UINT)RANDSTATEX10(s) + z) % (Im1 - 1)));
  z = _random_n(&x, Im1);
  RANDSTATEX11(s) = (double)(((UINT)RANDSTATEX11(s) + z) % Im1);
  z = _random_n(&x, Im1);
  RANDSTATEX12(s) = (double)(((UINT)RANDSTATEX12(s) + z) % Im1);
  z = _random_n(&x, Im2-1);
  RANDSTATEX20(s) = (double)(1 + (((UINT)RANDSTATEX20(s) + z) % (Im2 - 1)));
  z = _random_n(&x, Im2);
  RANDSTATEX21(s) = (double)(((UINT)RANDSTATEX21(s) + z) % Im2);
  z = _random_n(&x, Im2);
  RANDSTATEX22(s) = (double)(((UINT)RANDSTATEX22(s) + z) % Im2);

  /* Due to the mismatch, maybe it's possible that we can hit a degeneracy?
     Double-check, just in case... */
  if (!RANDSTATEX10(s) && !RANDSTATEX11(s) && !RANDSTATEX12(s))
    RANDSTATEX10(s) = 1;
  if (!RANDSTATEX20(s) && !RANDSTATEX21(s) && !RANDSTATEX22(s))
    RANDSTATEX20(s) = 1;
}

void S_random_state_init(ptr s, UINT x)
{
  /* Initial values are from Sebastian Egner's implementation: */
  RANDSTATEX10(s) = 1062452522.0;
  RANDSTATEX11(s) = 2961816100.0;
  RANDSTATEX12(s) = 342112271.0;
  RANDSTATEX20(s) = 2854655037.0;
  RANDSTATEX21(s) = 3321940838.0;
  RANDSTATEX22(s) = 3542344109.0;

  sch_srand_half(x & 0xFFFF, s);
  sch_srand_half((x >> 16) & 0xFFFF, s);
}

/**************************************************/

IBOOL S_random_state_check(double x10, double x11, double x12,
                           double x20, double x21, double x22)
{
#define CHECK_RS_FIELD(x, top) if ((x < 0.0) || (x > ((top) - 1))) return 0;

  CHECK_RS_FIELD(x10, Im1)
  CHECK_RS_FIELD(x11, Im1)
  CHECK_RS_FIELD(x12, Im1)
  CHECK_RS_FIELD(x20, Im2)
  CHECK_RS_FIELD(x21, Im2)
  CHECK_RS_FIELD(x22, Im2)

  if ((x10 == 0.0) && (x11 == 0.0) && (x12 == 0.0))
    return 0;
  if ((x20 == 0.0) && (x21 == 0.0) && (x22 == 0.0))
    return 0;

  return 1;
}
