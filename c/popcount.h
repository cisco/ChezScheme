
#if __GNUC__ >= 5
static int Spopcount_32(U32 x)
{
  return __builtin_popcount(x);
}
#else
static int Spopcount_32(U32 x)
{
  /* http://bits.stephan-brumme.com/countBits.html */
  /* count bits of each 2-bit chunk */
  x  = x - ((x >> 1) & 0x55555555);
  /* count bits of each 4-bit chunk */
  x  = (x & 0x33333333) + ((x >> 2) & 0x33333333);
  /* count bits of each 8-bit chunk */
  x  = x + (x >> 4);
  /* mask out junk */
  x &= 0xF0F0F0F;
  /* add all four 8-bit chunks */
  return (x * 0x01010101) >> 24;
}
#endif

#if ptr_bits == 32
static int Spopcount(uptr x)
{
  return Spopcount_32((U32)x);
}
#elif ptr_bits == 64
static int Spopcount(uptr x)
{
  return Spopcount_32((U32)(x & 0xFFFFFFFF)) + Spopcount_32((U32)(x >> 32));
}
#endif

