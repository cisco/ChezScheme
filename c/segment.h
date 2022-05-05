/* segment.h
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

/* segment_info */

#define SEGMENT_T1_SIZE ((uptr)1<<segment_t1_bits)
#define SEGMENT_T1_IDX(i) ((i)&(SEGMENT_T1_SIZE-1))

#ifdef segment_t3_bits

#define SEGMENT_T2_SIZE ((uptr)1<<segment_t2_bits)
#define SEGMENT_T2_IDX(i) (((i)>>segment_t1_bits)&(SEGMENT_T2_SIZE-1))
#define SEGMENT_T3_SIZE ((uptr)1<<segment_t3_bits)
#define SEGMENT_T3_IDX(i) ((i)>>(segment_t2_bits+segment_t1_bits))

FORCEINLINE seginfo *SegInfo(uptr i) {
  return AS_IMPLICIT_ATOMIC(seginfo *, S_segment_info[SEGMENT_T3_IDX(i)]->t2[SEGMENT_T2_IDX(i)]->t1[SEGMENT_T1_IDX(i)]);
}

FORCEINLINE seginfo *MaybeSegInfo(uptr i) {
  t2table *t2i; t1table *t1i;
  if ((t2i = AS_IMPLICIT_ATOMIC(t2table *, S_segment_info[SEGMENT_T3_IDX(i)])) == NULL) return NULL;
  if ((t1i = AS_IMPLICIT_ATOMIC(t1table *, t2i->t2[SEGMENT_T2_IDX(i)])) == NULL) return NULL;
  return AS_IMPLICIT_ATOMIC(seginfo *, t1i->t1[SEGMENT_T1_IDX(i)]);
}

#else /* segment_t3_bits */
#ifdef segment_t2_bits

#define SEGMENT_T2_SIZE ((uptr)1<<segment_t2_bits)
#define SEGMENT_T2_IDX(i) ((i)>>segment_t1_bits)
#define SEGMENT_T3_SIZE 0

FORCEINLINE seginfo *SegInfo(uptr i) {
  return S_segment_info[SEGMENT_T2_IDX(i)]->t1[SEGMENT_T1_IDX(i)];
}

FORCEINLINE seginfo *MaybeSegInfo(uptr i) {
  t1table *t1i;
  if ((t1i = S_segment_info[SEGMENT_T2_IDX(i)]) == NULL) return NULL;
  return t1i->t1[SEGMENT_T1_IDX(i)];
}

#else /* segment_t2_bits */

#define SEGMENT_T2_SIZE 0
#define SEGMENT_T3_SIZE 0

FORCEINLINE seginfo *SegInfo(uptr i) {
  return S_segment_info[SEGMENT_T1_IDX(i)];
}

FORCEINLINE seginfo *MaybeSegInfo(uptr i) {
  return S_segment_info[SEGMENT_T1_IDX(i)];
}

#endif /* segment_t2_bits */
#endif /* segment_t3_bits */

#define SegmentSpace(i) (SegInfo(i)->space)
#define SegmentGeneration(i) (SegInfo(i)->generation)
#define SegmentOldSpace(i) (SegInfo(i)->old_space)

/* Must be consistent with `eq-hash` in "../s/library.ss" */
FORCEINLINE uptr eq_hash(ptr key) {
  iptr x = (iptr)key >> primary_type_bits;
#if (ptr_bits == 64)
  iptr x1 = x ^ ((x >> 32) & (iptr)0xFFFFFFFF);
#else
  iptr x1 = x;
#endif
  iptr x2 = x1 ^ ((x1 >> 16) & (iptr)0xFFFF);
  iptr x3 = x2 ^ ((x2 >> 8) & (iptr)0xFF);
  return (uptr)x3;
}

FORCEINLINE ptr S_object_to_reference(ptr p) {
  if (p == Sfalse)
    return (ptr)0;
  else
    return ((ptr)((uptr)(p) + reference_disp));
}

FORCEINLINE ptr S_reference_to_object(ptr p) {
  if (p == (ptr)0)
    return Sfalse;
  else
    return ((ptr)((uptr)(p) - reference_disp));
}

/* An allocation region needs room at the end of a formarding pointer
   as a terminator */
#define allocation_segment_tail_padding ptr_bytes

/* We take advantage of the fact `reference_disp` is less than the
   minimum allocation size plus `allocation_segment_tail_padding`, so
   there's no possibility that the referece address for an object will
   be off of its GC-managed page (even for a pair or an bytevector
   with an empty payload). */
#define FOREIGN_REFERENCEP(p) (MaybeSegInfo(addr_get_segment(p)) == NULL)

/* checks whether address is on GC-managed page before adjusting it;
   it's not ok to check after adjusting if `reference_disp` is more
   than one word  */
FORCEINLINE ptr S_maybe_reference_to_object(ptr p) {
  if (p == (ptr)0)
    return Sfalse;
  else if (MaybeSegInfo(addr_get_segment(p)) == NULL)
    return (ptr)0;
  else
    return ((ptr)((uptr)(p) - reference_disp));
}
