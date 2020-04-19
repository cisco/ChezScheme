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

#ifdef WIN32
# ifndef __MINGW32__
#  undef FORCEINLINE
#  define FORCEINLINE static __forceinline
# endif
#else
#define FORCEINLINE static inline
#endif

/* segment_info */

#define SEGMENT_T1_SIZE (1<<segment_t1_bits)
#define SEGMENT_T1_IDX(i) ((i)&(SEGMENT_T1_SIZE-1))

#ifdef segment_t3_bits

#define SEGMENT_T2_SIZE (1<<segment_t2_bits)
#define SEGMENT_T2_IDX(i) (((i)>>segment_t1_bits)&(SEGMENT_T2_SIZE-1))
#define SEGMENT_T3_SIZE (1<<segment_t3_bits)
#define SEGMENT_T3_IDX(i) ((i)>>(segment_t2_bits+segment_t1_bits))

FORCEINLINE seginfo *SegInfo(uptr i) {
  return S_segment_info[SEGMENT_T3_IDX(i)]->t2[SEGMENT_T2_IDX(i)]->t1[SEGMENT_T1_IDX(i)];
}

FORCEINLINE seginfo *MaybeSegInfo(uptr i) {
  t2table *t2i; t1table *t1i;
  if ((t2i = S_segment_info[SEGMENT_T3_IDX(i)]) == NULL) return NULL;
  if ((t1i = t2i->t2[SEGMENT_T2_IDX(i)]) == NULL) return NULL;
  return t1i->t1[SEGMENT_T1_IDX(i)];
}

#else /* segment_t3_bits */
#ifdef segment_t2_bits

#define SEGMENT_T2_SIZE (1<<segment_t2_bits)
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
