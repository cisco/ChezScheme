/* vfasl.c
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

/*

   vfasl ("very fast load") format, where "data" corresponds to an
   image to load into memory, "table" is metadata to relocate that
   data, and the fixed-size header determines the overall size. The
   data can be loaded directly to the static generation on boot, since
   it's organized into pieces that should reside in a particular
   space.

        [vfasl_header]
     _
    /   [symbol] ...         -> space_symbol
   /    [rtd] ...            -> space_pure
d |     [closure] ...        -> space_pure
a |     [impure] ...         -> space_impure
t |     [pure_typed] ...     -> space_pure_typed
a |     [impure_record] ...  -> space_impure_record
  |     [code] ...           -> space_code
   \    [data] ...           -> space_data
    \_  [reloc] ...          -> (not kept for direct-to-static)
     _
t   /   [symbol reference offset] ...
a  /    [rtd reference offset] ...
b |     [singleton reference offset] ...
l  \
e   \_  [bitmap of pointers to relocate]

   The bitmap at the end has one bit for each pointer-sized word in
   the data, but it's shorter than the "data" size (divided by the
   pointer size then divided by 8 bits per byte) because the trailing
   zeros for data are omitted. The bitmap doesn't include some fixups
   that are handled more directly, such as for code and its
   relocations.

*/

typedef uptr vfoff;

/* Needs to match vspace enum order, maps vfasl spaces to allocation
   spaces: */
static ISPC vspace_spaces[] = {
  space_symbol,
  space_pure, /* rtd */
  space_pure, /* closure */
  space_impure,
  space_pure_typed_object,
  space_impure_record,
  space_code,
  space_data,
  space_data /* reloc --- but not really, since relocs are never in static */
};

#define ptr_add(p, n) ((ptr)((uptr)(p) + (n)))
#define ptr_subtract(p, n) ((ptr)((uptr)(p) - (n)))
#define ptr_diff(p, q) ((uptr)(p) - (uptr)(q))

#define segment_align(size) (((size)+bytes_per_segment-1) & ~(bytes_per_segment-1))

static uptr symbol_pos_to_offset(uptr sym_pos) {
  uptr syms_per_segment = bytes_per_segment / size_symbol;
  uptr segs = sym_pos / syms_per_segment;
  uptr syms = sym_pos - (segs * syms_per_segment);
  return (segs * bytes_per_segment) + (syms * size_symbol);
}

static void relink_code(ptr co, ptr sym_base, ptr *vspaces, uptr *vspace_offsets, IBOOL to_static);
static ptr find_pointer_from_offset(uptr p_off, ptr *vspaces, uptr *vspace_offsets);

static ptr lookup_singleton(iptr which);

/************************************************************/
/* Loading                                                  */

ptr S_vfasl(ptr bv, faslFile stream, iptr offset, iptr input_len)
{
  ptr vspaces[vspaces_count];
  uptr vspace_offsets[vspaces_count+1];
# define VSPACE_LENGTH(s) (vspace_offsets[(s)+1] - vspace_offsets[(s)])
# define VSPACE_END(s) ptr_add(vspaces[(s)], VSPACE_LENGTH(s))
  ptr tc = get_thread_context();
  octet header_space[size_vfasl_header];
  ptr header = TO_PTR(header_space);
  ptr table;
  vfoff *symrefs, *rtdrefs, *singletonrefs;
  octet *bm, *bm_end;
  iptr used_len;
  int s;
  void *bv_addr;
  IBOOL to_static = 0;

  used_len = sizeof(header);
  if (used_len > input_len)
    S_error("fasl-read", "input length mismatch");

  if (bv)
    memcpy(&header_space, &BVIT(bv, offset), size_vfasl_header);
  else
    S_fasl_bytesin(header_space, size_vfasl_header, stream);

  used_len += VFASLHEADER_DATA_SIZE(header) + VFASLHEADER_TABLE_SIZE(header);
  if (used_len > input_len)
    S_error("fasl-read", "input length mismatch");

  vspace_offsets[0] = 0;
  for (s = 1; s < vspaces_count; s++) {
    vspace_offsets[s] = VFASLHEADER_VSPACE_REL_OFFSETS(header, s-1);
  }
  vspace_offsets[vspaces_count] = VFASLHEADER_DATA_SIZE(header);

  bv_addr = (bv ? &BVIT(bv, size_vfasl_header + offset) : NULL);

  to_static = (S_vfasl_boot_mode > 0);
  
  for (s = 0; s < vspaces_count; s++) {
    uptr sz = vspace_offsets[s+1] - vspace_offsets[s];
    if (sz > 0) {
      if ((s == vspace_reloc) && to_static && !S_G.retain_static_relocation) {
        newspace_find_room(tc, type_untyped, sz, vspaces[s]);
      } else {
        find_room(tc, vspace_spaces[s], (to_static ? static_generation : 0), type_untyped, sz, vspaces[s]);
      }
      if (bv) {
        memcpy(TO_VOIDP(vspaces[s]), bv_addr, sz);
        bv_addr = TO_VOIDP(ptr_add(TO_PTR(bv_addr), sz));
      } else {
	ptr dest;
#ifdef CANNOT_READ_DIRECTLY_INTO_CODE
	if (s == vspace_code)
	  newspace_find_room(tc, type_untyped, sz, dest);
	else
	  dest = vspaces[s];
#else
	dest = vspaces[s];
#endif
        S_fasl_bytesin(TO_VOIDP(dest), sz, stream);
#ifdef CANNOT_READ_DIRECTLY_INTO_CODE
	if (dest != vspaces[s])
	  memcpy(TO_VOIDP(vspaces[s]), TO_VOIDP(dest), sz);
#endif
      }
    } else
      vspaces[s] = (ptr)0;
  }
  for (s = vspaces_count - 1; s--; ) {
    if (!vspaces[s])
      vspaces[s] = vspaces[s+1];
  }

  if (bv)
    table = TO_PTR(bv_addr);
  else {
    newspace_find_room(tc, type_untyped, ptr_align(VFASLHEADER_TABLE_SIZE(header)), table);
    S_fasl_bytesin(TO_VOIDP(table), VFASLHEADER_TABLE_SIZE(header), stream);
  }

  symrefs = TO_VOIDP(table);
  rtdrefs = TO_VOIDP(ptr_add(TO_PTR(symrefs), VFASLHEADER_SYMREF_COUNT(header) * sizeof(vfoff)));
  singletonrefs = TO_VOIDP(ptr_add(TO_PTR(rtdrefs), VFASLHEADER_RTDREF_COUNT(header) * sizeof(vfoff)));
  bm = TO_VOIDP(ptr_add(TO_PTR(singletonrefs), VFASLHEADER_SINGLETONREF_COUNT(header) * sizeof(vfoff)));
  bm_end = TO_VOIDP(ptr_add(TO_PTR(table), VFASLHEADER_TABLE_SIZE(header)));

#if 0
  printf("\n"
         "hdr  %ld\n"
         "syms %ld\n"
         "rtds %ld\n"
         "clos %ld\n"
         "code %ld\n"
         "rloc %ld\n"
         "data %ld\n"
         "othr %ld\n"
         "tabl %ld  symref %ld  rtdref %ld  sglref %ld\n",
         (uptr)size_vfasl_header,
         VSPACE_LENGTH(vspace_symbol),
         VSPACE_LENGTH(vspace_rtd),
         VSPACE_LENGTH(vspace_closure),
         VSPACE_LENGTH(vspace_code),
         VSPACE_LENGTH(vspace_reloc),
         VSPACE_LENGTH(vspace_data),
         (VSPACE_LENGTH(vspace_impure)
          + VSPACE_LENGTH(vspace_pure_typed)
          + VSPACE_LENGTH(vspace_impure_record)),
         VFASLHEADER_TABLE_SIZE(header),
         VFASLHEADER_SYMREF_COUNT(header) * sizeof(vfoff),
         VFASLHEADER_RTDREF_COUNT(header) * sizeof(vfoff),
         VFASLHEADER_SINGLETONREF_COUNT(header) * sizeof(vfoff));
#endif
    
  /* We have to convert an offset relative to the start of data in the
     vfasl format to an offset relative to an individual space, at
     least for target generations other than 0. Rely on the fact that
     the spaces and the references are both sorted. */
#define SPACE_OFFSET_DECLS                      \
  int s2 = 0;                                   \
  uptr offset2 = vspace_offsets[s2];            \
  uptr next_offset2 = vspace_offsets[s2+1]
#define INC_SPACE_OFFSET(off)                   \
  do {                                          \
    while ((off) >= next_offset2) {             \
      s2++;                                     \
      offset2 = next_offset2;                   \
      next_offset2 = vspace_offsets[s2+1];      \
    }                                           \
  } while (0)
#define SPACE_PTR(off) TO_VOIDP(ptr_add(vspaces[s2], (off) - offset2))

  /* Fix up pointers. The initial content has all pointers relative to
     the start of the data. Since the spaces of referenced pointers
     may be discontiguous, use `find_pointer_from_offset` to get each
     new pointer. */
  {
    SPACE_OFFSET_DECLS;
    uptr p_off = 0;
    while (bm != bm_end) {
      octet m = *bm;
#     define MAYBE_FIXUP(i)                                             \
      if (m & (1 << i)) {                                               \
        ptr *p3;                                                        \
        INC_SPACE_OFFSET(p_off);                                        \
        p3 = SPACE_PTR(p_off);                                          \
        *p3 = find_pointer_from_offset((uptr)*p3, vspaces, vspace_offsets); \
      }                                                                 \
      p_off += sizeof(uptr);

      MAYBE_FIXUP(0);
      MAYBE_FIXUP(1);
      MAYBE_FIXUP(2);
      MAYBE_FIXUP(3);
      MAYBE_FIXUP(4);
      MAYBE_FIXUP(5);
      MAYBE_FIXUP(6);
      MAYBE_FIXUP(7);

#     undef MAYBE_FIXUP
      bm++;
    }
  }

  /* Replace references to singletons like "" and #vu8().
     This needs to be before interning symbols, in case ""
     is a symbol name. */
  {
    SPACE_OFFSET_DECLS;
    vfoff i;
    for (i = 0; i < VFASLHEADER_SINGLETONREF_COUNT(header); i++) {
      uptr r_off;
      ptr *ref;
      r_off = singletonrefs[i];
      INC_SPACE_OFFSET(r_off);
      ref = SPACE_PTR(r_off);
      *ref = lookup_singleton(UNFIX(*ref));
    }
  }

  /* Intern symbols */
  {
    uptr in_seg_off = 0;
    ptr sym = TYPE(vspaces[vspace_symbol], type_symbol);
    ptr end_syms = TYPE(VSPACE_END(vspace_symbol), type_symbol);

    if (sym != end_syms) {
      tc_mutex_acquire();

      while (sym < end_syms) {
        ptr isym;
        IBOOL uninterned;

        /* Make sure we don't try to claim a symbol that crosses
           a segment boundary */
        if ((in_seg_off + size_symbol) > bytes_per_segment) {
          if (in_seg_off == bytes_per_segment) {
            in_seg_off = 0;
          } else {
            /* Back up, then round up to next segment */
            sym = ptr_add(ptr_subtract(sym, size_symbol),
                          (bytes_per_segment - (in_seg_off - size_symbol)));
            in_seg_off = 0;
          }
        }

        uninterned = (SYMPVAL(sym) == Sfalse);

        INITSYMVAL(sym) = sunbound;
        INITSYMCODE(sym, S_G.nonprocedure_code);

#if 0
        S_prin1(sym); printf("\n");
#endif

        if (!uninterned) {
          isym = S_intern4(sym);
          if (isym != sym) {
            /* The symbol was already interned, so point to the existing one */
            INITSYMVAL(sym) = isym;
            if (S_vfasl_boot_mode > 0) {
              IGEN gen = SegInfo(ptr_get_segment(isym))->generation;
              if (gen < static_generation) {
                printf("WARNING: vfasl symbol already interned, but at generation %d: %p ", gen, TO_VOIDP(isym));
                S_prin1(isym);
                printf("\n");
              }
            }
          }
        }

        sym = ptr_add(sym, size_symbol);
        in_seg_off += size_symbol;
      }

      tc_mutex_release();
    }
  }

  /* Replace symbol references with interned references */
  {
    SPACE_OFFSET_DECLS;
    ptr syms = vspaces[vspace_symbol];
    vfoff i;
    for (i = 0; i < VFASLHEADER_SYMREF_COUNT(header); i++) {
      uptr p2_off, sym_pos;
      ptr *p2, sym, val;
      p2_off = symrefs[i];
      INC_SPACE_OFFSET(p2_off);
      p2 = SPACE_PTR(p2_off);
      sym_pos = UNFIX(*p2);
      sym = TYPE(ptr_add(syms, symbol_pos_to_offset(sym_pos)), type_symbol);
      if ((val = SYMVAL(sym)) != sunbound)
        sym = val;
      *p2 = sym;
    }
  }
  
  /* Intern rtds */
  if (VSPACE_LENGTH(vspace_rtd) > 0) {
    ptr rtd = TYPE(vspaces[vspace_rtd], type_typed_object);
    ptr rtd_end = TYPE(VSPACE_END(vspace_rtd), type_typed_object);
    
    /* first one corresponds to base_rtd */
    RECORDINSTTYPE(rtd) = S_G.base_rtd;
    RECORDDESCUID(rtd) = S_G.base_rtd;

    tc_mutex_acquire();

    while (1) {
      ptr new_rtd, meta_rtd, parent_rtd;

      rtd = ptr_add(rtd, size_record_inst(UNFIX(RECORDDESCSIZE(RECORDINSTTYPE(rtd)))));
      if (rtd == rtd_end)
        break;

      /* fixup type of rtd (where the type is usually base_rtd) */
      meta_rtd = RECORDINSTTYPE(rtd);
      if (!Ssymbolp(RECORDDESCUID(meta_rtd)))
        RECORDINSTTYPE(rtd) = RECORDDESCUID(meta_rtd);
 
      /* fixup parent before continuing, relying on parents being earlier in `rtd`s;
         we let the rest of the ancestor vector get fixed up later */
      parent_rtd = rtd_parent(rtd);
      if (parent_rtd != Sfalse) {
        ptr parent_uid = RECORDDESCUID(parent_rtd);
        if (!Ssymbolp(parent_uid))
          rtd_parent(rtd) = parent_uid;
      }

      new_rtd = rtd;
      if (S_fasl_intern_rtd(&new_rtd)) {
        if (new_rtd == rtd) {
          S_error1("vfasl", "incompatible record type ~s", RECORDDESCNAME(rtd));
        } else {
          /* Use the UID field to record already-interned replacement: */
          RECORDDESCUID(rtd) = new_rtd;
        }
      }
    }

    tc_mutex_release();
  }
  
  /* Replace rtd references to interned references */
  {
    SPACE_OFFSET_DECLS;
    vfoff i;
    for (i = 0; i < VFASLHEADER_RTDREF_COUNT(header); i++) {
      uptr r_off;
      ptr *ref, rtd, uid;
      r_off = rtdrefs[i];
      INC_SPACE_OFFSET(r_off);
      ref = SPACE_PTR(r_off);
      rtd = *ref;
      uid = RECORDDESCUID(rtd);
      if (!Ssymbolp(uid)) {
        /* uid is replacement interned rtd */
        *ref = uid;
      }
    }
  }

  /* Fix code pointers on closures */
  {
    ptr cl = TYPE(vspaces[vspace_closure], type_closure);
    ptr end_closures = TYPE(VSPACE_END(vspace_closure), type_closure);
    uptr code_delta = (uptr)ptr_subtract(vspaces[vspace_code], vspace_offsets[vspace_code]);

    while (cl != end_closures) {
      ptr code = CLOSCODE(cl);
      code = ptr_add(code, code_delta);

#if 0
      printf("%p ", code);
      S_prin1(CODENAME(code));
      printf("\n");
#endif

      SETCLOSCODE(cl,code);
      cl = ptr_add(cl, size_closure(CLOSLEN(cl)));
    }
  }

  /* Fix code via relocations */
  {    
    ptr sym_base = vspaces[vspace_symbol];
    ptr code = TYPE(vspaces[vspace_code], type_typed_object);
    ptr code_end = TYPE(VSPACE_END(vspace_code), type_typed_object);
    S_record_code_mod(tc, (uptr)vspaces[vspace_code], (uptr)code_end - (uptr)code);
    while (code != code_end) {
      relink_code(code, sym_base, vspaces, vspace_offsets, to_static);
      code = ptr_add(code, size_code(CODELEN(code)));
    }
  }

  /* Turn result offset into a value, unboxing if it's a box (which
     supports a symbol result, for example). */
  {
    ptr v;
    ITYPE t;
    v = find_pointer_from_offset(VFASLHEADER_RESULT_OFFSET(header), vspaces, vspace_offsets);
    if (((t = TYPEBITS(v)) == type_typed_object)
        && TYPEP(TYPEFIELD(v), mask_box, type_box))
      v = Sunbox(v);

    return v;
  }
}

ptr S_vfasl_to(ptr bv)
{
  return S_vfasl(bv, NULL, 0, Sbytevector_length(bv));
}

/*************************************************************/
/* Code and relocation handling for save and load            */

#define VFASL_RELOC_C_ENTRY(p) (((uptr)(p) << vfasl_reloc_tag_bits) | vfasl_reloc_c_entry_tag)
#define VFASL_RELOC_LIBRARY_ENTRY(p) (((uptr)(p) << vfasl_reloc_tag_bits) | vfasl_reloc_library_entry_tag)
#define VFASL_RELOC_LIBRARY_ENTRY_CODE(p) (((uptr)(p) << vfasl_reloc_tag_bits) | vfasl_reloc_library_entry_code_tag)
#define VFASL_RELOC_SYMBOL(p) (((uptr)(p) << vfasl_reloc_tag_bits) | vfasl_reloc_symbol_tag)
#define VFASL_RELOC_SINGLETON(p) (((uptr)(p) << vfasl_reloc_tag_bits) | vfasl_reloc_singleton_tag)

#define VFASL_RELOC_TAG(p) (UNFIX(p) & ((1 << vfasl_reloc_tag_bits) - 1))
#define VFASL_RELOC_POS(p) (UNFIX(p) >> vfasl_reloc_tag_bits)

static void relink_code(ptr co, ptr sym_base, ptr *vspaces, uptr *vspace_offsets, IBOOL to_static) {
    ptr t; iptr a, m, n;

    t = CODERELOC(co);
    t = ptr_add(vspaces[vspace_reloc], (uptr)t - vspace_offsets[vspace_reloc]);

    if (to_static && !S_G.retain_static_relocation) {
      if ((CODETYPE(co) & (code_flag_template << code_flags_offset)) == 0)
        CODERELOC(co) = (ptr)0;
      else {
        ptr tc = get_thread_context();
        iptr sz = size_reloc_table(RELOCSIZE(t));
        ptr new_t;
        find_room(tc, space_data, static_generation, type_untyped, ptr_align(sz), new_t);
        memcpy(TO_VOIDP(new_t), TO_VOIDP(t), sz);
        t = new_t;
        CODERELOC(co) = t;
        RELOCCODE(t) = co;
      }
    } else {
      CODERELOC(co) = t;
      RELOCCODE(t) = co;
    }

    m = RELOCSIZE(t);
    a = 0;
    n = 0;
    while (n < m) {
      uptr entry, item_off, code_off; ptr obj; I32 saved_off;

        entry = RELOCIT(t, n); n += 1;
        if (RELOC_EXTENDED_FORMAT(entry)) {
            item_off = RELOCIT(t, n); n += 1;
            code_off = RELOCIT(t, n); n += 1;
        } else {
            item_off = RELOC_ITEM_OFFSET(entry);
            code_off = RELOC_CODE_OFFSET(entry);
        }
        a += code_off;

        /* offset is stored in place of constant-loading code: */
        memcpy(&saved_off, TO_VOIDP((ptr)((uptr)co + a)), sizeof(I32));
        obj = (ptr)(iptr)saved_off;

        if (FIXMEDIATE(obj)) {
          if (Sfixnump(obj)) {
            int tag = VFASL_RELOC_TAG(obj);
            iptr pos = VFASL_RELOC_POS(obj);
            if (tag == vfasl_reloc_singleton_tag)
              obj = lookup_singleton(pos);
            else if (tag == vfasl_reloc_c_entry_tag)
              obj = S_lookup_c_entry(pos);
            else if ((tag == vfasl_reloc_library_entry_tag)
                     || (tag == vfasl_reloc_library_entry_code_tag)) {
              obj = S_lookup_library_entry(pos, 1);
              if (tag == vfasl_reloc_library_entry_code_tag)
                obj = CLOSCODE(obj);
            } else if (tag == vfasl_reloc_symbol_tag) {
              ptr val;
              obj = TYPE(ptr_add(sym_base, symbol_pos_to_offset(pos)), type_symbol);
              if ((val = SYMVAL(obj)) != sunbound)
                obj = val;
            } else if (obj == FIX(0)) {
              /* leave as-is */
            } else {
              S_error_abort("vfasl: bad relocation tag");
            }
          } else {
            /* some other immediate, such as black-hole; leave as-is */
          }
        } else {
          uptr offset = (uptr)obj;

          obj = find_pointer_from_offset(offset, vspaces, vspace_offsets);
          if (TYPEBITS(obj) == type_typed_object) {
            ptr tf = TYPEFIELD(obj);
            if (TYPEP(tf, mask_record, type_record)) {
              while (1) {
                if (tf == S_G.base_rtd) {
                  /* Similar to symbols: potentially replace with interned */
                  ptr uid = RECORDDESCUID(obj);
                  if (!Ssymbolp(uid)) {
                    /* "uid" is actually the interned rtd to use instead */
                    obj = uid;
                  }
                  break;
                }
                tf = rtd_parent(tf);
                if (tf == Sfalse)
                  break;
              }
            }
          }
        }

        S_set_code_obj("vfasl", RELOC_TYPE(entry), co, a, obj, item_off);
    }
}

static ptr find_pointer_from_offset(uptr p_off, ptr *vspaces, uptr *vspace_offsets)
{
  int s = 0;
  ITYPE t = TYPEBITS(p_off);

  p_off = (uptr)UNTYPE(p_off, t);
  while (p_off >= vspace_offsets[s+1])
    s++;

  return TYPE(ptr_add(vspaces[s], p_off - vspace_offsets[s]), t);
}

/*************************************************************/
/* Singletons, such as ""                                    */

/* This array needs to be in the same order as the enumeration in "cmacros.ss" */
static ptr *singleton_refs[] = { &S_G.null_string,
                                 &S_G.null_vector,
                                 &S_G.null_fxvector,
                                 &S_G.null_flvector,
                                 &S_G.null_bytevector,
                                 &S_G.null_immutable_string,
                                 &S_G.null_immutable_vector,
                                 &S_G.null_immutable_bytevector,
                                 &S_G.eqp,
                                 &S_G.eqvp,
                                 &S_G.equalp,
                                 &S_G.symboleqp,
                                 &S_G.symbol_symbol,
                                 &S_G.symbol_ht_rtd };

static ptr lookup_singleton(iptr which) {
  ptr v;

  v = *(singleton_refs[which-1]);

  if (v == Sfalse) {
    if (which == singleton_symbol_ht_rtd) {
      S_G.symbol_ht_rtd = SYMVAL(S_intern((const unsigned char *)"$symbol-ht-rtd"));
      if (!Srecordp(S_G.symbol_ht_rtd)) S_error_abort("$symbol-ht-rtd has not been set");
    } else if (which == singleton_eq) {
      S_G.eqp = SYMVAL(S_intern((const unsigned char *)"eq?"));
      if (!Sprocedurep(S_G.eqp)) S_error_abort("eq? has not been set");
    } else
      S_error_abort("vfasl: singleton not ready");
    v = *(singleton_refs[which-1]);
  }

  return v;
}  
