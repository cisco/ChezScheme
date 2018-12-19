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

typedef uptr vfoff;

typedef struct vfasl_header {
  vfoff data_size;
  vfoff table_size;

  vfoff result_offset;

  /* symbol starting offset is 0 */
# define sym_end_offset     rtd_offset
  vfoff rtd_offset;
# define rtd_end_offset     closure_offset
  vfoff closure_offset;
# define closure_end_offset code_offset
  vfoff code_offset;
# define code_end_offset    other_offset
  vfoff other_offset;

  vfoff symref_count;
  vfoff rtdref_count;
  vfoff singletonref_count;
} vfasl_header;

/* vfasl format, where the fixed-size header determines the rest of the
   size:

        [vfasl_header]
     _
d   /   [symbol] ...
a  /    [rtd] ...
t |     [closure] ...
a  \    [code] ...
    \_  [other] ...

t   /   [vfoff: symbol reference offset] ...
a  /    [vfoff: rtd reference offset] ...
b |     [vfoff: singleton reference offset] ...
l  \    
e   \_  [bitmap of pointer offsets]
        
*/

/* Many chunks per vspace on first pass, one per vspace on second
   pass: */
typedef struct vfasl_chunk {
  ptr bytes;
  uptr length;
  uptr used;
  uptr swept;
  struct vfasl_chunk *next;
} vfasl_chunk;

/* One per vspace: */
struct vfasl_count_and_chunk {
  uptr total_bytes;
  vfasl_chunk *first;
};

enum {
  /* The order of these spaces matters: */
  vspace_symbol,
  vspace_rtd,
  vspace_closure,
  vspace_code,
  /* The rest of the spaces are "other" */
  vspace_array,
  vspace_typed,
  vspace_reloc,
  vspace_data, /* at end, so pointer bitmap ends with zeros */
  vspaces_count
};

typedef struct vfasl_info {
  ptr base_addr; /* address to make relocations relative to */

  uptr sym_count;

  vfoff symref_count;
  vfoff *symrefs;

  ptr base_rtd; /* track replacement base_rtd to recognize other rtds */

  vfoff rtdref_count;
  vfoff *rtdrefs;

  vfoff singletonref_count;
  vfoff *singletonrefs;

  struct vfasl_count_and_chunk spaces[vspaces_count];

  octet *ptr_bitmap;

  struct vfasl_hash_table *graph;
} vfasl_info;

#define ptr_add(p, n) ((ptr)((uptr)(p) + (n)))
#define ptr_subtract(p, n) ((ptr)((uptr)(p) - (n)))
#define ptr_diff(p, q) ((uptr)(p) - (uptr)(q))

#define byte_bits      8
#define log2_byte_bits 3

static ptr vfasl_copy_all(vfasl_info *vfi, ptr v);

static ptr copy(vfasl_info *vfi, ptr pp, seginfo *si);
static void sweep_ptrs(vfasl_info *vfi, ptr *pp, iptr n);
static uptr sweep_code_object(vfasl_info *vfi, ptr co);
static uptr sweep_record(vfasl_info *vfi, ptr co);
static uptr sweep(vfasl_info *vfi, ptr p);

static void relink_code(ptr co, ptr sym_base, ptr dest_base);

static void vfasl_relocate(vfasl_info *vfi, ptr *ppp);
static ptr vfasl_relocate_help(vfasl_info *vfi, ptr pp);
static ptr vfasl_find_room(vfasl_info *vfi, int s, ITYPE t, iptr n);
static void vfasl_register_rtd_reference(vfasl_info *vfi, ptr pp);
static void vfasl_register_symbol_reference(vfasl_info *vfi, ptr *pp, ptr p);
static void vfasl_register_singleton_reference(vfasl_info *vfi, ptr *pp, int which);
static void vfasl_register_forward(vfasl_info *vfi, ptr pp, ptr p);
static ptr vfasl_lookup_forward(vfasl_info *vfi, ptr p);

static void fasl_init_entry_tables();

static int detect_singleton(ptr p);
static ptr lookup_singleton(int which);

typedef struct vfasl_hash_table vfasl_hash_table;
static vfasl_hash_table *make_vfasl_hash_table();
static void free_vfasl_hash_table(vfasl_hash_table *ht);
static void vfasl_hash_table_set(vfasl_hash_table *ht, ptr key, ptr value);
static ptr vfasl_hash_table_ref(vfasl_hash_table *ht, ptr key);

static void sort_offsets(vfoff *p, vfoff len);

#define vfasl_fail(vfi, what) S_error("vfasl", "cannot encode " what)

#define print_stats(args) /* printf args */

ptr S_vfasl(ptr bv, void *stream, iptr input_len)
{
  ptr tc = get_thread_context();
  vfasl_header header;
  ptr data, table;
  vfoff *symrefs, *rtdrefs, *singletonrefs;
  octet *bm, *bm_end;
  iptr used_len;
  
  used_len = sizeof(header);
  if (used_len > input_len)
    S_error("fasl-read", "input length mismatch");

  if (bv)
    memcpy(&header, &BVIT(bv, 0), sizeof(vfasl_header));
  else {
    if (S_fasl_stream_read(stream, (octet*)&header, sizeof(header)) < 0)
      S_error("fasl-read", "input truncated");
  }

  used_len += header.data_size + header.table_size;
  if (used_len > input_len)
    S_error("fasl-read", "input length mismatch");

  if (bv) {
    ptr base_addr = &BVIT(bv, sizeof(vfasl_header));
    thread_find_room(tc, typemod, header.data_size, data);
    memcpy(data, base_addr, header.data_size);
    table = ptr_add(base_addr, header.data_size);
  } else {
    thread_find_room(tc, typemod, header.data_size, data);
    if (S_fasl_stream_read(stream, data, header.data_size) < 0)
      S_error("fasl-read", "input truncated");

    thread_find_room(tc, typemod, ptr_align(header.table_size), table);
    if (S_fasl_stream_read(stream, table, header.table_size) < 0)
      S_error("fasl-read", "input truncated");
  }

  symrefs = table;
  rtdrefs = ptr_add(symrefs, header.symref_count * sizeof(vfoff));
  singletonrefs = ptr_add(rtdrefs, header.rtdref_count * sizeof(vfoff));
  bm = ptr_add(singletonrefs, header.singletonref_count * sizeof(vfoff));
  bm_end = ptr_add(table, header.table_size);

  if (0)
    printf("\n"
           "hdr  %ld\n"
           "syms %ld\n"
           "rtds %ld\n"
           "clos %ld\n"
           "code %ld\n"
           "othr %ld\n"
           "tabl %ld  symref %ld  rtdref %ld  sglref %ld\n",
           sizeof(vfasl_header),
           header.sym_end_offset,
           header.rtd_end_offset - header.rtd_offset,
           header.closure_end_offset - header.closure_offset,
           header.code_end_offset - header.code_offset,
           header.data_size - header.other_offset,
           header.table_size,
           header.symref_count * sizeof(vfoff),
           header.rtdref_count * sizeof(vfoff),
           header.singletonref_count * sizeof(vfoff));

  /* Fix up pointers. The content `data` initially has all pointers
     relative to the start of the data, so add the `data` address
     to all pointers. */
  {
    ptr *p = data;
    while (bm != bm_end) {
      octet m;
      m = *bm;
#     define MAYBE_FIXUP(i) if (m & (1 << i)) ((uptr *)p)[i] += (uptr)data
      MAYBE_FIXUP(0);
      MAYBE_FIXUP(1);
      MAYBE_FIXUP(2);
      MAYBE_FIXUP(3);
      MAYBE_FIXUP(4);
      MAYBE_FIXUP(5);
      MAYBE_FIXUP(6);
      MAYBE_FIXUP(7);
#     undef MAYBE_FIXUP
      p += byte_bits;
      bm++;
    }
  }

  /* Intern symbols */
  {
    ptr sym = TYPE(data, type_symbol);
    ptr end_syms = TYPE(ptr_add(data, header.sym_end_offset), type_symbol);

    if (sym != end_syms) {
      tc_mutex_acquire()
    
      while (sym < end_syms) {
        ptr isym;
        
        INITSYMVAL(sym) = sunbound;
        INITSYMCODE(sym,S_G.nonprocedure_code);

        isym = S_intern4(sym);
        if (isym != sym) {
          /* The symbol was already interned, so point to the existing one */
          INITSYMVAL(sym) = isym;
        }

        sym = ptr_add(sym, size_symbol);
      }

      tc_mutex_release()
    }
  }

  /* Replace symbol references with interned references */
  {
    ptr syms = data;
    vfoff i;
    for (i = 0; i < header.symref_count; i++) {
      uptr sym_pos;
      ptr p2, sym, val;
      p2 = ptr_add(data, symrefs[i]);
      sym_pos = UNFIX(*(ptr **)p2);
      sym = TYPE(ptr_add(syms, sym_pos * size_symbol), type_symbol);
      if ((val = SYMVAL(sym)) != sunbound)
        sym = val;
      *(ptr **)p2 = sym;
    }
  }
  
  /* Intern rtds */
  if (header.rtd_offset < header.rtd_end_offset) {
    ptr rtd = TYPE(ptr_add(data, header.rtd_offset), type_typed_object);
    ptr rtd_end = TYPE(ptr_add(data, header.rtd_end_offset), type_typed_object);

    /* first one corresponds to base_rtd */
    RECORDINSTTYPE(rtd) = S_G.base_rtd;
    RECORDDESCUID(rtd) = S_G.base_rtd;

    while (1) {
      ptr new_rtd, parent_rtd;

      rtd = ptr_add(rtd, size_record_inst(UNFIX(RECORDDESCSIZE(S_G.base_rtd))));
      if (rtd == rtd_end)
        break;

      RECORDINSTTYPE(rtd) = S_G.base_rtd;

      /* fixup type and parent before continuing, relying on parents being earlier in `rtd`s */
      parent_rtd = RECORDDESCPARENT(rtd);
      if (parent_rtd != Sfalse) {
        ptr parent_uid = RECORDDESCUID(parent_rtd);
        if (!Ssymbolp(parent_uid))
          RECORDDESCPARENT(rtd) = parent_uid;
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
  }
  
  /* Replace rtd references to interned references */
  {
    vfoff i;
    for (i = 0; i < header.rtdref_count; i++) {
      ptr *ref, rtd, uid;
      ref = ptr_add(data, rtdrefs[i]);
      rtd = *ref;
      uid = RECORDDESCUID(rtd);
      if (!Ssymbolp(uid)) {
        /* uid is replacement interned rtd */
        *ref = uid;
      }
    }
  }

  /* Replace references to singletons like "" and #vu8() */
  {
    vfoff i;
    for (i = 0; i < header.singletonref_count; i++) {
      ptr *ref;
      ref = ptr_add(data, singletonrefs[i]);
      *ref = lookup_singleton(UNFIX(*ref));
    }
  }

  /* Fix code pointers on closures */
  {
    ptr cl = TYPE(ptr_add(data, header.closure_offset), type_closure);
    ptr end_closures = TYPE(ptr_add(data, header.closure_end_offset), type_closure);

    while (cl != end_closures) {
      ptr code = CLOSCODE(cl);
      code = ptr_add(code, (uptr)data);
      SETCLOSCODE(cl,code);
      cl = ptr_add(cl, size_closure(CLOSLEN(cl)));
    }
  }

  /* Fix code via relocations */
  {
    
    ptr sym_base = data;
    ptr code = TYPE(ptr_add(data, header.code_offset), type_typed_object);
    ptr code_end = TYPE(ptr_add(data, header.code_end_offset), type_typed_object);
    while (code != code_end) {
      relink_code(code, sym_base, data);
      code = ptr_add(code, size_code(CODELEN(code)));
    }
  }

  /* Turn result offset into a value, unboxing if it's a box (which
     supports a symbol result, for example). */
  {
    ptr v;
    ITYPE t;
    v = ptr_add(data, header.result_offset);
    if (((t = TYPEBITS(v)) == type_typed_object)
        && TYPEP(TYPEFIELD(v), mask_box, type_box))
      v = Sunbox(v);

    return v;
  }
}

ptr S_vfasl_to(ptr bv)
{
  return S_vfasl(bv, (ptr)0, Sbytevector_length(bv));
}

ptr S_to_vfasl(ptr v)
{
  vfasl_info *vfi;
  vfasl_header header;
  ITYPE t;
  int s;
  uptr size, data_size, bitmap_size, pre_bitmap_size;
  ptr bv, p;

  fasl_init_entry_tables();

  /* Box certain kinds of values where the vfasl process needs a
     pointer into data */
  if (IMMEDIATE(v)
      || detect_singleton(v)
      || ((t = TYPEBITS(v)) == type_symbol)
      || ((t == type_typed_object)
          && TYPEP(TYPEFIELD(v), mask_record, type_record)
          && (TYPEFIELD(v) == v))
      || ((t == type_typed_object)
          && TYPEP(TYPEFIELD(v), mask_box, type_box))) {
    v = Sbox(v);
  }

  vfi = malloc(sizeof(vfasl_info));

  vfi->base_addr = (ptr)0;
  vfi->sym_count = 0;
  vfi->symref_count = 0;
  vfi->symrefs = (ptr)0;
  vfi->base_rtd = S_G.base_rtd;
  vfi->rtdref_count = 0;
  vfi->rtdrefs = (ptr)0;
  vfi->singletonref_count = 0;
  vfi->singletonrefs = (ptr)0;
  vfi->graph = make_vfasl_hash_table();
  vfi->ptr_bitmap = (ptr)0;

  /* First pass: determine sizes */

  for (s = 0; s < vspaces_count; s++) {
    vfasl_chunk *c;

    c = malloc(sizeof(vfasl_chunk));
    c->bytes = (ptr)0;
    c->length = 0;
    c->used = 0;
    c->swept = 0;
    c->next = (ptr)0;

    vfi->spaces[s].first = c;
    vfi->spaces[s].total_bytes = 0;
  }

  (void)vfasl_copy_all(vfi, v);

  for (s = 0; s < vspaces_count; s++) {
    vfasl_chunk *c, *next;
    for (c = vfi->spaces[s].first; c; c = next) {
      next = c->next;
      free(c->bytes);
      free(c);
    }
  }

  free_vfasl_hash_table(vfi->graph);

  /* Setup for second pass: allocate to contiguous bytes */

  size = sizeof(vfasl_header);

  data_size = 0;
  for (s = 0; s < vspaces_count; s++) {
    data_size += vfi->spaces[s].total_bytes;
  }
  header.data_size = data_size;
  size += data_size;

  size += vfi->symref_count * sizeof(vfoff);
  size += vfi->rtdref_count * sizeof(vfoff);
  size += vfi->singletonref_count * sizeof(vfoff);

  header.table_size = size - data_size - sizeof(header); /* doesn't yet include the bitmap */

  header.rtd_offset = vfi->spaces[vspace_symbol].total_bytes;
  header.closure_offset = header.rtd_offset + vfi->spaces[vspace_rtd].total_bytes;
  header.code_offset = header.closure_offset + vfi->spaces[vspace_closure].total_bytes;
  header.other_offset = header.code_offset + vfi->spaces[vspace_code].total_bytes;

  header.symref_count = vfi->symref_count;
  header.rtdref_count = vfi->rtdref_count;
  header.singletonref_count = vfi->singletonref_count;

  pre_bitmap_size = size;

  bitmap_size = (data_size + (byte_bits-1)) >> log2_byte_bits;

  size += bitmap_size;

  bv = S_bytevector(size);
  memset(&BVIT(bv, 0), 0, size);

  p = &BVIT(bv, 0);

  /* Skip header for now */  
  p = ptr_add(p, sizeof(vfasl_header));

  vfi->base_addr = p;

  /* Set pointers to vspaces based on sizes frm first pass */
  for (s = 0; s < vspaces_count; s++) {
    vfasl_chunk *c;

    c = malloc(sizeof(vfasl_chunk));
    c->bytes = p;
    c->length = vfi->spaces[s].total_bytes;
    c->used = 0;
    c->swept = 0;
    c->next = (ptr)0;
    vfi->spaces[s].first = c;

    p = ptr_add(p, vfi->spaces[s].total_bytes);
    vfi->spaces[s].total_bytes = 0;
  }

  vfi->symrefs = p;
  p = ptr_add(p, sizeof(vfoff) * vfi->symref_count);

  vfi->base_rtd = S_G.base_rtd;
  vfi->rtdrefs = p;
  p = ptr_add(p, sizeof(vfoff) * vfi->rtdref_count);

  vfi->singletonrefs = p;
  p = ptr_add(p, sizeof(vfoff) * vfi->singletonref_count);

  vfi->sym_count = 0;
  vfi->symref_count = 0;
  vfi->rtdref_count = 0;
  vfi->singletonref_count = 0;

  vfi->graph = make_vfasl_hash_table();

  vfi->ptr_bitmap = p;

  /* Write data */

  v = vfasl_copy_all(vfi, v);

  header.result_offset = ptr_diff(v, vfi->base_addr);

  /* Make all pointers relative to the start of the data area */
  {
    ptr *p2 = vfi->base_addr;
    uptr base_addr = (uptr)vfi->base_addr;
    octet *bm = vfi->ptr_bitmap;
    octet *bm_end = bm + bitmap_size;
    uptr zeros = 0;
    for (; bm != bm_end; bm++, p2 += byte_bits) {
      octet m = *bm;
      if (m == 0) {
        zeros++;
      } else {
#       define MAYBE_FIXUP(i) if (m & (1 << i)) ((uptr *)p2)[i] -= base_addr;
        MAYBE_FIXUP(0);
        MAYBE_FIXUP(1);
        MAYBE_FIXUP(2);
        MAYBE_FIXUP(3);
        MAYBE_FIXUP(4);
        MAYBE_FIXUP(5);
        MAYBE_FIXUP(6);
        MAYBE_FIXUP(7);
#       undef MAYBE_FIXUP
        zeros = 0;
      }
    }

    /* We can ignore trailing zeros */
    header.table_size += (bitmap_size - zeros);
  }

  /* Truncate bytevector to match end of bitmaps */
  {
    uptr sz = sizeof(vfasl_header) + header.data_size + header.table_size;
    BYTEVECTOR_TYPE(bv) = (sz << bytevector_length_offset) | type_bytevector;
  }

  memcpy(&BVIT(bv, 0), &header, sizeof(vfasl_header));

  sort_offsets(vfi->symrefs, vfi->symref_count);
  sort_offsets(vfi->rtdrefs, vfi->rtdref_count);
  sort_offsets(vfi->singletonrefs, vfi->singletonref_count);

  for (s = 0; s < vspaces_count; s++) {
    free(vfi->spaces[s].first);
  }

  free_vfasl_hash_table(vfi->graph);

  free(vfi);

  return bv;
}

static ptr vfasl_copy_all(vfasl_info *vfi, ptr v) {
  seginfo *si;
  int s;
  int changed = 1;

  si = MaybeSegInfo(ptr_get_segment(v));
  
  v = copy(vfi, v, si);

  while (changed) {
    changed = 0;
    for (s = 0; s < vspaces_count; s++) {
      vfasl_chunk *c = vfi->spaces[s].first;
      while (c && (c->swept < c->used)) {
        ptr pp, pp_end;
        
        pp = ptr_add(c->bytes, c->swept);
        pp_end = ptr_add(c->bytes, c->used);
        c->swept = c->used;

        switch(s) {
        case vspace_symbol:
          while (pp < pp_end) {
            pp = ptr_add(pp, sweep(vfi, TYPE((ptr)pp, type_symbol)));
          }
          break;
        case vspace_closure:
          while (pp < pp_end) {
            pp = ptr_add(pp, sweep(vfi, TYPE((ptr)pp, type_closure)));
          }
          break;
        case vspace_array:
          while (pp < pp_end) {
            vfasl_relocate(vfi, pp);
            pp = ptr_add(pp, sizeof(ptr));
          }
          break;
        case vspace_rtd:
        case vspace_code:
        case vspace_typed:
          while (pp < pp_end) {
            pp = ptr_add(pp, sweep(vfi, TYPE((ptr)pp, type_typed_object)));
          }
          break;
        case vspace_data:
        case vspace_reloc:
          break;
        default:
          S_error_abort("vfasl: unrecognized space");
          break;
        }

        c = c->next;
        changed = 1;
      }
    }
  }

  return v;
}

static void vfasl_register_pointer(vfasl_info *vfi, ptr *pp) {
  if (vfi->ptr_bitmap) {
    uptr delta = ptr_diff(pp, vfi->base_addr) >> log2_ptr_bytes;
    uptr i = delta >> log2_byte_bits;
    uptr bit = (((uptr)1) << (delta & (byte_bits - 1)));
    vfi->ptr_bitmap[i] |= bit;
  }
}

static uptr ptr_base_diff(vfasl_info *vfi, ptr p) {
  if ((uptr)vfi->base_addr > (uptr)UNTYPE(p, TYPEBITS(p)))
    S_error_abort("vfasl: pointer not in region");
    
  return ptr_diff(p, vfi->base_addr);
}

static void vfasl_register_symbol_reference(vfasl_info *vfi, ptr *pp, ptr p) {
  if (vfi->symrefs)
    vfi->symrefs[vfi->symref_count] = ptr_base_diff(vfi, pp);
  vfi->symref_count++;
  *pp = SYMVAL(p); /* replace symbol reference with index of symbol */
}

static void vfasl_register_rtd_reference(vfasl_info *vfi, ptr pp) {
  if (vfi->rtdrefs)
    vfi->rtdrefs[vfi->rtdref_count] = ptr_base_diff(vfi, pp);
  vfi->rtdref_count++;
}

static void vfasl_register_singleton_reference(vfasl_info *vfi, ptr *pp, int which) {
  if (vfi->singletonrefs)
    vfi->singletonrefs[vfi->singletonref_count] = ptr_base_diff(vfi, pp);
  vfi->singletonref_count++;
  *pp = FIX(which);
}

static void vfasl_register_forward(vfasl_info *vfi, ptr pp, ptr p) {
  vfasl_hash_table_set(vfi->graph, pp, p);
}

static ptr vfasl_lookup_forward(vfasl_info *vfi, ptr p) {
  return vfasl_hash_table_ref(vfi->graph, p);
}

static ptr vfasl_find_room(vfasl_info *vfi, int s, ITYPE t, iptr n) {
  ptr p;

  vfi->spaces[s].total_bytes += n;
  
  if (vfi->spaces[s].first->used + n > vfi->spaces[s].first->length) {
    vfasl_chunk *c;
    iptr newlen = n * 2;
    if (newlen < 4096)
      newlen = 4096;

    c = malloc(sizeof(vfasl_chunk));
    c->bytes = malloc(newlen);
    c->length = newlen;
    c->used = 0;
    c->swept = 0;
    
    c->next = vfi->spaces[s].first;
    vfi->spaces[s].first = c;
  }

  p = ptr_add(vfi->spaces[s].first->bytes, vfi->spaces[s].first->used);
  vfi->spaces[s].first->used += n;

  return TYPE(p, t);
}

#define FIND_ROOM(vfi, s, t, n, p) p = vfasl_find_room(vfi, s, t, n)

#define copy_ptrs(ty, p1, p2, n) {\
  ptr *Q1, *Q2, *Q1END;\
  Q1 = (ptr *)UNTYPE((p1),ty);\
  Q2 = (ptr *)UNTYPE((p2),ty);\
  Q1END = (ptr *)((uptr)Q1 + n);\
  while (Q1 != Q1END) *Q1++ = *Q2++;}

static ptr copy(vfasl_info *vfi, ptr pp, seginfo *si) {
    ptr p, tf; ITYPE t;

    if ((t = TYPEBITS(pp)) == type_typed_object) {
      tf = TYPEFIELD(pp);
      if (TYPEP(tf, mask_record, type_record)) {
          ptr rtd; iptr n; int s;

          rtd = tf;

          if (tf == S_G.base_rtd) {
            if ((pp != S_G.base_rtd) && (vfi->base_rtd == S_G.base_rtd)) {
              /* make sure base_rtd is first one registered */
              (void)vfasl_relocate_help(vfi, S_G.base_rtd);
            }
            /* need type and parent before child; FIXME: stack overflow possible */
            if (RECORDDESCPARENT(pp) != Sfalse) {
              (void)vfasl_relocate_help(vfi, RECORDDESCPARENT(pp));
            }

            s = vspace_rtd;
          } else
            s = vspace_typed;

          n = size_record_inst(UNFIX(RECORDDESCSIZE(rtd)));

          FIND_ROOM(vfi, s, type_typed_object, n, p);
          copy_ptrs(type_typed_object, p, pp, n);

          if (pp == S_G.base_rtd)
            vfi->base_rtd = p;
      } else if (TYPEP(tf, mask_vector, type_vector)) {
          iptr len, n;
          len = Svector_length(pp);
          n = size_vector(len);
          FIND_ROOM(vfi, vspace_typed, type_typed_object, n, p);
          copy_ptrs(type_typed_object, p, pp, n);
      } else if (TYPEP(tf, mask_string, type_string)) {
          iptr n;
          n = size_string(Sstring_length(pp));
          FIND_ROOM(vfi, vspace_data, type_typed_object, n, p);
          copy_ptrs(type_typed_object, p, pp, n);
      } else if (TYPEP(tf, mask_fxvector, type_fxvector)) {
          iptr n;
          n = size_fxvector(Sfxvector_length(pp));
          FIND_ROOM(vfi, vspace_data, type_typed_object, n, p);
          copy_ptrs(type_typed_object, p, pp, n);
      } else if (TYPEP(tf, mask_bytevector, type_bytevector)) {
          iptr n;
          n = size_bytevector(Sbytevector_length(pp));
          FIND_ROOM(vfi, vspace_data, type_typed_object, n, p);
          copy_ptrs(type_typed_object, p, pp, n);
      } else if ((iptr)tf == type_tlc) {
          vfasl_fail(vfi, "tlc");
          return (ptr)0;
      } else if (TYPEP(tf, mask_box, type_box)) {
          FIND_ROOM(vfi, vspace_typed, type_typed_object, size_box, p);
          BOXTYPE(p) = (iptr)tf;
          INITBOXREF(p) = Sunbox(pp);
      } else if ((iptr)tf == type_ratnum) {
          FIND_ROOM(vfi, vspace_typed, type_typed_object, size_ratnum, p);
          RATTYPE(p) = type_ratnum;
          RATNUM(p) = RATNUM(pp);
          RATDEN(p) = RATDEN(pp);
      } else if ((iptr)tf == type_exactnum) {
          FIND_ROOM(vfi, vspace_typed, type_typed_object, size_exactnum, p);
          EXACTNUM_TYPE(p) = type_exactnum;
          EXACTNUM_REAL_PART(p) = EXACTNUM_REAL_PART(pp);
          EXACTNUM_IMAG_PART(p) = EXACTNUM_IMAG_PART(pp);
      } else if ((iptr)tf == type_inexactnum) {
          FIND_ROOM(vfi, vspace_data, type_typed_object, size_inexactnum, p);
          INEXACTNUM_TYPE(p) = type_inexactnum;
          INEXACTNUM_REAL_PART(p) = INEXACTNUM_REAL_PART(pp);
          INEXACTNUM_IMAG_PART(p) = INEXACTNUM_IMAG_PART(pp);
      } else if (TYPEP(tf, mask_bignum, type_bignum)) {
          iptr n;
          n = size_bignum(BIGLEN(pp));
          FIND_ROOM(vfi, vspace_data, type_typed_object, n, p);
          copy_ptrs(type_typed_object, p, pp, n);
      } else if (TYPEP(tf, mask_port, type_port)) {
          vfasl_fail(vfi, "port");
          return (ptr)0;
      } else if (TYPEP(tf, mask_code, type_code)) {
          iptr n;
          n = size_code(CODELEN(pp));
          FIND_ROOM(vfi, vspace_code, type_typed_object, n, p);
          copy_ptrs(type_typed_object, p, pp, n);
          if (CODERELOC(pp) == (ptr)0) {
            /* We only get here if we're vfasling code that belongs in
               the static generation. */
            ptr l; iptr ln;
            ln = size_reloc_table(0);
            FIND_ROOM(vfi, vspace_reloc, typemod, ln, l);
            RELOCSIZE(l) = 0;
            RELOCCODE(l) = p;
            CODERELOC(p) = l;
            vfasl_register_pointer(vfi, &CODERELOC(p));
          }
      } else if ((iptr)tf == type_rtd_counts) {
        /* prune counts, since GC will recreate as needed */
          return Sfalse;
      } else if ((iptr)tf == type_thread) {
          vfasl_fail(vfi, "thread");
          return (ptr)0;
      } else {
          S_error_abort("vfasl: illegal type");
          return (ptr)0 /* not reached */;
      }
    } else if (t == type_pair) {
      if (si->space == space_ephemeron) {
        vfasl_fail(vfi, "emphemeron");
        return (ptr)0;
      } else if (si->space == space_weakpair) {
        vfasl_fail(vfi, "weakpair");
        return (ptr)0;
      } else {
        FIND_ROOM(vfi, vspace_array, type_pair, size_pair, p);
      }
      INITCAR(p) = Scar(pp);
      INITCDR(p) = Scdr(pp);
    } else if (t == type_closure) {
        ptr code;
        code = CLOSCODE(pp);
        if (CODETYPE(code) & (code_flag_continuation << code_flags_offset)) {
          vfasl_fail(vfi, "continuation");
          return (ptr)0;
        } else {
            iptr len, n;
            len = CLOSLEN(pp);
            n = size_closure(len);
            FIND_ROOM(vfi, vspace_closure, type_closure, n, p);
            copy_ptrs(type_closure, p, pp, n);
        }
    } else if (t == type_symbol) {
        iptr pos = vfi->sym_count++;
        FIND_ROOM(vfi, vspace_symbol, type_symbol, size_symbol, p);
        INITSYMVAL(p) = FIX(pos);   /* stores symbol index for now; will get reset on load */
        INITSYMPVAL(p) = Snil;      /* will get reset on load */
        INITSYMPLIST(p) = Snil;
        INITSYMSPLIST(p) = Snil;
        INITSYMNAME(p) = SYMNAME(pp);
        INITSYMHASH(p) = SYMHASH(pp);
    } else if (t == type_flonum) {
        FIND_ROOM(vfi, vspace_data, type_flonum, size_flonum, p);
        FLODAT(p) = FLODAT(pp);
        /* note: unlike GC, sharing flonums */
    } else {
      S_error_abort("copy(gc): illegal type");
      return (ptr)0 /* not reached */;
    }

    vfasl_register_forward(vfi, pp, p);

    return p;
}

static ptr vfasl_relocate_help(vfasl_info *vfi, ptr pp) {
  ptr fpp;
  seginfo *si;
  
  si = MaybeSegInfo(ptr_get_segment(pp));
  if (!si)
    vfasl_fail(vfi, "unknown");
  
  fpp = vfasl_lookup_forward(vfi, pp);
  if (fpp)
    return fpp;
  else
    return copy(vfi, pp, si);
}

/* Use vfasl_relocate only on addresses that are in the vfasl target area */
static void vfasl_relocate(vfasl_info *vfi, ptr *ppp) {
  ptr pp = *ppp, tf;
  if (!IMMEDIATE(pp)) {
    int which_singleton;
    if ((which_singleton = detect_singleton(pp)))
      vfasl_register_singleton_reference(vfi, ppp, which_singleton);
    else {
      pp = vfasl_relocate_help(vfi, pp);
      *ppp = pp;
      if (!IMMEDIATE(pp)) {
        if (TYPEBITS(pp) == type_symbol)
          vfasl_register_symbol_reference(vfi, ppp, pp);
        else {
          if ((TYPEBITS(pp) == type_typed_object)
              && (((tf = TYPEFIELD(pp)) == vfi->base_rtd)
                  || (tf == S_G.base_rtd)))
            vfasl_register_rtd_reference(vfi, ppp);
          vfasl_register_pointer(vfi, ppp);
        }
      }
    }
  }
}

static void sweep_ptrs(vfasl_info *vfi, ptr *pp, iptr n) {
  ptr *end = pp + n;

  while (pp != end) {
    vfasl_relocate(vfi, pp);
    pp += 1;
  }
}

static uptr sweep(vfasl_info *vfi, ptr p) {
  ptr tf; ITYPE t;

  t = TYPEBITS(p);
  if (t == type_closure) {
    uptr len;
    ptr code;

    len = CLOSLEN(p);
    sweep_ptrs(vfi, &CLOSIT(p, 0), len);

    /* To code-entry pointer looks like an immediate to
       sweep, so relocate the code directly, and also make it
       relative to the base address. */
    code = vfasl_relocate_help(vfi, CLOSCODE(p));
    code = (ptr)ptr_diff(code, vfi->base_addr);
    SETCLOSCODE(p,code);

    return size_closure(len);
  } else if (t == type_symbol) {
    vfasl_relocate(vfi, &INITSYMNAME(p));
      /* other parts are replaced on load */
    return size_symbol;
  } else if (t == type_flonum) {
    /* nothing to sweep */;
    return size_flonum;
 /* typed objects */
  } else if (tf = TYPEFIELD(p), TYPEP(tf, mask_vector, type_vector)) {
    uptr len = Svector_length(p);
    sweep_ptrs(vfi, &INITVECTIT(p, 0), len);
    return size_vector(len);
  } else if (TYPEP(tf, mask_record, type_record)) {
    return sweep_record(vfi, p);
  } else if (TYPEP(tf, mask_box, type_box)) {
    vfasl_relocate(vfi, &INITBOXREF(p));
    return size_box;
  } else if ((iptr)tf == type_ratnum) {
    vfasl_relocate(vfi, &RATNUM(p));
    vfasl_relocate(vfi, &RATDEN(p));
    return size_ratnum;
  } else if ((iptr)tf == type_exactnum) {
    vfasl_relocate(vfi, &EXACTNUM_REAL_PART(p));
    vfasl_relocate(vfi, &EXACTNUM_IMAG_PART(p));
    return size_exactnum;
  } else if (TYPEP(tf, mask_code, type_code)) {
    return sweep_code_object(vfi, p);
  } else {
    S_error_abort("vfasl_sweep: illegal type");
    return 0;
  }
}

static uptr sweep_record(vfasl_info *vfi, ptr x)
{
    ptr *pp; ptr num; ptr rtd;

    rtd = RECORDINSTTYPE(x);
    if (rtd == S_G.base_rtd) {
      /* base-rtd is reset directly in all rtds */
      RECORDINSTTYPE(x) = vfi->base_rtd;

      if (x == vfi->base_rtd) {
        /* Don't need to save fields of base-rtd */
        ptr *pp = &RECORDINSTIT(x,0);
        ptr *ppend = (ptr *)((uptr)pp + UNFIX(RECORDDESCSIZE(rtd))) - 1;
        while (pp < ppend) {
          *pp = Snil;
          pp += 1;
        }
        return size_record_inst(UNFIX(RECORDDESCSIZE(rtd)));
      }
    } else
      vfasl_relocate(vfi, &RECORDINSTTYPE(x));
    
    num = RECORDDESCPM(rtd);
    pp = &RECORDINSTIT(x,0);

    /* process cells for which bit in pm is set; quit when pm == 0. */
    if (Sfixnump(num)) {
      /* ignore bit for already forwarded rtd */
        uptr mask = (uptr)UNFIX(num) >> 1;
        if (mask == (uptr)-1 >> 1) {
          ptr *ppend = (ptr *)((uptr)pp + UNFIX(RECORDDESCSIZE(rtd))) - 1;
          while (pp < ppend) {
            vfasl_relocate(vfi, pp);
            pp += 1;
          }
        } else {
          while (mask != 0) {
            if (mask & 1) vfasl_relocate(vfi, pp);
            mask >>= 1;
            pp += 1;
          }
        }
    } else {
      iptr index; bigit mask; INT bits;

      /* bignum pointer mask */
      num = RECORDDESCPM(rtd);
      vfasl_relocate(vfi, &RECORDDESCPM(rtd));
      index = BIGLEN(num) - 1;
      /* ignore bit for already forwarded rtd */
      mask = BIGIT(num,index) >> 1;
      bits = bigit_bits - 1;
      for (;;) {
        do {
          if (mask & 1) vfasl_relocate(vfi, pp);
          mask >>= 1;
          pp += 1;
        } while (--bits > 0);
        if (index-- == 0) break;
        mask = BIGIT(num,index);
        bits = bigit_bits;
      }
    }

    return size_record_inst(UNFIX(RECORDDESCSIZE(rtd)));
}

#define VFASL_RELOC_TAG_BITS         3

#define VFASL_RELOC_C_ENTRY_TAG            1
#define VFASL_RELOC_LIBRARY_ENTRY_TAG      2
#define VFASL_RELOC_LIBRARY_ENTRY_CODE_TAG 3
#define VFASL_RELOC_SYMBOL_TAG             4
#define VFASL_RELOC_SINGLETON_TAG          5
/* FXIME: rtds? */

#define VFASL_RELOC_C_ENTRY(p) (((uptr)(p) << VFASL_RELOC_TAG_BITS) | VFASL_RELOC_C_ENTRY_TAG)
#define VFASL_RELOC_LIBRARY_ENTRY(p) (((uptr)(p) << VFASL_RELOC_TAG_BITS) | VFASL_RELOC_LIBRARY_ENTRY_TAG)
#define VFASL_RELOC_LIBRARY_ENTRY_CODE(p) (((uptr)(p) << VFASL_RELOC_TAG_BITS) | VFASL_RELOC_LIBRARY_ENTRY_CODE_TAG)
#define VFASL_RELOC_SYMBOL(p) (((uptr)(p) << VFASL_RELOC_TAG_BITS) | VFASL_RELOC_SYMBOL_TAG)
#define VFASL_RELOC_SINGLETON(p) (((uptr)(p) << VFASL_RELOC_TAG_BITS) | VFASL_RELOC_SINGLETON_TAG)

#define VFASL_RELOC_TAG(p) (UNFIX(p) & ((1 << VFASL_RELOC_TAG_BITS) - 1))
#define VFASL_RELOC_POS(p) (UNFIX(p) >> VFASL_RELOC_TAG_BITS)

static uptr sweep_code_object(vfasl_info *vfi, ptr co) {
    ptr t, oldco, oldt; iptr a, m, n;

    vfasl_relocate(vfi, &CODENAME(co));
    vfasl_relocate(vfi, &CODEARITYMASK(co));
    vfasl_relocate(vfi, &CODEINFO(co));
    vfasl_relocate(vfi, &CODEPINFOS(co));

    oldt = CODERELOC(co);

    n = size_reloc_table(RELOCSIZE(oldt));
    t = vfasl_find_room(vfi, vspace_reloc, typemod, n);
    copy_ptrs(typemod, t, oldt, n);

    m = RELOCSIZE(t);
    oldco = RELOCCODE(t);
    a = 0;
    n = 0;
    while (n < m) {
        uptr entry, item_off, code_off; ptr obj, pos;
        int which_singleton;
 
        entry = RELOCIT(t, n); n += 1;
        if (RELOC_EXTENDED_FORMAT(entry)) {
            item_off = RELOCIT(t, n); n += 1;
            code_off = RELOCIT(t, n); n += 1;
        } else {
            item_off = RELOC_ITEM_OFFSET(entry);
            code_off = RELOC_CODE_OFFSET(entry);
        }
        a += code_off;
        obj = S_get_code_obj(RELOC_TYPE(entry), oldco, a, item_off);

        if ((which_singleton = detect_singleton(obj))) {
          obj = FIX(VFASL_RELOC_SINGLETON(which_singleton));
        } else if ((pos = vfasl_hash_table_ref(S_G.c_entries, obj))) {
          obj = FIX(VFASL_RELOC_C_ENTRY(pos));
        } else if ((pos = vfasl_hash_table_ref(S_G.library_entries, obj))) {
          obj = FIX(VFASL_RELOC_LIBRARY_ENTRY(pos));
        } else if ((pos = vfasl_hash_table_ref(S_G.library_entry_codes, obj))) {
          obj = FIX(VFASL_RELOC_LIBRARY_ENTRY_CODE(pos));
        } else if (Ssymbolp(obj)) {
          obj = vfasl_relocate_help(vfi, obj);
          obj = FIX(VFASL_RELOC_SYMBOL(UNFIX(SYMVAL(obj))));
        } else if (IMMEDIATE(obj)) {
          /* as-is */
          if (Sfixnump(obj))
            S_error("vfasl", "unexpected fixnum in relocation");
        } else {
          obj = vfasl_relocate_help(vfi, obj);
          obj = (ptr)ptr_diff(obj, vfi->base_addr);
        }

        S_set_code_obj("vfasl", RELOC_TYPE(entry) | reloc_force_abs, co, a, obj, item_off);
    }

    RELOCCODE(t) = co;
    CODERELOC(co) = t;

    vfasl_register_pointer(vfi, &RELOCCODE(t));
    vfasl_register_pointer(vfi, &CODERELOC(co));

    return size_code(CODELEN(co));
}

static void relink_code(ptr co, ptr sym_base, ptr dest_base) {
    ptr t; iptr a, m, n;

    t = CODERELOC(co);

    m = RELOCSIZE(t);
    a = 0;
    n = 0;
    while (n < m) {
      uptr entry, item_off, code_off; ptr obj;
 
        entry = RELOCIT(t, n); n += 1;
        if (RELOC_EXTENDED_FORMAT(entry)) {
            item_off = RELOCIT(t, n); n += 1;
            code_off = RELOCIT(t, n); n += 1;
        } else {
            item_off = RELOC_ITEM_OFFSET(entry);
            code_off = RELOC_CODE_OFFSET(entry);
        }
        a += code_off;
        obj = S_get_code_obj(RELOC_TYPE(entry) | reloc_force_abs, co, a, item_off);

        if (IMMEDIATE(obj)) {
          if (Sfixnump(obj)) {
            int tag = VFASL_RELOC_TAG(obj);
            int pos = VFASL_RELOC_POS(obj);
            if (tag == VFASL_RELOC_SINGLETON_TAG)
              obj = lookup_singleton(pos);
            else if (tag == VFASL_RELOC_C_ENTRY_TAG)
              obj = S_lookup_c_entry(pos);
            else if ((tag == VFASL_RELOC_LIBRARY_ENTRY_TAG)
                     || (tag == VFASL_RELOC_LIBRARY_ENTRY_CODE_TAG)) {
              obj = S_lookup_library_entry(pos, 1);
              if (tag == VFASL_RELOC_LIBRARY_ENTRY_CODE_TAG)
                obj = CLOSCODE(obj);
            } else if (tag == VFASL_RELOC_SYMBOL_TAG) {
              ptr val;
              obj = TYPE(ptr_add(sym_base, pos * size_symbol), type_symbol);
              if ((val = SYMVAL(obj)) != sunbound)
                obj = val;
            } else {
              S_error_abort("vfasl: bad relocation tag");
            }
          } else {
            /* some other immediate, such as black-hole; leave as-is */
          }
        } else {
          uptr offset = (uptr)obj;
          obj = ptr_add(dest_base, offset);
          if ((TYPEBITS(obj) == type_typed_object)
              && (TYPEFIELD(obj) == S_G.base_rtd)) {
            /* Similar to symbols: potentially replace with interned */
            ptr uid = RECORDDESCUID(obj);
            if (!Ssymbolp(uid)) {
              /* "uid" is actually the interned rtd to use instead */
              obj = uid;
            }
          }
        }

        S_set_code_obj("vfasl", RELOC_TYPE(entry), co, a, obj, item_off);
    }
}

/*************************************************************/

static void fasl_init_entry_tables()
{
  tc_mutex_acquire()

  if (!S_G.c_entries) {
    iptr i;
    
    S_G.c_entries = make_vfasl_hash_table();
    S_G.library_entries = make_vfasl_hash_table();
    S_G.library_entry_codes = make_vfasl_hash_table();

    for (i = Svector_length(S_G.c_entry_vector); i--; ) {
      ptr entry = Svector_ref(S_G.c_entry_vector, i);
      vfasl_hash_table_set(S_G.c_entries, entry, (ptr)i);
    }

    for (i = Svector_length(S_G.library_entry_vector); i--; ) {
      ptr entry = Svector_ref(S_G.library_entry_vector, i);
      if (entry != Sfalse) {
        vfasl_hash_table_set(S_G.library_entries, entry, (ptr)i);
        vfasl_hash_table_set(S_G.library_entry_codes, CLOSCODE(entry), (ptr)i);
      }
    }
  }

  tc_mutex_release()
}

/*************************************************************/

static int detect_singleton(ptr p) {
  if (p == S_G.null_string)
    return 1;
  else if (p == S_G.null_vector)
    return 2;
  else if (p == S_G.null_fxvector)
    return 3;
  else if (p == S_G.null_bytevector)
    return 4;
  else
    return 0;
}

static ptr lookup_singleton(int which) {
  switch (which) {
  case 1:
    return S_G.null_string;
  case 2:
    return S_G.null_vector;
  case 3:
    return S_G.null_fxvector;
  case 4:
    return S_G.null_bytevector;
  default:
    S_error("vfasl", "bad singleton index");
    return (ptr)0;
  }
}
  
/*************************************************************/

typedef struct hash_entry {
  ptr key, value;
} hash_entry;

struct vfasl_hash_table {
  uptr count;
  uptr size;
  hash_entry *entries;
};

#define HASH_CODE(p) ((uptr)(p) >> log2_ptr_bytes)
#define HASH_CODE2(p) (((uptr)(p) >> (log2_ptr_bytes + log2_ptr_bytes)) | 1)

static vfasl_hash_table *make_vfasl_hash_table() {
  vfasl_hash_table *ht;

  ht = malloc(sizeof(vfasl_hash_table));
  
  ht->count = 0;
  ht->size = 16;
  ht->entries = calloc(sizeof(hash_entry), ht->size);

  return ht;
}

static void free_vfasl_hash_table(vfasl_hash_table *ht) {
  free(ht->entries);
  free(ht);
}

static void vfasl_hash_table_set(vfasl_hash_table *ht, ptr key, ptr value) {
  uptr hc = HASH_CODE(key);
  uptr hc2 = HASH_CODE2(key);
  uptr size = ht->size;
    
  if (ht->count > ht->size >> 1) {
    /* rehash */
    uptr i;
    hash_entry *old_entries = ht->entries;
    
    ht->count = 0;
    ht->size *= 2;
    ht->entries = calloc(sizeof(hash_entry), ht->size);
    
    for (i = 0; i < size; i++) {
      if (old_entries[i].key)
        vfasl_hash_table_set(ht, old_entries[i].key, old_entries[i].value);
    }
    
    free(old_entries);
    size = ht->size;
  }

  hc = hc & (size - 1);
  while (ht->entries[hc].key) {
    hc = (hc + hc2) & (size - 1);
  }

  ht->entries[hc].key = key;
  ht->entries[hc].value = value;
  ht->count++;
}

static ptr vfasl_hash_table_ref(vfasl_hash_table *ht, ptr key) {
  uptr hc = HASH_CODE(key);
  uptr hc2 = HASH_CODE2(key);
  uptr size = ht->size;
  ptr old_key;
  
  hc = hc & (size - 1);
  while ((old_key = ht->entries[hc].key) != key) {
    if (!old_key)
      return (ptr)0;
    hc = (hc + hc2) & (size - 1);
  }

  return ht->entries[hc].value;
}

/*************************************************************/

static void sort_offsets(vfoff *p, vfoff len)
{
  while (1) {
    if (len > 1) {
      vfoff i, pivot = 0;

      {
        vfoff mid = len >> 2;
        vfoff tmp = p[mid];
        p[mid] = p[0];
        p[0] = tmp;
      }
      
      for (i = 1; i < len; i++) {
        if (p[i] < p[pivot]) {
          vfoff tmp = p[pivot];
          p[pivot] = p[i];
          pivot++;
          p[i] = p[pivot];
          p[pivot] = tmp;
        }
      }

      if (pivot > (len >> 1)) {
        sort_offsets(p+pivot+1, len-pivot-1);
        len = pivot;
      } else {
        sort_offsets(p, pivot);
        p = p+pivot+1;
        len = len-pivot-1;
      }
    } else
      return;
  }
}
