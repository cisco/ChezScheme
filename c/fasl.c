/* fasl.c
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

/* fasl representation:
 *
 * <fasl-file> -> <fasl-group>*
 *
 * <fasl-group> -> <fasl-header><fasl-object>*
 *
 * <fasl-header> -> {header}\0\0\0chez<uptr version><uptr machine-type>(<bootfile-name> ...)
 *
 * <bootfile-name> -> <octet char>*
 *
 * <fasl-object> -> <situation><uptr size><pcfasl> # size is the size in bytes of <pcfasl>
 *
 * <situation> -> {visit} | {revisit} | {visit-revisit}
 *
 * <pcfasl> -> <compressed><uptr uncompressed-size><compressed fasl> | {uncompressed}<fasl>
 * 
 * <compressed> -> {gzip} | {lz4}
 *
 * <fasl> -> {pair}<uptr n><fasl elt1>...<fasl eltn><fasl last-cdr>
 *
 *        -> {weak-pair}<fasl><fasl>
 *
 *        -> {box}<fasl>
 *
 *        -> {symbol}<faslstring>
 *
 *        -> {gensym}<faslstring name><faslstring uname>
 *
 *        -> {string}<faslstring>
 *
 *        -> {vector}<uptr n><fasl elt1>...<fasl eltn>
 *
 *        -> {fxvector}<uptr n><iptr elt1>...<iptr eltn>
 *
 *        -> {bytevector}<uptr n><octet elt1>...<octet eltn>
 *
 *        -> {immediate}<uptr>
 *
 *        -> {small-integer}<iptr>
 *
 *        -> {large-integer}<byte sign><uptr n><uptr bigit1>...<uptr bigitn>
 *
 *        -> {ratum}<fasl numerator><fasl denominator>
 *
 *        -> {inexactnum}<fasl real-part><fasl imag-part>
 *
 *        -> {exactnum}<fasl real-part><fasl imag-part>
 *
 *        -> {flonum}<uptr high><uptr low>
 *
 *        -> {entry}<uptr index>
 *
 *        -> {library}<uptr index>
 *
 *        -> {library-code}<uptr index>
 *
 *        -> {graph}<uptr graph-length><fasl object>
 *
 *        -> {graph-def}<uptr index><fasl object>
 *
 *        -> {graph-ref}<uptr index>
 *
 *        -> {base-rtd}
 *
 *        -> {rtd}<fasl uid><faslrecord>
 *
 *        -> {record}<faslrecord>
 *
 *        -> {eq-hashtable}<byte mutable?>
 *                         <byte weak?>
 *                         <uptr minlen>
 *                         <uptr veclen>
 *                         <uptr n>
 *                         <keyval1>...<keyvaln>
 *           <keyval> -> <fasl key><fasl val>
 *
 *        -> {symbol-hashtable}<byte mutable?>
 *                             <uptr minlen>
 *                             <byte equiv>     ; 0: eq?, 1: eqv?, 2: equal?, 3: symbol=?
 *                             <uptr veclen>
 *                             <uptr n>
 *                         <keyval1>...<keyvaln>
 *           <keyval> -> <fasl key><fasl val>
 *
 *        -> {closure}<uptr offset><fasl code>
 *
 *        -> {code}<byte flags>
 *                 <uptr free>       # number of free variables
 *                 <uptr n>          # length in bytes of code
 *                 <fasl name>
 *                 <fasl arity-mask> # two's complement encoding of accepted argument counts
 *                 <fasl info>       # inspector info
 *                 <fasl pinfo*>     # profiling info
 *                 <byte code1>...<byte coden>
 *                 <uptr m>          # length in uptrs of relocation table
 *                 <faslreloc>       # first relocation entry
 *                 ...
 *                 <faslreloc>       # last relocation entry
 *
 * <faslreloc> -> <byte type-etc>    # bit 0: extended entry, bit 1: expect item offset, bit 2+: type
 *                <uptr code-offset>
 *                <uptr item-offset> # omitted if bit 1 of type-etc is 0
 *                <fasl object>
 *
 * <faslstring> -> <uptr n><uptr char1>...<uptr charn>
 *
 * <faslrecord> -> <uptr size>       # size in bytes, not necessarily ptr-aligned
 *                 <uptr n>          # number of flds
 *                 <fasl rtd>
 *                 <field elt1>
 *                 ...
 *                 <field eltn>
 * <field> -> <padty fld-type-ptr><fasl object>
 *            <padty fld-type-u8><octet>
 *            <padty fld-type-i16><iptr>
 *            <padty fld-type-i24><iptr>
 *            <padty fld-type-i32><iptr>
 *            <padty fld-type-i40><iptr high><uptr low>      # 32-bit target
 *            <padty fld-type-i40><iptr>                     # 64-bit target
 *            <padty fld-type-i48><iptr high><uptr low>      # 32-bit target
 *            <padty fld-type-i48><iptr>                     # 64-bit target
 *            <padty fld-type-i56><iptr high><uptr low>      # 32-bit target
 *            <padty fld-type-i56><iptr>                     # 64-bit target
 *            <padty fld-type-i64><iptr high><uptr low>      # 32-bit target
 *            <padty fld-type-i64><iptr>                     # 64-bit target
 *            <padty fld-type-single><uptr>
 *            <padty fld-type-double><uptr high><uptr low>
 * <padty fld-type> -> <byte pad << 5 | fld-type>
 *
 * <uptr n> -> <ubyte1>*<ubyte0>
 * <ubyte1> -> k << 1 | 1, 0 <= k <= 127
 * <ubyte0> -> k << 1 | 0, 0 <= k <= 127
 *         each ubyte represents 7 bits of the uptr, least-significant first
 *         low-order bit is continuation bit: 1 iff more bytes are present
 *
 * <iptr n> -> <ibyte0> | <ibyte1><ubyte1>*<ubyte0>
 * <ibyte1> -> sign << 7 | k << 1 | 1, 0 <= k <= 63
 * <ibyte0> -> sign << 7 | k << 1 | 0, 0 <= k <= 63
 *         leading ibyte represents least-significant 6 bits and sign
 *         each ubyte represents 7 of the remaining bits of the iptr,
 *         least-significant first
 *
 * Notes:
 *  * a list of length n will appear to be shorter in the fasl
 *    representation when the tail of the list is shared, since the
 *    shared tail will be a {graph-def} or {graph-ref}.
 *
 *  * the length of a relocation table is the number of uptrs in the
 *    table, not the number of relocation entries.
 *
 *  * closure offset is the amount added to the code object before
 *    storing it in the code field of the closure.
 *
 *  * {graph} defines the size of the graph used to commonize shared
 *    structure, including cycles.  It must appear before {graph-def}
 *    or {graph-ref}.  A {graph-def} at index i must appear before
 *    a {graph-ref} at index i.
 *
 *  * after an rtd is read: if its uname is unbound, the rtd is placed
 *    into the symbol value slot of the uname; otherwise, the rtd is
 *    discarded and the existing symbol value of uname is returned
 *    instead.  Note that when many records appear within the same
 *    aggregrate structure, the full rtd will appear only in the
 *    first occurrence; the remainder will simply be graph references.
 *
 *  * at present, the fasl representation supports only records
 *    containing only scheme-object fields.
 */

#include "system.h"
#include "zlib.h"

#ifdef WIN32
#include <io.h>
#endif /* WIN32 */

#ifdef NAN_INCLUDE
#include NAN_INCLUDE
#endif

#define UFFO_TYPE_FD 2
#define UFFO_TYPE_BV 3

#define PREPARE_BYTEVECTOR(bv,n) {if (bv == Sfalse || Sbytevector_length(bv) < (n)) bv = S_bytevector(n);}

typedef struct unbufFaslFileObj {
  ptr path;
  INT type;
  INT fd;
} *unbufFaslFile;

typedef struct faslFileObj {
  unbufFaslFile uf;
  iptr size;
  octet *next;
  octet *end;
  octet *buf;
} *faslFile;

/* locally defined functions */
static INT uf_read PROTO((unbufFaslFile uf, octet *s, iptr n));
static octet uf_bytein PROTO((unbufFaslFile uf));
static uptr uf_uptrin PROTO((unbufFaslFile uf, INT *bytes_consumed));
static ptr fasl_entry PROTO((ptr tc, IFASLCODE situation, unbufFaslFile uf));
static ptr bv_fasl_entry PROTO((ptr tc, ptr bv, unbufFaslFile uf));
static void fillFaslFile PROTO((faslFile f));
static void bytesin PROTO((octet *s, iptr n, faslFile f));
static void toolarge PROTO((ptr path));
static iptr iptrin PROTO((faslFile f));
static uptr uptrin PROTO((faslFile f));
static float singlein PROTO((faslFile f));
static double doublein PROTO((faslFile f));
static iptr stringin PROTO((ptr *pstrbuf, iptr start, faslFile f));
static void faslin PROTO((ptr tc, ptr *x, ptr t, ptr *pstrbuf, faslFile f));
static void fasl_record PROTO((ptr tc, ptr *x, ptr t, ptr *pstrbuf, faslFile f));
static IBOOL rtd_equiv PROTO((ptr x, ptr y));
static IBOOL equalp PROTO((ptr x, ptr y));
#ifdef ARMV6
static void arm32_set_abs PROTO((void *address, uptr item));
static uptr arm32_get_abs PROTO((void *address));
static void arm32_set_jump PROTO((void *address, uptr item, IBOOL callp));
static uptr arm32_get_jump PROTO((void *address));
#endif /* ARMV6 */
#ifdef PPC32
static void ppc32_set_abs PROTO((void *address, uptr item));
static uptr ppc32_get_abs PROTO((void *address));
static void ppc32_set_jump PROTO((void *address, uptr item, IBOOL callp));
static uptr ppc32_get_jump PROTO((void *address));
#endif /* PPC32 */
#ifdef X86_64
static void x86_64_set_jump PROTO((void *address, uptr item, IBOOL callp));
static uptr x86_64_get_jump PROTO((void *address));
#endif /* X86_64 */
#ifdef SPARC64
static INT extract_reg_from_sethi PROTO((void *address));
static void emit_sethi_lo PROTO((U32 item, INT destreg, void *address));
static uptr sparc64_get_literal PROTO((void *address));
static void sparc64_set_call PROTO((void *address, U32 *call_addr, uptr item));
static U32 adjust_delay_inst PROTO((U32 delay_inst, U32 *old_call_addr, U32 *new_call_addr));
static INT sparc64_set_lit_only PROTO((void *address, uptr item, I32 destreg));
static void sparc64_set_literal PROTO((void *address, uptr item));
#endif /* SPARC64 */

static double s_nan;

void S_fasl_init() {
    if (S_boot_time) {
        S_protect(&S_G.base_rtd);
        S_G.base_rtd = Sfalse;
        S_protect(&S_G.rtd_key);
        S_G.rtd_key = S_intern((const unsigned char *)"*rtd*");
        S_protect(&S_G.eq_symbol);
        S_G.eq_symbol = S_intern((const unsigned char *)"eq");
        S_protect(&S_G.eq_ht_rtd);
        S_G.eq_ht_rtd = Sfalse;
        S_protect(&S_G.symbol_symbol);
        S_G.symbol_symbol = S_intern((const unsigned char *)"symbol");
        S_protect(&S_G.symbol_ht_rtd);
        S_G.symbol_ht_rtd = Sfalse;
        S_protect(&S_G.eqp);
        S_G.eqp = Sfalse;
        S_protect(&S_G.eqvp);
        S_G.eqvp = Sfalse;
        S_protect(&S_G.equalp);
        S_G.equalp = Sfalse;
        S_protect(&S_G.symboleqp);
        S_G.symboleqp = Sfalse;
    }

    MAKE_NAN(s_nan)
#ifndef WIN32 /* msvc returns true for s_nan==s_nan! */
    if (s_nan == s_nan) {
        fprintf(stderr, "s_nan == s_nan\n");
        S_abnormal_exit();
    }
#endif
}

ptr S_fasl_read(INT fd, IFASLCODE situation, ptr path) {
  ptr tc = get_thread_context();
  ptr x; struct unbufFaslFileObj uffo;

 /* acquire mutex in case we modify code pages */
  tc_mutex_acquire()
  uffo.path = path;
  uffo.type = UFFO_TYPE_FD;
  uffo.fd = fd;
  x = fasl_entry(tc, situation, &uffo);
  tc_mutex_release()
  return x;
}

ptr S_bv_fasl_read(ptr bv, ptr path) {
  ptr tc = get_thread_context();
  ptr x; struct unbufFaslFileObj uffo;

 /* acquire mutex in case we modify code pages */
  tc_mutex_acquire()
  uffo.path = path;
  uffo.type = UFFO_TYPE_BV;
  x = bv_fasl_entry(tc, bv, &uffo);
  tc_mutex_release()
  return x;
}

ptr S_boot_read(INT fd, const char *path) {
  ptr tc = get_thread_context();
  struct unbufFaslFileObj uffo;

  uffo.path = Sstring_utf8(path, -1);
  uffo.type = UFFO_TYPE_FD;
  uffo.fd = fd;
  return fasl_entry(tc, fasl_type_visit_revisit, &uffo);
}

#ifdef WIN32
#define IO_SIZE_T unsigned int
#else /* WIN32 */
#define IO_SIZE_T size_t
#endif /* WIN32 */

static INT uf_read(unbufFaslFile uf, octet *s, iptr n) {
  iptr k;
  while (n > 0) {
    uptr nx = n;

#if (iptr_bits > 32)
  if (WIN32 && (unsigned int)nx != nx) nx = 0xffffffff;
#endif

    switch (uf->type) {
      case UFFO_TYPE_FD:
        k = READ(uf->fd, s, (IO_SIZE_T)nx);
        if (k > 0)
          n -= k;
        else if (k == 0)
          return -1;
        else if (errno != EINTR)
         S_error1("", "error reading from ~a", uf->path);
        break;
      default:
        return -1;
    }

    s += k;
  }
  return 0;
}

static void uf_skipbytes(unbufFaslFile uf, iptr n) {
  switch (uf->type) {
    case UFFO_TYPE_FD:
       if (LSEEK(uf->fd, n, SEEK_CUR) == -1) {
         S_error1("", "error seeking ~a", uf->path);
       }
       break;
  }
}

static octet uf_bytein(unbufFaslFile uf) {
  octet buf[1];
  if (uf_read(uf, buf, 1) < 0)
    S_error1("", "unexpected eof in fasl file ~a", uf->path);
  return buf[0];
}

static uptr uf_uptrin(unbufFaslFile uf, INT *bytes_consumed) {
  uptr n, m; octet k;

  if (bytes_consumed) *bytes_consumed = 1;
  k = uf_bytein(uf);
  n = k >> 1;
  while (k & 1) {
    if (bytes_consumed) *bytes_consumed += 1;
    k = uf_bytein(uf);
    m = n << 7;
    if (m >> 7 != n) toolarge(uf->path);
    n = m | (k >> 1);
  }

  return n;
}

char *S_format_scheme_version(uptr n) {
  static char buf[16]; INT len;
  if ((n >> 16) != ((n >> 16) & 0xffff)) return "unknown";
  if ((n & 0xff) == 0)
    len = snprintf(buf, 16, "%d.%d", (int) n >> 16, (int) (n >> 8) & 0xff);
  else
    len = snprintf(buf, 16, "%d.%d.%d", (int) n >> 16, (int) (n >> 8) & 0xff, 
                   (int) n & 0xff);
  return len > 0 ? buf : "unknown";
}

char *S_lookup_machine_type(uptr n) {
  static char *machine_type_table[] = machine_type_names;
  if (n < machine_type_limit)
    return machine_type_table[n];
  else
    return "unknown";
}

static ptr fasl_entry(ptr tc, IFASLCODE situation, unbufFaslFile uf) {
  ptr x; ptr strbuf = S_G.null_string;
  octet tybuf[1]; IFASLCODE ty; iptr size;
  /* gcc (GCC) 4.8.5 20150623 (Red Hat 4.8.5-28) co-locates buf and x if we put the declaration of buf down where we use it */
  octet buf[SBUFSIZ];

  for (;;) {
    if (uf_read(uf, tybuf, 1) < 0) return Seof_object; 
    ty = tybuf[0];

    while (ty == fasl_type_header) {
      uptr n; ICHAR c;
    
     /* check for remainder of magic number */
      if (uf_bytein(uf) != 0 ||
          uf_bytein(uf) != 0 ||
          uf_bytein(uf) != 0 || 
          uf_bytein(uf) != 'c' || 
          uf_bytein(uf) != 'h' || 
          uf_bytein(uf) != 'e' || 
          uf_bytein(uf) != 'z')
        S_error1("", "malformed fasl-object header (missing magic word) found in ~a", uf->path);
    
      if ((n = uf_uptrin(uf, (INT *)0)) != scheme_version)
        S_error2("", "incompatible fasl-object version ~a found in ~a", S_string(S_format_scheme_version(n), -1), uf->path);
    
      if ((n = uf_uptrin(uf, (INT *)0)) != machine_type_any && n != machine_type)
        S_error2("", "incompatible fasl-object machine-type ~a found in ~a", S_string(S_lookup_machine_type(n), -1), uf->path);
    
      if (uf_bytein(uf) != '(')
        S_error1("", "malformed fasl-object header (missing open paren) found in ~a", uf->path);
    
      while ((c = uf_bytein(uf)) != ')')
        if (c < 0) S_error1("", "malformed fasl-object header (missing close paren) found in ~a", uf->path);
  
      ty = uf_bytein(uf);
    }
  
    switch (ty) {
      case fasl_type_visit:
      case fasl_type_revisit:
      case fasl_type_visit_revisit:
        break;
      default:
        S_error2("", "malformed fasl-object header (missing situation, got ~s) found in ~a", FIX(ty), uf->path);
        return (ptr)0;
    }
  
    size = uf_uptrin(uf, (INT *)0);
  
    if (ty == situation || situation == fasl_type_visit_revisit || ty == fasl_type_visit_revisit) {
      struct faslFileObj ffo;

      ty = uf_bytein(uf);
      switch (ty) {
        case fasl_type_gzip:
        case fasl_type_lz4: {
          ptr result; INT bytes_consumed;
          iptr dest_size = uf_uptrin(uf, &bytes_consumed);
          iptr src_size = size - (1 + bytes_consumed); /* adjust for u8 compression type and uptr dest_size */

          PREPARE_BYTEVECTOR(SRCBV(tc), src_size);
          PREPARE_BYTEVECTOR(DSTBV(tc), dest_size);
          if (uf_read(uf, &BVIT(SRCBV(tc),0), src_size) < 0)
            S_error1("", "unexpected eof in fasl file ~a", uf->path);
          result = S_bytevector_uncompress(DSTBV(tc), 0, dest_size, SRCBV(tc), 0, src_size,
                      (ty == fasl_type_gzip ? COMPRESS_GZIP : COMPRESS_LZ4));
          if (result != FIX(dest_size)) {
            if (Sstringp(result)) S_error2("fasl-read", "~@?", result, SRCBV(tc));
            S_error3("fasl-read", "uncompressed size ~s for ~s is smaller than expected size ~s", result, SRCBV(tc), FIX(dest_size));
          }
          ffo.size = dest_size;
          ffo.next = ffo.buf = &BVIT(DSTBV(tc),0);
          ffo.end = &BVIT(DSTBV(tc),dest_size);
          ffo.uf = uf;
          break;
        }
        case fasl_type_uncompressed: {
          ffo.size = size - 1; /* adjust for u8 compression type */
          ffo.next = ffo.end = ffo.buf = buf;
          ffo.uf = uf;
          break;
        }
        default:
          S_error2("", "malformed fasl-object header (missing possibly-compressed, got ~s) found in ~a", FIX(ty), uf->path);
          return (ptr)0;
      }
      faslin(tc, &x, S_G.null_vector, &strbuf, &ffo);
      S_flush_instruction_cache(tc);
      return x;
    } else {
      uf_skipbytes(uf, size);
    }
  }
}

static ptr bv_fasl_entry(ptr tc, ptr bv, unbufFaslFile uf) {
  ptr x; ptr strbuf = S_G.null_string;
  struct faslFileObj ffo;

  ffo.size = Sbytevector_length(bv);
  ffo.next = ffo.buf = &BVIT(bv, 0);
  ffo.end = &BVIT(bv, ffo.size);
  ffo.uf = uf;

  faslin(tc, &x, S_G.null_vector, &strbuf, &ffo);
  S_flush_instruction_cache(tc);
  return x;
}

static void fillFaslFile(faslFile f) {
  iptr n = f->size < SBUFSIZ ? f->size : SBUFSIZ;
  if (uf_read(f->uf, f->buf, n) < 0)
    S_error1("", "unexpected eof in fasl file ~a", f->uf->path);
  f->end = (f->next = f->buf) + n;
  f->size -= n;
}

#define bytein(f) ((((f)->next == (f)->end) ? fillFaslFile(f) : (void)0), *((f)->next++))

static void bytesin(octet *s, iptr n, faslFile f) {
  iptr avail = f->end - f->next;
  if (avail < n) {
    if (avail != 0) {
      memcpy(s, f->next, avail);
      f->next = f->end;
      n -= avail;
      s += avail;
    }
    if (uf_read(f->uf, s, n) < 0)
      S_error1("", "unexpected eof in fasl file ~a", f->uf->path);
    f->size -= n;
  } else {
    memcpy(s, f->next, n);
    f->next += n;
  }
}

static void toolarge(ptr path) {
  S_error1("", "fasl value too large for this machine type in ~a", path);
}

static iptr iptrin(faslFile f) {
  uptr n, m; octet k, k0;

  k0 = k = bytein(f);
  n = (k & 0x7f) >> 1;
  while (k & 1) {
    k = bytein(f);
    m = n << 7;
    if (m >> 7 != n) toolarge(f->uf->path);
    n = m | (k >> 1);
  }

  if (k0 & 0x80) {
    if (n < ((uptr)1 << (ptr_bits - 1))) {
      return -(iptr)n;
    } else if (n > ((uptr)1 << (ptr_bits - 1))) {
      toolarge(f->uf->path);
    }
#if (fixnum_bits > 32)
    return (iptr)0x8000000000000000;
#else
    return (iptr)0x80000000;
#endif
  } else {
    if (n >= ((uptr)1 << (ptr_bits - 1))) toolarge(f->uf->path);
    return (iptr)n;
  }
}

static uptr uptrin(faslFile f) {
  uptr n, m; octet k;

  k = bytein(f);
  n = k >> 1;
  while (k & 1) {
    k = bytein(f);
    m = n << 7;
    if (m >> 7 != n) toolarge(f->uf->path);
    n = m | (k >> 1);
  }

  return n;
}

static float singlein(faslFile f) {
  union { float f; U32 u; } val;

  val.u = (U32)uptrin(f);

  return val.f;
}

static double doublein(faslFile f) {
#ifdef LITTLE_ENDIAN_IEEE_DOUBLE
  union { double d; struct { U32 l; U32 h; } u; } val;
#else
  union { double d; struct { U32 h; U32 l; } u; } val;
#endif

  val.u.h = (U32)uptrin(f);
  val.u.l = (U32)uptrin(f);

  return val.d;
}

static iptr stringin(ptr *pstrbuf, iptr start, faslFile f) {
  iptr end, n, i; ptr p = *pstrbuf;

  end = start + (n = uptrin(f));
  if (Sstring_length(*pstrbuf) < end) {
     ptr newp = S_string((char *)0, end);
     for (i = 0; i != start; i += 1) Sstring_set(newp, i, Sstring_ref(p, i));
     *pstrbuf = p = newp;
  }
  for (i = start; i != end; i += 1) Sstring_set(p, i, uptrin(f));
  return n;
}

static void faslin(ptr tc, ptr *x, ptr t, ptr *pstrbuf, faslFile f) {
    IFASLCODE ty = bytein(f);
    switch (ty) {
        case fasl_type_pair: {
            iptr n; ptr p;
            n = uptrin(f);
            *x = p = Scons(FIX(0), FIX(0));
            faslin(tc, &INITCAR(p), t, pstrbuf, f);
            while (--n) {
                INITCDR(p) = Scons(FIX(0), FIX(0));
                p = INITCDR(p);
                faslin(tc, &INITCAR(p), t, pstrbuf, f);
            }
            faslin(tc, &INITCDR(p), t, pstrbuf, f);
            return;
        }
        case fasl_type_box:
        case fasl_type_immutable_box:
            *x = Sbox(FIX(0));
            faslin(tc, &INITBOXREF(*x), t, pstrbuf, f);
            if (ty == fasl_type_immutable_box)
              BOXTYPE(*x) = type_immutable_box;
            return;
        case fasl_type_symbol: {
            iptr n;
            n = stringin(pstrbuf, 0, f);
            *x = S_intern_sc(&STRIT(*pstrbuf, 0), n, Sfalse);
            return;
        }
        case fasl_type_gensym: {
            iptr pn, un;
            pn = stringin(pstrbuf, 0, f);
            un = stringin(pstrbuf, pn, f);
            *x = S_intern3(&STRIT(*pstrbuf, 0), pn, &STRIT(*pstrbuf, pn), un, Sfalse, Sfalse);
            return;
        }
        case fasl_type_ratnum:
            *x = S_rational(FIX(0), FIX(0));
            faslin(tc, &RATNUM(*x), t, pstrbuf, f);
            faslin(tc, &RATDEN(*x), t, pstrbuf, f);
            return;
        case fasl_type_exactnum:
            *x = S_exactnum(FIX(0), FIX(0));
            faslin(tc, &EXACTNUM_REAL_PART(*x), t, pstrbuf, f);
            faslin(tc, &EXACTNUM_IMAG_PART(*x), t, pstrbuf, f);
            return;
        case fasl_type_vector:
        case fasl_type_immutable_vector: {
            iptr n; ptr *p;
            n = uptrin(f);
            *x = S_vector(n);
            p = &INITVECTIT(*x, 0);
            while (n--) faslin(tc, p++, t, pstrbuf, f);
            if (ty == fasl_type_immutable_vector) {
              if (Svector_length(*x) == 0)
                *x = NULLIMMUTABLEVECTOR(tc);
              else
                VECTTYPE(*x) |= vector_immutable_flag;
            }
            return;
        }
        case fasl_type_fxvector:
        case fasl_type_immutable_fxvector: {
            iptr n; ptr *p;
            n = uptrin(f);
            *x = S_fxvector(n);
            p = &FXVECTIT(*x, 0);
            while (n--) {
              iptr t = iptrin(f);
              if (!FIXRANGE(t)) toolarge(f->uf->path);
              *p++ = FIX(t);
            }
            if (ty == fasl_type_immutable_fxvector) {
              if (Sfxvector_length(*x) == 0)
                *x = NULLIMMUTABLEFXVECTOR(tc);
              else
                FXVECTOR_TYPE(*x) |= fxvector_immutable_flag;
            }
            return;
        }
        case fasl_type_bytevector:
        case fasl_type_immutable_bytevector: {
            iptr n;
            n = uptrin(f);
            *x = S_bytevector(n);
            bytesin(&BVIT(*x,0), n, f);
            if (ty == fasl_type_immutable_bytevector) {
              if (Sbytevector_length(*x) == 0)
                *x = NULLIMMUTABLEBYTEVECTOR(tc);
              else
                BYTEVECTOR_TYPE(*x) |= bytevector_immutable_flag;
            }
            return;
        }
        case fasl_type_base_rtd: {
            ptr rtd;
            if ((rtd = S_G.base_rtd) == Sfalse) {
              if (!Srecordp(rtd)) S_error_abort("S_G.base-rtd has not been set");
            }
            *x = rtd;
            return;
        } case fasl_type_rtd: {
            ptr rtd, rtd_uid, plist, ls;

            faslin(tc, &rtd_uid, t, pstrbuf, f);

           /* look for rtd on uid's property list */
            plist = SYMSPLIST(rtd_uid);
            for (ls = plist; ls != Snil; ls = Scdr(Scdr(ls))) {
              if (Scar(ls) == S_G.rtd_key) {
                ptr tmp;
                *x = rtd = Scar(Scdr(ls));
                fasl_record(tc, &tmp, t, pstrbuf, f);
                if (!rtd_equiv(tmp, rtd))
                  S_error2("", "incompatible record type ~s in ~a", RECORDDESCNAME(tmp), f->uf->path);
                return;
              }
            }

            fasl_record(tc, x, t, pstrbuf, f);
            rtd = *x;

           /* register rtd on uid's property list */
            SETSYMSPLIST(rtd_uid, Scons(S_G.rtd_key, Scons(rtd, plist)));
            return;
        }
        case fasl_type_record: {
            fasl_record(tc, x, t, pstrbuf, f);
            return;
        }
        case fasl_type_eq_hashtable: {
            ptr rtd, ht, v; uptr subtype; uptr veclen, i, n;
            if ((rtd = S_G.eq_ht_rtd) == Sfalse) {
              S_G.eq_ht_rtd = rtd = SYMVAL(S_intern((const unsigned char *)"$eq-ht-rtd"));
              if (!Srecordp(rtd)) S_error_abort("$eq-ht-rtd has not been set");
            }
            *x = ht = S_record(size_record_inst(UNFIX(RECORDDESCSIZE(rtd))));
            RECORDINSTTYPE(ht) = rtd;
            INITPTRFIELD(ht,eq_hashtable_type_disp) = S_G.eq_symbol;
            INITPTRFIELD(ht,eq_hashtable_mutablep_disp) = bytein(f) ? Strue : Sfalse;
            switch ((subtype = bytein(f))) {
            case eq_hashtable_subtype_normal:
            case eq_hashtable_subtype_weak:
            case eq_hashtable_subtype_ephemeron:
              INITPTRFIELD(ht,eq_hashtable_subtype_disp) = FIX(subtype);
              break;
            default:
              S_error2("", "invalid eq-hashtable subtype code", FIX(subtype), f->uf->path);
            }
            INITPTRFIELD(ht,eq_hashtable_minlen_disp) = FIX(uptrin(f));
            veclen = uptrin(f);
            INITPTRFIELD(ht,eq_hashtable_vec_disp) = v = S_vector(veclen);
            n = uptrin(f);
            INITPTRFIELD(ht,eq_hashtable_size_disp) = FIX(n);
            for (i = 0; i < veclen ; i += 1) { INITVECTIT(v, i) = FIX(i); }
            while (n > 0) {
              ptr keyval;
              switch (subtype) {
              case eq_hashtable_subtype_normal:
                keyval = Scons(FIX(0), FIX(0));
                break;
              case eq_hashtable_subtype_weak:
                keyval = S_cons_in(space_weakpair, 0, FIX(0), FIX(0));
                break;
              case eq_hashtable_subtype_ephemeron:
              default:
                keyval = S_cons_in(space_ephemeron, 0, FIX(0), FIX(0));
                break;
              }
              faslin(tc, &INITCAR(keyval), t, pstrbuf, f);
              faslin(tc, &INITCDR(keyval), t, pstrbuf, f);
              i = ((uptr)Scar(keyval) >> primary_type_bits) & (veclen - 1);
              INITVECTIT(v, i) = S_tlc(keyval, ht, Svector_ref(v, i));
              n -= 1;
            }
            return;
        }
        case fasl_type_symbol_hashtable: {
            ptr rtd, ht, equiv, v; uptr equiv_code, veclen, i, n;
            if ((rtd = S_G.symbol_ht_rtd) == Sfalse) {
              S_G.symbol_ht_rtd = rtd = SYMVAL(S_intern((const unsigned char *)"$symbol-ht-rtd"));
              if (!Srecordp(rtd)) S_error_abort("$symbol-ht-rtd has not been set");
            }
            *x = ht = S_record(size_record_inst(UNFIX(RECORDDESCSIZE(rtd))));
            RECORDINSTTYPE(ht) = rtd;
            INITPTRFIELD(ht,symbol_hashtable_type_disp) = S_G.symbol_symbol;
            INITPTRFIELD(ht,symbol_hashtable_mutablep_disp) = bytein(f) ? Strue : Sfalse;
            INITPTRFIELD(ht,symbol_hashtable_minlen_disp) = FIX(uptrin(f));
            equiv_code = bytein(f);
            switch (equiv_code) {
              case 0:
                if ((equiv = S_G.eqp) == Sfalse) {
                  S_G.eqp = equiv = SYMVAL(S_intern((const unsigned char *)"eq?"));
                  if (!Sprocedurep(equiv)) S_error_abort("fasl: eq? has not been set");
                }
                break;
              case 1:
                if ((equiv = S_G.eqvp) == Sfalse) {
                  S_G.eqvp = equiv = SYMVAL(S_intern((const unsigned char *)"eqv?"));
                  if (!Sprocedurep(equiv)) S_error_abort("fasl: eqv? has not been set");
                }
                break;
              case 2:
                if ((equiv = S_G.equalp) == Sfalse) {
                  S_G.equalp = equiv = SYMVAL(S_intern((const unsigned char *)"equal?"));
                  if (!Sprocedurep(equiv)) S_error_abort("fasl: equal? has not been set");
                }
                break;
              case 3:
                if ((equiv = S_G.symboleqp) == Sfalse) {
                  S_G.symboleqp = equiv = SYMVAL(S_intern((const unsigned char *)"symbol=?"));
                  if (!Sprocedurep(equiv)) S_error_abort("fasl: symbol=? has not been set");
                }
                break;
              default:
                S_error2("", "invalid symbol-hashtable equiv code", FIX(equiv_code), f->uf->path);
                /* make compiler happy */
                equiv = Sfalse;
            }
            INITPTRFIELD(ht,symbol_hashtable_equivp_disp) = equiv;
            veclen = uptrin(f);
            INITPTRFIELD(ht,symbol_hashtable_vec_disp) = v = S_vector(veclen);
            n = uptrin(f);
            INITPTRFIELD(ht,symbol_hashtable_size_disp) = FIX(n);
            for (i = 0; i < veclen ; i += 1) { INITVECTIT(v, i) = Snil; }
            while (n > 0) {
              ptr keyval;
              keyval = Scons(FIX(0), FIX(0));
              faslin(tc, &INITCAR(keyval), t, pstrbuf, f);
              faslin(tc, &INITCDR(keyval), t, pstrbuf, f);
              i = UNFIX(SYMHASH(Scar(keyval))) & (veclen - 1);
              INITVECTIT(v, i) = Scons(keyval, Svector_ref(v, i));
              n -= 1;
            }
            return;
        }
        case fasl_type_closure: {
            ptr cod; iptr offset;
            offset = uptrin(f);
            *x = S_closure((ptr)0, 0);
            faslin(tc, &cod, t, pstrbuf, f);
            CLOSENTRY(*x) = (ptr)((uptr)cod + offset);
            return;
        }
        case fasl_type_flonum: {
            *x = Sflonum(doublein(f));
            return;
        }
        case fasl_type_inexactnum: {
            ptr rp, ip;
            faslin(tc, &rp, t, pstrbuf, f);
            faslin(tc, &ip, t, pstrbuf, f);
            *x = S_inexactnum(FLODAT(rp), FLODAT(ip));
            return;
        }
        case fasl_type_string:
        case fasl_type_immutable_string: {
            iptr i, n; ptr str;
            n = uptrin(f);
            str = S_string((char *)0, n);
            for (i = 0; i != n; i += 1) Sstring_set(str, i, uptrin(f));
            if (ty == fasl_type_immutable_string) {
              if (n == 0)
                str = NULLIMMUTABLESTRING(tc);
              else
                STRTYPE(str) |= string_immutable_flag;
            }
            *x = str;
            return;
        }
        case fasl_type_small_integer:
            *x = Sinteger(iptrin(f));
            return;
        case fasl_type_large_integer: {
            IBOOL sign; iptr n; ptr t; bigit *p;
            sign = bytein(f);
            n = uptrin(f);
            t = S_bignum(tc, n, sign);
            p = &BIGIT(t, 0);
            while (n--) *p++ = (bigit)uptrin(f);
            *x = S_normalize_bignum(t);
            return;
        }
        case fasl_type_weak_pair:
            *x = S_cons_in(space_weakpair, 0, FIX(0), FIX(0));
            faslin(tc, &INITCAR(*x), t, pstrbuf, f);
            faslin(tc, &INITCDR(*x), t, pstrbuf, f);
            return;
        case fasl_type_ephemeron:
            *x = S_cons_in(space_ephemeron, 0, FIX(0), FIX(0));
            faslin(tc, &INITCAR(*x), t, pstrbuf, f);
            faslin(tc, &INITCDR(*x), t, pstrbuf, f);
            return;
        case fasl_type_code: {
            iptr n, m, a; INT flags; iptr free;
            ptr co, reloc, name, pinfos;
            flags = bytein(f);
            free = uptrin(f);
            n = uptrin(f) /* length in bytes of code */;
            *x = co = S_code(tc, type_code | (flags << code_flags_offset), n);
            CODEFREE(co) = free;
            faslin(tc, &name, t, pstrbuf, f);
            if (Sstringp(name)) name = SYMNAME(S_intern_sc(&STRIT(name, 0), Sstring_length(name), name));
            CODENAME(co) = name;
            faslin(tc, &CODEARITYMASK(co), t, pstrbuf, f);
            faslin(tc, &CODEINFO(co), t, pstrbuf, f);
            faslin(tc, &pinfos, t, pstrbuf, f);
            CODEPINFOS(co) = pinfos;
            if (pinfos != Snil) {
              S_G.profile_counters = Scons(S_weak_cons(co, pinfos), S_G.profile_counters);
            }
            bytesin((octet *)&CODEIT(co, 0), n, f);
            m = uptrin(f);
            CODERELOC(co) = reloc = S_relocation_table(m);
            RELOCCODE(reloc) = co;
            a = 0;
            n = 0;
            while (n < m) {
              INT type_etc, type; uptr item_off, code_off;
              ptr obj;
              type_etc = bytein(f);
              type = type_etc >> 2;
              code_off = uptrin(f);
              item_off = (type_etc & 2) ? uptrin(f) : 0;
              if (type_etc & 1) {
                RELOCIT(reloc,n) = (type << reloc_type_offset)|reloc_extended_format ;    n += 1;
                RELOCIT(reloc,n) = item_off; n += 1;
                RELOCIT(reloc,n) = code_off; n += 1;
              } else {
                RELOCIT(reloc,n) = MAKE_SHORT_RELOC(type,code_off,item_off); n += 1;
              }
              a += code_off;
              faslin(tc, &obj, t, pstrbuf, f);
              S_set_code_obj("read", type, co, a, obj, item_off);
            }
            return;
        }
        case fasl_type_immediate:
            *x = (ptr)uptrin(f);
            return;
        case fasl_type_entry:
            *x = (ptr)S_lookup_c_entry(uptrin(f));
            return;
        case fasl_type_library:
            *x = S_lookup_library_entry(uptrin(f), 1);
            return;
        case fasl_type_library_code:
            *x = CLOSCODE(S_lookup_library_entry(uptrin(f), 1));
            return;
        case fasl_type_graph:
            faslin(tc, x, S_vector(uptrin(f)), pstrbuf, f);
            return;
        case fasl_type_graph_def: {
            ptr *p;
            p = &INITVECTIT(t, uptrin(f));
            faslin(tc, p, t, pstrbuf, f);
            *x = *p;
            return;
        }
        case fasl_type_graph_ref:
            *x = Svector_ref(t, uptrin(f));
            return;
        default:
            S_error2("", "invalid object type ~d in fasl file ~a", FIX(ty), f->uf->path);
    }
}

#define big 0
#define little 1
static void fasl_record(ptr tc, ptr *x, ptr t, ptr *pstrbuf, faslFile f) {
  uptr size, n, addr; ptr p; UINT padty;

  size = uptrin(f);
  n = uptrin(f);
  *x = p = S_record(size_record_inst(size));
  faslin(tc, &RECORDINSTTYPE(p), t, pstrbuf, f);
  addr = (uptr)&RECORDINSTIT(p, 0);
  for (; n != 0; n -= 1) {
    padty = bytein(f);
    addr += padty >> 4;
    switch (padty & 0xf) {
      case fasl_fld_ptr:
        faslin(tc, (ptr *)addr, t, pstrbuf, f);
        addr += sizeof(ptr);
        break;
      case fasl_fld_u8:
        *(U8 *)addr = (U8)bytein(f);
        addr += 1;
        break;
      case fasl_fld_i16:
        *(I16 *)addr = (I16)iptrin(f);
        addr += 2;
        break;
      case fasl_fld_i24: {
        iptr q = iptrin(f);
#if (native_endianness == little)
        *(U16 *)addr = (U16)q;
        *(U8 *)(addr + 2) = (U8)(q >> 16);
#elif (native_endianness == big)
        *(U16 *)addr = (U16)(q >> 8);
        *(U8 *)(addr + 2) = (U8)q;
#else
        unexpected_endianness();
#endif
        addr += 3;
        break;
      }
      case fasl_fld_i32:
        *(I32 *)addr = (I32)iptrin(f);
        addr += 4;
        break;
      case fasl_fld_i40: {
        I64 q;
#if (ptr_bits == 32)
        q = (I64)iptrin(f) << 32;
        q |= (U32)uptrin(f);
#elif (ptr_bits == 64)
        q = (I64)iptrin(f);
#else
        unexpected_ptr_bits();
#endif
#if (native_endianness == little)
        *(U32 *)addr = (U32)q;
        *(U8 *)(addr + 4) = (U8)(q >> 32);
#elif (native_endianness == big)
        *(U32 *)addr = (U32)(q >> 8);
        *(U8 *)(addr + 4) = (U8)q;
#else
        unexpected_endianness();
#endif
        addr += 5;
        break;
      }
      case fasl_fld_i48: {
        I64 q;
#if (ptr_bits == 32)
        q = (I64)iptrin(f) << 32;
        q |= (U32)uptrin(f);
#elif (ptr_bits == 64)
        q = (I64)iptrin(f);
#else
        unexpected_ptr_bits();
#endif
#if (native_endianness == little)
        *(U32 *)addr = (U32)q;
        *(U16 *)(addr + 4) = (U16)(q >> 32);
#elif (native_endianness == big)
        *(U32 *)addr = (U32)(q >> 16);
        *(U16 *)(addr + 4) = (U16)q;
#else
        unexpected_endianness();
#endif
        addr += 6;
        break;
      }
      case fasl_fld_i56: {
        I64 q;
#if (ptr_bits == 32)
        q = (I64)iptrin(f) << 32;
        q |= (U32)uptrin(f);
#elif (ptr_bits == 64)
        q = (I64)iptrin(f);
#else
        unexpected_ptr_bits();
#endif
#if (native_endianness == little)
        *(U32 *)addr = (U32)q;
        *(U16 *)(addr + 4) = (U16)(q >> 32);
        *(U8 *)(addr + 6) = (U8)(q >> 48);
#elif (native_endianness == big)
        *(U32 *)addr = (U32)(q >> 24);
        *(U32 *)(addr + 3) = (U32)q;
#else
        unexpected_endianness();
#endif
        addr += 7;
        break;
      }
      case fasl_fld_i64: {
        I64 q;
#if (ptr_bits == 32)
        q = (I64)iptrin(f) << 32;
        q |= (U32)uptrin(f);
#elif (ptr_bits == 64)
        q = (I64)iptrin(f);
#else
        unexpected_ptr_bits();
#endif
        *(I64 *)addr = q;
        addr += 8;
        break;
      }
      case fasl_fld_single:
        *(float *)addr = (float)singlein(f);
        addr += sizeof(float);
        break;
      case fasl_fld_double:
        *(double *)addr = (double)doublein(f);
        addr += sizeof(double);
        break;
      default:
        S_error1("", "unrecognized record fld type ~d", FIX(padty & 0xf));
        break;
    }
  }
}

/* limited version for checking rtd fields */
static IBOOL equalp(x, y) ptr x, y; {
  if (x == y) return 1;
  if (Spairp(x)) return Spairp(y) && equalp(Scar(x), Scar(y)) && equalp(Scdr(x), Scdr(y));
  if (Svectorp(x)) {
    iptr n;
    if (!Svectorp(y)) return 0;
    if ((n = Svector_length(x)) != Svector_length(y)) return 0;
    while (--n >= 0) if (!equalp(Svector_ref(x, n), Svector_ref(y, n))) return 0;
    return 1;
  }
  return Sbignump(x) && Sbignump(y) && S_big_eq(x, y);
}

static IBOOL rtd_equiv(x, y) ptr x, y; {
  return RECORDINSTTYPE(x) == RECORDINSTTYPE(y) &&
         RECORDDESCPARENT(x) == RECORDDESCPARENT(y) &&
         equalp(RECORDDESCPM(x), RECORDDESCPM(y)) &&
         equalp(RECORDDESCMPM(x), RECORDDESCMPM(y)) &&
         equalp(RECORDDESCFLDS(x), RECORDDESCFLDS(y)) &&
         RECORDDESCSIZE(x) == RECORDDESCSIZE(y) &&
         RECORDDESCFLAGS(x) == RECORDDESCFLAGS(y);
}

#ifdef HPUX
INT pax_decode21(INT x)
{
  INT x0_4, x5_6, x7_8, x9_19, x20;

  x20   = x & 0x1; x >>= 1;
  x9_19 = x & 0x7ff; x >>= 11;
  x7_8  = x & 0x3; x >>= 2;
  x5_6  = x & 0x3;
  x0_4  = x >> 2;

  return (((x20<<11 | x9_19)<<2 | x5_6)<<5 | x0_4)<<2 | x7_8;
}

INT pax_encode21(INT n)
{
  INT x0_4, x5_6, x7_8, x9_19, x20;

  x7_8  = n & 0x3; n >>= 2;
  x0_4  = n & 0x1f; n >>= 5;
  x5_6  = n & 0x3; n >>= 2;
  x9_19 = n & 0x7ff;
  x20   = n >> 11;

  return (((x0_4<<2 | x5_6)<<2 | x7_8)<<11 | x9_19)<<1 | x20;
}
#endif /* HPUX */

/* used here, in S_gc(), and in compile.ss */
void S_set_code_obj(who, typ, p, n, x, o) char *who; IFASLCODE typ; iptr n, o; ptr p, x; {
    void *address; uptr item;

    address = (void *)((uptr)p + n);
    item = (uptr)x + o;
    switch (typ) {
        case reloc_abs:
            *(uptr *)address = item;
            break;
#ifdef ARMV6
        case reloc_arm32_abs:
            arm32_set_abs(address, item);
            break;
        case reloc_arm32_jump:
            arm32_set_jump(address, item, 0);
            break;
        case reloc_arm32_call:
            arm32_set_jump(address, item, 1);
            break;
#endif /* ARMV6 */
#ifdef PPC32
        case reloc_ppc32_abs:
            ppc32_set_abs(address, item);
            break;
        case reloc_ppc32_jump:
            ppc32_set_jump(address, item, 0);
            break;
        case reloc_ppc32_call:
            ppc32_set_jump(address, item, 1);
            break;
#endif /* PPC32 */
#ifdef I386
        case reloc_rel:
            item = item - ((uptr)address + sizeof(uptr));
            *(uptr *)address = item;
            break;
#endif /* I386 */
#ifdef X86_64
        case reloc_x86_64_jump:
            x86_64_set_jump(address, item, 0);
            break;
        case reloc_x86_64_call:
            x86_64_set_jump(address, item, 1);
            break;
#endif /* X86_64 */
#ifdef SPARC64
        case reloc_sparc64abs:
            sparc64_set_literal(address, item);
            break;
      /* we don't use this presently since it can't handle out-of-range
         relocations */
        case reloc_sparc64rel:
           /* later: make the damn thing local by copying it an
              every other code object we can reach into a single
              close area of memory */
            item = item - (uptr)address;
            if ((iptr)item < -0x20000000 || (iptr)item > 0x1FFFFFFF)
              S_error1("", "sparc64rel address out of range ~x",
                       Sunsigned((uptr)address));
            *(U32 *)address = *(U32 *)address & ~0x3fffffff | item >> 2 & 0x3fffffff;
            break;
#endif /* SPARC64 */
#ifdef SPARC
        case reloc_sparcabs:
            *(U32 *)address = *(U32 *)address & ~0x3fffff | item >> 10 & 0x3fffff;
            *((U32 *)address + 1) = *((U32 *)address + 1) & ~0x3ff | item & 0x3ff;
            break;
        case reloc_sparcrel:
            item = item - (uptr)address;
            *(U32 *)address = *(U32 *)address & ~0x3fffffff | item >> 2 & 0x3fffffff;
            break;
#endif /* SPARC */
        default:
            S_error1(who, "invalid relocation type ~s", FIX(typ));
    }
}

/* used in S_gc() */
ptr S_get_code_obj(typ, p, n, o) IFASLCODE typ; iptr n, o; ptr p; {
    void *address; uptr item;

    address = (void *)((uptr)p + n);
    switch (typ) {
        case reloc_abs:
            item = *(uptr *)address;
            break;
#ifdef ARMV6
        case reloc_arm32_abs:
            item = arm32_get_abs(address);
            break;
        case reloc_arm32_jump:
        case reloc_arm32_call:
            item = arm32_get_jump(address);
            break;
#endif /* ARMV6 */
#ifdef PPC32
        case reloc_ppc32_abs:
            item = ppc32_get_abs(address);
            break;
        case reloc_ppc32_jump:
        case reloc_ppc32_call:
            item = ppc32_get_jump(address);
            break;
#endif /* PPC32 */
#ifdef I386
        case reloc_rel:
            item = *(uptr *)address;
            item = item + ((uptr)address + sizeof(uptr));
            break;
#endif /* I386 */
#ifdef X86_64
        case reloc_x86_64_jump:
        case reloc_x86_64_call:
            item = x86_64_get_jump(address);
            break;
#endif /* X86_64 */
#ifdef SPARC64
        case reloc_sparc64abs:
            item = sparc64_get_literal(address);
            break;
        case reloc_sparc64rel:
            item = (*(U32 *)address & 0x3fffffff) << 2;
            if (item & 0x80000000) /* sign bit set */
              item = item | 0xffffffff00000000;
            item = (uptr)address + (iptr)item;
            break;
#endif /* SPARC64 */
#ifdef SPARC
        case reloc_sparcabs:
            item = (*(U32 *)address & 0x3fffff) << 10 | *((U32 *)address + 1) & 0x3ff;
            break;
        case reloc_sparcrel:
            item = (*(U32 *)address & 0x3fffffff) << 2;
            item += (uptr)address;
            break;
#endif /* SPARC */
        default:
            S_error1("", "invalid relocation type ~s", FIX(typ));
            return (ptr)0 /* not reached */;
    }
    return (ptr)(item - o);
}


#ifdef ARMV6
static void arm32_set_abs(void *address, uptr item) {
  /* code generator produces ldrlit destreg, 0; brai 0; long 0 */
  /* we change long 0 => long item */
  *((U32 *)address + 2) = item;
}

static uptr arm32_get_abs(void *address) {
  return *((U32 *)address + 2);
}

#define MAKE_B(n) (0xEA000000 | (n))
#define MAKE_BL(n) (0xEB000000 | (n))
#define B_OR_BL_DISP(x) ((x) & 0xFFFFFF)
#define MAKE_BX(reg) (0xE12FFF10 | (reg))
#define MAKE_BLX(reg) (0xE12FFF30 | (reg))
#define MAKE_LDRLIT(dst,n) (0xE59F0000 | ((dst) << 12) | (n))
#define LDRLITP(x) (((x) & 0xFFFF0000) == 0xE59F0000)
#define LDRLIT_DST(x) (((x) >> 12) & 0xf)
#define MAKE_MOV(dst,src) (0xE1A00000 | ((dst) << 12) | (src))
#define MOV_SRC(x) ((x) & 0xf)
/* nop instruction is not supported by all ARMv6 chips, so use recommended mov r0, r0 */
#define NOP MAKE_MOV(0,0)

static void arm32_set_jump(void *address, uptr item, IBOOL callp) {
  /* code generator produces ldrlit %ip, 0; brai 0; long 0; bx or blx %ip */
  U32 inst = *((U32 *)address + 0);
  INT reg = LDRLITP(inst) ? LDRLIT_DST(inst) : MOV_SRC(*((U32 *)address + 1));
  I32 worddisp = (U32 *)item - ((U32 *)address + 2);
  if (worddisp >= -0x800000 && worddisp <= 0x7FFFFF) {
    worddisp &= 0xFFFFFF;
    *((U32 *)address + 0) = (callp ? MAKE_BL(worddisp) : MAKE_B(worddisp));
    *((U32 *)address + 1) = MAKE_MOV(reg,reg); /* effective NOP recording tmp reg for later use */
    *((U32 *)address + 2) = NOP;
    *((U32 *)address + 3) = NOP;
  } else {
    *((U32 *)address + 0) = MAKE_LDRLIT(reg,0);
    *((U32 *)address + 1) = MAKE_B(0);
    *((U32 *)address + 2) = item;
    *((U32 *)address + 3) = (callp ? MAKE_BLX(reg) : MAKE_BX(reg));
  }
}

static uptr arm32_get_jump(void *address) {
  U32 inst = *((U32 *)address + 0);
  if (LDRLITP(inst)) {
    return *((U32 *)address + 2);
  } else {
    I32 worddisp = B_OR_BL_DISP(inst);
    if (worddisp >= 0x800000) worddisp -= 0x1000000;
    return (uptr)(((U32 *)address + 2) + worddisp);
  }
}
#endif /* ARMV6 */

#ifdef PPC32

#define UPDATE_ADDIS(item, instr) (((instr) & ~0xFFFF) | (((item) >> 16) & 0xFFFF))
#define UPDATE_ADDI(item, instr)  (((instr) & ~0xFFFF) | ((item) & 0xFFFF))

#define MAKE_B(disp, callp) ((18 << 26) | (((disp) & 0xFFFFFF) << 2) | (callp))
#define MAKE_ADDIS(item)    ((15 << 26) | (((item) >> 16) & 0xFFFF))
#define MAKE_ORI(item)      ((24 << 26) | ((item) & 0xFFFF))
#define MAKE_NOP            ((24 << 26))
#define MAKE_MTCTR          ((31 << 26) | (9 << 16) | (467 << 1))
#define MAKE_BCTR(callp)    ((19 << 26) | (20 << 21) | (528 << 1) | (callp))

static void ppc32_set_abs(void *address, uptr item) {
  /* code generator produces addis destreg, %r0, 0 (hi) ; addi destreg, destreg, 0 (lo) */
  /* we change 0 (hi) => upper 16 bits of address */
  /* we change 0 (lo) => lower 16 bits of address */
  /* low part is signed: if negative, increment high part */
  item = item + (item << 1 & 0x10000);
  *((U32 *)address + 0) = UPDATE_ADDIS(item, *((U32 *)address + 0));
  *((U32 *)address + 1) = UPDATE_ADDI(item, *((U32 *)address + 1));
}

static uptr ppc32_get_abs(void *address) {
  uptr item = ((*((U32 *)address + 0) & 0xFFFF) << 16) | (*((U32 *)address + 1) & 0xFFFF);
  return item - (item << 1 & 0x10000);
}

static void ppc32_set_jump(void *address, uptr item, IBOOL callp) {
  iptr disp = (iptr *)item - (iptr *)address;
  if (-0x800000 <= disp && disp <= 0x7FFFFF) {
    *((U32 *)address + 0) = MAKE_B(disp, callp);
    *((U32 *)address + 1) = MAKE_NOP;
    *((U32 *)address + 2) = MAKE_NOP;
    *((U32 *)address + 3) = MAKE_NOP;
  } else {
    *((U32 *)address + 0) = MAKE_ADDIS(item);
    *((U32 *)address + 1) = MAKE_ORI(item);
    *((U32 *)address + 2) = MAKE_MTCTR;
    *((U32 *)address + 3) = MAKE_BCTR(callp);
  }
}

static uptr ppc32_get_jump(void *address) {
  uptr item, instr = *(U32 *)address;

  if ((instr >> 26) == 18) {
    /* bl disp */
    iptr disp = (instr >> 2) & 0xFFFFFF;
    if (disp & 0x800000) disp -= 0x1000000;
    item = (uptr)address + (disp << 2);
  } else {
    /* lis r0, high
       ori r0, r0, low */
    item = ((instr & 0xFFFF) << 16) | (*((U32 *)address + 1) & 0xFFFF);
  }

  return item;
}
#endif /* PPC32 */

#ifdef X86_64
static void x86_64_set_jump(void *address, uptr item, IBOOL callp) {
  I64 disp = (I64)item - ((I64)address + 5); /* 5 = size of call instruction */
  if ((I32)disp == disp) {
    *(octet *)address = callp ? 0xE8 : 0xE9;  /* call or jmp disp32 opcode */
    *(I32 *)((uptr)address + 1) = (I32)disp;
    *((octet *)address + 5) = 0x90; /* nop */
    *((octet *)address + 6) = 0x90; /* nop */
    *((octet *)address + 7) = 0x90; /* nop */
    *((octet *)address + 8) = 0x90; /* nop */
    *((octet *)address + 9) = 0x90; /* nop */
    *((octet *)address + 10) = 0x90; /* nop */
    *((octet *)address + 11) = 0x90; /* nop */
  } else {
    *(octet *)address = 0x48; /* REX w/REX.w set */
    *((octet *)address + 1)= 0xB8;  /* MOV imm64 to RAX */
    *(uptr *)((uptr)address + 2) = item;
    *((octet *)address + 10) = 0xFF;  /* call/jmp reg/mem opcode */
    *((octet *)address + 11) = callp ? 0xD0 : 0xE0; /* mod=11, ttt=010 (call) or 100 (jmp), r/m = 0 (RAX) */
  }
}

static uptr x86_64_get_jump(void *address) {
  if (*(octet *)address == 0x48) /* REX w/REX.w set */
   /* must be long form: move followed by call/jmp */
    return *(uptr *)((uptr)address + 2);
  else
   /* must be short form: call/jmp */
    return ((uptr)address + 5) + *(I32 *)((uptr)address + 1);
}
#endif /* X86_64 */

#ifdef SPARC64
#define ASMREG0 1
/* TMPREG is asm-literal-tmp in sparc64macros.ss */
#define TMPREG 5
/* CRETREG is retreg in sparc64macros.ss */
#define CRETREG 15
/* SRETREG is ret in sparc64macros.ss */
#define SRETREG 26

#define OP_ADDI 0x80002000
#define OP_CALL 0x40000000
#define OP_JSR 0x81C00000
#define OP_OR 0x80100000
#define OP_ORI 0x80102000
#define OP_SETHI 0x1000000
/* SLLXI is the 64-bit version */
#define OP_SLLXI 0x81283000
#define OP_XORI 0x80182000
/* NOP is sethi %g0,0 */
#define NOP 0x1000000
#define IMMMASK (U32)0x1fff
#define IMMRANGE(x) ((U32)(x) + (U32)0x1000 <= IMMMASK)
#define ADDI(src,imm,dst) (OP_ADDI | (dst) << 25 | (src) << 14 | (imm) & IMMMASK)
#define JSR(src) (OP_JSR | CRETREG << 25 | (src) << 14)
#define ORI(src,imm,dst) (OP_ORI | (dst) << 25 | (src) << 14 | (imm) & IMMMASK)
#define SETHI(dst,high) (OP_SETHI | (dst) << 25 | (high) & 0x3fffff)
#define CALL(disp) (OP_CALL | (disp) >> 2 & 0x3fffffff)


static INT extract_reg_from_sethi(address) void *address; {
  return *(U32 *)address >> 25;
}

static void emit_sethi_lo(U32 item, INT destreg, void *address) {
  U32 high = item >> 10;
  U32 low = item & 0x3ff;

 /* sethi destreg, high */
  *(U32 *)address = SETHI(destreg,high);
 /* setlo destreg, low */
  *((U32 *)address + 1) = ORI(destreg,low,destreg);
}

static uptr sparc64_get_literal(address) void *address; {
  uptr item;

 /* we may have "call disp" followed by delay instruction */
  item = *(U32 *)address;
  if (item >> 30 == OP_CALL >> 30) {
    item = (item & 0x3fffffff) << 2;
    if (item & 0x80000000) /* sign bit set */
      item = item | 0xffffffff00000000;
    item = (uptr)address + (iptr)item;
    return item;
  }

  item = (item & 0x3fffff) << 10 | *((U32 *)address + 1) & 0x3ff;
  if (*((U32 *)address + 2) != NOP) {
    item = item << 32 |
           (*((U32 *)address + 3) & 0x3fffff) << 10 |
            *((U32 *)address + 4) & 0x3ff;
  }
  return item;
}

static U32 adjust_delay_inst(delay_inst, old_call_addr, new_call_addr)
      U32 delay_inst; U32 *old_call_addr, *new_call_addr; {
  INT offset;

  offset = sizeof(U32) * (old_call_addr - new_call_addr);
  if (offset == 0) return delay_inst;

  if ((delay_inst & ~IMMMASK) == ADDI(CRETREG,0,SRETREG)) {
    INT k = delay_inst & IMMMASK;
    k = k - ((k << 1) & (IMMMASK+1));
    offset = k + offset;
    if (IMMRANGE(offset)) return ADDI(CRETREG,offset,SRETREG);
  } else if ((delay_inst & ~IMMMASK) == ADDI(CRETREG,0,CRETREG)) {
    INT k = delay_inst & IMMMASK;
    k = k - ((k << 1) & (IMMMASK+1));
    offset = k + offset;
    if (offset == 0) return NOP;
    if (IMMRANGE(offset)) return ADDI(CRETREG,offset,CRETREG);
  } else if (IMMRANGE(offset))
    return ADDI(CRETREG,offset,CRETREG);

  return 0; /* fortunately, not a valid instruction here */
}

static void sparc64_set_call(address, call_addr, item) void *address; U32 *call_addr; uptr item; {
  U32 delay_inst = *(call_addr + 1), new_delay_inst; iptr disp;

 /* later: make item local if it refers to Scheme code, i.e., is in the
    Scheme heap, by copying it and every other code object we can reach
    into a single close area of memory.  Or generate a close stub. */
  disp = item - (uptr)address;
  if (disp >= -0x20000000 && disp <= 0x1FFFFFFF &&
       (new_delay_inst = adjust_delay_inst(delay_inst, call_addr,
                                            (U32 *)address))) {
    *(U32 *)address = CALL(disp);
    *((U32 *)address + 1) = new_delay_inst;
  } else {
    INT n = sparc64_set_lit_only(address, item, ASMREG0);
    new_delay_inst = adjust_delay_inst(delay_inst, call_addr, (U32 *)address + n);
    *((U32 *)address + n) = JSR(ASMREG0);
    *((U32 *)address + n + 1) = new_delay_inst;
  }
}

static INT sparc64_set_lit_only(address, item, destreg) void *address; uptr item; I32 destreg; {

  if ((iptr)item >= -0xffffffff && item <= 0xffffffff) {
    uptr x, high, low;

    if ((iptr)item < 0) {
        x = 0x100000000 - item;
        high = x >> 10;
        low = x - (high << 10);
       /* sethi destreg, ~high */
        *(U32 *)address = OP_SETHI | destreg << 25 | ~high & 0x3fffff;
       /* xor.i destreg, low|0x1c00, destreg */
        *((U32 *)address + 1) = OP_XORI | destreg << 25 | destreg << 14 |
                   low | 0x1c00;
    } else {
        emit_sethi_lo(item, destreg, address);
    }
    *((U32 *)address + 2) = NOP;
    *((U32 *)address + 3) = NOP;
    *((U32 *)address + 4) = NOP;
    *((U32 *)address + 5) = NOP;
    return 2;
  } else {
    emit_sethi_lo(item >> 32, destreg, address);
   /* sll destreg, 32, destreg */
    *((U32 *)address + 2) = OP_SLLXI | destreg << 25 | destreg << 14 | 32;
    emit_sethi_lo(item & 0xffffffff, TMPREG, (void *)((U32 *)address+3));
   /* or destreg, tmpreg, destreg */
    *((U32 *)address + 5) = OP_OR | destreg << 25 | destreg << 14 | TMPREG;
    return 6;
  }
}

static void sparc64_set_literal(address, item) void *address; uptr item; {
  I32 destreg;

 /* case 1: we have call followed by delay inst */
  if (*(U32 *)address >> 30 == OP_CALL >> 30) {
    sparc64_set_call(address, (U32 *)address, item);
    return;
  }

  destreg = extract_reg_from_sethi(address);

 /* case 2: we have two-instr load-literal followed by jsr and delay inst */
  if (*((U32 *)address + 2) == JSR(destreg)) {
    sparc64_set_call(address, (U32 *)address + 2, item);
    return;
  }

 /* case 3: we have six-instr load-literal followed by jsr and a delay
    instruction we're willing to try to deal with */
  if (*((U32 *)address + 6) == JSR(destreg) &&
        (*((U32 *)address + 7) & ~IMMMASK == ADDI(CRETREG,0,SRETREG) ||
         *((U32 *)address + 7) == NOP)) {
    sparc64_set_call(address, (U32 *)address + 6, item);
    return;
  }

 /* case 4: we have a plain load-literal */
  sparc64_set_lit_only(address, item, destreg);
}
#endif /* SPARC64 */
