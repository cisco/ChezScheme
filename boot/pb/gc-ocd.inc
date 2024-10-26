static IGEN copy(thread_gc *tgc, ptr p, seginfo *si, ptr *dest)
{
  check_triggers(tgc, si);
  {
    ptr new_p;
    IGEN tg = TARGET_GENERATION(si);
    {
      ITYPE t = TYPEBITS(p);
      if (t == type_typed_object)
      {
        ptr tf = TYPEFIELD(p);
        if (TYPEP(tf, mask_record, type_record))
        {
          /* Do not inspect the type or first field of the rtd, because
             it may have been overwritten for forwarding. */
          {
            ptr rtd = RECORDINSTTYPE(p);
            ISPC p_spc = (((RECORDDESCPM(rtd)) == (FIX(-1)))
                          ? (((RECORDDESCMPM(rtd)) == (FIX(0)))
                             ? space_pure
                             : space_impure)
                          : (((RECORDDESCMPM(rtd)) == (FIX(0)))
                             ? space_pure_typed_object
                             : space_impure_record));
            {
              uptr len = UNFIX((RECORDDESCSIZE(rtd)));
              {
                uptr p_sz = size_record_inst(len);
                find_gc_room(tgc, p_spc, tg, type_typed_object, p_sz, new_p);
                RECORDINSTTYPE(new_p) = rtd;
                memcpy_aligned(&RECORDINSTIT(new_p, 0), &RECORDINSTIT(p, 0), len - ptr_bytes);
                if ((p_spc == space_pure) || ((p_spc == space_impure) || 0))
                {
                  {
                    uptr ua_size = unaligned_size_record_inst(len);
                    if (p_sz != ua_size)
                    {
                      *(((ptr*)(TO_VOIDP((((uptr)(UNTYPE(new_p, type_typed_object))) + ua_size))))) = FIX(0);
                    }
                  }
                }
              }
            }
          }
        }
        else if (TYPEP(tf, mask_vector, type_vector))
        {
          ISPC p_spc = ((((uptr)tf) & vector_immutable_flag)
                        ? space_pure
                        : space_impure);
          {
            uptr len = Svector_length(p);
            {
              uptr p_sz = size_vector(len);
              find_gc_room(tgc, p_spc, tg, type_typed_object, p_sz, new_p);
              VECTTYPE(new_p) = (uptr)tf;
              memcpy_aligned(&INITVECTIT(new_p, 0), &INITVECTIT(p, 0), ptr_bytes * len);
              if ((len & 1) == 0)
              {
                INITVECTIT(new_p, len) = FIX(0);
              }
            }
          }
        }
        else if (TYPEP(tf, mask_any_stencil_vector, type_any_stencil_vector))
        {
          ISPC p_spc = space_impure;
          {
            uptr len = Sstencil_vector_length(p);
            {
              uptr p_sz = size_stencil_vector(len);
              find_gc_room(tgc, p_spc, tg, type_typed_object, p_sz, new_p);
              STENVECTTYPE(new_p) = (uptr)tf;
              memcpy_aligned(&INITSTENVECTIT(new_p, 0), &INITSTENVECTIT(p, 0), ptr_bytes * len);
              if ((len & 1) == 0)
              {
                INITSTENVECTIT(new_p, len) = FIX(0);
              }
            }
          }
        }
        else if (TYPEP(tf, mask_string, type_string))
        {
          ISPC p_spc = space_data;
          {
            uptr sz = size_string((Sstring_length(p)));
            {
              uptr p_sz = sz;
              find_gc_room(tgc, p_spc, tg, type_typed_object, p_sz, new_p);
              memcpy_aligned(&STRTYPE(new_p), &STRTYPE(p), sz);
            }
          }
        }
        else if (TYPEP(tf, mask_fxvector, type_fxvector))
        {
          ISPC p_spc = space_data;
          {
            uptr sz = size_fxvector((Sfxvector_length(p)));
            {
              uptr p_sz = sz;
              find_gc_room(tgc, p_spc, tg, type_typed_object, p_sz, new_p);
              memcpy_aligned(&FXVECTOR_TYPE(new_p), &FXVECTOR_TYPE(p), sz);
            }
          }
        }
        else if (TYPEP(tf, mask_flvector, type_flvector))
        {
          ISPC p_spc = space_data;
          {
            uptr sz = size_flvector((Sflvector_length(p)));
            {
              uptr p_sz = sz;
              find_gc_room(tgc, p_spc, tg, type_typed_object, p_sz, new_p);
              memcpy_aligned(&FLVECTOR_TYPE(new_p), &FLVECTOR_TYPE(p), sz);
            }
          }
        }
        else if (TYPEP(tf, mask_bytevector, type_bytevector))
        {
          {
            ISPC p_at_spc = si->space;
            if (p_at_spc == space_reference_array)
            {
              ISPC p_spc = space_reference_array;
              {
                uptr sz = size_bytevector((Sbytevector_length(p)));
                {
                  uptr p_sz = sz;
                  find_gc_room(tgc, p_spc, tg, type_typed_object, p_sz, new_p);
                  BYTEVECTOR_TYPE(new_p) = (uptr)tf;
                  {
                    uptr len = Sbytevector_reference_length(p);
                    memcpy_aligned(&BVIT(new_p, 0), &BVIT(p, 0), ptr_bytes * len);
                    {
                      uptr extra = (Sbytevector_length(p)) - (len * ptr_bytes);
                      if (extra == 0)
                      {
                        if ((len & 1) == 0)
                        {
                          INITBVREFIT(new_p, len) = FIX(0);
                        }
                      }
                      else
                      {
                        INITBVREFIT(new_p, len) = INITBVREFIT(p, len);
                        {
                          uptr xlen = len + 1;
                          if ((xlen & 1) == 0)
                          {
                            INITBVREFIT(new_p, xlen) = FIX(0);
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
            else
            {
              ISPC p_spc = space_data;
              {
                uptr sz = size_bytevector((Sbytevector_length(p)));
                {
                  uptr p_sz = sz;
                  find_gc_room(tgc, p_spc, tg, type_typed_object, p_sz, new_p);
                  memcpy_aligned(&BYTEVECTOR_TYPE(new_p), &BYTEVECTOR_TYPE(p), sz);
                }
              }
            }
          }
        }
        else if ((iptr)tf == type_tlc)
        {
          ISPC p_spc = space_impure;
          {
            uptr p_sz = size_tlc;
            find_gc_room(tgc, p_spc, tg, type_typed_object, p_sz, new_p);
            TLCTYPE(new_p) = type_tlc;
            INITTLCHT(new_p) = INITTLCHT(p);
            {
              ptr next = INITTLCNEXT(p);
              {
                ptr keyval = INITTLCKEYVAL(p);
                INITTLCNEXT(new_p) = next;
                INITTLCKEYVAL(new_p) = keyval;
                if ((next != Sfalse) && (OLDSPACE(keyval)))
                {
                  GC_MUTEX_ACQUIRE();
                  tlcs_to_rehash = S_cons_in(tgc -> tc, space_new, 0, new_p, tlcs_to_rehash);
                  GC_MUTEX_RELEASE();
                }
              }
            }
          }
        }
        else if (TYPEP(tf, mask_box, type_box))
        {
          ISPC p_spc = (((BOXTYPE(p)) == type_immutable_box)
                        ? space_pure
                        : space_impure);
          {
            uptr p_sz = size_box;
            find_gc_room(tgc, p_spc, tg, type_typed_object, p_sz, new_p);
            BOXTYPE(new_p) = (uptr)tf;
            INITBOXREF(new_p) = INITBOXREF(p);
          }
        }
        else if ((iptr)tf == type_ratnum)
        {
          ISPC p_spc = space_data;
          {
            uptr p_sz = size_ratnum;
            find_gc_room(tgc, p_spc, tg, type_typed_object, p_sz, new_p);
            RATTYPE(new_p) = type_ratnum;
            {
              ptr tmp_p = RATNUM(p);
              relocate_pure(&tmp_p);
              RATNUM(new_p) = tmp_p;
            }
            {
              ptr tmp_p = RATDEN(p);
              relocate_pure(&tmp_p);
              RATDEN(new_p) = tmp_p;
            }
          }
        }
        else if ((iptr)tf == type_exactnum)
        {
          ISPC p_spc = space_data;
          {
            uptr p_sz = size_exactnum;
            find_gc_room(tgc, p_spc, tg, type_typed_object, p_sz, new_p);
            EXACTNUM_TYPE(new_p) = type_exactnum;
            {
              ptr tmp_p = EXACTNUM_REAL_PART(p);
              relocate_pure(&tmp_p);
              EXACTNUM_REAL_PART(new_p) = tmp_p;
            }
            {
              ptr tmp_p = EXACTNUM_IMAG_PART(p);
              relocate_pure(&tmp_p);
              EXACTNUM_IMAG_PART(new_p) = tmp_p;
            }
          }
        }
        else if ((iptr)tf == type_inexactnum)
        {
          ISPC p_spc = space_data;
          {
            uptr p_sz = size_inexactnum;
            find_gc_room(tgc, p_spc, tg, type_typed_object, p_sz, new_p);
            INEXACTNUM_TYPE(new_p) = type_inexactnum;
            {
              ptr tmp_p = TYPE(TO_PTR(&INEXACTNUM_REAL_PART(p)), type_flonum);
              if (flonum_is_forwarded_p(tmp_p, si))
                INEXACTNUM_REAL_PART(new_p) = FLODAT(FLONUM_FWDADDRESS(tmp_p));
              else
                INEXACTNUM_REAL_PART(new_p) = INEXACTNUM_REAL_PART(p);
            }
            {
              ptr tmp_p = TYPE(TO_PTR(&INEXACTNUM_IMAG_PART(p)), type_flonum);
              if (flonum_is_forwarded_p(tmp_p, si))
                INEXACTNUM_IMAG_PART(new_p) = FLODAT(FLONUM_FWDADDRESS(tmp_p));
              else
                INEXACTNUM_IMAG_PART(new_p) = INEXACTNUM_IMAG_PART(p);
            }
          }
        }
        else if (TYPEP(tf, mask_bignum, type_bignum))
        {
          ISPC p_spc = space_data;
          {
            uptr sz = size_bignum((BIGLEN(p)));
            {
              uptr p_sz = sz;
              find_gc_room(tgc, p_spc, tg, type_typed_object, p_sz, new_p);
              memcpy_aligned(&BIGTYPE(new_p), &BIGTYPE(p), sz);
            }
          }
        }
        else if (TYPEP(tf, mask_port, type_port))
        {
          ISPC p_spc = space_port;
          {
            uptr p_sz = size_port;
            find_gc_room(tgc, p_spc, tg, type_typed_object, p_sz, new_p);
            PORTTYPE(new_p) = (uptr)tf;
            PORTHANDLER(new_p) = PORTHANDLER(p);
            PORTOCNT(new_p) = PORTOCNT(p);
            PORTICNT(new_p) = PORTICNT(p);
            PORTOLAST(new_p) = PORTOLAST(p);
            PORTOBUF(new_p) = PORTOBUF(p);
            PORTILAST(new_p) = PORTILAST(p);
            PORTIBUF(new_p) = PORTIBUF(p);
            PORTINFO(new_p) = PORTINFO(p);
            PORTNAME(new_p) = PORTNAME(p);
          }
        }
        else if (TYPEP(tf, mask_code, type_code))
        {
          ISPC p_spc = space_code;
          {
            uptr len = CODELEN(p);
            {
              uptr p_sz = size_code(len);
              find_gc_room(tgc, p_spc, tg, type_typed_object, p_sz, new_p);
              CODETYPE(new_p) = (uptr)tf;
              CODELEN(new_p) = CODELEN(p);
              CODERELOC(new_p) = CODERELOC(p);
              CODENAME(new_p) = CODENAME(p);
              CODEARITYMASK(new_p) = CODEARITYMASK(p);
              CODEFREE(new_p) = CODEFREE(p);
              CODEINFO(new_p) = CODEINFO(p);
              CODEPINFOS(new_p) = CODEPINFOS(p);
              memcpy_aligned(&CODEIT(new_p, 0), &CODEIT(p, 0), len);
            }
          }
        }
        else if ((iptr)tf == type_thread)
        {
          ISPC p_spc = space_pure_typed_object;
          {
            uptr p_sz = size_thread;
            find_gc_room(tgc, p_spc, tg, type_typed_object, p_sz, new_p);
            THREADTYPE(new_p) = type_thread;
            THREADTC(new_p) = THREADTC(p);
          }
        }
        else if ((iptr)tf == type_rtd_counts)
        {
          ISPC p_spc = space_data;
          {
            uptr p_sz = size_rtd_counts;
            find_gc_room(tgc, p_spc, tg, type_typed_object, p_sz, new_p);
            memcpy_aligned(&RTDCOUNTSTYPE(new_p), &RTDCOUNTSTYPE(p), size_rtd_counts);
          }
        }
        else if ((iptr)tf == type_phantom)
        {
          ISPC p_spc = space_data;
          {
            uptr p_sz = size_phantom;
            find_gc_room(tgc, p_spc, tg, type_typed_object, p_sz, new_p);
            PHANTOMTYPE(new_p) = type_phantom;
            PHANTOMLEN(new_p) = PHANTOMLEN(p);
            GC_MUTEX_ACQUIRE();
            (S_G.bytesof[tg])[countof_phantom] += PHANTOMLEN(p);
            GC_MUTEX_RELEASE();
          }
        }
        else
        {
          S_error_abort("copy: illegal typed object type");
        }
      }
      else if (t == type_pair)
      {
        {
          ISPC p_at_spc = si->space;
          if (p_at_spc < space_weakpair)
          {
            ISPC p_spc = space_impure;
            {
              ptr cdr_p = Scdr(p);
              if ((cdr_p != p) && (((TYPEBITS(cdr_p)) == type_pair) && (((ptr_get_segment(cdr_p)) == (ptr_get_segment(p))) && (((FWDMARKER(cdr_p)) != forward_marker) && (!(si -> marked_mask))))))
              {
                uptr p_sz = 2 * (size_pair);
                find_gc_room(tgc, p_spc, tg, type_pair, p_sz, new_p);
                {
                  ptr new_cdr_p = (ptr)(((uptr)new_p) + size_pair);
                  INITCAR(new_p) = INITCAR(p);
                  INITCDR(new_p) = new_cdr_p;
                  INITCAR(new_cdr_p) = INITCAR(cdr_p);
                  INITCDR(new_cdr_p) = INITCDR(cdr_p);
                  FWDMARKER(cdr_p) = forward_marker;
                  FWDADDRESS(cdr_p) = new_cdr_p;
                }
              }
              else
              {
                uptr p_sz = size_pair;
                find_gc_room(tgc, p_spc, tg, type_pair, p_sz, new_p);
                INITCAR(new_p) = INITCAR(p);
                INITCDR(new_p) = INITCDR(p);
              }
            }
          }
          else if (p_at_spc == space_ephemeron)
          {
            ISPC p_spc = space_ephemeron;
            {
              uptr p_sz = size_ephemeron;
              find_gc_room(tgc, p_spc, tg, type_pair, p_sz, new_p);
              INITCAR(new_p) = INITCAR(p);
              INITCDR(new_p) = INITCDR(p);
              INITEPHEMERONPREVREF(new_p) = 0;
              INITEPHEMERONNEXT(new_p) = 0;
            }
          }
          else if (p_at_spc == space_weakpair)
          {
            ISPC p_spc = space_weakpair;
            {
              ptr cdr_p = Scdr(p);
              if ((cdr_p != p) && (((TYPEBITS(cdr_p)) == type_pair) && (((ptr_get_segment(cdr_p)) == (ptr_get_segment(p))) && (((FWDMARKER(cdr_p)) != forward_marker) && (!(si -> marked_mask))))))
              {
                uptr p_sz = 2 * (size_pair);
                find_gc_room(tgc, p_spc, tg, type_pair, p_sz, new_p);
                {
                  ptr new_cdr_p = (ptr)(((uptr)new_p) + size_pair);
                  INITCAR(new_p) = INITCAR(p);
                  INITCDR(new_p) = new_cdr_p;
                  INITCAR(new_cdr_p) = INITCAR(cdr_p);
                  INITCDR(new_cdr_p) = INITCDR(cdr_p);
                  FWDMARKER(cdr_p) = forward_marker;
                  FWDADDRESS(cdr_p) = new_cdr_p;
                }
              }
              else
              {
                uptr p_sz = size_pair;
                find_gc_room(tgc, p_spc, tg, type_pair, p_sz, new_p);
                INITCAR(new_p) = INITCAR(p);
                INITCDR(new_p) = INITCDR(p);
              }
            }
          }
          else
          {
            S_error_abort("misplaced pair");
          }
        }
      }
      else if (t == type_closure)
      {
        ptr code = CLOSCODE(p);
        relocate_pure(&code);
        if ((CODETYPE(code)) & (code_flag_continuation << code_flags_offset))
        {
          ISPC p_spc = space_continuation;
          {
            uptr p_sz = size_continuation;
            find_gc_room(tgc, p_spc, tg, type_closure, p_sz, new_p);
            SETCLOSCODE(new_p, code);
            if ((CONTLENGTH(p)) == opportunistic_1_shot_flag)
            {
              CONTLENGTH(new_p) = CONTCLENGTH(p);
              GC_MUTEX_ACQUIRE();
              conts_to_promote = S_cons_in(tgc -> tc, space_new, 0, new_p, conts_to_promote);
              GC_MUTEX_RELEASE();
            }
            else
            {
              CONTLENGTH(new_p) = CONTLENGTH(p);
            }
            CONTCLENGTH(new_p) = CONTCLENGTH(p);
            CONTWINDERS(new_p) = CONTWINDERS(p);
            CONTATTACHMENTS(new_p) = CONTATTACHMENTS(p);
            if ((CONTLENGTH(p)) == scaled_shot_1_shot_flag)
            {
            }
            else
            {
              CONTLINK(new_p) = CONTLINK(p);
              CONTRET(new_p) = CONTRET(p);
              CONTSTACK(new_p) = CONTSTACK(p);
            }
          }
        }
        else
        {
          ISPC p_spc = (((CODETYPE(code)) & (code_flag_mutable_closure << code_flags_offset))
                        ? space_impure
                        : space_pure);
          {
            uptr len = CODEFREE(code);
            {
              uptr p_sz = size_closure(len);
              find_gc_room(tgc, p_spc, tg, type_closure, p_sz, new_p);
              SETCLOSCODE(new_p, code);
              memcpy_aligned(&CLOSIT(new_p, 0), &CLOSIT(p, 0), ptr_bytes * len);
              if ((len & 1) == 0)
              {
                CLOSIT(new_p, len) = FIX(0);
              }
            }
          }
        }
      }
      else if (t == type_symbol)
      {
        ISPC p_spc = space_symbol;
        {
          uptr p_sz = size_symbol;
          find_gc_room(tgc, p_spc, tg, type_symbol, p_sz, new_p);
          INITSYMVAL(new_p) = INITSYMVAL(p);
          INITSYMPVAL(new_p) = INITSYMPVAL(p);
          INITSYMPLIST(new_p) = INITSYMPLIST(p);
          INITSYMNAME(new_p) = INITSYMNAME(p);
          INITSYMSPLIST(new_p) = INITSYMSPLIST(p);
          INITSYMHASH(new_p) = INITSYMHASH(p);
        }
      }
      else if (t == type_flonum)
      {
        ISPC p_spc = space_data;
        {
          uptr p_sz = size_flonum;
          find_gc_room(tgc, p_spc, tg, type_flonum, p_sz, new_p);
          FLODAT(new_p) = FLODAT(p);
          flonum_set_forwarded(tgc, p, si);
          FLONUM_FWDADDRESS(p) = new_p;
          *dest = new_p;
          tgc->sweep_change = SWEEP_CHANGE_PROGRESS;
          return tg;
        }
      }
      else
      {
        S_error_abort("copy: illegal type");
      }
    }
    tgc->sweep_change = SWEEP_CHANGE_PROGRESS;
    FWDADDRESS(p) = new_p;
    FWDMARKER(p) = forward_marker;
    *dest = new_p;
    return tg;
  }
}

static void sweep(thread_gc *tgc, ptr p, IGEN from_g)
{
  FLUSH_REMOTE_BLOCK
  {
    ITYPE t = TYPEBITS(p);
    if (t == type_typed_object)
    {
      ptr tf = TYPEFIELD(p);
      if (TYPEP(tf, mask_record, type_record))
      {
        relocate_pure(&RECORDINSTTYPE(p));
        {
          ptr rtd = RECORDINSTTYPE(p);
          {
            uptr len = UNFIX((RECORDDESCSIZE(rtd)));
            {
              ptr num = RECORDDESCPM(rtd);
              ptr* pp = &(RECORDINSTIT(p, 0));
              if (Sfixnump(num))
              {
                {
                  uptr mask = ((uptr)(UNFIX(num))) >> 1;
                  if (mask == (((uptr)-1) >> 1))
                  {
                    {
                      ptr* ppend = ((ptr*)(TO_VOIDP((((uptr)(TO_PTR(pp))) + len)))) - 1;
                      while (pp < ppend)
                      {
                        relocate_impure(&(*(pp)), from_g);
                        pp += 1;
                      }
                    }
                  }
                  else
                  {
                    while (mask != 0)
                    {
                      if (mask & 1)
                      {
                        relocate_impure(&(*(pp)), from_g);
                      }
                      mask >>= 1;
                      pp += 1;
                    }
                  }
                }
              }
              else
              {
                relocate_pure(&(RECORDDESCPM(rtd)));
                num = RECORDDESCPM(rtd);
                {
                  iptr index = (BIGLEN(num)) - 1;
                  bigit mask = (BIGIT(num, index)) >> 1;
                  INT bits = bigit_bits - 1;
                  while (1)
                  {
                    do
                    {
                      if (mask & 1)
                      {
                        relocate_impure(&(*(pp)), from_g);
                      }
                      mask >>= 1;
                      pp += 1;
                      bits -= 1;
                    }
                    while (bits > 0);
                    if (index == 0)
                    {
                      break;
                    }
                    index -= 1;
                    mask = BIGIT(num, index);
                    bits = bigit_bits;
                  }
                }
              }
            }
          }
        }
      }
      else if (TYPEP(tf, mask_vector, type_vector))
      {
        uptr len = Svector_length(p);
        {
          uptr idx, p_len = len;
          ptr *p_p = &INITVECTIT(p, 0);
          for (idx = 0; idx < p_len; idx++)
          {
            relocate_impure(&(p_p[idx]), from_g);
          }
        }
      }
      else if (TYPEP(tf, mask_any_stencil_vector, type_any_stencil_vector))
      {
        uptr len = Sstencil_vector_length(p);
        {
          uptr idx, p_len = len;
          ptr *p_p = &INITSTENVECTIT(p, 0);
          for (idx = 0; idx < p_len; idx++)
          {
            relocate_impure(&(p_p[idx]), from_g);
          }
        }
      }
      else if (TYPEP(tf, mask_string, type_string))
      {
      }
      else if (TYPEP(tf, mask_fxvector, type_fxvector))
      {
      }
      else if (TYPEP(tf, mask_flvector, type_flvector))
      {
      }
      else if (TYPEP(tf, mask_bytevector, type_bytevector))
      {
        {
          ISPC p_at_spc = SPACE(p);
          if (p_at_spc == space_reference_array)
          {
            {
              uptr len = Sbytevector_reference_length(p);
              {
                uptr idx, p_len = len;
                ptr *p_p = (ptr*)&BVIT(p, 0);
                for (idx = 0; idx < p_len; idx++)
                {
                  relocate_reference(&(p_p[idx]), from_g);
                }
              }
            }
          }
          else
          {
          }
        }
      }
      else if ((iptr)tf == type_tlc)
      {
        relocate_impure(&INITTLCHT(p), from_g);
        relocate_impure(&INITTLCKEYVAL(p), from_g);
        relocate_impure(&INITTLCNEXT(p), from_g);
      }
      else if (TYPEP(tf, mask_box, type_box))
      {
        relocate_impure(&INITBOXREF(p), from_g);
      }
      else if ((iptr)tf == type_ratnum)
      {
        relocate_pure(&RATNUM(p));
        relocate_pure(&RATDEN(p));
      }
      else if ((iptr)tf == type_exactnum)
      {
        relocate_pure(&EXACTNUM_REAL_PART(p));
        relocate_pure(&EXACTNUM_IMAG_PART(p));
      }
      else if ((iptr)tf == type_inexactnum)
      {
      }
      else if (TYPEP(tf, mask_bignum, type_bignum))
      {
      }
      else if (TYPEP(tf, mask_port, type_port))
      {
        relocate_impure(&PORTHANDLER(p), from_g);
        if (((uptr)tf) & PORT_FLAG_OUTPUT)
        {
          iptr n = ((iptr)(PORTOLAST(p))) - ((iptr)(PORTOBUF(p)));
          relocate_impure(&PORTOBUF(p), from_g);
          PORTOLAST(p) = (ptr)(((iptr)(PORTOBUF(p))) + n);
        }
        if (((uptr)tf) & PORT_FLAG_INPUT)
        {
          iptr n = ((iptr)(PORTILAST(p))) - ((iptr)(PORTIBUF(p)));
          relocate_impure(&PORTIBUF(p), from_g);
          PORTILAST(p) = (ptr)(((iptr)(PORTIBUF(p))) + n);
        }
        relocate_impure(&PORTINFO(p), from_g);
        relocate_impure(&PORTNAME(p), from_g);
      }
      else if (TYPEP(tf, mask_code, type_code))
      {
        relocate_pure(&CODENAME(p));
        relocate_pure(&CODEARITYMASK(p));
        relocate_pure(&CODEINFO(p));
        relocate_pure(&CODEPINFOS(p));
        {
          ptr t = CODERELOC(p);
          {
            iptr m = (t
                      ? (RELOCSIZE(t))
                      : 0);
            {
              ptr oldco = (t
                           ? (RELOCCODE(t))
                           : 0);
              {
                iptr a = 0;
                {
                  iptr n = 0;
                  while (n < m)
                  {
                    {
                      uptr entry = RELOCIT(t, n);
                      uptr item_off = 0;
                      uptr code_off = 0;
                      n = n + 1;
                      if (RELOC_EXTENDED_FORMAT(entry))
                      {
                        item_off = RELOCIT(t, n);
                        n = n + 1;
                        code_off = RELOCIT(t, n);
                        n = n + 1;
                      }
                      else
                      {
                        item_off = RELOC_ITEM_OFFSET(entry);
                        code_off = RELOC_CODE_OFFSET(entry);
                      }
                      a = a + code_off;
                      {
                        ptr obj = S_get_code_obj(RELOC_TYPE(entry), oldco, a, item_off);
                        relocate_pure(&obj);
                        S_set_code_obj("gc", RELOC_TYPE(entry), p, a, obj, item_off);
                      }
                    }
                  }
                  if ((from_g == static_generation) && ((!S_G.retain_static_relocation) && (0 == ((CODETYPE(p)) & (code_flag_template << code_flags_offset)))))
                  {
                    CODERELOC(p) = (ptr)0;
                  }
                  else
                  {
                    {
                      seginfo* t_si = SegInfo((ptr_get_segment(t)));
                      if (t_si -> old_space)
                      {
                        if (SEGMENT_IS_LOCAL(t_si, t))
                        {
                          n = size_reloc_table((RELOCSIZE(t)));
                          if (t_si -> use_marks)
                          {
                            if (!(marked(t_si, t)))
                            {
                              mark_untyped_data_object(tgc, t, n, t_si);
                            }
                          }
                          else
                          {
                            {
                              ptr oldt = t;
                              find_gc_room(tgc, space_data, from_g, type_untyped, n, t);
                              memcpy_aligned(TO_VOIDP(t), TO_VOIDP(oldt), n);
                            }
                          }
                        }
                        else
                        {
                          RECORD_REMOTE(t_si);
                        }
                      }
                    }
                    RELOCCODE(t) = p;
                    CODERELOC(p) = t;
                  }
                  S_record_code_mod(tgc -> tc, (uptr)(TO_PTR((&(CODEIT(p, 0))))), (uptr)(CODELEN(p)));
                }
              }
            }
          }
        }
      }
      else if ((iptr)tf == type_thread)
      {
        {
          ptr tc = (ptr)(THREADTC(p));
          if (tc != ((ptr)0))
          {
            {
              ptr old_stack = SCHEMESTACK(tc);
              if (OLDSPACE(old_stack))
              {
                {
                  iptr clength = ((uptr)(SFP(tc))) - ((uptr)old_stack);
                  SCHEMESTACK(tc) = copy_stack(tgc, old_stack, &(SCHEMESTACKSIZE(tc)), clength + (sizeof(ptr)));
                  SFP(tc) = (ptr)(((uptr)(SCHEMESTACK(tc))) + clength);
                  ESP(tc) = (ptr)((((uptr)(SCHEMESTACK(tc))) + (SCHEMESTACKSIZE(tc))) - stack_slop);
                }
              }
            }
            STACKCACHE(tc) = Snil;
            relocate_pure(&(CCHAIN(tc)));
            relocate_pure(&(STACKLINK(tc)));
            relocate_pure(&(WINDERS(tc)));
            relocate_pure(&(ATTACHMENTS(tc)));
            relocate_pure(&(HANDLERSTACK(tc)));
            CACHEDFRAME(tc) = Sfalse;
            {
              ptr xcp = FRAME(tc, 0);
              {
                iptr co = (ENTRYOFFSET(xcp)) + (((uptr)xcp) - ((uptr)(TO_PTR((ENTRYOFFSETADDR(xcp))))));
                {
                  ptr c_p = (ptr)(((uptr)xcp) - co);
                  {
                    seginfo* x_si = SegInfo((ptr_get_segment(c_p)));
                    if (x_si -> old_space)
                    {
                      relocate_code(c_p, x_si);
                      FRAME(tc, 0) = (ptr)(((uptr)c_p) + co);
                    }
                    {
                      uptr base = (uptr)(SCHEMESTACK(tc));
                      {
                        uptr fp = (uptr)(SFP(tc));
                        {
                          uptr ret = (uptr)(FRAME(tc, 0));
                          while (fp != base)
                          {
                            if (fp < base)
                            {
                              S_error_abort("sweep_stack(gc): malformed stack");
                            }
                            fp = fp - (ENTRYFRAMESIZE(ret));
                            {
                              iptr oldret = ret;
                              ptr num = ENTRYLIVEMASK(oldret);
                              ptr* pp = (ptr*)(TO_VOIDP(fp));
                              ret = (iptr)(*(pp));
                              {
                                ptr xcp = *(pp);
                                {
                                  iptr co = (ENTRYOFFSET(xcp)) + (((uptr)xcp) - ((uptr)(TO_PTR((ENTRYOFFSETADDR(xcp))))));
                                  {
                                    ptr c_p = (ptr)(((uptr)xcp) - co);
                                    {
                                      seginfo* x_si = SegInfo((ptr_get_segment(c_p)));
                                      if (x_si -> old_space)
                                      {
                                        relocate_code(c_p, x_si);
                                        *(pp) = (ptr)(((uptr)c_p) + co);
                                      }
                                      if (Sfixnump(num))
                                      {
                                        {
                                          uptr mask = UNFIX(num);
                                          while (mask != 0)
                                          {
                                            pp += 1;
                                            if (mask & 1)
                                            {
                                              relocate_pure(&(*(pp)));
                                            }
                                            mask >>= 1;
                                          }
                                        }
                                      }
                                      else
                                      {
                                        seginfo* n_si = SegInfo((ptr_get_segment(num)));
                                        if (!(n_si -> old_space))
                                        {
                                        }
                                        else if (SEGMENT_IS_LOCAL(n_si, num))
                                        {
                                          relocate_pure(&(*((ENTRYNONCOMPACTLIVEMASKADDR(oldret)))));
                                          num = ENTRYLIVEMASK(oldret);
                                        }
                                        else
                                        {
                                          RECORD_REMOTE(n_si);
                                          num = S_G.zero_length_bignum;
                                        }
                                        {
                                          iptr index = BIGLEN(num);
                                          while (index != 0)
                                          {
                                            index -= 1;
                                            {
                                              INT bits = bigit_bits;
                                              bigit mask = BIGIT(num, index);
                                              while (bits > 0)
                                              {
                                                bits -= 1;
                                                pp += 1;
                                                if (mask & 1)
                                                {
                                                  relocate_pure(&(*(pp)));
                                                }
                                                mask >>= 1;
                                              }
                                            }
                                          }
                                        }
                                      }
                                    }
                                  }
                                }
                              }
                            }
                          }
                          PAR_SWEEPERS_WORKAROUND();
                          U(tc) = 0;
                          V(tc) = 0;
                          W(tc) = 0;
                          X(tc) = 0;
                          Y(tc) = 0;
                          relocate_pure(&(THREADNO(tc)));
                          relocate_pure(&(CURRENTINPUT(tc)));
                          relocate_pure(&(CURRENTOUTPUT(tc)));
                          relocate_pure(&(CURRENTERROR(tc)));
                          relocate_pure(&(SFD(tc)));
                          relocate_pure(&(CURRENTMSO(tc)));
                          relocate_pure(&(TARGETMACHINE(tc)));
                          relocate_pure(&(FXLENGTHBV(tc)));
                          relocate_pure(&(FXFIRSTBITSETBV(tc)));
                          relocate_pure(&(COMPILEPROFILE(tc)));
                          relocate_pure(&(SUBSETMODE(tc)));
                          relocate_pure(&(DEFAULTRECORDEQUALPROCEDURE(tc)));
                          relocate_pure(&(DEFAULTRECORDHASHPROCEDURE(tc)));
                          relocate_pure(&(COMPRESSFORMAT(tc)));
                          relocate_pure(&(COMPRESSLEVEL(tc)));
                          relocate_pure(&(PARAMETERS(tc)));
                          DSTBV(tc) = Sfalse;
                          SRCBV(tc) = Sfalse;
                          {
                            INT i = 0;
                            while (i < virtual_register_count)
                            {
                              relocate_pure(&(VIRTREG(tc, i)));
                              i += 1;
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
      else if ((iptr)tf == type_rtd_counts)
      {
      }
      else if ((iptr)tf == type_phantom)
      {
      }
      else
      {
        S_error_abort("sweep: illegal typed object type");
      }
    }
    else if (t == type_pair)
    {
      {
        ISPC p_at_spc = SPACE(p);
        if (p_at_spc < space_weakpair)
        {
          relocate_impure(&INITCAR(p), from_g);
          relocate_impure(&INITCDR(p), from_g);
        }
        else if (p_at_spc == space_ephemeron)
        {
          add_ephemeron_to_pending(tgc, p);
        }
        else if (p_at_spc == space_weakpair)
        {
          relocate_impure(&INITCDR(p), from_g);
        }
        else
        {
          relocate_reference(&INITCAR(p), from_g);
          relocate_reference(&INITCDR(p), from_g);
        }
      }
    }
    else if (t == type_closure)
    {
      ptr code = CLOSCODE(p);
      relocate_pure(&code);
      if ((CODETYPE(code)) & (code_flag_continuation << code_flags_offset))
      {
        SETCLOSCODE(p, code);
        relocate_pure(&CONTWINDERS(p));
        relocate_impure(&CONTATTACHMENTS(p), from_g);
        if ((CONTLENGTH(p)) == scaled_shot_1_shot_flag)
        {
        }
        else
        {
          ptr stk = CONTSTACK(p);
          {
            seginfo* s_si = NULL;
            if ((stk != ((ptr)0)) && ((s_si = (SegInfo((ptr_get_segment(stk))))), (s_si -> old_space)))
            {
              if (!(SEGMENT_IS_LOCAL(s_si, stk)))
              {
                RECORD_REMOTE(s_si);
              }
              else
              {
                CONTSTACK(p) = copy_stack(tgc, CONTSTACK(p), &(CONTLENGTH(p)), CONTCLENGTH(p));
              }
            }
            relocate_pure(&CONTLINK(p));
            {
              ptr xcp = CONTRET(p);
              {
                iptr co = (ENTRYOFFSET(xcp)) + (((uptr)xcp) - ((uptr)(TO_PTR((ENTRYOFFSETADDR(xcp))))));
                {
                  ptr c_p = (ptr)(((uptr)xcp) - co);
                  {
                    seginfo* x_si = SegInfo((ptr_get_segment(c_p)));
                    if (x_si -> old_space)
                    {
                      relocate_code(c_p, x_si);
                      CONTRET(p) = (ptr)(((uptr)c_p) + co);
                    }
                    {
                      uptr stack = (uptr)(CONTSTACK(p));
                      {
                        uptr base = stack;
                        {
                          uptr fp = stack + (CONTCLENGTH(p));
                          {
                            uptr ret = (uptr)(CONTRET(p));
                            while (fp != base)
                            {
                              if (fp < base)
                              {
                                S_error_abort("sweep_stack(gc): malformed stack");
                              }
                              fp = fp - (ENTRYFRAMESIZE(ret));
                              {
                                iptr oldret = ret;
                                ptr num = ENTRYLIVEMASK(oldret);
                                ptr* pp = (ptr*)(TO_VOIDP(fp));
                                ret = (iptr)(*(pp));
                                {
                                  ptr xcp = *(pp);
                                  {
                                    iptr co = (ENTRYOFFSET(xcp)) + (((uptr)xcp) - ((uptr)(TO_PTR((ENTRYOFFSETADDR(xcp))))));
                                    {
                                      ptr c_p = (ptr)(((uptr)xcp) - co);
                                      {
                                        seginfo* x_si = SegInfo((ptr_get_segment(c_p)));
                                        if (x_si -> old_space)
                                        {
                                          relocate_code(c_p, x_si);
                                          *(pp) = (ptr)(((uptr)c_p) + co);
                                        }
                                        if (Sfixnump(num))
                                        {
                                          {
                                            uptr mask = UNFIX(num);
                                            while (mask != 0)
                                            {
                                              pp += 1;
                                              if (mask & 1)
                                              {
                                                relocate_pure(&(*(pp)));
                                              }
                                              mask >>= 1;
                                            }
                                          }
                                        }
                                        else
                                        {
                                          seginfo* n_si = SegInfo((ptr_get_segment(num)));
                                          if (!(n_si -> old_space))
                                          {
                                          }
                                          else if (SEGMENT_IS_LOCAL(n_si, num))
                                          {
                                            relocate_pure(&(*((ENTRYNONCOMPACTLIVEMASKADDR(oldret)))));
                                            num = ENTRYLIVEMASK(oldret);
                                          }
                                          else
                                          {
                                            RECORD_REMOTE(n_si);
                                            num = S_G.zero_length_bignum;
                                          }
                                          {
                                            iptr index = BIGLEN(num);
                                            while (index != 0)
                                            {
                                              index -= 1;
                                              {
                                                INT bits = bigit_bits;
                                                bigit mask = BIGIT(num, index);
                                                while (bits > 0)
                                                {
                                                  bits -= 1;
                                                  pp += 1;
                                                  if (mask & 1)
                                                  {
                                                    relocate_pure(&(*(pp)));
                                                  }
                                                  mask >>= 1;
                                                }
                                              }
                                            }
                                          }
                                        }
                                      }
                                    }
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
      else
      {
        uptr len = CODEFREE(code);
        if ((CODETYPE(code)) & (code_flag_mutable_closure << code_flags_offset))
        {
          SETCLOSCODE(p, code);
          {
            uptr idx, p_len = len;
            ptr *p_p = &CLOSIT(p, 0);
            for (idx = 0; idx < p_len; idx++)
            {
              relocate_impure(&(p_p[idx]), from_g);
            }
          }
        }
        else
        {
          SETCLOSCODE(p, code);
          {
            uptr idx, p_len = len;
            ptr *p_p = &CLOSIT(p, 0);
            for (idx = 0; idx < p_len; idx++)
            {
              relocate_pure(&(p_p[idx]));
            }
          }
        }
      }
    }
    else if (t == type_symbol)
    {
      relocate_impure(&INITSYMVAL(p), from_g);
      {
        ptr val = INITSYMVAL(p);
        {
          ptr code = ((Sprocedurep(val))
                      ? (CLOSCODE(val))
                      : (SYMCODE(p)));
          relocate_pure(&code);
          INITSYMCODE(p, code);
          relocate_impure(&INITSYMPLIST(p), from_g);
          relocate_impure(&INITSYMNAME(p), from_g);
          relocate_impure(&INITSYMSPLIST(p), from_g);
          relocate_impure(&INITSYMHASH(p), from_g);
        }
      }
    }
    else if (t == type_flonum)
    {
    }
    else
    {
      S_error_abort("sweep: illegal type");
    }
  }
  FLUSH_REMOTE(tgc, p);
}

static void sweep_object_in_old(thread_gc *tgc, ptr p)
{
  FLUSH_REMOTE_BLOCK
  {
    ITYPE t = TYPEBITS(p);
    if (t == type_typed_object)
    {
      ptr tf = TYPEFIELD(p);
      if (TYPEP(tf, mask_record, type_record))
      {
        relocate_pure(&RECORDINSTTYPE(p));
        {
          ptr rtd = RECORDINSTTYPE(p);
          {
            uptr len = UNFIX((RECORDDESCSIZE(rtd)));
            {
              ptr num = RECORDDESCPM(rtd);
              ptr* pp = &(RECORDINSTIT(p, 0));
              if (Sfixnump(num))
              {
                {
                  uptr mask = ((uptr)(UNFIX(num))) >> 1;
                  if (mask == (((uptr)-1) >> 1))
                  {
                    {
                      ptr* ppend = ((ptr*)(TO_VOIDP((((uptr)(TO_PTR(pp))) + len)))) - 1;
                      while (pp < ppend)
                      {
                        relocate_indirect((*(pp)));
                        pp += 1;
                      }
                    }
                  }
                  else
                  {
                    while (mask != 0)
                    {
                      if (mask & 1)
                      {
                        relocate_indirect((*(pp)));
                      }
                      mask >>= 1;
                      pp += 1;
                    }
                  }
                }
              }
              else
              {
                relocate_pure(&(RECORDDESCPM(rtd)));
                num = RECORDDESCPM(rtd);
                {
                  iptr index = (BIGLEN(num)) - 1;
                  bigit mask = (BIGIT(num, index)) >> 1;
                  INT bits = bigit_bits - 1;
                  while (1)
                  {
                    do
                    {
                      if (mask & 1)
                      {
                        relocate_indirect((*(pp)));
                      }
                      mask >>= 1;
                      pp += 1;
                      bits -= 1;
                    }
                    while (bits > 0);
                    if (index == 0)
                    {
                      break;
                    }
                    index -= 1;
                    mask = BIGIT(num, index);
                    bits = bigit_bits;
                  }
                }
              }
            }
          }
        }
      }
      else if (TYPEP(tf, mask_vector, type_vector))
      {
        uptr len = Svector_length(p);
        {
          uptr idx, p_len = len;
          ptr *p_p = &INITVECTIT(p, 0);
          for (idx = 0; idx < p_len; idx++)
          {
            relocate_indirect((p_p[idx]));
          }
        }
      }
      else if (TYPEP(tf, mask_any_stencil_vector, type_any_stencil_vector))
      {
        uptr len = Sstencil_vector_length(p);
        {
          uptr idx, p_len = len;
          ptr *p_p = &INITSTENVECTIT(p, 0);
          for (idx = 0; idx < p_len; idx++)
          {
            relocate_indirect((p_p[idx]));
          }
        }
      }
      else if (TYPEP(tf, mask_string, type_string))
      {
      }
      else if (TYPEP(tf, mask_fxvector, type_fxvector))
      {
      }
      else if (TYPEP(tf, mask_flvector, type_flvector))
      {
      }
      else if (TYPEP(tf, mask_bytevector, type_bytevector))
      {
        {
          ISPC p_at_spc = SPACE(p);
          if (p_at_spc == space_reference_array)
          {
            {
              uptr len = Sbytevector_reference_length(p);
              {
                uptr idx, p_len = len;
                ptr *p_p = (ptr*)&BVIT(p, 0);
                for (idx = 0; idx < p_len; idx++)
                {
                  relocate_reference_indirect((p_p[idx]));
                }
              }
            }
          }
          else
          {
          }
        }
      }
      else if ((iptr)tf == type_tlc)
      {
        relocate_indirect(INITTLCHT(p));
        relocate_indirect(INITTLCKEYVAL(p));
        relocate_indirect(INITTLCNEXT(p));
      }
      else if (TYPEP(tf, mask_box, type_box))
      {
        relocate_indirect(INITBOXREF(p));
      }
      else if ((iptr)tf == type_ratnum)
      {
        relocate_pure(&RATNUM(p));
        relocate_pure(&RATDEN(p));
      }
      else if ((iptr)tf == type_exactnum)
      {
        relocate_pure(&EXACTNUM_REAL_PART(p));
        relocate_pure(&EXACTNUM_IMAG_PART(p));
      }
      else if ((iptr)tf == type_inexactnum)
      {
      }
      else if (TYPEP(tf, mask_bignum, type_bignum))
      {
      }
      else if (TYPEP(tf, mask_port, type_port))
      {
        relocate_indirect(PORTHANDLER(p));
        if (((uptr)tf) & PORT_FLAG_OUTPUT)
        {
          relocate_indirect(PORTOBUF(p));
        }
        if (((uptr)tf) & PORT_FLAG_INPUT)
        {
          relocate_indirect(PORTIBUF(p));
        }
        relocate_indirect(PORTINFO(p));
        relocate_indirect(PORTNAME(p));
      }
      else if (TYPEP(tf, mask_code, type_code))
      {
        relocate_pure(&CODENAME(p));
        relocate_pure(&CODEARITYMASK(p));
        relocate_pure(&CODEINFO(p));
        relocate_pure(&CODEPINFOS(p));
        {
          ptr t = CODERELOC(p);
          {
            iptr m = (t
                      ? (RELOCSIZE(t))
                      : 0);
            {
              ptr oldco = (t
                           ? (RELOCCODE(t))
                           : 0);
              {
                iptr a = 0;
                {
                  iptr n = 0;
                  while (n < m)
                  {
                    {
                      uptr entry = RELOCIT(t, n);
                      uptr item_off = 0;
                      uptr code_off = 0;
                      n = n + 1;
                      if (RELOC_EXTENDED_FORMAT(entry))
                      {
                        item_off = RELOCIT(t, n);
                        n = n + 1;
                        code_off = RELOCIT(t, n);
                        n = n + 1;
                      }
                      else
                      {
                        item_off = RELOC_ITEM_OFFSET(entry);
                        code_off = RELOC_CODE_OFFSET(entry);
                      }
                      a = a + code_off;
                      {
                        ptr obj = S_get_code_obj(RELOC_TYPE(entry), oldco, a, item_off);
                        relocate_pure(&obj);
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
      else if ((iptr)tf == type_thread)
      {
        {
          ptr tc = (ptr)(THREADTC(p));
          if (tc != ((ptr)0))
          {
            STACKCACHE(tc) = Snil;
            relocate_pure(&(CCHAIN(tc)));
            relocate_pure(&(STACKLINK(tc)));
            relocate_pure(&(WINDERS(tc)));
            relocate_pure(&(ATTACHMENTS(tc)));
            relocate_pure(&(HANDLERSTACK(tc)));
            {
              ptr xcp = FRAME(tc, 0);
              {
                iptr co = (ENTRYOFFSET(xcp)) + (((uptr)xcp) - ((uptr)(TO_PTR((ENTRYOFFSETADDR(xcp))))));
                {
                  ptr c_p = (ptr)(((uptr)xcp) - co);
                  {
                    seginfo* x_si = SegInfo((ptr_get_segment(c_p)));
                    if (x_si -> old_space)
                    {
                      relocate_code(c_p, x_si);
                    }
                    {
                      uptr base = (uptr)(SCHEMESTACK(tc));
                      {
                        uptr fp = (uptr)(SFP(tc));
                        {
                          uptr ret = (uptr)(FRAME(tc, 0));
                          while (fp != base)
                          {
                            if (fp < base)
                            {
                              S_error_abort("sweep_stack(gc): malformed stack");
                            }
                            fp = fp - (ENTRYFRAMESIZE(ret));
                            {
                              iptr oldret = ret;
                              ptr num = ENTRYLIVEMASK(oldret);
                              ptr* pp = (ptr*)(TO_VOIDP(fp));
                              ret = (iptr)(*(pp));
                              {
                                ptr xcp = *(pp);
                                {
                                  iptr co = (ENTRYOFFSET(xcp)) + (((uptr)xcp) - ((uptr)(TO_PTR((ENTRYOFFSETADDR(xcp))))));
                                  {
                                    ptr c_p = (ptr)(((uptr)xcp) - co);
                                    {
                                      seginfo* x_si = SegInfo((ptr_get_segment(c_p)));
                                      if (x_si -> old_space)
                                      {
                                        relocate_code(c_p, x_si);
                                      }
                                      if (Sfixnump(num))
                                      {
                                        {
                                          uptr mask = UNFIX(num);
                                          while (mask != 0)
                                          {
                                            pp += 1;
                                            if (mask & 1)
                                            {
                                              relocate_pure(&(*(pp)));
                                            }
                                            mask >>= 1;
                                          }
                                        }
                                      }
                                      else
                                      {
                                        seginfo* n_si = SegInfo((ptr_get_segment(num)));
                                        if (!(n_si -> old_space))
                                        {
                                        }
                                        else if (SEGMENT_IS_LOCAL(n_si, num))
                                        {
                                          relocate_pure(&(*((ENTRYNONCOMPACTLIVEMASKADDR(oldret)))));
                                          num = ENTRYLIVEMASK(oldret);
                                        }
                                        else
                                        {
                                          RECORD_REMOTE(n_si);
                                          num = S_G.zero_length_bignum;
                                        }
                                        {
                                          iptr index = BIGLEN(num);
                                          while (index != 0)
                                          {
                                            index -= 1;
                                            {
                                              INT bits = bigit_bits;
                                              bigit mask = BIGIT(num, index);
                                              while (bits > 0)
                                              {
                                                bits -= 1;
                                                pp += 1;
                                                if (mask & 1)
                                                {
                                                  relocate_pure(&(*(pp)));
                                                }
                                                mask >>= 1;
                                              }
                                            }
                                          }
                                        }
                                      }
                                    }
                                  }
                                }
                              }
                            }
                          }
                          relocate_pure(&(THREADNO(tc)));
                          relocate_pure(&(CURRENTINPUT(tc)));
                          relocate_pure(&(CURRENTOUTPUT(tc)));
                          relocate_pure(&(CURRENTERROR(tc)));
                          relocate_pure(&(SFD(tc)));
                          relocate_pure(&(CURRENTMSO(tc)));
                          relocate_pure(&(TARGETMACHINE(tc)));
                          relocate_pure(&(FXLENGTHBV(tc)));
                          relocate_pure(&(FXFIRSTBITSETBV(tc)));
                          relocate_pure(&(COMPILEPROFILE(tc)));
                          relocate_pure(&(SUBSETMODE(tc)));
                          relocate_pure(&(DEFAULTRECORDEQUALPROCEDURE(tc)));
                          relocate_pure(&(DEFAULTRECORDHASHPROCEDURE(tc)));
                          relocate_pure(&(COMPRESSFORMAT(tc)));
                          relocate_pure(&(COMPRESSLEVEL(tc)));
                          relocate_pure(&(PARAMETERS(tc)));
                          {
                            INT i = 0;
                            while (i < virtual_register_count)
                            {
                              relocate_pure(&(VIRTREG(tc, i)));
                              i += 1;
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
      else if ((iptr)tf == type_rtd_counts)
      {
      }
      else if ((iptr)tf == type_phantom)
      {
      }
      else
      {
        S_error_abort("sweep-in-old: illegal typed object type");
      }
    }
    else if (t == type_pair)
    {
      {
        ISPC p_at_spc = SPACE(p);
        if (p_at_spc < space_weakpair)
        {
          relocate_indirect(INITCAR(p));
          relocate_indirect(INITCDR(p));
        }
        else if (p_at_spc == space_ephemeron)
        {
        }
        else if (p_at_spc == space_weakpair)
        {
          relocate_indirect(INITCDR(p));
        }
        else
        {
          S_error_abort("misplaced pair");
        }
      }
    }
    else if (t == type_closure)
    {
      ptr code = CLOSCODE(p);
      relocate_pure(&code);
      if ((CODETYPE(code)) & (code_flag_continuation << code_flags_offset))
      {
        SETCLOSCODE(p, code);
        relocate_pure(&CONTWINDERS(p));
        relocate_indirect(CONTATTACHMENTS(p));
        if ((CONTLENGTH(p)) == scaled_shot_1_shot_flag)
        {
        }
        else
        {
          relocate_pure(&CONTLINK(p));
          {
            ptr xcp = CONTRET(p);
            {
              iptr co = (ENTRYOFFSET(xcp)) + (((uptr)xcp) - ((uptr)(TO_PTR((ENTRYOFFSETADDR(xcp))))));
              {
                ptr c_p = (ptr)(((uptr)xcp) - co);
                {
                  seginfo* x_si = SegInfo((ptr_get_segment(c_p)));
                  if (x_si -> old_space)
                  {
                    relocate_code(c_p, x_si);
                  }
                  {
                    uptr stack = (uptr)(CONTSTACK(p));
                    {
                      uptr base = stack;
                      {
                        uptr fp = stack + (CONTCLENGTH(p));
                        {
                          uptr ret = (uptr)(CONTRET(p));
                          while (fp != base)
                          {
                            if (fp < base)
                            {
                              S_error_abort("sweep_stack(gc): malformed stack");
                            }
                            fp = fp - (ENTRYFRAMESIZE(ret));
                            {
                              iptr oldret = ret;
                              ptr num = ENTRYLIVEMASK(oldret);
                              ptr* pp = (ptr*)(TO_VOIDP(fp));
                              ret = (iptr)(*(pp));
                              {
                                ptr xcp = *(pp);
                                {
                                  iptr co = (ENTRYOFFSET(xcp)) + (((uptr)xcp) - ((uptr)(TO_PTR((ENTRYOFFSETADDR(xcp))))));
                                  {
                                    ptr c_p = (ptr)(((uptr)xcp) - co);
                                    {
                                      seginfo* x_si = SegInfo((ptr_get_segment(c_p)));
                                      if (x_si -> old_space)
                                      {
                                        relocate_code(c_p, x_si);
                                      }
                                      if (Sfixnump(num))
                                      {
                                        {
                                          uptr mask = UNFIX(num);
                                          while (mask != 0)
                                          {
                                            pp += 1;
                                            if (mask & 1)
                                            {
                                              relocate_pure(&(*(pp)));
                                            }
                                            mask >>= 1;
                                          }
                                        }
                                      }
                                      else
                                      {
                                        seginfo* n_si = SegInfo((ptr_get_segment(num)));
                                        if (!(n_si -> old_space))
                                        {
                                        }
                                        else if (SEGMENT_IS_LOCAL(n_si, num))
                                        {
                                          relocate_pure(&(*((ENTRYNONCOMPACTLIVEMASKADDR(oldret)))));
                                          num = ENTRYLIVEMASK(oldret);
                                        }
                                        else
                                        {
                                          RECORD_REMOTE(n_si);
                                          num = S_G.zero_length_bignum;
                                        }
                                        {
                                          iptr index = BIGLEN(num);
                                          while (index != 0)
                                          {
                                            index -= 1;
                                            {
                                              INT bits = bigit_bits;
                                              bigit mask = BIGIT(num, index);
                                              while (bits > 0)
                                              {
                                                bits -= 1;
                                                pp += 1;
                                                if (mask & 1)
                                                {
                                                  relocate_pure(&(*(pp)));
                                                }
                                                mask >>= 1;
                                              }
                                            }
                                          }
                                        }
                                      }
                                    }
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
      else
      {
        uptr len = CODEFREE(code);
        if ((CODETYPE(code)) & (code_flag_mutable_closure << code_flags_offset))
        {
          SETCLOSCODE(p, code);
          {
            uptr idx, p_len = len;
            ptr *p_p = &CLOSIT(p, 0);
            for (idx = 0; idx < p_len; idx++)
            {
              relocate_indirect((p_p[idx]));
            }
          }
        }
        else
        {
          SETCLOSCODE(p, code);
          {
            uptr idx, p_len = len;
            ptr *p_p = &CLOSIT(p, 0);
            for (idx = 0; idx < p_len; idx++)
            {
              relocate_pure(&(p_p[idx]));
            }
          }
        }
      }
    }
    else if (t == type_symbol)
    {
      relocate_indirect(INITSYMVAL(p));
      {
        ptr val = INITSYMVAL(p);
        {
          ptr code = ((Sprocedurep(val))
                      ? (CLOSCODE(val))
                      : (SYMCODE(p)));
          relocate_pure(&code);
          INITSYMCODE(p, code);
          relocate_indirect(INITSYMPLIST(p));
          relocate_indirect(INITSYMNAME(p));
          relocate_indirect(INITSYMSPLIST(p));
          relocate_indirect(INITSYMHASH(p));
        }
      }
    }
    else if (t == type_flonum)
    {
    }
    else
    {
      S_error_abort("sweep-in-old: illegal type");
    }
  }
  ASSERT_EMPTY_FLUSH_REMOTE();
}

static IGEN sweep_dirty_object(thread_gc *tgc, ptr p, IGEN youngest)
{
  FLUSH_REMOTE_BLOCK
  {
    ITYPE t = TYPEBITS(p);
    if (t == type_typed_object)
    {
      ptr tf = TYPEFIELD(p);
      if (TYPEP(tf, mask_record, type_record))
      {
        {
          ptr rtd = RECORDINSTTYPE(p);
          {
            ptr num = RECORDDESCMPM(rtd);
            ptr* pp = &(RECORDINSTIT(p, 0));
            if (Sfixnump(num))
            {
              {
                uptr mask = ((uptr)(UNFIX(num))) >> 1;
                while (mask != 0)
                {
                  if (mask & 1)
                  {
                    relocate_dirty(&(*(pp)), youngest);
                  }
                  mask >>= 1;
                  pp += 1;
                }
              }
            }
            else
            {
              {
                iptr index = (BIGLEN(num)) - 1;
                bigit mask = (BIGIT(num, index)) >> 1;
                INT bits = bigit_bits - 1;
                while (1)
                {
                  do
                  {
                    if (mask & 1)
                    {
                      relocate_dirty(&(*(pp)), youngest);
                    }
                    mask >>= 1;
                    pp += 1;
                    bits -= 1;
                  }
                  while (bits > 0);
                  if (index == 0)
                  {
                    break;
                  }
                  index -= 1;
                  mask = BIGIT(num, index);
                  bits = bigit_bits;
                }
              }
            }
          }
        }
      }
      else if (TYPEP(tf, mask_vector, type_vector))
      {
        uptr len = Svector_length(p);
        {
          uptr idx, p_len = len;
          ptr *p_p = &INITVECTIT(p, 0);
          for (idx = 0; idx < p_len; idx++)
          {
            relocate_dirty(&(p_p[idx]), youngest);
          }
        }
      }
      else if (TYPEP(tf, mask_any_stencil_vector, type_any_stencil_vector))
      {
        uptr len = Sstencil_vector_length(p);
        {
          uptr idx, p_len = len;
          ptr *p_p = &INITSTENVECTIT(p, 0);
          for (idx = 0; idx < p_len; idx++)
          {
            relocate_dirty(&(p_p[idx]), youngest);
          }
        }
      }
      else if (TYPEP(tf, mask_string, type_string))
      {
      }
      else if (TYPEP(tf, mask_fxvector, type_fxvector))
      {
      }
      else if (TYPEP(tf, mask_flvector, type_flvector))
      {
      }
      else if (TYPEP(tf, mask_bytevector, type_bytevector))
      {
        {
          ISPC p_at_spc = SPACE(p);
          if (p_at_spc == space_reference_array)
          {
            {
              uptr len = Sbytevector_reference_length(p);
              {
                uptr idx, p_len = len;
                ptr *p_p = (ptr*)&BVIT(p, 0);
                for (idx = 0; idx < p_len; idx++)
                {
                  relocate_reference_dirty(&(p_p[idx]), youngest);
                }
              }
            }
          }
          else
          {
          }
        }
      }
      else if ((iptr)tf == type_tlc)
      {
        relocate_dirty(&INITTLCHT(p), youngest);
        relocate_dirty(&INITTLCKEYVAL(p), youngest);
        relocate_dirty(&INITTLCNEXT(p), youngest);
      }
      else if (TYPEP(tf, mask_box, type_box))
      {
        relocate_dirty(&INITBOXREF(p), youngest);
      }
      else if ((iptr)tf == type_ratnum)
      {
      }
      else if ((iptr)tf == type_exactnum)
      {
      }
      else if ((iptr)tf == type_inexactnum)
      {
      }
      else if (TYPEP(tf, mask_bignum, type_bignum))
      {
      }
      else if (TYPEP(tf, mask_port, type_port))
      {
        relocate_dirty(&PORTHANDLER(p), youngest);
        if (((uptr)tf) & PORT_FLAG_OUTPUT)
        {
          iptr n = ((iptr)(PORTOLAST(p))) - ((iptr)(PORTOBUF(p)));
          relocate_dirty(&PORTOBUF(p), youngest);
          PORTOLAST(p) = (ptr)(((iptr)(PORTOBUF(p))) + n);
        }
        if (((uptr)tf) & PORT_FLAG_INPUT)
        {
          iptr n = ((iptr)(PORTILAST(p))) - ((iptr)(PORTIBUF(p)));
          relocate_dirty(&PORTIBUF(p), youngest);
          PORTILAST(p) = (ptr)(((iptr)(PORTIBUF(p))) + n);
        }
        relocate_dirty(&PORTINFO(p), youngest);
        relocate_dirty(&PORTNAME(p), youngest);
      }
      else if (TYPEP(tf, mask_code, type_code))
      {
      }
      else if ((iptr)tf == type_thread)
      {
      }
      else if ((iptr)tf == type_rtd_counts)
      {
      }
      else if ((iptr)tf == type_phantom)
      {
      }
      else
      {
        S_error_abort("sweep: illegal typed object type");
      }
    }
    else if (t == type_pair)
    {
      {
        ISPC p_at_spc = SPACE(p);
        if (p_at_spc < space_weakpair)
        {
          relocate_dirty(&INITCAR(p), youngest);
          relocate_dirty(&INITCDR(p), youngest);
        }
        else if (p_at_spc == space_ephemeron)
        {
          add_ephemeron_to_pending(tgc, p);
        }
        else if (p_at_spc == space_weakpair)
        {
          relocate_dirty(&INITCDR(p), youngest);
        }
        else
        {
          relocate_reference_dirty(&INITCAR(p), youngest);
          relocate_reference_dirty(&INITCDR(p), youngest);
        }
      }
    }
    else if (t == type_closure)
    {
      ptr code = CLOSCODE(p);
      {
        uptr len = CODEFREE(code);
        if ((CODETYPE(code)) & (code_flag_mutable_closure << code_flags_offset))
        {
          {
            uptr idx, p_len = len;
            ptr *p_p = &CLOSIT(p, 0);
            for (idx = 0; idx < p_len; idx++)
            {
              relocate_dirty(&(p_p[idx]), youngest);
            }
          }
        }
      }
    }
    else if (t == type_symbol)
    {
      relocate_dirty(&INITSYMVAL(p), youngest);
      {
        ptr val = INITSYMVAL(p);
        {
          ptr code = ((Sprocedurep(val))
                      ? (CLOSCODE(val))
                      : (SYMCODE(p)));
          relocate_dirty(&code, youngest);
          INITSYMCODE(p, code);
          relocate_dirty(&INITSYMPLIST(p), youngest);
          relocate_dirty(&INITSYMNAME(p), youngest);
          relocate_dirty(&INITSYMSPLIST(p), youngest);
          relocate_dirty(&INITSYMHASH(p), youngest);
        }
      }
    }
    else if (t == type_flonum)
    {
    }
    else
    {
      S_error_abort("sweep: illegal type");
    }
  }
  FLUSH_REMOTE(tgc, p);
  return youngest;
}

static void sweep_record(thread_gc *tgc, ptr p, IGEN from_g)
{
  FLUSH_REMOTE_BLOCK
  {
    relocate_pure(&RECORDINSTTYPE(p));
    {
      ptr rtd = RECORDINSTTYPE(p);
      {
        uptr len = UNFIX((RECORDDESCSIZE(rtd)));
        {
          ptr num = RECORDDESCPM(rtd);
          ptr* pp = &(RECORDINSTIT(p, 0));
          if (Sfixnump(num))
          {
            {
              uptr mask = ((uptr)(UNFIX(num))) >> 1;
              if (mask == (((uptr)-1) >> 1))
              {
                {
                  ptr* ppend = ((ptr*)(TO_VOIDP((((uptr)(TO_PTR(pp))) + len)))) - 1;
                  while (pp < ppend)
                  {
                    relocate_impure(&(*(pp)), from_g);
                    pp += 1;
                  }
                }
              }
              else
              {
                while (mask != 0)
                {
                  if (mask & 1)
                  {
                    relocate_impure(&(*(pp)), from_g);
                  }
                  mask >>= 1;
                  pp += 1;
                }
              }
            }
          }
          else
          {
            relocate_pure(&(RECORDDESCPM(rtd)));
            num = RECORDDESCPM(rtd);
            {
              iptr index = (BIGLEN(num)) - 1;
              bigit mask = (BIGIT(num, index)) >> 1;
              INT bits = bigit_bits - 1;
              while (1)
              {
                do
                {
                  if (mask & 1)
                  {
                    relocate_impure(&(*(pp)), from_g);
                  }
                  mask >>= 1;
                  pp += 1;
                  bits -= 1;
                }
                while (bits > 0);
                if (index == 0)
                {
                  break;
                }
                index -= 1;
                mask = BIGIT(num, index);
                bits = bigit_bits;
              }
            }
          }
        }
      }
    }
  }
  FLUSH_REMOTE(tgc, p);
}

static IGEN sweep_dirty_record(thread_gc *tgc, ptr p, IGEN youngest)
{
  FLUSH_REMOTE_BLOCK
  {
    {
      ptr rtd = RECORDINSTTYPE(p);
      {
        ptr num = RECORDDESCMPM(rtd);
        ptr* pp = &(RECORDINSTIT(p, 0));
        if (Sfixnump(num))
        {
          {
            uptr mask = ((uptr)(UNFIX(num))) >> 1;
            while (mask != 0)
            {
              if (mask & 1)
              {
                relocate_dirty(&(*(pp)), youngest);
              }
              mask >>= 1;
              pp += 1;
            }
          }
        }
        else
        {
          {
            iptr index = (BIGLEN(num)) - 1;
            bigit mask = (BIGIT(num, index)) >> 1;
            INT bits = bigit_bits - 1;
            while (1)
            {
              do
              {
                if (mask & 1)
                {
                  relocate_dirty(&(*(pp)), youngest);
                }
                mask >>= 1;
                pp += 1;
                bits -= 1;
              }
              while (bits > 0);
              if (index == 0)
              {
                break;
              }
              index -= 1;
              mask = BIGIT(num, index);
              bits = bigit_bits;
            }
          }
        }
      }
    }
  }
  FLUSH_REMOTE(tgc, p);
  return youngest;
}

static void sweep_symbol(thread_gc *tgc, ptr p, IGEN from_g)
{
  FLUSH_REMOTE_BLOCK
  {
    {
      relocate_impure(&INITSYMVAL(p), from_g);
      {
        ptr val = INITSYMVAL(p);
        {
          ptr code = ((Sprocedurep(val))
                      ? (CLOSCODE(val))
                      : (SYMCODE(p)));
          relocate_pure(&code);
          INITSYMCODE(p, code);
          relocate_impure(&INITSYMPLIST(p), from_g);
          relocate_impure(&INITSYMNAME(p), from_g);
          relocate_impure(&INITSYMSPLIST(p), from_g);
          relocate_impure(&INITSYMHASH(p), from_g);
        }
      }
    }
  }
  FLUSH_REMOTE(tgc, p);
}

static IGEN sweep_dirty_symbol(thread_gc *tgc, ptr p, IGEN youngest)
{
  FLUSH_REMOTE_BLOCK
  {
    {
      relocate_dirty(&INITSYMVAL(p), youngest);
      {
        ptr val = INITSYMVAL(p);
        {
          ptr code = ((Sprocedurep(val))
                      ? (CLOSCODE(val))
                      : (SYMCODE(p)));
          relocate_dirty(&code, youngest);
          INITSYMCODE(p, code);
          relocate_dirty(&INITSYMPLIST(p), youngest);
          relocate_dirty(&INITSYMNAME(p), youngest);
          relocate_dirty(&INITSYMSPLIST(p), youngest);
          relocate_dirty(&INITSYMHASH(p), youngest);
        }
      }
    }
  }
  FLUSH_REMOTE(tgc, p);
  return youngest;
}

static void sweep_thread(thread_gc *tgc, ptr p)
{
  FLUSH_REMOTE_BLOCK
  {
    {
      ptr tc = (ptr)(THREADTC(p));
      if (tc != ((ptr)0))
      {
        {
          ptr old_stack = SCHEMESTACK(tc);
          if (OLDSPACE(old_stack))
          {
            {
              iptr clength = ((uptr)(SFP(tc))) - ((uptr)old_stack);
              SCHEMESTACK(tc) = copy_stack(tgc, old_stack, &(SCHEMESTACKSIZE(tc)), clength + (sizeof(ptr)));
              SFP(tc) = (ptr)(((uptr)(SCHEMESTACK(tc))) + clength);
              ESP(tc) = (ptr)((((uptr)(SCHEMESTACK(tc))) + (SCHEMESTACKSIZE(tc))) - stack_slop);
            }
          }
        }
        STACKCACHE(tc) = Snil;
        relocate_pure(&(CCHAIN(tc)));
        relocate_pure(&(STACKLINK(tc)));
        relocate_pure(&(WINDERS(tc)));
        relocate_pure(&(ATTACHMENTS(tc)));
        relocate_pure(&(HANDLERSTACK(tc)));
        CACHEDFRAME(tc) = Sfalse;
        {
          ptr xcp = FRAME(tc, 0);
          {
            iptr co = (ENTRYOFFSET(xcp)) + (((uptr)xcp) - ((uptr)(TO_PTR((ENTRYOFFSETADDR(xcp))))));
            {
              ptr c_p = (ptr)(((uptr)xcp) - co);
              {
                seginfo* x_si = SegInfo((ptr_get_segment(c_p)));
                if (x_si -> old_space)
                {
                  relocate_code(c_p, x_si);
                  FRAME(tc, 0) = (ptr)(((uptr)c_p) + co);
                }
                {
                  uptr base = (uptr)(SCHEMESTACK(tc));
                  {
                    uptr fp = (uptr)(SFP(tc));
                    {
                      uptr ret = (uptr)(FRAME(tc, 0));
                      while (fp != base)
                      {
                        if (fp < base)
                        {
                          S_error_abort("sweep_stack(gc): malformed stack");
                        }
                        fp = fp - (ENTRYFRAMESIZE(ret));
                        {
                          iptr oldret = ret;
                          ptr num = ENTRYLIVEMASK(oldret);
                          ptr* pp = (ptr*)(TO_VOIDP(fp));
                          ret = (iptr)(*(pp));
                          {
                            ptr xcp = *(pp);
                            {
                              iptr co = (ENTRYOFFSET(xcp)) + (((uptr)xcp) - ((uptr)(TO_PTR((ENTRYOFFSETADDR(xcp))))));
                              {
                                ptr c_p = (ptr)(((uptr)xcp) - co);
                                {
                                  seginfo* x_si = SegInfo((ptr_get_segment(c_p)));
                                  if (x_si -> old_space)
                                  {
                                    relocate_code(c_p, x_si);
                                    *(pp) = (ptr)(((uptr)c_p) + co);
                                  }
                                  if (Sfixnump(num))
                                  {
                                    {
                                      uptr mask = UNFIX(num);
                                      while (mask != 0)
                                      {
                                        pp += 1;
                                        if (mask & 1)
                                        {
                                          relocate_pure(&(*(pp)));
                                        }
                                        mask >>= 1;
                                      }
                                    }
                                  }
                                  else
                                  {
                                    seginfo* n_si = SegInfo((ptr_get_segment(num)));
                                    if (!(n_si -> old_space))
                                    {
                                    }
                                    else if (SEGMENT_IS_LOCAL(n_si, num))
                                    {
                                      relocate_pure(&(*((ENTRYNONCOMPACTLIVEMASKADDR(oldret)))));
                                      num = ENTRYLIVEMASK(oldret);
                                    }
                                    else
                                    {
                                      RECORD_REMOTE(n_si);
                                      num = S_G.zero_length_bignum;
                                    }
                                    {
                                      iptr index = BIGLEN(num);
                                      while (index != 0)
                                      {
                                        index -= 1;
                                        {
                                          INT bits = bigit_bits;
                                          bigit mask = BIGIT(num, index);
                                          while (bits > 0)
                                          {
                                            bits -= 1;
                                            pp += 1;
                                            if (mask & 1)
                                            {
                                              relocate_pure(&(*(pp)));
                                            }
                                            mask >>= 1;
                                          }
                                        }
                                      }
                                    }
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                      PAR_SWEEPERS_WORKAROUND();
                      U(tc) = 0;
                      V(tc) = 0;
                      W(tc) = 0;
                      X(tc) = 0;
                      Y(tc) = 0;
                      relocate_pure(&(THREADNO(tc)));
                      relocate_pure(&(CURRENTINPUT(tc)));
                      relocate_pure(&(CURRENTOUTPUT(tc)));
                      relocate_pure(&(CURRENTERROR(tc)));
                      relocate_pure(&(SFD(tc)));
                      relocate_pure(&(CURRENTMSO(tc)));
                      relocate_pure(&(TARGETMACHINE(tc)));
                      relocate_pure(&(FXLENGTHBV(tc)));
                      relocate_pure(&(FXFIRSTBITSETBV(tc)));
                      relocate_pure(&(COMPILEPROFILE(tc)));
                      relocate_pure(&(SUBSETMODE(tc)));
                      relocate_pure(&(DEFAULTRECORDEQUALPROCEDURE(tc)));
                      relocate_pure(&(DEFAULTRECORDHASHPROCEDURE(tc)));
                      relocate_pure(&(COMPRESSFORMAT(tc)));
                      relocate_pure(&(COMPRESSLEVEL(tc)));
                      relocate_pure(&(PARAMETERS(tc)));
                      DSTBV(tc) = Sfalse;
                      SRCBV(tc) = Sfalse;
                      {
                        INT i = 0;
                        while (i < virtual_register_count)
                        {
                          relocate_pure(&(VIRTREG(tc, i)));
                          i += 1;
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  FLUSH_REMOTE(tgc, p);
}

static void sweep_port(thread_gc *tgc, ptr p, IGEN from_g)
{
  FLUSH_REMOTE_BLOCK
  {
    relocate_impure(&PORTHANDLER(p), from_g);
    if (((uptr)TYPEFIELD(p)) & PORT_FLAG_OUTPUT)
    {
      iptr n = ((iptr)(PORTOLAST(p))) - ((iptr)(PORTOBUF(p)));
      relocate_impure(&PORTOBUF(p), from_g);
      PORTOLAST(p) = (ptr)(((iptr)(PORTOBUF(p))) + n);
    }
    if (((uptr)TYPEFIELD(p)) & PORT_FLAG_INPUT)
    {
      iptr n = ((iptr)(PORTILAST(p))) - ((iptr)(PORTIBUF(p)));
      relocate_impure(&PORTIBUF(p), from_g);
      PORTILAST(p) = (ptr)(((iptr)(PORTIBUF(p))) + n);
    }
    relocate_impure(&PORTINFO(p), from_g);
    relocate_impure(&PORTNAME(p), from_g);
  }
  FLUSH_REMOTE(tgc, p);
}

static IGEN sweep_dirty_port(thread_gc *tgc, ptr p, IGEN youngest)
{
  FLUSH_REMOTE_BLOCK
  {
    relocate_dirty(&PORTHANDLER(p), youngest);
    if (((uptr)TYPEFIELD(p)) & PORT_FLAG_OUTPUT)
    {
      iptr n = ((iptr)(PORTOLAST(p))) - ((iptr)(PORTOBUF(p)));
      relocate_dirty(&PORTOBUF(p), youngest);
      PORTOLAST(p) = (ptr)(((iptr)(PORTOBUF(p))) + n);
    }
    if (((uptr)TYPEFIELD(p)) & PORT_FLAG_INPUT)
    {
      iptr n = ((iptr)(PORTILAST(p))) - ((iptr)(PORTIBUF(p)));
      relocate_dirty(&PORTIBUF(p), youngest);
      PORTILAST(p) = (ptr)(((iptr)(PORTIBUF(p))) + n);
    }
    relocate_dirty(&PORTINFO(p), youngest);
    relocate_dirty(&PORTNAME(p), youngest);
  }
  FLUSH_REMOTE(tgc, p);
  return youngest;
}

static void sweep_continuation(thread_gc *tgc, ptr p, IGEN from_g)
{
  FLUSH_REMOTE_BLOCK
  {
    {
      relocate_pure(&CONTWINDERS(p));
      relocate_impure(&CONTATTACHMENTS(p), from_g);
      if ((CONTLENGTH(p)) == scaled_shot_1_shot_flag)
      {
      }
      else
      {
        ptr stk = CONTSTACK(p);
        {
          seginfo* s_si = NULL;
          if ((stk != ((ptr)0)) && ((s_si = (SegInfo((ptr_get_segment(stk))))), (s_si -> old_space)))
          {
            if (!(SEGMENT_IS_LOCAL(s_si, stk)))
            {
              RECORD_REMOTE(s_si);
            }
            else
            {
              CONTSTACK(p) = copy_stack(tgc, CONTSTACK(p), &(CONTLENGTH(p)), CONTCLENGTH(p));
            }
          }
          relocate_pure(&CONTLINK(p));
          {
            ptr xcp = CONTRET(p);
            {
              iptr co = (ENTRYOFFSET(xcp)) + (((uptr)xcp) - ((uptr)(TO_PTR((ENTRYOFFSETADDR(xcp))))));
              {
                ptr c_p = (ptr)(((uptr)xcp) - co);
                {
                  seginfo* x_si = SegInfo((ptr_get_segment(c_p)));
                  if (x_si -> old_space)
                  {
                    relocate_code(c_p, x_si);
                    CONTRET(p) = (ptr)(((uptr)c_p) + co);
                  }
                  {
                    uptr stack = (uptr)(CONTSTACK(p));
                    {
                      uptr base = stack;
                      {
                        uptr fp = stack + (CONTCLENGTH(p));
                        {
                          uptr ret = (uptr)(CONTRET(p));
                          while (fp != base)
                          {
                            if (fp < base)
                            {
                              S_error_abort("sweep_stack(gc): malformed stack");
                            }
                            fp = fp - (ENTRYFRAMESIZE(ret));
                            {
                              iptr oldret = ret;
                              ptr num = ENTRYLIVEMASK(oldret);
                              ptr* pp = (ptr*)(TO_VOIDP(fp));
                              ret = (iptr)(*(pp));
                              {
                                ptr xcp = *(pp);
                                {
                                  iptr co = (ENTRYOFFSET(xcp)) + (((uptr)xcp) - ((uptr)(TO_PTR((ENTRYOFFSETADDR(xcp))))));
                                  {
                                    ptr c_p = (ptr)(((uptr)xcp) - co);
                                    {
                                      seginfo* x_si = SegInfo((ptr_get_segment(c_p)));
                                      if (x_si -> old_space)
                                      {
                                        relocate_code(c_p, x_si);
                                        *(pp) = (ptr)(((uptr)c_p) + co);
                                      }
                                      if (Sfixnump(num))
                                      {
                                        {
                                          uptr mask = UNFIX(num);
                                          while (mask != 0)
                                          {
                                            pp += 1;
                                            if (mask & 1)
                                            {
                                              relocate_pure(&(*(pp)));
                                            }
                                            mask >>= 1;
                                          }
                                        }
                                      }
                                      else
                                      {
                                        seginfo* n_si = SegInfo((ptr_get_segment(num)));
                                        if (!(n_si -> old_space))
                                        {
                                        }
                                        else if (SEGMENT_IS_LOCAL(n_si, num))
                                        {
                                          relocate_pure(&(*((ENTRYNONCOMPACTLIVEMASKADDR(oldret)))));
                                          num = ENTRYLIVEMASK(oldret);
                                        }
                                        else
                                        {
                                          RECORD_REMOTE(n_si);
                                          num = S_G.zero_length_bignum;
                                        }
                                        {
                                          iptr index = BIGLEN(num);
                                          while (index != 0)
                                          {
                                            index -= 1;
                                            {
                                              INT bits = bigit_bits;
                                              bigit mask = BIGIT(num, index);
                                              while (bits > 0)
                                              {
                                                bits -= 1;
                                                pp += 1;
                                                if (mask & 1)
                                                {
                                                  relocate_pure(&(*(pp)));
                                                }
                                                mask >>= 1;
                                              }
                                            }
                                          }
                                        }
                                      }
                                    }
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  FLUSH_REMOTE(tgc, p);
}

static void sweep_code_object(thread_gc *tgc, ptr p, IGEN from_g)
{
  FLUSH_REMOTE_BLOCK
  {
    relocate_pure(&CODENAME(p));
    relocate_pure(&CODEARITYMASK(p));
    relocate_pure(&CODEINFO(p));
    relocate_pure(&CODEPINFOS(p));
    {
      ptr t = CODERELOC(p);
      {
        iptr m = (t
                  ? (RELOCSIZE(t))
                  : 0);
        {
          ptr oldco = (t
                       ? (RELOCCODE(t))
                       : 0);
          {
            iptr a = 0;
            {
              iptr n = 0;
              while (n < m)
              {
                {
                  uptr entry = RELOCIT(t, n);
                  uptr item_off = 0;
                  uptr code_off = 0;
                  n = n + 1;
                  if (RELOC_EXTENDED_FORMAT(entry))
                  {
                    item_off = RELOCIT(t, n);
                    n = n + 1;
                    code_off = RELOCIT(t, n);
                    n = n + 1;
                  }
                  else
                  {
                    item_off = RELOC_ITEM_OFFSET(entry);
                    code_off = RELOC_CODE_OFFSET(entry);
                  }
                  a = a + code_off;
                  {
                    ptr obj = S_get_code_obj(RELOC_TYPE(entry), oldco, a, item_off);
                    relocate_pure(&obj);
                    S_set_code_obj("gc", RELOC_TYPE(entry), p, a, obj, item_off);
                  }
                }
              }
              if ((from_g == static_generation) && ((!S_G.retain_static_relocation) && (0 == ((CODETYPE(p)) & (code_flag_template << code_flags_offset)))))
              {
                CODERELOC(p) = (ptr)0;
              }
              else
              {
                {
                  seginfo* t_si = SegInfo((ptr_get_segment(t)));
                  if (t_si -> old_space)
                  {
                    if (SEGMENT_IS_LOCAL(t_si, t))
                    {
                      n = size_reloc_table((RELOCSIZE(t)));
                      if (t_si -> use_marks)
                      {
                        if (!(marked(t_si, t)))
                        {
                          mark_untyped_data_object(tgc, t, n, t_si);
                        }
                      }
                      else
                      {
                        {
                          ptr oldt = t;
                          find_gc_room(tgc, space_data, from_g, type_untyped, n, t);
                          memcpy_aligned(TO_VOIDP(t), TO_VOIDP(oldt), n);
                        }
                      }
                    }
                    else
                    {
                      RECORD_REMOTE(t_si);
                    }
                  }
                }
                RELOCCODE(t) = p;
                CODERELOC(p) = t;
              }
              S_record_code_mod(tgc -> tc, (uptr)(TO_PTR((&(CODEIT(p, 0))))), (uptr)(CODELEN(p)));
            }
          }
        }
      }
    }
  }
  FLUSH_REMOTE(tgc, p);
}

static uptr size_object(ptr p)
{
  ITYPE t = TYPEBITS(p);
  if (t == type_typed_object)
  {
    ptr tf = TYPEFIELD(p);
    if (TYPEP(tf, mask_record, type_record))
    {
      {
        ptr rtd = RECORDINSTTYPE(p);
        {
          uptr len = UNFIX((RECORDDESCSIZE(rtd)));
          {
            uptr p_sz = size_record_inst(len);
            return p_sz;
          }
        }
      }
    }
    else if (TYPEP(tf, mask_vector, type_vector))
    {
      uptr len = Svector_length(p);
      {
        uptr p_sz = size_vector(len);
        return p_sz;
      }
    }
    else if (TYPEP(tf, mask_any_stencil_vector, type_any_stencil_vector))
    {
      uptr len = Sstencil_vector_length(p);
      {
        uptr p_sz = size_stencil_vector(len);
        return p_sz;
      }
    }
    else if (TYPEP(tf, mask_string, type_string))
    {
      uptr sz = size_string((Sstring_length(p)));
      {
        uptr p_sz = sz;
        return p_sz;
      }
    }
    else if (TYPEP(tf, mask_fxvector, type_fxvector))
    {
      uptr sz = size_fxvector((Sfxvector_length(p)));
      {
        uptr p_sz = sz;
        return p_sz;
      }
    }
    else if (TYPEP(tf, mask_flvector, type_flvector))
    {
      uptr sz = size_flvector((Sflvector_length(p)));
      {
        uptr p_sz = sz;
        return p_sz;
      }
    }
    else if (TYPEP(tf, mask_bytevector, type_bytevector))
    {
      {
        ISPC p_at_spc = SPACE(p);
        if (p_at_spc == space_reference_array)
        {
          uptr sz = size_bytevector((Sbytevector_length(p)));
          {
            uptr p_sz = sz;
            return p_sz;
          }
        }
        else
        {
          uptr sz = size_bytevector((Sbytevector_length(p)));
          {
            uptr p_sz = sz;
            return p_sz;
          }
        }
      }
    }
    else if ((iptr)tf == type_tlc)
    {
      uptr p_sz = size_tlc;
      return p_sz;
    }
    else if (TYPEP(tf, mask_box, type_box))
    {
      uptr p_sz = size_box;
      return p_sz;
    }
    else if ((iptr)tf == type_ratnum)
    {
      uptr p_sz = size_ratnum;
      return p_sz;
    }
    else if ((iptr)tf == type_exactnum)
    {
      uptr p_sz = size_exactnum;
      return p_sz;
    }
    else if ((iptr)tf == type_inexactnum)
    {
      uptr p_sz = size_inexactnum;
      return p_sz;
    }
    else if (TYPEP(tf, mask_bignum, type_bignum))
    {
      uptr sz = size_bignum((BIGLEN(p)));
      {
        uptr p_sz = sz;
        return p_sz;
      }
    }
    else if (TYPEP(tf, mask_port, type_port))
    {
      uptr p_sz = size_port;
      return p_sz;
    }
    else if (TYPEP(tf, mask_code, type_code))
    {
      uptr len = CODELEN(p);
      {
        uptr p_sz = size_code(len);
        return p_sz;
      }
    }
    else if ((iptr)tf == type_thread)
    {
      uptr p_sz = size_thread;
      return p_sz;
    }
    else if ((iptr)tf == type_rtd_counts)
    {
      uptr p_sz = size_rtd_counts;
      return p_sz;
    }
    else if ((iptr)tf == type_phantom)
    {
      uptr p_sz = size_phantom;
      return p_sz;
    }
    else
    {
      S_error_abort("size: illegal typed object type");
    }
  }
  else if (t == type_pair)
  {
    {
      ISPC p_at_spc = SPACE(p);
      if (p_at_spc < space_weakpair)
      {
        uptr p_sz = size_pair;
        return p_sz;
      }
      else if (p_at_spc == space_ephemeron)
      {
        uptr p_sz = size_ephemeron;
        return p_sz;
      }
      else if (p_at_spc == space_weakpair)
      {
        uptr p_sz = size_pair;
        return p_sz;
      }
      else
      {
        S_error_abort("misplaced pair");
      }
    }
  }
  else if (t == type_closure)
  {
    ptr code = CLOSCODE(p);
    if ((CODETYPE(code)) & (code_flag_continuation << code_flags_offset))
    {
      uptr p_sz = size_continuation;
      return p_sz;
    }
    else
    {
      uptr len = CODEFREE(code);
      {
        uptr p_sz = size_closure(len);
        return p_sz;
      }
    }
  }
  else if (t == type_symbol)
  {
    uptr p_sz = size_symbol;
    return p_sz;
  }
  else if (t == type_flonum)
  {
    uptr p_sz = size_flonum;
    return p_sz;
  }
  else
  {
    S_error_abort("size: illegal type");
  }
}

static IGEN mark_object(thread_gc *tgc, ptr p, seginfo *si)
{
  check_triggers(tgc, si);
  if (!si->marked_mask) {
    init_mask(tgc, si->marked_mask, si->generation, 0);
  }
  {
    ITYPE t = TYPEBITS(p);
    if (t == type_typed_object)
    {
      ptr tf = TYPEFIELD(p);
      if (TYPEP(tf, mask_record, type_record))
      {
        /* Do not inspect the type or first field of the rtd, because
           it may have been overwritten for forwarding. */
        {
          ptr rtd = RECORDINSTTYPE(p);
          {
            uptr len = UNFIX((RECORDDESCSIZE(rtd)));
            {
              uptr p_sz = size_record_inst(len);
              {
                uptr addr = (uptr)UNTYPE(p, type_typed_object);
                if (addr_get_segment(addr) == addr_get_segment(addr + p_sz - 1))
                {
                  si->marked_count += p_sz;
                  {
                    uptr offset = 0;
                    while (offset < p_sz) {
                      ptr mark_p = (ptr)((uptr)p + offset);
                      si->marked_mask[segment_bitmap_byte(mark_p)] |= segment_bitmap_bit(mark_p);
                      offset += byte_alignment;
                    }
                  }
                }
                else
                {
                  uptr offset = 0;
                  while (offset < p_sz) {
                    ptr mark_p = (ptr)((uptr)p + offset);
                    seginfo *mark_si = SegInfo(ptr_get_segment(mark_p));
                    if (!mark_si->marked_mask) {
                      init_mask(tgc, mark_si->marked_mask, mark_si->generation, 0);
                    }
                    mark_si->marked_mask[segment_bitmap_byte(mark_p)] |= segment_bitmap_bit(mark_p);
                    mark_si->marked_count += byte_alignment;
                    offset += byte_alignment;
                  }
                }
              }
              push_sweep(p);
            }
          }
        }
      }
      else if (TYPEP(tf, mask_vector, type_vector))
      {
        uptr len = Svector_length(p);
        {
          uptr p_sz = size_vector(len);
          {
            uptr addr = (uptr)UNTYPE(p, type_typed_object);
            if (addr_get_segment(addr) == addr_get_segment(addr + p_sz - 1))
            {
              si->marked_count += p_sz;
              {
                uptr offset = 0;
                while (offset < p_sz) {
                  ptr mark_p = (ptr)((uptr)p + offset);
                  si->marked_mask[segment_bitmap_byte(mark_p)] |= segment_bitmap_bit(mark_p);
                  offset += byte_alignment;
                }
              }
            }
            else
            {
              uptr offset = 0;
              while (offset < p_sz) {
                ptr mark_p = (ptr)((uptr)p + offset);
                seginfo *mark_si = SegInfo(ptr_get_segment(mark_p));
                if (!mark_si->marked_mask) {
                  init_mask(tgc, mark_si->marked_mask, mark_si->generation, 0);
                }
                mark_si->marked_mask[segment_bitmap_byte(mark_p)] |= segment_bitmap_bit(mark_p);
                mark_si->marked_count += byte_alignment;
                offset += byte_alignment;
              }
            }
          }
          push_sweep(p);
        }
      }
      else if (TYPEP(tf, mask_any_stencil_vector, type_any_stencil_vector))
      {
        uptr len = Sstencil_vector_length(p);
        {
          uptr p_sz = size_stencil_vector(len);
          si->marked_count += p_sz;
          {
            uptr offset = 0;
            while (offset < p_sz) {
              ptr mark_p = (ptr)((uptr)p + offset);
              si->marked_mask[segment_bitmap_byte(mark_p)] |= segment_bitmap_bit(mark_p);
              offset += byte_alignment;
            }
          }
          push_sweep(p);
        }
      }
      else if (TYPEP(tf, mask_string, type_string))
      {
        uptr sz = size_string((Sstring_length(p)));
        {
          uptr p_sz = sz;
          si->marked_mask[segment_bitmap_byte(p)] |= segment_bitmap_bit(p);
          {
            uptr addr = (uptr)UNTYPE(p, type_typed_object);
            uptr seg = addr_get_segment(addr);
            uptr end_seg = addr_get_segment(addr + p_sz - 1);
            if (seg == end_seg) {
              si->marked_count += p_sz;
            } else {
              seginfo *mark_si; IGEN g;
              si->marked_count += ((uptr)build_ptr(seg+1,0)) - addr;
              seg++;
              while (seg < end_seg) {
                mark_si = SegInfo(seg);
                g = mark_si->generation;
                if (!fully_marked_mask[g]) init_fully_marked_mask(tgc, g);
                mark_si->marked_mask = fully_marked_mask[g];
                mark_si->marked_count = bytes_per_segment;
                seg++;
              }
              mark_si = SegInfo(end_seg);
              {
                if (!mark_si->marked_mask) {
                  init_mask(tgc, mark_si->marked_mask, mark_si->generation, 0);
                }
                /* no need to set a bit: it's enough to have made `marked_mask` non-NULL */
                mark_si->marked_count += addr + p_sz - (uptr)build_ptr(end_seg,0);
              }
            }
          }
        }
      }
      else if (TYPEP(tf, mask_fxvector, type_fxvector))
      {
        uptr sz = size_fxvector((Sfxvector_length(p)));
        {
          uptr p_sz = sz;
          si->marked_mask[segment_bitmap_byte(p)] |= segment_bitmap_bit(p);
          {
            uptr addr = (uptr)UNTYPE(p, type_typed_object);
            uptr seg = addr_get_segment(addr);
            uptr end_seg = addr_get_segment(addr + p_sz - 1);
            if (seg == end_seg) {
              si->marked_count += p_sz;
            } else {
              seginfo *mark_si; IGEN g;
              si->marked_count += ((uptr)build_ptr(seg+1,0)) - addr;
              seg++;
              while (seg < end_seg) {
                mark_si = SegInfo(seg);
                g = mark_si->generation;
                if (!fully_marked_mask[g]) init_fully_marked_mask(tgc, g);
                mark_si->marked_mask = fully_marked_mask[g];
                mark_si->marked_count = bytes_per_segment;
                seg++;
              }
              mark_si = SegInfo(end_seg);
              {
                if (!mark_si->marked_mask) {
                  init_mask(tgc, mark_si->marked_mask, mark_si->generation, 0);
                }
                /* no need to set a bit: it's enough to have made `marked_mask` non-NULL */
                mark_si->marked_count += addr + p_sz - (uptr)build_ptr(end_seg,0);
              }
            }
          }
        }
      }
      else if (TYPEP(tf, mask_flvector, type_flvector))
      {
        uptr sz = size_flvector((Sflvector_length(p)));
        {
          uptr p_sz = sz;
          si->marked_mask[segment_bitmap_byte(p)] |= segment_bitmap_bit(p);
          {
            uptr addr = (uptr)UNTYPE(p, type_typed_object);
            uptr seg = addr_get_segment(addr);
            uptr end_seg = addr_get_segment(addr + p_sz - 1);
            if (seg == end_seg) {
              si->marked_count += p_sz;
            } else {
              seginfo *mark_si; IGEN g;
              si->marked_count += ((uptr)build_ptr(seg+1,0)) - addr;
              seg++;
              while (seg < end_seg) {
                mark_si = SegInfo(seg);
                g = mark_si->generation;
                if (!fully_marked_mask[g]) init_fully_marked_mask(tgc, g);
                mark_si->marked_mask = fully_marked_mask[g];
                mark_si->marked_count = bytes_per_segment;
                seg++;
              }
              mark_si = SegInfo(end_seg);
              {
                if (!mark_si->marked_mask) {
                  init_mask(tgc, mark_si->marked_mask, mark_si->generation, 0);
                }
                /* no need to set a bit: it's enough to have made `marked_mask` non-NULL */
                mark_si->marked_count += addr + p_sz - (uptr)build_ptr(end_seg,0);
              }
            }
          }
        }
      }
      else if (TYPEP(tf, mask_bytevector, type_bytevector))
      {
        {
          ISPC p_at_spc = si->space;
          if (p_at_spc == space_reference_array)
          {
            uptr sz = size_bytevector((Sbytevector_length(p)));
            {
              uptr p_sz = sz;
              {
                uptr addr = (uptr)UNTYPE(p, type_typed_object);
                if (addr_get_segment(addr) == addr_get_segment(addr + p_sz - 1))
                {
                  si->marked_count += p_sz;
                  {
                    uptr offset = 0;
                    while (offset < p_sz) {
                      ptr mark_p = (ptr)((uptr)p + offset);
                      si->marked_mask[segment_bitmap_byte(mark_p)] |= segment_bitmap_bit(mark_p);
                      offset += byte_alignment;
                    }
                  }
                }
                else
                {
                  uptr offset = 0;
                  while (offset < p_sz) {
                    ptr mark_p = (ptr)((uptr)p + offset);
                    seginfo *mark_si = SegInfo(ptr_get_segment(mark_p));
                    if (!mark_si->marked_mask) {
                      init_mask(tgc, mark_si->marked_mask, mark_si->generation, 0);
                    }
                    mark_si->marked_mask[segment_bitmap_byte(mark_p)] |= segment_bitmap_bit(mark_p);
                    mark_si->marked_count += byte_alignment;
                    offset += byte_alignment;
                  }
                }
              }
              push_sweep(p);
            }
          }
          else
          {
            uptr sz = size_bytevector((Sbytevector_length(p)));
            {
              uptr p_sz = sz;
              si->marked_mask[segment_bitmap_byte(p)] |= segment_bitmap_bit(p);
              {
                uptr addr = (uptr)UNTYPE(p, type_typed_object);
                uptr seg = addr_get_segment(addr);
                uptr end_seg = addr_get_segment(addr + p_sz - 1);
                if (seg == end_seg) {
                  si->marked_count += p_sz;
                } else {
                  seginfo *mark_si; IGEN g;
                  si->marked_count += ((uptr)build_ptr(seg+1,0)) - addr;
                  seg++;
                  while (seg < end_seg) {
                    mark_si = SegInfo(seg);
                    g = mark_si->generation;
                    if (!fully_marked_mask[g]) init_fully_marked_mask(tgc, g);
                    mark_si->marked_mask = fully_marked_mask[g];
                    mark_si->marked_count = bytes_per_segment;
                    seg++;
                  }
                  mark_si = SegInfo(end_seg);
                  {
                    if (!mark_si->marked_mask) {
                      init_mask(tgc, mark_si->marked_mask, mark_si->generation, 0);
                    }
                    /* no need to set a bit: it's enough to have made `marked_mask` non-NULL */
                    mark_si->marked_count += addr + p_sz - (uptr)build_ptr(end_seg,0);
                  }
                }
              }
            }
          }
        }
      }
      else if ((iptr)tf == type_tlc)
      {
        uptr p_sz = size_tlc;
        si->marked_count += p_sz;
        {
          ptr mark_p = p;
          si->marked_mask[segment_bitmap_byte(mark_p)] |= segment_bitmap_bit(mark_p);
          mark_p = (ptr)((uptr)mark_p + byte_alignment);
          si->marked_mask[segment_bitmap_byte(mark_p)] |= segment_bitmap_bit(mark_p);
        }
        push_sweep(p);
        {
          ptr next = INITTLCNEXT(p);
          {
            ptr keyval = INITTLCKEYVAL(p);
            if ((next != Sfalse) && (OLDSPACE(keyval)))
            {
              GC_MUTEX_ACQUIRE();
              tlcs_to_rehash = S_cons_in(tgc -> tc, space_new, 0, p, tlcs_to_rehash);
              GC_MUTEX_RELEASE();
            }
          }
        }
      }
      else if (TYPEP(tf, mask_box, type_box))
      {
        uptr p_sz = size_box;
        si->marked_mask[segment_bitmap_byte(p)] |= segment_bitmap_bit(p);
        si->marked_count += p_sz;
        push_sweep(p);
      }
      else if ((iptr)tf == type_ratnum)
      {
        uptr p_sz = size_ratnum;
        relocate_pure(&RATNUM(p));
        relocate_pure(&RATDEN(p));
        si->marked_count += p_sz;
        {
          ptr mark_p = p;
          si->marked_mask[segment_bitmap_byte(mark_p)] |= segment_bitmap_bit(mark_p);
          mark_p = (ptr)((uptr)mark_p + byte_alignment);
          si->marked_mask[segment_bitmap_byte(mark_p)] |= segment_bitmap_bit(mark_p);
        }
        push_sweep(p);
      }
      else if ((iptr)tf == type_exactnum)
      {
        uptr p_sz = size_exactnum;
        relocate_pure(&EXACTNUM_REAL_PART(p));
        relocate_pure(&EXACTNUM_IMAG_PART(p));
        si->marked_count += p_sz;
        {
          ptr mark_p = p;
          si->marked_mask[segment_bitmap_byte(mark_p)] |= segment_bitmap_bit(mark_p);
          mark_p = (ptr)((uptr)mark_p + byte_alignment);
          si->marked_mask[segment_bitmap_byte(mark_p)] |= segment_bitmap_bit(mark_p);
        }
        push_sweep(p);
      }
      else if ((iptr)tf == type_inexactnum)
      {
        uptr p_sz = size_inexactnum;
        si->marked_mask[segment_bitmap_byte(p)] |= segment_bitmap_bit(p);
        si->marked_count += p_sz;
      }
      else if (TYPEP(tf, mask_bignum, type_bignum))
      {
        uptr sz = size_bignum((BIGLEN(p)));
        {
          uptr p_sz = sz;
          si->marked_mask[segment_bitmap_byte(p)] |= segment_bitmap_bit(p);
          {
            uptr addr = (uptr)UNTYPE(p, type_typed_object);
            uptr seg = addr_get_segment(addr);
            uptr end_seg = addr_get_segment(addr + p_sz - 1);
            if (seg == end_seg) {
              si->marked_count += p_sz;
            } else {
              seginfo *mark_si; IGEN g;
              si->marked_count += ((uptr)build_ptr(seg+1,0)) - addr;
              seg++;
              while (seg < end_seg) {
                mark_si = SegInfo(seg);
                g = mark_si->generation;
                if (!fully_marked_mask[g]) init_fully_marked_mask(tgc, g);
                mark_si->marked_mask = fully_marked_mask[g];
                mark_si->marked_count = bytes_per_segment;
                seg++;
              }
              mark_si = SegInfo(end_seg);
              {
                if (!mark_si->marked_mask) {
                  init_mask(tgc, mark_si->marked_mask, mark_si->generation, 0);
                }
                /* no need to set a bit: it's enough to have made `marked_mask` non-NULL */
                mark_si->marked_count += addr + p_sz - (uptr)build_ptr(end_seg,0);
              }
            }
          }
        }
      }
      else if (TYPEP(tf, mask_port, type_port))
      {
        uptr p_sz = size_port;
        si->marked_mask[segment_bitmap_byte(p)] |= segment_bitmap_bit(p);
        si->marked_count += p_sz;
        push_sweep(p);
      }
      else if (TYPEP(tf, mask_code, type_code))
      {
        uptr len = CODELEN(p);
        {
          uptr p_sz = size_code(len);
          si->marked_mask[segment_bitmap_byte(p)] |= segment_bitmap_bit(p);
          {
            uptr addr = (uptr)UNTYPE(p, type_typed_object);
            uptr seg = addr_get_segment(addr);
            uptr end_seg = addr_get_segment(addr + p_sz - 1);
            if (seg == end_seg) {
              si->marked_count += p_sz;
            } else {
              seginfo *mark_si; IGEN g;
              si->marked_count += ((uptr)build_ptr(seg+1,0)) - addr;
              seg++;
              while (seg < end_seg) {
                mark_si = SegInfo(seg);
                g = mark_si->generation;
                if (!fully_marked_mask[g]) init_fully_marked_mask(tgc, g);
                mark_si->marked_mask = fully_marked_mask[g];
                mark_si->marked_count = bytes_per_segment;
                seg++;
              }
              mark_si = SegInfo(end_seg);
              {
                if (!mark_si->marked_mask) {
                  init_mask(tgc, mark_si->marked_mask, mark_si->generation, 0);
                }
                /* no need to set a bit: it's enough to have made `marked_mask` non-NULL */
                mark_si->marked_count += addr + p_sz - (uptr)build_ptr(end_seg,0);
              }
            }
          }
          push_sweep(p);
        }
      }
      else if ((iptr)tf == type_thread)
      {
        uptr p_sz = size_thread;
        si->marked_mask[segment_bitmap_byte(p)] |= segment_bitmap_bit(p);
        si->marked_count += p_sz;
        push_sweep(p);
      }
      else if ((iptr)tf == type_rtd_counts)
      {
        uptr p_sz = size_rtd_counts;
        si->marked_mask[segment_bitmap_byte(p)] |= segment_bitmap_bit(p);
        si->marked_count += p_sz;
      }
      else if ((iptr)tf == type_phantom)
      {
        uptr p_sz = size_phantom;
        si->marked_mask[segment_bitmap_byte(p)] |= segment_bitmap_bit(p);
        si->marked_count += p_sz;
        GC_MUTEX_ACQUIRE();
        (S_G.bytesof[TARGET_GENERATION(si)])[countof_phantom] += PHANTOMLEN(p);
        GC_MUTEX_RELEASE();
      }
      else
      {
        S_error_abort("mark: illegal typed object type");
      }
    }
    else if (t == type_pair)
    {
      {
        ISPC p_at_spc = si->space;
        if (p_at_spc < space_weakpair)
        {
          uptr p_sz = size_pair;
          si->marked_mask[segment_bitmap_byte(p)] |= segment_bitmap_bit(p);
          si->marked_count += p_sz;
          push_sweep(p);
        }
        else if (p_at_spc == space_ephemeron)
        {
          uptr p_sz = size_ephemeron;
          add_ephemeron_to_pending(tgc, p);
          si->marked_mask[segment_bitmap_byte(p)] |= segment_bitmap_bit(p);
          si->marked_count += p_sz;
        }
        else if (p_at_spc == space_weakpair)
        {
          uptr p_sz = size_pair;
          si->marked_mask[segment_bitmap_byte(p)] |= segment_bitmap_bit(p);
          si->marked_count += p_sz;
          push_sweep(p);
        }
        else
        {
          S_error_abort("misplaced pair");
        }
      }
    }
    else if (t == type_closure)
    {
      ptr code = CLOSCODE(p);
      relocate_pure(&code);
      if ((CODETYPE(code)) & (code_flag_continuation << code_flags_offset))
      {
        uptr p_sz = size_continuation;
        if ((CONTLENGTH(p)) == opportunistic_1_shot_flag)
        {
          CONTLENGTH(p) = CONTCLENGTH(p);
          GC_MUTEX_ACQUIRE();
          conts_to_promote = S_cons_in(tgc -> tc, space_new, 0, p, conts_to_promote);
          GC_MUTEX_RELEASE();
        }
        else
        {
        }
        si->marked_mask[segment_bitmap_byte(p)] |= segment_bitmap_bit(p);
        si->marked_count += p_sz;
        push_sweep(p);
      }
      else
      {
        uptr len = CODEFREE(code);
        {
          uptr p_sz = size_closure(len);
          {
            ISPC p_at_spc = si->space;
            if (p_at_spc == space_pure)
            {
              si->marked_mask[segment_bitmap_byte(p)] |= segment_bitmap_bit(p);
              {
                uptr addr = (uptr)UNTYPE(p, type_closure);
                uptr seg = addr_get_segment(addr);
                uptr end_seg = addr_get_segment(addr + p_sz - 1);
                if (seg == end_seg) {
                  si->marked_count += p_sz;
                } else {
                  seginfo *mark_si; IGEN g;
                  si->marked_count += ((uptr)build_ptr(seg+1,0)) - addr;
                  seg++;
                  while (seg < end_seg) {
                    mark_si = SegInfo(seg);
                    g = mark_si->generation;
                    if (!fully_marked_mask[g]) init_fully_marked_mask(tgc, g);
                    mark_si->marked_mask = fully_marked_mask[g];
                    mark_si->marked_count = bytes_per_segment;
                    seg++;
                  }
                  mark_si = SegInfo(end_seg);
                  {
                    if (!mark_si->marked_mask) {
                      init_mask(tgc, mark_si->marked_mask, mark_si->generation, 0);
                    }
                    /* no need to set a bit: it's enough to have made `marked_mask` non-NULL */
                    mark_si->marked_count += addr + p_sz - (uptr)build_ptr(end_seg,0);
                  }
                }
              }
              push_sweep(p);
            }
            else
            {
              {
                uptr addr = (uptr)UNTYPE(p, type_closure);
                if (addr_get_segment(addr) == addr_get_segment(addr + p_sz - 1))
                {
                  si->marked_count += p_sz;
                  {
                    uptr offset = 0;
                    while (offset < p_sz) {
                      ptr mark_p = (ptr)((uptr)p + offset);
                      si->marked_mask[segment_bitmap_byte(mark_p)] |= segment_bitmap_bit(mark_p);
                      offset += byte_alignment;
                    }
                  }
                }
                else
                {
                  uptr offset = 0;
                  while (offset < p_sz) {
                    ptr mark_p = (ptr)((uptr)p + offset);
                    seginfo *mark_si = SegInfo(ptr_get_segment(mark_p));
                    if (!mark_si->marked_mask) {
                      init_mask(tgc, mark_si->marked_mask, mark_si->generation, 0);
                    }
                    mark_si->marked_mask[segment_bitmap_byte(mark_p)] |= segment_bitmap_bit(mark_p);
                    mark_si->marked_count += byte_alignment;
                    offset += byte_alignment;
                  }
                }
              }
              push_sweep(p);
            }
          }
        }
      }
    }
    else if (t == type_symbol)
    {
      uptr p_sz = size_symbol;
      si->marked_mask[segment_bitmap_byte(p)] |= segment_bitmap_bit(p);
      si->marked_count += p_sz;
      push_sweep(p);
    }
    else if (t == type_flonum)
    {
      uptr p_sz = size_flonum;
      si->marked_mask[segment_bitmap_byte(p)] |= segment_bitmap_bit(p);
      si->marked_count += p_sz;
    }
    else
    {
      S_error_abort("mark: illegal type");
    }
  }
  tgc->sweep_change = SWEEP_CHANGE_PROGRESS;
  ADD_BACKREFERENCE(p, si->generation);
  return si->generation;
}

static IBOOL object_directly_refers_to_self(ptr p)
{
  {
    ITYPE t = TYPEBITS(p);
    if (t == type_typed_object)
    {
      ptr tf = TYPEFIELD(p);
      if (TYPEP(tf, mask_record, type_record))
      {
        {
          ptr rtd = RECORDINSTTYPE(p);
          {
            uptr len = UNFIX((RECORDDESCSIZE(rtd)));
            {
              ptr num = RECORDDESCPM(rtd);
              ptr* pp = &(RECORDINSTIT(p, 0));
              if (Sfixnump(num))
              {
                {
                  uptr mask = ((uptr)(UNFIX(num))) >> 1;
                  if (mask == (((uptr)-1) >> 1))
                  {
                    {
                      ptr* ppend = ((ptr*)(TO_VOIDP((((uptr)(TO_PTR(pp))) + len)))) - 1;
                      while (pp < ppend)
                      {
                        if (p == *(pp)) return 1;
                        pp += 1;
                      }
                    }
                  }
                  else
                  {
                    while (mask != 0)
                    {
                      if (mask & 1)
                      {
                        if (p == *(pp)) return 1;
                      }
                      mask >>= 1;
                      pp += 1;
                    }
                  }
                }
              }
              else
              {
                if (p == RECORDDESCPM(rtd)) return 1;
                num = RECORDDESCPM(rtd);
                {
                  iptr index = (BIGLEN(num)) - 1;
                  bigit mask = (BIGIT(num, index)) >> 1;
                  INT bits = bigit_bits - 1;
                  while (1)
                  {
                    do
                    {
                      if (mask & 1)
                      {
                        if (p == *(pp)) return 1;
                      }
                      mask >>= 1;
                      pp += 1;
                      bits -= 1;
                    }
                    while (bits > 0);
                    if (index == 0)
                    {
                      break;
                    }
                    index -= 1;
                    mask = BIGIT(num, index);
                    bits = bigit_bits;
                  }
                }
              }
            }
          }
        }
      }
      else if (TYPEP(tf, mask_vector, type_vector))
      {
        uptr len = Svector_length(p);
        {
          uptr idx, p_len = len;
          ptr *p_p = &INITVECTIT(p, 0);
          for (idx = 0; idx < p_len; idx++)
          {
            if (p == p_p[idx]) return 1;
          }
        }
      }
      else if (TYPEP(tf, mask_any_stencil_vector, type_any_stencil_vector))
      {
        uptr len = Sstencil_vector_length(p);
        {
          uptr idx, p_len = len;
          ptr *p_p = &INITSTENVECTIT(p, 0);
          for (idx = 0; idx < p_len; idx++)
          {
            if (p == p_p[idx]) return 1;
          }
        }
      }
      else if (TYPEP(tf, mask_string, type_string))
      {
      }
      else if (TYPEP(tf, mask_fxvector, type_fxvector))
      {
      }
      else if (TYPEP(tf, mask_flvector, type_flvector))
      {
      }
      else if (TYPEP(tf, mask_bytevector, type_bytevector))
      {
        {
          ISPC p_at_spc = SPACE(p);
          if (p_at_spc == space_reference_array)
          {
            {
              uptr len = Sbytevector_reference_length(p);
              {
                uptr idx, p_len = len;
                ptr *p_p = (ptr*)&BVIT(p, 0);
                for (idx = 0; idx < p_len; idx++)
                {
                  if (p == S_maybe_reference_to_object(p_p[idx])) return 1;
                }
              }
            }
          }
          else
          {
          }
        }
      }
      else if ((iptr)tf == type_tlc)
      {
      }
      else if (TYPEP(tf, mask_box, type_box))
      {
        if (p == INITBOXREF(p)) return 1;
      }
      else if ((iptr)tf == type_ratnum)
      {
      }
      else if ((iptr)tf == type_exactnum)
      {
      }
      else if ((iptr)tf == type_inexactnum)
      {
      }
      else if (TYPEP(tf, mask_bignum, type_bignum))
      {
      }
      else if (TYPEP(tf, mask_port, type_port))
      {
        if (p == PORTINFO(p)) return 1;
      }
      else if (TYPEP(tf, mask_code, type_code))
      {
        {
          ptr t = CODERELOC(p);
          {
            iptr m = (t
                      ? (RELOCSIZE(t))
                      : 0);
            {
              ptr oldco = (t
                           ? (RELOCCODE(t))
                           : 0);
              {
                iptr a = 0;
                {
                  iptr n = 0;
                  while (n < m)
                  {
                    {
                      uptr entry = RELOCIT(t, n);
                      uptr item_off = 0;
                      uptr code_off = 0;
                      n = n + 1;
                      if (RELOC_EXTENDED_FORMAT(entry))
                      {
                        item_off = RELOCIT(t, n);
                        n = n + 1;
                        code_off = RELOCIT(t, n);
                        n = n + 1;
                      }
                      else
                      {
                        item_off = RELOC_ITEM_OFFSET(entry);
                        code_off = RELOC_CODE_OFFSET(entry);
                      }
                      a = a + code_off;
                      {
                        ptr obj = S_get_code_obj(RELOC_TYPE(entry), oldco, a, item_off);
                        if (p == obj) return 1;
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
      else if ((iptr)tf == type_thread)
      {
      }
      else if ((iptr)tf == type_rtd_counts)
      {
      }
      else if ((iptr)tf == type_phantom)
      {
      }
      else
      {
        S_error_abort("self-test: illegal typed object type");
      }
    }
    else if (t == type_pair)
    {
      {
        ISPC p_at_spc = SPACE(p);
        if (p_at_spc < space_weakpair)
        {
          if (p == INITCAR(p)) return 1;
          if (p == INITCDR(p)) return 1;
        }
        else if (p_at_spc == space_ephemeron)
        {
        }
        else if (p_at_spc == space_weakpair)
        {
          if (p == INITCDR(p)) return 1;
        }
        else
        {
          S_error_abort("misplaced pair");
        }
      }
    }
    else if (t == type_closure)
    {
      ptr code = CLOSCODE(p);
      if (p == code) return 1;
      if ((CODETYPE(code)) & (code_flag_continuation << code_flags_offset))
      {
      }
      else
      {
        uptr len = CODEFREE(code);
        {
          uptr idx, p_len = len;
          ptr *p_p = &CLOSIT(p, 0);
          for (idx = 0; idx < p_len; idx++)
          {
            if (p == p_p[idx]) return 1;
          }
        }
      }
    }
    else if (t == type_symbol)
    {
    }
    else if (t == type_flonum)
    {
    }
    else
    {
      S_error_abort("self-test: illegal type");
    }
  }
  return 0;
}

static void mark_untyped_data_object(thread_gc *tgc, ptr p, uptr p_sz, seginfo *si)
{
  if (!si->marked_mask) {
    init_mask(tgc, si->marked_mask, si->generation, 0);
  }
  si->marked_mask[segment_bitmap_byte(p)] |= segment_bitmap_bit(p);
  {
    uptr addr = (uptr)p;
    uptr seg = addr_get_segment(addr);
    uptr end_seg = addr_get_segment(addr + p_sz - 1);
    if (seg == end_seg) {
      si->marked_count += p_sz;
    } else {
      seginfo *mark_si; IGEN g;
      si->marked_count += ((uptr)build_ptr(seg+1,0)) - addr;
      seg++;
      while (seg < end_seg) {
        mark_si = SegInfo(seg);
        g = mark_si->generation;
        if (!fully_marked_mask[g]) init_fully_marked_mask(tgc, g);
        mark_si->marked_mask = fully_marked_mask[g];
        mark_si->marked_count = bytes_per_segment;
        seg++;
      }
      mark_si = SegInfo(end_seg);
      {
        if (!mark_si->marked_mask) {
          init_mask(tgc, mark_si->marked_mask, mark_si->generation, 0);
        }
        /* no need to set a bit: it's enough to have made `marked_mask` non-NULL */
        mark_si->marked_count += addr + p_sz - (uptr)build_ptr(end_seg,0);
      }
    }
  }
}

