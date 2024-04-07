#include "system.h"

#ifdef PORTABLE_BYTECODE
#ifdef ENABLE_LIBFFI

#include "ffi.h"

/* 
   Encoding of a function type:

     #(cached abi fixed-arg-count adjust-active? return-type ret-is-arg? arg-type ...)

   where `cached` is filled with a bytevector that starts as a
   `ffi_cif*` and has all of its associated data, fix-arg-count is 0
   for a non-varrags function, and a type is one of

     - a fixnum for an atomic: ffi_typerep_void, ffi_typerep_uint8, ...
     - a boxed fixnum representing a pointer to an atomic
     - a vector representing a struct (passed by copying)
     - a list of types representing a union
     - a (non-list) pair of a type and a count for an array
*/

# define CACHE_INDEX       0
# define ABI_INDEX         1
# define FIXED_COUNT_INDEX 2
# define ADJ_ACTIVE_INDEX  3
# define RET_TYPE_INDEX    4
# define RET_IS_ARG_INDEX  5
# define ARG_TYPE_START_INDEX 6

typedef struct alloc_state {
  /* to allocate exactly as much as needed in a single bytevector,
     we'll decode in two passes, where the first pass result is discarded
     except for the size */
  iptr alloc_size;
  ptr bv;
} alloc_state;

static ffi_type *decode_type(alloc_state *alloc, ptr type, ffi_abi abi, IBOOL *all_float);
static void *alloc_for_ffi(alloc_state *alloc, iptr sz);
static void closure_callback(ffi_cif *cif, void *ret, void **args, void *user_data);
static void check_prune_callables();

ffi_type *decode_type(alloc_state *alloc, ptr type, ffi_abi abi, IBOOL *all_float) {

  if (Sboxp(type))
    type = Sunbox(type);

  if (Sfixnump(type)) {
    ffi_type *out;
    IBOOL is_float = 0;
    switch(UNFIX(type)) {
    case ffi_typerep_void:
      out = &ffi_type_void;
      break;
    case ffi_typerep_uint8:
      out = &ffi_type_uint8;
      break;
    case ffi_typerep_sint8:
      out = &ffi_type_sint8;
      break;
    case ffi_typerep_uint16:
      out = &ffi_type_uint16;
      break;
    case ffi_typerep_sint16:
      out = &ffi_type_sint16;
      break;
    case ffi_typerep_uint32:
      out = &ffi_type_uint32;
      break;
    case ffi_typerep_sint32:
      out = &ffi_type_sint32;
      break;
    case ffi_typerep_uint64:
      out = &ffi_type_uint64;
      break;
    case ffi_typerep_sint64:
      out = &ffi_type_sint64;
      break;
    case ffi_typerep_float:
      is_float = 1;
      out = &ffi_type_float;
      break;
    case ffi_typerep_double:
      is_float = 1;
      out = &ffi_type_double;
      break;
    default:
      out = &ffi_type_pointer;
      break;
    }
    if (!is_float)
      *all_float = 0;
    return out;
  } else if (Svectorp(type)) {
    /* struct */
    iptr i, len = Svector_length(type);
    ffi_type *out = alloc_for_ffi(alloc, sizeof(ffi_type)), *elem_out;
    ffi_type **elements = (ffi_type **)alloc_for_ffi(alloc, (len+1) * sizeof(ffi_type*));

    for (i = 0; i < len; i++) {
      ptr a = Svector_ref(type, i);
      elem_out = decode_type(alloc, a, abi, all_float);
      elements[i] = elem_out;
    }
    elements[len] = NULL;

    out->size = 0;
    out->alignment = 0;
    out->type = FFI_TYPE_STRUCT;
    out->elements = elements;

    return out;
  } else if (Spairp(type)) {
    ptr rest = Scdr(type);
    if (Spairp(rest) || (rest == Snil)) {
      /* union */

      /* libffi doesn't support union types, so we try to make a
         reasonable approximation. The calling convention of a union type
         mostly likely depends on of the maximum size of all alternatives
         and whether it's floating-point or not. Synthesize a struct that
         is big enough and composed of only floats if the union
         alternative are only floats or integers otherwise. This is not
         guaranteed to be right, but it has a chance at working. */
      IBOOL union_all_float = 1;
      int align = 1;
      size_t sz = 0;
      iptr count;
      ffi_type *out = alloc_for_ffi(alloc, sizeof(ffi_type));
      ffi_type *elem_out, **elements;
      ffi_cif cif;
      
      /* find max required alignment and size: */
      while (type != Snil) {
        elem_out = decode_type(alloc, Scar(type), abi, &union_all_float);

        ffi_prep_cif(&cif, abi, 0, elem_out, NULL);
        if (elem_out->alignment > align)
          align = elem_out->alignment;
        if (elem_out->size > sz)
          sz = elem_out->size;
        
        type = Scdr(type);
      }

      if (!union_all_float)
        *all_float = 0;

      /* round size up to alignment: */
      if ((sz % align) != 0) {
        sz += (align - (sz % align));
      }

      /* Synthesize element list */
      count = 0;
      elements = NULL;
      while (!elements) { /* iterates exactly 2 times */
        if (count)
          elements = (ffi_type **)alloc_for_ffi(alloc, (count+1) * sizeof(ffi_type*));
        count = 0;

        if (!union_all_float) {
          /* build a struct out of integers */
          size_t remain_sz = sz;
          while (remain_sz >= 8) {
            if (elements)
              elements[count] = &ffi_type_sint64;
            remain_sz -= 8;
            count++;
          }
          while (remain_sz >= 4) {
            if (elements)
              elements[count] = &ffi_type_sint32;
            remain_sz -= 4;
            count++;
          }
          while (remain_sz >= 2) {
            if (elements)
              elements[count] = &ffi_type_sint16;
            remain_sz -= 2;
            count++;
          }
          while (remain_sz) {
            if (elements)
              elements[count] = &ffi_type_sint8;
            remain_sz -= 1;
            count++;
          }
          /* remain_sz should be 0 at this point */
        } else {
          /* build a struct out of doubles and floats */
          size_t remain_sz = sz;
          while (remain_sz >= sizeof(double)) {
            if (elements)
              elements[count] = &ffi_type_double;
            remain_sz -= sizeof(double);
            count++;
          }
          while (remain_sz >= sizeof(float)) {
            if (elements)
              elements[count] = &ffi_type_float;
            remain_sz -= sizeof(float);
            count++;
          }
          /* remain_sz should be 0 at this point */
        }
      }

      elements[count] = NULL;

      out->size = sz;
      out->alignment = align;
      out->type = FFI_TYPE_STRUCT;
      out->elements = elements;

      return out;
    } else {
      /* array */
#     if defined(__aarch64__)
#       define SMALL_ARRAY_THRESHOLD 64
#     else
#       define SMALL_ARRAY_THRESHOLD 32
#     endif

      /* libffi doesn't seem to support array types, but we try to make
         libffi work anyway by making a structure type that is used when
         an array appears as a struct field. If the array size is 4 or
         less, or if the total size is SMALL_ARRAY_THRESHOLD bytes or
         less, then we make a full `elements' array, because the x86_64
         ABI always shifts to memory mode after 32 bytes and the AArch64
         ABI shifts after 64 bytes.

         For a non-small element, we still put FFI_TYPE_STRUCT in
         out->type but make an elements array that contains a single
         instance of the element type, which seems to work ok. */
      ffi_type *out = alloc_for_ffi(alloc, sizeof(ffi_type));
      ffi_type *elem_out, **elements;
      ffi_cif cif;
      iptr len;
 
      elem_out = decode_type(alloc, Scar(type), abi, all_float);
      len = UNFIX(Scdr(type));

      ffi_prep_cif(&cif, abi, 0, elem_out, NULL);

      out->size = elem_out->size * len;
      out->alignment = elem_out->alignment;
      out->type = FFI_TYPE_STRUCT;

      if ((out->size <= SMALL_ARRAY_THRESHOLD) || (len <= 4)) {
        iptr i;
        elements = alloc_for_ffi(alloc, (len + 1) * sizeof(ffi_type*));
        for (i = 0; i < len; i++)
          elements[i] = elem_out;
        elements[len] = NULL;
      } else {
        elements = alloc_for_ffi(alloc, 2 * sizeof(ffi_type*));
        elements[0] = elem_out;
        elements[1] = NULL;
      }
      out->elements  = elements;

      return out;
    }
  } else {
    return &ffi_type_pointer;
  }
}

/* detect when an indirect integer size is smaller than the word size */
static unsigned int integer_ptr_size(ptr type) {
  if (Sboxp(type)) {
    type = Sunbox(type);
    if (Sfixnump(type)) {
      switch(UNFIX(type)) {
      case ffi_typerep_uint8:
      case ffi_typerep_sint8:
        return 1;
      case ffi_typerep_uint16:
      case ffi_typerep_sint16:
        return 2;
      case ffi_typerep_uint32:
      case ffi_typerep_sint32:
        return 4;
        break;
      }
    }
  }
  return sizeof(uptr);
}

static void *alloc_for_ffi(alloc_state *alloc, iptr sz) {
  void *result;

  sz = ptr_align(sz);

  if (alloc->alloc_size + sz > Sbytevector_length(alloc->bv))
    alloc->bv = S_bytevector((Sbytevector_length(alloc->bv) + sz) * 2);
  
  result = TO_VOIDP((uptr)TO_PTR(Sbytevector_data(alloc->bv)) + alloc->alloc_size);
  alloc->alloc_size += sz;
  return result;
}

ffi_cif *make_cif(ptr types) {
  ptr cached;
  ffi_abi abi;
  int n_var_req;
  alloc_state alloc;
  ffi_cif *cif;
  ffi_type *ret, **args;
  IBOOL all_float;
  iptr i, len, n_args;

  /* `types` is #(cached abi fixed-arg-count return-type arg-type ...) */

  cached = Svector_ref(types, CACHE_INDEX);
  if (cached != Sfalse)
    return (ffi_cif *)Sbytevector_data(cached);

  len = Svector_length(types);
  n_args = len - ARG_TYPE_START_INDEX;

  abi = UNFIX(Svector_ref(types, ABI_INDEX));
  if (abi == ffi_default_abi)
    abi = FFI_DEFAULT_ABI;
  n_var_req = UNFIX(Svector_ref(types, FIXED_COUNT_INDEX));

  /* first pass to get exact allocation size: */

  alloc.alloc_size = 0;
  alloc.bv = S_bytevector(sizeof(ffi_cif));

  (void)alloc_for_ffi(&alloc, sizeof(ffi_cif));
  (void)alloc_for_ffi(&alloc, n_args * sizeof(ffi_type*));

  all_float = 1;
  (void)decode_type(&alloc, Svector_ref(types, RET_TYPE_INDEX), abi, &all_float);
  
  for (i = 0; i < n_args; i++) {
    all_float = 1;
    (void)decode_type(&alloc, Svector_ref(types, i+ARG_TYPE_START_INDEX), abi, &all_float);
  }

  /* now we know the right size, to allocate as immobile */
  cached = S_bytevector2(get_thread_context(), alloc.alloc_size, space_immobile_data);
  S_immobilize_object(cached);

  alloc.alloc_size = 0;
  alloc.bv = cached;

  cif = alloc_for_ffi(&alloc, sizeof(ffi_cif));
  args = alloc_for_ffi(&alloc, n_args * sizeof(ffi_type*));

  all_float = 1;
  ret = decode_type(&alloc, Svector_ref(types, RET_TYPE_INDEX), abi, &all_float);

  for (i = 0; i < n_args; i++) {
    all_float = 1;
    args[i] = decode_type(&alloc, Svector_ref(types, i+ARG_TYPE_START_INDEX), abi, &all_float);
  }

  if (n_var_req > 0)
    ffi_prep_cif_var(cif, abi, n_var_req, n_args, ret, args);
  else
    ffi_prep_cif(cif, abi, n_args, ret, args);

  Svector_set(types, CACHE_INDEX, cached);

  return cif;
}

void S_ffi_call(ptr types, ptr proc, ptr *arena) {
  ptr *arena_start = arena;
  ffi_cif *cif = make_cif(types);
  iptr len = Svector_length(types), i;
  iptr n_args = len - ARG_TYPE_START_INDEX;
  void *rvalue, **args = TO_VOIDP((uptr)TO_PTR(arena) + ((n_args+1) * 8));
  uptr widened;
  void *actual_rvalue;

  if (sizeof(uptr) != sizeof(ffi_arg))
    S_error("foreign-call", "libffi argument promotion doesn't match ours");

  if (Svector_ref(types, RET_IS_ARG_INDEX) != Sfalse) {
    actual_rvalue = TO_VOIDP(*arena);
    if (integer_ptr_size(Svector_ref(types, RET_TYPE_INDEX)) < sizeof(uptr)) {
      /* libffi promotes the destination type to word size */
      rvalue = &widened;
    } else
      rvalue = actual_rvalue;
    arena++;
  } else {
    actual_rvalue = arena;
    rvalue = actual_rvalue;
  }

  for (i = 0; i < n_args; i++) {
    ptr type = Svector_ref(types, i + ARG_TYPE_START_INDEX);
    if (Sfixnump(type)) {
      args[i] = arena;
      /* adjust arguments that are not ptr-sized or not encoded as doubles/iptrs */
      switch(UNFIX(type)) {
#   ifdef PORTABLE_BYTECODE_BIGENDIAN
      case ffi_typerep_uint8:
      case ffi_typerep_sint8:
        {
          U8 s;
          s = *arena;
          memcpy(arena, &s, sizeof(U8));
        }
        break;
      case ffi_typerep_uint16:
      case ffi_typerep_sint16:
        {
          U16 s;
          s = *arena;
          memcpy(arena, &s, sizeof(U16));
        }
        break;
      case ffi_typerep_uint32:
      case ffi_typerep_sint32:
        {
          U32 s;
          s = *arena;
          memcpy(arena, &s, sizeof(U32));
        }
        break;
#   endif
      case ffi_typerep_uint64:
      case ffi_typerep_sint64:
        if (sizeof(I64) > sizeof(ptr)) {
#         ifdef PORTABLE_BYTECODE_BIGENDIAN
          {
            ptr lo = arena[0];
            arena[0] = arena[1];
            arena[1] = lo;
          }
#         endif
          arena += (sizeof(I64) - sizeof(ptr)) >> log2_ptr_bytes;
        }
        break;
      case ffi_typerep_double:
        arena += (sizeof(double) - sizeof(ptr)) >> log2_ptr_bytes;
        break;
      case ffi_typerep_float:
        {
          float f;
          double d;
          memcpy(&d, arena, sizeof(double));
          f = d;
          memcpy(arena, &f, sizeof(float));
        }
        arena += (sizeof(double) - sizeof(ptr)) >> log2_ptr_bytes;
        break;
      }
    } else
      args[i] = *(void **)arena;
    arena++;
  }

#ifdef PTHREADS
  if (Svector_ref(types, ADJ_ACTIVE_INDEX) != Sfalse) {
    Slock_object(types);
    Sdeactivate_thread();
  }
#endif

  ffi_call(cif, TO_VOIDP(proc), rvalue, args);

#ifdef PTHREADS
  if (Svector_ref(types, ADJ_ACTIVE_INDEX) != Sfalse) {
    (void)S_activate_thread();
    Sunlock_object(types);
  }
#endif

  /* fix up result for certain types: */
  {
    ptr ret_type = Svector_ref(types, RET_TYPE_INDEX);
    if (Sfixnump(ret_type)) {
      /* adjust arguments that are not encoded as iptrs or doubles,
         and also handle smaller-than-word results promoted by libffi;
         libffi appears to promote the size but not the sign on some
         platforms */
      switch(UNFIX(ret_type)) {
      case ffi_typerep_sint8:
        *(iptr *)arena_start = (I8)*(iptr *)arena_start;
        break;
      case ffi_typerep_uint8:
        *(uptr *)arena_start = (U8)*(uptr *)arena_start;
        break;
      case ffi_typerep_sint16:
        *(iptr *)arena_start = (I16)*(iptr *)arena_start;
        break;
      case ffi_typerep_uint16:
        *(uptr *)arena_start = (U16)*(uptr *)arena_start;
        break;
      case ffi_typerep_sint32:
        *(iptr *)arena_start = (I32)*(iptr *)arena_start;
        break;
      case ffi_typerep_uint32:
        *(uptr *)arena_start = (U32)*(uptr *)arena_start;
        break;
      case ffi_typerep_uint64:
      case ffi_typerep_sint64:
        if (sizeof(I64) > sizeof(ptr)) {
#         ifdef PORTABLE_BYTECODE_BIGENDIAN
          {
            ptr lo = arena_start[0];
            arena_start[0] = arena_start[1];
            arena_start[1] = lo;
          }
#         endif
        }
        break;
      case ffi_typerep_float:
        {
          float f;
          double d;
          memcpy(&f, arena_start, sizeof(float));
          d = f;
          memcpy(arena_start, &d, sizeof(double));
        }
        break;
      }
    } else if (Sboxp(ret_type) && (actual_rvalue != rvalue)) {
      /* handle smaller-than-word results promoted by libffi */
      ptr in_type = Sunbox(ret_type);
      if (Sfixnump(in_type)) {
        switch(UNFIX(in_type)) {
        case ffi_typerep_uint8:
        case ffi_typerep_sint8:
          if (actual_rvalue != rvalue)
            *(U8 *)actual_rvalue = widened;
          break;
        case ffi_typerep_uint16:
        case ffi_typerep_sint16:
          if (actual_rvalue != rvalue)
            *(U16 *)actual_rvalue = widened;
          break;
        case ffi_typerep_uint32:
        case ffi_typerep_sint32:
          if (actual_rvalue != rvalue)
            *(U32 *)actual_rvalue = widened;
          break;
        }
      }
    }
  }
}

ptr S_ffi_closure(ptr types, ptr proc) {
  ffi_cif *cif = make_cif(types);
  ffi_closure *closure;
  ptr vec;
  void *code;

  closure = ffi_closure_alloc(sizeof(ffi_closure), &code);

  if (!Sfixnump(TO_PTR(closure)) || !Sfixnump(TO_PTR(code)))
    S_error("foreign-callable", "libffi code allocation not sufficiently aligned");

  vec = S_vector_in(get_thread_context(), space_immobile_impure, 0, 3);
  S_immobilize_object(vec);

  Svector_set(vec, 0, proc);
  Svector_set(vec, 1, types);
  Svector_set(vec, 2, TO_PTR(code));
  
  ffi_prep_closure_loc(closure, cif, closure_callback, TO_VOIDP(vec), code);

  tc_mutex_acquire();
  S_G.foreign_callables = Scons(S_weak_cons(vec, Scons(TO_PTR(closure),
                                                       TO_PTR(code))),
                                S_G.foreign_callables);
  check_prune_callables();
  tc_mutex_release();
  
  return vec;
}

static void closure_callback(UNUSED ffi_cif *cif, void *ret, void **args, void *user_data) {
  ptr caller_saved[4]; /* first four registers are preserved */
  ptr vec = TO_PTR(user_data);
  ptr types = Svector_ref(vec, 1), type;
  ptr tc;
  ptr *arena_start, *arena;
  iptr len = Svector_length(types), i;
  iptr n_args = len - ARG_TYPE_START_INDEX;
  IBOOL ret_is_arg;
#ifdef PTHREADS
  int active_state;
#endif

#ifdef PTHREADS
  if (Svector_ref(types, ADJ_ACTIVE_INDEX) != Sfalse)
    active_state = S_activate_thread();
  else
    active_state = 0;
#endif

  tc = get_thread_context();
  arena_start = S_get_call_arena(tc);
  arena = arena_start;

  if (Svector_ref(types, RET_IS_ARG_INDEX) != Sfalse) {
    *arena = TO_PTR(ret);
    arena++;
    ret_is_arg = 1;
  } else
    ret_is_arg = 0;

  /* Move args in `args` to "arena" space */
  for (i = 0; i < n_args; i++) {
    type = Svector_ref(types, i + ARG_TYPE_START_INDEX);
    if (Sfixnump(type)) {
      switch(UNFIX(type)) {
      case ffi_typerep_uint8:
        *arena = (ptr)*(U8 *)args[i];
        break;
      case ffi_typerep_sint8:
        *arena = (ptr)*(I8 *)args[i];
        break;
      case ffi_typerep_uint16:
        *arena = (ptr)*(U16 *)args[i];
        break;
      case ffi_typerep_sint16:
        *arena = (ptr)*(I16 *)args[i];
        break;
      case ffi_typerep_uint32:
        *arena = (ptr)*(U32 *)args[i];
        break;
      case ffi_typerep_sint32:
        *arena = (ptr)*(I32 *)args[i];
        break;
      case ffi_typerep_uint64:
        if (sizeof(U64) > sizeof(ptr)) {
          arena[1] = (ptr)((*(U64 *)args[i]) >> 32);
          arena[0] = (ptr)*(U64 *)args[i];
          arena++;
        } else
          *arena = *(U64*)args[i];
        break;
      case ffi_typerep_sint64:
        if (sizeof(I64) > sizeof(ptr)) {
          arena[1] = (ptr)((*(I64 *)args[i]) >> 32);
          arena[0] = (ptr)*(I64 *)args[i];
          arena++;
        } else
          *arena = *(I64*)args[i];
        break;
      case ffi_typerep_float:
        *(double *)arena = *(float *)args[i];
        if (sizeof(double) > sizeof(ptr))
          arena++;
        break;
      case ffi_typerep_double:
        *(double *)arena = *(double *)(args[i]);
        if (sizeof(double) > sizeof(ptr))
          arena++;
        break;
      default:
        *arena = *(ptr *)args[i];
        break;
      }
    } else {
      /* all boxed or compound values are passed as an address */
      *arena = TO_PTR(args[i]);
    }
    arena++;
  }

  memcpy(caller_saved, &PBREGS(tc, 0), sizeof(caller_saved));

  S_generic_invoke(tc, Svector_ref(vec, 0));

  memcpy(&PBREGS(tc, 0), caller_saved, sizeof(caller_saved));

  if (!ret_is_arg) {
    /* move result to "arena" */
    type = Svector_ref(types, RET_TYPE_INDEX);

    if (Sfixnump(type)) {
      /* we don't need to handle promotion of int sizes,
         because pb already does that */
      switch(UNFIX(type)) {
      case ffi_typerep_uint64:
      case ffi_typerep_sint64:
        if (sizeof(U64) > sizeof(ptr)) {
#        ifdef PORTABLE_BYTECODE_BIGENDIAN
          ((U32 *)ret)[1] = arena_start[0];
          ((U32 *)ret)[0] = arena_start[1];
#        else
          ((U32 *)ret)[0] = arena_start[0];
          ((U32 *)ret)[1] = arena_start[1];
#        endif
        } else {
          *(ptr *)ret = *arena_start;
        }
        break;
      case ffi_typerep_float:
        *(float *)ret = *(double *)arena_start;
        break;
      case ffi_typerep_double:
        *(double *)ret = *(double *)arena_start;
        break;
      default:
        *(ptr *)ret = *arena_start;
        break;
      }
    } else {
      *(ptr *)ret = *arena_start;
    }
  } else {
#   ifdef PORTABLE_BYTECODE_BIGENDIAN
    type = Svector_ref(types, RET_TYPE_INDEX);
    if (Sboxp(type)) {
      /* match promotion as expected by libffi */
      ptr in_type = Sunbox(type);
      if (Sfixnump(in_type)) {
        switch(UNFIX(in_type)) {
        case ffi_typerep_sint8:
          {
            I8 v;
            memcpy(&v, ret, sizeof(I8));
            *(iptr *)ret = v;
          }
          break;
        case ffi_typerep_uint8:
          {
            U8 v;
            memcpy(&v, ret, sizeof(U8));
            *(uptr *)ret = v;
          }
          break;
        case ffi_typerep_sint16:
          {
            I16 v;
            memcpy(&v, ret, sizeof(I16));
            *(iptr *)ret = v;
          }
          break;
        case ffi_typerep_uint16:
          {
            U16 v;
            memcpy(&v, ret, sizeof(U16));
            *(uptr *)ret = v;
          }
          break;
        case ffi_typerep_sint32:
          {
            I32 v;
            memcpy(&v, ret, sizeof(I32));
            *(iptr *)ret = v;
          }
          break;
        case ffi_typerep_uint32:
          {
            U32 v;
            memcpy(&v, ret, sizeof(U32));
            *(uptr *)ret = v;
          }
          break;
        }
      }
    }
#endif
  }

#ifdef PTHREADS
  if (Svector_ref(types, ADJ_ACTIVE_INDEX) != Sfalse)
    S_unactivate_thread(active_state);
#endif
}

ptr Sforeign_callable_code_object(void *addr) {
  ptr p, result = Sfalse;

  tc_mutex_acquire();

  p = S_G.foreign_callables;

  while (p != Snil) {
    ptr a = Scar(p);
    if (Scdr(Scdr(a)) == TO_PTR(addr)) {
      result = Scar(a);
      break;
    }
    p = Scdr(p);
  }

  tc_mutex_release();

  return result;
}

/* called with tc mutex */
void check_prune_callables() {
  /* after every doubling of the callables list, check whether
     we need to prune after weak references are gone */
  if (S_G.foreign_callables_fuel <= 0) {
    ptr p = S_G.foreign_callables, prev = (ptr)0;
    iptr count = 0;

    while (p != Snil) {
      ptr a = Scar(p);
      if (Scar(a) == Sbwp_object) {
        ffi_closure_free(TO_VOIDP(Scar(Scdr(a))));
        p = Scdr(p);
        if (prev == (ptr)0)
          S_G.foreign_callables = p;
        else
          Sset_cdr(prev, p);
      } else {
        prev = p;
        p = Scdr(p);
        count++;
      }
    }
    S_G.foreign_callables_fuel = count;
  }

  S_G.foreign_callables_fuel--;
}

#else

/* libffi disabled */

void S_ffi_call(UNUSED ptr types, UNUSED ptr proc, UNUSED ptr *arena) {
  S_error("foreign-procedure", "protocol not supported (libffi unavailable)");
}

ptr S_ffi_closure(UNUSED ptr types, UNUSED ptr proc) {
  S_error("foreign-callable", "not supported (libffi unavailable)");
}

ptr Sforeign_callable_code_object(UNUSED void *addr) {
  return Sfalse;
}

#endif
#endif
