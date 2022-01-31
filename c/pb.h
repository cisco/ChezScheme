/* Interpreter for portable bytecode. See "pb.ss". */

/* Machine state is in the thread context: */
typedef struct machine_state {
  ptr machine_regs[pb_reg_count];
  double machine_fpregs[pb_fpreg_count];
  /* scratch space for libffi-based foreign calls, 
     somewhat analogous to the C stack: */
  ptr machine_call_arena[pb_call_arena_size];
} machine_state;

#define regs       (ms->machine_regs)
#define fpregs     (ms->machine_fpregs)
#define call_arena (ms->machine_call_arena)

/* The flag register doesn't have to be in the thread context, because
   it set and then used only in the next instruction. */

/* All instructions are 32 bits wide: */
typedef uint32_t instruction_t;

#define INSTR_op(instr)       ((instr) & 0xFF)

#define INSTR_d_dest(instr)   (((instr) >> 8) & 0xF)

#define INSTR_dr_dest(instr)  INSTR_d_dest(instr)
#define INSTR_dr_reg(instr)   (((instr) >> 16) & 0xF)

#define INSTR_di_dest(instr)  INSTR_d_dest(instr)
#define INSTR_di_imm(instr)   (((int32_t)(instr)) >> 16)
#define INSTR_di_imm_unsigned(instr) ((instr) >> 16)

#define INSTR_adr_dest(instr) INSTR_di_dest(instr)
#define INSTR_adr_imm(instr)  (((int32_t)(instr)) >> 12)

#define INSTR_drr_dest(instr) INSTR_d_dest(instr)
#define INSTR_drr_reg1(instr) (((instr) >> 12) & 0xF)
#define INSTR_drr_reg2(instr) (((instr) >> 16) & 0xF)

#define INSTR_dri_dest(instr) INSTR_d_dest(instr)
#define INSTR_dri_reg(instr)  (((instr) >> 12) & 0xF)
#define INSTR_dri_imm(instr)  (((int32_t)(instr)) >> 16)

#define INSTR_i_imm(instr)    (((int32_t)(instr)) >> 8)

#define INSTR_ii_low(instr)   (((instr) >> 8) & 0xFF)
#define INSTR_ii_high(instr)  ((instr) >> 16)

#define SHIFT_MASK(v) ((v) & (ptr_bits-1))

enum {
   Cretval = 9,
   Carg1 = 9,
   Carg2,
   Carg3,
   Carg4,
   Carg5,
   Carg6,
   Carg7
};

enum {
   Cfpretval = 1,
   Cfparg1 = 1,
   Cfparg2,
   Cfparg3,
   Cfparg4,
   Cfparg5,
   Cfparg6
};

#define SIGN_FLIP(r, a, b) ((~((a ^ b) | (r ^ ~b))) >> (ptr_bits-1))

#if (__GNUC__ >= 5) || defined(__clang__)
# define USE_OVERFLOW_INTRINSICS 1
#else
# define USE_OVERFLOW_INTRINSICS 0
#endif

/* Use `machine_state * RESTRICT_PTR`, because machine registers won't
   be modified in any way other than through the machine-state pointer */

#if (__GNUC__ >= 4) || defined(__clang__)
# define RESTRICT_PTR __restrict__
#endif

#ifdef _MSC_VER
# define RESTRICT_PTR __restrict
#endif

#ifndef RESTRICT_PTR
/* `restrict` is available in C99 and later, but we stick to C89 for now: */
# define RESTRICT_PTR /* restrict */
#endif

/* ********************************************************************** */
/* Implementations for instructions that can be used either within the
   interpreter loop or within a generated chunk. */

#define do_pb_mov16_pb_zero_bits_pb_shift0(instr) \
  regs[INSTR_di_dest(instr)] = (uptr)INSTR_di_imm_unsigned(instr)

#define do_pb_mov16_pb_zero_bits_pb_shift1(instr) \
  regs[INSTR_di_dest(instr)] = (uptr)INSTR_di_imm_unsigned(instr) << 16

#if ptr_bits == 64      
# define do_pb_mov16_pb_zero_bits_pb_shift2(instr) \
  regs[INSTR_di_dest(instr)] = (uptr)INSTR_di_imm_unsigned(instr) << 32
#else
# define do_pb_mov16_pb_zero_bits_pb_shift2(instr) \
  regs[INSTR_di_dest(instr)] = 0
#endif

#if ptr_bits == 64      
# define do_pb_mov16_pb_zero_bits_pb_shift3(instr) \
  regs[INSTR_di_dest(instr)] = (uptr)INSTR_di_imm_unsigned(instr) << 48
#else
# define do_pb_mov16_pb_zero_bits_pb_shift3(instr) \
  regs[INSTR_di_dest(instr)] = 0
#endif

#define do_pb_mov16_pb_keep_bits_pb_shift0(instr) \
  regs[INSTR_di_dest(instr)] |= (uptr)INSTR_di_imm_unsigned(instr)

#define do_pb_mov16_pb_keep_bits_pb_shift1(instr) \
  regs[INSTR_di_dest(instr)] |= (uptr)INSTR_di_imm_unsigned(instr) << 16

#if ptr_bits == 64      
# define do_pb_mov16_pb_keep_bits_pb_shift2(instr) \
  regs[INSTR_di_dest(instr)] |= (uptr)INSTR_di_imm_unsigned(instr) << 32
#else
# define do_pb_mov16_pb_keep_bits_pb_shift2(instr) \
  do { } while (0)
#endif

#if ptr_bits == 64      
# define do_pb_mov16_pb_keep_bits_pb_shift3(instr) \
  regs[INSTR_di_dest(instr)] |= (uptr)INSTR_di_imm_unsigned(instr) << 48
#else
# define do_pb_mov16_pb_keep_bits_pb_shift3(instr) \
  do { } while (0)
#endif

#define do_pb_mov_pb_i_i(instr) \
  regs[INSTR_dr_dest(instr)] = regs[INSTR_dr_reg(instr)]

#define do_pb_mov_pb_d_d(instr) \
  fpregs[INSTR_dr_dest(instr)] = fpregs[INSTR_dr_reg(instr)]

#define do_pb_mov_pb_i_d(instr) \
  fpregs[INSTR_dr_dest(instr)] = (double)(iptr)regs[INSTR_dr_reg(instr)]

#define do_pb_mov_pb_d_i(instr) \
  regs[INSTR_dr_dest(instr)] = (iptr)fpregs[INSTR_dr_reg(instr)]

#if ptr_bits == 64
# define do_pb_mov_pb_i_bits_d_bits(instr) \
  memcpy(&fpregs[INSTR_dr_dest(instr)], &regs[INSTR_dr_reg(instr)], sizeof(double))
# define do_pb_mov_pb_d_bits_i_bits(instr) \
  memcpy(&regs[INSTR_dr_dest(instr)], &fpregs[INSTR_dr_reg(instr)], sizeof(double))
#else
# define do_pb_mov_pb_i_i_bits_d_bits(instr)                            \
  do {                                                                  \
    uint64_t d;                                                         \
    d = regs[INSTR_drr_reg1(instr)] | ((uint64_t)regs[INSTR_drr_reg2(instr)] << 32); \
    memcpy(&fpregs[INSTR_drr_dest(instr)], &d, sizeof(double));         \
  } while (0)
# define do_pb_mov_pb_d_lo_bits_i_bits(instr)                     \
  do {                                                            \
    uint64_t d;                                                   \
    memcpy(&d, &fpregs[INSTR_dr_reg(instr)], sizeof(double));     \
    regs[INSTR_dr_dest(instr)] = d;                               \
  } while (0)
#define do_pb_mov_pb_d_hi_bits_i_bits(instr)                      \
  do {                                                            \
    uint64_t d;                                                   \
    memcpy(&d, &fpregs[INSTR_dr_reg(instr)], sizeof(double));     \
    d >>= 32;                                                     \
    regs[INSTR_dr_dest(instr)] = d;                               \
  } while (0)
#endif

#ifdef PORTABLE_BYTECODE_BIGENDIAN
# define FP_REG_FLOAT_START(p) ((char *)&(p) + 4)
#else
# define FP_REG_FLOAT_START(p) &(p)
#endif

#define do_pb_mov_pb_s_d(instr)                                         \
  do {                                                                  \
    float f;                                                            \
    memcpy(&f, FP_REG_FLOAT_START(fpregs[INSTR_dr_reg(instr)]), sizeof(float)); \
    fpregs[INSTR_dr_dest(instr)] = f;                                   \
  } while (0)

#define do_pb_mov_pb_d_s(instr)                                         \
  do {                                                                  \
    float f;                                                            \
    f = fpregs[INSTR_dr_reg(instr)];                                    \
    memcpy(FP_REG_FLOAT_START(fpregs[INSTR_dr_dest(instr)]), &f, sizeof(float)); \
  } while (0)

#define do_pb_mov_pb_d_s_d(instr)                 \
  do {                                            \
    float f;                                      \
    f = fpregs[INSTR_dr_reg(instr)];              \
    fpregs[INSTR_dr_dest(instr)] = (double)f;     \
  } while (0)

#define do_pb_bin_op_pb_no_signal_pb_add_pb_register(instr) \
  regs[INSTR_drr_dest(instr)] = regs[INSTR_drr_reg1(instr)] + regs[INSTR_drr_reg2(instr)]

#define do_pb_bin_op_pb_no_signal_pb_add_pb_immediate(instr) \
  regs[INSTR_dri_dest(instr)] = regs[INSTR_dri_reg(instr)] + (uptr)INSTR_dri_imm(instr)

#define do_pb_bin_op_pb_no_signal_pb_sub_pb_register(instr) \
  regs[INSTR_drr_dest(instr)] = regs[INSTR_drr_reg1(instr)] - regs[INSTR_drr_reg2(instr)]

#define do_pb_bin_op_pb_no_signal_pb_sub_pb_immediate(instr) \
  regs[INSTR_dri_dest(instr)] = regs[INSTR_dri_reg(instr)] - (uptr)INSTR_dri_imm(instr)

#define do_pb_bin_op_pb_no_signal_pb_mul_pb_register(instr) \
  regs[INSTR_drr_dest(instr)] = regs[INSTR_drr_reg1(instr)] * regs[INSTR_drr_reg2(instr)]

#define do_pb_bin_op_pb_no_signal_pb_mul_pb_immediate(instr) \
  regs[INSTR_dri_dest(instr)] = (uptr)regs[INSTR_dri_reg(instr)] * (uptr)INSTR_dri_imm(instr)

#define do_pb_bin_op_pb_no_signal_pb_div_pb_register(instr) \
  regs[INSTR_drr_dest(instr)] = (iptr)regs[INSTR_drr_reg1(instr)] / (iptr)regs[INSTR_drr_reg2(instr)]

#define do_pb_bin_op_pb_no_signal_pb_div_pb_immediate(instr) \
  regs[INSTR_dri_dest(instr)] = (iptr)regs[INSTR_dri_reg(instr)] / (iptr)INSTR_dri_imm(instr)

#define do_pb_bin_op_pb_no_signal_pb_and_pb_register(instr) \
  regs[INSTR_drr_dest(instr)] = regs[INSTR_drr_reg1(instr)] & regs[INSTR_drr_reg2(instr)]

#define do_pb_bin_op_pb_no_signal_pb_and_pb_immediate(instr) \
  regs[INSTR_dri_dest(instr)] = regs[INSTR_dri_reg(instr)] & (uptr)INSTR_dri_imm(instr)

#define do_pb_bin_op_pb_no_signal_pb_ior_pb_register(instr) \
  regs[INSTR_drr_dest(instr)] = regs[INSTR_drr_reg1(instr)] | regs[INSTR_drr_reg2(instr)]

#define do_pb_bin_op_pb_no_signal_pb_ior_pb_immediate(instr) \
  regs[INSTR_dri_dest(instr)] = regs[INSTR_dri_reg(instr)] | (uptr)INSTR_dri_imm(instr)

#define do_pb_bin_op_pb_no_signal_pb_xor_pb_register(instr) \
  regs[INSTR_drr_dest(instr)] = regs[INSTR_drr_reg1(instr)] ^ regs[INSTR_drr_reg2(instr)]

#define do_pb_bin_op_pb_no_signal_pb_xor_pb_immediate(instr) \
  regs[INSTR_dri_dest(instr)] = regs[INSTR_dri_reg(instr)] ^ (uptr)INSTR_dri_imm(instr)

#define do_pb_bin_op_pb_no_signal_pb_lsl_pb_register(instr) \
  regs[INSTR_drr_dest(instr)] = regs[INSTR_drr_reg1(instr)] << SHIFT_MASK(regs[INSTR_drr_reg2(instr)])

#define do_pb_bin_op_pb_no_signal_pb_lsl_pb_immediate(instr) \
  regs[INSTR_dri_dest(instr)] = regs[INSTR_dri_reg(instr)] << SHIFT_MASK(INSTR_dri_imm(instr))

#define do_pb_bin_op_pb_no_signal_pb_lsr_pb_register(instr) \
  regs[INSTR_drr_dest(instr)] = regs[INSTR_drr_reg1(instr)] >> SHIFT_MASK(regs[INSTR_drr_reg2(instr)])

#define do_pb_bin_op_pb_no_signal_pb_lsr_pb_immediate(instr) \
  regs[INSTR_dri_dest(instr)] = regs[INSTR_dri_reg(instr)] >> SHIFT_MASK(INSTR_dri_imm(instr))

#define do_pb_bin_op_pb_no_signal_pb_asr_pb_register(instr) \
  regs[INSTR_drr_dest(instr)] = (iptr)regs[INSTR_drr_reg1(instr)] >> SHIFT_MASK(regs[INSTR_drr_reg2(instr)])

#define do_pb_bin_op_pb_no_signal_pb_asr_pb_immediate(instr) \
  regs[INSTR_dri_dest(instr)] = (iptr)regs[INSTR_dri_reg(instr)] >> SHIFT_MASK(INSTR_dri_imm(instr))

#ifdef PORTABLE_BYTECODE_BIGENDIAN
# define do_pb_bin_op_pb_no_signal_pb_lslo_pb_register(instr) \
  regs[INSTR_drr_dest(instr)] = regs[INSTR_drr_reg1(instr)] >> regs[INSTR_drr_reg2(instr)]
#else
# define do_pb_bin_op_pb_no_signal_pb_lslo_pb_register(instr) \
  regs[INSTR_drr_dest(instr)] = regs[INSTR_drr_reg1(instr)] << regs[INSTR_drr_reg2(instr)]
#endif

#ifdef PORTABLE_BYTECODE_BIGENDIAN
# define do_pb_bin_op_pb_no_signal_pb_lslo_pb_immediate(instr) \
  regs[INSTR_dri_dest(instr)] = regs[INSTR_dri_reg(instr)] >> INSTR_dri_imm(instr)
#else
# define do_pb_bin_op_pb_no_signal_pb_lslo_pb_immediate(instr) \
  regs[INSTR_dri_dest(instr)] = regs[INSTR_dri_reg(instr)] << INSTR_dri_imm(instr);
#endif

#if USE_OVERFLOW_INTRINSICS
# define do_pb_bin_op_pb_signal_pb_add_pb_register(instr) \
  do {                                                    \
    iptr a = (iptr)regs[INSTR_drr_reg1(instr)];           \
    iptr b = (iptr)regs[INSTR_drr_reg2(instr)];           \
    iptr r;                                               \
    flag = __builtin_add_overflow(a, b, &r);              \
    regs[INSTR_drr_dest(instr)] = (uptr)r;                \
  } while (0)
#else
# define do_pb_bin_op_pb_signal_pb_add_pb_register(instr) \
  do {                                                    \
    uptr a = regs[INSTR_drr_reg1(instr)];                 \
    uptr b = regs[INSTR_drr_reg2(instr)];                 \
    uptr r = a + b;                                       \
    regs[INSTR_drr_dest(instr)] = r;                      \
    flag = SIGN_FLIP(r, a, b);                            \
  } while (0)
#endif

#if USE_OVERFLOW_INTRINSICS
# define do_pb_bin_op_pb_signal_pb_add_pb_immediate(instr)       \
  do {                                                           \
    iptr a = (iptr)regs[INSTR_dri_reg(instr)];                   \
    iptr b = INSTR_dri_imm(instr);                               \
    iptr r;                                                      \
    flag = __builtin_add_overflow(a, b, &r);                     \
    regs[INSTR_drr_dest(instr)] = (uptr)r;                       \
  } while (0)
#else
# define do_pb_bin_op_pb_signal_pb_add_pb_immediate(instr)       \
  do {                                                           \
    uptr a = regs[INSTR_dri_reg(instr)];                         \
    uptr b = (uptr)INSTR_dri_imm(instr);                         \
    uptr r = a + b;                                              \
    regs[INSTR_dri_dest(instr)] = r;                             \
    flag = SIGN_FLIP(r, a, b);                                   \
  } while (0)
#endif

#if USE_OVERFLOW_INTRINSICS
#define do_pb_bin_op_pb_signal_pb_sub_pb_register(instr)        \
  do {                                                          \
    iptr a = (iptr)regs[INSTR_drr_reg1(instr)];                 \
    iptr b = (iptr)regs[INSTR_drr_reg2(instr)];                 \
    iptr r;                                                     \
    flag = __builtin_sub_overflow(a, b, &r);                    \
    regs[INSTR_drr_dest(instr)] = (uptr)r;                      \
  } while (0)
#else
#define do_pb_bin_op_pb_signal_pb_sub_pb_register(instr)        \
  do {                                                          \
    uptr a = regs[INSTR_drr_reg1(instr)];                       \
    uptr b = regs[INSTR_drr_reg2(instr)];                       \
    uptr r = a - b;                                             \
    regs[INSTR_drr_dest(instr)] = r;                            \
    flag = SIGN_FLIP(r, a, ~b);                                 \
  } while (0)
#endif

#if USE_OVERFLOW_INTRINSICS
# define do_pb_bin_op_pb_signal_pb_sub_pb_immediate(instr) \
  do {                                                     \
    iptr a = (iptr)regs[INSTR_dri_reg(instr)];             \
    iptr b = INSTR_dri_imm(instr);                         \
    iptr r;                                                \
    flag = __builtin_sub_overflow(a, b, &r);               \
    regs[INSTR_drr_dest(instr)] = (uptr)r;                 \
  } while (0)
#else
# define do_pb_bin_op_pb_signal_pb_sub_pb_immediate(instr) \
  do {                                                     \
    uptr a = regs[INSTR_dri_reg(instr)];                   \
    uptr b = (uptr)INSTR_dri_imm(instr);                   \
    uptr r = a - b;                                        \
    regs[INSTR_dri_dest(instr)] = r;                       \
    flag = SIGN_FLIP(r, a, ~b);                            \
  } while (0)
#endif

#if USE_OVERFLOW_INTRINSICS
#define do_pb_bin_op_pb_signal_pb_mul_pb_register(instr)        \
  do {                                                          \
    iptr a = (iptr)regs[INSTR_drr_reg1(instr)];                 \
    iptr b = (iptr)regs[INSTR_drr_reg2(instr)];                 \
    iptr r;                                                     \
    flag = __builtin_mul_overflow(a, b, &r);                    \
    regs[INSTR_drr_dest(instr)] = (uptr)r;                      \
  } while (0)
#else
#define do_pb_bin_op_pb_signal_pb_mul_pb_register(instr)        \
  do {                                                          \
  uptr a = regs[INSTR_drr_reg1(instr)];                         \
  uptr b = regs[INSTR_drr_reg2(instr)];                         \
  uptr r = a * b;                                               \
  regs[INSTR_drr_dest(instr)] = r;                              \
  if (b != 0) {                                                 \
    if (b == (uptr)-1)                                          \
      flag = (a != r * (uptr)-1);                               \
    else                                                        \
      flag = ((iptr)a != (iptr)r / (iptr)b);                    \
  } else                                                        \
    flag = 0;                                                   \
  } while (0)
#endif

#if USE_OVERFLOW_INTRINSICS
# define do_pb_bin_op_pb_signal_pb_mul_pb_immediate(instr) \
  do {                                                     \
    iptr a = (iptr)regs[INSTR_dri_reg(instr)];             \
    iptr b = INSTR_dri_imm(instr);                         \
    iptr r;                                                \
    flag = __builtin_mul_overflow(a, b, &r);               \
    regs[INSTR_drr_dest(instr)] = (uptr)r;                 \
  } while (0)
#else
# define do_pb_bin_op_pb_signal_pb_mul_pb_immediate(instr) \
  do {                                                     \
    uptr a = regs[INSTR_dri_reg(instr)];                   \
    uptr b = (uptr)INSTR_dri_imm(instr);                   \
    uptr r = a * b;                                        \
    regs[INSTR_dri_dest(instr)] = r;                       \
    if (b != 0) {                                          \
      if (b == (uptr)-1)                                   \
        flag = (a != r * (uptr)-1);                        \
      else                                                 \
        flag = ((iptr)a != (iptr)r / (iptr)b);             \
    } else                                                 \
      flag = 0;                                            \
  } while (0)
#endif

#define do_pb_bin_op_pb_signal_pb_subz_pb_register(instr)               \
  do {                                                                  \
    iptr r = regs[INSTR_drr_reg1(instr)] - regs[INSTR_drr_reg2(instr)]; \
    regs[INSTR_drr_dest(instr)] = r;                                    \
    flag = (r == 0);                                                    \
  } while (0)

#define do_pb_bin_op_pb_signal_pb_subz_pb_immediate(instr)              \
  do {                                                                  \
    iptr r = regs[INSTR_dri_reg(instr)] - (uptr)INSTR_dri_imm(instr);   \
    regs[INSTR_dri_dest(instr)] = r;                                    \
    flag = (r == 0);                                                    \
  } while (0)

#define do_pb_bin_op_pb_signal_pb_subp_pb_register(instr)               \
  do {                                                                  \
    iptr r = regs[INSTR_drr_reg1(instr)] - regs[INSTR_drr_reg2(instr)]; \
    regs[INSTR_drr_dest(instr)] = r;                                    \
    flag = (r > 0);                                                     \
  } while (0)

#define do_pb_bin_op_pb_signal_pb_subp_pb_immediate(instr)              \
  do {                                                                  \
    iptr r = regs[INSTR_dri_reg(instr)] - (uptr)INSTR_dri_imm(instr);   \
    regs[INSTR_dri_dest(instr)] = r;                                    \
    flag = (r > 0);                                                     \
  } while (0)

#define do_pb_cmp_op_pb_eq_pb_register(instr) \
  flag = regs[INSTR_dr_dest(instr)] == regs[INSTR_dr_reg(instr)]

#define do_pb_cmp_op_pb_eq_pb_immediate(instr) \
  flag = regs[INSTR_di_dest(instr)] == (uptr)INSTR_di_imm(instr)

#define do_pb_cmp_op_pb_lt_pb_register(instr) \
  flag = (iptr)regs[INSTR_dr_dest(instr)] < (iptr)regs[INSTR_dr_reg(instr)]

#define do_pb_cmp_op_pb_lt_pb_immediate(instr) \
  flag = (iptr)regs[INSTR_di_dest(instr)] < (iptr)INSTR_di_imm(instr)

#define do_pb_cmp_op_pb_gt_pb_register(instr) \
  flag = (iptr)regs[INSTR_dr_dest(instr)] > (iptr)regs[INSTR_dr_reg(instr)]

#define do_pb_cmp_op_pb_gt_pb_immediate(instr) \
  flag = (iptr)regs[INSTR_di_dest(instr)] > (iptr)INSTR_di_imm(instr)

#define do_pb_cmp_op_pb_le_pb_register(instr) \
  flag = (iptr)regs[INSTR_dr_dest(instr)] <= (iptr)regs[INSTR_dr_reg(instr)]

#define do_pb_cmp_op_pb_le_pb_immediate(instr) \
  flag = (iptr)regs[INSTR_di_dest(instr)] <= (iptr)INSTR_di_imm(instr)

#define do_pb_cmp_op_pb_ge_pb_register(instr) \
  flag = (iptr)regs[INSTR_dr_dest(instr)] >= (iptr)regs[INSTR_dr_reg(instr)]

#define do_pb_cmp_op_pb_ge_pb_immediate(instr) \
  flag = (iptr)regs[INSTR_di_dest(instr)] >= (iptr)INSTR_di_imm(instr)

#define do_pb_cmp_op_pb_ab_pb_register(instr) \
  flag = regs[INSTR_dr_dest(instr)] > regs[INSTR_dr_reg(instr)]

#define do_pb_cmp_op_pb_ab_pb_immediate(instr) \
  flag = regs[INSTR_di_dest(instr)] > (uptr)INSTR_di_imm(instr)

#define do_pb_cmp_op_pb_bl_pb_register(instr) \
  flag = regs[INSTR_dr_dest(instr)] < regs[INSTR_dr_reg(instr)]

#define do_pb_cmp_op_pb_bl_pb_immediate(instr) \
  flag = regs[INSTR_di_dest(instr)] < (uptr)INSTR_di_imm(instr)

#define do_pb_cmp_op_pb_cs_pb_register(instr) \
  flag = ((regs[INSTR_dr_dest(instr)] & regs[INSTR_dr_reg(instr)]) != 0)

#define do_pb_cmp_op_pb_cs_pb_immediate(instr) \
  flag = ((regs[INSTR_di_dest(instr)] & (uptr)INSTR_di_imm(instr)) != 0)

#define do_pb_cmp_op_pb_cc_pb_register(instr) \
  flag = ((regs[INSTR_dr_dest(instr)] & regs[INSTR_dr_reg(instr)]) == 0)

#define do_pb_cmp_op_pb_cc_pb_immediate(instr) \
  flag = ((regs[INSTR_di_dest(instr)] & (uptr)INSTR_di_imm(instr)) == 0)

#define do_pb_fp_bin_op_pb_add_pb_register(instr) \
  fpregs[INSTR_drr_dest(instr)] = fpregs[INSTR_drr_reg1(instr)] + fpregs[INSTR_drr_reg2(instr)]

#define do_pb_fp_bin_op_pb_sub_pb_register(instr) \
  fpregs[INSTR_drr_dest(instr)] = fpregs[INSTR_drr_reg1(instr)] - fpregs[INSTR_drr_reg2(instr)]

#define do_pb_fp_bin_op_pb_mul_pb_register(instr) \
  fpregs[INSTR_drr_dest(instr)] = fpregs[INSTR_drr_reg1(instr)] * fpregs[INSTR_drr_reg2(instr)]

#define do_pb_fp_bin_op_pb_div_pb_register(instr) \
  fpregs[INSTR_drr_dest(instr)] = fpregs[INSTR_drr_reg1(instr)] / fpregs[INSTR_drr_reg2(instr)]

#define do_pb_un_op_pb_not_pb_register(instr) \
  regs[INSTR_dr_dest(instr)] = ~(regs[INSTR_dr_reg(instr)])

#define do_pb_un_op_pb_not_pb_immediate(instr) \
  regs[INSTR_di_dest(instr)] = ~((uptr)(iptr)INSTR_di_imm(instr))

#define do_pb_fp_un_op_pb_sqrt_pb_register(instr) \
  fpregs[INSTR_dr_dest(instr)] = sqrt(fpregs[INSTR_dr_reg(instr)])

#define do_pb_fp_cmp_op_pb_eq_pb_register(instr) \
  flag = fpregs[INSTR_dr_dest(instr)] == fpregs[INSTR_dr_reg(instr)]

#define do_pb_fp_cmp_op_pb_lt_pb_register(instr) \
  flag = fpregs[INSTR_dr_dest(instr)] < fpregs[INSTR_dr_reg(instr)]

#define do_pb_fp_cmp_op_pb_le_pb_register(instr) \
  flag = fpregs[INSTR_dr_dest(instr)] <= fpregs[INSTR_dr_reg(instr)]

#if ptr_bits == 64
#define do_pb_rev_op_pb_int16_pb_register(instr)                        \
  regs[INSTR_dr_dest(instr)] = ((uptr)((iptr)(regs[INSTR_dr_reg(instr)] << 56) >> 48) \
                                | ((regs[INSTR_dr_reg(instr)] & 0xFF00) >> 8))
#else
#define do_pb_rev_op_pb_int16_pb_register(instr)                        \
  regs[INSTR_dr_dest(instr)] = ((uptr)((iptr)(regs[INSTR_dr_reg(instr)] << 24) >> 16) \
                                | ((regs[INSTR_dr_reg(instr)] & 0xFF00) >> 8))
#endif

#define do_pb_rev_op_pb_uint16_pb_register(instr) \
  regs[INSTR_dr_dest(instr)] = (((regs[INSTR_dr_reg(instr)] & 0x00FF) << 8) \
                                | ((regs[INSTR_dr_reg(instr)] & 0xFF00) >> 8))

#if ptr_bits == 64
# define do_pb_rev_op_pb_int32_pb_register(instr)                       \
  regs[INSTR_dr_dest(instr)] = ((uptr)((iptr)(regs[INSTR_dr_reg(instr)] << 56) >> 32) \
                                | ((regs[INSTR_dr_reg(instr)] & (uptr)0xFF000000) >> 24) \
                                | ((regs[INSTR_dr_reg(instr)] & (uptr)0x00FF0000) >> 8) \
                                | ((regs[INSTR_dr_reg(instr)] & (uptr)0x0000FF00) << 8))
#else
# define do_pb_rev_op_pb_int32_pb_register(instr)                       \
  regs[INSTR_dr_dest(instr)] = ((regs[INSTR_dr_reg(instr)] << 24)       \
                                | ((regs[INSTR_dr_reg(instr)] & (uptr)0xFF000000) >> 24) \
                                | ((regs[INSTR_dr_reg(instr)] & (uptr)0x00FF0000) >> 8) \
                                | ((regs[INSTR_dr_reg(instr)] & (uptr)0x0000FF00) << 8))
#endif

#define do_pb_rev_op_pb_uint32_pb_register(instr)                       \
  regs[INSTR_dr_dest(instr)] = (((regs[INSTR_dr_reg(instr)] & (uptr)0x000000FF) << 24) \
                                | ((regs[INSTR_dr_reg(instr)] & (uptr)0xFF000000) >> 24) \
                                | ((regs[INSTR_dr_reg(instr)] & (uptr)0x00FF0000) >> 8) \
                                | ((regs[INSTR_dr_reg(instr)] & (uptr)0x0000FF00) << 8))

#if ptr_bits == 64
# define do_pb_rev_op_pb_int64_pb_register(instr)                       \
  regs[INSTR_dr_dest(instr)] = (((regs[INSTR_dr_reg(instr)] & (uptr)0x00000000000000FF) << 56) \
                                | ((regs[INSTR_dr_reg(instr)] & (uptr)0x000000000000FF00) << 40) \
                                | ((regs[INSTR_dr_reg(instr)] & (uptr)0x0000000000FF0000) << 24) \
                                | ((regs[INSTR_dr_reg(instr)] & (uptr)0x00000000FF000000) << 8) \
                                | ((regs[INSTR_dr_reg(instr)] & (uptr)0x000000FF00000000) >> 8) \
                                | ((regs[INSTR_dr_reg(instr)] & (uptr)0x0000FF0000000000) >> 24) \
                                | ((regs[INSTR_dr_reg(instr)] & (uptr)0x00FF000000000000) >> 40) \
                                | ((regs[INSTR_dr_reg(instr)] & (uptr)0xFF00000000000000) >> 56))
#else
# define do_pb_rev_op_pb_int64_pb_register(instr)                        \
  regs[INSTR_dr_dest(instr)] = (((regs[INSTR_dr_reg(instr)] & (uptr)0x000000FF) << 24) \
                                | ((regs[INSTR_dr_reg(instr)] & (uptr)0xFF000000) >> 24) \
                                | ((regs[INSTR_dr_reg(instr)] & (uptr)0x00FF0000) >> 8) \
                                | ((regs[INSTR_dr_reg(instr)] & (uptr)0x0000FF00) << 8))
#endif

#define do_pb_ld_op_pb_int8_pb_register(instr) \
  regs[INSTR_drr_dest(instr)] = *(int8_t *)TO_VOIDP(regs[INSTR_drr_reg1(instr)] + regs[INSTR_drr_reg2(instr)])

#if defined(__arm__)
/* Complicated load to avoid an internal compiler error from an old gcc on Raspbian: */
# define do_pb_ld_op_pb_int8_pb_immediate(instr)                        \
  do {                                                                  \
    int8_t v;                                                           \
    memcpy(&v, TO_VOIDP(regs[INSTR_dri_reg(instr)] + INSTR_dri_imm(instr)), sizeof(int8_t)); \
    regs[INSTR_dri_dest(instr)] = v;                                    \
  } while (0)
#else
# define do_pb_ld_op_pb_int8_pb_immediate(instr) \
  regs[INSTR_dri_dest(instr)] = *(int8_t *)TO_VOIDP(regs[INSTR_dri_reg(instr)] + INSTR_dri_imm(instr))
#endif

#define do_pb_ld_op_pb_uint8_pb_register(instr) \
  regs[INSTR_drr_dest(instr)] = *(uint8_t *)TO_VOIDP(regs[INSTR_drr_reg1(instr)] + regs[INSTR_drr_reg2(instr)])

#define do_pb_ld_op_pb_uint8_pb_immediate(instr) \
  regs[INSTR_dri_dest(instr)] = *(uint8_t *)TO_VOIDP(regs[INSTR_dri_reg(instr)] + INSTR_dri_imm(instr))

#define do_pb_ld_op_pb_int16_pb_register(instr) \
  regs[INSTR_drr_dest(instr)] = *(int16_t *)TO_VOIDP(regs[INSTR_drr_reg1(instr)] + regs[INSTR_drr_reg2(instr)])

#define do_pb_ld_op_pb_int16_pb_immediate(instr) \
  regs[INSTR_dri_dest(instr)] = *(int16_t *)TO_VOIDP(regs[INSTR_dri_reg(instr)] + INSTR_dri_imm(instr))

#define do_pb_ld_op_pb_uint16_pb_register(instr) \
  regs[INSTR_drr_dest(instr)] = *(uint16_t *)TO_VOIDP(regs[INSTR_drr_reg1(instr)] + regs[INSTR_drr_reg2(instr)])

#define do_pb_ld_op_pb_uint16_pb_immediate(instr) \
  regs[INSTR_dri_dest(instr)] = *(uint16_t *)TO_VOIDP(regs[INSTR_dri_reg(instr)] + INSTR_dri_imm(instr))

#define do_pb_ld_op_pb_int32_pb_register(instr) \
  regs[INSTR_drr_dest(instr)] = *(int32_t *)TO_VOIDP(regs[INSTR_drr_reg1(instr)] + regs[INSTR_drr_reg2(instr)])

#define do_pb_ld_op_pb_int32_pb_immediate(instr) \
  regs[INSTR_dri_dest(instr)] = *(int32_t *)TO_VOIDP(regs[INSTR_dri_reg(instr)] + INSTR_dri_imm(instr))

#define do_pb_ld_op_pb_uint32_pb_register(instr) \
  regs[INSTR_drr_dest(instr)] = *(uint32_t *)TO_VOIDP(regs[INSTR_drr_reg1(instr)] + regs[INSTR_drr_reg2(instr)])

#define do_pb_ld_op_pb_uint32_pb_immediate(instr) \
  regs[INSTR_dri_dest(instr)] = *(uint32_t *)TO_VOIDP(regs[INSTR_dri_reg(instr)] + INSTR_dri_imm(instr))

#define do_pb_ld_op_pb_int64_pb_register(instr) \
  regs[INSTR_drr_dest(instr)] = *(uptr *)TO_VOIDP(regs[INSTR_drr_reg1(instr)] + regs[INSTR_drr_reg2(instr)])

#define do_pb_ld_op_pb_int64_pb_immediate(instr) \
  regs[INSTR_dri_dest(instr)] = *(uptr *)TO_VOIDP(regs[INSTR_dri_reg(instr)] + INSTR_dri_imm(instr))

#define do_pb_ld_op_pb_double_pb_register(instr) \
  fpregs[INSTR_drr_dest(instr)] = *(double *)TO_VOIDP(regs[INSTR_drr_reg1(instr)] + regs[INSTR_drr_reg2(instr)])

#define do_pb_ld_op_pb_double_pb_immediate(instr) \
  fpregs[INSTR_dri_dest(instr)] = *(double *)TO_VOIDP(regs[INSTR_dri_reg(instr)] + INSTR_dri_imm(instr))

#define do_pb_ld_op_pb_single_pb_register(instr) \
  fpregs[INSTR_drr_dest(instr)] =  *(float *)TO_VOIDP(regs[INSTR_drr_reg1(instr)] + regs[INSTR_drr_reg2(instr)])

#define do_pb_ld_op_pb_single_pb_immediate(instr) \
  fpregs[INSTR_dri_dest(instr)] = *(float *)TO_VOIDP(regs[INSTR_dri_reg(instr)] + INSTR_dri_imm(instr))

#define do_pb_st_op_pb_int8_pb_register(instr) \
  *(char *)TO_VOIDP(regs[INSTR_drr_reg1(instr)] + regs[INSTR_drr_reg2(instr)]) = (char)regs[INSTR_drr_dest(instr)]

#define do_pb_st_op_pb_int8_pb_immediate(instr)                         \
  *(char *)TO_VOIDP(regs[INSTR_dri_reg(instr)] + INSTR_dri_imm(instr)) = (char)regs[INSTR_dri_dest(instr)]

#define do_pb_st_op_pb_int16_pb_register(instr) \
  *(short *)TO_VOIDP(regs[INSTR_drr_reg1(instr)] + regs[INSTR_drr_reg2(instr)]) = (short)regs[INSTR_drr_dest(instr)]

#define do_pb_st_op_pb_int16_pb_immediate(instr) \
  *(short *)TO_VOIDP(regs[INSTR_dri_reg(instr)] + INSTR_dri_imm(instr)) = (short)regs[INSTR_dri_dest(instr)]

#define do_pb_st_op_pb_int32_pb_register(instr) \
  *(int *)TO_VOIDP(regs[INSTR_drr_reg1(instr)] + regs[INSTR_drr_reg2(instr)]) = (int)regs[INSTR_drr_dest(instr)]

#define do_pb_st_op_pb_int32_pb_immediate(instr) \
  *(int *)TO_VOIDP(regs[INSTR_dri_reg(instr)] + INSTR_dri_imm(instr)) = (int)regs[INSTR_dri_dest(instr)]

#define do_pb_st_op_pb_int64_pb_register(instr) \
  *(uptr *)TO_VOIDP(regs[INSTR_drr_reg1(instr)] + regs[INSTR_drr_reg2(instr)]) = regs[INSTR_drr_dest(instr)]

#define do_pb_st_op_pb_int64_pb_immediate(instr) \
  *(uptr *)TO_VOIDP(regs[INSTR_dri_reg(instr)] + INSTR_dri_imm(instr)) = regs[INSTR_dri_dest(instr)]

#define do_pb_st_op_pb_double_pb_register(instr) \
  *(double *)TO_VOIDP(regs[INSTR_drr_reg1(instr)] + regs[INSTR_drr_reg2(instr)]) = fpregs[INSTR_drr_dest(instr)]

#define do_pb_st_op_pb_double_pb_immediate(instr) \
  *(double *)TO_VOIDP(regs[INSTR_dri_reg(instr)] + INSTR_dri_imm(instr)) = fpregs[INSTR_dri_dest(instr)]

#define do_pb_st_op_pb_single_pb_register(instr) \
  *(float *)TO_VOIDP(regs[INSTR_drr_reg1(instr)] + regs[INSTR_drr_reg2(instr)]) = fpregs[INSTR_drr_dest(instr)]

#define do_pb_st_op_pb_single_pb_immediate(instr) \
  *(float *)TO_VOIDP(regs[INSTR_dri_reg(instr)] + INSTR_dri_imm(instr)) = fpregs[INSTR_dri_dest(instr)]

#if defined(PTHREADS)
# define CAS_ANY_FENCE_SEQOK(addr, old_r, r) \
  CAS_ANY_FENCE(TO_VOIDP(addr), TO_VOIDP(old_r), TO_VOIDP(r))
#else
# define CAS_ANY_FENCE_SEQOK(addr, old_r, r) \
  (*(uptr *)TO_VOIDP(addr) = r, 1)
#endif

#define do_pb_inc_pb_register(instr)                                    \
  do {                                                                  \
    uptr addr = regs[INSTR_dr_dest(instr)];                             \
    while (1) {                                                         \
      uptr old_r = *(uptr *)TO_VOIDP(addr);                             \
      uptr r = old_r + regs[INSTR_dr_reg(instr)];                       \
      if (CAS_ANY_FENCE_SEQOK(addr, old_r, r)) {                        \
        flag = (r == 0);                                                \
        break;                                                          \
      }                                                                 \
    }                                                                   \
  } while (0)

#define do_pb_inc_pb_immediate(instr)                                   \
  do {                                                                  \
    uptr addr = regs[INSTR_di_dest(instr)];                             \
    while (1) {                                                         \
      uptr old_r = *(uptr *)TO_VOIDP(addr);                             \
      uptr r = old_r + INSTR_di_imm(instr);                             \
      if (CAS_ANY_FENCE_SEQOK(addr, old_r, r)) {                        \
        flag = (r == 0);                                                \
        break;                                                          \
      }                                                                 \
    }                                                                   \
  } while (0)

#if defined(PTHREADS)
# define do_pb_lock(instr)                                       \
  do {                                                           \
    uptr *l = TO_VOIDP(regs[INSTR_d_dest(instr)]);               \
    flag = CAS_ANY_FENCE(l, TO_VOIDP(0), TO_VOIDP(1));           \
  } while (0)
#else
# define do_pb_lock(instr)                             \
  do {                                                 \
    uptr *l = TO_VOIDP(regs[INSTR_d_dest(instr)]);     \
    if (*l == 0) {                                     \
      *l = 1;                                          \
      flag = 1;                                        \
    } else                                             \
      flag = 0;                                        \
  } while (0)
#endif

#if defined(PTHREADS)
# define do_pb_cas(instr)                                      \
  do {                                                         \
    uptr *l = TO_VOIDP(regs[INSTR_drr_dest(instr)]);           \
    uptr old = regs[INSTR_drr_reg1(instr)];                    \
    uptr new = regs[INSTR_drr_reg2(instr)];                    \
    flag = CAS_ANY_FENCE(l, TO_VOIDP(old), TO_VOIDP(new));     \
  } while (0)
#else
#define do_pb_cas(instr)                                 \
  do {                                                   \
    uptr *l = TO_VOIDP(regs[INSTR_drr_dest(instr)]);     \
    uptr old = regs[INSTR_drr_reg1(instr)];              \
    uptr new = regs[INSTR_drr_reg2(instr)];              \
    if (*l == old) {                                     \
      *l = new;                                          \
      flag = 1;                                          \
    } else                                               \
      flag = 0;                                          \
  } while (0)
#endif

#define do_pb_fence_pb_fence_store_store(instr) \
  STORE_FENCE()

#define do_pb_fence_pb_fence_acquire(instr) \
  ACQUIRE_FENCE()

#define do_pb_fence_pb_fence_release(instr) \
  RELEASE_FENCE()

#define do_pb_call_arena_in(instr) \
  *(ptr *)TO_VOIDP(((uptr)TO_PTR(call_arena) + INSTR_di_imm(instr))) = regs[INSTR_di_dest(instr)]

#define do_pb_fp_call_arena_in(instr) \
  *(double *)TO_VOIDP(((uptr)TO_PTR(call_arena) + INSTR_di_imm(instr))) = fpregs[INSTR_di_dest(instr)]

#define do_pb_call_arena_out(instr) \
  regs[INSTR_di_dest(instr)] = *(ptr *)TO_VOIDP((uptr)TO_PTR(call_arena) + INSTR_di_imm(instr))

#define do_pb_fp_call_arena_out(instr)                                  \
  fpregs[INSTR_di_dest(instr)] = *(double *)TO_VOIDP((uptr)TO_PTR(call_arena) + INSTR_di_imm(instr))

#define do_pb_stack_call(instr) \
  S_ffi_call(regs[INSTR_dr_reg(instr)], regs[INSTR_dr_dest(instr)], (ptr *)call_arena)

#define pb_bs_op_pb_register_addr(instr) \
  (*(uptr *)TO_VOIDP(regs[INSTR_dr_dest(instr)] + regs[INSTR_dr_reg(instr)]))

#define pb_bs_op_pb_immediate_addr(instr) \
  (*(uptr *)TO_VOIDP(regs[INSTR_di_dest(instr)] + INSTR_di_imm(instr)))

#if ptr_bits == 64      
# define decode_relocation(instr, ip)                   \
  ((uptr)INSTR_di_imm_unsigned(instr)                   \
   | ((uptr)INSTR_di_imm_unsigned((ip)[1]) << 16)       \
   | ((uptr)INSTR_di_imm_unsigned((ip)[2]) << 32)       \
   | ((uptr)INSTR_di_imm_unsigned((ip)[3]) << 48))
#else
# define decode_relocation(instr, ip)                   \
  ((uptr)INSTR_di_imm_unsigned(instr)                   \
   | ((uptr)INSTR_di_imm_unsigned((ip)[1]) << 16))
#endif

/* ********************************************************************** */
/* Support for generated chunks */

#define load_from_relocation(dest, ip) \
  regs[dest] = decode_relocation(((instruction_t *)TO_VOIDP(ip))[0], (instruction_t *)TO_VOIDP(ip))

#define load_code_relative(dest, ip) \
  regs[dest] = ip

#define code_rel(start_i, i) ((i)-(start_i))

#define MACHINE_STATE machine_state * RESTRICT_PTR
