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

#if C_COMPILER_HAS_BUILTIN(__builtin_add_overflow) \
  && C_COMPILER_HAS_BUILTIN(__builtin_sub_overflow) \
  && C_COMPILER_HAS_BUILTIN(__builtin_mul_overflow)
# define USE_OVERFLOW_INTRINSICS 1
#elif (__GNUC__ >= 5)
# define USE_OVERFLOW_INTRINSICS 1
#else
# define USE_OVERFLOW_INTRINSICS 0
#endif

#if C_COMPILER_HAS_BUILTIN(__builtin_bswap16) \
  && C_COMPILER_HAS_BUILTIN(__builtin_bswap32)
# define USE_BSWAP_INTRINSICS 1
#elif (__GNUC__ >= 5)
# define USE_BSWAP_INTRINSICS 1
#else
# define USE_BSWAP_INTRINSICS 0
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

#define doi_pb_mov16_pb_zero_bits_pb_shift0(instr) \
  do_pb_mov16_pb_zero_bits_pb_shift0(INSTR_di_dest(instr), INSTR_di_imm_unsigned(instr))
#define do_pb_mov16_pb_zero_bits_pb_shift0(dest, imm_unsigned) \
  regs[dest] = (uptr)imm_unsigned

#define doi_pb_mov16_pb_zero_bits_pb_shift1(instr) \
  do_pb_mov16_pb_zero_bits_pb_shift1(INSTR_di_dest(instr), INSTR_di_imm_unsigned(instr))
#define do_pb_mov16_pb_zero_bits_pb_shift1(dest, imm_unsigned) \
  regs[dest] = (uptr)imm_unsigned << 16

#if ptr_bits == 64      
# define doi_pb_mov16_pb_zero_bits_pb_shift2(instr) \
   do_pb_mov16_pb_zero_bits_pb_shift2(INSTR_di_dest(instr), INSTR_di_imm_unsigned(instr))
# define do_pb_mov16_pb_zero_bits_pb_shift2(dest, imm_unsigned) \
  regs[dest] = (uptr)imm_unsigned << 32
#else
# define doi_pb_mov16_pb_zero_bits_pb_shift2(instr) \
  do_pb_mov16_pb_zero_bits_pb_shift2(INSTR_di_dest(instr), INSTR_di_imm_unsigned(instr))
# define do_pb_mov16_pb_zero_bits_pb_shift2(dest, imm_unsigned)      \
  regs[dest] = 0
#endif

#if ptr_bits == 64      
# define doi_pb_mov16_pb_zero_bits_pb_shift3(instr) \
   do_pb_mov16_pb_zero_bits_pb_shift3(INSTR_di_dest(instr), INSTR_di_imm_unsigned(instr))
# define do_pb_mov16_pb_zero_bits_pb_shift3(dest, imm_unsigned) \
  regs[dest] = (uptr)imm_unsigned << 48
#else
# define doi_pb_mov16_pb_zero_bits_pb_shift3(instr) \
  do_pb_mov16_pb_zero_bits_pb_shift3(INSTR_di_dest(instr), INSTR_di_imm_unsigned(instr))
# define do_pb_mov16_pb_zero_bits_pb_shift3(dest, imm_unsigned)      \
  regs[dest] = 0
#endif

#define doi_pb_mov16_pb_keep_bits_pb_shift0(instr) \
  do_pb_mov16_pb_keep_bits_pb_shift0(INSTR_di_dest(instr), INSTR_di_imm_unsigned(instr))
#define do_pb_mov16_pb_keep_bits_pb_shift0(dest, imm_unsigned) \
  regs[dest] |= (uptr)imm_unsigned

#define doi_pb_mov16_pb_keep_bits_pb_shift1(instr) \
  do_pb_mov16_pb_keep_bits_pb_shift1(INSTR_di_dest(instr), INSTR_di_imm_unsigned(instr))
#define do_pb_mov16_pb_keep_bits_pb_shift1(dest, imm_unsigned) \
  regs[dest] |= (uptr)imm_unsigned << 16

#if ptr_bits == 64      
# define doi_pb_mov16_pb_keep_bits_pb_shift2(instr) \
   do_pb_mov16_pb_keep_bits_pb_shift2(INSTR_di_dest(instr), INSTR_di_imm_unsigned(instr))
# define do_pb_mov16_pb_keep_bits_pb_shift2(dest, imm_unsigned) \
  regs[dest] |= (uptr)imm_unsigned << 32
#else
# define doi_pb_mov16_pb_keep_bits_pb_shift2(instr) \
  do_pb_mov16_pb_keep_bits_pb_shift2(INSTR_di_dest(instr), INSTR_di_imm_unsigned(instr))
# define do_pb_mov16_pb_keep_bits_pb_shift2(dest, imm_unsigned) \
  do { } while (0)
#endif

#if ptr_bits == 64      
# define doi_pb_mov16_pb_keep_bits_pb_shift3(instr) \
   do_pb_mov16_pb_keep_bits_pb_shift3(INSTR_di_dest(instr), INSTR_di_imm_unsigned(instr))
# define do_pb_mov16_pb_keep_bits_pb_shift3(dest, imm_unsigned) \
  regs[dest] |= (uptr)imm_unsigned << 48
#else
# define doi_pb_mov16_pb_keep_bits_pb_shift3(instr) \
   do_pb_mov16_pb_keep_bits_pb_shift3(INSTR_di_dest(instr), INSTR_di_imm_unsigned(instr))
# define do_pb_mov16_pb_keep_bits_pb_shift3(dest, imm_unsigned) \
  do { } while (0)
#endif

#define doi_pb_mov_pb_i_i(instr) \
  do_pb_mov_pb_i_i(INSTR_dr_dest(instr), INSTR_dr_reg(instr))
#define do_pb_mov_pb_i_i(dest, reg) \
  regs[dest] = regs[reg]

#define doi_pb_mov_pb_d_d(instr) \
  do_pb_mov_pb_d_d(INSTR_dr_dest(instr), INSTR_dr_reg(instr))
#define do_pb_mov_pb_d_d(dest, reg) \
  fpregs[dest] = fpregs[reg]

#define doi_pb_mov_pb_i_d(instr) \
  do_pb_mov_pb_i_d(INSTR_dr_dest(instr), INSTR_dr_reg(instr))
#define do_pb_mov_pb_i_d(dest, reg) \
  fpregs[dest] = (double)(iptr)regs[reg]

#define doi_pb_mov_pb_d_i(instr) \
  do_pb_mov_pb_d_i(INSTR_dr_dest(instr), INSTR_dr_reg(instr))
#define do_pb_mov_pb_d_i(dest, reg) \
  regs[dest] = (iptr)fpregs[reg]

#if ptr_bits == 64
# define doi_pb_mov_pb_i_bits_d_bits(instr) \
   do_pb_mov_pb_i_bits_d_bits(INSTR_dr_dest(instr), INSTR_dr_reg(instr))
# define do_pb_mov_pb_i_bits_d_bits(dest, reg) \
  memcpy(&fpregs[dest], &regs[reg], sizeof(double))
# define doi_pb_mov_pb_d_bits_i_bits(instr) \
   do_pb_mov_pb_d_bits_i_bits(INSTR_dr_dest(instr), INSTR_dr_reg(instr))
# define do_pb_mov_pb_d_bits_i_bits(dest, reg) \
  memcpy(&regs[dest], &fpregs[reg], sizeof(double))
#else
# define doi_pb_mov_pb_i_i_bits_d_bits(instr) \
   do_pb_mov_pb_i_i_bits_d_bits(INSTR_drr_dest(instr), INSTR_drr_reg1(instr), INSTR_drr_reg2(instr))
# define do_pb_mov_pb_i_i_bits_d_bits(dest, reg1, reg2) \
  do {                                                                  \
    uint64_t d;                                                         \
    d = regs[reg1] | ((uint64_t)regs[reg2] << 32); \
    memcpy(&fpregs[dest], &d, sizeof(double));         \
  } while (0)
# define doi_pb_mov_pb_d_lo_bits_i_bits(instr) \
   do_pb_mov_pb_d_lo_bits_i_bits(INSTR_dr_dest(instr), INSTR_dr_reg(instr))
# define do_pb_mov_pb_d_lo_bits_i_bits(dest, reg) \
  do {                                            \
    uint64_t d;                                   \
    memcpy(&d, &fpregs[reg], sizeof(double));     \
    regs[dest] = d;                               \
  } while (0)
#define doi_pb_mov_pb_d_hi_bits_i_bits(instr) \
  do_pb_mov_pb_d_hi_bits_i_bits(INSTR_dr_dest(instr), INSTR_dr_reg(instr))
#define do_pb_mov_pb_d_hi_bits_i_bits(dest, reg) \
  do {                                            \
    uint64_t d;                                   \
    memcpy(&d, &fpregs[reg], sizeof(double));     \
    d >>= 32;                                     \
    regs[dest] = d;                               \
  } while (0)
#endif

#ifdef PORTABLE_BYTECODE_BIGENDIAN
# define FP_REG_FLOAT_START(p) ((char *)&(p) + 4)
#else
# define FP_REG_FLOAT_START(p) &(p)
#endif

#define doi_pb_mov_pb_s_d(instr) \
  do_pb_mov_pb_s_d(INSTR_dr_dest(instr), INSTR_dr_reg(instr))
#define do_pb_mov_pb_s_d(dest, reg)                                     \
  do {                                                                  \
    float f;                                                            \
    memcpy(&f, FP_REG_FLOAT_START(fpregs[reg]), sizeof(float));         \
    fpregs[dest] = f;                                                   \
  } while (0)

#define doi_pb_mov_pb_d_s(instr) \
  do_pb_mov_pb_d_s(INSTR_dr_dest(instr), INSTR_dr_reg(instr))
#define do_pb_mov_pb_d_s(dest, reg)                                     \
  do {                                                                  \
    float f;                                                            \
    f = (float)fpregs[reg];                                             \
    memcpy(FP_REG_FLOAT_START(fpregs[dest]), &f, sizeof(float));        \
  } while (0)

#define doi_pb_mov_pb_d_s_d(instr) \
  do_pb_mov_pb_d_s_d(INSTR_dr_dest(instr), INSTR_dr_reg(instr))
#define do_pb_mov_pb_d_s_d(dest, reg) \
  do {                                \
    float f;                          \
    f = (float)fpregs[reg];           \
    fpregs[dest] = (double)f;         \
  } while (0)

#define doi_pb_bin_op_pb_no_signal_pb_add_pb_register(instr) \
  do_pb_bin_op_pb_no_signal_pb_add_pb_register(INSTR_drr_dest(instr), INSTR_drr_reg1(instr), INSTR_drr_reg2(instr))
#define do_pb_bin_op_pb_no_signal_pb_add_pb_register(dest, reg1, reg2) \
  regs[dest] = regs[reg1] + regs[reg2]

#define doi_pb_bin_op_pb_no_signal_pb_add_pb_immediate(instr) \
  do_pb_bin_op_pb_no_signal_pb_add_pb_immediate(INSTR_dri_dest(instr), INSTR_dri_reg(instr), INSTR_dri_imm(instr))
#define do_pb_bin_op_pb_no_signal_pb_add_pb_immediate(dest, reg, imm) \
  regs[dest] = regs[reg] + (uptr)imm

#define doi_pb_bin_op_pb_no_signal_pb_sub_pb_register(instr) \
  do_pb_bin_op_pb_no_signal_pb_sub_pb_register(INSTR_drr_dest(instr), INSTR_drr_reg1(instr), INSTR_drr_reg2(instr))
#define do_pb_bin_op_pb_no_signal_pb_sub_pb_register(dest, reg1, reg2) \
  regs[dest] = regs[reg1] - regs[reg2]

#define doi_pb_bin_op_pb_no_signal_pb_sub_pb_immediate(instr) \
  do_pb_bin_op_pb_no_signal_pb_sub_pb_immediate(INSTR_dri_dest(instr), INSTR_dri_reg(instr), INSTR_dri_imm(instr))
#define do_pb_bin_op_pb_no_signal_pb_sub_pb_immediate(dest, reg, imm) \
  regs[dest] = regs[reg] - (uptr)imm

#define doi_pb_bin_op_pb_no_signal_pb_mul_pb_register(instr) \
  do_pb_bin_op_pb_no_signal_pb_mul_pb_register(INSTR_drr_dest(instr), INSTR_drr_reg1(instr), INSTR_drr_reg2(instr))
#define do_pb_bin_op_pb_no_signal_pb_mul_pb_register(dest, reg1, reg2) \
  regs[dest] = regs[reg1] * regs[reg2]

#define doi_pb_bin_op_pb_no_signal_pb_mul_pb_immediate(instr) \
  do_pb_bin_op_pb_no_signal_pb_mul_pb_immediate(INSTR_dri_dest(instr), INSTR_dri_reg(instr), INSTR_dri_imm(instr))
#define do_pb_bin_op_pb_no_signal_pb_mul_pb_immediate(dest, reg, imm) \
  regs[dest] = (uptr)regs[reg] * (uptr)imm

#define doi_pb_bin_op_pb_no_signal_pb_div_pb_register(instr) \
  do_pb_bin_op_pb_no_signal_pb_div_pb_register(INSTR_drr_dest(instr), INSTR_drr_reg1(instr), INSTR_drr_reg2(instr))
#define do_pb_bin_op_pb_no_signal_pb_div_pb_register(dest, reg1, reg2) \
  regs[dest] = (iptr)regs[reg1] / (iptr)regs[reg2]

#define doi_pb_bin_op_pb_no_signal_pb_div_pb_immediate(instr) \
  do_pb_bin_op_pb_no_signal_pb_div_pb_immediate(INSTR_dri_dest(instr), INSTR_dri_reg(instr), INSTR_dri_imm(instr))
#define do_pb_bin_op_pb_no_signal_pb_div_pb_immediate(dest, reg, imm) \
  regs[dest] = (iptr)regs[reg] / (iptr)imm

#define doi_pb_bin_op_pb_no_signal_pb_and_pb_register(instr) \
  do_pb_bin_op_pb_no_signal_pb_and_pb_register(INSTR_drr_dest(instr), INSTR_drr_reg1(instr), INSTR_drr_reg2(instr))
#define do_pb_bin_op_pb_no_signal_pb_and_pb_register(dest, reg1, reg2) \
  regs[dest] = regs[reg1] & regs[reg2]

#define doi_pb_bin_op_pb_no_signal_pb_and_pb_immediate(instr) \
  do_pb_bin_op_pb_no_signal_pb_and_pb_immediate(INSTR_dri_dest(instr), INSTR_dri_reg(instr), INSTR_dri_imm(instr))
#define do_pb_bin_op_pb_no_signal_pb_and_pb_immediate(dest, reg, imm) \
  regs[dest] = regs[reg] & (uptr)imm

#define doi_pb_bin_op_pb_no_signal_pb_ior_pb_register(instr) \
  do_pb_bin_op_pb_no_signal_pb_ior_pb_register(INSTR_drr_dest(instr), INSTR_drr_reg1(instr), INSTR_drr_reg2(instr))
#define do_pb_bin_op_pb_no_signal_pb_ior_pb_register(dest, reg1, reg2) \
  regs[dest] = regs[reg1] | regs[reg2]

#define doi_pb_bin_op_pb_no_signal_pb_ior_pb_immediate(instr) \
  do_pb_bin_op_pb_no_signal_pb_ior_pb_immediate(INSTR_dri_dest(instr), INSTR_dri_reg(instr), INSTR_dri_imm(instr))
#define do_pb_bin_op_pb_no_signal_pb_ior_pb_immediate(dest, reg, imm) \
  regs[dest] = regs[reg] | (uptr)imm

#define doi_pb_bin_op_pb_no_signal_pb_xor_pb_register(instr) \
  do_pb_bin_op_pb_no_signal_pb_xor_pb_register(INSTR_drr_dest(instr), INSTR_drr_reg1(instr), INSTR_drr_reg2(instr))
#define do_pb_bin_op_pb_no_signal_pb_xor_pb_register(dest, reg1, reg2) \
  regs[dest] = regs[reg1] ^ regs[reg2]

#define doi_pb_bin_op_pb_no_signal_pb_xor_pb_immediate(instr) \
  do_pb_bin_op_pb_no_signal_pb_xor_pb_immediate(INSTR_dri_dest(instr), INSTR_dri_reg(instr), INSTR_dri_imm(instr))
#define do_pb_bin_op_pb_no_signal_pb_xor_pb_immediate(dest, reg, imm) \
  regs[dest] = regs[reg] ^ (uptr)imm

#define doi_pb_bin_op_pb_no_signal_pb_lsl_pb_register(instr) \
  do_pb_bin_op_pb_no_signal_pb_lsl_pb_register(INSTR_drr_dest(instr), INSTR_drr_reg1(instr), INSTR_drr_reg2(instr))
#define do_pb_bin_op_pb_no_signal_pb_lsl_pb_register(dest, reg1, reg2) \
  regs[dest] = regs[reg1] << SHIFT_MASK(regs[reg2])

#define doi_pb_bin_op_pb_no_signal_pb_lsl_pb_immediate(instr) \
  do_pb_bin_op_pb_no_signal_pb_lsl_pb_immediate(INSTR_dri_dest(instr), INSTR_dri_reg(instr), INSTR_dri_imm(instr))
#define do_pb_bin_op_pb_no_signal_pb_lsl_pb_immediate(dest, reg, imm) \
  regs[dest] = regs[reg] << SHIFT_MASK(imm)

#define doi_pb_bin_op_pb_no_signal_pb_lsr_pb_register(instr) \
  do_pb_bin_op_pb_no_signal_pb_lsr_pb_register(INSTR_drr_dest(instr), INSTR_drr_reg1(instr), INSTR_drr_reg2(instr))
#define do_pb_bin_op_pb_no_signal_pb_lsr_pb_register(dest, reg1, reg2) \
  regs[dest] = regs[reg1] >> SHIFT_MASK(regs[reg2])

#define doi_pb_bin_op_pb_no_signal_pb_lsr_pb_immediate(instr) \
  do_pb_bin_op_pb_no_signal_pb_lsr_pb_immediate(INSTR_dri_dest(instr), INSTR_dri_reg(instr), INSTR_dri_imm(instr))
#define do_pb_bin_op_pb_no_signal_pb_lsr_pb_immediate(dest, reg, imm) \
  regs[dest] = regs[reg] >> SHIFT_MASK(imm)

#define doi_pb_bin_op_pb_no_signal_pb_asr_pb_register(instr) \
  do_pb_bin_op_pb_no_signal_pb_asr_pb_register(INSTR_drr_dest(instr), INSTR_drr_reg1(instr), INSTR_drr_reg2(instr))
#define do_pb_bin_op_pb_no_signal_pb_asr_pb_register(dest, reg1, reg2) \
  regs[dest] = (iptr)regs[reg1] >> SHIFT_MASK(regs[reg2])

#define doi_pb_bin_op_pb_no_signal_pb_asr_pb_immediate(instr) \
  do_pb_bin_op_pb_no_signal_pb_asr_pb_immediate(INSTR_dri_dest(instr), INSTR_dri_reg(instr), INSTR_dri_imm(instr))
#define do_pb_bin_op_pb_no_signal_pb_asr_pb_immediate(dest, reg, imm) \
  regs[dest] = (iptr)regs[reg] >> SHIFT_MASK(imm)

#ifdef PORTABLE_BYTECODE_BIGENDIAN
# define doi_pb_bin_op_pb_no_signal_pb_lslo_pb_register(instr) \
   do_pb_bin_op_pb_no_signal_pb_lslo_pb_register(INSTR_drr_dest(instr), INSTR_drr_reg1(instr), INSTR_drr_reg2(instr))
# define do_pb_bin_op_pb_no_signal_pb_lslo_pb_register(dest, reg1, reg2) \
  regs[dest] = regs[reg1] >> regs[reg2]
#else
# define doi_pb_bin_op_pb_no_signal_pb_lslo_pb_register(instr) \
   do_pb_bin_op_pb_no_signal_pb_lslo_pb_register(INSTR_drr_dest(instr), INSTR_drr_reg1(instr), INSTR_drr_reg2(instr))
# define do_pb_bin_op_pb_no_signal_pb_lslo_pb_register(dest, reg1, reg2) \
  regs[dest] = regs[reg1] << regs[reg2]
#endif

#ifdef PORTABLE_BYTECODE_BIGENDIAN
# define doi_pb_bin_op_pb_no_signal_pb_lslo_pb_immediate(instr) \
   do_pb_bin_op_pb_no_signal_pb_lslo_pb_immediate(INSTR_dri_dest(instr), INSTR_dri_reg(instr), INSTR_dri_imm(instr))
# define do_pb_bin_op_pb_no_signal_pb_lslo_pb_immediate(dest, reg, imm) \
  regs[dest] = regs[reg] >> imm
#else
# define doi_pb_bin_op_pb_no_signal_pb_lslo_pb_immediate(instr) \
   do_pb_bin_op_pb_no_signal_pb_lslo_pb_immediate(INSTR_dri_dest(instr), INSTR_dri_reg(instr), INSTR_dri_imm(instr))
# define do_pb_bin_op_pb_no_signal_pb_lslo_pb_immediate(dest, reg, imm) \
  regs[dest] = regs[reg] << imm;
#endif

#if USE_OVERFLOW_INTRINSICS
# define doi_pb_bin_op_pb_signal_pb_add_pb_register(instr) \
   do_pb_bin_op_pb_signal_pb_add_pb_register(INSTR_drr_dest(instr), INSTR_drr_reg1(instr), INSTR_drr_reg2(instr))
# define do_pb_bin_op_pb_signal_pb_add_pb_register(dest, reg1, reg2) \
  do {                                                               \
    iptr a = (iptr)regs[reg1];                                       \
    iptr b = (iptr)regs[reg2];                                       \
    iptr r;                                                          \
    flag = __builtin_add_overflow(a, b, &r);                         \
    regs[dest] = (uptr)r;                                            \
  } while (0)
#else
# define doi_pb_bin_op_pb_signal_pb_add_pb_register(instr) \
   do_pb_bin_op_pb_signal_pb_add_pb_register(INSTR_drr_dest(instr), INSTR_drr_reg1(instr), INSTR_drr_reg2(instr))
# define do_pb_bin_op_pb_signal_pb_add_pb_register(dest, reg1, reg2) \
  do {                                                               \
    uptr a = regs[reg1];                                             \
    uptr b = regs[reg2];                                             \
    uptr r = a + b;                                                  \
    regs[dest] = r;                                                  \
    flag = SIGN_FLIP(r, a, b);                                       \
  } while (0)
#endif

#if USE_OVERFLOW_INTRINSICS
# define doi_pb_bin_op_pb_signal_pb_add_pb_immediate(instr) \
   do_pb_bin_op_pb_signal_pb_add_pb_immediate(INSTR_drr_dest(instr), INSTR_dri_reg(instr), INSTR_dri_imm(instr))
# define do_pb_bin_op_pb_signal_pb_add_pb_immediate(dest, reg, imm) \
  do {                                                           \
    iptr a = (iptr)regs[reg];                                    \
    iptr b = imm;                                                \
    iptr r;                                                      \
    flag = __builtin_add_overflow(a, b, &r);                     \
    regs[dest] = (uptr)r;                                        \
  } while (0)
#else
# define doi_pb_bin_op_pb_signal_pb_add_pb_immediate(instr) \
   do_pb_bin_op_pb_signal_pb_add_pb_immediate(INSTR_dri_dest(instr), INSTR_dri_reg(instr), INSTR_dri_imm(instr))
# define do_pb_bin_op_pb_signal_pb_add_pb_immediate(dest, reg, imm) \
  do {                                                              \
    uptr a = regs[reg];                                             \
    uptr b = (uptr)imm;                                             \
    uptr r = a + b;                                                 \
    regs[dest] = r;                                                 \
    flag = SIGN_FLIP(r, a, b);                                      \
  } while (0)
#endif

#if USE_OVERFLOW_INTRINSICS
#define doi_pb_bin_op_pb_signal_pb_sub_pb_register(instr) \
  do_pb_bin_op_pb_signal_pb_sub_pb_register(INSTR_drr_dest(instr), INSTR_drr_reg1(instr), INSTR_drr_reg2(instr))
#define do_pb_bin_op_pb_signal_pb_sub_pb_register(dest, reg1, reg2) \
  do {                                                              \
    iptr a = (iptr)regs[reg1];                                      \
    iptr b = (iptr)regs[reg2];                                      \
    iptr r;                                                         \
    flag = __builtin_sub_overflow(a, b, &r);                        \
    regs[dest] = (uptr)r;                                           \
  } while (0)
#else
#define doi_pb_bin_op_pb_signal_pb_sub_pb_register(instr) \
  do_pb_bin_op_pb_signal_pb_sub_pb_register(INSTR_drr_dest(instr), INSTR_drr_reg1(instr), INSTR_drr_reg2(instr))
#define do_pb_bin_op_pb_signal_pb_sub_pb_register(dest, reg1, reg2) \
  do {                                                              \
    uptr a = regs[reg1];                                            \
    uptr b = regs[reg2];                                            \
    uptr r = a - b;                                                 \
    regs[dest] = r;                                                 \
    flag = SIGN_FLIP(r, a, ~b);                                     \
  } while (0)
#endif

#if USE_OVERFLOW_INTRINSICS
# define doi_pb_bin_op_pb_signal_pb_sub_pb_immediate(instr) \
   do_pb_bin_op_pb_signal_pb_sub_pb_immediate(INSTR_drr_dest(instr), INSTR_dri_reg(instr), INSTR_dri_imm(instr))
# define do_pb_bin_op_pb_signal_pb_sub_pb_immediate(dest, reg, imm) \
  do {                                                              \
    iptr a = (iptr)regs[reg];                                       \
    iptr b = imm;                                                   \
    iptr r;                                                         \
    flag = __builtin_sub_overflow(a, b, &r);                        \
    regs[dest] = (uptr)r;                                           \
  } while (0)
#else
# define doi_pb_bin_op_pb_signal_pb_sub_pb_immediate(instr) \
   do_pb_bin_op_pb_signal_pb_sub_pb_immediate(INSTR_dri_dest(instr), INSTR_dri_reg(instr), INSTR_dri_imm(instr))
# define do_pb_bin_op_pb_signal_pb_sub_pb_immediate(dest, reg, imm) \
  do {                                                              \
    uptr a = regs[reg];                                             \
    uptr b = (uptr)imm;                                             \
    uptr r = a - b;                                                 \
    regs[dest] = r;                                                 \
    flag = SIGN_FLIP(r, a, ~b);                                     \
  } while (0)
#endif

#if USE_OVERFLOW_INTRINSICS
#define doi_pb_bin_op_pb_signal_pb_mul_pb_register(instr) \
  do_pb_bin_op_pb_signal_pb_mul_pb_register(INSTR_drr_dest(instr), INSTR_drr_reg1(instr), INSTR_drr_reg2(instr))
#define do_pb_bin_op_pb_signal_pb_mul_pb_register(dest, reg1, reg2) \
  do {                                                          \
    iptr a = (iptr)regs[reg1];                                  \
    iptr b = (iptr)regs[reg2];                                  \
    iptr r;                                                     \
    flag = __builtin_mul_overflow(a, b, &r);                    \
    regs[dest] = (uptr)r;                                       \
  } while (0)
#else
#define doi_pb_bin_op_pb_signal_pb_mul_pb_register(instr) \
  do_pb_bin_op_pb_signal_pb_mul_pb_register(INSTR_drr_dest(instr), INSTR_drr_reg1(instr), INSTR_drr_reg2(instr))
#define do_pb_bin_op_pb_signal_pb_mul_pb_register(dest, reg1, reg2) \
  do {                                                              \
    uptr a = regs[reg1];                                            \
    uptr b = regs[reg2];                                            \
    uptr r = a * b;                                                 \
    regs[dest] = r;                                                 \
    if (b != 0) {                                                   \
      if (b == (uptr)-1)                                            \
        flag = (a != r * (uptr)-1);                                 \
      else                                                          \
        flag = ((iptr)a != (iptr)r / (iptr)b);                      \
    } else                                                          \
      flag = 0;                                                     \
  } while (0)
#endif

#if USE_OVERFLOW_INTRINSICS
# define doi_pb_bin_op_pb_signal_pb_mul_pb_immediate(instr) \
   do_pb_bin_op_pb_signal_pb_mul_pb_immediate(INSTR_drr_dest(instr), INSTR_dri_reg(instr), INSTR_dri_imm(instr))
# define do_pb_bin_op_pb_signal_pb_mul_pb_immediate(dest, reg, imm) \
  do {                                                              \
    iptr a = (iptr)regs[reg];                                       \
    iptr b = imm;                                                   \
    iptr r;                                                         \
    flag = __builtin_mul_overflow(a, b, &r);                        \
    regs[dest] = (uptr)r;                                           \
  } while (0)
#else
# define doi_pb_bin_op_pb_signal_pb_mul_pb_immediate(instr) \
   do_pb_bin_op_pb_signal_pb_mul_pb_immediate(INSTR_dri_dest(instr), INSTR_dri_reg(instr), INSTR_dri_imm(instr))
# define do_pb_bin_op_pb_signal_pb_mul_pb_immediate(dest, reg, imm) \
  do {                                                              \
    uptr a = regs[reg];                                             \
    uptr b = (uptr)imm;                                             \
    uptr r = a * b;                                                 \
    regs[dest] = r;                                                 \
    if (b != 0) {                                                   \
      if (b == (uptr)-1)                                            \
        flag = (a != r * (uptr)-1);                                 \
      else                                                          \
        flag = ((iptr)a != (iptr)r / (iptr)b);                      \
    } else                                                          \
      flag = 0;                                                     \
  } while (0)
#endif

#define doi_pb_bin_op_pb_signal_pb_subz_pb_register(instr) \
  do_pb_bin_op_pb_signal_pb_subz_pb_register(INSTR_drr_dest(instr), INSTR_drr_reg1(instr), INSTR_drr_reg2(instr))
#define do_pb_bin_op_pb_signal_pb_subz_pb_register(dest, reg1, reg2) \
  do {                                                               \
    iptr r = regs[reg1] - regs[reg2];                                \
    regs[dest] = r;                                                  \
    flag = (r == 0);                                                 \
  } while (0)

#define doi_pb_bin_op_pb_signal_pb_subz_pb_immediate(instr) \
  do_pb_bin_op_pb_signal_pb_subz_pb_immediate(INSTR_dri_dest(instr), INSTR_dri_reg(instr), INSTR_dri_imm(instr))
#define do_pb_bin_op_pb_signal_pb_subz_pb_immediate(dest, reg, imm) \
  do {                                                              \
    iptr r = regs[reg] - (uptr)imm;                                 \
    regs[dest] = r;                                                 \
    flag = (r == 0);                                                \
  } while (0)

#define doi_pb_bin_op_pb_signal_pb_subp_pb_register(instr) \
  do_pb_bin_op_pb_signal_pb_subp_pb_register(INSTR_drr_dest(instr), INSTR_drr_reg1(instr), INSTR_drr_reg2(instr))
#define do_pb_bin_op_pb_signal_pb_subp_pb_register(dest, reg1, reg2) \
  do {                                                               \
    iptr r = regs[reg1] - regs[reg2];                                \
    regs[dest] = r;                                                  \
    flag = (r > 0);                                                  \
  } while (0)

#define doi_pb_bin_op_pb_signal_pb_subp_pb_immediate(instr) \
  do_pb_bin_op_pb_signal_pb_subp_pb_immediate(INSTR_dri_dest(instr), INSTR_dri_reg(instr), INSTR_dri_imm(instr))
#define do_pb_bin_op_pb_signal_pb_subp_pb_immediate(dest, reg, imm) \
  do {                                                              \
    iptr r = regs[reg] - (uptr)imm;                                 \
    regs[dest] = r;                                                 \
    flag = (r > 0);                                                 \
  } while (0)

#define doi_pb_cmp_op_pb_eq_pb_register(instr) \
  do_pb_cmp_op_pb_eq_pb_register(INSTR_dr_dest(instr), INSTR_dr_reg(instr))
#define do_pb_cmp_op_pb_eq_pb_register(dest, reg) \
  flag = regs[dest] == regs[reg]

#define doi_pb_cmp_op_pb_eq_pb_immediate(instr) \
  do_pb_cmp_op_pb_eq_pb_immediate(INSTR_di_dest(instr), INSTR_di_imm(instr))
#define do_pb_cmp_op_pb_eq_pb_immediate(dest, imm) \
  flag = regs[dest] == (uptr)imm

#define doi_pb_cmp_op_pb_lt_pb_register(instr) \
  do_pb_cmp_op_pb_lt_pb_register(INSTR_dr_dest(instr), INSTR_dr_reg(instr))
#define do_pb_cmp_op_pb_lt_pb_register(dest, reg) \
  flag = (iptr)regs[dest] < (iptr)regs[reg]

#define doi_pb_cmp_op_pb_lt_pb_immediate(instr) \
  do_pb_cmp_op_pb_lt_pb_immediate(INSTR_di_dest(instr), INSTR_di_imm(instr))
#define do_pb_cmp_op_pb_lt_pb_immediate(dest, imm) \
  flag = (iptr)regs[dest] < (iptr)imm

#define doi_pb_cmp_op_pb_gt_pb_register(instr) \
  do_pb_cmp_op_pb_gt_pb_register(INSTR_dr_dest(instr), INSTR_dr_reg(instr))
#define do_pb_cmp_op_pb_gt_pb_register(dest, reg) \
  flag = (iptr)regs[dest] > (iptr)regs[reg]

#define doi_pb_cmp_op_pb_gt_pb_immediate(instr) \
  do_pb_cmp_op_pb_gt_pb_immediate(INSTR_di_dest(instr), INSTR_di_imm(instr))
#define do_pb_cmp_op_pb_gt_pb_immediate(dest, imm) \
  flag = (iptr)regs[dest] > (iptr)imm

#define doi_pb_cmp_op_pb_le_pb_register(instr) \
  do_pb_cmp_op_pb_le_pb_register(INSTR_dr_dest(instr), INSTR_dr_reg(instr))
#define do_pb_cmp_op_pb_le_pb_register(dest, reg) \
  flag = (iptr)regs[dest] <= (iptr)regs[reg]

#define doi_pb_cmp_op_pb_le_pb_immediate(instr) \
  do_pb_cmp_op_pb_le_pb_immediate(INSTR_di_dest(instr), INSTR_di_imm(instr))
#define do_pb_cmp_op_pb_le_pb_immediate(dest, imm) \
  flag = (iptr)regs[dest] <= (iptr)imm

#define doi_pb_cmp_op_pb_ge_pb_register(instr) \
  do_pb_cmp_op_pb_ge_pb_register(INSTR_dr_dest(instr), INSTR_dr_reg(instr))
#define do_pb_cmp_op_pb_ge_pb_register(dest, reg) \
  flag = (iptr)regs[dest] >= (iptr)regs[reg]

#define doi_pb_cmp_op_pb_ge_pb_immediate(instr) \
  do_pb_cmp_op_pb_ge_pb_immediate(INSTR_di_dest(instr), INSTR_di_imm(instr))
#define do_pb_cmp_op_pb_ge_pb_immediate(dest, imm) \
  flag = (iptr)regs[dest] >= (iptr)imm

#define doi_pb_cmp_op_pb_ab_pb_register(instr) \
  do_pb_cmp_op_pb_ab_pb_register(INSTR_dr_dest(instr), INSTR_dr_reg(instr))
#define do_pb_cmp_op_pb_ab_pb_register(dest, reg) \
  flag = regs[dest] > regs[reg]

#define doi_pb_cmp_op_pb_ab_pb_immediate(instr) \
  do_pb_cmp_op_pb_ab_pb_immediate(INSTR_di_dest(instr), INSTR_di_imm(instr))
#define do_pb_cmp_op_pb_ab_pb_immediate(dest, imm) \
  flag = regs[dest] > (uptr)imm

#define doi_pb_cmp_op_pb_bl_pb_register(instr) \
  do_pb_cmp_op_pb_bl_pb_register(INSTR_dr_dest(instr), INSTR_dr_reg(instr))
#define do_pb_cmp_op_pb_bl_pb_register(dest, reg) \
  flag = regs[dest] < regs[reg]

#define doi_pb_cmp_op_pb_bl_pb_immediate(instr) \
  do_pb_cmp_op_pb_bl_pb_immediate(INSTR_di_dest(instr), INSTR_di_imm(instr))
#define do_pb_cmp_op_pb_bl_pb_immediate(dest, imm) \
  flag = regs[dest] < (uptr)imm

#define doi_pb_cmp_op_pb_cs_pb_register(instr) \
  do_pb_cmp_op_pb_cs_pb_register(INSTR_dr_dest(instr), INSTR_dr_reg(instr))
#define do_pb_cmp_op_pb_cs_pb_register(dest, reg) \
  flag = ((regs[dest] & regs[reg]) != 0)

#define doi_pb_cmp_op_pb_cs_pb_immediate(instr) \
  do_pb_cmp_op_pb_cs_pb_immediate(INSTR_di_dest(instr), INSTR_di_imm(instr))
#define do_pb_cmp_op_pb_cs_pb_immediate(dest, imm) \
  flag = ((regs[dest] & (uptr)imm) != 0)

#define doi_pb_cmp_op_pb_cc_pb_register(instr) \
  do_pb_cmp_op_pb_cc_pb_register(INSTR_dr_dest(instr), INSTR_dr_reg(instr))
#define do_pb_cmp_op_pb_cc_pb_register(dest, reg) \
  flag = ((regs[dest] & regs[reg]) == 0)

#define doi_pb_cmp_op_pb_cc_pb_immediate(instr) \
  do_pb_cmp_op_pb_cc_pb_immediate(INSTR_di_dest(instr), INSTR_di_imm(instr))
#define do_pb_cmp_op_pb_cc_pb_immediate(dest, imm) \
  flag = ((regs[dest] & (uptr)imm) == 0)

#define doi_pb_fp_bin_op_pb_add_pb_register(instr) \
  do_pb_fp_bin_op_pb_add_pb_register(INSTR_drr_dest(instr), INSTR_drr_reg1(instr), INSTR_drr_reg2(instr))
#define do_pb_fp_bin_op_pb_add_pb_register(dest, reg1, reg2) \
  fpregs[dest] = fpregs[reg1] + fpregs[reg2]

#define doi_pb_fp_bin_op_pb_sub_pb_register(instr) \
  do_pb_fp_bin_op_pb_sub_pb_register(INSTR_drr_dest(instr), INSTR_drr_reg1(instr), INSTR_drr_reg2(instr))
#define do_pb_fp_bin_op_pb_sub_pb_register(dest, reg1, reg2) \
  fpregs[dest] = fpregs[reg1] - fpregs[reg2]

#define doi_pb_fp_bin_op_pb_mul_pb_register(instr) \
  do_pb_fp_bin_op_pb_mul_pb_register(INSTR_drr_dest(instr), INSTR_drr_reg1(instr), INSTR_drr_reg2(instr))
#define do_pb_fp_bin_op_pb_mul_pb_register(dest, reg1, reg2) \
  fpregs[dest] = fpregs[reg1] * fpregs[reg2]

#define doi_pb_fp_bin_op_pb_div_pb_register(instr) \
  do_pb_fp_bin_op_pb_div_pb_register(INSTR_drr_dest(instr), INSTR_drr_reg1(instr), INSTR_drr_reg2(instr))
#define do_pb_fp_bin_op_pb_div_pb_register(dest, reg1, reg2) \
  fpregs[dest] = fpregs[reg1] / fpregs[reg2]

#define doi_pb_un_op_pb_not_pb_register(instr) \
  do_pb_un_op_pb_not_pb_register(INSTR_dr_dest(instr), INSTR_dr_reg(instr))
#define do_pb_un_op_pb_not_pb_register(dest, reg) \
  regs[dest] = ~(regs[reg])

#define doi_pb_un_op_pb_not_pb_immediate(instr) \
  do_pb_un_op_pb_not_pb_immediate(INSTR_di_dest(instr), INSTR_di_imm(instr))
#define do_pb_un_op_pb_not_pb_immediate(dest, imm) \
  regs[dest] = ~((uptr)(iptr)imm)

#define doi_pb_fp_un_op_pb_sqrt_pb_register(instr) \
  do_pb_fp_un_op_pb_sqrt_pb_register(INSTR_dr_dest(instr), INSTR_dr_reg(instr))
#define do_pb_fp_un_op_pb_sqrt_pb_register(dest, reg) \
  fpregs[dest] = sqrt(fpregs[reg])

#define doi_pb_fp_cmp_op_pb_eq_pb_register(instr) \
  do_pb_fp_cmp_op_pb_eq_pb_register(INSTR_dr_dest(instr), INSTR_dr_reg(instr))
#define do_pb_fp_cmp_op_pb_eq_pb_register(dest, reg) \
  flag = fpregs[dest] == fpregs[reg]

#define doi_pb_fp_cmp_op_pb_lt_pb_register(instr) \
  do_pb_fp_cmp_op_pb_lt_pb_register(INSTR_dr_dest(instr), INSTR_dr_reg(instr))
#define do_pb_fp_cmp_op_pb_lt_pb_register(dest, reg) \
  flag = fpregs[dest] < fpregs[reg]

#define doi_pb_fp_cmp_op_pb_le_pb_register(instr) \
  do_pb_fp_cmp_op_pb_le_pb_register(INSTR_dr_dest(instr), INSTR_dr_reg(instr))
#define do_pb_fp_cmp_op_pb_le_pb_register(dest, reg) \
  flag = fpregs[dest] <= fpregs[reg]

#if ptr_bits == 64
#define doi_pb_rev_op_pb_int16_pb_register(instr) \
  do_pb_rev_op_pb_int16_pb_register(INSTR_dr_dest(instr), INSTR_dr_reg(instr))
# if USE_BSWAP_INTRINSICS
/* See note below on unsigned swap. */
#  define do_pb_rev_op_pb_int16_pb_register(dest, reg) \
  regs[dest] = ((uptr)(((iptr)((uptr)__builtin_bswap16(regs[reg]) << 48)) >> 48))
# else
#define do_pb_rev_op_pb_int16_pb_register(dest, reg) \
  regs[dest] = ((uptr)((iptr)(regs[reg] << 56) >> 48) \
                                | ((regs[reg] & 0xFF00) >> 8))
# endif
#else
#define doi_pb_rev_op_pb_int16_pb_register(instr) \
  do_pb_rev_op_pb_int16_pb_register(INSTR_dr_dest(instr), INSTR_dr_reg(instr))
#define do_pb_rev_op_pb_int16_pb_register(dest, reg) \
  regs[dest] = ((uptr)((iptr)(regs[reg] << 24) >> 16) \
                                | ((regs[reg] & 0xFF00) >> 8))
#endif

#define doi_pb_rev_op_pb_uint16_pb_register(instr) \
  do_pb_rev_op_pb_uint16_pb_register(INSTR_dr_dest(instr), INSTR_dr_reg(instr))
#define do_pb_rev_op_pb_uint16_pb_register(dest, reg) \
  regs[dest] = (((regs[reg] & 0x00FF) << 8) \
                                | ((regs[reg] & 0xFF00) >> 8))

#if ptr_bits == 64
# define doi_pb_rev_op_pb_int32_pb_register(instr) \
   do_pb_rev_op_pb_int32_pb_register(INSTR_dr_dest(instr), INSTR_dr_reg(instr))
# if USE_BSWAP_INTRINSICS
/* x86_64 GCC before 12.2 incorrectly compiles the code below to an unsigned swap.
   Defeat that by using the unsigned-swap intrinsic (which is good, anyway), then
   shift up and back. */
#  define do_pb_rev_op_pb_int32_pb_register(dest, reg) \
  regs[dest] = ((uptr)(((iptr)((uptr)__builtin_bswap32(regs[reg]) << 32)) >> 32))
# else
#  define do_pb_rev_op_pb_int32_pb_register(dest, reg) \
  regs[dest] = ((uptr)((iptr)(regs[reg] << 56) >> 32) \
                                | ((regs[reg] & (uptr)0xFF000000) >> 24) \
                                | ((regs[reg] & (uptr)0x00FF0000) >> 8) \
                                | ((regs[reg] & (uptr)0x0000FF00) << 8))
# endif
#else
# define doi_pb_rev_op_pb_int32_pb_register(instr) \
   do_pb_rev_op_pb_int32_pb_register(INSTR_dr_dest(instr), INSTR_dr_reg(instr))
# define do_pb_rev_op_pb_int32_pb_register(dest, reg) \
  regs[dest] = ((regs[reg] << 24)       \
                                | ((regs[reg] & (uptr)0xFF000000) >> 24) \
                                | ((regs[reg] & (uptr)0x00FF0000) >> 8) \
                                | ((regs[reg] & (uptr)0x0000FF00) << 8))
#endif

#define doi_pb_rev_op_pb_uint32_pb_register(instr) \
  do_pb_rev_op_pb_uint32_pb_register(INSTR_dr_dest(instr), INSTR_dr_reg(instr))
#define do_pb_rev_op_pb_uint32_pb_register(dest, reg) \
  regs[dest] = (((regs[reg] & (uptr)0x000000FF) << 24) \
                                | ((regs[reg] & (uptr)0xFF000000) >> 24) \
                                | ((regs[reg] & (uptr)0x00FF0000) >> 8) \
                                | ((regs[reg] & (uptr)0x0000FF00) << 8))

#if ptr_bits == 64
# define doi_pb_rev_op_pb_int64_pb_register(instr) \
   do_pb_rev_op_pb_int64_pb_register(INSTR_dr_dest(instr), INSTR_dr_reg(instr))
# define do_pb_rev_op_pb_int64_pb_register(dest, reg) \
  regs[dest] = (((regs[reg] & (uptr)0x00000000000000FF) << 56) \
                                | ((regs[reg] & (uptr)0x000000000000FF00) << 40) \
                                | ((regs[reg] & (uptr)0x0000000000FF0000) << 24) \
                                | ((regs[reg] & (uptr)0x00000000FF000000) << 8) \
                                | ((regs[reg] & (uptr)0x000000FF00000000) >> 8) \
                                | ((regs[reg] & (uptr)0x0000FF0000000000) >> 24) \
                                | ((regs[reg] & (uptr)0x00FF000000000000) >> 40) \
                                | ((regs[reg] & (uptr)0xFF00000000000000) >> 56))
#else
# define doi_pb_rev_op_pb_int64_pb_register(instr) \
   do_pb_rev_op_pb_int64_pb_register(INSTR_dr_dest(instr), INSTR_dr_reg(instr))
# define do_pb_rev_op_pb_int64_pb_register(dest, reg) \
  regs[dest] = (((regs[reg] & (uptr)0x000000FF) << 24) \
                                | ((regs[reg] & (uptr)0xFF000000) >> 24) \
                                | ((regs[reg] & (uptr)0x00FF0000) >> 8) \
                                | ((regs[reg] & (uptr)0x0000FF00) << 8))
#endif

#define doi_pb_ld_op_pb_int8_pb_register(instr) \
  do_pb_ld_op_pb_int8_pb_register(INSTR_drr_dest(instr), INSTR_drr_reg1(instr), INSTR_drr_reg2(instr))
#define do_pb_ld_op_pb_int8_pb_register(dest, reg1, reg2) \
  regs[dest] = *(int8_t *)TO_VOIDP(regs[reg1] + regs[reg2])

#if defined(__arm__)
/* Complicated load to avoid an internal compiler error from an old gcc on Raspbian: */
# define doi_pb_ld_op_pb_int8_pb_immediate(instr) \
   do_pb_ld_op_pb_int8_pb_immediate(INSTR_dri_dest(instr), INSTR_dri_reg(instr), INSTR_dri_imm(instr))
# define do_pb_ld_op_pb_int8_pb_immediate(dest, reg, imm) \
  do {                                                                  \
    int8_t v;                                                           \
    memcpy(&v, TO_VOIDP(regs[reg] + imm), sizeof(int8_t)); \
    regs[dest] = v;                                    \
  } while (0)
#else
# define doi_pb_ld_op_pb_int8_pb_immediate(instr) \
   do_pb_ld_op_pb_int8_pb_immediate(INSTR_dri_dest(instr), INSTR_dri_reg(instr), INSTR_dri_imm(instr))
# define do_pb_ld_op_pb_int8_pb_immediate(dest, reg, imm) \
  regs[dest] = *(int8_t *)TO_VOIDP(regs[reg] + imm)
#endif

#define doi_pb_ld_op_pb_uint8_pb_register(instr) \
  do_pb_ld_op_pb_uint8_pb_register(INSTR_drr_dest(instr), INSTR_drr_reg1(instr), INSTR_drr_reg2(instr))
#define do_pb_ld_op_pb_uint8_pb_register(dest, reg1, reg2) \
  regs[dest] = *(uint8_t *)TO_VOIDP(regs[reg1] + regs[reg2])

#define doi_pb_ld_op_pb_uint8_pb_immediate(instr) \
  do_pb_ld_op_pb_uint8_pb_immediate(INSTR_dri_dest(instr), INSTR_dri_reg(instr), INSTR_dri_imm(instr))
#define do_pb_ld_op_pb_uint8_pb_immediate(dest, reg, imm) \
  regs[dest] = *(uint8_t *)TO_VOIDP(regs[reg] + imm)

#define doi_pb_ld_op_pb_int16_pb_register(instr) \
  do_pb_ld_op_pb_int16_pb_register(INSTR_drr_dest(instr), INSTR_drr_reg1(instr), INSTR_drr_reg2(instr))
#define do_pb_ld_op_pb_int16_pb_register(dest, reg1, reg2) \
  regs[dest] = *(int16_t *)TO_VOIDP(regs[reg1] + regs[reg2])

#define doi_pb_ld_op_pb_int16_pb_immediate(instr) \
  do_pb_ld_op_pb_int16_pb_immediate(INSTR_dri_dest(instr), INSTR_dri_reg(instr), INSTR_dri_imm(instr))
#define do_pb_ld_op_pb_int16_pb_immediate(dest, reg, imm) \
  regs[dest] = *(int16_t *)TO_VOIDP(regs[reg] + imm)

#define doi_pb_ld_op_pb_uint16_pb_register(instr) \
  do_pb_ld_op_pb_uint16_pb_register(INSTR_drr_dest(instr), INSTR_drr_reg1(instr), INSTR_drr_reg2(instr))
#define do_pb_ld_op_pb_uint16_pb_register(dest, reg1, reg2) \
  regs[dest] = *(uint16_t *)TO_VOIDP(regs[reg1] + regs[reg2])

#define doi_pb_ld_op_pb_uint16_pb_immediate(instr) \
  do_pb_ld_op_pb_uint16_pb_immediate(INSTR_dri_dest(instr), INSTR_dri_reg(instr), INSTR_dri_imm(instr))
#define do_pb_ld_op_pb_uint16_pb_immediate(dest, reg, imm) \
  regs[dest] = *(uint16_t *)TO_VOIDP(regs[reg] + imm)

#define doi_pb_ld_op_pb_int32_pb_register(instr) \
  do_pb_ld_op_pb_int32_pb_register(INSTR_drr_dest(instr), INSTR_drr_reg1(instr), INSTR_drr_reg2(instr))
#define do_pb_ld_op_pb_int32_pb_register(dest, reg1, reg2) \
  regs[dest] = *(int32_t *)TO_VOIDP(regs[reg1] + regs[reg2])

#define doi_pb_ld_op_pb_int32_pb_immediate(instr) \
  do_pb_ld_op_pb_int32_pb_immediate(INSTR_dri_dest(instr), INSTR_dri_reg(instr), INSTR_dri_imm(instr))
#define do_pb_ld_op_pb_int32_pb_immediate(dest, reg, imm) \
  regs[dest] = *(int32_t *)TO_VOIDP(regs[reg] + imm)

#define doi_pb_ld_op_pb_uint32_pb_register(instr) \
  do_pb_ld_op_pb_uint32_pb_register(INSTR_drr_dest(instr), INSTR_drr_reg1(instr), INSTR_drr_reg2(instr))
#define do_pb_ld_op_pb_uint32_pb_register(dest, reg1, reg2) \
  regs[dest] = *(uint32_t *)TO_VOIDP(regs[reg1] + regs[reg2])

#define doi_pb_ld_op_pb_uint32_pb_immediate(instr) \
  do_pb_ld_op_pb_uint32_pb_immediate(INSTR_dri_dest(instr), INSTR_dri_reg(instr), INSTR_dri_imm(instr))
#define do_pb_ld_op_pb_uint32_pb_immediate(dest, reg, imm) \
  regs[dest] = *(uint32_t *)TO_VOIDP(regs[reg] + imm)

#define doi_pb_ld_op_pb_int64_pb_register(instr) \
  do_pb_ld_op_pb_int64_pb_register(INSTR_drr_dest(instr), INSTR_drr_reg1(instr), INSTR_drr_reg2(instr))
#define do_pb_ld_op_pb_int64_pb_register(dest, reg1, reg2) \
  regs[dest] = *(uptr *)TO_VOIDP(regs[reg1] + regs[reg2])

#define doi_pb_ld_op_pb_int64_pb_immediate(instr) \
  do_pb_ld_op_pb_int64_pb_immediate(INSTR_dri_dest(instr), INSTR_dri_reg(instr), INSTR_dri_imm(instr))
#define do_pb_ld_op_pb_int64_pb_immediate(dest, reg, imm) \
  regs[dest] = *(uptr *)TO_VOIDP(regs[reg] + imm)

#define doi_pb_ld_op_pb_double_pb_register(instr) \
  do_pb_ld_op_pb_double_pb_register(INSTR_drr_dest(instr), INSTR_drr_reg1(instr), INSTR_drr_reg2(instr))
#define do_pb_ld_op_pb_double_pb_register(dest, reg1, reg2) \
  fpregs[dest] = *(double *)TO_VOIDP(regs[reg1] + regs[reg2])

#define doi_pb_ld_op_pb_double_pb_immediate(instr) \
  do_pb_ld_op_pb_double_pb_immediate(INSTR_dri_dest(instr), INSTR_dri_reg(instr), INSTR_dri_imm(instr))
#define do_pb_ld_op_pb_double_pb_immediate(dest, reg, imm) \
  fpregs[dest] = *(double *)TO_VOIDP(regs[reg] + imm)

#define doi_pb_ld_op_pb_single_pb_register(instr) \
  do_pb_ld_op_pb_single_pb_register(INSTR_drr_dest(instr), INSTR_drr_reg1(instr), INSTR_drr_reg2(instr))
#define do_pb_ld_op_pb_single_pb_register(dest, reg1, reg2) \
  fpregs[dest] =  *(float *)TO_VOIDP(regs[reg1] + regs[reg2])

#define doi_pb_ld_op_pb_single_pb_immediate(instr) \
  do_pb_ld_op_pb_single_pb_immediate(INSTR_dri_dest(instr), INSTR_dri_reg(instr), INSTR_dri_imm(instr))
#define do_pb_ld_op_pb_single_pb_immediate(dest, reg, imm) \
  fpregs[dest] = *(float *)TO_VOIDP(regs[reg] + imm)

#define doi_pb_st_op_pb_int8_pb_register(instr) \
  do_pb_st_op_pb_int8_pb_register(INSTR_drr_dest(instr), INSTR_drr_reg1(instr), INSTR_drr_reg2(instr))
#define do_pb_st_op_pb_int8_pb_register(dest, reg1, reg2) \
  *(char *)TO_VOIDP(regs[reg1] + regs[reg2]) = (char)regs[dest]

#define doi_pb_st_op_pb_int8_pb_immediate(instr) \
  do_pb_st_op_pb_int8_pb_immediate(INSTR_dri_dest(instr), INSTR_dri_reg(instr), INSTR_dri_imm(instr))
#define do_pb_st_op_pb_int8_pb_immediate(dest, reg, imm) \
  *(char *)TO_VOIDP(regs[reg] + imm) = (char)regs[dest]

#define doi_pb_st_op_pb_int16_pb_register(instr) \
  do_pb_st_op_pb_int16_pb_register(INSTR_drr_dest(instr), INSTR_drr_reg1(instr), INSTR_drr_reg2(instr))
#define do_pb_st_op_pb_int16_pb_register(dest, reg1, reg2) \
  *(short *)TO_VOIDP(regs[reg1] + regs[reg2]) = (short)regs[dest]

#define doi_pb_st_op_pb_int16_pb_immediate(instr) \
  do_pb_st_op_pb_int16_pb_immediate(INSTR_dri_dest(instr), INSTR_dri_reg(instr), INSTR_dri_imm(instr))
#define do_pb_st_op_pb_int16_pb_immediate(dest, reg, imm) \
  *(short *)TO_VOIDP(regs[reg] + imm) = (short)regs[dest]

#define doi_pb_st_op_pb_int32_pb_register(instr) \
  do_pb_st_op_pb_int32_pb_register(INSTR_drr_dest(instr), INSTR_drr_reg1(instr), INSTR_drr_reg2(instr))
#define do_pb_st_op_pb_int32_pb_register(dest, reg1, reg2) \
  *(int *)TO_VOIDP(regs[reg1] + regs[reg2]) = (int)regs[dest]

#define doi_pb_st_op_pb_int32_pb_immediate(instr) \
  do_pb_st_op_pb_int32_pb_immediate(INSTR_dri_dest(instr), INSTR_dri_reg(instr), INSTR_dri_imm(instr))
#define do_pb_st_op_pb_int32_pb_immediate(dest, reg, imm) \
  *(int *)TO_VOIDP(regs[reg] + imm) = (int)regs[dest]

#define doi_pb_st_op_pb_int64_pb_register(instr) \
  do_pb_st_op_pb_int64_pb_register(INSTR_drr_dest(instr), INSTR_drr_reg1(instr), INSTR_drr_reg2(instr))
#define do_pb_st_op_pb_int64_pb_register(dest, reg1, reg2) \
  *(uptr *)TO_VOIDP(regs[reg1] + regs[reg2]) = regs[dest]

#define doi_pb_st_op_pb_int64_pb_immediate(instr) \
  do_pb_st_op_pb_int64_pb_immediate(INSTR_dri_dest(instr), INSTR_dri_reg(instr), INSTR_dri_imm(instr))
#define do_pb_st_op_pb_int64_pb_immediate(dest, reg, imm) \
  *(uptr *)TO_VOIDP(regs[reg] + imm) = regs[dest]

#define doi_pb_st_op_pb_double_pb_register(instr) \
  do_pb_st_op_pb_double_pb_register(INSTR_drr_dest(instr), INSTR_drr_reg1(instr), INSTR_drr_reg2(instr))
#define do_pb_st_op_pb_double_pb_register(dest, reg1, reg2) \
  *(double *)TO_VOIDP(regs[reg1] + regs[reg2]) = fpregs[dest]

#define doi_pb_st_op_pb_double_pb_immediate(instr) \
  do_pb_st_op_pb_double_pb_immediate(INSTR_dri_dest(instr), INSTR_dri_reg(instr), INSTR_dri_imm(instr))
#define do_pb_st_op_pb_double_pb_immediate(dest, reg, imm) \
  *(double *)TO_VOIDP(regs[reg] + imm) = fpregs[dest]

#define doi_pb_st_op_pb_single_pb_register(instr) \
  do_pb_st_op_pb_single_pb_register(INSTR_drr_dest(instr), INSTR_drr_reg1(instr), INSTR_drr_reg2(instr))
#define do_pb_st_op_pb_single_pb_register(dest, reg1, reg2) \
  *(float *)TO_VOIDP(regs[reg1] + regs[reg2]) = (float)fpregs[dest]

#define doi_pb_st_op_pb_single_pb_immediate(instr) \
  do_pb_st_op_pb_single_pb_immediate(INSTR_dri_dest(instr), INSTR_dri_reg(instr), INSTR_dri_imm(instr))
#define do_pb_st_op_pb_single_pb_immediate(dest, reg, imm) \
  *(float *)TO_VOIDP(regs[reg] + imm) = (float)fpregs[dest]

#if defined(PTHREADS)
# define COMPARE_AND_SWAP_PTR_SEQOK(addr, old_r, r) \
  COMPARE_AND_SWAP_PTR(TO_VOIDP(addr), TO_VOIDP(old_r), TO_VOIDP(r))
#else
# define COMPARE_AND_SWAP_PTR_SEQOK(addr, old_r, r) \
  (*(uptr *)TO_VOIDP(addr) = r, 1)
#endif

#define doi_pb_inc_pb_register(instr) \
  do_pb_inc_pb_register(INSTR_dr_dest(instr), INSTR_dr_reg(instr))
#define do_pb_inc_pb_register(dest, reg)                                \
  do {                                                                  \
    uptr addr = regs[dest];                                             \
    while (1) {                                                         \
      uptr old_r = *(uptr *)TO_VOIDP(addr);                             \
      uptr r = old_r + regs[reg];                                       \
      if (COMPARE_AND_SWAP_PTR_SEQOK(addr, old_r, r)) {                 \
        flag = (r == 0);                                                \
        break;                                                          \
      }                                                                 \
    }                                                                   \
  } while (0)

#define doi_pb_inc_pb_immediate(instr) \
  do_pb_inc_pb_immediate(INSTR_di_dest(instr), INSTR_di_imm(instr))
#define do_pb_inc_pb_immediate(dest, imm) \
  do {                                                                  \
    uptr addr = regs[dest];                                             \
    while (1) {                                                         \
      uptr old_r = *(uptr *)TO_VOIDP(addr);                             \
      uptr r = old_r + imm;                                             \
      if (COMPARE_AND_SWAP_PTR_SEQOK(addr, old_r, r)) {                 \
        flag = (r == 0);                                                \
        break;                                                          \
      }                                                                 \
    }                                                                   \
  } while (0)

#if defined(PTHREADS)
# define doi_pb_lock(instr) \
   do_pb_lock(INSTR_d_dest(instr))
# define do_pb_lock(dest)                                        \
  do {                                                           \
    uptr *l = TO_VOIDP(regs[dest]);                              \
    flag = COMPARE_AND_SWAP_PTR(l, TO_VOIDP(0), TO_VOIDP(1));    \
  } while (0)
#else
# define doi_pb_lock(instr) \
   do_pb_lock(INSTR_d_dest(instr))
# define do_pb_lock(dest) \
  do {                                                 \
    uptr *l = TO_VOIDP(regs[dest]);                    \
    if (*l == 0) {                                     \
      *l = 1;                                          \
      flag = 1;                                        \
    } else                                             \
      flag = 0;                                        \
  } while (0)
#endif

#if defined(PTHREADS)
# define doi_pb_cas(instr) \
   do_pb_cas(INSTR_drr_dest(instr), INSTR_drr_reg1(instr), INSTR_drr_reg2(instr))
# define do_pb_cas(dest, reg1, reg2)          \
  do {                                        \
    uptr *l = TO_VOIDP(regs[dest]);           \
    uptr old = regs[reg1];                    \
    uptr new = regs[reg2];                    \
    flag = COMPARE_AND_SWAP_PTR(l, TO_VOIDP(old), TO_VOIDP(new));     \
  } while (0)
#else
#define doi_pb_cas(instr) \
  do_pb_cas(INSTR_drr_dest(instr), INSTR_drr_reg1(instr), INSTR_drr_reg2(instr))
#define do_pb_cas(dest, reg1, reg2)                      \
  do {                                                   \
    uptr *l = TO_VOIDP(regs[dest]);                      \
    uptr old = regs[reg1];                               \
    uptr new = regs[reg2];                               \
    if (*l == old) {                                     \
      *l = new;                                          \
      flag = 1;                                          \
    } else                                               \
      flag = 0;                                          \
  } while (0)
#endif

#define doi_pb_fence_pb_fence_store_store(instr) \
  do_pb_fence_pb_fence_store_store()
#define do_pb_fence_pb_fence_store_store() \
  STORE_FENCE()

#define doi_pb_fence_pb_fence_acquire(instr) \
  do_pb_fence_pb_fence_acquire()
#define do_pb_fence_pb_fence_acquire() \
  ACQUIRE_FENCE()

#define doi_pb_fence_pb_fence_release(instr) \
  do_pb_fence_pb_fence_release()
#define do_pb_fence_pb_fence_release() \
  RELEASE_FENCE()

#define doi_pb_call_arena_in(instr) \
  do_pb_call_arena_in(INSTR_di_dest(instr), INSTR_di_imm(instr))
#define do_pb_call_arena_in(dest, imm) \
  *(ptr *)TO_VOIDP(((uptr)TO_PTR(call_arena) + imm)) = regs[dest]

#define doi_pb_fp_call_arena_in(instr) \
  do_pb_fp_call_arena_in(INSTR_di_dest(instr), INSTR_di_imm(instr))
#define do_pb_fp_call_arena_in(dest, imm) \
  *(double *)TO_VOIDP(((uptr)TO_PTR(call_arena) + imm)) = fpregs[dest]

#define doi_pb_call_arena_out(instr) \
  do_pb_call_arena_out(INSTR_di_dest(instr), INSTR_di_imm(instr))
#define do_pb_call_arena_out(dest, imm) \
  regs[dest] = *(ptr *)TO_VOIDP((uptr)TO_PTR(call_arena) + imm)

#define doi_pb_fp_call_arena_out(instr) \
  do_pb_fp_call_arena_out(INSTR_di_dest(instr), INSTR_di_imm(instr))
#define do_pb_fp_call_arena_out(dest, imm) \
  fpregs[dest] = *(double *)TO_VOIDP((uptr)TO_PTR(call_arena) + imm)

#define doi_pb_stack_call(instr) \
  do_pb_stack_call(INSTR_dr_dest(instr), INSTR_dr_reg(instr))
#define do_pb_stack_call(dest, reg) \
  S_ffi_call(regs[reg], regs[dest], (ptr *)call_arena)

#define geti_pb_bs_op_pb_register_addr(instr) \
  get_pb_bs_op_pb_register_addr(INSTR_dr_dest(instr), INSTR_dr_reg(instr))
#define get_pb_bs_op_pb_register_addr(dest, reg) \
  (*(uptr *)TO_VOIDP(regs[dest] + regs[reg]))

#define geti_pb_bs_op_pb_immediate_addr(instr) \
  get_pb_bs_op_pb_immediate_addr(INSTR_di_dest(instr), INSTR_di_imm(instr))
#define get_pb_bs_op_pb_immediate_addr(dest, imm) \
  (*(uptr *)TO_VOIDP(regs[dest] + imm))

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
  regs[dest] = (ptr)LOAD_UNALIGNED_UPTR(ip)

#define load_code_relative(dest, ip) \
  regs[dest] = ip

#define code_rel(start_i, i) ((i)-(start_i))

#define MACHINE_STATE machine_state * RESTRICT_PTR
