#include "system.h"
#include <string.h>
#include <math.h>

/* Interpreter for portable bytecode. See "pb.ss". */

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

#define SHIFT_MASK(v) ((v) & (ptr_bits-1))

static uptr regs[16];
static double fpregs[8];

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

void S_machine_init() {}

#define SIGN_FLIP(r, a, b) ((~((a ^ b) | (r ^ ~b))) >> (ptr_bits-1))

#if (__GNUC__ >= 5) || defined(__clang__)
# define USE_OVERFLOW_INTRINSICS 1
#else
# define USE_OVERFLOW_INTRINSICS 0
#endif

#if 0
# define TRACE(print, record) print
#elif 0
# define TRACE(print, record) record
static instruction_t *branch_from, *branch_to;
static instruction_t *jump_from, *jump_to;
static instruction_t *interp_from, *interp_to;
static instruction_t *call_from; static void *call_to;
#else
# define TRACE(print, record) /* empty */
#endif

void S_pb_interp(ptr tc, void *bytecode) {
  instruction_t *ip = (instruction_t *)bytecode, *next_ip, instr;
  int flag = 0;

  regs[0] = (uptr)tc;

  TRACE(printf("enter %p\n", ip), );

  while (1) {
    instr = *ip;
    next_ip = ip + 1;

    switch(INSTR_op(instr)) {
    case pb_mov16_pb_zero_bits_pb_shift0:
      regs[INSTR_di_dest(instr)] = (uptr)INSTR_di_imm_unsigned(instr);
      break;
    case pb_mov16_pb_zero_bits_pb_shift1:
      regs[INSTR_di_dest(instr)] = (uptr)INSTR_di_imm_unsigned(instr) << 16;
      break;
    case pb_mov16_pb_zero_bits_pb_shift2:
#if ptr_bits == 64      
      regs[INSTR_di_dest(instr)] = (uptr)INSTR_di_imm_unsigned(instr) << 32;
#else
      regs[INSTR_di_dest(instr)] = 0;
#endif
      break;
    case pb_mov16_pb_zero_bits_pb_shift3:
#if ptr_bits == 64      
      regs[INSTR_di_dest(instr)] = (uptr)INSTR_di_imm_unsigned(instr) << 48;
#else
      regs[INSTR_di_dest(instr)] = 0;
#endif
      break;
    case pb_mov16_pb_keep_bits_pb_shift0:
      regs[INSTR_di_dest(instr)] |= (uptr)INSTR_di_imm_unsigned(instr);
      break;
    case pb_mov16_pb_keep_bits_pb_shift1:
      regs[INSTR_di_dest(instr)] |= (uptr)INSTR_di_imm_unsigned(instr) << 16;
      break;
    case pb_mov16_pb_keep_bits_pb_shift2:
#if ptr_bits == 64      
      regs[INSTR_di_dest(instr)] |= (uptr)INSTR_di_imm_unsigned(instr) << 32;
#endif
      break;
    case pb_mov16_pb_keep_bits_pb_shift3:
#if ptr_bits == 64      
      regs[INSTR_di_dest(instr)] |= (uptr)INSTR_di_imm_unsigned(instr) << 48;
#endif
      break;
    case pb_mov_pb_i_i:
      regs[INSTR_dr_dest(instr)] = regs[INSTR_dr_reg(instr)];
      break;
    case pb_mov_pb_d_d:
      fpregs[INSTR_dr_dest(instr)] = fpregs[INSTR_dr_reg(instr)];
      break;
    case pb_mov_pb_i_d:
      fpregs[INSTR_dr_dest(instr)] = (double)(iptr)regs[INSTR_dr_reg(instr)];
      break;
    case pb_mov_pb_d_i:
      regs[INSTR_dr_dest(instr)] = (iptr)fpregs[INSTR_dr_reg(instr)];
      break;
#if ptr_bits == 64
    case pb_mov_pb_i_bits_d_bits:
      memcpy(&fpregs[INSTR_dr_dest(instr)], &regs[INSTR_dr_reg(instr)], sizeof(double));
      break;
    case pb_mov_pb_d_bits_i_bits:
      memcpy(&regs[INSTR_dr_dest(instr)], &fpregs[INSTR_dr_reg(instr)], sizeof(double));
      break;
#else
    case pb_mov_pb_i_i_bits_d_bits:
      {
        uint64_t d;
        d = regs[INSTR_drr_reg1(instr)] | ((uint64_t)regs[INSTR_drr_reg2(instr)] << 32);
        memcpy(&fpregs[INSTR_drr_dest(instr)], &d, sizeof(double));
      }
      break;
    case pb_mov_pb_d_lo_bits_i_bits:
      {
        uint64_t d;
        memcpy(&d, &fpregs[INSTR_dr_reg(instr)], sizeof(double));
        regs[INSTR_dr_dest(instr)] = d;
      }
      break;
    case pb_mov_pb_d_hi_bits_i_bits:
      {
        uint64_t d;
        memcpy(&d, &fpregs[INSTR_dr_reg(instr)], sizeof(double));
        d >>= 32;
        regs[INSTR_dr_dest(instr)] = d;
      }
      break;
#endif      
    case pb_mov_pb_s_d:
      {
        float f;
#ifdef PORTABLE_BYTECODE_BIGENDIAN
        memcpy(&f, (char *)&fpregs[INSTR_dr_reg(instr)] + 4, sizeof(float));
#else
        memcpy(&f, &fpregs[INSTR_dr_reg(instr)], sizeof(float));
#endif
        fpregs[INSTR_dr_dest(instr)] = f;
      }
      break;
    case pb_mov_pb_d_s:
      {
        float f;
        f = fpregs[INSTR_dr_reg(instr)];
#ifdef PORTABLE_BYTECODE_BIGENDIAN
        memcpy((char *)&fpregs[INSTR_dr_dest(instr)] + 4, &f, sizeof(float));
#else
        memcpy(&fpregs[INSTR_dr_dest(instr)], &f, sizeof(float));
#endif
      }
      break;
    case pb_mov_pb_d_s_d:
      {
        float f;
        f = fpregs[INSTR_dr_reg(instr)];
        fpregs[INSTR_dr_dest(instr)] = (double)f;
      }
      break;
    case pb_bin_op_pb_no_signal_pb_add_pb_register:
      regs[INSTR_drr_dest(instr)] = regs[INSTR_drr_reg1(instr)] + regs[INSTR_drr_reg2(instr)];
      break;
    case pb_bin_op_pb_no_signal_pb_add_pb_immediate:
      regs[INSTR_dri_dest(instr)] = regs[INSTR_dri_reg(instr)] + (uptr)INSTR_dri_imm(instr);
      break;
    case pb_bin_op_pb_no_signal_pb_sub_pb_register:
      regs[INSTR_drr_dest(instr)] = regs[INSTR_drr_reg1(instr)] - regs[INSTR_drr_reg2(instr)];
      break;
    case pb_bin_op_pb_no_signal_pb_sub_pb_immediate:
      regs[INSTR_dri_dest(instr)] = regs[INSTR_dri_reg(instr)] - (uptr)INSTR_dri_imm(instr);
      break;
    case pb_bin_op_pb_no_signal_pb_mul_pb_register:
      regs[INSTR_drr_dest(instr)] = regs[INSTR_drr_reg1(instr)] * regs[INSTR_drr_reg2(instr)];
      break;
    case pb_bin_op_pb_no_signal_pb_mul_pb_immediate:
      regs[INSTR_dri_dest(instr)] = (uptr)regs[INSTR_dri_reg(instr)] * (uptr)INSTR_dri_imm(instr);
      break;
    case pb_bin_op_pb_no_signal_pb_div_pb_register:
      regs[INSTR_drr_dest(instr)] = (iptr)regs[INSTR_drr_reg1(instr)] / (iptr)regs[INSTR_drr_reg2(instr)];
      break;
    case pb_bin_op_pb_no_signal_pb_div_pb_immediate:
      regs[INSTR_dri_dest(instr)] = (iptr)regs[INSTR_dri_reg(instr)] / (iptr)INSTR_dri_imm(instr);
      break;
    case pb_bin_op_pb_no_signal_pb_and_pb_register:
      regs[INSTR_drr_dest(instr)] = regs[INSTR_drr_reg1(instr)] & regs[INSTR_drr_reg2(instr)];
      break;
    case pb_bin_op_pb_no_signal_pb_and_pb_immediate:
      regs[INSTR_dri_dest(instr)] = regs[INSTR_dri_reg(instr)] & (uptr)INSTR_dri_imm(instr);
      break;
    case pb_bin_op_pb_no_signal_pb_ior_pb_register:
      regs[INSTR_drr_dest(instr)] = regs[INSTR_drr_reg1(instr)] | regs[INSTR_drr_reg2(instr)];
      break;
    case pb_bin_op_pb_no_signal_pb_ior_pb_immediate:
      regs[INSTR_dri_dest(instr)] = regs[INSTR_dri_reg(instr)] | (uptr)INSTR_dri_imm(instr);
      break;
    case pb_bin_op_pb_no_signal_pb_xor_pb_register:
      regs[INSTR_drr_dest(instr)] = regs[INSTR_drr_reg1(instr)] ^ regs[INSTR_drr_reg2(instr)];
      break;
    case pb_bin_op_pb_no_signal_pb_xor_pb_immediate:
      regs[INSTR_dri_dest(instr)] = regs[INSTR_dri_reg(instr)] ^ (uptr)INSTR_dri_imm(instr);
      break;
    case pb_bin_op_pb_no_signal_pb_lsl_pb_register:
      regs[INSTR_drr_dest(instr)] = regs[INSTR_drr_reg1(instr)] << SHIFT_MASK(regs[INSTR_drr_reg2(instr)]);
      break;
    case pb_bin_op_pb_no_signal_pb_lsl_pb_immediate:
      regs[INSTR_dri_dest(instr)] = regs[INSTR_dri_reg(instr)] << SHIFT_MASK(INSTR_dri_imm(instr));
      break;
    case pb_bin_op_pb_no_signal_pb_lsr_pb_register:
      regs[INSTR_drr_dest(instr)] = regs[INSTR_drr_reg1(instr)] >> SHIFT_MASK(regs[INSTR_drr_reg2(instr)]);
      break;
    case pb_bin_op_pb_no_signal_pb_lsr_pb_immediate:
      regs[INSTR_dri_dest(instr)] = regs[INSTR_dri_reg(instr)] >> SHIFT_MASK(INSTR_dri_imm(instr));
      break;
    case pb_bin_op_pb_no_signal_pb_asr_pb_register:
      regs[INSTR_drr_dest(instr)] = (iptr)regs[INSTR_drr_reg1(instr)] >> SHIFT_MASK(regs[INSTR_drr_reg2(instr)]);
      break;
    case pb_bin_op_pb_no_signal_pb_asr_pb_immediate:
      regs[INSTR_dri_dest(instr)] = (iptr)regs[INSTR_dri_reg(instr)] >> SHIFT_MASK(INSTR_dri_imm(instr));
      break;
    case pb_bin_op_pb_no_signal_pb_lslo_pb_register:
#ifdef PORTABLE_BYTECODE_BIGENDIAN
      regs[INSTR_drr_dest(instr)] = regs[INSTR_drr_reg1(instr)] >> regs[INSTR_drr_reg2(instr)];
#else
      regs[INSTR_drr_dest(instr)] = regs[INSTR_drr_reg1(instr)] << regs[INSTR_drr_reg2(instr)];
#endif
      break;
    case pb_bin_op_pb_no_signal_pb_lslo_pb_immediate:
#ifdef PORTABLE_BYTECODE_BIGENDIAN
      regs[INSTR_dri_dest(instr)] = regs[INSTR_dri_reg(instr)] >> INSTR_dri_imm(instr);
#else
      regs[INSTR_dri_dest(instr)] = regs[INSTR_dri_reg(instr)] << INSTR_dri_imm(instr);
#endif
      break;
    case pb_bin_op_pb_signal_pb_add_pb_register:
      {
#if USE_OVERFLOW_INTRINSICS
        iptr a = (iptr)regs[INSTR_drr_reg1(instr)];
        iptr b = (iptr)regs[INSTR_drr_reg2(instr)];
        iptr r;
        flag = __builtin_add_overflow(a, b, &r);
        regs[INSTR_drr_dest(instr)] = (uptr)r;
#else
        uptr a = regs[INSTR_drr_reg1(instr)];
        uptr b = regs[INSTR_drr_reg2(instr)];
        uptr r = a + b;
        regs[INSTR_drr_dest(instr)] = r;
        flag = SIGN_FLIP(r, a, b);
#endif
      }
      break;
    case pb_bin_op_pb_signal_pb_add_pb_immediate:
      {
#if USE_OVERFLOW_INTRINSICS
        iptr a = (iptr)regs[INSTR_dri_reg(instr)];
        iptr b = INSTR_dri_imm(instr);
        iptr r;
        flag = __builtin_add_overflow(a, b, &r);
        regs[INSTR_drr_dest(instr)] = (uptr)r;
#else
        uptr a = regs[INSTR_dri_reg(instr)];
        uptr b = (uptr)INSTR_dri_imm(instr);
        uptr r = a + b;
        regs[INSTR_dri_dest(instr)] = r;
        flag = SIGN_FLIP(r, a, b);
#endif
      }
      break;
    case pb_bin_op_pb_signal_pb_sub_pb_register:
      {
#if USE_OVERFLOW_INTRINSICS
        iptr a = (iptr)regs[INSTR_drr_reg1(instr)];
        iptr b = (iptr)regs[INSTR_drr_reg2(instr)];
        iptr r;
        flag = __builtin_sub_overflow(a, b, &r);
        regs[INSTR_drr_dest(instr)] = (uptr)r;
#else
        uptr a = regs[INSTR_drr_reg1(instr)];
        uptr b = regs[INSTR_drr_reg2(instr)];
        uptr r = a - b;
        regs[INSTR_drr_dest(instr)] = r;
        flag = SIGN_FLIP(r, a, ~b);
#endif
      }
      break;
    case pb_bin_op_pb_signal_pb_sub_pb_immediate:
      {
#if USE_OVERFLOW_INTRINSICS
        iptr a = (iptr)regs[INSTR_dri_reg(instr)];
        iptr b = INSTR_dri_imm(instr);
        iptr r;
        flag = __builtin_sub_overflow(a, b, &r);
        regs[INSTR_drr_dest(instr)] = (uptr)r;
#else
        uptr a = regs[INSTR_dri_reg(instr)];
        uptr b = (uptr)INSTR_dri_imm(instr);
        uptr r = a - b;
        regs[INSTR_dri_dest(instr)] = r;
        flag = SIGN_FLIP(r, a, ~b);
#endif
      }
      break;
    case pb_bin_op_pb_signal_pb_mul_pb_register:
      {
#if USE_OVERFLOW_INTRINSICS
        iptr a = (iptr)regs[INSTR_drr_reg1(instr)];
        iptr b = (iptr)regs[INSTR_drr_reg2(instr)];
        iptr r;
        flag = __builtin_mul_overflow(a, b, &r);
        regs[INSTR_drr_dest(instr)] = (uptr)r;
#else
        uptr a = regs[INSTR_drr_reg1(instr)];
        uptr b = regs[INSTR_drr_reg2(instr)];
        uptr r = a * b;
        regs[INSTR_drr_dest(instr)] = r;
        if (b != 0) {
          if (b == (uptr)-1)
            flag = (a != r * (uptr)-1);
          else
            flag = ((iptr)a != (iptr)r / (iptr)b);
        } else
          flag = 0;
#endif
      }
      break;
    case pb_bin_op_pb_signal_pb_mul_pb_immediate:
      {
#if USE_OVERFLOW_INTRINSICS
        iptr a = (iptr)regs[INSTR_dri_reg(instr)];
        iptr b = INSTR_dri_imm(instr);
        iptr r;
        flag = __builtin_mul_overflow(a, b, &r);
        regs[INSTR_drr_dest(instr)] = (uptr)r;
#else
        uptr a = regs[INSTR_dri_reg(instr)];
        uptr b = (uptr)INSTR_dri_imm(instr);
        uptr r = a * b;
        regs[INSTR_dri_dest(instr)] = r;
        if (b != 0) {
          if (b == (uptr)-1)
            flag = (a != r * (uptr)-1);
          else
            flag = ((iptr)a != (iptr)r / (iptr)b);
        } else
          flag = 0;
#endif
      }
      break;
    case pb_bin_op_pb_signal_pb_subz_pb_register:
      {
        iptr r = regs[INSTR_drr_reg1(instr)] - regs[INSTR_drr_reg2(instr)];
        regs[INSTR_drr_dest(instr)] = r;
        flag = (r == 0);
      }
      break;
    case pb_bin_op_pb_signal_pb_subz_pb_immediate:
      {
        iptr r = regs[INSTR_dri_reg(instr)] - (uptr)INSTR_dri_imm(instr);
        regs[INSTR_dri_dest(instr)] = r;
        flag = (r == 0);
      }
      break;
    case pb_bin_op_pb_signal_pb_subp_pb_register:
      {
        iptr r = regs[INSTR_drr_reg1(instr)] - regs[INSTR_drr_reg2(instr)];
        regs[INSTR_drr_dest(instr)] = r;
        flag = (r > 0);
      }
      break;
    case pb_bin_op_pb_signal_pb_subp_pb_immediate:
      {
        iptr r = regs[INSTR_dri_reg(instr)] - (uptr)INSTR_dri_imm(instr);
        regs[INSTR_dri_dest(instr)] = r;
        flag = (r > 0);
      }
      break;
    case pb_cmp_op_pb_eq_pb_register:
      flag = regs[INSTR_dr_dest(instr)] == regs[INSTR_dr_reg(instr)];
      break;
    case pb_cmp_op_pb_eq_pb_immediate:
      flag = regs[INSTR_di_dest(instr)] == (uptr)INSTR_di_imm(instr);
      break;
    case pb_cmp_op_pb_lt_pb_register:
      flag = (iptr)regs[INSTR_dr_dest(instr)] < (iptr)regs[INSTR_dr_reg(instr)];
      break;
    case pb_cmp_op_pb_lt_pb_immediate:
      flag = (iptr)regs[INSTR_di_dest(instr)] < (iptr)INSTR_di_imm(instr);
      break;
    case pb_cmp_op_pb_gt_pb_register:
      flag = (iptr)regs[INSTR_dr_dest(instr)] > (iptr)regs[INSTR_dr_reg(instr)];
      break;
    case pb_cmp_op_pb_gt_pb_immediate:
      flag = (iptr)regs[INSTR_di_dest(instr)] > (iptr)INSTR_di_imm(instr);
      break;
    case pb_cmp_op_pb_le_pb_register:
      flag = (iptr)regs[INSTR_dr_dest(instr)] <= (iptr)regs[INSTR_dr_reg(instr)];
      break;
    case pb_cmp_op_pb_le_pb_immediate:
      flag = (iptr)regs[INSTR_di_dest(instr)] <= (iptr)INSTR_di_imm(instr);
      break;
    case pb_cmp_op_pb_ge_pb_register:
      flag = (iptr)regs[INSTR_dr_dest(instr)] >= (iptr)regs[INSTR_dr_reg(instr)];
      break;
    case pb_cmp_op_pb_ge_pb_immediate:
      flag = (iptr)regs[INSTR_di_dest(instr)] >= (iptr)INSTR_di_imm(instr);
      break;
    case pb_cmp_op_pb_ab_pb_register:
      flag = regs[INSTR_dr_dest(instr)] > regs[INSTR_dr_reg(instr)];
      break;
    case pb_cmp_op_pb_ab_pb_immediate:
      flag = regs[INSTR_di_dest(instr)] > (uptr)INSTR_di_imm(instr);
      break;
    case pb_cmp_op_pb_bl_pb_register:
      flag = regs[INSTR_dr_dest(instr)] < regs[INSTR_dr_reg(instr)];
      break;
    case pb_cmp_op_pb_bl_pb_immediate:
      flag = regs[INSTR_di_dest(instr)] < (uptr)INSTR_di_imm(instr);
      break;
    case pb_cmp_op_pb_cs_pb_register:
      flag = ((regs[INSTR_dr_dest(instr)] & regs[INSTR_dr_reg(instr)]) != 0);
      break;
    case pb_cmp_op_pb_cs_pb_immediate:
      flag = ((regs[INSTR_di_dest(instr)] & (uptr)INSTR_di_imm(instr)) != 0);
      break;
    case pb_cmp_op_pb_cc_pb_register:
      flag = ((regs[INSTR_dr_dest(instr)] & regs[INSTR_dr_reg(instr)]) == 0);
      break;
    case pb_cmp_op_pb_cc_pb_immediate:
      flag = ((regs[INSTR_di_dest(instr)] & (uptr)INSTR_di_imm(instr)) == 0);
      break;
    case pb_fp_bin_op_pb_add_pb_register:
      fpregs[INSTR_drr_dest(instr)] = fpregs[INSTR_drr_reg1(instr)] + fpregs[INSTR_drr_reg2(instr)];
      break;
    case pb_fp_bin_op_pb_sub_pb_register:
      fpregs[INSTR_drr_dest(instr)] = fpregs[INSTR_drr_reg1(instr)] - fpregs[INSTR_drr_reg2(instr)];
      break;
    case pb_fp_bin_op_pb_mul_pb_register:
      fpregs[INSTR_drr_dest(instr)] = fpregs[INSTR_drr_reg1(instr)] * fpregs[INSTR_drr_reg2(instr)];
      break;
    case pb_fp_bin_op_pb_div_pb_register:
      fpregs[INSTR_drr_dest(instr)] = fpregs[INSTR_drr_reg1(instr)] / fpregs[INSTR_drr_reg2(instr)];
      break;
    case pb_un_op_pb_not_pb_register:
      regs[INSTR_dr_dest(instr)] = ~(regs[INSTR_dr_reg(instr)]);
      break;
    case pb_un_op_pb_not_pb_immediate:
      regs[INSTR_di_dest(instr)] = ~((uptr)(iptr)INSTR_di_imm(instr));
      break;
    case pb_fp_un_op_pb_sqrt_pb_register:
      fpregs[INSTR_dr_dest(instr)] = sqrt(fpregs[INSTR_dr_reg(instr)]);
      break;
    case pb_fp_cmp_op_pb_eq_pb_register:
      flag = fpregs[INSTR_dr_dest(instr)] == fpregs[INSTR_dr_reg(instr)];
      break;
    case pb_fp_cmp_op_pb_lt_pb_register:
      flag = fpregs[INSTR_dr_dest(instr)] < fpregs[INSTR_dr_reg(instr)];
      break;
    case pb_fp_cmp_op_pb_le_pb_register:
      flag = fpregs[INSTR_dr_dest(instr)] <= fpregs[INSTR_dr_reg(instr)];
      break;
    case pb_rev_op_pb_int16_pb_register:
#if ptr_bits == 64
      regs[INSTR_dr_dest(instr)] = ((uptr)((iptr)(regs[INSTR_dr_reg(instr)] << 56) >> 48)
                                    | ((regs[INSTR_dr_reg(instr)] & 0xFF00) >> 8));
#else
      regs[INSTR_dr_dest(instr)] = ((uptr)((iptr)(regs[INSTR_dr_reg(instr)] << 24) >> 16)
                                    | ((regs[INSTR_dr_reg(instr)] & 0xFF00) >> 8));
#endif
      break;
    case pb_rev_op_pb_uint16_pb_register:
      regs[INSTR_dr_dest(instr)] = (((regs[INSTR_dr_reg(instr)] & 0x00FF) << 8)
                                    | ((regs[INSTR_dr_reg(instr)] & 0xFF00) >> 8));
      break;
    case pb_rev_op_pb_int32_pb_register:
#if ptr_bits == 64
      regs[INSTR_dr_dest(instr)] = ((uptr)((iptr)(regs[INSTR_dr_reg(instr)] << 56) >> 32)
                                    | ((regs[INSTR_dr_reg(instr)] & (uptr)0xFF000000) >> 24)
                                    | ((regs[INSTR_dr_reg(instr)] & (uptr)0x00FF0000) >> 8)
                                    | ((regs[INSTR_dr_reg(instr)] & (uptr)0x0000FF00) << 8));
#else
      regs[INSTR_dr_dest(instr)] = ((regs[INSTR_dr_reg(instr)] << 24)
                                    | ((regs[INSTR_dr_reg(instr)] & (uptr)0xFF000000) >> 24)
                                    | ((regs[INSTR_dr_reg(instr)] & (uptr)0x00FF0000) >> 8)
                                    | ((regs[INSTR_dr_reg(instr)] & (uptr)0x0000FF00) << 8));
#endif
      break;
    case pb_rev_op_pb_uint32_pb_register:
      regs[INSTR_dr_dest(instr)] = (((regs[INSTR_dr_reg(instr)] & (uptr)0x000000FF) << 24)
                                    | ((regs[INSTR_dr_reg(instr)] & (uptr)0xFF000000) >> 24)
                                    | ((regs[INSTR_dr_reg(instr)] & (uptr)0x00FF0000) >> 8)
                                    | ((regs[INSTR_dr_reg(instr)] & (uptr)0x0000FF00) << 8));
      break;
    case pb_rev_op_pb_int64_pb_register:
#if ptr_bits == 64
      regs[INSTR_dr_dest(instr)] = (((regs[INSTR_dr_reg(instr)] & (uptr)0x00000000000000FF) << 56)
                                    | ((regs[INSTR_dr_reg(instr)] & (uptr)0x000000000000FF00) << 40)
                                    | ((regs[INSTR_dr_reg(instr)] & (uptr)0x0000000000FF0000) << 24)
                                    | ((regs[INSTR_dr_reg(instr)] & (uptr)0x00000000FF000000) << 8)
                                    | ((regs[INSTR_dr_reg(instr)] & (uptr)0x000000FF00000000) >> 8)
                                    | ((regs[INSTR_dr_reg(instr)] & (uptr)0x0000FF0000000000) >> 24)
                                    | ((regs[INSTR_dr_reg(instr)] & (uptr)0x00FF000000000000) >> 40)
                                    | ((regs[INSTR_dr_reg(instr)] & (uptr)0xFF00000000000000) >> 56));
#else
      regs[INSTR_dr_dest(instr)] = (((regs[INSTR_dr_reg(instr)] & (uptr)0x000000FF) << 24)
                                    | ((regs[INSTR_dr_reg(instr)] & (uptr)0xFF000000) >> 24)
                                    | ((regs[INSTR_dr_reg(instr)] & (uptr)0x00FF0000) >> 8)
                                    | ((regs[INSTR_dr_reg(instr)] & (uptr)0x0000FF00) << 8));
#endif
      break;
    case pb_ld_op_pb_int8_pb_register:
      regs[INSTR_drr_dest(instr)] = *(int8_t *)TO_VOIDP(regs[INSTR_drr_reg1(instr)] + regs[INSTR_drr_reg2(instr)]);
      break;
    case pb_ld_op_pb_int8_pb_immediate:
#if defined(__arm__)
      /* Complicated load to avoid an internal compiler error from an old gcc on Raspbian: */
      {
        int8_t v;
        memcpy(&v, TO_VOIDP(regs[INSTR_dri_reg(instr)] + INSTR_dri_imm(instr)), sizeof(int8_t));
        regs[INSTR_dri_dest(instr)] = v;
      }
#else
      regs[INSTR_dri_dest(instr)] = *(int8_t *)TO_VOIDP(regs[INSTR_dri_reg(instr)] + INSTR_dri_imm(instr));
#endif
      break;
    case pb_ld_op_pb_uint8_pb_register:
      regs[INSTR_drr_dest(instr)] = *(uint8_t *)TO_VOIDP(regs[INSTR_drr_reg1(instr)] + regs[INSTR_drr_reg2(instr)]);
      break;
    case pb_ld_op_pb_uint8_pb_immediate:
      regs[INSTR_dri_dest(instr)] = *(uint8_t *)TO_VOIDP(regs[INSTR_dri_reg(instr)] + INSTR_dri_imm(instr));
      break;
    case pb_ld_op_pb_int16_pb_register:
      regs[INSTR_drr_dest(instr)] = *(int16_t *)TO_VOIDP(regs[INSTR_drr_reg1(instr)] + regs[INSTR_drr_reg2(instr)]);
      break;
    case pb_ld_op_pb_int16_pb_immediate:
      regs[INSTR_dri_dest(instr)] = *(int16_t *)TO_VOIDP(regs[INSTR_dri_reg(instr)] + INSTR_dri_imm(instr));
      break;
    case pb_ld_op_pb_uint16_pb_register:
      regs[INSTR_drr_dest(instr)] = *(uint16_t *)TO_VOIDP(regs[INSTR_drr_reg1(instr)] + regs[INSTR_drr_reg2(instr)]);
      break;
    case pb_ld_op_pb_uint16_pb_immediate:
      regs[INSTR_dri_dest(instr)] = *(uint16_t *)TO_VOIDP(regs[INSTR_dri_reg(instr)] + INSTR_dri_imm(instr));
      break;
    case pb_ld_op_pb_int32_pb_register:
      regs[INSTR_drr_dest(instr)] = *(int32_t *)TO_VOIDP(regs[INSTR_drr_reg1(instr)] + regs[INSTR_drr_reg2(instr)]);
      break;
    case pb_ld_op_pb_int32_pb_immediate:
      regs[INSTR_dri_dest(instr)] = *(int32_t *)TO_VOIDP(regs[INSTR_dri_reg(instr)] + INSTR_dri_imm(instr));
      break;
    case pb_ld_op_pb_uint32_pb_register:
      regs[INSTR_drr_dest(instr)] = *(uint32_t *)TO_VOIDP(regs[INSTR_drr_reg1(instr)] + regs[INSTR_drr_reg2(instr)]);
      break;
    case pb_ld_op_pb_uint32_pb_immediate:
      regs[INSTR_dri_dest(instr)] = *(uint32_t *)TO_VOIDP(regs[INSTR_dri_reg(instr)] + INSTR_dri_imm(instr));
      break;
    case pb_ld_op_pb_int64_pb_register:
      regs[INSTR_drr_dest(instr)] = *(uptr *)TO_VOIDP(regs[INSTR_drr_reg1(instr)] + regs[INSTR_drr_reg2(instr)]);
      break;
    case pb_ld_op_pb_int64_pb_immediate:
      regs[INSTR_dri_dest(instr)] = *(uptr *)TO_VOIDP(regs[INSTR_dri_reg(instr)] + INSTR_dri_imm(instr));
      break;
    case pb_ld_op_pb_double_pb_register:
      fpregs[INSTR_drr_dest(instr)] = *(double *)TO_VOIDP(regs[INSTR_drr_reg1(instr)] + regs[INSTR_drr_reg2(instr)]);
      break;
    case pb_ld_op_pb_double_pb_immediate:
      fpregs[INSTR_dri_dest(instr)] = *(double *)TO_VOIDP(regs[INSTR_dri_reg(instr)] + INSTR_dri_imm(instr));
      break;
    case pb_ld_op_pb_single_pb_register:
      fpregs[INSTR_drr_dest(instr)] =  *(float *)TO_VOIDP(regs[INSTR_drr_reg1(instr)] + regs[INSTR_drr_reg2(instr)]);
      break;
    case pb_ld_op_pb_single_pb_immediate:
      fpregs[INSTR_dri_dest(instr)] = *(float *)TO_VOIDP(regs[INSTR_dri_reg(instr)] + INSTR_dri_imm(instr));
      break;
    case pb_st_op_pb_int8_pb_register:
      *(char *)TO_VOIDP(regs[INSTR_drr_reg1(instr)] + regs[INSTR_drr_reg2(instr)]) = (char)regs[INSTR_drr_dest(instr)];
      break;
    case pb_st_op_pb_int8_pb_immediate:
      *(char *)TO_VOIDP(regs[INSTR_dri_reg(instr)] + INSTR_dri_imm(instr)) = (char)regs[INSTR_dri_dest(instr)];
      break;
    case pb_st_op_pb_int16_pb_register:
      *(short *)TO_VOIDP(regs[INSTR_drr_reg1(instr)] + regs[INSTR_drr_reg2(instr)]) = (short)regs[INSTR_drr_dest(instr)];
      break;
    case pb_st_op_pb_int16_pb_immediate:
      *(short *)TO_VOIDP(regs[INSTR_dri_reg(instr)] + INSTR_dri_imm(instr)) = (short)regs[INSTR_dri_dest(instr)];
      break;
    case pb_st_op_pb_int32_pb_register:
      *(int *)TO_VOIDP(regs[INSTR_drr_reg1(instr)] + regs[INSTR_drr_reg2(instr)]) = (int)regs[INSTR_drr_dest(instr)];
      break;
    case pb_st_op_pb_int32_pb_immediate:
      *(int *)TO_VOIDP(regs[INSTR_dri_reg(instr)] + INSTR_dri_imm(instr)) = (int)regs[INSTR_dri_dest(instr)];
      break;
    case pb_st_op_pb_int64_pb_register:
      *(uptr *)TO_VOIDP(regs[INSTR_drr_reg1(instr)] + regs[INSTR_drr_reg2(instr)]) = regs[INSTR_drr_dest(instr)];
      break;
    case pb_st_op_pb_int64_pb_immediate:
      *(uptr *)TO_VOIDP(regs[INSTR_dri_reg(instr)] + INSTR_dri_imm(instr)) = regs[INSTR_dri_dest(instr)];
      break;
    case pb_st_op_pb_double_pb_register:
      *(double *)TO_VOIDP(regs[INSTR_drr_reg1(instr)] + regs[INSTR_drr_reg2(instr)]) = fpregs[INSTR_drr_dest(instr)];
      break;
    case pb_st_op_pb_double_pb_immediate:
      *(double *)TO_VOIDP(regs[INSTR_dri_reg(instr)] + INSTR_dri_imm(instr)) = fpregs[INSTR_dri_dest(instr)];
      break;
    case pb_st_op_pb_single_pb_register:
      *(float *)TO_VOIDP(regs[INSTR_drr_reg1(instr)] + regs[INSTR_drr_reg2(instr)]) = fpregs[INSTR_drr_dest(instr)];
      break;
    case pb_st_op_pb_single_pb_immediate:
      *(float *)TO_VOIDP(regs[INSTR_dri_reg(instr)] + INSTR_dri_imm(instr)) = fpregs[INSTR_dri_dest(instr)];
      break;
    case pb_b_op_pb_fals_pb_register:
      if (!flag) {
        next_ip = (instruction_t *)TO_VOIDP(regs[INSTR_dr_reg(instr)]);
        TRACE(printf("branch %p -> %p\n", ip, next_ip), { branch_from = ip; branch_to = next_ip; });
      }
      break;
    case pb_b_op_pb_fals_pb_immediate:
      if (!flag) {
        next_ip = (instruction_t *)TO_VOIDP((char *)next_ip + INSTR_i_imm(instr));
        TRACE(printf("branch %p -> %p\n", ip, next_ip), { branch_from = ip; branch_to = next_ip; });
      }
      break;
    case pb_b_op_pb_true_pb_register:
      if (flag) {
        next_ip = (instruction_t *)TO_VOIDP(regs[INSTR_dr_reg(instr)]);
        TRACE(printf("branch %p -> %p\n", ip, next_ip), { branch_from = ip; branch_to = next_ip; });
      }
      break;
    case pb_b_op_pb_true_pb_immediate:
      if (flag) {
        next_ip = (instruction_t *)TO_VOIDP((char *)next_ip + INSTR_i_imm(instr));
        TRACE(printf("branch %p -> %p\n", ip, next_ip), { branch_from = ip; branch_to = next_ip; });
      }
      break;
    case pb_b_op_pb_always_pb_register:
      next_ip = (instruction_t *)TO_VOIDP(regs[INSTR_dr_reg(instr)]);
      TRACE(printf("jump %p -> %p\n", ip, next_ip), { jump_from = ip; jump_to = next_ip; });
      break;
    case pb_b_op_pb_always_pb_immediate:
      next_ip = (instruction_t *)TO_VOIDP((char *)next_ip + INSTR_i_imm(instr));
      TRACE(printf("jump %p -> %p\n", ip, next_ip), { jump_from = ip; jump_to = next_ip; });
      break;
    case pb_bs_op_pb_register:
      next_ip = (instruction_t *)TO_VOIDP(*(uptr *)TO_VOIDP(regs[INSTR_dr_dest(instr)] + regs[INSTR_dr_reg(instr)]));
      TRACE(printf("jump %p -> %p\n", ip, next_ip), { jump_from = ip; jump_to = next_ip; });
      break;
    case pb_bs_op_pb_immediate:
      next_ip = (instruction_t *)TO_VOIDP(*(uptr *)TO_VOIDP(regs[INSTR_di_dest(instr)] + INSTR_di_imm(instr)));
      TRACE(printf("jump %p -> %p\n", ip, next_ip), { jump_from = ip; jump_to = next_ip; });
      break;
    case pb_return:
      return; /* <--- not break */
    case pb_adr:
      regs[INSTR_adr_dest(instr)] = (uptr)TO_PTR(next_ip) + INSTR_adr_imm(instr);
      break;
    case pb_interp:
      {
        void *code = TO_VOIDP(regs[INSTR_d_dest(instr)]);
        TRACE(printf("interp %p -> %p\n", ip, code), { interp_from = ip; interp_to = (instruction_t *)regs[0]; });
        S_pb_interp((ptr)regs[0], code);
      }
      break;
    case pb_call:
      {
        void *proc = TO_VOIDP(regs[INSTR_dri_dest(instr)]);
        TRACE(printf("call %p -> %p %x\n", ip, proc, INSTR_dri_imm(instr)), { call_from = ip; call_to = proc; });
        switch (INSTR_dri_imm(instr)) {
        case pb_call_void:
          ((pb_void_t)proc)();
          break;
        case pb_call_void_uptr:
          ((pb_void_uptr_t)proc)(regs[Carg1]);
          break;
        case pb_call_void_int32:
          ((pb_void_int32_t)proc)(regs[Carg1]);
          break;
        case pb_call_void_uint32:
          ((pb_void_uint32_t)proc)(regs[Carg1]);
          break;
        case pb_call_void_voids:
          ((pb_void_voids_t)proc)(TO_VOIDP(regs[Carg1]));
          break;
        case pb_call_void_uptr_uint32:
          ((pb_void_uptr_uint32_t)proc)(regs[Carg1], regs[Carg2]);
          break;
        case pb_call_void_int32_uptr:
          ((pb_void_int32_uptr_t)proc)(regs[Carg1], regs[Carg2]);
          break;
        case pb_call_void_int32_voids:
          ((pb_void_int32_voids_t)proc)(regs[Carg1], TO_VOIDP(regs[Carg2]));
          break;
        case pb_call_void_uptr_voids:
          ((pb_void_uptr_voids_t)proc)(regs[Carg1], TO_VOIDP(regs[Carg2]));
          break;
        case pb_call_void_int32_int32:
          ((pb_void_int32_int32_t)proc)(regs[Carg1], regs[Carg2]);
          break;
        case pb_call_void_uptr_uptr:
          ((pb_void_uptr_uptr_t)proc)(regs[Carg1], regs[Carg2]);
          break;
        case pb_call_void_voids_voids:
          ((pb_void_voids_voids_t)proc)(TO_VOIDP(regs[Carg1]), TO_VOIDP(regs[Carg2]));
          break;
        case pb_call_void_uptr_uptr_uptr:
          ((pb_void_uptr_uptr_uptr_t)proc)(regs[Carg1], regs[Carg2], regs[Carg3]);
          break;
        case pb_call_void_uptr_uptr_uptr_uptr_uptr:
          ((pb_void_uptr_uptr_uptr_uptr_uptr_t)proc)(regs[Carg1], regs[Carg2], regs[Carg3],
                                                     regs[Carg4], regs[Carg5]);
          break;
        case pb_call_int32:
          regs[Cretval] = ((pb_int32_t)proc)();
          break;
        case pb_call_int32_uptr:
          regs[Cretval] = ((pb_int32_uptr_t)proc)(regs[Carg1]);
          break;
        case pb_call_int32_voids:
          regs[Cretval] = ((pb_int32_voids_t)proc)(TO_VOIDP(regs[Carg1]));
          break;
        case pb_call_int32_uptr_int32:
          regs[Cretval] = ((pb_int32_uptr_int32_t)proc)(regs[Carg1], regs[Carg2]);
          break;
        case pb_call_int32_uptr_uptr:
          regs[Cretval] = ((pb_int32_uptr_uptr_t)proc)(regs[Carg1], regs[Carg2]);
          break;
        case pb_call_int32_int32_int32:
          regs[Cretval] = ((pb_int32_int32_int32_t)proc)(regs[Carg1], regs[Carg2]);
          break;
        case pb_call_int32_voids_int32:
          regs[Cretval] = ((pb_int32_voids_int32_t)proc)(TO_VOIDP(regs[Carg1]), regs[Carg2]);
          break;
        case pb_call_int32_int32_voids:
          regs[Cretval] = ((pb_int32_int32_voids_t)proc)(regs[Carg1], TO_VOIDP(regs[Carg2]));
          break;
        case pb_call_int32_double_double_double_double_double_double:
          regs[Cretval] = ((pb_int32_double_double_double_double_double_double_t)proc)(fpregs[Cfparg1], fpregs[Cfparg2], fpregs[Cfparg3],
                                                                                       fpregs[Cfparg4], fpregs[Cfparg5], fpregs[Cfparg6]);
          break;
        case pb_call_uint32:
          regs[Cretval] = ((pb_uint32_t)proc)();
          break;
        case pb_call_double_double:
          fpregs[Cfpretval] = ((pb_double_double_t)proc)(fpregs[Cfparg1]);
          break;
        case pb_call_double_uptr:
          fpregs[Cfpretval] = ((pb_double_uptr_t)proc)(regs[Carg1]);
          break;
        case pb_call_double_double_double:
          fpregs[Cfpretval] = ((pb_double_double_double_t)proc)(fpregs[Cfparg1], fpregs[Cfparg2]);
          break;
        case pb_call_int32_int32:
          regs[Cretval] = ((pb_int32_int32_t)proc)(regs[Carg1]);
          break;
        case pb_call_int32_int32_uptr:
          regs[Cretval] = ((pb_int32_int32_uptr_t)proc)(regs[Carg1], regs[Carg2]);
          break;
        case pb_call_int32_voids_voids_voids_voids_uptr:
          regs[Cretval] = ((pb_int32_voids_voids_voids_voids_uptr_t)proc)(TO_VOIDP(regs[Carg1]), TO_VOIDP(regs[Carg2]), TO_VOIDP(regs[Carg3]),
                                                                          TO_VOIDP(regs[Carg4]), regs[Carg5]);
          break;
        case pb_call_uptr:
          regs[Cretval] = ((pb_uptr_t)proc)();
          break;
        case pb_call_uptr_uptr:
          regs[Cretval] = ((pb_uptr_uptr_t)proc)(regs[Carg1]);
          break;
        case pb_call_uptr_int32:
          regs[Cretval] = ((pb_uptr_int32_t)proc)(regs[Carg1]);
          break;
        case pb_call_uptr_voids:
          regs[Cretval] = ((pb_uptr_voids_t)proc)(TO_VOIDP(regs[Carg1]));
          break;
        case pb_call_uptr_uptr_uptr:
          regs[Cretval] = ((pb_uptr_uptr_uptr_t)proc)(regs[Carg1], regs[Carg2]);
          break;
        case pb_call_uptr_uptr_int32:
          regs[Cretval] = ((pb_uptr_uptr_int32_t)proc)(regs[Carg1], regs[Carg2]);
          break;
        case pb_call_uptr_uptr_int64:
#if ptr_bits == 64
          regs[Cretval] = ((pb_uptr_uptr_int64_t)proc)(regs[Carg1], regs[Carg2]);
#else
          regs[Cretval] = ((pb_uptr_uptr_int64_t)proc)(regs[Carg1], regs[Carg2] | ((int64_t)regs[Carg3] << 32));
#endif
          break;
        case pb_call_uptr_int32_uptr:
          regs[Cretval] = ((pb_uptr_int32_uptr_t)proc)(regs[Carg1], regs[Carg2]);
          break;
        case pb_call_uptr_voids_uptr:
          regs[Cretval] = ((pb_uptr_voids_uptr_t)proc)(TO_VOIDP(regs[Carg1]), regs[Carg2]);
          break;
        case pb_call_uptr_uptr_voids:
          regs[Cretval] = ((pb_uptr_uptr_voids_t)proc)(regs[Carg1], TO_VOIDP(regs[Carg2]));
          break;
        case pb_call_uptr_voids_int32:
          regs[Cretval] = ((pb_uptr_voids_int32_t)proc)(TO_VOIDP(regs[Carg1]), regs[Carg2]);
          break;
        case pb_call_uptr_voids_voids:
          regs[Cretval] = ((pb_uptr_voids_voids_t)proc)(TO_VOIDP(regs[Carg1]), TO_VOIDP(regs[Carg2]));
          break;
        case pb_call_uptr_uptr_int32_int32:
          regs[Cretval] = ((pb_uptr_uptr_int32_int32_t)proc)(regs[Carg1], regs[Carg2], regs[Carg3]);
          break;
        case pb_call_uptr_voids_int32_int32:
          regs[Cretval] = ((pb_uptr_voids_int32_int32_t)proc)(TO_VOIDP(regs[Carg1]), regs[Carg2], regs[Carg3]);
          break;
        case pb_call_uptr_voids_uptr_uptr:
          regs[Cretval] = ((pb_uptr_voids_uptr_uptr_t)proc)(TO_VOIDP(regs[Carg1]), regs[Carg2], regs[Carg3]);
          break;
        case pb_call_uptr_uptr_uptr_int32:
          regs[Cretval] = ((pb_uptr_uptr_uptr_int32_t)proc)(regs[Carg1], regs[Carg2], regs[Carg3]);
          break;
        case pb_call_uptr_uptr_uptr_uptr:
          regs[Cretval] = ((pb_uptr_uptr_uptr_uptr_t)proc)(regs[Carg1], regs[Carg2], regs[Carg3]);
          break;
        case pb_call_uptr_int32_int32_uptr:
          regs[Cretval] = ((pb_uptr_int32_int32_uptr_t)proc)(regs[Carg1], regs[Carg2], regs[Carg3]);
          break;
        case pb_call_uptr_int32_uptr_uptr_uptr:
          regs[Cretval] = ((pb_uptr_int32_uptr_uptr_uptr_t)proc)(regs[Carg1], regs[Carg2], regs[Carg3],
                                                                 regs[Carg4]);
          break;
        case pb_call_uptr_uptr_uptr_uptr_uptr:
          regs[Cretval] = ((pb_uptr_uptr_uptr_uptr_uptr_t)proc)(regs[Carg1], regs[Carg2], regs[Carg3],
                                                                regs[Carg4]);
          break;
        case pb_call_uptr_int32_int32_uptr_uptr:
          regs[Cretval] = ((pb_uptr_int32_int32_uptr_uptr_t)proc)(regs[Carg1], regs[Carg2], regs[Carg3],
                                                                  regs[Carg4]);
          break;
        case pb_call_uptr_int32_int32_int32_uptr:
          regs[Cretval] = ((pb_uptr_int32_int32_int32_uptr_t)proc)(regs[Carg1], regs[Carg2], regs[Carg3],
                                                                   regs[Carg4]);
          break;
        case pb_call_uptr_int32_voids_uptr_uptr:
          regs[Cretval] = ((pb_uptr_int32_voids_uptr_uptr_t)proc)(regs[Carg1], TO_VOIDP(regs[Carg2]), regs[Carg3],
                                                                  regs[Carg4]);
          break;
        case pb_call_uptr_uptr_uptr_uptr_uptr_int32:
          regs[Cretval] = ((pb_uptr_uptr_uptr_uptr_uptr_int32_t)proc)(regs[Carg1], regs[Carg2], regs[Carg3],
                                                                      regs[Carg4], regs[Carg5]);
          break;
        case pb_call_uptr_uptr_uptr_uptr_uptr_uptr:
          regs[Cretval] = ((pb_uptr_uptr_uptr_uptr_uptr_uptr_t)proc)(regs[Carg1], regs[Carg2], regs[Carg3],
                                                                     regs[Carg4], regs[Carg5]);
          break;
        case pb_call_uptr_voids_voids_voids_voids_uptr:
          regs[Cretval] = ((pb_uptr_voids_voids_voids_voids_uptr_t)proc)(TO_VOIDP(regs[Carg1]), TO_VOIDP(regs[Carg2]), TO_VOIDP(regs[Carg3]),
                                                                         TO_VOIDP(regs[Carg4]), regs[Carg5]);
          break;
        case pb_call_uptr_uptr_int32_uptr_uptr_uptr_uptr:
          regs[Cretval] = ((pb_uptr_uptr_int32_uptr_uptr_uptr_uptr_t)proc)(regs[Carg1], regs[Carg2], regs[Carg3],
                                                                           regs[Carg4], regs[Carg5], regs[Carg6]);
          break;
        case pb_call_uptr_uptr_uptr_uptr_uptr_uptr_uptr:
          regs[Cretval] = ((pb_uptr_uptr_uptr_uptr_uptr_uptr_uptr_t)proc)(regs[Carg1], regs[Carg2], regs[Carg3],
                                                                          regs[Carg4], regs[Carg5], regs[Carg6]);
          break;
        case pb_call_uptr_uptr_uptr_uptr_uptr_uptr_uptr_int32:
          regs[Cretval] = ((pb_uptr_uptr_uptr_uptr_uptr_uptr_uptr_int32_t)proc)(regs[Carg1], regs[Carg2], regs[Carg3],
                                                                                regs[Carg4], regs[Carg5], regs[Carg6],
                                                                                regs[Carg7]);
          break;
        case pb_call_uptr_uptr_uptr_uptr_uptr_uptr_uptr_uptr:
          regs[Cretval] = ((pb_uptr_uptr_uptr_uptr_uptr_uptr_uptr_uptr_t)proc)(regs[Carg1], regs[Carg2], regs[Carg3],
                                                                               regs[Carg4], regs[Carg5], regs[Carg6],
                                                                               regs[Carg7]);
          break;
        case pb_call_uptr_double_double_double_double_double_double:
          regs[Cretval] = ((pb_uptr_double_double_double_double_double_double_t)proc)(fpregs[Cfparg1], fpregs[Cfparg2], fpregs[Cfparg3],
                                                                                      fpregs[Cfparg4], fpregs[Cfparg5], fpregs[Cfparg6]);
          break;
        case pb_call_voids:
          regs[Cretval] = TO_PTR(((pb_voids_t)proc)());
          break;
        case pb_call_voids_uptr:
          regs[Cretval] = TO_PTR(((pb_voids_uptr_t)proc)(regs[Carg1]));
          break;
        default:
          S_error_abort("unsupported call prototype");
          break;
        }
      }
      break;
    case pb_inc_pb_register:
      {
        uptr r = *(uptr *)TO_VOIDP(regs[INSTR_dr_dest(instr)]) + regs[INSTR_dr_reg(instr)];
        *(uptr *)TO_VOIDP(regs[INSTR_dr_dest(instr)]) = r;
        flag = (r == 0);
      }
      break;
    case pb_inc_pb_immediate:
      {
        uptr r = *(uptr *)TO_VOIDP(regs[INSTR_di_dest(instr)]) + INSTR_di_imm(instr);
        *(uptr *)TO_VOIDP(regs[INSTR_di_dest(instr)]) = r;
        flag = (r == 0);
      }
      break;
    case pb_lock:
      {
        uptr *l = TO_VOIDP(regs[INSTR_d_dest(instr)]);
        if (*l == 0) {
          *l = 1;
          flag = 1;
        } else
          flag = 0;
      }
      break;
    case pb_cas:
      {
        uptr *l = TO_VOIDP(regs[INSTR_drr_dest(instr)]);
        uptr old = regs[INSTR_drr_reg1(instr)];
        uptr new = regs[INSTR_drr_reg2(instr)];
        if (*l == old) {
          *l = new;
          flag = 1;
        } else
          flag = 0;
      }
      break;
    default:
      S_error_abort("illegal pb instruction");
      break;
    }
    ip = next_ip;
  }
}
