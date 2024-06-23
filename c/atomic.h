/* `STORE_FENCE` is used by the storage-management system, but
   `ACQUIRE_FENCE`, `RELEASE_FENCE`, and `COMPARE_AND_SWAP_PTR`
   are used only by the pb interpreter.

   It's always ok to map `ACQUIRE_FENCE` and `RELEASE_FENCE` to
   `STORE_FENCE`. For portability, we mainly rely on a
   `__sync_synchronize` intrinsic as provided by reasonably modern
   versions of GCC and Clang. In some cases, more specialized fence
   variants are available via inline assembly or platform-specific
   intrinsics. When inline assembly is written only for `STORE_FENCE`
   below, then the only advantage over using `__sync_synchronize` is
   to support environments with different or very old compilers.

   For `COMPARE_AND_SWAP_PTR`, we similarlly rely on a GCC/Clang
   `__sync_bool_compare_and_swap` intrinsic. Some inline-assembly
   versions are here --- but, again, the only advantage of those is to
   support environments with different or very old compilers. */

#if !defined(PTHREADS)
# define STORE_FENCE() do { } while (0)
#elif defined(_MSC_VER) && defined(_M_ARM64)
# define STORE_FENCE()   __dmb(_ARM64_BARRIER_ISHST)
# define ACQUIRE_FENCE() __dmb(_ARM64_BARRIER_ISH)
# define RELEASE_FENCE() ACQUIRE_FENCE()
#elif defined(__arm64__) || defined(__aarch64__)
# define STORE_FENCE()   __asm__ __volatile__ ("dmb ishst" : : : "memory")
# define ACQUIRE_FENCE() __asm__ __volatile__ ("dmb ish" : : : "memory")
# define RELEASE_FENCE() ACQUIRE_FENCE()
#elif defined(__arm__)
# if (arm_isa_version >= 7) || (__ARM_ARCH >= 7)
#  define STORE_FENCE()   __asm__ __volatile__ ("dmb ishst" : : : "memory")
#  define ACQUIRE_FENCE() __asm__ __volatile__ ("dmb ish" : : : "memory")
#  define RELEASE_FENCE() ACQUIRE_FENCE()
# else
#  define STORE_FENCE()   __asm__ __volatile__ ("mcr p15, 0, %0, c7, c10, 5" : : "r" (0) : "memory")
#  define ACQUIRE_FENCE() STORE_FENCE()
#  define RELEASE_FENCE() STORE_FENCE()
# endif
#elif defined(__powerpc64__)
# define STORE_FENCE()   __asm__ __volatile__ ("lwsync" : : : "memory")
# define ACQUIRE_FENCE() __asm__ __volatile__ ("sync" : : : "memory")
# define RELEASE_FENCE() ACQUIRE_FENCE()
#elif defined(__powerpc__) || defined(__POWERPC__)
# define STORE_FENCE()   __asm__ __volatile__ ("sync" : : : "memory")
# define ACQUIRE_FENCE() STORE_FENCE()
# define RELEASE_FENCE() STORE_FENCE()
#elif defined(__riscv)
# define STORE_FENCE()   __asm__ __volatile__ ("fence w,rw" : : : "memory")
# define ACQUIRE_FENCE() __asm__ __volatile__ ("fence r,rw" : : : "memory")
# define RELEASE_FENCE() __asm__ __volatile__ ("fence rw,r" : : : "memory")
#elif defined(__loongarch64)
# define STORE_FENCE()   __asm__ __volatile__ ("dbar 0" : : : "memory")
# define ACQUIRE_FENCE() STORE_FENCE()
# define RELEASE_FENCE() STORE_FENCE()
#elif (__GNUC__ >= 5) || C_COMPILER_HAS_BUILTIN(__sync_synchronize)
# define STORE_FENCE() __sync_synchronize()
# define ACQUIRE_FENCE() STORE_FENCE()
# define RELEASE_FENCE() STORE_FENCE()
#else
# define STORE_FENCE() do { } while (0)
#endif

#ifndef ACQUIRE_FENCE
# define ACQUIRE_FENCE() do { } while (0)
#endif
#ifndef RELEASE_FENCE
# define RELEASE_FENCE() do { } while (0)
#endif
  
#if !defined(PTHREADS)
# define COMPARE_AND_SWAP_PTR(a, old, new) ((*(ptr *)(a) == TO_PTR(old)) ? (*(ptr)(a) = TO_PTR(new), 1) : 0)
#elif defined(_MSC_VER)
# if ptr_bits == 64
#  define COMPARE_AND_SWAP_PTR(a, old, new) (_InterlockedCompareExchange64((__int64 *)(a), (__int64)(new), (__int64)(old)) == (__int64)(old))
# else
#  define COMPARE_AND_SWAP_PTR(a, old, new) (_InterlockedCompareExchange((long *)(a), (long)(new), (long)(old)) == (long)(old))
# endif
#elif defined(__arm64__) || defined(__aarch64__)
FORCEINLINE int COMPARE_AND_SWAP_PTR(volatile void *addr, void *old_val, void *new_val) {
  I64 ret;
  __asm__ __volatile__ ("mov %0, #0\n\t"
                        "0:\n\t"
                        "ldxr x12, [%1, #0]\n\t"
                        "cmp x12, %2\n\t"
                        "bne 1f\n\t"
                        "stxr w7, %3, [%1, #0]\n\t"
                        "cmp x7, #0\n\t"
                        "bne 1f\n\t"
                        "mov %0, #1\n\t"
                        "1:\n\t"
                        : "=&r" (ret)
                        : "r" (addr), "r" (old_val), "r" (new_val)
                        : "cc", "memory", "x12", "x7");
  return ret;
}
#elif defined(__arm__) && ((arm_isa_version >= 6) || (__ARM_ARCH >= 6))
FORCEINLINE int COMPARE_AND_SWAP_PTR(volatile void *addr, void *old_val, void *new_val) {
  int ret;
  __asm__ __volatile__ ("mov %0, #0\n\t"
                        "0:\n\t"
                        "ldrex r12, [%1]\n\t"
                        "cmp r12, %2\n\t"
                        "bne 1f\n\t"
                        "strex r7, %3, [%1]\n\t"
                        "cmp r7, #0\n\t"
                        "bne 1f\n\t"
                        "it eq\n\t"
                        "moveq %0, #1\n\t"
                        "1:\n\t"
                        : "=&r" (ret)
                        : "r" (addr), "r" (old_val), "r" (new_val)
                        : "cc", "memory", "r12", "r7");
  return ret;
}
#elif (__GNUC__ >= 5) || C_COMPILER_HAS_BUILTIN(__sync_bool_compare_and_swap)
# define COMPARE_AND_SWAP_PTR(a, old, new) __sync_bool_compare_and_swap((ptr *)(a), TO_PTR(old), TO_PTR(new))
#elif defined(__i386__) || defined(__x86_64__)
# if ptr_bits == 64
#   define CAS_OP_SIZE "q"
# else
#   define CAS_OP_SIZE ""
# endif
FORCEINLINE int COMPARE_AND_SWAP_PTR(volatile void *addr, void *old_val, void *new_val) {
  char result;
  __asm__ __volatile__("lock; cmpxchg" CAS_OP_SIZE " %3, %0; setz %1"
                       : "=m"(*(void **)addr), "=q"(result)
                       : "m"(*(void **)addr), "r" (new_val), "a"(old_val)
                       : "memory");
  return (int) result;
}
#elif defined(__powerpc64__)
FORCEINLINE int COMPARE_AND_SWAP_PTR(volatile void *addr, void *old_val, void *new_val) {
  int ret, tmp;
  __asm__ __volatile__ ("li %0, 0\n\t"
                        "0:\n\t"
                        "ldarx   %1,0,%2\n\t"
                        "cmpw    %3,%1\n\t"
                        "bne- 1f\n\t"
                        "stdcx.  %4,0,%2\n\t"
                        "bne- 1f\n\t"
                        "li %0, 1\n\t"
                        "1:\n\t"
                        : "=&r" (ret), "=&r" (tmp)
                        : "r" (addr), "r" (old_val), "r" (new_val)
                        : "cc", "memory");
  return ret;
}
#elif defined(__riscv)
# error expected a compiler with a CAS intrinsic for RISC-V
#elif defined(__loongarch64)
# error expected a compiler with a CAS intrinsic for LoongArch64
#else
# define COMPARE_AND_SWAP_PTR(a, old, new) ((*(ptr *)(a) == TO_PTR(old)) ? (*(ptr *)(a) = TO_PTR(new), 1) : 0)
#endif
