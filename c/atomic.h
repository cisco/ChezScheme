#if !defined(PTHREADS)
# define STORE_FENCE() do { } while (0)
#elif defined(__arm64__)
# define STORE_FENCE() __asm__ __volatile__ ("dmb ishst" : : : "memory")
# define ACQUIRE_FENCE() __asm__ __volatile__ ("dmb ish" : : : "memory")
# define RELEASE_FENCE() ACQUIRE_FENCE()
#elif defined(__arm__)
# if arm_isa_version == 7
#  define STORE_FENCE() __asm__ __volatile__ ("dmb ishst" : : : "memory")
#  define ACQUIRE_FENCE() __asm__ __volatile__ ("dmb ish" : : : "memory")
#  define RELEASE_FENCE() ACQUIRE_FENCE()
# else
#  define STORE_FENCE() __asm__ __volatile__ ("mcr p15, 0, %0, c7, c10, 5" : : "r" (0) : "memory")
#  define ACQUIRE_FENCE() STORE_FENCE()
#  define RELEASE_FENCE() STORE_FENCE()
# endif
#elif defined(__powerpc64__)
# define STORE_FENCE() __asm__ __volatile__ ("lwsync" : : : "memory")
# define ACQUIRE_FENCE() __asm__ __volatile__ ("sync" : : : "memory")
# define RELEASE_FENCE() ACQUIRE_FENCE()
#elif defined(__powerpc__) || defined(__POWERPC__)
# define STORE_FENCE() __asm__ __volatile__ ("sync" : : : "memory")
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
# define CAS_ANY_FENCE(a, old, new) ((*(a) == (old)) ? (*(a) = (new), 1) : 0)
#elif defined(__arm64__)
FORCEINLINE int CAS_LOAD_ACQUIRE(volatile void *addr, void *old_val, void *new_val) {
  long ret;
  __asm__ __volatile__ ("mov %0, #0\n\t"       
                        "0:\n\t"
                        "ldaxr x12, [%1, #0]\n\t"
                        "cmp x12, %2\n\t"
                        "bne 1f\n\t"
                        "stxr x7, %3, [%1, #0]\n\t"
                        "cmp x7, #0\n\t"
                        "bne 1f\n\t"
                        "moveq %0, #1\n\t"
                        "1:\n\t"
                        : "=&r" (ret)
                        : "r" (addr), "r" (old_val), "r" (new_val)
                        : "cc", "memory", "x12", "x7");
  return ret;
}
/* same as above, but ldaxr -> ldxr and stxr -> stlxr */
FORCEINLINE int CAS_STORE_RELEASE(volatile void *addr, void *old_val, void *new_val) {
  long ret;
  __asm__ __volatile__ ("mov %0, #0\n\t"       
                        "0:\n\t"
                        "ldxr x12, [%1, #0]\n\t"
                        "cmp x12, %2\n\t"
                        "bne 1f\n\t"
                        "stlxr x7, %3, [%1, #0]\n\t"
                        "cmp x7, #0\n\t"
                        "bne 1f\n\t"
                        "moveq %0, #1\n\t"
                        "1:\n\t"
                        : "=&r" (ret)
                        : "r" (addr), "r" (old_val), "r" (new_val)
                        : "cc", "memory", "x12", "x7");
  return ret;
}
#elif defined(__arm__)
FORCEINLINE int S_cas_any_fence(volatile void *addr, void *old_val, void *new_val) {
  int ret;
  __asm__ __volatile__ ("mcr p15, 0, %0, c7, c10, 5\n\t"
                        "mov %0, #0\n\t"       
                        "0:\n\t"
                        "ldrex r12, [%1, #0]\n\t"
                        "cmp r12, %2\n\t"
                        "bne 1f\n\t"
                        "strex r7, %3, [%1, #0]\n\t"
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
# define CAS_ANY_FENCE(a, old, new) S_cas_any_fence(a, old, new)
#elif (__GNUC__ >= 5) || defined(__clang__)
# define CAS_ANY_FENCE(a, old, new) __sync_bool_compare_and_swap(a, old, new)
#elif defined(_MSC_VER)
# if ptr_bits == 64
#  define CAS_ANY_FENCE(a, old, new) (_InterlockedCompareExchange64((__int64 *)(a), (__int64)(new), (__int64)(old)) == (__int64)(old))
# else
#  define CAS_ANY_FENCE(a, old, new) (_InterlockedCompareExchange64((long *)(a), (long)(new), (long)(old)) == (long)(old))
# endif
#elif defined(__i386__) || defined(__x86_64__)
# if ptr_bits == 64
#   define CAS_OP_SIZE "q"
# else
#   define CAS_OP_SIZE ""
# endif
FORCEINLINE int S_cas_any_fence(volatile void *addr, void *old_val, void *new_val) {
  char result;
  __asm__ __volatile__("lock; cmpxchg" CAS_OP_SIZE " %3, %0; setz %1"
                       : "=m"(*(void **)addr), "=q"(result)
                       : "m"(*(void **)addr), "r" (new_val), "a"(old_val)
                       : "memory");
  return (int) result;
}
# define CAS_ANY_FENCE(a, old, new) S_cas_any_fence(a, old, new)
#else
# define CAS_ANY_FENCE(a, old, new) ((*(a) == (old)) ? (*(a) = (new), 1) : 0)
#endif

#ifdef CAS_ANY_FENCE
# define CAS_LOAD_ACQUIRE(a, old, new)  CAS_ANY_FENCE(a, old, new)
# define CAS_STORE_RELEASE(a, old, new) CAS_ANY_FENCE(a, old, new)
#endif
