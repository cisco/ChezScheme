/* externs.h
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

/* This file sets up platform-dependent includes and extern declarations
 * for Scheme globals not intended for use outside of the system (prefixed
 * with S_).  Scheme globals intended for use outside of the system
 * (prefixed with S) are declared in scheme.h
 */

#include <stdlib.h>
#include <string.h>
#include <errno.h>

#ifndef WIN32
#include <unistd.h>

#if (machine_type == machine_type_i3qnx || machine_type == machine_type_ti3qnx)
off64_t lseek64(int,off64_t,int);
#endif

#endif

#ifdef SOLARIS
#include <fcntl.h>
#include <sys/wait.h>
#include <setjmp.h>
#endif

#ifdef WIN32
#include <fcntl.h>
#include <direct.h> /* for _getcwd */
#include <setjmp.h>
#endif

/* external procedure declarations */
/* prototypes gen. by ProtoGen Version 0.31 (Haydn Huntley) 1/18/93 */

/* alloc.c */
extern void S_alloc_init PROTO((void));
extern void S_protect PROTO((ptr *p));
extern void S_reset_scheme_stack PROTO((ptr tc, iptr n));
extern void S_reset_allocation_pointer PROTO((ptr tc));
extern ptr S_compute_bytes_allocated PROTO((ptr xg, ptr xs));
extern ptr S_find_more_room PROTO((ISPC s, IGEN g, iptr n, ptr old));
extern void S_dirty_set PROTO((ptr *loc, ptr x));
extern void S_scan_dirty PROTO((ptr **p, ptr **endp));
extern void S_scan_remembered_set PROTO((void));
extern void S_get_more_room PROTO((void));
extern ptr S_cons_in PROTO((ISPC s, IGEN g, ptr car, ptr cdr));
extern ptr S_symbol PROTO((ptr name));
extern ptr S_rational PROTO((ptr n, ptr d));
extern ptr S_tlc PROTO((ptr keyval, ptr tconc, ptr next));
extern ptr S_vector_in PROTO((ISPC s, IGEN g, iptr n));
extern ptr S_vector PROTO((iptr n));
extern ptr S_fxvector PROTO((iptr n));
extern ptr S_bytevector PROTO((iptr n));
extern ptr S_null_immutable_vector PROTO((void));
extern ptr S_null_immutable_fxvector PROTO((void));
extern ptr S_null_immutable_bytevector PROTO((void));
extern ptr S_null_immutable_string PROTO((void));
extern ptr S_record PROTO((iptr n));
extern ptr S_closure PROTO((ptr cod, iptr n));
extern ptr S_mkcontinuation PROTO((ISPC s, IGEN g, ptr nuate, ptr stack,
                iptr length, iptr clength, ptr link, ptr ret, ptr winders));
extern ptr S_inexactnum PROTO((double rp, double ip));
extern ptr S_exactnum PROTO((ptr a, ptr b));
extern ptr S_thread PROTO((ptr tc));
extern ptr S_ifile PROTO((iptr icount, ptr name, iptr fd, ptr info, iptr flags, char *ilast,
                  ptr ibuf));
extern ptr S_ofile PROTO((iptr ocount, ptr name, iptr fd, ptr info, iptr flags, char *olast,
            ptr obuf));
extern ptr S_iofile PROTO((iptr icount, iptr ocount, ptr name, iptr fd, ptr info, iptr flags,
            char *ilast, ptr ibuf, char *olast, ptr obuf));
extern ptr S_string PROTO((const char *s, iptr n));
extern ptr S_bignum PROTO((iptr n, IBOOL sign));
extern ptr S_code PROTO((ptr tc, iptr type, iptr n));
extern ptr S_relocation_table PROTO((iptr n));
extern ptr S_thread_get_more_room PROTO((iptr t, iptr n));

/* fasl.c */
extern void S_fasl_init PROTO((void));
ptr S_fasl_read PROTO((ptr file, IBOOL gzflag, ptr path));
ptr S_bv_fasl_read PROTO((ptr bv, ptr path));
/* S_boot_read's f argument is really gzFile, but zlib.h is not included everywhere */
ptr S_boot_read PROTO((gzFile file, const char *path));
char *S_format_scheme_version PROTO((uptr n));
char *S_lookup_machine_type PROTO((uptr n));
extern void S_set_code_obj PROTO((char *who, IFASLCODE typ, ptr p, iptr n,
             ptr x, iptr o));
extern ptr S_get_code_obj PROTO((IFASLCODE typ, ptr p, iptr n, iptr o));

/* flushcache.c */
extern void S_record_code_mod PROTO((ptr tc, uptr addr, uptr bytes));
extern void S_flush_instruction_cache PROTO((ptr tc));
extern void S_flushcache_init PROTO((void));

/* foreign.c */
extern void S_foreign_init PROTO((void));
extern void S_foreign_entry PROTO((void));

/* gcwrapper.c */
extern void S_ptr_tell PROTO((ptr p));
extern void S_addr_tell PROTO((ptr p));
extern void S_gc_init PROTO((void));
#ifndef WIN32
extern void S_register_child_process PROTO((INT child));
#endif /* WIN32 */
extern void S_fixup_counts PROTO((ptr counts));
extern void S_do_gc PROTO((IGEN g, IGEN gtarget));
extern void S_gc PROTO((ptr tc, IGEN mcg, IGEN tg));
extern void S_gc_init PROTO((void));
extern void S_set_maxgen PROTO((IGEN g));
extern IGEN S_maxgen PROTO((void));
extern void S_set_minfreegen PROTO((IGEN g));
extern IGEN S_minfreegen PROTO((void));
#ifndef WIN32
extern void S_register_child_process PROTO((INT child));
#endif /* WIN32 */
extern IBOOL S_enable_object_counts PROTO((void));
extern void S_set_enable_object_counts PROTO((IBOOL eoc));
extern ptr S_object_counts PROTO((void));
extern void S_do_gc PROTO((IGEN g, IGEN gtarget));
extern ptr S_locked_objects PROTO((void));
extern void S_compact_heap PROTO((void));
extern void S_check_heap PROTO((IBOOL aftergc));

/* gc-ocd.c */
extern void S_gc_ocd PROTO((ptr tc, IGEN mcg, IGEN tg));

/* gc-oce.c */
extern void S_gc_oce PROTO((ptr tc, IGEN mcg, IGEN tg));

/* intern.c */
extern void S_intern_init PROTO((void));
extern void S_resize_oblist PROTO((void));
extern ptr S_intern PROTO((const unsigned char *s));
extern ptr S_intern_sc PROTO((const string_char *s, iptr n, ptr name_str));
extern ptr S_intern3 PROTO((const string_char *pname, iptr plen, const string_char *uname, iptr ulen, ptr pname_str, ptr uame_str));
extern void S_intern_gensym PROTO((ptr g));
extern void S_retrofit_nonprocedure_code PROTO((void));

/* io.c */
extern IBOOL S_file_existsp PROTO((const char *inpath, IBOOL followp));
extern IBOOL S_file_regularp PROTO((const char *inpath, IBOOL followp));
extern IBOOL S_file_directoryp PROTO((const char *inpath, IBOOL followp));
extern IBOOL S_file_symbolic_linkp PROTO((const char *inpath));
#ifdef WIN32
extern ptr S_find_files PROTO((const char *wildpath));
#else
extern ptr S_directory_list PROTO((const char *inpath));
#endif
extern char *S_malloc_pathname PROTO((const char *inpath));
#ifdef WIN32
extern wchar_t *S_malloc_wide_pathname PROTO((const char *inpath));
#endif
extern IBOOL S_fixedpathp PROTO((const char *inpath));

/* new-io.c */
extern INT S_gzxfile_fd PROTO((ptr x));
extern gzFile S_gzxfile_gzfile PROTO((ptr x));
extern ptr S_new_open_input_fd PROTO((const char *filename, IBOOL compressed));
extern ptr S_new_open_output_fd PROTO((
  const char *filename, INT mode,
  IBOOL no_create, IBOOL no_fail, IBOOL no_truncate,
  IBOOL append, IBOOL lock, IBOOL replace, IBOOL compressed));
extern ptr S_new_open_input_output_fd PROTO((
  const char *filename, INT mode,
  IBOOL no_create, IBOOL no_fail, IBOOL no_truncate,
  IBOOL append, IBOOL lock, IBOOL replace, IBOOL compressed));
extern ptr S_close_fd PROTO((ptr file, IBOOL gzflag));
extern ptr S_compress_input_fd PROTO((INT fd, I64 fp));
extern ptr S_compress_output_fd PROTO((INT fd));

extern ptr S_bytevector_read PROTO((ptr file, ptr buffer, iptr start, iptr count, IBOOL gzflag));
extern ptr S_bytevector_read_nb PROTO((ptr file, ptr buffer, iptr start, iptr count, IBOOL gzflag));
extern ptr S_bytevector_write PROTO((ptr file, ptr buffer, iptr start, iptr count, IBOOL gzflag));
extern ptr S_put_byte PROTO((ptr file, INT byte, IBOOL gzflag));

extern ptr S_get_fd_pos PROTO((ptr file, IBOOL gzflag));
extern ptr S_set_fd_pos PROTO((ptr file, ptr pos, IBOOL gzflag));
extern ptr S_get_fd_non_blocking PROTO((ptr file, IBOOL gzflag));
extern ptr S_set_fd_non_blocking PROTO((ptr file, IBOOL x, IBOOL gzflag));
extern ptr S_get_fd_length PROTO((ptr file, IBOOL gzflag));
extern ptr S_set_fd_length PROTO((ptr file, ptr length, IBOOL gzflag));
extern void S_new_io_init PROTO((void));

/* thread.c */
extern void S_thread_init PROTO((void));
extern ptr S_create_thread_object PROTO((const char *who, ptr p_tc));
#ifdef PTHREADS
extern ptr S_fork_thread PROTO((ptr thunk));
extern scheme_mutex_t *S_make_mutex PROTO((void));
extern void S_mutex_free PROTO((scheme_mutex_t *m));
extern void S_mutex_acquire PROTO((scheme_mutex_t *m));
extern INT S_mutex_tryacquire PROTO((scheme_mutex_t *m));
extern void S_mutex_release PROTO((scheme_mutex_t *m));
extern s_thread_cond_t *S_make_condition PROTO((void));
extern void S_condition_free PROTO((s_thread_cond_t *c));
extern IBOOL S_condition_wait PROTO((s_thread_cond_t *c, scheme_mutex_t *m, ptr t));
#endif

/* scheme.c */
extern void S_generic_invoke PROTO((ptr tc, ptr code));

/* number.c */
extern void S_number_init PROTO((void));
extern ptr S_normalize_bignum PROTO((ptr x));
extern IBOOL S_integer_valuep PROTO((ptr x));
extern iptr S_integer_value PROTO((const char *who, ptr x));
extern I64 S_int64_value PROTO((char *who, ptr x));
extern IBOOL S_big_eq PROTO((ptr x, ptr y));
extern IBOOL S_big_lt PROTO((ptr x, ptr y));
extern ptr S_add PROTO((ptr x, ptr y));
extern ptr S_sub PROTO((ptr x, ptr y));
extern ptr S_mul PROTO((ptr x, ptr y));
extern ptr S_div PROTO((ptr x, ptr y));
extern ptr S_rem PROTO((ptr x, ptr y));
extern ptr S_trunc PROTO((ptr x, ptr y));
extern void S_trunc_rem PROTO((ptr x, ptr y, ptr *q, ptr *r));
extern ptr S_gcd PROTO((ptr x, ptr y));
extern ptr S_ash PROTO((ptr x, ptr n));
extern ptr S_big_positive_bit_field PROTO((ptr x, ptr fxstart, ptr fxend));
extern ptr S_integer_length PROTO((ptr x));
extern ptr S_big_first_bit_set PROTO((ptr x));
extern double S_random_double PROTO((U32 m1, U32 m2,
               U32 m3, U32 m4, double scale));
extern double S_floatify PROTO((ptr x));
extern ptr S_decode_float PROTO((double d));
extern ptr S_logand PROTO((ptr x, ptr y));
extern ptr S_logbitp PROTO((ptr k, ptr x));
extern ptr S_logbit0 PROTO((ptr k, ptr x));
extern ptr S_logbit1 PROTO((ptr k, ptr x));
extern ptr S_logtest PROTO((ptr x, ptr y));
extern ptr S_logor PROTO((ptr x, ptr y));
extern ptr S_logxor PROTO((ptr x, ptr y));
extern ptr S_lognot PROTO((ptr x));

/* prim.c */
extern ptr S_lookup_library_entry PROTO((iptr n, IBOOL errorp));
extern ptr S_lookup_c_entry PROTO((iptr i));
extern void S_prim_init PROTO((void));

/* prim5.c */
extern ptr S_strerror PROTO((INT errnum));
extern void S_prim5_init PROTO((void));
extern void S_dump_tc PROTO((ptr tc));

/* print.c */
extern void S_print_init PROTO((void));
extern void S_prin1 PROTO((ptr x));

/* schsig.c */
extern ptr S_get_scheme_arg PROTO((ptr tc, iptr n));
extern void S_put_scheme_arg PROTO((ptr tc, iptr n, ptr x));
extern iptr S_continuation_depth PROTO((ptr k));
extern ptr S_single_continuation PROTO((ptr k, iptr n));
extern void S_split_and_resize PROTO((void));
extern void S_handle_overflow PROTO((void));
extern void S_handle_overflood PROTO((void));
extern void S_handle_apply_overflood PROTO((void));
extern void S_overflow PROTO((ptr tc, iptr frame_request));
extern void S_error_reset PROTO((const char *s));
extern void S_error_abort PROTO((const char *s));
extern void S_abnormal_exit PROTO((void));
extern void S_error PROTO((const char *who, const char *s));
extern void S_error1 PROTO((const char *who, const char *s, ptr x));
extern void S_error2 PROTO((const char *who, const char *s, ptr x, ptr y));
extern void S_error3 PROTO((const char *who, const char *s, ptr x, ptr y, ptr z));
extern void S_boot_error PROTO((const ptr who, ptr s, ptr args));
extern void S_handle_docall_error PROTO((void));
extern void S_handle_arg_error PROTO((void));
extern void S_handle_nonprocedure_symbol PROTO((void));
extern void S_handle_values_error PROTO((void));
extern void S_handle_mvlet_error PROTO((void));
extern void S_register_scheme_signal PROTO((iptr sig));
extern void S_fire_collector PROTO((void));
extern void S_noncontinuable_interrupt PROTO((void));
extern void S_schsig_init PROTO((void));
#ifdef DEFINE_MATHERR
#include <math.h>
extern INT matherr PROTO((struct exception *x));
#endif /* DEFINE_MATHERR */

/* segment.c */
extern void S_segment_init PROTO((void));
extern void *S_getmem PROTO((iptr bytes, IBOOL zerofill));
extern void S_freemem PROTO((void *addr, iptr bytes));
extern iptr S_find_segments PROTO((ISPC s, IGEN g, iptr n));
extern void S_free_chunk PROTO((chunkinfo *chunk));
extern void S_free_chunks PROTO((void));
extern uptr S_curmembytes PROTO((void));
extern uptr S_maxmembytes PROTO((void));
extern void S_resetmaxmembytes PROTO((void));
extern void S_move_to_chunk_list PROTO((chunkinfo *chunk, chunkinfo **pchunk_list));

/* stats.c */
extern void S_stats_init PROTO((void));
extern ptr S_cputime PROTO((void));
extern ptr S_realtime PROTO((void));
extern ptr S_clock_gettime PROTO((I32 typeno));
extern ptr S_gmtime PROTO((ptr tzoff, ptr tspair));
extern ptr S_asctime PROTO((ptr dtvec));
extern ptr S_mktime PROTO((ptr dtvec));
extern ptr S_unique_id PROTO((void));
extern void s_gettime PROTO((INT typeno, struct timespec *tp));

/* symbol.c */
extern ptr S_symbol_value PROTO((ptr sym));
extern void S_set_symbol_value PROTO((ptr sym, ptr val));

/* machine-dependent .c files, e.g., x88k.c */
#ifdef FLUSHCACHE
extern INT S_flushcache_max_gap PROTO((void));
extern void S_doflush PROTO((uptr start, uptr end));
#endif
extern void S_machine_init PROTO((void));

/* schlib.c */
extern void S_initframe PROTO((ptr tc, iptr n));
extern void S_put_arg PROTO((ptr tc, iptr i, ptr x));
extern void S_return PROTO((void));
extern void S_call_help PROTO((ptr tc, IBOOL singlep));
extern void S_call_void PROTO((void));
extern ptr S_call_ptr PROTO((void));
extern iptr S_call_fixnum PROTO((void));
extern I32 S_call_int32 PROTO((void));
extern U32 S_call_uns32 PROTO((void));
extern double S_call_double PROTO((void));
extern float S_call_single PROTO((void));
extern U8 *S_call_bytevector PROTO((void));
extern I64 S_call_int64 PROTO((void));
extern U64 S_call_uns64 PROTO((void));
extern uptr S_call_fptr PROTO((void));

#ifdef WIN32
/* windows.c */
extern INT S_getpagesize(void);
extern ptr S_LastErrorString(void);
extern void *S_ntdlopen(const char *path);
extern void *S_ntdlsym(void *h, const char *s);
extern char *S_ntdlerror(void);
extern char *S_GetRegistry(char *buf, int bufsize, char *s);
extern int S_windows_flock(int fd, int operation);
extern int S_windows_chdir(const char *pathname);
extern int S_windows_chmod(const char *pathname, int mode);
extern int S_windows_mkdir(const char *pathname);
extern int S_windows_open(const char *pathname, int flags, int mode);
extern int S_windows_rename(const char *oldpathname, const char *newpathname);
extern int S_windows_rmdir(const char *pathname);
extern int S_windows_stat64(const char *pathname, struct STATBUF *buffer);
extern int S_windows_system(const char *command);
extern int S_windows_unlink(const char *pathname);
extern char *S_windows_getcwd(char *buffer, int maxlen);
#endif /* WIN32 */

#ifdef FEATURE_EXPEDITOR
/* expeditor.c */
extern void S_expeditor_init PROTO((void));
#endif /* FEATURE_EXPEDITOR */
