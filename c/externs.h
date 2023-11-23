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
#include <time.h>

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

#if !defined(NORETURN)
# if defined(__GNUC__) || defined(__clang__)
#  define NORETURN __attribute__((noreturn))
# elif defined(_MSC_VER)
#  define NORETURN __declspec(noreturn)
# else
#  define NORETURN
# endif /* defined(__GNUC__) || defined(__clang__) */
#endif /* !defined(NORETURN) */

/* external procedure declarations */
/* prototypes gen. by ProtoGen Version 0.31 (Haydn Huntley) 1/18/93 */

/* alloc.c */
extern void S_alloc_init(void);
extern void S_protect(ptr *p);
extern void S_reset_scheme_stack(ptr tc, iptr n);
extern void S_reset_allocation_pointer(ptr tc); /* call S_maybe_fire_collector afterward outside alloc mutex */
extern void S_maybe_fire_collector(thread_gc *tgc);
extern ptr S_compute_bytes_allocated(ptr xg, ptr xs);
extern ptr S_bytes_finalized();
extern ptr S_find_more_room(ISPC s, IGEN g, iptr n, ptr old);
extern void S_record_new_dirty_card(thread_gc *tgc, ptr *ppp, IGEN to_g);
extern ptr S_find_more_gc_room(thread_gc *tgc, IGEN g, ISPC s, iptr n, ptr old);
extern void S_close_off_thread_local_segment(ptr tc, ISPC s, IGEN g);
extern void S_dirty_set(ptr *loc, ptr x);
extern void S_mark_card_dirty(uptr card, IGEN to_g);
extern void S_scan_dirty(ptr *p, ptr *endp);
extern void S_scan_remembered_set(void);
extern void S_get_more_room(void);
extern ptr S_get_more_room_help(ptr tc, uptr ap, uptr type, uptr size);
extern ptr S_list_bits_ref(ptr p);
extern void S_list_bits_set(ptr p, iptr bits);
extern ptr S_cons_in(ptr tc, ISPC s, IGEN g, ptr car, ptr cdr);
extern ptr S_ephemeron_cons_in(IGEN g, ptr car, ptr cdr);
extern ptr S_symbol(ptr name);
extern ptr S_rational(ptr n, ptr d);
extern ptr S_tlc(ptr keyval, ptr tconc, ptr next);
extern ptr S_vector_in(ptr tc, ISPC s, IGEN g, iptr n);
extern ptr S_vector(iptr n);
extern ptr S_fxvector(iptr n);
extern ptr S_flvector(iptr n);
extern ptr S_bytevector(iptr n);
extern ptr S_bytevector2(ptr tc, iptr n, ISPC spc);
extern ptr S_null_immutable_vector(void);
extern ptr S_null_immutable_fxvector(void);
extern ptr S_null_immutable_bytevector(void);
extern ptr S_null_immutable_string(void);
extern ptr S_stencil_vector(uptr mask);
extern ptr S_system_stencil_vector(uptr mask);
extern ptr S_record(iptr n);
extern ptr S_closure(ptr cod, iptr n);
extern ptr S_mkcontinuation(ISPC s, IGEN g, ptr nuate, ptr stack,
                            iptr length, iptr clength, ptr link, ptr ret, ptr winders,
                            ptr attachments);
extern ptr S_inexactnum(double rp, double ip);
extern ptr S_exactnum(ptr a, ptr b);
extern ptr S_thread(ptr tc);
extern ptr S_string(const char *s, iptr n);
extern ptr S_bignum(ptr tc, iptr n, IBOOL sign);
extern ptr S_code(ptr tc, iptr type, iptr n);
extern ptr S_relocation_table(iptr n);
extern ptr S_weak_cons(ptr car, ptr cdr);
extern ptr S_box2(ptr ref, IBOOL immobile);
extern ptr S_phantom_bytevector(uptr sz);
extern void S_phantom_bytevector_adjust(ptr ph, uptr new_sz);

/* fasl.c */
extern void S_fasl_init(void);
extern ptr S_fasl_read(INT fd, IFASLCODE situation, ptr path, ptr externals);
extern ptr S_bv_fasl_read(ptr bv, int ty, uptr offset, uptr len, ptr path, ptr externals);
extern ptr S_boot_read(faslFile f, const char *path);
extern char *S_format_scheme_version(uptr n);
extern char *S_lookup_machine_type(uptr n);
extern void S_set_code_obj(char *who, IFASLCODE typ, ptr p, iptr n,
                           ptr x, iptr o);
extern ptr S_get_code_obj(IFASLCODE typ, ptr p, iptr n, iptr o);
extern int S_fasl_intern_rtd(ptr *x);
#ifdef X86_64
extern void x86_64_set_popcount_present(ptr code);
#endif
#ifdef PORTABLE_BYTECODE_SWAPENDIAN
extern void S_swap_dounderflow_header_endian(ptr code);
#endif
extern void S_fasl_init_fd(fileFaslFile ffo, ptr path, INT fd,
                           int buffer_mode, uptr size);
extern void S_fasl_init_bytes(faslFile ffo, ptr path, void *data, iptr len);
extern void S_fasl_init_bv(faslFile ffo, ptr path, ptr bv);
extern int S_fasl_bytein(faslFile f);
extern uptr S_fasl_uptrin(faslFile f, INT *bytes_consumed);
extern void S_fasl_bytesin(octet *s, iptr n, faslFile f);

/* vfasl.c */
extern ptr S_vfasl(ptr bv, faslFile stream, iptr offset, iptr len);
extern ptr S_vfasl_to(ptr v);

/* flushcache.c */
extern void S_record_code_mod(ptr tc, uptr addr, uptr bytes);
extern void S_flush_instruction_cache(ptr tc);
extern void S_flushcache_init(void);

/* foreign.c */
extern void S_foreign_init(void);
extern void S_foreign_entry(void);

/* gcwrapper.c */
extern void S_ptr_tell(ptr p);
extern void S_addr_tell(ptr p);
extern void S_gc_init(void);
#ifndef WIN32
extern void S_register_child_process(INT child);
#endif /* WIN32 */
extern void S_fixup_counts(ptr counts);
extern ptr S_do_gc(IGEN max_cg, IGEN min_tg, IGEN max_tg, ptr count_roots);
extern ptr S_gc(ptr tc, IGEN max_cg, IGEN min_tg, IGEN max_tg, ptr count_roots);
extern void S_gc_init(void);
extern void S_set_maxgen(IGEN g);
extern IGEN S_maxgen(void);
extern void S_set_minfreegen(IGEN g);
extern IGEN S_minfreegen(void);
extern void S_set_minmarkgen(IGEN g);
extern IGEN S_minmarkgen(void);
#ifndef WIN32
extern void S_register_child_process(INT child);
#endif /* WIN32 */
extern IBOOL S_enable_object_counts(void);
extern void S_set_enable_object_counts(IBOOL eoc);
extern ptr S_object_counts(void);
extern IBOOL S_enable_object_backreferences(void);
extern void S_set_enable_object_backreferences(IBOOL eoc);
extern ptr S_object_backreferences(void);
extern void S_immobilize_object(ptr v);
extern void S_mobilize_object(ptr v);
extern ptr S_locked_objects(void);
extern ptr S_unregister_guardian(ptr tconc);
extern void S_compact_heap(void);
extern void S_check_heap(IBOOL aftergc, IGEN target_gen);

/* gc-011.c */
extern void S_gc_011(ptr tc);

/* gc-par.c */
extern ptr S_gc_par(ptr tc, IGEN max_cg, IGEN min_tg, IGEN max_tg, ptr count_roots);

/* gc-ocd.c */
extern ptr S_gc_ocd(ptr tc, IGEN max_cg, IGEN min_tg, IGEN max_tg, ptr count_roots);

/* gc-oce.c */
extern ptr S_gc_oce(ptr tc, IGEN max_cg, IGEN min_tg, IGEN max_tg, ptr count_roots);
extern ptr S_count_size_increments(ptr ls, IGEN generation);

/* intern.c */
extern void S_intern_init(void);
extern void S_resize_oblist(void);
extern ptr S_intern(const unsigned char *s);
extern ptr S_intern_sc(const string_char *s, iptr n, ptr name_str);
extern ptr S_intern3(const string_char *pname, iptr plen, const string_char *uname, iptr ulen, ptr pname_str, ptr uame_str);
extern ptr S_intern4(ptr sym);
extern void S_intern_gensym(ptr sym, ptr sym_name);
extern void S_retrofit_nonprocedure_code(void);
extern ptr S_mkstring(const string_char *s, iptr n);
extern I32 S_symbol_hash32(ptr str);
extern I64 S_symbol_hash64(ptr str);

/* io.c */
extern IBOOL S_file_existsp(const char *inpath, IBOOL followp);
extern IBOOL S_file_regularp(const char *inpath, IBOOL followp);
extern IBOOL S_file_directoryp(const char *inpath, IBOOL followp);
extern IBOOL S_file_symbolic_linkp(const char *inpath);
#ifdef WIN32
extern ptr S_find_files(const char *wildpath);
#else
extern ptr S_directory_list(const char *inpath);
#endif
extern char *S_malloc_pathname(const char *inpath);
#ifdef WIN32
extern wchar_t *S_malloc_wide_pathname(const char *inpath);
#endif
extern IBOOL S_fixedpathp(const char *inpath);

/* compress-io.c */
extern INT S_zlib_compress_level(INT compress_level);
extern INT S_lz4_compress_level(INT compress_level);
extern glzFile S_glzdopen_output(INT fd, INT compress_format, INT compress_level);
extern glzFile S_glzdopen_input(INT fd);
extern glzFile S_glzopen_input(const char *path);
#ifdef WIN32
extern glzFile S_glzopen_input_w(const wchar_t *path);
#endif
extern IBOOL S_glzdirect(glzFile file);
extern INT S_glzclose(glzFile file);

extern INT S_glzread(glzFile file, void *buffer, UINT count);
extern INT S_glzwrite(glzFile file, void *buffer, UINT count);
extern long S_glzseek(glzFile file, long offset, INT whence);
extern INT S_glzgetc(glzFile file);
extern INT S_glzungetc(INT c, glzFile file);
extern INT S_glzrewind(glzFile file);

extern void S_glzerror(glzFile file, INT *errnum);
extern void S_glzclearerr(glzFile fdfile);


/* new-io.c */
extern INT S_gzxfile_fd(ptr x);
extern glzFile S_gzxfile_gzfile(ptr x);
extern ptr S_new_open_input_fd(const char *filename, IBOOL compressed);
extern ptr S_new_open_output_fd(const char *filename, INT mode, INT options);
extern ptr S_new_open_input_output_fd(const char *filename, INT mode, INT options);
extern ptr S_close_fd(ptr file, IBOOL gzflag);
extern ptr S_compress_input_fd(INT fd, I64 fp);
extern ptr S_compress_output_fd(INT fd);

extern ptr S_bytevector_read(ptr file, ptr buffer, iptr start, iptr count, IBOOL gzflag);
extern ptr S_bytevector_read_nb(ptr file, ptr buffer, iptr start, iptr count, IBOOL gzflag);
extern ptr S_bytevector_write(ptr file, ptr buffer, iptr start, iptr count, IBOOL gzflag);
extern ptr S_put_byte(ptr file, INT byte, IBOOL gzflag);

extern ptr S_get_fd_pos(ptr file, IBOOL gzflag);
extern ptr S_set_fd_pos(ptr file, ptr pos, IBOOL gzflag);
extern ptr S_get_fd_non_blocking(ptr file, IBOOL gzflag);
extern ptr S_set_fd_non_blocking(ptr file, IBOOL x, IBOOL gzflag);
extern ptr S_get_fd_length(ptr file, IBOOL gzflag);
extern ptr S_set_fd_length(ptr file, ptr length, IBOOL gzflag);
extern void S_new_io_init(void);

extern uptr S_bytevector_compress_size(iptr s_count, INT compress_format);
extern ptr S_bytevector_compress(ptr dest_bv, iptr d_start, iptr d_count,
                                 ptr src_bv, iptr s_start, iptr s_count,
                                 INT compress_format);
extern ptr S_bytevector_uncompress(ptr dest_bv, iptr d_start, iptr d_count,
                                   ptr src_bv, iptr s_start, iptr s_count,
                                   INT compress_format);

/* thread.c */
extern void S_thread_init(void);
extern ptr S_create_thread_object(const char *who, ptr p_tc);
#ifdef PTHREADS
extern ptr S_fork_thread(ptr thunk);
extern ptr S_make_mutex(void);
extern void S_mutex_free(scheme_mutex_t *m);
extern void S_mutex_acquire(scheme_mutex_t *m);
extern INT S_mutex_tryacquire(scheme_mutex_t *m);
extern void S_mutex_release(scheme_mutex_t *m);
extern IBOOL S_mutex_is_owner(scheme_mutex_t *m);
extern s_thread_cond_t *S_make_condition(void);
extern void S_condition_free(s_thread_cond_t *c);
extern IBOOL S_condition_wait(s_thread_cond_t *c, scheme_mutex_t *m, ptr t);
extern INT S_activate_thread(void);
extern void S_unactivate_thread(int mode);
#endif

/* scheme.c */
extern void S_generic_invoke(ptr tc, ptr code);

/* number.c */
extern void S_number_init(void);
extern ptr S_normalize_bignum(ptr x);
extern iptr S_integer_value(const char *who, ptr x);
extern I64 S_int64_value(char *who, ptr x);
extern IBOOL S_big_eq(ptr x, ptr y);
extern IBOOL S_big_lt(ptr x, ptr y);
extern ptr S_big_negate(ptr x);
extern ptr S_add(ptr x, ptr y);
extern ptr S_sub(ptr x, ptr y);
extern ptr S_mul(ptr x, ptr y);
extern ptr S_div(ptr x, ptr y);
extern ptr S_rem(ptr x, ptr y);
extern ptr S_trunc(ptr x, ptr y);
extern void S_trunc_rem(ptr tc, ptr x, ptr y, ptr *q, ptr *r);
extern ptr S_gcd(ptr x, ptr y);
extern ptr S_ash(ptr x, ptr n);
extern ptr S_big_positive_bit_field(ptr x, ptr fxstart, ptr fxend);
extern ptr S_integer_length(ptr x);
extern ptr S_big_trailing_zero_bits(ptr x);
extern ptr S_big_first_bit_set(ptr x);
extern double S_random_double(U32 m1, U32 m2,
               U32 m3, U32 m4, double scale);
extern double S_floatify(ptr x);
extern ptr S_decode_float(double d);
extern ptr S_logand(ptr x, ptr y);
extern ptr S_logbitp(ptr k, ptr x);
extern ptr S_logbit0(ptr k, ptr x);
extern ptr S_logbit1(ptr k, ptr x);
extern ptr S_logtest(ptr x, ptr y);
extern ptr S_logor(ptr x, ptr y);
extern ptr S_logxor(ptr x, ptr y);
extern ptr S_lognot(ptr x);
extern void S_bignum_mask_test(void);

/* prim.c */
extern ptr S_lookup_library_entry(iptr n, IBOOL errorp);
extern ptr S_lookup_c_entry(iptr i);
extern void S_prim_init(void);
extern void S_install_c_entry(iptr i, ptr x);
extern void S_check_c_entry_vector(void);

/* prim5.c */
extern ptr S_strerror(INT errnum);
extern void S_prim5_init(void);
extern void S_dump_tc(ptr tc);
extern ptr S_uninterned(ptr x);

/* print.c */
extern void S_print_init(void);
extern void S_prin1(ptr x);

/* schsig.c */
extern ptr S_get_scheme_arg(ptr tc, iptr n);
extern void S_put_scheme_arg(ptr tc, iptr n, ptr x);
extern iptr S_continuation_depth(ptr k);
extern ptr S_single_continuation(ptr k, iptr n);
extern void S_promote_to_multishot(ptr k);
extern void S_split_and_resize(void);
extern void S_handle_overflow(void);
extern void S_handle_overflood(void);
extern void S_handle_apply_overflood(void);
extern void S_overflow(ptr tc, iptr frame_request);
extern NORETURN void S_error_reset(const char *s);
extern NORETURN void S_error_abort(const char *s);
extern NORETURN void S_abnormal_exit(void);
extern NORETURN void S_error(const char *who, const char *s);
extern NORETURN void S_error1(const char *who, const char *s, ptr x);
extern NORETURN void S_error2(const char *who, const char *s, ptr x, ptr y);
extern NORETURN void S_error3(const char *who, const char *s, ptr x, ptr y, ptr z);
extern NORETURN void S_boot_error(const ptr who, ptr s, ptr args);
extern void S_handle_docall_error(void);
extern void S_handle_arg_error(void);
extern void S_handle_nonprocedure_symbol(void);
extern void S_handle_values_error(void);
extern void S_handle_mvlet_error(void);
extern void S_handle_event_detour(void);
extern ptr S_allocate_scheme_signal_queue(void);
extern ptr S_dequeue_scheme_signals(ptr tc);
extern void S_register_scheme_signal(iptr sig);
extern void S_fire_collector(void);
extern NORETURN void S_noncontinuable_interrupt(void);
extern void S_schsig_init(void);
#ifdef DEFINE_MATHERR
#include <math.h>
extern INT matherr(struct exception *x);
#endif /* DEFINE_MATHERR */

/* segment.c */
extern void S_segment_init(void);
extern void *S_getmem(iptr bytes, IBOOL zerofill, IBOOL for_code);
extern void S_freemem(void *addr, iptr bytes, IBOOL for_code);
extern iptr S_find_segments(thread_gc *creator, ISPC s, IGEN g, iptr n);
extern void S_free_chunk(chunkinfo *chunk, IBOOL for_code);
extern void S_free_chunks(void);
extern uptr S_curmembytes(void);
extern uptr S_maxmembytes(void);
extern void S_resetmaxmembytes(void);
extern void S_adjustmembytes(iptr amt);
extern void S_move_to_chunk_list(chunkinfo *chunk, chunkinfo **pchunk_list);
extern void S_thread_start_code_write(ptr tc, IGEN maxg, IBOOL current, void *hint, uptr hint_len);
extern void S_thread_end_code_write(ptr tc, IGEN maxg, IBOOL current, void *hint, uptr hint_len);

/* stats.c */
extern void S_stats_init(void);
extern ptr S_cputime(void);
extern ptr S_realtime(void);
extern ptr S_clock_gettime(I32 typeno);
extern ptr S_gmtime(ptr tzoff, ptr tspair);
extern ptr S_asctime(ptr dtvec);
extern ptr S_mktime(ptr dtvec);
extern ptr S_unique_id(void);
extern void S_gettime(INT typeno, struct timespec *tp);

/* symbol.c */
extern ptr S_symbol_value(ptr sym);
extern ptr S_symbol_racy_value(ptr sym);
extern void S_set_symbol_value(ptr sym, ptr val);

/* machine-dependent .c files, e.g., x88k.c */
#ifdef FLUSHCACHE
extern INT S_flushcache_max_gap(void);
extern void S_doflush(uptr start, uptr end);
#endif
extern void S_machine_init(void);

/* schlib.c */
extern void S_initframe(ptr tc, iptr n);
extern void S_put_arg(ptr tc, iptr i, ptr x);
extern void S_return(void);
extern void S_call_help(ptr tc, IBOOL singlep, IBOOL lock_ts);
extern void S_call_one_result(void);
extern void S_call_any_results(void);

#ifdef HAND_CODED_SETJMP_SIZE
/* w64setjmp.S */
extern int S_setjmp(void *b);
extern NORETURN void S_longjmp(void *b, int v);
#endif

#ifdef PORTABLE_BYTECODE
/* pb.c */
extern void S_pb_interp(ptr tc, void *bytecode);
extern ptr *S_get_call_arena(ptr tc);
#endif

#ifdef WIN32
/* windows.c */
extern INT S_getpagesize(void);
extern ptr S_LastErrorString(void);
extern HMODULE *S_enum_process_modules(void);
extern void *S_ntdlopen(const char *path);
extern void *S_ntdlsym(void *h, const char *s);
extern ptr S_ntdlerror(void);
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
extern void S_expeditor_init(void);
#endif /* FEATURE_EXPEDITOR */

/* random.c */
uptr S_random_state_next_integer(ptr s, uptr n);
double S_random_state_next_double(ptr s);
void S_random_state_init(ptr s, UINT x);
IBOOL S_random_state_check(double x10, double x11, double x12,
                           double x20, double x21, double x22);
/* ffi.c */
#ifdef PORTABLE_BYTECODE
extern void S_ffi_call(ptr types, ptr proc, ptr *stack);
extern ptr S_ffi_closure(ptr types, ptr proc);
#endif

/* self-exe.c */
extern char *S_get_process_executable_path(const char *execpath);

/* statics.c */
extern void scheme_statics(void);
