/* globals.h
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

/* globals that do NOT need to be preserved in a saved heap.
 * they must be initialized each time the system is brought up. */

/* gc.c */
EXTERN IBOOL S_checkheap;
EXTERN uptr S_checkheap_errors;
#ifndef WIN32
EXTERN ptr S_child_processes[static_generation+1];
#endif /* WIN32 */

/* scheme.c */
EXTERN IBOOL S_boot_time;
EXTERN int S_vfasl_boot_mode;
EXTERN IBOOL S_errors_to_console;
EXTERN ptr S_threads;
EXTERN uptr S_nthreads;
EXTERN uptr S_pagesize;
EXTERN void (*S_abnormal_exit_proc)();
EXTERN char *Sschemeheapdirs;
EXTERN char *Sdefaultheapdirs;
#ifdef PTHREADS
EXTERN s_thread_key_t S_tc_key;
EXTERN scheme_mutex_t S_tc_mutex;
EXTERN s_thread_cond_t S_collect_cond;
EXTERN s_thread_cond_t S_collect_thread0_cond;
EXTERN INT S_tc_mutex_depth;  /* counts how many acquires are by kernel */
EXTERN scheme_mutex_t S_alloc_mutex; /* ordered after S_tc_mutex */
EXTERN INT S_alloc_mutex_depth;  /* counts how many acquires are by kernel */
EXTERN s_thread_cond_t S_terminated_cond;
EXTERN int S_collect_waiting_threads;
EXTERN ptr S_collect_waiting_tcs[maximum_parallel_collect_threads];
EXTERN int S_num_preserve_ownership_threads;
# ifdef IMPLICIT_ATOMIC_AS_EXPLICIT
EXTERN s_thread_mutex_t S_implicit_mutex;
# endif
EXTERN s_thread_t S_main_thread_id;
#endif

/* segment.c */
/* update of the segment table is protected by alloc mutex */
#ifdef segment_t2_bits
#ifdef segment_t3_bits
EXTERN t2table *S_segment_info[1<<segment_t3_bits];
#else
EXTERN t1table *S_segment_info[1<<segment_t2_bits];
#endif
#else
EXTERN seginfo *S_segment_info[1<<segment_t1_bits];
#endif

EXTERN chunkinfo *S_chunks_full;
EXTERN chunkinfo *S_chunks[PARTIAL_CHUNK_POOLS+1];
EXTERN chunkinfo *S_code_chunks_full;
EXTERN chunkinfo *S_code_chunks[PARTIAL_CHUNK_POOLS+1];

/* schsig.c */

/* foreign.c */
#ifdef LOAD_SHARED_OBJECT
EXTERN ptr S_foreign_dynamic;
#endif

/* globals that do need to be preserved in a saved heap */
EXTERN struct S_G_struct {
  /* scheme.c */
    double thread_context[size_tc / sizeof(double)];
    thread_gc main_thread_gc;
    ptr active_threads_id;
    ptr error_invoke_code_object;
    ptr invoke_code_object;
    ptr dummy_code_object;
    ptr heap_reserve_ratio_id;
    IBOOL retain_static_relocation;
    IBOOL enable_object_counts;
    IBOOL enable_object_backreferences;
    ptr scheme_version_id;
    ptr make_load_binary_id;
    ptr load_binary;
    ptr profile_counters;

  /* foreign.c */
    ptr foreign_static;
    ptr foreign_names;

  /* thread.c */
    ptr threadno;

  /* segment.c */
    seginfo *occupied_segments[static_generation+1][max_real_space+1];
    uptr number_of_nonstatic_segments;
    uptr number_of_empty_segments;

  /* alloc.c */
    ptr *protected[max_protected];
    uptr protect_next;
    uptr bytes_of_space[static_generation+1][max_real_space+1]; /* protected by alloc mutex */
    uptr bytes_of_generation[static_generation+1]; /* protected by alloc mutex */
    uptr bitmask_overhead[static_generation+1];
    uptr g0_bytes_after_last_gc;
    uptr collect_trip_bytes;
    ptr nonprocedure_code;
    ptr null_string;
    ptr null_vector;
    ptr null_fxvector;
    ptr null_flvector;
    ptr null_bytevector;
    ptr null_immutable_string;
    ptr null_immutable_vector;
    ptr null_immutable_bytevector;
    ptr zero_length_bignum;
    seginfo *dirty_segments[DIRTY_SEGMENT_LISTS];

  /* schsig.c */
    ptr error_id;
    ptr nuate_id;
    ptr null_continuation_id;
    ptr collect_request_pending_id;
    ptr event_and_resume_id;
    ptr event_and_resume_star_id;

  /* gc.c */
    ptr guardians[static_generation+1];
    ptr locked_objects[static_generation+1];
    ptr unlocked_objects[static_generation+1];
    IGEN min_free_gen;
    IGEN new_min_free_gen;
    IGEN max_nonstatic_generation;
    IGEN new_max_nonstatic_generation;
    IGEN min_mark_gen;
    uptr countof[static_generation+1][countof_types];
    uptr bytesof[static_generation+1][countof_types];
    uptr gctimestamp[static_generation+1];
    ptr rtds_with_counts[static_generation+1];
    uptr countof_size[countof_types];
    ptr static_id;
    ptr countof_names;
    ptr gcbackreference[static_generation+1];
    IGEN prcgeneration;
    uptr bytes_finalized;
    dirtycardinfo *new_dirty_cards;
    IBOOL must_mark_gen0;

  /* intern.c */
    iptr oblist_length;
    iptr oblist_count;
    bucket **oblist;
    bucket_list *buckets_of_generation[static_generation];

  /* prim.c */
    ptr library_entry_vector;
    ptr c_entry_vector;

  /* fasl.c */
    ptr base_rtd;
    ptr rtd_key;
    ptr eq_symbol;
    ptr eq_ht_rtd;
    ptr symbol_symbol;
    ptr symbol_ht_rtd;
    ptr eqp;
    ptr eqvp;
    ptr equalp;
    ptr symboleqp;

  /* vfasl.c */
   struct vfasl_hash_table *c_entries;
   struct vfasl_hash_table *library_entries;
   struct vfasl_hash_table *library_entry_codes;

#ifdef PORTABLE_BYTECODE
  /* ffi.c */
  ptr foreign_callables;
  iptr foreign_callables_fuel;
#endif
} S_G;
