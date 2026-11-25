#ifndef GC_H
#define GC_H

#include <stdbool.h>
#include <stdint.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#include "gsvalue.h"

/* Relaxed/release atomic flag polled by LLVM-generated loop backedges & function prologues */
extern int32_t __gs_gc_stw_requested;

void gc_init(size_t init_size_words);
void gc_init_with_stack(size_t init_size_words, void *stack_anchor);
void gc_register_root(void **slot);

/* Multicore thread lifecycle & cooperative safepoint / blocking-section protocol */
void gc_thread_attach(void *stack_anchor);
void gc_thread_detach(void);
void gc_safepoint_slow(void);
void gc_enter_blocking(void *jmp_buf_on_stack);
void gc_leave_blocking(void);

/* Allocation APIs */
void *gc_alloc(size_t size_bytes);
void *gc_alloc_atomic(size_t size_bytes);
size_t gc_collection_count(void);

/* STW hooks used by `gs_conc.c` to trace in-flight concurrent containers & finalize dead handles */
bool gc_is_obj_marked(const void *payload_ptr);
bool gc_mark_ptr(const void *payload_ptr);

#ifdef __DEBUG
#ifndef __DEBUG_OUT
#define __DEBUG_OUT stderr
#endif
#endif

#endif
