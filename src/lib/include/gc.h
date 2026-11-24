#ifndef GC_H
#define GC_H

#include <stdint.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#include "gsvalue.h"

void gc_init(size_t init_size_words);
void gc_init_with_stack(size_t init_size_words, void *stack_anchor);
void gc_register_root(void **slot);
void *gc_alloc(size_t size_bytes);
size_t gc_collection_count(void);

#ifdef __DEBUG
#ifndef __DEBUG_OUT
#define __DEBUG_OUT stderr
#endif
#endif

#endif
