#ifndef GC_H
#define GC_H

#include <stdint.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#include "gsvalue.h"

void gc_init(size_t init_size_words);

// Size to allocate does not include header
void *gc_alloc(size_t size_bytes);



#ifdef __DEBUG

#ifndef __DEBUG_OUT
#define __DEBUG_OUT stdout
#endif

#endif

#endif
