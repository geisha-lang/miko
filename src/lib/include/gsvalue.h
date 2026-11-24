#ifndef GSVALUE_H
#define GSVALUE_H

#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>

typedef uintptr_t header_t;
typedef header_t value_t;

#define WORD_SIZE (sizeof(void *))

/* Pointer `p` points to object payload right after the 1-word header */
#define Get_head(p) (*((header_t *)(p) - 1))
#define Header_size (sizeof(header_t))

#define Is_struct(p) ((Get_head(p) & (header_t)1) == 0)
#define Is_closure(p) ((Get_head(p) & (header_t)1) != 0)

#define Size_mask (~(header_t)1)

#define Set_size(p, c) (Get_head(p) = (Get_head(p) & (header_t)1) | (((header_t)(c)) << 1))
#define Get_size(p) (Get_head(p) >> 1)

static inline void gsvalue_init(value_t *ptr, size_t size, bool is_closure)
{
    Get_head(ptr) = (((header_t)size) << 1) | (is_closure ? (header_t)1 : (header_t)0);
}

#endif
