#ifndef GSVALUE_H
#define GSVALUE_H

#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>

typedef uintptr_t header_t;
typedef header_t value_t;

#define WORD_SIZE (sizeof(void *))

/* Bit 0 of object header marks leaf allocations containing no interior GC pointers (`NOSCAN`) */
#define HEADER_NOSCAN_BIT ((header_t)1)

/* Pointer `p` points to object payload right after the 1-word header */
#define Get_head(p) (*((header_t *)(p) - 1))
#define Header_size (sizeof(header_t))

#define Is_noscan_hdr(h) (((h) & HEADER_NOSCAN_BIT) != 0)
#define Get_payload_words_hdr(h) ((size_t)((h) >> 1))

#define Set_size(p, c) (Get_head(p) = (Get_head(p) & HEADER_NOSCAN_BIT) | (((header_t)(c)) << 1))
#define Get_size(p) (Get_head(p) >> 1)

static inline void gsvalue_init(value_t *ptr, size_t size, bool noscan)
{
    Get_head(ptr) = (((header_t)size) << 1) | (noscan ? HEADER_NOSCAN_BIT : (header_t)0);
}

#endif
