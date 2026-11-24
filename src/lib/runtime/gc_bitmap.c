#include "../include/gc_bitmap.h"

#include <setjmp.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_GLOBAL_ROOTS 4096

static gc_heap_t *_heaps = NULL;
static gc_free_t *_free_list = NULL;
static uintptr_t _root_start = 0;
static void **_global_roots[MAX_GLOBAL_ROOTS];
static size_t _global_roots_count = 0;
static size_t _collections = 0;

static gc_heap_t *new_heap(size_t words);
static void *pickup_fragment(size_t size_words, size_t *out_total_words);
static void bitmap_collect(void);
static void bitmap_mark_roots(uintptr_t sp_low, uintptr_t sp_high);
static void bitmap_mark_obj(gc_heap_t *heap, uintptr_t hdr_addr);
static void bitmap_sweep(void);
static gc_heap_t *find_allocated_object_heap(uintptr_t payload_ptr);

size_t gc_collection_count(void)
{
    return _collections;
}

void gc_register_root(void **slot)
{
    if (slot != NULL && _global_roots_count < MAX_GLOBAL_ROOTS) {
        _global_roots[_global_roots_count++] = slot;
    }
}

void gc_init_with_stack(size_t init_size_words, void *stack_anchor)
{
    const char *env_words = getenv("GEISHA_GC_HEAP_WORDS");
    if (env_words != NULL) {
        long parsed = atol(env_words);
        if (parsed >= 32) {
            init_size_words = (size_t)parsed;
        }
    }
    if (init_size_words < 32) {
        init_size_words = 32;
    }

    _heaps = new_heap(init_size_words);
    if (_heaps == NULL) {
        fprintf(stderr, "GC initial heap allocation failed\n");
        abort();
    }

    _free_list = (gc_free_t *)_heaps->base;
    _free_list->size = _heaps->total_words;
    _free_list->next = NULL;

    _root_start = (uintptr_t)stack_anchor;
    _global_roots_count = 0;
    _collections = 0;
}

void gc_init(size_t init_size_words)
{
    void *frame = __builtin_frame_address(0);
    gc_init_with_stack(init_size_words, frame);
}

static gc_heap_t *new_heap(size_t max_size_words)
{
    /* Each free fragment needs at least 2 words (`size` + `next`) */
    if (max_size_words < 32) {
        max_size_words = 32;
    }
    if (max_size_words % BITS_PER_BITMAP != 0) {
        max_size_words = ((max_size_words / BITS_PER_BITMAP) + 1) * BITS_PER_BITMAP;
    }

    size_t bitslen = max_size_words / BITS_PER_BITMAP;

    gc_heap_t *h = (gc_heap_t *)calloc(1, sizeof(gc_heap_t));
    if (h == NULL) return NULL;

    void *raw_mem = calloc(max_size_words, WORD_SIZE);
    bitmap_t *alloc_bits = (bitmap_t *)calloc(bitslen, sizeof(bitmap_t));
    bitmap_t *mark_bits = (bitmap_t *)calloc(bitslen, sizeof(bitmap_t));

    if (raw_mem == NULL || alloc_bits == NULL || mark_bits == NULL) {
        free(raw_mem);
        free(alloc_bits);
        free(mark_bits);
        free(h);
        return NULL;
    }

    h->base = (uintptr_t)raw_mem;
    h->end = h->base + max_size_words * WORD_SIZE;
    h->total_words = max_size_words;
    h->bitslen = bitslen;
    h->alloc_bits = alloc_bits;
    h->mark_bits = mark_bits;
    h->next = NULL;
    return h;
}

void *gc_alloc(size_t size_bytes)
{
    /* Minimum payload of 1 word so total block >= 2 words (sizeof(gc_free_t) / WORD_SIZE == 2) */
    size_t payload_words = (size_bytes + WORD_SIZE - 1) / WORD_SIZE;
    if (payload_words == 0) {
        payload_words = 1;
    }
    size_t total_words = payload_words + 1; /* +1 word for object header */

    size_t actual_total_words = total_words;
    void *block = pickup_fragment(total_words, &actual_total_words);
    if (block == NULL) {
        bitmap_collect();
        block = pickup_fragment(total_words, &actual_total_words);
    }

    /* If live reachable memory genuinely exceeds current arena(s), grow by adding a new arena */
    if (block == NULL) {
        size_t grow_words = _heaps ? _heaps->total_words * 2 : 131072;
        if (grow_words < total_words * 2) {
            grow_words = total_words * 2;
        }
        gc_heap_t *extra = new_heap(grow_words);
        if (extra == NULL) {
            fprintf(stderr, "GC out of memory allocating %zu bytes\n", size_bytes);
            abort();
        }
        extra->next = _heaps;
        _heaps = extra;

        gc_free_t *frag = (gc_free_t *)extra->base;
        frag->size = extra->total_words;
        frag->next = _free_list;
        _free_list = frag;

        block = pickup_fragment(total_words, &actual_total_words);
        if (block == NULL) {
            fprintf(stderr, "GC internal fragmentation failure\n");
            abort();
        }
    }

    payload_words = actual_total_words - 1;

    /* Initialize header word: `(payload_words << 1)` */
    header_t *hdr = (header_t *)block;
    *hdr = ((header_t)payload_words) << 1;

    /* Find owning arena and set allocation bit at `hdr` */
    uintptr_t hdr_addr = (uintptr_t)hdr;
    for (gc_heap_t *h = _heaps; h != NULL; h = h->next) {
        if (hdr_addr >= h->base && hdr_addr < h->end) {
            uintptr_t off = hdr_addr - h->base;
            h->alloc_bits[OFFSET_TO_IDX(off)] |= OFFSET_TO_MASK(off);
            break;
        }
    }

    /* Zero-initialize payload so uninitialized padding words never masquerade as stale heap pointers */
    void *payload = (void *)(hdr + 1);
    memset(payload, 0, payload_words * WORD_SIZE);
    return payload;
}

static void *pickup_fragment(size_t size_words, size_t *out_total_words)
{
    gc_free_t **prev = &_free_list;
    gc_free_t *cur = _free_list;

    while (cur != NULL) {
        if (cur->size >= size_words) {
            size_t rem = cur->size - size_words;
            /* Only split if remainder can hold a `gc_free_t` (2 words); otherwise hand out the whole fragment */
            if (rem >= 2) {
                gc_free_t *split = (gc_free_t *)((uintptr_t)cur + size_words * WORD_SIZE);
                split->size = rem;
                split->next = cur->next;
                *prev = split;
                *out_total_words = size_words;
            } else {
                *out_total_words = cur->size;
                *prev = cur->next;
            }
            return (void *)cur;
        }
        prev = &cur->next;
        cur = cur->next;
    }
    return NULL;
}

__attribute__((noinline)) static void bitmap_collect(void)
{
    /* Spill callee-saved registers onto the stack so register-resident roots are scanned */
    jmp_buf regs;
    setjmp(regs);

    volatile uintptr_t sp_marker = 0;
    uintptr_t sp = (uintptr_t)&sp_marker;
    uintptr_t root_anchor = _root_start;

    uintptr_t lo = (sp < root_anchor) ? sp : root_anchor;
    uintptr_t hi = (sp < root_anchor) ? root_anchor : sp;

    /* Scan jmp_buf words explicitly as well */
    const uintptr_t *reg_words = (const uintptr_t *)&regs;
    size_t reg_count = sizeof(jmp_buf) / (sizeof(uintptr_t));
    for (size_t i = 0; i < reg_count; i++) {
        gc_heap_t *h = find_allocated_object_heap(reg_words[i]);
        if (h != NULL) {
            bitmap_mark_obj(h, reg_words[i] - Header_size);
        }
    }

    bitmap_mark_roots(lo, hi);
    bitmap_sweep();
    _collections++;
}

static gc_heap_t *find_allocated_object_heap(uintptr_t payload_ptr)
{
    if (payload_ptr == 0 || (payload_ptr % WORD_SIZE) != 0) {
        return NULL;
    }
    for (gc_heap_t *h = _heaps; h != NULL; h = h->next) {
        if (payload_ptr >= h->base + Header_size && payload_ptr < h->end) {
            uintptr_t hdr_addr = payload_ptr - Header_size;
            uintptr_t off = hdr_addr - h->base;
            if (h->alloc_bits[OFFSET_TO_IDX(off)] & OFFSET_TO_MASK(off)) {
                return h;
            }
        }
    }
    return NULL;
}

static void bitmap_mark_roots(uintptr_t sp_low, uintptr_t sp_high)
{
    /* 1. Scan C/LLVM call stack regardless of growth direction */
    uintptr_t cur = sp_low & ~(uintptr_t)(WORD_SIZE - 1);
    for (; cur <= sp_high; cur += WORD_SIZE) {
        uintptr_t candidate = *(const uintptr_t *)cur;
        gc_heap_t *h = find_allocated_object_heap(candidate);
        if (h != NULL) {
            bitmap_mark_obj(h, candidate - Header_size);
        }
    }

    /* 2. Scan registered global value roots */
    for (size_t i = 0; i < _global_roots_count; i++) {
        uintptr_t candidate = (uintptr_t)(*_global_roots[i]);
        gc_heap_t *h = find_allocated_object_heap(candidate);
        if (h != NULL) {
            bitmap_mark_obj(h, candidate - Header_size);
        }
    }
}

static void bitmap_mark_obj(gc_heap_t *heap, uintptr_t hdr_addr)
{
    uintptr_t off = hdr_addr - heap->base;
    size_t idx = OFFSET_TO_IDX(off);
    bitmap_t mask = OFFSET_TO_MASK(off);

    /* Already marked? Stop immediately to handle cycles and shared DAGs */
    if (heap->mark_bits[idx] & mask) {
        return;
    }
    heap->mark_bits[idx] |= mask;

    header_t hdr_val = *(const header_t *)hdr_addr;
    size_t payload_words = (size_t)(hdr_val >> 1);
    const uintptr_t *fields = (const uintptr_t *)(hdr_addr + Header_size);

    for (size_t i = 0; i < payload_words; i++) {
        uintptr_t field_val = fields[i];
        gc_heap_t *child_heap = find_allocated_object_heap(field_val);
        if (child_heap != NULL) {
            bitmap_mark_obj(child_heap, field_val - Header_size);
        }
    }
}

static void bitmap_sweep(void)
{
    _free_list = NULL;
    gc_free_t *tail = NULL;

    for (gc_heap_t *h = _heaps; h != NULL; h = h->next) {
        uintptr_t cur = h->base;
        uintptr_t free_start = 0;
        size_t free_words = 0;

        while (cur < h->end) {
            uintptr_t off = cur - h->base;
            size_t idx = OFFSET_TO_IDX(off);
            bitmap_t mask = OFFSET_TO_MASK(off);

            if (h->alloc_bits[idx] & mask) {
                header_t hdr_val = *(const header_t *)cur;
                size_t obj_total_words = (size_t)(hdr_val >> 1) + 1;

                if (h->mark_bits[idx] & mask) {
                    /* Live object: flush any preceding coalesced free run >= 2 words */
                    if (free_words >= 2) {
                        gc_free_t *frag = (gc_free_t *)free_start;
                        frag->size = free_words;
                        frag->next = NULL;
                        if (tail == NULL) {
                            _free_list = frag;
                        } else {
                            tail->next = frag;
                        }
                        tail = frag;
                    }
                    free_words = 0;
                } else {
                    /* Dead object: clear allocation bit and merge into free run */
                    h->alloc_bits[idx] &= ~mask;
                    if (free_words == 0) {
                        free_start = cur;
                    }
                    free_words += obj_total_words;
                }
                cur += obj_total_words * WORD_SIZE;
            } else {
                if (free_words == 0) {
                    free_start = cur;
                }
                free_words += 1;
                cur += WORD_SIZE;
            }
        }

        if (free_words >= 2) {
            gc_free_t *frag = (gc_free_t *)free_start;
            frag->size = free_words;
            frag->next = NULL;
            if (tail == NULL) {
                _free_list = frag;
            } else {
                tail->next = frag;
            }
            tail = frag;
        }

        memset(h->mark_bits, 0, h->bitslen * sizeof(bitmap_t));
    }
}
