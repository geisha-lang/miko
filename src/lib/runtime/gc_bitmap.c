#include "../include/gc_bitmap.h"
#include "../include/gs_conc.h"

#include <pthread.h>
#include <setjmp.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_GLOBAL_ROOTS 4096

/* Global safepoint flag polled by generated LLVM IR (`0` = normal execution, `1` = STW requested) */
int32_t __gs_gc_stw_requested = 0;

typedef struct gc_thread_t {
    uintptr_t root_start;
    uintptr_t safepoint_sp;
    jmp_buf safepoint_regs;
    bool is_blocked_or_parked;
    struct gc_thread_t *next;
    struct gc_thread_t *prev;
} gc_thread_t;

static pthread_mutex_t _gc_mu = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t _stw_park_cv = PTHREAD_COND_INITIALIZER;
static pthread_cond_t _stw_resume_cv = PTHREAD_COND_INITIALIZER;
static bool _stw_in_progress = false;

static _Thread_local gc_thread_t *_tls_thread = NULL;
static gc_thread_t *_threads_head = NULL;

static gc_heap_t *_heaps = NULL;
static gc_free_t *_free_list = NULL;
static size_t _default_init_words = 131072;
static size_t _last_sweep_total_words = 0;
static size_t _last_sweep_free_words = 0;
static void **_global_roots[MAX_GLOBAL_ROOTS];
static size_t _global_roots_count = 0;
static size_t _collections = 0;

static gc_heap_t *new_heap(size_t words);
static void *pickup_fragment(size_t size_words, size_t *out_total_words);
static void bitmap_collect_locked(gc_thread_t *collector);
static void bitmap_mark_stack_range(uintptr_t sp_low, uintptr_t sp_high);
static bool bitmap_mark_obj(gc_heap_t *heap, uintptr_t hdr_addr);
static void bitmap_sweep(void);
static gc_heap_t *find_allocated_object_heap(uintptr_t payload_ptr);

static bool all_threads_parked_locked(void)
{
    for (gc_thread_t *t = _threads_head; t != NULL; t = t->next) {
        if (!t->is_blocked_or_parked) {
            return false;
        }
    }
    return true;
}

size_t gc_collection_count(void)
{
    pthread_mutex_lock(&_gc_mu);
    size_t c = _collections;
    pthread_mutex_unlock(&_gc_mu);
    return c;
}

void gc_register_root(void **slot)
{
    if (slot == NULL) return;
    pthread_mutex_lock(&_gc_mu);
    if (_global_roots_count < MAX_GLOBAL_ROOTS) {
        _global_roots[_global_roots_count++] = slot;
    }
    pthread_mutex_unlock(&_gc_mu);
}

static void link_thread_locked(gc_thread_t *t)
{
    t->prev = NULL;
    t->next = _threads_head;
    if (_threads_head != NULL) {
        _threads_head->prev = t;
    }
    _threads_head = t;
}

static void unlink_thread_locked(gc_thread_t *t)
{
    if (t->prev != NULL) {
        t->prev->next = t->next;
    } else {
        _threads_head = t->next;
    }
    if (t->next != NULL) {
        t->next->prev = t->prev;
    }
    t->prev = NULL;
    t->next = NULL;
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

    pthread_mutex_lock(&_gc_mu);
    _default_init_words = init_size_words;
    _heaps = new_heap(init_size_words);
    if (_heaps == NULL) {
        pthread_mutex_unlock(&_gc_mu);
        fprintf(stderr, "GC initial heap allocation failed\n");
        abort();
    }

    _free_list = (gc_free_t *)_heaps->base;
    _free_list->size = _heaps->total_words;
    _free_list->next = NULL;

    _global_roots_count = 0;
    _collections = 0;
    _stw_in_progress = false;
    __atomic_store_n(&__gs_gc_stw_requested, 0, __ATOMIC_RELEASE);

    if (_tls_thread == NULL) {
        gc_thread_t *main_thr = (gc_thread_t *)calloc(1, sizeof(gc_thread_t));
        if (main_thr == NULL) {
            pthread_mutex_unlock(&_gc_mu);
            abort();
        }
        main_thr->root_start = (uintptr_t)stack_anchor;
        main_thr->is_blocked_or_parked = false;
        link_thread_locked(main_thr);
        _tls_thread = main_thr;
    } else {
        _tls_thread->root_start = (uintptr_t)stack_anchor;
        _tls_thread->is_blocked_or_parked = false;
    }
    pthread_mutex_unlock(&_gc_mu);
}

void gc_init(size_t init_size_words)
{
    void *frame = __builtin_frame_address(0);
    gc_init_with_stack(init_size_words, frame);
}

void gc_thread_attach(void *stack_anchor)
{
    if (_tls_thread != NULL) {
        return;
    }
    gc_thread_t *thr = (gc_thread_t *)calloc(1, sizeof(gc_thread_t));
    if (thr == NULL) {
        fprintf(stderr, "GC failed to allocate thread context\n");
        abort();
    }
    thr->root_start = (uintptr_t)stack_anchor;
    thr->is_blocked_or_parked = false;

    pthread_mutex_lock(&_gc_mu);
    while (_stw_in_progress) {
        pthread_cond_wait(&_stw_resume_cv, &_gc_mu);
    }
    link_thread_locked(thr);
    _tls_thread = thr;
    pthread_mutex_unlock(&_gc_mu);
}

void gc_thread_detach(void)
{
    gc_thread_t *thr = _tls_thread;
    if (thr == NULL) {
        return;
    }
    pthread_mutex_lock(&_gc_mu);
    unlink_thread_locked(thr);
    if (_stw_in_progress && all_threads_parked_locked()) {
        pthread_cond_signal(&_stw_park_cv);
    }
    pthread_mutex_unlock(&_gc_mu);
    _tls_thread = NULL;
    free(thr);
}

__attribute__((noinline)) void gc_safepoint_slow(void)
{
    gc_thread_t *self = _tls_thread;
    if (self == NULL) {
        return;
    }
    if (__atomic_load_n(&__gs_gc_stw_requested, __ATOMIC_ACQUIRE) == 0) {
        return;
    }

    jmp_buf regs;
    _setjmp(regs);
    volatile uintptr_t sp_marker = 0;

    pthread_mutex_lock(&_gc_mu);
    if (_stw_in_progress) {
        memcpy(&self->safepoint_regs, &regs, sizeof(jmp_buf));
        self->safepoint_sp = (uintptr_t)&sp_marker;
        self->is_blocked_or_parked = true;
        if (all_threads_parked_locked()) {
            pthread_cond_signal(&_stw_park_cv);
        }
        while (_stw_in_progress) {
            pthread_cond_wait(&_stw_resume_cv, &_gc_mu);
        }
        self->is_blocked_or_parked = false;
    }
    pthread_mutex_unlock(&_gc_mu);
}

void gc_enter_blocking(void *jmp_buf_on_stack)
{
    gc_thread_t *self = _tls_thread;
    if (self == NULL) {
        return;
    }
    volatile uintptr_t sp_here = 0;

    pthread_mutex_lock(&_gc_mu);
    if (jmp_buf_on_stack != NULL) {
        memcpy(&self->safepoint_regs, jmp_buf_on_stack, sizeof(jmp_buf));
        self->safepoint_sp = (uintptr_t)jmp_buf_on_stack;
    } else {
        self->safepoint_sp = (uintptr_t)&sp_here;
    }
    self->is_blocked_or_parked = true;
    if (_stw_in_progress && all_threads_parked_locked()) {
        pthread_cond_signal(&_stw_park_cv);
    }
    pthread_mutex_unlock(&_gc_mu);
}

void gc_leave_blocking(void)
{
    gc_thread_t *self = _tls_thread;
    if (self == NULL) {
        return;
    }
    pthread_mutex_lock(&_gc_mu);
    while (_stw_in_progress) {
        pthread_cond_wait(&_stw_resume_cv, &_gc_mu);
    }
    self->is_blocked_or_parked = false;
    pthread_mutex_unlock(&_gc_mu);
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

__attribute__((noinline)) static void *gc_alloc_impl(size_t size_bytes, bool noscan)
{
    size_t payload_words = (size_bytes + WORD_SIZE - 1) / WORD_SIZE;
    if (payload_words == 0) {
        payload_words = 1;
    }
    size_t total_words = payload_words + 1; /* +1 word for object header */

    /* Spill registers before taking `_gc_mu` in case this allocation triggers or parks for STW */
    jmp_buf regs;
    _setjmp(regs);
    volatile uintptr_t sp_marker = 0;

    gc_thread_t *self = _tls_thread;

    pthread_mutex_lock(&_gc_mu);
    while (_stw_in_progress) {
        if (self != NULL) {
            memcpy(&self->safepoint_regs, &regs, sizeof(jmp_buf));
            self->safepoint_sp = (uintptr_t)&sp_marker;
            self->is_blocked_or_parked = true;
            if (all_threads_parked_locked()) {
                pthread_cond_signal(&_stw_park_cv);
            }
        }
        pthread_cond_wait(&_stw_resume_cv, &_gc_mu);
        if (self != NULL) {
            self->is_blocked_or_parked = false;
        }
    }

    size_t actual_total_words = total_words;
    void *block = pickup_fragment(total_words, &actual_total_words);
    bool collected = false;
    if (block == NULL) {
        if (self != NULL) {
            memcpy(&self->safepoint_regs, &regs, sizeof(jmp_buf));
            self->safepoint_sp = (uintptr_t)&sp_marker;
        }
        bitmap_collect_locked(self);
        collected = true;
        block = pickup_fragment(total_words, &actual_total_words);
    }

    /* Grow heap if allocation still failed OR post-GC free headroom is below 40% (prevents GC thrashing!) */
    if (block == NULL || (collected && _last_sweep_free_words * 5 < _last_sweep_total_words * 2)) {
        size_t total_cur = _last_sweep_total_words > 0 ? _last_sweep_total_words : (_heaps ? _heaps->total_words : _default_init_words);
        size_t grow_words = total_cur;
        if (grow_words < total_words * 2) {
            grow_words = total_words * 2;
        }
        gc_heap_t *extra = new_heap(grow_words);
        if (extra == NULL && block == NULL) {
            pthread_mutex_unlock(&_gc_mu);
            fprintf(stderr, "GC out of memory allocating %zu bytes\n", size_bytes);
            abort();
        }
        if (extra != NULL) {
            extra->next = _heaps;
            _heaps = extra;

            gc_free_t *frag = (gc_free_t *)extra->base;
            frag->size = extra->total_words;
            frag->next = _free_list;
            _free_list = frag;
        }

        if (block == NULL) {
            block = pickup_fragment(total_words, &actual_total_words);
            if (block == NULL) {
                pthread_mutex_unlock(&_gc_mu);
                fprintf(stderr, "GC internal fragmentation failure\n");
                abort();
            }
        }
    }

    payload_words = actual_total_words - 1;

    header_t *hdr = (header_t *)block;
    *hdr = (((header_t)payload_words) << 1) | (noscan ? HEADER_NOSCAN_BIT : (header_t)0);

    uintptr_t hdr_addr = (uintptr_t)hdr;
    for (gc_heap_t *h = _heaps; h != NULL; h = h->next) {
        if (hdr_addr >= h->base && hdr_addr < h->end) {
            uintptr_t off = hdr_addr - h->base;
            h->alloc_bits[OFFSET_TO_IDX(off)] |= OFFSET_TO_MASK(off);
            break;
        }
    }
    pthread_mutex_unlock(&_gc_mu);

    /* Zero payload outside `_gc_mu`; safe because this thread cannot park at a safepoint before returning */
    void *payload = (void *)(hdr + 1);
    memset(payload, 0, payload_words * WORD_SIZE);
    return payload;
}

void *gc_alloc(size_t size_bytes)
{
    return gc_alloc_impl(size_bytes, false);
}

void *gc_alloc_atomic(size_t size_bytes)
{
    return gc_alloc_impl(size_bytes, true);
}

static void *pickup_fragment(size_t size_words, size_t *out_total_words)
{
    gc_free_t **prev = &_free_list;
    gc_free_t *cur = _free_list;

    while (cur != NULL) {
        if (cur->size >= size_words) {
            size_t rem = cur->size - size_words;
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

bool gc_is_obj_marked(const void *payload_ptr)
{
    uintptr_t addr = (uintptr_t)payload_ptr;
    gc_heap_t *h = find_allocated_object_heap(addr);
    if (h == NULL) {
        return false;
    }
    uintptr_t hdr_addr = addr - Header_size;
    uintptr_t off = hdr_addr - h->base;
    return (h->mark_bits[OFFSET_TO_IDX(off)] & OFFSET_TO_MASK(off)) != 0;
}

bool gc_mark_ptr(const void *payload_ptr)
{
    uintptr_t addr = (uintptr_t)payload_ptr;
    gc_heap_t *h = find_allocated_object_heap(addr);
    if (h == NULL) {
        return false;
    }
    return bitmap_mark_obj(h, addr - Header_size);
}

static void bitmap_collect_locked(gc_thread_t *collector)
{
    _stw_in_progress = true;
    __atomic_store_n(&__gs_gc_stw_requested, 1, __ATOMIC_RELEASE);

    if (collector != NULL) {
        collector->is_blocked_or_parked = true;
    }

    while (!all_threads_parked_locked()) {
        pthread_cond_wait(&_stw_park_cv, &_gc_mu);
    }

    /* 1. Mark roots across all registered threads (saved registers + stack ranges) */
    for (gc_thread_t *t = _threads_head; t != NULL; t = t->next) {
        const uintptr_t *reg_words = (const uintptr_t *)&t->safepoint_regs;
        size_t reg_count = sizeof(jmp_buf) / sizeof(uintptr_t);
        for (size_t i = 0; i < reg_count; i++) {
            gc_heap_t *h = find_allocated_object_heap(reg_words[i]);
            if (h != NULL) {
                bitmap_mark_obj(h, reg_words[i] - Header_size);
            }
        }
        if (t->safepoint_sp != 0 && t->root_start != 0) {
            uintptr_t lo = (t->safepoint_sp < t->root_start) ? t->safepoint_sp : t->root_start;
            uintptr_t hi = (t->safepoint_sp < t->root_start) ? t->root_start : t->safepoint_sp;
            bitmap_mark_stack_range(lo, hi);
        }
    }

    /* 2. Mark registered global value slots */
    for (size_t i = 0; i < _global_roots_count; i++) {
        uintptr_t candidate = (uintptr_t)(*_global_roots[i]);
        gc_heap_t *h = find_allocated_object_heap(candidate);
        if (h != NULL) {
            bitmap_mark_obj(h, candidate - Header_size);
        }
    }

    /* 3. Trace values held inside active/reachable Tasks, Channels, MVars, and Actors to a fixed point */
    gs_conc_mark_roots();

    /* 4. Reclaim unreachable synchronization handles BEFORE `mark_bits` are cleared by `bitmap_sweep` */
    gs_conc_sweep_unreachable();

    /* 5. Sweep heap arenas and coalesce free fragments */
    bitmap_sweep();
    _collections++;

    if (collector != NULL) {
        collector->is_blocked_or_parked = false;
    }
    __atomic_store_n(&__gs_gc_stw_requested, 0, __ATOMIC_RELEASE);
    _stw_in_progress = false;
    pthread_cond_broadcast(&_stw_resume_cv);
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

static void bitmap_mark_stack_range(uintptr_t sp_low, uintptr_t sp_high)
{
    uintptr_t cur = sp_low & ~(uintptr_t)(WORD_SIZE - 1);
    for (; cur <= sp_high; cur += WORD_SIZE) {
        uintptr_t candidate = *(const uintptr_t *)cur;
        gc_heap_t *h = find_allocated_object_heap(candidate);
        if (h != NULL) {
            bitmap_mark_obj(h, candidate - Header_size);
        }
    }
}

typedef struct {
    gc_heap_t *heap;
    uintptr_t hdr_addr;
} gc_mark_entry_t;

static gc_mark_entry_t *_mark_stack = NULL;
static size_t _mark_stack_cap = 0;

static void mark_stack_push(gc_heap_t *heap, uintptr_t hdr_addr, size_t *sp)
{
    if (*sp >= _mark_stack_cap) {
        size_t next_cap = _mark_stack_cap == 0 ? 4096 : _mark_stack_cap * 2;
        gc_mark_entry_t *grown = (gc_mark_entry_t *)realloc(_mark_stack, next_cap * sizeof(gc_mark_entry_t));
        if (grown == NULL) {
            fprintf(stderr, "GC mark stack allocation failed\n");
            abort();
        }
        _mark_stack = grown;
        _mark_stack_cap = next_cap;
    }
    _mark_stack[*sp].heap = heap;
    _mark_stack[*sp].hdr_addr = hdr_addr;
    (*sp)++;
}

static bool bitmap_mark_obj(gc_heap_t *root_heap, uintptr_t root_hdr_addr)
{
    uintptr_t root_off = root_hdr_addr - root_heap->base;
    size_t root_idx = OFFSET_TO_IDX(root_off);
    bitmap_t root_mask = OFFSET_TO_MASK(root_off);

    if (root_heap->mark_bits[root_idx] & root_mask) {
        return false;
    }
    root_heap->mark_bits[root_idx] |= root_mask;

    header_t root_hdr = *(const header_t *)root_hdr_addr;
    if (Is_noscan_hdr(root_hdr)) {
        return true;
    }

    size_t sp = 0;
    mark_stack_push(root_heap, root_hdr_addr, &sp);

    while (sp > 0) {
        sp--;
        uintptr_t hdr_addr = _mark_stack[sp].hdr_addr;
        header_t hdr_val = *(const header_t *)hdr_addr;
        if (Is_noscan_hdr(hdr_val)) {
            continue;
        }
        size_t payload_words = Get_payload_words_hdr(hdr_val);
        const uintptr_t *fields = (const uintptr_t *)(hdr_addr + Header_size);

        for (size_t i = 0; i < payload_words; i++) {
            uintptr_t field_val = fields[i];
            gc_heap_t *child_heap = find_allocated_object_heap(field_val);
            if (child_heap != NULL) {
                uintptr_t child_hdr = field_val - Header_size;
                uintptr_t coff = child_hdr - child_heap->base;
                size_t cidx = OFFSET_TO_IDX(coff);
                bitmap_t cmask = OFFSET_TO_MASK(coff);
                if ((child_heap->mark_bits[cidx] & cmask) == 0) {
                    child_heap->mark_bits[cidx] |= cmask;
                    if (!Is_noscan_hdr(*(const header_t *)child_hdr)) {
                        mark_stack_push(child_heap, child_hdr, &sp);
                    }
                }
            }
        }
    }
    return true;
}

static void bitmap_sweep(void)
{
    _free_list = NULL;
    gc_free_t *tail = NULL;
    size_t sum_total = 0;
    size_t sum_free = 0;

    for (gc_heap_t *h = _heaps; h != NULL; h = h->next) {
        sum_total += h->total_words;
        uintptr_t cur = h->base;
        uintptr_t free_start = 0;
        size_t free_words = 0;

        while (cur < h->end) {
            uintptr_t off = cur - h->base;
            size_t idx = OFFSET_TO_IDX(off);
            bitmap_t mask = OFFSET_TO_MASK(off);

            if (h->alloc_bits[idx] & mask) {
                header_t hdr_val = *(const header_t *)cur;
                size_t obj_total_words = Get_payload_words_hdr(hdr_val) + 1;

                if (h->mark_bits[idx] & mask) {
                    if (free_words >= 2) {
                        sum_free += free_words;
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
            sum_free += free_words;
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
    _last_sweep_total_words = sum_total;
    _last_sweep_free_words = sum_free;
}
