#include "../include/gc_bitmap.h"

#include <stdio.h>

static uintptr_t _free_list;
static uintptr_t _root_start;
static uintptr_t _root_end;
// static gc_hplst_t *_gc_heaps;
static gc_heap_t *_gc_heap;
static mark_ctx_t mark_ctx;

#define PTR_HEAP_OFF(p) ((uintptr_t)(p)-_gc_heap->base)

static inline void bitmap_set_alloc(const void *ptr, size_t size_words);
static inline void bitmap_mark_obj(const void *obj);
static inline void bitmap_mark_root();
static void bitmap_sweep();
static gc_heap_t *new_heap(size_t max_size_words);
static void bitmap_collect();
static void *pickup_fragment(size_t size_words);
inline static bool check_ptr(uintptr_t ptr);

/*
    Public garbage collection functions
*/

void gc_init(size_t init_size_words)
{
    _gc_heap = new_heap(init_size_words);
    mark_ctx.bitslen = _gc_heap->bitslen;
    mark_ctx.bits = (bitmap_t *)calloc(_gc_heap->bitslen, Bitmap_size);

    if (_gc_heap == NULL || mark_ctx.bits == NULL)
    {
        fprintf(stderr, "Initial allocate failed\n");
        abort();
    }

    _free_list = _gc_heap->base;
    Set_free_size(_free_list, init_size_words);
    Set_free_next(_free_list, NULL);

    // get frame address of gc_init (called from main entry)
    // Using frame 0 instead of 1 because LLVM may eliminate the caller's frame
    _root_start = (uintptr_t)__builtin_frame_address(0);
#ifdef __DEBUG
    fprintf(__DEBUG_OUT, "gc_init: heap base=%p, end=%p, root_start=%p\n",
            (void*)_gc_heap->base, (void*)_gc_heap->end, (void*)_root_start);
#endif
}

void *gc_alloc(size_t size_bytes)
{
    // align words, 1 word for header
    size_t words = size_bytes / WORD_SIZE + !!(size_bytes % WORD_SIZE) + 1;
    value_t *ret = (value_t *)pickup_fragment(words);
    if (ret == NULL)
    {
        bitmap_collect();
        ret = (value_t *)pickup_fragment(words);
        if (ret == NULL)
        {
            fprintf(stderr, "GC allocate failed\n");
            abort();
        }
    }
    // Initialize header with object size (words - 1 for header itself)
    // Header stores (size << 1), setting low bit for type (0 = struct)
    *ret = (words - 1) << 1;
    INC_PTR_BYTES(ret, Header_size);
    // bitmap_set_alloc expects pointer after header
    bitmap_set_alloc(ret, words);
#ifdef __DEBUG
    fprintf(__DEBUG_OUT, "gc_alloc: %zu bytes -> %p (%zu words)\n", size_bytes, (void*)ret, words);
#endif
    return ret;
}

/*
    bitmap functions
*/

void *pickup_fragment(size_t size_words)
{
    void *cur = (void *)_free_list;
    while (cur != NULL)
    {
        if (Get_free_size(cur) == size_words)
        {
            _free_list = (uintptr_t)Get_free_next(_free_list);
            return cur;
        }
        else if (Get_free_size(cur) > size_words)
        {
            gc_free_t *n = (gc_free_t *)_free_list;
            INC_PTR_BYTES(n, size_words * WORD_SIZE);
            n->next = Get_free_next(_free_list);
            Set_free_size(n, Get_free_size(cur) - size_words);
            _free_list = (uintptr_t)n;
            return cur;
        }
        else
        {
            cur = (void *)Get_free_next(_free_list);
        }
    }

    return cur;
}

gc_heap_t *new_heap(size_t max_size_words)
{
    // align size to bitmap
    if (max_size_words % Bitmap_size)
    {
        max_size_words = (max_size_words / Bitmap_size + 1) * Bitmap_size;
    }

    size_t bitslen = max_size_words / Bitmap_size;

    void *base = malloc(WORD_SIZE * max_size_words + bitslen * Bitmap_size + sizeof(gc_heap_t));
    if (base == NULL)
    {
        return NULL;
    }

    // reserved for gc_heap_t and bit maps
    memset(base, 0, sizeof(gc_heap_t) + bitslen * Bitmap_size);
    gc_heap_t *ret = (gc_heap_t *)base;

    // Skip past gc_heap_t fixed fields AND the bits[] flexible array
    // The bits[] array is stored inline in gc_heap_t, so heap space starts after it
    INC_PTR_BYTES(base, sizeof(gc_heap_t) + bitslen * Bitmap_size);

    ret->base = (uintptr_t)base;

    INC_PTR_BYTES(base, WORD_SIZE * max_size_words);
    ret->end = (uintptr_t)base;

    ret->bitslen = bitslen;

    return ret;
}

void bitmap_set_alloc(const void *ptr, size_t size_words)
{
    // before the pointed address there is a header
    ptr = (char *)ptr - Header_size;
    size_words++;

    const uintptr_t offset = PTR_HEAP_OFF(ptr);
    _gc_heap->bits[OFFSET_TO_IDX(offset)] |= OFFSET_TO_MASK(offset);
}

void bitmap_collect()
{
    // get the frame address before `gc_alloc`
    _root_end = (uintptr_t)__builtin_frame_address(1);

#ifdef __DEBUG
    fprintf(__DEBUG_OUT, "current root end address: %p\n", _root_end);
#endif

    bitmap_mark_root();
    bitmap_sweep();
}

void bitmap_mark_root()
{
    
    for (uintptr_t cur = _root_start; cur < _root_end; cur += WORD_SIZE) {
        uintptr_t val = *(uintptr_t *)cur;
        if (check_ptr(val)) {
            bitmap_mark_obj((void *)val);
        }
    }
}

void bitmap_mark_obj(const void *obj)
{
    // get actual address to header
    obj = (char *)obj - Header_size;

#ifdef __DEBUG

    fprintf(__DEBUG_OUT, "mark object at %p\n", obj);

#endif

    // mark object
    const uintptr_t offset = PTR_HEAP_OFF(obj);
    mark_ctx.bits[OFFSET_TO_IDX(offset)] |= OFFSET_TO_MASK(offset);

    // scan fields
    size_t obj_size = Get_size(obj);
    void *cur = obj;
    INC_PTR_BYTES(obj, obj_size * WORD_SIZE);
    while ((uintptr_t)cur < (uintptr_t)obj)
    {
        void *field = *(void **)cur;
        if (check_ptr((uintptr_t)field))
        {
            bitmap_mark_obj(field);
        }
        INC_PTR_BYTES(cur, 4); // increase by struct padding size
    }
}

void bitmap_sweep()
{
    // Reset free list - we'll rebuild it during sweep
    _free_list = 0;
    gc_free_t *last_free = NULL;

    uintptr_t cur = _gc_heap->base;
    uintptr_t free_start = 0;
    size_t free_size = 0;

    while (cur < _gc_heap->end)
    {
        const uintptr_t offset = cur - _gc_heap->base;
        const size_t idx = OFFSET_TO_IDX(offset);
        const bitmap_t mask = OFFSET_TO_MASK(offset);

        // Check if this position has an allocated object
        if (_gc_heap->bits[idx] & mask)
        {
            // Get object size from header
            value_t *obj = (value_t *)cur;
            INC_PTR_BYTES(obj, Header_size);
            size_t obj_words = Get_size(obj) + 1; // +1 for header

            // Check if object is marked
            if (mark_ctx.bits[idx] & mask)
            {
                // Object is alive - if we had a free run, add it to free list
                if (free_size > 0)
                {
                    gc_free_t *frag = (gc_free_t *)free_start;
                    frag->size = free_size;
                    frag->next = NULL;

                    if (last_free == NULL)
                    {
                        _free_list = free_start;
                    }
                    else
                    {
                        last_free->next = frag;
                    }
                    last_free = frag;

#ifdef __DEBUG
                    fprintf(__DEBUG_OUT, "free fragment at %p, size %zu words\n", (void *)free_start, free_size);
#endif
                    free_size = 0;
                }

#ifdef __DEBUG
                fprintf(__DEBUG_OUT, "keeping live object at %p, size %zu words\n", (void *)cur, obj_words);
#endif
                cur += obj_words * WORD_SIZE;
            }
            else
            {
                // Object is dead - clear allocation bit and add to free run
                _gc_heap->bits[idx] &= ~mask;

                if (free_size == 0)
                {
                    free_start = cur;
                }
                free_size += obj_words;

#ifdef __DEBUG
                fprintf(__DEBUG_OUT, "sweeping dead object at %p, size %zu words\n", (void *)cur, obj_words);
#endif
                cur += obj_words * WORD_SIZE;
            }
        }
        else
        {
            // No object here - extend free run or start new one
            if (free_size == 0)
            {
                free_start = cur;
            }
            free_size++;
            cur += WORD_SIZE;
        }
    }

    // Add final free run if any
    if (free_size > 0)
    {
        gc_free_t *frag = (gc_free_t *)free_start;
        frag->size = free_size;
        frag->next = NULL;

        if (last_free == NULL)
        {
            _free_list = free_start;
        }
        else
        {
            last_free->next = frag;
        }

#ifdef __DEBUG
        fprintf(__DEBUG_OUT, "final free fragment at %p, size %zu words\n", (void *)free_start, free_size);
#endif
    }

    // Clear mark bitmap for next collection
    memset(mark_ctx.bits, 0, mark_ctx.bitslen * Bitmap_size);
}

static inline bool check_ptr(uintptr_t ptr)
{
    // assume it is actual pointer when it is
    // not 0, is times of 8, and
    if (ptr == 0 || ptr % 8 != 0)
    {
        return false;
    }
    // in heap address space, and
    if (ptr < _gc_heap->base || ptr > _gc_heap->end)
    {
        return false;
    }
    // point to a object header
    ptr -= Header_size;
    const uintptr_t offset = PTR_HEAP_OFF(ptr);
    return (_gc_heap->bits[OFFSET_TO_IDX(offset)] & OFFSET_TO_MASK(offset));
}
