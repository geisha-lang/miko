#include "gc_bitmap.h"

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
    // _gc_heaps = (gc_heap_t *)malloc(sizeof(gc_heap_t));
    // _gc_heaps->heap = new_heap(init_size_words);
    // if (_gc_heaps == NULL)
    // {
    //     fprintf(stderr, "Failed allocate heap\n");
    //     abort(); // init heap failed, all hope lost
    // }
    // _gc_heaps->next = NULL;

    _gc_heap = new_heap(init_size_words);
    mark_ctx.bitslen = _gc_heap->bitslen;
    mark_ctx.bits = (bitmap_t *)calloc(_gc_heap->bitslen, Bitmap_size);

    if (_gc_heap == NULL || mark_ctx.bits == NULL)
    {
        fprintf(stderr, "Initial allocate failed\n");
        abort(); // init heap failed, all hope lost
    }

    _free_list = _gc_heap->base;
    Set_free_size(_free_list, init_size_words);
    Set_free_next(_free_list, NULL);

    // get frame address of main
    _root_start = __builtin_frame_address(1);
#ifdef __DEBUG
    fprintf(__DEBUG_OUT, "Root start at current main frame address: %p\n", _root_start);
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
            abort(); // TODO: extend new heap
        }
    }
    bitmap_set_alloc(ret, words);
    PTR_INC_BYTES(ret, Header_size);
    #ifdef __DEBUG
    
        fprintf(__DEBUG_OUT, "allocated object at %p\n", ret);
    
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

    INC_PTR_BYTES(base, sizeof(gc_heap_t));

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
    _root_end = __builtin_frame_address(1);

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
    PTR_INC_BYTES(obj, obj_size * WORD_SIZE);
    while ((uintptr_t)cur < (uintptr_t)obj)
    {
        void *field = *(void **)cur;
        if (check_ptr((uintptr_t)field))
        {
            bitmap_mark_obj(field);
        }
        PTR_INC_BYTES(cur, 4); // increase by struct padding size
    }
}

void bitmap_sweep()
{
    
}

static inline bool check_ptr(uintptr_t ptr)
{
    // assume it is actual pointer when it is
    // not 0, is times of 8, and
    if (ptr == 0 && ptr % 8 != 0)
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
