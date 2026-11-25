#include "../include/gs_conc.h"
#include "../include/gc.h"

#include <errno.h>
#include <pthread.h>
#include <setjmp.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>
#include <time.h>
#include <unistd.h>

/* ============================================================================
 * GC-Tracked Concurrent Object Registry
 * ============================================================================ */

typedef enum {
    CONC_KIND_MVAR = 1,
    CONC_KIND_CHAN = 2,
    CONC_KIND_TASK = 3,
    CONC_KIND_ACTOR = 4,
} gs_conc_kind_t;

typedef struct gs_conc_hdr_t {
    gs_conc_kind_t kind;
    void *gc_handle;         /* Payload pointer on GC heap holding `gs_conc_hdr_t *` */
    bool pinned_active;      /* True while a Task/Actor is actively executing */
    struct gs_conc_hdr_t *next;
    struct gs_conc_hdr_t *prev;
} gs_conc_hdr_t;

static pthread_mutex_t _reg_mu = PTHREAD_MUTEX_INITIALIZER;
static gs_conc_hdr_t *_reg_head = NULL;

static void reg_insert(gs_conc_hdr_t *node)
{
    pthread_mutex_lock(&_reg_mu);
    node->prev = NULL;
    node->next = _reg_head;
    if (_reg_head != NULL) {
        _reg_head->prev = node;
    }
    _reg_head = node;
    pthread_mutex_unlock(&_reg_mu);
}

/* Lock-order-safe condition wait:
 * Releases `mu` BEFORE `gc_leave_blocking()` so a waking thread NEVER holds an object mutex while waiting for STW!
 */
static void cond_wait_gc_safe(pthread_cond_t *cv, pthread_mutex_t *mu)
{
    jmp_buf jb;
    _setjmp(jb);
    gc_enter_blocking(&jb);
    pthread_cond_wait(cv, mu);
    pthread_mutex_unlock(mu);
    gc_leave_blocking();
    pthread_mutex_lock(mu);
}

static int cond_timedwait_gc_safe(pthread_cond_t *cv, pthread_mutex_t *mu, const struct timespec *abstime)
{
    jmp_buf jb;
    _setjmp(jb);
    gc_enter_blocking(&jb);
    int rc = pthread_cond_timedwait(cv, mu, abstime);
    pthread_mutex_unlock(mu);
    gc_leave_blocking();
    pthread_mutex_lock(mu);
    return rc;
}

/* ============================================================================
 * AtomicInt (`std.sync`)
 * ============================================================================ */

void *gs_atomic_new(int32_t init)
{
    int32_t *slot = (int32_t *)gc_alloc_atomic(sizeof(int64_t));
    __atomic_store_n(slot, init, __ATOMIC_SEQ_CST);
    return (void *)slot;
}

int32_t gs_atomic_load(void *a)
{
    return __atomic_load_n((int32_t *)a, __ATOMIC_SEQ_CST);
}

void gs_atomic_store(void *a, int32_t val)
{
    __atomic_store_n((int32_t *)a, val, __ATOMIC_SEQ_CST);
}

int32_t gs_atomic_add(void *a, int32_t delta)
{
    return __atomic_fetch_add((int32_t *)a, delta, __ATOMIC_SEQ_CST);
}

int32_t gs_atomic_cas(void *a, int32_t expected, int32_t desired)
{
    int32_t exp = expected;
    bool ok = __atomic_compare_exchange_n(
        (int32_t *)a, &exp, desired, false, __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST);
    return ok ? 1 : 0;
}

/* ============================================================================
 * MVar (`std.sync`)
 * ============================================================================ */

typedef struct {
    gs_conc_hdr_t hdr;
    pthread_mutex_t mu;
    pthread_cond_t full_cv;
    pthread_cond_t empty_cv;
    bool is_full;
    void *val_box;
} gs_mvar_t;

void *gs_mvar_new(void *init_box, int32_t is_full)
{
    volatile void *keep_box = init_box;
    void **handle = (void **)gc_alloc_atomic(sizeof(void *));
    gs_mvar_t *m = (gs_mvar_t *)calloc(1, sizeof(gs_mvar_t));
    if (m == NULL) abort();

    m->hdr.kind = CONC_KIND_MVAR;
    m->hdr.gc_handle = (void *)handle;
    m->hdr.pinned_active = false;
    pthread_mutex_init(&m->mu, NULL);
    pthread_cond_init(&m->full_cv, NULL);
    pthread_cond_init(&m->empty_cv, NULL);
    m->is_full = (is_full != 0);
    m->val_box = (is_full != 0) ? (void *)keep_box : NULL;

    *handle = m;
    reg_insert(&m->hdr);
    return (void *)handle;
}

void *gs_mvar_take(void *mvar_handle)
{
    gs_mvar_t *m = *(gs_mvar_t **)mvar_handle;
    pthread_mutex_lock(&m->mu);
    while (!m->is_full) {
        cond_wait_gc_safe(&m->full_cv, &m->mu);
    }
    void *out = m->val_box;
    m->val_box = NULL;
    m->is_full = false;
    pthread_cond_signal(&m->empty_cv);
    pthread_mutex_unlock(&m->mu);
    return out;
}

void gs_mvar_put(void *mvar_handle, void *val_box)
{
    gs_mvar_t *m = *(gs_mvar_t **)mvar_handle;
    pthread_mutex_lock(&m->mu);
    while (m->is_full) {
        cond_wait_gc_safe(&m->empty_cv, &m->mu);
    }
    m->val_box = val_box;
    m->is_full = true;
    pthread_cond_broadcast(&m->full_cv);
    pthread_mutex_unlock(&m->mu);
}

void *gs_mvar_read(void *mvar_handle)
{
    gs_mvar_t *m = *(gs_mvar_t **)mvar_handle;
    pthread_mutex_lock(&m->mu);
    while (!m->is_full) {
        cond_wait_gc_safe(&m->full_cv, &m->mu);
    }
    void *out = m->val_box;
    pthread_mutex_unlock(&m->mu);
    return out;
}

void *gs_mvar_try_take(void *mvar_handle)
{
    gs_mvar_t *m = *(gs_mvar_t **)mvar_handle;
    pthread_mutex_lock(&m->mu);
    void *out = NULL;
    if (m->is_full) {
        out = m->val_box;
        m->val_box = NULL;
        m->is_full = false;
        pthread_cond_signal(&m->empty_cv);
    }
    pthread_mutex_unlock(&m->mu);
    return out;
}

/* ============================================================================
 * Chan (`std.sync`) — Bounded & Unbounded MPMC Ring Queue
 * ============================================================================ */

typedef struct {
    gs_conc_hdr_t hdr;
    pthread_mutex_t mu;
    pthread_cond_t not_empty_cv;
    pthread_cond_t not_full_cv;
    bool bounded;
    bool closed;
    size_t max_cap;
    size_t buf_cap;
    size_t head;
    size_t len;
    void **buf;
} gs_chan_t;

void *gs_chan_new(int32_t capacity)
{
    void **handle = (void **)gc_alloc_atomic(sizeof(void *));
    gs_chan_t *ch = (gs_chan_t *)calloc(1, sizeof(gs_chan_t));
    if (ch == NULL) abort();

    ch->hdr.kind = CONC_KIND_CHAN;
    ch->hdr.gc_handle = (void *)handle;
    ch->hdr.pinned_active = false;
    pthread_mutex_init(&ch->mu, NULL);
    pthread_cond_init(&ch->not_empty_cv, NULL);
    pthread_cond_init(&ch->not_full_cv, NULL);

    if (capacity > 0) {
        ch->bounded = true;
        ch->max_cap = (size_t)capacity;
        ch->buf_cap = (size_t)capacity;
    } else {
        ch->bounded = false;
        ch->max_cap = 0;
        ch->buf_cap = 16;
    }
    ch->buf = (void **)calloc(ch->buf_cap, sizeof(void *));
    if (ch->buf == NULL) abort();

    *handle = ch;
    reg_insert(&ch->hdr);
    return (void *)handle;
}

static void chan_grow_locked(gs_chan_t *ch)
{
    size_t new_cap = ch->buf_cap * 2;
    void **nb = (void **)calloc(new_cap, sizeof(void *));
    if (nb == NULL) abort();
    for (size_t i = 0; i < ch->len; i++) {
        nb[i] = ch->buf[(ch->head + i) % ch->buf_cap];
    }
    free(ch->buf);
    ch->buf = nb;
    ch->buf_cap = new_cap;
    ch->head = 0;
}

int32_t gs_chan_send(void *chan_handle, void *val_box)
{
    gs_chan_t *ch = *(gs_chan_t **)chan_handle;
    pthread_mutex_lock(&ch->mu);
    while (!ch->closed && ch->bounded && ch->len >= ch->max_cap) {
        cond_wait_gc_safe(&ch->not_full_cv, &ch->mu);
    }
    if (ch->closed) {
        pthread_mutex_unlock(&ch->mu);
        return 0;
    }
    if (!ch->bounded && ch->len >= ch->buf_cap) {
        chan_grow_locked(ch);
    }
    size_t tail = (ch->head + ch->len) % ch->buf_cap;
    ch->buf[tail] = val_box;
    ch->len++;
    pthread_cond_signal(&ch->not_empty_cv);
    pthread_mutex_unlock(&ch->mu);
    return 1;
}

void *gs_chan_recv(void *chan_handle)
{
    gs_chan_t *ch = *(gs_chan_t **)chan_handle;
    pthread_mutex_lock(&ch->mu);
    while (ch->len == 0 && !ch->closed) {
        cond_wait_gc_safe(&ch->not_empty_cv, &ch->mu);
    }
    if (ch->len == 0) {
        pthread_mutex_unlock(&ch->mu);
        return NULL;
    }
    void *item = ch->buf[ch->head];
    ch->buf[ch->head] = NULL;
    ch->head = (ch->head + 1) % ch->buf_cap;
    ch->len--;
    if (ch->bounded) {
        pthread_cond_signal(&ch->not_full_cv);
    }
    pthread_mutex_unlock(&ch->mu);
    return item;
}

void *gs_chan_try_recv(void *chan_handle)
{
    gs_chan_t *ch = *(gs_chan_t **)chan_handle;
    pthread_mutex_lock(&ch->mu);
    if (ch->len == 0) {
        pthread_mutex_unlock(&ch->mu);
        return NULL;
    }
    void *item = ch->buf[ch->head];
    ch->buf[ch->head] = NULL;
    ch->head = (ch->head + 1) % ch->buf_cap;
    ch->len--;
    if (ch->bounded) {
        pthread_cond_signal(&ch->not_full_cv);
    }
    pthread_mutex_unlock(&ch->mu);
    return item;
}

void gs_chan_close(void *chan_handle)
{
    gs_chan_t *ch = *(gs_chan_t **)chan_handle;
    pthread_mutex_lock(&ch->mu);
    ch->closed = true;
    pthread_cond_broadcast(&ch->not_empty_cv);
    pthread_cond_broadcast(&ch->not_full_cv);
    pthread_mutex_unlock(&ch->mu);
}

int32_t gs_chan_is_closed(void *chan_handle)
{
    gs_chan_t *ch = *(gs_chan_t **)chan_handle;
    pthread_mutex_lock(&ch->mu);
    bool c = ch->closed;
    pthread_mutex_unlock(&ch->mu);
    return c ? 1 : 0;
}

/* ============================================================================
 * Work-Helping Native Thread Pool & `Task a` (`std.task`)
 * ============================================================================ */

typedef struct gs_task_t {
    gs_conc_hdr_t hdr;
    gs_thunk_runner_fn runner;
    void *cls_ptr;
    void *res_box;
    int32_t done; /* atomic: 0 = pending, 1 = completed */
    struct gs_task_t *q_next;
} gs_task_t;

static pthread_once_t _pool_once = PTHREAD_ONCE_INIT;
static pthread_mutex_t _pool_mu = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t _pool_work_cv = PTHREAD_COND_INITIALIZER;
static pthread_cond_t _task_event_cv = PTHREAD_COND_INITIALIZER;

static gs_task_t *_pool_q_head = NULL;
static gs_task_t *_pool_q_tail = NULL;

int32_t gs_conc_num_cpu(void)
{
    const char *env_t = getenv("GEISHA_THREADS");
    if (env_t != NULL) {
        int n = atoi(env_t);
        if (n >= 1 && n <= 256) return n;
    }
    long n = sysconf(_SC_NPROCESSORS_ONLN);
    if (n < 2) return 2;
    if (n > 64) return 64;
    return (int32_t)n;
}

static gs_task_t *pool_pop_locked(void)
{
    gs_task_t *t = _pool_q_head;
    if (t != NULL) {
        _pool_q_head = t->q_next;
        if (_pool_q_head == NULL) {
            _pool_q_tail = NULL;
        }
        t->q_next = NULL;
    }
    return t;
}

static void task_complete(gs_task_t *t, void *res_box)
{
    pthread_mutex_lock(&_pool_mu);
    t->res_box = res_box;
    t->cls_ptr = NULL;
    __atomic_store_n(&t->done, 1, __ATOMIC_RELEASE);
    t->hdr.pinned_active = false;
    pthread_cond_broadcast(&_task_event_cv);
    pthread_mutex_unlock(&_pool_mu);
}

static void execute_task(gs_task_t *t)
{
    void *res_box = t->runner(t->cls_ptr);
    task_complete(t, res_box);
}

static void *pool_worker_main(void *arg)
{
    (void)arg;
    volatile uintptr_t stack_anchor = 0;
    gc_thread_attach((void *)&stack_anchor);

    pthread_mutex_lock(&_pool_mu);
    for (;;) {
        gs_task_t *t = pool_pop_locked();
        if (t != NULL) {
            pthread_mutex_unlock(&_pool_mu);
            execute_task(t);
            pthread_mutex_lock(&_pool_mu);
        } else {
            cond_wait_gc_safe(&_pool_work_cv, &_pool_mu);
        }
    }
    pthread_mutex_unlock(&_pool_mu);
    gc_thread_detach();
    return NULL;
}

static void pool_init_once(void)
{
    int32_t n = gs_conc_num_cpu();
    for (int32_t i = 0; i < n; i++) {
        pthread_t tid;
        pthread_attr_t attr;
        pthread_attr_init(&attr);
        pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
        if (pthread_create(&tid, &attr, pool_worker_main, NULL) != 0) {
            fprintf(stderr, "Failed to spawn Geisha worker thread\n");
            abort();
        }
        pthread_attr_destroy(&attr);
    }
}

static gs_task_t *alloc_empty_task_with_handle(void **out_handle)
{
    void **handle = (void **)gc_alloc_atomic(sizeof(void *));
    gs_task_t *t = (gs_task_t *)calloc(1, sizeof(gs_task_t));
    if (t == NULL) abort();

    t->hdr.kind = CONC_KIND_TASK;
    t->hdr.gc_handle = (void *)handle;
    t->hdr.pinned_active = true;
    t->done = 0;

    *handle = t;
    reg_insert(&t->hdr);
    *out_handle = (void *)handle;
    return t;
}

void *gs_task_spawn(gs_thunk_runner_fn runner, void *cls_ptr)
{
    pthread_once(&_pool_once, pool_init_once);

    volatile void *keep_cls = cls_ptr;
    void *handle = NULL;
    gs_task_t *t = alloc_empty_task_with_handle(&handle);
    t->runner = runner;
    t->cls_ptr = (void *)keep_cls;

    pthread_mutex_lock(&_pool_mu);
    if (_pool_q_tail == NULL) {
        _pool_q_head = t;
        _pool_q_tail = t;
    } else {
        _pool_q_tail->q_next = t;
        _pool_q_tail = t;
    }
    pthread_cond_signal(&_pool_work_cv);
    /* Also wake any `await` callers so they can work-help if all pool threads are busy in nested awaits! */
    pthread_cond_broadcast(&_task_event_cv);
    pthread_mutex_unlock(&_pool_mu);

    return handle;
}

void *gs_task_await(void *task_handle)
{
    gs_task_t *t = *(gs_task_t **)task_handle;
    if (__atomic_load_n(&t->done, __ATOMIC_ACQUIRE) != 0) {
        return t->res_box;
    }

    pthread_mutex_lock(&_pool_mu);
    while (__atomic_load_n(&t->done, __ATOMIC_ACQUIRE) == 0) {
        /* Inline work-helping: dequeue and run pending pool tasks while waiting! */
        gs_task_t *other = pool_pop_locked();
        if (other != NULL) {
            pthread_mutex_unlock(&_pool_mu);
            execute_task(other);
            pthread_mutex_lock(&_pool_mu);
        } else {
            cond_wait_gc_safe(&_task_event_cv, &_pool_mu);
        }
    }
    void *res = t->res_box;
    pthread_mutex_unlock(&_pool_mu);
    return res;
}

int32_t gs_task_is_ready(void *task_handle)
{
    gs_task_t *t = *(gs_task_t **)task_handle;
    return __atomic_load_n(&t->done, __ATOMIC_ACQUIRE) != 0 ? 1 : 0;
}

void gs_task_wait_any(void *mut_arr_ptr, int32_t count, int32_t timeout_ms)
{
    int64_t len64 = *(int64_t *)mut_arr_ptr;
    void **handles = (void **)((uintptr_t)mut_arr_ptr + 8);
    int32_t n = (int32_t)len64;
    if (n <= 0 || count <= 0) {
        return;
    }
    int32_t target = (count < n) ? count : n;

    struct timespec deadline;
    bool has_deadline = (timeout_ms >= 0);
    if (has_deadline) {
        struct timeval tv;
        gettimeofday(&tv, NULL);
        int64_t total_us = (int64_t)tv.tv_usec + (int64_t)timeout_ms * 1000LL;
        deadline.tv_sec = tv.tv_sec + (time_t)(total_us / 1000000LL);
        deadline.tv_nsec = (long)((total_us % 1000000LL) * 1000LL);
    }

    pthread_mutex_lock(&_pool_mu);
    for (;;) {
        int32_t ready_cnt = 0;
        for (int32_t i = 0; i < n; i++) {
            gs_task_t *ti = *(gs_task_t **)(handles[i]);
            if (__atomic_load_n(&ti->done, __ATOMIC_ACQUIRE) != 0) {
                ready_cnt++;
            }
        }
        if (ready_cnt >= target || (has_deadline && timeout_ms == 0)) {
            break;
        }
        if (!has_deadline) {
            gs_task_t *other = pool_pop_locked();
            if (other != NULL) {
                pthread_mutex_unlock(&_pool_mu);
                execute_task(other);
                pthread_mutex_lock(&_pool_mu);
                continue;
            }
            cond_wait_gc_safe(&_task_event_cv, &_pool_mu);
        } else {
            int rc = cond_timedwait_gc_safe(&_task_event_cv, &_pool_mu, &deadline);
            if (rc == ETIMEDOUT) {
                break;
            }
        }
    }
    pthread_mutex_unlock(&_pool_mu);
}

void gs_conc_sleep_ms(int32_t ms)
{
    if (ms <= 0) return;
    struct timespec req;
    req.tv_sec = ms / 1000;
    req.tv_nsec = (long)(ms % 1000) * 1000000L;

    jmp_buf jb;
    _setjmp(jb);
    gc_enter_blocking(&jb);
    nanosleep(&req, NULL);
    gc_leave_blocking();
}

/* ============================================================================
 * Stateful Thread-Pinned Actors (`std.actor`)
 * ============================================================================ */

typedef struct gs_actor_msg_t {
    gs_actor_runner_fn runner;
    void *handler_cls;
    gs_task_t *reply_task; /* NULL for `actorSend` fire-and-forget */
    struct gs_actor_msg_t *next;
} gs_actor_msg_t;

typedef struct {
    gs_conc_hdr_t hdr;
    pthread_t tid;
    pthread_mutex_t mu;
    pthread_cond_t cv;
    gs_thunk_runner_fn init_runner;
    void *init_cls;
    void *state_box;
    gs_actor_msg_t *q_head;
    gs_actor_msg_t *q_tail;
    bool stop_requested;
    bool joined;
} gs_actor_t;

static void *actor_thread_main(void *arg)
{
    gs_actor_t *a = (gs_actor_t *)arg;
    volatile uintptr_t stack_anchor = 0;
    gc_thread_attach((void *)&stack_anchor);

    /* Initialize state `s` IN-PLACE on the actor's dedicated OS thread! */
    void *st_box = a->init_runner(a->init_cls);
    pthread_mutex_lock(&a->mu);
    a->state_box = st_box;
    a->init_cls = NULL;

    for (;;) {
        gs_actor_msg_t *msg = a->q_head;
        if (msg != NULL) {
            a->q_head = msg->next;
            if (a->q_head == NULL) {
                a->q_tail = NULL;
            }
            pthread_mutex_unlock(&a->mu);

            void *res_box = msg->runner(a->state_box, msg->handler_cls);
            if (msg->reply_task != NULL) {
                task_complete(msg->reply_task, res_box);
            }
            free(msg);

            pthread_mutex_lock(&a->mu);
        } else if (a->stop_requested) {
            break;
        } else {
            cond_wait_gc_safe(&a->cv, &a->mu);
        }
    }

    a->hdr.pinned_active = false;
    pthread_mutex_unlock(&a->mu);
    gc_thread_detach();
    return NULL;
}

void *gs_actor_spawn(gs_thunk_runner_fn init_runner, void *init_cls)
{
    volatile void *keep_cls = init_cls;
    void **handle = (void **)gc_alloc_atomic(sizeof(void *));
    gs_actor_t *a = (gs_actor_t *)calloc(1, sizeof(gs_actor_t));
    if (a == NULL) abort();

    a->hdr.kind = CONC_KIND_ACTOR;
    a->hdr.gc_handle = (void *)handle;
    a->hdr.pinned_active = true;
    pthread_mutex_init(&a->mu, NULL);
    pthread_cond_init(&a->cv, NULL);
    a->init_runner = init_runner;
    a->init_cls = (void *)keep_cls;

    *handle = a;
    reg_insert(&a->hdr);

    if (pthread_create(&a->tid, NULL, actor_thread_main, a) != 0) {
        fprintf(stderr, "Failed to spawn Geisha actor thread\n");
        abort();
    }
    return (void *)handle;
}

void *gs_actor_call(void *actor_handle, gs_actor_runner_fn runner, void *handler_cls)
{
    volatile void *keep_actor = actor_handle;
    volatile void *keep_cls = handler_cls;

    void *task_handle = NULL;
    gs_task_t *reply = alloc_empty_task_with_handle(&task_handle);

    gs_actor_t *a = *(gs_actor_t **)keep_actor;
    gs_actor_msg_t *msg = (gs_actor_msg_t *)calloc(1, sizeof(gs_actor_msg_t));
    if (msg == NULL) abort();
    msg->runner = runner;
    msg->handler_cls = (void *)keep_cls;
    msg->reply_task = reply;

    pthread_mutex_lock(&a->mu);
    if (a->q_tail == NULL) {
        a->q_head = msg;
        a->q_tail = msg;
    } else {
        a->q_tail->next = msg;
        a->q_tail = msg;
    }
    pthread_cond_signal(&a->cv);
    pthread_mutex_unlock(&a->mu);

    return task_handle;
}

void gs_actor_send(void *actor_handle, gs_actor_runner_fn runner, void *handler_cls)
{
    gs_actor_t *a = *(gs_actor_t **)actor_handle;
    gs_actor_msg_t *msg = (gs_actor_msg_t *)calloc(1, sizeof(gs_actor_msg_t));
    if (msg == NULL) abort();
    msg->runner = runner;
    msg->handler_cls = handler_cls;
    msg->reply_task = NULL;

    pthread_mutex_lock(&a->mu);
    if (a->q_tail == NULL) {
        a->q_head = msg;
        a->q_tail = msg;
    } else {
        a->q_tail->next = msg;
        a->q_tail = msg;
    }
    pthread_cond_signal(&a->cv);
    pthread_mutex_unlock(&a->mu);
}

void gs_actor_stop(void *actor_handle)
{
    gs_actor_t *a = *(gs_actor_t **)actor_handle;
    pthread_mutex_lock(&a->mu);
    if (a->joined) {
        pthread_mutex_unlock(&a->mu);
        return;
    }
    a->stop_requested = true;
    a->joined = true;
    pthread_cond_broadcast(&a->cv);
    pthread_t tid = a->tid;
    pthread_mutex_unlock(&a->mu);

    jmp_buf jb;
    _setjmp(jb);
    gc_enter_blocking(&jb);
    pthread_join(tid, NULL);
    gc_leave_blocking();
}

/* ============================================================================
 * Stop-The-World GC Root Tracing & Unreachable Handle Cleanup
 * (Called exclusively during STW when all mutator threads are parked!)
 * ============================================================================ */

void gs_conc_mark_roots(void)
{
    bool progress = true;
    while (progress) {
        progress = false;
        for (gs_conc_hdr_t *cur = _reg_head; cur != NULL; cur = cur->next) {
            if (cur->pinned_active) {
                if (gc_mark_ptr(cur->gc_handle)) {
                    progress = true;
                }
            }
            if (!cur->pinned_active && !gc_is_obj_marked(cur->gc_handle)) {
                continue;
            }

            switch (cur->kind) {
            case CONC_KIND_MVAR: {
                gs_mvar_t *m = (gs_mvar_t *)cur;
                if (m->is_full && m->val_box != NULL) {
                    if (gc_mark_ptr(m->val_box)) progress = true;
                }
                break;
            }
            case CONC_KIND_CHAN: {
                gs_chan_t *ch = (gs_chan_t *)cur;
                for (size_t i = 0; i < ch->len; i++) {
                    void *item = ch->buf[(ch->head + i) % ch->buf_cap];
                    if (item != NULL && gc_mark_ptr(item)) {
                        progress = true;
                    }
                }
                break;
            }
            case CONC_KIND_TASK: {
                gs_task_t *t = (gs_task_t *)cur;
                if (t->cls_ptr != NULL && gc_mark_ptr(t->cls_ptr)) progress = true;
                if (t->res_box != NULL && gc_mark_ptr(t->res_box)) progress = true;
                break;
            }
            case CONC_KIND_ACTOR: {
                gs_actor_t *a = (gs_actor_t *)cur;
                if (a->init_cls != NULL && gc_mark_ptr(a->init_cls)) progress = true;
                if (a->state_box != NULL && gc_mark_ptr(a->state_box)) progress = true;
                for (gs_actor_msg_t *msg = a->q_head; msg != NULL; msg = msg->next) {
                    if (msg->handler_cls != NULL && gc_mark_ptr(msg->handler_cls)) {
                        progress = true;
                    }
                    if (msg->reply_task != NULL && gc_mark_ptr(msg->reply_task->hdr.gc_handle)) {
                        progress = true;
                    }
                }
                break;
            }
            }
        }
    }
}

void gs_conc_sweep_unreachable(void)
{
    gs_conc_hdr_t *cur = _reg_head;
    while (cur != NULL) {
        gs_conc_hdr_t *next = cur->next;
        if (!cur->pinned_active && !gc_is_obj_marked(cur->gc_handle)) {
            if (cur->prev != NULL) {
                cur->prev->next = cur->next;
            } else {
                _reg_head = cur->next;
            }
            if (cur->next != NULL) {
                cur->next->prev = cur->prev;
            }

            switch (cur->kind) {
            case CONC_KIND_MVAR: {
                gs_mvar_t *m = (gs_mvar_t *)cur;
                pthread_mutex_destroy(&m->mu);
                pthread_cond_destroy(&m->full_cv);
                pthread_cond_destroy(&m->empty_cv);
                free(m);
                break;
            }
            case CONC_KIND_CHAN: {
                gs_chan_t *ch = (gs_chan_t *)cur;
                pthread_mutex_destroy(&ch->mu);
                pthread_cond_destroy(&ch->not_empty_cv);
                pthread_cond_destroy(&ch->not_full_cv);
                free(ch->buf);
                free(ch);
                break;
            }
            case CONC_KIND_TASK: {
                gs_task_t *t = (gs_task_t *)cur;
                free(t);
                break;
            }
            case CONC_KIND_ACTOR: {
                gs_actor_t *a = (gs_actor_t *)cur;
                if (!a->joined) {
                    pthread_detach(a->tid);
                }
                pthread_mutex_destroy(&a->mu);
                pthread_cond_destroy(&a->cv);
                free(a);
                break;
            }
            }
        }
        cur = next;
    }
}
