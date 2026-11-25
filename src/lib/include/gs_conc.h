#ifndef GS_CONC_H
#define GS_CONC_H

#include <stdbool.h>
#include <stdint.h>
#include <stddef.h>

/* Traced & swept by `gc_bitmap.c` during stop-the-world collection */
void gs_conc_mark_roots(void);
void gs_conc_sweep_unreachable(void);

/* Type-specialized LLVM trampolines:
 * - `gs_thunk_runner_fn(cls_ptr)` invokes `() -> T` and returns a GC-allocated box holding `T`
 * - `gs_actor_runner_fn(state_box, cls_ptr)` invokes `S -> R` and returns a GC-allocated box holding `R`
 */
typedef void *(*gs_thunk_runner_fn)(void *cls_ptr);
typedef void *(*gs_actor_runner_fn)(void *state_box, void *cls_ptr);

/* AtomicInt (`std.sync`) */
void *gs_atomic_new(int32_t init);
int32_t gs_atomic_load(void *a);
void gs_atomic_store(void *a, int32_t val);
int32_t gs_atomic_add(void *a, int32_t delta);
int32_t gs_atomic_cas(void *a, int32_t expected, int32_t desired);

/* MVar (`std.sync`) */
void *gs_mvar_new(void *init_box, int32_t is_full);
void *gs_mvar_take(void *mvar_handle);
void gs_mvar_put(void *mvar_handle, void *val_box);
void *gs_mvar_read(void *mvar_handle);
void *gs_mvar_try_take(void *mvar_handle);

/* Chan (`std.sync`) */
void *gs_chan_new(int32_t capacity);
int32_t gs_chan_send(void *chan_handle, void *val_box);
void *gs_chan_recv(void *chan_handle);
void *gs_chan_try_recv(void *chan_handle);
void gs_chan_close(void *chan_handle);
int32_t gs_chan_is_closed(void *chan_handle);

/* Task & Parallelism (`std.task`) */
void *gs_task_spawn(gs_thunk_runner_fn runner, void *cls_ptr);
void *gs_task_await(void *task_handle);
int32_t gs_task_is_ready(void *task_handle);
void gs_task_wait_any(void *mut_arr_ptr, int32_t count, int32_t timeout_ms);
int32_t gs_conc_num_cpu(void);
void gs_conc_sleep_ms(int32_t ms);

/* Actor (`std.actor`) */
void *gs_actor_spawn(gs_thunk_runner_fn init_runner, void *init_cls);
void *gs_actor_call(void *actor_handle, gs_actor_runner_fn runner, void *handler_cls);
void gs_actor_send(void *actor_handle, gs_actor_runner_fn runner, void *handler_cls);
void gs_actor_stop(void *actor_handle);

#endif
