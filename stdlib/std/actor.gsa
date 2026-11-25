// Thread-pinned sequential actors (`std.actor`):
// Encapsulate `!Send` local mutable state (`Cell`, `MutArray`, `Vec`, `StrMap`, `Ptr`)
// behind a sequential mailbox on a dedicated OS thread.

use std.task.{Task, await}

pub def actorSpawn(init: () -> s): Actor s = __concActorSpawn(init)

pub def actorCall(actor: Actor s, handler: s -> r): Task r =
    __concActorCall(actor, handler)

pub def actorAsk(actor: Actor s, handler: s -> r): r =
    await(__concActorCall(actor, handler))

pub def actorSend(actor: Actor s, handler: s -> Void): Void =
    __concActorSend(actor, handler)

pub def actorStop(actor: Actor s): Void = __concActorStop(actor)
