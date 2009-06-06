(** Represents the current "lock" *)
type token 

(** Calls a provided function with the VM locked. The abstract locking token is provided
    so the caller can call 'assert_locked' to make sure they don't perform side-effects outside
    the locked region. *)
val with_lock : API.ref_VM -> (token -> 'a -> 'b) -> 'a -> 'b

(** Raised by a call to 'assert_locked' if the VM is not locked by the provided token *)
exception Lock_not_held

(** Can be called to ensure we are still in the region protected by the lock and haven't accidentally
    dropped outside, eg by an accidental partial function application. *)
val assert_locked : API.ref_VM -> token -> unit

