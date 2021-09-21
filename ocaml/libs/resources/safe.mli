(** Trying to call [borrow_exn] after [move] or [release] has already been
    performed. *)
exception UseAfterMoveOrRelease

(** A type with a [release] operation that can be called at most once. Also
    called an affine type. *)
type 'a t

val create :
  ?on_finalise_leaked:('a -> unit) -> release:('a -> unit) -> 'a -> 'a t
(** [create ?on_finalise_leaked ~release v] wraps [v] and a [release] operation.
    The underlying value can be accessed with [borrow_exn], and the resource can
    be safely released by calling [safe_release] below. [on_finalise_leaked] is
    used internally in Unixfd to attach a finaliser that detects unreleased
    ['a t] values. *)

val borrow_exn : 'a t -> 'a
(** [borrow_exn t] returns the underlying value, but raises
    [UseAfterMoveRelease] if [t] has been [move]d or [release]d. An exception
    raised here indicates a bug in the calling code. The borrowed value should
    be used in the smallest possible scope, and shouldn't outlive the ['a t]
    value itself. A borrowed value should never be stored in another
    tuple/record/data structure. This rule cannot be checked at compile-time or
    run-time. Typically the returned value should be given directly to a
    lower-level routine that operates on the resource directly, but doesn't
    otherwise release it itself. *)

val move_exn : 'a t -> 'a t
(** [move_exn t] creates a shallow copy of [t]. Any further operation on [t]
    raises [UseAfterMoveOrRelease]. This is useful for moving a value from a
    local scope to a global cache: the cleanup performed on the local scope
    shouldn't release the resource, and moving it accomplishes that. *)

val safe_release : 'a t -> unit
(** [release t] executes [release] on the underlying value of [t] at most once.
    The original [t] drops any references to its underlying value.

    Any further [borrow_*] operation on [t] will fail. Any further [release]
    operation on [t] will be a no-op. Exceptions raised by [release] are
    passed-through.

    This can be used if you want to release a resource early, most of the time
    it is recommended to use [within] instead if possible. *)

val within : 'a t -> ('a t -> 'b) -> 'b
(** [within t f] executes [f] with [t] and then releases [t] on all successful
    and exceptional codepaths. *)
