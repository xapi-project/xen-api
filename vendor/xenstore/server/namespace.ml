
module type IO = sig
	val exists: Transaction.t -> Perms.t -> Store.Path.t -> bool
	val mkdir: ?with_watch:bool -> Transaction.t -> int -> Perms.t -> Store.Path.t -> unit
	val read: Transaction.t -> Perms.t -> Store.Path.t -> string
	val write: Transaction.t -> int -> Perms.t -> Store.Path.t -> string -> unit
	val list: Transaction.t -> Perms.t -> Store.Path.t -> string list
	val rm: Transaction.t -> Perms.t -> Store.Path.t -> unit
	val getperms: Transaction.t -> Perms.t -> Store.Path.t -> Xs_protocol.ACL.t
	val setperms: Transaction.t -> Perms.t -> Store.Path.t -> Xs_protocol.ACL.t -> unit
end

exception Unsupported

module Unsupported = struct
	let exists _ _ _ = raise Unsupported
	let mkdir ?with_watch:_ _ _ _ _ = raise Unsupported
	let read _ _ _ = raise Unsupported
	let write _ _ _ _ _ = raise Unsupported
	let list _ _ _ = raise Unsupported
	let rm _ _ _ = raise Unsupported
	let getperms _ _ _ = raise Unsupported
	let setperms _ _ _ _ = raise Unsupported
end
