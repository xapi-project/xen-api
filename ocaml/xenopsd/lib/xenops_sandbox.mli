module Chroot : sig
  type t = private {root: string; uid: int; gid: int}

  module Path : sig
    type t

    val root : t

    val of_string : relative:string -> t

    val concat : t -> string -> t
  end

  val absolute_path_outside : t -> Path.t -> string
  (** [absolute_path_outside chroot path] returns the absolute path outside the
      chroot *)

  val chroot_path_inside : Path.t -> string
  (** [chroot_path_inside path] returns the path when inside the chroot *)

  val create_dir : within:t -> int -> Path.t -> unit
  (** [create_dir ~within perm path] Creates the directory with path [path] inside
      the chroot [within] with its owner and group ids and permissions [perm]*)

  val of_domid :
    base:string -> daemon:string -> domid:int -> vm_uuid:string -> t
  (** [of_domid ~base ~daemon ~domid ~vm_uuid] describes a chroot for specified
     daemon and domain *)

  val create :
       base:string
    -> daemon:string
    -> domid:int
    -> vm_uuid:string
    -> Path.t list
    -> t
  (** [create ~base ~daemon ~domid paths] Creates the specified chroot with
      appropriate permissions on directory [base], and ensures that all [paths]
      are owned by the chrooted daemon and rw- *)

  val destroy : t -> unit
  (** [destroy chroot] Deletes the chroot *)
end

module type SANDBOX = sig
  val prepare : domid:int -> vm_uuid:string -> Chroot.Path.t -> string
  (** [prepare ~domid ~vm_uuid path] creates an empty [path] file owned by
      [domid] inside the chroot for [domid] and returns the absolute path to it
      outside the chroot *)

  val start :
       string
    -> vm_uuid:string
    -> domid:int
    -> paths:Chroot.Path.t list
    -> Chroot.t * string
  (** [start dbg ~vm_uuid ~domid ~paths] prepares a chroot for [domid], and asks
      the guard to create a socket restricted to [vm_uuid]. Also creates
      empty files specified in [path] owned by [domid] user. *)

  val read : domid:int -> Chroot.Path.t -> vm_uuid:string -> string

  val stop : string -> domid:int -> vm_uuid:string -> unit
end

module Varstore_guard : SANDBOX
