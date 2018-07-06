open Idl
open Rpc
open Types

(* Common parameters *)
let dbg = Param.mk ~name:"dbg"
    ~description:["Debug context from the caller"] Types.string

let unit = Param.mk Types.unit

let task_id = Param.mk ~name:"task_id"
    ~description:["Opaque string representing the task associated with the";
                  "operation."]
    Types.string



type uri = string [@@deriving rpcty]
(** A URI representing the means for accessing the volume data. The
    interpretation of the URI is specific to the implementation. Xapi will
    choose which implementation to use based on the URI scheme. *)

(** A list of blocks for copying *)
type blocklist = {
  blocksize : int [@version (1,1,0)]; (** Size of the individual blocks *)
  ranges : (int64 * int64) list       (** list of block ranges, where a range is
                                          a (start,length) pair,measured in units
                                          of [blocksize] *)
} [@@deriving rpcty]


type error = Unimplemented of string | UnexpectedError of string [@@deriving rpcty]

module E = Idl.Error.Make(struct
    type t = error
    let t = error
    let internal_error_of e = Some (UnexpectedError (Printexc.to_string e))
    end)
let error = E.error

(** A string representing a Xen domain on the local host. The string is
    guaranteed to be unique per-domain but it is not guaranteed to take any
    particular form. It may (for example) be a Xen domain id, a Xen VM uuid
    or a Xenstore path or anything else chosen by the toolstack.
    Implementations should not assume the string has any meaning. *)
type domain = string [@@deriving rpcty]

(** The choice of blkback to use. *)
type implementation =
  | Blkback of string  (** Use kernel blkback with the given 'params' key *)
  | Qdisk of string    (** Use userspace qemu qdisk with the given 'params' key *)
  | Tapdisk3 of string (** Use userspace tapdisk3 with the given 'params' key *)
[@@deriving rpcty]

(** A description of which Xen block backend to use. The toolstack needs this
    to setup the shared memory connection to blkfront in the VM. *)
type backend = {
  domain_uuid: string;           (** UUID of the domain hosting the backend *)
  implementation: implementation (** choice of implementation technology *)
}
[@@deriving rpcty]

(** True means the disk data is persistent and should be preserved when the
    datapath is closed i.e. when a VM is shutdown or rebooted. False means the
    data should be thrown away when the VM is shutdown or rebooted. *)
type persistent = bool [@@deriving rpcty]


(* Create some handy parameters for use in the function definitions below *)
let uri_p = Param.mk
    ~description:["A URI which represents how to access the volume disk data."]
    uri
let persistent = Param.mk persistent
let domain = Param.mk
    ~description:["An opaque string which represents the Xen domain."]
    domain
let backend = Param.mk backend

open Idl

module Datapath(R: RPC) = struct
  open R

  let implementation = R.implement Idl.Interface.{
      name = "Datapath";
      namespace = None;
      description = [
        "Xapi will call the functions here on VM start / shutdown / suspend";
        "/ resume / migrate. Every function is idempotent. Every function";
        "takes a domain parameter which allows the implementation to track";
        "how many domains are currently using the volume."];
      version=(1,0,0)
    }

  let open_ =
    declare "open" [
      "[open uri persistent] is called before a disk is attached to a VM.";
      "If persistent is true then care should be taken to persist all writes";
      "to the disk. If persistent is false then the implementation should";
      "configure a temporary location for writes so they can be thrown away";
      "on [close]."]
      (uri_p @-> persistent @-> returning unit error)

  let attach =
    declare "attach" [
      "[attach uri domain] prepares a connection between the storage named by";
      "[uri] and the Xen domain with id [domain]. The return value is the";
      "information needed by the Xen toolstack to setup the shared-memory";
      "blkfront protocol. Note that the same volume may be simultaneously";
      "attached to multiple hosts for example over a migrate. If an";
      "implementation needs to perform an explicit handover, then it should";
      "implement [activate] and [deactivate]. This function is idempotent."]
      (uri_p @-> domain @-> returning backend error)

  let activate =
    declare "activate" [
      "[activate uri domain] is called just before a VM needs to read or write";
      "its disk. This is an opportunity for an implementation which needs to ";
      "perform an explicit volume handover to do it. This function is called";
      "in the migration downtime window so delays here will be noticeable to";
      "users and should be minimised. This function is idempotent."]
      (uri_p @-> domain @-> returning unit error)

  let deactivate =
    declare "deactivate" [
      "[deactivate uri domain] is called as soon as a VM has finished reading";
      "or writing its disk. This is an opportunity for an implementation which";
      "needs to perform an explicit volume handover to do it. This function is";
      "called in the migration downtime window so delays here will be";
      "noticeable to users and should be minimised. This function is";
      "idempotent."]
      (uri_p @-> domain @-> returning unit error)

  let detach =
    declare "detach" [
      "[detach uri domain] is called sometime after a VM has finished reading";
      "or writing its disk. This is an opportunity to clean up any resources";
      "associated with the disk. This function is called outside the migration";
      "downtime window so can be slow without affecting users. This function";
      "is idempotent. This function should never fail. If an implementation is";
      "unable to perform some cleanup right away then it should queue the";
      "action internally. Any error result represents a bug in the";
      "implementation."]
      (uri_p @-> domain @-> returning unit error)

  let close =
    declare "close" [
      "[close uri] is called after a disk is detached and a VM shutdown. This";
      "is an opportunity to throw away writes if the disk is not persistent."]
      (uri_p @-> returning unit error)

end


module Data (R : RPC) = struct
  open R

  let implementation = implement Idl.Interface.{
      name = "Data";
      namespace = Some "Data";
      description = [
        "This interface is used for long-running data operations such as";
        "copying the contents of volumes or mirroring volumes to remote";
        "destinations"];
      version=(1,0,0)
    }

  type operation =
    | Copy of uri * uri
                [@doc [
                  "Copy (src,dst) represents an on-going copy operation from";
                  "the [src] URI to the [dst] URI"]]
    | Mirror of uri * uri
                  [@doc [
                    "Mirror (src,dst) represents an on-going mirror operation";
                    "from the [src] URI to the [dst] URI"]]
  [@@deriving rpcty]
  [@@doc ["The primary key for referring to a long-running operation"]]

  type operations = operation list
  [@@deriving rpcty]
  [@@doc ["A list of operations"]]

  type status = {
    failed : bool
        [@doc [
          "[failed] will be set to true if the operation has failed for some";
          "reason"]];
    progress : float option
        [@doc [
          "[progress] will be returned for a copy operation, and ranges";
          "between 0 and 1"]];
  }
  [@@deriving rpcty]
  [@@doc ["Status information for on-going tasks"]]

  let remote = Param.mk ~name:"remote"
      ~description:["A URI which represents how to access a remote volume";
                    "disk data."] uri

  let operation = Param.mk operation

  let blocklist = Param.mk blocklist

  let copy = declare "copy" [
      "[copy uri domain remote blocks] copies [blocks] from the local disk to";
      "a remote URI. This may be called as part of a Volume Mirroring";
      "operation, and hence may need to cooperate with whatever process is";
      "currently mirroring writes to ensure data integrity is maintained"]
      (uri_p @-> domain @-> remote @-> blocklist @-> returning operation error)

  let mirror = declare "mirror" [
      "[mirror uri domain remote] starts mirroring new writes to the volume to";
      "a remote URI (usually NBD). This is called as part of a volume";
      "mirroring process"]
      (uri_p @-> domain @-> remote @-> returning operation error)

  let status = Param.mk status
  let stat = declare "stat" [
      "[stat operation] returns the current status of [operation]. For a copy";
      "operation, this will contain progress information."]
      (operation @-> returning status error)

  let cancel = declare "cancel" [
      "[cancel operation] cancels a long-running operation. Note that the call";
      "may return before the operation has finished."]
      (operation @-> returning unit error)

  let destroy = declare "destroy" [
      "[destroy operation] destroys the information about a long-running";
      "operation. This should fail when run against an operation that is still";
      "in progress."]
      (operation @-> returning unit error)

  let operations = Param.mk operations
  let ls = declare "ls"
      ["[ls] returns a list of all current operations"]
      (unit @-> returning operations error)

end
