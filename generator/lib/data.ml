open Rpc
open Idl
open Common

type domain = string
[@@deriving rpcty]
[@@doc [
  "A string representing a Xen domain on the local host. The string is";
  "guaranteed to be unique per-domain but it is not guaranteed to take";
  "any particular form. It may (for example) be a Xen domain id, a Xen";
  "VM uuid or a Xenstore path or anything else chosen by the toolstack.";
  "Implementations should not assume the string has any meaning."]]

type implementation =
  | Blkback of string
        [@doc ["use kernel blkback with the given 'params' key"]]
  | Qdisk of string
        [@doc ["use userspace qemu qdisk with the given 'params' key"]]
  | Tapdisk3 of string
        [@doc ["use userspace tapdisk3 with the given 'params' key"]]
[@@doc ["The choice of blkback to use."]]
[@@deriving rpcty]

type backend = {
  domain_uuid: string
      [@doc ["UUID of the domain hosting the backend"]];
  implementation: implementation
      [@doc ["choice of implementation technology"]];
}
[@@doc [
  "A description of which Xen block backend to use. The toolstack needs";
  "this to setup the shared memory connection to blkfront in the VM."]]
[@@deriving rpcty]

type persistent = bool
[@@doc [
  "True means the disk data is persistent and should be preserved when";
  "the datapath is closed i.e. when a VM is shutdown or rebooted. False";
  "means the data should be thrown away when the VM is shutdown or";
  "rebooted."]]
[@@deriving rpcty]

(* Create some handy parameters for use in the function definitions below *)
let uri_p = Param.mk
    ~name:"uri"
    ~description:["A URI which represents how to access the volume disk data."]
    Common.uri

let persistent = Param.mk (* Inherit the description from the type *)
    ~name:"persistent"
    persistent

let domain = Param.mk
    ~name:"domain"
    ~description:["An opaque string which represents the Xen domain."]
    domain

let backend = Param.mk (* Inherit the description from the type *)
    ~name:"backend"
    backend

open Idl

module Datapath(R: RPC) = struct
  open R

  let open_ =
    declare "open" [
      "[open uri persistent] is called before a disk is attached to a VM.";
      "If persistent is true then care should be taken to persist all writes";
      "to the disk. If persistent is false then the implementation should";
      "configure a temporary location for writes so they can be thrown away";
      "on [close]."]
      (dbg @-> uri_p @-> persistent @-> returning unit error)

  let attach =
    declare "attach" [
      "[attach uri domain] prepares a connection between the storage named by";
      "[uri] and the Xen domain with id [domain]. The return value is the";
      "information needed by the Xen toolstack to setup the shared-memory";
      "blkfront protocol. Note that the same volume may be simultaneously";
      "attached to multiple hosts for example over a migrate. If an";
      "implementation needs to perform an explicit handover, then it should";
      "implement [activate] and [deactivate]. This function is idempotent."]
      (dbg @-> uri_p @-> domain @-> returning backend error)

  let activate =
    declare "activate" [
      "[activate uri domain] is called just before a VM needs to read or write";
      "its disk. This is an opportunity for an implementation which needs to";
      "perform an explicit volume handover to do it. This function is called";
      "in the migration downtime window so delays here will be noticeable to";
      "users and should be minimised. This function is idempotent."]
      (dbg @-> uri_p @-> domain @-> returning unit error)

  let deactivate =
    declare "deactivate" [
      "[deactivate uri domain] is called as soon as a VM has finished reading";
      "or writing its disk. This is an opportunity for an implementation which";
      "needs to perform an explicit volume handover to do it. This function is";
      "called in the migration downtime window so delays here will be";
      "noticeable to users and should be minimised. This function is idempotent."]
      (dbg @-> uri_p @-> domain @-> returning unit error)

  let detach =
    declare "detach" [
      "[detach uri domain] is called sometime after a VM has finished reading";
      "or writing its disk. This is an opportunity to clean up any resources";
      "associated with the disk. This function is called outside the migration";
      "downtime window so can be slow without affecting users. This function is";
      "idempotent. This function should never fail. If an implementation is";
      "unable to perform some cleanup right away then it should queue the";
      "action internally. Any error result represents a bug in the";
      "implementation."]
      (dbg @-> uri_p @-> domain @-> returning unit error)

  let close =
    declare "close" [
      "[close uri] is called after a disk is detached and a VM shutdown. This";
      "is an opportunity to throw away writes if the disk is not persistent."]
      (dbg @-> uri_p @-> returning unit error)

  let implementation = R.implement Idl.Interface.{
      name = "Datapath";
      namespace = Some "Datapath";
      description = [
        "Xapi will call the functions here on VM start / shutdown /";
        "suspend / resume / migrate. Every function is idempotent. Every";
        "function takes a domain parameter which allows the implementation";
        "to track how many domains are currently using the volume." ];
      version=(1,0,0);
    }

end


module Data (R : RPC) = struct
  open R

  type operation =
    | Copy of uri * uri
                [@doc ["Copy (src,dst) represents an on-going copy operation";
                       "from the [src] URI to the [dst] URI"]]
    | Mirror of uri * uri
                  [@doc ["Mirror (src,dst) represents an on-going mirror ";
                         "operation from the [src] URI to the [dst] URI"]]
  [@@doc ["The primary key for referring to a long-running operation"]]
  [@@deriving rpcty]

  type operations = operation list [@@deriving rpcty]
  [@@doc ["A list of operations"]]

  type status = {
    failed : bool [@doc
        ["[failed] will be set to true if the operation has failed for some ";
         "reason"]];

    progress : float option [@doc
        ["[progress] will be returned for a copy operation, and ranges ";
         "between 0 and 1"]];
  } [@@deriving rpcty] [@@doc ["Status information for on-going tasks"]]

  let remote = Param.mk ~name:"remote" ~description:
      ["A URI which represents how to access a remote volume disk data."]
      uri

  let operation = Param.mk ~name:"operation" operation

  let blocklist = Param.mk ~name:"blocklist" blocklist

  let copy = declare "copy"
      ["[copy uri domain remote blocks] copies [blocks] from the local disk ";
       "to a remote URI. This may be called as part of a Volume Mirroring ";
       "operation, and hence may need to cooperate with whatever process is ";
       "currently mirroring writes to ensure data integrity is maintained"]
      (dbg @-> uri_p @-> domain @-> remote @-> blocklist @-> returning operation error)

  let mirror = declare "mirror"
      ["[mirror uri domain remote] starts mirroring new writes to the volume ";
       "to a remote URI (usually NBD). This is called as part of a volume ";
       "mirroring process"]
      (dbg @-> uri_p @-> domain @-> remote @-> returning operation error)

  let status = Param.mk status
  let stat = declare "stat"
      ["[stat operation] returns the current status of [operation]. For a ";
       "copy operation, this will contain progress information."]
      (dbg @-> operation @-> returning status error)

  let cancel = declare "cancel"
      ["[cancel operation] cancels a long-running operation. Note that the ";
       "call may return before the operation has finished."]
      (dbg @-> operation @-> returning unit error)

  let destroy = declare "destroy"
      ["[destroy operation] destroys the information about a long-running ";
       "operation. This should fail when run against an operation that is ";
       "still in progress."]
      (dbg @-> operation @-> returning unit error)

  let operations = Param.mk operations
  let ls = declare "ls"
      ["[ls] returns a list of all current operations"]
      (dbg @-> unit @-> returning operations error)

  let implementation = implement Idl.Interface.{
      name = "Data";
      namespace = Some "Data";
      description = [
        "This interface is used for long-running data operations such as";
        "copying the contents of volumes or mirroring volumes to remote";
        "destinations"];
      version = (1,0,0)
    }

end

module DPCode = Datapath(Codegen.Gen ())
module DCode = Data(Codegen.Gen())

let interfaces = Codegen.Interfaces.create
    ~name:"datapath"
    ~title:"The Plugin interface"
    ~description:[
      "The xapi toolstack expects all plugins to support a basic query ";
      "interface."]
    ~interfaces:[DPCode.implementation (); DCode.implementation ()]
