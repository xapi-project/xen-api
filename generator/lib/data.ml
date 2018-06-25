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

type xendisk = {
  params: string; (** Put into the "params" key in xenstore *)

  extra: (string * string) list;
  (** Key-value pairs to be put into the "sm-data" subdirectory underneath the
      xenstore backend *)

  backend_type: string;
  (** The name of the xenstore directory corresponding to the backend.
      For example "qdisk". *)
} [@@deriving rpcty]

type block_device = {
  path: string;
  (** Path to the system local block device. This is equivalent to the SMAPIv1 params. *)
} [@@deriving rpcty]

type file = {
  path: string; (** Path to the raw file *)
} [@@deriving rpcty]

type nbd = {
  uri: string;
  (** NBD URI of the form nbd:unix:<domain-socket>:exportname=<NAME> (this
      format is used by qemu-system:
      https://manpages.debian.org/stretch/qemu-system-x86/qemu-system-x86_64.1.en.html) *)
} [@@deriving rpcty]

type implementation =
  | XenDisk of xendisk
  (** This value can be used for ring connection. *)
  | BlockDevice of block_device
  (** This value can be used for Domain0 block device access. *)
  | File of file
  | Nbd of nbd
[@@deriving rpcty]

type backend = {
  implementations: implementation list
      [@doc ["choice of implementation technologies"]];
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

let domain = Param.mk
    ~name:"domain"
    ~description:["An opaque string which represents the Xen domain."]
    domain

open Idl

module Datapath(R: RPC) = struct
  open R

  let open_ =
    let persistent = Param.mk (* Inherit the description from the type *)
        ~name:"persistent"
        persistent
    in
    declare "open" [
      "[open uri persistent] is called before a disk is attached to a VM.";
      "If persistent is true then care should be taken to persist all writes";
      "to the disk. If persistent is false then the implementation should";
      "configure a temporary location for writes so they can be thrown away";
      "on [close]."]
      (dbg @-> uri_p @-> persistent @-> returning unit error)

  let attach =
    let backend = Param.mk (* Inherit the description from the type *)
        ~name:"backend"
        backend
    in
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
      description = [ {|
Xapi will call the functions here on VM start / shutdown /
suspend / resume / migrate. Every function is idempotent. Every
function takes a domain parameter which allows the implementation
to track how many domains are currently using the volume. 

Volumes must be attached via the following sequence of calls:

1. [open url persistent] must be called first and is used to declare
   that the writes to the disks must either be persisted or not.
   [open] is not an exclusive operation - a disk may be opened on
   more than once host at once. The call returns `unit` or an
   error.

2. [attach url domain] is then called. The `domain` parameter is
   advisory. Note that this call is currently only ever called once.
   In the future the call may be made multiple times with different
   [domain] parameters if the disk is attached to multiple domains.
   The return value from this call is the information required to 
   attach the disk to a VM. This call is again, not exclusive. The
   volume may be attached to more than one host concurrently.

3. [activate url domain] is called to activate the datapath. This
   must be called before the volume is to be used by the VM, and 
   it is acceptible for this to be an exclusive operation, such that
   it is an error for a volume to be activated on more than one host
   simultaneously.
      |}];
      version=(5,0,0);
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
  let operations = Param.mk operations

  let copy =
    let blocklist = Param.mk ~name:"blocklist" blocklist in
    declare "copy"
      ["[copy uri domain remotes blocks] copies [blocks] from the local disk ";
       "to a remote URI. This may be called as part of a Volume Mirroring ";
       "operation, and hence may need to cooperate with whatever process is ";
       "currently mirroring writes to ensure data integrity is maintained.";
       "The [remote] parameter is a remotely accessible URI, for example,";
       "`nbd://root:pass@foo.com/path/to/disk` that must contain all necessary";
       "authentication tokens"]
      (dbg @-> uri_p @-> domain @-> remote @-> blocklist @-> returning operation error)

  let mirror = declare "mirror"
      ["[mirror uri domain remote] starts mirroring new writes to the volume ";
       "to a remote URI (usually NBD). This is called as part of a volume ";
       "mirroring process"]
      (dbg @-> uri_p @-> domain @-> remote @-> returning operation error)

  let stat =
    let status = Param.mk status in
    declare "stat"
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

  let ls =
    declare "ls"
      ["[ls] returns a list of all current operations"]
      (dbg @-> returning operations error)

  let implementation = implement Idl.Interface.{
      name = "Data";
      namespace = Some "Data";
      description = [ {|
This interface is used for long-running data operations such as
copying the contents of volumes or mirroring volumes to remote
destinations.

These operations are asynchronous and rely on the Tasks API to
report results and errors.

To mirror a VDI a sequence of these API calls is required:

1. Create a destination VDI using the Volume API on the destination
   SR. This must be the same size as the source. To minimize copying
   the destination VDI may be cloned from one that has been previously
   copied, as long as a disk from which the copy was made is still
   present on the source (even as a metadata-only disk)

2. Arrange for the destination disk to be accessible on the source
   host by suitable URL. This may be `nbd`, `iscsi`, `nfs` or
   other URL.

3. Start mirroring all new writes to the destination disk via the
   `Data.mirror` API call.

4. Find the list of blocks to copy via the CBT API call. Note that if
   the destination volume has not been 'prezeroed' then all of the
   blocks must be copied to the destination.

5. Start the background copy of the disk via a call to `DATA.copy`,
   passing in the list of blocks to copy. The plugin must ensure that
   the copy does not conflict with the mirror operation - ie., all
   writes from the mirror operation must not be overwritten by writes
   of old data from the copy operation.

6. The progress of the copy operation may be queried via the `Data.stat`
   call.

7. Once the copy operation has succesfully completed the destination
   disk will be a perfect mirror of the source.

     |}];
      version = (5,0,0)
    }

end

module DPCode = Datapath(Codegen.Gen ())
module DCode = Data(Codegen.Gen())

let interfaces = Codegen.Interfaces.create
    ~name:"datapath"
    ~title:"The SMAPIv3 Data interfaces"
    ~description:[ {|
The Datapath interfaces are provided to access the data stored
in the volumes. The `Datapath` interface is used to open and close the disks for read/write
operations from VMs and the `Data` interface is used for operations such as `copy` and `mirror`
    |}]
    ~interfaces:[DPCode.implementation (); DCode.implementation ()]
