open Rpc
open Idl
open Common

type exns =
  | Sr_not_attached of string (** An SR must be attached in order to access volumes *)
  | SR_does_not_exist of string (** The specified SR could not be found *)
  | Volume_does_not_exist of string (** The specified volume could not be found in the SR *)
  | Unimplemented of string (** The operation has not been implemented *)
  | Cancelled of string (** The operation has been cancelled *)
  | Activated_on_another_host of string (** The Volume is already active on another host *)
[@@deriving rpcty]

exception Sr_error of exns

let errors = Error.{
    def = exns;
    raiser = (fun e -> Sr_error e);
    matcher = (function | Sr_error e -> Some e | _ -> None);
  }

type health =
  | Healthy of string (** Storage is fully available *)
  | Recovering of string (** Storage is busy recovering, e.g. rebuilding mirrors *)
[@@deriving rpcty]

type sr = string [@@deriving rpcty]
(** Primary key for a specific Storage Repository. This can be any string
    which is meaningful to the implementation. For example this could be an
    NFS directory name, an LVM VG name or even a URI. This string is
    abstract. *)

(** A set of high-level properties associated with an SR. These
    properties can change dynamically and can be queried by a
    SR.stat call. *)
type sr_stat = {
  sr: sr;
  (** The URI identifying this volume. A typical value would be a file://
      URI pointing to a directory or block device *)

  name: string; (** Short, human-readable label for the SR. *)

  uuid: string option;
  (** Uuid that uniquely identifies this SR, if one is available.
      For SRs that are created by SR.create, this should be the
      value passed into that call, if it is possible to persist
      it. *)

  description: string;
  (** Longer, human-readable description of the SR. Descriptions are
      generally only displayed by clients when the user is examining SRs in
      detail. *)

  free_space: int64;
  (** Number of bytes free on the backing storage (in bytes) *)

  total_space: int64;
  (** Total physical size of the backing storage (in bytes) *)

  datasources: string list;
  (** URIs naming datasources: time-varying quantities representing
      anything from disk access latency to free space. The entities named
      by these URIs are self-describing. *)

  clustered: bool; (** Indicates whether the SR uses clustered local storage. *)

  health: health; (** The health status of the SR. *)
} [@@deriving rpcty]

type key = string [@@deriving rpcty]
(** Primary key for a volume. This can be any string which is meaningful to
    the implementation. For example this could be an NFS filename, an LVM LV
    name or even a URI. This string is abstract. *)

type key_list = key list [@@deriving rpcty]

(** A set of properties associated with a volume. These properties can
    change dynamically and can be queried by the Volume.stat call. *)
type volume = {
  key : key;
  (** A primary key for this volume. The key must be unique within the
      enclosing Storage Repository (SR). A typical value would be a
      filename or an LVM volume name. *)

  uuid : string option;
  (** A uuid (or guid) for the volume, if one is available. If a
      storage system has a built-in notion of a guid, then it will be
      returned here. *)

  name : string;
  (** Short, human-readable label for the volume. Names are commonly used
      by when displaying short lists of volumes. *)

  description : string;
  (** Longer, human-readable description of the volume. Descriptions are
      generally only displayed by clients when the user is examining
      volumes individually. *)

  read_write : bool;
  (** True means the VDI may be written to, false means the volume is
      read-only. Some storage media is read-only so all volumes are
      read-only; for example .iso disk images on an NFS share. Some volume
      are created read-only; for example because they are snapshots of some
      other VDI. *)

  sharable : bool;
  (** Indicates whether the VDI can be attached by
      multiple hosts at once. This is used for example by the HA statefile and
      XAPI redo log. *)

  virtual_size : int64;
  (** Size of the volume from the perspective of a VM (in bytes) *)

  physical_utilisation : int64;
  (** Amount of space currently used on the backing storage (in bytes) *)

  uri : string list;
  (** A list of URIs which can be opened and by a datapath plugin for I/O. A
      URI could reference a local block device, a remote NFS share, iSCSI LUN
      or RBD volume. In cases where the data may be accessed over several
      protocols, the list should be sorted into descending order of
      desirability. Xapi will open the most desirable URI for which it has an
      available datapath plugin. *)

  keys : (string * string) list;
  (** A list of key=value pairs which have been stored in the Volume
      metadata. These should not be interpreted by the Volume plugin. *)
} [@@deriving rpcty]

type base64 = string [@@deriving rpcty]
(** Base64-encoded data *)

type changed_blocks = {
  granularity: int;
  (** One bit in the changed block bitmap indicates the status of an area of
      this size, in bytes. *)

  bitmap: base64;
  (** The changed blocks between two volumes as a base64-encoded string.
      The bits in the bitmap indicate the status of consecutive blocks of size
      [granularity] bytes.
      Each bit is set if the corresponding area has changed. *)
} [@@deriving rpcty]


let sr = Param.mk ~name:"sr" ~description:["The Storage Repository"]
    Types.string


module Volume(R: RPC) = struct
  open R

  let key = Param.mk ~name:"key" ~description:["The volume key"] key

  let key2 = {key with Param.name=Some "key2"}

  let key_list = Param.mk ~name:"key list" ~description:["List of volume keys"] key_list

  let volume = Param.mk ~name:"volume" ~description:
      ["Properties of the volume"] volume

  let create =
    let name = Param.mk ~name:"name" ~description:
        ["A human-readable name to associate with the new disk. This name is ";
         "intended to be short, to be a good summary of the disk."]
        Types.string
    in
    let description = Param.mk ~name:"description" ~description:
        ["A human-readable description to associate with the new disk. This can ";
         "be arbitrarily long, up to the general string size limit."]
        Types.string
    in
    let size = Param.mk ~name:"size" ~description:
        ["A minimum size (in bytes) for the disk. Depending on the ";
         "characteristics of the implementation this may be rounded up to ";
         "(for example) the nearest convenient block size. The created disk ";
         "will not be smaller than this size."]
        Types.int64
    in
    let sharable = Param.mk ~name:"sharable" ~description:
        ["Indicates whether the VDI can be attached by";
         "multiple hosts at once.";
         "This is used for example by the HA statefile and XAPI redo log."]
        Types.bool
    in
    R.declare "create"
      ["[create sr name description size] creates a new volume in [sr] with ";
       "[name] and [description]. The volume will have size >= [size] i.e. it ";
       "is always permissable for an implementation to round-up the volume to ";
       "the nearest convenient block size"]
      (dbg @-> sr @-> name @-> description @-> size @-> sharable @-> returning volume errors)

  let snapshot = R.declare "snapshot"
      ["[snapshot sr volume] creates a new volue which is a  snapshot of ";
       "[volume] in [sr]. Snapshots should never be written to; they are ";
       "intended for backup/restore only. Note the name and description are ";
       "copied but any extra metadata associated by [set] is not copied.";
       "This can raise Activated_on_another_host(host_installation_uuid)";
       "if the VDI is already active on another host and snapshots";
       "can only be taken on the host that has the VDI active (if any).";
       "XAPI will take care of redirecting the request to the proper host"]
      (dbg @-> sr @-> key @-> returning volume errors)

  let clone = R.declare "clone"
      ["[clone sr volume] creates a new volume which is a writable clone of ";
       "[volume] in [sr]. Note the name and description are copied but any ";
       "extra metadata associated by [set] is not copied."]
      (dbg @-> sr @-> key @-> returning volume errors)

  let destroy = R.declare "destroy"
      ["[destroy sr volume] removes [volume] from [sr]"]
      (dbg @-> sr @-> key @-> returning unit errors)

  let new_name = Param.mk ~name:"new_name" ~description:["New name"]
      Types.string

  let set_name = R.declare "set_name"
      ["[set_name sr volume new_name] changes the name of [volume]"]
      (dbg @-> sr @-> key @-> new_name @-> returning unit errors)

  let set_description =
    let new_description = Param.mk ~name:"new_description"
        ~description:["New description"] Types.string
    in
    R.declare "set_description"
      ["[set_description sr volume new_description] changes the description ";
       "of [volume]"]
      (dbg @-> sr @-> key @-> new_description @-> returning unit errors)

  let k = Param.mk ~name:"k" ~description:["Key"] Types.string
  let v = Param.mk ~name:"v" ~description:["Value"] Types.string

  let set =
    R.declare "set"
      ["[set sr volume key value] associates [key] with [value] in the ";
       "metadata of [volume] Note these keys and values are not interpreted ";
       "by the plugin; they are intended for the higher-level software only."]
      (dbg @-> sr @-> key @-> k @-> v @-> returning unit errors)

  let unset = R.declare "unset"
      ["[unset sr volume key] removes [key] and any value associated with it ";
       "from the metadata of [volume] Note these keys and values are not ";
       "interpreted by the plugin; they are intended for the higher-level ";
       "software only."]
      (dbg @-> sr @-> key @-> k @-> returning unit errors)

  let resize =
    let new_size = Param.mk ~name:"new_size" ~description:["New disk size"]
        Types.int64
    in
    R.declare "resize"
      ["[resize sr volume new_size] enlarges [volume] to be at least ";
       "[new_size]."]
      (dbg @-> sr @-> key @-> new_size @-> returning unit errors)

  let stat = R.declare "stat"
      ["[stat sr volume] returns metadata associated with [volume]."]
      (dbg @-> sr @-> key @-> returning volume errors)

  let compare =
    let blocklist_result = Param.mk blocklist in
    R.declare "compare"
      ["[compare sr volume1 volume2] compares the two volumes and returns a ";
       "result of type blocklist that describes the differences between the ";
       "two volumes. If the two volumes are unrelated, or the second volume ";
       "does not exist, the result will be a list of the blocks that are ";
       "non-empty in volume1. If this information is not available to the ";
       "plugin, it should return a result indicating that all blocks are in ";
       "use."]
      (dbg @-> sr @-> key @-> key2 @-> returning blocklist_result errors)

  let similar_content = R.declare "similar_content"
      [ "[similar_content sr volume] returns a list of VDIs which have similar"
      ; "content to [vdi]"
      ]
      (dbg @-> sr @-> key @-> returning key_list errors)

  let enable_cbt = R.declare "enable_cbt"
      [ "[enable_cbt sr volume] enables Changed Block Tracking for [volume]"
      ]
      (dbg @-> sr @-> key @-> returning unit errors)

  let disable_cbt = R.declare "disable_cbt"
      ["[disable_cbt sr volume] disables Changed Block Tracking for [volume]"]
      (dbg @-> sr @-> key @-> returning unit errors)

  let data_destroy = R.declare "data_destroy"
      ["[data_destroy sr volume] deletes the data of the snapshot [volume]";
       "without deleting its changed block tracking metadata"]
      (dbg @-> sr @-> key @-> returning unit errors)

  let list_changed_blocks =
    let offset = Param.mk ~name:"offset" ~description:
        ["The offset of the extent for which changed blocks should be computed"]
        Types.int64
    in
    let length = Param.mk ~name:"length" ~description:
        ["The length of the extent for which changed blocks should be computed"]
        Types.int
    in
    let changed_blocks = Param.mk ~name:"changed_blocks" ~description:
        ["The changed blocks between two volumes in the specified extent"]
        changed_blocks
    in
    R.declare "list_changed_blocks"
      ["[list_changed_blocks sr volume1 volume2 offset length] returns the";
       "blocks that have changed between [volume1] and [volume2] in the extent";
       "specified by the given [offset] and [length] as a base64-encoded";
       "bitmap string. If this extent is not aligned to the granularity of the";
       "returned bitmap, then the bitmap will cover the area extended to the";
       "nearest block boundaries."]
      (dbg @-> sr @-> key @-> key2 @-> offset @-> length @-> returning changed_blocks errors)

  let implementation = R.implement
      {Idl.Interface.name = "Volume";
       namespace=Some "Volume";
       description=["Operations which operate on volumes (also known as ";
                    "Virtual Disk Images)"];
       version=(5,0,0)}
end

type configuration = (string * string) list [@@deriving rpcty]
(** Plugin-specific configuration which describes where and
    how to locate the storage repository. This may include the
    physical block device name, a remote NFS server and path
    or an RBD storage pool. *)


(** A set of properties that describe one result element of SR.probe.
    Result elements and properties can change dynamically based on
    changes to the the SR.probe input-parameters or the target. *)
type probe_result = {
  configuration : configuration;
  (** Plugin-specific configuration which describes where and
      how to locate the storage repository. This may include the physical block
      device name, a remote NFS server and path or an RBD storage pool. *)

  complete : bool;
  (** True if this configuration is complete and can be used to call SR.create
      or SR.attach. False if it requires further iterative calls to SR.probe, to
      potentially narrow down on a configuration that can be used. *)

  sr : sr_stat option; (** Existing SR found for this configuration *)

  extra_info : (string * string) list;
  (** Additional plugin-specific information about this configuration, that
      might be of use for an API user. This can for example include the LUN or
      the WWPN. *)
} [@@deriving rpcty]

type probe_results = probe_result list [@@deriving rpcty]

module Sr(R : RPC) = struct
  open R

  let configuration_p =
    Param.mk
      ~name:"configuration"
      ~description:["Plugin-specific configuration which describes where and";
                    "how to locate the storage repository. This may include";
                    "the physical block device name, a remote NFS server and";
                    "path or an RBD storage pool."]
      configuration

  let volumes = Param.mk ~name:"volumes"
      Types.{name="volumes";
             description=["A list of volumes"];
             ty=Array (typ_of_volume)}

  let probe =
    let probe_result_p = Param.mk ~name:"probe_result"
        ~description:["Contents of the storage device"] probe_results
    in
    R.declare "probe"
      ["[probe configuration]: can be used iteratively to narrow down configurations";
       "to use with SR.create, or to find existing SRs on the backing storage"]
      (dbg @-> configuration_p @-> returning probe_result_p errors)

  let create =
    let uuid = Param.mk ~name:"uuid" ~description:["A uuid to associate with the SR."]
        Types.string
    in
    let name = Param.mk ~name:"name" ~description:
        ["Human-readable name for the SR"] Types.string
    in
    let description = Param.mk ~name:"description" ~description:
        ["Human-readable description for the SR"] Types.string
    in
    R.declare "create"
      ["[create uuid configuration name description]: creates a fresh SR"]
      (dbg @-> uuid @-> configuration_p @-> name @-> description @-> returning configuration_p errors)

  let attach = R.declare "attach"
      ["[attach configuration]: attaches the SR to the local host. Once an SR is ";
       "attached then volumes may be manipulated."]
      (dbg @-> configuration_p @-> returning sr errors)

  let detach = R.declare "detach"
      ["[detach sr]: detaches the SR, clearing up any associated resources. ";
       "Once the SR is detached then volumes may not be manipulated."]
      (dbg @-> sr @-> returning unit errors)

  let destroy = R.declare "destroy"
      ["[destroy sr]: destroys the [sr] and deletes any volumes associated ";
       "with it. Note that an SR must be attached to be destroyed; otherwise ";
       "Sr_not_attached is thrown."]
      (dbg @-> sr @-> returning unit errors)

  let stat =
    let stat_result = Param.mk ~name:"sr" ~description:["SR metadata"] sr_stat in
    R.declare "stat"
      ["[stat sr] returns summary metadata associated with [sr]. Note this ";
       "call does not return details of sub-volumes, see SR.ls."]
      (dbg @-> sr @-> returning stat_result errors)

  let set_name =
    let new_name = Param.mk ~name:"new_name"
        ~description:["The new name of the SR"]
        Types.string
    in
    R.declare "set_name"
      ["[set_name sr new_name] changes the name of [sr]"]
      (dbg @-> sr @-> new_name @-> returning unit errors)

  let set_description =
    let new_description = Param.mk ~name:"new_description"
        ~description:["The new description for the SR"]
        Types.string
    in
    R.declare "set_description"
      ["[set_description sr new_description] changes the description of [sr]"]
      (dbg @-> sr @-> new_description @-> returning unit errors)

  let ls = R.declare "ls"
      ["[ls sr] returns a list of volumes contained within an attached SR."]
      (dbg @-> sr @-> returning volumes errors)

  let implementation = R.implement
      {Idl.Interface.name = "SR";
       namespace = Some "SR";
       description=["Operations which act on Storage Repositories"];
       version=(5,0,0)}

end

module V = Volume(Codegen.Gen ())
module S = Sr(Codegen.Gen ())

let interfaces = Codegen.Interfaces.create
    ~name:"volume"
    ~title:"The Volume plugin interface"
    ~description:[
      "The xapi toolstack delegates all storage control-plane functions to ";
      "\"Volume plugins\".These plugins allow the toolstack to ";
      "create/destroy/snapshot/clone volumes which are organised into groups ";
      "called Storage Repositories (SR). Volumes have a set of URIs which ";
      "can be used by the \"Datapath plugins\" to connect the disk data to ";
      "VMs."]
    ~interfaces:[S.implementation (); V.implementation ()]
