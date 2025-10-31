open Rpc
open Idl
open Common

(** Properties of this implementation. *)
type query_result = {
    plugin: string  (** Plugin name, used in the XenAPI as SR.type. *)
  ; name: string [@key "name"]  (** Short name. *)
  ; description: string [@key "description"]  (** Description. *)
  ; vendor: string
        (** Entity (e.g. company, project, group) which produced this
      implementation. *)
  ; copyright: string  (** Copyright. *)
  ; version: string  (** Version. *)
  ; required_api_version: string  (** Minimum required API version. *)
  ; features: string list  (** Features supported by this plugin. *)
  ; configuration: (string * string) list
        (** Key/description pairs describing required device_config parameters. *)
  ; required_cluster_stack: string list
        (** The plugin requires one of these cluster stacks to be active. *)
  ; supported_image_formats: string list
        (** List of image formats (VHD, raw, Qcow2...) supported. *)
}
[@@deriving rpcty]

type srs = string list [@@deriving rpcty]

let dbg =
  Param.mk ~name:"dbg"
    ~description:["Debug context from the caller"]
    Types.string

module Plugin (R : RPC) = struct
  open R

  let query =
    let query_result = Param.mk query_result in
    declare "query"
      [
        "Query this implementation and return its properties. This is "
      ; "called by xapi to determine whether it is compatible with xapi "
      ; "and to discover the supported features."
      ]
      (dbg @-> returning query_result error)

  let ls =
    let srs = Param.mk ~name:"srs" ~description:["The attached SRs"] srs in
    declare "ls"
      ["[ls dbg]: returns a list of attached SRs"]
      (dbg @-> returning srs error)

  let diagnostics =
    let diagnostics_p =
      Param.mk ~name:"diagnostics"
        ~description:
          [
            "A string containing loggable human-readable diagnostics information"
          ]
        Types.string
    in
    declare "diagnostics"
      [
        "Returns a printable set of backend diagnostic information. "
      ; "Implementations are encouraged to include any data which will "
      ; "be useful to diagnose problems. Note this data should not "
      ; "include personally-identifiable data as it is intended to be "
      ; " automatically included in bug reports."
      ]
      (dbg @-> returning diagnostics_p error)

  let implementation =
    R.implement
      {
        Idl.Interface.name= "Plugin"
      ; namespace= Some "Plugin"
      ; description=
          [
            "Discover properties of this implementation. Every implementation"
          ; "must support the query interface or it will not be recognised as"
          ; "a storage plugin by xapi."
          ]
      ; version= (1, 0, 0)
      }
end

module P = Plugin (Codegen.Gen ())

let interfaces =
  Codegen.Interfaces.create ~name:"plugin" ~title:"The Plugin interface"
    ~description:
      [
        "The xapi toolstack expects all plugins to support a basic query"
      ; "interface. This means that if you plan to implement both, a volume"
      ; "and a datapath plugin, make sure that both implement the query"
      ; "interface."
      ]
    ~interfaces:[P.implementation ()]
