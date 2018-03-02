open Rpc
open Idl
open Common

type query_result = {
  plugin : string [@doc
      ["plugin name, used in the XenAPI as SR.type"]];

  name : string  [@key "name"] [@doc
      ["short name"]];

  description : string [@key "description"] [@doc
      ["description"]];

  vendor : string [@doc
      ["entity (e.g. company, project, group) which produced this ";
       "implementation"]];

  copyright : string [@doc
      ["copyright"]];

  version : string [@doc
      ["version"]];

  required_api_version : string [@doc
      ["minimum required API version"]];

  features : string list [@doc
      ["features supported by this plugin"]];

  configuration : (string * string) list [@doc
      ["key/description pairs describing required device_config parameters"]];

  required_cluster_stack : string list [@doc
      ["the plugin requires one of these cluster stacks to be active"]];

} [@@deriving rpcty] [@@doc ["Properties of this implementation"]]

type srs = string list [@@deriving rpcty]

let dbg = Param.mk ~name:"dbg" ~description:["Debug context from the caller"]
    Types.string

module Plugin(R : RPC) = struct
  open R

  let unit = Param.mk Types.unit

  let query_result = Param.mk query_result

  let query = declare "query"
      ["Query this implementation and return its properties. This is ";
       "called by xapi to determine whether it is compatible with xapi ";
       "and to discover the supported features."]
      (dbg @-> unit @-> returning query_result error)

  let srs = Param.mk ~name:"srs" ~description:["The attached SRs"] srs

  let ls = declare "ls"
      ["[ls dbg]: returns a list of attached SRs"]
      (dbg @-> unit @-> returning srs error)

  let diagnostics_p = Param.mk ~name:"diagnostics" ~description:
      ["A string containing loggable human-readable diagnostics information"]
      Types.string

  let diagnostics = declare "diagnostics"
      ["Returns a printable set of backend diagnostic information. ";
       "Implementations are encouraged to include any data which will ";
       "be useful to diagnose problems. Note this data should not ";
       "include personally-identifiable data as it is intended to be ";
       " automatically included in bug reports."]
      (dbg @-> unit @-> returning diagnostics_p error)

  let implementation = R.implement
      {Idl.Interface.name = "Plugin";
       namespace = Some "Plugin";
       description=
         ["Discover properties of this implementation. Every implementation";
          "must support the query interface or it will not be recognised as";
          "a storage plugin by xapi."];
       version=(1,0,0)}
end

module P = Plugin(Codegen.Gen ())

let interfaces = Codegen.Interfaces.create
      ~name:"plugin"
      ~title:"The Datapath plugin interface"
      ~description:[
        "The Datapath plugin takes a URI which points to virtual disk data ";
        "and chooses a Xen datapath implementation: driver domain, blkback ";
        "implementation and caching strategy."]
      ~interfaces:[P.implementation ()]
