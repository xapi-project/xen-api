include Xenops_types.TopLevel
let service_name = "xenops"
let queue_name = ref (Xcp_service.common_prefix ^ service_name)
let default_sockets_dir = "/var/lib/xcp"
let default_path = ref (Filename.concat default_sockets_dir "xenopsd")
let forwarded_path =
  ref ((Filename.concat default_sockets_dir "xenopsd") ^ ".forwarded")
let set_sockets_dir =
  function
  | x ->
      (default_path := (Filename.concat x "xenopsd");
       forwarded_path := ((!default_path) ^ ".forwarded"))
let default_uri = function | () -> "file:" ^ (!default_path)
let json_url = function | () -> Printf.sprintf "file:%s.json" (!default_path)
type debug_info = string
let rec rpc_of_debug_info = function | __x781__ -> Rpc.String __x781__
and debug_info_of_rpc =
  function
  | __x780__ ->
      (match __x780__ with
       | Rpc.String x -> x
       | __x__ ->
           (if Rpc.get_debug ()
            then
              Printf.eprintf
                "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                "debug_info" "__x780__" (Rpc.to_string __x__)
                "String(string)"
            else ();
            raise (Rpc.Runtime_error ("String(string)", __x__))))
module Query =
  struct
    type t =
      {
      name: string ;
      vendor: string ;
      version: string ;
      features: string list ;
      instance_id: string }
    let rec rpc_of_t =
      function
      | __x791__ ->
          let __x792__ = __x791__.name
          and __x793__ = __x791__.vendor
          and __x794__ = __x791__.version
          and __x795__ = __x791__.features
          and __x796__ = __x791__.instance_id in
          Rpc.Dict
            [("name", (Rpc.String __x792__));
            ("vendor", (Rpc.String __x793__));
            ("version", (Rpc.String __x794__));
            ("features",
              (Rpc.Enum
                 (List.map (function | __x797__ -> Rpc.String __x797__)
                    __x795__)));
            ("instance_id", (Rpc.String __x796__))]
    and t_of_rpc =
      function
      | __x782__ ->
          (match __x782__ with
           | Rpc.Dict __x783__ ->
               let __x784__ =
                 try List.assoc "name" __x783__
                 with
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                          "t" "__x783__" (Printexc.to_string __x__)
                          "Looking for key name"
                      else ();
                      raise
                        (Rpc.Runtime_exception
                           ("Looking for key name",
                             (Printexc.to_string __x__))))
               and __x785__ =
                 try List.assoc "vendor" __x783__
                 with
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                          "t" "__x783__" (Printexc.to_string __x__)
                          "Looking for key vendor"
                      else ();
                      raise
                        (Rpc.Runtime_exception
                           ("Looking for key vendor",
                             (Printexc.to_string __x__))))
               and __x786__ =
                 try List.assoc "version" __x783__
                 with
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                          "t" "__x783__" (Printexc.to_string __x__)
                          "Looking for key version"
                      else ();
                      raise
                        (Rpc.Runtime_exception
                           ("Looking for key version",
                             (Printexc.to_string __x__))))
               and __x787__ =
                 try List.assoc "features" __x783__
                 with
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                          "t" "__x783__" (Printexc.to_string __x__)
                          "Looking for key features"
                      else ();
                      raise
                        (Rpc.Runtime_exception
                           ("Looking for key features",
                             (Printexc.to_string __x__))))
               and __x788__ =
                 try List.assoc "instance_id" __x783__
                 with
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                          "t" "__x783__" (Printexc.to_string __x__)
                          "Looking for key instance_id"
                      else ();
                      raise
                        (Rpc.Runtime_exception
                           ("Looking for key instance_id",
                             (Printexc.to_string __x__)))) in
               {
                 name =
                   ((match __x784__ with
                     | Rpc.String x -> x
                     | __x__ ->
                         (if Rpc.get_debug ()
                          then
                            Printf.eprintf
                              "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                              "t" "__x784__" (Rpc.to_string __x__)
                              "String(string)"
                          else ();
                          raise (Rpc.Runtime_error ("String(string)", __x__)))));
                 vendor =
                   ((match __x785__ with
                     | Rpc.String x -> x
                     | __x__ ->
                         (if Rpc.get_debug ()
                          then
                            Printf.eprintf
                              "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                              "t" "__x785__" (Rpc.to_string __x__)
                              "String(string)"
                          else ();
                          raise (Rpc.Runtime_error ("String(string)", __x__)))));
                 version =
                   ((match __x786__ with
                     | Rpc.String x -> x
                     | __x__ ->
                         (if Rpc.get_debug ()
                          then
                            Printf.eprintf
                              "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                              "t" "__x786__" (Rpc.to_string __x__)
                              "String(string)"
                          else ();
                          raise (Rpc.Runtime_error ("String(string)", __x__)))));
                 features =
                   ((match __x787__ with
                     | Rpc.Enum __x789__ ->
                         List.map
                           (function
                            | __x790__ ->
                                (match __x790__ with
                                 | Rpc.String x -> x
                                 | __x__ ->
                                     (if Rpc.get_debug ()
                                      then
                                        Printf.eprintf
                                          "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                          "t" "__x790__"
                                          (Rpc.to_string __x__)
                                          "String(string)"
                                      else ();
                                      raise
                                        (Rpc.Runtime_error
                                           ("String(string)", __x__)))))
                           __x789__
                     | __x__ ->
                         (if Rpc.get_debug ()
                          then
                            Printf.eprintf
                              "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                              "t" "__x787__" (Rpc.to_string __x__) "List"
                          else ();
                          raise (Rpc.Runtime_error ("List", __x__)))));
                 instance_id =
                   ((match __x788__ with
                     | Rpc.String x -> x
                     | __x__ ->
                         (if Rpc.get_debug ()
                          then
                            Printf.eprintf
                              "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                              "t" "__x788__" (Rpc.to_string __x__)
                              "String(string)"
                          else ();
                          raise (Rpc.Runtime_error ("String(string)", __x__)))))
               }
           | __x__ ->
               (if Rpc.get_debug ()
                then
                  Printf.eprintf
                    "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                    "t" "__x782__" (Rpc.to_string __x__) "Dict"
                else ();
                raise (Rpc.Runtime_error ("Dict", __x__))))
  end
type disk_list = disk list
let rec rpc_of_disk_list =
  function
  | __x801__ ->
      Rpc.Enum
        (List.map (function | __x802__ -> rpc_of_disk __x802__) __x801__)
and disk_list_of_rpc =
  function
  | __x798__ ->
      (match __x798__ with
       | Rpc.Enum __x799__ ->
           List.map (function | __x800__ -> disk_of_rpc __x800__) __x799__
       | __x__ ->
           (if Rpc.get_debug ()
            then
              Printf.eprintf
                "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                "disk_list" "__x798__" (Rpc.to_string __x__) "List"
            else ();
            raise (Rpc.Runtime_error ("List", __x__))))
module Network =
  struct
    type t =
      | Local of string 
      | Remote of string * string 
      | Sriov of Xcp_pci.address 
    let rec rpc_of_t =
      function
      | __x808__ ->
          (match __x808__ with
           | Sriov __x809__ ->
               Rpc.Enum [Rpc.String "Sriov"; Xcp_pci.rpc_of_address __x809__]
           | Remote (__x810__, __x811__) ->
               Rpc.Enum
                 [Rpc.String "Remote";
                 Rpc.String __x810__;
                 Rpc.String __x811__]
           | Local __x812__ ->
               Rpc.Enum [Rpc.String "Local"; Rpc.String __x812__])
    and t_of_rpc =
      function
      | __x803__ ->
          (match Rpc.lowerfn __x803__ with
           | Rpc.Enum ((Rpc.String "sriov")::__x804__::[]) ->
               Sriov (Xcp_pci.address_of_rpc __x804__)
           | Rpc.Enum ((Rpc.String "remote")::__x805__::__x806__::[]) ->
               Remote
                 (((match __x805__ with
                    | Rpc.String x -> x
                    | __x__ ->
                        (if Rpc.get_debug ()
                         then
                           Printf.eprintf
                             "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                             "t" "__x805__" (Rpc.to_string __x__)
                             "String(string)"
                         else ();
                         raise (Rpc.Runtime_error ("String(string)", __x__))))),
                   ((match __x806__ with
                     | Rpc.String x -> x
                     | __x__ ->
                         (if Rpc.get_debug ()
                          then
                            Printf.eprintf
                              "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                              "t" "__x806__" (Rpc.to_string __x__)
                              "String(string)"
                          else ();
                          raise (Rpc.Runtime_error ("String(string)", __x__))))))
           | Rpc.Enum ((Rpc.String "local")::__x807__::[]) ->
               Local
                 ((match __x807__ with
                   | Rpc.String x -> x
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "t" "__x807__" (Rpc.to_string __x__)
                            "String(string)"
                        else ();
                        raise (Rpc.Runtime_error ("String(string)", __x__)))))
           | __x__ ->
               (if Rpc.get_debug ()
                then
                  Printf.eprintf
                    "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                    "t" "__x803__" (Rpc.to_string __x__) "Enum[String s;...]"
                else ();
                raise (Rpc.Runtime_error ("Enum[String s;...]", __x__))))
    type ts = t list
    let rec rpc_of_ts =
      function
      | __x816__ ->
          Rpc.Enum
            (List.map (function | __x817__ -> rpc_of_t __x817__) __x816__)
    and ts_of_rpc =
      function
      | __x813__ ->
          (match __x813__ with
           | Rpc.Enum __x814__ ->
               List.map (function | __x815__ -> t_of_rpc __x815__) __x814__
           | __x__ ->
               (if Rpc.get_debug ()
                then
                  Printf.eprintf
                    "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                    "ts" "__x813__" (Rpc.to_string __x__) "List"
                else ();
                raise (Rpc.Runtime_error ("List", __x__))))
    let default_t = Local "xenbr0"
  end
module Pci =
  struct
    include Xcp_pci
    type id = (string * string)
    let rec rpc_of_id =
      function
      | __x821__ ->
          let (__x822__, __x823__) = __x821__ in
          Rpc.Enum [Rpc.String __x822__; Rpc.String __x823__]
    and id_of_rpc =
      function
      | __x818__ ->
          (match __x818__ with
           | Rpc.Enum (__x819__::__x820__::[]) ->
               (((match __x819__ with
                  | Rpc.String x -> x
                  | __x__ ->
                      (if Rpc.get_debug ()
                       then
                         Printf.eprintf
                           "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                           "id" "__x819__" (Rpc.to_string __x__)
                           "String(string)"
                       else ();
                       raise (Rpc.Runtime_error ("String(string)", __x__))))),
                 ((match __x820__ with
                   | Rpc.String x -> x
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "id" "__x820__" (Rpc.to_string __x__)
                            "String(string)"
                        else ();
                        raise (Rpc.Runtime_error ("String(string)", __x__))))))
           | __x__ ->
               (if Rpc.get_debug ()
                then
                  Printf.eprintf
                    "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                    "id" "__x818__" (Rpc.to_string __x__) "List"
                else ();
                raise (Rpc.Runtime_error ("List", __x__))))
    type t =
      {
      id: id ;
      position: int ;
      address: address ;
      msitranslate: bool option ;
      power_mgmt: bool option }
    let rec rpc_of_t =
      function
      | __x833__ ->
          let __x834__ = __x833__.id
          and __x835__ = __x833__.position
          and __x836__ = __x833__.address
          and __x837__ = __x833__.msitranslate
          and __x838__ = __x833__.power_mgmt in
          Rpc.Dict (("id", (rpc_of_id __x834__)) ::
            ("position", (Rpc.Int (Int64.of_int __x835__))) ::
            ("address", (rpc_of_address __x836__)) ::
            ((match match __x837__ with
                    | Some __x842__ -> Rpc.Enum [Rpc.Bool __x842__]
                    | None -> Rpc.Enum []
              with
              | Rpc.Enum [] ->
                  (match match __x838__ with
                         | Some __x840__ -> Rpc.Enum [Rpc.Bool __x840__]
                         | None -> Rpc.Enum []
                   with
                   | Rpc.Enum [] -> []
                   | Rpc.Enum (__x839__::[]) -> [("power_mgmt", __x839__)]
                   | _ -> assert false)
              | Rpc.Enum (__x841__::[]) -> ("msitranslate", __x841__) ::
                  ((match match __x838__ with
                          | Some __x840__ -> Rpc.Enum [Rpc.Bool __x840__]
                          | None -> Rpc.Enum []
                    with
                    | Rpc.Enum [] -> []
                    | Rpc.Enum (__x839__::[]) -> [("power_mgmt", __x839__)]
                    | _ -> assert false))
              | _ -> assert false)))
    and t_of_rpc =
      function
      | __x824__ ->
          (match __x824__ with
           | Rpc.Dict __x825__ ->
               let __x826__ =
                 try List.assoc "id" __x825__
                 with
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                          "t" "__x825__" (Printexc.to_string __x__)
                          "Looking for key id"
                      else ();
                      raise
                        (Rpc.Runtime_exception
                           ("Looking for key id", (Printexc.to_string __x__))))
               and __x827__ =
                 try List.assoc "position" __x825__
                 with
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                          "t" "__x825__" (Printexc.to_string __x__)
                          "Looking for key position"
                      else ();
                      raise
                        (Rpc.Runtime_exception
                           ("Looking for key position",
                             (Printexc.to_string __x__))))
               and __x828__ =
                 try List.assoc "address" __x825__
                 with
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                          "t" "__x825__" (Printexc.to_string __x__)
                          "Looking for key address"
                      else ();
                      raise
                        (Rpc.Runtime_exception
                           ("Looking for key address",
                             (Printexc.to_string __x__))))
               and __x829__ =
                 if List.mem_assoc "msitranslate" __x825__
                 then Rpc.Enum [List.assoc "msitranslate" __x825__]
                 else Rpc.Enum []
               and __x830__ =
                 if List.mem_assoc "power_mgmt" __x825__
                 then Rpc.Enum [List.assoc "power_mgmt" __x825__]
                 else Rpc.Enum [] in
               {
                 id = (id_of_rpc __x826__);
                 position =
                   ((match __x827__ with
                     | Rpc.Int x -> Int64.to_int x
                     | Rpc.String s -> int_of_string s
                     | __x__ ->
                         (if Rpc.get_debug ()
                          then
                            Printf.eprintf
                              "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                              "t" "__x827__" (Rpc.to_string __x__) "Int(int)"
                          else ();
                          raise (Rpc.Runtime_error ("Int(int)", __x__)))));
                 address = (address_of_rpc __x828__);
                 msitranslate =
                   ((match __x829__ with
                     | Rpc.Enum [] -> None
                     | Rpc.Enum (__x831__::[]) ->
                         Some
                           ((match __x831__ with
                             | Rpc.Bool x -> x
                             | __x__ ->
                                 (if Rpc.get_debug ()
                                  then
                                    Printf.eprintf
                                      "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                      "t" "__x831__" (Rpc.to_string __x__)
                                      "Bool"
                                  else ();
                                  raise (Rpc.Runtime_error ("Bool", __x__)))))
                     | __x__ ->
                         (if Rpc.get_debug ()
                          then
                            Printf.eprintf
                              "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                              "t" "__x829__" (Rpc.to_string __x__)
                              "Enum[]/Enum[_]"
                          else ();
                          raise (Rpc.Runtime_error ("Enum[]/Enum[_]", __x__)))));
                 power_mgmt =
                   ((match __x830__ with
                     | Rpc.Enum [] -> None
                     | Rpc.Enum (__x832__::[]) ->
                         Some
                           ((match __x832__ with
                             | Rpc.Bool x -> x
                             | __x__ ->
                                 (if Rpc.get_debug ()
                                  then
                                    Printf.eprintf
                                      "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                      "t" "__x832__" (Rpc.to_string __x__)
                                      "Bool"
                                  else ();
                                  raise (Rpc.Runtime_error ("Bool", __x__)))))
                     | __x__ ->
                         (if Rpc.get_debug ()
                          then
                            Printf.eprintf
                              "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                              "t" "__x830__" (Rpc.to_string __x__)
                              "Enum[]/Enum[_]"
                          else ();
                          raise (Rpc.Runtime_error ("Enum[]/Enum[_]", __x__)))))
               }
           | __x__ ->
               (if Rpc.get_debug ()
                then
                  Printf.eprintf
                    "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                    "t" "__x824__" (Rpc.to_string __x__) "Dict"
                else ();
                raise (Rpc.Runtime_error ("Dict", __x__))))
    type state = {
      plugged: bool }
    let rec rpc_of_state =
      function
      | __x846__ ->
          let __x847__ = __x846__.plugged in
          Rpc.Dict [("plugged", (Rpc.Bool __x847__))]
    and state_of_rpc =
      function
      | __x843__ ->
          (match __x843__ with
           | Rpc.Dict __x844__ ->
               let __x845__ =
                 try List.assoc "plugged" __x844__
                 with
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                          "state" "__x844__" (Printexc.to_string __x__)
                          "Looking for key plugged"
                      else ();
                      raise
                        (Rpc.Runtime_exception
                           ("Looking for key plugged",
                             (Printexc.to_string __x__)))) in
               {
                 plugged =
                   ((match __x845__ with
                     | Rpc.Bool x -> x
                     | __x__ ->
                         (if Rpc.get_debug ()
                          then
                            Printf.eprintf
                              "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                              "state" "__x845__" (Rpc.to_string __x__) "Bool"
                          else ();
                          raise (Rpc.Runtime_error ("Bool", __x__)))))
               }
           | __x__ ->
               (if Rpc.get_debug ()
                then
                  Printf.eprintf
                    "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                    "state" "__x843__" (Rpc.to_string __x__) "Dict"
                else ();
                raise (Rpc.Runtime_error ("Dict", __x__))))
  end
module Vgpu =
  struct
    include Xenops_types.Vgpu
    type implementation =
      | GVT_g of gvt_g 
      | Nvidia of nvidia 
      | MxGPU of mxgpu 
      | Empty 
    let rec rpc_of_implementation =
      function
      | __x852__ ->
          (match __x852__ with
           | Empty -> Rpc.String "Empty"
           | MxGPU __x853__ ->
               Rpc.Enum [Rpc.String "MxGPU"; rpc_of_mxgpu __x853__]
           | Nvidia __x854__ ->
               Rpc.Enum [Rpc.String "Nvidia"; rpc_of_nvidia __x854__]
           | GVT_g __x855__ ->
               Rpc.Enum [Rpc.String "GVT_g"; rpc_of_gvt_g __x855__])
    and implementation_of_rpc =
      function
      | __x848__ ->
          (match Rpc.lowerfn __x848__ with
           | Rpc.String "empty" -> Empty
           | Rpc.Enum ((Rpc.String "mxgpu")::__x849__::[]) ->
               MxGPU (mxgpu_of_rpc __x849__)
           | Rpc.Enum ((Rpc.String "nvidia")::__x850__::[]) ->
               Nvidia (nvidia_of_rpc __x850__)
           | Rpc.Enum ((Rpc.String "gvt_g")::__x851__::[]) ->
               GVT_g (gvt_g_of_rpc __x851__)
           | __x__ ->
               (if Rpc.get_debug ()
                then
                  Printf.eprintf
                    "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                    "implementation" "__x848__" (Rpc.to_string __x__)
                    "Enum[String s;...]"
                else ();
                raise (Rpc.Runtime_error ("Enum[String s;...]", __x__))))
    type id = (string * string)
    let rec rpc_of_id =
      function
      | __x859__ ->
          let (__x860__, __x861__) = __x859__ in
          Rpc.Enum [Rpc.String __x860__; Rpc.String __x861__]
    and id_of_rpc =
      function
      | __x856__ ->
          (match __x856__ with
           | Rpc.Enum (__x857__::__x858__::[]) ->
               (((match __x857__ with
                  | Rpc.String x -> x
                  | __x__ ->
                      (if Rpc.get_debug ()
                       then
                         Printf.eprintf
                           "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                           "id" "__x857__" (Rpc.to_string __x__)
                           "String(string)"
                       else ();
                       raise (Rpc.Runtime_error ("String(string)", __x__))))),
                 ((match __x858__ with
                   | Rpc.String x -> x
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "id" "__x858__" (Rpc.to_string __x__)
                            "String(string)"
                        else ();
                        raise (Rpc.Runtime_error ("String(string)", __x__))))))
           | __x__ ->
               (if Rpc.get_debug ()
                then
                  Printf.eprintf
                    "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                    "id" "__x856__" (Rpc.to_string __x__) "List"
                else ();
                raise (Rpc.Runtime_error ("List", __x__))))
    type t =
      {
      id: id ;
      position: int ;
      physical_pci_address: Pci.address ;
      implementation: implementation }
    let rec rpc_of_t =
      function
      | __x868__ ->
          let __x869__ = __x868__.id
          and __x870__ = __x868__.position
          and __x871__ = __x868__.physical_pci_address
          and __x872__ = __x868__.implementation in
          Rpc.Dict
            [("id", (rpc_of_id __x869__));
            ("position", (Rpc.Int (Int64.of_int __x870__)));
            ("physical_pci_address", (Pci.rpc_of_address __x871__));
            ("implementation", (rpc_of_implementation __x872__))]
    and t_of_rpc =
      function
      | __x862__ ->
          (match __x862__ with
           | Rpc.Dict __x863__ ->
               let __x864__ =
                 try List.assoc "id" __x863__
                 with
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                          "t" "__x863__" (Printexc.to_string __x__)
                          "Looking for key id"
                      else ();
                      raise
                        (Rpc.Runtime_exception
                           ("Looking for key id", (Printexc.to_string __x__))))
               and __x865__ =
                 try List.assoc "position" __x863__
                 with
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                          "t" "__x863__" (Printexc.to_string __x__)
                          "Looking for key position"
                      else ();
                      raise
                        (Rpc.Runtime_exception
                           ("Looking for key position",
                             (Printexc.to_string __x__))))
               and __x866__ =
                 try List.assoc "physical_pci_address" __x863__
                 with
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                          "t" "__x863__" (Printexc.to_string __x__)
                          "Looking for key physical_pci_address"
                      else ();
                      raise
                        (Rpc.Runtime_exception
                           ("Looking for key physical_pci_address",
                             (Printexc.to_string __x__))))
               and __x867__ =
                 try List.assoc "implementation" __x863__
                 with
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                          "t" "__x863__" (Printexc.to_string __x__)
                          "Looking for key implementation"
                      else ();
                      raise
                        (Rpc.Runtime_exception
                           ("Looking for key implementation",
                             (Printexc.to_string __x__)))) in
               {
                 id = (id_of_rpc __x864__);
                 position =
                   ((match __x865__ with
                     | Rpc.Int x -> Int64.to_int x
                     | Rpc.String s -> int_of_string s
                     | __x__ ->
                         (if Rpc.get_debug ()
                          then
                            Printf.eprintf
                              "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                              "t" "__x865__" (Rpc.to_string __x__) "Int(int)"
                          else ();
                          raise (Rpc.Runtime_error ("Int(int)", __x__)))));
                 physical_pci_address = (Pci.address_of_rpc __x866__);
                 implementation = (implementation_of_rpc __x867__)
               }
           | __x__ ->
               (if Rpc.get_debug ()
                then
                  Printf.eprintf
                    "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                    "t" "__x862__" (Rpc.to_string __x__) "Dict"
                else ();
                raise (Rpc.Runtime_error ("Dict", __x__))))
    let default_t =
      {
        id = ("", "");
        position = 0;
        physical_pci_address =
          (let open Pci in { domain = 0; bus = 0; dev = 0; fn = 0 });
        implementation = Empty
      }
    let upgrade_pci_info =
      function
      | x ->
          (match x with
           | {
               implementation = GVT_g
                 { physical_pci_address = Some address;_};_}
             |{
                implementation = Nvidia
                  { physical_pci_address = Some address;_};_}
             |{
                implementation = MxGPU { physical_function = Some address;_};_}
               -> { x with physical_pci_address = address }
           | _ -> x)
    let t_of_rpc =
      function
      | rpc ->
          ((Rpc.struct_extend rpc (rpc_of_t default_t)) |> t_of_rpc) |>
            upgrade_pci_info
    type state = {
      plugged: bool ;
      emulator_pid: int option }
    let rec rpc_of_state =
      function
      | __x878__ ->
          let __x879__ = __x878__.plugged
          and __x880__ = __x878__.emulator_pid in
          Rpc.Dict (("plugged", (Rpc.Bool __x879__)) ::
            ((match match __x880__ with
                    | Some __x882__ ->
                        Rpc.Enum [Rpc.Int (Int64.of_int __x882__)]
                    | None -> Rpc.Enum []
              with
              | Rpc.Enum [] -> []
              | Rpc.Enum (__x881__::[]) -> [("emulator_pid", __x881__)]
              | _ -> assert false)))
    and state_of_rpc =
      function
      | __x873__ ->
          (match __x873__ with
           | Rpc.Dict __x874__ ->
               let __x875__ =
                 try List.assoc "plugged" __x874__
                 with
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                          "state" "__x874__" (Printexc.to_string __x__)
                          "Looking for key plugged"
                      else ();
                      raise
                        (Rpc.Runtime_exception
                           ("Looking for key plugged",
                             (Printexc.to_string __x__))))
               and __x876__ =
                 if List.mem_assoc "emulator_pid" __x874__
                 then Rpc.Enum [List.assoc "emulator_pid" __x874__]
                 else Rpc.Enum [] in
               {
                 plugged =
                   ((match __x875__ with
                     | Rpc.Bool x -> x
                     | __x__ ->
                         (if Rpc.get_debug ()
                          then
                            Printf.eprintf
                              "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                              "state" "__x875__" (Rpc.to_string __x__) "Bool"
                          else ();
                          raise (Rpc.Runtime_error ("Bool", __x__)))));
                 emulator_pid =
                   ((match __x876__ with
                     | Rpc.Enum [] -> None
                     | Rpc.Enum (__x877__::[]) ->
                         Some
                           ((match __x877__ with
                             | Rpc.Int x -> Int64.to_int x
                             | Rpc.String s -> int_of_string s
                             | __x__ ->
                                 (if Rpc.get_debug ()
                                  then
                                    Printf.eprintf
                                      "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                      "state" "__x877__"
                                      (Rpc.to_string __x__) "Int(int)"
                                  else ();
                                  raise
                                    (Rpc.Runtime_error ("Int(int)", __x__)))))
                     | __x__ ->
                         (if Rpc.get_debug ()
                          then
                            Printf.eprintf
                              "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                              "state" "__x876__" (Rpc.to_string __x__)
                              "Enum[]/Enum[_]"
                          else ();
                          raise (Rpc.Runtime_error ("Enum[]/Enum[_]", __x__)))))
               }
           | __x__ ->
               (if Rpc.get_debug ()
                then
                  Printf.eprintf
                    "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                    "state" "__x873__" (Rpc.to_string __x__) "Dict"
                else ();
                raise (Rpc.Runtime_error ("Dict", __x__))))
  end
module Vusb =
  struct
    type id = (string * string)
    let rec rpc_of_id =
      function
      | __x886__ ->
          let (__x887__, __x888__) = __x886__ in
          Rpc.Enum [Rpc.String __x887__; Rpc.String __x888__]
    and id_of_rpc =
      function
      | __x883__ ->
          (match __x883__ with
           | Rpc.Enum (__x884__::__x885__::[]) ->
               (((match __x884__ with
                  | Rpc.String x -> x
                  | __x__ ->
                      (if Rpc.get_debug ()
                       then
                         Printf.eprintf
                           "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                           "id" "__x884__" (Rpc.to_string __x__)
                           "String(string)"
                       else ();
                       raise (Rpc.Runtime_error ("String(string)", __x__))))),
                 ((match __x885__ with
                   | Rpc.String x -> x
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "id" "__x885__" (Rpc.to_string __x__)
                            "String(string)"
                        else ();
                        raise (Rpc.Runtime_error ("String(string)", __x__))))))
           | __x__ ->
               (if Rpc.get_debug ()
                then
                  Printf.eprintf
                    "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                    "id" "__x883__" (Rpc.to_string __x__) "List"
                else ();
                raise (Rpc.Runtime_error ("List", __x__))))
    type t =
      {
      id: id ;
      hostbus: string ;
      hostport: string ;
      version: string ;
      path: string }
    let rec rpc_of_t =
      function
      | __x896__ ->
          let __x897__ = __x896__.id
          and __x898__ = __x896__.hostbus
          and __x899__ = __x896__.hostport
          and __x900__ = __x896__.version
          and __x901__ = __x896__.path in
          Rpc.Dict
            [("id", (rpc_of_id __x897__));
            ("hostbus", (Rpc.String __x898__));
            ("hostport", (Rpc.String __x899__));
            ("version", (Rpc.String __x900__));
            ("path", (Rpc.String __x901__))]
    and t_of_rpc =
      function
      | __x889__ ->
          (match __x889__ with
           | Rpc.Dict __x890__ ->
               let __x891__ =
                 try List.assoc "id" __x890__
                 with
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                          "t" "__x890__" (Printexc.to_string __x__)
                          "Looking for key id"
                      else ();
                      raise
                        (Rpc.Runtime_exception
                           ("Looking for key id", (Printexc.to_string __x__))))
               and __x892__ =
                 try List.assoc "hostbus" __x890__
                 with
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                          "t" "__x890__" (Printexc.to_string __x__)
                          "Looking for key hostbus"
                      else ();
                      raise
                        (Rpc.Runtime_exception
                           ("Looking for key hostbus",
                             (Printexc.to_string __x__))))
               and __x893__ =
                 try List.assoc "hostport" __x890__
                 with
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                          "t" "__x890__" (Printexc.to_string __x__)
                          "Looking for key hostport"
                      else ();
                      raise
                        (Rpc.Runtime_exception
                           ("Looking for key hostport",
                             (Printexc.to_string __x__))))
               and __x894__ =
                 try List.assoc "version" __x890__
                 with
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                          "t" "__x890__" (Printexc.to_string __x__)
                          "Looking for key version"
                      else ();
                      raise
                        (Rpc.Runtime_exception
                           ("Looking for key version",
                             (Printexc.to_string __x__))))
               and __x895__ =
                 try List.assoc "path" __x890__
                 with
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                          "t" "__x890__" (Printexc.to_string __x__)
                          "Looking for key path"
                      else ();
                      raise
                        (Rpc.Runtime_exception
                           ("Looking for key path",
                             (Printexc.to_string __x__)))) in
               {
                 id = (id_of_rpc __x891__);
                 hostbus =
                   ((match __x892__ with
                     | Rpc.String x -> x
                     | __x__ ->
                         (if Rpc.get_debug ()
                          then
                            Printf.eprintf
                              "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                              "t" "__x892__" (Rpc.to_string __x__)
                              "String(string)"
                          else ();
                          raise (Rpc.Runtime_error ("String(string)", __x__)))));
                 hostport =
                   ((match __x893__ with
                     | Rpc.String x -> x
                     | __x__ ->
                         (if Rpc.get_debug ()
                          then
                            Printf.eprintf
                              "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                              "t" "__x893__" (Rpc.to_string __x__)
                              "String(string)"
                          else ();
                          raise (Rpc.Runtime_error ("String(string)", __x__)))));
                 version =
                   ((match __x894__ with
                     | Rpc.String x -> x
                     | __x__ ->
                         (if Rpc.get_debug ()
                          then
                            Printf.eprintf
                              "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                              "t" "__x894__" (Rpc.to_string __x__)
                              "String(string)"
                          else ();
                          raise (Rpc.Runtime_error ("String(string)", __x__)))));
                 path =
                   ((match __x895__ with
                     | Rpc.String x -> x
                     | __x__ ->
                         (if Rpc.get_debug ()
                          then
                            Printf.eprintf
                              "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                              "t" "__x895__" (Rpc.to_string __x__)
                              "String(string)"
                          else ();
                          raise (Rpc.Runtime_error ("String(string)", __x__)))))
               }
           | __x__ ->
               (if Rpc.get_debug ()
                then
                  Printf.eprintf
                    "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                    "t" "__x889__" (Rpc.to_string __x__) "Dict"
                else ();
                raise (Rpc.Runtime_error ("Dict", __x__))))
    type state = {
      plugged: bool }
    let rec rpc_of_state =
      function
      | __x905__ ->
          let __x906__ = __x905__.plugged in
          Rpc.Dict [("plugged", (Rpc.Bool __x906__))]
    and state_of_rpc =
      function
      | __x902__ ->
          (match __x902__ with
           | Rpc.Dict __x903__ ->
               let __x904__ =
                 try List.assoc "plugged" __x903__
                 with
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                          "state" "__x903__" (Printexc.to_string __x__)
                          "Looking for key plugged"
                      else ();
                      raise
                        (Rpc.Runtime_exception
                           ("Looking for key plugged",
                             (Printexc.to_string __x__)))) in
               {
                 plugged =
                   ((match __x904__ with
                     | Rpc.Bool x -> x
                     | __x__ ->
                         (if Rpc.get_debug ()
                          then
                            Printf.eprintf
                              "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                              "state" "__x904__" (Rpc.to_string __x__) "Bool"
                          else ();
                          raise (Rpc.Runtime_error ("Bool", __x__)))))
               }
           | __x__ ->
               (if Rpc.get_debug ()
                then
                  Printf.eprintf
                    "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                    "state" "__x902__" (Rpc.to_string __x__) "Dict"
                else ();
                raise (Rpc.Runtime_error ("Dict", __x__))))
  end
module Vm = struct include Xenops_types.Vm end
module Vbd =
  struct
    type mode =
      | ReadOnly 
      | ReadWrite 
    let rec rpc_of_mode =
      function
      | __x908__ ->
          (match __x908__ with
           | ReadWrite -> Rpc.String "ReadWrite"
           | ReadOnly -> Rpc.String "ReadOnly")
    and mode_of_rpc =
      function
      | __x907__ ->
          (match Rpc.lowerfn __x907__ with
           | Rpc.String "readwrite" -> ReadWrite
           | Rpc.String "readonly" -> ReadOnly
           | __x__ ->
               (if Rpc.get_debug ()
                then
                  Printf.eprintf
                    "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                    "mode" "__x907__" (Rpc.to_string __x__)
                    "Enum[String s;...]"
                else ();
                raise (Rpc.Runtime_error ("Enum[String s;...]", __x__))))
    type ty =
      | CDROM 
      | Disk 
      | Floppy 
    let rec rpc_of_ty =
      function
      | __x910__ ->
          (match __x910__ with
           | Floppy -> Rpc.String "Floppy"
           | Disk -> Rpc.String "Disk"
           | CDROM -> Rpc.String "CDROM")
    and ty_of_rpc =
      function
      | __x909__ ->
          (match Rpc.lowerfn __x909__ with
           | Rpc.String "floppy" -> Floppy
           | Rpc.String "disk" -> Disk
           | Rpc.String "cdrom" -> CDROM
           | __x__ ->
               (if Rpc.get_debug ()
                then
                  Printf.eprintf
                    "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                    "ty" "__x909__" (Rpc.to_string __x__)
                    "Enum[String s;...]"
                else ();
                raise (Rpc.Runtime_error ("Enum[String s;...]", __x__))))
    type id = (string * string)
    let rec rpc_of_id =
      function
      | __x914__ ->
          let (__x915__, __x916__) = __x914__ in
          Rpc.Enum [Rpc.String __x915__; Rpc.String __x916__]
    and id_of_rpc =
      function
      | __x911__ ->
          (match __x911__ with
           | Rpc.Enum (__x912__::__x913__::[]) ->
               (((match __x912__ with
                  | Rpc.String x -> x
                  | __x__ ->
                      (if Rpc.get_debug ()
                       then
                         Printf.eprintf
                           "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                           "id" "__x912__" (Rpc.to_string __x__)
                           "String(string)"
                       else ();
                       raise (Rpc.Runtime_error ("String(string)", __x__))))),
                 ((match __x913__ with
                   | Rpc.String x -> x
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "id" "__x913__" (Rpc.to_string __x__)
                            "String(string)"
                        else ();
                        raise (Rpc.Runtime_error ("String(string)", __x__))))))
           | __x__ ->
               (if Rpc.get_debug ()
                then
                  Printf.eprintf
                    "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                    "id" "__x911__" (Rpc.to_string __x__) "List"
                else ();
                raise (Rpc.Runtime_error ("List", __x__))))
    type qos_class =
      | Highest 
      | High 
      | Normal 
      | Low 
      | Lowest 
      | Other of int 
    let rec rpc_of_qos_class =
      function
      | __x919__ ->
          (match __x919__ with
           | Other __x920__ ->
               Rpc.Enum [Rpc.String "Other"; Rpc.Int (Int64.of_int __x920__)]
           | Lowest -> Rpc.String "Lowest"
           | Low -> Rpc.String "Low"
           | Normal -> Rpc.String "Normal"
           | High -> Rpc.String "High"
           | Highest -> Rpc.String "Highest")
    and qos_class_of_rpc =
      function
      | __x917__ ->
          (match Rpc.lowerfn __x917__ with
           | Rpc.Enum ((Rpc.String "other")::__x918__::[]) ->
               Other
                 ((match __x918__ with
                   | Rpc.Int x -> Int64.to_int x
                   | Rpc.String s -> int_of_string s
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "qos_class" "__x918__" (Rpc.to_string __x__)
                            "Int(int)"
                        else ();
                        raise (Rpc.Runtime_error ("Int(int)", __x__)))))
           | Rpc.String "lowest" -> Lowest
           | Rpc.String "low" -> Low
           | Rpc.String "normal" -> Normal
           | Rpc.String "high" -> High
           | Rpc.String "highest" -> Highest
           | __x__ ->
               (if Rpc.get_debug ()
                then
                  Printf.eprintf
                    "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                    "qos_class" "__x917__" (Rpc.to_string __x__)
                    "Enum[String s;...]"
                else ();
                raise (Rpc.Runtime_error ("Enum[String s;...]", __x__))))
    type qos_scheduler =
      | RealTime of qos_class 
      | Idle 
      | BestEffort of qos_class 
    let rec rpc_of_qos_scheduler =
      function
      | __x924__ ->
          (match __x924__ with
           | BestEffort __x925__ ->
               Rpc.Enum [Rpc.String "BestEffort"; rpc_of_qos_class __x925__]
           | Idle -> Rpc.String "Idle"
           | RealTime __x926__ ->
               Rpc.Enum [Rpc.String "RealTime"; rpc_of_qos_class __x926__])
    and qos_scheduler_of_rpc =
      function
      | __x921__ ->
          (match Rpc.lowerfn __x921__ with
           | Rpc.Enum ((Rpc.String "besteffort")::__x922__::[]) ->
               BestEffort (qos_class_of_rpc __x922__)
           | Rpc.String "idle" -> Idle
           | Rpc.Enum ((Rpc.String "realtime")::__x923__::[]) ->
               RealTime (qos_class_of_rpc __x923__)
           | __x__ ->
               (if Rpc.get_debug ()
                then
                  Printf.eprintf
                    "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                    "qos_scheduler" "__x921__" (Rpc.to_string __x__)
                    "Enum[String s;...]"
                else ();
                raise (Rpc.Runtime_error ("Enum[String s;...]", __x__))))
    type qos =
      | Ionice of qos_scheduler 
    let rec rpc_of_qos =
      function
      | __x929__ ->
          (match __x929__ with
           | Ionice __x930__ ->
               Rpc.Enum [Rpc.String "Ionice"; rpc_of_qos_scheduler __x930__])
    and qos_of_rpc =
      function
      | __x927__ ->
          (match Rpc.lowerfn __x927__ with
           | Rpc.Enum ((Rpc.String "ionice")::__x928__::[]) ->
               Ionice (qos_scheduler_of_rpc __x928__)
           | __x__ ->
               (if Rpc.get_debug ()
                then
                  Printf.eprintf
                    "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                    "qos" "__x927__" (Rpc.to_string __x__)
                    "Enum[String s;...]"
                else ();
                raise (Rpc.Runtime_error ("Enum[String s;...]", __x__))))
    type t =
      {
      id: id ;
      position: Device_number.t option ;
      mode: mode ;
      backend: disk option ;
      ty: ty ;
      unpluggable: bool ;
      extra_backend_keys: (string * string) list ;
      extra_private_keys: (string * string) list ;
      qos: qos option ;
      persistent: bool }
    let rec rpc_of_t =
      function
      | __x948__ ->
          let __x949__ = __x948__.id
          and __x950__ = __x948__.position
          and __x951__ = __x948__.mode
          and __x952__ = __x948__.backend
          and __x953__ = __x948__.ty
          and __x954__ = __x948__.unpluggable
          and __x955__ = __x948__.extra_backend_keys
          and __x956__ = __x948__.extra_private_keys
          and __x957__ = __x948__.qos
          and __x958__ = __x948__.persistent in
          Rpc.Dict (("id", (rpc_of_id __x949__)) ::
            ((match match __x950__ with
                    | Some __x966__ ->
                        Rpc.Enum [Device_number.rpc_of_t __x966__]
                    | None -> Rpc.Enum []
              with
              | Rpc.Enum [] -> ("mode", (rpc_of_mode __x951__)) ::
                  ((match match __x952__ with
                          | Some __x964__ -> Rpc.Enum [rpc_of_disk __x964__]
                          | None -> Rpc.Enum []
                    with
                    | Rpc.Enum [] -> ("ty", (rpc_of_ty __x953__)) ::
                        ("unpluggable", (Rpc.Bool __x954__)) ::
                        ("extra_backend_keys",
                          (let dict =
                             List.map
                               (function
                                | (key, __x962__) ->
                                    (key, (Rpc.String __x962__))) __x955__ in
                           Rpc.Dict dict))
                        ::
                        ("extra_private_keys",
                          (let dict =
                             List.map
                               (function
                                | (key, __x961__) ->
                                    (key, (Rpc.String __x961__))) __x956__ in
                           Rpc.Dict dict))
                        ::
                        ((match match __x957__ with
                                | Some __x960__ ->
                                    Rpc.Enum [rpc_of_qos __x960__]
                                | None -> Rpc.Enum []
                          with
                          | Rpc.Enum [] ->
                              [("persistent", (Rpc.Bool __x958__))]
                          | Rpc.Enum (__x959__::[]) ->
                              [("qos", __x959__);
                              ("persistent", (Rpc.Bool __x958__))]
                          | _ -> assert false))
                    | Rpc.Enum (__x963__::[]) -> ("backend", __x963__) ::
                        ("ty", (rpc_of_ty __x953__)) ::
                        ("unpluggable", (Rpc.Bool __x954__)) ::
                        ("extra_backend_keys",
                          (let dict =
                             List.map
                               (function
                                | (key, __x962__) ->
                                    (key, (Rpc.String __x962__))) __x955__ in
                           Rpc.Dict dict))
                        ::
                        ("extra_private_keys",
                          (let dict =
                             List.map
                               (function
                                | (key, __x961__) ->
                                    (key, (Rpc.String __x961__))) __x956__ in
                           Rpc.Dict dict))
                        ::
                        ((match match __x957__ with
                                | Some __x960__ ->
                                    Rpc.Enum [rpc_of_qos __x960__]
                                | None -> Rpc.Enum []
                          with
                          | Rpc.Enum [] ->
                              [("persistent", (Rpc.Bool __x958__))]
                          | Rpc.Enum (__x959__::[]) ->
                              [("qos", __x959__);
                              ("persistent", (Rpc.Bool __x958__))]
                          | _ -> assert false))
                    | _ -> assert false))
              | Rpc.Enum (__x965__::[]) -> ("position", __x965__) ::
                  ("mode", (rpc_of_mode __x951__)) ::
                  ((match match __x952__ with
                          | Some __x964__ -> Rpc.Enum [rpc_of_disk __x964__]
                          | None -> Rpc.Enum []
                    with
                    | Rpc.Enum [] -> ("ty", (rpc_of_ty __x953__)) ::
                        ("unpluggable", (Rpc.Bool __x954__)) ::
                        ("extra_backend_keys",
                          (let dict =
                             List.map
                               (function
                                | (key, __x962__) ->
                                    (key, (Rpc.String __x962__))) __x955__ in
                           Rpc.Dict dict))
                        ::
                        ("extra_private_keys",
                          (let dict =
                             List.map
                               (function
                                | (key, __x961__) ->
                                    (key, (Rpc.String __x961__))) __x956__ in
                           Rpc.Dict dict))
                        ::
                        ((match match __x957__ with
                                | Some __x960__ ->
                                    Rpc.Enum [rpc_of_qos __x960__]
                                | None -> Rpc.Enum []
                          with
                          | Rpc.Enum [] ->
                              [("persistent", (Rpc.Bool __x958__))]
                          | Rpc.Enum (__x959__::[]) ->
                              [("qos", __x959__);
                              ("persistent", (Rpc.Bool __x958__))]
                          | _ -> assert false))
                    | Rpc.Enum (__x963__::[]) -> ("backend", __x963__) ::
                        ("ty", (rpc_of_ty __x953__)) ::
                        ("unpluggable", (Rpc.Bool __x954__)) ::
                        ("extra_backend_keys",
                          (let dict =
                             List.map
                               (function
                                | (key, __x962__) ->
                                    (key, (Rpc.String __x962__))) __x955__ in
                           Rpc.Dict dict))
                        ::
                        ("extra_private_keys",
                          (let dict =
                             List.map
                               (function
                                | (key, __x961__) ->
                                    (key, (Rpc.String __x961__))) __x956__ in
                           Rpc.Dict dict))
                        ::
                        ((match match __x957__ with
                                | Some __x960__ ->
                                    Rpc.Enum [rpc_of_qos __x960__]
                                | None -> Rpc.Enum []
                          with
                          | Rpc.Enum [] ->
                              [("persistent", (Rpc.Bool __x958__))]
                          | Rpc.Enum (__x959__::[]) ->
                              [("qos", __x959__);
                              ("persistent", (Rpc.Bool __x958__))]
                          | _ -> assert false))
                    | _ -> assert false))
              | _ -> assert false)))
    and t_of_rpc =
      function
      | __x931__ ->
          (match __x931__ with
           | Rpc.Dict __x932__ ->
               let __x933__ =
                 try List.assoc "id" __x932__
                 with
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                          "t" "__x932__" (Printexc.to_string __x__)
                          "Looking for key id"
                      else ();
                      raise
                        (Rpc.Runtime_exception
                           ("Looking for key id", (Printexc.to_string __x__))))
               and __x934__ =
                 if List.mem_assoc "position" __x932__
                 then Rpc.Enum [List.assoc "position" __x932__]
                 else Rpc.Enum []
               and __x935__ =
                 try List.assoc "mode" __x932__
                 with
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                          "t" "__x932__" (Printexc.to_string __x__)
                          "Looking for key mode"
                      else ();
                      raise
                        (Rpc.Runtime_exception
                           ("Looking for key mode",
                             (Printexc.to_string __x__))))
               and __x936__ =
                 if List.mem_assoc "backend" __x932__
                 then Rpc.Enum [List.assoc "backend" __x932__]
                 else Rpc.Enum []
               and __x937__ =
                 try List.assoc "ty" __x932__
                 with
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                          "t" "__x932__" (Printexc.to_string __x__)
                          "Looking for key ty"
                      else ();
                      raise
                        (Rpc.Runtime_exception
                           ("Looking for key ty", (Printexc.to_string __x__))))
               and __x938__ =
                 try List.assoc "unpluggable" __x932__
                 with
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                          "t" "__x932__" (Printexc.to_string __x__)
                          "Looking for key unpluggable"
                      else ();
                      raise
                        (Rpc.Runtime_exception
                           ("Looking for key unpluggable",
                             (Printexc.to_string __x__))))
               and __x939__ =
                 try List.assoc "extra_backend_keys" __x932__
                 with
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                          "t" "__x932__" (Printexc.to_string __x__)
                          "Looking for key extra_backend_keys"
                      else ();
                      raise
                        (Rpc.Runtime_exception
                           ("Looking for key extra_backend_keys",
                             (Printexc.to_string __x__))))
               and __x940__ =
                 try List.assoc "extra_private_keys" __x932__
                 with
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                          "t" "__x932__" (Printexc.to_string __x__)
                          "Looking for key extra_private_keys"
                      else ();
                      raise
                        (Rpc.Runtime_exception
                           ("Looking for key extra_private_keys",
                             (Printexc.to_string __x__))))
               and __x941__ =
                 if List.mem_assoc "qos" __x932__
                 then Rpc.Enum [List.assoc "qos" __x932__]
                 else Rpc.Enum []
               and __x942__ =
                 try List.assoc "persistent" __x932__
                 with
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                          "t" "__x932__" (Printexc.to_string __x__)
                          "Looking for key persistent"
                      else ();
                      raise
                        (Rpc.Runtime_exception
                           ("Looking for key persistent",
                             (Printexc.to_string __x__)))) in
               {
                 id = (id_of_rpc __x933__);
                 position =
                   ((match __x934__ with
                     | Rpc.Enum [] -> None
                     | Rpc.Enum (__x943__::[]) ->
                         Some (Device_number.t_of_rpc __x943__)
                     | __x__ ->
                         (if Rpc.get_debug ()
                          then
                            Printf.eprintf
                              "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                              "t" "__x934__" (Rpc.to_string __x__)
                              "Enum[]/Enum[_]"
                          else ();
                          raise (Rpc.Runtime_error ("Enum[]/Enum[_]", __x__)))));
                 mode = (mode_of_rpc __x935__);
                 backend =
                   ((match __x936__ with
                     | Rpc.Enum [] -> None
                     | Rpc.Enum (__x944__::[]) -> Some (disk_of_rpc __x944__)
                     | __x__ ->
                         (if Rpc.get_debug ()
                          then
                            Printf.eprintf
                              "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                              "t" "__x936__" (Rpc.to_string __x__)
                              "Enum[]/Enum[_]"
                          else ();
                          raise (Rpc.Runtime_error ("Enum[]/Enum[_]", __x__)))));
                 ty = (ty_of_rpc __x937__);
                 unpluggable =
                   ((match __x938__ with
                     | Rpc.Bool x -> x
                     | __x__ ->
                         (if Rpc.get_debug ()
                          then
                            Printf.eprintf
                              "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                              "t" "__x938__" (Rpc.to_string __x__) "Bool"
                          else ();
                          raise (Rpc.Runtime_error ("Bool", __x__)))));
                 extra_backend_keys =
                   ((match __x939__ with
                     | Rpc.Dict d ->
                         List.map
                           (function
                            | (key, __x945__) ->
                                (key,
                                  ((match __x945__ with
                                    | Rpc.String x -> x
                                    | __x__ ->
                                        (if Rpc.get_debug ()
                                         then
                                           Printf.eprintf
                                             "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                             "t" "__x945__"
                                             (Rpc.to_string __x__)
                                             "String(string)"
                                         else ();
                                         raise
                                           (Rpc.Runtime_error
                                              ("String(string)", __x__)))))))
                           d
                     | __x__ ->
                         (if Rpc.get_debug ()
                          then
                            Printf.eprintf
                              "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                              "t" "__x939__" (Rpc.to_string __x__) "Dict"
                          else ();
                          raise (Rpc.Runtime_error ("Dict", __x__)))));
                 extra_private_keys =
                   ((match __x940__ with
                     | Rpc.Dict d ->
                         List.map
                           (function
                            | (key, __x946__) ->
                                (key,
                                  ((match __x946__ with
                                    | Rpc.String x -> x
                                    | __x__ ->
                                        (if Rpc.get_debug ()
                                         then
                                           Printf.eprintf
                                             "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                             "t" "__x946__"
                                             (Rpc.to_string __x__)
                                             "String(string)"
                                         else ();
                                         raise
                                           (Rpc.Runtime_error
                                              ("String(string)", __x__)))))))
                           d
                     | __x__ ->
                         (if Rpc.get_debug ()
                          then
                            Printf.eprintf
                              "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                              "t" "__x940__" (Rpc.to_string __x__) "Dict"
                          else ();
                          raise (Rpc.Runtime_error ("Dict", __x__)))));
                 qos =
                   ((match __x941__ with
                     | Rpc.Enum [] -> None
                     | Rpc.Enum (__x947__::[]) -> Some (qos_of_rpc __x947__)
                     | __x__ ->
                         (if Rpc.get_debug ()
                          then
                            Printf.eprintf
                              "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                              "t" "__x941__" (Rpc.to_string __x__)
                              "Enum[]/Enum[_]"
                          else ();
                          raise (Rpc.Runtime_error ("Enum[]/Enum[_]", __x__)))));
                 persistent =
                   ((match __x942__ with
                     | Rpc.Bool x -> x
                     | __x__ ->
                         (if Rpc.get_debug ()
                          then
                            Printf.eprintf
                              "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                              "t" "__x942__" (Rpc.to_string __x__) "Bool"
                          else ();
                          raise (Rpc.Runtime_error ("Bool", __x__)))))
               }
           | __x__ ->
               (if Rpc.get_debug ()
                then
                  Printf.eprintf
                    "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                    "t" "__x931__" (Rpc.to_string __x__) "Dict"
                else ();
                raise (Rpc.Runtime_error ("Dict", __x__))))
    let default_t =
      {
        id = ("", "");
        position = None;
        mode = ReadWrite;
        backend = None;
        ty = Disk;
        unpluggable = true;
        extra_backend_keys = [];
        extra_private_keys = [];
        qos = None;
        persistent = true
      }
    let t_of_rpc =
      function
      | rpc -> (Rpc.struct_extend rpc (rpc_of_t default_t)) |> t_of_rpc
    type state =
      {
      active: bool ;
      plugged: bool ;
      qos_target: qos option ;
      backend_present: disk option }
    let rec rpc_of_state =
      function
      | __x975__ ->
          let __x976__ = __x975__.active
          and __x977__ = __x975__.plugged
          and __x978__ = __x975__.qos_target
          and __x979__ = __x975__.backend_present in
          Rpc.Dict (("active", (Rpc.Bool __x976__)) ::
            ("plugged", (Rpc.Bool __x977__)) ::
            ((match match __x978__ with
                    | Some __x983__ -> Rpc.Enum [rpc_of_qos __x983__]
                    | None -> Rpc.Enum []
              with
              | Rpc.Enum [] ->
                  (match match __x979__ with
                         | Some __x981__ -> Rpc.Enum [rpc_of_disk __x981__]
                         | None -> Rpc.Enum []
                   with
                   | Rpc.Enum [] -> []
                   | Rpc.Enum (__x980__::[]) ->
                       [("backend_present", __x980__)]
                   | _ -> assert false)
              | Rpc.Enum (__x982__::[]) -> ("qos_target", __x982__) ::
                  ((match match __x979__ with
                          | Some __x981__ -> Rpc.Enum [rpc_of_disk __x981__]
                          | None -> Rpc.Enum []
                    with
                    | Rpc.Enum [] -> []
                    | Rpc.Enum (__x980__::[]) ->
                        [("backend_present", __x980__)]
                    | _ -> assert false))
              | _ -> assert false)))
    and state_of_rpc =
      function
      | __x967__ ->
          (match __x967__ with
           | Rpc.Dict __x968__ ->
               let __x969__ =
                 try List.assoc "active" __x968__
                 with
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                          "state" "__x968__" (Printexc.to_string __x__)
                          "Looking for key active"
                      else ();
                      raise
                        (Rpc.Runtime_exception
                           ("Looking for key active",
                             (Printexc.to_string __x__))))
               and __x970__ =
                 try List.assoc "plugged" __x968__
                 with
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                          "state" "__x968__" (Printexc.to_string __x__)
                          "Looking for key plugged"
                      else ();
                      raise
                        (Rpc.Runtime_exception
                           ("Looking for key plugged",
                             (Printexc.to_string __x__))))
               and __x971__ =
                 if List.mem_assoc "qos_target" __x968__
                 then Rpc.Enum [List.assoc "qos_target" __x968__]
                 else Rpc.Enum []
               and __x972__ =
                 if List.mem_assoc "backend_present" __x968__
                 then Rpc.Enum [List.assoc "backend_present" __x968__]
                 else Rpc.Enum [] in
               {
                 active =
                   ((match __x969__ with
                     | Rpc.Bool x -> x
                     | __x__ ->
                         (if Rpc.get_debug ()
                          then
                            Printf.eprintf
                              "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                              "state" "__x969__" (Rpc.to_string __x__) "Bool"
                          else ();
                          raise (Rpc.Runtime_error ("Bool", __x__)))));
                 plugged =
                   ((match __x970__ with
                     | Rpc.Bool x -> x
                     | __x__ ->
                         (if Rpc.get_debug ()
                          then
                            Printf.eprintf
                              "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                              "state" "__x970__" (Rpc.to_string __x__) "Bool"
                          else ();
                          raise (Rpc.Runtime_error ("Bool", __x__)))));
                 qos_target =
                   ((match __x971__ with
                     | Rpc.Enum [] -> None
                     | Rpc.Enum (__x973__::[]) -> Some (qos_of_rpc __x973__)
                     | __x__ ->
                         (if Rpc.get_debug ()
                          then
                            Printf.eprintf
                              "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                              "state" "__x971__" (Rpc.to_string __x__)
                              "Enum[]/Enum[_]"
                          else ();
                          raise (Rpc.Runtime_error ("Enum[]/Enum[_]", __x__)))));
                 backend_present =
                   ((match __x972__ with
                     | Rpc.Enum [] -> None
                     | Rpc.Enum (__x974__::[]) -> Some (disk_of_rpc __x974__)
                     | __x__ ->
                         (if Rpc.get_debug ()
                          then
                            Printf.eprintf
                              "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                              "state" "__x972__" (Rpc.to_string __x__)
                              "Enum[]/Enum[_]"
                          else ();
                          raise (Rpc.Runtime_error ("Enum[]/Enum[_]", __x__)))))
               }
           | __x__ ->
               (if Rpc.get_debug ()
                then
                  Printf.eprintf
                    "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                    "state" "__x967__" (Rpc.to_string __x__) "Dict"
                else ();
                raise (Rpc.Runtime_error ("Dict", __x__))))
  end
module Vif =
  struct
    type id = (string * string)
    let rec rpc_of_id =
      function
      | __x987__ ->
          let (__x988__, __x989__) = __x987__ in
          Rpc.Enum [Rpc.String __x988__; Rpc.String __x989__]
    and id_of_rpc =
      function
      | __x984__ ->
          (match __x984__ with
           | Rpc.Enum (__x985__::__x986__::[]) ->
               (((match __x985__ with
                  | Rpc.String x -> x
                  | __x__ ->
                      (if Rpc.get_debug ()
                       then
                         Printf.eprintf
                           "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                           "id" "__x985__" (Rpc.to_string __x__)
                           "String(string)"
                       else ();
                       raise (Rpc.Runtime_error ("String(string)", __x__))))),
                 ((match __x986__ with
                   | Rpc.String x -> x
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "id" "__x986__" (Rpc.to_string __x__)
                            "String(string)"
                        else ();
                        raise (Rpc.Runtime_error ("String(string)", __x__))))))
           | __x__ ->
               (if Rpc.get_debug ()
                then
                  Printf.eprintf
                    "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                    "id" "__x984__" (Rpc.to_string __x__) "List"
                else ();
                raise (Rpc.Runtime_error ("List", __x__))))
    type ipv4_configuration =
      | Unspecified4 
      | Static4 of string list * string option 
    let rec rpc_of_ipv4_configuration =
      function
      | __x996__ ->
          (match __x996__ with
           | Static4 (__x997__, __x998__) ->
               Rpc.Enum
                 [Rpc.String "Static4";
                 Rpc.Enum
                   (List.map (function | __x999__ -> Rpc.String __x999__)
                      __x997__);
                 (match __x998__ with
                  | Some __x1000__ -> Rpc.Enum [Rpc.String __x1000__]
                  | None -> Rpc.Enum [])]
           | Unspecified4 -> Rpc.String "Unspecified4")
    and ipv4_configuration_of_rpc =
      function
      | __x990__ ->
          (match Rpc.lowerfn __x990__ with
           | Rpc.Enum ((Rpc.String "static4")::__x991__::__x992__::[]) ->
               Static4
                 (((match __x991__ with
                    | Rpc.Enum __x993__ ->
                        List.map
                          (function
                           | __x994__ ->
                               (match __x994__ with
                                | Rpc.String x -> x
                                | __x__ ->
                                    (if Rpc.get_debug ()
                                     then
                                       Printf.eprintf
                                         "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                         "ipv4_configuration" "__x994__"
                                         (Rpc.to_string __x__)
                                         "String(string)"
                                     else ();
                                     raise
                                       (Rpc.Runtime_error
                                          ("String(string)", __x__)))))
                          __x993__
                    | __x__ ->
                        (if Rpc.get_debug ()
                         then
                           Printf.eprintf
                             "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                             "ipv4_configuration" "__x991__"
                             (Rpc.to_string __x__) "List"
                         else ();
                         raise (Rpc.Runtime_error ("List", __x__))))),
                   ((match __x992__ with
                     | Rpc.Enum [] -> None
                     | Rpc.Enum (__x995__::[]) ->
                         Some
                           ((match __x995__ with
                             | Rpc.String x -> x
                             | __x__ ->
                                 (if Rpc.get_debug ()
                                  then
                                    Printf.eprintf
                                      "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                      "ipv4_configuration" "__x995__"
                                      (Rpc.to_string __x__) "String(string)"
                                  else ();
                                  raise
                                    (Rpc.Runtime_error
                                       ("String(string)", __x__)))))
                     | __x__ ->
                         (if Rpc.get_debug ()
                          then
                            Printf.eprintf
                              "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                              "ipv4_configuration" "__x992__"
                              (Rpc.to_string __x__) "Enum[]/Enum[_]"
                          else ();
                          raise (Rpc.Runtime_error ("Enum[]/Enum[_]", __x__))))))
           | Rpc.String "unspecified4" -> Unspecified4
           | __x__ ->
               (if Rpc.get_debug ()
                then
                  Printf.eprintf
                    "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                    "ipv4_configuration" "__x990__" (Rpc.to_string __x__)
                    "Enum[String s;...]"
                else ();
                raise (Rpc.Runtime_error ("Enum[String s;...]", __x__))))
    let default_ipv4_configuration = Unspecified4
    type ipv6_configuration =
      | Unspecified6 
      | Static6 of string list * string option 
    let rec rpc_of_ipv6_configuration =
      function
      | __x1007__ ->
          (match __x1007__ with
           | Static6 (__x1008__, __x1009__) ->
               Rpc.Enum
                 [Rpc.String "Static6";
                 Rpc.Enum
                   (List.map (function | __x1010__ -> Rpc.String __x1010__)
                      __x1008__);
                 (match __x1009__ with
                  | Some __x1011__ -> Rpc.Enum [Rpc.String __x1011__]
                  | None -> Rpc.Enum [])]
           | Unspecified6 -> Rpc.String "Unspecified6")
    and ipv6_configuration_of_rpc =
      function
      | __x1001__ ->
          (match Rpc.lowerfn __x1001__ with
           | Rpc.Enum ((Rpc.String "static6")::__x1002__::__x1003__::[]) ->
               Static6
                 (((match __x1002__ with
                    | Rpc.Enum __x1004__ ->
                        List.map
                          (function
                           | __x1005__ ->
                               (match __x1005__ with
                                | Rpc.String x -> x
                                | __x__ ->
                                    (if Rpc.get_debug ()
                                     then
                                       Printf.eprintf
                                         "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                         "ipv6_configuration" "__x1005__"
                                         (Rpc.to_string __x__)
                                         "String(string)"
                                     else ();
                                     raise
                                       (Rpc.Runtime_error
                                          ("String(string)", __x__)))))
                          __x1004__
                    | __x__ ->
                        (if Rpc.get_debug ()
                         then
                           Printf.eprintf
                             "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                             "ipv6_configuration" "__x1002__"
                             (Rpc.to_string __x__) "List"
                         else ();
                         raise (Rpc.Runtime_error ("List", __x__))))),
                   ((match __x1003__ with
                     | Rpc.Enum [] -> None
                     | Rpc.Enum (__x1006__::[]) ->
                         Some
                           ((match __x1006__ with
                             | Rpc.String x -> x
                             | __x__ ->
                                 (if Rpc.get_debug ()
                                  then
                                    Printf.eprintf
                                      "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                      "ipv6_configuration" "__x1006__"
                                      (Rpc.to_string __x__) "String(string)"
                                  else ();
                                  raise
                                    (Rpc.Runtime_error
                                       ("String(string)", __x__)))))
                     | __x__ ->
                         (if Rpc.get_debug ()
                          then
                            Printf.eprintf
                              "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                              "ipv6_configuration" "__x1003__"
                              (Rpc.to_string __x__) "Enum[]/Enum[_]"
                          else ();
                          raise (Rpc.Runtime_error ("Enum[]/Enum[_]", __x__))))))
           | Rpc.String "unspecified6" -> Unspecified6
           | __x__ ->
               (if Rpc.get_debug ()
                then
                  Printf.eprintf
                    "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                    "ipv6_configuration" "__x1001__" (Rpc.to_string __x__)
                    "Enum[String s;...]"
                else ();
                raise (Rpc.Runtime_error ("Enum[String s;...]", __x__))))
    let default_ipv6_configuration = Unspecified6
    type locked_addresses = {
      ipv4: string list ;
      ipv6: string list }
    let rec rpc_of_locked_addresses =
      function
      | __x1020__ ->
          let __x1021__ = __x1020__.ipv4
          and __x1022__ = __x1020__.ipv6 in
          Rpc.Dict
            [("ipv4",
               (Rpc.Enum
                  (List.map (function | __x1024__ -> Rpc.String __x1024__)
                     __x1021__)));
            ("ipv6",
              (Rpc.Enum
                 (List.map (function | __x1023__ -> Rpc.String __x1023__)
                    __x1022__)))]
    and locked_addresses_of_rpc =
      function
      | __x1012__ ->
          (match __x1012__ with
           | Rpc.Dict __x1013__ ->
               let __x1014__ =
                 try List.assoc "ipv4" __x1013__
                 with
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                          "locked_addresses" "__x1013__"
                          (Printexc.to_string __x__) "Looking for key ipv4"
                      else ();
                      raise
                        (Rpc.Runtime_exception
                           ("Looking for key ipv4",
                             (Printexc.to_string __x__))))
               and __x1015__ =
                 try List.assoc "ipv6" __x1013__
                 with
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                          "locked_addresses" "__x1013__"
                          (Printexc.to_string __x__) "Looking for key ipv6"
                      else ();
                      raise
                        (Rpc.Runtime_exception
                           ("Looking for key ipv6",
                             (Printexc.to_string __x__)))) in
               {
                 ipv4 =
                   ((match __x1014__ with
                     | Rpc.Enum __x1016__ ->
                         List.map
                           (function
                            | __x1017__ ->
                                (match __x1017__ with
                                 | Rpc.String x -> x
                                 | __x__ ->
                                     (if Rpc.get_debug ()
                                      then
                                        Printf.eprintf
                                          "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                          "locked_addresses" "__x1017__"
                                          (Rpc.to_string __x__)
                                          "String(string)"
                                      else ();
                                      raise
                                        (Rpc.Runtime_error
                                           ("String(string)", __x__)))))
                           __x1016__
                     | __x__ ->
                         (if Rpc.get_debug ()
                          then
                            Printf.eprintf
                              "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                              "locked_addresses" "__x1014__"
                              (Rpc.to_string __x__) "List"
                          else ();
                          raise (Rpc.Runtime_error ("List", __x__)))));
                 ipv6 =
                   ((match __x1015__ with
                     | Rpc.Enum __x1018__ ->
                         List.map
                           (function
                            | __x1019__ ->
                                (match __x1019__ with
                                 | Rpc.String x -> x
                                 | __x__ ->
                                     (if Rpc.get_debug ()
                                      then
                                        Printf.eprintf
                                          "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                          "locked_addresses" "__x1019__"
                                          (Rpc.to_string __x__)
                                          "String(string)"
                                      else ();
                                      raise
                                        (Rpc.Runtime_error
                                           ("String(string)", __x__)))))
                           __x1018__
                     | __x__ ->
                         (if Rpc.get_debug ()
                          then
                            Printf.eprintf
                              "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                              "locked_addresses" "__x1015__"
                              (Rpc.to_string __x__) "List"
                          else ();
                          raise (Rpc.Runtime_error ("List", __x__)))))
               }
           | __x__ ->
               (if Rpc.get_debug ()
                then
                  Printf.eprintf
                    "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                    "locked_addresses" "__x1012__" (Rpc.to_string __x__)
                    "Dict"
                else ();
                raise (Rpc.Runtime_error ("Dict", __x__))))
    type locking_mode =
      | Unlocked 
      | Disabled 
      | Locked of locked_addresses 
    let rec rpc_of_locking_mode =
      function
      | __x1027__ ->
          (match __x1027__ with
           | Locked __x1028__ ->
               Rpc.Enum
                 [Rpc.String "Locked"; rpc_of_locked_addresses __x1028__]
           | Disabled -> Rpc.String "Disabled"
           | Unlocked -> Rpc.String "Unlocked")
    and locking_mode_of_rpc =
      function
      | __x1025__ ->
          (match Rpc.lowerfn __x1025__ with
           | Rpc.Enum ((Rpc.String "locked")::__x1026__::[]) ->
               Locked (locked_addresses_of_rpc __x1026__)
           | Rpc.String "disabled" -> Disabled
           | Rpc.String "unlocked" -> Unlocked
           | __x__ ->
               (if Rpc.get_debug ()
                then
                  Printf.eprintf
                    "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                    "locking_mode" "__x1025__" (Rpc.to_string __x__)
                    "Enum[String s;...]"
                else ();
                raise (Rpc.Runtime_error ("Enum[String s;...]", __x__))))
    let default_locking_mode = Unlocked
    module PVS_proxy =
      struct
        type site = string
        let rec rpc_of_site = function | __x1030__ -> Rpc.String __x1030__
        and site_of_rpc =
          function
          | __x1029__ ->
              (match __x1029__ with
               | Rpc.String x -> x
               | __x__ ->
                   (if Rpc.get_debug ()
                    then
                      Printf.eprintf
                        "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                        "site" "__x1029__" (Rpc.to_string __x__)
                        "String(string)"
                    else ();
                    raise (Rpc.Runtime_error ("String(string)", __x__))))
        type server =
          {
          addresses: string list ;
          first_port: int ;
          last_port: int }
        let rec rpc_of_server =
          function
          | __x1038__ ->
              let __x1039__ = __x1038__.addresses
              and __x1040__ = __x1038__.first_port
              and __x1041__ = __x1038__.last_port in
              Rpc.Dict
                [("addresses",
                   (Rpc.Enum
                      (List.map
                         (function | __x1042__ -> Rpc.String __x1042__)
                         __x1039__)));
                ("first_port", (Rpc.Int (Int64.of_int __x1040__)));
                ("last_port", (Rpc.Int (Int64.of_int __x1041__)))]
        and server_of_rpc =
          function
          | __x1031__ ->
              (match __x1031__ with
               | Rpc.Dict __x1032__ ->
                   let __x1033__ =
                     try List.assoc "addresses" __x1032__
                     with
                     | __x__ ->
                         (if Rpc.get_debug ()
                          then
                            Printf.eprintf
                              "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                              "server" "__x1032__" (Printexc.to_string __x__)
                              "Looking for key addresses"
                          else ();
                          raise
                            (Rpc.Runtime_exception
                               ("Looking for key addresses",
                                 (Printexc.to_string __x__))))
                   and __x1034__ =
                     try List.assoc "first_port" __x1032__
                     with
                     | __x__ ->
                         (if Rpc.get_debug ()
                          then
                            Printf.eprintf
                              "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                              "server" "__x1032__" (Printexc.to_string __x__)
                              "Looking for key first_port"
                          else ();
                          raise
                            (Rpc.Runtime_exception
                               ("Looking for key first_port",
                                 (Printexc.to_string __x__))))
                   and __x1035__ =
                     try List.assoc "last_port" __x1032__
                     with
                     | __x__ ->
                         (if Rpc.get_debug ()
                          then
                            Printf.eprintf
                              "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                              "server" "__x1032__" (Printexc.to_string __x__)
                              "Looking for key last_port"
                          else ();
                          raise
                            (Rpc.Runtime_exception
                               ("Looking for key last_port",
                                 (Printexc.to_string __x__)))) in
                   {
                     addresses =
                       ((match __x1033__ with
                         | Rpc.Enum __x1036__ ->
                             List.map
                               (function
                                | __x1037__ ->
                                    (match __x1037__ with
                                     | Rpc.String x -> x
                                     | __x__ ->
                                         (if Rpc.get_debug ()
                                          then
                                            Printf.eprintf
                                              "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                              "server" "__x1037__"
                                              (Rpc.to_string __x__)
                                              "String(string)"
                                          else ();
                                          raise
                                            (Rpc.Runtime_error
                                               ("String(string)", __x__)))))
                               __x1036__
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                  "server" "__x1033__" (Rpc.to_string __x__)
                                  "List"
                              else ();
                              raise (Rpc.Runtime_error ("List", __x__)))));
                     first_port =
                       ((match __x1034__ with
                         | Rpc.Int x -> Int64.to_int x
                         | Rpc.String s -> int_of_string s
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                  "server" "__x1034__" (Rpc.to_string __x__)
                                  "Int(int)"
                              else ();
                              raise (Rpc.Runtime_error ("Int(int)", __x__)))));
                     last_port =
                       ((match __x1035__ with
                         | Rpc.Int x -> Int64.to_int x
                         | Rpc.String s -> int_of_string s
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                  "server" "__x1035__" (Rpc.to_string __x__)
                                  "Int(int)"
                              else ();
                              raise (Rpc.Runtime_error ("Int(int)", __x__)))))
                   }
               | __x__ ->
                   (if Rpc.get_debug ()
                    then
                      Printf.eprintf
                        "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                        "server" "__x1031__" (Rpc.to_string __x__) "Dict"
                    else ();
                    raise (Rpc.Runtime_error ("Dict", __x__))))
        type interface = string
        let rec rpc_of_interface =
          function | __x1044__ -> Rpc.String __x1044__
        and interface_of_rpc =
          function
          | __x1043__ ->
              (match __x1043__ with
               | Rpc.String x -> x
               | __x__ ->
                   (if Rpc.get_debug ()
                    then
                      Printf.eprintf
                        "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                        "interface" "__x1043__" (Rpc.to_string __x__)
                        "String(string)"
                    else ();
                    raise (Rpc.Runtime_error ("String(string)", __x__))))
        type t = (site * server list * interface)
        let rec rpc_of_t =
          function
          | __x1051__ ->
              let (__x1052__, __x1053__, __x1054__) = __x1051__ in
              Rpc.Enum
                [rpc_of_site __x1052__;
                Rpc.Enum
                  (List.map (function | __x1055__ -> rpc_of_server __x1055__)
                     __x1053__);
                rpc_of_interface __x1054__]
        and t_of_rpc =
          function
          | __x1045__ ->
              (match __x1045__ with
               | Rpc.Enum (__x1046__::__x1047__::__x1048__::[]) ->
                   ((site_of_rpc __x1046__),
                     ((match __x1047__ with
                       | Rpc.Enum __x1049__ ->
                           List.map
                             (function | __x1050__ -> server_of_rpc __x1050__)
                             __x1049__
                       | __x__ ->
                           (if Rpc.get_debug ()
                            then
                              Printf.eprintf
                                "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                "t" "__x1047__" (Rpc.to_string __x__) "List"
                            else ();
                            raise (Rpc.Runtime_error ("List", __x__))))),
                     (interface_of_rpc __x1048__))
               | __x__ ->
                   (if Rpc.get_debug ()
                    then
                      Printf.eprintf
                        "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                        "t" "__x1045__" (Rpc.to_string __x__) "List"
                    else ();
                    raise (Rpc.Runtime_error ("List", __x__))))
      end
    type t =
      {
      id: id ;
      position: int ;
      mac: string ;
      carrier: bool ;
      mtu: int ;
      rate: (int64 * int64) option ;
      backend: Network.t ;
      other_config: (string * string) list ;
      locking_mode: locking_mode ;
      extra_private_keys: (string * string) list ;
      ipv4_configuration: ipv4_configuration ;
      ipv6_configuration: ipv6_configuration ;
      pvs_proxy: PVS_proxy.t option ;
      vlan: int64 option }
    let rec rpc_of_t =
      function
      | __x1079__ ->
          let __x1080__ = __x1079__.id
          and __x1081__ = __x1079__.position
          and __x1082__ = __x1079__.mac
          and __x1083__ = __x1079__.carrier
          and __x1084__ = __x1079__.mtu
          and __x1085__ = __x1079__.rate
          and __x1086__ = __x1079__.backend
          and __x1087__ = __x1079__.other_config
          and __x1088__ = __x1079__.locking_mode
          and __x1089__ = __x1079__.extra_private_keys
          and __x1090__ = __x1079__.ipv4_configuration
          and __x1091__ = __x1079__.ipv6_configuration
          and __x1092__ = __x1079__.pvs_proxy
          and __x1093__ = __x1079__.vlan in
          Rpc.Dict (("id", (rpc_of_id __x1080__)) ::
            ("position", (Rpc.Int (Int64.of_int __x1081__))) ::
            ("mac", (Rpc.String __x1082__)) ::
            ("carrier", (Rpc.Bool __x1083__)) ::
            ("mtu", (Rpc.Int (Int64.of_int __x1084__))) ::
            ((match match __x1085__ with
                    | Some __x1101__ ->
                        Rpc.Enum
                          [(let (__x1102__, __x1103__) = __x1101__ in
                            Rpc.Enum [Rpc.Int __x1102__; Rpc.Int __x1103__])]
                    | None -> Rpc.Enum []
              with
              | Rpc.Enum [] -> ("backend", (Network.rpc_of_t __x1086__)) ::
                  ("other_config",
                    (let dict =
                       List.map
                         (function
                          | (key, __x1099__) -> (key, (Rpc.String __x1099__)))
                         __x1087__ in
                     Rpc.Dict dict))
                  :: ("locking_mode", (rpc_of_locking_mode __x1088__)) ::
                  ("extra_private_keys",
                    (let dict =
                       List.map
                         (function
                          | (key, __x1098__) -> (key, (Rpc.String __x1098__)))
                         __x1089__ in
                     Rpc.Dict dict))
                  ::
                  ("ipv4_configuration",
                    (rpc_of_ipv4_configuration __x1090__))
                  ::
                  ("ipv6_configuration",
                    (rpc_of_ipv6_configuration __x1091__))
                  ::
                  ((match match __x1092__ with
                          | Some __x1097__ ->
                              Rpc.Enum [PVS_proxy.rpc_of_t __x1097__]
                          | None -> Rpc.Enum []
                    with
                    | Rpc.Enum [] ->
                        (match match __x1093__ with
                               | Some __x1095__ ->
                                   Rpc.Enum [Rpc.Int __x1095__]
                               | None -> Rpc.Enum []
                         with
                         | Rpc.Enum [] -> []
                         | Rpc.Enum (__x1094__::[]) -> [("vlan", __x1094__)]
                         | _ -> assert false)
                    | Rpc.Enum (__x1096__::[]) -> ("pvs_proxy", __x1096__) ::
                        ((match match __x1093__ with
                                | Some __x1095__ ->
                                    Rpc.Enum [Rpc.Int __x1095__]
                                | None -> Rpc.Enum []
                          with
                          | Rpc.Enum [] -> []
                          | Rpc.Enum (__x1094__::[]) -> [("vlan", __x1094__)]
                          | _ -> assert false))
                    | _ -> assert false))
              | Rpc.Enum (__x1100__::[]) -> ("rate", __x1100__) ::
                  ("backend", (Network.rpc_of_t __x1086__)) ::
                  ("other_config",
                    (let dict =
                       List.map
                         (function
                          | (key, __x1099__) -> (key, (Rpc.String __x1099__)))
                         __x1087__ in
                     Rpc.Dict dict))
                  :: ("locking_mode", (rpc_of_locking_mode __x1088__)) ::
                  ("extra_private_keys",
                    (let dict =
                       List.map
                         (function
                          | (key, __x1098__) -> (key, (Rpc.String __x1098__)))
                         __x1089__ in
                     Rpc.Dict dict))
                  ::
                  ("ipv4_configuration",
                    (rpc_of_ipv4_configuration __x1090__))
                  ::
                  ("ipv6_configuration",
                    (rpc_of_ipv6_configuration __x1091__))
                  ::
                  ((match match __x1092__ with
                          | Some __x1097__ ->
                              Rpc.Enum [PVS_proxy.rpc_of_t __x1097__]
                          | None -> Rpc.Enum []
                    with
                    | Rpc.Enum [] ->
                        (match match __x1093__ with
                               | Some __x1095__ ->
                                   Rpc.Enum [Rpc.Int __x1095__]
                               | None -> Rpc.Enum []
                         with
                         | Rpc.Enum [] -> []
                         | Rpc.Enum (__x1094__::[]) -> [("vlan", __x1094__)]
                         | _ -> assert false)
                    | Rpc.Enum (__x1096__::[]) -> ("pvs_proxy", __x1096__) ::
                        ((match match __x1093__ with
                                | Some __x1095__ ->
                                    Rpc.Enum [Rpc.Int __x1095__]
                                | None -> Rpc.Enum []
                          with
                          | Rpc.Enum [] -> []
                          | Rpc.Enum (__x1094__::[]) -> [("vlan", __x1094__)]
                          | _ -> assert false))
                    | _ -> assert false))
              | _ -> assert false)))
    and t_of_rpc =
      function
      | __x1056__ ->
          (match __x1056__ with
           | Rpc.Dict __x1057__ ->
               let __x1058__ =
                 try List.assoc "id" __x1057__
                 with
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                          "t" "__x1057__" (Printexc.to_string __x__)
                          "Looking for key id"
                      else ();
                      raise
                        (Rpc.Runtime_exception
                           ("Looking for key id", (Printexc.to_string __x__))))
               and __x1059__ =
                 try List.assoc "position" __x1057__
                 with
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                          "t" "__x1057__" (Printexc.to_string __x__)
                          "Looking for key position"
                      else ();
                      raise
                        (Rpc.Runtime_exception
                           ("Looking for key position",
                             (Printexc.to_string __x__))))
               and __x1060__ =
                 try List.assoc "mac" __x1057__
                 with
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                          "t" "__x1057__" (Printexc.to_string __x__)
                          "Looking for key mac"
                      else ();
                      raise
                        (Rpc.Runtime_exception
                           ("Looking for key mac",
                             (Printexc.to_string __x__))))
               and __x1061__ =
                 try List.assoc "carrier" __x1057__
                 with
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                          "t" "__x1057__" (Printexc.to_string __x__)
                          "Looking for key carrier"
                      else ();
                      raise
                        (Rpc.Runtime_exception
                           ("Looking for key carrier",
                             (Printexc.to_string __x__))))
               and __x1062__ =
                 try List.assoc "mtu" __x1057__
                 with
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                          "t" "__x1057__" (Printexc.to_string __x__)
                          "Looking for key mtu"
                      else ();
                      raise
                        (Rpc.Runtime_exception
                           ("Looking for key mtu",
                             (Printexc.to_string __x__))))
               and __x1063__ =
                 if List.mem_assoc "rate" __x1057__
                 then Rpc.Enum [List.assoc "rate" __x1057__]
                 else Rpc.Enum []
               and __x1064__ =
                 try List.assoc "backend" __x1057__
                 with
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                          "t" "__x1057__" (Printexc.to_string __x__)
                          "Looking for key backend"
                      else ();
                      raise
                        (Rpc.Runtime_exception
                           ("Looking for key backend",
                             (Printexc.to_string __x__))))
               and __x1065__ =
                 try List.assoc "other_config" __x1057__
                 with
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                          "t" "__x1057__" (Printexc.to_string __x__)
                          "Looking for key other_config"
                      else ();
                      raise
                        (Rpc.Runtime_exception
                           ("Looking for key other_config",
                             (Printexc.to_string __x__))))
               and __x1066__ =
                 try List.assoc "locking_mode" __x1057__
                 with
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                          "t" "__x1057__" (Printexc.to_string __x__)
                          "Looking for key locking_mode"
                      else ();
                      raise
                        (Rpc.Runtime_exception
                           ("Looking for key locking_mode",
                             (Printexc.to_string __x__))))
               and __x1067__ =
                 try List.assoc "extra_private_keys" __x1057__
                 with
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                          "t" "__x1057__" (Printexc.to_string __x__)
                          "Looking for key extra_private_keys"
                      else ();
                      raise
                        (Rpc.Runtime_exception
                           ("Looking for key extra_private_keys",
                             (Printexc.to_string __x__))))
               and __x1068__ =
                 try List.assoc "ipv4_configuration" __x1057__
                 with
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                          "t" "__x1057__" (Printexc.to_string __x__)
                          "Looking for key ipv4_configuration"
                      else ();
                      raise
                        (Rpc.Runtime_exception
                           ("Looking for key ipv4_configuration",
                             (Printexc.to_string __x__))))
               and __x1069__ =
                 try List.assoc "ipv6_configuration" __x1057__
                 with
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                          "t" "__x1057__" (Printexc.to_string __x__)
                          "Looking for key ipv6_configuration"
                      else ();
                      raise
                        (Rpc.Runtime_exception
                           ("Looking for key ipv6_configuration",
                             (Printexc.to_string __x__))))
               and __x1070__ =
                 if List.mem_assoc "pvs_proxy" __x1057__
                 then Rpc.Enum [List.assoc "pvs_proxy" __x1057__]
                 else Rpc.Enum []
               and __x1071__ =
                 if List.mem_assoc "vlan" __x1057__
                 then Rpc.Enum [List.assoc "vlan" __x1057__]
                 else Rpc.Enum [] in
               {
                 id = (id_of_rpc __x1058__);
                 position =
                   ((match __x1059__ with
                     | Rpc.Int x -> Int64.to_int x
                     | Rpc.String s -> int_of_string s
                     | __x__ ->
                         (if Rpc.get_debug ()
                          then
                            Printf.eprintf
                              "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                              "t" "__x1059__" (Rpc.to_string __x__)
                              "Int(int)"
                          else ();
                          raise (Rpc.Runtime_error ("Int(int)", __x__)))));
                 mac =
                   ((match __x1060__ with
                     | Rpc.String x -> x
                     | __x__ ->
                         (if Rpc.get_debug ()
                          then
                            Printf.eprintf
                              "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                              "t" "__x1060__" (Rpc.to_string __x__)
                              "String(string)"
                          else ();
                          raise (Rpc.Runtime_error ("String(string)", __x__)))));
                 carrier =
                   ((match __x1061__ with
                     | Rpc.Bool x -> x
                     | __x__ ->
                         (if Rpc.get_debug ()
                          then
                            Printf.eprintf
                              "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                              "t" "__x1061__" (Rpc.to_string __x__) "Bool"
                          else ();
                          raise (Rpc.Runtime_error ("Bool", __x__)))));
                 mtu =
                   ((match __x1062__ with
                     | Rpc.Int x -> Int64.to_int x
                     | Rpc.String s -> int_of_string s
                     | __x__ ->
                         (if Rpc.get_debug ()
                          then
                            Printf.eprintf
                              "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                              "t" "__x1062__" (Rpc.to_string __x__)
                              "Int(int)"
                          else ();
                          raise (Rpc.Runtime_error ("Int(int)", __x__)))));
                 rate =
                   ((match __x1063__ with
                     | Rpc.Enum [] -> None
                     | Rpc.Enum (__x1072__::[]) ->
                         Some
                           ((match __x1072__ with
                             | Rpc.Enum (__x1073__::__x1074__::[]) ->
                                 (((match __x1073__ with
                                    | Rpc.Int x -> x
                                    | Rpc.String s -> Int64.of_string s
                                    | __x__ ->
                                        (if Rpc.get_debug ()
                                         then
                                           Printf.eprintf
                                             "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                             "t" "__x1073__"
                                             (Rpc.to_string __x__)
                                             "Int(int64)"
                                         else ();
                                         raise
                                           (Rpc.Runtime_error
                                              ("Int(int64)", __x__))))),
                                   ((match __x1074__ with
                                     | Rpc.Int x -> x
                                     | Rpc.String s -> Int64.of_string s
                                     | __x__ ->
                                         (if Rpc.get_debug ()
                                          then
                                            Printf.eprintf
                                              "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                              "t" "__x1074__"
                                              (Rpc.to_string __x__)
                                              "Int(int64)"
                                          else ();
                                          raise
                                            (Rpc.Runtime_error
                                               ("Int(int64)", __x__))))))
                             | __x__ ->
                                 (if Rpc.get_debug ()
                                  then
                                    Printf.eprintf
                                      "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                      "t" "__x1072__" (Rpc.to_string __x__)
                                      "List"
                                  else ();
                                  raise (Rpc.Runtime_error ("List", __x__)))))
                     | __x__ ->
                         (if Rpc.get_debug ()
                          then
                            Printf.eprintf
                              "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                              "t" "__x1063__" (Rpc.to_string __x__)
                              "Enum[]/Enum[_]"
                          else ();
                          raise (Rpc.Runtime_error ("Enum[]/Enum[_]", __x__)))));
                 backend = (Network.t_of_rpc __x1064__);
                 other_config =
                   ((match __x1065__ with
                     | Rpc.Dict d ->
                         List.map
                           (function
                            | (key, __x1075__) ->
                                (key,
                                  ((match __x1075__ with
                                    | Rpc.String x -> x
                                    | __x__ ->
                                        (if Rpc.get_debug ()
                                         then
                                           Printf.eprintf
                                             "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                             "t" "__x1075__"
                                             (Rpc.to_string __x__)
                                             "String(string)"
                                         else ();
                                         raise
                                           (Rpc.Runtime_error
                                              ("String(string)", __x__)))))))
                           d
                     | __x__ ->
                         (if Rpc.get_debug ()
                          then
                            Printf.eprintf
                              "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                              "t" "__x1065__" (Rpc.to_string __x__) "Dict"
                          else ();
                          raise (Rpc.Runtime_error ("Dict", __x__)))));
                 locking_mode = (locking_mode_of_rpc __x1066__);
                 extra_private_keys =
                   ((match __x1067__ with
                     | Rpc.Dict d ->
                         List.map
                           (function
                            | (key, __x1076__) ->
                                (key,
                                  ((match __x1076__ with
                                    | Rpc.String x -> x
                                    | __x__ ->
                                        (if Rpc.get_debug ()
                                         then
                                           Printf.eprintf
                                             "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                             "t" "__x1076__"
                                             (Rpc.to_string __x__)
                                             "String(string)"
                                         else ();
                                         raise
                                           (Rpc.Runtime_error
                                              ("String(string)", __x__)))))))
                           d
                     | __x__ ->
                         (if Rpc.get_debug ()
                          then
                            Printf.eprintf
                              "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                              "t" "__x1067__" (Rpc.to_string __x__) "Dict"
                          else ();
                          raise (Rpc.Runtime_error ("Dict", __x__)))));
                 ipv4_configuration = (ipv4_configuration_of_rpc __x1068__);
                 ipv6_configuration = (ipv6_configuration_of_rpc __x1069__);
                 pvs_proxy =
                   ((match __x1070__ with
                     | Rpc.Enum [] -> None
                     | Rpc.Enum (__x1077__::[]) ->
                         Some (PVS_proxy.t_of_rpc __x1077__)
                     | __x__ ->
                         (if Rpc.get_debug ()
                          then
                            Printf.eprintf
                              "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                              "t" "__x1070__" (Rpc.to_string __x__)
                              "Enum[]/Enum[_]"
                          else ();
                          raise (Rpc.Runtime_error ("Enum[]/Enum[_]", __x__)))));
                 vlan =
                   ((match __x1071__ with
                     | Rpc.Enum [] -> None
                     | Rpc.Enum (__x1078__::[]) ->
                         Some
                           ((match __x1078__ with
                             | Rpc.Int x -> x
                             | Rpc.String s -> Int64.of_string s
                             | __x__ ->
                                 (if Rpc.get_debug ()
                                  then
                                    Printf.eprintf
                                      "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                      "t" "__x1078__" (Rpc.to_string __x__)
                                      "Int(int64)"
                                  else ();
                                  raise
                                    (Rpc.Runtime_error ("Int(int64)", __x__)))))
                     | __x__ ->
                         (if Rpc.get_debug ()
                          then
                            Printf.eprintf
                              "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                              "t" "__x1071__" (Rpc.to_string __x__)
                              "Enum[]/Enum[_]"
                          else ();
                          raise (Rpc.Runtime_error ("Enum[]/Enum[_]", __x__)))))
               }
           | __x__ ->
               (if Rpc.get_debug ()
                then
                  Printf.eprintf
                    "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                    "t" "__x1056__" (Rpc.to_string __x__) "Dict"
                else ();
                raise (Rpc.Runtime_error ("Dict", __x__))))
    let default_t =
      {
        id = ("", "");
        position = 0;
        mac = "fe:ff:ff:ff:ff:ff";
        carrier = true;
        mtu = 1500;
        rate = None;
        backend = Network.default_t;
        other_config = [];
        locking_mode = default_locking_mode;
        extra_private_keys = [];
        ipv4_configuration = default_ipv4_configuration;
        ipv6_configuration = default_ipv6_configuration;
        pvs_proxy = None;
        vlan = None
      }
    let t_of_rpc =
      function
      | rpc -> (Rpc.struct_extend rpc (rpc_of_t default_t)) |> t_of_rpc
    type state =
      {
      active: bool ;
      plugged: bool ;
      kthread_pid: int ;
      media_present: bool ;
      device: string option ;
      pvs_rules_active: bool }
    let rec rpc_of_state =
      function
      | __x1113__ ->
          let __x1114__ = __x1113__.active
          and __x1115__ = __x1113__.plugged
          and __x1116__ = __x1113__.kthread_pid
          and __x1117__ = __x1113__.media_present
          and __x1118__ = __x1113__.device
          and __x1119__ = __x1113__.pvs_rules_active in
          Rpc.Dict (("active", (Rpc.Bool __x1114__)) ::
            ("plugged", (Rpc.Bool __x1115__)) ::
            ("kthread_pid", (Rpc.Int (Int64.of_int __x1116__))) ::
            ("media_present", (Rpc.Bool __x1117__)) ::
            ((match match __x1118__ with
                    | Some __x1121__ -> Rpc.Enum [Rpc.String __x1121__]
                    | None -> Rpc.Enum []
              with
              | Rpc.Enum [] -> [("pvs_rules_active", (Rpc.Bool __x1119__))]
              | Rpc.Enum (__x1120__::[]) ->
                  [("device", __x1120__);
                  ("pvs_rules_active", (Rpc.Bool __x1119__))]
              | _ -> assert false)))
    and state_of_rpc =
      function
      | __x1104__ ->
          (match __x1104__ with
           | Rpc.Dict __x1105__ ->
               let __x1106__ =
                 try List.assoc "active" __x1105__
                 with
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                          "state" "__x1105__" (Printexc.to_string __x__)
                          "Looking for key active"
                      else ();
                      raise
                        (Rpc.Runtime_exception
                           ("Looking for key active",
                             (Printexc.to_string __x__))))
               and __x1107__ =
                 try List.assoc "plugged" __x1105__
                 with
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                          "state" "__x1105__" (Printexc.to_string __x__)
                          "Looking for key plugged"
                      else ();
                      raise
                        (Rpc.Runtime_exception
                           ("Looking for key plugged",
                             (Printexc.to_string __x__))))
               and __x1108__ =
                 try List.assoc "kthread_pid" __x1105__
                 with
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                          "state" "__x1105__" (Printexc.to_string __x__)
                          "Looking for key kthread_pid"
                      else ();
                      raise
                        (Rpc.Runtime_exception
                           ("Looking for key kthread_pid",
                             (Printexc.to_string __x__))))
               and __x1109__ =
                 try List.assoc "media_present" __x1105__
                 with
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                          "state" "__x1105__" (Printexc.to_string __x__)
                          "Looking for key media_present"
                      else ();
                      raise
                        (Rpc.Runtime_exception
                           ("Looking for key media_present",
                             (Printexc.to_string __x__))))
               and __x1110__ =
                 if List.mem_assoc "device" __x1105__
                 then Rpc.Enum [List.assoc "device" __x1105__]
                 else Rpc.Enum []
               and __x1111__ =
                 try List.assoc "pvs_rules_active" __x1105__
                 with
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                          "state" "__x1105__" (Printexc.to_string __x__)
                          "Looking for key pvs_rules_active"
                      else ();
                      raise
                        (Rpc.Runtime_exception
                           ("Looking for key pvs_rules_active",
                             (Printexc.to_string __x__)))) in
               {
                 active =
                   ((match __x1106__ with
                     | Rpc.Bool x -> x
                     | __x__ ->
                         (if Rpc.get_debug ()
                          then
                            Printf.eprintf
                              "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                              "state" "__x1106__" (Rpc.to_string __x__)
                              "Bool"
                          else ();
                          raise (Rpc.Runtime_error ("Bool", __x__)))));
                 plugged =
                   ((match __x1107__ with
                     | Rpc.Bool x -> x
                     | __x__ ->
                         (if Rpc.get_debug ()
                          then
                            Printf.eprintf
                              "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                              "state" "__x1107__" (Rpc.to_string __x__)
                              "Bool"
                          else ();
                          raise (Rpc.Runtime_error ("Bool", __x__)))));
                 kthread_pid =
                   ((match __x1108__ with
                     | Rpc.Int x -> Int64.to_int x
                     | Rpc.String s -> int_of_string s
                     | __x__ ->
                         (if Rpc.get_debug ()
                          then
                            Printf.eprintf
                              "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                              "state" "__x1108__" (Rpc.to_string __x__)
                              "Int(int)"
                          else ();
                          raise (Rpc.Runtime_error ("Int(int)", __x__)))));
                 media_present =
                   ((match __x1109__ with
                     | Rpc.Bool x -> x
                     | __x__ ->
                         (if Rpc.get_debug ()
                          then
                            Printf.eprintf
                              "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                              "state" "__x1109__" (Rpc.to_string __x__)
                              "Bool"
                          else ();
                          raise (Rpc.Runtime_error ("Bool", __x__)))));
                 device =
                   ((match __x1110__ with
                     | Rpc.Enum [] -> None
                     | Rpc.Enum (__x1112__::[]) ->
                         Some
                           ((match __x1112__ with
                             | Rpc.String x -> x
                             | __x__ ->
                                 (if Rpc.get_debug ()
                                  then
                                    Printf.eprintf
                                      "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                      "state" "__x1112__"
                                      (Rpc.to_string __x__) "String(string)"
                                  else ();
                                  raise
                                    (Rpc.Runtime_error
                                       ("String(string)", __x__)))))
                     | __x__ ->
                         (if Rpc.get_debug ()
                          then
                            Printf.eprintf
                              "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                              "state" "__x1110__" (Rpc.to_string __x__)
                              "Enum[]/Enum[_]"
                          else ();
                          raise (Rpc.Runtime_error ("Enum[]/Enum[_]", __x__)))));
                 pvs_rules_active =
                   ((match __x1111__ with
                     | Rpc.Bool x -> x
                     | __x__ ->
                         (if Rpc.get_debug ()
                          then
                            Printf.eprintf
                              "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                              "state" "__x1111__" (Rpc.to_string __x__)
                              "Bool"
                          else ();
                          raise (Rpc.Runtime_error ("Bool", __x__)))))
               }
           | __x__ ->
               (if Rpc.get_debug ()
                then
                  Printf.eprintf
                    "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                    "state" "__x1104__" (Rpc.to_string __x__) "Dict"
                else ();
                raise (Rpc.Runtime_error ("Dict", __x__))))
  end
module Metadata =
  struct
    type t =
      {
      vm: Vm.t ;
      vbds: Vbd.t list ;
      vifs: Vif.t list ;
      pcis: Pci.t list ;
      vgpus: Vgpu.t list ;
      vusbs: Vusb.t list ;
      domains: string option }
    let rec rpc_of_t =
      function
      | __x1142__ ->
          let __x1143__ = __x1142__.vm
          and __x1144__ = __x1142__.vbds
          and __x1145__ = __x1142__.vifs
          and __x1146__ = __x1142__.pcis
          and __x1147__ = __x1142__.vgpus
          and __x1148__ = __x1142__.vusbs
          and __x1149__ = __x1142__.domains in
          Rpc.Dict (("vm", (Vm.rpc_of_t __x1143__)) ::
            ("vbds",
              (Rpc.Enum
                 (List.map (function | __x1156__ -> Vbd.rpc_of_t __x1156__)
                    __x1144__)))
            ::
            ("vifs",
              (Rpc.Enum
                 (List.map (function | __x1155__ -> Vif.rpc_of_t __x1155__)
                    __x1145__)))
            ::
            ("pcis",
              (Rpc.Enum
                 (List.map (function | __x1154__ -> Pci.rpc_of_t __x1154__)
                    __x1146__)))
            ::
            ("vgpus",
              (Rpc.Enum
                 (List.map (function | __x1153__ -> Vgpu.rpc_of_t __x1153__)
                    __x1147__)))
            ::
            ("vusbs",
              (Rpc.Enum
                 (List.map (function | __x1152__ -> Vusb.rpc_of_t __x1152__)
                    __x1148__)))
            ::
            ((match match __x1149__ with
                    | Some __x1151__ -> Rpc.Enum [Rpc.String __x1151__]
                    | None -> Rpc.Enum []
              with
              | Rpc.Enum [] -> []
              | Rpc.Enum (__x1150__::[]) -> [("domains", __x1150__)]
              | _ -> assert false)))
    and t_of_rpc =
      function
      | __x1122__ ->
          (match __x1122__ with
           | Rpc.Dict __x1123__ ->
               let __x1124__ =
                 try List.assoc "vm" __x1123__
                 with
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                          "t" "__x1123__" (Printexc.to_string __x__)
                          "Looking for key vm"
                      else ();
                      raise
                        (Rpc.Runtime_exception
                           ("Looking for key vm", (Printexc.to_string __x__))))
               and __x1125__ =
                 try List.assoc "vbds" __x1123__
                 with
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                          "t" "__x1123__" (Printexc.to_string __x__)
                          "Looking for key vbds"
                      else ();
                      raise
                        (Rpc.Runtime_exception
                           ("Looking for key vbds",
                             (Printexc.to_string __x__))))
               and __x1126__ =
                 try List.assoc "vifs" __x1123__
                 with
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                          "t" "__x1123__" (Printexc.to_string __x__)
                          "Looking for key vifs"
                      else ();
                      raise
                        (Rpc.Runtime_exception
                           ("Looking for key vifs",
                             (Printexc.to_string __x__))))
               and __x1127__ =
                 try List.assoc "pcis" __x1123__
                 with
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                          "t" "__x1123__" (Printexc.to_string __x__)
                          "Looking for key pcis"
                      else ();
                      raise
                        (Rpc.Runtime_exception
                           ("Looking for key pcis",
                             (Printexc.to_string __x__))))
               and __x1128__ =
                 try List.assoc "vgpus" __x1123__
                 with
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                          "t" "__x1123__" (Printexc.to_string __x__)
                          "Looking for key vgpus"
                      else ();
                      raise
                        (Rpc.Runtime_exception
                           ("Looking for key vgpus",
                             (Printexc.to_string __x__))))
               and __x1129__ =
                 try List.assoc "vusbs" __x1123__
                 with
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                          "t" "__x1123__" (Printexc.to_string __x__)
                          "Looking for key vusbs"
                      else ();
                      raise
                        (Rpc.Runtime_exception
                           ("Looking for key vusbs",
                             (Printexc.to_string __x__))))
               and __x1130__ =
                 if List.mem_assoc "domains" __x1123__
                 then Rpc.Enum [List.assoc "domains" __x1123__]
                 else Rpc.Enum [] in
               {
                 vm = (Vm.t_of_rpc __x1124__);
                 vbds =
                   ((match __x1125__ with
                     | Rpc.Enum __x1131__ ->
                         List.map
                           (function | __x1132__ -> Vbd.t_of_rpc __x1132__)
                           __x1131__
                     | __x__ ->
                         (if Rpc.get_debug ()
                          then
                            Printf.eprintf
                              "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                              "t" "__x1125__" (Rpc.to_string __x__) "List"
                          else ();
                          raise (Rpc.Runtime_error ("List", __x__)))));
                 vifs =
                   ((match __x1126__ with
                     | Rpc.Enum __x1133__ ->
                         List.map
                           (function | __x1134__ -> Vif.t_of_rpc __x1134__)
                           __x1133__
                     | __x__ ->
                         (if Rpc.get_debug ()
                          then
                            Printf.eprintf
                              "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                              "t" "__x1126__" (Rpc.to_string __x__) "List"
                          else ();
                          raise (Rpc.Runtime_error ("List", __x__)))));
                 pcis =
                   ((match __x1127__ with
                     | Rpc.Enum __x1135__ ->
                         List.map
                           (function | __x1136__ -> Pci.t_of_rpc __x1136__)
                           __x1135__
                     | __x__ ->
                         (if Rpc.get_debug ()
                          then
                            Printf.eprintf
                              "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                              "t" "__x1127__" (Rpc.to_string __x__) "List"
                          else ();
                          raise (Rpc.Runtime_error ("List", __x__)))));
                 vgpus =
                   ((match __x1128__ with
                     | Rpc.Enum __x1137__ ->
                         List.map
                           (function | __x1138__ -> Vgpu.t_of_rpc __x1138__)
                           __x1137__
                     | __x__ ->
                         (if Rpc.get_debug ()
                          then
                            Printf.eprintf
                              "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                              "t" "__x1128__" (Rpc.to_string __x__) "List"
                          else ();
                          raise (Rpc.Runtime_error ("List", __x__)))));
                 vusbs =
                   ((match __x1129__ with
                     | Rpc.Enum __x1139__ ->
                         List.map
                           (function | __x1140__ -> Vusb.t_of_rpc __x1140__)
                           __x1139__
                     | __x__ ->
                         (if Rpc.get_debug ()
                          then
                            Printf.eprintf
                              "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                              "t" "__x1129__" (Rpc.to_string __x__) "List"
                          else ();
                          raise (Rpc.Runtime_error ("List", __x__)))));
                 domains =
                   ((match __x1130__ with
                     | Rpc.Enum [] -> None
                     | Rpc.Enum (__x1141__::[]) ->
                         Some
                           ((match __x1141__ with
                             | Rpc.String x -> x
                             | __x__ ->
                                 (if Rpc.get_debug ()
                                  then
                                    Printf.eprintf
                                      "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                      "t" "__x1141__" (Rpc.to_string __x__)
                                      "String(string)"
                                  else ();
                                  raise
                                    (Rpc.Runtime_error
                                       ("String(string)", __x__)))))
                     | __x__ ->
                         (if Rpc.get_debug ()
                          then
                            Printf.eprintf
                              "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                              "t" "__x1130__" (Rpc.to_string __x__)
                              "Enum[]/Enum[_]"
                          else ();
                          raise (Rpc.Runtime_error ("Enum[]/Enum[_]", __x__)))))
               }
           | __x__ ->
               (if Rpc.get_debug ()
                then
                  Printf.eprintf
                    "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                    "t" "__x1122__" (Rpc.to_string __x__) "Dict"
                else ();
                raise (Rpc.Runtime_error ("Dict", __x__))))
    let default_t =
      {
        vm = Vm.default_t;
        vbds = [];
        vifs = [];
        pcis = [];
        vgpus = [];
        vusbs = [];
        domains = None
      }
    let t_of_rpc =
      function
      | rpc -> (Rpc.struct_extend rpc (rpc_of_t default_t)) |> t_of_rpc
  end
module Task =
  struct
    type id = string
    let rec rpc_of_id = function | __x1158__ -> Rpc.String __x1158__
    and id_of_rpc =
      function
      | __x1157__ ->
          (match __x1157__ with
           | Rpc.String x -> x
           | __x__ ->
               (if Rpc.get_debug ()
                then
                  Printf.eprintf
                    "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                    "id" "__x1157__" (Rpc.to_string __x__) "String(string)"
                else ();
                raise (Rpc.Runtime_error ("String(string)", __x__))))
    type async_result = Rpc.t
    let rec rpc_of_async_result =
      function | __x1160__ -> Rpc.rpc_of_t __x1160__
    and async_result_of_rpc = function | __x1159__ -> Rpc.t_of_rpc __x1159__
    type completion_t = {
      duration: float ;
      result: async_result option }
    let rec rpc_of_completion_t =
      function
      | __x1166__ ->
          let __x1167__ = __x1166__.duration
          and __x1168__ = __x1166__.result in
          Rpc.Dict (("duration", (Rpc.Float __x1167__)) ::
            ((match match __x1168__ with
                    | Some __x1170__ ->
                        Rpc.Enum [rpc_of_async_result __x1170__]
                    | None -> Rpc.Enum []
              with
              | Rpc.Enum [] -> []
              | Rpc.Enum (__x1169__::[]) -> [("result", __x1169__)]
              | _ -> assert false)))
    and completion_t_of_rpc =
      function
      | __x1161__ ->
          (match __x1161__ with
           | Rpc.Dict __x1162__ ->
               let __x1163__ =
                 try List.assoc "duration" __x1162__
                 with
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                          "completion_t" "__x1162__"
                          (Printexc.to_string __x__)
                          "Looking for key duration"
                      else ();
                      raise
                        (Rpc.Runtime_exception
                           ("Looking for key duration",
                             (Printexc.to_string __x__))))
               and __x1164__ =
                 if List.mem_assoc "result" __x1162__
                 then Rpc.Enum [List.assoc "result" __x1162__]
                 else Rpc.Enum [] in
               {
                 duration =
                   ((match __x1163__ with
                     | Rpc.Float x -> x
                     | Rpc.String s -> float_of_string s
                     | __x__ ->
                         (if Rpc.get_debug ()
                          then
                            Printf.eprintf
                              "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                              "completion_t" "__x1163__"
                              (Rpc.to_string __x__) "Float"
                          else ();
                          raise (Rpc.Runtime_error ("Float", __x__)))));
                 result =
                   ((match __x1164__ with
                     | Rpc.Enum [] -> None
                     | Rpc.Enum (__x1165__::[]) ->
                         Some (async_result_of_rpc __x1165__)
                     | __x__ ->
                         (if Rpc.get_debug ()
                          then
                            Printf.eprintf
                              "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                              "completion_t" "__x1164__"
                              (Rpc.to_string __x__) "Enum[]/Enum[_]"
                          else ();
                          raise (Rpc.Runtime_error ("Enum[]/Enum[_]", __x__)))))
               }
           | __x__ ->
               (if Rpc.get_debug ()
                then
                  Printf.eprintf
                    "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                    "completion_t" "__x1161__" (Rpc.to_string __x__) "Dict"
                else ();
                raise (Rpc.Runtime_error ("Dict", __x__))))
    type state =
      | Pending of float 
      | Completed of completion_t 
      | Failed of Rpc.t 
    let rec rpc_of_state =
      function
      | __x1175__ ->
          (match __x1175__ with
           | Failed __x1176__ ->
               Rpc.Enum [Rpc.String "Failed"; Rpc.rpc_of_t __x1176__]
           | Completed __x1177__ ->
               Rpc.Enum
                 [Rpc.String "Completed"; rpc_of_completion_t __x1177__]
           | Pending __x1178__ ->
               Rpc.Enum [Rpc.String "Pending"; Rpc.Float __x1178__])
    and state_of_rpc =
      function
      | __x1171__ ->
          (match Rpc.lowerfn __x1171__ with
           | Rpc.Enum ((Rpc.String "failed")::__x1172__::[]) ->
               Failed (Rpc.t_of_rpc __x1172__)
           | Rpc.Enum ((Rpc.String "completed")::__x1173__::[]) ->
               Completed (completion_t_of_rpc __x1173__)
           | Rpc.Enum ((Rpc.String "pending")::__x1174__::[]) ->
               Pending
                 ((match __x1174__ with
                   | Rpc.Float x -> x
                   | Rpc.String s -> float_of_string s
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "state" "__x1174__" (Rpc.to_string __x__) "Float"
                        else ();
                        raise (Rpc.Runtime_error ("Float", __x__)))))
           | __x__ ->
               (if Rpc.get_debug ()
                then
                  Printf.eprintf
                    "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                    "state" "__x1171__" (Rpc.to_string __x__)
                    "Enum[String s;...]"
                else ();
                raise (Rpc.Runtime_error ("Enum[String s;...]", __x__))))
    type t =
      {
      id: id ;
      dbg: string ;
      ctime: float ;
      state: state ;
      subtasks: (string * state) list ;
      debug_info: (string * string) list ;
      backtrace: string }
    let rec rpc_of_t =
      function
      | __x1190__ ->
          let __x1191__ = __x1190__.id
          and __x1192__ = __x1190__.dbg
          and __x1193__ = __x1190__.ctime
          and __x1194__ = __x1190__.state
          and __x1195__ = __x1190__.subtasks
          and __x1196__ = __x1190__.debug_info
          and __x1197__ = __x1190__.backtrace in
          Rpc.Dict
            [("id", (rpc_of_id __x1191__));
            ("dbg", (Rpc.String __x1192__));
            ("ctime", (Rpc.Float __x1193__));
            ("state", (rpc_of_state __x1194__));
            ("subtasks",
              ((let dict =
                  List.map
                    (function
                     | (key, __x1199__) -> (key, (rpc_of_state __x1199__)))
                    __x1195__ in
                Rpc.Dict dict)));
            ("debug_info",
              ((let dict =
                  List.map
                    (function
                     | (key, __x1198__) -> (key, (Rpc.String __x1198__)))
                    __x1196__ in
                Rpc.Dict dict)));
            ("backtrace", (Rpc.String __x1197__))]
    and t_of_rpc =
      function
      | __x1179__ ->
          (match __x1179__ with
           | Rpc.Dict __x1180__ ->
               let __x1181__ =
                 try List.assoc "id" __x1180__
                 with
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                          "t" "__x1180__" (Printexc.to_string __x__)
                          "Looking for key id"
                      else ();
                      raise
                        (Rpc.Runtime_exception
                           ("Looking for key id", (Printexc.to_string __x__))))
               and __x1182__ =
                 try List.assoc "dbg" __x1180__
                 with
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                          "t" "__x1180__" (Printexc.to_string __x__)
                          "Looking for key dbg"
                      else ();
                      raise
                        (Rpc.Runtime_exception
                           ("Looking for key dbg",
                             (Printexc.to_string __x__))))
               and __x1183__ =
                 try List.assoc "ctime" __x1180__
                 with
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                          "t" "__x1180__" (Printexc.to_string __x__)
                          "Looking for key ctime"
                      else ();
                      raise
                        (Rpc.Runtime_exception
                           ("Looking for key ctime",
                             (Printexc.to_string __x__))))
               and __x1184__ =
                 try List.assoc "state" __x1180__
                 with
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                          "t" "__x1180__" (Printexc.to_string __x__)
                          "Looking for key state"
                      else ();
                      raise
                        (Rpc.Runtime_exception
                           ("Looking for key state",
                             (Printexc.to_string __x__))))
               and __x1185__ =
                 try List.assoc "subtasks" __x1180__
                 with
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                          "t" "__x1180__" (Printexc.to_string __x__)
                          "Looking for key subtasks"
                      else ();
                      raise
                        (Rpc.Runtime_exception
                           ("Looking for key subtasks",
                             (Printexc.to_string __x__))))
               and __x1186__ =
                 try List.assoc "debug_info" __x1180__
                 with
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                          "t" "__x1180__" (Printexc.to_string __x__)
                          "Looking for key debug_info"
                      else ();
                      raise
                        (Rpc.Runtime_exception
                           ("Looking for key debug_info",
                             (Printexc.to_string __x__))))
               and __x1187__ =
                 try List.assoc "backtrace" __x1180__
                 with
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                          "t" "__x1180__" (Printexc.to_string __x__)
                          "Looking for key backtrace"
                      else ();
                      raise
                        (Rpc.Runtime_exception
                           ("Looking for key backtrace",
                             (Printexc.to_string __x__)))) in
               {
                 id = (id_of_rpc __x1181__);
                 dbg =
                   ((match __x1182__ with
                     | Rpc.String x -> x
                     | __x__ ->
                         (if Rpc.get_debug ()
                          then
                            Printf.eprintf
                              "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                              "t" "__x1182__" (Rpc.to_string __x__)
                              "String(string)"
                          else ();
                          raise (Rpc.Runtime_error ("String(string)", __x__)))));
                 ctime =
                   ((match __x1183__ with
                     | Rpc.Float x -> x
                     | Rpc.String s -> float_of_string s
                     | __x__ ->
                         (if Rpc.get_debug ()
                          then
                            Printf.eprintf
                              "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                              "t" "__x1183__" (Rpc.to_string __x__) "Float"
                          else ();
                          raise (Rpc.Runtime_error ("Float", __x__)))));
                 state = (state_of_rpc __x1184__);
                 subtasks =
                   ((match __x1185__ with
                     | Rpc.Dict d ->
                         List.map
                           (function
                            | (key, __x1188__) ->
                                (key, (state_of_rpc __x1188__))) d
                     | __x__ ->
                         (if Rpc.get_debug ()
                          then
                            Printf.eprintf
                              "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                              "t" "__x1185__" (Rpc.to_string __x__) "Dict"
                          else ();
                          raise (Rpc.Runtime_error ("Dict", __x__)))));
                 debug_info =
                   ((match __x1186__ with
                     | Rpc.Dict d ->
                         List.map
                           (function
                            | (key, __x1189__) ->
                                (key,
                                  ((match __x1189__ with
                                    | Rpc.String x -> x
                                    | __x__ ->
                                        (if Rpc.get_debug ()
                                         then
                                           Printf.eprintf
                                             "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                             "t" "__x1189__"
                                             (Rpc.to_string __x__)
                                             "String(string)"
                                         else ();
                                         raise
                                           (Rpc.Runtime_error
                                              ("String(string)", __x__)))))))
                           d
                     | __x__ ->
                         (if Rpc.get_debug ()
                          then
                            Printf.eprintf
                              "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                              "t" "__x1186__" (Rpc.to_string __x__) "Dict"
                          else ();
                          raise (Rpc.Runtime_error ("Dict", __x__)))));
                 backtrace =
                   ((match __x1187__ with
                     | Rpc.String x -> x
                     | __x__ ->
                         (if Rpc.get_debug ()
                          then
                            Printf.eprintf
                              "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                              "t" "__x1187__" (Rpc.to_string __x__)
                              "String(string)"
                          else ();
                          raise (Rpc.Runtime_error ("String(string)", __x__)))))
               }
           | __x__ ->
               (if Rpc.get_debug ()
                then
                  Printf.eprintf
                    "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                    "t" "__x1179__" (Rpc.to_string __x__) "Dict"
                else ();
                raise (Rpc.Runtime_error ("Dict", __x__))))
  end
module Dynamic =
  struct
    type id =
      | Vm of Vm.id 
      | Vbd of Vbd.id 
      | Vif of Vif.id 
      | Pci of Pci.id 
      | Vgpu of Vgpu.id 
      | Vusb of Vusb.id 
      | Task of Task.id 
    let rec rpc_of_id =
      function
      | __x1208__ ->
          (match __x1208__ with
           | Task __x1209__ ->
               Rpc.Enum [Rpc.String "Task"; Task.rpc_of_id __x1209__]
           | Vusb __x1210__ ->
               Rpc.Enum [Rpc.String "Vusb"; Vusb.rpc_of_id __x1210__]
           | Vgpu __x1211__ ->
               Rpc.Enum [Rpc.String "Vgpu"; Vgpu.rpc_of_id __x1211__]
           | Pci __x1212__ ->
               Rpc.Enum [Rpc.String "Pci"; Pci.rpc_of_id __x1212__]
           | Vif __x1213__ ->
               Rpc.Enum [Rpc.String "Vif"; Vif.rpc_of_id __x1213__]
           | Vbd __x1214__ ->
               Rpc.Enum [Rpc.String "Vbd"; Vbd.rpc_of_id __x1214__]
           | Vm __x1215__ ->
               Rpc.Enum [Rpc.String "Vm"; Vm.rpc_of_id __x1215__])
    and id_of_rpc =
      function
      | __x1200__ ->
          (match Rpc.lowerfn __x1200__ with
           | Rpc.Enum ((Rpc.String "task")::__x1201__::[]) ->
               Task (Task.id_of_rpc __x1201__)
           | Rpc.Enum ((Rpc.String "vusb")::__x1202__::[]) ->
               Vusb (Vusb.id_of_rpc __x1202__)
           | Rpc.Enum ((Rpc.String "vgpu")::__x1203__::[]) ->
               Vgpu (Vgpu.id_of_rpc __x1203__)
           | Rpc.Enum ((Rpc.String "pci")::__x1204__::[]) ->
               Pci (Pci.id_of_rpc __x1204__)
           | Rpc.Enum ((Rpc.String "vif")::__x1205__::[]) ->
               Vif (Vif.id_of_rpc __x1205__)
           | Rpc.Enum ((Rpc.String "vbd")::__x1206__::[]) ->
               Vbd (Vbd.id_of_rpc __x1206__)
           | Rpc.Enum ((Rpc.String "vm")::__x1207__::[]) ->
               Vm (Vm.id_of_rpc __x1207__)
           | __x__ ->
               (if Rpc.get_debug ()
                then
                  Printf.eprintf
                    "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                    "id" "__x1200__" (Rpc.to_string __x__)
                    "Enum[String s;...]"
                else ();
                raise (Rpc.Runtime_error ("Enum[String s;...]", __x__))))
    type barrier = (int * id list)
    let rec rpc_of_barrier =
      function
      | __x1221__ ->
          let (__x1222__, __x1223__) = __x1221__ in
          Rpc.Enum
            [Rpc.Int (Int64.of_int __x1222__);
            Rpc.Enum
              (List.map (function | __x1224__ -> rpc_of_id __x1224__)
                 __x1223__)]
    and barrier_of_rpc =
      function
      | __x1216__ ->
          (match __x1216__ with
           | Rpc.Enum (__x1217__::__x1218__::[]) ->
               (((match __x1217__ with
                  | Rpc.Int x -> Int64.to_int x
                  | Rpc.String s -> int_of_string s
                  | __x__ ->
                      (if Rpc.get_debug ()
                       then
                         Printf.eprintf
                           "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                           "barrier" "__x1217__" (Rpc.to_string __x__)
                           "Int(int)"
                       else ();
                       raise (Rpc.Runtime_error ("Int(int)", __x__))))),
                 ((match __x1218__ with
                   | Rpc.Enum __x1219__ ->
                       List.map (function | __x1220__ -> id_of_rpc __x1220__)
                         __x1219__
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "barrier" "__x1218__" (Rpc.to_string __x__)
                            "List"
                        else ();
                        raise (Rpc.Runtime_error ("List", __x__))))))
           | __x__ ->
               (if Rpc.get_debug ()
                then
                  Printf.eprintf
                    "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                    "barrier" "__x1216__" (Rpc.to_string __x__) "List"
                else ();
                raise (Rpc.Runtime_error ("List", __x__))))
    type t =
      | Vm_t of Vm.id * (Vm.t * Vm.state) option 
      | Vbd_t of Vbd.id * (Vbd.t * Vbd.state) option 
      | Vif_t of Vif.id * (Vif.t * Vif.state) option 
      | Pci_t of Pci.id * (Pci.t * Pci.state) option 
      | Vgpu_t of Vgpu.id * (Vgpu.t * Vgpu.state) option 
      | Vusb_t of Vusb.id * (Vusb.t * Vusb.state) option 
      | Task_t of Task.id * Task.t option 
    let rec rpc_of_t =
      function
      | __x1259__ ->
          (match __x1259__ with
           | Task_t (__x1260__, __x1261__) ->
               Rpc.Enum
                 [Rpc.String "Task_t";
                 Task.rpc_of_id __x1260__;
                 (match __x1261__ with
                  | Some __x1262__ -> Rpc.Enum [Task.rpc_of_t __x1262__]
                  | None -> Rpc.Enum [])]
           | Vusb_t (__x1263__, __x1264__) ->
               Rpc.Enum
                 [Rpc.String "Vusb_t";
                 Vusb.rpc_of_id __x1263__;
                 (match __x1264__ with
                  | Some __x1265__ ->
                      Rpc.Enum
                        [(let (__x1266__, __x1267__) = __x1265__ in
                          Rpc.Enum
                            [Vusb.rpc_of_t __x1266__;
                            Vusb.rpc_of_state __x1267__])]
                  | None -> Rpc.Enum [])]
           | Vgpu_t (__x1268__, __x1269__) ->
               Rpc.Enum
                 [Rpc.String "Vgpu_t";
                 Vgpu.rpc_of_id __x1268__;
                 (match __x1269__ with
                  | Some __x1270__ ->
                      Rpc.Enum
                        [(let (__x1271__, __x1272__) = __x1270__ in
                          Rpc.Enum
                            [Vgpu.rpc_of_t __x1271__;
                            Vgpu.rpc_of_state __x1272__])]
                  | None -> Rpc.Enum [])]
           | Pci_t (__x1273__, __x1274__) ->
               Rpc.Enum
                 [Rpc.String "Pci_t";
                 Pci.rpc_of_id __x1273__;
                 (match __x1274__ with
                  | Some __x1275__ ->
                      Rpc.Enum
                        [(let (__x1276__, __x1277__) = __x1275__ in
                          Rpc.Enum
                            [Pci.rpc_of_t __x1276__;
                            Pci.rpc_of_state __x1277__])]
                  | None -> Rpc.Enum [])]
           | Vif_t (__x1278__, __x1279__) ->
               Rpc.Enum
                 [Rpc.String "Vif_t";
                 Vif.rpc_of_id __x1278__;
                 (match __x1279__ with
                  | Some __x1280__ ->
                      Rpc.Enum
                        [(let (__x1281__, __x1282__) = __x1280__ in
                          Rpc.Enum
                            [Vif.rpc_of_t __x1281__;
                            Vif.rpc_of_state __x1282__])]
                  | None -> Rpc.Enum [])]
           | Vbd_t (__x1283__, __x1284__) ->
               Rpc.Enum
                 [Rpc.String "Vbd_t";
                 Vbd.rpc_of_id __x1283__;
                 (match __x1284__ with
                  | Some __x1285__ ->
                      Rpc.Enum
                        [(let (__x1286__, __x1287__) = __x1285__ in
                          Rpc.Enum
                            [Vbd.rpc_of_t __x1286__;
                            Vbd.rpc_of_state __x1287__])]
                  | None -> Rpc.Enum [])]
           | Vm_t (__x1288__, __x1289__) ->
               Rpc.Enum
                 [Rpc.String "Vm_t";
                 Vm.rpc_of_id __x1288__;
                 (match __x1289__ with
                  | Some __x1290__ ->
                      Rpc.Enum
                        [(let (__x1291__, __x1292__) = __x1290__ in
                          Rpc.Enum
                            [Vm.rpc_of_t __x1291__;
                            Vm.rpc_of_state __x1292__])]
                  | None -> Rpc.Enum [])])
    and t_of_rpc =
      function
      | __x1225__ ->
          (match Rpc.lowerfn __x1225__ with
           | Rpc.Enum ((Rpc.String "task_t")::__x1226__::__x1227__::[]) ->
               Task_t
                 ((Task.id_of_rpc __x1226__),
                   ((match __x1227__ with
                     | Rpc.Enum [] -> None
                     | Rpc.Enum (__x1228__::[]) ->
                         Some (Task.t_of_rpc __x1228__)
                     | __x__ ->
                         (if Rpc.get_debug ()
                          then
                            Printf.eprintf
                              "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                              "t" "__x1227__" (Rpc.to_string __x__)
                              "Enum[]/Enum[_]"
                          else ();
                          raise (Rpc.Runtime_error ("Enum[]/Enum[_]", __x__))))))
           | Rpc.Enum ((Rpc.String "vusb_t")::__x1229__::__x1230__::[]) ->
               Vusb_t
                 ((Vusb.id_of_rpc __x1229__),
                   ((match __x1230__ with
                     | Rpc.Enum [] -> None
                     | Rpc.Enum (__x1231__::[]) ->
                         Some
                           ((match __x1231__ with
                             | Rpc.Enum (__x1232__::__x1233__::[]) ->
                                 ((Vusb.t_of_rpc __x1232__),
                                   (Vusb.state_of_rpc __x1233__))
                             | __x__ ->
                                 (if Rpc.get_debug ()
                                  then
                                    Printf.eprintf
                                      "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                      "t" "__x1231__" (Rpc.to_string __x__)
                                      "List"
                                  else ();
                                  raise (Rpc.Runtime_error ("List", __x__)))))
                     | __x__ ->
                         (if Rpc.get_debug ()
                          then
                            Printf.eprintf
                              "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                              "t" "__x1230__" (Rpc.to_string __x__)
                              "Enum[]/Enum[_]"
                          else ();
                          raise (Rpc.Runtime_error ("Enum[]/Enum[_]", __x__))))))
           | Rpc.Enum ((Rpc.String "vgpu_t")::__x1234__::__x1235__::[]) ->
               Vgpu_t
                 ((Vgpu.id_of_rpc __x1234__),
                   ((match __x1235__ with
                     | Rpc.Enum [] -> None
                     | Rpc.Enum (__x1236__::[]) ->
                         Some
                           ((match __x1236__ with
                             | Rpc.Enum (__x1237__::__x1238__::[]) ->
                                 ((Vgpu.t_of_rpc __x1237__),
                                   (Vgpu.state_of_rpc __x1238__))
                             | __x__ ->
                                 (if Rpc.get_debug ()
                                  then
                                    Printf.eprintf
                                      "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                      "t" "__x1236__" (Rpc.to_string __x__)
                                      "List"
                                  else ();
                                  raise (Rpc.Runtime_error ("List", __x__)))))
                     | __x__ ->
                         (if Rpc.get_debug ()
                          then
                            Printf.eprintf
                              "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                              "t" "__x1235__" (Rpc.to_string __x__)
                              "Enum[]/Enum[_]"
                          else ();
                          raise (Rpc.Runtime_error ("Enum[]/Enum[_]", __x__))))))
           | Rpc.Enum ((Rpc.String "pci_t")::__x1239__::__x1240__::[]) ->
               Pci_t
                 ((Pci.id_of_rpc __x1239__),
                   ((match __x1240__ with
                     | Rpc.Enum [] -> None
                     | Rpc.Enum (__x1241__::[]) ->
                         Some
                           ((match __x1241__ with
                             | Rpc.Enum (__x1242__::__x1243__::[]) ->
                                 ((Pci.t_of_rpc __x1242__),
                                   (Pci.state_of_rpc __x1243__))
                             | __x__ ->
                                 (if Rpc.get_debug ()
                                  then
                                    Printf.eprintf
                                      "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                      "t" "__x1241__" (Rpc.to_string __x__)
                                      "List"
                                  else ();
                                  raise (Rpc.Runtime_error ("List", __x__)))))
                     | __x__ ->
                         (if Rpc.get_debug ()
                          then
                            Printf.eprintf
                              "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                              "t" "__x1240__" (Rpc.to_string __x__)
                              "Enum[]/Enum[_]"
                          else ();
                          raise (Rpc.Runtime_error ("Enum[]/Enum[_]", __x__))))))
           | Rpc.Enum ((Rpc.String "vif_t")::__x1244__::__x1245__::[]) ->
               Vif_t
                 ((Vif.id_of_rpc __x1244__),
                   ((match __x1245__ with
                     | Rpc.Enum [] -> None
                     | Rpc.Enum (__x1246__::[]) ->
                         Some
                           ((match __x1246__ with
                             | Rpc.Enum (__x1247__::__x1248__::[]) ->
                                 ((Vif.t_of_rpc __x1247__),
                                   (Vif.state_of_rpc __x1248__))
                             | __x__ ->
                                 (if Rpc.get_debug ()
                                  then
                                    Printf.eprintf
                                      "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                      "t" "__x1246__" (Rpc.to_string __x__)
                                      "List"
                                  else ();
                                  raise (Rpc.Runtime_error ("List", __x__)))))
                     | __x__ ->
                         (if Rpc.get_debug ()
                          then
                            Printf.eprintf
                              "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                              "t" "__x1245__" (Rpc.to_string __x__)
                              "Enum[]/Enum[_]"
                          else ();
                          raise (Rpc.Runtime_error ("Enum[]/Enum[_]", __x__))))))
           | Rpc.Enum ((Rpc.String "vbd_t")::__x1249__::__x1250__::[]) ->
               Vbd_t
                 ((Vbd.id_of_rpc __x1249__),
                   ((match __x1250__ with
                     | Rpc.Enum [] -> None
                     | Rpc.Enum (__x1251__::[]) ->
                         Some
                           ((match __x1251__ with
                             | Rpc.Enum (__x1252__::__x1253__::[]) ->
                                 ((Vbd.t_of_rpc __x1252__),
                                   (Vbd.state_of_rpc __x1253__))
                             | __x__ ->
                                 (if Rpc.get_debug ()
                                  then
                                    Printf.eprintf
                                      "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                      "t" "__x1251__" (Rpc.to_string __x__)
                                      "List"
                                  else ();
                                  raise (Rpc.Runtime_error ("List", __x__)))))
                     | __x__ ->
                         (if Rpc.get_debug ()
                          then
                            Printf.eprintf
                              "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                              "t" "__x1250__" (Rpc.to_string __x__)
                              "Enum[]/Enum[_]"
                          else ();
                          raise (Rpc.Runtime_error ("Enum[]/Enum[_]", __x__))))))
           | Rpc.Enum ((Rpc.String "vm_t")::__x1254__::__x1255__::[]) ->
               Vm_t
                 ((Vm.id_of_rpc __x1254__),
                   ((match __x1255__ with
                     | Rpc.Enum [] -> None
                     | Rpc.Enum (__x1256__::[]) ->
                         Some
                           ((match __x1256__ with
                             | Rpc.Enum (__x1257__::__x1258__::[]) ->
                                 ((Vm.t_of_rpc __x1257__),
                                   (Vm.state_of_rpc __x1258__))
                             | __x__ ->
                                 (if Rpc.get_debug ()
                                  then
                                    Printf.eprintf
                                      "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                      "t" "__x1256__" (Rpc.to_string __x__)
                                      "List"
                                  else ();
                                  raise (Rpc.Runtime_error ("List", __x__)))))
                     | __x__ ->
                         (if Rpc.get_debug ()
                          then
                            Printf.eprintf
                              "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                              "t" "__x1255__" (Rpc.to_string __x__)
                              "Enum[]/Enum[_]"
                          else ();
                          raise (Rpc.Runtime_error ("Enum[]/Enum[_]", __x__))))))
           | __x__ ->
               (if Rpc.get_debug ()
                then
                  Printf.eprintf
                    "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                    "t" "__x1225__" (Rpc.to_string __x__)
                    "Enum[String s;...]"
                else ();
                raise (Rpc.Runtime_error ("Enum[String s;...]", __x__))))
  end
module Host =
  struct
    type cpu_info =
      {
      cpu_count: int ;
      socket_count: int ;
      vendor: string ;
      speed: string ;
      modelname: string ;
      family: string ;
      model: string ;
      stepping: string ;
      flags: string ;
      features: int64 array ;
      features_pv: int64 array ;
      features_hvm: int64 array ;
      features_oldstyle: int64 array }
    let rec rpc_of_cpu_info =
      function
      | __x1316__ ->
          let __x1317__ = __x1316__.cpu_count
          and __x1318__ = __x1316__.socket_count
          and __x1319__ = __x1316__.vendor
          and __x1320__ = __x1316__.speed
          and __x1321__ = __x1316__.modelname
          and __x1322__ = __x1316__.family
          and __x1323__ = __x1316__.model
          and __x1324__ = __x1316__.stepping
          and __x1325__ = __x1316__.flags
          and __x1326__ = __x1316__.features
          and __x1327__ = __x1316__.features_pv
          and __x1328__ = __x1316__.features_hvm
          and __x1329__ = __x1316__.features_oldstyle in
          Rpc.Dict
            [("cpu_count", (Rpc.Int (Int64.of_int __x1317__)));
            ("socket_count", (Rpc.Int (Int64.of_int __x1318__)));
            ("vendor", (Rpc.String __x1319__));
            ("speed", (Rpc.String __x1320__));
            ("modelname", (Rpc.String __x1321__));
            ("family", (Rpc.String __x1322__));
            ("model", (Rpc.String __x1323__));
            ("stepping", (Rpc.String __x1324__));
            ("flags", (Rpc.String __x1325__));
            ("features",
              (Rpc.Enum
                 (Array.to_list
                    (Array.map (function | __x1333__ -> Rpc.Int __x1333__)
                       __x1326__))));
            ("features_pv",
              (Rpc.Enum
                 (Array.to_list
                    (Array.map (function | __x1332__ -> Rpc.Int __x1332__)
                       __x1327__))));
            ("features_hvm",
              (Rpc.Enum
                 (Array.to_list
                    (Array.map (function | __x1331__ -> Rpc.Int __x1331__)
                       __x1328__))));
            ("features_oldstyle",
              (Rpc.Enum
                 (Array.to_list
                    (Array.map (function | __x1330__ -> Rpc.Int __x1330__)
                       __x1329__))))]
    and cpu_info_of_rpc =
      function
      | __x1293__ ->
          (match __x1293__ with
           | Rpc.Dict __x1294__ ->
               let __x1295__ =
                 try List.assoc "cpu_count" __x1294__
                 with
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                          "cpu_info" "__x1294__" (Printexc.to_string __x__)
                          "Looking for key cpu_count"
                      else ();
                      raise
                        (Rpc.Runtime_exception
                           ("Looking for key cpu_count",
                             (Printexc.to_string __x__))))
               and __x1296__ =
                 try List.assoc "socket_count" __x1294__
                 with
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                          "cpu_info" "__x1294__" (Printexc.to_string __x__)
                          "Looking for key socket_count"
                      else ();
                      raise
                        (Rpc.Runtime_exception
                           ("Looking for key socket_count",
                             (Printexc.to_string __x__))))
               and __x1297__ =
                 try List.assoc "vendor" __x1294__
                 with
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                          "cpu_info" "__x1294__" (Printexc.to_string __x__)
                          "Looking for key vendor"
                      else ();
                      raise
                        (Rpc.Runtime_exception
                           ("Looking for key vendor",
                             (Printexc.to_string __x__))))
               and __x1298__ =
                 try List.assoc "speed" __x1294__
                 with
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                          "cpu_info" "__x1294__" (Printexc.to_string __x__)
                          "Looking for key speed"
                      else ();
                      raise
                        (Rpc.Runtime_exception
                           ("Looking for key speed",
                             (Printexc.to_string __x__))))
               and __x1299__ =
                 try List.assoc "modelname" __x1294__
                 with
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                          "cpu_info" "__x1294__" (Printexc.to_string __x__)
                          "Looking for key modelname"
                      else ();
                      raise
                        (Rpc.Runtime_exception
                           ("Looking for key modelname",
                             (Printexc.to_string __x__))))
               and __x1300__ =
                 try List.assoc "family" __x1294__
                 with
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                          "cpu_info" "__x1294__" (Printexc.to_string __x__)
                          "Looking for key family"
                      else ();
                      raise
                        (Rpc.Runtime_exception
                           ("Looking for key family",
                             (Printexc.to_string __x__))))
               and __x1301__ =
                 try List.assoc "model" __x1294__
                 with
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                          "cpu_info" "__x1294__" (Printexc.to_string __x__)
                          "Looking for key model"
                      else ();
                      raise
                        (Rpc.Runtime_exception
                           ("Looking for key model",
                             (Printexc.to_string __x__))))
               and __x1302__ =
                 try List.assoc "stepping" __x1294__
                 with
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                          "cpu_info" "__x1294__" (Printexc.to_string __x__)
                          "Looking for key stepping"
                      else ();
                      raise
                        (Rpc.Runtime_exception
                           ("Looking for key stepping",
                             (Printexc.to_string __x__))))
               and __x1303__ =
                 try List.assoc "flags" __x1294__
                 with
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                          "cpu_info" "__x1294__" (Printexc.to_string __x__)
                          "Looking for key flags"
                      else ();
                      raise
                        (Rpc.Runtime_exception
                           ("Looking for key flags",
                             (Printexc.to_string __x__))))
               and __x1304__ =
                 try List.assoc "features" __x1294__
                 with
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                          "cpu_info" "__x1294__" (Printexc.to_string __x__)
                          "Looking for key features"
                      else ();
                      raise
                        (Rpc.Runtime_exception
                           ("Looking for key features",
                             (Printexc.to_string __x__))))
               and __x1305__ =
                 try List.assoc "features_pv" __x1294__
                 with
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                          "cpu_info" "__x1294__" (Printexc.to_string __x__)
                          "Looking for key features_pv"
                      else ();
                      raise
                        (Rpc.Runtime_exception
                           ("Looking for key features_pv",
                             (Printexc.to_string __x__))))
               and __x1306__ =
                 try List.assoc "features_hvm" __x1294__
                 with
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                          "cpu_info" "__x1294__" (Printexc.to_string __x__)
                          "Looking for key features_hvm"
                      else ();
                      raise
                        (Rpc.Runtime_exception
                           ("Looking for key features_hvm",
                             (Printexc.to_string __x__))))
               and __x1307__ =
                 try List.assoc "features_oldstyle" __x1294__
                 with
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                          "cpu_info" "__x1294__" (Printexc.to_string __x__)
                          "Looking for key features_oldstyle"
                      else ();
                      raise
                        (Rpc.Runtime_exception
                           ("Looking for key features_oldstyle",
                             (Printexc.to_string __x__)))) in
               {
                 cpu_count =
                   ((match __x1295__ with
                     | Rpc.Int x -> Int64.to_int x
                     | Rpc.String s -> int_of_string s
                     | __x__ ->
                         (if Rpc.get_debug ()
                          then
                            Printf.eprintf
                              "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                              "cpu_info" "__x1295__" (Rpc.to_string __x__)
                              "Int(int)"
                          else ();
                          raise (Rpc.Runtime_error ("Int(int)", __x__)))));
                 socket_count =
                   ((match __x1296__ with
                     | Rpc.Int x -> Int64.to_int x
                     | Rpc.String s -> int_of_string s
                     | __x__ ->
                         (if Rpc.get_debug ()
                          then
                            Printf.eprintf
                              "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                              "cpu_info" "__x1296__" (Rpc.to_string __x__)
                              "Int(int)"
                          else ();
                          raise (Rpc.Runtime_error ("Int(int)", __x__)))));
                 vendor =
                   ((match __x1297__ with
                     | Rpc.String x -> x
                     | __x__ ->
                         (if Rpc.get_debug ()
                          then
                            Printf.eprintf
                              "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                              "cpu_info" "__x1297__" (Rpc.to_string __x__)
                              "String(string)"
                          else ();
                          raise (Rpc.Runtime_error ("String(string)", __x__)))));
                 speed =
                   ((match __x1298__ with
                     | Rpc.String x -> x
                     | __x__ ->
                         (if Rpc.get_debug ()
                          then
                            Printf.eprintf
                              "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                              "cpu_info" "__x1298__" (Rpc.to_string __x__)
                              "String(string)"
                          else ();
                          raise (Rpc.Runtime_error ("String(string)", __x__)))));
                 modelname =
                   ((match __x1299__ with
                     | Rpc.String x -> x
                     | __x__ ->
                         (if Rpc.get_debug ()
                          then
                            Printf.eprintf
                              "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                              "cpu_info" "__x1299__" (Rpc.to_string __x__)
                              "String(string)"
                          else ();
                          raise (Rpc.Runtime_error ("String(string)", __x__)))));
                 family =
                   ((match __x1300__ with
                     | Rpc.String x -> x
                     | __x__ ->
                         (if Rpc.get_debug ()
                          then
                            Printf.eprintf
                              "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                              "cpu_info" "__x1300__" (Rpc.to_string __x__)
                              "String(string)"
                          else ();
                          raise (Rpc.Runtime_error ("String(string)", __x__)))));
                 model =
                   ((match __x1301__ with
                     | Rpc.String x -> x
                     | __x__ ->
                         (if Rpc.get_debug ()
                          then
                            Printf.eprintf
                              "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                              "cpu_info" "__x1301__" (Rpc.to_string __x__)
                              "String(string)"
                          else ();
                          raise (Rpc.Runtime_error ("String(string)", __x__)))));
                 stepping =
                   ((match __x1302__ with
                     | Rpc.String x -> x
                     | __x__ ->
                         (if Rpc.get_debug ()
                          then
                            Printf.eprintf
                              "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                              "cpu_info" "__x1302__" (Rpc.to_string __x__)
                              "String(string)"
                          else ();
                          raise (Rpc.Runtime_error ("String(string)", __x__)))));
                 flags =
                   ((match __x1303__ with
                     | Rpc.String x -> x
                     | __x__ ->
                         (if Rpc.get_debug ()
                          then
                            Printf.eprintf
                              "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                              "cpu_info" "__x1303__" (Rpc.to_string __x__)
                              "String(string)"
                          else ();
                          raise (Rpc.Runtime_error ("String(string)", __x__)))));
                 features =
                   ((match __x1304__ with
                     | Rpc.Enum __x1308__ ->
                         Array.of_list
                           (List.map
                              (function
                               | __x1309__ ->
                                   (match __x1309__ with
                                    | Rpc.Int x -> x
                                    | Rpc.String s -> Int64.of_string s
                                    | __x__ ->
                                        (if Rpc.get_debug ()
                                         then
                                           Printf.eprintf
                                             "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                             "cpu_info" "__x1309__"
                                             (Rpc.to_string __x__)
                                             "Int(int64)"
                                         else ();
                                         raise
                                           (Rpc.Runtime_error
                                              ("Int(int64)", __x__)))))
                              __x1308__)
                     | __x__ ->
                         (if Rpc.get_debug ()
                          then
                            Printf.eprintf
                              "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                              "cpu_info" "__x1304__" (Rpc.to_string __x__)
                              "List"
                          else ();
                          raise (Rpc.Runtime_error ("List", __x__)))));
                 features_pv =
                   ((match __x1305__ with
                     | Rpc.Enum __x1310__ ->
                         Array.of_list
                           (List.map
                              (function
                               | __x1311__ ->
                                   (match __x1311__ with
                                    | Rpc.Int x -> x
                                    | Rpc.String s -> Int64.of_string s
                                    | __x__ ->
                                        (if Rpc.get_debug ()
                                         then
                                           Printf.eprintf
                                             "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                             "cpu_info" "__x1311__"
                                             (Rpc.to_string __x__)
                                             "Int(int64)"
                                         else ();
                                         raise
                                           (Rpc.Runtime_error
                                              ("Int(int64)", __x__)))))
                              __x1310__)
                     | __x__ ->
                         (if Rpc.get_debug ()
                          then
                            Printf.eprintf
                              "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                              "cpu_info" "__x1305__" (Rpc.to_string __x__)
                              "List"
                          else ();
                          raise (Rpc.Runtime_error ("List", __x__)))));
                 features_hvm =
                   ((match __x1306__ with
                     | Rpc.Enum __x1312__ ->
                         Array.of_list
                           (List.map
                              (function
                               | __x1313__ ->
                                   (match __x1313__ with
                                    | Rpc.Int x -> x
                                    | Rpc.String s -> Int64.of_string s
                                    | __x__ ->
                                        (if Rpc.get_debug ()
                                         then
                                           Printf.eprintf
                                             "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                             "cpu_info" "__x1313__"
                                             (Rpc.to_string __x__)
                                             "Int(int64)"
                                         else ();
                                         raise
                                           (Rpc.Runtime_error
                                              ("Int(int64)", __x__)))))
                              __x1312__)
                     | __x__ ->
                         (if Rpc.get_debug ()
                          then
                            Printf.eprintf
                              "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                              "cpu_info" "__x1306__" (Rpc.to_string __x__)
                              "List"
                          else ();
                          raise (Rpc.Runtime_error ("List", __x__)))));
                 features_oldstyle =
                   ((match __x1307__ with
                     | Rpc.Enum __x1314__ ->
                         Array.of_list
                           (List.map
                              (function
                               | __x1315__ ->
                                   (match __x1315__ with
                                    | Rpc.Int x -> x
                                    | Rpc.String s -> Int64.of_string s
                                    | __x__ ->
                                        (if Rpc.get_debug ()
                                         then
                                           Printf.eprintf
                                             "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                             "cpu_info" "__x1315__"
                                             (Rpc.to_string __x__)
                                             "Int(int64)"
                                         else ();
                                         raise
                                           (Rpc.Runtime_error
                                              ("Int(int64)", __x__)))))
                              __x1314__)
                     | __x__ ->
                         (if Rpc.get_debug ()
                          then
                            Printf.eprintf
                              "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                              "cpu_info" "__x1307__" (Rpc.to_string __x__)
                              "List"
                          else ();
                          raise (Rpc.Runtime_error ("List", __x__)))))
               }
           | __x__ ->
               (if Rpc.get_debug ()
                then
                  Printf.eprintf
                    "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                    "cpu_info" "__x1293__" (Rpc.to_string __x__) "Dict"
                else ();
                raise (Rpc.Runtime_error ("Dict", __x__))))
    type hypervisor = {
      version: string ;
      capabilities: string }
    let rec rpc_of_hypervisor =
      function
      | __x1338__ ->
          let __x1339__ = __x1338__.version
          and __x1340__ = __x1338__.capabilities in
          Rpc.Dict
            [("version", (Rpc.String __x1339__));
            ("capabilities", (Rpc.String __x1340__))]
    and hypervisor_of_rpc =
      function
      | __x1334__ ->
          (match __x1334__ with
           | Rpc.Dict __x1335__ ->
               let __x1336__ =
                 try List.assoc "version" __x1335__
                 with
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                          "hypervisor" "__x1335__" (Printexc.to_string __x__)
                          "Looking for key version"
                      else ();
                      raise
                        (Rpc.Runtime_exception
                           ("Looking for key version",
                             (Printexc.to_string __x__))))
               and __x1337__ =
                 try List.assoc "capabilities" __x1335__
                 with
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                          "hypervisor" "__x1335__" (Printexc.to_string __x__)
                          "Looking for key capabilities"
                      else ();
                      raise
                        (Rpc.Runtime_exception
                           ("Looking for key capabilities",
                             (Printexc.to_string __x__)))) in
               {
                 version =
                   ((match __x1336__ with
                     | Rpc.String x -> x
                     | __x__ ->
                         (if Rpc.get_debug ()
                          then
                            Printf.eprintf
                              "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                              "hypervisor" "__x1336__" (Rpc.to_string __x__)
                              "String(string)"
                          else ();
                          raise (Rpc.Runtime_error ("String(string)", __x__)))));
                 capabilities =
                   ((match __x1337__ with
                     | Rpc.String x -> x
                     | __x__ ->
                         (if Rpc.get_debug ()
                          then
                            Printf.eprintf
                              "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                              "hypervisor" "__x1337__" (Rpc.to_string __x__)
                              "String(string)"
                          else ();
                          raise (Rpc.Runtime_error ("String(string)", __x__)))))
               }
           | __x__ ->
               (if Rpc.get_debug ()
                then
                  Printf.eprintf
                    "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                    "hypervisor" "__x1334__" (Rpc.to_string __x__) "Dict"
                else ();
                raise (Rpc.Runtime_error ("Dict", __x__))))
    type t = {
      cpu_info: cpu_info ;
      hypervisor: hypervisor }
    let rec rpc_of_t =
      function
      | __x1345__ ->
          let __x1346__ = __x1345__.cpu_info
          and __x1347__ = __x1345__.hypervisor in
          Rpc.Dict
            [("cpu_info", (rpc_of_cpu_info __x1346__));
            ("hypervisor", (rpc_of_hypervisor __x1347__))]
    and t_of_rpc =
      function
      | __x1341__ ->
          (match __x1341__ with
           | Rpc.Dict __x1342__ ->
               let __x1343__ =
                 try List.assoc "cpu_info" __x1342__
                 with
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                          "t" "__x1342__" (Printexc.to_string __x__)
                          "Looking for key cpu_info"
                      else ();
                      raise
                        (Rpc.Runtime_exception
                           ("Looking for key cpu_info",
                             (Printexc.to_string __x__))))
               and __x1344__ =
                 try List.assoc "hypervisor" __x1342__
                 with
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                          "t" "__x1342__" (Printexc.to_string __x__)
                          "Looking for key hypervisor"
                      else ();
                      raise
                        (Rpc.Runtime_exception
                           ("Looking for key hypervisor",
                             (Printexc.to_string __x__)))) in
               {
                 cpu_info = (cpu_info_of_rpc __x1343__);
                 hypervisor = (hypervisor_of_rpc __x1344__)
               }
           | __x__ ->
               (if Rpc.get_debug ()
                then
                  Printf.eprintf
                    "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                    "t" "__x1341__" (Rpc.to_string __x__) "Dict"
                else ();
                raise (Rpc.Runtime_error ("Dict", __x__))))
    type guest_agent_feature =
      {
      name: string ;
      licensed: bool ;
      parameters: (string * string) list }
    let rec rpc_of_guest_agent_feature =
      function
      | __x1354__ ->
          let __x1355__ = __x1354__.name
          and __x1356__ = __x1354__.licensed
          and __x1357__ = __x1354__.parameters in
          Rpc.Dict
            [("name", (Rpc.String __x1355__));
            ("licensed", (Rpc.Bool __x1356__));
            ("parameters",
              ((let dict =
                  List.map
                    (function
                     | (key, __x1358__) -> (key, (Rpc.String __x1358__)))
                    __x1357__ in
                Rpc.Dict dict)))]
    and guest_agent_feature_of_rpc =
      function
      | __x1348__ ->
          (match __x1348__ with
           | Rpc.Dict __x1349__ ->
               let __x1350__ =
                 try List.assoc "name" __x1349__
                 with
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                          "guest_agent_feature" "__x1349__"
                          (Printexc.to_string __x__) "Looking for key name"
                      else ();
                      raise
                        (Rpc.Runtime_exception
                           ("Looking for key name",
                             (Printexc.to_string __x__))))
               and __x1351__ =
                 try List.assoc "licensed" __x1349__
                 with
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                          "guest_agent_feature" "__x1349__"
                          (Printexc.to_string __x__)
                          "Looking for key licensed"
                      else ();
                      raise
                        (Rpc.Runtime_exception
                           ("Looking for key licensed",
                             (Printexc.to_string __x__))))
               and __x1352__ =
                 try List.assoc "parameters" __x1349__
                 with
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                          "guest_agent_feature" "__x1349__"
                          (Printexc.to_string __x__)
                          "Looking for key parameters"
                      else ();
                      raise
                        (Rpc.Runtime_exception
                           ("Looking for key parameters",
                             (Printexc.to_string __x__)))) in
               {
                 name =
                   ((match __x1350__ with
                     | Rpc.String x -> x
                     | __x__ ->
                         (if Rpc.get_debug ()
                          then
                            Printf.eprintf
                              "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                              "guest_agent_feature" "__x1350__"
                              (Rpc.to_string __x__) "String(string)"
                          else ();
                          raise (Rpc.Runtime_error ("String(string)", __x__)))));
                 licensed =
                   ((match __x1351__ with
                     | Rpc.Bool x -> x
                     | __x__ ->
                         (if Rpc.get_debug ()
                          then
                            Printf.eprintf
                              "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                              "guest_agent_feature" "__x1351__"
                              (Rpc.to_string __x__) "Bool"
                          else ();
                          raise (Rpc.Runtime_error ("Bool", __x__)))));
                 parameters =
                   ((match __x1352__ with
                     | Rpc.Dict d ->
                         List.map
                           (function
                            | (key, __x1353__) ->
                                (key,
                                  ((match __x1353__ with
                                    | Rpc.String x -> x
                                    | __x__ ->
                                        (if Rpc.get_debug ()
                                         then
                                           Printf.eprintf
                                             "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                             "guest_agent_feature"
                                             "__x1353__"
                                             (Rpc.to_string __x__)
                                             "String(string)"
                                         else ();
                                         raise
                                           (Rpc.Runtime_error
                                              ("String(string)", __x__)))))))
                           d
                     | __x__ ->
                         (if Rpc.get_debug ()
                          then
                            Printf.eprintf
                              "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                              "guest_agent_feature" "__x1352__"
                              (Rpc.to_string __x__) "Dict"
                          else ();
                          raise (Rpc.Runtime_error ("Dict", __x__)))))
               }
           | __x__ ->
               (if Rpc.get_debug ()
                then
                  Printf.eprintf
                    "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                    "guest_agent_feature" "__x1348__" (Rpc.to_string __x__)
                    "Dict"
                else ();
                raise (Rpc.Runtime_error ("Dict", __x__))))
  end
module TASK = struct  end
module HOST = struct  end
module VM = struct  end
module PCI = struct  end
module VBD = struct  end
module VUSB = struct  end
module VIF = struct  end
module VGPU = struct  end
module UPDATES = struct  end
module DEBUG = struct  end
type __exn_ty30 = (string * string)
type __exn_ty29 = (string * string)
type __exn_ty28 = string
type __exn_ty26 = int
type __exn_ty25 = (power_state * power_state)
type __exn_ty23 = (string * float)
type __exn_ty20 = (string * string * string)
type __exn_ty15 = (string * string)
type __exn_ty14 = (int64 * int64)
type __exn_ty13 = string list
type __exn_ty11 = string
type __exn_ty10 = (string * string * string * string)
type __exn_ty9 = int64
type __exn_ty8 = string
type __exn_ty7 = (string * string list)
type __exn_ty5 = string
type __exn_ty4 = (string * string * string)
type __exn_ty2 = string
type __exn_ty1 = (string * int * int)
type __exn_ty0 = string
exception Already_exists of __exn_ty30 
exception Does_not_exist of __exn_ty29 
exception Unimplemented of __exn_ty28 
exception Domain_not_built 
exception Invalid_vcpus of __exn_ty26 
exception Bad_power_state of __exn_ty25 
exception Failed_to_acknowledge_shutdown_request 
exception Failed_to_shutdown of __exn_ty23 
exception Device_is_connected 
exception Device_not_connected 
exception Device_detach_rejected of __exn_ty20 
exception Media_not_ejectable 
exception Media_present 
exception Media_not_present 
exception No_bootable_device 
exception Bootloader_error of __exn_ty15 
exception Cannot_free_this_much_memory of __exn_ty14 
exception Vms_failed_to_cooperate of __exn_ty13 
exception IO_error 
exception Failed_to_contact_remote_service of __exn_ty11 
exception Hook_failed of __exn_ty10 
exception Not_enough_memory of __exn_ty9 
exception Cancelled of __exn_ty8 
exception Storage_backend_error of __exn_ty7 
exception PCIBack_not_loaded 
exception Failed_to_run_script of __exn_ty5 
exception Failed_to_start_emulator of __exn_ty4 
exception Ballooning_timeout_before_migration 
exception Unknown_RPC of __exn_ty2 
exception Message_param_count_mismatch of __exn_ty1 
exception Internal_error of __exn_ty0 
module Exception =
  struct
    type exnty =
      | Internal_error of string 
      | Message_param_count_mismatch of (string * int * int) 
      | Unknown_RPC of string 
      | Ballooning_timeout_before_migration 
      | Failed_to_start_emulator of (string * string * string) 
      | Failed_to_run_script of string 
      | PCIBack_not_loaded 
      | Storage_backend_error of (string * string list) 
      | Cancelled of string 
      | Not_enough_memory of int64 
      | Hook_failed of (string * string * string * string) 
      | Failed_to_contact_remote_service of string 
      | IO_error 
      | Vms_failed_to_cooperate of string list 
      | Cannot_free_this_much_memory of (int64 * int64) 
      | Bootloader_error of (string * string) 
      | No_bootable_device 
      | Media_not_present 
      | Media_present 
      | Media_not_ejectable 
      | Device_detach_rejected of (string * string * string) 
      | Device_not_connected 
      | Device_is_connected 
      | Failed_to_shutdown of (string * float) 
      | Failed_to_acknowledge_shutdown_request 
      | Bad_power_state of (power_state * power_state) 
      | Invalid_vcpus of int 
      | Domain_not_built 
      | Unimplemented of string 
      | Does_not_exist of (string * string) 
      | Already_exists of (string * string) 
    let rpc_of_exnty =
      function
      | __x53__ ->
          (match __x53__ with
           | Already_exists __x54__ ->
               Rpc.Enum
                 [Rpc.String "Already_exists";
                 (let (__x55__, __x56__) = __x54__ in
                  Rpc.Enum [Rpc.String __x55__; Rpc.String __x56__])]
           | Does_not_exist __x57__ ->
               Rpc.Enum
                 [Rpc.String "Does_not_exist";
                 (let (__x58__, __x59__) = __x57__ in
                  Rpc.Enum [Rpc.String __x58__; Rpc.String __x59__])]
           | Unimplemented __x60__ ->
               Rpc.Enum [Rpc.String "Unimplemented"; Rpc.String __x60__]
           | Domain_not_built -> Rpc.String "Domain_not_built"
           | Invalid_vcpus __x61__ ->
               Rpc.Enum
                 [Rpc.String "Invalid_vcpus"; Rpc.Int (Int64.of_int __x61__)]
           | Bad_power_state __x62__ ->
               Rpc.Enum
                 [Rpc.String "Bad_power_state";
                 (let (__x63__, __x64__) = __x62__ in
                  Rpc.Enum
                    [rpc_of_power_state __x63__; rpc_of_power_state __x64__])]
           | Failed_to_acknowledge_shutdown_request ->
               Rpc.String "Failed_to_acknowledge_shutdown_request"
           | Failed_to_shutdown __x65__ ->
               Rpc.Enum
                 [Rpc.String "Failed_to_shutdown";
                 (let (__x66__, __x67__) = __x65__ in
                  Rpc.Enum [Rpc.String __x66__; Rpc.Float __x67__])]
           | Device_is_connected -> Rpc.String "Device_is_connected"
           | Device_not_connected -> Rpc.String "Device_not_connected"
           | Device_detach_rejected __x68__ ->
               Rpc.Enum
                 [Rpc.String "Device_detach_rejected";
                 (let (__x69__, __x70__, __x71__) = __x68__ in
                  Rpc.Enum
                    [Rpc.String __x69__;
                    Rpc.String __x70__;
                    Rpc.String __x71__])]
           | Media_not_ejectable -> Rpc.String "Media_not_ejectable"
           | Media_present -> Rpc.String "Media_present"
           | Media_not_present -> Rpc.String "Media_not_present"
           | No_bootable_device -> Rpc.String "No_bootable_device"
           | Bootloader_error __x72__ ->
               Rpc.Enum
                 [Rpc.String "Bootloader_error";
                 (let (__x73__, __x74__) = __x72__ in
                  Rpc.Enum [Rpc.String __x73__; Rpc.String __x74__])]
           | Cannot_free_this_much_memory __x75__ ->
               Rpc.Enum
                 [Rpc.String "Cannot_free_this_much_memory";
                 (let (__x76__, __x77__) = __x75__ in
                  Rpc.Enum [Rpc.Int __x76__; Rpc.Int __x77__])]
           | Vms_failed_to_cooperate __x78__ ->
               Rpc.Enum
                 [Rpc.String "Vms_failed_to_cooperate";
                 Rpc.Enum
                   (List.map (function | __x79__ -> Rpc.String __x79__)
                      __x78__)]
           | IO_error -> Rpc.String "IO_error"
           | Failed_to_contact_remote_service __x80__ ->
               Rpc.Enum
                 [Rpc.String "Failed_to_contact_remote_service";
                 Rpc.String __x80__]
           | Hook_failed __x81__ ->
               Rpc.Enum
                 [Rpc.String "Hook_failed";
                 (let (__x82__, __x83__, __x84__, __x85__) = __x81__ in
                  Rpc.Enum
                    [Rpc.String __x82__;
                    Rpc.String __x83__;
                    Rpc.String __x84__;
                    Rpc.String __x85__])]
           | Not_enough_memory __x86__ ->
               Rpc.Enum [Rpc.String "Not_enough_memory"; Rpc.Int __x86__]
           | Cancelled __x87__ ->
               Rpc.Enum [Rpc.String "Cancelled"; Rpc.String __x87__]
           | Storage_backend_error __x88__ ->
               Rpc.Enum
                 [Rpc.String "Storage_backend_error";
                 (let (__x89__, __x90__) = __x88__ in
                  Rpc.Enum
                    [Rpc.String __x89__;
                    Rpc.Enum
                      (List.map (function | __x91__ -> Rpc.String __x91__)
                         __x90__)])]
           | PCIBack_not_loaded -> Rpc.String "PCIBack_not_loaded"
           | Failed_to_run_script __x92__ ->
               Rpc.Enum
                 [Rpc.String "Failed_to_run_script"; Rpc.String __x92__]
           | Failed_to_start_emulator __x93__ ->
               Rpc.Enum
                 [Rpc.String "Failed_to_start_emulator";
                 (let (__x94__, __x95__, __x96__) = __x93__ in
                  Rpc.Enum
                    [Rpc.String __x94__;
                    Rpc.String __x95__;
                    Rpc.String __x96__])]
           | Ballooning_timeout_before_migration ->
               Rpc.String "Ballooning_timeout_before_migration"
           | Unknown_RPC __x97__ ->
               Rpc.Enum [Rpc.String "Unknown_RPC"; Rpc.String __x97__]
           | Message_param_count_mismatch __x98__ ->
               Rpc.Enum
                 [Rpc.String "Message_param_count_mismatch";
                 (let (__x99__, __x100__, __x101__) = __x98__ in
                  Rpc.Enum
                    [Rpc.String __x99__;
                    Rpc.Int (Int64.of_int __x100__);
                    Rpc.Int (Int64.of_int __x101__)])]
           | Internal_error __x102__ ->
               Rpc.Enum [Rpc.String "Internal_error"; Rpc.String __x102__])
    let exnty_of_rpc =
      function
      | __x1__ ->
          (match Rpc.lowerfn __x1__ with
           | Rpc.Enum ((Rpc.String "already_exists")::__x2__::[]) ->
               Already_exists
                 ((match __x2__ with
                   | Rpc.Enum (__x3__::__x4__::[]) ->
                       (((match __x3__ with
                          | Rpc.String x -> x
                          | __x__ ->
                              (if Rpc.get_debug ()
                               then
                                 Printf.eprintf
                                   "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                   "exnty" "__x3__" (Rpc.to_string __x__)
                                   "String(string)"
                               else ();
                               raise
                                 (Rpc.Runtime_error ("String(string)", __x__))))),
                         ((match __x4__ with
                           | Rpc.String x -> x
                           | __x__ ->
                               (if Rpc.get_debug ()
                                then
                                  Printf.eprintf
                                    "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                    "exnty" "__x4__" (Rpc.to_string __x__)
                                    "String(string)"
                                else ();
                                raise
                                  (Rpc.Runtime_error
                                     ("String(string)", __x__))))))
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "exnty" "__x2__" (Rpc.to_string __x__) "List"
                        else ();
                        raise (Rpc.Runtime_error ("List", __x__)))))
           | Rpc.Enum ((Rpc.String "does_not_exist")::__x5__::[]) ->
               Does_not_exist
                 ((match __x5__ with
                   | Rpc.Enum (__x6__::__x7__::[]) ->
                       (((match __x6__ with
                          | Rpc.String x -> x
                          | __x__ ->
                              (if Rpc.get_debug ()
                               then
                                 Printf.eprintf
                                   "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                   "exnty" "__x6__" (Rpc.to_string __x__)
                                   "String(string)"
                               else ();
                               raise
                                 (Rpc.Runtime_error ("String(string)", __x__))))),
                         ((match __x7__ with
                           | Rpc.String x -> x
                           | __x__ ->
                               (if Rpc.get_debug ()
                                then
                                  Printf.eprintf
                                    "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                    "exnty" "__x7__" (Rpc.to_string __x__)
                                    "String(string)"
                                else ();
                                raise
                                  (Rpc.Runtime_error
                                     ("String(string)", __x__))))))
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "exnty" "__x5__" (Rpc.to_string __x__) "List"
                        else ();
                        raise (Rpc.Runtime_error ("List", __x__)))))
           | Rpc.Enum ((Rpc.String "unimplemented")::__x8__::[]) ->
               Unimplemented
                 ((match __x8__ with
                   | Rpc.String x -> x
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "exnty" "__x8__" (Rpc.to_string __x__)
                            "String(string)"
                        else ();
                        raise (Rpc.Runtime_error ("String(string)", __x__)))))
           | Rpc.String "domain_not_built" -> Domain_not_built
           | Rpc.Enum ((Rpc.String "invalid_vcpus")::__x9__::[]) ->
               Invalid_vcpus
                 ((match __x9__ with
                   | Rpc.Int x -> Int64.to_int x
                   | Rpc.String s -> int_of_string s
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "exnty" "__x9__" (Rpc.to_string __x__) "Int(int)"
                        else ();
                        raise (Rpc.Runtime_error ("Int(int)", __x__)))))
           | Rpc.Enum ((Rpc.String "bad_power_state")::__x10__::[]) ->
               Bad_power_state
                 ((match __x10__ with
                   | Rpc.Enum (__x11__::__x12__::[]) ->
                       ((power_state_of_rpc __x11__),
                         (power_state_of_rpc __x12__))
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "exnty" "__x10__" (Rpc.to_string __x__) "List"
                        else ();
                        raise (Rpc.Runtime_error ("List", __x__)))))
           | Rpc.String "failed_to_acknowledge_shutdown_request" ->
               Failed_to_acknowledge_shutdown_request
           | Rpc.Enum ((Rpc.String "failed_to_shutdown")::__x13__::[]) ->
               Failed_to_shutdown
                 ((match __x13__ with
                   | Rpc.Enum (__x14__::__x15__::[]) ->
                       (((match __x14__ with
                          | Rpc.String x -> x
                          | __x__ ->
                              (if Rpc.get_debug ()
                               then
                                 Printf.eprintf
                                   "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                   "exnty" "__x14__" (Rpc.to_string __x__)
                                   "String(string)"
                               else ();
                               raise
                                 (Rpc.Runtime_error ("String(string)", __x__))))),
                         ((match __x15__ with
                           | Rpc.Float x -> x
                           | Rpc.String s -> float_of_string s
                           | __x__ ->
                               (if Rpc.get_debug ()
                                then
                                  Printf.eprintf
                                    "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                    "exnty" "__x15__" (Rpc.to_string __x__)
                                    "Float"
                                else ();
                                raise (Rpc.Runtime_error ("Float", __x__))))))
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "exnty" "__x13__" (Rpc.to_string __x__) "List"
                        else ();
                        raise (Rpc.Runtime_error ("List", __x__)))))
           | Rpc.String "device_is_connected" -> Device_is_connected
           | Rpc.String "device_not_connected" -> Device_not_connected
           | Rpc.Enum ((Rpc.String "device_detach_rejected")::__x16__::[]) ->
               Device_detach_rejected
                 ((match __x16__ with
                   | Rpc.Enum (__x17__::__x18__::__x19__::[]) ->
                       (((match __x17__ with
                          | Rpc.String x -> x
                          | __x__ ->
                              (if Rpc.get_debug ()
                               then
                                 Printf.eprintf
                                   "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                   "exnty" "__x17__" (Rpc.to_string __x__)
                                   "String(string)"
                               else ();
                               raise
                                 (Rpc.Runtime_error ("String(string)", __x__))))),
                         ((match __x18__ with
                           | Rpc.String x -> x
                           | __x__ ->
                               (if Rpc.get_debug ()
                                then
                                  Printf.eprintf
                                    "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                    "exnty" "__x18__" (Rpc.to_string __x__)
                                    "String(string)"
                                else ();
                                raise
                                  (Rpc.Runtime_error
                                     ("String(string)", __x__))))),
                         ((match __x19__ with
                           | Rpc.String x -> x
                           | __x__ ->
                               (if Rpc.get_debug ()
                                then
                                  Printf.eprintf
                                    "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                    "exnty" "__x19__" (Rpc.to_string __x__)
                                    "String(string)"
                                else ();
                                raise
                                  (Rpc.Runtime_error
                                     ("String(string)", __x__))))))
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "exnty" "__x16__" (Rpc.to_string __x__) "List"
                        else ();
                        raise (Rpc.Runtime_error ("List", __x__)))))
           | Rpc.String "media_not_ejectable" -> Media_not_ejectable
           | Rpc.String "media_present" -> Media_present
           | Rpc.String "media_not_present" -> Media_not_present
           | Rpc.String "no_bootable_device" -> No_bootable_device
           | Rpc.Enum ((Rpc.String "bootloader_error")::__x20__::[]) ->
               Bootloader_error
                 ((match __x20__ with
                   | Rpc.Enum (__x21__::__x22__::[]) ->
                       (((match __x21__ with
                          | Rpc.String x -> x
                          | __x__ ->
                              (if Rpc.get_debug ()
                               then
                                 Printf.eprintf
                                   "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                   "exnty" "__x21__" (Rpc.to_string __x__)
                                   "String(string)"
                               else ();
                               raise
                                 (Rpc.Runtime_error ("String(string)", __x__))))),
                         ((match __x22__ with
                           | Rpc.String x -> x
                           | __x__ ->
                               (if Rpc.get_debug ()
                                then
                                  Printf.eprintf
                                    "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                    "exnty" "__x22__" (Rpc.to_string __x__)
                                    "String(string)"
                                else ();
                                raise
                                  (Rpc.Runtime_error
                                     ("String(string)", __x__))))))
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "exnty" "__x20__" (Rpc.to_string __x__) "List"
                        else ();
                        raise (Rpc.Runtime_error ("List", __x__)))))
           | Rpc.Enum ((Rpc.String
               "cannot_free_this_much_memory")::__x23__::[]) ->
               Cannot_free_this_much_memory
                 ((match __x23__ with
                   | Rpc.Enum (__x24__::__x25__::[]) ->
                       (((match __x24__ with
                          | Rpc.Int x -> x
                          | Rpc.String s -> Int64.of_string s
                          | __x__ ->
                              (if Rpc.get_debug ()
                               then
                                 Printf.eprintf
                                   "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                   "exnty" "__x24__" (Rpc.to_string __x__)
                                   "Int(int64)"
                               else ();
                               raise
                                 (Rpc.Runtime_error ("Int(int64)", __x__))))),
                         ((match __x25__ with
                           | Rpc.Int x -> x
                           | Rpc.String s -> Int64.of_string s
                           | __x__ ->
                               (if Rpc.get_debug ()
                                then
                                  Printf.eprintf
                                    "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                    "exnty" "__x25__" (Rpc.to_string __x__)
                                    "Int(int64)"
                                else ();
                                raise
                                  (Rpc.Runtime_error ("Int(int64)", __x__))))))
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "exnty" "__x23__" (Rpc.to_string __x__) "List"
                        else ();
                        raise (Rpc.Runtime_error ("List", __x__)))))
           | Rpc.Enum ((Rpc.String "vms_failed_to_cooperate")::__x26__::[])
               ->
               Vms_failed_to_cooperate
                 ((match __x26__ with
                   | Rpc.Enum __x27__ ->
                       List.map
                         (function
                          | __x28__ ->
                              (match __x28__ with
                               | Rpc.String x -> x
                               | __x__ ->
                                   (if Rpc.get_debug ()
                                    then
                                      Printf.eprintf
                                        "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                        "exnty" "__x28__"
                                        (Rpc.to_string __x__)
                                        "String(string)"
                                    else ();
                                    raise
                                      (Rpc.Runtime_error
                                         ("String(string)", __x__)))))
                         __x27__
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "exnty" "__x26__" (Rpc.to_string __x__) "List"
                        else ();
                        raise (Rpc.Runtime_error ("List", __x__)))))
           | Rpc.String "io_error" -> IO_error
           | Rpc.Enum ((Rpc.String
               "failed_to_contact_remote_service")::__x29__::[]) ->
               Failed_to_contact_remote_service
                 ((match __x29__ with
                   | Rpc.String x -> x
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "exnty" "__x29__" (Rpc.to_string __x__)
                            "String(string)"
                        else ();
                        raise (Rpc.Runtime_error ("String(string)", __x__)))))
           | Rpc.Enum ((Rpc.String "hook_failed")::__x30__::[]) ->
               Hook_failed
                 ((match __x30__ with
                   | Rpc.Enum (__x31__::__x32__::__x33__::__x34__::[]) ->
                       (((match __x31__ with
                          | Rpc.String x -> x
                          | __x__ ->
                              (if Rpc.get_debug ()
                               then
                                 Printf.eprintf
                                   "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                   "exnty" "__x31__" (Rpc.to_string __x__)
                                   "String(string)"
                               else ();
                               raise
                                 (Rpc.Runtime_error ("String(string)", __x__))))),
                         ((match __x32__ with
                           | Rpc.String x -> x
                           | __x__ ->
                               (if Rpc.get_debug ()
                                then
                                  Printf.eprintf
                                    "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                    "exnty" "__x32__" (Rpc.to_string __x__)
                                    "String(string)"
                                else ();
                                raise
                                  (Rpc.Runtime_error
                                     ("String(string)", __x__))))),
                         ((match __x33__ with
                           | Rpc.String x -> x
                           | __x__ ->
                               (if Rpc.get_debug ()
                                then
                                  Printf.eprintf
                                    "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                    "exnty" "__x33__" (Rpc.to_string __x__)
                                    "String(string)"
                                else ();
                                raise
                                  (Rpc.Runtime_error
                                     ("String(string)", __x__))))),
                         ((match __x34__ with
                           | Rpc.String x -> x
                           | __x__ ->
                               (if Rpc.get_debug ()
                                then
                                  Printf.eprintf
                                    "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                    "exnty" "__x34__" (Rpc.to_string __x__)
                                    "String(string)"
                                else ();
                                raise
                                  (Rpc.Runtime_error
                                     ("String(string)", __x__))))))
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "exnty" "__x30__" (Rpc.to_string __x__) "List"
                        else ();
                        raise (Rpc.Runtime_error ("List", __x__)))))
           | Rpc.Enum ((Rpc.String "not_enough_memory")::__x35__::[]) ->
               Not_enough_memory
                 ((match __x35__ with
                   | Rpc.Int x -> x
                   | Rpc.String s -> Int64.of_string s
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "exnty" "__x35__" (Rpc.to_string __x__)
                            "Int(int64)"
                        else ();
                        raise (Rpc.Runtime_error ("Int(int64)", __x__)))))
           | Rpc.Enum ((Rpc.String "cancelled")::__x36__::[]) ->
               Cancelled
                 ((match __x36__ with
                   | Rpc.String x -> x
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "exnty" "__x36__" (Rpc.to_string __x__)
                            "String(string)"
                        else ();
                        raise (Rpc.Runtime_error ("String(string)", __x__)))))
           | Rpc.Enum ((Rpc.String "storage_backend_error")::__x37__::[]) ->
               Storage_backend_error
                 ((match __x37__ with
                   | Rpc.Enum (__x38__::__x39__::[]) ->
                       (((match __x38__ with
                          | Rpc.String x -> x
                          | __x__ ->
                              (if Rpc.get_debug ()
                               then
                                 Printf.eprintf
                                   "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                   "exnty" "__x38__" (Rpc.to_string __x__)
                                   "String(string)"
                               else ();
                               raise
                                 (Rpc.Runtime_error ("String(string)", __x__))))),
                         ((match __x39__ with
                           | Rpc.Enum __x40__ ->
                               List.map
                                 (function
                                  | __x41__ ->
                                      (match __x41__ with
                                       | Rpc.String x -> x
                                       | __x__ ->
                                           (if Rpc.get_debug ()
                                            then
                                              Printf.eprintf
                                                "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                                "exnty" "__x41__"
                                                (Rpc.to_string __x__)
                                                "String(string)"
                                            else ();
                                            raise
                                              (Rpc.Runtime_error
                                                 ("String(string)", __x__)))))
                                 __x40__
                           | __x__ ->
                               (if Rpc.get_debug ()
                                then
                                  Printf.eprintf
                                    "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                    "exnty" "__x39__" (Rpc.to_string __x__)
                                    "List"
                                else ();
                                raise (Rpc.Runtime_error ("List", __x__))))))
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "exnty" "__x37__" (Rpc.to_string __x__) "List"
                        else ();
                        raise (Rpc.Runtime_error ("List", __x__)))))
           | Rpc.String "pciback_not_loaded" -> PCIBack_not_loaded
           | Rpc.Enum ((Rpc.String "failed_to_run_script")::__x42__::[]) ->
               Failed_to_run_script
                 ((match __x42__ with
                   | Rpc.String x -> x
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "exnty" "__x42__" (Rpc.to_string __x__)
                            "String(string)"
                        else ();
                        raise (Rpc.Runtime_error ("String(string)", __x__)))))
           | Rpc.Enum ((Rpc.String "failed_to_start_emulator")::__x43__::[])
               ->
               Failed_to_start_emulator
                 ((match __x43__ with
                   | Rpc.Enum (__x44__::__x45__::__x46__::[]) ->
                       (((match __x44__ with
                          | Rpc.String x -> x
                          | __x__ ->
                              (if Rpc.get_debug ()
                               then
                                 Printf.eprintf
                                   "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                   "exnty" "__x44__" (Rpc.to_string __x__)
                                   "String(string)"
                               else ();
                               raise
                                 (Rpc.Runtime_error ("String(string)", __x__))))),
                         ((match __x45__ with
                           | Rpc.String x -> x
                           | __x__ ->
                               (if Rpc.get_debug ()
                                then
                                  Printf.eprintf
                                    "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                    "exnty" "__x45__" (Rpc.to_string __x__)
                                    "String(string)"
                                else ();
                                raise
                                  (Rpc.Runtime_error
                                     ("String(string)", __x__))))),
                         ((match __x46__ with
                           | Rpc.String x -> x
                           | __x__ ->
                               (if Rpc.get_debug ()
                                then
                                  Printf.eprintf
                                    "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                    "exnty" "__x46__" (Rpc.to_string __x__)
                                    "String(string)"
                                else ();
                                raise
                                  (Rpc.Runtime_error
                                     ("String(string)", __x__))))))
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "exnty" "__x43__" (Rpc.to_string __x__) "List"
                        else ();
                        raise (Rpc.Runtime_error ("List", __x__)))))
           | Rpc.String "ballooning_timeout_before_migration" ->
               Ballooning_timeout_before_migration
           | Rpc.Enum ((Rpc.String "unknown_rpc")::__x47__::[]) ->
               Unknown_RPC
                 ((match __x47__ with
                   | Rpc.String x -> x
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "exnty" "__x47__" (Rpc.to_string __x__)
                            "String(string)"
                        else ();
                        raise (Rpc.Runtime_error ("String(string)", __x__)))))
           | Rpc.Enum ((Rpc.String
               "message_param_count_mismatch")::__x48__::[]) ->
               Message_param_count_mismatch
                 ((match __x48__ with
                   | Rpc.Enum (__x49__::__x50__::__x51__::[]) ->
                       (((match __x49__ with
                          | Rpc.String x -> x
                          | __x__ ->
                              (if Rpc.get_debug ()
                               then
                                 Printf.eprintf
                                   "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                   "exnty" "__x49__" (Rpc.to_string __x__)
                                   "String(string)"
                               else ();
                               raise
                                 (Rpc.Runtime_error ("String(string)", __x__))))),
                         ((match __x50__ with
                           | Rpc.Int x -> Int64.to_int x
                           | Rpc.String s -> int_of_string s
                           | __x__ ->
                               (if Rpc.get_debug ()
                                then
                                  Printf.eprintf
                                    "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                    "exnty" "__x50__" (Rpc.to_string __x__)
                                    "Int(int)"
                                else ();
                                raise (Rpc.Runtime_error ("Int(int)", __x__))))),
                         ((match __x51__ with
                           | Rpc.Int x -> Int64.to_int x
                           | Rpc.String s -> int_of_string s
                           | __x__ ->
                               (if Rpc.get_debug ()
                                then
                                  Printf.eprintf
                                    "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                    "exnty" "__x51__" (Rpc.to_string __x__)
                                    "Int(int)"
                                else ();
                                raise (Rpc.Runtime_error ("Int(int)", __x__))))))
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "exnty" "__x48__" (Rpc.to_string __x__) "List"
                        else ();
                        raise (Rpc.Runtime_error ("List", __x__)))))
           | Rpc.Enum ((Rpc.String "internal_error")::__x52__::[]) ->
               Internal_error
                 ((match __x52__ with
                   | Rpc.String x -> x
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "exnty" "__x52__" (Rpc.to_string __x__)
                            "String(string)"
                        else ();
                        raise (Rpc.Runtime_error ("String(string)", __x__)))))
           | __x__ ->
               (if Rpc.get_debug ()
                then
                  Printf.eprintf
                    "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                    "exnty" "__x1__" (Rpc.to_string __x__)
                    "Enum[String s;...]"
                else ();
                raise (Rpc.Runtime_error ("Enum[String s;...]", __x__))))
  end
let exnty_of_exn =
  function
  | x ->
      (match x with
       | Internal_error x -> Exception.Internal_error x
       | Message_param_count_mismatch x ->
           Exception.Message_param_count_mismatch x
       | Unknown_RPC x -> Exception.Unknown_RPC x
       | Ballooning_timeout_before_migration ->
           Exception.Ballooning_timeout_before_migration
       | Failed_to_start_emulator x -> Exception.Failed_to_start_emulator x
       | Failed_to_run_script x -> Exception.Failed_to_run_script x
       | PCIBack_not_loaded -> Exception.PCIBack_not_loaded
       | Storage_backend_error x -> Exception.Storage_backend_error x
       | Cancelled x -> Exception.Cancelled x
       | Not_enough_memory x -> Exception.Not_enough_memory x
       | Hook_failed x -> Exception.Hook_failed x
       | Failed_to_contact_remote_service x ->
           Exception.Failed_to_contact_remote_service x
       | IO_error -> Exception.IO_error
       | Vms_failed_to_cooperate x -> Exception.Vms_failed_to_cooperate x
       | Cannot_free_this_much_memory x ->
           Exception.Cannot_free_this_much_memory x
       | Bootloader_error x -> Exception.Bootloader_error x
       | No_bootable_device -> Exception.No_bootable_device
       | Media_not_present -> Exception.Media_not_present
       | Media_present -> Exception.Media_present
       | Media_not_ejectable -> Exception.Media_not_ejectable
       | Device_detach_rejected x -> Exception.Device_detach_rejected x
       | Device_not_connected -> Exception.Device_not_connected
       | Device_is_connected -> Exception.Device_is_connected
       | Failed_to_shutdown x -> Exception.Failed_to_shutdown x
       | Failed_to_acknowledge_shutdown_request ->
           Exception.Failed_to_acknowledge_shutdown_request
       | Bad_power_state x -> Exception.Bad_power_state x
       | Invalid_vcpus x -> Exception.Invalid_vcpus x
       | Domain_not_built -> Exception.Domain_not_built
       | Unimplemented x -> Exception.Unimplemented x
       | Does_not_exist x -> Exception.Does_not_exist x
       | Already_exists x -> Exception.Already_exists x
       | e -> Exception.Internal_error (Printexc.to_string e))
let exn_of_exnty =
  function
  | x ->
      (match x with
       | Exception.Internal_error x -> Internal_error x
       | Exception.Message_param_count_mismatch x ->
           Message_param_count_mismatch x
       | Exception.Unknown_RPC x -> Unknown_RPC x
       | Exception.Ballooning_timeout_before_migration ->
           Ballooning_timeout_before_migration
       | Exception.Failed_to_start_emulator x -> Failed_to_start_emulator x
       | Exception.Failed_to_run_script x -> Failed_to_run_script x
       | Exception.PCIBack_not_loaded -> PCIBack_not_loaded
       | Exception.Storage_backend_error x -> Storage_backend_error x
       | Exception.Cancelled x -> Cancelled x
       | Exception.Not_enough_memory x -> Not_enough_memory x
       | Exception.Hook_failed x -> Hook_failed x
       | Exception.Failed_to_contact_remote_service x ->
           Failed_to_contact_remote_service x
       | Exception.IO_error -> IO_error
       | Exception.Vms_failed_to_cooperate x -> Vms_failed_to_cooperate x
       | Exception.Cannot_free_this_much_memory x ->
           Cannot_free_this_much_memory x
       | Exception.Bootloader_error x -> Bootloader_error x
       | Exception.No_bootable_device -> No_bootable_device
       | Exception.Media_not_present -> Media_not_present
       | Exception.Media_present -> Media_present
       | Exception.Media_not_ejectable -> Media_not_ejectable
       | Exception.Device_detach_rejected x -> Device_detach_rejected x
       | Exception.Device_not_connected -> Device_not_connected
       | Exception.Device_is_connected -> Device_is_connected
       | Exception.Failed_to_shutdown x -> Failed_to_shutdown x
       | Exception.Failed_to_acknowledge_shutdown_request ->
           Failed_to_acknowledge_shutdown_request
       | Exception.Bad_power_state x -> Bad_power_state x
       | Exception.Invalid_vcpus x -> Invalid_vcpus x
       | Exception.Domain_not_built -> Domain_not_built
       | Exception.Unimplemented x -> Unimplemented x
       | Exception.Does_not_exist x -> Does_not_exist x
       | Exception.Already_exists x -> Already_exists x)
module Args =
  struct
    module Query =
      struct
        let rpc_of___x1__ = function | __x106__ -> rpc_of_debug_info __x106__
        and __x1___of_rpc = function | __x105__ -> debug_info_of_rpc __x105__
        and rpc_of___x2__ = function | __x104__ -> Rpc.Null
        and __x2___of_rpc =
          function
          | __x103__ ->
              (match __x103__ with
               | Rpc.Null -> ()
               | __x__ ->
                   (if Rpc.get_debug ()
                    then
                      Printf.eprintf
                        "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                        "__x2__" "__x103__" (Rpc.to_string __x__) "Null"
                    else ();
                    raise (Rpc.Runtime_error ("Null", __x__))))
        type response = Query.t
        let rpc_of_response = function | __x108__ -> Query.rpc_of_t __x108__
        let response_of_rpc = function | __x107__ -> Query.t_of_rpc __x107__
        let call_of_query =
          function
          | __x1__ ->
              (function
               | __x2__ ->
                   Rpc.call "query"
                     [rpc_of___x1__ __x1__; rpc_of___x2__ __x2__])
      end
    module Get_diagnostics =
      struct
        let rpc_of___x1__ = function | __x112__ -> rpc_of_debug_info __x112__
        and __x1___of_rpc = function | __x111__ -> debug_info_of_rpc __x111__
        and rpc_of___x2__ = function | __x110__ -> Rpc.Null
        and __x2___of_rpc =
          function
          | __x109__ ->
              (match __x109__ with
               | Rpc.Null -> ()
               | __x__ ->
                   (if Rpc.get_debug ()
                    then
                      Printf.eprintf
                        "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                        "__x2__" "__x109__" (Rpc.to_string __x__) "Null"
                    else ();
                    raise (Rpc.Runtime_error ("Null", __x__))))
        type response = string
        let rpc_of_response = function | __x114__ -> Rpc.String __x114__
        let response_of_rpc =
          function
          | __x113__ ->
              (match __x113__ with
               | Rpc.String x -> x
               | __x__ ->
                   (if Rpc.get_debug ()
                    then
                      Printf.eprintf
                        "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                        "response" "__x113__" (Rpc.to_string __x__)
                        "String(string)"
                    else ();
                    raise (Rpc.Runtime_error ("String(string)", __x__))))
        let call_of_get_diagnostics =
          function
          | __x1__ ->
              (function
               | __x2__ ->
                   Rpc.call "get_diagnostics"
                     [rpc_of___x1__ __x1__; rpc_of___x2__ __x2__])
      end
    module TASK =
      struct
        module Stat =
          struct
            let rpc_of___x1__ =
              function | __x118__ -> rpc_of_debug_info __x118__
            and __x1___of_rpc =
              function | __x117__ -> debug_info_of_rpc __x117__
            and rpc_of___x2__ =
              function | __x116__ -> Task.rpc_of_id __x116__
            and __x2___of_rpc =
              function | __x115__ -> Task.id_of_rpc __x115__
            type response = Task.t
            let rpc_of_response =
              function | __x120__ -> Task.rpc_of_t __x120__
            let response_of_rpc =
              function | __x119__ -> Task.t_of_rpc __x119__
            let call_of_stat =
              function
              | __x1__ ->
                  (function
                   | __x2__ ->
                       Rpc.call "TASK.stat"
                         [rpc_of___x1__ __x1__; rpc_of___x2__ __x2__])
          end
        module Cancel =
          struct
            let rpc_of___x1__ =
              function | __x124__ -> rpc_of_debug_info __x124__
            and __x1___of_rpc =
              function | __x123__ -> debug_info_of_rpc __x123__
            and rpc_of___x2__ =
              function | __x122__ -> Task.rpc_of_id __x122__
            and __x2___of_rpc =
              function | __x121__ -> Task.id_of_rpc __x121__
            type response = unit
            let rpc_of_response = function | __x126__ -> Rpc.Null
            let response_of_rpc =
              function
              | __x125__ ->
                  (match __x125__ with
                   | Rpc.Null -> ()
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "response" "__x125__" (Rpc.to_string __x__)
                            "Null"
                        else ();
                        raise (Rpc.Runtime_error ("Null", __x__))))
            let call_of_cancel =
              function
              | __x1__ ->
                  (function
                   | __x2__ ->
                       Rpc.call "TASK.cancel"
                         [rpc_of___x1__ __x1__; rpc_of___x2__ __x2__])
          end
        module Destroy =
          struct
            let rpc_of___x1__ =
              function | __x130__ -> rpc_of_debug_info __x130__
            and __x1___of_rpc =
              function | __x129__ -> debug_info_of_rpc __x129__
            and rpc_of___x2__ =
              function | __x128__ -> Task.rpc_of_id __x128__
            and __x2___of_rpc =
              function | __x127__ -> Task.id_of_rpc __x127__
            type response = unit
            let rpc_of_response = function | __x132__ -> Rpc.Null
            let response_of_rpc =
              function
              | __x131__ ->
                  (match __x131__ with
                   | Rpc.Null -> ()
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "response" "__x131__" (Rpc.to_string __x__)
                            "Null"
                        else ();
                        raise (Rpc.Runtime_error ("Null", __x__))))
            let call_of_destroy =
              function
              | __x1__ ->
                  (function
                   | __x2__ ->
                       Rpc.call "TASK.destroy"
                         [rpc_of___x1__ __x1__; rpc_of___x2__ __x2__])
          end
        module List =
          struct
            let rpc_of___x1__ =
              function | __x134__ -> rpc_of_debug_info __x134__
            and __x1___of_rpc =
              function | __x133__ -> debug_info_of_rpc __x133__
            type response = Task.t list
            let rpc_of_response =
              function
              | __x138__ ->
                  Rpc.Enum
                    (List.map (function | __x139__ -> Task.rpc_of_t __x139__)
                       __x138__)
            let response_of_rpc =
              function
              | __x135__ ->
                  (match __x135__ with
                   | Rpc.Enum __x136__ ->
                       List.map
                         (function | __x137__ -> Task.t_of_rpc __x137__)
                         __x136__
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "response" "__x135__" (Rpc.to_string __x__)
                            "List"
                        else ();
                        raise (Rpc.Runtime_error ("List", __x__))))
            let call_of_list =
              function
              | __x1__ -> Rpc.call "TASK.list" [rpc_of___x1__ __x1__]
          end
      end
    module HOST =
      struct
        module Stat =
          struct
            let rpc_of___x1__ =
              function | __x141__ -> rpc_of_debug_info __x141__
            and __x1___of_rpc =
              function | __x140__ -> debug_info_of_rpc __x140__
            type response = Host.t
            let rpc_of_response =
              function | __x143__ -> Host.rpc_of_t __x143__
            let response_of_rpc =
              function | __x142__ -> Host.t_of_rpc __x142__
            let call_of_stat =
              function
              | __x1__ -> Rpc.call "HOST.stat" [rpc_of___x1__ __x1__]
          end
        module Get_console_data =
          struct
            let rpc_of___x1__ =
              function | __x145__ -> rpc_of_debug_info __x145__
            and __x1___of_rpc =
              function | __x144__ -> debug_info_of_rpc __x144__
            type response = string
            let rpc_of_response = function | __x147__ -> Rpc.String __x147__
            let response_of_rpc =
              function
              | __x146__ ->
                  (match __x146__ with
                   | Rpc.String x -> x
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "response" "__x146__" (Rpc.to_string __x__)
                            "String(string)"
                        else ();
                        raise (Rpc.Runtime_error ("String(string)", __x__))))
            let call_of_get_console_data =
              function
              | __x1__ ->
                  Rpc.call "HOST.get_console_data" [rpc_of___x1__ __x1__]
          end
        module Get_total_memory_mib =
          struct
            let rpc_of___x1__ =
              function | __x149__ -> rpc_of_debug_info __x149__
            and __x1___of_rpc =
              function | __x148__ -> debug_info_of_rpc __x148__
            type response = int64
            let rpc_of_response = function | __x151__ -> Rpc.Int __x151__
            let response_of_rpc =
              function
              | __x150__ ->
                  (match __x150__ with
                   | Rpc.Int x -> x
                   | Rpc.String s -> Int64.of_string s
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "response" "__x150__" (Rpc.to_string __x__)
                            "Int(int64)"
                        else ();
                        raise (Rpc.Runtime_error ("Int(int64)", __x__))))
            let call_of_get_total_memory_mib =
              function
              | __x1__ ->
                  Rpc.call "HOST.get_total_memory_mib" [rpc_of___x1__ __x1__]
          end
        module Send_debug_keys =
          struct
            let rpc_of___x1__ =
              function | __x155__ -> rpc_of_debug_info __x155__
            and __x1___of_rpc =
              function | __x154__ -> debug_info_of_rpc __x154__
            and rpc_of___x2__ = function | __x153__ -> Rpc.String __x153__
            and __x2___of_rpc =
              function
              | __x152__ ->
                  (match __x152__ with
                   | Rpc.String x -> x
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "__x2__" "__x152__" (Rpc.to_string __x__)
                            "String(string)"
                        else ();
                        raise (Rpc.Runtime_error ("String(string)", __x__))))
            type response = unit
            let rpc_of_response = function | __x157__ -> Rpc.Null
            let response_of_rpc =
              function
              | __x156__ ->
                  (match __x156__ with
                   | Rpc.Null -> ()
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "response" "__x156__" (Rpc.to_string __x__)
                            "Null"
                        else ();
                        raise (Rpc.Runtime_error ("Null", __x__))))
            let call_of_send_debug_keys =
              function
              | __x1__ ->
                  (function
                   | __x2__ ->
                       Rpc.call "HOST.send_debug_keys"
                         [rpc_of___x1__ __x1__; rpc_of___x2__ __x2__])
          end
        module Set_worker_pool_size =
          struct
            let rpc_of___x1__ =
              function | __x161__ -> rpc_of_debug_info __x161__
            and __x1___of_rpc =
              function | __x160__ -> debug_info_of_rpc __x160__
            and rpc_of___x2__ =
              function | __x159__ -> Rpc.Int (Int64.of_int __x159__)
            and __x2___of_rpc =
              function
              | __x158__ ->
                  (match __x158__ with
                   | Rpc.Int x -> Int64.to_int x
                   | Rpc.String s -> int_of_string s
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "__x2__" "__x158__" (Rpc.to_string __x__)
                            "Int(int)"
                        else ();
                        raise (Rpc.Runtime_error ("Int(int)", __x__))))
            type response = unit
            let rpc_of_response = function | __x163__ -> Rpc.Null
            let response_of_rpc =
              function
              | __x162__ ->
                  (match __x162__ with
                   | Rpc.Null -> ()
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "response" "__x162__" (Rpc.to_string __x__)
                            "Null"
                        else ();
                        raise (Rpc.Runtime_error ("Null", __x__))))
            let call_of_set_worker_pool_size =
              function
              | __x1__ ->
                  (function
                   | __x2__ ->
                       Rpc.call "HOST.set_worker_pool_size"
                         [rpc_of___x1__ __x1__; rpc_of___x2__ __x2__])
          end
        module Update_guest_agent_features =
          struct
            let rpc_of___x1__ =
              function | __x170__ -> rpc_of_debug_info __x170__
            and __x1___of_rpc =
              function | __x169__ -> debug_info_of_rpc __x169__
            and rpc_of___x2__ =
              function
              | __x167__ ->
                  Rpc.Enum
                    (List.map
                       (function
                        | __x168__ ->
                            Host.rpc_of_guest_agent_feature __x168__)
                       __x167__)
            and __x2___of_rpc =
              function
              | __x164__ ->
                  (match __x164__ with
                   | Rpc.Enum __x165__ ->
                       List.map
                         (function
                          | __x166__ ->
                              Host.guest_agent_feature_of_rpc __x166__)
                         __x165__
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "__x2__" "__x164__" (Rpc.to_string __x__) "List"
                        else ();
                        raise (Rpc.Runtime_error ("List", __x__))))
            type response = unit
            let rpc_of_response = function | __x172__ -> Rpc.Null
            let response_of_rpc =
              function
              | __x171__ ->
                  (match __x171__ with
                   | Rpc.Null -> ()
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "response" "__x171__" (Rpc.to_string __x__)
                            "Null"
                        else ();
                        raise (Rpc.Runtime_error ("Null", __x__))))
            let call_of_update_guest_agent_features =
              function
              | __x1__ ->
                  (function
                   | __x2__ ->
                       Rpc.call "HOST.update_guest_agent_features"
                         [rpc_of___x1__ __x1__; rpc_of___x2__ __x2__])
          end
        module Upgrade_cpu_features =
          struct
            let rpc_of___x1__ =
              function | __x181__ -> rpc_of_debug_info __x181__
            and __x1___of_rpc =
              function | __x180__ -> debug_info_of_rpc __x180__
            and rpc_of___x2__ =
              function
              | __x178__ ->
                  Rpc.Enum
                    (Array.to_list
                       (Array.map (function | __x179__ -> Rpc.Int __x179__)
                          __x178__))
            and __x2___of_rpc =
              function
              | __x175__ ->
                  (match __x175__ with
                   | Rpc.Enum __x176__ ->
                       Array.of_list
                         (List.map
                            (function
                             | __x177__ ->
                                 (match __x177__ with
                                  | Rpc.Int x -> x
                                  | Rpc.String s -> Int64.of_string s
                                  | __x__ ->
                                      (if Rpc.get_debug ()
                                       then
                                         Printf.eprintf
                                           "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                           "__x2__" "__x177__"
                                           (Rpc.to_string __x__) "Int(int64)"
                                       else ();
                                       raise
                                         (Rpc.Runtime_error
                                            ("Int(int64)", __x__)))))
                            __x176__)
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "__x2__" "__x175__" (Rpc.to_string __x__) "List"
                        else ();
                        raise (Rpc.Runtime_error ("List", __x__))))
            and rpc_of___x3__ = function | __x174__ -> Rpc.Bool __x174__
            and __x3___of_rpc =
              function
              | __x173__ ->
                  (match __x173__ with
                   | Rpc.Bool x -> x
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "__x3__" "__x173__" (Rpc.to_string __x__) "Bool"
                        else ();
                        raise (Rpc.Runtime_error ("Bool", __x__))))
            type response = int64 array
            let rpc_of_response =
              function
              | __x185__ ->
                  Rpc.Enum
                    (Array.to_list
                       (Array.map (function | __x186__ -> Rpc.Int __x186__)
                          __x185__))
            let response_of_rpc =
              function
              | __x182__ ->
                  (match __x182__ with
                   | Rpc.Enum __x183__ ->
                       Array.of_list
                         (List.map
                            (function
                             | __x184__ ->
                                 (match __x184__ with
                                  | Rpc.Int x -> x
                                  | Rpc.String s -> Int64.of_string s
                                  | __x__ ->
                                      (if Rpc.get_debug ()
                                       then
                                         Printf.eprintf
                                           "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                           "response" "__x184__"
                                           (Rpc.to_string __x__) "Int(int64)"
                                       else ();
                                       raise
                                         (Rpc.Runtime_error
                                            ("Int(int64)", __x__)))))
                            __x183__)
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "response" "__x182__" (Rpc.to_string __x__)
                            "List"
                        else ();
                        raise (Rpc.Runtime_error ("List", __x__))))
            let call_of_upgrade_cpu_features =
              function
              | __x1__ ->
                  (function
                   | __x2__ ->
                       (function
                        | __x3__ ->
                            Rpc.call "HOST.upgrade_cpu_features"
                              [rpc_of___x1__ __x1__;
                              rpc_of___x2__ __x2__;
                              rpc_of___x3__ __x3__]))
          end
      end
    module VM =
      struct
        module Add =
          struct
            let rpc_of___x1__ =
              function | __x190__ -> rpc_of_debug_info __x190__
            and __x1___of_rpc =
              function | __x189__ -> debug_info_of_rpc __x189__
            and rpc_of___x2__ = function | __x188__ -> Vm.rpc_of_t __x188__
            and __x2___of_rpc = function | __x187__ -> Vm.t_of_rpc __x187__
            type response = Vm.id
            let rpc_of_response =
              function | __x192__ -> Vm.rpc_of_id __x192__
            let response_of_rpc =
              function | __x191__ -> Vm.id_of_rpc __x191__
            let call_of_add =
              function
              | __x1__ ->
                  (function
                   | __x2__ ->
                       Rpc.call "VM.add"
                         [rpc_of___x1__ __x1__; rpc_of___x2__ __x2__])
          end
        module Remove =
          struct
            let rpc_of___x1__ =
              function | __x196__ -> rpc_of_debug_info __x196__
            and __x1___of_rpc =
              function | __x195__ -> debug_info_of_rpc __x195__
            and rpc_of___x2__ = function | __x194__ -> Vm.rpc_of_id __x194__
            and __x2___of_rpc = function | __x193__ -> Vm.id_of_rpc __x193__
            type response = unit
            let rpc_of_response = function | __x198__ -> Rpc.Null
            let response_of_rpc =
              function
              | __x197__ ->
                  (match __x197__ with
                   | Rpc.Null -> ()
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "response" "__x197__" (Rpc.to_string __x__)
                            "Null"
                        else ();
                        raise (Rpc.Runtime_error ("Null", __x__))))
            let call_of_remove =
              function
              | __x1__ ->
                  (function
                   | __x2__ ->
                       Rpc.call "VM.remove"
                         [rpc_of___x1__ __x1__; rpc_of___x2__ __x2__])
          end
        module Generate_state_string =
          struct
            let rpc_of___x1__ =
              function | __x202__ -> rpc_of_debug_info __x202__
            and __x1___of_rpc =
              function | __x201__ -> debug_info_of_rpc __x201__
            and rpc_of___x2__ = function | __x200__ -> Vm.rpc_of_t __x200__
            and __x2___of_rpc = function | __x199__ -> Vm.t_of_rpc __x199__
            type response = string
            let rpc_of_response = function | __x204__ -> Rpc.String __x204__
            let response_of_rpc =
              function
              | __x203__ ->
                  (match __x203__ with
                   | Rpc.String x -> x
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "response" "__x203__" (Rpc.to_string __x__)
                            "String(string)"
                        else ();
                        raise (Rpc.Runtime_error ("String(string)", __x__))))
            let call_of_generate_state_string =
              function
              | __x1__ ->
                  (function
                   | __x2__ ->
                       Rpc.call "VM.generate_state_string"
                         [rpc_of___x1__ __x1__; rpc_of___x2__ __x2__])
          end
        module Migrate =
          struct
            let rpc_of___x1__ =
              function | __x222__ -> rpc_of_debug_info __x222__
            and __x1___of_rpc =
              function | __x221__ -> debug_info_of_rpc __x221__
            and rpc_of___x2__ = function | __x220__ -> Vm.rpc_of_id __x220__
            and __x2___of_rpc = function | __x219__ -> Vm.id_of_rpc __x219__
            and rpc_of___x3__ =
              function
              | __x217__ ->
                  let dict =
                    List.map
                      (function
                       | (key, __x218__) -> (key, (Rpc.String __x218__)))
                      __x217__ in
                  Rpc.Dict dict
            and __x3___of_rpc =
              function
              | __x215__ ->
                  (match __x215__ with
                   | Rpc.Dict d ->
                       List.map
                         (function
                          | (key, __x216__) ->
                              (key,
                                ((match __x216__ with
                                  | Rpc.String x -> x
                                  | __x__ ->
                                      (if Rpc.get_debug ()
                                       then
                                         Printf.eprintf
                                           "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                           "__x3__" "__x216__"
                                           (Rpc.to_string __x__)
                                           "String(string)"
                                       else ();
                                       raise
                                         (Rpc.Runtime_error
                                            ("String(string)", __x__))))))) d
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "__x3__" "__x215__" (Rpc.to_string __x__) "Dict"
                        else ();
                        raise (Rpc.Runtime_error ("Dict", __x__))))
            and rpc_of___x4__ =
              function
              | __x213__ ->
                  let dict =
                    List.map
                      (function
                       | (key, __x214__) ->
                           (key, (Network.rpc_of_t __x214__))) __x213__ in
                  Rpc.Dict dict
            and __x4___of_rpc =
              function
              | __x211__ ->
                  (match __x211__ with
                   | Rpc.Dict d ->
                       List.map
                         (function
                          | (key, __x212__) ->
                              (key, (Network.t_of_rpc __x212__))) d
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "__x4__" "__x211__" (Rpc.to_string __x__) "Dict"
                        else ();
                        raise (Rpc.Runtime_error ("Dict", __x__))))
            and rpc_of___x5__ =
              function
              | __x209__ ->
                  let dict =
                    List.map
                      (function
                       | (key, __x210__) ->
                           (key, (Pci.rpc_of_address __x210__))) __x209__ in
                  Rpc.Dict dict
            and __x5___of_rpc =
              function
              | __x207__ ->
                  (match __x207__ with
                   | Rpc.Dict d ->
                       List.map
                         (function
                          | (key, __x208__) ->
                              (key, (Pci.address_of_rpc __x208__))) d
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "__x5__" "__x207__" (Rpc.to_string __x__) "Dict"
                        else ();
                        raise (Rpc.Runtime_error ("Dict", __x__))))
            and rpc_of___x6__ = function | __x206__ -> Rpc.String __x206__
            and __x6___of_rpc =
              function
              | __x205__ ->
                  (match __x205__ with
                   | Rpc.String x -> x
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "__x6__" "__x205__" (Rpc.to_string __x__)
                            "String(string)"
                        else ();
                        raise (Rpc.Runtime_error ("String(string)", __x__))))
            type response = Task.id
            let rpc_of_response =
              function | __x224__ -> Task.rpc_of_id __x224__
            let response_of_rpc =
              function | __x223__ -> Task.id_of_rpc __x223__
            let call_of_migrate =
              function
              | __x1__ ->
                  (function
                   | __x2__ ->
                       (function
                        | __x3__ ->
                            (function
                             | __x4__ ->
                                 (function
                                  | __x5__ ->
                                      (function
                                       | __x6__ ->
                                           Rpc.call "VM.migrate"
                                             [rpc_of___x1__ __x1__;
                                             rpc_of___x2__ __x2__;
                                             rpc_of___x3__ __x3__;
                                             rpc_of___x4__ __x4__;
                                             rpc_of___x5__ __x5__;
                                             rpc_of___x6__ __x6__])))))
          end
        module Migrate_receive_memory =
          struct
            let rpc_of___x1__ =
              function | __x234__ -> rpc_of_debug_info __x234__
            and __x1___of_rpc =
              function | __x233__ -> debug_info_of_rpc __x233__
            and rpc_of___x2__ = function | __x232__ -> Vm.rpc_of_id __x232__
            and __x2___of_rpc = function | __x231__ -> Vm.id_of_rpc __x231__
            and rpc_of___x3__ = function | __x230__ -> Rpc.Int __x230__
            and __x3___of_rpc =
              function
              | __x229__ ->
                  (match __x229__ with
                   | Rpc.Int x -> x
                   | Rpc.String s -> Int64.of_string s
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "__x3__" "__x229__" (Rpc.to_string __x__)
                            "Int(int64)"
                        else ();
                        raise (Rpc.Runtime_error ("Int(int64)", __x__))))
            and rpc_of___x4__ = function | __x228__ -> Rpc.String __x228__
            and __x4___of_rpc =
              function
              | __x227__ ->
                  (match __x227__ with
                   | Rpc.String x -> x
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "__x4__" "__x227__" (Rpc.to_string __x__)
                            "String(string)"
                        else ();
                        raise (Rpc.Runtime_error ("String(string)", __x__))))
            and rpc_of___x5__ =
              function | __x226__ -> Xcp_channel.rpc_of_t __x226__
            and __x5___of_rpc =
              function | __x225__ -> Xcp_channel.t_of_rpc __x225__
            type response = Task.id option
            let rpc_of_response =
              function
              | __x237__ ->
                  (match __x237__ with
                   | Some __x238__ -> Rpc.Enum [Task.rpc_of_id __x238__]
                   | None -> Rpc.Enum [])
            let response_of_rpc =
              function
              | __x235__ ->
                  (match __x235__ with
                   | Rpc.Enum [] -> None
                   | Rpc.Enum (__x236__::[]) ->
                       Some (Task.id_of_rpc __x236__)
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "response" "__x235__" (Rpc.to_string __x__)
                            "Enum[]/Enum[_]"
                        else ();
                        raise (Rpc.Runtime_error ("Enum[]/Enum[_]", __x__))))
            let call_of_migrate_receive_memory =
              function
              | __x1__ ->
                  (function
                   | __x2__ ->
                       (function
                        | __x3__ ->
                            (function
                             | __x4__ ->
                                 (function
                                  | __x5__ ->
                                      Rpc.call "VM.migrate_receive_memory"
                                        [rpc_of___x1__ __x1__;
                                        rpc_of___x2__ __x2__;
                                        rpc_of___x3__ __x3__;
                                        rpc_of___x4__ __x4__;
                                        rpc_of___x5__ __x5__]))))
          end
        module Create =
          struct
            let rpc_of___x1__ =
              function | __x242__ -> rpc_of_debug_info __x242__
            and __x1___of_rpc =
              function | __x241__ -> debug_info_of_rpc __x241__
            and rpc_of___x2__ = function | __x240__ -> Vm.rpc_of_id __x240__
            and __x2___of_rpc = function | __x239__ -> Vm.id_of_rpc __x239__
            type response = Task.id
            let rpc_of_response =
              function | __x244__ -> Task.rpc_of_id __x244__
            let response_of_rpc =
              function | __x243__ -> Task.id_of_rpc __x243__
            let call_of_create =
              function
              | __x1__ ->
                  (function
                   | __x2__ ->
                       Rpc.call "VM.create"
                         [rpc_of___x1__ __x1__; rpc_of___x2__ __x2__])
          end
        module Build =
          struct
            let rpc_of___x1__ =
              function | __x250__ -> rpc_of_debug_info __x250__
            and __x1___of_rpc =
              function | __x249__ -> debug_info_of_rpc __x249__
            and rpc_of___x2__ = function | __x248__ -> Vm.rpc_of_id __x248__
            and __x2___of_rpc = function | __x247__ -> Vm.id_of_rpc __x247__
            and rpc_of___x3__ = function | __x246__ -> Rpc.Bool __x246__
            and __x3___of_rpc =
              function
              | __x245__ ->
                  (match __x245__ with
                   | Rpc.Bool x -> x
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "__x3__" "__x245__" (Rpc.to_string __x__) "Bool"
                        else ();
                        raise (Rpc.Runtime_error ("Bool", __x__))))
            type response = Task.id
            let rpc_of_response =
              function | __x252__ -> Task.rpc_of_id __x252__
            let response_of_rpc =
              function | __x251__ -> Task.id_of_rpc __x251__
            let call_of_build =
              function
              | __x1__ ->
                  (function
                   | __x2__ ->
                       (function
                        | __x3__ ->
                            Rpc.call "VM.build"
                              [rpc_of___x1__ __x1__;
                              rpc_of___x2__ __x2__;
                              rpc_of___x3__ __x3__]))
          end
        module Create_device_model =
          struct
            let rpc_of___x1__ =
              function | __x258__ -> rpc_of_debug_info __x258__
            and __x1___of_rpc =
              function | __x257__ -> debug_info_of_rpc __x257__
            and rpc_of___x2__ = function | __x256__ -> Vm.rpc_of_id __x256__
            and __x2___of_rpc = function | __x255__ -> Vm.id_of_rpc __x255__
            and rpc_of___x3__ = function | __x254__ -> Rpc.Bool __x254__
            and __x3___of_rpc =
              function
              | __x253__ ->
                  (match __x253__ with
                   | Rpc.Bool x -> x
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "__x3__" "__x253__" (Rpc.to_string __x__) "Bool"
                        else ();
                        raise (Rpc.Runtime_error ("Bool", __x__))))
            type response = Task.id
            let rpc_of_response =
              function | __x260__ -> Task.rpc_of_id __x260__
            let response_of_rpc =
              function | __x259__ -> Task.id_of_rpc __x259__
            let call_of_create_device_model =
              function
              | __x1__ ->
                  (function
                   | __x2__ ->
                       (function
                        | __x3__ ->
                            Rpc.call "VM.create_device_model"
                              [rpc_of___x1__ __x1__;
                              rpc_of___x2__ __x2__;
                              rpc_of___x3__ __x3__]))
          end
        module Destroy =
          struct
            let rpc_of___x1__ =
              function | __x264__ -> rpc_of_debug_info __x264__
            and __x1___of_rpc =
              function | __x263__ -> debug_info_of_rpc __x263__
            and rpc_of___x2__ = function | __x262__ -> Vm.rpc_of_id __x262__
            and __x2___of_rpc = function | __x261__ -> Vm.id_of_rpc __x261__
            type response = Task.id
            let rpc_of_response =
              function | __x266__ -> Task.rpc_of_id __x266__
            let response_of_rpc =
              function | __x265__ -> Task.id_of_rpc __x265__
            let call_of_destroy =
              function
              | __x1__ ->
                  (function
                   | __x2__ ->
                       Rpc.call "VM.destroy"
                         [rpc_of___x1__ __x1__; rpc_of___x2__ __x2__])
          end
        module Pause =
          struct
            let rpc_of___x1__ =
              function | __x270__ -> rpc_of_debug_info __x270__
            and __x1___of_rpc =
              function | __x269__ -> debug_info_of_rpc __x269__
            and rpc_of___x2__ = function | __x268__ -> Vm.rpc_of_id __x268__
            and __x2___of_rpc = function | __x267__ -> Vm.id_of_rpc __x267__
            type response = Task.id
            let rpc_of_response =
              function | __x272__ -> Task.rpc_of_id __x272__
            let response_of_rpc =
              function | __x271__ -> Task.id_of_rpc __x271__
            let call_of_pause =
              function
              | __x1__ ->
                  (function
                   | __x2__ ->
                       Rpc.call "VM.pause"
                         [rpc_of___x1__ __x1__; rpc_of___x2__ __x2__])
          end
        module Unpause =
          struct
            let rpc_of___x1__ =
              function | __x276__ -> rpc_of_debug_info __x276__
            and __x1___of_rpc =
              function | __x275__ -> debug_info_of_rpc __x275__
            and rpc_of___x2__ = function | __x274__ -> Vm.rpc_of_id __x274__
            and __x2___of_rpc = function | __x273__ -> Vm.id_of_rpc __x273__
            type response = Task.id
            let rpc_of_response =
              function | __x278__ -> Task.rpc_of_id __x278__
            let response_of_rpc =
              function | __x277__ -> Task.id_of_rpc __x277__
            let call_of_unpause =
              function
              | __x1__ ->
                  (function
                   | __x2__ ->
                       Rpc.call "VM.unpause"
                         [rpc_of___x1__ __x1__; rpc_of___x2__ __x2__])
          end
        module Request_rdp =
          struct
            let rpc_of___x1__ =
              function | __x284__ -> rpc_of_debug_info __x284__
            and __x1___of_rpc =
              function | __x283__ -> debug_info_of_rpc __x283__
            and rpc_of___x2__ = function | __x282__ -> Vm.rpc_of_id __x282__
            and __x2___of_rpc = function | __x281__ -> Vm.id_of_rpc __x281__
            and rpc_of___x3__ = function | __x280__ -> Rpc.Bool __x280__
            and __x3___of_rpc =
              function
              | __x279__ ->
                  (match __x279__ with
                   | Rpc.Bool x -> x
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "__x3__" "__x279__" (Rpc.to_string __x__) "Bool"
                        else ();
                        raise (Rpc.Runtime_error ("Bool", __x__))))
            type response = Task.id
            let rpc_of_response =
              function | __x286__ -> Task.rpc_of_id __x286__
            let response_of_rpc =
              function | __x285__ -> Task.id_of_rpc __x285__
            let call_of_request_rdp =
              function
              | __x1__ ->
                  (function
                   | __x2__ ->
                       (function
                        | __x3__ ->
                            Rpc.call "VM.request_rdp"
                              [rpc_of___x1__ __x1__;
                              rpc_of___x2__ __x2__;
                              rpc_of___x3__ __x3__]))
          end
        module Run_script =
          struct
            let rpc_of___x1__ =
              function | __x292__ -> rpc_of_debug_info __x292__
            and __x1___of_rpc =
              function | __x291__ -> debug_info_of_rpc __x291__
            and rpc_of___x2__ = function | __x290__ -> Vm.rpc_of_id __x290__
            and __x2___of_rpc = function | __x289__ -> Vm.id_of_rpc __x289__
            and rpc_of___x3__ = function | __x288__ -> Rpc.String __x288__
            and __x3___of_rpc =
              function
              | __x287__ ->
                  (match __x287__ with
                   | Rpc.String x -> x
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "__x3__" "__x287__" (Rpc.to_string __x__)
                            "String(string)"
                        else ();
                        raise (Rpc.Runtime_error ("String(string)", __x__))))
            type response = Task.id
            let rpc_of_response =
              function | __x294__ -> Task.rpc_of_id __x294__
            let response_of_rpc =
              function | __x293__ -> Task.id_of_rpc __x293__
            let call_of_run_script =
              function
              | __x1__ ->
                  (function
                   | __x2__ ->
                       (function
                        | __x3__ ->
                            Rpc.call "VM.run_script"
                              [rpc_of___x1__ __x1__;
                              rpc_of___x2__ __x2__;
                              rpc_of___x3__ __x3__]))
          end
        module Set_xsdata =
          struct
            let rpc_of___x1__ =
              function | __x302__ -> rpc_of_debug_info __x302__
            and __x1___of_rpc =
              function | __x301__ -> debug_info_of_rpc __x301__
            and rpc_of___x2__ = function | __x300__ -> Vm.rpc_of_id __x300__
            and __x2___of_rpc = function | __x299__ -> Vm.id_of_rpc __x299__
            and rpc_of___x3__ =
              function
              | __x297__ ->
                  let dict =
                    List.map
                      (function
                       | (key, __x298__) -> (key, (Rpc.String __x298__)))
                      __x297__ in
                  Rpc.Dict dict
            and __x3___of_rpc =
              function
              | __x295__ ->
                  (match __x295__ with
                   | Rpc.Dict d ->
                       List.map
                         (function
                          | (key, __x296__) ->
                              (key,
                                ((match __x296__ with
                                  | Rpc.String x -> x
                                  | __x__ ->
                                      (if Rpc.get_debug ()
                                       then
                                         Printf.eprintf
                                           "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                           "__x3__" "__x296__"
                                           (Rpc.to_string __x__)
                                           "String(string)"
                                       else ();
                                       raise
                                         (Rpc.Runtime_error
                                            ("String(string)", __x__))))))) d
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "__x3__" "__x295__" (Rpc.to_string __x__) "Dict"
                        else ();
                        raise (Rpc.Runtime_error ("Dict", __x__))))
            type response = Task.id
            let rpc_of_response =
              function | __x304__ -> Task.rpc_of_id __x304__
            let response_of_rpc =
              function | __x303__ -> Task.id_of_rpc __x303__
            let call_of_set_xsdata =
              function
              | __x1__ ->
                  (function
                   | __x2__ ->
                       (function
                        | __x3__ ->
                            Rpc.call "VM.set_xsdata"
                              [rpc_of___x1__ __x1__;
                              rpc_of___x2__ __x2__;
                              rpc_of___x3__ __x3__]))
          end
        module Set_vcpus =
          struct
            let rpc_of___x1__ =
              function | __x310__ -> rpc_of_debug_info __x310__
            and __x1___of_rpc =
              function | __x309__ -> debug_info_of_rpc __x309__
            and rpc_of___x2__ = function | __x308__ -> Vm.rpc_of_id __x308__
            and __x2___of_rpc = function | __x307__ -> Vm.id_of_rpc __x307__
            and rpc_of___x3__ =
              function | __x306__ -> Rpc.Int (Int64.of_int __x306__)
            and __x3___of_rpc =
              function
              | __x305__ ->
                  (match __x305__ with
                   | Rpc.Int x -> Int64.to_int x
                   | Rpc.String s -> int_of_string s
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "__x3__" "__x305__" (Rpc.to_string __x__)
                            "Int(int)"
                        else ();
                        raise (Rpc.Runtime_error ("Int(int)", __x__))))
            type response = Task.id
            let rpc_of_response =
              function | __x312__ -> Task.rpc_of_id __x312__
            let response_of_rpc =
              function | __x311__ -> Task.id_of_rpc __x311__
            let call_of_set_vcpus =
              function
              | __x1__ ->
                  (function
                   | __x2__ ->
                       (function
                        | __x3__ ->
                            Rpc.call "VM.set_vcpus"
                              [rpc_of___x1__ __x1__;
                              rpc_of___x2__ __x2__;
                              rpc_of___x3__ __x3__]))
          end
        module Set_shadow_multiplier =
          struct
            let rpc_of___x1__ =
              function | __x318__ -> rpc_of_debug_info __x318__
            and __x1___of_rpc =
              function | __x317__ -> debug_info_of_rpc __x317__
            and rpc_of___x2__ = function | __x316__ -> Vm.rpc_of_id __x316__
            and __x2___of_rpc = function | __x315__ -> Vm.id_of_rpc __x315__
            and rpc_of___x3__ = function | __x314__ -> Rpc.Float __x314__
            and __x3___of_rpc =
              function
              | __x313__ ->
                  (match __x313__ with
                   | Rpc.Float x -> x
                   | Rpc.String s -> float_of_string s
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "__x3__" "__x313__" (Rpc.to_string __x__) "Float"
                        else ();
                        raise (Rpc.Runtime_error ("Float", __x__))))
            type response = Task.id
            let rpc_of_response =
              function | __x320__ -> Task.rpc_of_id __x320__
            let response_of_rpc =
              function | __x319__ -> Task.id_of_rpc __x319__
            let call_of_set_shadow_multiplier =
              function
              | __x1__ ->
                  (function
                   | __x2__ ->
                       (function
                        | __x3__ ->
                            Rpc.call "VM.set_shadow_multiplier"
                              [rpc_of___x1__ __x1__;
                              rpc_of___x2__ __x2__;
                              rpc_of___x3__ __x3__]))
          end
        module Set_memory_dynamic_range =
          struct
            let rpc_of___x1__ =
              function | __x328__ -> rpc_of_debug_info __x328__
            and __x1___of_rpc =
              function | __x327__ -> debug_info_of_rpc __x327__
            and rpc_of___x2__ = function | __x326__ -> Vm.rpc_of_id __x326__
            and __x2___of_rpc = function | __x325__ -> Vm.id_of_rpc __x325__
            and rpc_of___x3__ = function | __x324__ -> Rpc.Int __x324__
            and __x3___of_rpc =
              function
              | __x323__ ->
                  (match __x323__ with
                   | Rpc.Int x -> x
                   | Rpc.String s -> Int64.of_string s
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "__x3__" "__x323__" (Rpc.to_string __x__)
                            "Int(int64)"
                        else ();
                        raise (Rpc.Runtime_error ("Int(int64)", __x__))))
            and rpc_of___x4__ = function | __x322__ -> Rpc.Int __x322__
            and __x4___of_rpc =
              function
              | __x321__ ->
                  (match __x321__ with
                   | Rpc.Int x -> x
                   | Rpc.String s -> Int64.of_string s
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "__x4__" "__x321__" (Rpc.to_string __x__)
                            "Int(int64)"
                        else ();
                        raise (Rpc.Runtime_error ("Int(int64)", __x__))))
            type response = Task.id
            let rpc_of_response =
              function | __x330__ -> Task.rpc_of_id __x330__
            let response_of_rpc =
              function | __x329__ -> Task.id_of_rpc __x329__
            let call_of_set_memory_dynamic_range =
              function
              | __x1__ ->
                  (function
                   | __x2__ ->
                       (function
                        | __x3__ ->
                            (function
                             | __x4__ ->
                                 Rpc.call "VM.set_memory_dynamic_range"
                                   [rpc_of___x1__ __x1__;
                                   rpc_of___x2__ __x2__;
                                   rpc_of___x3__ __x3__;
                                   rpc_of___x4__ __x4__])))
          end
        module Stat =
          struct
            let rpc_of___x1__ =
              function | __x334__ -> rpc_of_debug_info __x334__
            and __x1___of_rpc =
              function | __x333__ -> debug_info_of_rpc __x333__
            and rpc_of___x2__ = function | __x332__ -> Vm.rpc_of_id __x332__
            and __x2___of_rpc = function | __x331__ -> Vm.id_of_rpc __x331__
            type response = (Vm.t * Vm.state)
            let rpc_of_response =
              function
              | __x338__ ->
                  let (__x339__, __x340__) = __x338__ in
                  Rpc.Enum [Vm.rpc_of_t __x339__; Vm.rpc_of_state __x340__]
            let response_of_rpc =
              function
              | __x335__ ->
                  (match __x335__ with
                   | Rpc.Enum (__x336__::__x337__::[]) ->
                       ((Vm.t_of_rpc __x336__), (Vm.state_of_rpc __x337__))
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "response" "__x335__" (Rpc.to_string __x__)
                            "List"
                        else ();
                        raise (Rpc.Runtime_error ("List", __x__))))
            let call_of_stat =
              function
              | __x1__ ->
                  (function
                   | __x2__ ->
                       Rpc.call "VM.stat"
                         [rpc_of___x1__ __x1__; rpc_of___x2__ __x2__])
          end
        module Exists =
          struct
            let rpc_of___x1__ =
              function | __x344__ -> rpc_of_debug_info __x344__
            and __x1___of_rpc =
              function | __x343__ -> debug_info_of_rpc __x343__
            and rpc_of___x2__ = function | __x342__ -> Vm.rpc_of_id __x342__
            and __x2___of_rpc = function | __x341__ -> Vm.id_of_rpc __x341__
            type response = bool
            let rpc_of_response = function | __x346__ -> Rpc.Bool __x346__
            let response_of_rpc =
              function
              | __x345__ ->
                  (match __x345__ with
                   | Rpc.Bool x -> x
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "response" "__x345__" (Rpc.to_string __x__)
                            "Bool"
                        else ();
                        raise (Rpc.Runtime_error ("Bool", __x__))))
            let call_of_exists =
              function
              | __x1__ ->
                  (function
                   | __x2__ ->
                       Rpc.call "VM.exists"
                         [rpc_of___x1__ __x1__; rpc_of___x2__ __x2__])
          end
        module List =
          struct
            let rpc_of___x1__ =
              function | __x350__ -> rpc_of_debug_info __x350__
            and __x1___of_rpc =
              function | __x349__ -> debug_info_of_rpc __x349__
            and rpc_of___x2__ = function | __x348__ -> Rpc.Null
            and __x2___of_rpc =
              function
              | __x347__ ->
                  (match __x347__ with
                   | Rpc.Null -> ()
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "__x2__" "__x347__" (Rpc.to_string __x__) "Null"
                        else ();
                        raise (Rpc.Runtime_error ("Null", __x__))))
            type response = (Vm.t * Vm.state) list
            let rpc_of_response =
              function
              | __x356__ ->
                  Rpc.Enum
                    (List.map
                       (function
                        | __x357__ ->
                            let (__x358__, __x359__) = __x357__ in
                            Rpc.Enum
                              [Vm.rpc_of_t __x358__;
                              Vm.rpc_of_state __x359__]) __x356__)
            let response_of_rpc =
              function
              | __x351__ ->
                  (match __x351__ with
                   | Rpc.Enum __x352__ ->
                       List.map
                         (function
                          | __x353__ ->
                              (match __x353__ with
                               | Rpc.Enum (__x354__::__x355__::[]) ->
                                   ((Vm.t_of_rpc __x354__),
                                     (Vm.state_of_rpc __x355__))
                               | __x__ ->
                                   (if Rpc.get_debug ()
                                    then
                                      Printf.eprintf
                                        "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                        "response" "__x353__"
                                        (Rpc.to_string __x__) "List"
                                    else ();
                                    raise (Rpc.Runtime_error ("List", __x__)))))
                         __x352__
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "response" "__x351__" (Rpc.to_string __x__)
                            "List"
                        else ();
                        raise (Rpc.Runtime_error ("List", __x__))))
            let call_of_list =
              function
              | __x1__ ->
                  (function
                   | __x2__ ->
                       Rpc.call "VM.list"
                         [rpc_of___x1__ __x1__; rpc_of___x2__ __x2__])
          end
        module Delay =
          struct
            let rpc_of___x1__ =
              function | __x365__ -> rpc_of_debug_info __x365__
            and __x1___of_rpc =
              function | __x364__ -> debug_info_of_rpc __x364__
            and rpc_of___x2__ = function | __x363__ -> Vm.rpc_of_id __x363__
            and __x2___of_rpc = function | __x362__ -> Vm.id_of_rpc __x362__
            and rpc_of___x3__ = function | __x361__ -> Rpc.Float __x361__
            and __x3___of_rpc =
              function
              | __x360__ ->
                  (match __x360__ with
                   | Rpc.Float x -> x
                   | Rpc.String s -> float_of_string s
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "__x3__" "__x360__" (Rpc.to_string __x__) "Float"
                        else ();
                        raise (Rpc.Runtime_error ("Float", __x__))))
            type response = Task.id
            let rpc_of_response =
              function | __x367__ -> Task.rpc_of_id __x367__
            let response_of_rpc =
              function | __x366__ -> Task.id_of_rpc __x366__
            let call_of_delay =
              function
              | __x1__ ->
                  (function
                   | __x2__ ->
                       (function
                        | __x3__ ->
                            Rpc.call "VM.delay"
                              [rpc_of___x1__ __x1__;
                              rpc_of___x2__ __x2__;
                              rpc_of___x3__ __x3__]))
          end
        module Start =
          struct
            let rpc_of___x1__ =
              function | __x373__ -> rpc_of_debug_info __x373__
            and __x1___of_rpc =
              function | __x372__ -> debug_info_of_rpc __x372__
            and rpc_of___x2__ = function | __x371__ -> Vm.rpc_of_id __x371__
            and __x2___of_rpc = function | __x370__ -> Vm.id_of_rpc __x370__
            and rpc_of___x3__ = function | __x369__ -> Rpc.Bool __x369__
            and __x3___of_rpc =
              function
              | __x368__ ->
                  (match __x368__ with
                   | Rpc.Bool x -> x
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "__x3__" "__x368__" (Rpc.to_string __x__) "Bool"
                        else ();
                        raise (Rpc.Runtime_error ("Bool", __x__))))
            type response = Task.id
            let rpc_of_response =
              function | __x375__ -> Task.rpc_of_id __x375__
            let response_of_rpc =
              function | __x374__ -> Task.id_of_rpc __x374__
            let call_of_start =
              function
              | __x1__ ->
                  (function
                   | __x2__ ->
                       (function
                        | __x3__ ->
                            Rpc.call "VM.start"
                              [rpc_of___x1__ __x1__;
                              rpc_of___x2__ __x2__;
                              rpc_of___x3__ __x3__]))
          end
        module Shutdown =
          struct
            let rpc_of___x1__ =
              function | __x383__ -> rpc_of_debug_info __x383__
            and __x1___of_rpc =
              function | __x382__ -> debug_info_of_rpc __x382__
            and rpc_of___x2__ = function | __x381__ -> Vm.rpc_of_id __x381__
            and __x2___of_rpc = function | __x380__ -> Vm.id_of_rpc __x380__
            and rpc_of___x3__ =
              function
              | __x378__ ->
                  (match __x378__ with
                   | Some __x379__ -> Rpc.Enum [Rpc.Float __x379__]
                   | None -> Rpc.Enum [])
            and __x3___of_rpc =
              function
              | __x376__ ->
                  (match __x376__ with
                   | Rpc.Enum [] -> None
                   | Rpc.Enum (__x377__::[]) ->
                       Some
                         ((match __x377__ with
                           | Rpc.Float x -> x
                           | Rpc.String s -> float_of_string s
                           | __x__ ->
                               (if Rpc.get_debug ()
                                then
                                  Printf.eprintf
                                    "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                    "__x3__" "__x377__" (Rpc.to_string __x__)
                                    "Float"
                                else ();
                                raise (Rpc.Runtime_error ("Float", __x__)))))
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "__x3__" "__x376__" (Rpc.to_string __x__)
                            "Enum[]/Enum[_]"
                        else ();
                        raise (Rpc.Runtime_error ("Enum[]/Enum[_]", __x__))))
            type response = Task.id
            let rpc_of_response =
              function | __x385__ -> Task.rpc_of_id __x385__
            let response_of_rpc =
              function | __x384__ -> Task.id_of_rpc __x384__
            let call_of_shutdown =
              function
              | __x1__ ->
                  (function
                   | __x2__ ->
                       (function
                        | __x3__ ->
                            Rpc.call "VM.shutdown"
                              [rpc_of___x1__ __x1__;
                              rpc_of___x2__ __x2__;
                              rpc_of___x3__ __x3__]))
          end
        module Reboot =
          struct
            let rpc_of___x1__ =
              function | __x393__ -> rpc_of_debug_info __x393__
            and __x1___of_rpc =
              function | __x392__ -> debug_info_of_rpc __x392__
            and rpc_of___x2__ = function | __x391__ -> Vm.rpc_of_id __x391__
            and __x2___of_rpc = function | __x390__ -> Vm.id_of_rpc __x390__
            and rpc_of___x3__ =
              function
              | __x388__ ->
                  (match __x388__ with
                   | Some __x389__ -> Rpc.Enum [Rpc.Float __x389__]
                   | None -> Rpc.Enum [])
            and __x3___of_rpc =
              function
              | __x386__ ->
                  (match __x386__ with
                   | Rpc.Enum [] -> None
                   | Rpc.Enum (__x387__::[]) ->
                       Some
                         ((match __x387__ with
                           | Rpc.Float x -> x
                           | Rpc.String s -> float_of_string s
                           | __x__ ->
                               (if Rpc.get_debug ()
                                then
                                  Printf.eprintf
                                    "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                    "__x3__" "__x387__" (Rpc.to_string __x__)
                                    "Float"
                                else ();
                                raise (Rpc.Runtime_error ("Float", __x__)))))
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "__x3__" "__x386__" (Rpc.to_string __x__)
                            "Enum[]/Enum[_]"
                        else ();
                        raise (Rpc.Runtime_error ("Enum[]/Enum[_]", __x__))))
            type response = Task.id
            let rpc_of_response =
              function | __x395__ -> Task.rpc_of_id __x395__
            let response_of_rpc =
              function | __x394__ -> Task.id_of_rpc __x394__
            let call_of_reboot =
              function
              | __x1__ ->
                  (function
                   | __x2__ ->
                       (function
                        | __x3__ ->
                            Rpc.call "VM.reboot"
                              [rpc_of___x1__ __x1__;
                              rpc_of___x2__ __x2__;
                              rpc_of___x3__ __x3__]))
          end
        module Suspend =
          struct
            let rpc_of___x1__ =
              function | __x401__ -> rpc_of_debug_info __x401__
            and __x1___of_rpc =
              function | __x400__ -> debug_info_of_rpc __x400__
            and rpc_of___x2__ = function | __x399__ -> Vm.rpc_of_id __x399__
            and __x2___of_rpc = function | __x398__ -> Vm.id_of_rpc __x398__
            and rpc_of___x3__ = function | __x397__ -> rpc_of_disk __x397__
            and __x3___of_rpc = function | __x396__ -> disk_of_rpc __x396__
            type response = Task.id
            let rpc_of_response =
              function | __x403__ -> Task.rpc_of_id __x403__
            let response_of_rpc =
              function | __x402__ -> Task.id_of_rpc __x402__
            let call_of_suspend =
              function
              | __x1__ ->
                  (function
                   | __x2__ ->
                       (function
                        | __x3__ ->
                            Rpc.call "VM.suspend"
                              [rpc_of___x1__ __x1__;
                              rpc_of___x2__ __x2__;
                              rpc_of___x3__ __x3__]))
          end
        module Resume =
          struct
            let rpc_of___x1__ =
              function | __x409__ -> rpc_of_debug_info __x409__
            and __x1___of_rpc =
              function | __x408__ -> debug_info_of_rpc __x408__
            and rpc_of___x2__ = function | __x407__ -> Vm.rpc_of_id __x407__
            and __x2___of_rpc = function | __x406__ -> Vm.id_of_rpc __x406__
            and rpc_of___x3__ = function | __x405__ -> rpc_of_disk __x405__
            and __x3___of_rpc = function | __x404__ -> disk_of_rpc __x404__
            type response = Task.id
            let rpc_of_response =
              function | __x411__ -> Task.rpc_of_id __x411__
            let response_of_rpc =
              function | __x410__ -> Task.id_of_rpc __x410__
            let call_of_resume =
              function
              | __x1__ ->
                  (function
                   | __x2__ ->
                       (function
                        | __x3__ ->
                            Rpc.call "VM.resume"
                              [rpc_of___x1__ __x1__;
                              rpc_of___x2__ __x2__;
                              rpc_of___x3__ __x3__]))
          end
        module S3suspend =
          struct
            let rpc_of___x1__ =
              function | __x415__ -> rpc_of_debug_info __x415__
            and __x1___of_rpc =
              function | __x414__ -> debug_info_of_rpc __x414__
            and rpc_of___x2__ = function | __x413__ -> Vm.rpc_of_id __x413__
            and __x2___of_rpc = function | __x412__ -> Vm.id_of_rpc __x412__
            type response = Task.id
            let rpc_of_response =
              function | __x417__ -> Task.rpc_of_id __x417__
            let response_of_rpc =
              function | __x416__ -> Task.id_of_rpc __x416__
            let call_of_s3suspend =
              function
              | __x1__ ->
                  (function
                   | __x2__ ->
                       Rpc.call "VM.s3suspend"
                         [rpc_of___x1__ __x1__; rpc_of___x2__ __x2__])
          end
        module S3resume =
          struct
            let rpc_of___x1__ =
              function | __x421__ -> rpc_of_debug_info __x421__
            and __x1___of_rpc =
              function | __x420__ -> debug_info_of_rpc __x420__
            and rpc_of___x2__ = function | __x419__ -> Vm.rpc_of_id __x419__
            and __x2___of_rpc = function | __x418__ -> Vm.id_of_rpc __x418__
            type response = Task.id
            let rpc_of_response =
              function | __x423__ -> Task.rpc_of_id __x423__
            let response_of_rpc =
              function | __x422__ -> Task.id_of_rpc __x422__
            let call_of_s3resume =
              function
              | __x1__ ->
                  (function
                   | __x2__ ->
                       Rpc.call "VM.s3resume"
                         [rpc_of___x1__ __x1__; rpc_of___x2__ __x2__])
          end
        module Export_metadata =
          struct
            let rpc_of___x1__ =
              function | __x427__ -> rpc_of_debug_info __x427__
            and __x1___of_rpc =
              function | __x426__ -> debug_info_of_rpc __x426__
            and rpc_of___x2__ = function | __x425__ -> Vm.rpc_of_id __x425__
            and __x2___of_rpc = function | __x424__ -> Vm.id_of_rpc __x424__
            type response = string
            let rpc_of_response = function | __x429__ -> Rpc.String __x429__
            let response_of_rpc =
              function
              | __x428__ ->
                  (match __x428__ with
                   | Rpc.String x -> x
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "response" "__x428__" (Rpc.to_string __x__)
                            "String(string)"
                        else ();
                        raise (Rpc.Runtime_error ("String(string)", __x__))))
            let call_of_export_metadata =
              function
              | __x1__ ->
                  (function
                   | __x2__ ->
                       Rpc.call "VM.export_metadata"
                         [rpc_of___x1__ __x1__; rpc_of___x2__ __x2__])
          end
        module Import_metadata =
          struct
            let rpc_of___x1__ =
              function | __x433__ -> rpc_of_debug_info __x433__
            and __x1___of_rpc =
              function | __x432__ -> debug_info_of_rpc __x432__
            and rpc_of___x2__ = function | __x431__ -> Rpc.String __x431__
            and __x2___of_rpc =
              function
              | __x430__ ->
                  (match __x430__ with
                   | Rpc.String x -> x
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "__x2__" "__x430__" (Rpc.to_string __x__)
                            "String(string)"
                        else ();
                        raise (Rpc.Runtime_error ("String(string)", __x__))))
            type response = Vm.id
            let rpc_of_response =
              function | __x435__ -> Vm.rpc_of_id __x435__
            let response_of_rpc =
              function | __x434__ -> Vm.id_of_rpc __x434__
            let call_of_import_metadata =
              function
              | __x1__ ->
                  (function
                   | __x2__ ->
                       Rpc.call "VM.import_metadata"
                         [rpc_of___x1__ __x1__; rpc_of___x2__ __x2__])
          end
      end
    module PCI =
      struct
        module Add =
          struct
            let rpc_of___x1__ =
              function | __x439__ -> rpc_of_debug_info __x439__
            and __x1___of_rpc =
              function | __x438__ -> debug_info_of_rpc __x438__
            and rpc_of___x2__ = function | __x437__ -> Pci.rpc_of_t __x437__
            and __x2___of_rpc = function | __x436__ -> Pci.t_of_rpc __x436__
            type response = Pci.id
            let rpc_of_response =
              function | __x441__ -> Pci.rpc_of_id __x441__
            let response_of_rpc =
              function | __x440__ -> Pci.id_of_rpc __x440__
            let call_of_add =
              function
              | __x1__ ->
                  (function
                   | __x2__ ->
                       Rpc.call "PCI.add"
                         [rpc_of___x1__ __x1__; rpc_of___x2__ __x2__])
          end
        module Remove =
          struct
            let rpc_of___x1__ =
              function | __x445__ -> rpc_of_debug_info __x445__
            and __x1___of_rpc =
              function | __x444__ -> debug_info_of_rpc __x444__
            and rpc_of___x2__ = function | __x443__ -> Pci.rpc_of_id __x443__
            and __x2___of_rpc = function | __x442__ -> Pci.id_of_rpc __x442__
            type response = unit
            let rpc_of_response = function | __x447__ -> Rpc.Null
            let response_of_rpc =
              function
              | __x446__ ->
                  (match __x446__ with
                   | Rpc.Null -> ()
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "response" "__x446__" (Rpc.to_string __x__)
                            "Null"
                        else ();
                        raise (Rpc.Runtime_error ("Null", __x__))))
            let call_of_remove =
              function
              | __x1__ ->
                  (function
                   | __x2__ ->
                       Rpc.call "PCI.remove"
                         [rpc_of___x1__ __x1__; rpc_of___x2__ __x2__])
          end
        module Stat =
          struct
            let rpc_of___x1__ =
              function | __x451__ -> rpc_of_debug_info __x451__
            and __x1___of_rpc =
              function | __x450__ -> debug_info_of_rpc __x450__
            and rpc_of___x2__ = function | __x449__ -> Pci.rpc_of_id __x449__
            and __x2___of_rpc = function | __x448__ -> Pci.id_of_rpc __x448__
            type response = (Pci.t * Pci.state)
            let rpc_of_response =
              function
              | __x455__ ->
                  let (__x456__, __x457__) = __x455__ in
                  Rpc.Enum [Pci.rpc_of_t __x456__; Pci.rpc_of_state __x457__]
            let response_of_rpc =
              function
              | __x452__ ->
                  (match __x452__ with
                   | Rpc.Enum (__x453__::__x454__::[]) ->
                       ((Pci.t_of_rpc __x453__), (Pci.state_of_rpc __x454__))
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "response" "__x452__" (Rpc.to_string __x__)
                            "List"
                        else ();
                        raise (Rpc.Runtime_error ("List", __x__))))
            let call_of_stat =
              function
              | __x1__ ->
                  (function
                   | __x2__ ->
                       Rpc.call "PCI.stat"
                         [rpc_of___x1__ __x1__; rpc_of___x2__ __x2__])
          end
        module List =
          struct
            let rpc_of___x1__ =
              function | __x461__ -> rpc_of_debug_info __x461__
            and __x1___of_rpc =
              function | __x460__ -> debug_info_of_rpc __x460__
            and rpc_of___x2__ = function | __x459__ -> Vm.rpc_of_id __x459__
            and __x2___of_rpc = function | __x458__ -> Vm.id_of_rpc __x458__
            type response = (Pci.t * Pci.state) list
            let rpc_of_response =
              function
              | __x467__ ->
                  Rpc.Enum
                    (List.map
                       (function
                        | __x468__ ->
                            let (__x469__, __x470__) = __x468__ in
                            Rpc.Enum
                              [Pci.rpc_of_t __x469__;
                              Pci.rpc_of_state __x470__]) __x467__)
            let response_of_rpc =
              function
              | __x462__ ->
                  (match __x462__ with
                   | Rpc.Enum __x463__ ->
                       List.map
                         (function
                          | __x464__ ->
                              (match __x464__ with
                               | Rpc.Enum (__x465__::__x466__::[]) ->
                                   ((Pci.t_of_rpc __x465__),
                                     (Pci.state_of_rpc __x466__))
                               | __x__ ->
                                   (if Rpc.get_debug ()
                                    then
                                      Printf.eprintf
                                        "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                        "response" "__x464__"
                                        (Rpc.to_string __x__) "List"
                                    else ();
                                    raise (Rpc.Runtime_error ("List", __x__)))))
                         __x463__
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "response" "__x462__" (Rpc.to_string __x__)
                            "List"
                        else ();
                        raise (Rpc.Runtime_error ("List", __x__))))
            let call_of_list =
              function
              | __x1__ ->
                  (function
                   | __x2__ ->
                       Rpc.call "PCI.list"
                         [rpc_of___x1__ __x1__; rpc_of___x2__ __x2__])
          end
      end
    module VBD =
      struct
        module Add =
          struct
            let rpc_of___x1__ =
              function | __x474__ -> rpc_of_debug_info __x474__
            and __x1___of_rpc =
              function | __x473__ -> debug_info_of_rpc __x473__
            and rpc_of___x2__ = function | __x472__ -> Vbd.rpc_of_t __x472__
            and __x2___of_rpc = function | __x471__ -> Vbd.t_of_rpc __x471__
            type response = Vbd.id
            let rpc_of_response =
              function | __x476__ -> Vbd.rpc_of_id __x476__
            let response_of_rpc =
              function | __x475__ -> Vbd.id_of_rpc __x475__
            let call_of_add =
              function
              | __x1__ ->
                  (function
                   | __x2__ ->
                       Rpc.call "VBD.add"
                         [rpc_of___x1__ __x1__; rpc_of___x2__ __x2__])
          end
        module Plug =
          struct
            let rpc_of___x1__ =
              function | __x480__ -> rpc_of_debug_info __x480__
            and __x1___of_rpc =
              function | __x479__ -> debug_info_of_rpc __x479__
            and rpc_of___x2__ = function | __x478__ -> Vbd.rpc_of_id __x478__
            and __x2___of_rpc = function | __x477__ -> Vbd.id_of_rpc __x477__
            type response = Task.id
            let rpc_of_response =
              function | __x482__ -> Task.rpc_of_id __x482__
            let response_of_rpc =
              function | __x481__ -> Task.id_of_rpc __x481__
            let call_of_plug =
              function
              | __x1__ ->
                  (function
                   | __x2__ ->
                       Rpc.call "VBD.plug"
                         [rpc_of___x1__ __x1__; rpc_of___x2__ __x2__])
          end
        module Unplug =
          struct
            let rpc_of___x1__ =
              function | __x488__ -> rpc_of_debug_info __x488__
            and __x1___of_rpc =
              function | __x487__ -> debug_info_of_rpc __x487__
            and rpc_of___x2__ = function | __x486__ -> Vbd.rpc_of_id __x486__
            and __x2___of_rpc = function | __x485__ -> Vbd.id_of_rpc __x485__
            and rpc_of___x3__ = function | __x484__ -> Rpc.Bool __x484__
            and __x3___of_rpc =
              function
              | __x483__ ->
                  (match __x483__ with
                   | Rpc.Bool x -> x
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "__x3__" "__x483__" (Rpc.to_string __x__) "Bool"
                        else ();
                        raise (Rpc.Runtime_error ("Bool", __x__))))
            type response = Task.id
            let rpc_of_response =
              function | __x490__ -> Task.rpc_of_id __x490__
            let response_of_rpc =
              function | __x489__ -> Task.id_of_rpc __x489__
            let call_of_unplug =
              function
              | __x1__ ->
                  (function
                   | __x2__ ->
                       (function
                        | __x3__ ->
                            Rpc.call "VBD.unplug"
                              [rpc_of___x1__ __x1__;
                              rpc_of___x2__ __x2__;
                              rpc_of___x3__ __x3__]))
          end
        module Eject =
          struct
            let rpc_of___x1__ =
              function | __x494__ -> rpc_of_debug_info __x494__
            and __x1___of_rpc =
              function | __x493__ -> debug_info_of_rpc __x493__
            and rpc_of___x2__ = function | __x492__ -> Vbd.rpc_of_id __x492__
            and __x2___of_rpc = function | __x491__ -> Vbd.id_of_rpc __x491__
            type response = Task.id
            let rpc_of_response =
              function | __x496__ -> Task.rpc_of_id __x496__
            let response_of_rpc =
              function | __x495__ -> Task.id_of_rpc __x495__
            let call_of_eject =
              function
              | __x1__ ->
                  (function
                   | __x2__ ->
                       Rpc.call "VBD.eject"
                         [rpc_of___x1__ __x1__; rpc_of___x2__ __x2__])
          end
        module Insert =
          struct
            let rpc_of___x1__ =
              function | __x502__ -> rpc_of_debug_info __x502__
            and __x1___of_rpc =
              function | __x501__ -> debug_info_of_rpc __x501__
            and rpc_of___x2__ = function | __x500__ -> Vbd.rpc_of_id __x500__
            and __x2___of_rpc = function | __x499__ -> Vbd.id_of_rpc __x499__
            and rpc_of___x3__ = function | __x498__ -> rpc_of_disk __x498__
            and __x3___of_rpc = function | __x497__ -> disk_of_rpc __x497__
            type response = Task.id
            let rpc_of_response =
              function | __x504__ -> Task.rpc_of_id __x504__
            let response_of_rpc =
              function | __x503__ -> Task.id_of_rpc __x503__
            let call_of_insert =
              function
              | __x1__ ->
                  (function
                   | __x2__ ->
                       (function
                        | __x3__ ->
                            Rpc.call "VBD.insert"
                              [rpc_of___x1__ __x1__;
                              rpc_of___x2__ __x2__;
                              rpc_of___x3__ __x3__]))
          end
        module Stat =
          struct
            let rpc_of___x1__ =
              function | __x508__ -> rpc_of_debug_info __x508__
            and __x1___of_rpc =
              function | __x507__ -> debug_info_of_rpc __x507__
            and rpc_of___x2__ = function | __x506__ -> Vbd.rpc_of_id __x506__
            and __x2___of_rpc = function | __x505__ -> Vbd.id_of_rpc __x505__
            type response = (Vbd.t * Vbd.state)
            let rpc_of_response =
              function
              | __x512__ ->
                  let (__x513__, __x514__) = __x512__ in
                  Rpc.Enum [Vbd.rpc_of_t __x513__; Vbd.rpc_of_state __x514__]
            let response_of_rpc =
              function
              | __x509__ ->
                  (match __x509__ with
                   | Rpc.Enum (__x510__::__x511__::[]) ->
                       ((Vbd.t_of_rpc __x510__), (Vbd.state_of_rpc __x511__))
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "response" "__x509__" (Rpc.to_string __x__)
                            "List"
                        else ();
                        raise (Rpc.Runtime_error ("List", __x__))))
            let call_of_stat =
              function
              | __x1__ ->
                  (function
                   | __x2__ ->
                       Rpc.call "VBD.stat"
                         [rpc_of___x1__ __x1__; rpc_of___x2__ __x2__])
          end
        module List =
          struct
            let rpc_of___x1__ =
              function | __x518__ -> rpc_of_debug_info __x518__
            and __x1___of_rpc =
              function | __x517__ -> debug_info_of_rpc __x517__
            and rpc_of___x2__ = function | __x516__ -> Vm.rpc_of_id __x516__
            and __x2___of_rpc = function | __x515__ -> Vm.id_of_rpc __x515__
            type response = (Vbd.t * Vbd.state) list
            let rpc_of_response =
              function
              | __x524__ ->
                  Rpc.Enum
                    (List.map
                       (function
                        | __x525__ ->
                            let (__x526__, __x527__) = __x525__ in
                            Rpc.Enum
                              [Vbd.rpc_of_t __x526__;
                              Vbd.rpc_of_state __x527__]) __x524__)
            let response_of_rpc =
              function
              | __x519__ ->
                  (match __x519__ with
                   | Rpc.Enum __x520__ ->
                       List.map
                         (function
                          | __x521__ ->
                              (match __x521__ with
                               | Rpc.Enum (__x522__::__x523__::[]) ->
                                   ((Vbd.t_of_rpc __x522__),
                                     (Vbd.state_of_rpc __x523__))
                               | __x__ ->
                                   (if Rpc.get_debug ()
                                    then
                                      Printf.eprintf
                                        "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                        "response" "__x521__"
                                        (Rpc.to_string __x__) "List"
                                    else ();
                                    raise (Rpc.Runtime_error ("List", __x__)))))
                         __x520__
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "response" "__x519__" (Rpc.to_string __x__)
                            "List"
                        else ();
                        raise (Rpc.Runtime_error ("List", __x__))))
            let call_of_list =
              function
              | __x1__ ->
                  (function
                   | __x2__ ->
                       Rpc.call "VBD.list"
                         [rpc_of___x1__ __x1__; rpc_of___x2__ __x2__])
          end
        module Remove =
          struct
            let rpc_of___x1__ =
              function | __x531__ -> rpc_of_debug_info __x531__
            and __x1___of_rpc =
              function | __x530__ -> debug_info_of_rpc __x530__
            and rpc_of___x2__ = function | __x529__ -> Vbd.rpc_of_id __x529__
            and __x2___of_rpc = function | __x528__ -> Vbd.id_of_rpc __x528__
            type response = unit
            let rpc_of_response = function | __x533__ -> Rpc.Null
            let response_of_rpc =
              function
              | __x532__ ->
                  (match __x532__ with
                   | Rpc.Null -> ()
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "response" "__x532__" (Rpc.to_string __x__)
                            "Null"
                        else ();
                        raise (Rpc.Runtime_error ("Null", __x__))))
            let call_of_remove =
              function
              | __x1__ ->
                  (function
                   | __x2__ ->
                       Rpc.call "VBD.remove"
                         [rpc_of___x1__ __x1__; rpc_of___x2__ __x2__])
          end
      end
    module VUSB =
      struct
        module Add =
          struct
            let rpc_of___x1__ =
              function | __x537__ -> rpc_of_debug_info __x537__
            and __x1___of_rpc =
              function | __x536__ -> debug_info_of_rpc __x536__
            and rpc_of___x2__ = function | __x535__ -> Vusb.rpc_of_t __x535__
            and __x2___of_rpc = function | __x534__ -> Vusb.t_of_rpc __x534__
            type response = Vusb.id
            let rpc_of_response =
              function | __x539__ -> Vusb.rpc_of_id __x539__
            let response_of_rpc =
              function | __x538__ -> Vusb.id_of_rpc __x538__
            let call_of_add =
              function
              | __x1__ ->
                  (function
                   | __x2__ ->
                       Rpc.call "VUSB.add"
                         [rpc_of___x1__ __x1__; rpc_of___x2__ __x2__])
          end
        module Plug =
          struct
            let rpc_of___x1__ =
              function | __x543__ -> rpc_of_debug_info __x543__
            and __x1___of_rpc =
              function | __x542__ -> debug_info_of_rpc __x542__
            and rpc_of___x2__ =
              function | __x541__ -> Vusb.rpc_of_id __x541__
            and __x2___of_rpc =
              function | __x540__ -> Vusb.id_of_rpc __x540__
            type response = Task.id
            let rpc_of_response =
              function | __x545__ -> Task.rpc_of_id __x545__
            let response_of_rpc =
              function | __x544__ -> Task.id_of_rpc __x544__
            let call_of_plug =
              function
              | __x1__ ->
                  (function
                   | __x2__ ->
                       Rpc.call "VUSB.plug"
                         [rpc_of___x1__ __x1__; rpc_of___x2__ __x2__])
          end
        module Unplug =
          struct
            let rpc_of___x1__ =
              function | __x549__ -> rpc_of_debug_info __x549__
            and __x1___of_rpc =
              function | __x548__ -> debug_info_of_rpc __x548__
            and rpc_of___x2__ =
              function | __x547__ -> Vusb.rpc_of_id __x547__
            and __x2___of_rpc =
              function | __x546__ -> Vusb.id_of_rpc __x546__
            type response = Task.id
            let rpc_of_response =
              function | __x551__ -> Task.rpc_of_id __x551__
            let response_of_rpc =
              function | __x550__ -> Task.id_of_rpc __x550__
            let call_of_unplug =
              function
              | __x1__ ->
                  (function
                   | __x2__ ->
                       Rpc.call "VUSB.unplug"
                         [rpc_of___x1__ __x1__; rpc_of___x2__ __x2__])
          end
        module Stat =
          struct
            let rpc_of___x1__ =
              function | __x555__ -> rpc_of_debug_info __x555__
            and __x1___of_rpc =
              function | __x554__ -> debug_info_of_rpc __x554__
            and rpc_of___x2__ =
              function | __x553__ -> Vusb.rpc_of_id __x553__
            and __x2___of_rpc =
              function | __x552__ -> Vusb.id_of_rpc __x552__
            type response = (Vusb.t * Vusb.state)
            let rpc_of_response =
              function
              | __x559__ ->
                  let (__x560__, __x561__) = __x559__ in
                  Rpc.Enum
                    [Vusb.rpc_of_t __x560__; Vusb.rpc_of_state __x561__]
            let response_of_rpc =
              function
              | __x556__ ->
                  (match __x556__ with
                   | Rpc.Enum (__x557__::__x558__::[]) ->
                       ((Vusb.t_of_rpc __x557__),
                         (Vusb.state_of_rpc __x558__))
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "response" "__x556__" (Rpc.to_string __x__)
                            "List"
                        else ();
                        raise (Rpc.Runtime_error ("List", __x__))))
            let call_of_stat =
              function
              | __x1__ ->
                  (function
                   | __x2__ ->
                       Rpc.call "VUSB.stat"
                         [rpc_of___x1__ __x1__; rpc_of___x2__ __x2__])
          end
        module List =
          struct
            let rpc_of___x1__ =
              function | __x565__ -> rpc_of_debug_info __x565__
            and __x1___of_rpc =
              function | __x564__ -> debug_info_of_rpc __x564__
            and rpc_of___x2__ = function | __x563__ -> Vm.rpc_of_id __x563__
            and __x2___of_rpc = function | __x562__ -> Vm.id_of_rpc __x562__
            type response = (Vusb.t * Vusb.state) list
            let rpc_of_response =
              function
              | __x571__ ->
                  Rpc.Enum
                    (List.map
                       (function
                        | __x572__ ->
                            let (__x573__, __x574__) = __x572__ in
                            Rpc.Enum
                              [Vusb.rpc_of_t __x573__;
                              Vusb.rpc_of_state __x574__]) __x571__)
            let response_of_rpc =
              function
              | __x566__ ->
                  (match __x566__ with
                   | Rpc.Enum __x567__ ->
                       List.map
                         (function
                          | __x568__ ->
                              (match __x568__ with
                               | Rpc.Enum (__x569__::__x570__::[]) ->
                                   ((Vusb.t_of_rpc __x569__),
                                     (Vusb.state_of_rpc __x570__))
                               | __x__ ->
                                   (if Rpc.get_debug ()
                                    then
                                      Printf.eprintf
                                        "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                        "response" "__x568__"
                                        (Rpc.to_string __x__) "List"
                                    else ();
                                    raise (Rpc.Runtime_error ("List", __x__)))))
                         __x567__
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "response" "__x566__" (Rpc.to_string __x__)
                            "List"
                        else ();
                        raise (Rpc.Runtime_error ("List", __x__))))
            let call_of_list =
              function
              | __x1__ ->
                  (function
                   | __x2__ ->
                       Rpc.call "VUSB.list"
                         [rpc_of___x1__ __x1__; rpc_of___x2__ __x2__])
          end
        module Remove =
          struct
            let rpc_of___x1__ =
              function | __x578__ -> rpc_of_debug_info __x578__
            and __x1___of_rpc =
              function | __x577__ -> debug_info_of_rpc __x577__
            and rpc_of___x2__ =
              function | __x576__ -> Vusb.rpc_of_id __x576__
            and __x2___of_rpc =
              function | __x575__ -> Vusb.id_of_rpc __x575__
            type response = unit
            let rpc_of_response = function | __x580__ -> Rpc.Null
            let response_of_rpc =
              function
              | __x579__ ->
                  (match __x579__ with
                   | Rpc.Null -> ()
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "response" "__x579__" (Rpc.to_string __x__)
                            "Null"
                        else ();
                        raise (Rpc.Runtime_error ("Null", __x__))))
            let call_of_remove =
              function
              | __x1__ ->
                  (function
                   | __x2__ ->
                       Rpc.call "VUSB.remove"
                         [rpc_of___x1__ __x1__; rpc_of___x2__ __x2__])
          end
      end
    module VIF =
      struct
        module Add =
          struct
            let rpc_of___x1__ =
              function | __x584__ -> rpc_of_debug_info __x584__
            and __x1___of_rpc =
              function | __x583__ -> debug_info_of_rpc __x583__
            and rpc_of___x2__ = function | __x582__ -> Vif.rpc_of_t __x582__
            and __x2___of_rpc = function | __x581__ -> Vif.t_of_rpc __x581__
            type response = Vif.id
            let rpc_of_response =
              function | __x586__ -> Vif.rpc_of_id __x586__
            let response_of_rpc =
              function | __x585__ -> Vif.id_of_rpc __x585__
            let call_of_add =
              function
              | __x1__ ->
                  (function
                   | __x2__ ->
                       Rpc.call "VIF.add"
                         [rpc_of___x1__ __x1__; rpc_of___x2__ __x2__])
          end
        module Plug =
          struct
            let rpc_of___x1__ =
              function | __x590__ -> rpc_of_debug_info __x590__
            and __x1___of_rpc =
              function | __x589__ -> debug_info_of_rpc __x589__
            and rpc_of___x2__ = function | __x588__ -> Vif.rpc_of_id __x588__
            and __x2___of_rpc = function | __x587__ -> Vif.id_of_rpc __x587__
            type response = Task.id
            let rpc_of_response =
              function | __x592__ -> Task.rpc_of_id __x592__
            let response_of_rpc =
              function | __x591__ -> Task.id_of_rpc __x591__
            let call_of_plug =
              function
              | __x1__ ->
                  (function
                   | __x2__ ->
                       Rpc.call "VIF.plug"
                         [rpc_of___x1__ __x1__; rpc_of___x2__ __x2__])
          end
        module Unplug =
          struct
            let rpc_of___x1__ =
              function | __x598__ -> rpc_of_debug_info __x598__
            and __x1___of_rpc =
              function | __x597__ -> debug_info_of_rpc __x597__
            and rpc_of___x2__ = function | __x596__ -> Vif.rpc_of_id __x596__
            and __x2___of_rpc = function | __x595__ -> Vif.id_of_rpc __x595__
            and rpc_of___x3__ = function | __x594__ -> Rpc.Bool __x594__
            and __x3___of_rpc =
              function
              | __x593__ ->
                  (match __x593__ with
                   | Rpc.Bool x -> x
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "__x3__" "__x593__" (Rpc.to_string __x__) "Bool"
                        else ();
                        raise (Rpc.Runtime_error ("Bool", __x__))))
            type response = Task.id
            let rpc_of_response =
              function | __x600__ -> Task.rpc_of_id __x600__
            let response_of_rpc =
              function | __x599__ -> Task.id_of_rpc __x599__
            let call_of_unplug =
              function
              | __x1__ ->
                  (function
                   | __x2__ ->
                       (function
                        | __x3__ ->
                            Rpc.call "VIF.unplug"
                              [rpc_of___x1__ __x1__;
                              rpc_of___x2__ __x2__;
                              rpc_of___x3__ __x3__]))
          end
        module Move =
          struct
            let rpc_of___x1__ =
              function | __x606__ -> rpc_of_debug_info __x606__
            and __x1___of_rpc =
              function | __x605__ -> debug_info_of_rpc __x605__
            and rpc_of___x2__ = function | __x604__ -> Vif.rpc_of_id __x604__
            and __x2___of_rpc = function | __x603__ -> Vif.id_of_rpc __x603__
            and rpc_of___x3__ =
              function | __x602__ -> Network.rpc_of_t __x602__
            and __x3___of_rpc =
              function | __x601__ -> Network.t_of_rpc __x601__
            type response = Task.id
            let rpc_of_response =
              function | __x608__ -> Task.rpc_of_id __x608__
            let response_of_rpc =
              function | __x607__ -> Task.id_of_rpc __x607__
            let call_of_move =
              function
              | __x1__ ->
                  (function
                   | __x2__ ->
                       (function
                        | __x3__ ->
                            Rpc.call "VIF.move"
                              [rpc_of___x1__ __x1__;
                              rpc_of___x2__ __x2__;
                              rpc_of___x3__ __x3__]))
          end
        module Stat =
          struct
            let rpc_of___x1__ =
              function | __x612__ -> rpc_of_debug_info __x612__
            and __x1___of_rpc =
              function | __x611__ -> debug_info_of_rpc __x611__
            and rpc_of___x2__ = function | __x610__ -> Vif.rpc_of_id __x610__
            and __x2___of_rpc = function | __x609__ -> Vif.id_of_rpc __x609__
            type response = (Vif.t * Vif.state)
            let rpc_of_response =
              function
              | __x616__ ->
                  let (__x617__, __x618__) = __x616__ in
                  Rpc.Enum [Vif.rpc_of_t __x617__; Vif.rpc_of_state __x618__]
            let response_of_rpc =
              function
              | __x613__ ->
                  (match __x613__ with
                   | Rpc.Enum (__x614__::__x615__::[]) ->
                       ((Vif.t_of_rpc __x614__), (Vif.state_of_rpc __x615__))
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "response" "__x613__" (Rpc.to_string __x__)
                            "List"
                        else ();
                        raise (Rpc.Runtime_error ("List", __x__))))
            let call_of_stat =
              function
              | __x1__ ->
                  (function
                   | __x2__ ->
                       Rpc.call "VIF.stat"
                         [rpc_of___x1__ __x1__; rpc_of___x2__ __x2__])
          end
        module List =
          struct
            let rpc_of___x1__ =
              function | __x622__ -> rpc_of_debug_info __x622__
            and __x1___of_rpc =
              function | __x621__ -> debug_info_of_rpc __x621__
            and rpc_of___x2__ = function | __x620__ -> Vm.rpc_of_id __x620__
            and __x2___of_rpc = function | __x619__ -> Vm.id_of_rpc __x619__
            type response = (Vif.t * Vif.state) list
            let rpc_of_response =
              function
              | __x628__ ->
                  Rpc.Enum
                    (List.map
                       (function
                        | __x629__ ->
                            let (__x630__, __x631__) = __x629__ in
                            Rpc.Enum
                              [Vif.rpc_of_t __x630__;
                              Vif.rpc_of_state __x631__]) __x628__)
            let response_of_rpc =
              function
              | __x623__ ->
                  (match __x623__ with
                   | Rpc.Enum __x624__ ->
                       List.map
                         (function
                          | __x625__ ->
                              (match __x625__ with
                               | Rpc.Enum (__x626__::__x627__::[]) ->
                                   ((Vif.t_of_rpc __x626__),
                                     (Vif.state_of_rpc __x627__))
                               | __x__ ->
                                   (if Rpc.get_debug ()
                                    then
                                      Printf.eprintf
                                        "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                        "response" "__x625__"
                                        (Rpc.to_string __x__) "List"
                                    else ();
                                    raise (Rpc.Runtime_error ("List", __x__)))))
                         __x624__
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "response" "__x623__" (Rpc.to_string __x__)
                            "List"
                        else ();
                        raise (Rpc.Runtime_error ("List", __x__))))
            let call_of_list =
              function
              | __x1__ ->
                  (function
                   | __x2__ ->
                       Rpc.call "VIF.list"
                         [rpc_of___x1__ __x1__; rpc_of___x2__ __x2__])
          end
        module Remove =
          struct
            let rpc_of___x1__ =
              function | __x635__ -> rpc_of_debug_info __x635__
            and __x1___of_rpc =
              function | __x634__ -> debug_info_of_rpc __x634__
            and rpc_of___x2__ = function | __x633__ -> Vif.rpc_of_id __x633__
            and __x2___of_rpc = function | __x632__ -> Vif.id_of_rpc __x632__
            type response = unit
            let rpc_of_response = function | __x637__ -> Rpc.Null
            let response_of_rpc =
              function
              | __x636__ ->
                  (match __x636__ with
                   | Rpc.Null -> ()
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "response" "__x636__" (Rpc.to_string __x__)
                            "Null"
                        else ();
                        raise (Rpc.Runtime_error ("Null", __x__))))
            let call_of_remove =
              function
              | __x1__ ->
                  (function
                   | __x2__ ->
                       Rpc.call "VIF.remove"
                         [rpc_of___x1__ __x1__; rpc_of___x2__ __x2__])
          end
        module Set_carrier =
          struct
            let rpc_of___x1__ =
              function | __x643__ -> rpc_of_debug_info __x643__
            and __x1___of_rpc =
              function | __x642__ -> debug_info_of_rpc __x642__
            and rpc_of___x2__ = function | __x641__ -> Vif.rpc_of_id __x641__
            and __x2___of_rpc = function | __x640__ -> Vif.id_of_rpc __x640__
            and rpc_of___x3__ = function | __x639__ -> Rpc.Bool __x639__
            and __x3___of_rpc =
              function
              | __x638__ ->
                  (match __x638__ with
                   | Rpc.Bool x -> x
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "__x3__" "__x638__" (Rpc.to_string __x__) "Bool"
                        else ();
                        raise (Rpc.Runtime_error ("Bool", __x__))))
            type response = Task.id
            let rpc_of_response =
              function | __x645__ -> Task.rpc_of_id __x645__
            let response_of_rpc =
              function | __x644__ -> Task.id_of_rpc __x644__
            let call_of_set_carrier =
              function
              | __x1__ ->
                  (function
                   | __x2__ ->
                       (function
                        | __x3__ ->
                            Rpc.call "VIF.set_carrier"
                              [rpc_of___x1__ __x1__;
                              rpc_of___x2__ __x2__;
                              rpc_of___x3__ __x3__]))
          end
        module Set_locking_mode =
          struct
            let rpc_of___x1__ =
              function | __x651__ -> rpc_of_debug_info __x651__
            and __x1___of_rpc =
              function | __x650__ -> debug_info_of_rpc __x650__
            and rpc_of___x2__ = function | __x649__ -> Vif.rpc_of_id __x649__
            and __x2___of_rpc = function | __x648__ -> Vif.id_of_rpc __x648__
            and rpc_of___x3__ =
              function | __x647__ -> Vif.rpc_of_locking_mode __x647__
            and __x3___of_rpc =
              function | __x646__ -> Vif.locking_mode_of_rpc __x646__
            type response = Task.id
            let rpc_of_response =
              function | __x653__ -> Task.rpc_of_id __x653__
            let response_of_rpc =
              function | __x652__ -> Task.id_of_rpc __x652__
            let call_of_set_locking_mode =
              function
              | __x1__ ->
                  (function
                   | __x2__ ->
                       (function
                        | __x3__ ->
                            Rpc.call "VIF.set_locking_mode"
                              [rpc_of___x1__ __x1__;
                              rpc_of___x2__ __x2__;
                              rpc_of___x3__ __x3__]))
          end
        module Set_ipv4_configuration =
          struct
            let rpc_of___x1__ =
              function | __x659__ -> rpc_of_debug_info __x659__
            and __x1___of_rpc =
              function | __x658__ -> debug_info_of_rpc __x658__
            and rpc_of___x2__ = function | __x657__ -> Vif.rpc_of_id __x657__
            and __x2___of_rpc = function | __x656__ -> Vif.id_of_rpc __x656__
            and rpc_of___x3__ =
              function | __x655__ -> Vif.rpc_of_ipv4_configuration __x655__
            and __x3___of_rpc =
              function | __x654__ -> Vif.ipv4_configuration_of_rpc __x654__
            type response = Task.id
            let rpc_of_response =
              function | __x661__ -> Task.rpc_of_id __x661__
            let response_of_rpc =
              function | __x660__ -> Task.id_of_rpc __x660__
            let call_of_set_ipv4_configuration =
              function
              | __x1__ ->
                  (function
                   | __x2__ ->
                       (function
                        | __x3__ ->
                            Rpc.call "VIF.set_ipv4_configuration"
                              [rpc_of___x1__ __x1__;
                              rpc_of___x2__ __x2__;
                              rpc_of___x3__ __x3__]))
          end
        module Set_ipv6_configuration =
          struct
            let rpc_of___x1__ =
              function | __x667__ -> rpc_of_debug_info __x667__
            and __x1___of_rpc =
              function | __x666__ -> debug_info_of_rpc __x666__
            and rpc_of___x2__ = function | __x665__ -> Vif.rpc_of_id __x665__
            and __x2___of_rpc = function | __x664__ -> Vif.id_of_rpc __x664__
            and rpc_of___x3__ =
              function | __x663__ -> Vif.rpc_of_ipv6_configuration __x663__
            and __x3___of_rpc =
              function | __x662__ -> Vif.ipv6_configuration_of_rpc __x662__
            type response = Task.id
            let rpc_of_response =
              function | __x669__ -> Task.rpc_of_id __x669__
            let response_of_rpc =
              function | __x668__ -> Task.id_of_rpc __x668__
            let call_of_set_ipv6_configuration =
              function
              | __x1__ ->
                  (function
                   | __x2__ ->
                       (function
                        | __x3__ ->
                            Rpc.call "VIF.set_ipv6_configuration"
                              [rpc_of___x1__ __x1__;
                              rpc_of___x2__ __x2__;
                              rpc_of___x3__ __x3__]))
          end
        module Set_pvs_proxy =
          struct
            let rpc_of___x1__ =
              function | __x677__ -> rpc_of_debug_info __x677__
            and __x1___of_rpc =
              function | __x676__ -> debug_info_of_rpc __x676__
            and rpc_of___x2__ = function | __x675__ -> Vif.rpc_of_id __x675__
            and __x2___of_rpc = function | __x674__ -> Vif.id_of_rpc __x674__
            and rpc_of___x3__ =
              function
              | __x672__ ->
                  (match __x672__ with
                   | Some __x673__ ->
                       Rpc.Enum [Vif.PVS_proxy.rpc_of_t __x673__]
                   | None -> Rpc.Enum [])
            and __x3___of_rpc =
              function
              | __x670__ ->
                  (match __x670__ with
                   | Rpc.Enum [] -> None
                   | Rpc.Enum (__x671__::[]) ->
                       Some (Vif.PVS_proxy.t_of_rpc __x671__)
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "__x3__" "__x670__" (Rpc.to_string __x__)
                            "Enum[]/Enum[_]"
                        else ();
                        raise (Rpc.Runtime_error ("Enum[]/Enum[_]", __x__))))
            type response = Task.id
            let rpc_of_response =
              function | __x679__ -> Task.rpc_of_id __x679__
            let response_of_rpc =
              function | __x678__ -> Task.id_of_rpc __x678__
            let call_of_set_pvs_proxy =
              function
              | __x1__ ->
                  (function
                   | __x2__ ->
                       (function
                        | __x3__ ->
                            Rpc.call "VIF.set_pvs_proxy"
                              [rpc_of___x1__ __x1__;
                              rpc_of___x2__ __x2__;
                              rpc_of___x3__ __x3__]))
          end
      end
    module VGPU =
      struct
        module Add =
          struct
            let rpc_of___x1__ =
              function | __x683__ -> rpc_of_debug_info __x683__
            and __x1___of_rpc =
              function | __x682__ -> debug_info_of_rpc __x682__
            and rpc_of___x2__ = function | __x681__ -> Vgpu.rpc_of_t __x681__
            and __x2___of_rpc = function | __x680__ -> Vgpu.t_of_rpc __x680__
            type response = Vgpu.id
            let rpc_of_response =
              function | __x685__ -> Vgpu.rpc_of_id __x685__
            let response_of_rpc =
              function | __x684__ -> Vgpu.id_of_rpc __x684__
            let call_of_add =
              function
              | __x1__ ->
                  (function
                   | __x2__ ->
                       Rpc.call "VGPU.add"
                         [rpc_of___x1__ __x1__; rpc_of___x2__ __x2__])
          end
        module Remove =
          struct
            let rpc_of___x1__ =
              function | __x689__ -> rpc_of_debug_info __x689__
            and __x1___of_rpc =
              function | __x688__ -> debug_info_of_rpc __x688__
            and rpc_of___x2__ =
              function | __x687__ -> Vgpu.rpc_of_id __x687__
            and __x2___of_rpc =
              function | __x686__ -> Vgpu.id_of_rpc __x686__
            type response = unit
            let rpc_of_response = function | __x691__ -> Rpc.Null
            let response_of_rpc =
              function
              | __x690__ ->
                  (match __x690__ with
                   | Rpc.Null -> ()
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "response" "__x690__" (Rpc.to_string __x__)
                            "Null"
                        else ();
                        raise (Rpc.Runtime_error ("Null", __x__))))
            let call_of_remove =
              function
              | __x1__ ->
                  (function
                   | __x2__ ->
                       Rpc.call "VGPU.remove"
                         [rpc_of___x1__ __x1__; rpc_of___x2__ __x2__])
          end
        module Stat =
          struct
            let rpc_of___x1__ =
              function | __x695__ -> rpc_of_debug_info __x695__
            and __x1___of_rpc =
              function | __x694__ -> debug_info_of_rpc __x694__
            and rpc_of___x2__ =
              function | __x693__ -> Vgpu.rpc_of_id __x693__
            and __x2___of_rpc =
              function | __x692__ -> Vgpu.id_of_rpc __x692__
            type response = (Vgpu.t * Vgpu.state)
            let rpc_of_response =
              function
              | __x699__ ->
                  let (__x700__, __x701__) = __x699__ in
                  Rpc.Enum
                    [Vgpu.rpc_of_t __x700__; Vgpu.rpc_of_state __x701__]
            let response_of_rpc =
              function
              | __x696__ ->
                  (match __x696__ with
                   | Rpc.Enum (__x697__::__x698__::[]) ->
                       ((Vgpu.t_of_rpc __x697__),
                         (Vgpu.state_of_rpc __x698__))
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "response" "__x696__" (Rpc.to_string __x__)
                            "List"
                        else ();
                        raise (Rpc.Runtime_error ("List", __x__))))
            let call_of_stat =
              function
              | __x1__ ->
                  (function
                   | __x2__ ->
                       Rpc.call "VGPU.stat"
                         [rpc_of___x1__ __x1__; rpc_of___x2__ __x2__])
          end
        module List =
          struct
            let rpc_of___x1__ =
              function | __x705__ -> rpc_of_debug_info __x705__
            and __x1___of_rpc =
              function | __x704__ -> debug_info_of_rpc __x704__
            and rpc_of___x2__ = function | __x703__ -> Vm.rpc_of_id __x703__
            and __x2___of_rpc = function | __x702__ -> Vm.id_of_rpc __x702__
            type response = (Vgpu.t * Vgpu.state) list
            let rpc_of_response =
              function
              | __x711__ ->
                  Rpc.Enum
                    (List.map
                       (function
                        | __x712__ ->
                            let (__x713__, __x714__) = __x712__ in
                            Rpc.Enum
                              [Vgpu.rpc_of_t __x713__;
                              Vgpu.rpc_of_state __x714__]) __x711__)
            let response_of_rpc =
              function
              | __x706__ ->
                  (match __x706__ with
                   | Rpc.Enum __x707__ ->
                       List.map
                         (function
                          | __x708__ ->
                              (match __x708__ with
                               | Rpc.Enum (__x709__::__x710__::[]) ->
                                   ((Vgpu.t_of_rpc __x709__),
                                     (Vgpu.state_of_rpc __x710__))
                               | __x__ ->
                                   (if Rpc.get_debug ()
                                    then
                                      Printf.eprintf
                                        "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                        "response" "__x708__"
                                        (Rpc.to_string __x__) "List"
                                    else ();
                                    raise (Rpc.Runtime_error ("List", __x__)))))
                         __x707__
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "response" "__x706__" (Rpc.to_string __x__)
                            "List"
                        else ();
                        raise (Rpc.Runtime_error ("List", __x__))))
            let call_of_list =
              function
              | __x1__ ->
                  (function
                   | __x2__ ->
                       Rpc.call "VGPU.list"
                         [rpc_of___x1__ __x1__; rpc_of___x2__ __x2__])
          end
      end
    module UPDATES =
      struct
        module Get =
          struct
            let rpc_of___x1__ =
              function | __x724__ -> rpc_of_debug_info __x724__
            and __x1___of_rpc =
              function | __x723__ -> debug_info_of_rpc __x723__
            and rpc_of___x2__ =
              function
              | __x721__ ->
                  (match __x721__ with
                   | Some __x722__ ->
                       Rpc.Enum [Rpc.Int (Int64.of_int __x722__)]
                   | None -> Rpc.Enum [])
            and __x2___of_rpc =
              function
              | __x719__ ->
                  (match __x719__ with
                   | Rpc.Enum [] -> None
                   | Rpc.Enum (__x720__::[]) ->
                       Some
                         ((match __x720__ with
                           | Rpc.Int x -> Int64.to_int x
                           | Rpc.String s -> int_of_string s
                           | __x__ ->
                               (if Rpc.get_debug ()
                                then
                                  Printf.eprintf
                                    "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                    "__x2__" "__x720__" (Rpc.to_string __x__)
                                    "Int(int)"
                                else ();
                                raise (Rpc.Runtime_error ("Int(int)", __x__)))))
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "__x2__" "__x719__" (Rpc.to_string __x__)
                            "Enum[]/Enum[_]"
                        else ();
                        raise (Rpc.Runtime_error ("Enum[]/Enum[_]", __x__))))
            and rpc_of___x3__ =
              function
              | __x717__ ->
                  (match __x717__ with
                   | Some __x718__ ->
                       Rpc.Enum [Rpc.Int (Int64.of_int __x718__)]
                   | None -> Rpc.Enum [])
            and __x3___of_rpc =
              function
              | __x715__ ->
                  (match __x715__ with
                   | Rpc.Enum [] -> None
                   | Rpc.Enum (__x716__::[]) ->
                       Some
                         ((match __x716__ with
                           | Rpc.Int x -> Int64.to_int x
                           | Rpc.String s -> int_of_string s
                           | __x__ ->
                               (if Rpc.get_debug ()
                                then
                                  Printf.eprintf
                                    "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                    "__x3__" "__x716__" (Rpc.to_string __x__)
                                    "Int(int)"
                                else ();
                                raise (Rpc.Runtime_error ("Int(int)", __x__)))))
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "__x3__" "__x715__" (Rpc.to_string __x__)
                            "Enum[]/Enum[_]"
                        else ();
                        raise (Rpc.Runtime_error ("Enum[]/Enum[_]", __x__))))
            type response = (Dynamic.barrier list * Dynamic.id list * int)
            let rpc_of_response =
              function
              | __x733__ ->
                  let (__x734__, __x735__, __x736__) = __x733__ in
                  Rpc.Enum
                    [Rpc.Enum
                       (List.map
                          (function
                           | __x737__ -> Dynamic.rpc_of_barrier __x737__)
                          __x734__);
                    Rpc.Enum
                      (List.map
                         (function | __x738__ -> Dynamic.rpc_of_id __x738__)
                         __x735__);
                    Rpc.Int (Int64.of_int __x736__)]
            let response_of_rpc =
              function
              | __x725__ ->
                  (match __x725__ with
                   | Rpc.Enum (__x726__::__x727__::__x728__::[]) ->
                       (((match __x726__ with
                          | Rpc.Enum __x729__ ->
                              List.map
                                (function
                                 | __x730__ ->
                                     Dynamic.barrier_of_rpc __x730__)
                                __x729__
                          | __x__ ->
                              (if Rpc.get_debug ()
                               then
                                 Printf.eprintf
                                   "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                   "response" "__x726__"
                                   (Rpc.to_string __x__) "List"
                               else ();
                               raise (Rpc.Runtime_error ("List", __x__))))),
                         ((match __x727__ with
                           | Rpc.Enum __x731__ ->
                               List.map
                                 (function
                                  | __x732__ -> Dynamic.id_of_rpc __x732__)
                                 __x731__
                           | __x__ ->
                               (if Rpc.get_debug ()
                                then
                                  Printf.eprintf
                                    "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                    "response" "__x727__"
                                    (Rpc.to_string __x__) "List"
                                else ();
                                raise (Rpc.Runtime_error ("List", __x__))))),
                         ((match __x728__ with
                           | Rpc.Int x -> Int64.to_int x
                           | Rpc.String s -> int_of_string s
                           | __x__ ->
                               (if Rpc.get_debug ()
                                then
                                  Printf.eprintf
                                    "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                    "response" "__x728__"
                                    (Rpc.to_string __x__) "Int(int)"
                                else ();
                                raise (Rpc.Runtime_error ("Int(int)", __x__))))))
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "response" "__x725__" (Rpc.to_string __x__)
                            "List"
                        else ();
                        raise (Rpc.Runtime_error ("List", __x__))))
            let call_of_get =
              function
              | __x1__ ->
                  (function
                   | __x2__ ->
                       (function
                        | __x3__ ->
                            Rpc.call "UPDATES.get"
                              [rpc_of___x1__ __x1__;
                              rpc_of___x2__ __x2__;
                              rpc_of___x3__ __x3__]))
          end
        module Last_id =
          struct
            let rpc_of___x1__ =
              function | __x740__ -> rpc_of_debug_info __x740__
            and __x1___of_rpc =
              function | __x739__ -> debug_info_of_rpc __x739__
            type response = int
            let rpc_of_response =
              function | __x742__ -> Rpc.Int (Int64.of_int __x742__)
            let response_of_rpc =
              function
              | __x741__ ->
                  (match __x741__ with
                   | Rpc.Int x -> Int64.to_int x
                   | Rpc.String s -> int_of_string s
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "response" "__x741__" (Rpc.to_string __x__)
                            "Int(int)"
                        else ();
                        raise (Rpc.Runtime_error ("Int(int)", __x__))))
            let call_of_last_id =
              function
              | __x1__ -> Rpc.call "UPDATES.last_id" [rpc_of___x1__ __x1__]
          end
        module Inject_barrier =
          struct
            let rpc_of___x1__ =
              function | __x748__ -> rpc_of_debug_info __x748__
            and __x1___of_rpc =
              function | __x747__ -> debug_info_of_rpc __x747__
            and rpc_of___x2__ = function | __x746__ -> Vm.rpc_of_id __x746__
            and __x2___of_rpc = function | __x745__ -> Vm.id_of_rpc __x745__
            and rpc_of___x3__ =
              function | __x744__ -> Rpc.Int (Int64.of_int __x744__)
            and __x3___of_rpc =
              function
              | __x743__ ->
                  (match __x743__ with
                   | Rpc.Int x -> Int64.to_int x
                   | Rpc.String s -> int_of_string s
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "__x3__" "__x743__" (Rpc.to_string __x__)
                            "Int(int)"
                        else ();
                        raise (Rpc.Runtime_error ("Int(int)", __x__))))
            type response = unit
            let rpc_of_response = function | __x750__ -> Rpc.Null
            let response_of_rpc =
              function
              | __x749__ ->
                  (match __x749__ with
                   | Rpc.Null -> ()
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "response" "__x749__" (Rpc.to_string __x__)
                            "Null"
                        else ();
                        raise (Rpc.Runtime_error ("Null", __x__))))
            let call_of_inject_barrier =
              function
              | __x1__ ->
                  (function
                   | __x2__ ->
                       (function
                        | __x3__ ->
                            Rpc.call "UPDATES.inject_barrier"
                              [rpc_of___x1__ __x1__;
                              rpc_of___x2__ __x2__;
                              rpc_of___x3__ __x3__]))
          end
        module Remove_barrier =
          struct
            let rpc_of___x1__ =
              function | __x754__ -> rpc_of_debug_info __x754__
            and __x1___of_rpc =
              function | __x753__ -> debug_info_of_rpc __x753__
            and rpc_of___x2__ =
              function | __x752__ -> Rpc.Int (Int64.of_int __x752__)
            and __x2___of_rpc =
              function
              | __x751__ ->
                  (match __x751__ with
                   | Rpc.Int x -> Int64.to_int x
                   | Rpc.String s -> int_of_string s
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "__x2__" "__x751__" (Rpc.to_string __x__)
                            "Int(int)"
                        else ();
                        raise (Rpc.Runtime_error ("Int(int)", __x__))))
            type response = unit
            let rpc_of_response = function | __x756__ -> Rpc.Null
            let response_of_rpc =
              function
              | __x755__ ->
                  (match __x755__ with
                   | Rpc.Null -> ()
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "response" "__x755__" (Rpc.to_string __x__)
                            "Null"
                        else ();
                        raise (Rpc.Runtime_error ("Null", __x__))))
            let call_of_remove_barrier =
              function
              | __x1__ ->
                  (function
                   | __x2__ ->
                       Rpc.call "UPDATES.remove_barrier"
                         [rpc_of___x1__ __x1__; rpc_of___x2__ __x2__])
          end
        module Refresh_vm =
          struct
            let rpc_of___x1__ =
              function | __x760__ -> rpc_of_debug_info __x760__
            and __x1___of_rpc =
              function | __x759__ -> debug_info_of_rpc __x759__
            and rpc_of___x2__ = function | __x758__ -> Vm.rpc_of_id __x758__
            and __x2___of_rpc = function | __x757__ -> Vm.id_of_rpc __x757__
            type response = unit
            let rpc_of_response = function | __x762__ -> Rpc.Null
            let response_of_rpc =
              function
              | __x761__ ->
                  (match __x761__ with
                   | Rpc.Null -> ()
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "response" "__x761__" (Rpc.to_string __x__)
                            "Null"
                        else ();
                        raise (Rpc.Runtime_error ("Null", __x__))))
            let call_of_refresh_vm =
              function
              | __x1__ ->
                  (function
                   | __x2__ ->
                       Rpc.call "UPDATES.refresh_vm"
                         [rpc_of___x1__ __x1__; rpc_of___x2__ __x2__])
          end
      end
    module DEBUG =
      struct
        module Trigger =
          struct
            let rpc_of___x1__ =
              function | __x771__ -> rpc_of_debug_info __x771__
            and __x1___of_rpc =
              function | __x770__ -> debug_info_of_rpc __x770__
            and rpc_of___x2__ = function | __x769__ -> Rpc.String __x769__
            and __x2___of_rpc =
              function
              | __x768__ ->
                  (match __x768__ with
                   | Rpc.String x -> x
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "__x2__" "__x768__" (Rpc.to_string __x__)
                            "String(string)"
                        else ();
                        raise (Rpc.Runtime_error ("String(string)", __x__))))
            and rpc_of___x3__ =
              function
              | __x766__ ->
                  Rpc.Enum
                    (List.map (function | __x767__ -> Rpc.String __x767__)
                       __x766__)
            and __x3___of_rpc =
              function
              | __x763__ ->
                  (match __x763__ with
                   | Rpc.Enum __x764__ ->
                       List.map
                         (function
                          | __x765__ ->
                              (match __x765__ with
                               | Rpc.String x -> x
                               | __x__ ->
                                   (if Rpc.get_debug ()
                                    then
                                      Printf.eprintf
                                        "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                        "__x3__" "__x765__"
                                        (Rpc.to_string __x__)
                                        "String(string)"
                                    else ();
                                    raise
                                      (Rpc.Runtime_error
                                         ("String(string)", __x__)))))
                         __x764__
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "__x3__" "__x763__" (Rpc.to_string __x__) "List"
                        else ();
                        raise (Rpc.Runtime_error ("List", __x__))))
            type response = unit
            let rpc_of_response = function | __x773__ -> Rpc.Null
            let response_of_rpc =
              function
              | __x772__ ->
                  (match __x772__ with
                   | Rpc.Null -> ()
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "response" "__x772__" (Rpc.to_string __x__)
                            "Null"
                        else ();
                        raise (Rpc.Runtime_error ("Null", __x__))))
            let call_of_trigger =
              function
              | __x1__ ->
                  (function
                   | __x2__ ->
                       (function
                        | __x3__ ->
                            Rpc.call "DEBUG.trigger"
                              [rpc_of___x1__ __x1__;
                              rpc_of___x2__ __x2__;
                              rpc_of___x3__ __x3__]))
          end
        module Shutdown =
          struct
            let rpc_of___x1__ =
              function | __x777__ -> rpc_of_debug_info __x777__
            and __x1___of_rpc =
              function | __x776__ -> debug_info_of_rpc __x776__
            and rpc_of___x2__ = function | __x775__ -> Rpc.Null
            and __x2___of_rpc =
              function
              | __x774__ ->
                  (match __x774__ with
                   | Rpc.Null -> ()
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "__x2__" "__x774__" (Rpc.to_string __x__) "Null"
                        else ();
                        raise (Rpc.Runtime_error ("Null", __x__))))
            type response = unit
            let rpc_of_response = function | __x779__ -> Rpc.Null
            let response_of_rpc =
              function
              | __x778__ ->
                  (match __x778__ with
                   | Rpc.Null -> ()
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "response" "__x778__" (Rpc.to_string __x__)
                            "Null"
                        else ();
                        raise (Rpc.Runtime_error ("Null", __x__))))
            let call_of_shutdown =
              function
              | __x1__ ->
                  (function
                   | __x2__ ->
                       Rpc.call "DEBUG.shutdown"
                         [rpc_of___x1__ __x1__; rpc_of___x2__ __x2__])
          end
      end
  end
module type RPCM  =
  sig
    type 'a t
    val rpc : Rpc.call -> Rpc.response t
    val bind : 'a t -> ('a -> 'b t) -> 'b t
    val return : 'a -> 'a t
    val fail : exn -> 'b t
  end
module type RPC  = sig val rpc : Rpc.call -> Rpc.response end
module ClientM(R:RPCM) =
  struct
    let query =
      function
      | __x0__ ->
          (function
           | __x1__ ->
               let call = Args.Query.call_of_query __x0__ __x1__ in
               R.bind (R.rpc call)
                 (function
                  | response ->
                      if response.Rpc.success
                      then
                        R.return
                          (Args.Query.response_of_rpc response.Rpc.contents)
                      else
                        (let e =
                           exn_of_exnty
                             (Exception.exnty_of_rpc response.Rpc.contents) in
                         R.fail e)))
    let get_diagnostics =
      function
      | __x0__ ->
          (function
           | __x1__ ->
               let call =
                 Args.Get_diagnostics.call_of_get_diagnostics __x0__ __x1__ in
               R.bind (R.rpc call)
                 (function
                  | response ->
                      if response.Rpc.success
                      then
                        R.return
                          (Args.Get_diagnostics.response_of_rpc
                             response.Rpc.contents)
                      else
                        (let e =
                           exn_of_exnty
                             (Exception.exnty_of_rpc response.Rpc.contents) in
                         R.fail e)))
    module TASK =
      struct
        let stat =
          function
          | __x0__ ->
              (function
               | __x1__ ->
                   let call = Args.TASK.Stat.call_of_stat __x0__ __x1__ in
                   R.bind (R.rpc call)
                     (function
                      | response ->
                          if response.Rpc.success
                          then
                            R.return
                              (Args.TASK.Stat.response_of_rpc
                                 response.Rpc.contents)
                          else
                            (let e =
                               exn_of_exnty
                                 (Exception.exnty_of_rpc
                                    response.Rpc.contents) in
                             R.fail e)))
        let cancel =
          function
          | __x0__ ->
              (function
               | __x1__ ->
                   let call = Args.TASK.Cancel.call_of_cancel __x0__ __x1__ in
                   R.bind (R.rpc call)
                     (function
                      | response ->
                          if response.Rpc.success
                          then
                            R.return
                              (Args.TASK.Cancel.response_of_rpc
                                 response.Rpc.contents)
                          else
                            (let e =
                               exn_of_exnty
                                 (Exception.exnty_of_rpc
                                    response.Rpc.contents) in
                             R.fail e)))
        let destroy =
          function
          | __x0__ ->
              (function
               | __x1__ ->
                   let call = Args.TASK.Destroy.call_of_destroy __x0__ __x1__ in
                   R.bind (R.rpc call)
                     (function
                      | response ->
                          if response.Rpc.success
                          then
                            R.return
                              (Args.TASK.Destroy.response_of_rpc
                                 response.Rpc.contents)
                          else
                            (let e =
                               exn_of_exnty
                                 (Exception.exnty_of_rpc
                                    response.Rpc.contents) in
                             R.fail e)))
        let list =
          function
          | __x0__ ->
              let call = Args.TASK.List.call_of_list __x0__ in
              R.bind (R.rpc call)
                (function
                 | response ->
                     if response.Rpc.success
                     then
                       R.return
                         (Args.TASK.List.response_of_rpc
                            response.Rpc.contents)
                     else
                       (let e =
                          exn_of_exnty
                            (Exception.exnty_of_rpc response.Rpc.contents) in
                        R.fail e))
      end
    module HOST =
      struct
        let stat =
          function
          | __x0__ ->
              let call = Args.HOST.Stat.call_of_stat __x0__ in
              R.bind (R.rpc call)
                (function
                 | response ->
                     if response.Rpc.success
                     then
                       R.return
                         (Args.HOST.Stat.response_of_rpc
                            response.Rpc.contents)
                     else
                       (let e =
                          exn_of_exnty
                            (Exception.exnty_of_rpc response.Rpc.contents) in
                        R.fail e))
        let get_console_data =
          function
          | __x0__ ->
              let call =
                Args.HOST.Get_console_data.call_of_get_console_data __x0__ in
              R.bind (R.rpc call)
                (function
                 | response ->
                     if response.Rpc.success
                     then
                       R.return
                         (Args.HOST.Get_console_data.response_of_rpc
                            response.Rpc.contents)
                     else
                       (let e =
                          exn_of_exnty
                            (Exception.exnty_of_rpc response.Rpc.contents) in
                        R.fail e))
        let get_total_memory_mib =
          function
          | __x0__ ->
              let call =
                Args.HOST.Get_total_memory_mib.call_of_get_total_memory_mib
                  __x0__ in
              R.bind (R.rpc call)
                (function
                 | response ->
                     if response.Rpc.success
                     then
                       R.return
                         (Args.HOST.Get_total_memory_mib.response_of_rpc
                            response.Rpc.contents)
                     else
                       (let e =
                          exn_of_exnty
                            (Exception.exnty_of_rpc response.Rpc.contents) in
                        R.fail e))
        let send_debug_keys =
          function
          | __x0__ ->
              (function
               | __x1__ ->
                   let call =
                     Args.HOST.Send_debug_keys.call_of_send_debug_keys __x0__
                       __x1__ in
                   R.bind (R.rpc call)
                     (function
                      | response ->
                          if response.Rpc.success
                          then
                            R.return
                              (Args.HOST.Send_debug_keys.response_of_rpc
                                 response.Rpc.contents)
                          else
                            (let e =
                               exn_of_exnty
                                 (Exception.exnty_of_rpc
                                    response.Rpc.contents) in
                             R.fail e)))
        let set_worker_pool_size =
          function
          | __x0__ ->
              (function
               | __x1__ ->
                   let call =
                     Args.HOST.Set_worker_pool_size.call_of_set_worker_pool_size
                       __x0__ __x1__ in
                   R.bind (R.rpc call)
                     (function
                      | response ->
                          if response.Rpc.success
                          then
                            R.return
                              (Args.HOST.Set_worker_pool_size.response_of_rpc
                                 response.Rpc.contents)
                          else
                            (let e =
                               exn_of_exnty
                                 (Exception.exnty_of_rpc
                                    response.Rpc.contents) in
                             R.fail e)))
        let update_guest_agent_features =
          function
          | __x0__ ->
              (function
               | __x1__ ->
                   let call =
                     Args.HOST.Update_guest_agent_features.call_of_update_guest_agent_features
                       __x0__ __x1__ in
                   R.bind (R.rpc call)
                     (function
                      | response ->
                          if response.Rpc.success
                          then
                            R.return
                              (Args.HOST.Update_guest_agent_features.response_of_rpc
                                 response.Rpc.contents)
                          else
                            (let e =
                               exn_of_exnty
                                 (Exception.exnty_of_rpc
                                    response.Rpc.contents) in
                             R.fail e)))
        let upgrade_cpu_features =
          function
          | __x0__ ->
              (function
               | __x1__ ->
                   (function
                    | __x2__ ->
                        let call =
                          Args.HOST.Upgrade_cpu_features.call_of_upgrade_cpu_features
                            __x0__ __x1__ __x2__ in
                        R.bind (R.rpc call)
                          (function
                           | response ->
                               if response.Rpc.success
                               then
                                 R.return
                                   (Args.HOST.Upgrade_cpu_features.response_of_rpc
                                      response.Rpc.contents)
                               else
                                 (let e =
                                    exn_of_exnty
                                      (Exception.exnty_of_rpc
                                         response.Rpc.contents) in
                                  R.fail e))))
      end
    module VM =
      struct
        let add =
          function
          | __x0__ ->
              (function
               | __x1__ ->
                   let call = Args.VM.Add.call_of_add __x0__ __x1__ in
                   R.bind (R.rpc call)
                     (function
                      | response ->
                          if response.Rpc.success
                          then
                            R.return
                              (Args.VM.Add.response_of_rpc
                                 response.Rpc.contents)
                          else
                            (let e =
                               exn_of_exnty
                                 (Exception.exnty_of_rpc
                                    response.Rpc.contents) in
                             R.fail e)))
        let remove =
          function
          | __x0__ ->
              (function
               | __x1__ ->
                   let call = Args.VM.Remove.call_of_remove __x0__ __x1__ in
                   R.bind (R.rpc call)
                     (function
                      | response ->
                          if response.Rpc.success
                          then
                            R.return
                              (Args.VM.Remove.response_of_rpc
                                 response.Rpc.contents)
                          else
                            (let e =
                               exn_of_exnty
                                 (Exception.exnty_of_rpc
                                    response.Rpc.contents) in
                             R.fail e)))
        let generate_state_string =
          function
          | __x0__ ->
              (function
               | __x1__ ->
                   let call =
                     Args.VM.Generate_state_string.call_of_generate_state_string
                       __x0__ __x1__ in
                   R.bind (R.rpc call)
                     (function
                      | response ->
                          if response.Rpc.success
                          then
                            R.return
                              (Args.VM.Generate_state_string.response_of_rpc
                                 response.Rpc.contents)
                          else
                            (let e =
                               exn_of_exnty
                                 (Exception.exnty_of_rpc
                                    response.Rpc.contents) in
                             R.fail e)))
        let migrate =
          function
          | __x0__ ->
              (function
               | __x1__ ->
                   (function
                    | __x2__ ->
                        (function
                         | __x3__ ->
                             (function
                              | __x4__ ->
                                  (function
                                   | __x5__ ->
                                       let call =
                                         Args.VM.Migrate.call_of_migrate
                                           __x0__ __x1__ __x2__ __x3__ __x4__
                                           __x5__ in
                                       R.bind (R.rpc call)
                                         (function
                                          | response ->
                                              if response.Rpc.success
                                              then
                                                R.return
                                                  (Args.VM.Migrate.response_of_rpc
                                                     response.Rpc.contents)
                                              else
                                                (let e =
                                                   exn_of_exnty
                                                     (Exception.exnty_of_rpc
                                                        response.Rpc.contents) in
                                                 R.fail e)))))))
        let migrate_receive_memory =
          function
          | __x0__ ->
              (function
               | __x1__ ->
                   (function
                    | __x2__ ->
                        (function
                         | __x3__ ->
                             (function
                              | __x4__ ->
                                  let call =
                                    Args.VM.Migrate_receive_memory.call_of_migrate_receive_memory
                                      __x0__ __x1__ __x2__ __x3__ __x4__ in
                                  R.bind (R.rpc call)
                                    (function
                                     | response ->
                                         if response.Rpc.success
                                         then
                                           R.return
                                             (Args.VM.Migrate_receive_memory.response_of_rpc
                                                response.Rpc.contents)
                                         else
                                           (let e =
                                              exn_of_exnty
                                                (Exception.exnty_of_rpc
                                                   response.Rpc.contents) in
                                            R.fail e))))))
        let create =
          function
          | __x0__ ->
              (function
               | __x1__ ->
                   let call = Args.VM.Create.call_of_create __x0__ __x1__ in
                   R.bind (R.rpc call)
                     (function
                      | response ->
                          if response.Rpc.success
                          then
                            R.return
                              (Args.VM.Create.response_of_rpc
                                 response.Rpc.contents)
                          else
                            (let e =
                               exn_of_exnty
                                 (Exception.exnty_of_rpc
                                    response.Rpc.contents) in
                             R.fail e)))
        let build =
          function
          | __x0__ ->
              (function
               | __x1__ ->
                   (function
                    | __x2__ ->
                        let call =
                          Args.VM.Build.call_of_build __x0__ __x1__ __x2__ in
                        R.bind (R.rpc call)
                          (function
                           | response ->
                               if response.Rpc.success
                               then
                                 R.return
                                   (Args.VM.Build.response_of_rpc
                                      response.Rpc.contents)
                               else
                                 (let e =
                                    exn_of_exnty
                                      (Exception.exnty_of_rpc
                                         response.Rpc.contents) in
                                  R.fail e))))
        let create_device_model =
          function
          | __x0__ ->
              (function
               | __x1__ ->
                   (function
                    | __x2__ ->
                        let call =
                          Args.VM.Create_device_model.call_of_create_device_model
                            __x0__ __x1__ __x2__ in
                        R.bind (R.rpc call)
                          (function
                           | response ->
                               if response.Rpc.success
                               then
                                 R.return
                                   (Args.VM.Create_device_model.response_of_rpc
                                      response.Rpc.contents)
                               else
                                 (let e =
                                    exn_of_exnty
                                      (Exception.exnty_of_rpc
                                         response.Rpc.contents) in
                                  R.fail e))))
        let destroy =
          function
          | __x0__ ->
              (function
               | __x1__ ->
                   let call = Args.VM.Destroy.call_of_destroy __x0__ __x1__ in
                   R.bind (R.rpc call)
                     (function
                      | response ->
                          if response.Rpc.success
                          then
                            R.return
                              (Args.VM.Destroy.response_of_rpc
                                 response.Rpc.contents)
                          else
                            (let e =
                               exn_of_exnty
                                 (Exception.exnty_of_rpc
                                    response.Rpc.contents) in
                             R.fail e)))
        let pause =
          function
          | __x0__ ->
              (function
               | __x1__ ->
                   let call = Args.VM.Pause.call_of_pause __x0__ __x1__ in
                   R.bind (R.rpc call)
                     (function
                      | response ->
                          if response.Rpc.success
                          then
                            R.return
                              (Args.VM.Pause.response_of_rpc
                                 response.Rpc.contents)
                          else
                            (let e =
                               exn_of_exnty
                                 (Exception.exnty_of_rpc
                                    response.Rpc.contents) in
                             R.fail e)))
        let unpause =
          function
          | __x0__ ->
              (function
               | __x1__ ->
                   let call = Args.VM.Unpause.call_of_unpause __x0__ __x1__ in
                   R.bind (R.rpc call)
                     (function
                      | response ->
                          if response.Rpc.success
                          then
                            R.return
                              (Args.VM.Unpause.response_of_rpc
                                 response.Rpc.contents)
                          else
                            (let e =
                               exn_of_exnty
                                 (Exception.exnty_of_rpc
                                    response.Rpc.contents) in
                             R.fail e)))
        let request_rdp =
          function
          | __x0__ ->
              (function
               | __x1__ ->
                   (function
                    | __x2__ ->
                        let call =
                          Args.VM.Request_rdp.call_of_request_rdp __x0__
                            __x1__ __x2__ in
                        R.bind (R.rpc call)
                          (function
                           | response ->
                               if response.Rpc.success
                               then
                                 R.return
                                   (Args.VM.Request_rdp.response_of_rpc
                                      response.Rpc.contents)
                               else
                                 (let e =
                                    exn_of_exnty
                                      (Exception.exnty_of_rpc
                                         response.Rpc.contents) in
                                  R.fail e))))
        let run_script =
          function
          | __x0__ ->
              (function
               | __x1__ ->
                   (function
                    | __x2__ ->
                        let call =
                          Args.VM.Run_script.call_of_run_script __x0__ __x1__
                            __x2__ in
                        R.bind (R.rpc call)
                          (function
                           | response ->
                               if response.Rpc.success
                               then
                                 R.return
                                   (Args.VM.Run_script.response_of_rpc
                                      response.Rpc.contents)
                               else
                                 (let e =
                                    exn_of_exnty
                                      (Exception.exnty_of_rpc
                                         response.Rpc.contents) in
                                  R.fail e))))
        let set_xsdata =
          function
          | __x0__ ->
              (function
               | __x1__ ->
                   (function
                    | __x2__ ->
                        let call =
                          Args.VM.Set_xsdata.call_of_set_xsdata __x0__ __x1__
                            __x2__ in
                        R.bind (R.rpc call)
                          (function
                           | response ->
                               if response.Rpc.success
                               then
                                 R.return
                                   (Args.VM.Set_xsdata.response_of_rpc
                                      response.Rpc.contents)
                               else
                                 (let e =
                                    exn_of_exnty
                                      (Exception.exnty_of_rpc
                                         response.Rpc.contents) in
                                  R.fail e))))
        let set_vcpus =
          function
          | __x0__ ->
              (function
               | __x1__ ->
                   (function
                    | __x2__ ->
                        let call =
                          Args.VM.Set_vcpus.call_of_set_vcpus __x0__ __x1__
                            __x2__ in
                        R.bind (R.rpc call)
                          (function
                           | response ->
                               if response.Rpc.success
                               then
                                 R.return
                                   (Args.VM.Set_vcpus.response_of_rpc
                                      response.Rpc.contents)
                               else
                                 (let e =
                                    exn_of_exnty
                                      (Exception.exnty_of_rpc
                                         response.Rpc.contents) in
                                  R.fail e))))
        let set_shadow_multiplier =
          function
          | __x0__ ->
              (function
               | __x1__ ->
                   (function
                    | __x2__ ->
                        let call =
                          Args.VM.Set_shadow_multiplier.call_of_set_shadow_multiplier
                            __x0__ __x1__ __x2__ in
                        R.bind (R.rpc call)
                          (function
                           | response ->
                               if response.Rpc.success
                               then
                                 R.return
                                   (Args.VM.Set_shadow_multiplier.response_of_rpc
                                      response.Rpc.contents)
                               else
                                 (let e =
                                    exn_of_exnty
                                      (Exception.exnty_of_rpc
                                         response.Rpc.contents) in
                                  R.fail e))))
        let set_memory_dynamic_range =
          function
          | __x0__ ->
              (function
               | __x1__ ->
                   (function
                    | __x2__ ->
                        (function
                         | __x3__ ->
                             let call =
                               Args.VM.Set_memory_dynamic_range.call_of_set_memory_dynamic_range
                                 __x0__ __x1__ __x2__ __x3__ in
                             R.bind (R.rpc call)
                               (function
                                | response ->
                                    if response.Rpc.success
                                    then
                                      R.return
                                        (Args.VM.Set_memory_dynamic_range.response_of_rpc
                                           response.Rpc.contents)
                                    else
                                      (let e =
                                         exn_of_exnty
                                           (Exception.exnty_of_rpc
                                              response.Rpc.contents) in
                                       R.fail e)))))
        let stat =
          function
          | __x0__ ->
              (function
               | __x1__ ->
                   let call = Args.VM.Stat.call_of_stat __x0__ __x1__ in
                   R.bind (R.rpc call)
                     (function
                      | response ->
                          if response.Rpc.success
                          then
                            R.return
                              (Args.VM.Stat.response_of_rpc
                                 response.Rpc.contents)
                          else
                            (let e =
                               exn_of_exnty
                                 (Exception.exnty_of_rpc
                                    response.Rpc.contents) in
                             R.fail e)))
        let exists =
          function
          | __x0__ ->
              (function
               | __x1__ ->
                   let call = Args.VM.Exists.call_of_exists __x0__ __x1__ in
                   R.bind (R.rpc call)
                     (function
                      | response ->
                          if response.Rpc.success
                          then
                            R.return
                              (Args.VM.Exists.response_of_rpc
                                 response.Rpc.contents)
                          else
                            (let e =
                               exn_of_exnty
                                 (Exception.exnty_of_rpc
                                    response.Rpc.contents) in
                             R.fail e)))
        let list =
          function
          | __x0__ ->
              (function
               | __x1__ ->
                   let call = Args.VM.List.call_of_list __x0__ __x1__ in
                   R.bind (R.rpc call)
                     (function
                      | response ->
                          if response.Rpc.success
                          then
                            R.return
                              (Args.VM.List.response_of_rpc
                                 response.Rpc.contents)
                          else
                            (let e =
                               exn_of_exnty
                                 (Exception.exnty_of_rpc
                                    response.Rpc.contents) in
                             R.fail e)))
        let delay =
          function
          | __x0__ ->
              (function
               | __x1__ ->
                   (function
                    | __x2__ ->
                        let call =
                          Args.VM.Delay.call_of_delay __x0__ __x1__ __x2__ in
                        R.bind (R.rpc call)
                          (function
                           | response ->
                               if response.Rpc.success
                               then
                                 R.return
                                   (Args.VM.Delay.response_of_rpc
                                      response.Rpc.contents)
                               else
                                 (let e =
                                    exn_of_exnty
                                      (Exception.exnty_of_rpc
                                         response.Rpc.contents) in
                                  R.fail e))))
        let start =
          function
          | __x0__ ->
              (function
               | __x1__ ->
                   (function
                    | __x2__ ->
                        let call =
                          Args.VM.Start.call_of_start __x0__ __x1__ __x2__ in
                        R.bind (R.rpc call)
                          (function
                           | response ->
                               if response.Rpc.success
                               then
                                 R.return
                                   (Args.VM.Start.response_of_rpc
                                      response.Rpc.contents)
                               else
                                 (let e =
                                    exn_of_exnty
                                      (Exception.exnty_of_rpc
                                         response.Rpc.contents) in
                                  R.fail e))))
        let shutdown =
          function
          | __x0__ ->
              (function
               | __x1__ ->
                   (function
                    | __x2__ ->
                        let call =
                          Args.VM.Shutdown.call_of_shutdown __x0__ __x1__
                            __x2__ in
                        R.bind (R.rpc call)
                          (function
                           | response ->
                               if response.Rpc.success
                               then
                                 R.return
                                   (Args.VM.Shutdown.response_of_rpc
                                      response.Rpc.contents)
                               else
                                 (let e =
                                    exn_of_exnty
                                      (Exception.exnty_of_rpc
                                         response.Rpc.contents) in
                                  R.fail e))))
        let reboot =
          function
          | __x0__ ->
              (function
               | __x1__ ->
                   (function
                    | __x2__ ->
                        let call =
                          Args.VM.Reboot.call_of_reboot __x0__ __x1__ __x2__ in
                        R.bind (R.rpc call)
                          (function
                           | response ->
                               if response.Rpc.success
                               then
                                 R.return
                                   (Args.VM.Reboot.response_of_rpc
                                      response.Rpc.contents)
                               else
                                 (let e =
                                    exn_of_exnty
                                      (Exception.exnty_of_rpc
                                         response.Rpc.contents) in
                                  R.fail e))))
        let suspend =
          function
          | __x0__ ->
              (function
               | __x1__ ->
                   (function
                    | __x2__ ->
                        let call =
                          Args.VM.Suspend.call_of_suspend __x0__ __x1__
                            __x2__ in
                        R.bind (R.rpc call)
                          (function
                           | response ->
                               if response.Rpc.success
                               then
                                 R.return
                                   (Args.VM.Suspend.response_of_rpc
                                      response.Rpc.contents)
                               else
                                 (let e =
                                    exn_of_exnty
                                      (Exception.exnty_of_rpc
                                         response.Rpc.contents) in
                                  R.fail e))))
        let resume =
          function
          | __x0__ ->
              (function
               | __x1__ ->
                   (function
                    | __x2__ ->
                        let call =
                          Args.VM.Resume.call_of_resume __x0__ __x1__ __x2__ in
                        R.bind (R.rpc call)
                          (function
                           | response ->
                               if response.Rpc.success
                               then
                                 R.return
                                   (Args.VM.Resume.response_of_rpc
                                      response.Rpc.contents)
                               else
                                 (let e =
                                    exn_of_exnty
                                      (Exception.exnty_of_rpc
                                         response.Rpc.contents) in
                                  R.fail e))))
        let s3suspend =
          function
          | __x0__ ->
              (function
               | __x1__ ->
                   let call =
                     Args.VM.S3suspend.call_of_s3suspend __x0__ __x1__ in
                   R.bind (R.rpc call)
                     (function
                      | response ->
                          if response.Rpc.success
                          then
                            R.return
                              (Args.VM.S3suspend.response_of_rpc
                                 response.Rpc.contents)
                          else
                            (let e =
                               exn_of_exnty
                                 (Exception.exnty_of_rpc
                                    response.Rpc.contents) in
                             R.fail e)))
        let s3resume =
          function
          | __x0__ ->
              (function
               | __x1__ ->
                   let call = Args.VM.S3resume.call_of_s3resume __x0__ __x1__ in
                   R.bind (R.rpc call)
                     (function
                      | response ->
                          if response.Rpc.success
                          then
                            R.return
                              (Args.VM.S3resume.response_of_rpc
                                 response.Rpc.contents)
                          else
                            (let e =
                               exn_of_exnty
                                 (Exception.exnty_of_rpc
                                    response.Rpc.contents) in
                             R.fail e)))
        let export_metadata =
          function
          | __x0__ ->
              (function
               | __x1__ ->
                   let call =
                     Args.VM.Export_metadata.call_of_export_metadata __x0__
                       __x1__ in
                   R.bind (R.rpc call)
                     (function
                      | response ->
                          if response.Rpc.success
                          then
                            R.return
                              (Args.VM.Export_metadata.response_of_rpc
                                 response.Rpc.contents)
                          else
                            (let e =
                               exn_of_exnty
                                 (Exception.exnty_of_rpc
                                    response.Rpc.contents) in
                             R.fail e)))
        let import_metadata =
          function
          | __x0__ ->
              (function
               | __x1__ ->
                   let call =
                     Args.VM.Import_metadata.call_of_import_metadata __x0__
                       __x1__ in
                   R.bind (R.rpc call)
                     (function
                      | response ->
                          if response.Rpc.success
                          then
                            R.return
                              (Args.VM.Import_metadata.response_of_rpc
                                 response.Rpc.contents)
                          else
                            (let e =
                               exn_of_exnty
                                 (Exception.exnty_of_rpc
                                    response.Rpc.contents) in
                             R.fail e)))
      end
    module PCI =
      struct
        let add =
          function
          | __x0__ ->
              (function
               | __x1__ ->
                   let call = Args.PCI.Add.call_of_add __x0__ __x1__ in
                   R.bind (R.rpc call)
                     (function
                      | response ->
                          if response.Rpc.success
                          then
                            R.return
                              (Args.PCI.Add.response_of_rpc
                                 response.Rpc.contents)
                          else
                            (let e =
                               exn_of_exnty
                                 (Exception.exnty_of_rpc
                                    response.Rpc.contents) in
                             R.fail e)))
        let remove =
          function
          | __x0__ ->
              (function
               | __x1__ ->
                   let call = Args.PCI.Remove.call_of_remove __x0__ __x1__ in
                   R.bind (R.rpc call)
                     (function
                      | response ->
                          if response.Rpc.success
                          then
                            R.return
                              (Args.PCI.Remove.response_of_rpc
                                 response.Rpc.contents)
                          else
                            (let e =
                               exn_of_exnty
                                 (Exception.exnty_of_rpc
                                    response.Rpc.contents) in
                             R.fail e)))
        let stat =
          function
          | __x0__ ->
              (function
               | __x1__ ->
                   let call = Args.PCI.Stat.call_of_stat __x0__ __x1__ in
                   R.bind (R.rpc call)
                     (function
                      | response ->
                          if response.Rpc.success
                          then
                            R.return
                              (Args.PCI.Stat.response_of_rpc
                                 response.Rpc.contents)
                          else
                            (let e =
                               exn_of_exnty
                                 (Exception.exnty_of_rpc
                                    response.Rpc.contents) in
                             R.fail e)))
        let list =
          function
          | __x0__ ->
              (function
               | __x1__ ->
                   let call = Args.PCI.List.call_of_list __x0__ __x1__ in
                   R.bind (R.rpc call)
                     (function
                      | response ->
                          if response.Rpc.success
                          then
                            R.return
                              (Args.PCI.List.response_of_rpc
                                 response.Rpc.contents)
                          else
                            (let e =
                               exn_of_exnty
                                 (Exception.exnty_of_rpc
                                    response.Rpc.contents) in
                             R.fail e)))
      end
    module VBD =
      struct
        let add =
          function
          | __x0__ ->
              (function
               | __x1__ ->
                   let call = Args.VBD.Add.call_of_add __x0__ __x1__ in
                   R.bind (R.rpc call)
                     (function
                      | response ->
                          if response.Rpc.success
                          then
                            R.return
                              (Args.VBD.Add.response_of_rpc
                                 response.Rpc.contents)
                          else
                            (let e =
                               exn_of_exnty
                                 (Exception.exnty_of_rpc
                                    response.Rpc.contents) in
                             R.fail e)))
        let plug =
          function
          | __x0__ ->
              (function
               | __x1__ ->
                   let call = Args.VBD.Plug.call_of_plug __x0__ __x1__ in
                   R.bind (R.rpc call)
                     (function
                      | response ->
                          if response.Rpc.success
                          then
                            R.return
                              (Args.VBD.Plug.response_of_rpc
                                 response.Rpc.contents)
                          else
                            (let e =
                               exn_of_exnty
                                 (Exception.exnty_of_rpc
                                    response.Rpc.contents) in
                             R.fail e)))
        let unplug =
          function
          | __x0__ ->
              (function
               | __x1__ ->
                   (function
                    | __x2__ ->
                        let call =
                          Args.VBD.Unplug.call_of_unplug __x0__ __x1__ __x2__ in
                        R.bind (R.rpc call)
                          (function
                           | response ->
                               if response.Rpc.success
                               then
                                 R.return
                                   (Args.VBD.Unplug.response_of_rpc
                                      response.Rpc.contents)
                               else
                                 (let e =
                                    exn_of_exnty
                                      (Exception.exnty_of_rpc
                                         response.Rpc.contents) in
                                  R.fail e))))
        let eject =
          function
          | __x0__ ->
              (function
               | __x1__ ->
                   let call = Args.VBD.Eject.call_of_eject __x0__ __x1__ in
                   R.bind (R.rpc call)
                     (function
                      | response ->
                          if response.Rpc.success
                          then
                            R.return
                              (Args.VBD.Eject.response_of_rpc
                                 response.Rpc.contents)
                          else
                            (let e =
                               exn_of_exnty
                                 (Exception.exnty_of_rpc
                                    response.Rpc.contents) in
                             R.fail e)))
        let insert =
          function
          | __x0__ ->
              (function
               | __x1__ ->
                   (function
                    | __x2__ ->
                        let call =
                          Args.VBD.Insert.call_of_insert __x0__ __x1__ __x2__ in
                        R.bind (R.rpc call)
                          (function
                           | response ->
                               if response.Rpc.success
                               then
                                 R.return
                                   (Args.VBD.Insert.response_of_rpc
                                      response.Rpc.contents)
                               else
                                 (let e =
                                    exn_of_exnty
                                      (Exception.exnty_of_rpc
                                         response.Rpc.contents) in
                                  R.fail e))))
        let stat =
          function
          | __x0__ ->
              (function
               | __x1__ ->
                   let call = Args.VBD.Stat.call_of_stat __x0__ __x1__ in
                   R.bind (R.rpc call)
                     (function
                      | response ->
                          if response.Rpc.success
                          then
                            R.return
                              (Args.VBD.Stat.response_of_rpc
                                 response.Rpc.contents)
                          else
                            (let e =
                               exn_of_exnty
                                 (Exception.exnty_of_rpc
                                    response.Rpc.contents) in
                             R.fail e)))
        let list =
          function
          | __x0__ ->
              (function
               | __x1__ ->
                   let call = Args.VBD.List.call_of_list __x0__ __x1__ in
                   R.bind (R.rpc call)
                     (function
                      | response ->
                          if response.Rpc.success
                          then
                            R.return
                              (Args.VBD.List.response_of_rpc
                                 response.Rpc.contents)
                          else
                            (let e =
                               exn_of_exnty
                                 (Exception.exnty_of_rpc
                                    response.Rpc.contents) in
                             R.fail e)))
        let remove =
          function
          | __x0__ ->
              (function
               | __x1__ ->
                   let call = Args.VBD.Remove.call_of_remove __x0__ __x1__ in
                   R.bind (R.rpc call)
                     (function
                      | response ->
                          if response.Rpc.success
                          then
                            R.return
                              (Args.VBD.Remove.response_of_rpc
                                 response.Rpc.contents)
                          else
                            (let e =
                               exn_of_exnty
                                 (Exception.exnty_of_rpc
                                    response.Rpc.contents) in
                             R.fail e)))
      end
    module VUSB =
      struct
        let add =
          function
          | __x0__ ->
              (function
               | __x1__ ->
                   let call = Args.VUSB.Add.call_of_add __x0__ __x1__ in
                   R.bind (R.rpc call)
                     (function
                      | response ->
                          if response.Rpc.success
                          then
                            R.return
                              (Args.VUSB.Add.response_of_rpc
                                 response.Rpc.contents)
                          else
                            (let e =
                               exn_of_exnty
                                 (Exception.exnty_of_rpc
                                    response.Rpc.contents) in
                             R.fail e)))
        let plug =
          function
          | __x0__ ->
              (function
               | __x1__ ->
                   let call = Args.VUSB.Plug.call_of_plug __x0__ __x1__ in
                   R.bind (R.rpc call)
                     (function
                      | response ->
                          if response.Rpc.success
                          then
                            R.return
                              (Args.VUSB.Plug.response_of_rpc
                                 response.Rpc.contents)
                          else
                            (let e =
                               exn_of_exnty
                                 (Exception.exnty_of_rpc
                                    response.Rpc.contents) in
                             R.fail e)))
        let unplug =
          function
          | __x0__ ->
              (function
               | __x1__ ->
                   let call = Args.VUSB.Unplug.call_of_unplug __x0__ __x1__ in
                   R.bind (R.rpc call)
                     (function
                      | response ->
                          if response.Rpc.success
                          then
                            R.return
                              (Args.VUSB.Unplug.response_of_rpc
                                 response.Rpc.contents)
                          else
                            (let e =
                               exn_of_exnty
                                 (Exception.exnty_of_rpc
                                    response.Rpc.contents) in
                             R.fail e)))
        let stat =
          function
          | __x0__ ->
              (function
               | __x1__ ->
                   let call = Args.VUSB.Stat.call_of_stat __x0__ __x1__ in
                   R.bind (R.rpc call)
                     (function
                      | response ->
                          if response.Rpc.success
                          then
                            R.return
                              (Args.VUSB.Stat.response_of_rpc
                                 response.Rpc.contents)
                          else
                            (let e =
                               exn_of_exnty
                                 (Exception.exnty_of_rpc
                                    response.Rpc.contents) in
                             R.fail e)))
        let list =
          function
          | __x0__ ->
              (function
               | __x1__ ->
                   let call = Args.VUSB.List.call_of_list __x0__ __x1__ in
                   R.bind (R.rpc call)
                     (function
                      | response ->
                          if response.Rpc.success
                          then
                            R.return
                              (Args.VUSB.List.response_of_rpc
                                 response.Rpc.contents)
                          else
                            (let e =
                               exn_of_exnty
                                 (Exception.exnty_of_rpc
                                    response.Rpc.contents) in
                             R.fail e)))
        let remove =
          function
          | __x0__ ->
              (function
               | __x1__ ->
                   let call = Args.VUSB.Remove.call_of_remove __x0__ __x1__ in
                   R.bind (R.rpc call)
                     (function
                      | response ->
                          if response.Rpc.success
                          then
                            R.return
                              (Args.VUSB.Remove.response_of_rpc
                                 response.Rpc.contents)
                          else
                            (let e =
                               exn_of_exnty
                                 (Exception.exnty_of_rpc
                                    response.Rpc.contents) in
                             R.fail e)))
      end
    module VIF =
      struct
        let add =
          function
          | __x0__ ->
              (function
               | __x1__ ->
                   let call = Args.VIF.Add.call_of_add __x0__ __x1__ in
                   R.bind (R.rpc call)
                     (function
                      | response ->
                          if response.Rpc.success
                          then
                            R.return
                              (Args.VIF.Add.response_of_rpc
                                 response.Rpc.contents)
                          else
                            (let e =
                               exn_of_exnty
                                 (Exception.exnty_of_rpc
                                    response.Rpc.contents) in
                             R.fail e)))
        let plug =
          function
          | __x0__ ->
              (function
               | __x1__ ->
                   let call = Args.VIF.Plug.call_of_plug __x0__ __x1__ in
                   R.bind (R.rpc call)
                     (function
                      | response ->
                          if response.Rpc.success
                          then
                            R.return
                              (Args.VIF.Plug.response_of_rpc
                                 response.Rpc.contents)
                          else
                            (let e =
                               exn_of_exnty
                                 (Exception.exnty_of_rpc
                                    response.Rpc.contents) in
                             R.fail e)))
        let unplug =
          function
          | __x0__ ->
              (function
               | __x1__ ->
                   (function
                    | __x2__ ->
                        let call =
                          Args.VIF.Unplug.call_of_unplug __x0__ __x1__ __x2__ in
                        R.bind (R.rpc call)
                          (function
                           | response ->
                               if response.Rpc.success
                               then
                                 R.return
                                   (Args.VIF.Unplug.response_of_rpc
                                      response.Rpc.contents)
                               else
                                 (let e =
                                    exn_of_exnty
                                      (Exception.exnty_of_rpc
                                         response.Rpc.contents) in
                                  R.fail e))))
        let move =
          function
          | __x0__ ->
              (function
               | __x1__ ->
                   (function
                    | __x2__ ->
                        let call =
                          Args.VIF.Move.call_of_move __x0__ __x1__ __x2__ in
                        R.bind (R.rpc call)
                          (function
                           | response ->
                               if response.Rpc.success
                               then
                                 R.return
                                   (Args.VIF.Move.response_of_rpc
                                      response.Rpc.contents)
                               else
                                 (let e =
                                    exn_of_exnty
                                      (Exception.exnty_of_rpc
                                         response.Rpc.contents) in
                                  R.fail e))))
        let stat =
          function
          | __x0__ ->
              (function
               | __x1__ ->
                   let call = Args.VIF.Stat.call_of_stat __x0__ __x1__ in
                   R.bind (R.rpc call)
                     (function
                      | response ->
                          if response.Rpc.success
                          then
                            R.return
                              (Args.VIF.Stat.response_of_rpc
                                 response.Rpc.contents)
                          else
                            (let e =
                               exn_of_exnty
                                 (Exception.exnty_of_rpc
                                    response.Rpc.contents) in
                             R.fail e)))
        let list =
          function
          | __x0__ ->
              (function
               | __x1__ ->
                   let call = Args.VIF.List.call_of_list __x0__ __x1__ in
                   R.bind (R.rpc call)
                     (function
                      | response ->
                          if response.Rpc.success
                          then
                            R.return
                              (Args.VIF.List.response_of_rpc
                                 response.Rpc.contents)
                          else
                            (let e =
                               exn_of_exnty
                                 (Exception.exnty_of_rpc
                                    response.Rpc.contents) in
                             R.fail e)))
        let remove =
          function
          | __x0__ ->
              (function
               | __x1__ ->
                   let call = Args.VIF.Remove.call_of_remove __x0__ __x1__ in
                   R.bind (R.rpc call)
                     (function
                      | response ->
                          if response.Rpc.success
                          then
                            R.return
                              (Args.VIF.Remove.response_of_rpc
                                 response.Rpc.contents)
                          else
                            (let e =
                               exn_of_exnty
                                 (Exception.exnty_of_rpc
                                    response.Rpc.contents) in
                             R.fail e)))
        let set_carrier =
          function
          | __x0__ ->
              (function
               | __x1__ ->
                   (function
                    | __x2__ ->
                        let call =
                          Args.VIF.Set_carrier.call_of_set_carrier __x0__
                            __x1__ __x2__ in
                        R.bind (R.rpc call)
                          (function
                           | response ->
                               if response.Rpc.success
                               then
                                 R.return
                                   (Args.VIF.Set_carrier.response_of_rpc
                                      response.Rpc.contents)
                               else
                                 (let e =
                                    exn_of_exnty
                                      (Exception.exnty_of_rpc
                                         response.Rpc.contents) in
                                  R.fail e))))
        let set_locking_mode =
          function
          | __x0__ ->
              (function
               | __x1__ ->
                   (function
                    | __x2__ ->
                        let call =
                          Args.VIF.Set_locking_mode.call_of_set_locking_mode
                            __x0__ __x1__ __x2__ in
                        R.bind (R.rpc call)
                          (function
                           | response ->
                               if response.Rpc.success
                               then
                                 R.return
                                   (Args.VIF.Set_locking_mode.response_of_rpc
                                      response.Rpc.contents)
                               else
                                 (let e =
                                    exn_of_exnty
                                      (Exception.exnty_of_rpc
                                         response.Rpc.contents) in
                                  R.fail e))))
        let set_ipv4_configuration =
          function
          | __x0__ ->
              (function
               | __x1__ ->
                   (function
                    | __x2__ ->
                        let call =
                          Args.VIF.Set_ipv4_configuration.call_of_set_ipv4_configuration
                            __x0__ __x1__ __x2__ in
                        R.bind (R.rpc call)
                          (function
                           | response ->
                               if response.Rpc.success
                               then
                                 R.return
                                   (Args.VIF.Set_ipv4_configuration.response_of_rpc
                                      response.Rpc.contents)
                               else
                                 (let e =
                                    exn_of_exnty
                                      (Exception.exnty_of_rpc
                                         response.Rpc.contents) in
                                  R.fail e))))
        let set_ipv6_configuration =
          function
          | __x0__ ->
              (function
               | __x1__ ->
                   (function
                    | __x2__ ->
                        let call =
                          Args.VIF.Set_ipv6_configuration.call_of_set_ipv6_configuration
                            __x0__ __x1__ __x2__ in
                        R.bind (R.rpc call)
                          (function
                           | response ->
                               if response.Rpc.success
                               then
                                 R.return
                                   (Args.VIF.Set_ipv6_configuration.response_of_rpc
                                      response.Rpc.contents)
                               else
                                 (let e =
                                    exn_of_exnty
                                      (Exception.exnty_of_rpc
                                         response.Rpc.contents) in
                                  R.fail e))))
        let set_pvs_proxy =
          function
          | __x0__ ->
              (function
               | __x1__ ->
                   (function
                    | __x2__ ->
                        let call =
                          Args.VIF.Set_pvs_proxy.call_of_set_pvs_proxy __x0__
                            __x1__ __x2__ in
                        R.bind (R.rpc call)
                          (function
                           | response ->
                               if response.Rpc.success
                               then
                                 R.return
                                   (Args.VIF.Set_pvs_proxy.response_of_rpc
                                      response.Rpc.contents)
                               else
                                 (let e =
                                    exn_of_exnty
                                      (Exception.exnty_of_rpc
                                         response.Rpc.contents) in
                                  R.fail e))))
      end
    module VGPU =
      struct
        let add =
          function
          | __x0__ ->
              (function
               | __x1__ ->
                   let call = Args.VGPU.Add.call_of_add __x0__ __x1__ in
                   R.bind (R.rpc call)
                     (function
                      | response ->
                          if response.Rpc.success
                          then
                            R.return
                              (Args.VGPU.Add.response_of_rpc
                                 response.Rpc.contents)
                          else
                            (let e =
                               exn_of_exnty
                                 (Exception.exnty_of_rpc
                                    response.Rpc.contents) in
                             R.fail e)))
        let remove =
          function
          | __x0__ ->
              (function
               | __x1__ ->
                   let call = Args.VGPU.Remove.call_of_remove __x0__ __x1__ in
                   R.bind (R.rpc call)
                     (function
                      | response ->
                          if response.Rpc.success
                          then
                            R.return
                              (Args.VGPU.Remove.response_of_rpc
                                 response.Rpc.contents)
                          else
                            (let e =
                               exn_of_exnty
                                 (Exception.exnty_of_rpc
                                    response.Rpc.contents) in
                             R.fail e)))
        let stat =
          function
          | __x0__ ->
              (function
               | __x1__ ->
                   let call = Args.VGPU.Stat.call_of_stat __x0__ __x1__ in
                   R.bind (R.rpc call)
                     (function
                      | response ->
                          if response.Rpc.success
                          then
                            R.return
                              (Args.VGPU.Stat.response_of_rpc
                                 response.Rpc.contents)
                          else
                            (let e =
                               exn_of_exnty
                                 (Exception.exnty_of_rpc
                                    response.Rpc.contents) in
                             R.fail e)))
        let list =
          function
          | __x0__ ->
              (function
               | __x1__ ->
                   let call = Args.VGPU.List.call_of_list __x0__ __x1__ in
                   R.bind (R.rpc call)
                     (function
                      | response ->
                          if response.Rpc.success
                          then
                            R.return
                              (Args.VGPU.List.response_of_rpc
                                 response.Rpc.contents)
                          else
                            (let e =
                               exn_of_exnty
                                 (Exception.exnty_of_rpc
                                    response.Rpc.contents) in
                             R.fail e)))
      end
    module UPDATES =
      struct
        let get =
          function
          | __x0__ ->
              (function
               | __x1__ ->
                   (function
                    | __x2__ ->
                        let call =
                          Args.UPDATES.Get.call_of_get __x0__ __x1__ __x2__ in
                        R.bind (R.rpc call)
                          (function
                           | response ->
                               if response.Rpc.success
                               then
                                 R.return
                                   (Args.UPDATES.Get.response_of_rpc
                                      response.Rpc.contents)
                               else
                                 (let e =
                                    exn_of_exnty
                                      (Exception.exnty_of_rpc
                                         response.Rpc.contents) in
                                  R.fail e))))
        let last_id =
          function
          | __x0__ ->
              let call = Args.UPDATES.Last_id.call_of_last_id __x0__ in
              R.bind (R.rpc call)
                (function
                 | response ->
                     if response.Rpc.success
                     then
                       R.return
                         (Args.UPDATES.Last_id.response_of_rpc
                            response.Rpc.contents)
                     else
                       (let e =
                          exn_of_exnty
                            (Exception.exnty_of_rpc response.Rpc.contents) in
                        R.fail e))
        let inject_barrier =
          function
          | __x0__ ->
              (function
               | __x1__ ->
                   (function
                    | __x2__ ->
                        let call =
                          Args.UPDATES.Inject_barrier.call_of_inject_barrier
                            __x0__ __x1__ __x2__ in
                        R.bind (R.rpc call)
                          (function
                           | response ->
                               if response.Rpc.success
                               then
                                 R.return
                                   (Args.UPDATES.Inject_barrier.response_of_rpc
                                      response.Rpc.contents)
                               else
                                 (let e =
                                    exn_of_exnty
                                      (Exception.exnty_of_rpc
                                         response.Rpc.contents) in
                                  R.fail e))))
        let remove_barrier =
          function
          | __x0__ ->
              (function
               | __x1__ ->
                   let call =
                     Args.UPDATES.Remove_barrier.call_of_remove_barrier
                       __x0__ __x1__ in
                   R.bind (R.rpc call)
                     (function
                      | response ->
                          if response.Rpc.success
                          then
                            R.return
                              (Args.UPDATES.Remove_barrier.response_of_rpc
                                 response.Rpc.contents)
                          else
                            (let e =
                               exn_of_exnty
                                 (Exception.exnty_of_rpc
                                    response.Rpc.contents) in
                             R.fail e)))
        let refresh_vm =
          function
          | __x0__ ->
              (function
               | __x1__ ->
                   let call =
                     Args.UPDATES.Refresh_vm.call_of_refresh_vm __x0__ __x1__ in
                   R.bind (R.rpc call)
                     (function
                      | response ->
                          if response.Rpc.success
                          then
                            R.return
                              (Args.UPDATES.Refresh_vm.response_of_rpc
                                 response.Rpc.contents)
                          else
                            (let e =
                               exn_of_exnty
                                 (Exception.exnty_of_rpc
                                    response.Rpc.contents) in
                             R.fail e)))
      end
    module DEBUG =
      struct
        let trigger =
          function
          | __x0__ ->
              (function
               | __x1__ ->
                   (function
                    | __x2__ ->
                        let call =
                          Args.DEBUG.Trigger.call_of_trigger __x0__ __x1__
                            __x2__ in
                        R.bind (R.rpc call)
                          (function
                           | response ->
                               if response.Rpc.success
                               then
                                 R.return
                                   (Args.DEBUG.Trigger.response_of_rpc
                                      response.Rpc.contents)
                               else
                                 (let e =
                                    exn_of_exnty
                                      (Exception.exnty_of_rpc
                                         response.Rpc.contents) in
                                  R.fail e))))
        let shutdown =
          function
          | __x0__ ->
              (function
               | __x1__ ->
                   let call =
                     Args.DEBUG.Shutdown.call_of_shutdown __x0__ __x1__ in
                   R.bind (R.rpc call)
                     (function
                      | response ->
                          if response.Rpc.success
                          then
                            R.return
                              (Args.DEBUG.Shutdown.response_of_rpc
                                 response.Rpc.contents)
                          else
                            (let e =
                               exn_of_exnty
                                 (Exception.exnty_of_rpc
                                    response.Rpc.contents) in
                             R.fail e)))
      end
  end
module Client(R:RPC) =
  struct
    module RPCM =
      struct
        type 'a t = 'a
        let rpc = R.rpc
        let bind = function | a -> (function | f -> f a)
        let return = function | x -> x
        let fail = raise
      end
    include (ClientM)(RPCM)
  end
module type Server_impl  =
  sig
    type context
    val query : context -> debug_info -> unit -> Query.t
    val get_diagnostics : context -> debug_info -> unit -> string
    module TASK :
    sig
      val stat : context -> debug_info -> Task.id -> Task.t
      val cancel : context -> debug_info -> Task.id -> unit
      val destroy : context -> debug_info -> Task.id -> unit
      val list : context -> debug_info -> Task.t list
    end
    module HOST :
    sig
      val stat : context -> debug_info -> Host.t
      val get_console_data : context -> debug_info -> string
      val get_total_memory_mib : context -> debug_info -> int64
      val send_debug_keys : context -> debug_info -> string -> unit
      val set_worker_pool_size : context -> debug_info -> int -> unit
      val update_guest_agent_features :
        context -> debug_info -> Host.guest_agent_feature list -> unit
      val upgrade_cpu_features :
        context -> debug_info -> int64 array -> bool -> int64 array
    end
    module VM :
    sig
      val add : context -> debug_info -> Vm.t -> Vm.id
      val remove : context -> debug_info -> Vm.id -> unit
      val generate_state_string : context -> debug_info -> Vm.t -> string
      val migrate :
        context ->
          debug_info ->
            Vm.id ->
              (string * string) list ->
                (string * Network.t) list ->
                  (string * Pci.address) list -> string -> Task.id
      val migrate_receive_memory :
        context ->
          debug_info ->
            Vm.id -> int64 -> string -> Xcp_channel.t -> Task.id option
      val create : context -> debug_info -> Vm.id -> Task.id
      val build : context -> debug_info -> Vm.id -> bool -> Task.id
      val create_device_model :
        context -> debug_info -> Vm.id -> bool -> Task.id
      val destroy : context -> debug_info -> Vm.id -> Task.id
      val pause : context -> debug_info -> Vm.id -> Task.id
      val unpause : context -> debug_info -> Vm.id -> Task.id
      val request_rdp : context -> debug_info -> Vm.id -> bool -> Task.id
      val run_script : context -> debug_info -> Vm.id -> string -> Task.id
      val set_xsdata :
        context -> debug_info -> Vm.id -> (string * string) list -> Task.id
      val set_vcpus : context -> debug_info -> Vm.id -> int -> Task.id
      val set_shadow_multiplier :
        context -> debug_info -> Vm.id -> float -> Task.id
      val set_memory_dynamic_range :
        context -> debug_info -> Vm.id -> int64 -> int64 -> Task.id
      val stat : context -> debug_info -> Vm.id -> (Vm.t * Vm.state)
      val exists : context -> debug_info -> Vm.id -> bool
      val list : context -> debug_info -> unit -> (Vm.t * Vm.state) list
      val delay : context -> debug_info -> Vm.id -> float -> Task.id
      val start : context -> debug_info -> Vm.id -> bool -> Task.id
      val shutdown :
        context -> debug_info -> Vm.id -> float option -> Task.id
      val reboot : context -> debug_info -> Vm.id -> float option -> Task.id
      val suspend : context -> debug_info -> Vm.id -> disk -> Task.id
      val resume : context -> debug_info -> Vm.id -> disk -> Task.id
      val s3suspend : context -> debug_info -> Vm.id -> Task.id
      val s3resume : context -> debug_info -> Vm.id -> Task.id
      val export_metadata : context -> debug_info -> Vm.id -> string
      val import_metadata : context -> debug_info -> string -> Vm.id
    end
    module PCI :
    sig
      val add : context -> debug_info -> Pci.t -> Pci.id
      val remove : context -> debug_info -> Pci.id -> unit
      val stat : context -> debug_info -> Pci.id -> (Pci.t * Pci.state)
      val list : context -> debug_info -> Vm.id -> (Pci.t * Pci.state) list
    end
    module VBD :
    sig
      val add : context -> debug_info -> Vbd.t -> Vbd.id
      val plug : context -> debug_info -> Vbd.id -> Task.id
      val unplug : context -> debug_info -> Vbd.id -> bool -> Task.id
      val eject : context -> debug_info -> Vbd.id -> Task.id
      val insert : context -> debug_info -> Vbd.id -> disk -> Task.id
      val stat : context -> debug_info -> Vbd.id -> (Vbd.t * Vbd.state)
      val list : context -> debug_info -> Vm.id -> (Vbd.t * Vbd.state) list
      val remove : context -> debug_info -> Vbd.id -> unit
    end
    module VUSB :
    sig
      val add : context -> debug_info -> Vusb.t -> Vusb.id
      val plug : context -> debug_info -> Vusb.id -> Task.id
      val unplug : context -> debug_info -> Vusb.id -> Task.id
      val stat : context -> debug_info -> Vusb.id -> (Vusb.t * Vusb.state)
      val list : context -> debug_info -> Vm.id -> (Vusb.t * Vusb.state) list
      val remove : context -> debug_info -> Vusb.id -> unit
    end
    module VIF :
    sig
      val add : context -> debug_info -> Vif.t -> Vif.id
      val plug : context -> debug_info -> Vif.id -> Task.id
      val unplug : context -> debug_info -> Vif.id -> bool -> Task.id
      val move : context -> debug_info -> Vif.id -> Network.t -> Task.id
      val stat : context -> debug_info -> Vif.id -> (Vif.t * Vif.state)
      val list : context -> debug_info -> Vm.id -> (Vif.t * Vif.state) list
      val remove : context -> debug_info -> Vif.id -> unit
      val set_carrier : context -> debug_info -> Vif.id -> bool -> Task.id
      val set_locking_mode :
        context -> debug_info -> Vif.id -> Vif.locking_mode -> Task.id
      val set_ipv4_configuration :
        context -> debug_info -> Vif.id -> Vif.ipv4_configuration -> Task.id
      val set_ipv6_configuration :
        context -> debug_info -> Vif.id -> Vif.ipv6_configuration -> Task.id
      val set_pvs_proxy :
        context -> debug_info -> Vif.id -> Vif.PVS_proxy.t option -> Task.id
    end
    module VGPU :
    sig
      val add : context -> debug_info -> Vgpu.t -> Vgpu.id
      val remove : context -> debug_info -> Vgpu.id -> unit
      val stat : context -> debug_info -> Vgpu.id -> (Vgpu.t * Vgpu.state)
      val list : context -> debug_info -> Vm.id -> (Vgpu.t * Vgpu.state) list
    end
    module UPDATES :
    sig
      val get :
        context ->
          debug_info ->
            int option ->
              int option -> (Dynamic.barrier list * Dynamic.id list * int)
      val last_id : context -> debug_info -> int
      val inject_barrier : context -> debug_info -> Vm.id -> int -> unit
      val remove_barrier : context -> debug_info -> int -> unit
      val refresh_vm : context -> debug_info -> Vm.id -> unit
    end
    module DEBUG :
    sig
      val trigger : context -> debug_info -> string -> string list -> unit
      val shutdown : context -> debug_info -> unit -> unit
    end
  end
module type Server_implM  =
  sig
    type 'a t
    val bind : 'a t -> ('a -> 'b t) -> 'b t
    val return : 'a -> 'a t
    val fail : exn -> 'a t
    val handle_failure : (unit -> 'b t) -> (exn -> 'b t) -> 'b t
    type context
    val query : context -> debug_info -> unit -> Query.t t
    val get_diagnostics : context -> debug_info -> unit -> string t
    module TASK :
    sig
      val stat : context -> debug_info -> Task.id -> Task.t t
      val cancel : context -> debug_info -> Task.id -> unit t
      val destroy : context -> debug_info -> Task.id -> unit t
      val list : context -> debug_info -> Task.t list t
    end
    module HOST :
    sig
      val stat : context -> debug_info -> Host.t t
      val get_console_data : context -> debug_info -> string t
      val get_total_memory_mib : context -> debug_info -> int64 t
      val send_debug_keys : context -> debug_info -> string -> unit t
      val set_worker_pool_size : context -> debug_info -> int -> unit t
      val update_guest_agent_features :
        context -> debug_info -> Host.guest_agent_feature list -> unit t
      val upgrade_cpu_features :
        context -> debug_info -> int64 array -> bool -> int64 array t
    end
    module VM :
    sig
      val add : context -> debug_info -> Vm.t -> Vm.id t
      val remove : context -> debug_info -> Vm.id -> unit t
      val generate_state_string : context -> debug_info -> Vm.t -> string t
      val migrate :
        context ->
          debug_info ->
            Vm.id ->
              (string * string) list ->
                (string * Network.t) list ->
                  (string * Pci.address) list -> string -> Task.id t
      val migrate_receive_memory :
        context ->
          debug_info ->
            Vm.id -> int64 -> string -> Xcp_channel.t -> Task.id option t
      val create : context -> debug_info -> Vm.id -> Task.id t
      val build : context -> debug_info -> Vm.id -> bool -> Task.id t
      val create_device_model :
        context -> debug_info -> Vm.id -> bool -> Task.id t
      val destroy : context -> debug_info -> Vm.id -> Task.id t
      val pause : context -> debug_info -> Vm.id -> Task.id t
      val unpause : context -> debug_info -> Vm.id -> Task.id t
      val request_rdp : context -> debug_info -> Vm.id -> bool -> Task.id t
      val run_script : context -> debug_info -> Vm.id -> string -> Task.id t
      val set_xsdata :
        context -> debug_info -> Vm.id -> (string * string) list -> Task.id t
      val set_vcpus : context -> debug_info -> Vm.id -> int -> Task.id t
      val set_shadow_multiplier :
        context -> debug_info -> Vm.id -> float -> Task.id t
      val set_memory_dynamic_range :
        context -> debug_info -> Vm.id -> int64 -> int64 -> Task.id t
      val stat : context -> debug_info -> Vm.id -> (Vm.t * Vm.state) t
      val exists : context -> debug_info -> Vm.id -> bool t
      val list : context -> debug_info -> unit -> (Vm.t * Vm.state) list t
      val delay : context -> debug_info -> Vm.id -> float -> Task.id t
      val start : context -> debug_info -> Vm.id -> bool -> Task.id t
      val shutdown :
        context -> debug_info -> Vm.id -> float option -> Task.id t
      val reboot :
        context -> debug_info -> Vm.id -> float option -> Task.id t
      val suspend : context -> debug_info -> Vm.id -> disk -> Task.id t
      val resume : context -> debug_info -> Vm.id -> disk -> Task.id t
      val s3suspend : context -> debug_info -> Vm.id -> Task.id t
      val s3resume : context -> debug_info -> Vm.id -> Task.id t
      val export_metadata : context -> debug_info -> Vm.id -> string t
      val import_metadata : context -> debug_info -> string -> Vm.id t
    end
    module PCI :
    sig
      val add : context -> debug_info -> Pci.t -> Pci.id t
      val remove : context -> debug_info -> Pci.id -> unit t
      val stat : context -> debug_info -> Pci.id -> (Pci.t * Pci.state) t
      val list : context -> debug_info -> Vm.id -> (Pci.t * Pci.state) list t
    end
    module VBD :
    sig
      val add : context -> debug_info -> Vbd.t -> Vbd.id t
      val plug : context -> debug_info -> Vbd.id -> Task.id t
      val unplug : context -> debug_info -> Vbd.id -> bool -> Task.id t
      val eject : context -> debug_info -> Vbd.id -> Task.id t
      val insert : context -> debug_info -> Vbd.id -> disk -> Task.id t
      val stat : context -> debug_info -> Vbd.id -> (Vbd.t * Vbd.state) t
      val list : context -> debug_info -> Vm.id -> (Vbd.t * Vbd.state) list t
      val remove : context -> debug_info -> Vbd.id -> unit t
    end
    module VUSB :
    sig
      val add : context -> debug_info -> Vusb.t -> Vusb.id t
      val plug : context -> debug_info -> Vusb.id -> Task.id t
      val unplug : context -> debug_info -> Vusb.id -> Task.id t
      val stat : context -> debug_info -> Vusb.id -> (Vusb.t * Vusb.state) t
      val list :
        context -> debug_info -> Vm.id -> (Vusb.t * Vusb.state) list t
      val remove : context -> debug_info -> Vusb.id -> unit t
    end
    module VIF :
    sig
      val add : context -> debug_info -> Vif.t -> Vif.id t
      val plug : context -> debug_info -> Vif.id -> Task.id t
      val unplug : context -> debug_info -> Vif.id -> bool -> Task.id t
      val move : context -> debug_info -> Vif.id -> Network.t -> Task.id t
      val stat : context -> debug_info -> Vif.id -> (Vif.t * Vif.state) t
      val list : context -> debug_info -> Vm.id -> (Vif.t * Vif.state) list t
      val remove : context -> debug_info -> Vif.id -> unit t
      val set_carrier : context -> debug_info -> Vif.id -> bool -> Task.id t
      val set_locking_mode :
        context -> debug_info -> Vif.id -> Vif.locking_mode -> Task.id t
      val set_ipv4_configuration :
        context ->
          debug_info -> Vif.id -> Vif.ipv4_configuration -> Task.id t
      val set_ipv6_configuration :
        context ->
          debug_info -> Vif.id -> Vif.ipv6_configuration -> Task.id t
      val set_pvs_proxy :
        context ->
          debug_info -> Vif.id -> Vif.PVS_proxy.t option -> Task.id t
    end
    module VGPU :
    sig
      val add : context -> debug_info -> Vgpu.t -> Vgpu.id t
      val remove : context -> debug_info -> Vgpu.id -> unit t
      val stat : context -> debug_info -> Vgpu.id -> (Vgpu.t * Vgpu.state) t
      val list :
        context -> debug_info -> Vm.id -> (Vgpu.t * Vgpu.state) list t
    end
    module UPDATES :
    sig
      val get :
        context ->
          debug_info ->
            int option ->
              int option -> (Dynamic.barrier list * Dynamic.id list * int) t
      val last_id : context -> debug_info -> int t
      val inject_barrier : context -> debug_info -> Vm.id -> int -> unit t
      val remove_barrier : context -> debug_info -> int -> unit t
      val refresh_vm : context -> debug_info -> Vm.id -> unit t
    end
    module DEBUG :
    sig
      val trigger : context -> debug_info -> string -> string list -> unit t
      val shutdown : context -> debug_info -> unit -> unit t
    end
  end
module ServerM(Impl:Server_implM) =
  struct
    let process =
      function
      | x ->
          (function
           | call ->
               Impl.handle_failure
                 (function
                  | () ->
                      let contents =
                        match ((call.Rpc.name), (call.Rpc.params)) with
                        | ("query", __x1__::__x2__::[]) ->
                            Impl.bind
                              (Impl.query x (Args.Query.__x1___of_rpc __x1__)
                                 (Args.Query.__x2___of_rpc __x2__))
                              (function
                               | x ->
                                   Impl.return (Args.Query.rpc_of_response x))
                        | ("query", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("query", 2, (List.length call.Rpc.params)))
                        | ("get_diagnostics", __x1__::__x2__::[]) ->
                            Impl.bind
                              (Impl.get_diagnostics x
                                 (Args.Get_diagnostics.__x1___of_rpc __x1__)
                                 (Args.Get_diagnostics.__x2___of_rpc __x2__))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.Get_diagnostics.rpc_of_response x))
                        | ("get_diagnostics", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("get_diagnostics", 2,
                                   (List.length call.Rpc.params)))
                        | ("TASK.stat", __x1__::__x2__::[]) ->
                            Impl.bind
                              (Impl.TASK.stat x
                                 (Args.TASK.Stat.__x1___of_rpc __x1__)
                                 (Args.TASK.Stat.__x2___of_rpc __x2__))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.TASK.Stat.rpc_of_response x))
                        | ("TASK.stat", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("TASK.stat", 2,
                                   (List.length call.Rpc.params)))
                        | ("TASK.cancel", __x1__::__x2__::[]) ->
                            Impl.bind
                              (Impl.TASK.cancel x
                                 (Args.TASK.Cancel.__x1___of_rpc __x1__)
                                 (Args.TASK.Cancel.__x2___of_rpc __x2__))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.TASK.Cancel.rpc_of_response x))
                        | ("TASK.cancel", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("TASK.cancel", 2,
                                   (List.length call.Rpc.params)))
                        | ("TASK.destroy", __x1__::__x2__::[]) ->
                            Impl.bind
                              (Impl.TASK.destroy x
                                 (Args.TASK.Destroy.__x1___of_rpc __x1__)
                                 (Args.TASK.Destroy.__x2___of_rpc __x2__))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.TASK.Destroy.rpc_of_response x))
                        | ("TASK.destroy", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("TASK.destroy", 2,
                                   (List.length call.Rpc.params)))
                        | ("TASK.list", __x1__::[]) ->
                            Impl.bind
                              (Impl.TASK.list x
                                 (Args.TASK.List.__x1___of_rpc __x1__))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.TASK.List.rpc_of_response x))
                        | ("TASK.list", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("TASK.list", 1,
                                   (List.length call.Rpc.params)))
                        | ("HOST.stat", __x1__::[]) ->
                            Impl.bind
                              (Impl.HOST.stat x
                                 (Args.HOST.Stat.__x1___of_rpc __x1__))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.HOST.Stat.rpc_of_response x))
                        | ("HOST.stat", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("HOST.stat", 1,
                                   (List.length call.Rpc.params)))
                        | ("HOST.get_console_data", __x1__::[]) ->
                            Impl.bind
                              (Impl.HOST.get_console_data x
                                 (Args.HOST.Get_console_data.__x1___of_rpc
                                    __x1__))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.HOST.Get_console_data.rpc_of_response
                                        x))
                        | ("HOST.get_console_data", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("HOST.get_console_data", 1,
                                   (List.length call.Rpc.params)))
                        | ("HOST.get_total_memory_mib", __x1__::[]) ->
                            Impl.bind
                              (Impl.HOST.get_total_memory_mib x
                                 (Args.HOST.Get_total_memory_mib.__x1___of_rpc
                                    __x1__))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.HOST.Get_total_memory_mib.rpc_of_response
                                        x))
                        | ("HOST.get_total_memory_mib", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("HOST.get_total_memory_mib", 1,
                                   (List.length call.Rpc.params)))
                        | ("HOST.send_debug_keys", __x1__::__x2__::[]) ->
                            Impl.bind
                              (Impl.HOST.send_debug_keys x
                                 (Args.HOST.Send_debug_keys.__x1___of_rpc
                                    __x1__)
                                 (Args.HOST.Send_debug_keys.__x2___of_rpc
                                    __x2__))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.HOST.Send_debug_keys.rpc_of_response
                                        x))
                        | ("HOST.send_debug_keys", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("HOST.send_debug_keys", 2,
                                   (List.length call.Rpc.params)))
                        | ("HOST.set_worker_pool_size", __x1__::__x2__::[])
                            ->
                            Impl.bind
                              (Impl.HOST.set_worker_pool_size x
                                 (Args.HOST.Set_worker_pool_size.__x1___of_rpc
                                    __x1__)
                                 (Args.HOST.Set_worker_pool_size.__x2___of_rpc
                                    __x2__))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.HOST.Set_worker_pool_size.rpc_of_response
                                        x))
                        | ("HOST.set_worker_pool_size", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("HOST.set_worker_pool_size", 2,
                                   (List.length call.Rpc.params)))
                        | ("HOST.update_guest_agent_features",
                           __x1__::__x2__::[]) ->
                            Impl.bind
                              (Impl.HOST.update_guest_agent_features x
                                 (Args.HOST.Update_guest_agent_features.__x1___of_rpc
                                    __x1__)
                                 (Args.HOST.Update_guest_agent_features.__x2___of_rpc
                                    __x2__))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.HOST.Update_guest_agent_features.rpc_of_response
                                        x))
                        | ("HOST.update_guest_agent_features", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("HOST.update_guest_agent_features", 2,
                                   (List.length call.Rpc.params)))
                        | ("HOST.upgrade_cpu_features",
                           __x1__::__x2__::__x3__::[]) ->
                            Impl.bind
                              (Impl.HOST.upgrade_cpu_features x
                                 (Args.HOST.Upgrade_cpu_features.__x1___of_rpc
                                    __x1__)
                                 (Args.HOST.Upgrade_cpu_features.__x2___of_rpc
                                    __x2__)
                                 (Args.HOST.Upgrade_cpu_features.__x3___of_rpc
                                    __x3__))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.HOST.Upgrade_cpu_features.rpc_of_response
                                        x))
                        | ("HOST.upgrade_cpu_features", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("HOST.upgrade_cpu_features", 3,
                                   (List.length call.Rpc.params)))
                        | ("VM.add", __x1__::__x2__::[]) ->
                            Impl.bind
                              (Impl.VM.add x
                                 (Args.VM.Add.__x1___of_rpc __x1__)
                                 (Args.VM.Add.__x2___of_rpc __x2__))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.VM.Add.rpc_of_response x))
                        | ("VM.add", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("VM.add", 2, (List.length call.Rpc.params)))
                        | ("VM.remove", __x1__::__x2__::[]) ->
                            Impl.bind
                              (Impl.VM.remove x
                                 (Args.VM.Remove.__x1___of_rpc __x1__)
                                 (Args.VM.Remove.__x2___of_rpc __x2__))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.VM.Remove.rpc_of_response x))
                        | ("VM.remove", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("VM.remove", 2,
                                   (List.length call.Rpc.params)))
                        | ("VM.generate_state_string", __x1__::__x2__::[]) ->
                            Impl.bind
                              (Impl.VM.generate_state_string x
                                 (Args.VM.Generate_state_string.__x1___of_rpc
                                    __x1__)
                                 (Args.VM.Generate_state_string.__x2___of_rpc
                                    __x2__))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.VM.Generate_state_string.rpc_of_response
                                        x))
                        | ("VM.generate_state_string", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("VM.generate_state_string", 2,
                                   (List.length call.Rpc.params)))
                        | ("VM.migrate",
                           __x1__::__x2__::__x3__::__x4__::__x5__::__x6__::[])
                            ->
                            Impl.bind
                              (Impl.VM.migrate x
                                 (Args.VM.Migrate.__x1___of_rpc __x1__)
                                 (Args.VM.Migrate.__x2___of_rpc __x2__)
                                 (Args.VM.Migrate.__x3___of_rpc __x3__)
                                 (Args.VM.Migrate.__x4___of_rpc __x4__)
                                 (Args.VM.Migrate.__x5___of_rpc __x5__)
                                 (Args.VM.Migrate.__x6___of_rpc __x6__))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.VM.Migrate.rpc_of_response x))
                        | ("VM.migrate", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("VM.migrate", 6,
                                   (List.length call.Rpc.params)))
                        | ("VM.migrate_receive_memory",
                           __x1__::__x2__::__x3__::__x4__::__x5__::[]) ->
                            Impl.bind
                              (Impl.VM.migrate_receive_memory x
                                 (Args.VM.Migrate_receive_memory.__x1___of_rpc
                                    __x1__)
                                 (Args.VM.Migrate_receive_memory.__x2___of_rpc
                                    __x2__)
                                 (Args.VM.Migrate_receive_memory.__x3___of_rpc
                                    __x3__)
                                 (Args.VM.Migrate_receive_memory.__x4___of_rpc
                                    __x4__)
                                 (Args.VM.Migrate_receive_memory.__x5___of_rpc
                                    __x5__))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.VM.Migrate_receive_memory.rpc_of_response
                                        x))
                        | ("VM.migrate_receive_memory", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("VM.migrate_receive_memory", 5,
                                   (List.length call.Rpc.params)))
                        | ("VM.create", __x1__::__x2__::[]) ->
                            Impl.bind
                              (Impl.VM.create x
                                 (Args.VM.Create.__x1___of_rpc __x1__)
                                 (Args.VM.Create.__x2___of_rpc __x2__))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.VM.Create.rpc_of_response x))
                        | ("VM.create", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("VM.create", 2,
                                   (List.length call.Rpc.params)))
                        | ("VM.build", __x1__::__x2__::__x3__::[]) ->
                            Impl.bind
                              (Impl.VM.build x
                                 (Args.VM.Build.__x1___of_rpc __x1__)
                                 (Args.VM.Build.__x2___of_rpc __x2__)
                                 (Args.VM.Build.__x3___of_rpc __x3__))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.VM.Build.rpc_of_response x))
                        | ("VM.build", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("VM.build", 3,
                                   (List.length call.Rpc.params)))
                        | ("VM.create_device_model",
                           __x1__::__x2__::__x3__::[]) ->
                            Impl.bind
                              (Impl.VM.create_device_model x
                                 (Args.VM.Create_device_model.__x1___of_rpc
                                    __x1__)
                                 (Args.VM.Create_device_model.__x2___of_rpc
                                    __x2__)
                                 (Args.VM.Create_device_model.__x3___of_rpc
                                    __x3__))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.VM.Create_device_model.rpc_of_response
                                        x))
                        | ("VM.create_device_model", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("VM.create_device_model", 3,
                                   (List.length call.Rpc.params)))
                        | ("VM.destroy", __x1__::__x2__::[]) ->
                            Impl.bind
                              (Impl.VM.destroy x
                                 (Args.VM.Destroy.__x1___of_rpc __x1__)
                                 (Args.VM.Destroy.__x2___of_rpc __x2__))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.VM.Destroy.rpc_of_response x))
                        | ("VM.destroy", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("VM.destroy", 2,
                                   (List.length call.Rpc.params)))
                        | ("VM.pause", __x1__::__x2__::[]) ->
                            Impl.bind
                              (Impl.VM.pause x
                                 (Args.VM.Pause.__x1___of_rpc __x1__)
                                 (Args.VM.Pause.__x2___of_rpc __x2__))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.VM.Pause.rpc_of_response x))
                        | ("VM.pause", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("VM.pause", 2,
                                   (List.length call.Rpc.params)))
                        | ("VM.unpause", __x1__::__x2__::[]) ->
                            Impl.bind
                              (Impl.VM.unpause x
                                 (Args.VM.Unpause.__x1___of_rpc __x1__)
                                 (Args.VM.Unpause.__x2___of_rpc __x2__))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.VM.Unpause.rpc_of_response x))
                        | ("VM.unpause", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("VM.unpause", 2,
                                   (List.length call.Rpc.params)))
                        | ("VM.request_rdp", __x1__::__x2__::__x3__::[]) ->
                            Impl.bind
                              (Impl.VM.request_rdp x
                                 (Args.VM.Request_rdp.__x1___of_rpc __x1__)
                                 (Args.VM.Request_rdp.__x2___of_rpc __x2__)
                                 (Args.VM.Request_rdp.__x3___of_rpc __x3__))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.VM.Request_rdp.rpc_of_response x))
                        | ("VM.request_rdp", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("VM.request_rdp", 3,
                                   (List.length call.Rpc.params)))
                        | ("VM.run_script", __x1__::__x2__::__x3__::[]) ->
                            Impl.bind
                              (Impl.VM.run_script x
                                 (Args.VM.Run_script.__x1___of_rpc __x1__)
                                 (Args.VM.Run_script.__x2___of_rpc __x2__)
                                 (Args.VM.Run_script.__x3___of_rpc __x3__))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.VM.Run_script.rpc_of_response x))
                        | ("VM.run_script", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("VM.run_script", 3,
                                   (List.length call.Rpc.params)))
                        | ("VM.set_xsdata", __x1__::__x2__::__x3__::[]) ->
                            Impl.bind
                              (Impl.VM.set_xsdata x
                                 (Args.VM.Set_xsdata.__x1___of_rpc __x1__)
                                 (Args.VM.Set_xsdata.__x2___of_rpc __x2__)
                                 (Args.VM.Set_xsdata.__x3___of_rpc __x3__))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.VM.Set_xsdata.rpc_of_response x))
                        | ("VM.set_xsdata", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("VM.set_xsdata", 3,
                                   (List.length call.Rpc.params)))
                        | ("VM.set_vcpus", __x1__::__x2__::__x3__::[]) ->
                            Impl.bind
                              (Impl.VM.set_vcpus x
                                 (Args.VM.Set_vcpus.__x1___of_rpc __x1__)
                                 (Args.VM.Set_vcpus.__x2___of_rpc __x2__)
                                 (Args.VM.Set_vcpus.__x3___of_rpc __x3__))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.VM.Set_vcpus.rpc_of_response x))
                        | ("VM.set_vcpus", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("VM.set_vcpus", 3,
                                   (List.length call.Rpc.params)))
                        | ("VM.set_shadow_multiplier",
                           __x1__::__x2__::__x3__::[]) ->
                            Impl.bind
                              (Impl.VM.set_shadow_multiplier x
                                 (Args.VM.Set_shadow_multiplier.__x1___of_rpc
                                    __x1__)
                                 (Args.VM.Set_shadow_multiplier.__x2___of_rpc
                                    __x2__)
                                 (Args.VM.Set_shadow_multiplier.__x3___of_rpc
                                    __x3__))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.VM.Set_shadow_multiplier.rpc_of_response
                                        x))
                        | ("VM.set_shadow_multiplier", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("VM.set_shadow_multiplier", 3,
                                   (List.length call.Rpc.params)))
                        | ("VM.set_memory_dynamic_range",
                           __x1__::__x2__::__x3__::__x4__::[]) ->
                            Impl.bind
                              (Impl.VM.set_memory_dynamic_range x
                                 (Args.VM.Set_memory_dynamic_range.__x1___of_rpc
                                    __x1__)
                                 (Args.VM.Set_memory_dynamic_range.__x2___of_rpc
                                    __x2__)
                                 (Args.VM.Set_memory_dynamic_range.__x3___of_rpc
                                    __x3__)
                                 (Args.VM.Set_memory_dynamic_range.__x4___of_rpc
                                    __x4__))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.VM.Set_memory_dynamic_range.rpc_of_response
                                        x))
                        | ("VM.set_memory_dynamic_range", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("VM.set_memory_dynamic_range", 4,
                                   (List.length call.Rpc.params)))
                        | ("VM.stat", __x1__::__x2__::[]) ->
                            Impl.bind
                              (Impl.VM.stat x
                                 (Args.VM.Stat.__x1___of_rpc __x1__)
                                 (Args.VM.Stat.__x2___of_rpc __x2__))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.VM.Stat.rpc_of_response x))
                        | ("VM.stat", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("VM.stat", 2,
                                   (List.length call.Rpc.params)))
                        | ("VM.exists", __x1__::__x2__::[]) ->
                            Impl.bind
                              (Impl.VM.exists x
                                 (Args.VM.Exists.__x1___of_rpc __x1__)
                                 (Args.VM.Exists.__x2___of_rpc __x2__))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.VM.Exists.rpc_of_response x))
                        | ("VM.exists", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("VM.exists", 2,
                                   (List.length call.Rpc.params)))
                        | ("VM.list", __x1__::__x2__::[]) ->
                            Impl.bind
                              (Impl.VM.list x
                                 (Args.VM.List.__x1___of_rpc __x1__)
                                 (Args.VM.List.__x2___of_rpc __x2__))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.VM.List.rpc_of_response x))
                        | ("VM.list", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("VM.list", 2,
                                   (List.length call.Rpc.params)))
                        | ("VM.delay", __x1__::__x2__::__x3__::[]) ->
                            Impl.bind
                              (Impl.VM.delay x
                                 (Args.VM.Delay.__x1___of_rpc __x1__)
                                 (Args.VM.Delay.__x2___of_rpc __x2__)
                                 (Args.VM.Delay.__x3___of_rpc __x3__))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.VM.Delay.rpc_of_response x))
                        | ("VM.delay", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("VM.delay", 3,
                                   (List.length call.Rpc.params)))
                        | ("VM.start", __x1__::__x2__::__x3__::[]) ->
                            Impl.bind
                              (Impl.VM.start x
                                 (Args.VM.Start.__x1___of_rpc __x1__)
                                 (Args.VM.Start.__x2___of_rpc __x2__)
                                 (Args.VM.Start.__x3___of_rpc __x3__))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.VM.Start.rpc_of_response x))
                        | ("VM.start", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("VM.start", 3,
                                   (List.length call.Rpc.params)))
                        | ("VM.shutdown", __x1__::__x2__::__x3__::[]) ->
                            Impl.bind
                              (Impl.VM.shutdown x
                                 (Args.VM.Shutdown.__x1___of_rpc __x1__)
                                 (Args.VM.Shutdown.__x2___of_rpc __x2__)
                                 (Args.VM.Shutdown.__x3___of_rpc __x3__))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.VM.Shutdown.rpc_of_response x))
                        | ("VM.shutdown", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("VM.shutdown", 3,
                                   (List.length call.Rpc.params)))
                        | ("VM.reboot", __x1__::__x2__::__x3__::[]) ->
                            Impl.bind
                              (Impl.VM.reboot x
                                 (Args.VM.Reboot.__x1___of_rpc __x1__)
                                 (Args.VM.Reboot.__x2___of_rpc __x2__)
                                 (Args.VM.Reboot.__x3___of_rpc __x3__))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.VM.Reboot.rpc_of_response x))
                        | ("VM.reboot", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("VM.reboot", 3,
                                   (List.length call.Rpc.params)))
                        | ("VM.suspend", __x1__::__x2__::__x3__::[]) ->
                            Impl.bind
                              (Impl.VM.suspend x
                                 (Args.VM.Suspend.__x1___of_rpc __x1__)
                                 (Args.VM.Suspend.__x2___of_rpc __x2__)
                                 (Args.VM.Suspend.__x3___of_rpc __x3__))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.VM.Suspend.rpc_of_response x))
                        | ("VM.suspend", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("VM.suspend", 3,
                                   (List.length call.Rpc.params)))
                        | ("VM.resume", __x1__::__x2__::__x3__::[]) ->
                            Impl.bind
                              (Impl.VM.resume x
                                 (Args.VM.Resume.__x1___of_rpc __x1__)
                                 (Args.VM.Resume.__x2___of_rpc __x2__)
                                 (Args.VM.Resume.__x3___of_rpc __x3__))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.VM.Resume.rpc_of_response x))
                        | ("VM.resume", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("VM.resume", 3,
                                   (List.length call.Rpc.params)))
                        | ("VM.s3suspend", __x1__::__x2__::[]) ->
                            Impl.bind
                              (Impl.VM.s3suspend x
                                 (Args.VM.S3suspend.__x1___of_rpc __x1__)
                                 (Args.VM.S3suspend.__x2___of_rpc __x2__))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.VM.S3suspend.rpc_of_response x))
                        | ("VM.s3suspend", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("VM.s3suspend", 2,
                                   (List.length call.Rpc.params)))
                        | ("VM.s3resume", __x1__::__x2__::[]) ->
                            Impl.bind
                              (Impl.VM.s3resume x
                                 (Args.VM.S3resume.__x1___of_rpc __x1__)
                                 (Args.VM.S3resume.__x2___of_rpc __x2__))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.VM.S3resume.rpc_of_response x))
                        | ("VM.s3resume", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("VM.s3resume", 2,
                                   (List.length call.Rpc.params)))
                        | ("VM.export_metadata", __x1__::__x2__::[]) ->
                            Impl.bind
                              (Impl.VM.export_metadata x
                                 (Args.VM.Export_metadata.__x1___of_rpc
                                    __x1__)
                                 (Args.VM.Export_metadata.__x2___of_rpc
                                    __x2__))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.VM.Export_metadata.rpc_of_response
                                        x))
                        | ("VM.export_metadata", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("VM.export_metadata", 2,
                                   (List.length call.Rpc.params)))
                        | ("VM.import_metadata", __x1__::__x2__::[]) ->
                            Impl.bind
                              (Impl.VM.import_metadata x
                                 (Args.VM.Import_metadata.__x1___of_rpc
                                    __x1__)
                                 (Args.VM.Import_metadata.__x2___of_rpc
                                    __x2__))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.VM.Import_metadata.rpc_of_response
                                        x))
                        | ("VM.import_metadata", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("VM.import_metadata", 2,
                                   (List.length call.Rpc.params)))
                        | ("PCI.add", __x1__::__x2__::[]) ->
                            Impl.bind
                              (Impl.PCI.add x
                                 (Args.PCI.Add.__x1___of_rpc __x1__)
                                 (Args.PCI.Add.__x2___of_rpc __x2__))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.PCI.Add.rpc_of_response x))
                        | ("PCI.add", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("PCI.add", 2,
                                   (List.length call.Rpc.params)))
                        | ("PCI.remove", __x1__::__x2__::[]) ->
                            Impl.bind
                              (Impl.PCI.remove x
                                 (Args.PCI.Remove.__x1___of_rpc __x1__)
                                 (Args.PCI.Remove.__x2___of_rpc __x2__))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.PCI.Remove.rpc_of_response x))
                        | ("PCI.remove", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("PCI.remove", 2,
                                   (List.length call.Rpc.params)))
                        | ("PCI.stat", __x1__::__x2__::[]) ->
                            Impl.bind
                              (Impl.PCI.stat x
                                 (Args.PCI.Stat.__x1___of_rpc __x1__)
                                 (Args.PCI.Stat.__x2___of_rpc __x2__))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.PCI.Stat.rpc_of_response x))
                        | ("PCI.stat", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("PCI.stat", 2,
                                   (List.length call.Rpc.params)))
                        | ("PCI.list", __x1__::__x2__::[]) ->
                            Impl.bind
                              (Impl.PCI.list x
                                 (Args.PCI.List.__x1___of_rpc __x1__)
                                 (Args.PCI.List.__x2___of_rpc __x2__))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.PCI.List.rpc_of_response x))
                        | ("PCI.list", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("PCI.list", 2,
                                   (List.length call.Rpc.params)))
                        | ("VBD.add", __x1__::__x2__::[]) ->
                            Impl.bind
                              (Impl.VBD.add x
                                 (Args.VBD.Add.__x1___of_rpc __x1__)
                                 (Args.VBD.Add.__x2___of_rpc __x2__))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.VBD.Add.rpc_of_response x))
                        | ("VBD.add", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("VBD.add", 2,
                                   (List.length call.Rpc.params)))
                        | ("VBD.plug", __x1__::__x2__::[]) ->
                            Impl.bind
                              (Impl.VBD.plug x
                                 (Args.VBD.Plug.__x1___of_rpc __x1__)
                                 (Args.VBD.Plug.__x2___of_rpc __x2__))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.VBD.Plug.rpc_of_response x))
                        | ("VBD.plug", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("VBD.plug", 2,
                                   (List.length call.Rpc.params)))
                        | ("VBD.unplug", __x1__::__x2__::__x3__::[]) ->
                            Impl.bind
                              (Impl.VBD.unplug x
                                 (Args.VBD.Unplug.__x1___of_rpc __x1__)
                                 (Args.VBD.Unplug.__x2___of_rpc __x2__)
                                 (Args.VBD.Unplug.__x3___of_rpc __x3__))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.VBD.Unplug.rpc_of_response x))
                        | ("VBD.unplug", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("VBD.unplug", 3,
                                   (List.length call.Rpc.params)))
                        | ("VBD.eject", __x1__::__x2__::[]) ->
                            Impl.bind
                              (Impl.VBD.eject x
                                 (Args.VBD.Eject.__x1___of_rpc __x1__)
                                 (Args.VBD.Eject.__x2___of_rpc __x2__))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.VBD.Eject.rpc_of_response x))
                        | ("VBD.eject", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("VBD.eject", 2,
                                   (List.length call.Rpc.params)))
                        | ("VBD.insert", __x1__::__x2__::__x3__::[]) ->
                            Impl.bind
                              (Impl.VBD.insert x
                                 (Args.VBD.Insert.__x1___of_rpc __x1__)
                                 (Args.VBD.Insert.__x2___of_rpc __x2__)
                                 (Args.VBD.Insert.__x3___of_rpc __x3__))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.VBD.Insert.rpc_of_response x))
                        | ("VBD.insert", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("VBD.insert", 3,
                                   (List.length call.Rpc.params)))
                        | ("VBD.stat", __x1__::__x2__::[]) ->
                            Impl.bind
                              (Impl.VBD.stat x
                                 (Args.VBD.Stat.__x1___of_rpc __x1__)
                                 (Args.VBD.Stat.__x2___of_rpc __x2__))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.VBD.Stat.rpc_of_response x))
                        | ("VBD.stat", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("VBD.stat", 2,
                                   (List.length call.Rpc.params)))
                        | ("VBD.list", __x1__::__x2__::[]) ->
                            Impl.bind
                              (Impl.VBD.list x
                                 (Args.VBD.List.__x1___of_rpc __x1__)
                                 (Args.VBD.List.__x2___of_rpc __x2__))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.VBD.List.rpc_of_response x))
                        | ("VBD.list", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("VBD.list", 2,
                                   (List.length call.Rpc.params)))
                        | ("VBD.remove", __x1__::__x2__::[]) ->
                            Impl.bind
                              (Impl.VBD.remove x
                                 (Args.VBD.Remove.__x1___of_rpc __x1__)
                                 (Args.VBD.Remove.__x2___of_rpc __x2__))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.VBD.Remove.rpc_of_response x))
                        | ("VBD.remove", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("VBD.remove", 2,
                                   (List.length call.Rpc.params)))
                        | ("VUSB.add", __x1__::__x2__::[]) ->
                            Impl.bind
                              (Impl.VUSB.add x
                                 (Args.VUSB.Add.__x1___of_rpc __x1__)
                                 (Args.VUSB.Add.__x2___of_rpc __x2__))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.VUSB.Add.rpc_of_response x))
                        | ("VUSB.add", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("VUSB.add", 2,
                                   (List.length call.Rpc.params)))
                        | ("VUSB.plug", __x1__::__x2__::[]) ->
                            Impl.bind
                              (Impl.VUSB.plug x
                                 (Args.VUSB.Plug.__x1___of_rpc __x1__)
                                 (Args.VUSB.Plug.__x2___of_rpc __x2__))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.VUSB.Plug.rpc_of_response x))
                        | ("VUSB.plug", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("VUSB.plug", 2,
                                   (List.length call.Rpc.params)))
                        | ("VUSB.unplug", __x1__::__x2__::[]) ->
                            Impl.bind
                              (Impl.VUSB.unplug x
                                 (Args.VUSB.Unplug.__x1___of_rpc __x1__)
                                 (Args.VUSB.Unplug.__x2___of_rpc __x2__))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.VUSB.Unplug.rpc_of_response x))
                        | ("VUSB.unplug", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("VUSB.unplug", 2,
                                   (List.length call.Rpc.params)))
                        | ("VUSB.stat", __x1__::__x2__::[]) ->
                            Impl.bind
                              (Impl.VUSB.stat x
                                 (Args.VUSB.Stat.__x1___of_rpc __x1__)
                                 (Args.VUSB.Stat.__x2___of_rpc __x2__))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.VUSB.Stat.rpc_of_response x))
                        | ("VUSB.stat", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("VUSB.stat", 2,
                                   (List.length call.Rpc.params)))
                        | ("VUSB.list", __x1__::__x2__::[]) ->
                            Impl.bind
                              (Impl.VUSB.list x
                                 (Args.VUSB.List.__x1___of_rpc __x1__)
                                 (Args.VUSB.List.__x2___of_rpc __x2__))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.VUSB.List.rpc_of_response x))
                        | ("VUSB.list", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("VUSB.list", 2,
                                   (List.length call.Rpc.params)))
                        | ("VUSB.remove", __x1__::__x2__::[]) ->
                            Impl.bind
                              (Impl.VUSB.remove x
                                 (Args.VUSB.Remove.__x1___of_rpc __x1__)
                                 (Args.VUSB.Remove.__x2___of_rpc __x2__))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.VUSB.Remove.rpc_of_response x))
                        | ("VUSB.remove", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("VUSB.remove", 2,
                                   (List.length call.Rpc.params)))
                        | ("VIF.add", __x1__::__x2__::[]) ->
                            Impl.bind
                              (Impl.VIF.add x
                                 (Args.VIF.Add.__x1___of_rpc __x1__)
                                 (Args.VIF.Add.__x2___of_rpc __x2__))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.VIF.Add.rpc_of_response x))
                        | ("VIF.add", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("VIF.add", 2,
                                   (List.length call.Rpc.params)))
                        | ("VIF.plug", __x1__::__x2__::[]) ->
                            Impl.bind
                              (Impl.VIF.plug x
                                 (Args.VIF.Plug.__x1___of_rpc __x1__)
                                 (Args.VIF.Plug.__x2___of_rpc __x2__))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.VIF.Plug.rpc_of_response x))
                        | ("VIF.plug", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("VIF.plug", 2,
                                   (List.length call.Rpc.params)))
                        | ("VIF.unplug", __x1__::__x2__::__x3__::[]) ->
                            Impl.bind
                              (Impl.VIF.unplug x
                                 (Args.VIF.Unplug.__x1___of_rpc __x1__)
                                 (Args.VIF.Unplug.__x2___of_rpc __x2__)
                                 (Args.VIF.Unplug.__x3___of_rpc __x3__))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.VIF.Unplug.rpc_of_response x))
                        | ("VIF.unplug", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("VIF.unplug", 3,
                                   (List.length call.Rpc.params)))
                        | ("VIF.move", __x1__::__x2__::__x3__::[]) ->
                            Impl.bind
                              (Impl.VIF.move x
                                 (Args.VIF.Move.__x1___of_rpc __x1__)
                                 (Args.VIF.Move.__x2___of_rpc __x2__)
                                 (Args.VIF.Move.__x3___of_rpc __x3__))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.VIF.Move.rpc_of_response x))
                        | ("VIF.move", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("VIF.move", 3,
                                   (List.length call.Rpc.params)))
                        | ("VIF.stat", __x1__::__x2__::[]) ->
                            Impl.bind
                              (Impl.VIF.stat x
                                 (Args.VIF.Stat.__x1___of_rpc __x1__)
                                 (Args.VIF.Stat.__x2___of_rpc __x2__))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.VIF.Stat.rpc_of_response x))
                        | ("VIF.stat", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("VIF.stat", 2,
                                   (List.length call.Rpc.params)))
                        | ("VIF.list", __x1__::__x2__::[]) ->
                            Impl.bind
                              (Impl.VIF.list x
                                 (Args.VIF.List.__x1___of_rpc __x1__)
                                 (Args.VIF.List.__x2___of_rpc __x2__))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.VIF.List.rpc_of_response x))
                        | ("VIF.list", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("VIF.list", 2,
                                   (List.length call.Rpc.params)))
                        | ("VIF.remove", __x1__::__x2__::[]) ->
                            Impl.bind
                              (Impl.VIF.remove x
                                 (Args.VIF.Remove.__x1___of_rpc __x1__)
                                 (Args.VIF.Remove.__x2___of_rpc __x2__))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.VIF.Remove.rpc_of_response x))
                        | ("VIF.remove", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("VIF.remove", 2,
                                   (List.length call.Rpc.params)))
                        | ("VIF.set_carrier", __x1__::__x2__::__x3__::[]) ->
                            Impl.bind
                              (Impl.VIF.set_carrier x
                                 (Args.VIF.Set_carrier.__x1___of_rpc __x1__)
                                 (Args.VIF.Set_carrier.__x2___of_rpc __x2__)
                                 (Args.VIF.Set_carrier.__x3___of_rpc __x3__))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.VIF.Set_carrier.rpc_of_response x))
                        | ("VIF.set_carrier", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("VIF.set_carrier", 3,
                                   (List.length call.Rpc.params)))
                        | ("VIF.set_locking_mode",
                           __x1__::__x2__::__x3__::[]) ->
                            Impl.bind
                              (Impl.VIF.set_locking_mode x
                                 (Args.VIF.Set_locking_mode.__x1___of_rpc
                                    __x1__)
                                 (Args.VIF.Set_locking_mode.__x2___of_rpc
                                    __x2__)
                                 (Args.VIF.Set_locking_mode.__x3___of_rpc
                                    __x3__))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.VIF.Set_locking_mode.rpc_of_response
                                        x))
                        | ("VIF.set_locking_mode", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("VIF.set_locking_mode", 3,
                                   (List.length call.Rpc.params)))
                        | ("VIF.set_ipv4_configuration",
                           __x1__::__x2__::__x3__::[]) ->
                            Impl.bind
                              (Impl.VIF.set_ipv4_configuration x
                                 (Args.VIF.Set_ipv4_configuration.__x1___of_rpc
                                    __x1__)
                                 (Args.VIF.Set_ipv4_configuration.__x2___of_rpc
                                    __x2__)
                                 (Args.VIF.Set_ipv4_configuration.__x3___of_rpc
                                    __x3__))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.VIF.Set_ipv4_configuration.rpc_of_response
                                        x))
                        | ("VIF.set_ipv4_configuration", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("VIF.set_ipv4_configuration", 3,
                                   (List.length call.Rpc.params)))
                        | ("VIF.set_ipv6_configuration",
                           __x1__::__x2__::__x3__::[]) ->
                            Impl.bind
                              (Impl.VIF.set_ipv6_configuration x
                                 (Args.VIF.Set_ipv6_configuration.__x1___of_rpc
                                    __x1__)
                                 (Args.VIF.Set_ipv6_configuration.__x2___of_rpc
                                    __x2__)
                                 (Args.VIF.Set_ipv6_configuration.__x3___of_rpc
                                    __x3__))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.VIF.Set_ipv6_configuration.rpc_of_response
                                        x))
                        | ("VIF.set_ipv6_configuration", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("VIF.set_ipv6_configuration", 3,
                                   (List.length call.Rpc.params)))
                        | ("VIF.set_pvs_proxy", __x1__::__x2__::__x3__::[])
                            ->
                            Impl.bind
                              (Impl.VIF.set_pvs_proxy x
                                 (Args.VIF.Set_pvs_proxy.__x1___of_rpc __x1__)
                                 (Args.VIF.Set_pvs_proxy.__x2___of_rpc __x2__)
                                 (Args.VIF.Set_pvs_proxy.__x3___of_rpc __x3__))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.VIF.Set_pvs_proxy.rpc_of_response
                                        x))
                        | ("VIF.set_pvs_proxy", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("VIF.set_pvs_proxy", 3,
                                   (List.length call.Rpc.params)))
                        | ("VGPU.add", __x1__::__x2__::[]) ->
                            Impl.bind
                              (Impl.VGPU.add x
                                 (Args.VGPU.Add.__x1___of_rpc __x1__)
                                 (Args.VGPU.Add.__x2___of_rpc __x2__))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.VGPU.Add.rpc_of_response x))
                        | ("VGPU.add", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("VGPU.add", 2,
                                   (List.length call.Rpc.params)))
                        | ("VGPU.remove", __x1__::__x2__::[]) ->
                            Impl.bind
                              (Impl.VGPU.remove x
                                 (Args.VGPU.Remove.__x1___of_rpc __x1__)
                                 (Args.VGPU.Remove.__x2___of_rpc __x2__))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.VGPU.Remove.rpc_of_response x))
                        | ("VGPU.remove", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("VGPU.remove", 2,
                                   (List.length call.Rpc.params)))
                        | ("VGPU.stat", __x1__::__x2__::[]) ->
                            Impl.bind
                              (Impl.VGPU.stat x
                                 (Args.VGPU.Stat.__x1___of_rpc __x1__)
                                 (Args.VGPU.Stat.__x2___of_rpc __x2__))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.VGPU.Stat.rpc_of_response x))
                        | ("VGPU.stat", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("VGPU.stat", 2,
                                   (List.length call.Rpc.params)))
                        | ("VGPU.list", __x1__::__x2__::[]) ->
                            Impl.bind
                              (Impl.VGPU.list x
                                 (Args.VGPU.List.__x1___of_rpc __x1__)
                                 (Args.VGPU.List.__x2___of_rpc __x2__))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.VGPU.List.rpc_of_response x))
                        | ("VGPU.list", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("VGPU.list", 2,
                                   (List.length call.Rpc.params)))
                        | ("UPDATES.get", __x1__::__x2__::__x3__::[]) ->
                            Impl.bind
                              (Impl.UPDATES.get x
                                 (Args.UPDATES.Get.__x1___of_rpc __x1__)
                                 (Args.UPDATES.Get.__x2___of_rpc __x2__)
                                 (Args.UPDATES.Get.__x3___of_rpc __x3__))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.UPDATES.Get.rpc_of_response x))
                        | ("UPDATES.get", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("UPDATES.get", 3,
                                   (List.length call.Rpc.params)))
                        | ("UPDATES.last_id", __x1__::[]) ->
                            Impl.bind
                              (Impl.UPDATES.last_id x
                                 (Args.UPDATES.Last_id.__x1___of_rpc __x1__))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.UPDATES.Last_id.rpc_of_response x))
                        | ("UPDATES.last_id", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("UPDATES.last_id", 1,
                                   (List.length call.Rpc.params)))
                        | ("UPDATES.inject_barrier",
                           __x1__::__x2__::__x3__::[]) ->
                            Impl.bind
                              (Impl.UPDATES.inject_barrier x
                                 (Args.UPDATES.Inject_barrier.__x1___of_rpc
                                    __x1__)
                                 (Args.UPDATES.Inject_barrier.__x2___of_rpc
                                    __x2__)
                                 (Args.UPDATES.Inject_barrier.__x3___of_rpc
                                    __x3__))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.UPDATES.Inject_barrier.rpc_of_response
                                        x))
                        | ("UPDATES.inject_barrier", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("UPDATES.inject_barrier", 3,
                                   (List.length call.Rpc.params)))
                        | ("UPDATES.remove_barrier", __x1__::__x2__::[]) ->
                            Impl.bind
                              (Impl.UPDATES.remove_barrier x
                                 (Args.UPDATES.Remove_barrier.__x1___of_rpc
                                    __x1__)
                                 (Args.UPDATES.Remove_barrier.__x2___of_rpc
                                    __x2__))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.UPDATES.Remove_barrier.rpc_of_response
                                        x))
                        | ("UPDATES.remove_barrier", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("UPDATES.remove_barrier", 2,
                                   (List.length call.Rpc.params)))
                        | ("UPDATES.refresh_vm", __x1__::__x2__::[]) ->
                            Impl.bind
                              (Impl.UPDATES.refresh_vm x
                                 (Args.UPDATES.Refresh_vm.__x1___of_rpc
                                    __x1__)
                                 (Args.UPDATES.Refresh_vm.__x2___of_rpc
                                    __x2__))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.UPDATES.Refresh_vm.rpc_of_response
                                        x))
                        | ("UPDATES.refresh_vm", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("UPDATES.refresh_vm", 2,
                                   (List.length call.Rpc.params)))
                        | ("DEBUG.trigger", __x1__::__x2__::__x3__::[]) ->
                            Impl.bind
                              (Impl.DEBUG.trigger x
                                 (Args.DEBUG.Trigger.__x1___of_rpc __x1__)
                                 (Args.DEBUG.Trigger.__x2___of_rpc __x2__)
                                 (Args.DEBUG.Trigger.__x3___of_rpc __x3__))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.DEBUG.Trigger.rpc_of_response x))
                        | ("DEBUG.trigger", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("DEBUG.trigger", 3,
                                   (List.length call.Rpc.params)))
                        | ("DEBUG.shutdown", __x1__::__x2__::[]) ->
                            Impl.bind
                              (Impl.DEBUG.shutdown x
                                 (Args.DEBUG.Shutdown.__x1___of_rpc __x1__)
                                 (Args.DEBUG.Shutdown.__x2___of_rpc __x2__))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.DEBUG.Shutdown.rpc_of_response x))
                        | ("DEBUG.shutdown", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("DEBUG.shutdown", 2,
                                   (List.length call.Rpc.params)))
                        | (x, _) -> Impl.fail (Unknown_RPC x) in
                      Impl.bind contents
                        (function
                         | contents ->
                             Impl.return
                               { Rpc.success = true; Rpc.contents = contents
                               }))
                 (function
                  | e ->
                      Impl.return
                        {
                          Rpc.success = false;
                          Rpc.contents =
                            (Exception.rpc_of_exnty (exnty_of_exn e))
                        }))
  end
module Server(Impl:Server_impl) =
  struct
    module ImplM =
      struct
        type 'a t = 'a
        let bind = function | a -> (function | f -> f a)
        let return = function | a -> a
        let fail = raise
        let handle_failure =
          function | f -> (function | g -> (try f () with | e -> g e))
        include Impl
      end
    module M = (ServerM)(ImplM)
    include M
  end
