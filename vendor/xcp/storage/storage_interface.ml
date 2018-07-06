let service_name = "storage"
let queue_name = ref (Xcp_service.common_prefix ^ service_name)
let default_sockets_dir = "/var/lib/xcp"
let default_path = ref (Filename.concat default_sockets_dir service_name)
let set_sockets_dir =
  function | x -> default_path := (Filename.concat x service_name)
let uri = function | () -> "file:" ^ (!default_path)
type sr = string
let rec rpc_of_sr = function | __x866__ -> Rpc.String __x866__
and sr_of_rpc =
  function
  | __x865__ ->
      (match __x865__ with
       | Rpc.String x -> x
       | __x__ ->
           (if Rpc.get_debug ()
            then
              Printf.eprintf
                "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                "sr" "__x865__" (Rpc.to_string __x__) "String(string)"
            else ();
            raise (Rpc.Runtime_error ("String(string)", __x__))))
type vdi = string
let rec rpc_of_vdi = function | __x868__ -> Rpc.String __x868__
and vdi_of_rpc =
  function
  | __x867__ ->
      (match __x867__ with
       | Rpc.String x -> x
       | __x__ ->
           (if Rpc.get_debug ()
            then
              Printf.eprintf
                "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                "vdi" "__x867__" (Rpc.to_string __x__) "String(string)"
            else ();
            raise (Rpc.Runtime_error ("String(string)", __x__))))
type debug_info = string
let rec rpc_of_debug_info = function | __x870__ -> Rpc.String __x870__
and debug_info_of_rpc =
  function
  | __x869__ ->
      (match __x869__ with
       | Rpc.String x -> x
       | __x__ ->
           (if Rpc.get_debug ()
            then
              Printf.eprintf
                "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                "debug_info" "__x869__" (Rpc.to_string __x__)
                "String(string)"
            else ();
            raise (Rpc.Runtime_error ("String(string)", __x__))))
type attach_info =
  {
  params: string ;
  o_direct: bool ;
  o_direct_reason: string ;
  xenstore_data: (string * string) list }
let rec rpc_of_attach_info =
  function
  | __x878__ ->
      let __x879__ = __x878__.params
      and __x880__ = __x878__.o_direct
      and __x881__ = __x878__.o_direct_reason
      and __x882__ = __x878__.xenstore_data in
      Rpc.Dict
        [("params", (Rpc.String __x879__));
        ("o_direct", (Rpc.Bool __x880__));
        ("o_direct_reason", (Rpc.String __x881__));
        ("xenstore_data",
          ((let dict =
              List.map
                (function | (key, __x883__) -> (key, (Rpc.String __x883__)))
                __x882__ in
            Rpc.Dict dict)))]
and attach_info_of_rpc =
  function
  | __x871__ ->
      (match __x871__ with
       | Rpc.Dict __x872__ ->
           let __x873__ =
             try List.assoc "params" __x872__
             with
             | __x__ ->
                 (if Rpc.get_debug ()
                  then
                    Printf.eprintf
                      "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                      "attach_info" "__x872__" (Printexc.to_string __x__)
                      "Looking for key params"
                  else ();
                  raise
                    (Rpc.Runtime_exception
                       ("Looking for key params", (Printexc.to_string __x__))))
           and __x874__ =
             try List.assoc "o_direct" __x872__
             with
             | __x__ ->
                 (if Rpc.get_debug ()
                  then
                    Printf.eprintf
                      "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                      "attach_info" "__x872__" (Printexc.to_string __x__)
                      "Looking for key o_direct"
                  else ();
                  raise
                    (Rpc.Runtime_exception
                       ("Looking for key o_direct",
                         (Printexc.to_string __x__))))
           and __x875__ =
             try List.assoc "o_direct_reason" __x872__
             with
             | __x__ ->
                 (if Rpc.get_debug ()
                  then
                    Printf.eprintf
                      "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                      "attach_info" "__x872__" (Printexc.to_string __x__)
                      "Looking for key o_direct_reason"
                  else ();
                  raise
                    (Rpc.Runtime_exception
                       ("Looking for key o_direct_reason",
                         (Printexc.to_string __x__))))
           and __x876__ =
             try List.assoc "xenstore_data" __x872__
             with
             | __x__ ->
                 (if Rpc.get_debug ()
                  then
                    Printf.eprintf
                      "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                      "attach_info" "__x872__" (Printexc.to_string __x__)
                      "Looking for key xenstore_data"
                  else ();
                  raise
                    (Rpc.Runtime_exception
                       ("Looking for key xenstore_data",
                         (Printexc.to_string __x__)))) in
           {
             params =
               ((match __x873__ with
                 | Rpc.String x -> x
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                          "attach_info" "__x873__" (Rpc.to_string __x__)
                          "String(string)"
                      else ();
                      raise (Rpc.Runtime_error ("String(string)", __x__)))));
             o_direct =
               ((match __x874__ with
                 | Rpc.Bool x -> x
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                          "attach_info" "__x874__" (Rpc.to_string __x__)
                          "Bool"
                      else ();
                      raise (Rpc.Runtime_error ("Bool", __x__)))));
             o_direct_reason =
               ((match __x875__ with
                 | Rpc.String x -> x
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                          "attach_info" "__x875__" (Rpc.to_string __x__)
                          "String(string)"
                      else ();
                      raise (Rpc.Runtime_error ("String(string)", __x__)))));
             xenstore_data =
               ((match __x876__ with
                 | Rpc.Dict d ->
                     List.map
                       (function
                        | (key, __x877__) ->
                            (key,
                              ((match __x877__ with
                                | Rpc.String x -> x
                                | __x__ ->
                                    (if Rpc.get_debug ()
                                     then
                                       Printf.eprintf
                                         "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                         "attach_info" "__x877__"
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
                          "attach_info" "__x876__" (Rpc.to_string __x__)
                          "Dict"
                      else ();
                      raise (Rpc.Runtime_error ("Dict", __x__)))))
           }
       | __x__ ->
           (if Rpc.get_debug ()
            then
              Printf.eprintf
                "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                "attach_info" "__x871__" (Rpc.to_string __x__) "Dict"
            else ();
            raise (Rpc.Runtime_error ("Dict", __x__))))
type xendisk =
  {
  params: string ;
  extra: (string * string) list ;
  backend_type: string }
let rec rpc_of_xendisk =
  function
  | __x890__ ->
      let __x891__ = __x890__.params
      and __x892__ = __x890__.extra
      and __x893__ = __x890__.backend_type in
      Rpc.Dict
        [("params", (Rpc.String __x891__));
        ("extra",
          ((let dict =
              List.map
                (function | (key, __x894__) -> (key, (Rpc.String __x894__)))
                __x892__ in
            Rpc.Dict dict)));
        ("backend_type", (Rpc.String __x893__))]
and xendisk_of_rpc =
  function
  | __x884__ ->
      (match __x884__ with
       | Rpc.Dict __x885__ ->
           let __x886__ =
             try List.assoc "params" __x885__
             with
             | __x__ ->
                 (if Rpc.get_debug ()
                  then
                    Printf.eprintf
                      "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                      "xendisk" "__x885__" (Printexc.to_string __x__)
                      "Looking for key params"
                  else ();
                  raise
                    (Rpc.Runtime_exception
                       ("Looking for key params", (Printexc.to_string __x__))))
           and __x887__ =
             try List.assoc "extra" __x885__
             with
             | __x__ ->
                 (if Rpc.get_debug ()
                  then
                    Printf.eprintf
                      "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                      "xendisk" "__x885__" (Printexc.to_string __x__)
                      "Looking for key extra"
                  else ();
                  raise
                    (Rpc.Runtime_exception
                       ("Looking for key extra", (Printexc.to_string __x__))))
           and __x888__ =
             try List.assoc "backend_type" __x885__
             with
             | __x__ ->
                 (if Rpc.get_debug ()
                  then
                    Printf.eprintf
                      "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                      "xendisk" "__x885__" (Printexc.to_string __x__)
                      "Looking for key backend_type"
                  else ();
                  raise
                    (Rpc.Runtime_exception
                       ("Looking for key backend_type",
                         (Printexc.to_string __x__)))) in
           {
             params =
               ((match __x886__ with
                 | Rpc.String x -> x
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                          "xendisk" "__x886__" (Rpc.to_string __x__)
                          "String(string)"
                      else ();
                      raise (Rpc.Runtime_error ("String(string)", __x__)))));
             extra =
               ((match __x887__ with
                 | Rpc.Dict d ->
                     List.map
                       (function
                        | (key, __x889__) ->
                            (key,
                              ((match __x889__ with
                                | Rpc.String x -> x
                                | __x__ ->
                                    (if Rpc.get_debug ()
                                     then
                                       Printf.eprintf
                                         "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                         "xendisk" "__x889__"
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
                          "xendisk" "__x887__" (Rpc.to_string __x__) "Dict"
                      else ();
                      raise (Rpc.Runtime_error ("Dict", __x__)))));
             backend_type =
               ((match __x888__ with
                 | Rpc.String x -> x
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                          "xendisk" "__x888__" (Rpc.to_string __x__)
                          "String(string)"
                      else ();
                      raise (Rpc.Runtime_error ("String(string)", __x__)))))
           }
       | __x__ ->
           (if Rpc.get_debug ()
            then
              Printf.eprintf
                "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                "xendisk" "__x884__" (Rpc.to_string __x__) "Dict"
            else ();
            raise (Rpc.Runtime_error ("Dict", __x__))))
type block_device = {
  path: string }
let rec rpc_of_block_device =
  function
  | __x898__ ->
      let __x899__ = __x898__.path in
      Rpc.Dict [("path", (Rpc.String __x899__))]
and block_device_of_rpc =
  function
  | __x895__ ->
      (match __x895__ with
       | Rpc.Dict __x896__ ->
           let __x897__ =
             try List.assoc "path" __x896__
             with
             | __x__ ->
                 (if Rpc.get_debug ()
                  then
                    Printf.eprintf
                      "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                      "block_device" "__x896__" (Printexc.to_string __x__)
                      "Looking for key path"
                  else ();
                  raise
                    (Rpc.Runtime_exception
                       ("Looking for key path", (Printexc.to_string __x__)))) in
           {
             path =
               ((match __x897__ with
                 | Rpc.String x -> x
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                          "block_device" "__x897__" (Rpc.to_string __x__)
                          "String(string)"
                      else ();
                      raise (Rpc.Runtime_error ("String(string)", __x__)))))
           }
       | __x__ ->
           (if Rpc.get_debug ()
            then
              Printf.eprintf
                "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                "block_device" "__x895__" (Rpc.to_string __x__) "Dict"
            else ();
            raise (Rpc.Runtime_error ("Dict", __x__))))
type file = {
  path: string }
let rec rpc_of_file =
  function
  | __x903__ ->
      let __x904__ = __x903__.path in
      Rpc.Dict [("path", (Rpc.String __x904__))]
and file_of_rpc =
  function
  | __x900__ ->
      (match __x900__ with
       | Rpc.Dict __x901__ ->
           let __x902__ =
             try List.assoc "path" __x901__
             with
             | __x__ ->
                 (if Rpc.get_debug ()
                  then
                    Printf.eprintf
                      "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                      "file" "__x901__" (Printexc.to_string __x__)
                      "Looking for key path"
                  else ();
                  raise
                    (Rpc.Runtime_exception
                       ("Looking for key path", (Printexc.to_string __x__)))) in
           {
             path =
               ((match __x902__ with
                 | Rpc.String x -> x
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                          "file" "__x902__" (Rpc.to_string __x__)
                          "String(string)"
                      else ();
                      raise (Rpc.Runtime_error ("String(string)", __x__)))))
           }
       | __x__ ->
           (if Rpc.get_debug ()
            then
              Printf.eprintf
                "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                "file" "__x900__" (Rpc.to_string __x__) "Dict"
            else ();
            raise (Rpc.Runtime_error ("Dict", __x__))))
type nbd = {
  uri: string }
let rec rpc_of_nbd =
  function
  | __x908__ ->
      let __x909__ = __x908__.uri in
      Rpc.Dict [("uri", (Rpc.String __x909__))]
and nbd_of_rpc =
  function
  | __x905__ ->
      (match __x905__ with
       | Rpc.Dict __x906__ ->
           let __x907__ =
             try List.assoc "uri" __x906__
             with
             | __x__ ->
                 (if Rpc.get_debug ()
                  then
                    Printf.eprintf
                      "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                      "nbd" "__x906__" (Printexc.to_string __x__)
                      "Looking for key uri"
                  else ();
                  raise
                    (Rpc.Runtime_exception
                       ("Looking for key uri", (Printexc.to_string __x__)))) in
           {
             uri =
               ((match __x907__ with
                 | Rpc.String x -> x
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                          "nbd" "__x907__" (Rpc.to_string __x__)
                          "String(string)"
                      else ();
                      raise (Rpc.Runtime_error ("String(string)", __x__)))))
           }
       | __x__ ->
           (if Rpc.get_debug ()
            then
              Printf.eprintf
                "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                "nbd" "__x905__" (Rpc.to_string __x__) "Dict"
            else ();
            raise (Rpc.Runtime_error ("Dict", __x__))))
type implementation =
  | XenDisk of xendisk 
  | BlockDevice of block_device 
  | File of file 
  | Nbd of nbd 
let rec rpc_of_implementation =
  function
  | __x915__ ->
      (match __x915__ with
       | Nbd __x916__ -> Rpc.Enum [Rpc.String "Nbd"; rpc_of_nbd __x916__]
       | File __x917__ -> Rpc.Enum [Rpc.String "File"; rpc_of_file __x917__]
       | BlockDevice __x918__ ->
           Rpc.Enum [Rpc.String "BlockDevice"; rpc_of_block_device __x918__]
       | XenDisk __x919__ ->
           Rpc.Enum [Rpc.String "XenDisk"; rpc_of_xendisk __x919__])
and implementation_of_rpc =
  function
  | __x910__ ->
      (match Rpc.lowerfn __x910__ with
       | Rpc.Enum ((Rpc.String "nbd")::__x911__::[]) ->
           Nbd (nbd_of_rpc __x911__)
       | Rpc.Enum ((Rpc.String "file")::__x912__::[]) ->
           File (file_of_rpc __x912__)
       | Rpc.Enum ((Rpc.String "blockdevice")::__x913__::[]) ->
           BlockDevice (block_device_of_rpc __x913__)
       | Rpc.Enum ((Rpc.String "xendisk")::__x914__::[]) ->
           XenDisk (xendisk_of_rpc __x914__)
       | __x__ ->
           (if Rpc.get_debug ()
            then
              Printf.eprintf
                "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                "implementation" "__x910__" (Rpc.to_string __x__)
                "Enum[String s;...]"
            else ();
            raise (Rpc.Runtime_error ("Enum[String s;...]", __x__))))
type backend = {
  implementations: implementation list }
let rec rpc_of_backend =
  function
  | __x925__ ->
      let __x926__ = __x925__.implementations in
      Rpc.Dict
        [("implementations",
           (Rpc.Enum
              (List.map
                 (function | __x927__ -> rpc_of_implementation __x927__)
                 __x926__)))]
and backend_of_rpc =
  function
  | __x920__ ->
      (match __x920__ with
       | Rpc.Dict __x921__ ->
           let __x922__ =
             try List.assoc "implementations" __x921__
             with
             | __x__ ->
                 (if Rpc.get_debug ()
                  then
                    Printf.eprintf
                      "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                      "backend" "__x921__" (Printexc.to_string __x__)
                      "Looking for key implementations"
                  else ();
                  raise
                    (Rpc.Runtime_exception
                       ("Looking for key implementations",
                         (Printexc.to_string __x__)))) in
           {
             implementations =
               ((match __x922__ with
                 | Rpc.Enum __x923__ ->
                     List.map
                       (function | __x924__ -> implementation_of_rpc __x924__)
                       __x923__
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                          "backend" "__x922__" (Rpc.to_string __x__) "List"
                      else ();
                      raise (Rpc.Runtime_error ("List", __x__)))))
           }
       | __x__ ->
           (if Rpc.get_debug ()
            then
              Printf.eprintf
                "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                "backend" "__x920__" (Rpc.to_string __x__) "Dict"
            else ();
            raise (Rpc.Runtime_error ("Dict", __x__))))
let parse_nbd_uri =
  function
  | nbd ->
      let { uri } = nbd in
      let fail =
        function
        | () ->
            failwith
              ("Could not parse NBD URI returned from the storage backend: "
                 ^ uri) in
      (match String.split_on_char ':' uri with
       | "nbd"::"unix"::socket::exportname::[] ->
           let prefix = "exportname=" in
           (if not (Astring.String.is_prefix ~affix:prefix exportname)
            then fail ()
            else ();
            (match Astring.String.cuts ~empty:false ~sep:prefix exportname
             with
             | exportname::[] -> (socket, exportname)
             | _ -> fail ()))
       | _ -> fail ())
type content_id = string
let rec rpc_of_content_id = function | __x929__ -> Rpc.String __x929__
and content_id_of_rpc =
  function
  | __x928__ ->
      (match __x928__ with
       | Rpc.String x -> x
       | __x__ ->
           (if Rpc.get_debug ()
            then
              Printf.eprintf
                "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                "content_id" "__x928__" (Rpc.to_string __x__)
                "String(string)"
            else ();
            raise (Rpc.Runtime_error ("String(string)", __x__))))
type vdi_info =
  {
  vdi: vdi ;
  uuid: string option ;
  content_id: content_id ;
  name_label: string ;
  name_description: string ;
  ty: string ;
  metadata_of_pool: string ;
  is_a_snapshot: bool ;
  snapshot_time: string ;
  snapshot_of: vdi ;
  read_only: bool ;
  cbt_enabled: bool ;
  virtual_size: int64 ;
  physical_utilisation: int64 ;
  persistent: bool ;
  sharable: bool ;
  sm_config: (string * string) list }
let rec rpc_of_vdi_info =
  function
  | __x951__ ->
      let __x952__ = __x951__.vdi
      and __x953__ = __x951__.uuid
      and __x954__ = __x951__.content_id
      and __x955__ = __x951__.name_label
      and __x956__ = __x951__.name_description
      and __x957__ = __x951__.ty
      and __x958__ = __x951__.metadata_of_pool
      and __x959__ = __x951__.is_a_snapshot
      and __x960__ = __x951__.snapshot_time
      and __x961__ = __x951__.snapshot_of
      and __x962__ = __x951__.read_only
      and __x963__ = __x951__.cbt_enabled
      and __x964__ = __x951__.virtual_size
      and __x965__ = __x951__.physical_utilisation
      and __x966__ = __x951__.persistent
      and __x967__ = __x951__.sharable
      and __x968__ = __x951__.sm_config in
      Rpc.Dict (("vdi", (rpc_of_vdi __x952__)) ::
        ((match match __x953__ with
                | Some __x971__ -> Rpc.Enum [Rpc.String __x971__]
                | None -> Rpc.Enum []
          with
          | Rpc.Enum [] ->
              [("content_id", (rpc_of_content_id __x954__));
              ("name_label", (Rpc.String __x955__));
              ("name_description", (Rpc.String __x956__));
              ("ty", (Rpc.String __x957__));
              ("metadata_of_pool", (Rpc.String __x958__));
              ("is_a_snapshot", (Rpc.Bool __x959__));
              ("snapshot_time", (Rpc.String __x960__));
              ("snapshot_of", (rpc_of_vdi __x961__));
              ("read_only", (Rpc.Bool __x962__));
              ("cbt_enabled", (Rpc.Bool __x963__));
              ("virtual_size", (Rpc.Int __x964__));
              ("physical_utilisation", (Rpc.Int __x965__));
              ("persistent", (Rpc.Bool __x966__));
              ("sharable", (Rpc.Bool __x967__));
              ("sm_config",
                ((let dict =
                    List.map
                      (function
                       | (key, __x969__) -> (key, (Rpc.String __x969__)))
                      __x968__ in
                  Rpc.Dict dict)))]
          | Rpc.Enum (__x970__::[]) ->
              [("uuid", __x970__);
              ("content_id", (rpc_of_content_id __x954__));
              ("name_label", (Rpc.String __x955__));
              ("name_description", (Rpc.String __x956__));
              ("ty", (Rpc.String __x957__));
              ("metadata_of_pool", (Rpc.String __x958__));
              ("is_a_snapshot", (Rpc.Bool __x959__));
              ("snapshot_time", (Rpc.String __x960__));
              ("snapshot_of", (rpc_of_vdi __x961__));
              ("read_only", (Rpc.Bool __x962__));
              ("cbt_enabled", (Rpc.Bool __x963__));
              ("virtual_size", (Rpc.Int __x964__));
              ("physical_utilisation", (Rpc.Int __x965__));
              ("persistent", (Rpc.Bool __x966__));
              ("sharable", (Rpc.Bool __x967__));
              ("sm_config",
                ((let dict =
                    List.map
                      (function
                       | (key, __x969__) -> (key, (Rpc.String __x969__)))
                      __x968__ in
                  Rpc.Dict dict)))]
          | _ -> assert false)))
and vdi_info_of_rpc =
  function
  | __x930__ ->
      (match __x930__ with
       | Rpc.Dict __x931__ ->
           let __x932__ =
             try List.assoc "vdi" __x931__
             with
             | __x__ ->
                 (if Rpc.get_debug ()
                  then
                    Printf.eprintf
                      "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                      "vdi_info" "__x931__" (Printexc.to_string __x__)
                      "Looking for key vdi"
                  else ();
                  raise
                    (Rpc.Runtime_exception
                       ("Looking for key vdi", (Printexc.to_string __x__))))
           and __x933__ =
             if List.mem_assoc "uuid" __x931__
             then Rpc.Enum [List.assoc "uuid" __x931__]
             else Rpc.Enum []
           and __x934__ =
             try List.assoc "content_id" __x931__
             with
             | __x__ ->
                 (if Rpc.get_debug ()
                  then
                    Printf.eprintf
                      "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                      "vdi_info" "__x931__" (Printexc.to_string __x__)
                      "Looking for key content_id"
                  else ();
                  raise
                    (Rpc.Runtime_exception
                       ("Looking for key content_id",
                         (Printexc.to_string __x__))))
           and __x935__ =
             try List.assoc "name_label" __x931__
             with
             | __x__ ->
                 (if Rpc.get_debug ()
                  then
                    Printf.eprintf
                      "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                      "vdi_info" "__x931__" (Printexc.to_string __x__)
                      "Looking for key name_label"
                  else ();
                  raise
                    (Rpc.Runtime_exception
                       ("Looking for key name_label",
                         (Printexc.to_string __x__))))
           and __x936__ =
             try List.assoc "name_description" __x931__
             with
             | __x__ ->
                 (if Rpc.get_debug ()
                  then
                    Printf.eprintf
                      "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                      "vdi_info" "__x931__" (Printexc.to_string __x__)
                      "Looking for key name_description"
                  else ();
                  raise
                    (Rpc.Runtime_exception
                       ("Looking for key name_description",
                         (Printexc.to_string __x__))))
           and __x937__ =
             try List.assoc "ty" __x931__
             with
             | __x__ ->
                 (if Rpc.get_debug ()
                  then
                    Printf.eprintf
                      "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                      "vdi_info" "__x931__" (Printexc.to_string __x__)
                      "Looking for key ty"
                  else ();
                  raise
                    (Rpc.Runtime_exception
                       ("Looking for key ty", (Printexc.to_string __x__))))
           and __x938__ =
             try List.assoc "metadata_of_pool" __x931__
             with
             | __x__ ->
                 (if Rpc.get_debug ()
                  then
                    Printf.eprintf
                      "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                      "vdi_info" "__x931__" (Printexc.to_string __x__)
                      "Looking for key metadata_of_pool"
                  else ();
                  raise
                    (Rpc.Runtime_exception
                       ("Looking for key metadata_of_pool",
                         (Printexc.to_string __x__))))
           and __x939__ =
             try List.assoc "is_a_snapshot" __x931__
             with
             | __x__ ->
                 (if Rpc.get_debug ()
                  then
                    Printf.eprintf
                      "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                      "vdi_info" "__x931__" (Printexc.to_string __x__)
                      "Looking for key is_a_snapshot"
                  else ();
                  raise
                    (Rpc.Runtime_exception
                       ("Looking for key is_a_snapshot",
                         (Printexc.to_string __x__))))
           and __x940__ =
             try List.assoc "snapshot_time" __x931__
             with
             | __x__ ->
                 (if Rpc.get_debug ()
                  then
                    Printf.eprintf
                      "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                      "vdi_info" "__x931__" (Printexc.to_string __x__)
                      "Looking for key snapshot_time"
                  else ();
                  raise
                    (Rpc.Runtime_exception
                       ("Looking for key snapshot_time",
                         (Printexc.to_string __x__))))
           and __x941__ =
             try List.assoc "snapshot_of" __x931__
             with
             | __x__ ->
                 (if Rpc.get_debug ()
                  then
                    Printf.eprintf
                      "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                      "vdi_info" "__x931__" (Printexc.to_string __x__)
                      "Looking for key snapshot_of"
                  else ();
                  raise
                    (Rpc.Runtime_exception
                       ("Looking for key snapshot_of",
                         (Printexc.to_string __x__))))
           and __x942__ =
             try List.assoc "read_only" __x931__
             with
             | __x__ ->
                 (if Rpc.get_debug ()
                  then
                    Printf.eprintf
                      "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                      "vdi_info" "__x931__" (Printexc.to_string __x__)
                      "Looking for key read_only"
                  else ();
                  raise
                    (Rpc.Runtime_exception
                       ("Looking for key read_only",
                         (Printexc.to_string __x__))))
           and __x943__ =
             try List.assoc "cbt_enabled" __x931__
             with
             | __x__ ->
                 (if Rpc.get_debug ()
                  then
                    Printf.eprintf
                      "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                      "vdi_info" "__x931__" (Printexc.to_string __x__)
                      "Looking for key cbt_enabled"
                  else ();
                  raise
                    (Rpc.Runtime_exception
                       ("Looking for key cbt_enabled",
                         (Printexc.to_string __x__))))
           and __x944__ =
             try List.assoc "virtual_size" __x931__
             with
             | __x__ ->
                 (if Rpc.get_debug ()
                  then
                    Printf.eprintf
                      "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                      "vdi_info" "__x931__" (Printexc.to_string __x__)
                      "Looking for key virtual_size"
                  else ();
                  raise
                    (Rpc.Runtime_exception
                       ("Looking for key virtual_size",
                         (Printexc.to_string __x__))))
           and __x945__ =
             try List.assoc "physical_utilisation" __x931__
             with
             | __x__ ->
                 (if Rpc.get_debug ()
                  then
                    Printf.eprintf
                      "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                      "vdi_info" "__x931__" (Printexc.to_string __x__)
                      "Looking for key physical_utilisation"
                  else ();
                  raise
                    (Rpc.Runtime_exception
                       ("Looking for key physical_utilisation",
                         (Printexc.to_string __x__))))
           and __x946__ =
             try List.assoc "persistent" __x931__
             with
             | __x__ ->
                 (if Rpc.get_debug ()
                  then
                    Printf.eprintf
                      "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                      "vdi_info" "__x931__" (Printexc.to_string __x__)
                      "Looking for key persistent"
                  else ();
                  raise
                    (Rpc.Runtime_exception
                       ("Looking for key persistent",
                         (Printexc.to_string __x__))))
           and __x947__ =
             try List.assoc "sharable" __x931__
             with
             | __x__ ->
                 (if Rpc.get_debug ()
                  then
                    Printf.eprintf
                      "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                      "vdi_info" "__x931__" (Printexc.to_string __x__)
                      "Looking for key sharable"
                  else ();
                  raise
                    (Rpc.Runtime_exception
                       ("Looking for key sharable",
                         (Printexc.to_string __x__))))
           and __x948__ =
             try List.assoc "sm_config" __x931__
             with
             | __x__ ->
                 (if Rpc.get_debug ()
                  then
                    Printf.eprintf
                      "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                      "vdi_info" "__x931__" (Printexc.to_string __x__)
                      "Looking for key sm_config"
                  else ();
                  raise
                    (Rpc.Runtime_exception
                       ("Looking for key sm_config",
                         (Printexc.to_string __x__)))) in
           {
             vdi = (vdi_of_rpc __x932__);
             uuid =
               ((match __x933__ with
                 | Rpc.Enum [] -> None
                 | Rpc.Enum (__x949__::[]) ->
                     Some
                       ((match __x949__ with
                         | Rpc.String x -> x
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                  "vdi_info" "__x949__" (Rpc.to_string __x__)
                                  "String(string)"
                              else ();
                              raise
                                (Rpc.Runtime_error ("String(string)", __x__)))))
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                          "vdi_info" "__x933__" (Rpc.to_string __x__)
                          "Enum[]/Enum[_]"
                      else ();
                      raise (Rpc.Runtime_error ("Enum[]/Enum[_]", __x__)))));
             content_id = (content_id_of_rpc __x934__);
             name_label =
               ((match __x935__ with
                 | Rpc.String x -> x
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                          "vdi_info" "__x935__" (Rpc.to_string __x__)
                          "String(string)"
                      else ();
                      raise (Rpc.Runtime_error ("String(string)", __x__)))));
             name_description =
               ((match __x936__ with
                 | Rpc.String x -> x
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                          "vdi_info" "__x936__" (Rpc.to_string __x__)
                          "String(string)"
                      else ();
                      raise (Rpc.Runtime_error ("String(string)", __x__)))));
             ty =
               ((match __x937__ with
                 | Rpc.String x -> x
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                          "vdi_info" "__x937__" (Rpc.to_string __x__)
                          "String(string)"
                      else ();
                      raise (Rpc.Runtime_error ("String(string)", __x__)))));
             metadata_of_pool =
               ((match __x938__ with
                 | Rpc.String x -> x
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                          "vdi_info" "__x938__" (Rpc.to_string __x__)
                          "String(string)"
                      else ();
                      raise (Rpc.Runtime_error ("String(string)", __x__)))));
             is_a_snapshot =
               ((match __x939__ with
                 | Rpc.Bool x -> x
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                          "vdi_info" "__x939__" (Rpc.to_string __x__) "Bool"
                      else ();
                      raise (Rpc.Runtime_error ("Bool", __x__)))));
             snapshot_time =
               ((match __x940__ with
                 | Rpc.String x -> x
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                          "vdi_info" "__x940__" (Rpc.to_string __x__)
                          "String(string)"
                      else ();
                      raise (Rpc.Runtime_error ("String(string)", __x__)))));
             snapshot_of = (vdi_of_rpc __x941__);
             read_only =
               ((match __x942__ with
                 | Rpc.Bool x -> x
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                          "vdi_info" "__x942__" (Rpc.to_string __x__) "Bool"
                      else ();
                      raise (Rpc.Runtime_error ("Bool", __x__)))));
             cbt_enabled =
               ((match __x943__ with
                 | Rpc.Bool x -> x
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                          "vdi_info" "__x943__" (Rpc.to_string __x__) "Bool"
                      else ();
                      raise (Rpc.Runtime_error ("Bool", __x__)))));
             virtual_size =
               ((match __x944__ with
                 | Rpc.Int x -> x
                 | Rpc.String s -> Int64.of_string s
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                          "vdi_info" "__x944__" (Rpc.to_string __x__)
                          "Int(int64)"
                      else ();
                      raise (Rpc.Runtime_error ("Int(int64)", __x__)))));
             physical_utilisation =
               ((match __x945__ with
                 | Rpc.Int x -> x
                 | Rpc.String s -> Int64.of_string s
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                          "vdi_info" "__x945__" (Rpc.to_string __x__)
                          "Int(int64)"
                      else ();
                      raise (Rpc.Runtime_error ("Int(int64)", __x__)))));
             persistent =
               ((match __x946__ with
                 | Rpc.Bool x -> x
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                          "vdi_info" "__x946__" (Rpc.to_string __x__) "Bool"
                      else ();
                      raise (Rpc.Runtime_error ("Bool", __x__)))));
             sharable =
               ((match __x947__ with
                 | Rpc.Bool x -> x
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                          "vdi_info" "__x947__" (Rpc.to_string __x__) "Bool"
                      else ();
                      raise (Rpc.Runtime_error ("Bool", __x__)))));
             sm_config =
               ((match __x948__ with
                 | Rpc.Dict d ->
                     List.map
                       (function
                        | (key, __x950__) ->
                            (key,
                              ((match __x950__ with
                                | Rpc.String x -> x
                                | __x__ ->
                                    (if Rpc.get_debug ()
                                     then
                                       Printf.eprintf
                                         "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                         "vdi_info" "__x950__"
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
                          "vdi_info" "__x948__" (Rpc.to_string __x__) "Dict"
                      else ();
                      raise (Rpc.Runtime_error ("Dict", __x__)))))
           }
       | __x__ ->
           (if Rpc.get_debug ()
            then
              Printf.eprintf
                "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                "vdi_info" "__x930__" (Rpc.to_string __x__) "Dict"
            else ();
            raise (Rpc.Runtime_error ("Dict", __x__))))
let default_vdi_info =
  {
    vdi = "";
    uuid = None;
    content_id = "";
    name_label = "";
    name_description = "";
    ty = "user";
    metadata_of_pool = "";
    is_a_snapshot = false;
    snapshot_time =
      (Xapi_stdext_date.Date.to_string Xapi_stdext_date.Date.never);
    snapshot_of = "";
    read_only = false;
    cbt_enabled = false;
    virtual_size = 0L;
    physical_utilisation = 0L;
    persistent = true;
    sharable = false;
    sm_config = []
  }
let vdi_info_of_rpc =
  function
  | rpc ->
      (Rpc.struct_extend rpc (rpc_of_vdi_info default_vdi_info)) |>
        vdi_info_of_rpc
type sr_health =
  | Healthy 
  | Recovering 
let rec rpc_of_sr_health =
  function
  | __x973__ ->
      (match __x973__ with
       | Recovering -> Rpc.String "Recovering"
       | Healthy -> Rpc.String "Healthy")
and sr_health_of_rpc =
  function
  | __x972__ ->
      (match Rpc.lowerfn __x972__ with
       | Rpc.String "recovering" -> Recovering
       | Rpc.String "healthy" -> Healthy
       | __x__ ->
           (if Rpc.get_debug ()
            then
              Printf.eprintf
                "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                "sr_health" "__x972__" (Rpc.to_string __x__)
                "Enum[String s;...]"
            else ();
            raise (Rpc.Runtime_error ("Enum[String s;...]", __x__))))
type sr_info =
  {
  sr_uuid: string option ;
  name_label: string ;
  name_description: string ;
  total_space: int64 ;
  free_space: int64 ;
  clustered: bool ;
  health: sr_health }
let rec rpc_of_sr_info =
  function
  | __x984__ ->
      let __x985__ = __x984__.sr_uuid
      and __x986__ = __x984__.name_label
      and __x987__ = __x984__.name_description
      and __x988__ = __x984__.total_space
      and __x989__ = __x984__.free_space
      and __x990__ = __x984__.clustered
      and __x991__ = __x984__.health in
      Rpc.Dict
        ((match match __x985__ with
                | Some __x993__ -> Rpc.Enum [Rpc.String __x993__]
                | None -> Rpc.Enum []
          with
          | Rpc.Enum [] ->
              [("name_label", (Rpc.String __x986__));
              ("name_description", (Rpc.String __x987__));
              ("total_space", (Rpc.Int __x988__));
              ("free_space", (Rpc.Int __x989__));
              ("clustered", (Rpc.Bool __x990__));
              ("health", (rpc_of_sr_health __x991__))]
          | Rpc.Enum (__x992__::[]) ->
              [("sr_uuid", __x992__);
              ("name_label", (Rpc.String __x986__));
              ("name_description", (Rpc.String __x987__));
              ("total_space", (Rpc.Int __x988__));
              ("free_space", (Rpc.Int __x989__));
              ("clustered", (Rpc.Bool __x990__));
              ("health", (rpc_of_sr_health __x991__))]
          | _ -> assert false))
and sr_info_of_rpc =
  function
  | __x974__ ->
      (match __x974__ with
       | Rpc.Dict __x975__ ->
           let __x976__ =
             if List.mem_assoc "sr_uuid" __x975__
             then Rpc.Enum [List.assoc "sr_uuid" __x975__]
             else Rpc.Enum []
           and __x977__ =
             try List.assoc "name_label" __x975__
             with
             | __x__ ->
                 (if Rpc.get_debug ()
                  then
                    Printf.eprintf
                      "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                      "sr_info" "__x975__" (Printexc.to_string __x__)
                      "Looking for key name_label"
                  else ();
                  raise
                    (Rpc.Runtime_exception
                       ("Looking for key name_label",
                         (Printexc.to_string __x__))))
           and __x978__ =
             try List.assoc "name_description" __x975__
             with
             | __x__ ->
                 (if Rpc.get_debug ()
                  then
                    Printf.eprintf
                      "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                      "sr_info" "__x975__" (Printexc.to_string __x__)
                      "Looking for key name_description"
                  else ();
                  raise
                    (Rpc.Runtime_exception
                       ("Looking for key name_description",
                         (Printexc.to_string __x__))))
           and __x979__ =
             try List.assoc "total_space" __x975__
             with
             | __x__ ->
                 (if Rpc.get_debug ()
                  then
                    Printf.eprintf
                      "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                      "sr_info" "__x975__" (Printexc.to_string __x__)
                      "Looking for key total_space"
                  else ();
                  raise
                    (Rpc.Runtime_exception
                       ("Looking for key total_space",
                         (Printexc.to_string __x__))))
           and __x980__ =
             try List.assoc "free_space" __x975__
             with
             | __x__ ->
                 (if Rpc.get_debug ()
                  then
                    Printf.eprintf
                      "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                      "sr_info" "__x975__" (Printexc.to_string __x__)
                      "Looking for key free_space"
                  else ();
                  raise
                    (Rpc.Runtime_exception
                       ("Looking for key free_space",
                         (Printexc.to_string __x__))))
           and __x981__ =
             try List.assoc "clustered" __x975__
             with
             | __x__ ->
                 (if Rpc.get_debug ()
                  then
                    Printf.eprintf
                      "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                      "sr_info" "__x975__" (Printexc.to_string __x__)
                      "Looking for key clustered"
                  else ();
                  raise
                    (Rpc.Runtime_exception
                       ("Looking for key clustered",
                         (Printexc.to_string __x__))))
           and __x982__ =
             try List.assoc "health" __x975__
             with
             | __x__ ->
                 (if Rpc.get_debug ()
                  then
                    Printf.eprintf
                      "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                      "sr_info" "__x975__" (Printexc.to_string __x__)
                      "Looking for key health"
                  else ();
                  raise
                    (Rpc.Runtime_exception
                       ("Looking for key health", (Printexc.to_string __x__)))) in
           {
             sr_uuid =
               ((match __x976__ with
                 | Rpc.Enum [] -> None
                 | Rpc.Enum (__x983__::[]) ->
                     Some
                       ((match __x983__ with
                         | Rpc.String x -> x
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                  "sr_info" "__x983__" (Rpc.to_string __x__)
                                  "String(string)"
                              else ();
                              raise
                                (Rpc.Runtime_error ("String(string)", __x__)))))
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                          "sr_info" "__x976__" (Rpc.to_string __x__)
                          "Enum[]/Enum[_]"
                      else ();
                      raise (Rpc.Runtime_error ("Enum[]/Enum[_]", __x__)))));
             name_label =
               ((match __x977__ with
                 | Rpc.String x -> x
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                          "sr_info" "__x977__" (Rpc.to_string __x__)
                          "String(string)"
                      else ();
                      raise (Rpc.Runtime_error ("String(string)", __x__)))));
             name_description =
               ((match __x978__ with
                 | Rpc.String x -> x
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                          "sr_info" "__x978__" (Rpc.to_string __x__)
                          "String(string)"
                      else ();
                      raise (Rpc.Runtime_error ("String(string)", __x__)))));
             total_space =
               ((match __x979__ with
                 | Rpc.Int x -> x
                 | Rpc.String s -> Int64.of_string s
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                          "sr_info" "__x979__" (Rpc.to_string __x__)
                          "Int(int64)"
                      else ();
                      raise (Rpc.Runtime_error ("Int(int64)", __x__)))));
             free_space =
               ((match __x980__ with
                 | Rpc.Int x -> x
                 | Rpc.String s -> Int64.of_string s
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                          "sr_info" "__x980__" (Rpc.to_string __x__)
                          "Int(int64)"
                      else ();
                      raise (Rpc.Runtime_error ("Int(int64)", __x__)))));
             clustered =
               ((match __x981__ with
                 | Rpc.Bool x -> x
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                          "sr_info" "__x981__" (Rpc.to_string __x__) "Bool"
                      else ();
                      raise (Rpc.Runtime_error ("Bool", __x__)))));
             health = (sr_health_of_rpc __x982__)
           }
       | __x__ ->
           (if Rpc.get_debug ()
            then
              Printf.eprintf
                "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                "sr_info" "__x974__" (Rpc.to_string __x__) "Dict"
            else ();
            raise (Rpc.Runtime_error ("Dict", __x__))))
let string_of_vdi_info =
  function | (x : vdi_info) -> Jsonrpc.to_string (rpc_of_vdi_info x)
type dp = string
let rec rpc_of_dp = function | __x995__ -> Rpc.String __x995__
and dp_of_rpc =
  function
  | __x994__ ->
      (match __x994__ with
       | Rpc.String x -> x
       | __x__ ->
           (if Rpc.get_debug ()
            then
              Printf.eprintf
                "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                "dp" "__x994__" (Rpc.to_string __x__) "String(string)"
            else ();
            raise (Rpc.Runtime_error ("String(string)", __x__))))
type dp_stat_t =
  {
  superstate: Vdi_automaton.state ;
  dps: (string * Vdi_automaton.state) list }
let rec rpc_of_dp_stat_t =
  function
  | __x1001__ ->
      let __x1002__ = __x1001__.superstate
      and __x1003__ = __x1001__.dps in
      Rpc.Dict
        [("superstate", (Vdi_automaton.rpc_of_state __x1002__));
        ("dps",
          ((let dict =
              List.map
                (function
                 | (key, __x1004__) ->
                     (key, (Vdi_automaton.rpc_of_state __x1004__))) __x1003__ in
            Rpc.Dict dict)))]
and dp_stat_t_of_rpc =
  function
  | __x996__ ->
      (match __x996__ with
       | Rpc.Dict __x997__ ->
           let __x998__ =
             try List.assoc "superstate" __x997__
             with
             | __x__ ->
                 (if Rpc.get_debug ()
                  then
                    Printf.eprintf
                      "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                      "dp_stat_t" "__x997__" (Printexc.to_string __x__)
                      "Looking for key superstate"
                  else ();
                  raise
                    (Rpc.Runtime_exception
                       ("Looking for key superstate",
                         (Printexc.to_string __x__))))
           and __x999__ =
             try List.assoc "dps" __x997__
             with
             | __x__ ->
                 (if Rpc.get_debug ()
                  then
                    Printf.eprintf
                      "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                      "dp_stat_t" "__x997__" (Printexc.to_string __x__)
                      "Looking for key dps"
                  else ();
                  raise
                    (Rpc.Runtime_exception
                       ("Looking for key dps", (Printexc.to_string __x__)))) in
           {
             superstate = (Vdi_automaton.state_of_rpc __x998__);
             dps =
               ((match __x999__ with
                 | Rpc.Dict d ->
                     List.map
                       (function
                        | (key, __x1000__) ->
                            (key, (Vdi_automaton.state_of_rpc __x1000__))) d
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                          "dp_stat_t" "__x999__" (Rpc.to_string __x__) "Dict"
                      else ();
                      raise (Rpc.Runtime_error ("Dict", __x__)))))
           }
       | __x__ ->
           (if Rpc.get_debug ()
            then
              Printf.eprintf
                "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                "dp_stat_t" "__x996__" (Rpc.to_string __x__) "Dict"
            else ();
            raise (Rpc.Runtime_error ("Dict", __x__))))
let string_of_dp_stat_t =
  function | (x : dp_stat_t) -> Jsonrpc.to_string (rpc_of_dp_stat_t x)
type probe =
  {
  configuration: (string * string) list ;
  complete: bool ;
  sr: sr_info option ;
  extra_info: (string * string) list }
let rec rpc_of_probe =
  function
  | __x1014__ ->
      let __x1015__ = __x1014__.configuration
      and __x1016__ = __x1014__.complete
      and __x1017__ = __x1014__.sr
      and __x1018__ = __x1014__.extra_info in
      Rpc.Dict
        (("configuration",
           (let dict =
              List.map
                (function | (key, __x1022__) -> (key, (Rpc.String __x1022__)))
                __x1015__ in
            Rpc.Dict dict))
        :: ("complete", (Rpc.Bool __x1016__)) ::
        ((match match __x1017__ with
                | Some __x1021__ -> Rpc.Enum [rpc_of_sr_info __x1021__]
                | None -> Rpc.Enum []
          with
          | Rpc.Enum [] ->
              [("extra_info",
                 ((let dict =
                     List.map
                       (function
                        | (key, __x1019__) -> (key, (Rpc.String __x1019__)))
                       __x1018__ in
                   Rpc.Dict dict)))]
          | Rpc.Enum (__x1020__::[]) ->
              [("sr", __x1020__);
              ("extra_info",
                ((let dict =
                    List.map
                      (function
                       | (key, __x1019__) -> (key, (Rpc.String __x1019__)))
                      __x1018__ in
                  Rpc.Dict dict)))]
          | _ -> assert false)))
and probe_of_rpc =
  function
  | __x1005__ ->
      (match __x1005__ with
       | Rpc.Dict __x1006__ ->
           let __x1007__ =
             try List.assoc "configuration" __x1006__
             with
             | __x__ ->
                 (if Rpc.get_debug ()
                  then
                    Printf.eprintf
                      "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                      "probe" "__x1006__" (Printexc.to_string __x__)
                      "Looking for key configuration"
                  else ();
                  raise
                    (Rpc.Runtime_exception
                       ("Looking for key configuration",
                         (Printexc.to_string __x__))))
           and __x1008__ =
             try List.assoc "complete" __x1006__
             with
             | __x__ ->
                 (if Rpc.get_debug ()
                  then
                    Printf.eprintf
                      "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                      "probe" "__x1006__" (Printexc.to_string __x__)
                      "Looking for key complete"
                  else ();
                  raise
                    (Rpc.Runtime_exception
                       ("Looking for key complete",
                         (Printexc.to_string __x__))))
           and __x1009__ =
             if List.mem_assoc "sr" __x1006__
             then Rpc.Enum [List.assoc "sr" __x1006__]
             else Rpc.Enum []
           and __x1010__ =
             try List.assoc "extra_info" __x1006__
             with
             | __x__ ->
                 (if Rpc.get_debug ()
                  then
                    Printf.eprintf
                      "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                      "probe" "__x1006__" (Printexc.to_string __x__)
                      "Looking for key extra_info"
                  else ();
                  raise
                    (Rpc.Runtime_exception
                       ("Looking for key extra_info",
                         (Printexc.to_string __x__)))) in
           {
             configuration =
               ((match __x1007__ with
                 | Rpc.Dict d ->
                     List.map
                       (function
                        | (key, __x1011__) ->
                            (key,
                              ((match __x1011__ with
                                | Rpc.String x -> x
                                | __x__ ->
                                    (if Rpc.get_debug ()
                                     then
                                       Printf.eprintf
                                         "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                         "probe" "__x1011__"
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
                          "probe" "__x1007__" (Rpc.to_string __x__) "Dict"
                      else ();
                      raise (Rpc.Runtime_error ("Dict", __x__)))));
             complete =
               ((match __x1008__ with
                 | Rpc.Bool x -> x
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                          "probe" "__x1008__" (Rpc.to_string __x__) "Bool"
                      else ();
                      raise (Rpc.Runtime_error ("Bool", __x__)))));
             sr =
               ((match __x1009__ with
                 | Rpc.Enum [] -> None
                 | Rpc.Enum (__x1012__::[]) ->
                     Some (sr_info_of_rpc __x1012__)
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                          "probe" "__x1009__" (Rpc.to_string __x__)
                          "Enum[]/Enum[_]"
                      else ();
                      raise (Rpc.Runtime_error ("Enum[]/Enum[_]", __x__)))));
             extra_info =
               ((match __x1010__ with
                 | Rpc.Dict d ->
                     List.map
                       (function
                        | (key, __x1013__) ->
                            (key,
                              ((match __x1013__ with
                                | Rpc.String x -> x
                                | __x__ ->
                                    (if Rpc.get_debug ()
                                     then
                                       Printf.eprintf
                                         "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                         "probe" "__x1013__"
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
                          "probe" "__x1010__" (Rpc.to_string __x__) "Dict"
                      else ();
                      raise (Rpc.Runtime_error ("Dict", __x__)))))
           }
       | __x__ ->
           (if Rpc.get_debug ()
            then
              Printf.eprintf
                "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                "probe" "__x1005__" (Rpc.to_string __x__) "Dict"
            else ();
            raise (Rpc.Runtime_error ("Dict", __x__))))
type probe_result =
  | Raw of string 
  | Probe of probe list 
let rec rpc_of_probe_result =
  function
  | __x1028__ ->
      (match __x1028__ with
       | Probe __x1029__ ->
           Rpc.Enum
             [Rpc.String "Probe";
             Rpc.Enum
               (List.map (function | __x1030__ -> rpc_of_probe __x1030__)
                  __x1029__)]
       | Raw __x1031__ -> Rpc.Enum [Rpc.String "Raw"; Rpc.String __x1031__])
and probe_result_of_rpc =
  function
  | __x1023__ ->
      (match Rpc.lowerfn __x1023__ with
       | Rpc.Enum ((Rpc.String "probe")::__x1024__::[]) ->
           Probe
             ((match __x1024__ with
               | Rpc.Enum __x1025__ ->
                   List.map (function | __x1026__ -> probe_of_rpc __x1026__)
                     __x1025__
               | __x__ ->
                   (if Rpc.get_debug ()
                    then
                      Printf.eprintf
                        "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                        "probe_result" "__x1024__" (Rpc.to_string __x__)
                        "List"
                    else ();
                    raise (Rpc.Runtime_error ("List", __x__)))))
       | Rpc.Enum ((Rpc.String "raw")::__x1027__::[]) ->
           Raw
             ((match __x1027__ with
               | Rpc.String x -> x
               | __x__ ->
                   (if Rpc.get_debug ()
                    then
                      Printf.eprintf
                        "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                        "probe_result" "__x1027__" (Rpc.to_string __x__)
                        "String(string)"
                    else ();
                    raise (Rpc.Runtime_error ("String(string)", __x__)))))
       | __x__ ->
           (if Rpc.get_debug ()
            then
              Printf.eprintf
                "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                "probe_result" "__x1023__" (Rpc.to_string __x__)
                "Enum[String s;...]"
            else ();
            raise (Rpc.Runtime_error ("Enum[String s;...]", __x__))))
module Mirror =
  struct
    type id = string
    let rec rpc_of_id = function | __x1033__ -> Rpc.String __x1033__
    and id_of_rpc =
      function
      | __x1032__ ->
          (match __x1032__ with
           | Rpc.String x -> x
           | __x__ ->
               (if Rpc.get_debug ()
                then
                  Printf.eprintf
                    "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                    "id" "__x1032__" (Rpc.to_string __x__) "String(string)"
                else ();
                raise (Rpc.Runtime_error ("String(string)", __x__))))
    type state =
      | Receiving 
      | Sending 
      | Copying 
    let rec rpc_of_state =
      function
      | __x1035__ ->
          (match __x1035__ with
           | Copying -> Rpc.String "Copying"
           | Sending -> Rpc.String "Sending"
           | Receiving -> Rpc.String "Receiving")
    and state_of_rpc =
      function
      | __x1034__ ->
          (match Rpc.lowerfn __x1034__ with
           | Rpc.String "copying" -> Copying
           | Rpc.String "sending" -> Sending
           | Rpc.String "receiving" -> Receiving
           | __x__ ->
               (if Rpc.get_debug ()
                then
                  Printf.eprintf
                    "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                    "state" "__x1034__" (Rpc.to_string __x__)
                    "Enum[String s;...]"
                else ();
                raise (Rpc.Runtime_error ("Enum[String s;...]", __x__))))
    type t =
      {
      source_vdi: vdi ;
      dest_vdi: vdi ;
      state: state list ;
      failed: bool }
    let rec rpc_of_t =
      function
      | __x1044__ ->
          let __x1045__ = __x1044__.source_vdi
          and __x1046__ = __x1044__.dest_vdi
          and __x1047__ = __x1044__.state
          and __x1048__ = __x1044__.failed in
          Rpc.Dict
            [("source_vdi", (rpc_of_vdi __x1045__));
            ("dest_vdi", (rpc_of_vdi __x1046__));
            ("state",
              (Rpc.Enum
                 (List.map (function | __x1049__ -> rpc_of_state __x1049__)
                    __x1047__)));
            ("failed", (Rpc.Bool __x1048__))]
    and t_of_rpc =
      function
      | __x1036__ ->
          (match __x1036__ with
           | Rpc.Dict __x1037__ ->
               let __x1038__ =
                 try List.assoc "source_vdi" __x1037__
                 with
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                          "t" "__x1037__" (Printexc.to_string __x__)
                          "Looking for key source_vdi"
                      else ();
                      raise
                        (Rpc.Runtime_exception
                           ("Looking for key source_vdi",
                             (Printexc.to_string __x__))))
               and __x1039__ =
                 try List.assoc "dest_vdi" __x1037__
                 with
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                          "t" "__x1037__" (Printexc.to_string __x__)
                          "Looking for key dest_vdi"
                      else ();
                      raise
                        (Rpc.Runtime_exception
                           ("Looking for key dest_vdi",
                             (Printexc.to_string __x__))))
               and __x1040__ =
                 try List.assoc "state" __x1037__
                 with
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                          "t" "__x1037__" (Printexc.to_string __x__)
                          "Looking for key state"
                      else ();
                      raise
                        (Rpc.Runtime_exception
                           ("Looking for key state",
                             (Printexc.to_string __x__))))
               and __x1041__ =
                 try List.assoc "failed" __x1037__
                 with
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                          "t" "__x1037__" (Printexc.to_string __x__)
                          "Looking for key failed"
                      else ();
                      raise
                        (Rpc.Runtime_exception
                           ("Looking for key failed",
                             (Printexc.to_string __x__)))) in
               {
                 source_vdi = (vdi_of_rpc __x1038__);
                 dest_vdi = (vdi_of_rpc __x1039__);
                 state =
                   ((match __x1040__ with
                     | Rpc.Enum __x1042__ ->
                         List.map
                           (function | __x1043__ -> state_of_rpc __x1043__)
                           __x1042__
                     | __x__ ->
                         (if Rpc.get_debug ()
                          then
                            Printf.eprintf
                              "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                              "t" "__x1040__" (Rpc.to_string __x__) "List"
                          else ();
                          raise (Rpc.Runtime_error ("List", __x__)))));
                 failed =
                   ((match __x1041__ with
                     | Rpc.Bool x -> x
                     | __x__ ->
                         (if Rpc.get_debug ()
                          then
                            Printf.eprintf
                              "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                              "t" "__x1041__" (Rpc.to_string __x__) "Bool"
                          else ();
                          raise (Rpc.Runtime_error ("Bool", __x__)))))
               }
           | __x__ ->
               (if Rpc.get_debug ()
                then
                  Printf.eprintf
                    "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                    "t" "__x1036__" (Rpc.to_string __x__) "Dict"
                else ();
                raise (Rpc.Runtime_error ("Dict", __x__))))
    type mirror_receive_result_vhd_t =
      {
      mirror_vdi: vdi_info ;
      mirror_datapath: dp ;
      copy_diffs_from: content_id option ;
      copy_diffs_to: vdi ;
      dummy_vdi: vdi }
    let rec rpc_of_mirror_receive_result_vhd_t =
      function
      | __x1058__ ->
          let __x1059__ = __x1058__.mirror_vdi
          and __x1060__ = __x1058__.mirror_datapath
          and __x1061__ = __x1058__.copy_diffs_from
          and __x1062__ = __x1058__.copy_diffs_to
          and __x1063__ = __x1058__.dummy_vdi in
          Rpc.Dict (("mirror_vdi", (rpc_of_vdi_info __x1059__)) ::
            ("mirror_datapath", (rpc_of_dp __x1060__)) ::
            ((match match __x1061__ with
                    | Some __x1065__ ->
                        Rpc.Enum [rpc_of_content_id __x1065__]
                    | None -> Rpc.Enum []
              with
              | Rpc.Enum [] ->
                  [("copy_diffs_to", (rpc_of_vdi __x1062__));
                  ("dummy_vdi", (rpc_of_vdi __x1063__))]
              | Rpc.Enum (__x1064__::[]) ->
                  [("copy_diffs_from", __x1064__);
                  ("copy_diffs_to", (rpc_of_vdi __x1062__));
                  ("dummy_vdi", (rpc_of_vdi __x1063__))]
              | _ -> assert false)))
    and mirror_receive_result_vhd_t_of_rpc =
      function
      | __x1050__ ->
          (match __x1050__ with
           | Rpc.Dict __x1051__ ->
               let __x1052__ =
                 try List.assoc "mirror_vdi" __x1051__
                 with
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                          "mirror_receive_result_vhd_t" "__x1051__"
                          (Printexc.to_string __x__)
                          "Looking for key mirror_vdi"
                      else ();
                      raise
                        (Rpc.Runtime_exception
                           ("Looking for key mirror_vdi",
                             (Printexc.to_string __x__))))
               and __x1053__ =
                 try List.assoc "mirror_datapath" __x1051__
                 with
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                          "mirror_receive_result_vhd_t" "__x1051__"
                          (Printexc.to_string __x__)
                          "Looking for key mirror_datapath"
                      else ();
                      raise
                        (Rpc.Runtime_exception
                           ("Looking for key mirror_datapath",
                             (Printexc.to_string __x__))))
               and __x1054__ =
                 if List.mem_assoc "copy_diffs_from" __x1051__
                 then Rpc.Enum [List.assoc "copy_diffs_from" __x1051__]
                 else Rpc.Enum []
               and __x1055__ =
                 try List.assoc "copy_diffs_to" __x1051__
                 with
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                          "mirror_receive_result_vhd_t" "__x1051__"
                          (Printexc.to_string __x__)
                          "Looking for key copy_diffs_to"
                      else ();
                      raise
                        (Rpc.Runtime_exception
                           ("Looking for key copy_diffs_to",
                             (Printexc.to_string __x__))))
               and __x1056__ =
                 try List.assoc "dummy_vdi" __x1051__
                 with
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                          "mirror_receive_result_vhd_t" "__x1051__"
                          (Printexc.to_string __x__)
                          "Looking for key dummy_vdi"
                      else ();
                      raise
                        (Rpc.Runtime_exception
                           ("Looking for key dummy_vdi",
                             (Printexc.to_string __x__)))) in
               {
                 mirror_vdi = (vdi_info_of_rpc __x1052__);
                 mirror_datapath = (dp_of_rpc __x1053__);
                 copy_diffs_from =
                   ((match __x1054__ with
                     | Rpc.Enum [] -> None
                     | Rpc.Enum (__x1057__::[]) ->
                         Some (content_id_of_rpc __x1057__)
                     | __x__ ->
                         (if Rpc.get_debug ()
                          then
                            Printf.eprintf
                              "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                              "mirror_receive_result_vhd_t" "__x1054__"
                              (Rpc.to_string __x__) "Enum[]/Enum[_]"
                          else ();
                          raise (Rpc.Runtime_error ("Enum[]/Enum[_]", __x__)))));
                 copy_diffs_to = (vdi_of_rpc __x1055__);
                 dummy_vdi = (vdi_of_rpc __x1056__)
               }
           | __x__ ->
               (if Rpc.get_debug ()
                then
                  Printf.eprintf
                    "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                    "mirror_receive_result_vhd_t" "__x1050__"
                    (Rpc.to_string __x__) "Dict"
                else ();
                raise (Rpc.Runtime_error ("Dict", __x__))))
    type mirror_receive_result =
      | Vhd_mirror of mirror_receive_result_vhd_t 
    let rec rpc_of_mirror_receive_result =
      function
      | __x1068__ ->
          (match __x1068__ with
           | Vhd_mirror __x1069__ ->
               Rpc.Enum
                 [Rpc.String "Vhd_mirror";
                 rpc_of_mirror_receive_result_vhd_t __x1069__])
    and mirror_receive_result_of_rpc =
      function
      | __x1066__ ->
          (match Rpc.lowerfn __x1066__ with
           | Rpc.Enum ((Rpc.String "vhd_mirror")::__x1067__::[]) ->
               Vhd_mirror (mirror_receive_result_vhd_t_of_rpc __x1067__)
           | __x__ ->
               (if Rpc.get_debug ()
                then
                  Printf.eprintf
                    "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                    "mirror_receive_result" "__x1066__" (Rpc.to_string __x__)
                    "Enum[String s;...]"
                else ();
                raise (Rpc.Runtime_error ("Enum[String s;...]", __x__))))
    type similars = content_id list
    let rec rpc_of_similars =
      function
      | __x1073__ ->
          Rpc.Enum
            (List.map (function | __x1074__ -> rpc_of_content_id __x1074__)
               __x1073__)
    and similars_of_rpc =
      function
      | __x1070__ ->
          (match __x1070__ with
           | Rpc.Enum __x1071__ ->
               List.map (function | __x1072__ -> content_id_of_rpc __x1072__)
                 __x1071__
           | __x__ ->
               (if Rpc.get_debug ()
                then
                  Printf.eprintf
                    "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                    "similars" "__x1070__" (Rpc.to_string __x__) "List"
                else ();
                raise (Rpc.Runtime_error ("List", __x__))))
  end
type async_result_t =
  | Vdi_info of vdi_info 
  | Mirror_id of Mirror.id 
let rec rpc_of_async_result_t =
  function
  | __x1078__ ->
      (match __x1078__ with
       | Mirror_id __x1079__ ->
           Rpc.Enum [Rpc.String "Mirror_id"; Mirror.rpc_of_id __x1079__]
       | Vdi_info __x1080__ ->
           Rpc.Enum [Rpc.String "Vdi_info"; rpc_of_vdi_info __x1080__])
and async_result_t_of_rpc =
  function
  | __x1075__ ->
      (match Rpc.lowerfn __x1075__ with
       | Rpc.Enum ((Rpc.String "mirror_id")::__x1076__::[]) ->
           Mirror_id (Mirror.id_of_rpc __x1076__)
       | Rpc.Enum ((Rpc.String "vdi_info")::__x1077__::[]) ->
           Vdi_info (vdi_info_of_rpc __x1077__)
       | __x__ ->
           (if Rpc.get_debug ()
            then
              Printf.eprintf
                "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                "async_result_t" "__x1075__" (Rpc.to_string __x__)
                "Enum[String s;...]"
            else ();
            raise (Rpc.Runtime_error ("Enum[String s;...]", __x__))))
module Task =
  struct
    type id = string
    let rec rpc_of_id = function | __x1082__ -> Rpc.String __x1082__
    and id_of_rpc =
      function
      | __x1081__ ->
          (match __x1081__ with
           | Rpc.String x -> x
           | __x__ ->
               (if Rpc.get_debug ()
                then
                  Printf.eprintf
                    "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                    "id" "__x1081__" (Rpc.to_string __x__) "String(string)"
                else ();
                raise (Rpc.Runtime_error ("String(string)", __x__))))
    type async_result = async_result_t
    let rec rpc_of_async_result =
      function | __x1084__ -> rpc_of_async_result_t __x1084__
    and async_result_of_rpc =
      function | __x1083__ -> async_result_t_of_rpc __x1083__
    type completion_t = {
      duration: float ;
      result: async_result option }
    let rec rpc_of_completion_t =
      function
      | __x1090__ ->
          let __x1091__ = __x1090__.duration
          and __x1092__ = __x1090__.result in
          Rpc.Dict (("duration", (Rpc.Float __x1091__)) ::
            ((match match __x1092__ with
                    | Some __x1094__ ->
                        Rpc.Enum [rpc_of_async_result __x1094__]
                    | None -> Rpc.Enum []
              with
              | Rpc.Enum [] -> []
              | Rpc.Enum (__x1093__::[]) -> [("result", __x1093__)]
              | _ -> assert false)))
    and completion_t_of_rpc =
      function
      | __x1085__ ->
          (match __x1085__ with
           | Rpc.Dict __x1086__ ->
               let __x1087__ =
                 try List.assoc "duration" __x1086__
                 with
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                          "completion_t" "__x1086__"
                          (Printexc.to_string __x__)
                          "Looking for key duration"
                      else ();
                      raise
                        (Rpc.Runtime_exception
                           ("Looking for key duration",
                             (Printexc.to_string __x__))))
               and __x1088__ =
                 if List.mem_assoc "result" __x1086__
                 then Rpc.Enum [List.assoc "result" __x1086__]
                 else Rpc.Enum [] in
               {
                 duration =
                   ((match __x1087__ with
                     | Rpc.Float x -> x
                     | Rpc.String s -> float_of_string s
                     | __x__ ->
                         (if Rpc.get_debug ()
                          then
                            Printf.eprintf
                              "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                              "completion_t" "__x1087__"
                              (Rpc.to_string __x__) "Float"
                          else ();
                          raise (Rpc.Runtime_error ("Float", __x__)))));
                 result =
                   ((match __x1088__ with
                     | Rpc.Enum [] -> None
                     | Rpc.Enum (__x1089__::[]) ->
                         Some (async_result_of_rpc __x1089__)
                     | __x__ ->
                         (if Rpc.get_debug ()
                          then
                            Printf.eprintf
                              "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                              "completion_t" "__x1088__"
                              (Rpc.to_string __x__) "Enum[]/Enum[_]"
                          else ();
                          raise (Rpc.Runtime_error ("Enum[]/Enum[_]", __x__)))))
               }
           | __x__ ->
               (if Rpc.get_debug ()
                then
                  Printf.eprintf
                    "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                    "completion_t" "__x1085__" (Rpc.to_string __x__) "Dict"
                else ();
                raise (Rpc.Runtime_error ("Dict", __x__))))
    type state =
      | Pending of float 
      | Completed of completion_t 
      | Failed of Rpc.t 
    let rec rpc_of_state =
      function
      | __x1099__ ->
          (match __x1099__ with
           | Failed __x1100__ ->
               Rpc.Enum [Rpc.String "Failed"; Rpc.rpc_of_t __x1100__]
           | Completed __x1101__ ->
               Rpc.Enum
                 [Rpc.String "Completed"; rpc_of_completion_t __x1101__]
           | Pending __x1102__ ->
               Rpc.Enum [Rpc.String "Pending"; Rpc.Float __x1102__])
    and state_of_rpc =
      function
      | __x1095__ ->
          (match Rpc.lowerfn __x1095__ with
           | Rpc.Enum ((Rpc.String "failed")::__x1096__::[]) ->
               Failed (Rpc.t_of_rpc __x1096__)
           | Rpc.Enum ((Rpc.String "completed")::__x1097__::[]) ->
               Completed (completion_t_of_rpc __x1097__)
           | Rpc.Enum ((Rpc.String "pending")::__x1098__::[]) ->
               Pending
                 ((match __x1098__ with
                   | Rpc.Float x -> x
                   | Rpc.String s -> float_of_string s
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "state" "__x1098__" (Rpc.to_string __x__) "Float"
                        else ();
                        raise (Rpc.Runtime_error ("Float", __x__)))))
           | __x__ ->
               (if Rpc.get_debug ()
                then
                  Printf.eprintf
                    "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                    "state" "__x1095__" (Rpc.to_string __x__)
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
      | __x1114__ ->
          let __x1115__ = __x1114__.id
          and __x1116__ = __x1114__.dbg
          and __x1117__ = __x1114__.ctime
          and __x1118__ = __x1114__.state
          and __x1119__ = __x1114__.subtasks
          and __x1120__ = __x1114__.debug_info
          and __x1121__ = __x1114__.backtrace in
          Rpc.Dict
            [("id", (rpc_of_id __x1115__));
            ("dbg", (Rpc.String __x1116__));
            ("ctime", (Rpc.Float __x1117__));
            ("state", (rpc_of_state __x1118__));
            ("subtasks",
              ((let dict =
                  List.map
                    (function
                     | (key, __x1123__) -> (key, (rpc_of_state __x1123__)))
                    __x1119__ in
                Rpc.Dict dict)));
            ("debug_info",
              ((let dict =
                  List.map
                    (function
                     | (key, __x1122__) -> (key, (Rpc.String __x1122__)))
                    __x1120__ in
                Rpc.Dict dict)));
            ("backtrace", (Rpc.String __x1121__))]
    and t_of_rpc =
      function
      | __x1103__ ->
          (match __x1103__ with
           | Rpc.Dict __x1104__ ->
               let __x1105__ =
                 try List.assoc "id" __x1104__
                 with
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                          "t" "__x1104__" (Printexc.to_string __x__)
                          "Looking for key id"
                      else ();
                      raise
                        (Rpc.Runtime_exception
                           ("Looking for key id", (Printexc.to_string __x__))))
               and __x1106__ =
                 try List.assoc "dbg" __x1104__
                 with
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                          "t" "__x1104__" (Printexc.to_string __x__)
                          "Looking for key dbg"
                      else ();
                      raise
                        (Rpc.Runtime_exception
                           ("Looking for key dbg",
                             (Printexc.to_string __x__))))
               and __x1107__ =
                 try List.assoc "ctime" __x1104__
                 with
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                          "t" "__x1104__" (Printexc.to_string __x__)
                          "Looking for key ctime"
                      else ();
                      raise
                        (Rpc.Runtime_exception
                           ("Looking for key ctime",
                             (Printexc.to_string __x__))))
               and __x1108__ =
                 try List.assoc "state" __x1104__
                 with
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                          "t" "__x1104__" (Printexc.to_string __x__)
                          "Looking for key state"
                      else ();
                      raise
                        (Rpc.Runtime_exception
                           ("Looking for key state",
                             (Printexc.to_string __x__))))
               and __x1109__ =
                 try List.assoc "subtasks" __x1104__
                 with
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                          "t" "__x1104__" (Printexc.to_string __x__)
                          "Looking for key subtasks"
                      else ();
                      raise
                        (Rpc.Runtime_exception
                           ("Looking for key subtasks",
                             (Printexc.to_string __x__))))
               and __x1110__ =
                 try List.assoc "debug_info" __x1104__
                 with
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                          "t" "__x1104__" (Printexc.to_string __x__)
                          "Looking for key debug_info"
                      else ();
                      raise
                        (Rpc.Runtime_exception
                           ("Looking for key debug_info",
                             (Printexc.to_string __x__))))
               and __x1111__ =
                 try List.assoc "backtrace" __x1104__
                 with
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                          "t" "__x1104__" (Printexc.to_string __x__)
                          "Looking for key backtrace"
                      else ();
                      raise
                        (Rpc.Runtime_exception
                           ("Looking for key backtrace",
                             (Printexc.to_string __x__)))) in
               {
                 id = (id_of_rpc __x1105__);
                 dbg =
                   ((match __x1106__ with
                     | Rpc.String x -> x
                     | __x__ ->
                         (if Rpc.get_debug ()
                          then
                            Printf.eprintf
                              "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                              "t" "__x1106__" (Rpc.to_string __x__)
                              "String(string)"
                          else ();
                          raise (Rpc.Runtime_error ("String(string)", __x__)))));
                 ctime =
                   ((match __x1107__ with
                     | Rpc.Float x -> x
                     | Rpc.String s -> float_of_string s
                     | __x__ ->
                         (if Rpc.get_debug ()
                          then
                            Printf.eprintf
                              "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                              "t" "__x1107__" (Rpc.to_string __x__) "Float"
                          else ();
                          raise (Rpc.Runtime_error ("Float", __x__)))));
                 state = (state_of_rpc __x1108__);
                 subtasks =
                   ((match __x1109__ with
                     | Rpc.Dict d ->
                         List.map
                           (function
                            | (key, __x1112__) ->
                                (key, (state_of_rpc __x1112__))) d
                     | __x__ ->
                         (if Rpc.get_debug ()
                          then
                            Printf.eprintf
                              "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                              "t" "__x1109__" (Rpc.to_string __x__) "Dict"
                          else ();
                          raise (Rpc.Runtime_error ("Dict", __x__)))));
                 debug_info =
                   ((match __x1110__ with
                     | Rpc.Dict d ->
                         List.map
                           (function
                            | (key, __x1113__) ->
                                (key,
                                  ((match __x1113__ with
                                    | Rpc.String x -> x
                                    | __x__ ->
                                        (if Rpc.get_debug ()
                                         then
                                           Printf.eprintf
                                             "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                             "t" "__x1113__"
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
                              "t" "__x1110__" (Rpc.to_string __x__) "Dict"
                          else ();
                          raise (Rpc.Runtime_error ("Dict", __x__)))));
                 backtrace =
                   ((match __x1111__ with
                     | Rpc.String x -> x
                     | __x__ ->
                         (if Rpc.get_debug ()
                          then
                            Printf.eprintf
                              "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                              "t" "__x1111__" (Rpc.to_string __x__)
                              "String(string)"
                          else ();
                          raise (Rpc.Runtime_error ("String(string)", __x__)))))
               }
           | __x__ ->
               (if Rpc.get_debug ()
                then
                  Printf.eprintf
                    "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                    "t" "__x1103__" (Rpc.to_string __x__) "Dict"
                else ();
                raise (Rpc.Runtime_error ("Dict", __x__))))
  end
module Dynamic =
  struct
    type id =
      | Task of Task.id 
      | Vdi of vdi 
      | Dp of dp 
      | Mirror of Mirror.id 
    let rec rpc_of_id =
      function
      | __x1129__ ->
          (match __x1129__ with
           | Mirror __x1130__ ->
               Rpc.Enum [Rpc.String "Mirror"; Mirror.rpc_of_id __x1130__]
           | Dp __x1131__ -> Rpc.Enum [Rpc.String "Dp"; rpc_of_dp __x1131__]
           | Vdi __x1132__ ->
               Rpc.Enum [Rpc.String "Vdi"; rpc_of_vdi __x1132__]
           | Task __x1133__ ->
               Rpc.Enum [Rpc.String "Task"; Task.rpc_of_id __x1133__])
    and id_of_rpc =
      function
      | __x1124__ ->
          (match Rpc.lowerfn __x1124__ with
           | Rpc.Enum ((Rpc.String "mirror")::__x1125__::[]) ->
               Mirror (Mirror.id_of_rpc __x1125__)
           | Rpc.Enum ((Rpc.String "dp")::__x1126__::[]) ->
               Dp (dp_of_rpc __x1126__)
           | Rpc.Enum ((Rpc.String "vdi")::__x1127__::[]) ->
               Vdi (vdi_of_rpc __x1127__)
           | Rpc.Enum ((Rpc.String "task")::__x1128__::[]) ->
               Task (Task.id_of_rpc __x1128__)
           | __x__ ->
               (if Rpc.get_debug ()
                then
                  Printf.eprintf
                    "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                    "id" "__x1124__" (Rpc.to_string __x__)
                    "Enum[String s;...]"
                else ();
                raise (Rpc.Runtime_error ("Enum[String s;...]", __x__))))
    type t =
      | Task_t of Task.id * Task.t 
      | Vdi_t of vdi * vdi_info 
      | Dp_t of dp * dp_stat_t 
      | Mirror_t of Mirror.id * Mirror.t 
    let rec rpc_of_t =
      function
      | __x1143__ ->
          (match __x1143__ with
           | Mirror_t (__x1144__, __x1145__) ->
               Rpc.Enum
                 [Rpc.String "Mirror_t";
                 Mirror.rpc_of_id __x1144__;
                 Mirror.rpc_of_t __x1145__]
           | Dp_t (__x1146__, __x1147__) ->
               Rpc.Enum
                 [Rpc.String "Dp_t";
                 rpc_of_dp __x1146__;
                 rpc_of_dp_stat_t __x1147__]
           | Vdi_t (__x1148__, __x1149__) ->
               Rpc.Enum
                 [Rpc.String "Vdi_t";
                 rpc_of_vdi __x1148__;
                 rpc_of_vdi_info __x1149__]
           | Task_t (__x1150__, __x1151__) ->
               Rpc.Enum
                 [Rpc.String "Task_t";
                 Task.rpc_of_id __x1150__;
                 Task.rpc_of_t __x1151__])
    and t_of_rpc =
      function
      | __x1134__ ->
          (match Rpc.lowerfn __x1134__ with
           | Rpc.Enum ((Rpc.String "mirror_t")::__x1135__::__x1136__::[]) ->
               Mirror_t
                 ((Mirror.id_of_rpc __x1135__), (Mirror.t_of_rpc __x1136__))
           | Rpc.Enum ((Rpc.String "dp_t")::__x1137__::__x1138__::[]) ->
               Dp_t ((dp_of_rpc __x1137__), (dp_stat_t_of_rpc __x1138__))
           | Rpc.Enum ((Rpc.String "vdi_t")::__x1139__::__x1140__::[]) ->
               Vdi_t ((vdi_of_rpc __x1139__), (vdi_info_of_rpc __x1140__))
           | Rpc.Enum ((Rpc.String "task_t")::__x1141__::__x1142__::[]) ->
               Task_t ((Task.id_of_rpc __x1141__), (Task.t_of_rpc __x1142__))
           | __x__ ->
               (if Rpc.get_debug ()
                then
                  Printf.eprintf
                    "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                    "t" "__x1134__" (Rpc.to_string __x__)
                    "Enum[String s;...]"
                else ();
                raise (Rpc.Runtime_error ("Enum[String s;...]", __x__))))
  end
type uuid = string
let rec rpc_of_uuid = function | __x1153__ -> Rpc.String __x1153__
and uuid_of_rpc =
  function
  | __x1152__ ->
      (match __x1152__ with
       | Rpc.String x -> x
       | __x__ ->
           (if Rpc.get_debug ()
            then
              Printf.eprintf
                "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                "uuid" "__x1152__" (Rpc.to_string __x__) "String(string)"
            else ();
            raise (Rpc.Runtime_error ("String(string)", __x__))))
type query_result =
  {
  driver: string ;
  name: string ;
  description: string ;
  vendor: string ;
  copyright: string ;
  version: string ;
  required_api_version: string ;
  features: string list ;
  configuration: (string * string) list ;
  required_cluster_stack: string list }
let rec rpc_of_query_result =
  function
  | __x1171__ ->
      let __x1172__ = __x1171__.driver
      and __x1173__ = __x1171__.name
      and __x1174__ = __x1171__.description
      and __x1175__ = __x1171__.vendor
      and __x1176__ = __x1171__.copyright
      and __x1177__ = __x1171__.version
      and __x1178__ = __x1171__.required_api_version
      and __x1179__ = __x1171__.features
      and __x1180__ = __x1171__.configuration
      and __x1181__ = __x1171__.required_cluster_stack in
      Rpc.Dict
        [("driver", (Rpc.String __x1172__));
        ("name", (Rpc.String __x1173__));
        ("description", (Rpc.String __x1174__));
        ("vendor", (Rpc.String __x1175__));
        ("copyright", (Rpc.String __x1176__));
        ("version", (Rpc.String __x1177__));
        ("required_api_version", (Rpc.String __x1178__));
        ("features",
          (Rpc.Enum
             (List.map (function | __x1184__ -> Rpc.String __x1184__)
                __x1179__)));
        ("configuration",
          ((let dict =
              List.map
                (function | (key, __x1183__) -> (key, (Rpc.String __x1183__)))
                __x1180__ in
            Rpc.Dict dict)));
        ("required_cluster_stack",
          (Rpc.Enum
             (List.map (function | __x1182__ -> Rpc.String __x1182__)
                __x1181__)))]
and query_result_of_rpc =
  function
  | __x1154__ ->
      (match __x1154__ with
       | Rpc.Dict __x1155__ ->
           let __x1156__ =
             try List.assoc "driver" __x1155__
             with
             | __x__ ->
                 (if Rpc.get_debug ()
                  then
                    Printf.eprintf
                      "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                      "query_result" "__x1155__" (Printexc.to_string __x__)
                      "Looking for key driver"
                  else ();
                  raise
                    (Rpc.Runtime_exception
                       ("Looking for key driver", (Printexc.to_string __x__))))
           and __x1157__ =
             try List.assoc "name" __x1155__
             with
             | __x__ ->
                 (if Rpc.get_debug ()
                  then
                    Printf.eprintf
                      "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                      "query_result" "__x1155__" (Printexc.to_string __x__)
                      "Looking for key name"
                  else ();
                  raise
                    (Rpc.Runtime_exception
                       ("Looking for key name", (Printexc.to_string __x__))))
           and __x1158__ =
             try List.assoc "description" __x1155__
             with
             | __x__ ->
                 (if Rpc.get_debug ()
                  then
                    Printf.eprintf
                      "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                      "query_result" "__x1155__" (Printexc.to_string __x__)
                      "Looking for key description"
                  else ();
                  raise
                    (Rpc.Runtime_exception
                       ("Looking for key description",
                         (Printexc.to_string __x__))))
           and __x1159__ =
             try List.assoc "vendor" __x1155__
             with
             | __x__ ->
                 (if Rpc.get_debug ()
                  then
                    Printf.eprintf
                      "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                      "query_result" "__x1155__" (Printexc.to_string __x__)
                      "Looking for key vendor"
                  else ();
                  raise
                    (Rpc.Runtime_exception
                       ("Looking for key vendor", (Printexc.to_string __x__))))
           and __x1160__ =
             try List.assoc "copyright" __x1155__
             with
             | __x__ ->
                 (if Rpc.get_debug ()
                  then
                    Printf.eprintf
                      "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                      "query_result" "__x1155__" (Printexc.to_string __x__)
                      "Looking for key copyright"
                  else ();
                  raise
                    (Rpc.Runtime_exception
                       ("Looking for key copyright",
                         (Printexc.to_string __x__))))
           and __x1161__ =
             try List.assoc "version" __x1155__
             with
             | __x__ ->
                 (if Rpc.get_debug ()
                  then
                    Printf.eprintf
                      "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                      "query_result" "__x1155__" (Printexc.to_string __x__)
                      "Looking for key version"
                  else ();
                  raise
                    (Rpc.Runtime_exception
                       ("Looking for key version",
                         (Printexc.to_string __x__))))
           and __x1162__ =
             try List.assoc "required_api_version" __x1155__
             with
             | __x__ ->
                 (if Rpc.get_debug ()
                  then
                    Printf.eprintf
                      "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                      "query_result" "__x1155__" (Printexc.to_string __x__)
                      "Looking for key required_api_version"
                  else ();
                  raise
                    (Rpc.Runtime_exception
                       ("Looking for key required_api_version",
                         (Printexc.to_string __x__))))
           and __x1163__ =
             try List.assoc "features" __x1155__
             with
             | __x__ ->
                 (if Rpc.get_debug ()
                  then
                    Printf.eprintf
                      "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                      "query_result" "__x1155__" (Printexc.to_string __x__)
                      "Looking for key features"
                  else ();
                  raise
                    (Rpc.Runtime_exception
                       ("Looking for key features",
                         (Printexc.to_string __x__))))
           and __x1164__ =
             try List.assoc "configuration" __x1155__
             with
             | __x__ ->
                 (if Rpc.get_debug ()
                  then
                    Printf.eprintf
                      "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                      "query_result" "__x1155__" (Printexc.to_string __x__)
                      "Looking for key configuration"
                  else ();
                  raise
                    (Rpc.Runtime_exception
                       ("Looking for key configuration",
                         (Printexc.to_string __x__))))
           and __x1165__ =
             try List.assoc "required_cluster_stack" __x1155__
             with
             | __x__ ->
                 (if Rpc.get_debug ()
                  then
                    Printf.eprintf
                      "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                      "query_result" "__x1155__" (Printexc.to_string __x__)
                      "Looking for key required_cluster_stack"
                  else ();
                  raise
                    (Rpc.Runtime_exception
                       ("Looking for key required_cluster_stack",
                         (Printexc.to_string __x__)))) in
           {
             driver =
               ((match __x1156__ with
                 | Rpc.String x -> x
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                          "query_result" "__x1156__" (Rpc.to_string __x__)
                          "String(string)"
                      else ();
                      raise (Rpc.Runtime_error ("String(string)", __x__)))));
             name =
               ((match __x1157__ with
                 | Rpc.String x -> x
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                          "query_result" "__x1157__" (Rpc.to_string __x__)
                          "String(string)"
                      else ();
                      raise (Rpc.Runtime_error ("String(string)", __x__)))));
             description =
               ((match __x1158__ with
                 | Rpc.String x -> x
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                          "query_result" "__x1158__" (Rpc.to_string __x__)
                          "String(string)"
                      else ();
                      raise (Rpc.Runtime_error ("String(string)", __x__)))));
             vendor =
               ((match __x1159__ with
                 | Rpc.String x -> x
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                          "query_result" "__x1159__" (Rpc.to_string __x__)
                          "String(string)"
                      else ();
                      raise (Rpc.Runtime_error ("String(string)", __x__)))));
             copyright =
               ((match __x1160__ with
                 | Rpc.String x -> x
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                          "query_result" "__x1160__" (Rpc.to_string __x__)
                          "String(string)"
                      else ();
                      raise (Rpc.Runtime_error ("String(string)", __x__)))));
             version =
               ((match __x1161__ with
                 | Rpc.String x -> x
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                          "query_result" "__x1161__" (Rpc.to_string __x__)
                          "String(string)"
                      else ();
                      raise (Rpc.Runtime_error ("String(string)", __x__)))));
             required_api_version =
               ((match __x1162__ with
                 | Rpc.String x -> x
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                          "query_result" "__x1162__" (Rpc.to_string __x__)
                          "String(string)"
                      else ();
                      raise (Rpc.Runtime_error ("String(string)", __x__)))));
             features =
               ((match __x1163__ with
                 | Rpc.Enum __x1166__ ->
                     List.map
                       (function
                        | __x1167__ ->
                            (match __x1167__ with
                             | Rpc.String x -> x
                             | __x__ ->
                                 (if Rpc.get_debug ()
                                  then
                                    Printf.eprintf
                                      "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                      "query_result" "__x1167__"
                                      (Rpc.to_string __x__) "String(string)"
                                  else ();
                                  raise
                                    (Rpc.Runtime_error
                                       ("String(string)", __x__)))))
                       __x1166__
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                          "query_result" "__x1163__" (Rpc.to_string __x__)
                          "List"
                      else ();
                      raise (Rpc.Runtime_error ("List", __x__)))));
             configuration =
               ((match __x1164__ with
                 | Rpc.Dict d ->
                     List.map
                       (function
                        | (key, __x1168__) ->
                            (key,
                              ((match __x1168__ with
                                | Rpc.String x -> x
                                | __x__ ->
                                    (if Rpc.get_debug ()
                                     then
                                       Printf.eprintf
                                         "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                         "query_result" "__x1168__"
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
                          "query_result" "__x1164__" (Rpc.to_string __x__)
                          "Dict"
                      else ();
                      raise (Rpc.Runtime_error ("Dict", __x__)))));
             required_cluster_stack =
               ((match __x1165__ with
                 | Rpc.Enum __x1169__ ->
                     List.map
                       (function
                        | __x1170__ ->
                            (match __x1170__ with
                             | Rpc.String x -> x
                             | __x__ ->
                                 (if Rpc.get_debug ()
                                  then
                                    Printf.eprintf
                                      "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                      "query_result" "__x1170__"
                                      (Rpc.to_string __x__) "String(string)"
                                  else ();
                                  raise
                                    (Rpc.Runtime_error
                                       ("String(string)", __x__)))))
                       __x1169__
                 | __x__ ->
                     (if Rpc.get_debug ()
                      then
                        Printf.eprintf
                          "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                          "query_result" "__x1165__" (Rpc.to_string __x__)
                          "List"
                      else ();
                      raise (Rpc.Runtime_error ("List", __x__)))))
           }
       | __x__ ->
           (if Rpc.get_debug ()
            then
              Printf.eprintf
                "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                "query_result" "__x1154__" (Rpc.to_string __x__) "Dict"
            else ();
            raise (Rpc.Runtime_error ("Dict", __x__))))
module Query = struct  end
module DP = struct  end
module SR = struct  end
module VDI = struct  end
module DATA = struct module MIRROR = struct  end end
module Policy = struct  end
module TASK = struct  end
module UPDATES = struct  end
type __exn_ty17 = (string * string list)
type __exn_ty16 = string
type __exn_ty15 = string
type __exn_ty14 = (Vdi_automaton.state * Vdi_automaton.state)
type __exn_ty13 = (string * string list)
type __exn_ty12 = (string * string)
type __exn_ty11 = string
type __exn_ty10 = string option
type __exn_ty9 = string
type __exn_ty8 = string
type __exn_ty7 = uuid
type __exn_ty6 = string
type __exn_ty5 = string
type __exn_ty4 = (string * string)
type __exn_ty3 = string
type __exn_ty2 = string
type __exn_ty1 = (string * int * int)
type __exn_ty0 = string
exception Backend_error_with_backtrace of __exn_ty17 
exception Sr_not_attached of __exn_ty16 
exception Vdi_does_not_exist of __exn_ty15 
exception Illegal_transition of __exn_ty14 
exception Backend_error of __exn_ty13 
exception Does_not_exist of __exn_ty12 
exception Cancelled of __exn_ty11 
exception Redirect of __exn_ty10 
exception Sr_attached of __exn_ty9 
exception Unimplemented of __exn_ty8 
exception Activated_on_another_host of __exn_ty7 
exception Duplicated_key of __exn_ty6 
exception No_storage_plugin_for_sr of __exn_ty5 
exception Content_ids_do_not_match of __exn_ty4 
exception Missing_configuration_parameter of __exn_ty3 
exception Unknown_RPC of __exn_ty2 
exception Message_param_count_mismatch of __exn_ty1 
exception Internal_error of __exn_ty0 
module Exception =
  struct
    type exnty =
      | Internal_error of string 
      | Message_param_count_mismatch of (string * int * int) 
      | Unknown_RPC of string 
      | Missing_configuration_parameter of string 
      | Content_ids_do_not_match of (string * string) 
      | No_storage_plugin_for_sr of string 
      | Duplicated_key of string 
      | Activated_on_another_host of uuid 
      | Unimplemented of string 
      | Sr_attached of string 
      | Redirect of string option 
      | Cancelled of string 
      | Does_not_exist of (string * string) 
      | Backend_error of (string * string list) 
      | Illegal_transition of (Vdi_automaton.state * Vdi_automaton.state) 
      | Vdi_does_not_exist of string 
      | Sr_not_attached of string 
      | Backend_error_with_backtrace of (string * string list) 
    let rpc_of_exnty =
      function
      | __x38__ ->
          (match __x38__ with
           | Backend_error_with_backtrace __x39__ ->
               Rpc.Enum
                 [Rpc.String "Backend_error_with_backtrace";
                 (let (__x40__, __x41__) = __x39__ in
                  Rpc.Enum
                    [Rpc.String __x40__;
                    Rpc.Enum
                      (List.map (function | __x42__ -> Rpc.String __x42__)
                         __x41__)])]
           | Sr_not_attached __x43__ ->
               Rpc.Enum [Rpc.String "Sr_not_attached"; Rpc.String __x43__]
           | Vdi_does_not_exist __x44__ ->
               Rpc.Enum [Rpc.String "Vdi_does_not_exist"; Rpc.String __x44__]
           | Illegal_transition __x45__ ->
               Rpc.Enum
                 [Rpc.String "Illegal_transition";
                 (let (__x46__, __x47__) = __x45__ in
                  Rpc.Enum
                    [Vdi_automaton.rpc_of_state __x46__;
                    Vdi_automaton.rpc_of_state __x47__])]
           | Backend_error __x48__ ->
               Rpc.Enum
                 [Rpc.String "Backend_error";
                 (let (__x49__, __x50__) = __x48__ in
                  Rpc.Enum
                    [Rpc.String __x49__;
                    Rpc.Enum
                      (List.map (function | __x51__ -> Rpc.String __x51__)
                         __x50__)])]
           | Does_not_exist __x52__ ->
               Rpc.Enum
                 [Rpc.String "Does_not_exist";
                 (let (__x53__, __x54__) = __x52__ in
                  Rpc.Enum [Rpc.String __x53__; Rpc.String __x54__])]
           | Cancelled __x55__ ->
               Rpc.Enum [Rpc.String "Cancelled"; Rpc.String __x55__]
           | Redirect __x56__ ->
               Rpc.Enum
                 [Rpc.String "Redirect";
                 (match __x56__ with
                  | Some __x57__ -> Rpc.Enum [Rpc.String __x57__]
                  | None -> Rpc.Enum [])]
           | Sr_attached __x58__ ->
               Rpc.Enum [Rpc.String "Sr_attached"; Rpc.String __x58__]
           | Unimplemented __x59__ ->
               Rpc.Enum [Rpc.String "Unimplemented"; Rpc.String __x59__]
           | Activated_on_another_host __x60__ ->
               Rpc.Enum
                 [Rpc.String "Activated_on_another_host";
                 rpc_of_uuid __x60__]
           | Duplicated_key __x61__ ->
               Rpc.Enum [Rpc.String "Duplicated_key"; Rpc.String __x61__]
           | No_storage_plugin_for_sr __x62__ ->
               Rpc.Enum
                 [Rpc.String "No_storage_plugin_for_sr"; Rpc.String __x62__]
           | Content_ids_do_not_match __x63__ ->
               Rpc.Enum
                 [Rpc.String "Content_ids_do_not_match";
                 (let (__x64__, __x65__) = __x63__ in
                  Rpc.Enum [Rpc.String __x64__; Rpc.String __x65__])]
           | Missing_configuration_parameter __x66__ ->
               Rpc.Enum
                 [Rpc.String "Missing_configuration_parameter";
                 Rpc.String __x66__]
           | Unknown_RPC __x67__ ->
               Rpc.Enum [Rpc.String "Unknown_RPC"; Rpc.String __x67__]
           | Message_param_count_mismatch __x68__ ->
               Rpc.Enum
                 [Rpc.String "Message_param_count_mismatch";
                 (let (__x69__, __x70__, __x71__) = __x68__ in
                  Rpc.Enum
                    [Rpc.String __x69__;
                    Rpc.Int (Int64.of_int __x70__);
                    Rpc.Int (Int64.of_int __x71__)])]
           | Internal_error __x72__ ->
               Rpc.Enum [Rpc.String "Internal_error"; Rpc.String __x72__])
    let exnty_of_rpc =
      function
      | __x1__ ->
          (match Rpc.lowerfn __x1__ with
           | Rpc.Enum ((Rpc.String
               "backend_error_with_backtrace")::__x2__::[]) ->
               Backend_error_with_backtrace
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
                           | Rpc.Enum __x5__ ->
                               List.map
                                 (function
                                  | __x6__ ->
                                      (match __x6__ with
                                       | Rpc.String x -> x
                                       | __x__ ->
                                           (if Rpc.get_debug ()
                                            then
                                              Printf.eprintf
                                                "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                                "exnty" "__x6__"
                                                (Rpc.to_string __x__)
                                                "String(string)"
                                            else ();
                                            raise
                                              (Rpc.Runtime_error
                                                 ("String(string)", __x__)))))
                                 __x5__
                           | __x__ ->
                               (if Rpc.get_debug ()
                                then
                                  Printf.eprintf
                                    "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                    "exnty" "__x4__" (Rpc.to_string __x__)
                                    "List"
                                else ();
                                raise (Rpc.Runtime_error ("List", __x__))))))
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "exnty" "__x2__" (Rpc.to_string __x__) "List"
                        else ();
                        raise (Rpc.Runtime_error ("List", __x__)))))
           | Rpc.Enum ((Rpc.String "sr_not_attached")::__x7__::[]) ->
               Sr_not_attached
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
                        raise (Rpc.Runtime_error ("String(string)", __x__)))))
           | Rpc.Enum ((Rpc.String "vdi_does_not_exist")::__x8__::[]) ->
               Vdi_does_not_exist
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
           | Rpc.Enum ((Rpc.String "illegal_transition")::__x9__::[]) ->
               Illegal_transition
                 ((match __x9__ with
                   | Rpc.Enum (__x10__::__x11__::[]) ->
                       ((Vdi_automaton.state_of_rpc __x10__),
                         (Vdi_automaton.state_of_rpc __x11__))
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "exnty" "__x9__" (Rpc.to_string __x__) "List"
                        else ();
                        raise (Rpc.Runtime_error ("List", __x__)))))
           | Rpc.Enum ((Rpc.String "backend_error")::__x12__::[]) ->
               Backend_error
                 ((match __x12__ with
                   | Rpc.Enum (__x13__::__x14__::[]) ->
                       (((match __x13__ with
                          | Rpc.String x -> x
                          | __x__ ->
                              (if Rpc.get_debug ()
                               then
                                 Printf.eprintf
                                   "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                   "exnty" "__x13__" (Rpc.to_string __x__)
                                   "String(string)"
                               else ();
                               raise
                                 (Rpc.Runtime_error ("String(string)", __x__))))),
                         ((match __x14__ with
                           | Rpc.Enum __x15__ ->
                               List.map
                                 (function
                                  | __x16__ ->
                                      (match __x16__ with
                                       | Rpc.String x -> x
                                       | __x__ ->
                                           (if Rpc.get_debug ()
                                            then
                                              Printf.eprintf
                                                "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                                "exnty" "__x16__"
                                                (Rpc.to_string __x__)
                                                "String(string)"
                                            else ();
                                            raise
                                              (Rpc.Runtime_error
                                                 ("String(string)", __x__)))))
                                 __x15__
                           | __x__ ->
                               (if Rpc.get_debug ()
                                then
                                  Printf.eprintf
                                    "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                    "exnty" "__x14__" (Rpc.to_string __x__)
                                    "List"
                                else ();
                                raise (Rpc.Runtime_error ("List", __x__))))))
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "exnty" "__x12__" (Rpc.to_string __x__) "List"
                        else ();
                        raise (Rpc.Runtime_error ("List", __x__)))))
           | Rpc.Enum ((Rpc.String "does_not_exist")::__x17__::[]) ->
               Does_not_exist
                 ((match __x17__ with
                   | Rpc.Enum (__x18__::__x19__::[]) ->
                       (((match __x18__ with
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
                                 (Rpc.Runtime_error ("String(string)", __x__))))),
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
                            "exnty" "__x17__" (Rpc.to_string __x__) "List"
                        else ();
                        raise (Rpc.Runtime_error ("List", __x__)))))
           | Rpc.Enum ((Rpc.String "cancelled")::__x20__::[]) ->
               Cancelled
                 ((match __x20__ with
                   | Rpc.String x -> x
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "exnty" "__x20__" (Rpc.to_string __x__)
                            "String(string)"
                        else ();
                        raise (Rpc.Runtime_error ("String(string)", __x__)))))
           | Rpc.Enum ((Rpc.String "redirect")::__x21__::[]) ->
               Redirect
                 ((match __x21__ with
                   | Rpc.Enum [] -> None
                   | Rpc.Enum (__x22__::[]) ->
                       Some
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
                                     ("String(string)", __x__)))))
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "exnty" "__x21__" (Rpc.to_string __x__)
                            "Enum[]/Enum[_]"
                        else ();
                        raise (Rpc.Runtime_error ("Enum[]/Enum[_]", __x__)))))
           | Rpc.Enum ((Rpc.String "sr_attached")::__x23__::[]) ->
               Sr_attached
                 ((match __x23__ with
                   | Rpc.String x -> x
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "exnty" "__x23__" (Rpc.to_string __x__)
                            "String(string)"
                        else ();
                        raise (Rpc.Runtime_error ("String(string)", __x__)))))
           | Rpc.Enum ((Rpc.String "unimplemented")::__x24__::[]) ->
               Unimplemented
                 ((match __x24__ with
                   | Rpc.String x -> x
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "exnty" "__x24__" (Rpc.to_string __x__)
                            "String(string)"
                        else ();
                        raise (Rpc.Runtime_error ("String(string)", __x__)))))
           | Rpc.Enum ((Rpc.String "activated_on_another_host")::__x25__::[])
               -> Activated_on_another_host (uuid_of_rpc __x25__)
           | Rpc.Enum ((Rpc.String "duplicated_key")::__x26__::[]) ->
               Duplicated_key
                 ((match __x26__ with
                   | Rpc.String x -> x
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "exnty" "__x26__" (Rpc.to_string __x__)
                            "String(string)"
                        else ();
                        raise (Rpc.Runtime_error ("String(string)", __x__)))))
           | Rpc.Enum ((Rpc.String "no_storage_plugin_for_sr")::__x27__::[])
               ->
               No_storage_plugin_for_sr
                 ((match __x27__ with
                   | Rpc.String x -> x
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "exnty" "__x27__" (Rpc.to_string __x__)
                            "String(string)"
                        else ();
                        raise (Rpc.Runtime_error ("String(string)", __x__)))))
           | Rpc.Enum ((Rpc.String "content_ids_do_not_match")::__x28__::[])
               ->
               Content_ids_do_not_match
                 ((match __x28__ with
                   | Rpc.Enum (__x29__::__x30__::[]) ->
                       (((match __x29__ with
                          | Rpc.String x -> x
                          | __x__ ->
                              (if Rpc.get_debug ()
                               then
                                 Printf.eprintf
                                   "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                   "exnty" "__x29__" (Rpc.to_string __x__)
                                   "String(string)"
                               else ();
                               raise
                                 (Rpc.Runtime_error ("String(string)", __x__))))),
                         ((match __x30__ with
                           | Rpc.String x -> x
                           | __x__ ->
                               (if Rpc.get_debug ()
                                then
                                  Printf.eprintf
                                    "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                    "exnty" "__x30__" (Rpc.to_string __x__)
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
                            "exnty" "__x28__" (Rpc.to_string __x__) "List"
                        else ();
                        raise (Rpc.Runtime_error ("List", __x__)))))
           | Rpc.Enum ((Rpc.String
               "missing_configuration_parameter")::__x31__::[]) ->
               Missing_configuration_parameter
                 ((match __x31__ with
                   | Rpc.String x -> x
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "exnty" "__x31__" (Rpc.to_string __x__)
                            "String(string)"
                        else ();
                        raise (Rpc.Runtime_error ("String(string)", __x__)))))
           | Rpc.Enum ((Rpc.String "unknown_rpc")::__x32__::[]) ->
               Unknown_RPC
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
                        raise (Rpc.Runtime_error ("String(string)", __x__)))))
           | Rpc.Enum ((Rpc.String
               "message_param_count_mismatch")::__x33__::[]) ->
               Message_param_count_mismatch
                 ((match __x33__ with
                   | Rpc.Enum (__x34__::__x35__::__x36__::[]) ->
                       (((match __x34__ with
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
                                 (Rpc.Runtime_error ("String(string)", __x__))))),
                         ((match __x35__ with
                           | Rpc.Int x -> Int64.to_int x
                           | Rpc.String s -> int_of_string s
                           | __x__ ->
                               (if Rpc.get_debug ()
                                then
                                  Printf.eprintf
                                    "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                    "exnty" "__x35__" (Rpc.to_string __x__)
                                    "Int(int)"
                                else ();
                                raise (Rpc.Runtime_error ("Int(int)", __x__))))),
                         ((match __x36__ with
                           | Rpc.Int x -> Int64.to_int x
                           | Rpc.String s -> int_of_string s
                           | __x__ ->
                               (if Rpc.get_debug ()
                                then
                                  Printf.eprintf
                                    "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                    "exnty" "__x36__" (Rpc.to_string __x__)
                                    "Int(int)"
                                else ();
                                raise (Rpc.Runtime_error ("Int(int)", __x__))))))
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "exnty" "__x33__" (Rpc.to_string __x__) "List"
                        else ();
                        raise (Rpc.Runtime_error ("List", __x__)))))
           | Rpc.Enum ((Rpc.String "internal_error")::__x37__::[]) ->
               Internal_error
                 ((match __x37__ with
                   | Rpc.String x -> x
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "exnty" "__x37__" (Rpc.to_string __x__)
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
       | Missing_configuration_parameter x ->
           Exception.Missing_configuration_parameter x
       | Content_ids_do_not_match x -> Exception.Content_ids_do_not_match x
       | No_storage_plugin_for_sr x -> Exception.No_storage_plugin_for_sr x
       | Duplicated_key x -> Exception.Duplicated_key x
       | Activated_on_another_host x -> Exception.Activated_on_another_host x
       | Unimplemented x -> Exception.Unimplemented x
       | Sr_attached x -> Exception.Sr_attached x
       | Redirect x -> Exception.Redirect x
       | Cancelled x -> Exception.Cancelled x
       | Does_not_exist x -> Exception.Does_not_exist x
       | Backend_error x -> Exception.Backend_error x
       | Illegal_transition x -> Exception.Illegal_transition x
       | Vdi_does_not_exist x -> Exception.Vdi_does_not_exist x
       | Sr_not_attached x -> Exception.Sr_not_attached x
       | Backend_error_with_backtrace x ->
           Exception.Backend_error_with_backtrace x
       | e -> Exception.Internal_error (Printexc.to_string e))
let exn_of_exnty =
  function
  | x ->
      (match x with
       | Exception.Internal_error x -> Internal_error x
       | Exception.Message_param_count_mismatch x ->
           Message_param_count_mismatch x
       | Exception.Unknown_RPC x -> Unknown_RPC x
       | Exception.Missing_configuration_parameter x ->
           Missing_configuration_parameter x
       | Exception.Content_ids_do_not_match x -> Content_ids_do_not_match x
       | Exception.No_storage_plugin_for_sr x -> No_storage_plugin_for_sr x
       | Exception.Duplicated_key x -> Duplicated_key x
       | Exception.Activated_on_another_host x -> Activated_on_another_host x
       | Exception.Unimplemented x -> Unimplemented x
       | Exception.Sr_attached x -> Sr_attached x
       | Exception.Redirect x -> Redirect x
       | Exception.Cancelled x -> Cancelled x
       | Exception.Does_not_exist x -> Does_not_exist x
       | Exception.Backend_error x -> Backend_error x
       | Exception.Illegal_transition x -> Illegal_transition x
       | Exception.Vdi_does_not_exist x -> Vdi_does_not_exist x
       | Exception.Sr_not_attached x -> Sr_not_attached x
       | Exception.Backend_error_with_backtrace x ->
           Backend_error_with_backtrace x)
module Args =
  struct
    module Query =
      struct
        module Query =
          struct
            type request = {
              dbg: string }
            let rpc_of_request =
              function
              | __x78__ ->
                  let __x79__ = __x78__.dbg in
                  Rpc.Dict [("dbg", (Rpc.String __x79__))]
            let request_of_rpc =
              function
              | __x75__ ->
                  (match __x75__ with
                   | Rpc.Dict __x76__ ->
                       let __x77__ =
                         try List.assoc "dbg" __x76__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x76__"
                                  (Printexc.to_string __x__)
                                  "Looking for key dbg"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key dbg",
                                     (Printexc.to_string __x__)))) in
                       {
                         dbg =
                           ((match __x77__ with
                             | Rpc.String x -> x
                             | __x__ ->
                                 (if Rpc.get_debug ()
                                  then
                                    Printf.eprintf
                                      "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                      "request" "__x77__"
                                      (Rpc.to_string __x__) "String(string)"
                                  else ();
                                  raise
                                    (Rpc.Runtime_error
                                       ("String(string)", __x__)))))
                       }
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "request" "__x75__" (Rpc.to_string __x__) "Dict"
                        else ();
                        raise (Rpc.Runtime_error ("Dict", __x__))))
            type response = query_result
            let rpc_of_response =
              function | __x74__ -> rpc_of_query_result __x74__
            let response_of_rpc =
              function | __x73__ -> query_result_of_rpc __x73__
            let call_of_query ~dbg  =
              let arg = { dbg } in
              Rpc.call "Query.query" [rpc_of_request arg]
          end
        module Diagnostics =
          struct
            type request = {
              dbg: string }
            let rpc_of_request =
              function
              | __x85__ ->
                  let __x86__ = __x85__.dbg in
                  Rpc.Dict [("dbg", (Rpc.String __x86__))]
            let request_of_rpc =
              function
              | __x82__ ->
                  (match __x82__ with
                   | Rpc.Dict __x83__ ->
                       let __x84__ =
                         try List.assoc "dbg" __x83__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x83__"
                                  (Printexc.to_string __x__)
                                  "Looking for key dbg"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key dbg",
                                     (Printexc.to_string __x__)))) in
                       {
                         dbg =
                           ((match __x84__ with
                             | Rpc.String x -> x
                             | __x__ ->
                                 (if Rpc.get_debug ()
                                  then
                                    Printf.eprintf
                                      "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                      "request" "__x84__"
                                      (Rpc.to_string __x__) "String(string)"
                                  else ();
                                  raise
                                    (Rpc.Runtime_error
                                       ("String(string)", __x__)))))
                       }
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "request" "__x82__" (Rpc.to_string __x__) "Dict"
                        else ();
                        raise (Rpc.Runtime_error ("Dict", __x__))))
            type response = string
            let rpc_of_response = function | __x81__ -> Rpc.String __x81__
            let response_of_rpc =
              function
              | __x80__ ->
                  (match __x80__ with
                   | Rpc.String x -> x
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "response" "__x80__" (Rpc.to_string __x__)
                            "String(string)"
                        else ();
                        raise (Rpc.Runtime_error ("String(string)", __x__))))
            let call_of_diagnostics ~dbg  =
              let arg = { dbg } in
              Rpc.call "Query.diagnostics" [rpc_of_request arg]
          end
      end
    module DP =
      struct
        module Create =
          struct
            type request = {
              dbg: debug_info ;
              id: string }
            let rpc_of_request =
              function
              | __x93__ ->
                  let __x94__ = __x93__.dbg
                  and __x95__ = __x93__.id in
                  Rpc.Dict
                    [("dbg", (rpc_of_debug_info __x94__));
                    ("id", (Rpc.String __x95__))]
            let request_of_rpc =
              function
              | __x89__ ->
                  (match __x89__ with
                   | Rpc.Dict __x90__ ->
                       let __x91__ =
                         try List.assoc "dbg" __x90__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x90__"
                                  (Printexc.to_string __x__)
                                  "Looking for key dbg"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key dbg",
                                     (Printexc.to_string __x__))))
                       and __x92__ =
                         try List.assoc "id" __x90__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x90__"
                                  (Printexc.to_string __x__)
                                  "Looking for key id"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key id",
                                     (Printexc.to_string __x__)))) in
                       {
                         dbg = (debug_info_of_rpc __x91__);
                         id =
                           ((match __x92__ with
                             | Rpc.String x -> x
                             | __x__ ->
                                 (if Rpc.get_debug ()
                                  then
                                    Printf.eprintf
                                      "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                      "request" "__x92__"
                                      (Rpc.to_string __x__) "String(string)"
                                  else ();
                                  raise
                                    (Rpc.Runtime_error
                                       ("String(string)", __x__)))))
                       }
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "request" "__x89__" (Rpc.to_string __x__) "Dict"
                        else ();
                        raise (Rpc.Runtime_error ("Dict", __x__))))
            type response = dp
            let rpc_of_response = function | __x88__ -> rpc_of_dp __x88__
            let response_of_rpc = function | __x87__ -> dp_of_rpc __x87__
            let call_of_create ~dbg  ~id  =
              let arg = { id; dbg } in
              Rpc.call "DP.create" [rpc_of_request arg]
          end
        module Destroy =
          struct
            type request = {
              dbg: debug_info ;
              dp: dp ;
              allow_leak: bool }
            let rpc_of_request =
              function
              | __x103__ ->
                  let __x104__ = __x103__.dbg
                  and __x105__ = __x103__.dp
                  and __x106__ = __x103__.allow_leak in
                  Rpc.Dict
                    [("dbg", (rpc_of_debug_info __x104__));
                    ("dp", (rpc_of_dp __x105__));
                    ("allow_leak", (Rpc.Bool __x106__))]
            let request_of_rpc =
              function
              | __x98__ ->
                  (match __x98__ with
                   | Rpc.Dict __x99__ ->
                       let __x100__ =
                         try List.assoc "dbg" __x99__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x99__"
                                  (Printexc.to_string __x__)
                                  "Looking for key dbg"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key dbg",
                                     (Printexc.to_string __x__))))
                       and __x101__ =
                         try List.assoc "dp" __x99__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x99__"
                                  (Printexc.to_string __x__)
                                  "Looking for key dp"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key dp",
                                     (Printexc.to_string __x__))))
                       and __x102__ =
                         try List.assoc "allow_leak" __x99__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x99__"
                                  (Printexc.to_string __x__)
                                  "Looking for key allow_leak"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key allow_leak",
                                     (Printexc.to_string __x__)))) in
                       {
                         dbg = (debug_info_of_rpc __x100__);
                         dp = (dp_of_rpc __x101__);
                         allow_leak =
                           ((match __x102__ with
                             | Rpc.Bool x -> x
                             | __x__ ->
                                 (if Rpc.get_debug ()
                                  then
                                    Printf.eprintf
                                      "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                      "request" "__x102__"
                                      (Rpc.to_string __x__) "Bool"
                                  else ();
                                  raise (Rpc.Runtime_error ("Bool", __x__)))))
                       }
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "request" "__x98__" (Rpc.to_string __x__) "Dict"
                        else ();
                        raise (Rpc.Runtime_error ("Dict", __x__))))
            type response = unit
            let rpc_of_response = function | __x97__ -> Rpc.Null
            let response_of_rpc =
              function
              | __x96__ ->
                  (match __x96__ with
                   | Rpc.Null -> ()
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "response" "__x96__" (Rpc.to_string __x__) "Null"
                        else ();
                        raise (Rpc.Runtime_error ("Null", __x__))))
            let call_of_destroy ~dbg  ~dp  ~allow_leak  =
              let arg = { allow_leak; dp; dbg } in
              Rpc.call "DP.destroy" [rpc_of_request arg]
          end
        module Attach_info =
          struct
            type request = {
              dbg: debug_info ;
              sr: sr ;
              vdi: vdi ;
              dp: dp }
            let rpc_of_request =
              function
              | __x115__ ->
                  let __x116__ = __x115__.dbg
                  and __x117__ = __x115__.sr
                  and __x118__ = __x115__.vdi
                  and __x119__ = __x115__.dp in
                  Rpc.Dict
                    [("dbg", (rpc_of_debug_info __x116__));
                    ("sr", (rpc_of_sr __x117__));
                    ("vdi", (rpc_of_vdi __x118__));
                    ("dp", (rpc_of_dp __x119__))]
            let request_of_rpc =
              function
              | __x109__ ->
                  (match __x109__ with
                   | Rpc.Dict __x110__ ->
                       let __x111__ =
                         try List.assoc "dbg" __x110__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x110__"
                                  (Printexc.to_string __x__)
                                  "Looking for key dbg"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key dbg",
                                     (Printexc.to_string __x__))))
                       and __x112__ =
                         try List.assoc "sr" __x110__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x110__"
                                  (Printexc.to_string __x__)
                                  "Looking for key sr"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key sr",
                                     (Printexc.to_string __x__))))
                       and __x113__ =
                         try List.assoc "vdi" __x110__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x110__"
                                  (Printexc.to_string __x__)
                                  "Looking for key vdi"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key vdi",
                                     (Printexc.to_string __x__))))
                       and __x114__ =
                         try List.assoc "dp" __x110__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x110__"
                                  (Printexc.to_string __x__)
                                  "Looking for key dp"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key dp",
                                     (Printexc.to_string __x__)))) in
                       {
                         dbg = (debug_info_of_rpc __x111__);
                         sr = (sr_of_rpc __x112__);
                         vdi = (vdi_of_rpc __x113__);
                         dp = (dp_of_rpc __x114__)
                       }
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "request" "__x109__" (Rpc.to_string __x__) "Dict"
                        else ();
                        raise (Rpc.Runtime_error ("Dict", __x__))))
            type response = backend
            let rpc_of_response =
              function | __x108__ -> rpc_of_backend __x108__
            let response_of_rpc =
              function | __x107__ -> backend_of_rpc __x107__
            let call_of_attach_info ~dbg  ~sr  ~vdi  ~dp  =
              let arg = { dp; vdi; sr; dbg } in
              Rpc.call "DP.attach_info" [rpc_of_request arg]
          end
        module Diagnostics =
          struct
            let rpc_of___x1__ = function | __x121__ -> Rpc.Null
            and __x1___of_rpc =
              function
              | __x120__ ->
                  (match __x120__ with
                   | Rpc.Null -> ()
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "__x1__" "__x120__" (Rpc.to_string __x__) "Null"
                        else ();
                        raise (Rpc.Runtime_error ("Null", __x__))))
            type response = string
            let rpc_of_response = function | __x123__ -> Rpc.String __x123__
            let response_of_rpc =
              function
              | __x122__ ->
                  (match __x122__ with
                   | Rpc.String x -> x
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "response" "__x122__" (Rpc.to_string __x__)
                            "String(string)"
                        else ();
                        raise (Rpc.Runtime_error ("String(string)", __x__))))
            let call_of_diagnostics =
              function
              | __x1__ -> Rpc.call "DP.diagnostics" [rpc_of___x1__ __x1__]
          end
        module Stat_vdi =
          struct
            type request = {
              dbg: debug_info ;
              sr: sr ;
              vdi: vdi }
            let rpc_of_request =
              function
              | __x133__ ->
                  let __x134__ = __x133__.dbg
                  and __x135__ = __x133__.sr
                  and __x136__ = __x133__.vdi in
                  Rpc.Dict
                    [("dbg", (rpc_of_debug_info __x134__));
                    ("sr", (rpc_of_sr __x135__));
                    ("vdi", (rpc_of_vdi __x136__))]
            let request_of_rpc =
              function
              | __x128__ ->
                  (match __x128__ with
                   | Rpc.Dict __x129__ ->
                       let __x130__ =
                         try List.assoc "dbg" __x129__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x129__"
                                  (Printexc.to_string __x__)
                                  "Looking for key dbg"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key dbg",
                                     (Printexc.to_string __x__))))
                       and __x131__ =
                         try List.assoc "sr" __x129__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x129__"
                                  (Printexc.to_string __x__)
                                  "Looking for key sr"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key sr",
                                     (Printexc.to_string __x__))))
                       and __x132__ =
                         try List.assoc "vdi" __x129__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x129__"
                                  (Printexc.to_string __x__)
                                  "Looking for key vdi"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key vdi",
                                     (Printexc.to_string __x__)))) in
                       {
                         dbg = (debug_info_of_rpc __x130__);
                         sr = (sr_of_rpc __x131__);
                         vdi = (vdi_of_rpc __x132__)
                       }
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "request" "__x128__" (Rpc.to_string __x__) "Dict"
                        else ();
                        raise (Rpc.Runtime_error ("Dict", __x__))))
            let rpc_of___x4__ = function | __x125__ -> Rpc.Null
            and __x4___of_rpc =
              function
              | __x124__ ->
                  (match __x124__ with
                   | Rpc.Null -> ()
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "__x4__" "__x124__" (Rpc.to_string __x__) "Null"
                        else ();
                        raise (Rpc.Runtime_error ("Null", __x__))))
            type response = dp_stat_t
            let rpc_of_response =
              function | __x127__ -> rpc_of_dp_stat_t __x127__
            let response_of_rpc =
              function | __x126__ -> dp_stat_t_of_rpc __x126__
            let call_of_stat_vdi ~dbg  ~sr  ~vdi  =
              function
              | __x4__ ->
                  let arg = { vdi; sr; dbg } in
                  Rpc.call "DP.stat_vdi"
                    [rpc_of_request arg; rpc_of___x4__ __x4__]
          end
      end
    module SR =
      struct
        module Create =
          struct
            type request =
              {
              dbg: debug_info ;
              sr: sr ;
              name_label: string ;
              name_description: string ;
              device_config: (string * string) list ;
              physical_size: int64 }
            let rpc_of_request =
              function
              | __x150__ ->
                  let __x151__ = __x150__.dbg
                  and __x152__ = __x150__.sr
                  and __x153__ = __x150__.name_label
                  and __x154__ = __x150__.name_description
                  and __x155__ = __x150__.device_config
                  and __x156__ = __x150__.physical_size in
                  Rpc.Dict
                    [("dbg", (rpc_of_debug_info __x151__));
                    ("sr", (rpc_of_sr __x152__));
                    ("name_label", (Rpc.String __x153__));
                    ("name_description", (Rpc.String __x154__));
                    ("device_config",
                      ((let dict =
                          List.map
                            (function
                             | (key, __x157__) ->
                                 (key, (Rpc.String __x157__))) __x155__ in
                        Rpc.Dict dict)));
                    ("physical_size", (Rpc.Int __x156__))]
            let request_of_rpc =
              function
              | __x141__ ->
                  (match __x141__ with
                   | Rpc.Dict __x142__ ->
                       let __x143__ =
                         try List.assoc "dbg" __x142__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x142__"
                                  (Printexc.to_string __x__)
                                  "Looking for key dbg"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key dbg",
                                     (Printexc.to_string __x__))))
                       and __x144__ =
                         try List.assoc "sr" __x142__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x142__"
                                  (Printexc.to_string __x__)
                                  "Looking for key sr"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key sr",
                                     (Printexc.to_string __x__))))
                       and __x145__ =
                         try List.assoc "name_label" __x142__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x142__"
                                  (Printexc.to_string __x__)
                                  "Looking for key name_label"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key name_label",
                                     (Printexc.to_string __x__))))
                       and __x146__ =
                         try List.assoc "name_description" __x142__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x142__"
                                  (Printexc.to_string __x__)
                                  "Looking for key name_description"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key name_description",
                                     (Printexc.to_string __x__))))
                       and __x147__ =
                         try List.assoc "device_config" __x142__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x142__"
                                  (Printexc.to_string __x__)
                                  "Looking for key device_config"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key device_config",
                                     (Printexc.to_string __x__))))
                       and __x148__ =
                         try List.assoc "physical_size" __x142__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x142__"
                                  (Printexc.to_string __x__)
                                  "Looking for key physical_size"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key physical_size",
                                     (Printexc.to_string __x__)))) in
                       {
                         dbg = (debug_info_of_rpc __x143__);
                         sr = (sr_of_rpc __x144__);
                         name_label =
                           ((match __x145__ with
                             | Rpc.String x -> x
                             | __x__ ->
                                 (if Rpc.get_debug ()
                                  then
                                    Printf.eprintf
                                      "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                      "request" "__x145__"
                                      (Rpc.to_string __x__) "String(string)"
                                  else ();
                                  raise
                                    (Rpc.Runtime_error
                                       ("String(string)", __x__)))));
                         name_description =
                           ((match __x146__ with
                             | Rpc.String x -> x
                             | __x__ ->
                                 (if Rpc.get_debug ()
                                  then
                                    Printf.eprintf
                                      "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                      "request" "__x146__"
                                      (Rpc.to_string __x__) "String(string)"
                                  else ();
                                  raise
                                    (Rpc.Runtime_error
                                       ("String(string)", __x__)))));
                         device_config =
                           ((match __x147__ with
                             | Rpc.Dict d ->
                                 List.map
                                   (function
                                    | (key, __x149__) ->
                                        (key,
                                          ((match __x149__ with
                                            | Rpc.String x -> x
                                            | __x__ ->
                                                (if Rpc.get_debug ()
                                                 then
                                                   Printf.eprintf
                                                     "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                                     "request" "__x149__"
                                                     (Rpc.to_string __x__)
                                                     "String(string)"
                                                 else ();
                                                 raise
                                                   (Rpc.Runtime_error
                                                      ("String(string)",
                                                        __x__))))))) d
                             | __x__ ->
                                 (if Rpc.get_debug ()
                                  then
                                    Printf.eprintf
                                      "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                      "request" "__x147__"
                                      (Rpc.to_string __x__) "Dict"
                                  else ();
                                  raise (Rpc.Runtime_error ("Dict", __x__)))));
                         physical_size =
                           ((match __x148__ with
                             | Rpc.Int x -> x
                             | Rpc.String s -> Int64.of_string s
                             | __x__ ->
                                 (if Rpc.get_debug ()
                                  then
                                    Printf.eprintf
                                      "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                      "request" "__x148__"
                                      (Rpc.to_string __x__) "Int(int64)"
                                  else ();
                                  raise
                                    (Rpc.Runtime_error ("Int(int64)", __x__)))))
                       }
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "request" "__x141__" (Rpc.to_string __x__) "Dict"
                        else ();
                        raise (Rpc.Runtime_error ("Dict", __x__))))
            type response = (string * string) list
            let rpc_of_response =
              function
              | __x139__ ->
                  let dict =
                    List.map
                      (function
                       | (key, __x140__) -> (key, (Rpc.String __x140__)))
                      __x139__ in
                  Rpc.Dict dict
            let response_of_rpc =
              function
              | __x137__ ->
                  (match __x137__ with
                   | Rpc.Dict d ->
                       List.map
                         (function
                          | (key, __x138__) ->
                              (key,
                                ((match __x138__ with
                                  | Rpc.String x -> x
                                  | __x__ ->
                                      (if Rpc.get_debug ()
                                       then
                                         Printf.eprintf
                                           "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                           "response" "__x138__"
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
                            "response" "__x137__" (Rpc.to_string __x__)
                            "Dict"
                        else ();
                        raise (Rpc.Runtime_error ("Dict", __x__))))
            let call_of_create ~dbg  ~sr  ~name_label  ~name_description 
              ~device_config  ~physical_size  =
              let arg =
                {
                  physical_size;
                  device_config;
                  name_description;
                  name_label;
                  sr;
                  dbg
                } in
              Rpc.call "SR.create" [rpc_of_request arg]
          end
        module Set_name_label =
          struct
            type request = {
              dbg: debug_info ;
              sr: sr ;
              new_name_label: string }
            let rpc_of_request =
              function
              | __x165__ ->
                  let __x166__ = __x165__.dbg
                  and __x167__ = __x165__.sr
                  and __x168__ = __x165__.new_name_label in
                  Rpc.Dict
                    [("dbg", (rpc_of_debug_info __x166__));
                    ("sr", (rpc_of_sr __x167__));
                    ("new_name_label", (Rpc.String __x168__))]
            let request_of_rpc =
              function
              | __x160__ ->
                  (match __x160__ with
                   | Rpc.Dict __x161__ ->
                       let __x162__ =
                         try List.assoc "dbg" __x161__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x161__"
                                  (Printexc.to_string __x__)
                                  "Looking for key dbg"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key dbg",
                                     (Printexc.to_string __x__))))
                       and __x163__ =
                         try List.assoc "sr" __x161__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x161__"
                                  (Printexc.to_string __x__)
                                  "Looking for key sr"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key sr",
                                     (Printexc.to_string __x__))))
                       and __x164__ =
                         try List.assoc "new_name_label" __x161__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x161__"
                                  (Printexc.to_string __x__)
                                  "Looking for key new_name_label"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key new_name_label",
                                     (Printexc.to_string __x__)))) in
                       {
                         dbg = (debug_info_of_rpc __x162__);
                         sr = (sr_of_rpc __x163__);
                         new_name_label =
                           ((match __x164__ with
                             | Rpc.String x -> x
                             | __x__ ->
                                 (if Rpc.get_debug ()
                                  then
                                    Printf.eprintf
                                      "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                      "request" "__x164__"
                                      (Rpc.to_string __x__) "String(string)"
                                  else ();
                                  raise
                                    (Rpc.Runtime_error
                                       ("String(string)", __x__)))))
                       }
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "request" "__x160__" (Rpc.to_string __x__) "Dict"
                        else ();
                        raise (Rpc.Runtime_error ("Dict", __x__))))
            type response = unit
            let rpc_of_response = function | __x159__ -> Rpc.Null
            let response_of_rpc =
              function
              | __x158__ ->
                  (match __x158__ with
                   | Rpc.Null -> ()
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "response" "__x158__" (Rpc.to_string __x__)
                            "Null"
                        else ();
                        raise (Rpc.Runtime_error ("Null", __x__))))
            let call_of_set_name_label ~dbg  ~sr  ~new_name_label  =
              let arg = { new_name_label; sr; dbg } in
              Rpc.call "SR.set_name_label" [rpc_of_request arg]
          end
        module Set_name_description =
          struct
            type request =
              {
              dbg: debug_info ;
              sr: sr ;
              new_name_description: string }
            let rpc_of_request =
              function
              | __x176__ ->
                  let __x177__ = __x176__.dbg
                  and __x178__ = __x176__.sr
                  and __x179__ = __x176__.new_name_description in
                  Rpc.Dict
                    [("dbg", (rpc_of_debug_info __x177__));
                    ("sr", (rpc_of_sr __x178__));
                    ("new_name_description", (Rpc.String __x179__))]
            let request_of_rpc =
              function
              | __x171__ ->
                  (match __x171__ with
                   | Rpc.Dict __x172__ ->
                       let __x173__ =
                         try List.assoc "dbg" __x172__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x172__"
                                  (Printexc.to_string __x__)
                                  "Looking for key dbg"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key dbg",
                                     (Printexc.to_string __x__))))
                       and __x174__ =
                         try List.assoc "sr" __x172__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x172__"
                                  (Printexc.to_string __x__)
                                  "Looking for key sr"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key sr",
                                     (Printexc.to_string __x__))))
                       and __x175__ =
                         try List.assoc "new_name_description" __x172__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x172__"
                                  (Printexc.to_string __x__)
                                  "Looking for key new_name_description"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key new_name_description",
                                     (Printexc.to_string __x__)))) in
                       {
                         dbg = (debug_info_of_rpc __x173__);
                         sr = (sr_of_rpc __x174__);
                         new_name_description =
                           ((match __x175__ with
                             | Rpc.String x -> x
                             | __x__ ->
                                 (if Rpc.get_debug ()
                                  then
                                    Printf.eprintf
                                      "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                      "request" "__x175__"
                                      (Rpc.to_string __x__) "String(string)"
                                  else ();
                                  raise
                                    (Rpc.Runtime_error
                                       ("String(string)", __x__)))))
                       }
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "request" "__x171__" (Rpc.to_string __x__) "Dict"
                        else ();
                        raise (Rpc.Runtime_error ("Dict", __x__))))
            type response = unit
            let rpc_of_response = function | __x170__ -> Rpc.Null
            let response_of_rpc =
              function
              | __x169__ ->
                  (match __x169__ with
                   | Rpc.Null -> ()
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "response" "__x169__" (Rpc.to_string __x__)
                            "Null"
                        else ();
                        raise (Rpc.Runtime_error ("Null", __x__))))
            let call_of_set_name_description ~dbg  ~sr  ~new_name_description
               =
              let arg = { new_name_description; sr; dbg } in
              Rpc.call "SR.set_name_description" [rpc_of_request arg]
          end
        module Probe =
          struct
            type request =
              {
              dbg: debug_info ;
              queue: string ;
              device_config: (string * string) list ;
              sm_config: (string * string) list }
            let rpc_of_request =
              function
              | __x190__ ->
                  let __x191__ = __x190__.dbg
                  and __x192__ = __x190__.queue
                  and __x193__ = __x190__.device_config
                  and __x194__ = __x190__.sm_config in
                  Rpc.Dict
                    [("dbg", (rpc_of_debug_info __x191__));
                    ("queue", (Rpc.String __x192__));
                    ("device_config",
                      ((let dict =
                          List.map
                            (function
                             | (key, __x196__) ->
                                 (key, (Rpc.String __x196__))) __x193__ in
                        Rpc.Dict dict)));
                    ("sm_config",
                      ((let dict =
                          List.map
                            (function
                             | (key, __x195__) ->
                                 (key, (Rpc.String __x195__))) __x194__ in
                        Rpc.Dict dict)))]
            let request_of_rpc =
              function
              | __x182__ ->
                  (match __x182__ with
                   | Rpc.Dict __x183__ ->
                       let __x184__ =
                         try List.assoc "dbg" __x183__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x183__"
                                  (Printexc.to_string __x__)
                                  "Looking for key dbg"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key dbg",
                                     (Printexc.to_string __x__))))
                       and __x185__ =
                         try List.assoc "queue" __x183__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x183__"
                                  (Printexc.to_string __x__)
                                  "Looking for key queue"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key queue",
                                     (Printexc.to_string __x__))))
                       and __x186__ =
                         try List.assoc "device_config" __x183__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x183__"
                                  (Printexc.to_string __x__)
                                  "Looking for key device_config"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key device_config",
                                     (Printexc.to_string __x__))))
                       and __x187__ =
                         try List.assoc "sm_config" __x183__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x183__"
                                  (Printexc.to_string __x__)
                                  "Looking for key sm_config"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key sm_config",
                                     (Printexc.to_string __x__)))) in
                       {
                         dbg = (debug_info_of_rpc __x184__);
                         queue =
                           ((match __x185__ with
                             | Rpc.String x -> x
                             | __x__ ->
                                 (if Rpc.get_debug ()
                                  then
                                    Printf.eprintf
                                      "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                      "request" "__x185__"
                                      (Rpc.to_string __x__) "String(string)"
                                  else ();
                                  raise
                                    (Rpc.Runtime_error
                                       ("String(string)", __x__)))));
                         device_config =
                           ((match __x186__ with
                             | Rpc.Dict d ->
                                 List.map
                                   (function
                                    | (key, __x188__) ->
                                        (key,
                                          ((match __x188__ with
                                            | Rpc.String x -> x
                                            | __x__ ->
                                                (if Rpc.get_debug ()
                                                 then
                                                   Printf.eprintf
                                                     "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                                     "request" "__x188__"
                                                     (Rpc.to_string __x__)
                                                     "String(string)"
                                                 else ();
                                                 raise
                                                   (Rpc.Runtime_error
                                                      ("String(string)",
                                                        __x__))))))) d
                             | __x__ ->
                                 (if Rpc.get_debug ()
                                  then
                                    Printf.eprintf
                                      "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                      "request" "__x186__"
                                      (Rpc.to_string __x__) "Dict"
                                  else ();
                                  raise (Rpc.Runtime_error ("Dict", __x__)))));
                         sm_config =
                           ((match __x187__ with
                             | Rpc.Dict d ->
                                 List.map
                                   (function
                                    | (key, __x189__) ->
                                        (key,
                                          ((match __x189__ with
                                            | Rpc.String x -> x
                                            | __x__ ->
                                                (if Rpc.get_debug ()
                                                 then
                                                   Printf.eprintf
                                                     "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                                     "request" "__x189__"
                                                     (Rpc.to_string __x__)
                                                     "String(string)"
                                                 else ();
                                                 raise
                                                   (Rpc.Runtime_error
                                                      ("String(string)",
                                                        __x__))))))) d
                             | __x__ ->
                                 (if Rpc.get_debug ()
                                  then
                                    Printf.eprintf
                                      "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                      "request" "__x187__"
                                      (Rpc.to_string __x__) "Dict"
                                  else ();
                                  raise (Rpc.Runtime_error ("Dict", __x__)))))
                       }
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "request" "__x182__" (Rpc.to_string __x__) "Dict"
                        else ();
                        raise (Rpc.Runtime_error ("Dict", __x__))))
            type response = probe_result
            let rpc_of_response =
              function | __x181__ -> rpc_of_probe_result __x181__
            let response_of_rpc =
              function | __x180__ -> probe_result_of_rpc __x180__
            let call_of_probe ~dbg  ~queue  ~device_config  ~sm_config  =
              let arg = { sm_config; device_config; queue; dbg } in
              Rpc.call "SR.probe" [rpc_of_request arg]
          end
        module Attach =
          struct
            type request =
              {
              dbg: debug_info ;
              sr: sr ;
              device_config: (string * string) list }
            let rpc_of_request =
              function
              | __x205__ ->
                  let __x206__ = __x205__.dbg
                  and __x207__ = __x205__.sr
                  and __x208__ = __x205__.device_config in
                  Rpc.Dict
                    [("dbg", (rpc_of_debug_info __x206__));
                    ("sr", (rpc_of_sr __x207__));
                    ("device_config",
                      ((let dict =
                          List.map
                            (function
                             | (key, __x209__) ->
                                 (key, (Rpc.String __x209__))) __x208__ in
                        Rpc.Dict dict)))]
            let request_of_rpc =
              function
              | __x199__ ->
                  (match __x199__ with
                   | Rpc.Dict __x200__ ->
                       let __x201__ =
                         try List.assoc "dbg" __x200__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x200__"
                                  (Printexc.to_string __x__)
                                  "Looking for key dbg"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key dbg",
                                     (Printexc.to_string __x__))))
                       and __x202__ =
                         try List.assoc "sr" __x200__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x200__"
                                  (Printexc.to_string __x__)
                                  "Looking for key sr"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key sr",
                                     (Printexc.to_string __x__))))
                       and __x203__ =
                         try List.assoc "device_config" __x200__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x200__"
                                  (Printexc.to_string __x__)
                                  "Looking for key device_config"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key device_config",
                                     (Printexc.to_string __x__)))) in
                       {
                         dbg = (debug_info_of_rpc __x201__);
                         sr = (sr_of_rpc __x202__);
                         device_config =
                           ((match __x203__ with
                             | Rpc.Dict d ->
                                 List.map
                                   (function
                                    | (key, __x204__) ->
                                        (key,
                                          ((match __x204__ with
                                            | Rpc.String x -> x
                                            | __x__ ->
                                                (if Rpc.get_debug ()
                                                 then
                                                   Printf.eprintf
                                                     "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                                     "request" "__x204__"
                                                     (Rpc.to_string __x__)
                                                     "String(string)"
                                                 else ();
                                                 raise
                                                   (Rpc.Runtime_error
                                                      ("String(string)",
                                                        __x__))))))) d
                             | __x__ ->
                                 (if Rpc.get_debug ()
                                  then
                                    Printf.eprintf
                                      "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                      "request" "__x203__"
                                      (Rpc.to_string __x__) "Dict"
                                  else ();
                                  raise (Rpc.Runtime_error ("Dict", __x__)))))
                       }
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "request" "__x199__" (Rpc.to_string __x__) "Dict"
                        else ();
                        raise (Rpc.Runtime_error ("Dict", __x__))))
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
            let call_of_attach ~dbg  ~sr  ~device_config  =
              let arg = { device_config; sr; dbg } in
              Rpc.call "SR.attach" [rpc_of_request arg]
          end
        module Detach =
          struct
            type request = {
              dbg: debug_info ;
              sr: sr }
            let rpc_of_request =
              function
              | __x216__ ->
                  let __x217__ = __x216__.dbg
                  and __x218__ = __x216__.sr in
                  Rpc.Dict
                    [("dbg", (rpc_of_debug_info __x217__));
                    ("sr", (rpc_of_sr __x218__))]
            let request_of_rpc =
              function
              | __x212__ ->
                  (match __x212__ with
                   | Rpc.Dict __x213__ ->
                       let __x214__ =
                         try List.assoc "dbg" __x213__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x213__"
                                  (Printexc.to_string __x__)
                                  "Looking for key dbg"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key dbg",
                                     (Printexc.to_string __x__))))
                       and __x215__ =
                         try List.assoc "sr" __x213__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x213__"
                                  (Printexc.to_string __x__)
                                  "Looking for key sr"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key sr",
                                     (Printexc.to_string __x__)))) in
                       {
                         dbg = (debug_info_of_rpc __x214__);
                         sr = (sr_of_rpc __x215__)
                       }
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "request" "__x212__" (Rpc.to_string __x__) "Dict"
                        else ();
                        raise (Rpc.Runtime_error ("Dict", __x__))))
            type response = unit
            let rpc_of_response = function | __x211__ -> Rpc.Null
            let response_of_rpc =
              function
              | __x210__ ->
                  (match __x210__ with
                   | Rpc.Null -> ()
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "response" "__x210__" (Rpc.to_string __x__)
                            "Null"
                        else ();
                        raise (Rpc.Runtime_error ("Null", __x__))))
            let call_of_detach ~dbg  ~sr  =
              let arg = { sr; dbg } in
              Rpc.call "SR.detach" [rpc_of_request arg]
          end
        module Reset =
          struct
            type request = {
              dbg: debug_info ;
              sr: sr }
            let rpc_of_request =
              function
              | __x225__ ->
                  let __x226__ = __x225__.dbg
                  and __x227__ = __x225__.sr in
                  Rpc.Dict
                    [("dbg", (rpc_of_debug_info __x226__));
                    ("sr", (rpc_of_sr __x227__))]
            let request_of_rpc =
              function
              | __x221__ ->
                  (match __x221__ with
                   | Rpc.Dict __x222__ ->
                       let __x223__ =
                         try List.assoc "dbg" __x222__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x222__"
                                  (Printexc.to_string __x__)
                                  "Looking for key dbg"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key dbg",
                                     (Printexc.to_string __x__))))
                       and __x224__ =
                         try List.assoc "sr" __x222__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x222__"
                                  (Printexc.to_string __x__)
                                  "Looking for key sr"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key sr",
                                     (Printexc.to_string __x__)))) in
                       {
                         dbg = (debug_info_of_rpc __x223__);
                         sr = (sr_of_rpc __x224__)
                       }
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "request" "__x221__" (Rpc.to_string __x__) "Dict"
                        else ();
                        raise (Rpc.Runtime_error ("Dict", __x__))))
            type response = unit
            let rpc_of_response = function | __x220__ -> Rpc.Null
            let response_of_rpc =
              function
              | __x219__ ->
                  (match __x219__ with
                   | Rpc.Null -> ()
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "response" "__x219__" (Rpc.to_string __x__)
                            "Null"
                        else ();
                        raise (Rpc.Runtime_error ("Null", __x__))))
            let call_of_reset ~dbg  ~sr  =
              let arg = { sr; dbg } in
              Rpc.call "SR.reset" [rpc_of_request arg]
          end
        module Destroy =
          struct
            type request = {
              dbg: debug_info ;
              sr: sr }
            let rpc_of_request =
              function
              | __x234__ ->
                  let __x235__ = __x234__.dbg
                  and __x236__ = __x234__.sr in
                  Rpc.Dict
                    [("dbg", (rpc_of_debug_info __x235__));
                    ("sr", (rpc_of_sr __x236__))]
            let request_of_rpc =
              function
              | __x230__ ->
                  (match __x230__ with
                   | Rpc.Dict __x231__ ->
                       let __x232__ =
                         try List.assoc "dbg" __x231__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x231__"
                                  (Printexc.to_string __x__)
                                  "Looking for key dbg"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key dbg",
                                     (Printexc.to_string __x__))))
                       and __x233__ =
                         try List.assoc "sr" __x231__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x231__"
                                  (Printexc.to_string __x__)
                                  "Looking for key sr"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key sr",
                                     (Printexc.to_string __x__)))) in
                       {
                         dbg = (debug_info_of_rpc __x232__);
                         sr = (sr_of_rpc __x233__)
                       }
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "request" "__x230__" (Rpc.to_string __x__) "Dict"
                        else ();
                        raise (Rpc.Runtime_error ("Dict", __x__))))
            type response = unit
            let rpc_of_response = function | __x229__ -> Rpc.Null
            let response_of_rpc =
              function
              | __x228__ ->
                  (match __x228__ with
                   | Rpc.Null -> ()
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "response" "__x228__" (Rpc.to_string __x__)
                            "Null"
                        else ();
                        raise (Rpc.Runtime_error ("Null", __x__))))
            let call_of_destroy ~dbg  ~sr  =
              let arg = { sr; dbg } in
              Rpc.call "SR.destroy" [rpc_of_request arg]
          end
        module Scan =
          struct
            type request = {
              dbg: debug_info ;
              sr: sr }
            let rpc_of_request =
              function
              | __x246__ ->
                  let __x247__ = __x246__.dbg
                  and __x248__ = __x246__.sr in
                  Rpc.Dict
                    [("dbg", (rpc_of_debug_info __x247__));
                    ("sr", (rpc_of_sr __x248__))]
            let request_of_rpc =
              function
              | __x242__ ->
                  (match __x242__ with
                   | Rpc.Dict __x243__ ->
                       let __x244__ =
                         try List.assoc "dbg" __x243__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x243__"
                                  (Printexc.to_string __x__)
                                  "Looking for key dbg"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key dbg",
                                     (Printexc.to_string __x__))))
                       and __x245__ =
                         try List.assoc "sr" __x243__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x243__"
                                  (Printexc.to_string __x__)
                                  "Looking for key sr"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key sr",
                                     (Printexc.to_string __x__)))) in
                       {
                         dbg = (debug_info_of_rpc __x244__);
                         sr = (sr_of_rpc __x245__)
                       }
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "request" "__x242__" (Rpc.to_string __x__) "Dict"
                        else ();
                        raise (Rpc.Runtime_error ("Dict", __x__))))
            type response = vdi_info list
            let rpc_of_response =
              function
              | __x240__ ->
                  Rpc.Enum
                    (List.map
                       (function | __x241__ -> rpc_of_vdi_info __x241__)
                       __x240__)
            let response_of_rpc =
              function
              | __x237__ ->
                  (match __x237__ with
                   | Rpc.Enum __x238__ ->
                       List.map
                         (function | __x239__ -> vdi_info_of_rpc __x239__)
                         __x238__
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "response" "__x237__" (Rpc.to_string __x__)
                            "List"
                        else ();
                        raise (Rpc.Runtime_error ("List", __x__))))
            let call_of_scan ~dbg  ~sr  =
              let arg = { sr; dbg } in
              Rpc.call "SR.scan" [rpc_of_request arg]
          end
        module Update_snapshot_info_src =
          struct
            type request =
              {
              dbg: debug_info ;
              sr: sr ;
              vdi: vdi ;
              url: string ;
              dest: sr ;
              dest_vdi: vdi ;
              snapshot_pairs: (vdi * vdi) list }
            let rpc_of_request =
              function
              | __x263__ ->
                  let __x264__ = __x263__.dbg
                  and __x265__ = __x263__.sr
                  and __x266__ = __x263__.vdi
                  and __x267__ = __x263__.url
                  and __x268__ = __x263__.dest
                  and __x269__ = __x263__.dest_vdi
                  and __x270__ = __x263__.snapshot_pairs in
                  Rpc.Dict
                    [("dbg", (rpc_of_debug_info __x264__));
                    ("sr", (rpc_of_sr __x265__));
                    ("vdi", (rpc_of_vdi __x266__));
                    ("url", (Rpc.String __x267__));
                    ("dest", (rpc_of_sr __x268__));
                    ("dest_vdi", (rpc_of_vdi __x269__));
                    ("snapshot_pairs",
                      ((let is_a_real_dict =
                          try
                            let (_ : vdi) = vdi_of_rpc (Rpc.String "") in
                            true
                          with | _ -> false in
                        let dict =
                          List.map
                            (function
                             | (__x271__, __x272__) ->
                                 ((rpc_of_vdi __x271__),
                                   (rpc_of_vdi __x272__))) __x270__ in
                        if is_a_real_dict
                        then
                          Rpc.Dict
                            (List.map
                               (function
                                | (Rpc.String k, v) -> (k, v)
                                | _ -> assert false) dict)
                        else
                          Rpc.Enum
                            (List.map (function | (k, v) -> Rpc.Enum [k; v])
                               dict))))]
            let request_of_rpc =
              function
              | __x251__ ->
                  (match __x251__ with
                   | Rpc.Dict __x252__ ->
                       let __x253__ =
                         try List.assoc "dbg" __x252__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x252__"
                                  (Printexc.to_string __x__)
                                  "Looking for key dbg"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key dbg",
                                     (Printexc.to_string __x__))))
                       and __x254__ =
                         try List.assoc "sr" __x252__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x252__"
                                  (Printexc.to_string __x__)
                                  "Looking for key sr"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key sr",
                                     (Printexc.to_string __x__))))
                       and __x255__ =
                         try List.assoc "vdi" __x252__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x252__"
                                  (Printexc.to_string __x__)
                                  "Looking for key vdi"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key vdi",
                                     (Printexc.to_string __x__))))
                       and __x256__ =
                         try List.assoc "url" __x252__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x252__"
                                  (Printexc.to_string __x__)
                                  "Looking for key url"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key url",
                                     (Printexc.to_string __x__))))
                       and __x257__ =
                         try List.assoc "dest" __x252__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x252__"
                                  (Printexc.to_string __x__)
                                  "Looking for key dest"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key dest",
                                     (Printexc.to_string __x__))))
                       and __x258__ =
                         try List.assoc "dest_vdi" __x252__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x252__"
                                  (Printexc.to_string __x__)
                                  "Looking for key dest_vdi"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key dest_vdi",
                                     (Printexc.to_string __x__))))
                       and __x259__ =
                         try List.assoc "snapshot_pairs" __x252__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x252__"
                                  (Printexc.to_string __x__)
                                  "Looking for key snapshot_pairs"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key snapshot_pairs",
                                     (Printexc.to_string __x__)))) in
                       {
                         dbg = (debug_info_of_rpc __x253__);
                         sr = (sr_of_rpc __x254__);
                         vdi = (vdi_of_rpc __x255__);
                         url =
                           ((match __x256__ with
                             | Rpc.String x -> x
                             | __x__ ->
                                 (if Rpc.get_debug ()
                                  then
                                    Printf.eprintf
                                      "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                      "request" "__x256__"
                                      (Rpc.to_string __x__) "String(string)"
                                  else ();
                                  raise
                                    (Rpc.Runtime_error
                                       ("String(string)", __x__)))));
                         dest = (sr_of_rpc __x257__);
                         dest_vdi = (vdi_of_rpc __x258__);
                         snapshot_pairs =
                           (let is_a_real_dict =
                              try
                                let (_ : vdi) = vdi_of_rpc (Rpc.String "") in
                                true
                              with | _ -> false in
                            if is_a_real_dict
                            then
                              (match __x259__ with
                               | Rpc.Dict d ->
                                   List.map
                                     (function
                                      | (key, __x260__) ->
                                          ((vdi_of_rpc (Rpc.String key)),
                                            (vdi_of_rpc __x260__))) d
                               | __x__ ->
                                   (if Rpc.get_debug ()
                                    then
                                      Printf.eprintf
                                        "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                        "request" "__x259__"
                                        (Rpc.to_string __x__) "Dict"
                                    else ();
                                    raise (Rpc.Runtime_error ("Dict", __x__))))
                            else
                              (match __x259__ with
                               | Rpc.Enum e ->
                                   List.map
                                     (function
                                      | __x260__ ->
                                          (match __x260__ with
                                           | Rpc.Enum
                                               (__x261__::__x262__::[]) ->
                                               ((vdi_of_rpc __x261__),
                                                 (vdi_of_rpc __x262__))
                                           | __x__ ->
                                               (if Rpc.get_debug ()
                                                then
                                                  Printf.eprintf
                                                    "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                                    "request" "__x260__"
                                                    (Rpc.to_string __x__)
                                                    "List"
                                                else ();
                                                raise
                                                  (Rpc.Runtime_error
                                                     ("List", __x__))))) e
                               | __x__ ->
                                   (if Rpc.get_debug ()
                                    then
                                      Printf.eprintf
                                        "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                        "request" "__x259__"
                                        (Rpc.to_string __x__) "Enum"
                                    else ();
                                    raise (Rpc.Runtime_error ("Enum", __x__)))))
                       }
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "request" "__x251__" (Rpc.to_string __x__) "Dict"
                        else ();
                        raise (Rpc.Runtime_error ("Dict", __x__))))
            type response = unit
            let rpc_of_response = function | __x250__ -> Rpc.Null
            let response_of_rpc =
              function
              | __x249__ ->
                  (match __x249__ with
                   | Rpc.Null -> ()
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "response" "__x249__" (Rpc.to_string __x__)
                            "Null"
                        else ();
                        raise (Rpc.Runtime_error ("Null", __x__))))
            let call_of_update_snapshot_info_src ~dbg  ~sr  ~vdi  ~url  ~dest
               ~dest_vdi  ~snapshot_pairs  =
              let arg = { snapshot_pairs; dest_vdi; dest; url; vdi; sr; dbg } in
              Rpc.call "SR.update_snapshot_info_src" [rpc_of_request arg]
          end
        module Update_snapshot_info_dest =
          struct
            type request =
              {
              dbg: debug_info ;
              sr: sr ;
              vdi: vdi ;
              src_vdi: vdi_info ;
              snapshot_pairs: (vdi * vdi_info) list }
            let rpc_of_request =
              function
              | __x285__ ->
                  let __x286__ = __x285__.dbg
                  and __x287__ = __x285__.sr
                  and __x288__ = __x285__.vdi
                  and __x289__ = __x285__.src_vdi
                  and __x290__ = __x285__.snapshot_pairs in
                  Rpc.Dict
                    [("dbg", (rpc_of_debug_info __x286__));
                    ("sr", (rpc_of_sr __x287__));
                    ("vdi", (rpc_of_vdi __x288__));
                    ("src_vdi", (rpc_of_vdi_info __x289__));
                    ("snapshot_pairs",
                      ((let is_a_real_dict =
                          try
                            let (_ : vdi) = vdi_of_rpc (Rpc.String "") in
                            true
                          with | _ -> false in
                        let dict =
                          List.map
                            (function
                             | (__x291__, __x292__) ->
                                 ((rpc_of_vdi __x291__),
                                   (rpc_of_vdi_info __x292__))) __x290__ in
                        if is_a_real_dict
                        then
                          Rpc.Dict
                            (List.map
                               (function
                                | (Rpc.String k, v) -> (k, v)
                                | _ -> assert false) dict)
                        else
                          Rpc.Enum
                            (List.map (function | (k, v) -> Rpc.Enum [k; v])
                               dict))))]
            let request_of_rpc =
              function
              | __x275__ ->
                  (match __x275__ with
                   | Rpc.Dict __x276__ ->
                       let __x277__ =
                         try List.assoc "dbg" __x276__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x276__"
                                  (Printexc.to_string __x__)
                                  "Looking for key dbg"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key dbg",
                                     (Printexc.to_string __x__))))
                       and __x278__ =
                         try List.assoc "sr" __x276__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x276__"
                                  (Printexc.to_string __x__)
                                  "Looking for key sr"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key sr",
                                     (Printexc.to_string __x__))))
                       and __x279__ =
                         try List.assoc "vdi" __x276__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x276__"
                                  (Printexc.to_string __x__)
                                  "Looking for key vdi"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key vdi",
                                     (Printexc.to_string __x__))))
                       and __x280__ =
                         try List.assoc "src_vdi" __x276__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x276__"
                                  (Printexc.to_string __x__)
                                  "Looking for key src_vdi"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key src_vdi",
                                     (Printexc.to_string __x__))))
                       and __x281__ =
                         try List.assoc "snapshot_pairs" __x276__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x276__"
                                  (Printexc.to_string __x__)
                                  "Looking for key snapshot_pairs"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key snapshot_pairs",
                                     (Printexc.to_string __x__)))) in
                       {
                         dbg = (debug_info_of_rpc __x277__);
                         sr = (sr_of_rpc __x278__);
                         vdi = (vdi_of_rpc __x279__);
                         src_vdi = (vdi_info_of_rpc __x280__);
                         snapshot_pairs =
                           (let is_a_real_dict =
                              try
                                let (_ : vdi) = vdi_of_rpc (Rpc.String "") in
                                true
                              with | _ -> false in
                            if is_a_real_dict
                            then
                              (match __x281__ with
                               | Rpc.Dict d ->
                                   List.map
                                     (function
                                      | (key, __x282__) ->
                                          ((vdi_of_rpc (Rpc.String key)),
                                            (vdi_info_of_rpc __x282__))) d
                               | __x__ ->
                                   (if Rpc.get_debug ()
                                    then
                                      Printf.eprintf
                                        "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                        "request" "__x281__"
                                        (Rpc.to_string __x__) "Dict"
                                    else ();
                                    raise (Rpc.Runtime_error ("Dict", __x__))))
                            else
                              (match __x281__ with
                               | Rpc.Enum e ->
                                   List.map
                                     (function
                                      | __x282__ ->
                                          (match __x282__ with
                                           | Rpc.Enum
                                               (__x283__::__x284__::[]) ->
                                               ((vdi_of_rpc __x283__),
                                                 (vdi_info_of_rpc __x284__))
                                           | __x__ ->
                                               (if Rpc.get_debug ()
                                                then
                                                  Printf.eprintf
                                                    "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                                    "request" "__x282__"
                                                    (Rpc.to_string __x__)
                                                    "List"
                                                else ();
                                                raise
                                                  (Rpc.Runtime_error
                                                     ("List", __x__))))) e
                               | __x__ ->
                                   (if Rpc.get_debug ()
                                    then
                                      Printf.eprintf
                                        "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                        "request" "__x281__"
                                        (Rpc.to_string __x__) "Enum"
                                    else ();
                                    raise (Rpc.Runtime_error ("Enum", __x__)))))
                       }
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "request" "__x275__" (Rpc.to_string __x__) "Dict"
                        else ();
                        raise (Rpc.Runtime_error ("Dict", __x__))))
            type response = unit
            let rpc_of_response = function | __x274__ -> Rpc.Null
            let response_of_rpc =
              function
              | __x273__ ->
                  (match __x273__ with
                   | Rpc.Null -> ()
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "response" "__x273__" (Rpc.to_string __x__)
                            "Null"
                        else ();
                        raise (Rpc.Runtime_error ("Null", __x__))))
            let call_of_update_snapshot_info_dest ~dbg  ~sr  ~vdi  ~src_vdi 
              ~snapshot_pairs  =
              let arg = { snapshot_pairs; src_vdi; vdi; sr; dbg } in
              Rpc.call "SR.update_snapshot_info_dest" [rpc_of_request arg]
          end
        module Stat =
          struct
            type request = {
              dbg: debug_info ;
              sr: sr }
            let rpc_of_request =
              function
              | __x299__ ->
                  let __x300__ = __x299__.dbg
                  and __x301__ = __x299__.sr in
                  Rpc.Dict
                    [("dbg", (rpc_of_debug_info __x300__));
                    ("sr", (rpc_of_sr __x301__))]
            let request_of_rpc =
              function
              | __x295__ ->
                  (match __x295__ with
                   | Rpc.Dict __x296__ ->
                       let __x297__ =
                         try List.assoc "dbg" __x296__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x296__"
                                  (Printexc.to_string __x__)
                                  "Looking for key dbg"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key dbg",
                                     (Printexc.to_string __x__))))
                       and __x298__ =
                         try List.assoc "sr" __x296__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x296__"
                                  (Printexc.to_string __x__)
                                  "Looking for key sr"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key sr",
                                     (Printexc.to_string __x__)))) in
                       {
                         dbg = (debug_info_of_rpc __x297__);
                         sr = (sr_of_rpc __x298__)
                       }
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "request" "__x295__" (Rpc.to_string __x__) "Dict"
                        else ();
                        raise (Rpc.Runtime_error ("Dict", __x__))))
            type response = sr_info
            let rpc_of_response =
              function | __x294__ -> rpc_of_sr_info __x294__
            let response_of_rpc =
              function | __x293__ -> sr_info_of_rpc __x293__
            let call_of_stat ~dbg  ~sr  =
              let arg = { sr; dbg } in
              Rpc.call "SR.stat" [rpc_of_request arg]
          end
        module List =
          struct
            type request = {
              dbg: debug_info }
            let rpc_of_request =
              function
              | __x310__ ->
                  let __x311__ = __x310__.dbg in
                  Rpc.Dict [("dbg", (rpc_of_debug_info __x311__))]
            let request_of_rpc =
              function
              | __x307__ ->
                  (match __x307__ with
                   | Rpc.Dict __x308__ ->
                       let __x309__ =
                         try List.assoc "dbg" __x308__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x308__"
                                  (Printexc.to_string __x__)
                                  "Looking for key dbg"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key dbg",
                                     (Printexc.to_string __x__)))) in
                       { dbg = (debug_info_of_rpc __x309__) }
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "request" "__x307__" (Rpc.to_string __x__) "Dict"
                        else ();
                        raise (Rpc.Runtime_error ("Dict", __x__))))
            type response = sr list
            let rpc_of_response =
              function
              | __x305__ ->
                  Rpc.Enum
                    (List.map (function | __x306__ -> rpc_of_sr __x306__)
                       __x305__)
            let response_of_rpc =
              function
              | __x302__ ->
                  (match __x302__ with
                   | Rpc.Enum __x303__ ->
                       List.map (function | __x304__ -> sr_of_rpc __x304__)
                         __x303__
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "response" "__x302__" (Rpc.to_string __x__)
                            "List"
                        else ();
                        raise (Rpc.Runtime_error ("List", __x__))))
            let call_of_list ~dbg  =
              let arg = { dbg } in Rpc.call "SR.list" [rpc_of_request arg]
          end
      end
    module VDI =
      struct
        module Create =
          struct
            type request = {
              dbg: debug_info ;
              sr: sr ;
              vdi_info: vdi_info }
            let rpc_of_request =
              function
              | __x319__ ->
                  let __x320__ = __x319__.dbg
                  and __x321__ = __x319__.sr
                  and __x322__ = __x319__.vdi_info in
                  Rpc.Dict
                    [("dbg", (rpc_of_debug_info __x320__));
                    ("sr", (rpc_of_sr __x321__));
                    ("vdi_info", (rpc_of_vdi_info __x322__))]
            let request_of_rpc =
              function
              | __x314__ ->
                  (match __x314__ with
                   | Rpc.Dict __x315__ ->
                       let __x316__ =
                         try List.assoc "dbg" __x315__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x315__"
                                  (Printexc.to_string __x__)
                                  "Looking for key dbg"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key dbg",
                                     (Printexc.to_string __x__))))
                       and __x317__ =
                         try List.assoc "sr" __x315__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x315__"
                                  (Printexc.to_string __x__)
                                  "Looking for key sr"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key sr",
                                     (Printexc.to_string __x__))))
                       and __x318__ =
                         try List.assoc "vdi_info" __x315__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x315__"
                                  (Printexc.to_string __x__)
                                  "Looking for key vdi_info"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key vdi_info",
                                     (Printexc.to_string __x__)))) in
                       {
                         dbg = (debug_info_of_rpc __x316__);
                         sr = (sr_of_rpc __x317__);
                         vdi_info = (vdi_info_of_rpc __x318__)
                       }
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "request" "__x314__" (Rpc.to_string __x__) "Dict"
                        else ();
                        raise (Rpc.Runtime_error ("Dict", __x__))))
            type response = vdi_info
            let rpc_of_response =
              function | __x313__ -> rpc_of_vdi_info __x313__
            let response_of_rpc =
              function | __x312__ -> vdi_info_of_rpc __x312__
            let call_of_create ~dbg  ~sr  ~vdi_info  =
              let arg = { vdi_info; sr; dbg } in
              Rpc.call "VDI.create" [rpc_of_request arg]
          end
        module Set_name_label =
          struct
            type request =
              {
              dbg: debug_info ;
              sr: sr ;
              vdi: vdi ;
              new_name_label: string }
            let rpc_of_request =
              function
              | __x331__ ->
                  let __x332__ = __x331__.dbg
                  and __x333__ = __x331__.sr
                  and __x334__ = __x331__.vdi
                  and __x335__ = __x331__.new_name_label in
                  Rpc.Dict
                    [("dbg", (rpc_of_debug_info __x332__));
                    ("sr", (rpc_of_sr __x333__));
                    ("vdi", (rpc_of_vdi __x334__));
                    ("new_name_label", (Rpc.String __x335__))]
            let request_of_rpc =
              function
              | __x325__ ->
                  (match __x325__ with
                   | Rpc.Dict __x326__ ->
                       let __x327__ =
                         try List.assoc "dbg" __x326__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x326__"
                                  (Printexc.to_string __x__)
                                  "Looking for key dbg"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key dbg",
                                     (Printexc.to_string __x__))))
                       and __x328__ =
                         try List.assoc "sr" __x326__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x326__"
                                  (Printexc.to_string __x__)
                                  "Looking for key sr"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key sr",
                                     (Printexc.to_string __x__))))
                       and __x329__ =
                         try List.assoc "vdi" __x326__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x326__"
                                  (Printexc.to_string __x__)
                                  "Looking for key vdi"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key vdi",
                                     (Printexc.to_string __x__))))
                       and __x330__ =
                         try List.assoc "new_name_label" __x326__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x326__"
                                  (Printexc.to_string __x__)
                                  "Looking for key new_name_label"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key new_name_label",
                                     (Printexc.to_string __x__)))) in
                       {
                         dbg = (debug_info_of_rpc __x327__);
                         sr = (sr_of_rpc __x328__);
                         vdi = (vdi_of_rpc __x329__);
                         new_name_label =
                           ((match __x330__ with
                             | Rpc.String x -> x
                             | __x__ ->
                                 (if Rpc.get_debug ()
                                  then
                                    Printf.eprintf
                                      "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                      "request" "__x330__"
                                      (Rpc.to_string __x__) "String(string)"
                                  else ();
                                  raise
                                    (Rpc.Runtime_error
                                       ("String(string)", __x__)))))
                       }
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "request" "__x325__" (Rpc.to_string __x__) "Dict"
                        else ();
                        raise (Rpc.Runtime_error ("Dict", __x__))))
            type response = unit
            let rpc_of_response = function | __x324__ -> Rpc.Null
            let response_of_rpc =
              function
              | __x323__ ->
                  (match __x323__ with
                   | Rpc.Null -> ()
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "response" "__x323__" (Rpc.to_string __x__)
                            "Null"
                        else ();
                        raise (Rpc.Runtime_error ("Null", __x__))))
            let call_of_set_name_label ~dbg  ~sr  ~vdi  ~new_name_label  =
              let arg = { new_name_label; vdi; sr; dbg } in
              Rpc.call "VDI.set_name_label" [rpc_of_request arg]
          end
        module Set_name_description =
          struct
            type request =
              {
              dbg: debug_info ;
              sr: sr ;
              vdi: vdi ;
              new_name_description: string }
            let rpc_of_request =
              function
              | __x344__ ->
                  let __x345__ = __x344__.dbg
                  and __x346__ = __x344__.sr
                  and __x347__ = __x344__.vdi
                  and __x348__ = __x344__.new_name_description in
                  Rpc.Dict
                    [("dbg", (rpc_of_debug_info __x345__));
                    ("sr", (rpc_of_sr __x346__));
                    ("vdi", (rpc_of_vdi __x347__));
                    ("new_name_description", (Rpc.String __x348__))]
            let request_of_rpc =
              function
              | __x338__ ->
                  (match __x338__ with
                   | Rpc.Dict __x339__ ->
                       let __x340__ =
                         try List.assoc "dbg" __x339__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x339__"
                                  (Printexc.to_string __x__)
                                  "Looking for key dbg"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key dbg",
                                     (Printexc.to_string __x__))))
                       and __x341__ =
                         try List.assoc "sr" __x339__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x339__"
                                  (Printexc.to_string __x__)
                                  "Looking for key sr"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key sr",
                                     (Printexc.to_string __x__))))
                       and __x342__ =
                         try List.assoc "vdi" __x339__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x339__"
                                  (Printexc.to_string __x__)
                                  "Looking for key vdi"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key vdi",
                                     (Printexc.to_string __x__))))
                       and __x343__ =
                         try List.assoc "new_name_description" __x339__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x339__"
                                  (Printexc.to_string __x__)
                                  "Looking for key new_name_description"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key new_name_description",
                                     (Printexc.to_string __x__)))) in
                       {
                         dbg = (debug_info_of_rpc __x340__);
                         sr = (sr_of_rpc __x341__);
                         vdi = (vdi_of_rpc __x342__);
                         new_name_description =
                           ((match __x343__ with
                             | Rpc.String x -> x
                             | __x__ ->
                                 (if Rpc.get_debug ()
                                  then
                                    Printf.eprintf
                                      "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                      "request" "__x343__"
                                      (Rpc.to_string __x__) "String(string)"
                                  else ();
                                  raise
                                    (Rpc.Runtime_error
                                       ("String(string)", __x__)))))
                       }
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "request" "__x338__" (Rpc.to_string __x__) "Dict"
                        else ();
                        raise (Rpc.Runtime_error ("Dict", __x__))))
            type response = unit
            let rpc_of_response = function | __x337__ -> Rpc.Null
            let response_of_rpc =
              function
              | __x336__ ->
                  (match __x336__ with
                   | Rpc.Null -> ()
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "response" "__x336__" (Rpc.to_string __x__)
                            "Null"
                        else ();
                        raise (Rpc.Runtime_error ("Null", __x__))))
            let call_of_set_name_description ~dbg  ~sr  ~vdi 
              ~new_name_description  =
              let arg = { new_name_description; vdi; sr; dbg } in
              Rpc.call "VDI.set_name_description" [rpc_of_request arg]
          end
        module Snapshot =
          struct
            type request = {
              dbg: debug_info ;
              sr: sr ;
              vdi_info: vdi_info }
            let rpc_of_request =
              function
              | __x356__ ->
                  let __x357__ = __x356__.dbg
                  and __x358__ = __x356__.sr
                  and __x359__ = __x356__.vdi_info in
                  Rpc.Dict
                    [("dbg", (rpc_of_debug_info __x357__));
                    ("sr", (rpc_of_sr __x358__));
                    ("vdi_info", (rpc_of_vdi_info __x359__))]
            let request_of_rpc =
              function
              | __x351__ ->
                  (match __x351__ with
                   | Rpc.Dict __x352__ ->
                       let __x353__ =
                         try List.assoc "dbg" __x352__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x352__"
                                  (Printexc.to_string __x__)
                                  "Looking for key dbg"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key dbg",
                                     (Printexc.to_string __x__))))
                       and __x354__ =
                         try List.assoc "sr" __x352__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x352__"
                                  (Printexc.to_string __x__)
                                  "Looking for key sr"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key sr",
                                     (Printexc.to_string __x__))))
                       and __x355__ =
                         try List.assoc "vdi_info" __x352__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x352__"
                                  (Printexc.to_string __x__)
                                  "Looking for key vdi_info"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key vdi_info",
                                     (Printexc.to_string __x__)))) in
                       {
                         dbg = (debug_info_of_rpc __x353__);
                         sr = (sr_of_rpc __x354__);
                         vdi_info = (vdi_info_of_rpc __x355__)
                       }
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "request" "__x351__" (Rpc.to_string __x__) "Dict"
                        else ();
                        raise (Rpc.Runtime_error ("Dict", __x__))))
            type response = vdi_info
            let rpc_of_response =
              function | __x350__ -> rpc_of_vdi_info __x350__
            let response_of_rpc =
              function | __x349__ -> vdi_info_of_rpc __x349__
            let call_of_snapshot ~dbg  ~sr  ~vdi_info  =
              let arg = { vdi_info; sr; dbg } in
              Rpc.call "VDI.snapshot" [rpc_of_request arg]
          end
        module Clone =
          struct
            type request = {
              dbg: debug_info ;
              sr: sr ;
              vdi_info: vdi_info }
            let rpc_of_request =
              function
              | __x367__ ->
                  let __x368__ = __x367__.dbg
                  and __x369__ = __x367__.sr
                  and __x370__ = __x367__.vdi_info in
                  Rpc.Dict
                    [("dbg", (rpc_of_debug_info __x368__));
                    ("sr", (rpc_of_sr __x369__));
                    ("vdi_info", (rpc_of_vdi_info __x370__))]
            let request_of_rpc =
              function
              | __x362__ ->
                  (match __x362__ with
                   | Rpc.Dict __x363__ ->
                       let __x364__ =
                         try List.assoc "dbg" __x363__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x363__"
                                  (Printexc.to_string __x__)
                                  "Looking for key dbg"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key dbg",
                                     (Printexc.to_string __x__))))
                       and __x365__ =
                         try List.assoc "sr" __x363__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x363__"
                                  (Printexc.to_string __x__)
                                  "Looking for key sr"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key sr",
                                     (Printexc.to_string __x__))))
                       and __x366__ =
                         try List.assoc "vdi_info" __x363__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x363__"
                                  (Printexc.to_string __x__)
                                  "Looking for key vdi_info"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key vdi_info",
                                     (Printexc.to_string __x__)))) in
                       {
                         dbg = (debug_info_of_rpc __x364__);
                         sr = (sr_of_rpc __x365__);
                         vdi_info = (vdi_info_of_rpc __x366__)
                       }
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "request" "__x362__" (Rpc.to_string __x__) "Dict"
                        else ();
                        raise (Rpc.Runtime_error ("Dict", __x__))))
            type response = vdi_info
            let rpc_of_response =
              function | __x361__ -> rpc_of_vdi_info __x361__
            let response_of_rpc =
              function | __x360__ -> vdi_info_of_rpc __x360__
            let call_of_clone ~dbg  ~sr  ~vdi_info  =
              let arg = { vdi_info; sr; dbg } in
              Rpc.call "VDI.clone" [rpc_of_request arg]
          end
        module Resize =
          struct
            type request =
              {
              dbg: debug_info ;
              sr: sr ;
              vdi: vdi ;
              new_size: int64 }
            let rpc_of_request =
              function
              | __x379__ ->
                  let __x380__ = __x379__.dbg
                  and __x381__ = __x379__.sr
                  and __x382__ = __x379__.vdi
                  and __x383__ = __x379__.new_size in
                  Rpc.Dict
                    [("dbg", (rpc_of_debug_info __x380__));
                    ("sr", (rpc_of_sr __x381__));
                    ("vdi", (rpc_of_vdi __x382__));
                    ("new_size", (Rpc.Int __x383__))]
            let request_of_rpc =
              function
              | __x373__ ->
                  (match __x373__ with
                   | Rpc.Dict __x374__ ->
                       let __x375__ =
                         try List.assoc "dbg" __x374__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x374__"
                                  (Printexc.to_string __x__)
                                  "Looking for key dbg"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key dbg",
                                     (Printexc.to_string __x__))))
                       and __x376__ =
                         try List.assoc "sr" __x374__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x374__"
                                  (Printexc.to_string __x__)
                                  "Looking for key sr"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key sr",
                                     (Printexc.to_string __x__))))
                       and __x377__ =
                         try List.assoc "vdi" __x374__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x374__"
                                  (Printexc.to_string __x__)
                                  "Looking for key vdi"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key vdi",
                                     (Printexc.to_string __x__))))
                       and __x378__ =
                         try List.assoc "new_size" __x374__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x374__"
                                  (Printexc.to_string __x__)
                                  "Looking for key new_size"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key new_size",
                                     (Printexc.to_string __x__)))) in
                       {
                         dbg = (debug_info_of_rpc __x375__);
                         sr = (sr_of_rpc __x376__);
                         vdi = (vdi_of_rpc __x377__);
                         new_size =
                           ((match __x378__ with
                             | Rpc.Int x -> x
                             | Rpc.String s -> Int64.of_string s
                             | __x__ ->
                                 (if Rpc.get_debug ()
                                  then
                                    Printf.eprintf
                                      "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                      "request" "__x378__"
                                      (Rpc.to_string __x__) "Int(int64)"
                                  else ();
                                  raise
                                    (Rpc.Runtime_error ("Int(int64)", __x__)))))
                       }
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "request" "__x373__" (Rpc.to_string __x__) "Dict"
                        else ();
                        raise (Rpc.Runtime_error ("Dict", __x__))))
            type response = int64
            let rpc_of_response = function | __x372__ -> Rpc.Int __x372__
            let response_of_rpc =
              function
              | __x371__ ->
                  (match __x371__ with
                   | Rpc.Int x -> x
                   | Rpc.String s -> Int64.of_string s
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "response" "__x371__" (Rpc.to_string __x__)
                            "Int(int64)"
                        else ();
                        raise (Rpc.Runtime_error ("Int(int64)", __x__))))
            let call_of_resize ~dbg  ~sr  ~vdi  ~new_size  =
              let arg = { new_size; vdi; sr; dbg } in
              Rpc.call "VDI.resize" [rpc_of_request arg]
          end
        module Destroy =
          struct
            type request = {
              dbg: debug_info ;
              sr: sr ;
              vdi: vdi }
            let rpc_of_request =
              function
              | __x391__ ->
                  let __x392__ = __x391__.dbg
                  and __x393__ = __x391__.sr
                  and __x394__ = __x391__.vdi in
                  Rpc.Dict
                    [("dbg", (rpc_of_debug_info __x392__));
                    ("sr", (rpc_of_sr __x393__));
                    ("vdi", (rpc_of_vdi __x394__))]
            let request_of_rpc =
              function
              | __x386__ ->
                  (match __x386__ with
                   | Rpc.Dict __x387__ ->
                       let __x388__ =
                         try List.assoc "dbg" __x387__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x387__"
                                  (Printexc.to_string __x__)
                                  "Looking for key dbg"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key dbg",
                                     (Printexc.to_string __x__))))
                       and __x389__ =
                         try List.assoc "sr" __x387__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x387__"
                                  (Printexc.to_string __x__)
                                  "Looking for key sr"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key sr",
                                     (Printexc.to_string __x__))))
                       and __x390__ =
                         try List.assoc "vdi" __x387__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x387__"
                                  (Printexc.to_string __x__)
                                  "Looking for key vdi"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key vdi",
                                     (Printexc.to_string __x__)))) in
                       {
                         dbg = (debug_info_of_rpc __x388__);
                         sr = (sr_of_rpc __x389__);
                         vdi = (vdi_of_rpc __x390__)
                       }
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "request" "__x386__" (Rpc.to_string __x__) "Dict"
                        else ();
                        raise (Rpc.Runtime_error ("Dict", __x__))))
            type response = unit
            let rpc_of_response = function | __x385__ -> Rpc.Null
            let response_of_rpc =
              function
              | __x384__ ->
                  (match __x384__ with
                   | Rpc.Null -> ()
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "response" "__x384__" (Rpc.to_string __x__)
                            "Null"
                        else ();
                        raise (Rpc.Runtime_error ("Null", __x__))))
            let call_of_destroy ~dbg  ~sr  ~vdi  =
              let arg = { vdi; sr; dbg } in
              Rpc.call "VDI.destroy" [rpc_of_request arg]
          end
        module Stat =
          struct
            type request = {
              dbg: debug_info ;
              sr: sr ;
              vdi: vdi }
            let rpc_of_request =
              function
              | __x402__ ->
                  let __x403__ = __x402__.dbg
                  and __x404__ = __x402__.sr
                  and __x405__ = __x402__.vdi in
                  Rpc.Dict
                    [("dbg", (rpc_of_debug_info __x403__));
                    ("sr", (rpc_of_sr __x404__));
                    ("vdi", (rpc_of_vdi __x405__))]
            let request_of_rpc =
              function
              | __x397__ ->
                  (match __x397__ with
                   | Rpc.Dict __x398__ ->
                       let __x399__ =
                         try List.assoc "dbg" __x398__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x398__"
                                  (Printexc.to_string __x__)
                                  "Looking for key dbg"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key dbg",
                                     (Printexc.to_string __x__))))
                       and __x400__ =
                         try List.assoc "sr" __x398__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x398__"
                                  (Printexc.to_string __x__)
                                  "Looking for key sr"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key sr",
                                     (Printexc.to_string __x__))))
                       and __x401__ =
                         try List.assoc "vdi" __x398__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x398__"
                                  (Printexc.to_string __x__)
                                  "Looking for key vdi"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key vdi",
                                     (Printexc.to_string __x__)))) in
                       {
                         dbg = (debug_info_of_rpc __x399__);
                         sr = (sr_of_rpc __x400__);
                         vdi = (vdi_of_rpc __x401__)
                       }
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "request" "__x397__" (Rpc.to_string __x__) "Dict"
                        else ();
                        raise (Rpc.Runtime_error ("Dict", __x__))))
            type response = vdi_info
            let rpc_of_response =
              function | __x396__ -> rpc_of_vdi_info __x396__
            let response_of_rpc =
              function | __x395__ -> vdi_info_of_rpc __x395__
            let call_of_stat ~dbg  ~sr  ~vdi  =
              let arg = { vdi; sr; dbg } in
              Rpc.call "VDI.stat" [rpc_of_request arg]
          end
        module Introduce =
          struct
            type request =
              {
              dbg: debug_info ;
              sr: sr ;
              uuid: string ;
              sm_config: (string * string) list ;
              location: string }
            let rpc_of_request =
              function
              | __x416__ ->
                  let __x417__ = __x416__.dbg
                  and __x418__ = __x416__.sr
                  and __x419__ = __x416__.uuid
                  and __x420__ = __x416__.sm_config
                  and __x421__ = __x416__.location in
                  Rpc.Dict
                    [("dbg", (rpc_of_debug_info __x417__));
                    ("sr", (rpc_of_sr __x418__));
                    ("uuid", (Rpc.String __x419__));
                    ("sm_config",
                      ((let dict =
                          List.map
                            (function
                             | (key, __x422__) ->
                                 (key, (Rpc.String __x422__))) __x420__ in
                        Rpc.Dict dict)));
                    ("location", (Rpc.String __x421__))]
            let request_of_rpc =
              function
              | __x408__ ->
                  (match __x408__ with
                   | Rpc.Dict __x409__ ->
                       let __x410__ =
                         try List.assoc "dbg" __x409__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x409__"
                                  (Printexc.to_string __x__)
                                  "Looking for key dbg"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key dbg",
                                     (Printexc.to_string __x__))))
                       and __x411__ =
                         try List.assoc "sr" __x409__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x409__"
                                  (Printexc.to_string __x__)
                                  "Looking for key sr"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key sr",
                                     (Printexc.to_string __x__))))
                       and __x412__ =
                         try List.assoc "uuid" __x409__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x409__"
                                  (Printexc.to_string __x__)
                                  "Looking for key uuid"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key uuid",
                                     (Printexc.to_string __x__))))
                       and __x413__ =
                         try List.assoc "sm_config" __x409__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x409__"
                                  (Printexc.to_string __x__)
                                  "Looking for key sm_config"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key sm_config",
                                     (Printexc.to_string __x__))))
                       and __x414__ =
                         try List.assoc "location" __x409__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x409__"
                                  (Printexc.to_string __x__)
                                  "Looking for key location"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key location",
                                     (Printexc.to_string __x__)))) in
                       {
                         dbg = (debug_info_of_rpc __x410__);
                         sr = (sr_of_rpc __x411__);
                         uuid =
                           ((match __x412__ with
                             | Rpc.String x -> x
                             | __x__ ->
                                 (if Rpc.get_debug ()
                                  then
                                    Printf.eprintf
                                      "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                      "request" "__x412__"
                                      (Rpc.to_string __x__) "String(string)"
                                  else ();
                                  raise
                                    (Rpc.Runtime_error
                                       ("String(string)", __x__)))));
                         sm_config =
                           ((match __x413__ with
                             | Rpc.Dict d ->
                                 List.map
                                   (function
                                    | (key, __x415__) ->
                                        (key,
                                          ((match __x415__ with
                                            | Rpc.String x -> x
                                            | __x__ ->
                                                (if Rpc.get_debug ()
                                                 then
                                                   Printf.eprintf
                                                     "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                                     "request" "__x415__"
                                                     (Rpc.to_string __x__)
                                                     "String(string)"
                                                 else ();
                                                 raise
                                                   (Rpc.Runtime_error
                                                      ("String(string)",
                                                        __x__))))))) d
                             | __x__ ->
                                 (if Rpc.get_debug ()
                                  then
                                    Printf.eprintf
                                      "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                      "request" "__x413__"
                                      (Rpc.to_string __x__) "Dict"
                                  else ();
                                  raise (Rpc.Runtime_error ("Dict", __x__)))));
                         location =
                           ((match __x414__ with
                             | Rpc.String x -> x
                             | __x__ ->
                                 (if Rpc.get_debug ()
                                  then
                                    Printf.eprintf
                                      "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                      "request" "__x414__"
                                      (Rpc.to_string __x__) "String(string)"
                                  else ();
                                  raise
                                    (Rpc.Runtime_error
                                       ("String(string)", __x__)))))
                       }
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "request" "__x408__" (Rpc.to_string __x__) "Dict"
                        else ();
                        raise (Rpc.Runtime_error ("Dict", __x__))))
            type response = vdi_info
            let rpc_of_response =
              function | __x407__ -> rpc_of_vdi_info __x407__
            let response_of_rpc =
              function | __x406__ -> vdi_info_of_rpc __x406__
            let call_of_introduce ~dbg  ~sr  ~uuid  ~sm_config  ~location  =
              let arg = { location; sm_config; uuid; sr; dbg } in
              Rpc.call "VDI.introduce" [rpc_of_request arg]
          end
        module Set_persistent =
          struct
            type request =
              {
              dbg: debug_info ;
              sr: sr ;
              vdi: vdi ;
              persistent: bool }
            let rpc_of_request =
              function
              | __x431__ ->
                  let __x432__ = __x431__.dbg
                  and __x433__ = __x431__.sr
                  and __x434__ = __x431__.vdi
                  and __x435__ = __x431__.persistent in
                  Rpc.Dict
                    [("dbg", (rpc_of_debug_info __x432__));
                    ("sr", (rpc_of_sr __x433__));
                    ("vdi", (rpc_of_vdi __x434__));
                    ("persistent", (Rpc.Bool __x435__))]
            let request_of_rpc =
              function
              | __x425__ ->
                  (match __x425__ with
                   | Rpc.Dict __x426__ ->
                       let __x427__ =
                         try List.assoc "dbg" __x426__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x426__"
                                  (Printexc.to_string __x__)
                                  "Looking for key dbg"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key dbg",
                                     (Printexc.to_string __x__))))
                       and __x428__ =
                         try List.assoc "sr" __x426__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x426__"
                                  (Printexc.to_string __x__)
                                  "Looking for key sr"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key sr",
                                     (Printexc.to_string __x__))))
                       and __x429__ =
                         try List.assoc "vdi" __x426__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x426__"
                                  (Printexc.to_string __x__)
                                  "Looking for key vdi"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key vdi",
                                     (Printexc.to_string __x__))))
                       and __x430__ =
                         try List.assoc "persistent" __x426__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x426__"
                                  (Printexc.to_string __x__)
                                  "Looking for key persistent"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key persistent",
                                     (Printexc.to_string __x__)))) in
                       {
                         dbg = (debug_info_of_rpc __x427__);
                         sr = (sr_of_rpc __x428__);
                         vdi = (vdi_of_rpc __x429__);
                         persistent =
                           ((match __x430__ with
                             | Rpc.Bool x -> x
                             | __x__ ->
                                 (if Rpc.get_debug ()
                                  then
                                    Printf.eprintf
                                      "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                      "request" "__x430__"
                                      (Rpc.to_string __x__) "Bool"
                                  else ();
                                  raise (Rpc.Runtime_error ("Bool", __x__)))))
                       }
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "request" "__x425__" (Rpc.to_string __x__) "Dict"
                        else ();
                        raise (Rpc.Runtime_error ("Dict", __x__))))
            type response = unit
            let rpc_of_response = function | __x424__ -> Rpc.Null
            let response_of_rpc =
              function
              | __x423__ ->
                  (match __x423__ with
                   | Rpc.Null -> ()
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "response" "__x423__" (Rpc.to_string __x__)
                            "Null"
                        else ();
                        raise (Rpc.Runtime_error ("Null", __x__))))
            let call_of_set_persistent ~dbg  ~sr  ~vdi  ~persistent  =
              let arg = { persistent; vdi; sr; dbg } in
              Rpc.call "VDI.set_persistent" [rpc_of_request arg]
          end
        module Epoch_begin =
          struct
            type request =
              {
              dbg: debug_info ;
              sr: sr ;
              vdi: vdi ;
              persistent: bool }
            let rpc_of_request =
              function
              | __x444__ ->
                  let __x445__ = __x444__.dbg
                  and __x446__ = __x444__.sr
                  and __x447__ = __x444__.vdi
                  and __x448__ = __x444__.persistent in
                  Rpc.Dict
                    [("dbg", (rpc_of_debug_info __x445__));
                    ("sr", (rpc_of_sr __x446__));
                    ("vdi", (rpc_of_vdi __x447__));
                    ("persistent", (Rpc.Bool __x448__))]
            let request_of_rpc =
              function
              | __x438__ ->
                  (match __x438__ with
                   | Rpc.Dict __x439__ ->
                       let __x440__ =
                         try List.assoc "dbg" __x439__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x439__"
                                  (Printexc.to_string __x__)
                                  "Looking for key dbg"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key dbg",
                                     (Printexc.to_string __x__))))
                       and __x441__ =
                         try List.assoc "sr" __x439__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x439__"
                                  (Printexc.to_string __x__)
                                  "Looking for key sr"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key sr",
                                     (Printexc.to_string __x__))))
                       and __x442__ =
                         try List.assoc "vdi" __x439__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x439__"
                                  (Printexc.to_string __x__)
                                  "Looking for key vdi"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key vdi",
                                     (Printexc.to_string __x__))))
                       and __x443__ =
                         try List.assoc "persistent" __x439__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x439__"
                                  (Printexc.to_string __x__)
                                  "Looking for key persistent"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key persistent",
                                     (Printexc.to_string __x__)))) in
                       {
                         dbg = (debug_info_of_rpc __x440__);
                         sr = (sr_of_rpc __x441__);
                         vdi = (vdi_of_rpc __x442__);
                         persistent =
                           ((match __x443__ with
                             | Rpc.Bool x -> x
                             | __x__ ->
                                 (if Rpc.get_debug ()
                                  then
                                    Printf.eprintf
                                      "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                      "request" "__x443__"
                                      (Rpc.to_string __x__) "Bool"
                                  else ();
                                  raise (Rpc.Runtime_error ("Bool", __x__)))))
                       }
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "request" "__x438__" (Rpc.to_string __x__) "Dict"
                        else ();
                        raise (Rpc.Runtime_error ("Dict", __x__))))
            type response = unit
            let rpc_of_response = function | __x437__ -> Rpc.Null
            let response_of_rpc =
              function
              | __x436__ ->
                  (match __x436__ with
                   | Rpc.Null -> ()
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "response" "__x436__" (Rpc.to_string __x__)
                            "Null"
                        else ();
                        raise (Rpc.Runtime_error ("Null", __x__))))
            let call_of_epoch_begin ~dbg  ~sr  ~vdi  ~persistent  =
              let arg = { persistent; vdi; sr; dbg } in
              Rpc.call "VDI.epoch_begin" [rpc_of_request arg]
          end
        module Attach =
          struct
            type request =
              {
              dbg: debug_info ;
              dp: dp ;
              sr: sr ;
              vdi: vdi ;
              read_write: bool }
            let rpc_of_request =
              function
              | __x458__ ->
                  let __x459__ = __x458__.dbg
                  and __x460__ = __x458__.dp
                  and __x461__ = __x458__.sr
                  and __x462__ = __x458__.vdi
                  and __x463__ = __x458__.read_write in
                  Rpc.Dict
                    [("dbg", (rpc_of_debug_info __x459__));
                    ("dp", (rpc_of_dp __x460__));
                    ("sr", (rpc_of_sr __x461__));
                    ("vdi", (rpc_of_vdi __x462__));
                    ("read_write", (Rpc.Bool __x463__))]
            let request_of_rpc =
              function
              | __x451__ ->
                  (match __x451__ with
                   | Rpc.Dict __x452__ ->
                       let __x453__ =
                         try List.assoc "dbg" __x452__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x452__"
                                  (Printexc.to_string __x__)
                                  "Looking for key dbg"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key dbg",
                                     (Printexc.to_string __x__))))
                       and __x454__ =
                         try List.assoc "dp" __x452__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x452__"
                                  (Printexc.to_string __x__)
                                  "Looking for key dp"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key dp",
                                     (Printexc.to_string __x__))))
                       and __x455__ =
                         try List.assoc "sr" __x452__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x452__"
                                  (Printexc.to_string __x__)
                                  "Looking for key sr"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key sr",
                                     (Printexc.to_string __x__))))
                       and __x456__ =
                         try List.assoc "vdi" __x452__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x452__"
                                  (Printexc.to_string __x__)
                                  "Looking for key vdi"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key vdi",
                                     (Printexc.to_string __x__))))
                       and __x457__ =
                         try List.assoc "read_write" __x452__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x452__"
                                  (Printexc.to_string __x__)
                                  "Looking for key read_write"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key read_write",
                                     (Printexc.to_string __x__)))) in
                       {
                         dbg = (debug_info_of_rpc __x453__);
                         dp = (dp_of_rpc __x454__);
                         sr = (sr_of_rpc __x455__);
                         vdi = (vdi_of_rpc __x456__);
                         read_write =
                           ((match __x457__ with
                             | Rpc.Bool x -> x
                             | __x__ ->
                                 (if Rpc.get_debug ()
                                  then
                                    Printf.eprintf
                                      "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                      "request" "__x457__"
                                      (Rpc.to_string __x__) "Bool"
                                  else ();
                                  raise (Rpc.Runtime_error ("Bool", __x__)))))
                       }
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "request" "__x451__" (Rpc.to_string __x__) "Dict"
                        else ();
                        raise (Rpc.Runtime_error ("Dict", __x__))))
            type response = attach_info
            let rpc_of_response =
              function | __x450__ -> rpc_of_attach_info __x450__
            let response_of_rpc =
              function | __x449__ -> attach_info_of_rpc __x449__
            let call_of_attach ~dbg  ~dp  ~sr  ~vdi  ~read_write  =
              let arg = { read_write; vdi; sr; dp; dbg } in
              Rpc.call "VDI.attach" [rpc_of_request arg]
          end
        module Attach2 =
          struct
            type request =
              {
              dbg: debug_info ;
              dp: dp ;
              sr: sr ;
              vdi: vdi ;
              read_write: bool }
            let rpc_of_request =
              function
              | __x473__ ->
                  let __x474__ = __x473__.dbg
                  and __x475__ = __x473__.dp
                  and __x476__ = __x473__.sr
                  and __x477__ = __x473__.vdi
                  and __x478__ = __x473__.read_write in
                  Rpc.Dict
                    [("dbg", (rpc_of_debug_info __x474__));
                    ("dp", (rpc_of_dp __x475__));
                    ("sr", (rpc_of_sr __x476__));
                    ("vdi", (rpc_of_vdi __x477__));
                    ("read_write", (Rpc.Bool __x478__))]
            let request_of_rpc =
              function
              | __x466__ ->
                  (match __x466__ with
                   | Rpc.Dict __x467__ ->
                       let __x468__ =
                         try List.assoc "dbg" __x467__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x467__"
                                  (Printexc.to_string __x__)
                                  "Looking for key dbg"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key dbg",
                                     (Printexc.to_string __x__))))
                       and __x469__ =
                         try List.assoc "dp" __x467__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x467__"
                                  (Printexc.to_string __x__)
                                  "Looking for key dp"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key dp",
                                     (Printexc.to_string __x__))))
                       and __x470__ =
                         try List.assoc "sr" __x467__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x467__"
                                  (Printexc.to_string __x__)
                                  "Looking for key sr"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key sr",
                                     (Printexc.to_string __x__))))
                       and __x471__ =
                         try List.assoc "vdi" __x467__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x467__"
                                  (Printexc.to_string __x__)
                                  "Looking for key vdi"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key vdi",
                                     (Printexc.to_string __x__))))
                       and __x472__ =
                         try List.assoc "read_write" __x467__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x467__"
                                  (Printexc.to_string __x__)
                                  "Looking for key read_write"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key read_write",
                                     (Printexc.to_string __x__)))) in
                       {
                         dbg = (debug_info_of_rpc __x468__);
                         dp = (dp_of_rpc __x469__);
                         sr = (sr_of_rpc __x470__);
                         vdi = (vdi_of_rpc __x471__);
                         read_write =
                           ((match __x472__ with
                             | Rpc.Bool x -> x
                             | __x__ ->
                                 (if Rpc.get_debug ()
                                  then
                                    Printf.eprintf
                                      "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                      "request" "__x472__"
                                      (Rpc.to_string __x__) "Bool"
                                  else ();
                                  raise (Rpc.Runtime_error ("Bool", __x__)))))
                       }
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "request" "__x466__" (Rpc.to_string __x__) "Dict"
                        else ();
                        raise (Rpc.Runtime_error ("Dict", __x__))))
            type response = backend
            let rpc_of_response =
              function | __x465__ -> rpc_of_backend __x465__
            let response_of_rpc =
              function | __x464__ -> backend_of_rpc __x464__
            let call_of_attach2 ~dbg  ~dp  ~sr  ~vdi  ~read_write  =
              let arg = { read_write; vdi; sr; dp; dbg } in
              Rpc.call "VDI.attach2" [rpc_of_request arg]
          end
        module Activate =
          struct
            type request = {
              dbg: debug_info ;
              dp: dp ;
              sr: sr ;
              vdi: vdi }
            let rpc_of_request =
              function
              | __x487__ ->
                  let __x488__ = __x487__.dbg
                  and __x489__ = __x487__.dp
                  and __x490__ = __x487__.sr
                  and __x491__ = __x487__.vdi in
                  Rpc.Dict
                    [("dbg", (rpc_of_debug_info __x488__));
                    ("dp", (rpc_of_dp __x489__));
                    ("sr", (rpc_of_sr __x490__));
                    ("vdi", (rpc_of_vdi __x491__))]
            let request_of_rpc =
              function
              | __x481__ ->
                  (match __x481__ with
                   | Rpc.Dict __x482__ ->
                       let __x483__ =
                         try List.assoc "dbg" __x482__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x482__"
                                  (Printexc.to_string __x__)
                                  "Looking for key dbg"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key dbg",
                                     (Printexc.to_string __x__))))
                       and __x484__ =
                         try List.assoc "dp" __x482__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x482__"
                                  (Printexc.to_string __x__)
                                  "Looking for key dp"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key dp",
                                     (Printexc.to_string __x__))))
                       and __x485__ =
                         try List.assoc "sr" __x482__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x482__"
                                  (Printexc.to_string __x__)
                                  "Looking for key sr"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key sr",
                                     (Printexc.to_string __x__))))
                       and __x486__ =
                         try List.assoc "vdi" __x482__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x482__"
                                  (Printexc.to_string __x__)
                                  "Looking for key vdi"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key vdi",
                                     (Printexc.to_string __x__)))) in
                       {
                         dbg = (debug_info_of_rpc __x483__);
                         dp = (dp_of_rpc __x484__);
                         sr = (sr_of_rpc __x485__);
                         vdi = (vdi_of_rpc __x486__)
                       }
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "request" "__x481__" (Rpc.to_string __x__) "Dict"
                        else ();
                        raise (Rpc.Runtime_error ("Dict", __x__))))
            type response = unit
            let rpc_of_response = function | __x480__ -> Rpc.Null
            let response_of_rpc =
              function
              | __x479__ ->
                  (match __x479__ with
                   | Rpc.Null -> ()
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "response" "__x479__" (Rpc.to_string __x__)
                            "Null"
                        else ();
                        raise (Rpc.Runtime_error ("Null", __x__))))
            let call_of_activate ~dbg  ~dp  ~sr  ~vdi  =
              let arg = { vdi; sr; dp; dbg } in
              Rpc.call "VDI.activate" [rpc_of_request arg]
          end
        module Deactivate =
          struct
            type request = {
              dbg: debug_info ;
              dp: dp ;
              sr: sr ;
              vdi: vdi }
            let rpc_of_request =
              function
              | __x500__ ->
                  let __x501__ = __x500__.dbg
                  and __x502__ = __x500__.dp
                  and __x503__ = __x500__.sr
                  and __x504__ = __x500__.vdi in
                  Rpc.Dict
                    [("dbg", (rpc_of_debug_info __x501__));
                    ("dp", (rpc_of_dp __x502__));
                    ("sr", (rpc_of_sr __x503__));
                    ("vdi", (rpc_of_vdi __x504__))]
            let request_of_rpc =
              function
              | __x494__ ->
                  (match __x494__ with
                   | Rpc.Dict __x495__ ->
                       let __x496__ =
                         try List.assoc "dbg" __x495__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x495__"
                                  (Printexc.to_string __x__)
                                  "Looking for key dbg"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key dbg",
                                     (Printexc.to_string __x__))))
                       and __x497__ =
                         try List.assoc "dp" __x495__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x495__"
                                  (Printexc.to_string __x__)
                                  "Looking for key dp"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key dp",
                                     (Printexc.to_string __x__))))
                       and __x498__ =
                         try List.assoc "sr" __x495__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x495__"
                                  (Printexc.to_string __x__)
                                  "Looking for key sr"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key sr",
                                     (Printexc.to_string __x__))))
                       and __x499__ =
                         try List.assoc "vdi" __x495__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x495__"
                                  (Printexc.to_string __x__)
                                  "Looking for key vdi"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key vdi",
                                     (Printexc.to_string __x__)))) in
                       {
                         dbg = (debug_info_of_rpc __x496__);
                         dp = (dp_of_rpc __x497__);
                         sr = (sr_of_rpc __x498__);
                         vdi = (vdi_of_rpc __x499__)
                       }
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "request" "__x494__" (Rpc.to_string __x__) "Dict"
                        else ();
                        raise (Rpc.Runtime_error ("Dict", __x__))))
            type response = unit
            let rpc_of_response = function | __x493__ -> Rpc.Null
            let response_of_rpc =
              function
              | __x492__ ->
                  (match __x492__ with
                   | Rpc.Null -> ()
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "response" "__x492__" (Rpc.to_string __x__)
                            "Null"
                        else ();
                        raise (Rpc.Runtime_error ("Null", __x__))))
            let call_of_deactivate ~dbg  ~dp  ~sr  ~vdi  =
              let arg = { vdi; sr; dp; dbg } in
              Rpc.call "VDI.deactivate" [rpc_of_request arg]
          end
        module Detach =
          struct
            type request = {
              dbg: debug_info ;
              dp: dp ;
              sr: sr ;
              vdi: vdi }
            let rpc_of_request =
              function
              | __x513__ ->
                  let __x514__ = __x513__.dbg
                  and __x515__ = __x513__.dp
                  and __x516__ = __x513__.sr
                  and __x517__ = __x513__.vdi in
                  Rpc.Dict
                    [("dbg", (rpc_of_debug_info __x514__));
                    ("dp", (rpc_of_dp __x515__));
                    ("sr", (rpc_of_sr __x516__));
                    ("vdi", (rpc_of_vdi __x517__))]
            let request_of_rpc =
              function
              | __x507__ ->
                  (match __x507__ with
                   | Rpc.Dict __x508__ ->
                       let __x509__ =
                         try List.assoc "dbg" __x508__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x508__"
                                  (Printexc.to_string __x__)
                                  "Looking for key dbg"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key dbg",
                                     (Printexc.to_string __x__))))
                       and __x510__ =
                         try List.assoc "dp" __x508__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x508__"
                                  (Printexc.to_string __x__)
                                  "Looking for key dp"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key dp",
                                     (Printexc.to_string __x__))))
                       and __x511__ =
                         try List.assoc "sr" __x508__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x508__"
                                  (Printexc.to_string __x__)
                                  "Looking for key sr"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key sr",
                                     (Printexc.to_string __x__))))
                       and __x512__ =
                         try List.assoc "vdi" __x508__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x508__"
                                  (Printexc.to_string __x__)
                                  "Looking for key vdi"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key vdi",
                                     (Printexc.to_string __x__)))) in
                       {
                         dbg = (debug_info_of_rpc __x509__);
                         dp = (dp_of_rpc __x510__);
                         sr = (sr_of_rpc __x511__);
                         vdi = (vdi_of_rpc __x512__)
                       }
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "request" "__x507__" (Rpc.to_string __x__) "Dict"
                        else ();
                        raise (Rpc.Runtime_error ("Dict", __x__))))
            type response = unit
            let rpc_of_response = function | __x506__ -> Rpc.Null
            let response_of_rpc =
              function
              | __x505__ ->
                  (match __x505__ with
                   | Rpc.Null -> ()
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "response" "__x505__" (Rpc.to_string __x__)
                            "Null"
                        else ();
                        raise (Rpc.Runtime_error ("Null", __x__))))
            let call_of_detach ~dbg  ~dp  ~sr  ~vdi  =
              let arg = { vdi; sr; dp; dbg } in
              Rpc.call "VDI.detach" [rpc_of_request arg]
          end
        module Epoch_end =
          struct
            type request = {
              dbg: debug_info ;
              sr: sr ;
              vdi: vdi }
            let rpc_of_request =
              function
              | __x525__ ->
                  let __x526__ = __x525__.dbg
                  and __x527__ = __x525__.sr
                  and __x528__ = __x525__.vdi in
                  Rpc.Dict
                    [("dbg", (rpc_of_debug_info __x526__));
                    ("sr", (rpc_of_sr __x527__));
                    ("vdi", (rpc_of_vdi __x528__))]
            let request_of_rpc =
              function
              | __x520__ ->
                  (match __x520__ with
                   | Rpc.Dict __x521__ ->
                       let __x522__ =
                         try List.assoc "dbg" __x521__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x521__"
                                  (Printexc.to_string __x__)
                                  "Looking for key dbg"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key dbg",
                                     (Printexc.to_string __x__))))
                       and __x523__ =
                         try List.assoc "sr" __x521__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x521__"
                                  (Printexc.to_string __x__)
                                  "Looking for key sr"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key sr",
                                     (Printexc.to_string __x__))))
                       and __x524__ =
                         try List.assoc "vdi" __x521__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x521__"
                                  (Printexc.to_string __x__)
                                  "Looking for key vdi"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key vdi",
                                     (Printexc.to_string __x__)))) in
                       {
                         dbg = (debug_info_of_rpc __x522__);
                         sr = (sr_of_rpc __x523__);
                         vdi = (vdi_of_rpc __x524__)
                       }
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "request" "__x520__" (Rpc.to_string __x__) "Dict"
                        else ();
                        raise (Rpc.Runtime_error ("Dict", __x__))))
            type response = unit
            let rpc_of_response = function | __x519__ -> Rpc.Null
            let response_of_rpc =
              function
              | __x518__ ->
                  (match __x518__ with
                   | Rpc.Null -> ()
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "response" "__x518__" (Rpc.to_string __x__)
                            "Null"
                        else ();
                        raise (Rpc.Runtime_error ("Null", __x__))))
            let call_of_epoch_end ~dbg  ~sr  ~vdi  =
              let arg = { vdi; sr; dbg } in
              Rpc.call "VDI.epoch_end" [rpc_of_request arg]
          end
        module Get_url =
          struct
            type request = {
              dbg: debug_info ;
              sr: sr ;
              vdi: vdi }
            let rpc_of_request =
              function
              | __x536__ ->
                  let __x537__ = __x536__.dbg
                  and __x538__ = __x536__.sr
                  and __x539__ = __x536__.vdi in
                  Rpc.Dict
                    [("dbg", (rpc_of_debug_info __x537__));
                    ("sr", (rpc_of_sr __x538__));
                    ("vdi", (rpc_of_vdi __x539__))]
            let request_of_rpc =
              function
              | __x531__ ->
                  (match __x531__ with
                   | Rpc.Dict __x532__ ->
                       let __x533__ =
                         try List.assoc "dbg" __x532__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x532__"
                                  (Printexc.to_string __x__)
                                  "Looking for key dbg"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key dbg",
                                     (Printexc.to_string __x__))))
                       and __x534__ =
                         try List.assoc "sr" __x532__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x532__"
                                  (Printexc.to_string __x__)
                                  "Looking for key sr"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key sr",
                                     (Printexc.to_string __x__))))
                       and __x535__ =
                         try List.assoc "vdi" __x532__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x532__"
                                  (Printexc.to_string __x__)
                                  "Looking for key vdi"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key vdi",
                                     (Printexc.to_string __x__)))) in
                       {
                         dbg = (debug_info_of_rpc __x533__);
                         sr = (sr_of_rpc __x534__);
                         vdi = (vdi_of_rpc __x535__)
                       }
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "request" "__x531__" (Rpc.to_string __x__) "Dict"
                        else ();
                        raise (Rpc.Runtime_error ("Dict", __x__))))
            type response = string
            let rpc_of_response = function | __x530__ -> Rpc.String __x530__
            let response_of_rpc =
              function
              | __x529__ ->
                  (match __x529__ with
                   | Rpc.String x -> x
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "response" "__x529__" (Rpc.to_string __x__)
                            "String(string)"
                        else ();
                        raise (Rpc.Runtime_error ("String(string)", __x__))))
            let call_of_get_url ~dbg  ~sr  ~vdi  =
              let arg = { vdi; sr; dbg } in
              Rpc.call "VDI.get_url" [rpc_of_request arg]
          end
        module Similar_content =
          struct
            type request = {
              dbg: debug_info ;
              sr: sr ;
              vdi: vdi }
            let rpc_of_request =
              function
              | __x550__ ->
                  let __x551__ = __x550__.dbg
                  and __x552__ = __x550__.sr
                  and __x553__ = __x550__.vdi in
                  Rpc.Dict
                    [("dbg", (rpc_of_debug_info __x551__));
                    ("sr", (rpc_of_sr __x552__));
                    ("vdi", (rpc_of_vdi __x553__))]
            let request_of_rpc =
              function
              | __x545__ ->
                  (match __x545__ with
                   | Rpc.Dict __x546__ ->
                       let __x547__ =
                         try List.assoc "dbg" __x546__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x546__"
                                  (Printexc.to_string __x__)
                                  "Looking for key dbg"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key dbg",
                                     (Printexc.to_string __x__))))
                       and __x548__ =
                         try List.assoc "sr" __x546__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x546__"
                                  (Printexc.to_string __x__)
                                  "Looking for key sr"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key sr",
                                     (Printexc.to_string __x__))))
                       and __x549__ =
                         try List.assoc "vdi" __x546__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x546__"
                                  (Printexc.to_string __x__)
                                  "Looking for key vdi"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key vdi",
                                     (Printexc.to_string __x__)))) in
                       {
                         dbg = (debug_info_of_rpc __x547__);
                         sr = (sr_of_rpc __x548__);
                         vdi = (vdi_of_rpc __x549__)
                       }
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "request" "__x545__" (Rpc.to_string __x__) "Dict"
                        else ();
                        raise (Rpc.Runtime_error ("Dict", __x__))))
            type response = vdi_info list
            let rpc_of_response =
              function
              | __x543__ ->
                  Rpc.Enum
                    (List.map
                       (function | __x544__ -> rpc_of_vdi_info __x544__)
                       __x543__)
            let response_of_rpc =
              function
              | __x540__ ->
                  (match __x540__ with
                   | Rpc.Enum __x541__ ->
                       List.map
                         (function | __x542__ -> vdi_info_of_rpc __x542__)
                         __x541__
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "response" "__x540__" (Rpc.to_string __x__)
                            "List"
                        else ();
                        raise (Rpc.Runtime_error ("List", __x__))))
            let call_of_similar_content ~dbg  ~sr  ~vdi  =
              let arg = { vdi; sr; dbg } in
              Rpc.call "VDI.similar_content" [rpc_of_request arg]
          end
        module Get_by_name =
          struct
            type request = {
              dbg: debug_info ;
              sr: sr ;
              name: string }
            let rpc_of_request =
              function
              | __x561__ ->
                  let __x562__ = __x561__.dbg
                  and __x563__ = __x561__.sr
                  and __x564__ = __x561__.name in
                  Rpc.Dict
                    [("dbg", (rpc_of_debug_info __x562__));
                    ("sr", (rpc_of_sr __x563__));
                    ("name", (Rpc.String __x564__))]
            let request_of_rpc =
              function
              | __x556__ ->
                  (match __x556__ with
                   | Rpc.Dict __x557__ ->
                       let __x558__ =
                         try List.assoc "dbg" __x557__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x557__"
                                  (Printexc.to_string __x__)
                                  "Looking for key dbg"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key dbg",
                                     (Printexc.to_string __x__))))
                       and __x559__ =
                         try List.assoc "sr" __x557__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x557__"
                                  (Printexc.to_string __x__)
                                  "Looking for key sr"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key sr",
                                     (Printexc.to_string __x__))))
                       and __x560__ =
                         try List.assoc "name" __x557__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x557__"
                                  (Printexc.to_string __x__)
                                  "Looking for key name"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key name",
                                     (Printexc.to_string __x__)))) in
                       {
                         dbg = (debug_info_of_rpc __x558__);
                         sr = (sr_of_rpc __x559__);
                         name =
                           ((match __x560__ with
                             | Rpc.String x -> x
                             | __x__ ->
                                 (if Rpc.get_debug ()
                                  then
                                    Printf.eprintf
                                      "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                      "request" "__x560__"
                                      (Rpc.to_string __x__) "String(string)"
                                  else ();
                                  raise
                                    (Rpc.Runtime_error
                                       ("String(string)", __x__)))))
                       }
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "request" "__x556__" (Rpc.to_string __x__) "Dict"
                        else ();
                        raise (Rpc.Runtime_error ("Dict", __x__))))
            type response = vdi_info
            let rpc_of_response =
              function | __x555__ -> rpc_of_vdi_info __x555__
            let response_of_rpc =
              function | __x554__ -> vdi_info_of_rpc __x554__
            let call_of_get_by_name ~dbg  ~sr  ~name  =
              let arg = { name; sr; dbg } in
              Rpc.call "VDI.get_by_name" [rpc_of_request arg]
          end
        module Set_content_id =
          struct
            type request =
              {
              dbg: debug_info ;
              sr: sr ;
              vdi: vdi ;
              content_id: content_id }
            let rpc_of_request =
              function
              | __x573__ ->
                  let __x574__ = __x573__.dbg
                  and __x575__ = __x573__.sr
                  and __x576__ = __x573__.vdi
                  and __x577__ = __x573__.content_id in
                  Rpc.Dict
                    [("dbg", (rpc_of_debug_info __x574__));
                    ("sr", (rpc_of_sr __x575__));
                    ("vdi", (rpc_of_vdi __x576__));
                    ("content_id", (rpc_of_content_id __x577__))]
            let request_of_rpc =
              function
              | __x567__ ->
                  (match __x567__ with
                   | Rpc.Dict __x568__ ->
                       let __x569__ =
                         try List.assoc "dbg" __x568__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x568__"
                                  (Printexc.to_string __x__)
                                  "Looking for key dbg"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key dbg",
                                     (Printexc.to_string __x__))))
                       and __x570__ =
                         try List.assoc "sr" __x568__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x568__"
                                  (Printexc.to_string __x__)
                                  "Looking for key sr"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key sr",
                                     (Printexc.to_string __x__))))
                       and __x571__ =
                         try List.assoc "vdi" __x568__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x568__"
                                  (Printexc.to_string __x__)
                                  "Looking for key vdi"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key vdi",
                                     (Printexc.to_string __x__))))
                       and __x572__ =
                         try List.assoc "content_id" __x568__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x568__"
                                  (Printexc.to_string __x__)
                                  "Looking for key content_id"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key content_id",
                                     (Printexc.to_string __x__)))) in
                       {
                         dbg = (debug_info_of_rpc __x569__);
                         sr = (sr_of_rpc __x570__);
                         vdi = (vdi_of_rpc __x571__);
                         content_id = (content_id_of_rpc __x572__)
                       }
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "request" "__x567__" (Rpc.to_string __x__) "Dict"
                        else ();
                        raise (Rpc.Runtime_error ("Dict", __x__))))
            type response = unit
            let rpc_of_response = function | __x566__ -> Rpc.Null
            let response_of_rpc =
              function
              | __x565__ ->
                  (match __x565__ with
                   | Rpc.Null -> ()
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "response" "__x565__" (Rpc.to_string __x__)
                            "Null"
                        else ();
                        raise (Rpc.Runtime_error ("Null", __x__))))
            let call_of_set_content_id ~dbg  ~sr  ~vdi  ~content_id  =
              let arg = { content_id; vdi; sr; dbg } in
              Rpc.call "VDI.set_content_id" [rpc_of_request arg]
          end
        module Compose =
          struct
            type request = {
              dbg: debug_info ;
              sr: sr ;
              vdi1: vdi ;
              vdi2: vdi }
            let rpc_of_request =
              function
              | __x586__ ->
                  let __x587__ = __x586__.dbg
                  and __x588__ = __x586__.sr
                  and __x589__ = __x586__.vdi1
                  and __x590__ = __x586__.vdi2 in
                  Rpc.Dict
                    [("dbg", (rpc_of_debug_info __x587__));
                    ("sr", (rpc_of_sr __x588__));
                    ("vdi1", (rpc_of_vdi __x589__));
                    ("vdi2", (rpc_of_vdi __x590__))]
            let request_of_rpc =
              function
              | __x580__ ->
                  (match __x580__ with
                   | Rpc.Dict __x581__ ->
                       let __x582__ =
                         try List.assoc "dbg" __x581__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x581__"
                                  (Printexc.to_string __x__)
                                  "Looking for key dbg"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key dbg",
                                     (Printexc.to_string __x__))))
                       and __x583__ =
                         try List.assoc "sr" __x581__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x581__"
                                  (Printexc.to_string __x__)
                                  "Looking for key sr"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key sr",
                                     (Printexc.to_string __x__))))
                       and __x584__ =
                         try List.assoc "vdi1" __x581__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x581__"
                                  (Printexc.to_string __x__)
                                  "Looking for key vdi1"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key vdi1",
                                     (Printexc.to_string __x__))))
                       and __x585__ =
                         try List.assoc "vdi2" __x581__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x581__"
                                  (Printexc.to_string __x__)
                                  "Looking for key vdi2"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key vdi2",
                                     (Printexc.to_string __x__)))) in
                       {
                         dbg = (debug_info_of_rpc __x582__);
                         sr = (sr_of_rpc __x583__);
                         vdi1 = (vdi_of_rpc __x584__);
                         vdi2 = (vdi_of_rpc __x585__)
                       }
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "request" "__x580__" (Rpc.to_string __x__) "Dict"
                        else ();
                        raise (Rpc.Runtime_error ("Dict", __x__))))
            type response = unit
            let rpc_of_response = function | __x579__ -> Rpc.Null
            let response_of_rpc =
              function
              | __x578__ ->
                  (match __x578__ with
                   | Rpc.Null -> ()
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "response" "__x578__" (Rpc.to_string __x__)
                            "Null"
                        else ();
                        raise (Rpc.Runtime_error ("Null", __x__))))
            let call_of_compose ~dbg  ~sr  ~vdi1  ~vdi2  =
              let arg = { vdi2; vdi1; sr; dbg } in
              Rpc.call "VDI.compose" [rpc_of_request arg]
          end
        module Add_to_sm_config =
          struct
            type request =
              {
              dbg: debug_info ;
              sr: sr ;
              vdi: vdi ;
              key: string ;
              value: string }
            let rpc_of_request =
              function
              | __x600__ ->
                  let __x601__ = __x600__.dbg
                  and __x602__ = __x600__.sr
                  and __x603__ = __x600__.vdi
                  and __x604__ = __x600__.key
                  and __x605__ = __x600__.value in
                  Rpc.Dict
                    [("dbg", (rpc_of_debug_info __x601__));
                    ("sr", (rpc_of_sr __x602__));
                    ("vdi", (rpc_of_vdi __x603__));
                    ("key", (Rpc.String __x604__));
                    ("value", (Rpc.String __x605__))]
            let request_of_rpc =
              function
              | __x593__ ->
                  (match __x593__ with
                   | Rpc.Dict __x594__ ->
                       let __x595__ =
                         try List.assoc "dbg" __x594__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x594__"
                                  (Printexc.to_string __x__)
                                  "Looking for key dbg"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key dbg",
                                     (Printexc.to_string __x__))))
                       and __x596__ =
                         try List.assoc "sr" __x594__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x594__"
                                  (Printexc.to_string __x__)
                                  "Looking for key sr"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key sr",
                                     (Printexc.to_string __x__))))
                       and __x597__ =
                         try List.assoc "vdi" __x594__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x594__"
                                  (Printexc.to_string __x__)
                                  "Looking for key vdi"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key vdi",
                                     (Printexc.to_string __x__))))
                       and __x598__ =
                         try List.assoc "key" __x594__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x594__"
                                  (Printexc.to_string __x__)
                                  "Looking for key key"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key key",
                                     (Printexc.to_string __x__))))
                       and __x599__ =
                         try List.assoc "value" __x594__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x594__"
                                  (Printexc.to_string __x__)
                                  "Looking for key value"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key value",
                                     (Printexc.to_string __x__)))) in
                       {
                         dbg = (debug_info_of_rpc __x595__);
                         sr = (sr_of_rpc __x596__);
                         vdi = (vdi_of_rpc __x597__);
                         key =
                           ((match __x598__ with
                             | Rpc.String x -> x
                             | __x__ ->
                                 (if Rpc.get_debug ()
                                  then
                                    Printf.eprintf
                                      "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                      "request" "__x598__"
                                      (Rpc.to_string __x__) "String(string)"
                                  else ();
                                  raise
                                    (Rpc.Runtime_error
                                       ("String(string)", __x__)))));
                         value =
                           ((match __x599__ with
                             | Rpc.String x -> x
                             | __x__ ->
                                 (if Rpc.get_debug ()
                                  then
                                    Printf.eprintf
                                      "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                      "request" "__x599__"
                                      (Rpc.to_string __x__) "String(string)"
                                  else ();
                                  raise
                                    (Rpc.Runtime_error
                                       ("String(string)", __x__)))))
                       }
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "request" "__x593__" (Rpc.to_string __x__) "Dict"
                        else ();
                        raise (Rpc.Runtime_error ("Dict", __x__))))
            type response = unit
            let rpc_of_response = function | __x592__ -> Rpc.Null
            let response_of_rpc =
              function
              | __x591__ ->
                  (match __x591__ with
                   | Rpc.Null -> ()
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "response" "__x591__" (Rpc.to_string __x__)
                            "Null"
                        else ();
                        raise (Rpc.Runtime_error ("Null", __x__))))
            let call_of_add_to_sm_config ~dbg  ~sr  ~vdi  ~key  ~value  =
              let arg = { value; key; vdi; sr; dbg } in
              Rpc.call "VDI.add_to_sm_config" [rpc_of_request arg]
          end
        module Remove_from_sm_config =
          struct
            type request = {
              dbg: debug_info ;
              sr: sr ;
              vdi: vdi ;
              key: string }
            let rpc_of_request =
              function
              | __x614__ ->
                  let __x615__ = __x614__.dbg
                  and __x616__ = __x614__.sr
                  and __x617__ = __x614__.vdi
                  and __x618__ = __x614__.key in
                  Rpc.Dict
                    [("dbg", (rpc_of_debug_info __x615__));
                    ("sr", (rpc_of_sr __x616__));
                    ("vdi", (rpc_of_vdi __x617__));
                    ("key", (Rpc.String __x618__))]
            let request_of_rpc =
              function
              | __x608__ ->
                  (match __x608__ with
                   | Rpc.Dict __x609__ ->
                       let __x610__ =
                         try List.assoc "dbg" __x609__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x609__"
                                  (Printexc.to_string __x__)
                                  "Looking for key dbg"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key dbg",
                                     (Printexc.to_string __x__))))
                       and __x611__ =
                         try List.assoc "sr" __x609__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x609__"
                                  (Printexc.to_string __x__)
                                  "Looking for key sr"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key sr",
                                     (Printexc.to_string __x__))))
                       and __x612__ =
                         try List.assoc "vdi" __x609__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x609__"
                                  (Printexc.to_string __x__)
                                  "Looking for key vdi"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key vdi",
                                     (Printexc.to_string __x__))))
                       and __x613__ =
                         try List.assoc "key" __x609__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x609__"
                                  (Printexc.to_string __x__)
                                  "Looking for key key"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key key",
                                     (Printexc.to_string __x__)))) in
                       {
                         dbg = (debug_info_of_rpc __x610__);
                         sr = (sr_of_rpc __x611__);
                         vdi = (vdi_of_rpc __x612__);
                         key =
                           ((match __x613__ with
                             | Rpc.String x -> x
                             | __x__ ->
                                 (if Rpc.get_debug ()
                                  then
                                    Printf.eprintf
                                      "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                      "request" "__x613__"
                                      (Rpc.to_string __x__) "String(string)"
                                  else ();
                                  raise
                                    (Rpc.Runtime_error
                                       ("String(string)", __x__)))))
                       }
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "request" "__x608__" (Rpc.to_string __x__) "Dict"
                        else ();
                        raise (Rpc.Runtime_error ("Dict", __x__))))
            type response = unit
            let rpc_of_response = function | __x607__ -> Rpc.Null
            let response_of_rpc =
              function
              | __x606__ ->
                  (match __x606__ with
                   | Rpc.Null -> ()
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "response" "__x606__" (Rpc.to_string __x__)
                            "Null"
                        else ();
                        raise (Rpc.Runtime_error ("Null", __x__))))
            let call_of_remove_from_sm_config ~dbg  ~sr  ~vdi  ~key  =
              let arg = { key; vdi; sr; dbg } in
              Rpc.call "VDI.remove_from_sm_config" [rpc_of_request arg]
          end
        module Enable_cbt =
          struct
            type request = {
              dbg: debug_info ;
              sr: sr ;
              vdi: vdi }
            let rpc_of_request =
              function
              | __x626__ ->
                  let __x627__ = __x626__.dbg
                  and __x628__ = __x626__.sr
                  and __x629__ = __x626__.vdi in
                  Rpc.Dict
                    [("dbg", (rpc_of_debug_info __x627__));
                    ("sr", (rpc_of_sr __x628__));
                    ("vdi", (rpc_of_vdi __x629__))]
            let request_of_rpc =
              function
              | __x621__ ->
                  (match __x621__ with
                   | Rpc.Dict __x622__ ->
                       let __x623__ =
                         try List.assoc "dbg" __x622__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x622__"
                                  (Printexc.to_string __x__)
                                  "Looking for key dbg"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key dbg",
                                     (Printexc.to_string __x__))))
                       and __x624__ =
                         try List.assoc "sr" __x622__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x622__"
                                  (Printexc.to_string __x__)
                                  "Looking for key sr"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key sr",
                                     (Printexc.to_string __x__))))
                       and __x625__ =
                         try List.assoc "vdi" __x622__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x622__"
                                  (Printexc.to_string __x__)
                                  "Looking for key vdi"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key vdi",
                                     (Printexc.to_string __x__)))) in
                       {
                         dbg = (debug_info_of_rpc __x623__);
                         sr = (sr_of_rpc __x624__);
                         vdi = (vdi_of_rpc __x625__)
                       }
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "request" "__x621__" (Rpc.to_string __x__) "Dict"
                        else ();
                        raise (Rpc.Runtime_error ("Dict", __x__))))
            type response = unit
            let rpc_of_response = function | __x620__ -> Rpc.Null
            let response_of_rpc =
              function
              | __x619__ ->
                  (match __x619__ with
                   | Rpc.Null -> ()
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "response" "__x619__" (Rpc.to_string __x__)
                            "Null"
                        else ();
                        raise (Rpc.Runtime_error ("Null", __x__))))
            let call_of_enable_cbt ~dbg  ~sr  ~vdi  =
              let arg = { vdi; sr; dbg } in
              Rpc.call "VDI.enable_cbt" [rpc_of_request arg]
          end
        module Disable_cbt =
          struct
            type request = {
              dbg: debug_info ;
              sr: sr ;
              vdi: vdi }
            let rpc_of_request =
              function
              | __x637__ ->
                  let __x638__ = __x637__.dbg
                  and __x639__ = __x637__.sr
                  and __x640__ = __x637__.vdi in
                  Rpc.Dict
                    [("dbg", (rpc_of_debug_info __x638__));
                    ("sr", (rpc_of_sr __x639__));
                    ("vdi", (rpc_of_vdi __x640__))]
            let request_of_rpc =
              function
              | __x632__ ->
                  (match __x632__ with
                   | Rpc.Dict __x633__ ->
                       let __x634__ =
                         try List.assoc "dbg" __x633__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x633__"
                                  (Printexc.to_string __x__)
                                  "Looking for key dbg"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key dbg",
                                     (Printexc.to_string __x__))))
                       and __x635__ =
                         try List.assoc "sr" __x633__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x633__"
                                  (Printexc.to_string __x__)
                                  "Looking for key sr"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key sr",
                                     (Printexc.to_string __x__))))
                       and __x636__ =
                         try List.assoc "vdi" __x633__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x633__"
                                  (Printexc.to_string __x__)
                                  "Looking for key vdi"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key vdi",
                                     (Printexc.to_string __x__)))) in
                       {
                         dbg = (debug_info_of_rpc __x634__);
                         sr = (sr_of_rpc __x635__);
                         vdi = (vdi_of_rpc __x636__)
                       }
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "request" "__x632__" (Rpc.to_string __x__) "Dict"
                        else ();
                        raise (Rpc.Runtime_error ("Dict", __x__))))
            type response = unit
            let rpc_of_response = function | __x631__ -> Rpc.Null
            let response_of_rpc =
              function
              | __x630__ ->
                  (match __x630__ with
                   | Rpc.Null -> ()
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "response" "__x630__" (Rpc.to_string __x__)
                            "Null"
                        else ();
                        raise (Rpc.Runtime_error ("Null", __x__))))
            let call_of_disable_cbt ~dbg  ~sr  ~vdi  =
              let arg = { vdi; sr; dbg } in
              Rpc.call "VDI.disable_cbt" [rpc_of_request arg]
          end
        module Data_destroy =
          struct
            type request = {
              dbg: debug_info ;
              sr: sr ;
              vdi: vdi }
            let rpc_of_request =
              function
              | __x648__ ->
                  let __x649__ = __x648__.dbg
                  and __x650__ = __x648__.sr
                  and __x651__ = __x648__.vdi in
                  Rpc.Dict
                    [("dbg", (rpc_of_debug_info __x649__));
                    ("sr", (rpc_of_sr __x650__));
                    ("vdi", (rpc_of_vdi __x651__))]
            let request_of_rpc =
              function
              | __x643__ ->
                  (match __x643__ with
                   | Rpc.Dict __x644__ ->
                       let __x645__ =
                         try List.assoc "dbg" __x644__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x644__"
                                  (Printexc.to_string __x__)
                                  "Looking for key dbg"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key dbg",
                                     (Printexc.to_string __x__))))
                       and __x646__ =
                         try List.assoc "sr" __x644__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x644__"
                                  (Printexc.to_string __x__)
                                  "Looking for key sr"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key sr",
                                     (Printexc.to_string __x__))))
                       and __x647__ =
                         try List.assoc "vdi" __x644__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x644__"
                                  (Printexc.to_string __x__)
                                  "Looking for key vdi"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key vdi",
                                     (Printexc.to_string __x__)))) in
                       {
                         dbg = (debug_info_of_rpc __x645__);
                         sr = (sr_of_rpc __x646__);
                         vdi = (vdi_of_rpc __x647__)
                       }
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "request" "__x643__" (Rpc.to_string __x__) "Dict"
                        else ();
                        raise (Rpc.Runtime_error ("Dict", __x__))))
            type response = unit
            let rpc_of_response = function | __x642__ -> Rpc.Null
            let response_of_rpc =
              function
              | __x641__ ->
                  (match __x641__ with
                   | Rpc.Null -> ()
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "response" "__x641__" (Rpc.to_string __x__)
                            "Null"
                        else ();
                        raise (Rpc.Runtime_error ("Null", __x__))))
            let call_of_data_destroy ~dbg  ~sr  ~vdi  =
              let arg = { vdi; sr; dbg } in
              Rpc.call "VDI.data_destroy" [rpc_of_request arg]
          end
        module List_changed_blocks =
          struct
            type request =
              {
              dbg: debug_info ;
              sr: sr ;
              vdi_from: vdi ;
              vdi_to: vdi }
            let rpc_of_request =
              function
              | __x660__ ->
                  let __x661__ = __x660__.dbg
                  and __x662__ = __x660__.sr
                  and __x663__ = __x660__.vdi_from
                  and __x664__ = __x660__.vdi_to in
                  Rpc.Dict
                    [("dbg", (rpc_of_debug_info __x661__));
                    ("sr", (rpc_of_sr __x662__));
                    ("vdi_from", (rpc_of_vdi __x663__));
                    ("vdi_to", (rpc_of_vdi __x664__))]
            let request_of_rpc =
              function
              | __x654__ ->
                  (match __x654__ with
                   | Rpc.Dict __x655__ ->
                       let __x656__ =
                         try List.assoc "dbg" __x655__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x655__"
                                  (Printexc.to_string __x__)
                                  "Looking for key dbg"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key dbg",
                                     (Printexc.to_string __x__))))
                       and __x657__ =
                         try List.assoc "sr" __x655__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x655__"
                                  (Printexc.to_string __x__)
                                  "Looking for key sr"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key sr",
                                     (Printexc.to_string __x__))))
                       and __x658__ =
                         try List.assoc "vdi_from" __x655__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x655__"
                                  (Printexc.to_string __x__)
                                  "Looking for key vdi_from"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key vdi_from",
                                     (Printexc.to_string __x__))))
                       and __x659__ =
                         try List.assoc "vdi_to" __x655__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x655__"
                                  (Printexc.to_string __x__)
                                  "Looking for key vdi_to"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key vdi_to",
                                     (Printexc.to_string __x__)))) in
                       {
                         dbg = (debug_info_of_rpc __x656__);
                         sr = (sr_of_rpc __x657__);
                         vdi_from = (vdi_of_rpc __x658__);
                         vdi_to = (vdi_of_rpc __x659__)
                       }
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "request" "__x654__" (Rpc.to_string __x__) "Dict"
                        else ();
                        raise (Rpc.Runtime_error ("Dict", __x__))))
            type response = string
            let rpc_of_response = function | __x653__ -> Rpc.String __x653__
            let response_of_rpc =
              function
              | __x652__ ->
                  (match __x652__ with
                   | Rpc.String x -> x
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "response" "__x652__" (Rpc.to_string __x__)
                            "String(string)"
                        else ();
                        raise (Rpc.Runtime_error ("String(string)", __x__))))
            let call_of_list_changed_blocks ~dbg  ~sr  ~vdi_from  ~vdi_to  =
              let arg = { vdi_to; vdi_from; sr; dbg } in
              Rpc.call "VDI.list_changed_blocks" [rpc_of_request arg]
          end
      end
    module Get_by_name =
      struct
        type request = {
          dbg: debug_info ;
          name: string }
        let rpc_of_request =
          function
          | __x675__ ->
              let __x676__ = __x675__.dbg
              and __x677__ = __x675__.name in
              Rpc.Dict
                [("dbg", (rpc_of_debug_info __x676__));
                ("name", (Rpc.String __x677__))]
        let request_of_rpc =
          function
          | __x671__ ->
              (match __x671__ with
               | Rpc.Dict __x672__ ->
                   let __x673__ =
                     try List.assoc "dbg" __x672__
                     with
                     | __x__ ->
                         (if Rpc.get_debug ()
                          then
                            Printf.eprintf
                              "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                              "request" "__x672__" (Printexc.to_string __x__)
                              "Looking for key dbg"
                          else ();
                          raise
                            (Rpc.Runtime_exception
                               ("Looking for key dbg",
                                 (Printexc.to_string __x__))))
                   and __x674__ =
                     try List.assoc "name" __x672__
                     with
                     | __x__ ->
                         (if Rpc.get_debug ()
                          then
                            Printf.eprintf
                              "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                              "request" "__x672__" (Printexc.to_string __x__)
                              "Looking for key name"
                          else ();
                          raise
                            (Rpc.Runtime_exception
                               ("Looking for key name",
                                 (Printexc.to_string __x__)))) in
                   {
                     dbg = (debug_info_of_rpc __x673__);
                     name =
                       ((match __x674__ with
                         | Rpc.String x -> x
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                  "request" "__x674__" (Rpc.to_string __x__)
                                  "String(string)"
                              else ();
                              raise
                                (Rpc.Runtime_error ("String(string)", __x__)))))
                   }
               | __x__ ->
                   (if Rpc.get_debug ()
                    then
                      Printf.eprintf
                        "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                        "request" "__x671__" (Rpc.to_string __x__) "Dict"
                    else ();
                    raise (Rpc.Runtime_error ("Dict", __x__))))
        type response = (sr * vdi_info)
        let rpc_of_response =
          function
          | __x668__ ->
              let (__x669__, __x670__) = __x668__ in
              Rpc.Enum [rpc_of_sr __x669__; rpc_of_vdi_info __x670__]
        let response_of_rpc =
          function
          | __x665__ ->
              (match __x665__ with
               | Rpc.Enum (__x666__::__x667__::[]) ->
                   ((sr_of_rpc __x666__), (vdi_info_of_rpc __x667__))
               | __x__ ->
                   (if Rpc.get_debug ()
                    then
                      Printf.eprintf
                        "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                        "response" "__x665__" (Rpc.to_string __x__) "List"
                    else ();
                    raise (Rpc.Runtime_error ("List", __x__))))
        let call_of_get_by_name ~dbg  ~name  =
          let arg = { name; dbg } in
          Rpc.call "get_by_name" [rpc_of_request arg]
      end
    module DATA =
      struct
        module Copy_into =
          struct
            type request =
              {
              dbg: debug_info ;
              sr: sr ;
              vdi: vdi ;
              url: string ;
              dest: sr ;
              dest_vdi: vdi }
            let rpc_of_request =
              function
              | __x688__ ->
                  let __x689__ = __x688__.dbg
                  and __x690__ = __x688__.sr
                  and __x691__ = __x688__.vdi
                  and __x692__ = __x688__.url
                  and __x693__ = __x688__.dest
                  and __x694__ = __x688__.dest_vdi in
                  Rpc.Dict
                    [("dbg", (rpc_of_debug_info __x689__));
                    ("sr", (rpc_of_sr __x690__));
                    ("vdi", (rpc_of_vdi __x691__));
                    ("url", (Rpc.String __x692__));
                    ("dest", (rpc_of_sr __x693__));
                    ("dest_vdi", (rpc_of_vdi __x694__))]
            let request_of_rpc =
              function
              | __x680__ ->
                  (match __x680__ with
                   | Rpc.Dict __x681__ ->
                       let __x682__ =
                         try List.assoc "dbg" __x681__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x681__"
                                  (Printexc.to_string __x__)
                                  "Looking for key dbg"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key dbg",
                                     (Printexc.to_string __x__))))
                       and __x683__ =
                         try List.assoc "sr" __x681__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x681__"
                                  (Printexc.to_string __x__)
                                  "Looking for key sr"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key sr",
                                     (Printexc.to_string __x__))))
                       and __x684__ =
                         try List.assoc "vdi" __x681__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x681__"
                                  (Printexc.to_string __x__)
                                  "Looking for key vdi"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key vdi",
                                     (Printexc.to_string __x__))))
                       and __x685__ =
                         try List.assoc "url" __x681__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x681__"
                                  (Printexc.to_string __x__)
                                  "Looking for key url"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key url",
                                     (Printexc.to_string __x__))))
                       and __x686__ =
                         try List.assoc "dest" __x681__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x681__"
                                  (Printexc.to_string __x__)
                                  "Looking for key dest"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key dest",
                                     (Printexc.to_string __x__))))
                       and __x687__ =
                         try List.assoc "dest_vdi" __x681__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x681__"
                                  (Printexc.to_string __x__)
                                  "Looking for key dest_vdi"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key dest_vdi",
                                     (Printexc.to_string __x__)))) in
                       {
                         dbg = (debug_info_of_rpc __x682__);
                         sr = (sr_of_rpc __x683__);
                         vdi = (vdi_of_rpc __x684__);
                         url =
                           ((match __x685__ with
                             | Rpc.String x -> x
                             | __x__ ->
                                 (if Rpc.get_debug ()
                                  then
                                    Printf.eprintf
                                      "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                      "request" "__x685__"
                                      (Rpc.to_string __x__) "String(string)"
                                  else ();
                                  raise
                                    (Rpc.Runtime_error
                                       ("String(string)", __x__)))));
                         dest = (sr_of_rpc __x686__);
                         dest_vdi = (vdi_of_rpc __x687__)
                       }
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "request" "__x680__" (Rpc.to_string __x__) "Dict"
                        else ();
                        raise (Rpc.Runtime_error ("Dict", __x__))))
            type response = Task.id
            let rpc_of_response =
              function | __x679__ -> Task.rpc_of_id __x679__
            let response_of_rpc =
              function | __x678__ -> Task.id_of_rpc __x678__
            let call_of_copy_into ~dbg  ~sr  ~vdi  ~url  ~dest  ~dest_vdi  =
              let arg = { dest_vdi; dest; url; vdi; sr; dbg } in
              Rpc.call "DATA.copy_into" [rpc_of_request arg]
          end
        module Copy =
          struct
            type request =
              {
              dbg: debug_info ;
              sr: sr ;
              vdi: vdi ;
              dp: dp ;
              url: string ;
              dest: sr }
            let rpc_of_request =
              function
              | __x705__ ->
                  let __x706__ = __x705__.dbg
                  and __x707__ = __x705__.sr
                  and __x708__ = __x705__.vdi
                  and __x709__ = __x705__.dp
                  and __x710__ = __x705__.url
                  and __x711__ = __x705__.dest in
                  Rpc.Dict
                    [("dbg", (rpc_of_debug_info __x706__));
                    ("sr", (rpc_of_sr __x707__));
                    ("vdi", (rpc_of_vdi __x708__));
                    ("dp", (rpc_of_dp __x709__));
                    ("url", (Rpc.String __x710__));
                    ("dest", (rpc_of_sr __x711__))]
            let request_of_rpc =
              function
              | __x697__ ->
                  (match __x697__ with
                   | Rpc.Dict __x698__ ->
                       let __x699__ =
                         try List.assoc "dbg" __x698__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x698__"
                                  (Printexc.to_string __x__)
                                  "Looking for key dbg"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key dbg",
                                     (Printexc.to_string __x__))))
                       and __x700__ =
                         try List.assoc "sr" __x698__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x698__"
                                  (Printexc.to_string __x__)
                                  "Looking for key sr"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key sr",
                                     (Printexc.to_string __x__))))
                       and __x701__ =
                         try List.assoc "vdi" __x698__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x698__"
                                  (Printexc.to_string __x__)
                                  "Looking for key vdi"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key vdi",
                                     (Printexc.to_string __x__))))
                       and __x702__ =
                         try List.assoc "dp" __x698__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x698__"
                                  (Printexc.to_string __x__)
                                  "Looking for key dp"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key dp",
                                     (Printexc.to_string __x__))))
                       and __x703__ =
                         try List.assoc "url" __x698__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x698__"
                                  (Printexc.to_string __x__)
                                  "Looking for key url"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key url",
                                     (Printexc.to_string __x__))))
                       and __x704__ =
                         try List.assoc "dest" __x698__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x698__"
                                  (Printexc.to_string __x__)
                                  "Looking for key dest"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key dest",
                                     (Printexc.to_string __x__)))) in
                       {
                         dbg = (debug_info_of_rpc __x699__);
                         sr = (sr_of_rpc __x700__);
                         vdi = (vdi_of_rpc __x701__);
                         dp = (dp_of_rpc __x702__);
                         url =
                           ((match __x703__ with
                             | Rpc.String x -> x
                             | __x__ ->
                                 (if Rpc.get_debug ()
                                  then
                                    Printf.eprintf
                                      "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                      "request" "__x703__"
                                      (Rpc.to_string __x__) "String(string)"
                                  else ();
                                  raise
                                    (Rpc.Runtime_error
                                       ("String(string)", __x__)))));
                         dest = (sr_of_rpc __x704__)
                       }
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "request" "__x697__" (Rpc.to_string __x__) "Dict"
                        else ();
                        raise (Rpc.Runtime_error ("Dict", __x__))))
            type response = Task.id
            let rpc_of_response =
              function | __x696__ -> Task.rpc_of_id __x696__
            let response_of_rpc =
              function | __x695__ -> Task.id_of_rpc __x695__
            let call_of_copy ~dbg  ~sr  ~vdi  ~dp  ~url  ~dest  =
              let arg = { dest; url; dp; vdi; sr; dbg } in
              Rpc.call "DATA.copy" [rpc_of_request arg]
          end
        module MIRROR =
          struct
            module Start =
              struct
                type request =
                  {
                  dbg: debug_info ;
                  sr: sr ;
                  vdi: vdi ;
                  dp: dp ;
                  url: string ;
                  dest: sr }
                let rpc_of_request =
                  function
                  | __x722__ ->
                      let __x723__ = __x722__.dbg
                      and __x724__ = __x722__.sr
                      and __x725__ = __x722__.vdi
                      and __x726__ = __x722__.dp
                      and __x727__ = __x722__.url
                      and __x728__ = __x722__.dest in
                      Rpc.Dict
                        [("dbg", (rpc_of_debug_info __x723__));
                        ("sr", (rpc_of_sr __x724__));
                        ("vdi", (rpc_of_vdi __x725__));
                        ("dp", (rpc_of_dp __x726__));
                        ("url", (Rpc.String __x727__));
                        ("dest", (rpc_of_sr __x728__))]
                let request_of_rpc =
                  function
                  | __x714__ ->
                      (match __x714__ with
                       | Rpc.Dict __x715__ ->
                           let __x716__ =
                             try List.assoc "dbg" __x715__
                             with
                             | __x__ ->
                                 (if Rpc.get_debug ()
                                  then
                                    Printf.eprintf
                                      "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                      "request" "__x715__"
                                      (Printexc.to_string __x__)
                                      "Looking for key dbg"
                                  else ();
                                  raise
                                    (Rpc.Runtime_exception
                                       ("Looking for key dbg",
                                         (Printexc.to_string __x__))))
                           and __x717__ =
                             try List.assoc "sr" __x715__
                             with
                             | __x__ ->
                                 (if Rpc.get_debug ()
                                  then
                                    Printf.eprintf
                                      "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                      "request" "__x715__"
                                      (Printexc.to_string __x__)
                                      "Looking for key sr"
                                  else ();
                                  raise
                                    (Rpc.Runtime_exception
                                       ("Looking for key sr",
                                         (Printexc.to_string __x__))))
                           and __x718__ =
                             try List.assoc "vdi" __x715__
                             with
                             | __x__ ->
                                 (if Rpc.get_debug ()
                                  then
                                    Printf.eprintf
                                      "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                      "request" "__x715__"
                                      (Printexc.to_string __x__)
                                      "Looking for key vdi"
                                  else ();
                                  raise
                                    (Rpc.Runtime_exception
                                       ("Looking for key vdi",
                                         (Printexc.to_string __x__))))
                           and __x719__ =
                             try List.assoc "dp" __x715__
                             with
                             | __x__ ->
                                 (if Rpc.get_debug ()
                                  then
                                    Printf.eprintf
                                      "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                      "request" "__x715__"
                                      (Printexc.to_string __x__)
                                      "Looking for key dp"
                                  else ();
                                  raise
                                    (Rpc.Runtime_exception
                                       ("Looking for key dp",
                                         (Printexc.to_string __x__))))
                           and __x720__ =
                             try List.assoc "url" __x715__
                             with
                             | __x__ ->
                                 (if Rpc.get_debug ()
                                  then
                                    Printf.eprintf
                                      "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                      "request" "__x715__"
                                      (Printexc.to_string __x__)
                                      "Looking for key url"
                                  else ();
                                  raise
                                    (Rpc.Runtime_exception
                                       ("Looking for key url",
                                         (Printexc.to_string __x__))))
                           and __x721__ =
                             try List.assoc "dest" __x715__
                             with
                             | __x__ ->
                                 (if Rpc.get_debug ()
                                  then
                                    Printf.eprintf
                                      "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                      "request" "__x715__"
                                      (Printexc.to_string __x__)
                                      "Looking for key dest"
                                  else ();
                                  raise
                                    (Rpc.Runtime_exception
                                       ("Looking for key dest",
                                         (Printexc.to_string __x__)))) in
                           {
                             dbg = (debug_info_of_rpc __x716__);
                             sr = (sr_of_rpc __x717__);
                             vdi = (vdi_of_rpc __x718__);
                             dp = (dp_of_rpc __x719__);
                             url =
                               ((match __x720__ with
                                 | Rpc.String x -> x
                                 | __x__ ->
                                     (if Rpc.get_debug ()
                                      then
                                        Printf.eprintf
                                          "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                          "request" "__x720__"
                                          (Rpc.to_string __x__)
                                          "String(string)"
                                      else ();
                                      raise
                                        (Rpc.Runtime_error
                                           ("String(string)", __x__)))));
                             dest = (sr_of_rpc __x721__)
                           }
                       | __x__ ->
                           (if Rpc.get_debug ()
                            then
                              Printf.eprintf
                                "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                "request" "__x714__" (Rpc.to_string __x__)
                                "Dict"
                            else ();
                            raise (Rpc.Runtime_error ("Dict", __x__))))
                type response = Task.id
                let rpc_of_response =
                  function | __x713__ -> Task.rpc_of_id __x713__
                let response_of_rpc =
                  function | __x712__ -> Task.id_of_rpc __x712__
                let call_of_start ~dbg  ~sr  ~vdi  ~dp  ~url  ~dest  =
                  let arg = { dest; url; dp; vdi; sr; dbg } in
                  Rpc.call "DATA.MIRROR.start" [rpc_of_request arg]
              end
            module Stop =
              struct
                type request = {
                  dbg: debug_info ;
                  id: Mirror.id }
                let rpc_of_request =
                  function
                  | __x735__ ->
                      let __x736__ = __x735__.dbg
                      and __x737__ = __x735__.id in
                      Rpc.Dict
                        [("dbg", (rpc_of_debug_info __x736__));
                        ("id", (Mirror.rpc_of_id __x737__))]
                let request_of_rpc =
                  function
                  | __x731__ ->
                      (match __x731__ with
                       | Rpc.Dict __x732__ ->
                           let __x733__ =
                             try List.assoc "dbg" __x732__
                             with
                             | __x__ ->
                                 (if Rpc.get_debug ()
                                  then
                                    Printf.eprintf
                                      "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                      "request" "__x732__"
                                      (Printexc.to_string __x__)
                                      "Looking for key dbg"
                                  else ();
                                  raise
                                    (Rpc.Runtime_exception
                                       ("Looking for key dbg",
                                         (Printexc.to_string __x__))))
                           and __x734__ =
                             try List.assoc "id" __x732__
                             with
                             | __x__ ->
                                 (if Rpc.get_debug ()
                                  then
                                    Printf.eprintf
                                      "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                      "request" "__x732__"
                                      (Printexc.to_string __x__)
                                      "Looking for key id"
                                  else ();
                                  raise
                                    (Rpc.Runtime_exception
                                       ("Looking for key id",
                                         (Printexc.to_string __x__)))) in
                           {
                             dbg = (debug_info_of_rpc __x733__);
                             id = (Mirror.id_of_rpc __x734__)
                           }
                       | __x__ ->
                           (if Rpc.get_debug ()
                            then
                              Printf.eprintf
                                "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                "request" "__x731__" (Rpc.to_string __x__)
                                "Dict"
                            else ();
                            raise (Rpc.Runtime_error ("Dict", __x__))))
                type response = unit
                let rpc_of_response = function | __x730__ -> Rpc.Null
                let response_of_rpc =
                  function
                  | __x729__ ->
                      (match __x729__ with
                       | Rpc.Null -> ()
                       | __x__ ->
                           (if Rpc.get_debug ()
                            then
                              Printf.eprintf
                                "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                "response" "__x729__" (Rpc.to_string __x__)
                                "Null"
                            else ();
                            raise (Rpc.Runtime_error ("Null", __x__))))
                let call_of_stop ~dbg  ~id  =
                  let arg = { id; dbg } in
                  Rpc.call "DATA.MIRROR.stop" [rpc_of_request arg]
              end
            module Stat =
              struct
                type request = {
                  dbg: debug_info ;
                  id: Mirror.id }
                let rpc_of_request =
                  function
                  | __x744__ ->
                      let __x745__ = __x744__.dbg
                      and __x746__ = __x744__.id in
                      Rpc.Dict
                        [("dbg", (rpc_of_debug_info __x745__));
                        ("id", (Mirror.rpc_of_id __x746__))]
                let request_of_rpc =
                  function
                  | __x740__ ->
                      (match __x740__ with
                       | Rpc.Dict __x741__ ->
                           let __x742__ =
                             try List.assoc "dbg" __x741__
                             with
                             | __x__ ->
                                 (if Rpc.get_debug ()
                                  then
                                    Printf.eprintf
                                      "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                      "request" "__x741__"
                                      (Printexc.to_string __x__)
                                      "Looking for key dbg"
                                  else ();
                                  raise
                                    (Rpc.Runtime_exception
                                       ("Looking for key dbg",
                                         (Printexc.to_string __x__))))
                           and __x743__ =
                             try List.assoc "id" __x741__
                             with
                             | __x__ ->
                                 (if Rpc.get_debug ()
                                  then
                                    Printf.eprintf
                                      "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                      "request" "__x741__"
                                      (Printexc.to_string __x__)
                                      "Looking for key id"
                                  else ();
                                  raise
                                    (Rpc.Runtime_exception
                                       ("Looking for key id",
                                         (Printexc.to_string __x__)))) in
                           {
                             dbg = (debug_info_of_rpc __x742__);
                             id = (Mirror.id_of_rpc __x743__)
                           }
                       | __x__ ->
                           (if Rpc.get_debug ()
                            then
                              Printf.eprintf
                                "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                "request" "__x740__" (Rpc.to_string __x__)
                                "Dict"
                            else ();
                            raise (Rpc.Runtime_error ("Dict", __x__))))
                type response = Mirror.t
                let rpc_of_response =
                  function | __x739__ -> Mirror.rpc_of_t __x739__
                let response_of_rpc =
                  function | __x738__ -> Mirror.t_of_rpc __x738__
                let call_of_stat ~dbg  ~id  =
                  let arg = { id; dbg } in
                  Rpc.call "DATA.MIRROR.stat" [rpc_of_request arg]
              end
            module Receive_start =
              struct
                type request =
                  {
                  dbg: debug_info ;
                  sr: sr ;
                  vdi_info: vdi_info ;
                  id: Mirror.id ;
                  similar: Mirror.similars }
                let rpc_of_request =
                  function
                  | __x756__ ->
                      let __x757__ = __x756__.dbg
                      and __x758__ = __x756__.sr
                      and __x759__ = __x756__.vdi_info
                      and __x760__ = __x756__.id
                      and __x761__ = __x756__.similar in
                      Rpc.Dict
                        [("dbg", (rpc_of_debug_info __x757__));
                        ("sr", (rpc_of_sr __x758__));
                        ("vdi_info", (rpc_of_vdi_info __x759__));
                        ("id", (Mirror.rpc_of_id __x760__));
                        ("similar", (Mirror.rpc_of_similars __x761__))]
                let request_of_rpc =
                  function
                  | __x749__ ->
                      (match __x749__ with
                       | Rpc.Dict __x750__ ->
                           let __x751__ =
                             try List.assoc "dbg" __x750__
                             with
                             | __x__ ->
                                 (if Rpc.get_debug ()
                                  then
                                    Printf.eprintf
                                      "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                      "request" "__x750__"
                                      (Printexc.to_string __x__)
                                      "Looking for key dbg"
                                  else ();
                                  raise
                                    (Rpc.Runtime_exception
                                       ("Looking for key dbg",
                                         (Printexc.to_string __x__))))
                           and __x752__ =
                             try List.assoc "sr" __x750__
                             with
                             | __x__ ->
                                 (if Rpc.get_debug ()
                                  then
                                    Printf.eprintf
                                      "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                      "request" "__x750__"
                                      (Printexc.to_string __x__)
                                      "Looking for key sr"
                                  else ();
                                  raise
                                    (Rpc.Runtime_exception
                                       ("Looking for key sr",
                                         (Printexc.to_string __x__))))
                           and __x753__ =
                             try List.assoc "vdi_info" __x750__
                             with
                             | __x__ ->
                                 (if Rpc.get_debug ()
                                  then
                                    Printf.eprintf
                                      "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                      "request" "__x750__"
                                      (Printexc.to_string __x__)
                                      "Looking for key vdi_info"
                                  else ();
                                  raise
                                    (Rpc.Runtime_exception
                                       ("Looking for key vdi_info",
                                         (Printexc.to_string __x__))))
                           and __x754__ =
                             try List.assoc "id" __x750__
                             with
                             | __x__ ->
                                 (if Rpc.get_debug ()
                                  then
                                    Printf.eprintf
                                      "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                      "request" "__x750__"
                                      (Printexc.to_string __x__)
                                      "Looking for key id"
                                  else ();
                                  raise
                                    (Rpc.Runtime_exception
                                       ("Looking for key id",
                                         (Printexc.to_string __x__))))
                           and __x755__ =
                             try List.assoc "similar" __x750__
                             with
                             | __x__ ->
                                 (if Rpc.get_debug ()
                                  then
                                    Printf.eprintf
                                      "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                      "request" "__x750__"
                                      (Printexc.to_string __x__)
                                      "Looking for key similar"
                                  else ();
                                  raise
                                    (Rpc.Runtime_exception
                                       ("Looking for key similar",
                                         (Printexc.to_string __x__)))) in
                           {
                             dbg = (debug_info_of_rpc __x751__);
                             sr = (sr_of_rpc __x752__);
                             vdi_info = (vdi_info_of_rpc __x753__);
                             id = (Mirror.id_of_rpc __x754__);
                             similar = (Mirror.similars_of_rpc __x755__)
                           }
                       | __x__ ->
                           (if Rpc.get_debug ()
                            then
                              Printf.eprintf
                                "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                "request" "__x749__" (Rpc.to_string __x__)
                                "Dict"
                            else ();
                            raise (Rpc.Runtime_error ("Dict", __x__))))
                type response = Mirror.mirror_receive_result
                let rpc_of_response =
                  function
                  | __x748__ -> Mirror.rpc_of_mirror_receive_result __x748__
                let response_of_rpc =
                  function
                  | __x747__ -> Mirror.mirror_receive_result_of_rpc __x747__
                let call_of_receive_start ~dbg  ~sr  ~vdi_info  ~id  ~similar
                   =
                  let arg = { similar; id; vdi_info; sr; dbg } in
                  Rpc.call "DATA.MIRROR.receive_start" [rpc_of_request arg]
              end
            module Receive_finalize =
              struct
                type request = {
                  dbg: debug_info ;
                  id: Mirror.id }
                let rpc_of_request =
                  function
                  | __x768__ ->
                      let __x769__ = __x768__.dbg
                      and __x770__ = __x768__.id in
                      Rpc.Dict
                        [("dbg", (rpc_of_debug_info __x769__));
                        ("id", (Mirror.rpc_of_id __x770__))]
                let request_of_rpc =
                  function
                  | __x764__ ->
                      (match __x764__ with
                       | Rpc.Dict __x765__ ->
                           let __x766__ =
                             try List.assoc "dbg" __x765__
                             with
                             | __x__ ->
                                 (if Rpc.get_debug ()
                                  then
                                    Printf.eprintf
                                      "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                      "request" "__x765__"
                                      (Printexc.to_string __x__)
                                      "Looking for key dbg"
                                  else ();
                                  raise
                                    (Rpc.Runtime_exception
                                       ("Looking for key dbg",
                                         (Printexc.to_string __x__))))
                           and __x767__ =
                             try List.assoc "id" __x765__
                             with
                             | __x__ ->
                                 (if Rpc.get_debug ()
                                  then
                                    Printf.eprintf
                                      "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                      "request" "__x765__"
                                      (Printexc.to_string __x__)
                                      "Looking for key id"
                                  else ();
                                  raise
                                    (Rpc.Runtime_exception
                                       ("Looking for key id",
                                         (Printexc.to_string __x__)))) in
                           {
                             dbg = (debug_info_of_rpc __x766__);
                             id = (Mirror.id_of_rpc __x767__)
                           }
                       | __x__ ->
                           (if Rpc.get_debug ()
                            then
                              Printf.eprintf
                                "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                "request" "__x764__" (Rpc.to_string __x__)
                                "Dict"
                            else ();
                            raise (Rpc.Runtime_error ("Dict", __x__))))
                type response = unit
                let rpc_of_response = function | __x763__ -> Rpc.Null
                let response_of_rpc =
                  function
                  | __x762__ ->
                      (match __x762__ with
                       | Rpc.Null -> ()
                       | __x__ ->
                           (if Rpc.get_debug ()
                            then
                              Printf.eprintf
                                "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                "response" "__x762__" (Rpc.to_string __x__)
                                "Null"
                            else ();
                            raise (Rpc.Runtime_error ("Null", __x__))))
                let call_of_receive_finalize ~dbg  ~id  =
                  let arg = { id; dbg } in
                  Rpc.call "DATA.MIRROR.receive_finalize"
                    [rpc_of_request arg]
              end
            module Receive_cancel =
              struct
                type request = {
                  dbg: debug_info ;
                  id: Mirror.id }
                let rpc_of_request =
                  function
                  | __x777__ ->
                      let __x778__ = __x777__.dbg
                      and __x779__ = __x777__.id in
                      Rpc.Dict
                        [("dbg", (rpc_of_debug_info __x778__));
                        ("id", (Mirror.rpc_of_id __x779__))]
                let request_of_rpc =
                  function
                  | __x773__ ->
                      (match __x773__ with
                       | Rpc.Dict __x774__ ->
                           let __x775__ =
                             try List.assoc "dbg" __x774__
                             with
                             | __x__ ->
                                 (if Rpc.get_debug ()
                                  then
                                    Printf.eprintf
                                      "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                      "request" "__x774__"
                                      (Printexc.to_string __x__)
                                      "Looking for key dbg"
                                  else ();
                                  raise
                                    (Rpc.Runtime_exception
                                       ("Looking for key dbg",
                                         (Printexc.to_string __x__))))
                           and __x776__ =
                             try List.assoc "id" __x774__
                             with
                             | __x__ ->
                                 (if Rpc.get_debug ()
                                  then
                                    Printf.eprintf
                                      "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                      "request" "__x774__"
                                      (Printexc.to_string __x__)
                                      "Looking for key id"
                                  else ();
                                  raise
                                    (Rpc.Runtime_exception
                                       ("Looking for key id",
                                         (Printexc.to_string __x__)))) in
                           {
                             dbg = (debug_info_of_rpc __x775__);
                             id = (Mirror.id_of_rpc __x776__)
                           }
                       | __x__ ->
                           (if Rpc.get_debug ()
                            then
                              Printf.eprintf
                                "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                "request" "__x773__" (Rpc.to_string __x__)
                                "Dict"
                            else ();
                            raise (Rpc.Runtime_error ("Dict", __x__))))
                type response = unit
                let rpc_of_response = function | __x772__ -> Rpc.Null
                let response_of_rpc =
                  function
                  | __x771__ ->
                      (match __x771__ with
                       | Rpc.Null -> ()
                       | __x__ ->
                           (if Rpc.get_debug ()
                            then
                              Printf.eprintf
                                "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                "response" "__x771__" (Rpc.to_string __x__)
                                "Null"
                            else ();
                            raise (Rpc.Runtime_error ("Null", __x__))))
                let call_of_receive_cancel ~dbg  ~id  =
                  let arg = { id; dbg } in
                  Rpc.call "DATA.MIRROR.receive_cancel" [rpc_of_request arg]
              end
            module List =
              struct
                type request = {
                  dbg: debug_info }
                let rpc_of_request =
                  function
                  | __x792__ ->
                      let __x793__ = __x792__.dbg in
                      Rpc.Dict [("dbg", (rpc_of_debug_info __x793__))]
                let request_of_rpc =
                  function
                  | __x789__ ->
                      (match __x789__ with
                       | Rpc.Dict __x790__ ->
                           let __x791__ =
                             try List.assoc "dbg" __x790__
                             with
                             | __x__ ->
                                 (if Rpc.get_debug ()
                                  then
                                    Printf.eprintf
                                      "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                      "request" "__x790__"
                                      (Printexc.to_string __x__)
                                      "Looking for key dbg"
                                  else ();
                                  raise
                                    (Rpc.Runtime_exception
                                       ("Looking for key dbg",
                                         (Printexc.to_string __x__)))) in
                           { dbg = (debug_info_of_rpc __x791__) }
                       | __x__ ->
                           (if Rpc.get_debug ()
                            then
                              Printf.eprintf
                                "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                "request" "__x789__" (Rpc.to_string __x__)
                                "Dict"
                            else ();
                            raise (Rpc.Runtime_error ("Dict", __x__))))
                type response = (Mirror.id * Mirror.t) list
                let rpc_of_response =
                  function
                  | __x785__ ->
                      Rpc.Enum
                        (List.map
                           (function
                            | __x786__ ->
                                let (__x787__, __x788__) = __x786__ in
                                Rpc.Enum
                                  [Mirror.rpc_of_id __x787__;
                                  Mirror.rpc_of_t __x788__]) __x785__)
                let response_of_rpc =
                  function
                  | __x780__ ->
                      (match __x780__ with
                       | Rpc.Enum __x781__ ->
                           List.map
                             (function
                              | __x782__ ->
                                  (match __x782__ with
                                   | Rpc.Enum (__x783__::__x784__::[]) ->
                                       ((Mirror.id_of_rpc __x783__),
                                         (Mirror.t_of_rpc __x784__))
                                   | __x__ ->
                                       (if Rpc.get_debug ()
                                        then
                                          Printf.eprintf
                                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                            "response" "__x782__"
                                            (Rpc.to_string __x__) "List"
                                        else ();
                                        raise
                                          (Rpc.Runtime_error ("List", __x__)))))
                             __x781__
                       | __x__ ->
                           (if Rpc.get_debug ()
                            then
                              Printf.eprintf
                                "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                "response" "__x780__" (Rpc.to_string __x__)
                                "List"
                            else ();
                            raise (Rpc.Runtime_error ("List", __x__))))
                let call_of_list ~dbg  =
                  let arg = { dbg } in
                  Rpc.call "DATA.MIRROR.list" [rpc_of_request arg]
              end
          end
      end
    module Policy =
      struct
        module Get_backend_vm =
          struct
            type request = {
              dbg: debug_info ;
              vm: string ;
              sr: sr ;
              vdi: vdi }
            let rpc_of_request =
              function
              | __x802__ ->
                  let __x803__ = __x802__.dbg
                  and __x804__ = __x802__.vm
                  and __x805__ = __x802__.sr
                  and __x806__ = __x802__.vdi in
                  Rpc.Dict
                    [("dbg", (rpc_of_debug_info __x803__));
                    ("vm", (Rpc.String __x804__));
                    ("sr", (rpc_of_sr __x805__));
                    ("vdi", (rpc_of_vdi __x806__))]
            let request_of_rpc =
              function
              | __x796__ ->
                  (match __x796__ with
                   | Rpc.Dict __x797__ ->
                       let __x798__ =
                         try List.assoc "dbg" __x797__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x797__"
                                  (Printexc.to_string __x__)
                                  "Looking for key dbg"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key dbg",
                                     (Printexc.to_string __x__))))
                       and __x799__ =
                         try List.assoc "vm" __x797__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x797__"
                                  (Printexc.to_string __x__)
                                  "Looking for key vm"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key vm",
                                     (Printexc.to_string __x__))))
                       and __x800__ =
                         try List.assoc "sr" __x797__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x797__"
                                  (Printexc.to_string __x__)
                                  "Looking for key sr"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key sr",
                                     (Printexc.to_string __x__))))
                       and __x801__ =
                         try List.assoc "vdi" __x797__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x797__"
                                  (Printexc.to_string __x__)
                                  "Looking for key vdi"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key vdi",
                                     (Printexc.to_string __x__)))) in
                       {
                         dbg = (debug_info_of_rpc __x798__);
                         vm =
                           ((match __x799__ with
                             | Rpc.String x -> x
                             | __x__ ->
                                 (if Rpc.get_debug ()
                                  then
                                    Printf.eprintf
                                      "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                      "request" "__x799__"
                                      (Rpc.to_string __x__) "String(string)"
                                  else ();
                                  raise
                                    (Rpc.Runtime_error
                                       ("String(string)", __x__)))));
                         sr = (sr_of_rpc __x800__);
                         vdi = (vdi_of_rpc __x801__)
                       }
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "request" "__x796__" (Rpc.to_string __x__) "Dict"
                        else ();
                        raise (Rpc.Runtime_error ("Dict", __x__))))
            type response = string
            let rpc_of_response = function | __x795__ -> Rpc.String __x795__
            let response_of_rpc =
              function
              | __x794__ ->
                  (match __x794__ with
                   | Rpc.String x -> x
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "response" "__x794__" (Rpc.to_string __x__)
                            "String(string)"
                        else ();
                        raise (Rpc.Runtime_error ("String(string)", __x__))))
            let call_of_get_backend_vm ~dbg  ~vm  ~sr  ~vdi  =
              let arg = { vdi; sr; vm; dbg } in
              Rpc.call "Policy.get_backend_vm" [rpc_of_request arg]
          end
      end
    module TASK =
      struct
        module Stat =
          struct
            type request = {
              dbg: debug_info ;
              task: Task.id }
            let rpc_of_request =
              function
              | __x813__ ->
                  let __x814__ = __x813__.dbg
                  and __x815__ = __x813__.task in
                  Rpc.Dict
                    [("dbg", (rpc_of_debug_info __x814__));
                    ("task", (Task.rpc_of_id __x815__))]
            let request_of_rpc =
              function
              | __x809__ ->
                  (match __x809__ with
                   | Rpc.Dict __x810__ ->
                       let __x811__ =
                         try List.assoc "dbg" __x810__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x810__"
                                  (Printexc.to_string __x__)
                                  "Looking for key dbg"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key dbg",
                                     (Printexc.to_string __x__))))
                       and __x812__ =
                         try List.assoc "task" __x810__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x810__"
                                  (Printexc.to_string __x__)
                                  "Looking for key task"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key task",
                                     (Printexc.to_string __x__)))) in
                       {
                         dbg = (debug_info_of_rpc __x811__);
                         task = (Task.id_of_rpc __x812__)
                       }
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "request" "__x809__" (Rpc.to_string __x__) "Dict"
                        else ();
                        raise (Rpc.Runtime_error ("Dict", __x__))))
            type response = Task.t
            let rpc_of_response =
              function | __x808__ -> Task.rpc_of_t __x808__
            let response_of_rpc =
              function | __x807__ -> Task.t_of_rpc __x807__
            let call_of_stat ~dbg  ~task  =
              let arg = { task; dbg } in
              Rpc.call "TASK.stat" [rpc_of_request arg]
          end
        module Cancel =
          struct
            type request = {
              dbg: debug_info ;
              task: Task.id }
            let rpc_of_request =
              function
              | __x822__ ->
                  let __x823__ = __x822__.dbg
                  and __x824__ = __x822__.task in
                  Rpc.Dict
                    [("dbg", (rpc_of_debug_info __x823__));
                    ("task", (Task.rpc_of_id __x824__))]
            let request_of_rpc =
              function
              | __x818__ ->
                  (match __x818__ with
                   | Rpc.Dict __x819__ ->
                       let __x820__ =
                         try List.assoc "dbg" __x819__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x819__"
                                  (Printexc.to_string __x__)
                                  "Looking for key dbg"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key dbg",
                                     (Printexc.to_string __x__))))
                       and __x821__ =
                         try List.assoc "task" __x819__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x819__"
                                  (Printexc.to_string __x__)
                                  "Looking for key task"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key task",
                                     (Printexc.to_string __x__)))) in
                       {
                         dbg = (debug_info_of_rpc __x820__);
                         task = (Task.id_of_rpc __x821__)
                       }
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "request" "__x818__" (Rpc.to_string __x__) "Dict"
                        else ();
                        raise (Rpc.Runtime_error ("Dict", __x__))))
            type response = unit
            let rpc_of_response = function | __x817__ -> Rpc.Null
            let response_of_rpc =
              function
              | __x816__ ->
                  (match __x816__ with
                   | Rpc.Null -> ()
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "response" "__x816__" (Rpc.to_string __x__)
                            "Null"
                        else ();
                        raise (Rpc.Runtime_error ("Null", __x__))))
            let call_of_cancel ~dbg  ~task  =
              let arg = { task; dbg } in
              Rpc.call "TASK.cancel" [rpc_of_request arg]
          end
        module Destroy =
          struct
            type request = {
              dbg: debug_info ;
              task: Task.id }
            let rpc_of_request =
              function
              | __x831__ ->
                  let __x832__ = __x831__.dbg
                  and __x833__ = __x831__.task in
                  Rpc.Dict
                    [("dbg", (rpc_of_debug_info __x832__));
                    ("task", (Task.rpc_of_id __x833__))]
            let request_of_rpc =
              function
              | __x827__ ->
                  (match __x827__ with
                   | Rpc.Dict __x828__ ->
                       let __x829__ =
                         try List.assoc "dbg" __x828__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x828__"
                                  (Printexc.to_string __x__)
                                  "Looking for key dbg"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key dbg",
                                     (Printexc.to_string __x__))))
                       and __x830__ =
                         try List.assoc "task" __x828__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x828__"
                                  (Printexc.to_string __x__)
                                  "Looking for key task"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key task",
                                     (Printexc.to_string __x__)))) in
                       {
                         dbg = (debug_info_of_rpc __x829__);
                         task = (Task.id_of_rpc __x830__)
                       }
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "request" "__x827__" (Rpc.to_string __x__) "Dict"
                        else ();
                        raise (Rpc.Runtime_error ("Dict", __x__))))
            type response = unit
            let rpc_of_response = function | __x826__ -> Rpc.Null
            let response_of_rpc =
              function
              | __x825__ ->
                  (match __x825__ with
                   | Rpc.Null -> ()
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "response" "__x825__" (Rpc.to_string __x__)
                            "Null"
                        else ();
                        raise (Rpc.Runtime_error ("Null", __x__))))
            let call_of_destroy ~dbg  ~task  =
              let arg = { task; dbg } in
              Rpc.call "TASK.destroy" [rpc_of_request arg]
          end
        module List =
          struct
            type request = {
              dbg: debug_info }
            let rpc_of_request =
              function
              | __x842__ ->
                  let __x843__ = __x842__.dbg in
                  Rpc.Dict [("dbg", (rpc_of_debug_info __x843__))]
            let request_of_rpc =
              function
              | __x839__ ->
                  (match __x839__ with
                   | Rpc.Dict __x840__ ->
                       let __x841__ =
                         try List.assoc "dbg" __x840__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x840__"
                                  (Printexc.to_string __x__)
                                  "Looking for key dbg"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key dbg",
                                     (Printexc.to_string __x__)))) in
                       { dbg = (debug_info_of_rpc __x841__) }
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "request" "__x839__" (Rpc.to_string __x__) "Dict"
                        else ();
                        raise (Rpc.Runtime_error ("Dict", __x__))))
            type response = Task.t list
            let rpc_of_response =
              function
              | __x837__ ->
                  Rpc.Enum
                    (List.map (function | __x838__ -> Task.rpc_of_t __x838__)
                       __x837__)
            let response_of_rpc =
              function
              | __x834__ ->
                  (match __x834__ with
                   | Rpc.Enum __x835__ ->
                       List.map
                         (function | __x836__ -> Task.t_of_rpc __x836__)
                         __x835__
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "response" "__x834__" (Rpc.to_string __x__)
                            "List"
                        else ();
                        raise (Rpc.Runtime_error ("List", __x__))))
            let call_of_list ~dbg  =
              let arg = { dbg } in Rpc.call "TASK.list" [rpc_of_request arg]
          end
      end
    module UPDATES =
      struct
        module Get =
          struct
            type request =
              {
              dbg: debug_info ;
              from: string ;
              timeout: int option }
            let rpc_of_request =
              function
              | __x859__ ->
                  let __x860__ = __x859__.dbg
                  and __x861__ = __x859__.from
                  and __x862__ = __x859__.timeout in
                  Rpc.Dict (("dbg", (rpc_of_debug_info __x860__)) ::
                    ("from", (Rpc.String __x861__)) ::
                    ((match match __x862__ with
                            | Some __x864__ ->
                                Rpc.Enum [Rpc.Int (Int64.of_int __x864__)]
                            | None -> Rpc.Enum []
                      with
                      | Rpc.Enum [] -> []
                      | Rpc.Enum (__x863__::[]) -> [("timeout", __x863__)]
                      | _ -> assert false)))
            let request_of_rpc =
              function
              | __x853__ ->
                  (match __x853__ with
                   | Rpc.Dict __x854__ ->
                       let __x855__ =
                         try List.assoc "dbg" __x854__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x854__"
                                  (Printexc.to_string __x__)
                                  "Looking for key dbg"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key dbg",
                                     (Printexc.to_string __x__))))
                       and __x856__ =
                         try List.assoc "from" __x854__
                         with
                         | __x__ ->
                             (if Rpc.get_debug ()
                              then
                                Printf.eprintf
                                  "Runtime error in '%s_of_rpc:%s': caught exception '%s' while doing '%s'\\n"
                                  "request" "__x854__"
                                  (Printexc.to_string __x__)
                                  "Looking for key from"
                              else ();
                              raise
                                (Rpc.Runtime_exception
                                   ("Looking for key from",
                                     (Printexc.to_string __x__))))
                       and __x857__ =
                         if List.mem_assoc "timeout" __x854__
                         then Rpc.Enum [List.assoc "timeout" __x854__]
                         else Rpc.Enum [] in
                       {
                         dbg = (debug_info_of_rpc __x855__);
                         from =
                           ((match __x856__ with
                             | Rpc.String x -> x
                             | __x__ ->
                                 (if Rpc.get_debug ()
                                  then
                                    Printf.eprintf
                                      "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                      "request" "__x856__"
                                      (Rpc.to_string __x__) "String(string)"
                                  else ();
                                  raise
                                    (Rpc.Runtime_error
                                       ("String(string)", __x__)))));
                         timeout =
                           ((match __x857__ with
                             | Rpc.Enum [] -> None
                             | Rpc.Enum (__x858__::[]) ->
                                 Some
                                   ((match __x858__ with
                                     | Rpc.Int x -> Int64.to_int x
                                     | Rpc.String s -> int_of_string s
                                     | __x__ ->
                                         (if Rpc.get_debug ()
                                          then
                                            Printf.eprintf
                                              "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                              "request" "__x858__"
                                              (Rpc.to_string __x__)
                                              "Int(int)"
                                          else ();
                                          raise
                                            (Rpc.Runtime_error
                                               ("Int(int)", __x__)))))
                             | __x__ ->
                                 (if Rpc.get_debug ()
                                  then
                                    Printf.eprintf
                                      "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                      "request" "__x857__"
                                      (Rpc.to_string __x__) "Enum[]/Enum[_]"
                                  else ();
                                  raise
                                    (Rpc.Runtime_error
                                       ("Enum[]/Enum[_]", __x__)))))
                       }
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "request" "__x853__" (Rpc.to_string __x__) "Dict"
                        else ();
                        raise (Rpc.Runtime_error ("Dict", __x__))))
            type response = (Dynamic.id list * string)
            let rpc_of_response =
              function
              | __x849__ ->
                  let (__x850__, __x851__) = __x849__ in
                  Rpc.Enum
                    [Rpc.Enum
                       (List.map
                          (function | __x852__ -> Dynamic.rpc_of_id __x852__)
                          __x850__);
                    Rpc.String __x851__]
            let response_of_rpc =
              function
              | __x844__ ->
                  (match __x844__ with
                   | Rpc.Enum (__x845__::__x846__::[]) ->
                       (((match __x845__ with
                          | Rpc.Enum __x847__ ->
                              List.map
                                (function
                                 | __x848__ -> Dynamic.id_of_rpc __x848__)
                                __x847__
                          | __x__ ->
                              (if Rpc.get_debug ()
                               then
                                 Printf.eprintf
                                   "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                   "response" "__x845__"
                                   (Rpc.to_string __x__) "List"
                               else ();
                               raise (Rpc.Runtime_error ("List", __x__))))),
                         ((match __x846__ with
                           | Rpc.String x -> x
                           | __x__ ->
                               (if Rpc.get_debug ()
                                then
                                  Printf.eprintf
                                    "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                                    "response" "__x846__"
                                    (Rpc.to_string __x__) "String(string)"
                                else ();
                                raise
                                  (Rpc.Runtime_error
                                     ("String(string)", __x__))))))
                   | __x__ ->
                       (if Rpc.get_debug ()
                        then
                          Printf.eprintf
                            "Runtime error in '%s_of_rpc:%s': got '%s' when '%s' was expected\\n"
                            "response" "__x844__" (Rpc.to_string __x__)
                            "List"
                        else ();
                        raise (Rpc.Runtime_error ("List", __x__))))
            let call_of_get ~dbg  ~from  ~timeout  =
              let arg = { timeout; from; dbg } in
              Rpc.call "UPDATES.get" [rpc_of_request arg]
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
    module Query =
      struct
        let query ~dbg  =
          let call = Args.Query.Query.call_of_query ~dbg in
          R.bind (R.rpc call)
            (function
             | response ->
                 if response.Rpc.success
                 then
                   R.return
                     (Args.Query.Query.response_of_rpc response.Rpc.contents)
                 else
                   (let e =
                      exn_of_exnty
                        (Exception.exnty_of_rpc response.Rpc.contents) in
                    R.fail e))
        let diagnostics ~dbg  =
          let call = Args.Query.Diagnostics.call_of_diagnostics ~dbg in
          R.bind (R.rpc call)
            (function
             | response ->
                 if response.Rpc.success
                 then
                   R.return
                     (Args.Query.Diagnostics.response_of_rpc
                        response.Rpc.contents)
                 else
                   (let e =
                      exn_of_exnty
                        (Exception.exnty_of_rpc response.Rpc.contents) in
                    R.fail e))
      end
    module DP =
      struct
        let create ~dbg  ~id  =
          let call = Args.DP.Create.call_of_create ~dbg ~id in
          R.bind (R.rpc call)
            (function
             | response ->
                 if response.Rpc.success
                 then
                   R.return
                     (Args.DP.Create.response_of_rpc response.Rpc.contents)
                 else
                   (let e =
                      exn_of_exnty
                        (Exception.exnty_of_rpc response.Rpc.contents) in
                    R.fail e))
        let destroy ~dbg  ~dp  ~allow_leak  =
          let call = Args.DP.Destroy.call_of_destroy ~dbg ~dp ~allow_leak in
          R.bind (R.rpc call)
            (function
             | response ->
                 if response.Rpc.success
                 then
                   R.return
                     (Args.DP.Destroy.response_of_rpc response.Rpc.contents)
                 else
                   (let e =
                      exn_of_exnty
                        (Exception.exnty_of_rpc response.Rpc.contents) in
                    R.fail e))
        let attach_info ~dbg  ~sr  ~vdi  ~dp  =
          let call =
            Args.DP.Attach_info.call_of_attach_info ~dbg ~sr ~vdi ~dp in
          R.bind (R.rpc call)
            (function
             | response ->
                 if response.Rpc.success
                 then
                   R.return
                     (Args.DP.Attach_info.response_of_rpc
                        response.Rpc.contents)
                 else
                   (let e =
                      exn_of_exnty
                        (Exception.exnty_of_rpc response.Rpc.contents) in
                    R.fail e))
        let diagnostics =
          function
          | __x0__ ->
              let call = Args.DP.Diagnostics.call_of_diagnostics __x0__ in
              R.bind (R.rpc call)
                (function
                 | response ->
                     if response.Rpc.success
                     then
                       R.return
                         (Args.DP.Diagnostics.response_of_rpc
                            response.Rpc.contents)
                     else
                       (let e =
                          exn_of_exnty
                            (Exception.exnty_of_rpc response.Rpc.contents) in
                        R.fail e))
        let stat_vdi ~dbg  ~sr  ~vdi  =
          function
          | __x3__ ->
              let call =
                Args.DP.Stat_vdi.call_of_stat_vdi ~dbg ~sr ~vdi __x3__ in
              R.bind (R.rpc call)
                (function
                 | response ->
                     if response.Rpc.success
                     then
                       R.return
                         (Args.DP.Stat_vdi.response_of_rpc
                            response.Rpc.contents)
                     else
                       (let e =
                          exn_of_exnty
                            (Exception.exnty_of_rpc response.Rpc.contents) in
                        R.fail e))
      end
    module SR =
      struct
        let create ~dbg  ~sr  ~name_label  ~name_description  ~device_config 
          ~physical_size  =
          let call =
            Args.SR.Create.call_of_create ~dbg ~sr ~name_label
              ~name_description ~device_config ~physical_size in
          R.bind (R.rpc call)
            (function
             | response ->
                 if response.Rpc.success
                 then
                   R.return
                     (Args.SR.Create.response_of_rpc response.Rpc.contents)
                 else
                   (let e =
                      exn_of_exnty
                        (Exception.exnty_of_rpc response.Rpc.contents) in
                    R.fail e))
        let set_name_label ~dbg  ~sr  ~new_name_label  =
          let call =
            Args.SR.Set_name_label.call_of_set_name_label ~dbg ~sr
              ~new_name_label in
          R.bind (R.rpc call)
            (function
             | response ->
                 if response.Rpc.success
                 then
                   R.return
                     (Args.SR.Set_name_label.response_of_rpc
                        response.Rpc.contents)
                 else
                   (let e =
                      exn_of_exnty
                        (Exception.exnty_of_rpc response.Rpc.contents) in
                    R.fail e))
        let set_name_description ~dbg  ~sr  ~new_name_description  =
          let call =
            Args.SR.Set_name_description.call_of_set_name_description ~dbg
              ~sr ~new_name_description in
          R.bind (R.rpc call)
            (function
             | response ->
                 if response.Rpc.success
                 then
                   R.return
                     (Args.SR.Set_name_description.response_of_rpc
                        response.Rpc.contents)
                 else
                   (let e =
                      exn_of_exnty
                        (Exception.exnty_of_rpc response.Rpc.contents) in
                    R.fail e))
        let probe ~dbg  ~queue  ~device_config  ~sm_config  =
          let call =
            Args.SR.Probe.call_of_probe ~dbg ~queue ~device_config ~sm_config in
          R.bind (R.rpc call)
            (function
             | response ->
                 if response.Rpc.success
                 then
                   R.return
                     (Args.SR.Probe.response_of_rpc response.Rpc.contents)
                 else
                   (let e =
                      exn_of_exnty
                        (Exception.exnty_of_rpc response.Rpc.contents) in
                    R.fail e))
        let attach ~dbg  ~sr  ~device_config  =
          let call = Args.SR.Attach.call_of_attach ~dbg ~sr ~device_config in
          R.bind (R.rpc call)
            (function
             | response ->
                 if response.Rpc.success
                 then
                   R.return
                     (Args.SR.Attach.response_of_rpc response.Rpc.contents)
                 else
                   (let e =
                      exn_of_exnty
                        (Exception.exnty_of_rpc response.Rpc.contents) in
                    R.fail e))
        let detach ~dbg  ~sr  =
          let call = Args.SR.Detach.call_of_detach ~dbg ~sr in
          R.bind (R.rpc call)
            (function
             | response ->
                 if response.Rpc.success
                 then
                   R.return
                     (Args.SR.Detach.response_of_rpc response.Rpc.contents)
                 else
                   (let e =
                      exn_of_exnty
                        (Exception.exnty_of_rpc response.Rpc.contents) in
                    R.fail e))
        let reset ~dbg  ~sr  =
          let call = Args.SR.Reset.call_of_reset ~dbg ~sr in
          R.bind (R.rpc call)
            (function
             | response ->
                 if response.Rpc.success
                 then
                   R.return
                     (Args.SR.Reset.response_of_rpc response.Rpc.contents)
                 else
                   (let e =
                      exn_of_exnty
                        (Exception.exnty_of_rpc response.Rpc.contents) in
                    R.fail e))
        let destroy ~dbg  ~sr  =
          let call = Args.SR.Destroy.call_of_destroy ~dbg ~sr in
          R.bind (R.rpc call)
            (function
             | response ->
                 if response.Rpc.success
                 then
                   R.return
                     (Args.SR.Destroy.response_of_rpc response.Rpc.contents)
                 else
                   (let e =
                      exn_of_exnty
                        (Exception.exnty_of_rpc response.Rpc.contents) in
                    R.fail e))
        let scan ~dbg  ~sr  =
          let call = Args.SR.Scan.call_of_scan ~dbg ~sr in
          R.bind (R.rpc call)
            (function
             | response ->
                 if response.Rpc.success
                 then
                   R.return
                     (Args.SR.Scan.response_of_rpc response.Rpc.contents)
                 else
                   (let e =
                      exn_of_exnty
                        (Exception.exnty_of_rpc response.Rpc.contents) in
                    R.fail e))
        let update_snapshot_info_src ~dbg  ~sr  ~vdi  ~url  ~dest  ~dest_vdi 
          ~snapshot_pairs  =
          let call =
            Args.SR.Update_snapshot_info_src.call_of_update_snapshot_info_src
              ~dbg ~sr ~vdi ~url ~dest ~dest_vdi ~snapshot_pairs in
          R.bind (R.rpc call)
            (function
             | response ->
                 if response.Rpc.success
                 then
                   R.return
                     (Args.SR.Update_snapshot_info_src.response_of_rpc
                        response.Rpc.contents)
                 else
                   (let e =
                      exn_of_exnty
                        (Exception.exnty_of_rpc response.Rpc.contents) in
                    R.fail e))
        let update_snapshot_info_dest ~dbg  ~sr  ~vdi  ~src_vdi 
          ~snapshot_pairs  =
          let call =
            Args.SR.Update_snapshot_info_dest.call_of_update_snapshot_info_dest
              ~dbg ~sr ~vdi ~src_vdi ~snapshot_pairs in
          R.bind (R.rpc call)
            (function
             | response ->
                 if response.Rpc.success
                 then
                   R.return
                     (Args.SR.Update_snapshot_info_dest.response_of_rpc
                        response.Rpc.contents)
                 else
                   (let e =
                      exn_of_exnty
                        (Exception.exnty_of_rpc response.Rpc.contents) in
                    R.fail e))
        let stat ~dbg  ~sr  =
          let call = Args.SR.Stat.call_of_stat ~dbg ~sr in
          R.bind (R.rpc call)
            (function
             | response ->
                 if response.Rpc.success
                 then
                   R.return
                     (Args.SR.Stat.response_of_rpc response.Rpc.contents)
                 else
                   (let e =
                      exn_of_exnty
                        (Exception.exnty_of_rpc response.Rpc.contents) in
                    R.fail e))
        let list ~dbg  =
          let call = Args.SR.List.call_of_list ~dbg in
          R.bind (R.rpc call)
            (function
             | response ->
                 if response.Rpc.success
                 then
                   R.return
                     (Args.SR.List.response_of_rpc response.Rpc.contents)
                 else
                   (let e =
                      exn_of_exnty
                        (Exception.exnty_of_rpc response.Rpc.contents) in
                    R.fail e))
      end
    module VDI =
      struct
        let create ~dbg  ~sr  ~vdi_info  =
          let call = Args.VDI.Create.call_of_create ~dbg ~sr ~vdi_info in
          R.bind (R.rpc call)
            (function
             | response ->
                 if response.Rpc.success
                 then
                   R.return
                     (Args.VDI.Create.response_of_rpc response.Rpc.contents)
                 else
                   (let e =
                      exn_of_exnty
                        (Exception.exnty_of_rpc response.Rpc.contents) in
                    R.fail e))
        let set_name_label ~dbg  ~sr  ~vdi  ~new_name_label  =
          let call =
            Args.VDI.Set_name_label.call_of_set_name_label ~dbg ~sr ~vdi
              ~new_name_label in
          R.bind (R.rpc call)
            (function
             | response ->
                 if response.Rpc.success
                 then
                   R.return
                     (Args.VDI.Set_name_label.response_of_rpc
                        response.Rpc.contents)
                 else
                   (let e =
                      exn_of_exnty
                        (Exception.exnty_of_rpc response.Rpc.contents) in
                    R.fail e))
        let set_name_description ~dbg  ~sr  ~vdi  ~new_name_description  =
          let call =
            Args.VDI.Set_name_description.call_of_set_name_description ~dbg
              ~sr ~vdi ~new_name_description in
          R.bind (R.rpc call)
            (function
             | response ->
                 if response.Rpc.success
                 then
                   R.return
                     (Args.VDI.Set_name_description.response_of_rpc
                        response.Rpc.contents)
                 else
                   (let e =
                      exn_of_exnty
                        (Exception.exnty_of_rpc response.Rpc.contents) in
                    R.fail e))
        let snapshot ~dbg  ~sr  ~vdi_info  =
          let call = Args.VDI.Snapshot.call_of_snapshot ~dbg ~sr ~vdi_info in
          R.bind (R.rpc call)
            (function
             | response ->
                 if response.Rpc.success
                 then
                   R.return
                     (Args.VDI.Snapshot.response_of_rpc response.Rpc.contents)
                 else
                   (let e =
                      exn_of_exnty
                        (Exception.exnty_of_rpc response.Rpc.contents) in
                    R.fail e))
        let clone ~dbg  ~sr  ~vdi_info  =
          let call = Args.VDI.Clone.call_of_clone ~dbg ~sr ~vdi_info in
          R.bind (R.rpc call)
            (function
             | response ->
                 if response.Rpc.success
                 then
                   R.return
                     (Args.VDI.Clone.response_of_rpc response.Rpc.contents)
                 else
                   (let e =
                      exn_of_exnty
                        (Exception.exnty_of_rpc response.Rpc.contents) in
                    R.fail e))
        let resize ~dbg  ~sr  ~vdi  ~new_size  =
          let call = Args.VDI.Resize.call_of_resize ~dbg ~sr ~vdi ~new_size in
          R.bind (R.rpc call)
            (function
             | response ->
                 if response.Rpc.success
                 then
                   R.return
                     (Args.VDI.Resize.response_of_rpc response.Rpc.contents)
                 else
                   (let e =
                      exn_of_exnty
                        (Exception.exnty_of_rpc response.Rpc.contents) in
                    R.fail e))
        let destroy ~dbg  ~sr  ~vdi  =
          let call = Args.VDI.Destroy.call_of_destroy ~dbg ~sr ~vdi in
          R.bind (R.rpc call)
            (function
             | response ->
                 if response.Rpc.success
                 then
                   R.return
                     (Args.VDI.Destroy.response_of_rpc response.Rpc.contents)
                 else
                   (let e =
                      exn_of_exnty
                        (Exception.exnty_of_rpc response.Rpc.contents) in
                    R.fail e))
        let stat ~dbg  ~sr  ~vdi  =
          let call = Args.VDI.Stat.call_of_stat ~dbg ~sr ~vdi in
          R.bind (R.rpc call)
            (function
             | response ->
                 if response.Rpc.success
                 then
                   R.return
                     (Args.VDI.Stat.response_of_rpc response.Rpc.contents)
                 else
                   (let e =
                      exn_of_exnty
                        (Exception.exnty_of_rpc response.Rpc.contents) in
                    R.fail e))
        let introduce ~dbg  ~sr  ~uuid  ~sm_config  ~location  =
          let call =
            Args.VDI.Introduce.call_of_introduce ~dbg ~sr ~uuid ~sm_config
              ~location in
          R.bind (R.rpc call)
            (function
             | response ->
                 if response.Rpc.success
                 then
                   R.return
                     (Args.VDI.Introduce.response_of_rpc
                        response.Rpc.contents)
                 else
                   (let e =
                      exn_of_exnty
                        (Exception.exnty_of_rpc response.Rpc.contents) in
                    R.fail e))
        let set_persistent ~dbg  ~sr  ~vdi  ~persistent  =
          let call =
            Args.VDI.Set_persistent.call_of_set_persistent ~dbg ~sr ~vdi
              ~persistent in
          R.bind (R.rpc call)
            (function
             | response ->
                 if response.Rpc.success
                 then
                   R.return
                     (Args.VDI.Set_persistent.response_of_rpc
                        response.Rpc.contents)
                 else
                   (let e =
                      exn_of_exnty
                        (Exception.exnty_of_rpc response.Rpc.contents) in
                    R.fail e))
        let epoch_begin ~dbg  ~sr  ~vdi  ~persistent  =
          let call =
            Args.VDI.Epoch_begin.call_of_epoch_begin ~dbg ~sr ~vdi
              ~persistent in
          R.bind (R.rpc call)
            (function
             | response ->
                 if response.Rpc.success
                 then
                   R.return
                     (Args.VDI.Epoch_begin.response_of_rpc
                        response.Rpc.contents)
                 else
                   (let e =
                      exn_of_exnty
                        (Exception.exnty_of_rpc response.Rpc.contents) in
                    R.fail e))
        let attach ~dbg  ~dp  ~sr  ~vdi  ~read_write  =
          let call =
            Args.VDI.Attach.call_of_attach ~dbg ~dp ~sr ~vdi ~read_write in
          R.bind (R.rpc call)
            (function
             | response ->
                 if response.Rpc.success
                 then
                   R.return
                     (Args.VDI.Attach.response_of_rpc response.Rpc.contents)
                 else
                   (let e =
                      exn_of_exnty
                        (Exception.exnty_of_rpc response.Rpc.contents) in
                    R.fail e))
        let attach2 ~dbg  ~dp  ~sr  ~vdi  ~read_write  =
          let call =
            Args.VDI.Attach2.call_of_attach2 ~dbg ~dp ~sr ~vdi ~read_write in
          R.bind (R.rpc call)
            (function
             | response ->
                 if response.Rpc.success
                 then
                   R.return
                     (Args.VDI.Attach2.response_of_rpc response.Rpc.contents)
                 else
                   (let e =
                      exn_of_exnty
                        (Exception.exnty_of_rpc response.Rpc.contents) in
                    R.fail e))
        let activate ~dbg  ~dp  ~sr  ~vdi  =
          let call = Args.VDI.Activate.call_of_activate ~dbg ~dp ~sr ~vdi in
          R.bind (R.rpc call)
            (function
             | response ->
                 if response.Rpc.success
                 then
                   R.return
                     (Args.VDI.Activate.response_of_rpc response.Rpc.contents)
                 else
                   (let e =
                      exn_of_exnty
                        (Exception.exnty_of_rpc response.Rpc.contents) in
                    R.fail e))
        let deactivate ~dbg  ~dp  ~sr  ~vdi  =
          let call = Args.VDI.Deactivate.call_of_deactivate ~dbg ~dp ~sr ~vdi in
          R.bind (R.rpc call)
            (function
             | response ->
                 if response.Rpc.success
                 then
                   R.return
                     (Args.VDI.Deactivate.response_of_rpc
                        response.Rpc.contents)
                 else
                   (let e =
                      exn_of_exnty
                        (Exception.exnty_of_rpc response.Rpc.contents) in
                    R.fail e))
        let detach ~dbg  ~dp  ~sr  ~vdi  =
          let call = Args.VDI.Detach.call_of_detach ~dbg ~dp ~sr ~vdi in
          R.bind (R.rpc call)
            (function
             | response ->
                 if response.Rpc.success
                 then
                   R.return
                     (Args.VDI.Detach.response_of_rpc response.Rpc.contents)
                 else
                   (let e =
                      exn_of_exnty
                        (Exception.exnty_of_rpc response.Rpc.contents) in
                    R.fail e))
        let epoch_end ~dbg  ~sr  ~vdi  =
          let call = Args.VDI.Epoch_end.call_of_epoch_end ~dbg ~sr ~vdi in
          R.bind (R.rpc call)
            (function
             | response ->
                 if response.Rpc.success
                 then
                   R.return
                     (Args.VDI.Epoch_end.response_of_rpc
                        response.Rpc.contents)
                 else
                   (let e =
                      exn_of_exnty
                        (Exception.exnty_of_rpc response.Rpc.contents) in
                    R.fail e))
        let get_url ~dbg  ~sr  ~vdi  =
          let call = Args.VDI.Get_url.call_of_get_url ~dbg ~sr ~vdi in
          R.bind (R.rpc call)
            (function
             | response ->
                 if response.Rpc.success
                 then
                   R.return
                     (Args.VDI.Get_url.response_of_rpc response.Rpc.contents)
                 else
                   (let e =
                      exn_of_exnty
                        (Exception.exnty_of_rpc response.Rpc.contents) in
                    R.fail e))
        let similar_content ~dbg  ~sr  ~vdi  =
          let call =
            Args.VDI.Similar_content.call_of_similar_content ~dbg ~sr ~vdi in
          R.bind (R.rpc call)
            (function
             | response ->
                 if response.Rpc.success
                 then
                   R.return
                     (Args.VDI.Similar_content.response_of_rpc
                        response.Rpc.contents)
                 else
                   (let e =
                      exn_of_exnty
                        (Exception.exnty_of_rpc response.Rpc.contents) in
                    R.fail e))
        let get_by_name ~dbg  ~sr  ~name  =
          let call = Args.VDI.Get_by_name.call_of_get_by_name ~dbg ~sr ~name in
          R.bind (R.rpc call)
            (function
             | response ->
                 if response.Rpc.success
                 then
                   R.return
                     (Args.VDI.Get_by_name.response_of_rpc
                        response.Rpc.contents)
                 else
                   (let e =
                      exn_of_exnty
                        (Exception.exnty_of_rpc response.Rpc.contents) in
                    R.fail e))
        let set_content_id ~dbg  ~sr  ~vdi  ~content_id  =
          let call =
            Args.VDI.Set_content_id.call_of_set_content_id ~dbg ~sr ~vdi
              ~content_id in
          R.bind (R.rpc call)
            (function
             | response ->
                 if response.Rpc.success
                 then
                   R.return
                     (Args.VDI.Set_content_id.response_of_rpc
                        response.Rpc.contents)
                 else
                   (let e =
                      exn_of_exnty
                        (Exception.exnty_of_rpc response.Rpc.contents) in
                    R.fail e))
        let compose ~dbg  ~sr  ~vdi1  ~vdi2  =
          let call = Args.VDI.Compose.call_of_compose ~dbg ~sr ~vdi1 ~vdi2 in
          R.bind (R.rpc call)
            (function
             | response ->
                 if response.Rpc.success
                 then
                   R.return
                     (Args.VDI.Compose.response_of_rpc response.Rpc.contents)
                 else
                   (let e =
                      exn_of_exnty
                        (Exception.exnty_of_rpc response.Rpc.contents) in
                    R.fail e))
        let add_to_sm_config ~dbg  ~sr  ~vdi  ~key  ~value  =
          let call =
            Args.VDI.Add_to_sm_config.call_of_add_to_sm_config ~dbg ~sr ~vdi
              ~key ~value in
          R.bind (R.rpc call)
            (function
             | response ->
                 if response.Rpc.success
                 then
                   R.return
                     (Args.VDI.Add_to_sm_config.response_of_rpc
                        response.Rpc.contents)
                 else
                   (let e =
                      exn_of_exnty
                        (Exception.exnty_of_rpc response.Rpc.contents) in
                    R.fail e))
        let remove_from_sm_config ~dbg  ~sr  ~vdi  ~key  =
          let call =
            Args.VDI.Remove_from_sm_config.call_of_remove_from_sm_config ~dbg
              ~sr ~vdi ~key in
          R.bind (R.rpc call)
            (function
             | response ->
                 if response.Rpc.success
                 then
                   R.return
                     (Args.VDI.Remove_from_sm_config.response_of_rpc
                        response.Rpc.contents)
                 else
                   (let e =
                      exn_of_exnty
                        (Exception.exnty_of_rpc response.Rpc.contents) in
                    R.fail e))
        let enable_cbt ~dbg  ~sr  ~vdi  =
          let call = Args.VDI.Enable_cbt.call_of_enable_cbt ~dbg ~sr ~vdi in
          R.bind (R.rpc call)
            (function
             | response ->
                 if response.Rpc.success
                 then
                   R.return
                     (Args.VDI.Enable_cbt.response_of_rpc
                        response.Rpc.contents)
                 else
                   (let e =
                      exn_of_exnty
                        (Exception.exnty_of_rpc response.Rpc.contents) in
                    R.fail e))
        let disable_cbt ~dbg  ~sr  ~vdi  =
          let call = Args.VDI.Disable_cbt.call_of_disable_cbt ~dbg ~sr ~vdi in
          R.bind (R.rpc call)
            (function
             | response ->
                 if response.Rpc.success
                 then
                   R.return
                     (Args.VDI.Disable_cbt.response_of_rpc
                        response.Rpc.contents)
                 else
                   (let e =
                      exn_of_exnty
                        (Exception.exnty_of_rpc response.Rpc.contents) in
                    R.fail e))
        let data_destroy ~dbg  ~sr  ~vdi  =
          let call = Args.VDI.Data_destroy.call_of_data_destroy ~dbg ~sr ~vdi in
          R.bind (R.rpc call)
            (function
             | response ->
                 if response.Rpc.success
                 then
                   R.return
                     (Args.VDI.Data_destroy.response_of_rpc
                        response.Rpc.contents)
                 else
                   (let e =
                      exn_of_exnty
                        (Exception.exnty_of_rpc response.Rpc.contents) in
                    R.fail e))
        let list_changed_blocks ~dbg  ~sr  ~vdi_from  ~vdi_to  =
          let call =
            Args.VDI.List_changed_blocks.call_of_list_changed_blocks ~dbg ~sr
              ~vdi_from ~vdi_to in
          R.bind (R.rpc call)
            (function
             | response ->
                 if response.Rpc.success
                 then
                   R.return
                     (Args.VDI.List_changed_blocks.response_of_rpc
                        response.Rpc.contents)
                 else
                   (let e =
                      exn_of_exnty
                        (Exception.exnty_of_rpc response.Rpc.contents) in
                    R.fail e))
      end
    let get_by_name ~dbg  ~name  =
      let call = Args.Get_by_name.call_of_get_by_name ~dbg ~name in
      R.bind (R.rpc call)
        (function
         | response ->
             if response.Rpc.success
             then
               R.return
                 (Args.Get_by_name.response_of_rpc response.Rpc.contents)
             else
               (let e =
                  exn_of_exnty (Exception.exnty_of_rpc response.Rpc.contents) in
                R.fail e))
    module DATA =
      struct
        let copy_into ~dbg  ~sr  ~vdi  ~url  ~dest  ~dest_vdi  =
          let call =
            Args.DATA.Copy_into.call_of_copy_into ~dbg ~sr ~vdi ~url ~dest
              ~dest_vdi in
          R.bind (R.rpc call)
            (function
             | response ->
                 if response.Rpc.success
                 then
                   R.return
                     (Args.DATA.Copy_into.response_of_rpc
                        response.Rpc.contents)
                 else
                   (let e =
                      exn_of_exnty
                        (Exception.exnty_of_rpc response.Rpc.contents) in
                    R.fail e))
        let copy ~dbg  ~sr  ~vdi  ~dp  ~url  ~dest  =
          let call = Args.DATA.Copy.call_of_copy ~dbg ~sr ~vdi ~dp ~url ~dest in
          R.bind (R.rpc call)
            (function
             | response ->
                 if response.Rpc.success
                 then
                   R.return
                     (Args.DATA.Copy.response_of_rpc response.Rpc.contents)
                 else
                   (let e =
                      exn_of_exnty
                        (Exception.exnty_of_rpc response.Rpc.contents) in
                    R.fail e))
        module MIRROR =
          struct
            let start ~dbg  ~sr  ~vdi  ~dp  ~url  ~dest  =
              let call =
                Args.DATA.MIRROR.Start.call_of_start ~dbg ~sr ~vdi ~dp ~url
                  ~dest in
              R.bind (R.rpc call)
                (function
                 | response ->
                     if response.Rpc.success
                     then
                       R.return
                         (Args.DATA.MIRROR.Start.response_of_rpc
                            response.Rpc.contents)
                     else
                       (let e =
                          exn_of_exnty
                            (Exception.exnty_of_rpc response.Rpc.contents) in
                        R.fail e))
            let stop ~dbg  ~id  =
              let call = Args.DATA.MIRROR.Stop.call_of_stop ~dbg ~id in
              R.bind (R.rpc call)
                (function
                 | response ->
                     if response.Rpc.success
                     then
                       R.return
                         (Args.DATA.MIRROR.Stop.response_of_rpc
                            response.Rpc.contents)
                     else
                       (let e =
                          exn_of_exnty
                            (Exception.exnty_of_rpc response.Rpc.contents) in
                        R.fail e))
            let stat ~dbg  ~id  =
              let call = Args.DATA.MIRROR.Stat.call_of_stat ~dbg ~id in
              R.bind (R.rpc call)
                (function
                 | response ->
                     if response.Rpc.success
                     then
                       R.return
                         (Args.DATA.MIRROR.Stat.response_of_rpc
                            response.Rpc.contents)
                     else
                       (let e =
                          exn_of_exnty
                            (Exception.exnty_of_rpc response.Rpc.contents) in
                        R.fail e))
            let receive_start ~dbg  ~sr  ~vdi_info  ~id  ~similar  =
              let call =
                Args.DATA.MIRROR.Receive_start.call_of_receive_start ~dbg ~sr
                  ~vdi_info ~id ~similar in
              R.bind (R.rpc call)
                (function
                 | response ->
                     if response.Rpc.success
                     then
                       R.return
                         (Args.DATA.MIRROR.Receive_start.response_of_rpc
                            response.Rpc.contents)
                     else
                       (let e =
                          exn_of_exnty
                            (Exception.exnty_of_rpc response.Rpc.contents) in
                        R.fail e))
            let receive_finalize ~dbg  ~id  =
              let call =
                Args.DATA.MIRROR.Receive_finalize.call_of_receive_finalize
                  ~dbg ~id in
              R.bind (R.rpc call)
                (function
                 | response ->
                     if response.Rpc.success
                     then
                       R.return
                         (Args.DATA.MIRROR.Receive_finalize.response_of_rpc
                            response.Rpc.contents)
                     else
                       (let e =
                          exn_of_exnty
                            (Exception.exnty_of_rpc response.Rpc.contents) in
                        R.fail e))
            let receive_cancel ~dbg  ~id  =
              let call =
                Args.DATA.MIRROR.Receive_cancel.call_of_receive_cancel ~dbg
                  ~id in
              R.bind (R.rpc call)
                (function
                 | response ->
                     if response.Rpc.success
                     then
                       R.return
                         (Args.DATA.MIRROR.Receive_cancel.response_of_rpc
                            response.Rpc.contents)
                     else
                       (let e =
                          exn_of_exnty
                            (Exception.exnty_of_rpc response.Rpc.contents) in
                        R.fail e))
            let list ~dbg  =
              let call = Args.DATA.MIRROR.List.call_of_list ~dbg in
              R.bind (R.rpc call)
                (function
                 | response ->
                     if response.Rpc.success
                     then
                       R.return
                         (Args.DATA.MIRROR.List.response_of_rpc
                            response.Rpc.contents)
                     else
                       (let e =
                          exn_of_exnty
                            (Exception.exnty_of_rpc response.Rpc.contents) in
                        R.fail e))
          end
      end
    module Policy =
      struct
        let get_backend_vm ~dbg  ~vm  ~sr  ~vdi  =
          let call =
            Args.Policy.Get_backend_vm.call_of_get_backend_vm ~dbg ~vm ~sr
              ~vdi in
          R.bind (R.rpc call)
            (function
             | response ->
                 if response.Rpc.success
                 then
                   R.return
                     (Args.Policy.Get_backend_vm.response_of_rpc
                        response.Rpc.contents)
                 else
                   (let e =
                      exn_of_exnty
                        (Exception.exnty_of_rpc response.Rpc.contents) in
                    R.fail e))
      end
    module TASK =
      struct
        let stat ~dbg  ~task  =
          let call = Args.TASK.Stat.call_of_stat ~dbg ~task in
          R.bind (R.rpc call)
            (function
             | response ->
                 if response.Rpc.success
                 then
                   R.return
                     (Args.TASK.Stat.response_of_rpc response.Rpc.contents)
                 else
                   (let e =
                      exn_of_exnty
                        (Exception.exnty_of_rpc response.Rpc.contents) in
                    R.fail e))
        let cancel ~dbg  ~task  =
          let call = Args.TASK.Cancel.call_of_cancel ~dbg ~task in
          R.bind (R.rpc call)
            (function
             | response ->
                 if response.Rpc.success
                 then
                   R.return
                     (Args.TASK.Cancel.response_of_rpc response.Rpc.contents)
                 else
                   (let e =
                      exn_of_exnty
                        (Exception.exnty_of_rpc response.Rpc.contents) in
                    R.fail e))
        let destroy ~dbg  ~task  =
          let call = Args.TASK.Destroy.call_of_destroy ~dbg ~task in
          R.bind (R.rpc call)
            (function
             | response ->
                 if response.Rpc.success
                 then
                   R.return
                     (Args.TASK.Destroy.response_of_rpc response.Rpc.contents)
                 else
                   (let e =
                      exn_of_exnty
                        (Exception.exnty_of_rpc response.Rpc.contents) in
                    R.fail e))
        let list ~dbg  =
          let call = Args.TASK.List.call_of_list ~dbg in
          R.bind (R.rpc call)
            (function
             | response ->
                 if response.Rpc.success
                 then
                   R.return
                     (Args.TASK.List.response_of_rpc response.Rpc.contents)
                 else
                   (let e =
                      exn_of_exnty
                        (Exception.exnty_of_rpc response.Rpc.contents) in
                    R.fail e))
      end
    module UPDATES =
      struct
        let get ~dbg  ~from  ~timeout  =
          let call = Args.UPDATES.Get.call_of_get ~dbg ~from ~timeout in
          R.bind (R.rpc call)
            (function
             | response ->
                 if response.Rpc.success
                 then
                   R.return
                     (Args.UPDATES.Get.response_of_rpc response.Rpc.contents)
                 else
                   (let e =
                      exn_of_exnty
                        (Exception.exnty_of_rpc response.Rpc.contents) in
                    R.fail e))
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
    module Query :
    sig
      val query : context -> dbg:string -> query_result
      val diagnostics : context -> dbg:string -> string
    end
    module DP :
    sig
      val create : context -> dbg:debug_info -> id:string -> dp
      val destroy :
        context -> dbg:debug_info -> dp:dp -> allow_leak:bool -> unit
      val attach_info :
        context -> dbg:debug_info -> sr:sr -> vdi:vdi -> dp:dp -> backend
      val diagnostics : context -> unit -> string
      val stat_vdi :
        context -> dbg:debug_info -> sr:sr -> vdi:vdi -> unit -> dp_stat_t
    end
    module SR :
    sig
      val create :
        context ->
          dbg:debug_info ->
            sr:sr ->
              name_label:string ->
                name_description:string ->
                  device_config:(string * string) list ->
                    physical_size:int64 -> (string * string) list
      val set_name_label :
        context -> dbg:debug_info -> sr:sr -> new_name_label:string -> unit
      val set_name_description :
        context ->
          dbg:debug_info -> sr:sr -> new_name_description:string -> unit
      val probe :
        context ->
          dbg:debug_info ->
            queue:string ->
              device_config:(string * string) list ->
                sm_config:(string * string) list -> probe_result
      val attach :
        context ->
          dbg:debug_info ->
            sr:sr -> device_config:(string * string) list -> unit
      val detach : context -> dbg:debug_info -> sr:sr -> unit
      val reset : context -> dbg:debug_info -> sr:sr -> unit
      val destroy : context -> dbg:debug_info -> sr:sr -> unit
      val scan : context -> dbg:debug_info -> sr:sr -> vdi_info list
      val update_snapshot_info_src :
        context ->
          dbg:debug_info ->
            sr:sr ->
              vdi:vdi ->
                url:string ->
                  dest:sr ->
                    dest_vdi:vdi -> snapshot_pairs:(vdi * vdi) list -> unit
      val update_snapshot_info_dest :
        context ->
          dbg:debug_info ->
            sr:sr ->
              vdi:vdi ->
                src_vdi:vdi_info ->
                  snapshot_pairs:(vdi * vdi_info) list -> unit
      val stat : context -> dbg:debug_info -> sr:sr -> sr_info
      val list : context -> dbg:debug_info -> sr list
    end
    module VDI :
    sig
      val create :
        context -> dbg:debug_info -> sr:sr -> vdi_info:vdi_info -> vdi_info
      val set_name_label :
        context ->
          dbg:debug_info -> sr:sr -> vdi:vdi -> new_name_label:string -> unit
      val set_name_description :
        context ->
          dbg:debug_info ->
            sr:sr -> vdi:vdi -> new_name_description:string -> unit
      val snapshot :
        context -> dbg:debug_info -> sr:sr -> vdi_info:vdi_info -> vdi_info
      val clone :
        context -> dbg:debug_info -> sr:sr -> vdi_info:vdi_info -> vdi_info
      val resize :
        context ->
          dbg:debug_info -> sr:sr -> vdi:vdi -> new_size:int64 -> int64
      val destroy : context -> dbg:debug_info -> sr:sr -> vdi:vdi -> unit
      val stat : context -> dbg:debug_info -> sr:sr -> vdi:vdi -> vdi_info
      val introduce :
        context ->
          dbg:debug_info ->
            sr:sr ->
              uuid:string ->
                sm_config:(string * string) list ->
                  location:string -> vdi_info
      val set_persistent :
        context ->
          dbg:debug_info -> sr:sr -> vdi:vdi -> persistent:bool -> unit
      val epoch_begin :
        context ->
          dbg:debug_info -> sr:sr -> vdi:vdi -> persistent:bool -> unit
      val attach :
        context ->
          dbg:debug_info ->
            dp:dp -> sr:sr -> vdi:vdi -> read_write:bool -> attach_info
      val attach2 :
        context ->
          dbg:debug_info ->
            dp:dp -> sr:sr -> vdi:vdi -> read_write:bool -> backend
      val activate :
        context -> dbg:debug_info -> dp:dp -> sr:sr -> vdi:vdi -> unit
      val deactivate :
        context -> dbg:debug_info -> dp:dp -> sr:sr -> vdi:vdi -> unit
      val detach :
        context -> dbg:debug_info -> dp:dp -> sr:sr -> vdi:vdi -> unit
      val epoch_end : context -> dbg:debug_info -> sr:sr -> vdi:vdi -> unit
      val get_url : context -> dbg:debug_info -> sr:sr -> vdi:vdi -> string
      val similar_content :
        context -> dbg:debug_info -> sr:sr -> vdi:vdi -> vdi_info list
      val get_by_name :
        context -> dbg:debug_info -> sr:sr -> name:string -> vdi_info
      val set_content_id :
        context ->
          dbg:debug_info -> sr:sr -> vdi:vdi -> content_id:content_id -> unit
      val compose :
        context -> dbg:debug_info -> sr:sr -> vdi1:vdi -> vdi2:vdi -> unit
      val add_to_sm_config :
        context ->
          dbg:debug_info ->
            sr:sr -> vdi:vdi -> key:string -> value:string -> unit
      val remove_from_sm_config :
        context -> dbg:debug_info -> sr:sr -> vdi:vdi -> key:string -> unit
      val enable_cbt : context -> dbg:debug_info -> sr:sr -> vdi:vdi -> unit
      val disable_cbt : context -> dbg:debug_info -> sr:sr -> vdi:vdi -> unit
      val data_destroy :
        context -> dbg:debug_info -> sr:sr -> vdi:vdi -> unit
      val list_changed_blocks :
        context ->
          dbg:debug_info -> sr:sr -> vdi_from:vdi -> vdi_to:vdi -> string
    end
    val get_by_name :
      context -> dbg:debug_info -> name:string -> (sr * vdi_info)
    module DATA :
    sig
      val copy_into :
        context ->
          dbg:debug_info ->
            sr:sr ->
              vdi:vdi -> url:string -> dest:sr -> dest_vdi:vdi -> Task.id
      val copy :
        context ->
          dbg:debug_info ->
            sr:sr -> vdi:vdi -> dp:dp -> url:string -> dest:sr -> Task.id
      module MIRROR :
      sig
        val start :
          context ->
            dbg:debug_info ->
              sr:sr -> vdi:vdi -> dp:dp -> url:string -> dest:sr -> Task.id
        val stop : context -> dbg:debug_info -> id:Mirror.id -> unit
        val stat : context -> dbg:debug_info -> id:Mirror.id -> Mirror.t
        val receive_start :
          context ->
            dbg:debug_info ->
              sr:sr ->
                vdi_info:vdi_info ->
                  id:Mirror.id ->
                    similar:Mirror.similars -> Mirror.mirror_receive_result
        val receive_finalize :
          context -> dbg:debug_info -> id:Mirror.id -> unit
        val receive_cancel :
          context -> dbg:debug_info -> id:Mirror.id -> unit
        val list : context -> dbg:debug_info -> (Mirror.id * Mirror.t) list
      end
    end
    module Policy :
    sig
      val get_backend_vm :
        context -> dbg:debug_info -> vm:string -> sr:sr -> vdi:vdi -> string
    end
    module TASK :
    sig
      val stat : context -> dbg:debug_info -> task:Task.id -> Task.t
      val cancel : context -> dbg:debug_info -> task:Task.id -> unit
      val destroy : context -> dbg:debug_info -> task:Task.id -> unit
      val list : context -> dbg:debug_info -> Task.t list
    end
    module UPDATES :
    sig
      val get :
        context ->
          dbg:debug_info ->
            from:string -> timeout:int option -> (Dynamic.id list * string)
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
    module Query :
    sig
      val query : context -> dbg:string -> query_result t
      val diagnostics : context -> dbg:string -> string t
    end
    module DP :
    sig
      val create : context -> dbg:debug_info -> id:string -> dp t
      val destroy :
        context -> dbg:debug_info -> dp:dp -> allow_leak:bool -> unit t
      val attach_info :
        context -> dbg:debug_info -> sr:sr -> vdi:vdi -> dp:dp -> backend t
      val diagnostics : context -> unit -> string t
      val stat_vdi :
        context -> dbg:debug_info -> sr:sr -> vdi:vdi -> unit -> dp_stat_t t
    end
    module SR :
    sig
      val create :
        context ->
          dbg:debug_info ->
            sr:sr ->
              name_label:string ->
                name_description:string ->
                  device_config:(string * string) list ->
                    physical_size:int64 -> (string * string) list t
      val set_name_label :
        context -> dbg:debug_info -> sr:sr -> new_name_label:string -> unit t
      val set_name_description :
        context ->
          dbg:debug_info -> sr:sr -> new_name_description:string -> unit t
      val probe :
        context ->
          dbg:debug_info ->
            queue:string ->
              device_config:(string * string) list ->
                sm_config:(string * string) list -> probe_result t
      val attach :
        context ->
          dbg:debug_info ->
            sr:sr -> device_config:(string * string) list -> unit t
      val detach : context -> dbg:debug_info -> sr:sr -> unit t
      val reset : context -> dbg:debug_info -> sr:sr -> unit t
      val destroy : context -> dbg:debug_info -> sr:sr -> unit t
      val scan : context -> dbg:debug_info -> sr:sr -> vdi_info list t
      val update_snapshot_info_src :
        context ->
          dbg:debug_info ->
            sr:sr ->
              vdi:vdi ->
                url:string ->
                  dest:sr ->
                    dest_vdi:vdi -> snapshot_pairs:(vdi * vdi) list -> unit t
      val update_snapshot_info_dest :
        context ->
          dbg:debug_info ->
            sr:sr ->
              vdi:vdi ->
                src_vdi:vdi_info ->
                  snapshot_pairs:(vdi * vdi_info) list -> unit t
      val stat : context -> dbg:debug_info -> sr:sr -> sr_info t
      val list : context -> dbg:debug_info -> sr list t
    end
    module VDI :
    sig
      val create :
        context -> dbg:debug_info -> sr:sr -> vdi_info:vdi_info -> vdi_info t
      val set_name_label :
        context ->
          dbg:debug_info ->
            sr:sr -> vdi:vdi -> new_name_label:string -> unit t
      val set_name_description :
        context ->
          dbg:debug_info ->
            sr:sr -> vdi:vdi -> new_name_description:string -> unit t
      val snapshot :
        context -> dbg:debug_info -> sr:sr -> vdi_info:vdi_info -> vdi_info t
      val clone :
        context -> dbg:debug_info -> sr:sr -> vdi_info:vdi_info -> vdi_info t
      val resize :
        context ->
          dbg:debug_info -> sr:sr -> vdi:vdi -> new_size:int64 -> int64 t
      val destroy : context -> dbg:debug_info -> sr:sr -> vdi:vdi -> unit t
      val stat : context -> dbg:debug_info -> sr:sr -> vdi:vdi -> vdi_info t
      val introduce :
        context ->
          dbg:debug_info ->
            sr:sr ->
              uuid:string ->
                sm_config:(string * string) list ->
                  location:string -> vdi_info t
      val set_persistent :
        context ->
          dbg:debug_info -> sr:sr -> vdi:vdi -> persistent:bool -> unit t
      val epoch_begin :
        context ->
          dbg:debug_info -> sr:sr -> vdi:vdi -> persistent:bool -> unit t
      val attach :
        context ->
          dbg:debug_info ->
            dp:dp -> sr:sr -> vdi:vdi -> read_write:bool -> attach_info t
      val attach2 :
        context ->
          dbg:debug_info ->
            dp:dp -> sr:sr -> vdi:vdi -> read_write:bool -> backend t
      val activate :
        context -> dbg:debug_info -> dp:dp -> sr:sr -> vdi:vdi -> unit t
      val deactivate :
        context -> dbg:debug_info -> dp:dp -> sr:sr -> vdi:vdi -> unit t
      val detach :
        context -> dbg:debug_info -> dp:dp -> sr:sr -> vdi:vdi -> unit t
      val epoch_end : context -> dbg:debug_info -> sr:sr -> vdi:vdi -> unit t
      val get_url : context -> dbg:debug_info -> sr:sr -> vdi:vdi -> string t
      val similar_content :
        context -> dbg:debug_info -> sr:sr -> vdi:vdi -> vdi_info list t
      val get_by_name :
        context -> dbg:debug_info -> sr:sr -> name:string -> vdi_info t
      val set_content_id :
        context ->
          dbg:debug_info ->
            sr:sr -> vdi:vdi -> content_id:content_id -> unit t
      val compose :
        context -> dbg:debug_info -> sr:sr -> vdi1:vdi -> vdi2:vdi -> unit t
      val add_to_sm_config :
        context ->
          dbg:debug_info ->
            sr:sr -> vdi:vdi -> key:string -> value:string -> unit t
      val remove_from_sm_config :
        context -> dbg:debug_info -> sr:sr -> vdi:vdi -> key:string -> unit t
      val enable_cbt :
        context -> dbg:debug_info -> sr:sr -> vdi:vdi -> unit t
      val disable_cbt :
        context -> dbg:debug_info -> sr:sr -> vdi:vdi -> unit t
      val data_destroy :
        context -> dbg:debug_info -> sr:sr -> vdi:vdi -> unit t
      val list_changed_blocks :
        context ->
          dbg:debug_info -> sr:sr -> vdi_from:vdi -> vdi_to:vdi -> string t
    end
    val get_by_name :
      context -> dbg:debug_info -> name:string -> (sr * vdi_info) t
    module DATA :
    sig
      val copy_into :
        context ->
          dbg:debug_info ->
            sr:sr ->
              vdi:vdi -> url:string -> dest:sr -> dest_vdi:vdi -> Task.id t
      val copy :
        context ->
          dbg:debug_info ->
            sr:sr -> vdi:vdi -> dp:dp -> url:string -> dest:sr -> Task.id t
      module MIRROR :
      sig
        val start :
          context ->
            dbg:debug_info ->
              sr:sr -> vdi:vdi -> dp:dp -> url:string -> dest:sr -> Task.id t
        val stop : context -> dbg:debug_info -> id:Mirror.id -> unit t
        val stat : context -> dbg:debug_info -> id:Mirror.id -> Mirror.t t
        val receive_start :
          context ->
            dbg:debug_info ->
              sr:sr ->
                vdi_info:vdi_info ->
                  id:Mirror.id ->
                    similar:Mirror.similars -> Mirror.mirror_receive_result t
        val receive_finalize :
          context -> dbg:debug_info -> id:Mirror.id -> unit t
        val receive_cancel :
          context -> dbg:debug_info -> id:Mirror.id -> unit t
        val list : context -> dbg:debug_info -> (Mirror.id * Mirror.t) list t
      end
    end
    module Policy :
    sig
      val get_backend_vm :
        context ->
          dbg:debug_info -> vm:string -> sr:sr -> vdi:vdi -> string t
    end
    module TASK :
    sig
      val stat : context -> dbg:debug_info -> task:Task.id -> Task.t t
      val cancel : context -> dbg:debug_info -> task:Task.id -> unit t
      val destroy : context -> dbg:debug_info -> task:Task.id -> unit t
      val list : context -> dbg:debug_info -> Task.t list t
    end
    module UPDATES :
    sig
      val get :
        context ->
          dbg:debug_info ->
            from:string -> timeout:int option -> (Dynamic.id list * string) t
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
                        | ("Query.query", arg::[]) ->
                            let params = Args.Query.Query.request_of_rpc arg in
                            Impl.bind
                              (Impl.Query.query x
                                 ~dbg:(params.Args.Query.Query.dbg))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.Query.Query.rpc_of_response x))
                        | ("Query.query", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("Query.query", 1,
                                   (List.length call.Rpc.params)))
                        | ("Query.diagnostics", arg::[]) ->
                            let params =
                              Args.Query.Diagnostics.request_of_rpc arg in
                            Impl.bind
                              (Impl.Query.diagnostics x
                                 ~dbg:(params.Args.Query.Diagnostics.dbg))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.Query.Diagnostics.rpc_of_response
                                        x))
                        | ("Query.diagnostics", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("Query.diagnostics", 1,
                                   (List.length call.Rpc.params)))
                        | ("DP.create", arg::[]) ->
                            let params = Args.DP.Create.request_of_rpc arg in
                            Impl.bind
                              (Impl.DP.create x
                                 ~dbg:(params.Args.DP.Create.dbg)
                                 ~id:(params.Args.DP.Create.id))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.DP.Create.rpc_of_response x))
                        | ("DP.create", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("DP.create", 1,
                                   (List.length call.Rpc.params)))
                        | ("DP.destroy", arg::[]) ->
                            let params = Args.DP.Destroy.request_of_rpc arg in
                            Impl.bind
                              (Impl.DP.destroy x
                                 ~dbg:(params.Args.DP.Destroy.dbg)
                                 ~dp:(params.Args.DP.Destroy.dp)
                                 ~allow_leak:(params.Args.DP.Destroy.allow_leak))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.DP.Destroy.rpc_of_response x))
                        | ("DP.destroy", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("DP.destroy", 1,
                                   (List.length call.Rpc.params)))
                        | ("DP.attach_info", arg::[]) ->
                            let params =
                              Args.DP.Attach_info.request_of_rpc arg in
                            Impl.bind
                              (Impl.DP.attach_info x
                                 ~dbg:(params.Args.DP.Attach_info.dbg)
                                 ~sr:(params.Args.DP.Attach_info.sr)
                                 ~vdi:(params.Args.DP.Attach_info.vdi)
                                 ~dp:(params.Args.DP.Attach_info.dp))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.DP.Attach_info.rpc_of_response x))
                        | ("DP.attach_info", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("DP.attach_info", 1,
                                   (List.length call.Rpc.params)))
                        | ("DP.diagnostics", __x1__::[]) ->
                            Impl.bind
                              (Impl.DP.diagnostics x
                                 (Args.DP.Diagnostics.__x1___of_rpc __x1__))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.DP.Diagnostics.rpc_of_response x))
                        | ("DP.diagnostics", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("DP.diagnostics", 1,
                                   (List.length call.Rpc.params)))
                        | ("DP.stat_vdi", arg::__x4__::[]) ->
                            let params = Args.DP.Stat_vdi.request_of_rpc arg in
                            Impl.bind
                              (Impl.DP.stat_vdi x
                                 ~dbg:(params.Args.DP.Stat_vdi.dbg)
                                 ~sr:(params.Args.DP.Stat_vdi.sr)
                                 ~vdi:(params.Args.DP.Stat_vdi.vdi)
                                 (Args.DP.Stat_vdi.__x4___of_rpc __x4__))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.DP.Stat_vdi.rpc_of_response x))
                        | ("DP.stat_vdi", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("DP.stat_vdi", 2,
                                   (List.length call.Rpc.params)))
                        | ("SR.create", arg::[]) ->
                            let params = Args.SR.Create.request_of_rpc arg in
                            Impl.bind
                              (Impl.SR.create x
                                 ~dbg:(params.Args.SR.Create.dbg)
                                 ~sr:(params.Args.SR.Create.sr)
                                 ~name_label:(params.Args.SR.Create.name_label)
                                 ~name_description:(params.Args.SR.Create.name_description)
                                 ~device_config:(params.Args.SR.Create.device_config)
                                 ~physical_size:(params.Args.SR.Create.physical_size))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.SR.Create.rpc_of_response x))
                        | ("SR.create", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("SR.create", 1,
                                   (List.length call.Rpc.params)))
                        | ("SR.set_name_label", arg::[]) ->
                            let params =
                              Args.SR.Set_name_label.request_of_rpc arg in
                            Impl.bind
                              (Impl.SR.set_name_label x
                                 ~dbg:(params.Args.SR.Set_name_label.dbg)
                                 ~sr:(params.Args.SR.Set_name_label.sr)
                                 ~new_name_label:(params.Args.SR.Set_name_label.new_name_label))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.SR.Set_name_label.rpc_of_response
                                        x))
                        | ("SR.set_name_label", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("SR.set_name_label", 1,
                                   (List.length call.Rpc.params)))
                        | ("SR.set_name_description", arg::[]) ->
                            let params =
                              Args.SR.Set_name_description.request_of_rpc arg in
                            Impl.bind
                              (Impl.SR.set_name_description x
                                 ~dbg:(params.Args.SR.Set_name_description.dbg)
                                 ~sr:(params.Args.SR.Set_name_description.sr)
                                 ~new_name_description:(params.Args.SR.Set_name_description.new_name_description))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.SR.Set_name_description.rpc_of_response
                                        x))
                        | ("SR.set_name_description", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("SR.set_name_description", 1,
                                   (List.length call.Rpc.params)))
                        | ("SR.probe", arg::[]) ->
                            let params = Args.SR.Probe.request_of_rpc arg in
                            Impl.bind
                              (Impl.SR.probe x
                                 ~dbg:(params.Args.SR.Probe.dbg)
                                 ~queue:(params.Args.SR.Probe.queue)
                                 ~device_config:(params.Args.SR.Probe.device_config)
                                 ~sm_config:(params.Args.SR.Probe.sm_config))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.SR.Probe.rpc_of_response x))
                        | ("SR.probe", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("SR.probe", 1,
                                   (List.length call.Rpc.params)))
                        | ("SR.attach", arg::[]) ->
                            let params = Args.SR.Attach.request_of_rpc arg in
                            Impl.bind
                              (Impl.SR.attach x
                                 ~dbg:(params.Args.SR.Attach.dbg)
                                 ~sr:(params.Args.SR.Attach.sr)
                                 ~device_config:(params.Args.SR.Attach.device_config))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.SR.Attach.rpc_of_response x))
                        | ("SR.attach", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("SR.attach", 1,
                                   (List.length call.Rpc.params)))
                        | ("SR.detach", arg::[]) ->
                            let params = Args.SR.Detach.request_of_rpc arg in
                            Impl.bind
                              (Impl.SR.detach x
                                 ~dbg:(params.Args.SR.Detach.dbg)
                                 ~sr:(params.Args.SR.Detach.sr))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.SR.Detach.rpc_of_response x))
                        | ("SR.detach", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("SR.detach", 1,
                                   (List.length call.Rpc.params)))
                        | ("SR.reset", arg::[]) ->
                            let params = Args.SR.Reset.request_of_rpc arg in
                            Impl.bind
                              (Impl.SR.reset x
                                 ~dbg:(params.Args.SR.Reset.dbg)
                                 ~sr:(params.Args.SR.Reset.sr))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.SR.Reset.rpc_of_response x))
                        | ("SR.reset", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("SR.reset", 1,
                                   (List.length call.Rpc.params)))
                        | ("SR.destroy", arg::[]) ->
                            let params = Args.SR.Destroy.request_of_rpc arg in
                            Impl.bind
                              (Impl.SR.destroy x
                                 ~dbg:(params.Args.SR.Destroy.dbg)
                                 ~sr:(params.Args.SR.Destroy.sr))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.SR.Destroy.rpc_of_response x))
                        | ("SR.destroy", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("SR.destroy", 1,
                                   (List.length call.Rpc.params)))
                        | ("SR.scan", arg::[]) ->
                            let params = Args.SR.Scan.request_of_rpc arg in
                            Impl.bind
                              (Impl.SR.scan x ~dbg:(params.Args.SR.Scan.dbg)
                                 ~sr:(params.Args.SR.Scan.sr))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.SR.Scan.rpc_of_response x))
                        | ("SR.scan", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("SR.scan", 1,
                                   (List.length call.Rpc.params)))
                        | ("SR.update_snapshot_info_src", arg::[]) ->
                            let params =
                              Args.SR.Update_snapshot_info_src.request_of_rpc
                                arg in
                            Impl.bind
                              (Impl.SR.update_snapshot_info_src x
                                 ~dbg:(params.Args.SR.Update_snapshot_info_src.dbg)
                                 ~sr:(params.Args.SR.Update_snapshot_info_src.sr)
                                 ~vdi:(params.Args.SR.Update_snapshot_info_src.vdi)
                                 ~url:(params.Args.SR.Update_snapshot_info_src.url)
                                 ~dest:(params.Args.SR.Update_snapshot_info_src.dest)
                                 ~dest_vdi:(params.Args.SR.Update_snapshot_info_src.dest_vdi)
                                 ~snapshot_pairs:(params.Args.SR.Update_snapshot_info_src.snapshot_pairs))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.SR.Update_snapshot_info_src.rpc_of_response
                                        x))
                        | ("SR.update_snapshot_info_src", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("SR.update_snapshot_info_src", 1,
                                   (List.length call.Rpc.params)))
                        | ("SR.update_snapshot_info_dest", arg::[]) ->
                            let params =
                              Args.SR.Update_snapshot_info_dest.request_of_rpc
                                arg in
                            Impl.bind
                              (Impl.SR.update_snapshot_info_dest x
                                 ~dbg:(params.Args.SR.Update_snapshot_info_dest.dbg)
                                 ~sr:(params.Args.SR.Update_snapshot_info_dest.sr)
                                 ~vdi:(params.Args.SR.Update_snapshot_info_dest.vdi)
                                 ~src_vdi:(params.Args.SR.Update_snapshot_info_dest.src_vdi)
                                 ~snapshot_pairs:(params.Args.SR.Update_snapshot_info_dest.snapshot_pairs))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.SR.Update_snapshot_info_dest.rpc_of_response
                                        x))
                        | ("SR.update_snapshot_info_dest", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("SR.update_snapshot_info_dest", 1,
                                   (List.length call.Rpc.params)))
                        | ("SR.stat", arg::[]) ->
                            let params = Args.SR.Stat.request_of_rpc arg in
                            Impl.bind
                              (Impl.SR.stat x ~dbg:(params.Args.SR.Stat.dbg)
                                 ~sr:(params.Args.SR.Stat.sr))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.SR.Stat.rpc_of_response x))
                        | ("SR.stat", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("SR.stat", 1,
                                   (List.length call.Rpc.params)))
                        | ("SR.list", arg::[]) ->
                            let params = Args.SR.List.request_of_rpc arg in
                            Impl.bind
                              (Impl.SR.list x ~dbg:(params.Args.SR.List.dbg))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.SR.List.rpc_of_response x))
                        | ("SR.list", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("SR.list", 1,
                                   (List.length call.Rpc.params)))
                        | ("VDI.create", arg::[]) ->
                            let params = Args.VDI.Create.request_of_rpc arg in
                            Impl.bind
                              (Impl.VDI.create x
                                 ~dbg:(params.Args.VDI.Create.dbg)
                                 ~sr:(params.Args.VDI.Create.sr)
                                 ~vdi_info:(params.Args.VDI.Create.vdi_info))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.VDI.Create.rpc_of_response x))
                        | ("VDI.create", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("VDI.create", 1,
                                   (List.length call.Rpc.params)))
                        | ("VDI.set_name_label", arg::[]) ->
                            let params =
                              Args.VDI.Set_name_label.request_of_rpc arg in
                            Impl.bind
                              (Impl.VDI.set_name_label x
                                 ~dbg:(params.Args.VDI.Set_name_label.dbg)
                                 ~sr:(params.Args.VDI.Set_name_label.sr)
                                 ~vdi:(params.Args.VDI.Set_name_label.vdi)
                                 ~new_name_label:(params.Args.VDI.Set_name_label.new_name_label))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.VDI.Set_name_label.rpc_of_response
                                        x))
                        | ("VDI.set_name_label", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("VDI.set_name_label", 1,
                                   (List.length call.Rpc.params)))
                        | ("VDI.set_name_description", arg::[]) ->
                            let params =
                              Args.VDI.Set_name_description.request_of_rpc
                                arg in
                            Impl.bind
                              (Impl.VDI.set_name_description x
                                 ~dbg:(params.Args.VDI.Set_name_description.dbg)
                                 ~sr:(params.Args.VDI.Set_name_description.sr)
                                 ~vdi:(params.Args.VDI.Set_name_description.vdi)
                                 ~new_name_description:(params.Args.VDI.Set_name_description.new_name_description))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.VDI.Set_name_description.rpc_of_response
                                        x))
                        | ("VDI.set_name_description", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("VDI.set_name_description", 1,
                                   (List.length call.Rpc.params)))
                        | ("VDI.snapshot", arg::[]) ->
                            let params = Args.VDI.Snapshot.request_of_rpc arg in
                            Impl.bind
                              (Impl.VDI.snapshot x
                                 ~dbg:(params.Args.VDI.Snapshot.dbg)
                                 ~sr:(params.Args.VDI.Snapshot.sr)
                                 ~vdi_info:(params.Args.VDI.Snapshot.vdi_info))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.VDI.Snapshot.rpc_of_response x))
                        | ("VDI.snapshot", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("VDI.snapshot", 1,
                                   (List.length call.Rpc.params)))
                        | ("VDI.clone", arg::[]) ->
                            let params = Args.VDI.Clone.request_of_rpc arg in
                            Impl.bind
                              (Impl.VDI.clone x
                                 ~dbg:(params.Args.VDI.Clone.dbg)
                                 ~sr:(params.Args.VDI.Clone.sr)
                                 ~vdi_info:(params.Args.VDI.Clone.vdi_info))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.VDI.Clone.rpc_of_response x))
                        | ("VDI.clone", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("VDI.clone", 1,
                                   (List.length call.Rpc.params)))
                        | ("VDI.resize", arg::[]) ->
                            let params = Args.VDI.Resize.request_of_rpc arg in
                            Impl.bind
                              (Impl.VDI.resize x
                                 ~dbg:(params.Args.VDI.Resize.dbg)
                                 ~sr:(params.Args.VDI.Resize.sr)
                                 ~vdi:(params.Args.VDI.Resize.vdi)
                                 ~new_size:(params.Args.VDI.Resize.new_size))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.VDI.Resize.rpc_of_response x))
                        | ("VDI.resize", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("VDI.resize", 1,
                                   (List.length call.Rpc.params)))
                        | ("VDI.destroy", arg::[]) ->
                            let params = Args.VDI.Destroy.request_of_rpc arg in
                            Impl.bind
                              (Impl.VDI.destroy x
                                 ~dbg:(params.Args.VDI.Destroy.dbg)
                                 ~sr:(params.Args.VDI.Destroy.sr)
                                 ~vdi:(params.Args.VDI.Destroy.vdi))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.VDI.Destroy.rpc_of_response x))
                        | ("VDI.destroy", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("VDI.destroy", 1,
                                   (List.length call.Rpc.params)))
                        | ("VDI.stat", arg::[]) ->
                            let params = Args.VDI.Stat.request_of_rpc arg in
                            Impl.bind
                              (Impl.VDI.stat x
                                 ~dbg:(params.Args.VDI.Stat.dbg)
                                 ~sr:(params.Args.VDI.Stat.sr)
                                 ~vdi:(params.Args.VDI.Stat.vdi))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.VDI.Stat.rpc_of_response x))
                        | ("VDI.stat", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("VDI.stat", 1,
                                   (List.length call.Rpc.params)))
                        | ("VDI.introduce", arg::[]) ->
                            let params =
                              Args.VDI.Introduce.request_of_rpc arg in
                            Impl.bind
                              (Impl.VDI.introduce x
                                 ~dbg:(params.Args.VDI.Introduce.dbg)
                                 ~sr:(params.Args.VDI.Introduce.sr)
                                 ~uuid:(params.Args.VDI.Introduce.uuid)
                                 ~sm_config:(params.Args.VDI.Introduce.sm_config)
                                 ~location:(params.Args.VDI.Introduce.location))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.VDI.Introduce.rpc_of_response x))
                        | ("VDI.introduce", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("VDI.introduce", 1,
                                   (List.length call.Rpc.params)))
                        | ("VDI.set_persistent", arg::[]) ->
                            let params =
                              Args.VDI.Set_persistent.request_of_rpc arg in
                            Impl.bind
                              (Impl.VDI.set_persistent x
                                 ~dbg:(params.Args.VDI.Set_persistent.dbg)
                                 ~sr:(params.Args.VDI.Set_persistent.sr)
                                 ~vdi:(params.Args.VDI.Set_persistent.vdi)
                                 ~persistent:(params.Args.VDI.Set_persistent.persistent))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.VDI.Set_persistent.rpc_of_response
                                        x))
                        | ("VDI.set_persistent", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("VDI.set_persistent", 1,
                                   (List.length call.Rpc.params)))
                        | ("VDI.epoch_begin", arg::[]) ->
                            let params =
                              Args.VDI.Epoch_begin.request_of_rpc arg in
                            Impl.bind
                              (Impl.VDI.epoch_begin x
                                 ~dbg:(params.Args.VDI.Epoch_begin.dbg)
                                 ~sr:(params.Args.VDI.Epoch_begin.sr)
                                 ~vdi:(params.Args.VDI.Epoch_begin.vdi)
                                 ~persistent:(params.Args.VDI.Epoch_begin.persistent))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.VDI.Epoch_begin.rpc_of_response x))
                        | ("VDI.epoch_begin", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("VDI.epoch_begin", 1,
                                   (List.length call.Rpc.params)))
                        | ("VDI.attach", arg::[]) ->
                            let params = Args.VDI.Attach.request_of_rpc arg in
                            Impl.bind
                              (Impl.VDI.attach x
                                 ~dbg:(params.Args.VDI.Attach.dbg)
                                 ~dp:(params.Args.VDI.Attach.dp)
                                 ~sr:(params.Args.VDI.Attach.sr)
                                 ~vdi:(params.Args.VDI.Attach.vdi)
                                 ~read_write:(params.Args.VDI.Attach.read_write))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.VDI.Attach.rpc_of_response x))
                        | ("VDI.attach", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("VDI.attach", 1,
                                   (List.length call.Rpc.params)))
                        | ("VDI.attach2", arg::[]) ->
                            let params = Args.VDI.Attach2.request_of_rpc arg in
                            Impl.bind
                              (Impl.VDI.attach2 x
                                 ~dbg:(params.Args.VDI.Attach2.dbg)
                                 ~dp:(params.Args.VDI.Attach2.dp)
                                 ~sr:(params.Args.VDI.Attach2.sr)
                                 ~vdi:(params.Args.VDI.Attach2.vdi)
                                 ~read_write:(params.Args.VDI.Attach2.read_write))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.VDI.Attach2.rpc_of_response x))
                        | ("VDI.attach2", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("VDI.attach2", 1,
                                   (List.length call.Rpc.params)))
                        | ("VDI.activate", arg::[]) ->
                            let params = Args.VDI.Activate.request_of_rpc arg in
                            Impl.bind
                              (Impl.VDI.activate x
                                 ~dbg:(params.Args.VDI.Activate.dbg)
                                 ~dp:(params.Args.VDI.Activate.dp)
                                 ~sr:(params.Args.VDI.Activate.sr)
                                 ~vdi:(params.Args.VDI.Activate.vdi))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.VDI.Activate.rpc_of_response x))
                        | ("VDI.activate", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("VDI.activate", 1,
                                   (List.length call.Rpc.params)))
                        | ("VDI.deactivate", arg::[]) ->
                            let params =
                              Args.VDI.Deactivate.request_of_rpc arg in
                            Impl.bind
                              (Impl.VDI.deactivate x
                                 ~dbg:(params.Args.VDI.Deactivate.dbg)
                                 ~dp:(params.Args.VDI.Deactivate.dp)
                                 ~sr:(params.Args.VDI.Deactivate.sr)
                                 ~vdi:(params.Args.VDI.Deactivate.vdi))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.VDI.Deactivate.rpc_of_response x))
                        | ("VDI.deactivate", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("VDI.deactivate", 1,
                                   (List.length call.Rpc.params)))
                        | ("VDI.detach", arg::[]) ->
                            let params = Args.VDI.Detach.request_of_rpc arg in
                            Impl.bind
                              (Impl.VDI.detach x
                                 ~dbg:(params.Args.VDI.Detach.dbg)
                                 ~dp:(params.Args.VDI.Detach.dp)
                                 ~sr:(params.Args.VDI.Detach.sr)
                                 ~vdi:(params.Args.VDI.Detach.vdi))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.VDI.Detach.rpc_of_response x))
                        | ("VDI.detach", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("VDI.detach", 1,
                                   (List.length call.Rpc.params)))
                        | ("VDI.epoch_end", arg::[]) ->
                            let params =
                              Args.VDI.Epoch_end.request_of_rpc arg in
                            Impl.bind
                              (Impl.VDI.epoch_end x
                                 ~dbg:(params.Args.VDI.Epoch_end.dbg)
                                 ~sr:(params.Args.VDI.Epoch_end.sr)
                                 ~vdi:(params.Args.VDI.Epoch_end.vdi))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.VDI.Epoch_end.rpc_of_response x))
                        | ("VDI.epoch_end", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("VDI.epoch_end", 1,
                                   (List.length call.Rpc.params)))
                        | ("VDI.get_url", arg::[]) ->
                            let params = Args.VDI.Get_url.request_of_rpc arg in
                            Impl.bind
                              (Impl.VDI.get_url x
                                 ~dbg:(params.Args.VDI.Get_url.dbg)
                                 ~sr:(params.Args.VDI.Get_url.sr)
                                 ~vdi:(params.Args.VDI.Get_url.vdi))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.VDI.Get_url.rpc_of_response x))
                        | ("VDI.get_url", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("VDI.get_url", 1,
                                   (List.length call.Rpc.params)))
                        | ("VDI.similar_content", arg::[]) ->
                            let params =
                              Args.VDI.Similar_content.request_of_rpc arg in
                            Impl.bind
                              (Impl.VDI.similar_content x
                                 ~dbg:(params.Args.VDI.Similar_content.dbg)
                                 ~sr:(params.Args.VDI.Similar_content.sr)
                                 ~vdi:(params.Args.VDI.Similar_content.vdi))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.VDI.Similar_content.rpc_of_response
                                        x))
                        | ("VDI.similar_content", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("VDI.similar_content", 1,
                                   (List.length call.Rpc.params)))
                        | ("VDI.get_by_name", arg::[]) ->
                            let params =
                              Args.VDI.Get_by_name.request_of_rpc arg in
                            Impl.bind
                              (Impl.VDI.get_by_name x
                                 ~dbg:(params.Args.VDI.Get_by_name.dbg)
                                 ~sr:(params.Args.VDI.Get_by_name.sr)
                                 ~name:(params.Args.VDI.Get_by_name.name))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.VDI.Get_by_name.rpc_of_response x))
                        | ("VDI.get_by_name", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("VDI.get_by_name", 1,
                                   (List.length call.Rpc.params)))
                        | ("VDI.set_content_id", arg::[]) ->
                            let params =
                              Args.VDI.Set_content_id.request_of_rpc arg in
                            Impl.bind
                              (Impl.VDI.set_content_id x
                                 ~dbg:(params.Args.VDI.Set_content_id.dbg)
                                 ~sr:(params.Args.VDI.Set_content_id.sr)
                                 ~vdi:(params.Args.VDI.Set_content_id.vdi)
                                 ~content_id:(params.Args.VDI.Set_content_id.content_id))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.VDI.Set_content_id.rpc_of_response
                                        x))
                        | ("VDI.set_content_id", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("VDI.set_content_id", 1,
                                   (List.length call.Rpc.params)))
                        | ("VDI.compose", arg::[]) ->
                            let params = Args.VDI.Compose.request_of_rpc arg in
                            Impl.bind
                              (Impl.VDI.compose x
                                 ~dbg:(params.Args.VDI.Compose.dbg)
                                 ~sr:(params.Args.VDI.Compose.sr)
                                 ~vdi1:(params.Args.VDI.Compose.vdi1)
                                 ~vdi2:(params.Args.VDI.Compose.vdi2))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.VDI.Compose.rpc_of_response x))
                        | ("VDI.compose", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("VDI.compose", 1,
                                   (List.length call.Rpc.params)))
                        | ("VDI.add_to_sm_config", arg::[]) ->
                            let params =
                              Args.VDI.Add_to_sm_config.request_of_rpc arg in
                            Impl.bind
                              (Impl.VDI.add_to_sm_config x
                                 ~dbg:(params.Args.VDI.Add_to_sm_config.dbg)
                                 ~sr:(params.Args.VDI.Add_to_sm_config.sr)
                                 ~vdi:(params.Args.VDI.Add_to_sm_config.vdi)
                                 ~key:(params.Args.VDI.Add_to_sm_config.key)
                                 ~value:(params.Args.VDI.Add_to_sm_config.value))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.VDI.Add_to_sm_config.rpc_of_response
                                        x))
                        | ("VDI.add_to_sm_config", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("VDI.add_to_sm_config", 1,
                                   (List.length call.Rpc.params)))
                        | ("VDI.remove_from_sm_config", arg::[]) ->
                            let params =
                              Args.VDI.Remove_from_sm_config.request_of_rpc
                                arg in
                            Impl.bind
                              (Impl.VDI.remove_from_sm_config x
                                 ~dbg:(params.Args.VDI.Remove_from_sm_config.dbg)
                                 ~sr:(params.Args.VDI.Remove_from_sm_config.sr)
                                 ~vdi:(params.Args.VDI.Remove_from_sm_config.vdi)
                                 ~key:(params.Args.VDI.Remove_from_sm_config.key))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.VDI.Remove_from_sm_config.rpc_of_response
                                        x))
                        | ("VDI.remove_from_sm_config", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("VDI.remove_from_sm_config", 1,
                                   (List.length call.Rpc.params)))
                        | ("VDI.enable_cbt", arg::[]) ->
                            let params =
                              Args.VDI.Enable_cbt.request_of_rpc arg in
                            Impl.bind
                              (Impl.VDI.enable_cbt x
                                 ~dbg:(params.Args.VDI.Enable_cbt.dbg)
                                 ~sr:(params.Args.VDI.Enable_cbt.sr)
                                 ~vdi:(params.Args.VDI.Enable_cbt.vdi))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.VDI.Enable_cbt.rpc_of_response x))
                        | ("VDI.enable_cbt", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("VDI.enable_cbt", 1,
                                   (List.length call.Rpc.params)))
                        | ("VDI.disable_cbt", arg::[]) ->
                            let params =
                              Args.VDI.Disable_cbt.request_of_rpc arg in
                            Impl.bind
                              (Impl.VDI.disable_cbt x
                                 ~dbg:(params.Args.VDI.Disable_cbt.dbg)
                                 ~sr:(params.Args.VDI.Disable_cbt.sr)
                                 ~vdi:(params.Args.VDI.Disable_cbt.vdi))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.VDI.Disable_cbt.rpc_of_response x))
                        | ("VDI.disable_cbt", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("VDI.disable_cbt", 1,
                                   (List.length call.Rpc.params)))
                        | ("VDI.data_destroy", arg::[]) ->
                            let params =
                              Args.VDI.Data_destroy.request_of_rpc arg in
                            Impl.bind
                              (Impl.VDI.data_destroy x
                                 ~dbg:(params.Args.VDI.Data_destroy.dbg)
                                 ~sr:(params.Args.VDI.Data_destroy.sr)
                                 ~vdi:(params.Args.VDI.Data_destroy.vdi))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.VDI.Data_destroy.rpc_of_response x))
                        | ("VDI.data_destroy", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("VDI.data_destroy", 1,
                                   (List.length call.Rpc.params)))
                        | ("VDI.list_changed_blocks", arg::[]) ->
                            let params =
                              Args.VDI.List_changed_blocks.request_of_rpc arg in
                            Impl.bind
                              (Impl.VDI.list_changed_blocks x
                                 ~dbg:(params.Args.VDI.List_changed_blocks.dbg)
                                 ~sr:(params.Args.VDI.List_changed_blocks.sr)
                                 ~vdi_from:(params.Args.VDI.List_changed_blocks.vdi_from)
                                 ~vdi_to:(params.Args.VDI.List_changed_blocks.vdi_to))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.VDI.List_changed_blocks.rpc_of_response
                                        x))
                        | ("VDI.list_changed_blocks", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("VDI.list_changed_blocks", 1,
                                   (List.length call.Rpc.params)))
                        | ("get_by_name", arg::[]) ->
                            let params = Args.Get_by_name.request_of_rpc arg in
                            Impl.bind
                              (Impl.get_by_name x
                                 ~dbg:(params.Args.Get_by_name.dbg)
                                 ~name:(params.Args.Get_by_name.name))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.Get_by_name.rpc_of_response x))
                        | ("get_by_name", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("get_by_name", 1,
                                   (List.length call.Rpc.params)))
                        | ("DATA.copy_into", arg::[]) ->
                            let params =
                              Args.DATA.Copy_into.request_of_rpc arg in
                            Impl.bind
                              (Impl.DATA.copy_into x
                                 ~dbg:(params.Args.DATA.Copy_into.dbg)
                                 ~sr:(params.Args.DATA.Copy_into.sr)
                                 ~vdi:(params.Args.DATA.Copy_into.vdi)
                                 ~url:(params.Args.DATA.Copy_into.url)
                                 ~dest:(params.Args.DATA.Copy_into.dest)
                                 ~dest_vdi:(params.Args.DATA.Copy_into.dest_vdi))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.DATA.Copy_into.rpc_of_response x))
                        | ("DATA.copy_into", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("DATA.copy_into", 1,
                                   (List.length call.Rpc.params)))
                        | ("DATA.copy", arg::[]) ->
                            let params = Args.DATA.Copy.request_of_rpc arg in
                            Impl.bind
                              (Impl.DATA.copy x
                                 ~dbg:(params.Args.DATA.Copy.dbg)
                                 ~sr:(params.Args.DATA.Copy.sr)
                                 ~vdi:(params.Args.DATA.Copy.vdi)
                                 ~dp:(params.Args.DATA.Copy.dp)
                                 ~url:(params.Args.DATA.Copy.url)
                                 ~dest:(params.Args.DATA.Copy.dest))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.DATA.Copy.rpc_of_response x))
                        | ("DATA.copy", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("DATA.copy", 1,
                                   (List.length call.Rpc.params)))
                        | ("DATA.MIRROR.start", arg::[]) ->
                            let params =
                              Args.DATA.MIRROR.Start.request_of_rpc arg in
                            Impl.bind
                              (Impl.DATA.MIRROR.start x
                                 ~dbg:(params.Args.DATA.MIRROR.Start.dbg)
                                 ~sr:(params.Args.DATA.MIRROR.Start.sr)
                                 ~vdi:(params.Args.DATA.MIRROR.Start.vdi)
                                 ~dp:(params.Args.DATA.MIRROR.Start.dp)
                                 ~url:(params.Args.DATA.MIRROR.Start.url)
                                 ~dest:(params.Args.DATA.MIRROR.Start.dest))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.DATA.MIRROR.Start.rpc_of_response
                                        x))
                        | ("DATA.MIRROR.start", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("DATA.MIRROR.start", 1,
                                   (List.length call.Rpc.params)))
                        | ("DATA.MIRROR.stop", arg::[]) ->
                            let params =
                              Args.DATA.MIRROR.Stop.request_of_rpc arg in
                            Impl.bind
                              (Impl.DATA.MIRROR.stop x
                                 ~dbg:(params.Args.DATA.MIRROR.Stop.dbg)
                                 ~id:(params.Args.DATA.MIRROR.Stop.id))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.DATA.MIRROR.Stop.rpc_of_response x))
                        | ("DATA.MIRROR.stop", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("DATA.MIRROR.stop", 1,
                                   (List.length call.Rpc.params)))
                        | ("DATA.MIRROR.stat", arg::[]) ->
                            let params =
                              Args.DATA.MIRROR.Stat.request_of_rpc arg in
                            Impl.bind
                              (Impl.DATA.MIRROR.stat x
                                 ~dbg:(params.Args.DATA.MIRROR.Stat.dbg)
                                 ~id:(params.Args.DATA.MIRROR.Stat.id))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.DATA.MIRROR.Stat.rpc_of_response x))
                        | ("DATA.MIRROR.stat", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("DATA.MIRROR.stat", 1,
                                   (List.length call.Rpc.params)))
                        | ("DATA.MIRROR.receive_start", arg::[]) ->
                            let params =
                              Args.DATA.MIRROR.Receive_start.request_of_rpc
                                arg in
                            Impl.bind
                              (Impl.DATA.MIRROR.receive_start x
                                 ~dbg:(params.Args.DATA.MIRROR.Receive_start.dbg)
                                 ~sr:(params.Args.DATA.MIRROR.Receive_start.sr)
                                 ~vdi_info:(params.Args.DATA.MIRROR.Receive_start.vdi_info)
                                 ~id:(params.Args.DATA.MIRROR.Receive_start.id)
                                 ~similar:(params.Args.DATA.MIRROR.Receive_start.similar))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.DATA.MIRROR.Receive_start.rpc_of_response
                                        x))
                        | ("DATA.MIRROR.receive_start", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("DATA.MIRROR.receive_start", 1,
                                   (List.length call.Rpc.params)))
                        | ("DATA.MIRROR.receive_finalize", arg::[]) ->
                            let params =
                              Args.DATA.MIRROR.Receive_finalize.request_of_rpc
                                arg in
                            Impl.bind
                              (Impl.DATA.MIRROR.receive_finalize x
                                 ~dbg:(params.Args.DATA.MIRROR.Receive_finalize.dbg)
                                 ~id:(params.Args.DATA.MIRROR.Receive_finalize.id))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.DATA.MIRROR.Receive_finalize.rpc_of_response
                                        x))
                        | ("DATA.MIRROR.receive_finalize", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("DATA.MIRROR.receive_finalize", 1,
                                   (List.length call.Rpc.params)))
                        | ("DATA.MIRROR.receive_cancel", arg::[]) ->
                            let params =
                              Args.DATA.MIRROR.Receive_cancel.request_of_rpc
                                arg in
                            Impl.bind
                              (Impl.DATA.MIRROR.receive_cancel x
                                 ~dbg:(params.Args.DATA.MIRROR.Receive_cancel.dbg)
                                 ~id:(params.Args.DATA.MIRROR.Receive_cancel.id))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.DATA.MIRROR.Receive_cancel.rpc_of_response
                                        x))
                        | ("DATA.MIRROR.receive_cancel", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("DATA.MIRROR.receive_cancel", 1,
                                   (List.length call.Rpc.params)))
                        | ("DATA.MIRROR.list", arg::[]) ->
                            let params =
                              Args.DATA.MIRROR.List.request_of_rpc arg in
                            Impl.bind
                              (Impl.DATA.MIRROR.list x
                                 ~dbg:(params.Args.DATA.MIRROR.List.dbg))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.DATA.MIRROR.List.rpc_of_response x))
                        | ("DATA.MIRROR.list", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("DATA.MIRROR.list", 1,
                                   (List.length call.Rpc.params)))
                        | ("Policy.get_backend_vm", arg::[]) ->
                            let params =
                              Args.Policy.Get_backend_vm.request_of_rpc arg in
                            Impl.bind
                              (Impl.Policy.get_backend_vm x
                                 ~dbg:(params.Args.Policy.Get_backend_vm.dbg)
                                 ~vm:(params.Args.Policy.Get_backend_vm.vm)
                                 ~sr:(params.Args.Policy.Get_backend_vm.sr)
                                 ~vdi:(params.Args.Policy.Get_backend_vm.vdi))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.Policy.Get_backend_vm.rpc_of_response
                                        x))
                        | ("Policy.get_backend_vm", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("Policy.get_backend_vm", 1,
                                   (List.length call.Rpc.params)))
                        | ("TASK.stat", arg::[]) ->
                            let params = Args.TASK.Stat.request_of_rpc arg in
                            Impl.bind
                              (Impl.TASK.stat x
                                 ~dbg:(params.Args.TASK.Stat.dbg)
                                 ~task:(params.Args.TASK.Stat.task))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.TASK.Stat.rpc_of_response x))
                        | ("TASK.stat", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("TASK.stat", 1,
                                   (List.length call.Rpc.params)))
                        | ("TASK.cancel", arg::[]) ->
                            let params = Args.TASK.Cancel.request_of_rpc arg in
                            Impl.bind
                              (Impl.TASK.cancel x
                                 ~dbg:(params.Args.TASK.Cancel.dbg)
                                 ~task:(params.Args.TASK.Cancel.task))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.TASK.Cancel.rpc_of_response x))
                        | ("TASK.cancel", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("TASK.cancel", 1,
                                   (List.length call.Rpc.params)))
                        | ("TASK.destroy", arg::[]) ->
                            let params = Args.TASK.Destroy.request_of_rpc arg in
                            Impl.bind
                              (Impl.TASK.destroy x
                                 ~dbg:(params.Args.TASK.Destroy.dbg)
                                 ~task:(params.Args.TASK.Destroy.task))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.TASK.Destroy.rpc_of_response x))
                        | ("TASK.destroy", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("TASK.destroy", 1,
                                   (List.length call.Rpc.params)))
                        | ("TASK.list", arg::[]) ->
                            let params = Args.TASK.List.request_of_rpc arg in
                            Impl.bind
                              (Impl.TASK.list x
                                 ~dbg:(params.Args.TASK.List.dbg))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.TASK.List.rpc_of_response x))
                        | ("TASK.list", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("TASK.list", 1,
                                   (List.length call.Rpc.params)))
                        | ("UPDATES.get", arg::[]) ->
                            let params = Args.UPDATES.Get.request_of_rpc arg in
                            Impl.bind
                              (Impl.UPDATES.get x
                                 ~dbg:(params.Args.UPDATES.Get.dbg)
                                 ~from:(params.Args.UPDATES.Get.from)
                                 ~timeout:(params.Args.UPDATES.Get.timeout))
                              (function
                               | x ->
                                   Impl.return
                                     (Args.UPDATES.Get.rpc_of_response x))
                        | ("UPDATES.get", _) ->
                            Impl.fail
                              (Message_param_count_mismatch
                                 ("UPDATES.get", 1,
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
