module Unixext = Xapi_stdext_unix.Unixext

module D = Debug.Make (struct let name = "cert_distrib" end)

module XenAPI = Client.Client

type 'a remote_call =
  API.ref_host -> (Rpc.call -> Rpc.response) -> API.ref_session -> 'a

module WireProtocol = struct
  module Sexp = Sexplib.Sexp
  open Sexplib0.Sexp_conv

  (** [conflict_resolution] is used to determine how to treat existing
      certs in /etc/stunnel/certs-pool
      Erase_old => existing certs in the trusted certs dir will be removed
      Merge     => merge incoming certs with certs in the trusted certs dir,
                   resolving conflicts by taking the incoming cert *)
  type conflict_resolution = Erase_old | Merge [@@deriving sexp]

  type certificate = HostPoolCertificate | ApplianceCertificate
  [@@deriving sexp]

  type certificate_file = {filename: string; content: string} [@@deriving sexp]

  type command =
    | CollectOne of certificate * string
    | CollectMany of certificate * string list
    | Write of (certificate * conflict_resolution * certificate_file list)
    | GenBundle
  [@@deriving sexp]

  type result =
    | CollectOneResult of certificate_file
    | CollectManyResult of certificate_file list
    | WriteResult
    | GenBundleResult
  [@@deriving sexp]

  let string_of_conflict_resolution x =
    x |> sexp_of_conflict_resolution |> Sexp.to_string

  let string_of_certificate x = x |> sexp_of_certificate |> Sexp.to_string

  let string_of_command x = x |> sexp_of_command |> Sexp.to_string

  let command_of_string x =
    try Some (x |> Sexp.of_string |> command_of_sexp) with _ -> None

  let dbg_of_command = function
    | CollectOne (typ, _) ->
        Printf.sprintf "CollectOne (%s)" (string_of_certificate typ)
    | CollectMany (typ, _) ->
        Printf.sprintf "CollectMany (%s)" (string_of_certificate typ)
    | Write (typ, resolution, _) ->
        Printf.sprintf "Write (%s, %s)"
          (string_of_certificate typ)
          (string_of_conflict_resolution resolution)
    | GenBundle ->
        "GenBundle"

  let string_of_result x = x |> sexp_of_result |> Sexp.to_string

  let result_of_string x =
    try Some (x |> Sexp.of_string |> result_of_sexp) with _ -> None

  let dbg_of_result = function
    | CollectOneResult _ ->
        "CollectResult"
    | CollectManyResult _ ->
        "CollectManyResult"
    | WriteResult ->
        "WriteResult"
    | GenBundleResult ->
        "GenBundleResult"

  let pair_of_certificate_file {filename; content} = (filename, content)

  let certificate_file_of_pair (filename, content) = {filename; content}
end

let raise_internal ?e ?details msg : 'a =
  let e =
    Option.fold ~none:""
      ~some:(fun e -> e |> Printexc.to_string |> Printf.sprintf "exception: %s")
      e
  in
  let details = Option.value ~default:"" details in
  [msg; details; e] |> String.concat ". " |> D.error "%s" ;
  raise Api_errors.(Server_error (internal_error, [msg]))

module type CertificateProvider = sig
  val store_path : string

  val certificate_of_id_content :
    string -> string -> WireProtocol.certificate_file

  val read_certificate : string -> WireProtocol.certificate_file
end

module HostPoolProvider = struct
  let certificate_path = !Xapi_globs.server_cert_internal_path

  let store_path = !Xapi_globs.trusted_pool_certs_dir

  let certificate_of_id_content uuid content =
    WireProtocol.{filename= uuid ^ ".pem"; content}

  let read_certificate uuid =
    let open Gencertlib.Pem in
    match parse_file certificate_path with
    | Error reason ->
        raise_internal
          ~details:(Printf.sprintf "reason: %s" reason)
          (Printf.sprintf "failed to parse %s" certificate_path)
    | Ok {host_cert; _} ->
        certificate_of_id_content uuid host_cert
end

module ApplianceProvider = struct
  let store_path = !Xapi_globs.trusted_certs_dir

  let certificate_of_id_content filename content =
    WireProtocol.{filename; content}

  let read_certificate filename =
    let content =
      Unixext.read_lines (Filename.concat store_path filename)
      |> String.concat "\n"
    in
    certificate_of_id_content filename content
end

let provider_of_certificate (typ : WireProtocol.certificate) :
    (module CertificateProvider) =
  match typ with
  | HostPoolCertificate ->
      (module HostPoolProvider : CertificateProvider)
  | ApplianceCertificate ->
      (module ApplianceProvider : CertificateProvider)

(* eventually the remote calls should probably become API calls in the datamodel
   but they remain here for quick development *)
module Worker : sig
  val local_exec : __context:Context.t -> command:string -> string

  val remote_collect_cert :
       WireProtocol.certificate
    -> string
    -> WireProtocol.certificate_file remote_call

  val remote_collect_certs :
       WireProtocol.certificate
    -> string list
    -> WireProtocol.certificate_file list remote_call

  val remote_write_certs_fs :
       WireProtocol.certificate
    -> WireProtocol.conflict_resolution
    -> WireProtocol.certificate_file list
    -> unit remote_call

  val remote_regen_bundle : unit remote_call

  val local_collect_certs :
       __context:Context.t
    -> WireProtocol.certificate
    -> string list
    -> WireProtocol.certificate_file list

  val local_write_cert_fs :
       __context:Context.t
    -> WireProtocol.certificate
    -> WireProtocol.conflict_resolution
    -> WireProtocol.certificate_file list
    -> unit

  val local_regen_bundle : __context:Context.t -> unit
end = struct
  open WireProtocol

  let read_cert typ id =
    let module P = (val provider_of_certificate typ : CertificateProvider) in
    P.read_certificate id

  let write_certs_fs typ strategy certs =
    let open Helpers.FileSys in
    let module P = (val provider_of_certificate typ : CertificateProvider) in
    let pool_certs = P.store_path in
    let pool_certs_bk = Printf.sprintf "%s.bk" pool_certs in
    let mv_or_cp = match strategy with Erase_old -> mv | Merge -> cpr in
    ( try
        Unixext.mkdir_rec pool_certs_bk 0o700 ;
        mv_or_cp ~src:pool_certs ~dest:pool_certs_bk
      with e ->
        D.debug
          "write_certs_fs: ignoring failure, mv_or_cp %s to %s. exception: %s"
          pool_certs pool_certs_bk (Printexc.to_string e)
    ) ;
    ( try
        Unixext.mkdir_rec pool_certs 0o700 ;
        certs
        |> List.iter (function {filename; content} ->
               let fname = Filename.concat P.store_path filename in
               redirect content ~fname
               )
      with e ->
        (* on fail, try reset to previous cert state *)
        ( try mv ~src:pool_certs_bk ~dest:pool_certs
          with e ->
            D.debug
              "write_certs_fs: ignoring failure when trying to restore cert \
               state, mv %s to %s. exception: %s"
              pool_certs_bk pool_certs (Printexc.to_string e)
        ) ;
        raise_internal ~e "write_certs_fs: failed to write certs"
    ) ;
    try rmrf pool_certs_bk
    with e ->
      D.debug "write_certs_fs: ignoring failed to remove %s. exception: %s"
        pool_certs_bk (Printexc.to_string e)

  let regen_bundle () = Helpers.update_ca_bundle ()

  let restart_stunnel ~__context =
    Xapi_mgmt_iface.reconfigure_stunnel ~__context

  let with_log prefix f =
    D.debug "%s: start" prefix ;
    let res = f () in
    D.debug "%s: end" prefix ; res

  let local_exec ~__context ~command =
    with_log "Worker.local_exec" @@ fun () ->
    let p =
      match command_of_string command with
      | None ->
          raise_internal
            ~details:(Printf.sprintf "command string: %s" command)
            "local_exec: failed to parse command"
      | Some x ->
          x
    in
    D.debug "Worker.local_exec: command is '%s'" (dbg_of_command p) ;
    let r =
      match p with
      | CollectOne (typ, id) ->
          CollectOneResult (read_cert typ id)
      | CollectMany (typ, ids) ->
          CollectManyResult (List.map (read_cert typ) ids)
      | Write (typ, strategy, certs) ->
          write_certs_fs typ strategy certs ;
          WriteResult
      | GenBundle ->
          regen_bundle () ; GenBundleResult
    in
    string_of_result r

  let result_or_fail x =
    result_of_string x |> function
    | Some x ->
        x
    | None ->
        raise_internal
          ~details:(Printf.sprintf "result string: %s" x)
          "result_or_fail: failed to parse result"

  let remote_call_sync host command rpc session_id =
    XenAPI.Host.cert_distrib_atom rpc session_id host (string_of_command command)
    |> result_or_fail

  let unexpected_result name r =
    raise_internal
      ~details:(Printf.sprintf "r = %s" (dbg_of_result r))
      (Printf.sprintf "%s: unexpected result" name)

  let remote_collect_cert typ id host rpc session_id =
    remote_call_sync host (CollectOne (typ, id)) rpc session_id |> function
    | CollectOneResult cert ->
        cert
    | r ->
        unexpected_result "remote_collect_cert" r

  let remote_collect_certs typ ids host rpc session_id =
    remote_call_sync host (CollectMany (typ, ids)) rpc session_id |> function
    | CollectManyResult certs ->
        certs
    | r ->
        unexpected_result "remote_collect_certs" r

  let remote_write_certs_fs typ strategy certs host rpc session_id =
    remote_call_sync host (Write (typ, strategy, certs)) rpc session_id
    |> function
    | WriteResult ->
        ()
    | r ->
        unexpected_result "remove_write_certs_fs" r

  let remote_regen_bundle host rpc session_id =
    remote_call_sync host GenBundle rpc session_id |> function
    | GenBundleResult ->
        ()
    | r ->
        unexpected_result "remote_regen_bundle" r

  let local_collect_certs ~__context typ ids =
    let command = string_of_command (CollectMany (typ, ids)) in
    match result_or_fail (local_exec ~__context ~command) with
    | CollectManyResult certs ->
        certs
    | r ->
        unexpected_result "local_collect_certs" r

  let local_write_cert_fs ~__context typ strategy certs =
    let command = string_of_command (Write (typ, strategy, certs)) in
    match result_or_fail (local_exec ~__context ~command) with
    | WriteResult ->
        ()
    | r ->
        unexpected_result "local_write_certs_fs" r

  let local_regen_bundle ~__context =
    let command = string_of_command GenBundle in
    match result_or_fail (local_exec ~__context ~command) with
    | GenBundleResult ->
        ()
    | r ->
        unexpected_result "local_regen_bundle" r
end

let local_exec = Worker.local_exec

let collect_pool_certs ~__context ~rpc ~session_id ~map ~from_hosts =
  from_hosts
  |> List.map (fun host ->
         let uuid = Db.Host.get_uuid ~__context ~self:host in
         let cert =
           Worker.remote_collect_cert HostPoolCertificate uuid host rpc
             session_id
         in
         map cert
     )

let _distrib_m = Mutex.create ()

(* where possible, the master host should control the certificate
 * distributions.  this allows us to coordinate multiple parties that are
 * trying to modify /etc/stunnel at the same time with [lock]!
 *
 * we apply this lock to all top level distribution calls, with the exception of
 * the pool join functions that execute on the joiner.
 *)
let lock (f : unit -> 'a) : 'a =
  D.debug "cert_distrib.ml: locking..." ;
  Mutex.lock _distrib_m ;
  Fun.protect
    ~finally:(fun () ->
      D.debug "cert_distrib.ml: unlocking..." ;
      Mutex.unlock _distrib_m
      )
    f

let exchange_certificates_among_all_members ~__context =
  (* here we coordinate the certificate distribution. from a high level
     we do the following:
     a) collect certs from all the members, aggregating them on the master.
        the cert collected will be [Worker.my_cert]
     b) tell all hosts to write the certs collected in (a) to [Worker.pool_certs]).
        the original contents of [Worker.pool_certs] will either be removed or
        simply added to, depending on [conflict_resolution].
     c) tell all_hosts to regenerate their trust bundles. the old bundle is
        removed completely. see 'update-ca-bundle.sh'. only at this point would we
        expect 'things to go wrong', e.g. new connections fail, if the certs have not
        been set up correctly
     NB: if any individual call fails, then we don't continue with the distribution.
         for example: suppose that the master is unable to obtain the cert from a host,
         then we are guaranteed not to modify the trusted certs on any hosts. however
         we do not guarantee 'atomicity', so if regenerating the bundle on one host
         fails, then state across the pool will most likely become inconsistent, and
         manual intervention may be required *)
  lock @@ fun () ->
  let all_hosts = Xapi_pool_helpers.get_master_slaves_list ~__context in
  Helpers.call_api_functions ~__context @@ fun rpc session_id ->
  let certs =
    collect_pool_certs ~__context ~rpc ~session_id ~from_hosts:all_hosts
      ~map:Fun.id
  in
  List.iter
    (fun host ->
      Worker.remote_write_certs_fs HostPoolCertificate Erase_old certs host rpc
        session_id
      )
    all_hosts ;
  List.iter
    (fun host -> Worker.remote_regen_bundle host rpc session_id)
    all_hosts

let import_joiner ~__context ~uuid ~certificate ~to_hosts =
  let joiner_certificate =
    HostPoolProvider.certificate_of_id_content uuid certificate
  in
  Helpers.call_api_functions ~__context @@ fun rpc session_id ->
  List.iter
    (fun host ->
      Worker.remote_write_certs_fs HostPoolCertificate Merge
        [joiner_certificate] host rpc session_id
      )
    to_hosts ;
  List.iter
    (fun host -> Worker.remote_regen_bundle host rpc session_id)
    to_hosts

(* This function is called on the pool that is incorporating a new host *)
let exchange_certificates_with_joiner ~__context ~uuid ~certificate =
  lock @@ fun () ->
  let all_hosts = Db.Host.get_all ~__context in
  import_joiner ~__context ~uuid ~certificate ~to_hosts:all_hosts ;
  Helpers.call_api_functions ~__context @@ fun rpc session_id ->
  collect_pool_certs ~__context ~rpc ~session_id ~from_hosts:all_hosts
    ~map:WireProtocol.pair_of_certificate_file

(* This function is called on the host that is joining a pool *)
let import_joining_pool_certs ~__context ~pool_certs =
  let pool_certs = List.map WireProtocol.certificate_file_of_pair pool_certs in
  Worker.local_write_cert_fs ~__context HostPoolCertificate Merge pool_certs ;
  Worker.local_regen_bundle ~__context

let collect_ca_certs ~__context ~names =
  Worker.local_collect_certs ApplianceCertificate ~__context names
  |> List.map WireProtocol.pair_of_certificate_file

(* This function is called on the pool that is incorporating a new host *)
let exchange_ca_certificates_with_joiner ~__context ~import ~export =
  lock @@ fun () ->
  let all_hosts = Db.Host.get_all ~__context in
  let appliance_certs = List.map WireProtocol.certificate_file_of_pair import in

  Helpers.call_api_functions ~__context @@ fun rpc session_id ->
  List.iter
    (fun host ->
      Worker.remote_write_certs_fs ApplianceCertificate Merge appliance_certs
        host rpc session_id
      )
    all_hosts ;

  List.iter
    (fun host -> Worker.remote_regen_bundle host rpc session_id)
    all_hosts ;

  collect_ca_certs ~__context ~names:export

(* This function is called on the host that is joining a pool *)
let import_joining_pool_ca_certificates ~__context ~ca_certs =
  let appliance_certs =
    List.map WireProtocol.certificate_file_of_pair ca_certs
  in
  Worker.local_write_cert_fs ~__context ApplianceCertificate Merge
    appliance_certs ;
  Worker.local_regen_bundle ~__context
