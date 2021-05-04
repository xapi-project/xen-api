module Unixext = Xapi_stdext_unix.Unixext

module D = Debug.Make (struct let name = "cert_distrib" end)

module XenAPI = Client.Client

type existing_cert_strategy = Erase_old | Merge [@@deriving sexp]

type 'a remote_call =
  API.ref_host -> (Rpc.call -> Rpc.response) -> API.ref_session -> 'a

module WireProtocol = struct
  module Sexp = Sexplib.Sexp
  open Sexplib0.Sexp_conv

  type cert_blob = string [@@deriving sexp]

  type cert = {uuid: string; blob: cert_blob} [@@deriving sexp]

  type command =
    | Collect
    | Write of (existing_cert_strategy * cert list)
    | GenBundle
  [@@deriving sexp]

  type result = CollectResult of cert_blob | WriteResult | GenBundleResult
  [@@deriving sexp]

  let string_of_command x = x |> sexp_of_command |> Sexp.to_string

  let command_of_string x =
    try Some (x |> Sexp.of_string |> command_of_sexp) with _ -> None

  let dbg_of_command = function
    | Collect ->
        "Collect"
    | Write (Erase_old, _) ->
        "Write (Erase_old)"
    | Write (Merge, _) ->
        "Write (Merge)"
    | GenBundle ->
        "GenBundle"

  let string_of_result x = x |> sexp_of_result |> Sexp.to_string

  let result_of_string x =
    try Some (x |> Sexp.of_string |> result_of_sexp) with _ -> None

  let dbg_of_result = function
    | CollectResult _ ->
        "CollectResult"
    | WriteResult ->
        "WriteResult"
    | GenBundleResult ->
        "GenBundleResult"
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

(* eventually the remote calls should probably become API calls in the datamodel
   but they remain here for quick development *)
module Worker : sig
  val local_exec : __context:Context.t -> command:string -> string

  val remote_collect_cert : WireProtocol.cert_blob remote_call

  val remote_write_certs_fs :
    existing_cert_strategy -> WireProtocol.cert list -> unit remote_call

  val remote_regen_bundle : unit remote_call

  val local_write_cert_fs :
       __context:Context.t
    -> existing_cert_strategy
    -> WireProtocol.cert list
    -> unit

  val local_regen_bundle : __context:Context.t -> unit
end = struct
  open WireProtocol

  let my_cert = !Xapi_globs.server_cert_internal_path

  let pool_certs = !Xapi_globs.trusted_pool_certs_dir

  let pool_certs_bk = Printf.sprintf "%s.bk" pool_certs

  let get_my_cert () : cert_blob =
    let open Gencertlib.Pem in
    match parse_file my_cert with
    | Error reason ->
        raise_internal
          ~details:(Printf.sprintf "reason: %s" reason)
          (Printf.sprintf "failed to parse %s" my_cert)
    | Ok {host_cert; _} ->
        host_cert

  let write_certs_fs strategy certs =
    let open Helpers.FileSys in
    let mv_or_cp = match strategy with Erase_old -> mv | Merge -> cpr in
    ( try mv_or_cp ~src:pool_certs ~dest:pool_certs_bk
      with e ->
        D.debug
          "write_certs_fs: ignoring failure, mv_or_cp %s to %s. exception: %s"
          pool_certs pool_certs_bk (Printexc.to_string e)
    ) ;
    ( try
        Unixext.mkdir_rec pool_certs 0o700 ;
        certs
        |> List.iter (fun {uuid; blob} ->
               let fname = Printf.sprintf "%s/%s.pem" pool_certs uuid in
               redirect blob ~fname)
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

  let regen_bundle () =
    ignore
      (Forkhelpers.execute_command_get_output
         "/opt/xensource/bin/update-ca-bundle.sh" [])

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
      | Collect ->
          let cert = get_my_cert () in
          CollectResult cert
      | Write (strategy, certs) ->
          write_certs_fs strategy certs ;
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
    XenAPI.Host.cert_distrib_atom rpc session_id host
      (string_of_command command)
    |> result_or_fail

  let unexpected_result name r =
    raise_internal
      ~details:(Printf.sprintf "r = %s" (dbg_of_result r))
      (Printf.sprintf "%s: unexpected result" name)

  let remote_collect_cert host rpc session_id =
    remote_call_sync host Collect rpc session_id |> function
    | CollectResult cert ->
        cert
    | r ->
        unexpected_result "remote_collect_cert" r

  let remote_write_certs_fs strategy certs host rpc session_id =
    remote_call_sync host (Write (strategy, certs)) rpc session_id |> function
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

  let local_write_cert_fs ~__context strategy certs =
    let command = WireProtocol.string_of_command (Write (strategy, certs)) in
    match result_or_fail (local_exec ~__context ~command) with
    | WriteResult ->
        ()
    | r ->
        unexpected_result "local_write_certs_fs" r

  let local_regen_bundle ~__context =
    let command = WireProtocol.string_of_command GenBundle in
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
         let blob = Worker.remote_collect_cert host rpc session_id in
         map WireProtocol.{uuid; blob})

let exchange_certificates_among_all_members ~__context =
  (* here we coordinate the certificate distribution. from a high level
     we do the following:
     a) collect certs from all the members, aggregating them on the master.
        the cert collected will be [Worker.my_cert]
     b) tell all hosts to write the certs collected in (a) to [Worker.pool_certs]).
        the original contents of [Worker.pool_certs] will either be removed or
        simply added to, depending on [existing_cert_strategy].
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
  let all_hosts = Xapi_pool_helpers.get_master_slaves_list ~__context in
  Helpers.call_api_functions ~__context @@ fun rpc session_id ->
  let certs =
    collect_pool_certs ~__context ~rpc ~session_id ~from_hosts:all_hosts
      ~map:Fun.id
  in
  List.iter
    (fun host ->
      Worker.remote_write_certs_fs Erase_old certs host rpc session_id)
    all_hosts ;
  List.iter
    (fun host -> Worker.remote_regen_bundle host rpc session_id)
    all_hosts

let import_joiner ~__context ~uuid ~certificate ~to_hosts =
  let joiner_certificate = WireProtocol.{uuid; blob= certificate} in
  Helpers.call_api_functions ~__context @@ fun rpc session_id ->
  List.iter
    (fun host ->
      Worker.remote_write_certs_fs Merge [joiner_certificate] host rpc
        session_id)
    to_hosts ;
  List.iter
    (fun host -> Worker.remote_regen_bundle host rpc session_id)
    to_hosts

(* This function is called on the pool that is incorporating a new host *)
let exchange_certificates_with_joiner ~__context ~uuid ~certificate =
  let all_hosts = Db.Host.get_all ~__context in
  import_joiner ~__context ~uuid ~certificate ~to_hosts:all_hosts ;
  Helpers.call_api_functions ~__context @@ fun rpc session_id ->
  collect_pool_certs ~__context ~rpc ~session_id ~from_hosts:all_hosts
    ~map:(fun WireProtocol.{uuid; blob} -> (uuid, blob))

(* This function is called on the host that is joining a pool *)
let import_joining_pool_certs ~__context ~pool_certs =
  let pool_certs =
    List.map (fun (uuid, blob) -> WireProtocol.{uuid; blob}) pool_certs
  in
  Worker.local_write_cert_fs ~__context Merge pool_certs ;
  Worker.local_regen_bundle ~__context
