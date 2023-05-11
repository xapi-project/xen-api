open Xapi_blobstore_core
open Xen_api_lwt_unix
open Lwt.Syntax

module Key = Bounded_string.Make (struct let max_length = 1024 end)

module Value = Bounded_string.Make (struct let max_length = 128 * 1024 end)

let max_key_count = 256

let max_data_size = 256 * 1024

module IO = Lwt

type config = {vtpm: Uuidm.t; target: Uri.t; uname: string; pwd: string}

let pp_config =
  Fmt.Dump.(
    record
      [
        field "vtpm" (fun t -> t.vtpm) Uuidm.pp
      ; field "target" (fun t -> t.target) Uri.pp
      ; field "uname" (fun t -> t.uname) Fmt.string
      ; field "length(pwd)" (fun t -> t.pwd |> String.length) Fmt.int
      ]
  )

type t = {
    cache: Xen_api_lwt_unix.SessionCache.t
  ; uri: Uri.t
  ; vtpm: API.ref_VTPM
  ; lock: Lwt_mutex.t
}

(* TODO: on cohttp 6.x we can also use a Cohttp_lwt_unix.Connection_cache.t here *)

let name = __MODULE__

let version = "0.1" (* TODO: from dune-build-info *)

let call t f = Xen_api_lwt_unix.SessionCache.with_session t.cache f

let shutdown = Lwt_switch.create ()

(* logout sessions *)
let () = Lwt_main.at_exit (fun () -> Lwt_switch.turn_off shutdown)

let cache = ref None

let get_or_create_cache config =
  match !cache with
  | Some c -> c
  | None ->
    let c = Xen_api_lwt_unix.SessionCache.create_uri ~switch:shutdown
      ~target:config.target ~uname:config.uname ~pwd:config.pwd ~version
      ~originator:name () in
    cache := Some c;
    c

let connect config =
  let cache = get_or_create_cache config in
  let t =
    {cache; vtpm= Ref.null; uri= config.target; lock= Lwt_mutex.create ()}
  in
  let+ vtpm = call t @@ VTPM.get_by_uuid ~uuid:(Uuidm.to_string config.vtpm) in
  {t with vtpm}

let disconnect _t = Lwt.return_unit

(* we may need to store multiple keys, but XAPI only has a single field:
   serialize using Pbrt for now since this is all binary *)
module M = Map.Make (Key)



let serialize t =
  let alist = List.of_seq (
    t |> M.to_seq |> Seq.map @@ fun (k, v) ->
    (* we pass it through XAPI APIs, had to encode *)
    k |> Key.to_string |> Base64.encode_string, `String (Value.to_string v |> Base64.encode_string)
  ) in
  `Assoc alist |> Yojson.Safe.to_string

let deserialize t =
  if String.length t = 0 then M.empty
  else
  match t |> Yojson.Safe.from_string with
  | `Assoc alist ->
    M.of_seq (
      alist |> List.to_seq |> Seq.map @@ fun (k, v) ->
      k |> Base64.decode_exn |> Key.of_string_exn,
      v |> Yojson.Safe.Util.to_string |> Base64.decode_exn |> Value.of_string_exn
    )
  | _ -> invalid_arg "malformed JSON"

let list t =
  let+ serialized = call t @@ VTPM.get_contents ~self:t.vtpm in
  deserialize serialized |> M.to_seq |> Seq.map fst |> List.of_seq

let lookup t =
  let+ blob = call t @@ VTPM.get_contents ~self:t.vtpm in
  deserialize blob

let get t k =
  let+ r = lookup t in
  M.find_opt k r

let delete t k =
  Lwt_mutex.with_lock t.lock @@ fun () ->
  let* r = lookup t in
  let r' = M.remove k r in
  if r' == r then
    Lwt.return_unit (* nothing changed, map physically equal *)
  else
    call t @@ VTPM.set_contents ~self:t.vtpm ~contents:(serialize r)

let put t k v =
  Lwt_mutex.with_lock t.lock @@ fun () ->
  let* r = lookup t in
  let contents = M.add k v r |> serialize in
  call t @@ VTPM.set_contents ~self:t.vtpm ~contents
