(*
 * Copyright (C) Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

let default_pool_secret = "default_pool_secret"

let new_pool_secret = "new_pool_secret"

(* we could also test with a fistpoint 'During' the computation,
   but this adds even more complexity to the test *)
type fistpoint_time = Before | After

let string_of_fistpoint_time = function Before -> "Before" | After -> "After"

type fistpoint_action =
  | Accept_new_pool_secret
  | Send_new_pool_secret
  | Cleanup

let string_of_fistpoint_action = function
  | Accept_new_pool_secret ->
      "Accept_new_pool_secret"
  | Send_new_pool_secret ->
      "Send_new_pool_secret"
  | Cleanup ->
      "Cleanup"

(* we place fistpoints either just before
   executing some action or just after *)
type fistpoint = fistpoint_time * fistpoint_action

let string_of_fistpoint (t, p) =
  Printf.sprintf "%s:%s"
    (string_of_fistpoint_time t)
    (string_of_fistpoint_action p)

exception Fistpoint of fistpoint

let () =
  Printexc.register_printer (function
    | Fistpoint fp ->
        Some (string_of_fistpoint fp)
    | _ ->
        None
    )

type host_id = int

type member = {
    id: host_id
  ; mutable current_pool_secret: string
  ; mutable staged_pool_secret: string option
  ; mutable fistpoint: fistpoint option
}

type psr_state = {
    mutable checkpoint: string option
  ; mutable pool_secret_backups: (string * string) option
}

(* in the real implementation, the checkpoint and pool secret backups
   are stored on the master, so we model that here *)
type master = {member: member; psr_state: psr_state}

type host_t = Master of master | Member of member

let map_r f = Rresult.R.reword_error (fun (failure, e) -> (failure, f e))

let string_of_failure = function
  | Xapi_psr.Failed_during_accept_new_pool_secret ->
      "Failed_during_accept_new_pool_secret"
  | Failed_during_send_new_pool_secret ->
      "Failed_send_new_pool_secret"
  | Failed_during_cleanup ->
      "Failed_during_cleanup"

let string_of_r = function
  | Ok () ->
      "Success"
  | Error (failure, host_id) ->
      Printf.sprintf "%s: %d" (string_of_failure failure) host_id

let mk_hosts num =
  if num < 1 then failwith (Printf.sprintf "expected num > 0, but num=%d" num) ;
  let member =
    {
      id= 0
    ; current_pool_secret= default_pool_secret
    ; staged_pool_secret= None
    ; fistpoint= None
    }
  in
  let master =
    {member; psr_state= {checkpoint= None; pool_secret_backups= None}}
  in
  let members = List.init (num - 1) (fun id -> {member with id= id + 1}) in
  (master, members)

module Impl =
functor
  (Hosts : sig
     val master : master
   end)
  ->
  struct
    open Hosts

    type pool_secret = string

    type pool_secrets = pool_secret * pool_secret

    type host = host_t

    let save_checkpoint x = master.psr_state.checkpoint <- Some x

    let retrieve_checkpoint () = master.psr_state.checkpoint

    let backup pool_secrets =
      master.psr_state.pool_secret_backups <- Some pool_secrets

    let retrieve () = Option.get master.psr_state.pool_secret_backups

    let iter_host f = function Member m -> f m | Master m -> f m.member

    let tell_accept_new_pool_secret (_, staged_pool_secret) =
      iter_host (fun h ->
          let f () = h.staged_pool_secret <- Some staged_pool_secret in
          match h.fistpoint with
          | Some ((Before, Accept_new_pool_secret) as fp) ->
              raise (Fistpoint fp)
          | Some ((After, Accept_new_pool_secret) as fp) ->
              f () ; raise (Fistpoint fp)
          | _ ->
              f ()
      )

    let tell_send_new_pool_secret _ =
      iter_host (fun h ->
          let f () = h.current_pool_secret <- Option.get h.staged_pool_secret in
          match h.fistpoint with
          | Some ((Before, Send_new_pool_secret) as fp) ->
              raise (Fistpoint fp)
          | Some ((After, Send_new_pool_secret) as fp) ->
              f () ; raise (Fistpoint fp)
          | _ ->
              f ()
      )

    let tell_cleanup_old_pool_secret _ =
      iter_host (fun h ->
          let f () = h.staged_pool_secret <- None in
          match h.fistpoint with
          | Some ((Before, Cleanup) as fp) ->
              raise (Fistpoint fp)
          | Some ((After, Cleanup) as fp) ->
              f () ; raise (Fistpoint fp)
          | _ ->
              f ()
      )

    let cleanup_master _ =
      let f () =
        master.member.staged_pool_secret <- None ;
        master.psr_state.checkpoint <- None ;
        master.psr_state.pool_secret_backups <- None
      in
      match master.member.fistpoint with
      | Some ((Before, Cleanup) as fp) ->
          raise (Fistpoint fp)
      | Some ((After, Cleanup) as fp) ->
          f () ; raise (Fistpoint fp)
      | _ ->
          f ()
  end

module type PSR = sig
  val start :
       string * string
    -> master:master
    -> members:member list
    -> host_id Xapi_psr.r
end

(* the test implementation has been defined above,
   and we use this function to access it *)
let mk_psr master =
  let module PSR = Xapi_psr.Make (Impl (struct let master = master end)) in
  let module PSR = struct
    include PSR

    let start pool_secrets ~master ~members =
      start pool_secrets ~master:(Master master)
        ~members:(List.map (fun m -> Member m) members)
      |> map_r (function Member m -> m.id | Master m -> m.member.id)
  end in
  (module PSR : PSR)

let r' = Alcotest.testable (Fmt.of_to_string string_of_r) ( = )

let check_psr_succeeded r exp_pool_secret master members =
  let hosts = master.member :: members in
  Alcotest.check r' "PSR should be successful" (Ok ()) r ;
  List.iter
    (fun h ->
      Alcotest.(
        check string "new pool secret should be correct" exp_pool_secret
          h.current_pool_secret
      )
    )
    hosts ;
  List.iter
    (fun h ->
      Alcotest.(
        check (option string) "staged_pool_secret is null" None
          h.staged_pool_secret
      )
    )
    hosts ;
  let psr_state = master.psr_state in
  Alcotest.(
    check (option string) "checkpoint was cleaned up" None psr_state.checkpoint
  ) ;
  Alcotest.(
    check
      (option (pair string string))
      "pool secret backups were cleaned up" None psr_state.pool_secret_backups
  )

(* we test almost all combinations of hosts and fistpoints. the only
   case we don't test is one case of master cleanup failure,
   since it is slightly different - it has its own unit test (see below).
*)
let almost_all_possible_fists ~num_hosts =
  let multiply fp_times fp_actions host_ids =
    List.map
      (fun time ->
        List.map
          (fun (action, mk_expected_err) ->
            List.map
              (fun host_id -> ((time, action), host_id, mk_expected_err host_id))
              host_ids
          )
          fp_actions
      )
      fp_times
    |> List.concat
    |> List.concat
  in
  let range = List.init num_hosts Fun.id in
  let open Xapi_psr in
  let master_fistpoints =
    multiply [Before; After]
      [
        ( Accept_new_pool_secret
        , fun id -> Error (Failed_during_accept_new_pool_secret, id)
        )
      ; ( Send_new_pool_secret
        , fun id -> Error (Failed_during_send_new_pool_secret, id)
        )
      ]
      [0]
    |> List.cons ((Before, Cleanup), 0, Error (Failed_during_cleanup, 0))
  in
  let member_fistpoints =
    multiply [Before; After]
      [
        ( Accept_new_pool_secret
        , fun id -> Error (Failed_during_accept_new_pool_secret, id)
        )
      ; ( Send_new_pool_secret
        , fun id -> Error (Failed_during_send_new_pool_secret, id)
        )
      ; (Cleanup, fun id -> Error (Failed_during_cleanup, id))
      ]
      (List.tl range)
  in
  List.append member_fistpoints master_fistpoints

let test_no_fistpoint () =
  let master, members = mk_hosts 6 in
  let module PSR = (val mk_psr master) in
  let r = PSR.start (default_pool_secret, new_pool_secret) ~master ~members in
  check_psr_succeeded r new_pool_secret master members

(* try PSR with a fistpoint primed, check that it
   fails; then retry without any fistpoints and
   check that it succeeds. *)
let test_fail_once () =
  let num_hosts = 3 in
  let master, members = mk_hosts num_hosts in
  let module PSR = (val mk_psr master) in
  let hosts = master.member :: members in
  almost_all_possible_fists ~num_hosts
  |> List.iter (fun (fistpoint, host_id, exp_err) ->
         let new_pool_secret =
           Printf.sprintf "%s-%d-%s" new_pool_secret host_id
             (string_of_fistpoint fistpoint)
         in
         let faulty_host = List.nth hosts host_id in
         faulty_host.fistpoint <- Some fistpoint ;
         let r =
           PSR.start (default_pool_secret, new_pool_secret) ~master ~members
         in
         Alcotest.check r'
           (Printf.sprintf "PSR should fail with %s" (string_of_r exp_err))
           exp_err r ;
         faulty_host.fistpoint <- None ;
         let r = PSR.start (default_pool_secret, "not_used") ~master ~members in
         check_psr_succeeded r new_pool_secret master members
     )

let test_master_fails_during_cleanup () =
  let open Xapi_psr in
  let num_hosts = 6 in
  let master, members = mk_hosts num_hosts in
  let module PSR = (val mk_psr master) in
  let fistpoint = (After, Cleanup) in
  let exp_err = Error (Failed_during_cleanup, 0) in
  master.member.fistpoint <- Some fistpoint ;
  let r = PSR.start (default_pool_secret, new_pool_secret) ~master ~members in
  Alcotest.check r'
    (Printf.sprintf "PSR should fail with %s" (string_of_r exp_err))
    exp_err r ;
  master.member.fistpoint <- None ;
  let r =
    PSR.start (default_pool_secret, "this_ps_gets_used") ~master ~members
  in
  check_psr_succeeded r "this_ps_gets_used" master members

let tests =
  [
    ( "PSR"
    , [
        ("test_no_fistpoint", `Quick, test_no_fistpoint)
      ; ("test_fail_once", `Quick, test_fail_once)
      ; ( "test_master_fails_during_cleanup"
        , `Quick
        , test_master_fails_during_cleanup
        )
      ]
    )
  ]
