(*
 * Copyright (c) Cloud Software Group, Inc
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

module Pam = struct
  include Pam

  let unsafe_crypt_r = Pam.unsafe_crypt_r [@@alert "-unsafe"]
  (* Suppress the alert the purpose of testing. *)
end

let valid_salts =
  [
    "salt" (* Don't need to specify algorithm, will default to something. *)
  ; "$5$salt$" (* 5 = SHA-256 should work. *)
  ; "$6$salt$" (* 6 = SHA-512 should work. *)
  ]

let invalid_salts =
  [
    "" (* Salt cannot be empty. *)
  ; "$" (* Salt cannot be $. *)
  ; "$9$salt$" (* Salt must specify valid algorithm constant. *)
  ; "$6,rounds=1000$salt$" (* Salt cannot specify iteration count. *)
  ; "£6£salt£" (* Only American currency is legal tender. *)
  ]

let test_salts ~msg ~succeeds salts =
  let test salt =
    let actual =
      Option.is_some (Pam.unsafe_crypt_r ~key:"password" ~setting:salt)
    in
    Alcotest.((check' bool) ~msg ~expected:succeeds ~actual)
  in
  List.iter test salts

let test_valid_salts () =
  test_salts ~msg:"Hash can be computed from valid salt" ~succeeds:true
    valid_salts

let test_invalid_salts () =
  test_salts ~msg:"Hash cannot be computed from invalid salt" ~succeeds:false
    invalid_salts

let test_salt_truncation () =
  let salt_max_length = 16 in
  let salt = "a_salt_that_is_longer_than_is_actually_accepted" in
  assert (String.length salt > salt_max_length) ;
  let test prefix_length =
    (* The C API accepts at most 16 chars for the salt, optionally
       enclosed within $k$salt$ - anything else is ignored (implicitly
       truncated). *)
    let truncated_salt = String.sub salt 0 prefix_length in
    let sha512 = Printf.sprintf "$6$%s$" in
    let key = "password" in
    let h = Pam.unsafe_crypt_r ~key ~setting:(sha512 salt) in
    let h' = Pam.unsafe_crypt_r ~key ~setting:(sha512 truncated_salt) in
    if Option.(is_none h || is_none h') then
      failwith (Printf.sprintf "Failed to compute hash in %s" __FUNCTION__)
    else
      Option.equal ( = ) h h'
  in
  let msg =
    Printf.sprintf
      "Hash computed with implicitly truncated salt is the same as explicitly \
       truncated (len = %d)\n\
       ."
  in
  let expectation len =
    (* We expect all lengths greater than max salt length to succeed,
       as they are implicitly truncated. Any length < salt_max_length
       should fail. *)
    len >= salt_max_length
  in
  for len = 0 to String.length salt do
    let msg = msg len in
    let actual = test len in
    let expected = expectation len in
    Alcotest.(check' bool) ~msg ~expected ~actual
  done

(* Invalidate the following tests if any hash fails to be computed. *)
let unsafe_crypt_r ~key ~setting =
  match Pam.unsafe_crypt_r ~key ~setting with
  | Some hash ->
      hash
  | _ ->
      failwith "Invalid input provided to crypt_r"

let test_crypt_r_many_threads () =
  Printexc.record_backtrace true ;
  let settings = ["$6$salt$"; "$5$salt123$"; "$6$foobar$"; "salt"] in
  (* Each test case is a 3-tuple (key, setting, hash). A thread is
     spawned for each test case. The hash component stores the expected
     result of hashing key with setting. These hashes are computed prior
     to spawning the threads so they are guaranteed to have been computed
     sequentially. *)
  let test_cases =
    let create_case setting =
      let key = "password" in
      let hash = unsafe_crypt_r ~key ~setting in
      (key, setting, hash)
    in
    List.map create_case settings
  in
  let num_cases = List.length test_cases in
  let thread_count = Atomic.make 0 in
  let ready () = Atomic.get thread_count >= num_cases in
  let m = Mutex.create () in
  let c = Condition.create () in
  (* Each thread will populate an entry in the results array. *)
  let results : (unit, _) result array = Array.make num_cases (Ok ()) in
  let spawn i (key, setting, expectation) =
    let loop () =
      let now = Unix.gettimeofday in
      let start = now () in
      while now () -. start < 0.2 do
        let actual = unsafe_crypt_r ~key ~setting in
        if actual <> expectation then
          failwith (Printf.sprintf "%s <> %s" actual expectation)
      done
    in
    (* Record that this thread has been started, then wait for the
       main thread to broadcast that the others have also started. *)
    Atomic.incr thread_count ;
    Mutex.lock m ;
    while not (ready ()) do
      Condition.wait c m
    done ;
    Mutex.unlock m ;
    (* Run the test, capturing any exception as a result to the
       negative. *)
    results.(i) <- Rresult.R.trap_exn loop ()
  in
  (* Spawn a thread per valid test case. *)
  let tids = List.mapi (fun i -> Thread.create (spawn i)) test_cases in
  (* Wait for all threads to identify themselves as having started
     before broadcasting that they should start hashing. *)
  while not (ready ()) do
    Unix.sleepf 0.1
  done ;
  Mutex.lock m ;
  Condition.broadcast c ;
  Mutex.unlock m ;
  List.iter Thread.join tids ;
  (* Re-raise the first encountered trapped exception with its
     backtrace to ensure the test fails if any thread reported
     failure. *)
  let reraise = function
    | Error (`Exn_trap (exn, bt)) ->
        Printexc.raise_with_backtrace exn bt
    | _ ->
        ()
  in
  Array.iter reraise results

(* This test hashes strings of language 'a'+ over a small range of lengths to
   ensure no duplicates occur. A suitable cryptographic hash function should have
   no collisions doing this. So, if a collision occurs, it is more likely because
   the underlying algorithm has a maximum length key size (and is truncating our
   input). *)
let test_increasing_length () =
  let min, max = (50, 140) in
  (* Records hash -> length, so colliding lengths can be reported. *)
  let tbl = Hashtbl.create 127 in
  let setting = "$6$salt$" in
  let go len =
    let key = String.make len 'a' in
    let hash =
      try unsafe_crypt_r ~key ~setting
      with _ ->
        failwith (Printf.sprintf "Failed to compute hash aa..a of length %d" len)
    in
    match Hashtbl.find_opt tbl hash with
    | Some len' ->
        failwith
          (Printf.sprintf "Hash value a.. (len = %d) matches a.. (len %d)" len
             len'
          )
    | _ ->
        Hashtbl.add tbl hash len
  in
  for i = min to max do
    go i
  done

(* This test demonstrates the behaviour that the C API will
   (expectedly) only read up to the null terminator character. OCaml
   strings are stored as an array of words, with the final byte
   specifying how many padding bytes precede it. Since the number of
   words and number of padding bytes is used to determine string length,
   there is no reliance on a C-style null terminator - so '\0' can appear
   anywhere in an OCaml string. *)
let test_c_truncation () =
  let key = "password" in
  let key' = key ^ "\x00_arbitrary_data_here" in
  let setting = "$6$salt$" in
  let hash = unsafe_crypt_r ~key ~setting in
  let hash' = unsafe_crypt_r ~key:key' ~setting in
  if hash <> hash' then
    failwith "Expected truncation using C-style null termination failed"

(* Make following tests fail if the safe API fails to return a valid result. *)
let crypt ~algo ~key ~salt =
  let open struct exception CryptException of Pam.crypt_err end in
  match Pam.crypt ~algo ~key ~salt with
  | Ok hash ->
      hash
  | Error e ->
      raise (CryptException e)

(* Test trivial correspondence between safe API invocation and unsafe calls. *)
let test_api_correspondence () =
  let cases =
    [
      ("$5$salt123$", Pam.SHA256, "salt123")
    ; ("$6$salt456$", Pam.SHA512, "salt456")
    ]
  in
  let go (setting, algo, salt) =
    let key = "password" in
    let h = unsafe_crypt_r ~key ~setting in
    let h' = crypt ~algo ~key ~salt in
    if h <> h' then
      failwith
        "Hashes differ between invocations of safe and unsafe crypt_r APIs"
  in
  List.iter go cases

(** Ensure the safe API fails in the way you expect. *)
let test_safe_failures () =
  let key = "password" in
  let cases =
    [
      (* Salt exceeding maximum length. *)
      ( (fun () ->
          Pam.crypt ~algo:SHA256 ~key ~salt:"asaltthatexceedsthemaximumlength"
        )
      , Pam.SaltTooLong
      )
    ]
  in
  let test (case, expected_error) =
    match case () with
    | Ok _ ->
        failwith "Expected crypt error"
    | Error e when e <> expected_error ->
        failwith "Actual crypt error does not match expectation"
    | Error _ ->
        ()
  in
  List.iter test cases

let tests () =
  [
    ("Valid salts", `Quick, test_valid_salts)
  ; ("Invalid salts", `Quick, test_invalid_salts)
  ; ("Implicit salt truncation", `Quick, test_salt_truncation)
  ; ("Increasing string length", `Quick, test_increasing_length)
  ; ("C-style termination", `Quick, test_c_truncation)
  ; ("Safe and unsafe API", `Quick, test_api_correspondence)
  ; ("Safe API error reporting", `Quick, test_safe_failures)
  ; ("Multiple threads", `Quick, test_crypt_r_many_threads)
  ]
