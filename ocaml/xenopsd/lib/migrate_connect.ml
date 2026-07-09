(*
 * Copyright (c) Cloud Software Group, Inc.
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

(* TLS transport for the VM-migration data connections in [Xenops_server].

   When [migration-tls = "ktls"] is set in xenopsd.conf and the migration URI
   uses https, this module spawns the external [ktls-helper] binary to
   perform the TLS handshake and install a kTLS-enabled socket in the kernel,
   then receives the resulting socket fd via SCM_RIGHTS and passes it to the
   caller. The fd behaves like an ordinary TCP socket for [read]/[write]: the
   kernel encrypts outgoing data and decrypts incoming data transparently.

   On any failure to PRODUCE the kTLS fd (helper missing, handshake error,
   kTLS install rejected by the kernel, SCM_RIGHTS message lost, ...) this
   module logs a single [warn] and transparently falls back to the existing
   [Open_uri.with_open_uri] (stunnel) path, so the migration still succeeds
   over TLS. Failures raised AFTER the fd has been handed to the caller are
   migration-layer failures and propagate unchanged. *)

module D = Debug.Make (struct let name = "migrate_connect" end)

(* Transport selected by the xenopsd.conf [migration-tls] option. Empty or
   unknown values mean the default stunnel path (the historical behaviour). *)
module Migration_tls = struct
  type t = Stunnel | Ktls

  let to_string = function Stunnel -> "stunnel" | Ktls -> "ktls"

  (* Parse a conf value, defaulting unknown/empty to [Stunnel] with a warning.
     Config parsing must never crash xenopsd, so this does not raise (mirrors
     the [Xenops_server.compressor] convention). *)
  let of_string s =
    match String.lowercase_ascii (String.trim s) with
    | "" | "stunnel" ->
        Stunnel
    | "ktls" ->
        Ktls
    | other ->
        D.warn "unknown migration-tls value %S; using stunnel" other ;
        Stunnel
end

let migration_tls = ref Migration_tls.Stunnel

let ktls_enabled () = !migration_tls = Migration_tls.Ktls

(* Path to the helper binary. Registered as an Xcp_service resource
   ("xenopsd-tls-helper") so it is access-checked at startup and overridable in
   xenopsd.conf; this ref holds the resolved path. *)
let helper_path = ref "/usr/libexec/xapi/ktls-helper"

(* Maximum time to wait for the helper to finish handshaking and hand back the
   fd before abandoning the kTLS path and falling back to stunnel. *)
let helper_timeout = ref 30.0

(* Default port for an https URI with no explicit port. NB: [Constants.https_port]
   would couple xenopsd's library to xapi-consts; that coupling was reverted for
   a build issue, so the value is kept local here. *)
let default_https_port = 443

(* Close [fd] ignoring errors, via the canonical stdext helper. *)
let close_ignore fd =
  Xapi_stdext_pervasives.Pervasiveext.ignore_exn (fun () -> Unix.close fd)

let protect ~finally protected =
  Xapi_stdext_pervasives.Pervasiveext.finally protected finally

(* [let@ () = protect ~finally:cleanup in body] runs [body] then always runs
   [cleanup], reading top to bottom instead of nesting [body] inside a thunk. *)
let ( let@ ) f x = f x

let recv_one_fd sock =
  (* The 16-byte buffer holds enough space for the throwaway 1-byte SCM_RIGHTS
     control payload the helper sends alongside the fd. *)
  let buf = Bytes.make 16 '\000' in
  let _len, _, fd = Fd_send_recv.recv_fd sock buf 0 (Bytes.length buf) [] in
  fd

(* Build the helper argv from the resolved verification policy and the options.
   [verify_cert] is the same [Stunnel.verification_config option] the stunnel
   fallback would use, so the kTLS path honours the pool's certificate-
   verification on/off switch (single source of truth). *)
let helper_args ~host ~port ~fd_uuid ~verify_cert =
  (* SNI selects which certificate the destination serves (the pool-internal
     cert), independently of whether we verify it, so it is always sent — the
     migration profile uses "pool", matching the stunnel client. *)
  let sni_name =
    Option.bind verify_cert (fun cfg -> cfg.Stunnel.sni)
    |> Option.value ~default:"pool"
  in
  let verify_args =
    match verify_cert with
    | None ->
        (* No CA to verify against: certificate verification is disabled
           pool-wide (/var/xapi/verify-certificates absent or emergency-disabled)
           or this is a cross-pool migration. This is the same [verify_cert] the
           stunnel fallback receives, so the kTLS path mirrors stunnel exactly
           and is never more permissive. *)
        ["--no-verify"]
    | Some cfg ->
        ["--cert-bundle-file"; cfg.Stunnel.cert_bundle_path]
  in
  (* Pass the cipher list and ECDHE curve the helper must negotiate, sourced
     from the same [Stunnel.Openssl] values the stunnel client uses, so the two
     paths stay in lock-step and the helper carries no cipher policy of its own. *)
  let cipher_args =
    [
      "--ciphers"
    ; Stunnel.Openssl.default_ciphers
    ; "--curves"
    ; Stunnel.Openssl.default_curve
    ]
  in
  ["--host"; host; "--port"; string_of_int port; "--sni"; sni_name]
  @ verify_args
  @ cipher_args
  @ ["--send-fd"; fd_uuid]

(* Spawn the helper and return the kTLS-enabled fd it sends back via SCM_RIGHTS.
   Raises on any failure; the caller decides whether to fall back. *)
let connect_via_ktls_helper ~host ~port ~verify_cert =
  if not (Sys.file_exists !helper_path) then
    failwith (Printf.sprintf "kTLS helper not found at %s" !helper_path) ;
  let sock_xenopsd, sock_helper =
    Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0
  in
  let fd_uuid = Uuidx.(to_string (make ())) in
  let configs = [(fd_uuid, sock_helper)] in
  let args = helper_args ~host ~port ~fd_uuid ~verify_cert in
  D.debug "spawning %s for %s:%d" !helper_path host port ;
  let pid =
    (* Route the helper's stdout+stderr to syslog under "ktls-helper" so its
       "kTLS active" line and any error line are visible. *)
    try
      Forkhelpers.safe_close_and_exec None None None configs
        ~syslog_stdout:(Forkhelpers.Syslog_WithKey "ktls-helper")
        ~redirect_stderr_to_stdout:true !helper_path args
    with e -> close_ignore sock_helper ; close_ignore sock_xenopsd ; raise e
  in
  (* The helper now holds its own copy of sock_helper; we don't need ours. *)
  close_ignore sock_helper ;
  (* [sock_xenopsd] is closed on every path by the [protect] cleanup below; the
     helper is always reaped via [waitpid] (killed first if it times out) so it
     cannot leak. *)
  let@ () = protect ~finally:(fun () -> close_ignore sock_xenopsd) in
  let received_fd =
    try
      if
        not
          (Xapi_stdext_threads.Threadext.wait_timed_read sock_xenopsd
             !helper_timeout
          )
      then (
        (* Kill the helper so we don't leak it. *)
        ( try Unix.kill (Forkhelpers.getpid pid) Sys.sigkill with _ -> ()
        ) ;
        failwith
          (Printf.sprintf "helper did not respond within %.0fs" !helper_timeout)
      ) ;
      recv_one_fd sock_xenopsd
    with e ->
      (try ignore (Forkhelpers.waitpid pid) with _ -> ()) ;
      raise e
  in
  (* Reap the helper and check its exit status. Close the fd we would return on
     ANY failure to confirm a clean exit (a raising waitpid, or a non-zero or
     other status); on a clean exit it is handed to the caller, so it must NOT
     be closed here. *)
  ( try
      match snd (Forkhelpers.waitpid pid) with
      | Unix.WEXITED 0 ->
          ()
      | status ->
          let reason =
            match status with
            | Unix.WEXITED n ->
                Printf.sprintf "exited with status %d" n
            | Unix.WSIGNALED n ->
                Printf.sprintf "killed by signal %d" n
            | Unix.WSTOPPED n ->
                Printf.sprintf "stopped by signal %d" n
          in
          failwith (Printf.sprintf "ktls-helper %s" reason)
    with e -> close_ignore received_fd ; raise e
  ) ;
  D.debug "received kTLS fd from helper for %s:%d" host port ;
  received_fd

(** Drop-in replacement for [Open_uri.with_open_uri] on the migration paths.

    - When [migration-tls = "ktls"] and the URI is https, spawn the helper,
      receive the kTLS-enabled fd via SCM_RIGHTS, and pass it to [f]. On any
      failure to produce the fd, log a [warn] and fall back to
      [Open_uri.with_open_uri].
    - Otherwise behave exactly as [Open_uri.with_open_uri]. *)
let with_open_uri ?verify_cert uri f =
  let fallback () = Open_uri.with_open_uri ?verify_cert uri f in
  let is_https = Uri.scheme uri = Some "https" in
  if not (is_https && ktls_enabled ()) then
    fallback ()
  else
    match Uri.host uri with
    | None ->
        fallback ()
    | Some host -> (
        let port = Option.value ~default:default_https_port (Uri.port uri) in
        (* [Open_uri]'s [?verify_cert] is itself a [verification_config option],
           so this binding is an option-of-option; flatten it for the helper
           (the [fallback] above re-passes the original via [?verify_cert]). *)
        let verify_cert = Option.join verify_cert in
        (* Only fall back to stunnel when the kTLS path fails to PRODUCE the fd
           (helper spawn / handshake / kTLS install / SCM_RIGHTS). Once [f fd]
           is invoked the kTLS fd has been committed; any exception raised by
           [f] is a migration-layer failure and MUST propagate unchanged, since
           silently retrying [f] over a fresh stunnel socket would re-enter the
           in-progress migration on the destination and corrupt its state. *)
        let fd_or_none =
          try Some (connect_via_ktls_helper ~host ~port ~verify_cert)
          with e ->
            D.warn
              "migration-tls=ktls connect failed for %s:%d (%s); falling back \
               to stunnel for this connection"
              host port (Printexc.to_string e) ;
            None
        in
        match fd_or_none with
        | None ->
            fallback ()
        | Some fd ->
            let@ () = protect ~finally:(fun () -> close_ignore fd) in
            f fd
      )
