(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
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

(** Path to the sha1sum binary (used in the new import/export code to append checksums *)
let sha1sum = "/usr/bin/sha1sum"

open Xapi_stdext_pervasives.Pervasiveext

(** Helper function to prevent double-closes of file descriptors *)
let close to_close fd = 
  if List.mem fd !to_close then Unix.close fd;
  to_close := List.filter (fun x -> fd <> x) !to_close 

(** Fork a slave sha1sum process, execute a function with the input file descriptor
    and return the result of sha1sum, guaranteeing to reap the process. *)
let sha1sum f = 
  let input_out, input_in = Unix.pipe () in
  let result_out, result_in = Unix.pipe () in

  Unix.set_close_on_exec result_out;
  Unix.set_close_on_exec input_in;

  let to_close = ref [ input_out; input_in; result_out; result_in ] in
  let close = close to_close in

  finally
    (fun () ->
       let args = [] in
       let pid = Forkhelpers.safe_close_and_exec (Some input_out) (Some result_in) None [] sha1sum args in

       close result_in;
       close input_out;

       finally
         (fun () -> 
            finally
              (fun () -> f input_in)
              (fun () -> close input_in);
            let buffer = Bytes.make 1024 '\000' in
            let n = Unix.read result_out buffer 0 (Bytes.length buffer) in
            let raw = Bytes.sub_string buffer 0 n in
            let result = match String.split_on_char ' ' raw with
              | result :: _ -> result
              | _ -> failwith (Printf.sprintf "Unable to parse sha1sum output: %s" raw) in
            close result_out;
            result)
         (fun () ->
            Forkhelpers.waitpid_fail_if_bad_exit pid
         )
    ) (fun () -> List.iter close !to_close)


