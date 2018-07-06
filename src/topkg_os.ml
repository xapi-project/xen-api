(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Topkg_result

(* Environment variables. *)

module Env = struct
  let var name = try Some (Sys.getenv name) with Not_found -> None
  let opt_var name ~absent = match var name with None -> absent | Some v -> v
end

(* Directory operations *)

module Dir = struct

  (* Existence *)

  let exists dir =
    try Ok (Sys.(file_exists dir && is_directory dir)) with
    | Sys_error e -> R.error_msgf "%s: %s" dir e

  let must_exist dir = exists dir >>= function
  | true -> Ok dir
  | false -> R.error_msgf "%s: no such directory" dir

  (* Current working directory *)

  let current () = try Ok (Sys.getcwd ()) with Sys_error e -> R.error_msg e
  let set_current d =
    try Ok (Sys.chdir d) with Sys_error e -> R.error_msgf "%s: %s" d e

  let with_current dir f v =
    current ()
    >>= fun original -> set_current dir
    >>= fun () -> let r = f v in set_current original
    >>= fun () -> Ok r

  (* Directory contents *)

  let contents ?(dotfiles = false) ?(rel = false) p =
    try
      let files = Array.to_list @@ Sys.readdir p in
      if rel && dotfiles then Ok files else
      let rec loop acc = function
      | [] -> List.rev acc
      | f :: fs ->
          let acc =
            if not dotfiles && Topkg_string.is_prefix "." f then acc else
            if rel then f :: acc else Topkg_fpath.append p f :: acc
          in
          loop acc fs
      in
      Ok (loop [] files)
    with
    | Sys_error e -> R.error_msgf "%s: %s" p e
end

(* File system operations *)

module File = struct

  (* Famous file paths *)

  let null = match Sys.os_type with
  | "Win32" -> "NUL"
  | _ -> "/dev/null"

  let dash = "-"

  (* Existence and deletion *)

  let exists f =
    try Ok (Sys.(file_exists f && not (is_directory f))) with
    | Sys_error e -> R.error_msgf "%s: %s" f e

  let must_exist f = exists f >>= function
  | true -> Ok f
  | false -> R.error_msgf "%s: no such file" f

  let delete ?(must_exist = false) f =
    try
      if not must_exist && not (Sys.file_exists f) then Ok () else
      Ok (Sys.remove f)
    with
    | Sys_error e -> R.error_msgf "%s: %s" f e

  (* Folding over files *)

  let fold ?(skip = fun _ -> false) f acc paths =
    let is_dir d = try Sys.is_directory d with Sys_error _ -> false in
    let readdir d =
      try Array.to_list (Sys.readdir d) with Sys_error _ -> []
    in
    let keep p = not (skip p) in
    let process acc file = f file acc in
    let rec aux f acc = function
    | (d :: ds) :: up ->
        let paths = List.rev_map (Filename.concat d) (readdir d) in
        let paths = List.find_all keep paths in
        let dirs, files = List.partition is_dir paths in
        let acc = List.fold_left process acc files in
        aux f acc (dirs :: ds :: up)
    | [] :: [] -> acc
    | [] :: up -> aux f acc up
    | _ -> assert false
    in
    let paths = List.find_all keep paths in
    let dirs, files = List.partition is_dir paths in
    let acc = List.fold_left process acc files in
    Ok (aux f acc (dirs :: []))

  (* Reading and writing *)

  let with_parent_check op op_name file =
    let err_no_parent op_name file =
      Topkg_string.strf
        "%s: Cannot %s file, parent directory does not exist" file op_name
    in
    (Dir.must_exist (Topkg_fpath.dirname file)
     >>= fun _ -> Ok (op file))
    |> R.reword_error @@ fun _ -> `Msg (err_no_parent op_name file)

  let safe_open_in_bin = with_parent_check open_in_bin "read"
  let safe_open_out_bin = with_parent_check open_out_bin "write"

  let read file =
    try
      let close ic = if file = dash then () else close_in_noerr ic in
      (if file = dash then Ok stdin else safe_open_in_bin file) >>= fun ic ->
      try
        let len = in_channel_length ic in
        let buf = Bytes.create len in
        really_input ic buf 0 len; close ic;
        Ok (Bytes.unsafe_to_string buf)
      with exn -> close ic; raise exn
    with Sys_error e -> R.error_msgf "%s: %s" file e

  let write file s =
    try
      let close oc = if file = dash then () else close_out_noerr oc in
      (if file = dash then Ok stdout else safe_open_out_bin file) >>= fun oc ->
      try output_string oc s; flush oc; close oc; Ok ()
      with exn -> close oc; raise exn
    with Sys_error e -> R.error_msgf "%s: %s" file e

  let write_subst file vars s = (* very ugly mister, too lazy to rewrite *)
    try
      let close oc = if file = dash then () else close_out_noerr oc in
      (if file = dash then Ok stdout else safe_open_out_bin file) >>= fun oc ->
      try
        let start = ref 0 in
        let last = ref 0 in
        let len = String.length s in
        while (!last < len - 4) do
          if not (s.[!last] = '%' && s.[!last + 1] = '%') then incr last else
          begin
            let start_subst = !last in
            let last_id = ref (!last + 2) in
            let stop = ref false in
            while (!last_id < len - 1 && not !stop) do
              if not (s.[!last_id] = '%' && s.[!last_id + 1] = '%') then begin
                if s.[!last_id] <> ' ' then (incr last_id) else
                (stop := true; last := !last_id)
              end else begin
                let id_start = start_subst + 2 in
                let id = String.sub s (id_start) (!last_id - id_start) in
                try
                  let subst = List.assoc id vars in
                  output oc (Bytes.unsafe_of_string s)
                    !start (start_subst - !start);
                  output_string oc subst;
                  stop := true;
                  start := !last_id + 2;
                  last := !last_id + 2;
                with Not_found ->
                  stop := true;
                  last := !last_id
              end
            done
          end
        done;
        output oc (Bytes.unsafe_of_string s) !start (len - !start);
        flush oc;
        close oc;
        Ok ()
      with exn -> close oc; raise exn
    with Sys_error e -> R.error_msgf "%s: %s" file e

  let tmp () =
    try
      let f = Filename.temp_file (Filename.basename Sys.argv.(0)) "topkg" in
      at_exit (fun () -> ignore (delete f)); Ok f
    with Sys_error e -> R.error_msg e
end

(* Running commands *)

module Cmd = struct

  let err_empty_line = "no command, empty command line"

  let line ?stdout ?stderr cmd =
    let strf = Printf.sprintf in
    if Topkg_cmd.is_empty cmd then failwith err_empty_line else
    let cmd = List.rev_map Filename.quote (Topkg_cmd.to_rev_list cmd) in
    let cmd = String.concat " " cmd in
    let redirect fd f = strf " %d>%s" fd (Filename.quote f) in
    let stdout = match stdout with None -> "" | Some f -> redirect 1 f in
    let stderr = match stderr with None -> "" | Some f -> redirect 2 f in
    let win_quote = if Sys.win32 then "\"" else "" in
    strf "%s%s%s%s%s" win_quote cmd stdout stderr win_quote

  let exec ?stdout ?stderr cmd =
    try
      let line = line ?stdout ?stderr cmd in
      Topkg_log.debug (fun m -> m ~header:"EXEC" "@[<1>[%s]@]" line);
      Ok ((), (cmd, `Exited (Sys.command line)))
    with Sys_error e | Failure e -> R.error_msg e

  (* Command existence *)

  let test_cmd = match Sys.os_type with
  | "Win32" -> Topkg_cmd.v "where"
  | _ -> Topkg_cmd.v "type"

  let cmd_bin cmd =
    try List.hd (Topkg_cmd.to_list cmd) with
    | Failure _ -> failwith err_empty_line

  let exists cmd =
    try
      let bin = cmd_bin cmd in
      let cmd = Topkg_cmd.(test_cmd % bin) in
      match exec ~stdout:File.null ~stderr:File.null cmd with
      | Ok (_, (_, `Exited 0)) -> Ok true
      | Ok _ -> Ok false
      | Error _ as e -> e
    with
    Failure e -> R.error_msg e

  let must_exist cmd = exists cmd >>= function
  | false -> R.error_msgf "%s: no such command" (cmd_bin cmd)
  | true -> Ok cmd

  (* Running commands *)

  type run_status = Topkg_cmd.t * [ `Exited of int ]

  let success r = r >>= function
    | (v, (_, `Exited 0)) -> Ok v
    | (v, (cmd, `Exited c)) ->
        R.error_msgf "cmd %a: exited with %d" Topkg_cmd.dump cmd c

  let run ?err:stderr cmd = exec ?stderr cmd |> success
  let run_status ?err:stderr cmd =
    exec ?stderr cmd >>= function ((), (_, st)) -> Ok st

  type run_out = { cmd : Topkg_cmd.t; err : Topkg_fpath.t option }

  let out_string ?(trim = true) o =
    File.tmp ()
    >>= fun file -> exec ?stderr:o.err ~stdout:file o.cmd
    >>= fun ((), st) -> File.read file
    >>= fun out -> Ok ((if trim then String.trim out else out), st)

  let out_lines ?trim o =
    out_string ?trim o >>= function (s, st) ->
    Ok ((if s = "" then [] else Topkg_string.cuts ~sep:'\n' s), st)

  let out_file stdout o = exec ?stderr:o.err ~stdout o.cmd
  let out_stdout o = exec ?stderr:o.err ?stdout:None o.cmd

  let to_string ?trim o = out_string ?trim o |> success
  let to_lines ?trim o = out_lines ?trim o |> success
  let to_file stdout o = out_file stdout o |> success
  let run_out ?err cmd = { cmd; err }
end

(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
