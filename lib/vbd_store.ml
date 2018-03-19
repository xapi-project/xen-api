open Lwt.Infix

let section = Lwt_log.Section.make "Vbd_store"

module Make(Config : sig
    val vbd_list_dir : string
    val vbd_list_file_name : string
  end) = struct

  let vbd_list_dir = Config.vbd_list_dir
  let vbd_list_file_name = Config.vbd_list_file_name
  let vbd_list_file = vbd_list_dir ^ "/" ^ vbd_list_file_name

  let m = Lwt_mutex.create ()

  let log_and_reraise_error msg e =
    Lwt_log.error_f ~section "%s: %s" msg (Printexc.to_string e) >>= fun () ->
    Lwt.fail e

  let create_dir_if_doesnt_exist () =
    Lwt.catch
      (fun () -> Lwt_unix.mkdir vbd_list_dir 0o755)
      (function
        | Unix.Unix_error (EEXIST, "mkdir", dir) when dir = vbd_list_dir -> Lwt.return_unit
        | e ->
          (* In any other case we let the client fail. In this case the user/admin should go and fix the root cause of the issue *)
          log_and_reraise_error ("Failed to create directory " ^ vbd_list_dir) e
      )

  let transform_vbd_list f =
    Lwt_mutex.with_lock m (fun () ->
        create_dir_if_doesnt_exist () >>= fun () ->
        (* We cannot have one stream here piped through a chain of functions,
           because the beginning of the stream (Lwt_io.lines_of_file) would read
           what the end of the stream writes (Lwt_io.lines_to_file), and it would
           overwrite the original file with duplicate entries. Instead, we read
           the whole stream into a list here to ensure the file gets closed. *)
        Lwt.catch
          (fun () -> Lwt_io.lines_of_file vbd_list_file |> Lwt_stream.to_list)
          (function
            | Unix.Unix_error (ENOENT, "open", file) when file = vbd_list_file -> Lwt.return []
            | e ->
              (* In any other case we let the client fail. In this case the user/admin should go and fix the root cause of the issue *)
              log_and_reraise_error ("Failed to read file " ^ vbd_list_file) e
          )
        >>= fun l ->
        let l = f l in
        Lwt.catch
          (fun () -> Lwt_stream.of_list l |> Lwt_io.lines_to_file vbd_list_file)
          (log_and_reraise_error ("Failed to write to " ^ vbd_list_file))
      )

  let add vbd_uuid =
    transform_vbd_list (List.append [vbd_uuid])

  let remove vbd_uuid =
    transform_vbd_list (List.filter ((<>) vbd_uuid))

  let get_all () =
    (* Nothing should delete the vbd_list_file, so we do not have to use a
       Lwt.catch block here to prevent races where the file gets deleted after we
       check that it exists but before we use it. If it does get deleted, then it
       is fine to fail here, because that was done by an external program and is
       an error that the user/admin should fix. *)
    Lwt_unix.file_exists vbd_list_file >>= fun exists ->
    if exists then
      Lwt_mutex.with_lock m (fun () ->
          Lwt.catch
            (fun () -> Lwt_io.lines_of_file vbd_list_file |> Lwt_stream.to_list)
            (log_and_reraise_error ("Failed to read " ^ vbd_list_file))
        )
    else
      Lwt.return []

end
