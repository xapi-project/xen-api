
module Impl = Vhd_format.F.From_file(Vhd_format_lwt.IO)
open Vhd_format.F
open Vhd_format_lwt.IO

module In = From_input(Input)
open In

let get_vhd_vsize filename =
  Vhd_format_lwt.IO.openfile filename false >>= fun fd ->
  let rec loop = function
    | End -> return ()
    | Cons (hd, tl) ->
      begin match hd with
        | Fragment.Footer x ->
          let size = x.Footer.current_size in
          Printf.printf "%Ld\n" size;
          exit 0
        | _ ->
          ()
      end;
      tl () >>= fun x ->
      loop x
  in
  Vhd_format_lwt.IO.get_file_size filename >>= fun file_size ->
  openstream (Some file_size)
    (Input.of_fd (Vhd_format_lwt.IO.to_file_descr fd)) >>= fun stream ->
  loop stream >>= fun () -> Vhd_format_lwt.IO.close fd

let _ =
  let t = get_vhd_vsize Sys.argv.(1) in
  Lwt_main.run t
