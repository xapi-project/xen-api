open Lwt

module Impl = Vhd.F.From_file(Vhd_lwt.IO)
open Impl
open Vhd.F
open Vhd_lwt.IO

module In = From_input(Input)
open In

let get_vhd_vsize filename =
  Vhd_lwt.IO.openfile filename false >>= fun fd ->
  let rec loop = function
    | End -> return ()
    | Cons (hd, tl) ->
      begin match hd with
      | Fragment.Footer x ->
	let size = x.Footer.current_size in
	Printf.printf "%Ld\n" size;
	exit 1
      | _ ->
	()
      end;
      tl () >>= fun x ->
      loop x in
  openstream (Input.of_fd (Vhd_lwt.IO.to_file_descr fd)) >>= fun stream ->
  loop stream >>= fun () -> Vhd_lwt.IO.close fd

let _ =
  let t = get_vhd_vsize Sys.argv.(1) in
  Lwt_main.run t
