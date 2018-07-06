(* Copyright (C) 2014 Citrix Inc
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)
let (|>) a b = b a

open Gnt

let main shr_h dev_h =
  let share = Gntshr.share_pages_exn shr_h 0 1 true in
  let io_page_shr_side = Gntshr.(share.mapping) |> Io_page.to_cstruct in
  List.iter (fun r -> Printf.printf "Shared a page with gntref = %d\n%!" r) Gntshr.(share.refs);
  Printf.printf "Shared page(s) OK. Now trying to map.\n%!";
  let local_mapping = Gnttab.map_exn dev_h Gnttab.({domid=0; ref=List.hd Gntshr.(share.refs)}) true in
  let io_page_map_side = Gnttab.Local_mapping.(to_buf local_mapping) |> Io_page.to_cstruct in
  Printf.printf "Mapping OK. Now writing randow stuff in one side and check we have the same thing on the other side.\n%!";
  let random_string = String.create 4096 in
  let zero_string = String.make 4096 '\000' in
  let zero_string2 = String.make 4096 '\000' in
  Cstruct.blit_from_string random_string 0 io_page_shr_side 0 4096;
  Cstruct.blit_to_string io_page_shr_side 0 zero_string 0 4096;
  assert (zero_string = random_string);
  Printf.printf "I blitted random 4096 chars on the page, and read back the same, all OK!\n%!";
  Cstruct.blit_to_string io_page_map_side 0 zero_string2 0 4096;
  assert (zero_string2 = random_string);
  Printf.printf "I read the same as well from the map side...\n%!";
  Printf.printf "Now unmapping and unsharing everything.\n%!";
  Gnttab.unmap_exn dev_h local_mapping;
  Gntshr.munmap_exn shr_h share;
  Printf.printf "Now trying to share and map 10 pages as a vector.\n%!";
  let share = Gntshr.share_pages_exn shr_h 0 10 true in
  let io_page_shr_side = Gntshr.(share.mapping) |> Io_page.to_cstruct in
  let refs = Gntshr.(share.refs) in
  let grants = List.map (fun ref -> Gnttab.({domid=0; ref})) refs in
  let local_mapping = Gnttab.mapv_exn dev_h grants true in
  let io_page_map_side = Gnttab.Local_mapping.(to_buf local_mapping) |> Io_page.to_cstruct in
  let random_string = String.create (4096*10) in
  let zero_string = String.make (4096*10) '\000' in
  let zero_string2 = String.make (4096*10) '\000' in
  Cstruct.blit_from_string random_string 0 io_page_shr_side 0 (4096*10);
  Cstruct.blit_to_string io_page_shr_side 0 zero_string 0 (4096*10);
  assert (zero_string = random_string);
  Printf.printf "I blitted random 4096*10 chars on the page, and read back the same, all OK!\n%!";
  Cstruct.blit_to_string io_page_map_side 0 zero_string2 0 (4096*10);
  assert (zero_string2 = random_string);
  Printf.printf "I read the same as well from the map side...\n%!";
  Printf.printf "Success! Now unmapping and unsharing everything!\n%!";
  Gnttab.unmap_exn dev_h local_mapping;
  Gntshr.munmap_exn shr_h share

let just_grant_and_ungrant shr_h n =
  let share = Gntshr.share_pages_exn shr_h 0 n true in
  Gntshr.munmap_exn shr_h share

let _ =
  let shr_h = Gntshr.interface_open () in
  let dev_h = Gnttab.interface_open () in
  for i = 0 to 10000 do
    Printf.printf "%d " i;
    just_grant_and_ungrant shr_h 2
  done;
  Printf.printf "\n%!";
  Gntshr.interface_close shr_h;
  Gnttab.interface_close dev_h
