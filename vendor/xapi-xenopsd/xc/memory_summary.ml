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
open Xenops_utils

let xc = Xenctrl.interface_open() 

let hash = ref false
let delay = ref (-1.0)

let _ =

  Arg.parse [ "-hash", Arg.Set hash, "Use hashes";
              "-delay", Arg.Set_float delay, "Delay between updates" ]
    (fun x -> Printf.fprintf stderr "Ignoring argument: %s" x)
    "Display domains and memory use";

  if not !hash
  then Printf.printf "# time host_total host_free domainN_total domainN+1_total...\n";

  let ( +* ) = Int64.add and ( /* ) = Int64.div and ( ** ) = Int64.mul in
  let finished = ref false in
  while not !finished do
    finished := !delay < 0.;
    if !delay > 0. then ignore(Unix.select [] [] [] !delay);
    flush stdout;

    let physinfo = Xenctrl.physinfo xc in
    let one_page = 4096L in
    let total_pages = Int64.of_nativeint physinfo.Xenctrl.total_pages in
    let free_pages = Int64.of_nativeint physinfo.Xenctrl.free_pages +*
                     (Int64.of_nativeint physinfo.Xenctrl.scrub_pages) in

    let domains = Xenctrl.domain_getinfolist xc 0 in
    let domains = List.map
        (fun di ->
           di.Xenctrl.domid,
           Int64.of_nativeint di.Xenctrl.total_memory_pages
        )
        domains
    in

    if not !hash then begin
      Printf.printf "%s %Ld %Ld" (Date.to_string (Date.of_float (Unix.gettimeofday ()))) (total_pages ** one_page) (free_pages ** one_page);
      let domains = List.stable_sort (fun (a, _) (b, _) -> compare a b) domains in
      List.iter
        (fun (_, total) -> Printf.printf " %Ld" (total ** one_page))
        domains;
      Printf.printf "\n"
    end else begin
      Printf.printf "Total host memory: %Ld MiB\n\n" (total_pages /* 256L);

      let nhashes = 55 in
      let hashes pages =
        let n = int_of_float (Int64.to_float pages /.
                              (Int64.to_float total_pages) *. (float_of_int nhashes)) in
        let hashes = String.make n '#' in
        let spaces = String.make (nhashes - n) ' ' in
        hashes ^ spaces in

      Printf.printf "%10s %s (%Ld MiB)\n" "free"
        (hashes free_pages) (free_pages /* 256L);
      List.iter
        (fun (domid, total) ->
           Printf.printf "%10s %s (%Ld MiB)\n" (string_of_int domid)
             (hashes total) (total /* 256L)
        )
        domains
    end
  done
