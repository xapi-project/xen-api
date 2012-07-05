(*
Copyright (c) 2011, Mickaël Delahaye <mickael.delahaye@gmail.com>

Permission to use, copy, modify, and/or distribute this software for any purpose
with or without fee is hereby granted, provided that the above copyright notice
and this permission notice appear in all copies.

THE SOFTWARE IS PROVIDED “AS IS” AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND
FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS
OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF
THIS SOFTWARE.
*)

(** Example of CPU-time measurement with Oclock *)

let main () =
  try
    while true do
      Printf.printf "PID (0 for current process)? ";
      let pid = read_int () in
      begin try
        let clk = Oclock.getcpuclockid pid in
        let time = Oclock.gettime clk in
        Printf.printf "The process %d used %Ld ns of CPU time up until now.\n\n" pid time;
      with Invalid_argument s | Failure s ->
        Printf.printf "Error: %s\n" s;
      end;
    done;
  with End_of_file ->
    Printf.printf "Bye!\n"
  
let () =
  main ()
