(* LVM uses uuids that aren't really proper uuids. This module manipulates them *)

type t = string

let format = [6; 4; 4; 4; 4; 4; 6] 

let charlist = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ!#"

let dev_random = "/dev/urandom"

let read_random n = 
  let ic = open_in_bin dev_random in
  try
    let result = Array.init n (fun _ -> input_byte ic) in
    close_in ic;
    result
  with e ->
    close_in ic;
    raise e

let create () =
(*  let bytes = read_random 32 in*)
  let s = String.make (32+6) '-' in
  let rec make i j n f =
    if n=0 then match f with
      | n'::ns -> make i (j+1) n' ns
      | _ -> ()
    else (s.[j] <- charlist.[Random.int 62]; make (i+1) (j+1) (n-1) f)
  in
  make 0 0 (List.hd format) (List.tl format);
    s

let add_hyphens str =
  let str2 = String.make (32+6) '-' in
  let foldfn (i,j) n = String.blit str i str2 j n; (i+n,j+n+1) in
  ignore(List.fold_left foldfn (0,0) format);
  str2

let remove_hyphens str =
  let str2 = String.create 32 in
  let foldfn (i,j) n = String.blit str i str2 j n; (i+n+1, j+n) in
  ignore(List.fold_left foldfn (0,0) format);
  str2
