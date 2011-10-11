open Debug

type lvcreate_t = {
  lvc_id : string;
  lvc_segments : Allocator.t
}

and lvrename_t = {
  lvmv_new_name : string;
}

and lvreduce_t = {
  lvrd_new_extent_count : int64;
}

and lvexpand_t = {
  lvex_segments : Allocator.t;
}

(** First string corresponds to the name of the LV. *)
and operation =
	| LvCreate of string * lvcreate_t
	| LvReduce of string * lvreduce_t
	| LvExpand of string * lvexpand_t
	| LvRename of string * lvrename_t
	| LvRemove of string
	| LvAddTag of string * Tag.t
	| LvRemoveTag of string * Tag.t

and sequenced_op = {
  so_seqno : int;
  so_op : operation
} with rpc

(** Marshal to and from a string *)
let redo_to_string (l : sequenced_op) = 
  let s = Marshal.to_string l [] in
  let len = String.length s in
  Printf.sprintf "%012d%s" len s

(** Return tuple of operation * int where int is total length of the marshalled operation *)
let redo_of_string s ofs = 
  let len = int_of_string (String.sub s ofs 12) in
  let res = String.sub s (ofs+12) len in
  ((Marshal.from_string res 0 : sequenced_op),(12+len))


exception OutOfSize of int

(** The initial pos is the absolute position of the first unset byte in the LV *)
let read_initial_pos fd offset =
  ignore(Unix.LargeFile.lseek fd offset Unix.SEEK_SET);
  let pos = try Int64.of_string (Unixext.really_read_string fd 12) with _ -> (Int64.add offset 12L) in
  pos
    
let write_initial_pos fd offset pos =
  ignore(Unix.LargeFile.lseek fd offset Unix.SEEK_SET);
  let pos_str = Printf.sprintf "%012Ld" pos in
  ignore(Unix.write fd pos_str 0 12)
  
let write fd offset size ops =
  let pos = read_initial_pos fd offset in
  let rec write ofs ops =
    match ops with
      | op::ops ->
	  let str = redo_to_string op in
	  debug ("LVM REDO: " ^ str);
	  let len = String.length str in
	  if (Int64.add ofs (Int64.of_int len)) > (Int64.add offset size) then
	    raise (OutOfSize op.so_seqno)
	  else begin
	    ignore(Unix.LargeFile.lseek fd ofs Unix.SEEK_SET);
	    ignore(Unix.write fd str 0 len);
	    let new_pos = Int64.add ofs (Int64.of_int len) in
	    write_initial_pos fd offset new_pos;
	    write new_pos ops
	  end
      | [] -> ()
  in write pos (List.rev ops)

let read fd offset size =
  debug "Redo.read";
  let end_ofs = read_initial_pos fd offset in
  let start_ofs = Int64.add offset 12L in
  let size = Int64.sub end_ofs start_ofs in
  debug (Printf.sprintf "start_ofs: %Ld end_ofs: %Ld size: %Ld" start_ofs end_ofs size);
  ignore(Unix.LargeFile.lseek fd start_ofs Unix.SEEK_SET);
  let string = Unixext.really_read_string fd (Int64.to_int size) in
  let rec read_ops pos ops =
    debug (Printf.sprintf "Reading from pos: %d" pos);
    if pos>=String.length string 
    then ops 
    else 
      let (op,length)=redo_of_string string pos in
      read_ops (pos+length) (op::ops)
  in
  let result = read_ops 0 [] in
  debug "Redo.read finished";
  result

let reset fd offset =
  write_initial_pos fd offset (Int64.add offset 12L)

(** Converts the redo operation to a human-readable string. *)
let redo_to_human_readable op =
	let lvcreate_t_to_string l =
		Printf.sprintf "{id:'%s', segments:[%s]}" l.lvc_id (Allocator.to_string l.lvc_segments) in
	let lvexpand_t_to_string l =
		Printf.sprintf "[%s]" (Allocator.to_string l.lvex_segments) in
	let opstr =
		match op.so_op with
			| LvCreate (name,lvc) -> Printf.sprintf "LvCreate(%s,%s)" name (lvcreate_t_to_string lvc)
			| LvRemove name -> Printf.sprintf "LvRemove(%s)" name
			| LvReduce (name,lvrd) -> Printf.sprintf "LvReduce(%s,%Ld)" name lvrd.lvrd_new_extent_count
			| LvExpand (name,lvex) -> Printf.sprintf "LvExpand(%s,%s)" name (lvexpand_t_to_string lvex)
			| LvRename (name,lvmv) -> Printf.sprintf "LvRename(%s,%s)" name lvmv.lvmv_new_name
			| LvAddTag (name,tag)	 -> Printf.sprintf "LvAddTag(%s,%s)" name (Tag.string_of tag)
			| LvRemoveTag (name,tag) -> Printf.sprintf "LvRemoveTag(%s,%s)" name (Tag.string_of tag) in
	Printf.sprintf "{seqno=%d; op=%s}" op.so_seqno opstr
