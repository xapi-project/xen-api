open Absty
open Fun
open Listext

type stat = 
    | Read
    | Write
    | Visible
	
and striped_segment = {
  st_stripe_size : int64; (* In sectors *)
  st_stripes : (string * int64) list; (* pv name * start extent *)
}

and linear_segment = {
  l_pv_name : string;
  l_pv_start_extent : int64;
}

and segclass = 
  | Linear of linear_segment
  | Striped of striped_segment

and segment = 
    { s_start_extent : int64; 
      s_extent_count : int64;
      s_cls : segclass; }

and logical_volume = {
  name : string;
  id : string;
  tags : Tag.t list;
  status : stat list;
  segments : segment list;
} with rpc

let status_to_string s =
  match s with
    | Read -> "READ"
    | Write -> "WRITE"
    | Visible -> "VISIBLE"

let status_of_string s =
  match s with
    | "READ" -> Read
    | "WRITE" -> Write
    | "VISIBLE" -> Visible
    | _ -> failwith "Bad LV status string"

let sort_segments s =
  List.sort (fun s1 s2 -> compare s1.s_start_extent s2.s_start_extent) s

let write_to_buffer b lv =
  let bprintf = Printf.bprintf in
  bprintf b "\n%s {\nid = \"%s\"\nstatus = [%s]\n" lv.name lv.id 
    (String.concat ", " (List.map (o quote status_to_string) lv.status));
  if List.length lv.tags > 0 then 
    bprintf b "tags = [%s]\n" (String.concat ", " (List.map (quote ++ Tag.string_of) lv.tags));
  bprintf b "segment_count = %d\n\n" (List.length lv.segments);
  Listext.List.iteri
    (fun i s -> 
      bprintf b "segment%d {\nstart_extent = %Ld\nextent_count = %Ld\n\n"
	(i+1) s.s_start_extent s.s_extent_count;
       match s.s_cls with
	 | Linear l ->
	     bprintf b "type = \"striped\"\nstripe_count = 1\t#linear\n\n";
	     bprintf b "stripes = [\n\"%s\", %Ld\n]\n}\n" l.l_pv_name l.l_pv_start_extent
	 | Striped st ->
	     let stripes = List.length st.st_stripes in
	     bprintf b "type = \"striped\"\nstripe_count = %d\nstripe_size = %Ld\n\nstripes = [\n"
	       stripes st.st_stripe_size;
	     List.iter (fun (pv,offset) -> bprintf b "%s, %Ld\n" (quote pv) offset) st.st_stripes;
	     bprintf b "]\n}\n") lv.segments;
  bprintf b "}\n"

let segment_of_metadata name config =
  let start_extent = expect_mapped_int "start_extent" config in
  let extent_count = expect_mapped_int "extent_count" config in
  let ty = expect_mapped_string "type" config in
  if ty<>"striped" then failwith (Printf.sprintf "Cannot handle LV segment type '%s'" ty);
  let stripes = expect_mapped_array "stripes" config in
  let rec handle_stripes ss =
    match ss with
      | name::offset::rest ->
	  let name = expect_string "name" name in
	  let offset = expect_int "offset" offset in
	  (name,offset)::handle_stripes rest
      | _ -> []
  in 
  {s_start_extent = start_extent;
   s_extent_count = extent_count;
   s_cls = 
      if List.length stripes = 2 then 
	match stripes with 
	  | [name;offset] ->
	      Linear { l_pv_name=expect_string "name" name;
		       l_pv_start_extent=expect_int "offset" offset }
	  | _ -> failwith "Invalid format of segment"
      else 
	let stripe_size = expect_mapped_int "stripe_size" config in
	let stripes = (handle_stripes stripes) in
	Striped {st_stripe_size=stripe_size;
		 st_stripes=stripes}
  }

(** Builds a logical_volume structure out of a name and metadata. *)
let of_metadata name config =
	let id = expect_mapped_string "id" config in
	let status = map_expected_mapped_array "status"
		(fun a -> status_of_string (expect_string "status" a)) config in
	let tags =
		List.map Tag.of_string
			(if List.mem_assoc "tags" config
			 then map_expected_mapped_array "tags" (expect_string "tags") config
			 else []) in
	let segments = filter_structs config in
	let segments = List.map
		(fun (a,_) ->
			 segment_of_metadata a (expect_mapped_struct a segments)) segments in
	{ name = name;
	  id = id;
	  status = status;
	  tags = tags;
	  segments = sort_segments segments }

let allocation_of_segment s =
  match s.s_cls with
    | Linear l ->
	[(l.l_pv_name, (l.l_pv_start_extent, s.s_extent_count))]
    | Striped st ->
(* LVM appears to always round up the number of extents allocated such
   that it's divisible by the number of stripes, so we always fully allocate
   each extent in each PV. Let's be tolerant to broken metadata when this
   isn't the case by rounding up rather than down, so partially allocated
   extents are included in the allocation *)
	let extent_count = s.s_extent_count in
	let nstripes = Int64.of_int (List.length st.st_stripes) in
	List.map (fun (name,start) ->
		    let allocated_extents = 
		      Int64.div 
			(Int64.sub 
			   (Int64.add 
			      extent_count nstripes) 1L) nstripes
		    in
		    (name,(start,allocated_extents)))
	  (st.st_stripes)

let allocation_of_lv lv =
  List.flatten 
    (List.map allocation_of_segment lv.segments)

let size_in_extents lv =
  List.fold_left (Int64.add) 0L
    (List.map (fun seg -> seg.s_extent_count) lv.segments)
	    
let reduce_size_to lv new_seg_count =
  let cur_size = size_in_extents lv in
  Debug.debug "Beginning reduce_size_to:";
  if cur_size < new_seg_count then (failwith (Printf.sprintf "Cannot reduce size: current size (%Ld) is less than requested size (%Ld)" cur_size new_seg_count));
  let rec doit segs left acc =
    match segs with 
      | s::ss ->
	  Debug.debug (Printf.sprintf "Lv.reduce_size_to: s.s_start_extent=%Ld s.s_extent_count=%Ld left=%Ld" 
			  s.s_start_extent s.s_extent_count left);
	  if left > s.s_extent_count then
	    doit ss (Int64.sub left s.s_extent_count) (s::acc)
	  else
	    {s with s_extent_count = left}::acc
      | _ -> acc
  in
  {lv with segments = sort_segments (doit lv.segments new_seg_count [])}

let increase_allocation lv new_segs =
  {lv with segments = sort_segments (lv.segments @ new_segs)}
