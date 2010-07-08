(** Physical Volume module *)

open Absty

(** Start with the meta-metadata - this is how we actually locate the 
    metadata on disk. It's a bit backwards, because a PV is part of a 
    volume group, but it's the PV that contains the volume group info *)

open Lvmmarshal

(** Dummy mode stuff *)

let dummy_fname dev ty =
  let fname = Printf.sprintf "%s/%s/%s" (!Constants.dummy_base) dev ty in
  let basedir = Filename.dirname fname in
  Unixext.mkdir_rec basedir 0o755;
  fname

    
(** Label bits and pieces *)

module Label = struct 
  type label_header = {
    id : string; (* 8 bytes, equal to label_id in Constants *)
    sector : int64;
    crc : int32;
    offset : int32;
    ty : string (* 8 bytes, equal to "LVM2 001" - Constants.label_type*)
  }
      
  and disk_locn = {
    dl_offset : int64;
    dl_size : int64;
  }

  and pv_header = {
    pvh_id : string; (* 32 bytes, 'uuid' *)
    pvh_device_size : int64;
    pvh_extents: disk_locn list;
    pvh_metadata_areas: disk_locn list;
  }

  and t = {
    device : string;
    label_header : label_header;
    pv_header : pv_header;
  } with rpc 

  let unmarshal_header b0 =
    let id,b = unmarshal_string 8 b0 in
    let sector_xl,b = unmarshal_uint64 b in
    let crc_xl,b = unmarshal_uint32 b in
    let offset,b = unmarshal_uint32 b in
    let ty,b = unmarshal_string 8 b in
    let crc_check = String.sub (fst b0) (20 + snd b0) (Constants.label_size - 20) in (* wtf? *)
    let calculated_crc = Crc.crc crc_check Crc.initial_crc in
    if calculated_crc <> crc_xl then
      failwith "Bad checksum in PV Label";
    {id=id;
     sector=sector_xl;
     crc=crc_xl;
     offset=offset;
     ty=ty}
      
  let find_label dev =
    let fd = Unix.openfile (if !Constants.dummy_mode then dummy_fname dev "pvh" else dev) [Unix.O_RDONLY] 0o000 in
    let buf = really_read fd (if !Constants.dummy_mode then (Constants.sector_size * 2) else Constants.label_scan_size) in
    Unix.close fd;
    let rec find n =
      if n>3 then failwith "No label found" else begin
	let b = (buf,n*Constants.sector_size) in
	let (s,b') = unmarshal_string 8 b in
	Printf.fprintf stderr "String='%s' (looking for %s)\n" s Constants.label_id;
	if s=Constants.label_id then (unmarshal_header b,b) else find (n+1)
      end
    in find 0
    
  let label_header_to_ascii label =
    Printf.sprintf "id: %s\nsector: %Ld\ncrc: %ld\noffset: %ld\nty: %s\n" label.id label.sector label.crc label.offset label.ty
      
  let read_pv_header label b =
    let b = skip (Int32.to_int label.offset) b in
    let id,b = unmarshal_string 32 b in
    let size,b = unmarshal_uint64 b in
    let rec do_disk_locn b acc =
      let offset,b = unmarshal_uint64 b in
      if offset=0L 
      then (List.rev acc,skip 8 b) 
      else 
	let size,b = unmarshal_uint64 b in
	do_disk_locn b ({dl_offset=offset; dl_size=size}::acc)
    in 
    let disk_areas,b = do_disk_locn b [] in
    let disk_areas2,b = do_disk_locn b [] in
    { pvh_id=Lvm_uuid.add_hyphens id;
      pvh_device_size=size;
      pvh_extents=disk_areas;
      pvh_metadata_areas=disk_areas2},b

  let pvh_to_ascii pvh =
    let disk_area_list_to_ascii l =
      (String.concat "," (List.map (fun da -> Printf.sprintf "{offset=%Ld,size=%Ld}" da.dl_offset da.dl_size) l)) in  
    Printf.sprintf "pvh_id: %s\npvh_device_size: %Ld\npvh_areas1: %s\npvh_areas2: %s\n"
      pvh.pvh_id pvh.pvh_device_size 
      (disk_area_list_to_ascii pvh.pvh_extents)
      (disk_area_list_to_ascii pvh.pvh_metadata_areas)

  let write_label_and_pv_header l =
    let label = l.label_header in
    let pvh = l.pv_header in
    let dev = l.device in

    let header = (String.make Constants.sector_size '\000', 0) in (* label header is 1 sector long *)
    let pos = Int64.mul label.sector (Int64.of_int Constants.sector_size) in
    assert(label.offset=32l);
    
    let header = marshal_string header label.id in
    let header = marshal_int64  header label.sector in
    
    let crc_pos = header in (* Set later! *)
    let header = marshal_int32  header 0l in
    
    let (do_crc_str,do_crc_from) = header in
    let header = marshal_int32  header label.offset in
    let header = marshal_string header label.ty in
    
    assert(snd header = 32);

    Debug.debug (Printf.sprintf "write_label_and_pv_header:\nPV header:\n%s" (pvh_to_ascii pvh));

    (* PV header *)
    let header = marshal_string header (Lvm_uuid.remove_hyphens pvh.pvh_id) in
    let header = marshal_int64 header pvh.pvh_device_size in
    
    let do_disk_locn header l =       
      let header = List.fold_left (fun header e ->
        let header = marshal_int64 header e.dl_offset in
        marshal_int64 header e.dl_size) header l
      in
      marshal_int64 (marshal_int64 header 0L) 0L
    in
    
    let header = do_disk_locn header pvh.pvh_extents in
    let header = do_disk_locn header pvh.pvh_metadata_areas in
    
    (* Now calc CRC *)
    let crc = Crc.crc (String.sub do_crc_str do_crc_from (Constants.label_size - do_crc_from)) Crc.initial_crc in
    ignore(marshal_int32 crc_pos crc);
    
    let fd = 
      if !Constants.dummy_mode then begin
	let fname = dummy_fname dev "pvh" in 
	Unix.openfile fname [Unix.O_RDWR; Unix.O_DSYNC; Unix.O_CREAT] 0o644
      end else begin
	let fd = Unix.openfile dev [Unix.O_RDWR; Unix.O_DSYNC] 0o000 in
	fd
      end
    in

    ignore(Unix.LargeFile.lseek fd pos Unix.SEEK_SET);
      
    let str = fst header in
    let len = String.length str in
    if len <> (Unix.write fd str 0 len) then failwith "Short write!";
    
    Unix.close fd

  let get_metadata_locations label = 
    label.pv_header.pvh_metadata_areas

  let get_pv_id label =
    label.pv_header.pvh_id

  let get_device label = 
    label.device

  let find device =
    let label,b = find_label device in
    let pvh,b' = read_pv_header label b in    
    { device = device;
      label_header = label;
      pv_header = pvh; }
      
  let create device id size extents_start extents_size mda_start mda_size =
    let label = {
      id=Constants.label_id;
      sector=1L;
      crc=0l;
      offset=32l;
      ty=Constants.label_type;
    } in
    let pvh = {
      pvh_id=id;
      pvh_device_size=size;
      pvh_extents=[{dl_offset=(Int64.add mda_start mda_size); dl_size=0L}];
      pvh_metadata_areas=[{dl_offset=mda_start; dl_size=mda_size}];
    } in
    { device = device;
      label_header = label;
      pv_header = pvh }

  let to_ascii label =
    Printf.sprintf "Label header:\n%s\nPV Header:\n%s\n" 
      (label_header_to_ascii label.label_header)
      (pvh_to_ascii label.pv_header)

end
    
module MDAHeader = struct
  let mda_header_size = Constants.sector_size

  type mda_raw_locn = {
    mrl_offset: int64;
    mrl_size: int64;
    mrl_checksum: int32;
    mrl_filler: int32;
  }

  and mda_header = {
    mdah_checksum : int32;
    mdah_magic : string;
    mdah_version : int32;
    mdah_start: int64;
    mdah_size: int64;
    mdah_raw_locns : mda_raw_locn list;
  } with rpc

  let unmarshal_mda_header device location =
    let offset,fd = 
      if !Constants.dummy_mode 
      then (0L,Unix.openfile (dummy_fname device "mdah") [Unix.O_RDONLY] 0o000) 
      else (location.Label.dl_offset,Unix.openfile device [Unix.O_RDONLY] 0o000) in
    ignore(Unix.LargeFile.lseek fd offset Unix.SEEK_SET);
    let buf = really_read fd (mda_header_size) in
    Unix.close fd;
    let checksum,b = unmarshal_uint32 (buf,0) in
    let magic,b = unmarshal_string 16 b in
    let version,b = unmarshal_uint32 b in
    let start,b = unmarshal_uint64 b in
    let size,b = unmarshal_uint64 b in
    let rec read_raw_locns b acc =
      let offset,b = unmarshal_uint64 b in
      let size,b = unmarshal_uint64 b in
      let checksum,b = unmarshal_uint32 b in
      let filler,b = unmarshal_uint32 b in
      if (offset=0L) 
      then (List.rev acc),b
      else 
	read_raw_locns b ({mrl_offset=offset;mrl_size=size;mrl_checksum=checksum;mrl_filler=filler}::acc)
    in
    let raw_locns,b = read_raw_locns b [] in
    let crc_to_check = String.sub buf 4 (mda_header_size - 4) in
    let crc = Crc.crc crc_to_check Crc.initial_crc in
    if crc <> checksum then
      failwith "Bad checksum in MDA header";
    {mdah_checksum=checksum;
     mdah_magic=magic;
     mdah_version=version;
     mdah_start=start;
     mdah_size=size;
     mdah_raw_locns=raw_locns}

  let to_ascii mdah =
    let rl2ascii r = Printf.sprintf "{offset:%Ld,size:%Ld,checksum:%ld,filler:%ld}" r.mrl_offset r.mrl_size r.mrl_checksum r.mrl_filler in
    Printf.sprintf "checksum: %ld\nmagic: %s\nversion: %ld\nstart: %Ld\nsize: %Ld\nraw_locns:[%s]\n"
      mdah.mdah_checksum mdah.mdah_magic mdah.mdah_version mdah.mdah_start mdah.mdah_size (String.concat "," (List.map rl2ascii mdah.mdah_raw_locns))
          
  let write_mda_header mdah device  =
    Debug.debug "Writing MDA header";
    Debug.debug (Printf.sprintf "Writing: %s" (to_ascii mdah));
    let realheader = (String.make mda_header_size '\000', 0) in (* Mda header is 1 sector long *)
    let header = marshal_int32 realheader 0l in (* Write the checksum later *)
    let header = marshal_string header mdah.mdah_magic in
    let header = marshal_int32 header mdah.mdah_version in
    let header = marshal_int64 header mdah.mdah_start in
    let header = marshal_int64 header mdah.mdah_size in
    let write_raw_locn header locn =
      let header = marshal_int64 header locn.mrl_offset in
      let header = marshal_int64 header locn.mrl_size in
      let header = marshal_int32 header locn.mrl_checksum in
      let header = marshal_int32 header locn.mrl_filler in
      header
    in
    let header = List.fold_left write_raw_locn header mdah.mdah_raw_locns in
    let header = write_raw_locn header {mrl_offset=0L; mrl_size=0L; mrl_checksum=0l; mrl_filler=0l} in
    let crcable = String.sub (fst realheader) 4 (mda_header_size - 4) in
    let crc = Crc.crc crcable Crc.initial_crc in
    let _ = marshal_int32 realheader crc in
    
    let fd = 
      if !Constants.dummy_mode then begin
	Unix.openfile (dummy_fname device "mdah") [Unix.O_RDWR; Unix.O_DSYNC; Unix.O_CREAT] 0o644
      end else begin
        let fd = Unix.openfile device [Unix.O_RDWR; Unix.O_DSYNC] 0o000 in
	ignore(Unix.LargeFile.lseek fd mdah.mdah_start Unix.SEEK_SET);
	fd
      end
    in
    let written = Unix.write fd (fst header) 0 mda_header_size in
    if written <> Constants.sector_size then failwith "Wrote short!";
    Unix.close fd
      
	let read_md dev mdah n =
		(* debug *)
		let oc = open_out "/tmp/hdr" in
		Printf.fprintf oc "%s\n%!" (to_ascii mdah);
		close_out oc;

		let locn = List.nth mdah.mdah_raw_locns n in
		let fd =
			if !Constants.dummy_mode then begin
				Unix.openfile (dummy_fname dev "md") [Unix.O_RDONLY] 0o000
			end else begin
				let fd = Unix.openfile dev [Unix.O_RDONLY] 0o000 in
				ignore(Unix.LargeFile.lseek fd (Int64.add mdah.mdah_start locn.mrl_offset) Unix.SEEK_SET);
				fd
			end
		in
		let md =
			(* Include terminating \0 in this string.
			 * The checksum calculation in original lvm does so, too.*)
			if(Int64.add locn.mrl_offset locn.mrl_size > mdah.mdah_size)
			then (* wrap around *)
				let firstbit = Int64.to_int (Int64.sub mdah.mdah_size locn.mrl_offset) in
				let firstbitstr = really_read fd firstbit in
				let secondbit = (Int64.to_int locn.mrl_size) - firstbit in
				if not !Constants.dummy_mode then ignore(Unix.LargeFile.lseek fd (Int64.add mdah.mdah_start 512L) Unix.SEEK_SET);
				let secondbitstr = really_read fd secondbit in
				firstbitstr ^ secondbitstr
			else
				really_read fd (Int64.to_int locn.mrl_size) in
		let checksum = Crc.crc md Crc.initial_crc in
		Unix.close fd;
		if checksum <> locn.mrl_checksum then
			Printf.fprintf stderr "Checksum invalid in metadata: Found %lx, expecting %lx\n" checksum locn.mrl_checksum;
		md
      
  let write_md device mdah md =
    (* Find the current raw location of the metadata, assuming there's only one copy *)
    let current = List.hd mdah.mdah_raw_locns in
    
    (* Find the new place to write (as an offset from the position of the metadata area)
     * LVM always rounds up to the next sector, so we'll do the same. *)
    let newpos = Utils.int64_round_up (Int64.add current.mrl_offset current.mrl_size) 512L in

    (* Check if we've gone outside the mda *)
    let newpos = 
      if newpos >= mdah.mdah_size then
	(Int64.add 512L (Int64.sub newpos mdah.mdah_size))
      else 
	newpos
    in

    (* debug *)
    let oc = open_out "/tmp/hdr" in
    Printf.fprintf oc "%s\n%!" (to_ascii mdah);
    close_out oc;

    (* Add on the position of the metadata area *)
    let absnewpos = Int64.add newpos mdah.mdah_start in

    let size = String.length md in

    let fd = 
      if !Constants.dummy_mode then begin
	Unix.openfile (dummy_fname device "md") [Unix.O_RDWR; Unix.O_DSYNC; Unix.O_CREAT] 0o644
      end else begin
	let fd = Unix.openfile device [Unix.O_RDWR; Unix.O_DSYNC] 0o000 in
	ignore(Unix.LargeFile.lseek fd absnewpos Unix.SEEK_SET);          
	fd
      end
    in

    (* Check whether we're going to wrap or not *)
    if Int64.add newpos (Int64.of_int size) > mdah.mdah_size 
    then begin
      let firstbit = Int64.to_int (Int64.sub mdah.mdah_size newpos) in
      if (Unix.write fd md 0 firstbit) <> firstbit then failwith "Wrote short!";
      let secondbit = size - firstbit in
      if not !Constants.dummy_mode then ignore(Unix.LargeFile.lseek fd (Int64.add 512L mdah.mdah_start) Unix.SEEK_SET);
      if (Unix.write fd md firstbit secondbit) <> secondbit then failwith "Wrote short!"; 
      Unix.close fd;
    end else begin    
      if (Unix.write fd md 0 size) <> size then 
	failwith "Wrote short!";
      Unix.close fd;
    end;


    (* Now we have to update the crc and pointer to the metadata *)
    
    let checksum = Crc.crc md Crc.initial_crc in
    let new_raw_locn = {
      mrl_offset=newpos;
      mrl_size=Int64.of_int size;
      mrl_checksum=checksum;
      mrl_filler=0l;
    } in

    let mdah = {mdah with mdah_raw_locns=[new_raw_locn]} in
    write_mda_header mdah device;
    mdah


  let create_blank () =
    let mda_raw_locn = {
      mrl_offset = 512L;
      mrl_size = 0L;
      mrl_checksum = 0l;
      mrl_filler=0l;
    } in
    let mda_header = {
      mdah_checksum = 0l;
      mdah_magic = Constants.fmtt_magic;
      mdah_version = 1l;
      mdah_start = Constants.mdah_start;
      mdah_size = Constants.mdah_size;
      mdah_raw_locns = [mda_raw_locn]
    } in
    mda_header
end

  (** Here's the actual PV data that's part of the volume group *)
  
type status = 
    | Allocatable
	
and physical_volume = {
  name : string;
  id : string;
  dev : string;
  real_device : string; (* Actual device we're reading/writing to/from *)
  status : status list;
  dev_size : int64;
  pe_start : int64;
  pe_count : int64;
  label : Label.t;  (* The one label for this PV *)
  mda_headers : MDAHeader.mda_header list; 
} with rpc 

let status_to_string s =
  match s with
    | Allocatable -> "ALLOCATABLE"

let status_of_string s =
  match s with
    | "ALLOCATABLE" -> Allocatable
    | _ -> failwith "Bad status string"

let write_to_buffer b pv =
  let bprintf = Printf.bprintf in
  bprintf b "\n%s {\nid = \"%s\"\ndevice = \"%s\"\n\n" pv.name pv.id pv.dev;
  bprintf b "status = [%s]\ndev_size = %Ld\npe_start = %Ld\npe_count = %Ld\n}\n" 
    (String.concat ", " (List.map (o quote status_to_string) pv.status))
    pv.dev_size pv.pe_start pv.pe_count

let of_metadata name config pvdatas =
  let id = expect_mapped_string "id" config in
  let device = expect_mapped_string "device" config in
  let status = map_expected_mapped_array "status" 
    (fun a -> status_of_string (expect_string "status" a)) config in
  let dev_size = expect_mapped_int "dev_size" config in
  let pe_start = expect_mapped_int "pe_start" config in
  let pe_count = expect_mapped_int "pe_count" config in
  let label,mdahs = 
    try 
      let res = List.find (fun (label,mdahs) -> id=Label.get_pv_id label) pvdatas in
      Printf.fprintf stderr "Found cached PV label data\n";
      res
    with Not_found -> 
      try
	Printf.fprintf stderr "No cached PV data found - loading from device '%s'\n" device;
	let label = Label.find device in
	let mda_locs = Label.get_metadata_locations label in
	let mdahs = List.map (MDAHeader.unmarshal_mda_header device) mda_locs in
	(label,mdahs)
      with e ->
	Printf.fprintf stderr "Error: Could not find label and/or MDA headers on device '%s'\n" 
	  device;
	raise e
  in
  let real_device = Label.get_device label in
  if real_device <> device then
    Printf.fprintf stderr "WARNING: PV.device and real_device are not the same";
  {name=name;
   id=id;
   dev=device;
   real_device=real_device;
   status=status;
   dev_size=dev_size;
   pe_start=pe_start;
   pe_count=pe_count;
   label=label;
   mda_headers=mdahs;
  }

(** Find the metadata area on a device and return the text of the metadata *)
let find_metadata device =
  let label = Label.find device in
  Debug.debug (Printf.sprintf "Label found: \n%s\n" 
    (Label.to_ascii label));
  let mda_locs = Label.get_metadata_locations label in
  let mdahs = List.map (MDAHeader.unmarshal_mda_header device) mda_locs in
  let mdt = MDAHeader.read_md device (List.hd mdahs) 0 in  
  (mdt, (label, mdahs))

let human_readable pv =
  let label=pv.label in
  let b=Buffer.create 1000 in
  let label_str=Label.to_ascii label in
  let mdah_ascii = String.concat "\n" (List.map MDAHeader.to_ascii pv.mda_headers) in
  write_to_buffer b pv;
  Printf.sprintf "Label:\n%s\nMDA Headers:\n%s\n%s\n" 
    label_str mdah_ascii (Buffer.contents b)

let create_new dev name =
  let size = 
    if !Constants.dummy_mode then 
      Constants.tib
    else 
      let fd = Unix.openfile dev [Unix.O_RDONLY] 0 in
      let size = Unixext.blkgetsize64 fd in
      Unix.close fd;
      size
  in
  (* Arbitrarily put the MDA at 4096. We'll have a 10 meg MDA too *)
  let dev_size = Int64.div size (Int64.of_int Constants.sector_size) in
  let mda_pos = Constants.mdah_start in
  let mda_len = Constants.mdah_size in
  let pe_start_byte = 
    Utils.int64_round_up (Int64.add mda_pos mda_len) Constants.pe_align in
  let pe_start_sector = Int64.div pe_start_byte 
    (Int64.of_int Constants.sector_size) in
  let pe_count = Int64.div (Int64.sub size pe_start_byte) Constants.extent_size in
  let mda_len = Int64.sub pe_start_byte mda_pos in
  let id=Lvm_uuid.create () in
  let label = Label.create dev id size pe_start_sector 
    (Int64.mul pe_count Constants.extent_size)
    mda_pos mda_len in
  let mda_header = MDAHeader.create_blank () in
  Label.write_label_and_pv_header label;
  MDAHeader.write_mda_header mda_header dev;
  let pv = { name=name;
	     id=id;
	     dev=dev;
	     real_device=dev;
	     status=[Allocatable];
	     dev_size = dev_size;
	     pe_start=pe_start_sector;
	     pe_count=pe_count;
	     label = label;
	     mda_headers = [mda_header]; }
  in
  pv
      
      
