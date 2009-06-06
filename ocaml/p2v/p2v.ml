(***
  P2V SERVER
 ***)

open Pervasiveext
open Stringext
open Client
open Opt
open Unixext

module D = Debug.Debugger(struct let name = "p2v" end)
open D

let listen_addr = Unix.ADDR_INET(Unix.inet_addr_of_string "0.0.0.0", 81)

(* connect and get session id *)
let xs = Xs.domain_open ()
let host, port, session_id, vm = Domain.guest_get_api_access ~xs 

(* get vm and session references *)
let this_vm = Ref.of_string vm
and session_id = Ref.of_string session_id
and rpc xml = Xmlrpcclient.do_secure_xml_rpc ~host ~version:"1.1" ~port ~path:"/" xml

let assert_dir path mode =
    if not (Sys.file_exists path) then Unix.mkdir path mode

type fs_metadata = { mounted_at : string }
type fs_metadata_hashtbl = (string, fs_metadata) Hashtbl.t
let new_fs_metadata mntpoint = { mounted_at = mntpoint }
let fs_metadata : fs_metadata_hashtbl = Hashtbl.create 10

(* running external commands/utility functions *)
exception SynchronousCommandError of Unix.process_status

let run_sync command =
    debug "Executing %s" command;
    match Unix.system command with
        | Unix.WEXITED(x) -> x
        | x -> raise (SynchronousCommandError x)

let run_checked_sync command =
    match run_sync command with
        | 0 -> ()
        | n -> raise (SynchronousCommandError (Unix.WEXITED n))

let unix_really_write oc s = Unix.write oc s 0 (String.length s)

module FsTab = struct
    type entry = { volume : string; 
                   mntpoint : string; 
                   fstype : string; 
                   options : string list;
                   dump : int; pass : int }

    type t = entry list

    let new_fstab_entry volume mntpoint fstype options dump pass =
        { volume=volume; mntpoint=mntpoint; fstype=fstype;
          options=options; dump=dump; pass=pass }

    let entry_of_metadata volume metadata =
        new_fstab_entry volume metadata.mounted_at "ext3" ["defaults"] 0 0

    let entry_of_string line =
        let line = String.strip String.isspace (
            if String.contains line '#'  then
                String.sub line 0 (String.index line '#')
            else
                line
        ) in
        let parts = String.split_f String.isspace line in
        match parts with 
	  | [ volume; mntpoint; fstype; options; dump; pass ] ->
              let options = String.split ',' options in
              let dump = int_of_string dump in
              let pass = int_of_string pass in
              new_fstab_entry volume mntpoint fstype options dump pass
	  | _ -> failwith ("malformed fstab entry "^line)

    let read filename =
        let fd = open_in filename in
        let rec _read () =
            try
                let line = input_line fd in
                let line = String.strip String.isspace line in
                if String.startswith "#" line then
                    _read ()
                else
                    (entry_of_string line)::(_read ())
            with
                | End_of_file -> []
        in
        finally _read (fun () -> close_in fd)

    let string_of_entry e =
        let options = String.concat "," e.options in
        Printf.sprintf "%s %s %s %s %d %d" e.volume e.mntpoint e.fstype options e.dump e.pass

    let is_local e = 
        String.startswith "/dev" e.volume || String.startswith "LABEL=" e.volume

    let filter fn es = List.filter fn es

    let update original updates =
        let select e =
            let selected = ref e in
            List.iter (fun e2 -> if e2.mntpoint = e.mntpoint then selected := e2 else ()) updates;
            !selected
        in
        let mapped = List.map select original in

        (* add new entries: *)
        let exists l mntpoint = 
            List.fold_left (fun x e -> x || (e.mntpoint = mntpoint)) false l in
        let new_entries = filter (fun x -> not (exists original x.mntpoint)) updates in
        mapped @ new_entries
end

(* XXX copied from xapi/helpers.ml: should move to util *)
let get_process_output ?(handler=(fun _ _ -> failwith "internal error")) cmd : string =
    let inchan = Unix.open_process_in cmd in

    let buffer = Buffer.create 1024
    and buf = String.make 1024 '\000' in

    let rec read_until_eof () =
        let rd = input inchan buf 0 1024 in
        if rd = 0 then
            ()
        else begin
            Buffer.add_substring buffer buf 0 rd;
            read_until_eof ()
        end
    in
    (* Make sure an exception doesn't prevent us from waiting for the child process *)
    read_until_eof ();
    match Unix.close_process_in inchan with
    | Unix.WEXITED 0 -> Buffer.contents buffer
    | x -> raise (SynchronousCommandError x)

module RuntimeEnv = struct
    exception AdminInterfaceError
    exception ErrorFindingIP

    let get_iface_ip iface =
        let ifconfig = get_process_output ("/sbin/ifconfig " ^ iface) in 
        let lines = String.split '\n' ifconfig in
        let ip_substr x = 
            let plain = String.strip String.isspace x in
            let fst = String.index plain ':' + 1 in
            let len = (String.index_from plain fst ' ') - fst in
            String.sub plain fst len in
        match List.filter (fun x ->String.has_substr x "inet addr:") lines with
            | [ip] -> ip_substr ip
            | _ -> raise ErrorFindingIP

    let configure_networking session_id rpc =
        let set_other_config_value ~session_id ~rpc ~self ~key ~value =
            let () = try
                Client.VM.remove_from_other_config ~session_id ~rpc ~key ~self:this_vm
            with exn -> () in
            Client.VM.add_to_other_config ~session_id ~rpc ~self:this_vm ~key ~value
        in
        run_checked_sync "dhclient eth0";
        (* write our IP address to the other_config field so that the client
           knows how to connect to us. *)
        let x = get_iface_ip "eth0" in
        debug "got ip: %s; writing to other-config" x;
        set_other_config_value ~session_id ~rpc ~self:this_vm ~key:"ip" ~value:x
end

module Compression = struct
    type compression = Uncompressed | Gzip | Bzip2

    let of_string = function
        | "uncompressed" -> Uncompressed
        | "gzip" -> Gzip 
        | "bzip2" -> Bzip2
	| _ -> failwith "Unknown compression type"

    let tar_parameter_of = function
        | Uncompressed -> ""
        | Gzip -> "z"
        | Bzip2 -> "j"
end

module Filesystem = struct
    type filesystem = Ext3 | Swap

    let make volume fs fsopts =
        let creation_tool = match fs with
            | Ext3 -> "mkfs.ext3"
            | Swap -> "mkswap" in
        let device = Printf.sprintf "/dev/%s" volume in
        let optstring = match fsopts with
            | None -> ""
            | Some x -> "-O "^x
        in
        run_checked_sync (Printf.sprintf "%s %s %s" creation_tool optstring device)

    let of_string = function
        | "ext3" -> Ext3
        | "swap" -> Swap
	| _ -> failwith "Unknown filesystem type"

    let string_of = function
        | Ext3 -> "ext3"
        | Swap -> "swap"
end

(** wait for a file to appear.  Useful for waiting on devices appearing in 
    /sys/block. *)
let rec wait_on_file fname = function
    | 0     -> raise Not_found 
    | tries ->
        if Sys.file_exists fname then 
            ()
        else begin 
            Unix.sleep 1; wait_on_file fname (tries - 1) 
        end

let umount mntpoint =
    run_checked_sync ("umount " ^ mntpoint)

(* Mounting and unmounting devices: *)
type mount_action = { options : string list;
                      fstype : string option;
                      mntpoint : string option;
                      src : string }

let new_mount_action ?options ?fstype ?mntpoint src =
    let options = match options with 
        | None -> []
        | Some x -> x in
    { options = options ; fstype = fstype ; mntpoint = mntpoint;
      src = src } 

let mount action =
    let mkname prefix =
        (* make unique mountpoints *)
        let i = ref 1 in
        let _mkname x = prefix ^ "-" ^ (string_of_int x) in
        let () = 
            while Sys.file_exists (_mkname !i) do
                i := !i + 1
            done
        in _mkname !i
    in
    let optionstring =
        if action.options = [] then "" else "-o " ^ (String.concat "," action.options) in
    let fstype_string = match action.fstype with
        | None -> ""
        | Some fstype -> "-t " ^ fstype in
    let mntpoint = match action.mntpoint with
        | None ->
            let name = mkname "/tmp/withmnt" in
            let () = assert_dir name 0o700 in
            name
        | Some x -> x
    in
    let mountcmd = 
        Printf.sprintf "mount %s %s %s %s" fstype_string optionstring action.src mntpoint in
    debug "mount: about to execute %s" mountcmd;
    ignore (run_checked_sync mountcmd);
    mntpoint

let with_mounted actions fn =
    let rec _with_mounted actions mountpoints fn =
        let cleanup x actual_mount () =
            let mntpoint = unbox actual_mount.mntpoint in
            umount mntpoint;
            if x.mntpoint = None then Unix.rmdir mntpoint
        in
        match actions with
            | [] -> 
                fn mountpoints
            | x::xs -> 
                let actual_mount = { x with mntpoint = Some (mount x) } in
                finally (fun () -> _with_mounted xs (actual_mount::mountpoints) fn) (cleanup x actual_mount)
    in
    _with_mounted actions [] fn

let with_single_mount action fn = 
    let call a = 
        match a with
            | [x] -> fn (unbox x.mntpoint)
            | _   -> failwith "mount gave unexpected return value for with_single_mount"
    in
    with_mounted [ action ] call

(** Get an argument from an association list, writing out appropriate HTTP
    error codes, with a useful body, and raising an appropriate exception *)
let optional_arg query arg =
    try 
        Some (List.assoc arg query)
    with
        Not_found -> None

let select_arg bio query arg =
    try
        List.assoc arg query
    with
        Not_found as e -> begin
            let s = Buf_io.fd_of bio in
            Http.output_http s (Http.http_500_internal_error);
            error "HTTP 500: An error occurred: a required parameter '%s' was not present in the RPC - aborting.  This is likely a bug in your P2V client." arg;
            let msg = Printf.sprintf "\r\nRequired parameter '%s' was not present.\r\n" arg in
            ignore (unix_really_write s msg);
            raise e
        end

let exn_to_http sock fn = 
    try fn ()
    with
      | Api_errors.Server_error(code, params) as e -> begin
            Http.output_http sock Http.http_500_internal_error;
            ignore (unix_really_write sock ("\r\nAPI Error: "^Api_errors.to_string e))
        end
      | Failure e -> begin
            Http.output_http sock Http.http_500_internal_error;
            ignore (unix_really_write sock ("\r\nServer error: "^e))
        end
      | exn -> begin
            Http.output_http sock Http.http_500_internal_error;
        end

(** Create a disk with numbered ID exposed over HTTP: add to ID -> VBD map;
    create a vbd for the vdi and attach the disk locally. *)
let make_disk volume sr size bootable =
    let vmuuid = Client.VM.get_uuid ~rpc ~session_id ~self:this_vm in
    let vdi = Client.VDI.create ~rpc ~session_id ~sR:sr 
        ~name_label:"Automatically created." ~name_description:""
        ~sharable:false ~read_only:false ~other_config:[] ~virtual_size:size
        ~_type:`system ~sm_config:[ Xapi_globs._sm_vm_hint, vmuuid ] ~xenstore_data:[] ~tags:[] in
    let vbd = Client.VBD.create ~rpc ~session_id ~vM:this_vm ~vDI:vdi 
        ~bootable ~mode:`RW ~_type:`Disk ~unpluggable:true ~qos_algorithm_type:"" 
        ~qos_algorithm_params:[] ~userdevice:volume ~empty:false 
        ~other_config:["owner", ""] in

    (* plug the disk in *)
    Client.VBD.plug ~rpc ~session_id ~self:vbd;
    try
        let sys_path = "/dev/" ^ volume in
        wait_on_file sys_path 10
    with
        Not_found -> failwith "Device did not appear in /sys/block"

(** HTTP callback for make-disk *)
let make_disk_callback req bio =
    let volume = select_arg bio req.Http.query "volume"
    and size = Int64.of_string (select_arg bio req.Http.query "size")
    and bootable = select_arg bio req.Http.query "bootable" = "true"
    and sr_uuid = select_arg bio req.Http.query "sr" in

    let s = Buf_io.fd_of bio in
    exn_to_http s (fun () ->
        let sr = Client.SR.get_by_uuid ~rpc ~session_id ~uuid:sr_uuid in
        make_disk volume sr size bootable;
        Http.output_http s (Http.http_200_ok ())
    )

(** Partition a disk according to a list of sizes.  Only deals with 
    primary partitions.  Assumes -1 means use rest of disk.  Assumes
    the disk has already been made with make_disk. *)
let partition_disk volume partition_sizes =
    let device_node = Printf.sprintf "/dev/%s" volume in
    let fd = Unix.open_process_out ("/sbin/fdisk " ^ device_node) in

    (* write partitions: *)
    let count n = 
        let rec _count n m = if m <= n then m::(_count n (m + 1)) else [] in
        _count n 1 
    in
    let mkpart part_num size = 
        let len = if size = -1 then "" else "+" ^ (string_of_int size) ^ "M" in
        begin
            output_string fd "n\n"; flush fd;   (* new partition *)
            output_string fd "p\n"; flush fd;   (* primary *)
            output_string fd ((string_of_int (part_num)) ^ "\n"); flush fd; (* number *)
            output_string fd "\n"; flush fd;    (* defualt start cyl *)
            output_string fd (len ^ "\n"); flush fd (* size *)
        end 
    in 
    List.iter2 mkpart (count (List.length partition_sizes)) partition_sizes;

    (* save changes *)
    output_string fd "w\n"; flush fd;

    (* check exit code *)
    let () =
        match (Unix.close_process_out fd) with
            | Unix.WEXITED(0) -> ()
            | _               -> failwith "Partitioning failed." 
    in ()

let partition_disk_callback req bio =
    let rec shorten l = match l with
    | [] -> []
    | None::_ -> []
    | (Some x)::xs -> x::(shorten xs) in
    
    let volume = select_arg bio req.Http.query "volume"
    and parts = List.map int_of_string (shorten (List.map (optional_arg req.Http.query) [ "part1"; "part2"; "part3"; "part4" ])) in
    
    let s = Buf_io.fd_of bio in
    exn_to_http s (fun () ->
        partition_disk volume parts;
        Http.output_http s (Http.http_200_ok ())
    )

let mkfs_callback req bio =
    let volume = select_arg bio req.Http.query "volume"
    and fs = Filesystem.of_string (select_arg bio req.Http.query "fs") in
    let fsopts = optional_arg req.Http.query "fsopts" in

    let s = Buf_io.fd_of bio in
    exn_to_http s (fun () ->
        Filesystem.make volume fs fsopts;
        Http.output_http s (Http.http_200_ok ())
    )

(** Unpack a tar-file from stdin to a volume *)
let unpack_tar volume compression data_iter (src:Http_svr.Chunked.t) =
    let compression_string = Compression.tar_parameter_of compression in
    let _unpack_tar mntpoint =
        let tar = Unix.open_process_out (Printf.sprintf "tar -SC %s -x%sf -" mntpoint compression_string) in
        finally (fun () -> data_iter (output_string tar) src) (fun () -> ignore (Unix.close_process_out tar)) in
    with_single_mount (new_mount_action ("/dev/" ^ volume)) _unpack_tar 

let tar_callback req bio =
    (* parse args *)
    let volume = select_arg bio req.Http.query "volume"
    and compression = Compression.of_string (select_arg bio req.Http.query "compression") in

    (* process incoming tarfile *)
    let blksize = 1024 * 1024 in
    let data_iter fn chunks = 
        let data = ref (Http_svr.Chunked.read chunks blksize) in
        while !data <> "" do
            fn !data; data := Http_svr.Chunked.read chunks blksize
        done 
    in
    let chunks = Http_svr.Chunked.of_bufio bio in
    let s = Buf_io.fd_of bio in
    exn_to_http s (fun () ->
        unpack_tar volume compression data_iter chunks;
        Http.output_http s (Http.http_200_ok ())
    )

let print_callback req bio =
    let chunks = Http_svr.Chunked.of_bufio bio in
    let data = ref (Http_svr.Chunked.read chunks 1024) in
    while !data <> "" do
        Printf.printf "data: %s\n %!" !data; data := Http_svr.Chunked.read chunks 1024
    done;
    let s = Buf_io.fd_of bio in
    Http.output_http s (Http.http_200_ok ())

let set_fs_metadata volume md =
    Hashtbl.replace fs_metadata volume md

let set_fs_metadata_callback req bio =
    let volume = select_arg bio req.Http.query "volume" in
    let mntpoint = select_arg bio req.Http.query "mntpoint" in

    let s = Buf_io.fd_of bio in
    exn_to_http s (fun () ->
        set_fs_metadata volume (new_fs_metadata mntpoint);
        Http.output_http s (Http.http_200_ok ())
    )

(** Update fstab based on the metadata supplied via set_fs_metadata *)
let update_fstab root_vol =
    let _update_fstab mntpoint = 
        (* work out new entries based on the filesystems we have received *)
        let new_local = 
            let a = ref [] in
            Hashtbl.iter (fun v m -> a := (FsTab.entry_of_metadata ("/dev/"^v) m)::!a) fs_metadata;
            !a
        in
        (* fix up fstab: *)
        let fstab_file = mntpoint ^ "/etc/fstab" in
        let log_fstab prefix f = List.iter (fun e -> debug "%s: fstab - %s" prefix (FsTab.string_of_entry e)) f in
        let fstab = FsTab.read fstab_file in
        log_fstab "initial" fstab; let fstab = FsTab.filter (fun x -> not (FsTab.is_local x)) fstab in
        log_fstab "filtered" fstab; let fstab = FsTab.update fstab new_local in
        log_fstab "updated" fstab; 
        log_fstab "new local" new_local;
        let fd = open_out fstab_file in
            List.iter (fun e -> output_string fd ((FsTab.string_of_entry e)^"\n")) fstab;
        close_out fd
    in 
    with_single_mount (new_mount_action ("/dev/"^root_vol)) _update_fstab

let update_fstab_callback req bio =
    let root_vol = select_arg bio req.Http.query "root-vol" in

    let s = Buf_io.fd_of bio in
    exn_to_http s (fun () ->
        update_fstab root_vol;
        Http.output_http s (Http.http_200_ok ())
    )

(** Get the guest on the PV road *)

(* find the index of a substring *)
let strindex str searchstr =
    let rec strindex str searchstr pos =
        if str = "" then raise Not_found;
        if String.startswith searchstr str then
            pos
        else
            strindex (String.sub str 1 (String.length str - 1)) searchstr (pos + 1)
    in strindex str searchstr 0

exception GrubConfigError

let paravirtualise root_vol boot_merged =
    (* set bootloader params -- assume grub for now: *)
    Client.VM.set_PV_bootloader ~session_id ~rpc ~self:this_vm ~value:"pygrub";
    Client.VM.set_PV_kernel ~session_id ~rpc ~self:this_vm ~value:"";
    Client.VM.set_PV_ramdisk ~session_id ~rpc ~self:this_vm ~value:"";
    Client.VM.set_PV_args ~session_id ~rpc ~self:this_vm ~value:"";

    (* rewrite menu.lst or grub.conf so that it has the correct root= value
       in all kernel lines; this makes grubby work when we install a new
       kernel in the next stage. *)
    let update_grub_conf mntpoint =
        let grub_confs = [ "/boot/grub/menu.lst"; "/boot/grub/grub.conf" ] in
        let grub_conf = 
            let rec select fn lst =
                match lst with
                    | [] -> raise Not_found
                    | x::xs -> if (fn x) then x else (select fn xs)
            in select (fun x -> Sys.file_exists (mntpoint ^ x)) grub_confs
        in

        (* backup the file, then write out a new one: *)
        debug "Backing up grub.conf...";
        let gdc = Unix.openfile (mntpoint ^ grub_conf) [ Unix.O_RDONLY ] 0o644 in
        let gdc_bak = Unix.openfile (mntpoint ^ "/boot/grub/grub.conf.orig") [ Unix.O_RDWR; Unix.O_CREAT ] 0o644 in
        finally (fun () -> ignore (copy_file gdc gdc_bak)) (fun () -> Unix.close gdc; Unix.close gdc_bak);
        debug "Backup complete";

        (* now write out a new one: here are the function to manipulate various
           aspects of the command line - we apply each in turn to the input
           lines to get a set of output lines: *)
        let tweak_root parts =
            let update_root s = if String.startswith "root=" s then ("root=/dev/"^root_vol) else s in
            match parts with
              | cmd::rest -> cmd::(List.map update_root rest)
              | x -> x
        in
        let remove_console parts =
            List.filter (fun part -> not (String.startswith "console=" part)) parts
        in
        let update_boot parts =
            let insert_boot k =
                (* /vmlinuz -> /boot/vmlinuz; (hd0,0)/vmlinuz -> (hd0,0)/boot/vmlinuz *)
                let parts = String.split ~limit:2 '/' k in
                match parts with
                    | [ disk; path ] -> (disk ^ "/boot/" ^ path)
                    | _ -> raise GrubConfigError
            in
            if boot_merged then begin
                match parts with
                  | command::file::rest as x ->
                        if command = "kernel" || command = "module" || command = "initrd" then
                            command::(insert_boot file)::rest
                        else x
                  | x -> x
            end else parts
        in

        (* read in the existing file *)
        let lines = 
            let gdc_bak = open_in (mntpoint ^ "/boot/grub/grub.conf.orig") in
            finally (fun () -> 
                let lines = ref [] in
                let () = try
                    while true do
                        lines := (input_line gdc_bak)::!lines
                    done
                    with End_of_file -> lines := List.rev !lines
                in !lines
            ) (fun () -> close_in gdc_bak) in
        (* log what we read *)
        List.iter (fun x -> debug "GRUB: %s" x) lines;
        
        (* split "   xxx" into "   ", "xxx" *)
        let lstrip_save s =
            let rec _lstrip_save s w =
                let l = String.length s in
                if l > 0 then begin
                    let first = String.get s 0 in
                    if String.isspace first then
                        _lstrip_save (String.sub s 1 (l - 1)) ((String.of_char first)^w)
                    else (w, s)
                end else (w, s)
            in
            _lstrip_save s ""
        in
        
        (* split "  ", "x y z" into "  ", ["x"; "y"; "z"] *)
        let split_lines =
            let split_command (w, str) = (w, String.split_f String.isspace str) in
            List.map split_command (List.map lstrip_save lines)
        in

        (* now apply the tweaks: *)
        let tweak_line tweak_fun line =
            let is_comment (w, parts) =
                match parts with
                  | x::xs -> String.startswith "#" x
                  | _ -> false
            in
            if not (is_comment line) then
                let (w, parts) = line in
                (w, tweak_fun parts)
            else
                line
        in
        let new_lines = List.map (tweak_line tweak_root) split_lines in
        let new_lines = List.map (tweak_line remove_console) new_lines in
        let new_lines = List.map (tweak_line update_boot) new_lines in
        let gdc = open_out (mntpoint ^ grub_conf) in
        finally (fun () ->
            let remerged_lines = List.map (fun (w, parts) ->
                w^(String.concat " " parts)
                ) new_lines in
            List.iter (fun x -> debug "Update GRUB: %s" x) remerged_lines;
            List.iter (fun x -> output_string gdc (x^"\n")) remerged_lines
        ) (fun () -> close_out gdc)
    in
    with_single_mount (new_mount_action ("/dev/"^root_vol)) update_grub_conf;

    (* in-place P2V invocation: *)
    let inplace_p2v mntpoint = 
        (* ensure /mnt exists in the target so we can mount the inplace-p2v 
           iso. *)
        let iso_mount = mntpoint ^ "/mnt" in
        let p2v_scripts_mount = mntpoint ^ "/mnt2" in
        assert_dir iso_mount 0o766;
        assert_dir p2v_scripts_mount 0o766;

        (* function to invoke the in-place P2V script. *)
        let invoke actions = 
            (* in the chroot /mnt is the data disk, /mnt2 is a tmpfs waiting for the scripts,
               since for some reason, bind mounts don't work from the rootfs here *)
            ignore (Unix.system (Printf.sprintf "cp -a /opt/xensource/p2v/scripts/* %s/mnt2" mntpoint));
            ignore (Unix.system (Printf.sprintf "env EXTERNAL_P2V=Y chroot %s mnt2/xen-setup -b /mnt/Linux" mntpoint));
            List.iter (fun x ->
                if Sys.file_exists (mntpoint^x) then Unix.unlink (mntpoint^x)
                ) [ "/xenkernel"; "/xeninitrd"; "/boot/xenkernel"; "/boot/xeninitrd"]
        in
        (* make up a mounts list.  We have to optionally omit /sys if the 
           directory doesn't exist in the target filesystem, e.g. on 2.4-based
           kernel like RHEL 3. Mount a tmpfs on p2v_scripts_mount to copy the
           P2V scripts into.  This has to be done because for some reason bind
           mounts from the rootfs here don't work...! *)
        let mount_actions = [
            new_mount_action ~mntpoint:iso_mount "/dev/xvdp";
            new_mount_action ~mntpoint:p2v_scripts_mount ~fstype:"tmpfs" "scripts";
            new_mount_action ~mntpoint:(mntpoint^"/proc") ~fstype:"proc" "none";
            new_mount_action ~mntpoint:(mntpoint^"/dev")  ~options:["bind"] "/dev";
            ] in
        let mount_actions = 
            if Sys.file_exists ("mntpoint"^"/sys") then
                (new_mount_action ~mntpoint:(mntpoint^"/sys") ~fstype:"sysfs" "none")::mount_actions
            else
                mount_actions
        in
        let () = with_mounted mount_actions invoke in ()
    in
    let () = with_single_mount (new_mount_action ("/dev/"^root_vol)) inplace_p2v in ()

let paravirtualise_callback req bio =
    let root_disk = select_arg bio req.Http.query "root-vol"
    and boot_merged = (select_arg bio req.Http.query "boot-merged") = "true" in

    let s = Buf_io.fd_of bio in 
    try
        paravirtualise root_disk boot_merged;
        Http.output_http s (Http.http_200_ok ())
    with
      | Failure e -> begin
            Http.output_http s Http.http_500_internal_error;
            ignore (unix_really_write s ("\r\nServer error: "^e))
        end
      | GrubConfigError -> begin
            Http.output_http s Http.http_500_internal_error;
            ignore (unix_really_write s "\r\nUnable to parse grub config.  Please check and correct it, then try again.")
        end
      | exn -> begin
            Http.output_http s Http.http_500_internal_error;
            ignore (unix_really_write s "\r\nInternal server error.")
        end

let completed () =
    (* remove xvdp, the P2V server ISO: *)
    let vbds = Client.VM.get_VBDs ~rpc ~session_id ~self:this_vm in
    let is_xvdp x = (Client.VBD.get_device ~rpc ~session_id ~self:x = "xvdp") in
    let () = match List.filter is_xvdp vbds with
        | xvdp::_ -> 
            Client.VBD.unplug ~rpc ~session_id ~self:xvdp;
            Client.VBD.destroy ~rpc ~session_id ~self:xvdp
        | [] -> ()
    in
    (* halt *)
    run_checked_sync "halt"

let completed_callback req bio =
    let s = Buf_io.fd_of bio in
    Http.output_http s (Http.http_200_ok ());
    (* close the socket ehre since we won't get to the normal cleanup code *)
    Unix.close s;
    completed ()

let _ = 
    Stunnel.init_stunnel_path ();
    Logs.set "p2v" Log.Debug [ "stderr" ]; 
    Logs.set_default Log.Info  [ "stderr" ];
    Logs.set_default Log.Warn  [ "stderr" ];
    Logs.set_default Log.Error [ "stderr" ];

    debug "hello";

    RuntimeEnv.configure_networking session_id rpc;

    Http_svr.add_handler Http.Get "/make-disk" (Http_svr.BufIO make_disk_callback);
    Http_svr.add_handler Http.Get "/partition-disk" (Http_svr.BufIO partition_disk_callback);
    Http_svr.add_handler Http.Get "/mkfs" (Http_svr.BufIO mkfs_callback);
    Http_svr.add_handler Http.Put "/unpack-tar" (Http_svr.BufIO tar_callback);
    Http_svr.add_handler Http.Get "/paravirtualise" (Http_svr.BufIO paravirtualise_callback);
    Http_svr.add_handler Http.Get "/set-fs-metadata" (Http_svr.BufIO set_fs_metadata_callback);
    Http_svr.add_handler Http.Get "/update-fstab" (Http_svr.BufIO update_fstab_callback);
    Http_svr.add_handler Http.Get "/completed" (Http_svr.BufIO completed_callback);
    Http_svr.add_handler Http.Put "/print" (Http_svr.BufIO print_callback);

    let inet_sock = Http_svr.bind listen_addr in
    let (_ : Http_svr.server) = Http_svr.start (inet_sock, "inet_rpc") in
    while (true) do Thread.delay 10000. done;
