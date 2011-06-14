open Printf

external socket_netlink_udev : unit -> Unix.file_descr = "stub_socket_netlink_udev"
external bind_netlink_udev : Unix.file_descr -> unit = "stub_bind_netlink_udev"
external receive_events_udev : Unix.file_descr -> string = "stub_receive_events_udev"

exception Timeout

let wait_for action event timeout =
	let socket = socket_netlink_udev () in
	bind_netlink_udev socket;
	
	let devpath = sprintf "/sys%s" event in
	let fileexists = Sys.file_exists devpath in
	let cc = ref false in

	(* allow to go faster by just checking the file exists or not for specific action in the sys *)
	if action == "add" && fileexists then (
		cc := true
	) else if action == "remove" && not fileexists then (
		cc := true
	);

	if not !cc then (
		let time = ref timeout in
		let found = ref false in
		while !time > 0.0 && !found = false
		do
			let t1 = Unix.gettimeofday () in
			let (is,_,_) = Unix.select [socket] [] [] !time in
			let t2 = Unix.gettimeofday () in
			time := !time -. (t2 -. t1);
			if List.mem socket is then (
				let s = receive_events_udev socket in
				let idx = String.index s '@' in
				if idx > -1 then (
					let ac = String.sub s 0 idx in
					let ev = String.sub s (idx+1) (String.length s - idx - 1) in
					if ac = action && event = ev then (
						found := true
					)
				)
			)
		done;
		if not !found then (
			Unix.close socket;
			raise Timeout
		)
	);
	Unix.close socket;
	()
