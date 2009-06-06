(** Functions relating to memory requirements of Xen domains *)

open Printf

module D = Debug.Debugger(struct let name = "xenops" end)
open D

let ( +++ ) = Int64.add
let ( --- ) = Int64.sub
let ( *** ) = Int64.mul
let ( /// ) = Int64.div

(* === Host memory properties === **)

let get_free_memory_kib ~xc =
	Xc.pages_to_kib (Int64.of_nativeint (Xc.physinfo xc).Xc.free_pages)
let get_scrub_memory_kib ~xc =
	Xc.pages_to_kib (Int64.of_nativeint (Xc.physinfo xc).Xc.scrub_pages)
let get_total_memory_mib ~xc =
	Xc.pages_to_mib (Int64.of_nativeint ((Xc.physinfo xc).Xc.total_pages))

let bytes_per_kib  = 1024L
let bytes_per_mib  = 1048576L
let bytes_per_page = Int64.of_int (Mmap.getpagesize ())
let kib_per_page   = bytes_per_page /// bytes_per_kib
let kib_per_mib    = 1024L
let pages_per_mib  = bytes_per_mib /// bytes_per_page

(* === Arithmetic rounding functions === **)

let round_down_to_multiple_of x y =
	(x /// y) *** y

let round_up_to_multiple_of x y =
	((x +++ y --- 1L) /// y) *** y

(* === Memory rounding functions === **)

let round_bytes_down_to_nearest_page_boundary value = round_down_to_multiple_of value bytes_per_page
let round_bytes_down_to_nearest_mib           value = round_down_to_multiple_of value bytes_per_mib
let round_kib_down_to_nearest_page_boundary   value = round_down_to_multiple_of value kib_per_page
let round_kib_up_to_nearest_page_boundary     value = round_up_to_multiple_of value kib_per_page
let round_kib_up_to_nearest_mib               value = round_up_to_multiple_of value kib_per_mib

(* === Division functions === *)

let divide_rounding_down numerator denominator =
	numerator /// denominator

let divide_rounding_up numerator denominator =
	(numerator +++ denominator --- 1L) /// denominator

(* === Memory unit conversion functions === **)

let bytes_of_kib   value = value *** bytes_per_kib
let bytes_of_pages value = value *** bytes_per_page
let bytes_of_mib   value = value *** bytes_per_mib
let kib_of_mib     value = value *** kib_per_mib
let kib_of_pages   value = value *** kib_per_page
let pages_of_mib   value = value *** pages_per_mib

let kib_of_bytes_free   value = divide_rounding_down value bytes_per_kib
let pages_of_bytes_free value = divide_rounding_down value bytes_per_page
let pages_of_kib_free   value = divide_rounding_down value kib_per_page
let mib_of_bytes_free   value = divide_rounding_down value bytes_per_mib
let mib_of_kib_free     value = divide_rounding_down value kib_per_mib
let mib_of_pages_free   value = divide_rounding_down value pages_per_mib

let kib_of_bytes_used   value = divide_rounding_up value bytes_per_kib
let pages_of_bytes_used value = divide_rounding_up value bytes_per_page
let pages_of_kib_used   value = divide_rounding_up value kib_per_page
let mib_of_bytes_used   value = divide_rounding_up value bytes_per_mib
let mib_of_kib_used     value = divide_rounding_up value kib_per_mib
let mib_of_pages_used   value = divide_rounding_up value pages_per_mib

(** See the calculations in tools/python/xen/xend *)
module HVM = struct
	(** The amount of memory needed to be free in KiB.
	    See image.py:getRequiredAvailableMemory *)
	let required_initial_reservation kib =
		(* Apparently this was derived empirically:
		 * 2.4 KiB overhead per 1 MiB RAM
		 * + 4 MiB to avoid running out of memory altogether *)
		let extra_kib = 2.4 *. (Int64.to_float kib /. 1024.) +. 4096. in
		let extra_kib = Int64.of_float extra_kib in
		kib +++ (round_kib_up_to_nearest_page_boundary extra_kib)

	(** See image.py:getRequiredAvailableMemory *)
	let required_available = required_initial_reservation

	(** Shadow memory needed by the domain.
	    See image.py:getRequiredShadowMemory. *)
	let required_shadow vcpu_count max_kib multiplier =
		(* add extra overheads *)
		let initial_kib = required_initial_reservation max_kib in
		let initial_mib = mib_of_kib_used initial_kib in
		(* Apparently we need the following shadow allocation: *)
		let vcpu_pages = 256L *** (Int64.of_int vcpu_count) in
		let p2m_map_pages = initial_mib in
		let shadow_resident_pages = initial_mib in
		let kib = kib_of_pages (vcpu_pages +++ p2m_map_pages +++ shadow_resident_pages) in
		(* NB: the Xen default is lower than this but is for safety, not performance *)
		let kib = round_kib_up_to_nearest_mib kib in
		Int64.of_float ((Int64.to_float kib) *. multiplier)

	let round_shadow_multiplier vcpus kib requested domid = 
	  let mib_from_kib kib = Int64.to_int (Int64.div kib 1024L) in
	  let requested_shadow_mib = mib_from_kib (required_shadow vcpus kib requested) in
	  let default_shadow_mib = mib_from_kib (required_shadow vcpus kib 1.) in
	  Xc.with_intf
	    (fun xc ->
	       let actual = Xc.shadow_allocation_get xc domid in
	       let actual_multiplier = float_of_int actual /. (float_of_int default_shadow_mib) in
	       debug "Actual shadow value is %d MiB [multiplier = %0.2f]; requested value was %d MiB [multiplier = %.2f]" actual actual_multiplier requested_shadow_mib requested;
	       (* Inevitably due to rounding the 'actual' multiplier will be different from the user-supplied
		  value. However if the requested MiB value was set then we record the user's float. If Xen
		  overrode us then we record the actual multiplier value *)
	       if actual <> requested_shadow_mib then actual_multiplier else requested)
end

module Linux = struct
	let required_initial_reservation kib = kib
	let required_available = required_initial_reservation
end

let required_to_boot hvm vcpus mem_kib mem_target_kib shadow_multiplier =
	if hvm then (
		Int64.add (HVM.required_available mem_kib)
		          (HVM.required_shadow vcpus mem_kib shadow_multiplier)
	) else (
		mem_target_kib
	)

(** Returns true if (and only if) the   *)
(** specified argument is a power of 2. *)
let is_power_of_2 n =
	(n > 0) && (n land (0 - n) = n)

let wait_xen_free_mem ~xc ?(maximum_wait_time_seconds=256) requested_memory_kib =
	let rec wait accumulated_wait_time_seconds =
		let free_memory_kib = get_free_memory_kib ~xc in
		let scrub_memory_kib = get_scrub_memory_kib ~xc in
		(* At exponentially increasing intervals, write  *)
		(* a debug message saying how long we've waited: *)
		if is_power_of_2 accumulated_wait_time_seconds then
			debug "Waited %i second(s) for memory to become available: %Ld free, %Ld scrub, %Ld requested"
				accumulated_wait_time_seconds free_memory_kib scrub_memory_kib requested_memory_kib;
		if free_memory_kib >= requested_memory_kib
		then true
		else begin
			(* Give up if we've already waited the maximum amount of time. *)
			if accumulated_wait_time_seconds >= maximum_wait_time_seconds
			then false
			else begin
				Thread.delay 1.0;
				wait (accumulated_wait_time_seconds + 1)
			end
		end
	in
	wait 0
