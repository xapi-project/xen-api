(** Functions relating to memory requirements of Xen domains *)

open Printf

module D = Debug.Debugger(struct let name = "xenops" end)
open D

let ( +++ ) = Int64.add
let ( --- ) = Int64.sub
let ( *** ) = Int64.mul
let ( /// ) = Int64.div

(* === Memory conversion factors ============================================ *)

let bytes_per_kib  = 1024L
let bytes_per_mib  = 1048576L
let bytes_per_page = Int64.of_int (Mmap.getpagesize ())
let kib_per_page   = bytes_per_page /// bytes_per_kib
let kib_per_mib    = 1024L
let pages_per_mib  = bytes_per_mib /// bytes_per_page

(* === Arithmetic functions ================================================= *)

(** Returns true if (and only if) the specified argument is a power of 2. *)
let is_power_of_2 n =
	(n > 0) && (n land (0 - n) = n)

let round_down_to_multiple_of x y =
	(x /// y) *** y

let round_up_to_multiple_of x y =
	((x +++ y --- 1L) /// y) *** y

(* === Memory rounding functions ============================================ *)

let round_up = round_up_to_multiple_of
let round_down = round_down_to_multiple_of

let round_bytes_down_to_nearest_page_boundary v = round_down v bytes_per_page
let round_bytes_down_to_nearest_mib           v = round_down v bytes_per_mib
let round_kib_down_to_nearest_page_boundary   v = round_down v kib_per_page
let round_kib_up_to_nearest_page_boundary     v = round_up   v kib_per_page
let round_kib_up_to_nearest_mib               v = round_up   v kib_per_mib
let round_pages_up_to_nearest_mib             v = round_up   v pages_per_mib

(* === Division functions =================================================== *)

let divide_rounding_down numerator denominator =
	numerator /// denominator

let divide_rounding_up numerator denominator =
	(numerator +++ denominator --- 1L) /// denominator

(* === Memory unit conversion functions ===================================== *)

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

(* === Host memory properties =============================================== *)

let get_free_memory_kib ~xc =
	kib_of_pages (Int64.of_nativeint (Xc.physinfo xc).Xc.free_pages)
let get_scrub_memory_kib ~xc =
	kib_of_pages (Int64.of_nativeint (Xc.physinfo xc).Xc.scrub_pages)
let get_total_memory_mib ~xc =
	mib_of_pages_free (Int64.of_nativeint ((Xc.physinfo xc).Xc.total_pages))
let get_total_memory_bytes ~xc =
	bytes_of_pages (Int64.of_nativeint ((Xc.physinfo xc).Xc.total_pages))

(* === Domain memory breakdown ============================================== *)

(*           ╤  ╔════════════════╗                       ╤            *)
(*           │  ║ shadow         ║                       │            *)
(*           │  ╠════════════════╣                       │            *)
(*           │  ║ extra external ║                       │            *)
(*  overhead │  ╠════════════════╣            ╤          │            *)
(*           │  ║ extra internal ║            │          │            *)
(*           │  ╠════════════════╣            │          │            *)
(*           │  ║ video          ║            │          │ footprint  *)
(*           ╪  ╠════════════════╣  ╤         │ xen      │            *)
(*           │  ║                ║  │         │ maximum  │            *)
(*           │  ║                ║  │         │          │            *)
(*           │  ║ guest          ║  │ target  │          │            *)
(*           │  ║                ║  │         │          │            *)
(*    static │  ║                ║  │         │          │            *)
(*   maximum │  ╠════════════════╣  ╧         ╧          ╧            *)
(*           │  ║                ║                                    *)
(*           │  ║                ║                                    *)
(*           │  ║ balloon        ║                                    *)
(*           │  ║                ║                                    *)
(*           │  ║                ║                                    *)
(*           ╧  ╚════════════════╝                                    *)

(* === Domain memory breakdown: HVM guests ================================== *)

module HVM = struct

	let xen_video_mib = 4L

	let xen_extra_internal_mib = 1L
	let xen_extra_external_mib = 1L

	let xen_max_mib target_mib =
		target_mib +++
		xen_video_mib +++
		xen_extra_internal_mib

	let xen_shadow_mib max_mib vcpu_count multiplier =
		let vcpu_pages = 256L *** (Int64.of_int vcpu_count) in
		let p2m_map_pages = max_mib in
		let shadow_resident_pages = max_mib in
		let total_mib = mib_of_pages_used
			(vcpu_pages +++ p2m_map_pages +++ shadow_resident_pages) in
		let total_mib_multiplied =
			Int64.of_float ((Int64.to_float total_mib) *. multiplier) in
		max 1L total_mib_multiplied

	let xen_overhead_mib max_mib vcpu_count multiplier =
		xen_video_mib +++
		xen_extra_internal_mib +++
		xen_extra_external_mib +++
		(xen_shadow_mib max_mib vcpu_count multiplier)

	let xen_footprint_mib target_mib max_mib vcpu_count multiplier =
		target_mib +++ (xen_overhead_mib max_mib vcpu_count multiplier)

	let round_shadow_multiplier max_mib vcpu_count requested_multiplier domid =
		let shadow_mib = xen_shadow_mib max_mib vcpu_count in
		let requested_shadow_mib = shadow_mib requested_multiplier in
		let default_shadow_mib = shadow_mib 1. in
		Xc.with_intf (fun xc ->
			let actual_shadow_mib =
				Int64.of_int (Xc.shadow_allocation_get xc domid) in
			let actual_multiplier =
				(Int64.to_float actual_shadow_mib) /.
				(Int64.to_float default_shadow_mib) in
			debug
				"actual shadow value is %Ld MiB [multiplier = %0.2f]; \
				requested value was %Ld MiB [multiplier = %.2f]"
				actual_shadow_mib actual_multiplier
				requested_shadow_mib requested_multiplier;
				(* Inevitably due to rounding the actual multiplier may   *)
				(* be different from the supplied value. However if the   *)
				(* supplied value was accepted then we record that value. *)
				(* If Xen overrode us then we record the actual value.    *)
			if actual_shadow_mib <> requested_shadow_mib
			then actual_multiplier
			else requested_multiplier
		)

end

(* === Domain memory breakdown: Linux guests ================================ *)

module Linux = struct

	let xen_video_mib = 0L
	let xen_extra_internal_mib = 0L
	let xen_extra_external_mib = 1L
	let xen_max_mib target_mib = target_mib
	let xen_shadow_mib = 0L
	let xen_overhead_mib = xen_extra_external_mib
	let xen_footprint_mib target_mib = xen_extra_external_mib +++ target_mib

end

(* === Miscellaneous functions ============================================== *)

(** @deprecated Use [memory_check.vm_compute_start_memory] instead. *)
let required_to_boot hvm vcpus mem_kib mem_target_kib shadow_multiplier =
	let max_mib = mib_of_kib_used mem_kib in
	let target_mib = mib_of_kib_used mem_target_kib in
	if hvm
	then HVM.xen_footprint_mib target_mib max_mib vcpus shadow_multiplier
	else Linux.xen_footprint_mib target_mib

let wait_xen_free_mem ~xc ?(maximum_wait_time_seconds=256) requested_memory_kib =
	let rec wait accumulated_wait_time_seconds =
		let free_memory_kib = get_free_memory_kib ~xc in
		let scrub_memory_kib = get_scrub_memory_kib ~xc in
		(* At exponentially increasing intervals, write  *)
		(* a debug message saying how long we've waited: *)
		if is_power_of_2 accumulated_wait_time_seconds then
			debug
				"Waited %i second(s) for memory to become available: \
				%Ld free, %Ld scrub, %Ld requested"
				accumulated_wait_time_seconds free_memory_kib
				scrub_memory_kib requested_memory_kib;
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
