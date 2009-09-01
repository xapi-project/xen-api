(** Returns the current amount of free memory in the host. *)
val get_free_memory_kib : xc:Xc.handle -> int64
(** Returns the current amount of scrub memory in the host. *)
val get_scrub_memory_kib : xc:Xc.handle -> int64
(** Returns the total amount of memory available in this host. *)
val get_total_memory_mib : xc:Xc.handle -> int64
(** Returns the total amount of memory available in this host. *)
val get_total_memory_bytes : xc:Xc.handle -> int64

(** The current page size (in bytes). *)
val bytes_per_page : int64
(** The current page size (in kib). *)
val kib_per_page : int64

(** For the given values x and y, calculates the greatest *)
(** value x' <= x, such that x' is evenly divisible by y. *)
val round_down_to_multiple_of : int64 -> int64 -> int64
(** For the given values x and y, calculates the smallest *)
(** value x' >= x, such that x' is evenly divisible by y. *)
val round_up_to_multiple_of : int64 -> int64 -> int64

(** Rounds down the given value (in bytes) to the nearest page boundary. *)
val round_bytes_down_to_nearest_page_boundary : int64 -> int64
(** Rounds down the given value (in bytes) to the nearest mib boundary. *)
val round_bytes_down_to_nearest_mib : int64 -> int64
(** Rounds down the given value (in kib) to the nearest page boundary. *)
val round_kib_down_to_nearest_page_boundary : int64 -> int64
(** Rounds up the given value (in kib) to the nearest page boundary. *)
val round_kib_up_to_nearest_page_boundary : int64 -> int64
(** Rounds up the given value (in kib) to the nearest mib boundary. *)
val round_kib_up_to_nearest_mib : int64 -> int64

(** Converts the given memory value from kib to bytes. *)
val bytes_of_kib : int64 -> int64
(** Converts the given memory value from mib to bytes. *)
val bytes_of_mib : int64 -> int64
(** Converts the given memory value from pages to bytes. *)
val bytes_of_pages : int64 -> int64

(** Converts the given free memory value from bytes to kib. *)
val kib_of_bytes_free : int64 -> int64
(** Converts the given used memory value from bytes to kib. *)
val kib_of_bytes_used : int64 -> int64
(** Converts the given memory value from pages to kib. *)
val kib_of_pages : int64 -> int64
(** Converts the given memory value from mib to kib. *)
val kib_of_mib : int64 -> int64

(** Converts the given free memory value from bytes to pages. *)
val pages_of_bytes_free : int64 -> int64
(** Converts the given used memory value from bytes to pages. *)
val pages_of_bytes_used : int64 -> int64
(** Converts the given free memory value from kib to pages. *)
val pages_of_kib_free : int64 -> int64
(** Converts the given used memory value from kib to pages. *)
val pages_of_kib_used : int64 -> int64
(** Converts the given memory value from mib to pages. *)
val pages_of_mib : int64 -> int64

(** Converts the given free memory value from bytes to mib. *)
val mib_of_bytes_free : int64 -> int64
(** Converts the given used memory value from bytes to mib. *)
val mib_of_bytes_used : int64 -> int64
(** Converts the given free memory value from kib to mib. *)
val mib_of_kib_free : int64 -> int64
(** Converts the given used memory value from kib to mib. *)
val mib_of_kib_used : int64 -> int64
(** Converts the given free memory value from pages to mib. *)
val mib_of_pages_free : int64 -> int64
(** Converts the given used memory value from pages to mib. *)
val mib_of_pages_used : int64 -> int64

module HVM :
sig
	val required_initial_reservation : int64 -> int64
	val required_available : int64 -> int64
	val required_shadow : int -> int64 -> float -> int64
	val round_shadow_multiplier : int -> int64 -> float -> Xc.domid -> float
end

module Linux :
sig
	val required_initial_reservation : 'a -> 'a
	val required_available : 'a -> 'a
end

val required_to_boot : bool -> int -> int64 -> int64 -> float -> int64

(** Waits up to the specified maximum wait time for Xen to *)
(** indicate that the requested amount of memory is free.  *)
(** Returns true if (and only if) the requested amount of  *)
(** memory is free within the specified maximum wait time. *)
val wait_xen_free_mem : xc:Xc.handle -> ?maximum_wait_time_seconds:int -> int64 -> bool
