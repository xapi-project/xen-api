(*
 * Copyright (C) 2006-2010 Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)
(** Module to read CPUID information and do feature masking checks. *)

(** {2 Types and Conversion} *)

(** Manufacturer of the CPU. *)
type manufacturer =
| AMD		(** AMD *)
| Intel		(** Intel *)
| Unknown	(** Other manufacturer *)

(** Indicates whether CPUID features can be masked. *)
and maskability =
| No		(** No masking possible *)
| Base		(** Only base features can be masked *)
| Full		(** Both base and extended features can be masked *)

(** CPU feature bit vector. *)
and features

(** CPUID information. *)
and cpu_info =
	{
		manufacturer: manufacturer;		(** Manufacturer of the CPU *)
		family: int32;					(** Family number of the CPU *)
		model: int32;					(** Model number of the CPU *)
		stepping: int32;				(** Stepping number of the CPU *)
		features: features;				(** Feature bit vector of the CPU *)
		physical_features: features;	(** Physical Feature bit vector of the CPU *)
		maskable: maskability;			(** Indicates whether the CPU supports
									        Intel FlexMigration or AMD Extended Migration,
									        or cannot be masked *)
	}
	
(** Convert {!features} into a string of 4 groups of 8 hexadecimal digits, separated by dashes. *)
val features_to_string : features -> string

(** Convert a feature string into a {!features} value. The string must contain
 *  32 hexadecimal digits and may have spaces and dashes. *)
val string_to_features : string -> features


(** {2 Reading CPUID Information} *)

(** Read the CPUID information from the hardware. *)
val read_cpu_info : unit -> cpu_info


(** {2 Masking Checks} *)

(** Apply a mask to given features. *)
val mask_features : features -> features -> features

(** Check that this CPU can be masked to fit the pool. Raises exception
 *  indicating the reason if this is not possible. *)
val assert_maskability : cpu_info -> manufacturer -> features -> unit

(** Return the CPU masking string to add to the Xen command-line, 
 *  or raise an exception saying why it can't be done. *)
val xen_masking_string : cpu_info -> features -> (string * string) list

(** Raised by {!string_to_features} if the given string is malformed,
 *  or by {!assert_maskability} if the CPU features cannot be masked to 
 *  obtain given features. *)
exception InvalidFeatureString of string

(** Raised by {!assert_maskability} if the CPU does not support feature masking. *)
exception MaskingNotSupported of string

(** Raised by {!assert_maskability} if manufacturers are not equal. *)
exception ManufacturersDiffer

