(*
 * Copyright (C) 2015 Citrix Systems Inc.
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

(** Consolidation functions are used to map samples onto recorded values.
    When interpreting the metrics, consider which consolidation function you
    should be using. For example, if your concern is peak (as in numerically
    greatest) load, then you should be using the `Max consolidation function. *)
type cf = [`Average | `Min | `Max]

module Legend : sig
  type cls = [`VM | `Host | `Other of string]

  (** A legend identifies a specific data source. *)
  type t = string * cf * cls * Uuidm.t

  val of_string : string -> [`Ok of t | `Error of [> `Msg of string]]

  val find_data_source : API.data_source_t list -> t -> API.data_source_t option
  (** Find the data_source record corresponding to a legend. The data source
      contains useful information such as the units. *)
end

(** Points are averaged over an interval and stored in separate 'archives'.
    The server will automatically select an interval given a start time but
    you can manually select the interval if you want a particular resolution. *)
type interval =
  [ `Seconds  (** every 5 seconds for 10 minutes *)
  | `Minute  (** every minute for 2 hours *)
  | `Hour  (** every hour for one week *)
  | `Day  (** every day for one year *)
  | `Other of int ]

val seconds_of_interval : interval -> int
(** The length of the interval in seconds *)

val archive_length_of_interval : interval -> int
(** The length of the archive associated with the interval *)

module Updates : sig
  val uri :
       host:Uri.t
    -> authentication:Xen_api_auth.t
    -> start:int
    -> ?include_host:bool
    -> ?interval:interval
    -> ?cf:cf
    -> unit
    -> Uri.t
  (** [updates ~host ~authentication ~start ?include_host ?interval ?cf ()]
      fetches VM metrics updates for VMs resident on [host], beginning from
      [start] seconds since the epoch.
      Note the resolution of the resulting metrics may decrease with age.
      If [?include_host] is true then host metrics will be included.
      If [?cf] is specified then only this consolidation function will be
      provided, otherwise all will be provided. *)

  val parse : string -> Rrd_updates.t
  (** [parse result] converts the result of an HTTP GET of the uri into
      an Rrd_updates.t *)
end
