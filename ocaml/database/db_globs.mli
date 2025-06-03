(*
 * Copyright (C) Cloud Software Group
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

val redo_log_block_device_io : string ref

val redo_log_connect_delay : float ref

val redo_log_max_block_time_empty : float ref

val redo_log_max_block_time_read : float ref

val redo_log_max_block_time_writedelta : float ref

val redo_log_max_block_time_writedb : float ref

val redo_log_initial_backoff_delay : int

val redo_log_exponentiation_base : int

val redo_log_maximum_backoff_delay : int

val redo_log_max_dying_processes : int

val redo_log_comms_socket_stem : string

val redo_log_max_startup_time : float ref

val redo_log_length_of_half : int

val ha_metadata_db : string

val gen_metadata_db : string

val static_vdis_dir : string ref

val http_limit_max_rpc_size : int

val idempotent_map : bool ref

val permanent_master_failure_retry_interval : float ref

val master_connection_reset_timeout : float ref

val master_connection_retry_timeout : float ref

val master_connection_default_timeout : float ref

val pool_secret : Db_secret_string.t ref

val restart_fn : (unit -> unit) ref

val https_port : int ref

val snapshot_db : string

val db_conf_path : string ref
