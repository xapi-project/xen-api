(* Database globals *)

let redo_log_block_device_io = ref "block_device_io"

(** The delay between each attempt to connect to the block device I/O process *)
let redo_log_connect_delay = ref 0.1

(** The maximum time, in seconds, for which we are prepared to wait for a response from the block device I/O process before assuming that it has died while emptying *)
let redo_log_max_block_time_empty = ref 2.

(** The maximum time, in seconds, for which we are prepared to wait for a response from the block device I/O process before assuming that it has died while reading *)
let redo_log_max_block_time_read = ref 30.

(** The maximum time, in seconds, for which we are prepared to wait for a response from the block device I/O process before assuming that it has died while writing a delta *)
let redo_log_max_block_time_writedelta = ref 2.

(** The maximum time, in seconds, for which we are prepared to wait for a response from the block device I/O process before assuming that it has died while writing a database *)
let redo_log_max_block_time_writedb = ref 30.

(** {3 Settings related to the exponential back-off of repeated attempts to reconnect after failure} *)
(** The initial backoff delay, in seconds *)
let redo_log_initial_backoff_delay = 2

(** The factor by which the backoff delay is multiplied with each successive failure *)
let redo_log_exponentiation_base = 2

(** The maximum permitted backoff delay, in seconds *)
let redo_log_maximum_backoff_delay = 120

(** The maximum permitted number of block device I/O processes we are waiting to die after being killed *)
let redo_log_max_dying_processes = 2

(** The prefix of the file used as a socket to communicate with the block device I/O process *)
let redo_log_comms_socket_stem = "sock-blkdev-io"

(** The maximum time, in seconds, for which we are prepared to wait for a response from the block device I/O process before assuming that it has died while initially connecting to it *)
let redo_log_max_startup_time = ref 5.

(** The length, in bytes, of one redo log which constitutes half of the VDI *)
let redo_log_length_of_half = 60 * 1024 * 1024

(* temporary path for the HA metadata database *)
let ha_metadata_db = Filename.concat "/var/lib/xcp" "ha_metadata.db"

(* temporary path for the general metadata database *)
let gen_metadata_db = Filename.concat "/var/lib/xcp" "gen_metadata.db"

let static_vdis_dir = ref "/etc/xensource/static-vdis"

(* Note the following has an equivalent in the xapi layer *)
let http_limit_max_rpc_size = 300 * 1024 (* 300K *)

(* add_to_map is idempotent *)
let idempotent_map = ref false

(** Dynamic configurations to be read whenever xapi (re)start *)

let permanent_master_failure_retry_interval = ref 60.

let master_connection_reset_timeout = ref 120.

(* amount of time to retry master_connection before (if
   restart_on_connection_timeout is set) restarting xapi; -ve means don't
   timeout: *)
let master_connection_retry_timeout = ref (-1.)

let master_connection_default_timeout = ref 10.

let pool_secret = ref ""

(* Function to execute when the database wants to be restarted *)
let restart_fn : (unit -> unit) ref = ref (fun () -> exit 0)

let https_port = ref 443

let snapshot_db = Filename.concat "/var/lib/xcp" "snapshot.db"

let db_conf_path = ref (Filename.concat "/etc/xensource" "db.conf")
