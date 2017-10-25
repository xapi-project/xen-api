
(** Xapi's local Unix domain socket *)
let xapi_unix_domain_socket_uri = "file:///var/xapi/xapi"

(** Location of the xensource-inventory file on XenServer *)
let xensource_inventory_filename = "/etc/xensource-inventory"

(** The IANA-reserved port for NBD *)
let standard_nbd_port = 10809

let project_url = "http://github.com/xapi-project/xapi-nbd"

let xapi_nbd_persistent_dir = "/var/lib/xapi-nbd"

let vbd_list_file_name = "VBDs_to_clean_up"

(** When logging in via xapi's Unix domain socket to perform some cleanups, we
    keep trying to log in up to this many seconds, because xapi may not be
    ready when xapi-nbd starts up. *)
let wait_for_xapi_timeout_seconds = 300.0

(** We sleep for this many seconds before the next login attempt. *)
let wait_for_xapi_retry_delay_seconds = 4.0

(** Allow no more than this many client connections. *)
let connection_limit = 16
