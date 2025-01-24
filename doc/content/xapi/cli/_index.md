+++
title = "XE CLI architecture"
menuTitle = "CLI"
+++

{{% notice info %}}
The links in this page point to the source files of xapi
[v1.132.0](https://github.com/xapi-project/xen-api/tree/v1.132.0), not to the
latest source code. Meanwhile, the CLI server code in xapi has been moved to a
library separate from the main xapi binary, and has its own subdirectory
`ocaml/xapi-cli-server`.
{{% /notice %}}

## Architecture

-   **The actual CLI** is a very lightweight binary in
    [ocaml/xe-cli](https://github.com/xapi-project/xen-api/tree/v1.132.0/ocaml/xe-cli)
    -   It is just a dumb client, that does everything that xapi tells
        it to do
    -   This is a security issue
        -   We must trust the xenserver that we connect to, because it
            can tell xe to read local files, download files, ...
    -   When it is first called, it takes the few command-line arguments
        it needs, and then passes the rest to xapi in a HTTP PUT request
        -   Each argument is in a separate line
    -   Then it loops doing what xapi tells it to do, in a loop, until
        xapi tells it to exit or an exception happens

-   **The protocol** description is in
    [ocaml/xapi-cli-protocol/cli_protocol.ml](https://github.com/xapi-project/xen-api/blob/v1.132.0/ocaml/xapi-cli-protocol/cli_protocol.ml)
    -   The CLI has such a protocol that one binary can talk to multiple
        versions of xapi as long as their CLI protocol versions are
        compatible
    -   and the CLI can be changed without updating the xe binary
    -   and also for performance reasons, it is more efficient this way
        than by having a CLI that makes XenAPI calls

-   **Xapi**
    -   The HTTP POST request is sent to the `/cli` URL
    -   In `Xapi.server_init`, xapi [registers the appropriate function
        to handle these
        requests](https://github.com/xapi-project/xen-api/blob/v1.132.0/ocaml/xapi/xapi.ml#L804),
        defined in [common_http_handlers in the same
        file](https://github.com/xapi-project/xen-api/blob/v1.132.0/ocaml/xapi/xapi.ml#L589):
        `Xapi_cli.handler`
    -   The relevant code is in `ocaml/xapi/records.ml`,
        `ocaml/xapi/cli_*.ml`
        -   CLI object definitions are in `records.ml`, command
            definitions in `cli_frontend.ml` (in
            [cmdtable_data](https://github.com/xapi-project/xen-api/blob/v1.132.0/ocaml/xapi/cli_frontend.ml#L72)),
            implementations of commands in `cli_operations.ml`
    -   When a command is received, it is parsed into a command name and
        a parameter list of key-value pairs
        -   and the command table
            [is](https://github.com/xapi-project/xen-api/blob/v1.132.0/ocaml/xapi/xapi_cli.ml#L157)
            [populated
            lazily](https://github.com/xapi-project/xen-api/blob/v1.132.0/ocaml/xapi/cli_frontend.ml#L3005)
            from the commands defined in `cmdtable_data` in
            `cli_frontend.ml`, and [automatically
            generated](https://github.com/xapi-project/xen-api/blob/v1.132.0/ocaml/xapi/cli_operations.ml#L740)
            low-level parameter commands (the ones defined in [section
            A.3.2 of the XenServer Administrator's
            Guide](http://docs.citrix.com/content/dam/docs/en-us/xenserver/xenserver-7-0/downloads/xenserver-7-0-administrators-guide.pdf))
            are also added for a list of standard classes
        -   the command table maps command names to records that contain
            the implementation of the command, among other things
    -   Then the command name [is looked
        up](https://github.com/xapi-project/xen-api/blob/v1.132.0/ocaml/xapi/xapi_cli.ml#L86)
        in the command table, and the corresponding operation is
        executed with the parsed key-value parameter list passed to it

## Walk-through: CLI handler in xapi (external calls)

### Definitions for the HTTP handler

    Constants.cli_uri = "/cli"

    Datamodel.http_actions = [...;
      ("post_cli", (Post, Constants.cli_uri, false, [], _R_READ_ONLY, []));
    ...]

    (* these public http actions will NOT be checked by RBAC *)
    (* they are meant to be used in exceptional cases where RBAC is already *)
    (* checked inside them, such as in the XMLRPC (API) calls *)
    Datamodel.public_http_actions_with_no_rbac_check` = [...
      "post_cli";  (* CLI commands -> calls XMLRPC *)
    ...]

    Xapi.common_http_handlers = [...;
      ("post_cli", (Http_svr.BufIO Xapi_cli.handler));
    ...]

    Xapi.server_init () =
      ...
      "Registering http handlers", [], (fun () -> List.iter Xapi_http.add_handler common_http_handlers);
      ...

Due to there definitions, `Xapi_http.add_handler` does not perform RBAC checks for `post_cli`. This means that the CLI handler does not use `Xapi_http.assert_credentials_ok` when a request comes in, as most other handlers do. The reason is that RBAC checking is delegated to the actual XenAPI calls that are being done by the commands in `Cli_operations`.

This means that the `Xapi_http.add_handler call` so resolves to simply:

    Http_svr.Server.add_handler server Http.Post "/cli" (Http_svr.BufIO Xapi_cli.handler))

...which means that the function `Xapi_cli.handler` is called directly when an HTTP POST request with path `/cli` comes in.

### High-level request processing

`Xapi_cli.handler`:

- Reads the body of the HTTP request, limitted to `Xapi_globs.http_limit_max_cli_size = 200 * 1024` characters.
- Sends a protocol version string to the client: `"XenSource thin CLI protocol"` plus binary encoded major (0) and (2) minor numbers.
- Reads the protocol version from the client and exits with an error if it does not match the above.
- Calls `Xapi_cli.parse_session_and_args` with the request's body to extract the session ref, if there.
- Calls `Cli_frontend.parse_commandline` to parse the rest of the command line from the body.
- Calls `Xapi_cli.exec_command` to execute the command.
- On error, calls `exception_handler`.

`Xapi_cli.parse_session_and_args`:

- Is passed the request body and reads it line by line. Each line is considered an argument.
- Removes any CR chars from the end of each argument.
- If the first arg starts with `session_id=`, the the bit after this prefix is considered to be a session reference.
- Returns the session ref (if there) and (remaining) list of args.

`Cli_frontend.parse_commandline`:

- Returns the command name and assoc list of param names and values. It handles `--name` and `-flag` arguments by turning them into key/value string pairs.

`Xapi_cli.exec_command`:

- Finds username/password params.
- Get the rpc function: this is the so-called "`fake_rpc` callback", which does not use the network or HTTP at all, but goes straight to `Api_server.callback1` (the XenAPI RPC entry point). This function is used by the CLI handler to do loopback XenAPI calls.
- Logs the parsed xe command, omitting sensitive data.
- Continues as `Xapi_cli.do_rpcs`
- Looks up the command name in the command table from `Cli_frontend` (raises an error if not found).
- Checks if all required params have been supplied (raises an error if not).
- Checks that the host is a pool master (raises an error if not).
- Depending on the command, a `session.login_with_password` or `session.slave_local_login_with_password` XenAPI call is made with the supplied username and password. If the authentication passes, then a session reference is returned for the RBAC role that belongs to the user. This session is used to do further XenAPI calls.
- Next, the implementation of the command in `Cli_operations` is executed.

### Command implementations

The various commands are implemented in `cli_operations.ml`. These functions are only called after user authentication has passed (see above). However, RBAC restrictions are only enforced inside any XenAPI calls that are made, and _not_ on any of the other code in `cli_operations.ml`.

The type of each command implementation function is as follows (see `cli_cmdtable.ml`):

    type op =
      Cli_printer.print_fn ->
      (Rpc.call -> Rpc.response) ->
      API.ref_session -> ((string*string) list) -> unit

So each function receives a printer for sending text output to the xe client, and rpc function and session reference for doing XenAPI calls, and a key/value pair param list. Here is a typical example:

    let bond_create printer rpc session_id params =
      let network = List.assoc "network-uuid" params in
      let mac = List.assoc_default "mac" params "" in
      let network = Client.Network.get_by_uuid rpc session_id network in
      let pifs = List.assoc "pif-uuids" params in
      let uuids = String.split ',' pifs in
      let pifs = List.map (fun uuid -> Client.PIF.get_by_uuid rpc session_id uuid) uuids in
      let mode = Record_util.bond_mode_of_string (List.assoc_default "mode" params "") in
      let properties = read_map_params "properties" params in
      let bond = Client.Bond.create rpc session_id network pifs mac mode properties in
      let uuid = Client.Bond.get_uuid rpc session_id bond in
      printer (Cli_printer.PList [ uuid])

- The necessary parameters are looked up in `params` using `List.assoc` or similar.
- UUIDs are translated into reference by `get_by_uuid` XenAPI calls (note that the `Client` module is the XenAPI client, and functions in there require the rpc function and session reference).
- Then the main API call is made (`Client.Bond.create` in this case).
- Further API calls may be made to output data for the client, and passed to the `printer`.

This is the common case for CLI operations: they do API calls based on the parameters that were passed in.

However, other commands are more complicated, for example `vm_import/export` and `vm_migrate`. These contain a lot more logic in the CLI commands, and also send commands to the client to instruct it to read or write files and/or do HTTP calls.

Yet other commands do not actually do any XenAPI calls, but instead get "helpful" information from other places. Example: `diagnostic_gc_stats`, which displays statistics from xapi's OCaml GC.

## Tutorials

The following tutorials show how to extend the CLI (and XenAPI):

-   [Adding a field](../guides/howtos/add-field)
-   [Adding a function](../guides/howtos/add-function)
