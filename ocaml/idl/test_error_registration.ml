(* An API error must be declared in BOTH of two append-only lists.

   (a) [Api_errors.errors], built by every [add_error] call in
       ocaml/xapi-consts/api_errors.ml, is what makes the code raisable.
   (b) [Datamodel_errors.errors], built by every [error] call in
       ocaml/idl/datamodel_errors.ml, declares its parameters and is what
       puts the code in the generated SDK and the error reference.

   A code with (a) and no (b) compiles, is raisable, and reaches a client
   as a string that appears in no reference. A code with (b) and no (a)
   normally cannot happen, since the datamodel entries name [Api_errors]
   values and the compiler catches a missing one, but an entry added as a
   bare string literal would slip through.

   Both lists are read at run time rather than parsed out of the source.
   api_errors.ml derives the whole AUTH_ENABLE_FAILED_* family by
   concatenating a prefix and a suffix, so those codes have no string
   literal to grep for and a textual lint could not be total over the file.

   There is no third list: ocaml/idl/datamodel_lifecycle.ml holds
   prototyped_of_class, prototyped_of_field and prototyped_of_message, and
   names no error code at all. *)

module StringSet = Set.Make (String)

let declared_list = !Api_errors.errors

let registered_list =
  Hashtbl.fold (fun name _ acc -> name :: acc) Datamodel_errors.errors []

let declared = StringSet.of_list declared_list

let registered = StringSet.of_list registered_list

(* Codes already declared without a datamodel entry when this check was
   written, listed rather than counted so each is visible and can be
   discharged on its own.

   Never raised anywhere in the tree, so nothing today can observe the
   missing entry. Registering these is a separate decision from deleting
   them, and neither belongs in this change. *)
let unregistered_and_unraised =
  [
    "AUTH_ENABLE_FAILED_NO_SUPPORT_ENCRYPT_TYPE"
  ; "BALLOONING_DISABLED"
  ; "CA_CERTIFICATE_EXPIRED"
  ; "CERT_REFRESH_IN_PROGRESS"
  ; "CLUSTER_HAS_NO_CERTIFICATE"
  ; "GET_UPDATES_IN_PROGRESS"
  ; "NO_LOCAL_STORAGE"
  ; "SR_ATTACHED"
  ; "SR_NOT_SHARED"
  ; "VM_SNAPSHOT_FAILED"
  ]

(* Raised, so the missing entry is observable today. Each wants its own
   change, with a parameter list and a ~doc:. Grep for the binding rather
   than the string:
     CLIENT_ERROR                    Api_errors.client_error
     REVERT_ONLY_ALLOWED_ON_SNAPSHOT Api_errors.only_revert_snapshot
     VDI_IO_ERROR                    Api_errors.vdi_io_error *)
let unregistered_but_raised =
  ["CLIENT_ERROR"; "REVERT_ONLY_ALLOWED_ON_SNAPSHOT"; "VDI_IO_ERROR"]

let known_unregistered = unregistered_and_unraised @ unregistered_but_raised

let known_unregistered_set = StringSet.of_list known_unregistered

(* Pinning the length is what stops the baseline being used as an escape
   hatch: discharging a code means deleting its line and lowering this
   number, and no edit that adds one still passes. *)
let max_known_unregistered = 13

(* Each check is a name and the verdict it reached. Collecting them rather
   than running them in sequence makes the total a [List.length] instead of
   a counter, which is what [min_checks] tests. *)
let checks =
  let describe = Printf.sprintf in
  List.concat
    [
      StringSet.elements declared
      |> List.map (fun code ->
          ( describe
              "%s is declared in api_errors.ml but has no entry in \
               datamodel_errors.ml, so it is invisible to the SDK and the \
               error reference"
              code
          , StringSet.mem code registered
            || StringSet.mem code known_unregistered_set
          )
      )
    ; StringSet.elements registered
      |> List.map (fun code ->
          ( describe
              "%s has an entry in datamodel_errors.ml but is not declared by \
               add_error in api_errors.ml"
              code
          , StringSet.mem code declared
          )
      )
    ; known_unregistered
      |> List.concat_map (fun code ->
          [
            ( describe
                "%s is in known_unregistered but is no longer declared in \
                 api_errors.ml -- remove it from the list"
                code
            , StringSet.mem code declared
            )
          ; ( describe
                "%s is in known_unregistered but is now registered -- remove \
                 it from the list"
                code
            , not (StringSet.mem code registered)
            )
          ]
      )
    ; [
        ( describe
            "known_unregistered holds %d codes and may hold at most %d -- it \
             may only shrink, and a new code needs an entry in both files \
             rather than a line here"
            (List.length known_unregistered)
            max_known_unregistered
        , List.length known_unregistered <= max_known_unregistered
        )
      ; ( describe
            "api_errors.ml calls add_error %d times for %d distinct codes -- a \
             duplicate string means two bindings answer to one error"
            (List.length declared_list)
            (StringSet.cardinal declared)
        , List.length declared_list = StringSet.cardinal declared
        )
      ; ( describe
            "datamodel_errors.ml registers %d entries for %d distinct codes -- \
             Hashtbl.add shadows rather than replaces, so the duplicate stays \
             reachable"
            (List.length registered_list)
            (StringSet.cardinal registered)
        , List.length registered_list = StringSet.cardinal registered
        )
      ]
    ]

(* Both lists are accumulated by side effect as their modules initialise.
   An initialiser that never runs, or a refactor that moves the
   accumulator, leaves one of them empty -- and every check above is
   generated by iterating one list or the other, so an empty list yields
   no checks rather than failing ones.

   Today the cross-checks would still catch that: the other list's codes
   all fail to find a counterpart. But once known_unregistered has been
   discharged to nothing, which is the end state this list is working
   towards, two empty lists leave only the three self-consistent checks
   below, and they pass having compared nothing.

   Emptiness is the whole of the condition, so it is tested directly. A
   threshold to compare the counts against would have to be a number
   written out by hand -- deriving one from the lists would make it true
   of the empty list, the case being guarded -- and it would say nothing
   this does not. *)
let () =
  let failures = List.filter (fun (_, ok) -> not ok) checks in
  List.iter (fun (name, _) -> print_endline ("  FAIL  " ^ name)) failures ;
  Printf.printf "\n%d checks, %d failed (%d codes declared, %d registered)\n"
    (List.length checks) (List.length failures)
    (StringSet.cardinal declared)
    (StringSet.cardinal registered) ;
  if StringSet.is_empty declared || StringSet.is_empty registered then (
    Printf.eprintf
      "BROKEN CHECK: %d codes declared and %d registered -- a list came back \
       empty, so this verified nothing\n"
      (StringSet.cardinal declared)
      (StringSet.cardinal registered) ;
    exit 2
  ) ;
  exit (match failures with [] -> 0 | _ -> 1)
