(*
   Copyright (c) Cloud Software Group, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License as published
   by the Free Software Foundation; version 2.1 only. with the special
   exception on linking described in file LICENSE.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Lesser General Public License for more details.
 *)

(* This module provides functions to communicate with with the
   host-driver-tool that manages multi-version drivers and reports
   the state of them in JSON *)

open Debug.Make (struct let name = __MODULE__ end)

(** types to represent the JSON output of the script that reports
    drivers *)
type variant = {
    version: string
  ; hw_present: bool
  ; priority: float
  ; dev_status: string
}

type driver = {
    ty: string
  ; name: string
  ; descr: string
  ; info: string
  ; selected: string option
  ; active: string option
  ; variants: (string * variant) list
}

type t = {protocol: string; operation: string; drivers: (string * driver) list}

(** names for drivers and variants must only have certain characters *)
let is_legal_char = function
  | 'a' .. 'z' ->
      true
  | 'A' .. 'Z' ->
      true
  | '0' .. '9' ->
      true
  | '_' ->
      true
  | '.' ->
      true
  | '@' ->
      true
  | _ ->
      false

module R = Result

(** create an error result from printf format string *)
let error fmt = Printf.ksprintf (fun str -> R.error str) fmt

(** currently unused but needs to be used for names of drivers *)
let _is_legal_name name =
  match String.for_all is_legal_char name with
  | true ->
      R.ok name
  | false ->
      error "'%s' contains illegal characters" name

(* Wrap the combinators to parse JSON such that they don't raise
   exceptions but return Result.t values. These can be used for
   monadic error handling *)
module J = struct
  module U = Yojson.Basic.Util
  module B = Yojson.Basic

  let wrap f x =
    try f x |> R.ok with
    | U.Type_error (msg, json) ->
        error "%s in %s" msg (Yojson.Basic.to_string json)
    | e ->
        R.error (Printexc.to_string e)

  let wrap2 f x y =
    try f x y |> R.ok with
    | U.Type_error (msg, json) ->
        error "%s in %s" msg (Yojson.Basic.to_string json)
    | e ->
        R.error (Printexc.to_string e)

  let _keys = wrap U.keys

  let _values = wrap U.values

  let _combine = wrap2 U.combine

  let member key json =
    match U.member key json with
    | x ->
        R.ok x
    | exception U.Type_error (msg, json) ->
        error "%s in %s" msg (Yojson.Basic.to_string json)
    | exception e ->
        R.error (Printexc.to_string e)

  let _path = wrap2 U.path

  let _index = wrap2 U.index

  let _map = wrap2 U.map

  let to_assoc = wrap U.to_assoc

  let _to_option f = wrap (U.to_option f)

  let to_bool = wrap U.to_bool

  let _to_bool_option = wrap U.to_bool_option

  let to_number = wrap U.to_number

  let _to_number_option = wrap U.to_number_option

  let _to_float = wrap U.to_float

  let _to_float_option = wrap U.to_float_option

  let _to_int = wrap U.to_int

  let _to_int_option = wrap U.to_int_option

  let _to_list = wrap U.to_list

  let to_string = wrap U.to_string

  let to_string_option = wrap U.to_string_option

  let _convert_each f = wrap (U.convert_each f)
end

(** combinators for results and accessing members in JSON objects *)
let ( let* ) = Result.bind

let ( ||> ) = Result.bind

let ( ||. ) json key = json ||> J.member key

let ( |. ) json key = json |> J.member key

(** combine a list of ok list of oks into an ok list: 
    ('a, 'b) result list -> ('a list, 'b) result.  *)
let _combine results =
  let rec loop acc = function
    | [] ->
        R.Ok (List.rev acc)
    | R.Ok x :: xs ->
        loop (x :: acc) xs
    | (R.Error _ as err) :: _ ->
        err
  in
  loop [] results

(** combine an association list where the value is ok/error into an ok
    association list *)
let combine_assoc results =
  let rec loop acc = function
    | [] ->
        R.Ok (List.rev acc)
    | (key, R.Ok x) :: xs ->
        loop ((key, x) :: acc) xs
    | (_, (R.Error _ as err)) :: _ ->
        err
  in
  loop [] results

let _protocol json = json |. "protocol" ||. "version" ||> J.to_string

let _operation json = json |. "operation" ||. "reboot" ||> J.to_bool

let variant json =
  let* version = json |. "version" ||> J.to_string in
  let* hw_present = json |. "hardware_present" ||> J.to_bool in
  let* priority = json |. "priority" ||> J.to_number in
  let* dev_status = json |. "status" ||> J.to_string in
  R.ok {version; hw_present; priority; dev_status}

let variants json =
  let* assoc = J.to_assoc json in
  assoc |> List.map (fun (name, json) -> (name, variant json)) |> combine_assoc

let driver json =
  let* ty = json |. "type" ||> J.to_string in
  let* name = json |. "friendly_name" ||> J.to_string in
  let* descr = json |. "description" ||> J.to_string in
  let* info = json |. "info" ||> J.to_string in
  let* selected = json |. "selected" ||> J.to_string_option in
  let* active = json |. "active" ||> J.to_string_option in
  let* variants = json |. "variants" ||> variants in
  R.ok {ty; name; descr; info; selected; active; variants}

let drivers json =
  let* assoc = J.to_assoc json in
  assoc |> List.map (fun (name, json) -> (name, driver json)) |> combine_assoc

let t json = json |. "drivers" ||> drivers

let parse str =
  match J.B.from_string str |> t with
  | R.Ok x ->
      x
  | R.Error msg ->
      Helpers.internal_error ~log_err:true "%s parsing failed: %s" __FUNCTION__
        msg
  | exception e ->
      raise e

let read path =
  match J.B.from_file path |> t with
  | R.Ok x ->
      x
  | R.Error msg ->
      Helpers.internal_error ~log_err:true "%s parsing %s failed: %s"
        __FUNCTION__ path msg
  | exception e ->
      raise e

let call args =
  let path = !Xapi_globs.driver_tool in
  try
    let stdout, _stderr = Forkhelpers.execute_command_get_output path args in
    debug "%s: executed %s %s" __FUNCTION__ path (String.concat " " args) ;
    stdout
  with e ->
    Helpers.internal_error ~log_err:true "%s: failed to run %s %s: %s"
      __FUNCTION__ path (String.concat " " args) (Printexc.to_string e)

module Mock = struct
  let drivertool_sh =
    {|#!/usr/bin/env bash

set -o errexit
set -o pipefail

function selection {
  cat <<EOF
{
  "driver": "$1",
  "variant": "$2",
  "exit": 0
}
EOF
}

function list() {
  cat <<EOF

{
    "protocol": {
        "version": 0.1
    },
    "operation": {
        "reboot": false
    },
    "drivers": {
        "avago-megaraid-sas": {
            "type": "storage",
            "friendly_name": "avago-megaraid-sas",
            "description": "avago-megaraid-sas",
            "info": "avago-megaraid-sas",
            "selected": null,
            "active": null,
            "variants": {
                "generic": {
                    "version": "1.2",
                    "hardware_present": false,
                    "priority": 100,
                    "status": "beta"
                }
            }
        },
        "avago-mpt3sas": {
            "type": "storage",
            "friendly_name": "avago-mpt3sas",
            "description": "avago-mpt3sas",
            "info": "avago-mpt3sas",
            "selected": null,
            "active": null,
            "variants": {
                "generic": {
                    "version": "1.2",
                    "hardware_present": false,
                    "priority": 100,
                    "status": "beta"
                }
            }
        },
        "broadcom-bnxt-en": {
            "type": "network",
            "friendly_name": "broadcom-bnxt-en",
            "description": "broadcom-bnxt-en",
            "info": "broadcom-bnxt-en",
            "selected": "generic",
            "active": "generic",
            "variants": {
                "generic": {
                    "version": "1.2",
                    "hardware_present": true,
                    "priority": 100,
                    "status": "beta"
                }
            }
        },
        "broadcom-mpi3mr": {
            "type": "storage",
            "friendly_name": "broadcom-mpi3mr",
            "description": "broadcom-mpi3mr",
            "info": "broadcom-mpi3mr",
            "selected": null,
            "active": null,
            "variants": {
                "generic": {
                    "version": "1.2",
                    "hardware_present": false,
                    "priority": 100,
                    "status": "beta"
                }
            }
        },
        "chelsio-cxgb4": {
            "type": "network",
            "friendly_name": "chelsio-cxgb4",
            "description": "chelsio-cxgb4",
            "info": "chelsio-cxgb4",
            "selected": "oem",
            "active": "oem",
            "variants": {
                "generic": {
                    "version": "1.2",
                    "hardware_present": true,
                    "priority": 100,
                    "status": "beta"
                },
                "oem": {
                    "version": "1.3",
                    "hardware_present": true,
                    "priority": 80,
                    "status": "beta"
                }
            }
        },
        "cisco-enic": {
            "type": "network",
            "friendly_name": "cisco-enic",
            "description": "cisco-enic",
            "info": "cisco-enic",
            "selected": "generic",
            "active": "dell",
            "variants": {
                "generic": {
                    "version": "1.2",
                    "hardware_present": true,
                    "priority": 100,
                    "status": "beta"
                },
                "dell": {
                    "version": "1.5",
                    "hardware_present": true,
                    "priority": 90,
                    "status": "beta"
                }
            }
        },
        "cisco-fnic": {
            "type": "network",
            "friendly_name": "cisco-fnic",
            "description": "cisco-fnic",
            "info": "cisco-fnic",
            "selected": null,
            "active": null,
            "variants": {
                "generic": {
                    "version": "1.2",
                    "hardware_present": false,
                    "priority": 100,
                    "status": "beta"
                }
            }
        },
        "emulex-lpfc": {
            "type": "network",
            "friendly_name": "emulex-lpfc",
            "description": "emulex-lpfc",
            "info": "emulex-lpfc",
            "selected": null,
            "active": null,
            "variants": {
                "generic": {
                    "version": "1.2",
                    "hardware_present": false,
                    "priority": 100,
                    "status": "beta"
                }
            }
        },
        "intel-e1000e": {
            "type": "network",
            "friendly_name": "intel-e1000e",
            "description": "intel-e1000e",
            "info": "intel-e1000e",
            "selected": null,
            "active": null,
            "variants": {
                "generic": {
                    "version": "1.2",
                    "hardware_present": false,
                    "priority": 100,
                    "status": "beta"
                }
            }
        },
        "intel-fm10k": {
            "type": "network",
            "friendly_name": "intel-fm10k",
            "description": "intel-fm10k",
            "info": "intel-fm10k",
            "selected": null,
            "active": null,
            "variants": {
                "generic": {
                    "version": "1.2",
                    "hardware_present": false,
                    "priority": 100,
                    "status": "beta"
                }
            }
        },
        "intel-i40e": {
            "type": "network",
            "friendly_name": "intel-i40e",
            "description": "intel-i40e",
            "info": "intel-i40e",
            "selected": null,
            "active": null,
            "variants": {
                "generic": {
                    "version": "1.2",
                    "hardware_present": false,
                    "priority": 100,
                    "status": "beta"
                }
            }
        },
        "intel-ice": {
            "type": "network",
            "friendly_name": "intel-ice",
            "description": "intel-ice",
            "info": "intel-ice",
            "selected": null,
            "active": null,
            "variants": {
                "generic": {
                    "version": "1.2",
                    "hardware_present": false,
                    "priority": 100,
                    "status": "beta"
                }
            }
        },
        "intel-igb": {
            "type": "network",
            "friendly_name": "intel-igb",
            "description": "intel-igb",
            "info": "intel-igb",
            "selected": null,
            "active": null,
            "variants": {
                "generic": {
                    "version": "1.2",
                    "hardware_present": false,
                    "priority": 100,
                    "status": "beta"
                }
            }
        },
        "intel-igc": {
            "type": "network",
            "friendly_name": "intel-igc",
            "description": "",
            "info": "",
            "selected": null,
            "active": null,
            "variants": {
                "generic": {
                    "version": "1.2",
                    "hardware_present": false,
                    "priority": 100,
                    "status": "beta"
                }
            }
        },
        "intel-ixgbe": {
            "type": "network",
            "friendly_name": "intel-ixgbe",
            "description": "intel-ixgbe",
            "info": "intel-ixgbe",
            "selected": null,
            "active": null,
            "variants": {
                "generic": {
                    "version": "1.2",
                    "hardware_present": false,
                    "priority": 100,
                    "status": "beta"
                }
            }
        },
        "mellanox-mlnxen": {
            "type": "network",
            "friendly_name": "mellanox-mlnxen",
            "description": "mellanox-mlnxen",
            "info": "mellanox-mlnxen",
            "selected": null,
            "active": null,
            "variants": {
                "generic": {
                    "version": "1.2",
                    "hardware_present": false,
                    "priority": 100,
                    "status": "beta"
                }
            }
        },
        "microsemi-aacraid": {
            "type": "storage",
            "friendly_name": "microsemi-aacrai",
            "description": "microsemi-aacrai",
            "info": "microsemi-aacrai",
            "selected": null,
            "active": null,
            "variants": {
                "generic": {
                    "version": "1.2",
                    "hardware_present": false,
                    "priority": 100,
                    "status": "beta"
                }
            }
        },
        "microsemi-smartpqi": {
            "type": "storage",
            "friendly_name": "microsemi-smartpqi",
            "description": "microsemi-smartpqi",
            "info": "microsemi-smartpqi",
            "selected": null,
            "active": null,
            "variants": {
                "generic": {
                    "version": "1.2",
                    "hardware_present": false,
                    "priority": 100,
                    "status": "beta"
                }
            }
        },
        "qlogic-fastlinq": {
            "type": "network",
            "friendly_name": "qlogic-fastlinq",
            "description": "qlogic-fastlinq",
            "info": "qlogic-fastlinq",
            "selected": null,
            "active": null,
            "variants": {
                "generic": {
                    "version": "1.2",
                    "hardware_present": false,
                    "priority": 100,
                    "status": "beta"
                }
            }
        },
        "qlogic-netxtreme2": {
            "type": "network",
            "friendly_name": "qlogic-netxtreme2",
            "description": "qlogic-netxtreme2",
            "info": "qlogic-netxtreme2",
            "selected": null,
            "active": null,
            "variants": {
                "generic": {
                    "version": "1.2",
                    "hardware_present": false,
                    "priority": 100,
                    "status": "beta"
                }
            }
        }
    }
}
EOF
}

# Initialize variables with default values
l_flag=false
s_flag=false
n_value=""
v_value=""

# Use getopt to parse command-line options
while getopts "lsn:v:" opt; do
  case "$opt" in
    l)
      l_flag=true
      ;;
    s)
      s_flag=true
      ;;
    n)
      n_value="$OPTARG"
      ;;
    v)
      v_value="$OPTARG"
      ;;
    \?)  # Invalid option
      echo "Invalid option: -$OPTARG" >&2  #>&2 redirects error message to stderr
      exit 1
      ;;
    :)   # Missing argument for option
      echo "Option -$OPTARG requires an argument." >&2
      exit 1
      ;;
  esac
done

# Shift the remaining positional parameters (if any)
shift $((OPTIND - 1))

# We don't properly prevent illegal combinations because this is just a
# mock. So we recognise -l first.
if $l_flag; then
  list
  exit 0
fi

if $s_flag; then
  if [ -z "$n_value" ]; then
    echo "missing -n" >&2
    exit 1
  fi
  if [ -z "$v_value" ]; then
    echo "missing -v" >&2
    exit 1
  fi

  selection "$n_value" "$v_value"
  exit 0
fi
|}

  let install () =
    let path = !Xapi_globs.driver_tool in
    try
      Xapi_stdext_unix.Unixext.write_string_to_file path drivertool_sh ;
      Unix.chmod path 0o755
    with e ->
      Helpers.internal_error "%s: can't install %s: %s" __FUNCTION__ path
        (Printexc.to_string e)
end
