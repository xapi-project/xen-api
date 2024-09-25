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

module D = Debug.Make (struct let name = __MODULE__ end)

open D

let ( // ) = Filename.concat

module Group = struct
  module Internal = struct
    type t

    let name = "internal"
  end

  module External = struct
    type t

    let name = "external"
  end

  module Host = struct
    type t

    let name = "host"
  end

  module SM = struct
    type t

    let name = "SM"
  end

  type _ group =
    | Internal_Host_SM : (Internal.t * Host.t * SM.t) group
    | EXTERNAL : External.t group

  type t = Group : 'a group -> t

  let all = [Group Internal_Host_SM; Group EXTERNAL]

  module Originator = struct
    type t = Internal_Host_SM | EXTERNAL

    let of_string = function
      | s
        when String.equal
               (String.lowercase_ascii SM.name)
               (String.lowercase_ascii s) ->
          Internal_Host_SM
      | s
        when String.equal
               (String.lowercase_ascii External.name)
               (String.lowercase_ascii s) ->
          EXTERNAL
      | _ ->
          EXTERNAL

    let to_string = function
      | Internal_Host_SM ->
          SM.name
      | EXTERNAL ->
          External.name
  end

  module Creator = struct
    type t = {
        user: string option
      ; endpoint: string option
      ; originator: Originator.t
    }

    let make ?user ?endpoint originator = {originator; user; endpoint}

    let to_string c =
      Printf.sprintf "Creator -> user:%s endpoint:%s originator:%s"
        (Option.value c.user ~default:"")
        (Option.value c.endpoint ~default:"")
        (Originator.to_string c.originator)
  end

  let of_originator = function
    | Originator.Internal_Host_SM ->
        Group Internal_Host_SM
    | Originator.EXTERNAL ->
        Group EXTERNAL

  let get_originator = function
    | Group Internal_Host_SM ->
        Originator.Internal_Host_SM
    | Group EXTERNAL ->
        Originator.EXTERNAL

  let of_creator creator = of_originator creator.Creator.originator

  let to_cgroup : type a. a group -> string = function
    | Internal_Host_SM ->
        Internal.name // Host.name // SM.name
    | EXTERNAL ->
        External.name
end

module Cgroup = struct
  type t = string

  let cgroup_dir = Atomic.make None

  let dir_of group : t option =
    match group with
    | Group.Group group ->
        Option.map
          (fun dir -> dir // Group.to_cgroup group)
          (Atomic.get cgroup_dir)

  let init dir =
    let () = Atomic.set cgroup_dir (Some dir) in
    Group.all
    |> List.filter_map dir_of
    |> List.iter (fun dir -> Xapi_stdext_unix.Unixext.mkdir_rec dir 0o755)

  let write_cur_tid_to_cgroup_file filename =
    try
      let perms = 0o640 in
      let mode = [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] in
      Xapi_stdext_unix.Unixext.with_file filename mode perms @@ fun fd ->
      (* Writing 0 to the task file will automatically transform in writing
         the current caller tid to the file.

         Writing 0 to the processes file will automatically write the caller's
         pid to file. *)
      let buf = "0\n" in
      let len = String.length buf in
      if Unix.write fd (Bytes.unsafe_of_string buf) 0 len <> len then
        warn "writing current tid to %s failed" filename
    with exn ->
      warn "writing current tid to %s failed with exception: %s" filename
        (Printexc.to_string exn)

  let attach_task group =
    let tasks_file = dir_of group // "tasks" in
    write_cur_tid_to_cgroup_file tasks_file

  let set_cur_cgroup ~originator =
    match originator with
    | Group.Originator.Internal_Host_SM ->
        attach_task (Group Internal_Host_SM)
    | Group.Originator.EXTERNAL ->
        attach_task (Group EXTERNAL)

  let set_cgroup creator =
    set_cur_cgroup ~originator:creator.Group.Creator.originator
end

let of_originator originator =
  originator |> Group.Creator.make |> Cgroup.set_cgroup

let of_req_originator originator =
  try
    originator
    |> Option.value ~default:Group.Originator.(to_string EXTERNAL)
    |> Group.Originator.of_string
    |> of_originator
  with _ -> ()
