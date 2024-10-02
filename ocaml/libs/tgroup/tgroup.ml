(*
 * Copyright (C) Cloud Software Group
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT Group WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

module D = Debug.Make (struct let name = __MODULE__ end)

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

  module Server = struct
    type t

    let name = "server"
  end

  module SM = struct
    type t

    let name = "SM"
  end

  type _ group =
    | Internal_Host_SM : (Internal.t * Host.t * SM.t) group
    | EXTERNAL : External.t group
    | Internal_Server : (Internal.t * Server.t) group

  type t = Group : 'a group -> t

  let all = [Group Internal_Host_SM; Group EXTERNAL; Group Internal_Server]

  module Originator = struct
    type t = Internal_Host_SM | EXTERNAL | Internal_Server

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
      | Internal_Server ->
          Server.name
      | EXTERNAL ->
          External.name
  end

  module Creator = struct
    type t = {originator: Originator.t}

    let make originator = {originator}
  end

  let of_originator = function
    | Originator.Internal_Host_SM ->
        Group Internal_Host_SM
    | Originator.Internal_Server ->
        Group Internal_Server
    | Originator.EXTERNAL ->
        Group EXTERNAL

  let get_originator = function
    | Group Internal_Host_SM ->
        Originator.Internal_Host_SM
    | Group EXTERNAL ->
        Originator.EXTERNAL
    | Group Internal_Server ->
        Originator.Internal_Server

  let of_creator creator = of_originator creator.Creator.originator

  let to_cgroup : type a. a group -> string = function
    | Internal_Host_SM ->
        Internal.name // Host.name // SM.name
    | EXTERNAL ->
        External.name
    | Internal_Server ->
        Internal.name // Server.name
end

module Cgroup = struct
  type t = string

  exception Cgroups_not_initialized

  let cgroup_dir = Atomic.make None

  let dir_of group : t =
    match group with
    | Group.Group group -> (
      try (cgroup_dir |> Atomic.get |> Option.get) // Group.to_cgroup group
      with Invalid_argument _ -> raise Cgroups_not_initialized
    )

  let init dir =
    let () = Atomic.set cgroup_dir (Some dir) in
    Group.all
    |> List.map dir_of
    |> List.iter (fun dir -> Xapi_stdext_unix.Unixext.mkdir_rec dir 0o755)
end
