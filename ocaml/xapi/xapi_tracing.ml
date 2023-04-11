(*
 * Copyright (C) 2023 Cloud Software Group
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
let create ~__context ~name_label:_ ~hosts:_ ~status:_ ~tags:_ ~endpoints:_
    ~components:_ ~filters:_ ~processors:_ =
  ()

let destroy ~__context ~self:_ = ()

let set_hosts ~__context ~self:_ ~hosts:_ = ()

let set_status ~__context ~self:_ ~status:_ = ()

let set_tags ~__context ~self:_ ~tags:_ = ()

let set_endpoints ~__context ~self:_ ~endpoints:_ = ()

let set_components ~__context ~self:_ ~components:_ = ()

(* Will implement later, this function will set / unset providers on veraious components *)
let set_filters ~__context ~self:_ ~filters:_ = ()

let set_processors ~__context ~self:_ ~processors:_ = ()
