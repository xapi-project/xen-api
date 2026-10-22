(*
 * Copyright (C) 2026 Cloud Software Group
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

type profile = P1g_6gb | P2g_12gb | P4g_24gb

let profiles = [P1g_6gb; P2g_12gb; P4g_24gb]

let request p = Gpu.Gpu_capacity.{profile= p; vgpu_type= p; weight= 1}

let measured = [P2g_12gb; P1g_6gb; P1g_6gb]
