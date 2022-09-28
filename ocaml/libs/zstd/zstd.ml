(*
 * Copyright (C) 2018 Citrix Systems Inc.
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

module Default = Xapi_compression.Make (struct
  (** Path to the zstd binary *)
  let executable = "/usr/bin/zstd"

  let compress_options = []

  let decompress_options = ["--decompress"; "--stdout"]
end)

module Adaptive = Xapi_compression.Make (struct
  (** Path to the zstd binary *)
  let executable = "/usr/bin/zstd"

  (* default compression level is 3 on a scale 1..19 with lower=faster
     and higher=more compression. By default 1 thread is used *)
  let compress_options = ["--adapt=min=1,max=4"; "--threads=1"]

  let decompress_options = ["--decompress"; "--stdout"]
end)

module Fast = Xapi_compression.Make (struct
  (** Path to the zstd binary *)
  let executable = "/usr/bin/zstd"

  (** --threads=2 doesn't improve speed *)
  let compress_options = ["--fast"]

  let decompress_options = ["--decompress"; "--stdout"]
end)

module Null = Xapi_compression.Make (struct
  (** Path to the zstd binary *)
  let executable = "/usr/bin/cat"

  let compress_options = []

  let decompress_options = []
end)
