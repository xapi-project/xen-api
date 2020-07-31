(*
 * Copyright (C) Citrix Systems Inc.
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

type reader = Rrd_reader_functor.reader = {
  read_payload: unit -> Rrd_protocol.payload;
  cleanup: unit -> unit;
}

include Rrd_file_reader
include Rrd_page_reader

module FileReader = Rrd_file_reader
module PageReader = Rrd_page_reader
