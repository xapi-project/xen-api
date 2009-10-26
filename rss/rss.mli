(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
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

type item_info = {
  item_title : string;
  item_link : string option;
  item_description : string;
  item_pubdate : string;
}
type channel_info = {
  chan_title : string;
  chan_description : string;
  chan_language : string;
  chan_pubdate : string;
  chan_items : item_info list;
}
val to_xml : channel_info list -> Xml.xml
val to_stream : channel_info list -> out_channel -> unit
