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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Lesser General Public License for more details.
 *)


open Data_source

let to_API_data_source (ds : t) = {
  API.data_source_name_label = ds.name;
  data_source_name_description = ds.description;
  data_source_enabled = ds.enabled;
  data_source_standard = ds.standard;
  data_source_units = ds.units;
  data_source_min = ds.min;
  data_source_max = ds.max;
  data_source_value = 0.;
}
