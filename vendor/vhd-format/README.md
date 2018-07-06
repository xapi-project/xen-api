ocaml-vhd
=========

A pure OCaml library to read and write [vhd](http://en.wikipedia.org/wiki/VHD_(file_format)) format data, plus a simple command-line tool which allows vhd files to be interrogated, manipulated, format-converted and streamed to and from files and remote servers.

Example usage
-------------

To initialise your environment in utop:
```
open Lwt;;
#require "vhd-format";;
#require "vhd-format-lwt";;
module V = Vhd.Make(Vhd_lwt);;
```

To open a file and read the first sector:
```
V.Vhd_IO.openfile "foo.vhd" >>= fun f -> V.Vhd_IO.read_sector f 0L;;
- : Cstruct.t option = Some {Cstruct.buffer = <abstr>; off = 0; len = 512}   
```

To read the vhd file header:
```
V.Vhd_IO.openfile "89a62601-c82a-447a-a7fd-f7f379195e80.vhd" >>= fun f -> return f.Vhd.Vhd.header;;
- : Vhd.Header.t = {
 Vhd.Header.table_offset = 1536L; max_table_entries = 12288;
 block_size_sectors_shift = 12; checksum = -3001l; parent_unique_id = <abstr>;  
 parent_time_stamp = 0l; parent_unicode_name = [||];
 parent_locators =
  [|{Vhd.Parent_locator.platform_code = Vhd.Platform_code.None;
     platform_data_space = 0l; platform_data_space_original = 0l;
     platform_data_length = 0l; platform_data_offset = 0L;
     platform_data = {Cstruct.buffer = <abstr>; off = 0; len = 0}};
    {Vhd.Parent_locator.platform_code = Vhd.Platform_code.None;
     platform_data_space = 0l; platform_data_space_original = 0l;
     platform_data_length = 0l; platform_data_offset = 0L;
     platform_data = {Cstruct.buffer = <abstr>; off = 0; len = 0}};
    {Vhd.Parent_locator.platform_code = Vhd.Platform_code.None;
     platform_data_space = 0l; platform_data_space_original = 0l;
     platform_data_length = 0l; platform_data_offset = 0L;
     platform_data = {Cstruct.buffer = <abstr>; off = 0; len = 0}};
    {Vhd.Parent_locator.platform_code = Vhd.Platform_code.None;
     platform_data_space = 0l; platform_data_space_original = 0l;
     platform_data_length = 0l; platform_data_offset = 0L;
     platform_data = {Cstruct.buffer = <abstr>; off = 0; len = 0}};
    {Vhd.Parent_locator.platform_code = Vhd.Platform_code.None;
     platform_data_space = 0l; platform_data_space_original = 0l;
     platform_data_length = 0l; platform_data_offset = 0L;
     platform_data = {Cstruct.buffer = <abstr>; off = 0; len = 0}};
    {Vhd.Parent_locator.platform_code = Vhd.Platform_code.None;
     platform_data_space = 0l; platform_data_space_original = 0l;
     platform_data_length = 0l; platform_data_offset = 0L;
     platform_data = {Cstruct.buffer = <abstr>; off = 0; len = 0}};
    {Vhd.Parent_locator.platform_code = Vhd.Platform_code.None;
     platform_data_space = 0l; platform_data_space_original = 0l;
     platform_data_length = 0l; platform_data_offset = 0L;
     platform_data = {Cstruct.buffer = <abstr>; off = 0; len = 0}};
    {Vhd.Parent_locator.platform_code = Vhd.Platform_code.None;
     platform_data_space = 0l; platform_data_space_original = 0l;
     platform_data_length = 0l; platform_data_offset = 0L;
     platform_data = {Cstruct.buffer = <abstr>; off = 0; len = 0}}|]}
```

