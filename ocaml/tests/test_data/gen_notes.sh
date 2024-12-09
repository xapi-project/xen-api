#!/bin/bash
#
# Copyright (c) Cloud Software Group, Inc.
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as published
# by the Free Software Foundation; version 2.1 only. with the special
# exception on linking described in file LICENSE.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.

elf_file=test_data/xenserver_elf_file
as "$@" -o $elf_file

sections=$(readelf -n $elf_file | grep -Po "(?<=Displaying notes found in: ).*")
for dep in $sections; do
        objcopy "$elf_file" "$dep" --only-section="$dep" -O binary
done

