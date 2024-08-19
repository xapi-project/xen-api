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

test_file_dir=$1
mkdir -p "${test_file_dir}"
cd "${test_file_dir}" || exit

echo "========= Generating regular tar file ========="
echo "This is file-1" > file1.txt
echo "This is file-2" > file2.txt
tar -cvf test_tar_ext_regular.tar file1.txt file2.txt

echo "========= Generating tar file with illegal path ========="
mkdir test_illegal_dir
touch test_illegal_dir/file
tar --absolute-names -cvf test_tar_ext_illegal_path.tar test_illegal_dir/../file1.txt

echo "========= Generating tar file trying to escape the current dir ========="
mkdir current_dir
mkdir another_dir
touch current_dir/file
touch another_dir/escaped_file
tar --absolute-names -cvf current_dir/test_tar_ext_trying_to_escape.tar current_dir/../another_dir/escaped_file

echo "========= Generating tar file with absolute path starting from '/' ========="
tar --absolute-names -cvf test_tar_ext_absolute_path.tar /usr/bin/ls

echo "========= Generating tar file with unsupported file type ========="
ln -s file1.txt link
tar -cvf test_tar_ext_unsupported_file_type.tar link

echo "========= Generating tar file unpacked exceeds max size limit ========="
dd if=/dev/zero of=file1 bs=1M count=1
dd if=/dev/zero of=file2 bs=1M count=1
dd if=/dev/zero of=file3 bs=1M count=1
tar -cvf test_tar_ext_unpacked_exceeds_max_size.tar file1 file2 file3

echo "========= Generating size mismatch tar file ========="
split -b 100000 test_tar_ext_unpacked_exceeds_max_size.tar test_tar_ext_file_size_mismatch.
mv test_tar_ext_file_size_mismatch.aa test_tar_ext_file_size_mismatch.tar

echo "========= Generating incomplete tar file ========="
mv file1.txt test_tar_ext_file_incomplete.tar

echo "========= Generating corrupted tar file ========="
cp test_tar_ext_regular.tar test_tar_ext_corrupted_file.tar
sed -i 's/file1.txt/file3.txt/g' test_tar_ext_corrupted_file.tar

echo "========= Generating unpacking failure file ========="
cp test_tar_ext_regular.tar test_tar_ext_unpacking_failure.tar
sed -i 's/file1.txt/file.txt/g' test_tar_ext_unpacking_failure.tar
