Create a sparse raw file
  $ dd if=/dev/random of=test.raw bs=2097152 count=16 > /dev/null 2>&1
  $ dd if=/dev/zero of=test.raw bs=2097152 count=10 seek=3 conv=notrunc > /dev/null 2>&1

Convert to .vhd
  $ qemu-img convert -f raw -O vpc test.raw test.vhd

Check if the right clusters are found to be allocated (legacy JSON format)
  $ ./main.exe read_headers test.vhd
  {"virtual_size":33562624,"cluster_bits":21,"data_clusters":[0,1,2,13,14,15]}

Check fully allocated file
  $ dd if=/dev/random of=test.raw bs=2097152 count=16 > /dev/null 2>&1
  $ qemu-img convert -f raw -O vpc test.raw test.vhd
  $ ./main.exe read_headers test.vhd
  {"virtual_size":33562624,"cluster_bits":21,"data_clusters":[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]}

Check empty file
  $ dd if=/dev/zero of=test.raw bs=2097152 count=16 > /dev/null 2>&1
  $ qemu-img convert -f raw -O vpc test.raw test.vhd
  $ ./main.exe read_headers test.vhd
  {"virtual_size":33562624,"cluster_bits":21,"data_clusters":[]}
