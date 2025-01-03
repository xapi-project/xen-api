.section ".note.gnu.build-id", "a"
    .p2align 2
    .long 1f - 0f           # name size (not including padding)
    .long 3f - 2f           # desc size (not including padding)
    .long 0x1               # type
0:  .asciz "gnu.build-id"   # name
1:  .p2align 2
2:  .long 0x000000          # desc
3:  .p2align 2
