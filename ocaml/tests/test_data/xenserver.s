.section ".note.XenServer", "a"
    .p2align 2
    .long 1f - 0f           # name size (not including padding)
    .long 3f - 2f           # desc size (not including padding)
    .long 0x1               # type
0:  .asciz "XenServer"	    # name
1:  .p2align 2
2:  .asciz "v2.1.3+0.1fix"  # desc
3:  .p2align 2
