.section ".note.XenServerTwo", "a"
    .p2align 2
    .long 1f - 0f           # name size (not including padding)
    .long 3f - 2f           # desc size (not including padding)
    .long 0x2               # type
0:  .asciz "XenServer"	    # name
1:  .p2align 2
2:  .asciz "Built on December 25th"  # desc
3:  .p2align 2

.section ".note.XenServerTwo", "a"
    .p2align 2
    .long 1f - 0f           # name size (not including padding)
    .long 3f - 2f           # desc size (not including padding)
    .long 0x1               # type
0:  .asciz "XenServer"	    # name
1:  .p2align 2
2:  .asciz "2.0.0-rc.2"  # desc
3:  .p2align 2

