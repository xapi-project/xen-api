.section ".note.Linux", "a"
    .p2align 2
    .long 1f - 0f           # name size (not including padding)
    .long 3f - 2f           # desc size (not including padding)
    .long 0x257             # type
0:  .asciz "Linux"	    # name
1:  .p2align 2
2:  .asciz "4.19.0+1"       # desc
3:  .p2align 2
