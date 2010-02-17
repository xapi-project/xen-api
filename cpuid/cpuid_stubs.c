/*
 * Copyright (C) 2006-2010 Citrix Systems Inc.
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
 */

#include <stdint.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>

CAMLprim value do_cpuid(value leaf, value word)
{
    int32_t eax, ebx, ecx, edx, tmp;

    CAMLparam2(leaf, word);
    CAMLlocal1(rv);

    eax = Int32_val(leaf);
    ecx = Int32_val(word);

    /* Execute CPUID; the MOVs are because ocamlc uses -fPIC and
     * 32-bit gcc won't let you just use "=b" to get at %ebx in PIC */
    asm("mov %%ebx, %4 ; cpuid ; mov %%ebx, %1 ; mov %4, %%ebx " 
        : "+a" (eax), "=r" (ebx), "+c" (ecx), "=d" (edx), "=r" (tmp));
    
    /* Wrap the return value up as an OCaml tuple */
    rv = caml_alloc_tuple(4);
    Store_field(rv, 0, caml_copy_int32(eax));
    Store_field(rv, 1, caml_copy_int32(ebx));
    Store_field(rv, 2, caml_copy_int32(ecx));
    Store_field(rv, 3, caml_copy_int32(edx));

    CAMLreturn(rv);
}
