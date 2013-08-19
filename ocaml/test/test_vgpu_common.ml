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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

open Xapi_vgpu_type

let k100 = {
	vendor_name = "NVIDIA Corporation";
	model_name = "GRID K100";
	framebuffer_size = 268435456L;
	max_heads = 2L;
	size = Int64.div Constants.pgpu_default_size 8L;
	internal_config = [];
}

let k140q = {
	vendor_name = "NVIDIA Corporation";
	model_name = "GRID K140Q";
	framebuffer_size = 1006632960L;
	max_heads = 2L;
	size = Int64.div Constants.pgpu_default_size 4L;
	internal_config = [];
}

let k200 = {
	vendor_name = "NVIDIA Corporation";
	model_name = "GRID K200";
	framebuffer_size = 268435456L;
	max_heads = 2L;
	size = Int64.div Constants.pgpu_default_size 8L;
	internal_config = [];
}

let k240q = {
	vendor_name = "NVIDIA Corporation";
	model_name = "GRID K240Q";
	framebuffer_size = 1006632960L;
	max_heads = 2L;
	size = Int64.div Constants.pgpu_default_size 4L;
	internal_config = [];
}

let k260q = {
	vendor_name = "NVIDIA Corporation";
	model_name = "GRID K260Q";
	framebuffer_size = 2013265920L;
	max_heads = 4L;
	size = Int64.div Constants.pgpu_default_size 2L;
	internal_config = [];
}

let k1_vgpu_types = [
	k100;
	k140q;
	entire_gpu;
]

let k2_vgpu_types = [
	k200;
	k240q;
	k260q;
	entire_gpu;
]
