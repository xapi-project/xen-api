/*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
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

function $editables() {

}

$editables.prototype = {
    vm : {
	name_description : function (ref,new_description) {
	    $xapi.xapi.VM.set_name_description(function() {}, $xapi.session,ref,new_description);
	},
	name_label : function (ref,new_name) {
	    $xapi.xapi.VM.set_name_label(function() {}, $xapi.session,ref,new_name);
	},
	memory_static_max : function (ref, new_mem_max) {
	    $xapi.xapi.VM.set_memory_static_max(function() {}, $xapi.session,ref,new_mem_max);
	},
	memory_static_min : function (ref, new_mem_min) {
	    $xapi.xapi.VM.set_memory_static_min(function() {}, $xapi.session,ref,new_mem_min);
	},
	memory_dynamic_max : function (ref, new_mem_max) {
	    $xapi.xapi.VM.set_memory_dynamic_max(function() {}, $xapi.session,ref,new_mem_max);
	},
	memory_dynamic_min : function (ref, new_mem_min) {
	    $xapi.xapi.VM.set_memory_dynamic_min(function() {}, $xapi.session,ref,new_mem_min);
	},
	VCPUs_max : function (ref, new_vcpu_max) {
	    $xapi.xapi.VM.set_VCPUs_max(function() {}, $xapi.session, ref, new_vcpu_max);
	}
    }
}

