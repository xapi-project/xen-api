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

/* Example of how to add functionality to devweb */

$(document).ready(function() {$('#topbar').append($($.create("button",{ns:xhtmlns,id:"searchbutton","class":"topbutton"},["Networks"])).click(createnetworkswin));});

function createnetworkswin() {
    var win=$dw.createWindow("networksview","Networks");
    var host;

    $('.content',win).empty().append($($.create(
	"div",{ns:xhtmlns,"class":"nethost"},[
	    "HOST: Test",
	    "div",{ns:xhtmlns,"class":"netbond"},[
		"BOND0",
		"div",{ns:xhtmlns,"class":"netphyscontainer"},[
		    "div",{ns:xhtmlns,"class":"netphys"},["eth0"],
		    "div",{ns:xhtmlns,"class":"netphys"},["eth1"]],
		"div",{ns:xhtmlns,"class":"netvlancontainer"},[
		    "div",{ns:xhtmlns,"class":"netvlan",style:"width:50%"},["untagged"],
		    "div",{ns:xhtmlns,"class":"netvlan",style:"width:50%"},["vlan0"]]],
	    "br",{ns:xhtmlns,style:"clear:both;"},[],
	    "HOST: Test",
	    "div",{ns:xhtmlns,"class":"netbond"},[
		"BOND0",
		"div",{ns:xhtmlns,"class":"netphyscontainer"},[
		    "div",{ns:xhtmlns,"class":"netphys"},["eth0"],
		    "div",{ns:xhtmlns,"class":"netphys"},["eth1"]],
		"div",{ns:xhtmlns,"class":"netvlancontainer"},[
		    "div",{ns:xhtmlns,"class":"netvlan",style:"width:50%"},["untagged"],
		    "div",{ns:xhtmlns,"class":"netvlan",style:"width:50%"},["vlan0"]]],
	    "br",{ns:xhtmlns,style:"clear:both;"},[]
	])));
}