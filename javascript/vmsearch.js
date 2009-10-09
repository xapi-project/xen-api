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

$(document).ready(function() {$('#topbar').append($($.create("button",{ns:xhtmlns,id:"searchbutton","class":"topbutton"},["Search"])).click(createsearch));});

function createsearch() {

    var win=$dw.createWindow("search","search");

    var content=$('.content',win);	
    
    content.append($($.create("label",{ns:xhtmlns,"for":"vmsearch"},["VM:","input",{ns:xhtmlns,type:"text",value:"",id:"vmsearchinput","class":"credentials"},[]],"div",{ns:xhtmlns,id:"searchresults"},[])));

    $('#vmsearchinput',content).keyup(dosearch);
}


function dosearch() {

    var searchterm=$('#vmsearchinput').val();

    var results=$('#searchresults').empty();

    var vms=searchresults(searchterm);

    for(var i=0; i<vms.length; i++) {
	var vm=vms[i];
	var vmspan=$($.create("span",{ns:xhtmlns},[$xapi.xo.vm[vm].name_label],"br",{ns:xhtmlns},[]));

	vmspan.click(function(vm) {return (function() {$dw.createObjView(vm.slice(10))})}(vm));
	results.append(vmspan);
    }
}

function searchresults(searchterm)  {

    var results=new Array;
    var regex=new RegExp(searchterm);

    for(var vm in $xapi.xo.vm) {
	if(regex.test($xapi.xo.vm[vm].name_label))
	    results=results.concat(vm);
    }

    return results;

}


