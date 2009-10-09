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

$(document).ready(function() {$('#topbar').append($($.create("button",{ns:xhtmlns,id:"searchbutton","class":"topbutton"},["HA Plan"])).click(createhaplan));});

function docontent(win,placedvms,disabledhosts) {
    var height;
    var vms=[];
    var hosts=[];
    var c;
    var content=$('.content',win);	
    var resident;
    var vm;
    var i;
    var x;
    var mem;

    globalvms=placedvms;
    
    var errors="";

    for(vm in placedvms) {
	if(placedvms[vm].error_code) {
	    alert("Cant place VM: "+$xapi.xo.vm[vm].name_label);
	}
    }

    function isdisabled(host) {
	for(x=0; x<disabledhosts.length; x++)
	    if(disabledhosts[x]==host)
		return true;
	return false;
    }
	
    $.each($xapi.xo.host,function(i,val) {
	vms=[];
	if(!isdisabled(i)) {
	    $.each(val.resident_VMs,function(j,vm) {
		if($xapi.xo.vm_metrics[$xapi.xo.vm[vm].metrics]) {
		    mem=$xapi.xo.vm_metrics[$xapi.xo.vm[vm].metrics].memory_actual;
		} else {
		    mem=$xapi.xo.vm[$xapi.xo.vm[vm].memory_dynamic_max];
		}
		height=mem / (8*1024*1024);
		vms=vms.concat(["div",{ns:xhtmlns,"class":"HAVM",style:"height:"+height+"px"},
				[$xapi.xo.vm[vm].name_label]]);
	    });
	    $.each(placedvms,function(key,val) {
		if(val.host==i) {
		    height=$xapi.xo.vm_metrics[$xapi.xo.vm[key].metrics].memory_actual / (8*1024*1024);
		    vms=vms.concat(["div",{ns:xhtmlns,"class":"HAVMNEW",style:"height:"+height+"px"},
				    [$xapi.xo.vm[key].name_label]]);
		}
	    });
	}
	c=(isdisabled(i))?"HAHOSTDISABLED":"HAHOST";
	if($xapi.xo.host_metrics[val.metrics].live) {
	    height=$xapi.xo.host_metrics[val.metrics].memory_total/(8*1024*1024);
	    hosts=hosts.concat(["div",{ns:xhtmlns,"class":c,id:'ha'+i,style:"height:"+height+"px"},["Host: "+val.name_label].concat(vms)]);
	}

    });

    content.empty().append($($.create("div",{ns:xhtmlns},hosts)));

    $('.HAHOST',win).click(function(evt) {
	resident=[];
	vms=[];
	var tgt=$(evt.target);
	host=tgt.attr("id").slice(2);
	disabledhosts=disabledhosts.concat(host);

	for(xx=0; xx<disabledhosts.length; xx++) {
	    host=disabledhosts[xx];
	    for(i=0; i<$xapi.xo.host[host].resident_VMs.length; i++) {
		vm=$xapi.xo.host[host].resident_VMs[i];
		if(! $xapi.xo.vm[vm].is_control_domain) {
		    vms=vms.concat(vm);
		}
	    }
	}

	if (typeof netscape != "undefined") { 
	    //netscape.security.PrivilegeManager.enablePrivilege("UniversalXPConnect UniversalBrowserRead"); 
	} 

	$xapi.xapi.pool.ha_compute_vm_failover_plan(
	    (function(host,win,disabledhosts) {
		return function(vms) {
		    docontent(win,$xapi.check(vms),disabledhosts);
		}
	    })(host,win,disabledhosts),
	    $xapi.session,disabledhosts,vms);
	
    });
}

function createhaplan() {
    var win=$dw.createWindow("haplan","HA plan");
    var host;

    docontent(win,{},[]);    
}

