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

/* Some override functions for particular classes */


function $overrides() {

}


$overrides.prototype = {
    vm : function(window, id, classes) {
	var pre=$('.pre',window);
	pre.empty();
	
	var ao=classes.vm[id].allowed_operations;

	var pre=$('.pre',window);

	if($xapi.apiversion.minor>2) {
	    elt=$($.create("input",{type:"button",value:"plot rrds","class":"vmop",ns:xhtmlns},[]));
	    elt.click(function() { $dw.testrrd(classes.vm[id].uuid,["vbd","vif","cpu","memory"]) });
	    pre.append(elt);
	}
	
	for(opn=0; opn<ao.length; opn++) {
	    op=ao[opn];
	    
	    switch(op) {
	    case 'start':
		elt=$($.create("input",{type:"button",value:op,"class":"vmop",ns:xhtmlns},[]));
		function scope(myop,myid) {
		    elt.click(function() { 
			if (typeof netscape != "undefined") { 
			    //netscape.security.PrivilegeManager.enablePrivilege("UniversalXPConnect UniversalBrowserRead");
			} 
			$xapi.xapi.VM.start(function() {}, $xapi.session, myid,false,false) });
		};
		scope(op,id);
		pre.append(elt);
		break;
	    case 'hard_shutdown':
	    case 'hard_reboot':
	    case 'clean_shutdown':
	    case 'clean_reboot':
	    case 'suspend':
	    case 'unpause':
	    case 'pause':
		elt=$($.create("input",{type:"button",value:op,"class":"vmop",ns:xhtmlns},[]));
		function scope(myop,myid) {
		    elt.click(function() { 
			if (typeof netscape != "undefined") { 
			    //netscape.security.PrivilegeManager.enablePrivilege("UniversalXPConnect UniversalBrowserRead"); 
			} 
			$xapi.xapi.VM[myop](function() {}, $xapi.session, myid) });
		};
		scope(op,id);
		pre.append(elt);
		break;
	    case 'resume':
		elt=$($.create("input",{type:"button",value:op,"class":"vmop",ns:xhtmlns},[]));
		function scope(myop,myid) {
		    elt.click(function() { 
			$xapi.xapi.VM.resume(function() {}, $xapi.session, myid,false,false) });
		};
		scope(op,id);
		pre.append(elt);
		break;
	    default:
		break;
	    }
	}
    },

    pbd : function(window,id) {
	var pre=$('.pre',window);

	pre.empty();

	var elt=$($.create("input",{type:"button",value:"plug","class":"vmop",ns:xhtmlns},[]));
	elt.click(function(id){return function() {
	    $xapi.xapi.PBD.plug(function() {}, $xapi.session,id)}}(id));
	pre.append(elt);

	var elt=$($.create("input",{type:"button",value:"unplug","class":"vmop",ns:xhtmlns},[]));
	elt.click(function(id){return function() {
	    $xapi.xapi.PBD.unplug(function() {}, $xapi.session,id)}}(id));
	pre.append(elt);

    },

    console : function(window, id, classes) {
	var content=$('.content',window);

	var head;

	var consolestuff;
	
	var params = ["param",{ns:xhtmlns, name:"ipaddress",value:"127.0.0.1"},[],
	    "param",{ns:xhtmlns, name:"code",value:((jQuery.browser.msie)?"com.citrix.xenserver.console.Initialize":"com.citrix.xenserver.console.Initialize.class")},[],
	    "param",{ns:xhtmlns, name:"archive",value:"XenServerConsole.jar"},[],
//	    "param",{ns:xhtmlns, name:"type",value:"application/x-java-applet;version=1.5"},[],
	    "param",{ns:xhtmlns, name:"scriptable",value:"false"},[],
	    "param",{ns:xhtmlns, name:"password",value:"none"},[],
	    "param",{ns:xhtmlns, name:"port",value:"5900"},[],
	    "param",{ns:xhtmlns, name:"url",value:classes.console[id].location},[],
	    "param",{ns:xhtmlns, name:"session",value:$xapi.session},[],
	    "param",{ns:xhtmlns, name:"useurl",value:"true"},[],
	    "param",{ns:xhtmlns, name:"backcolor",value:"ff:ff:ff"},[]];

	if(jQuery.browser.msie) {
	    consolestuff=["object",{classid:"clsid:8AD9C840-044E-11D1-B3E9-00805F499D93", width:"640",height:"500"},params];
	    alert("msie!");
	} else {
	    consolestuff=["object",{ns:xhtmlns, classid:"java:com.citrix.xenserver.console.Initialize.class", type:"application/x-java-applet", archive:"XenServerConsole.jar", width:"100%",height:"300px"},params];
	}
	   	
	content.append($($.create("div",{ns:xhtmlns,"class":"javacontainer"},consolestuff)));	
	
    },

    host : function(window,id,classes) {
	
	var pre=$('.pre',window);
	pre.empty();

	if($xapi.apiversion.minor>2) {
	    elt=$($.create("input",{type:"button",value:"getrrd","class":"vmop",ns:xhtmlns},[]));
	    elt.click(function() { $dw.testrrd(classes.host[id].uuid,["pif","cpu","memory","latency"]) });
	    pre.append(elt);
	}
    }
}

