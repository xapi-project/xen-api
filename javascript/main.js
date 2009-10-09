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

function setstatus(text,colour) {

	$('#status').text(text);

	switch(colour) {
	case 0: /* red */
		$('#status').removeClass("connected").removeClass("connecting").addClass("disconnected");
		break;
	case 1: /* yellow */
		$('#status').removeClass("connected").addClass("connecting").removeClass("disconnected");
		break;
	case 2: /* green */
		$('#status').addClass("connected").removeClass("connecting").removeClass("disconnected");
		break;
	}
}

function login() {
    $dw.closeall();
    //$xapi.master_address=$('#master_address').val();
    if($xapi.xapi_xmlrpc) delete $xapi.xapi_xmlrpc;
    if($xapi.xapi_json) delete $xapi.xapi_json;
    if($xapi.xapi) delete $xapi.xapi;
    delete graphs;
    graphs=new Object;
    $xapi.username=$('#cr_username').val();
    $xapi.password=$('#cr_password').val();
	setstatus("Detecting server version",1);

    $xapi.syncDetectServerVersion();
    var vsnstring;
    switch($xapi.mastersoftwareversion.product_version) {
    case "4.0.0":
	vsnstring="Rio";
	break;
    case "4.1.0":
	vsnstring="Miami";
	break;
    case "5.0.0":
	vsnstring="Orlando";
	break;
    case "5.1.0":
	vsnstring="Midnight Ride";
	break;
    }

    setstatus("Connecting to "+vsnstring+" server",1);

//    $xapi.detectServerVersion(function () { 
	$xapi.init( 
	    function() 
	    {
		$dw.populateXapiObjects()
		if($xapi.status==0) {
			setstatus("Failed to connect",0);
		} else {
			setstatus("Connected to "+vsnstring+" server",2);
		}
		if($xapi.status!=0) {
		    $.cookie('xapi_username',$('#cr_username').val());
		    $.cookie('xapi_password',$('#cr_password').val());
		    //		    $.cookie('xapi_host',$('#master_address').val());
		    $.cookie('xapi_autologin',"true");
		}
	    })
//    });
}

$(document).ready(function(){
    $('#devwebmain').jqDrag('.jqDrag').jqResize('.jqResize');
    
    if($.cookie('xapi_username')) 
	$('#cr_username').val($.cookie('xapi_username'));
    if($.cookie('xapi_password'))
	$('#cr_password').val($.cookie('xapi_password'));
    //if($.cookie('xapi_host'))
    //$('#master_address').val($.cookie('xapi_host'));

    $dw=new $devweb();

    if($.cookie('xapi_autologin'))
	login();

    $('#connectbutton').click(login);

    $('g > g').click(function(evt) {
	var tgt=$($(evt.target).parents('g')[0]);
	var classname=$('title',tgt).text().toLowerCase();
	if(classname in $xapi.xo)
	    $dw.createClassView(classname);
    });
});    
