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

/* Api call window */

$(document).ready(function() {$('#topbar').append($($.create("button",{ns:xhtmlns,id:"apibutton","class":"topbutton"},["API call"])).click(test));});

var apiwin;
var mycallparams={};

function create_ty(number,param,callparams) {
    var callback=(function (number) { return (function(evt) {callparams[number]=$(evt.target).val()})})(number);
    var cb2=(function (number) { return (function(evt,data,formatted){callparams[number]=data[0].split(" ")[0];});})(number);
    var cb3=(function (number) { return (function(evt){callparams[number]=evt.target.checked})})(number);
    var common=["label",{ns:xhtmlns,"class":"apicalllab"},[param.name]];
    var i;

    switch(param.ty.ty) {
    case "string":
    case "int":
    case "float":
	return $($.create("input",{ns:xhtmlns,"class":"apicallinput"},[])).keyup(callback).change(callback);
    case "bool":
	callparams[number]=false;
	var ret=$($.create("input",{ns:xhtmlns,type:"checkbox","class":"apicallcheck"},[])).autocomplete(tmp,{matchContains:true}).result(cb3);
	return ret;
    case "datetime":
	callback=(function (number) { return (function(evt) {callparams[number]=new Date(Date.parse($(evt.target).val()))})})(number);
	return $($.create("input",{ns:xhtmlns,"class":"apicallinput"},[])).keyup(callback).change(callback);
    case "enum":
	var tmp=param.ty.values;
	callparams[number]=tmp[0];
	var ret=$($.create("input",{ns:xhtmlns,value:tmp[0],"class":"apicallinput"},[])).autocomplete(tmp,{matchContains:true}).result(cb2);
	return ret;
    case "set":
	return $('');
    case "map":
	return $('');
    case "record":
	return $('');
    case "ref":
	var tmp=[];
	var val="";
	var cls=param.ty.class.toLowerCase();
	if(cls=="session") {
	    tmp[0]=$xapi.session;
	    val=$xapi.session;
	    callparams[number]=val;
	} else {
	    var objs=$xapi.xo[cls];
	    var j=0;
	    for(i in objs) {
		var name = i;	    
		if(objs[i].name_label)
		    name=name+" ("+objs[i].name_label+")";
		if(objs[i].uuid)
		    name=name+" ("+objs[i].uuid+")";
		tmp[j++]=name;
	    }
	}
	var ret=$($.create("input",{ns:xhtmlns,value:val,"class":"apicallinput"},[])).autocomplete(tmp,{matchContains:true}).result(cb2);
	return ret;
    }
}

function api_result(event,data,formatted) {
    $('#msgresdiv').hide().empty();
    var params=messages[data].params;
    $('#msgdoc').empty().append($($.create("p",{ns:xhtmlns},[messages[data].doc])));
    mycallparams=[];
    var form=$('#msgargsform').empty();
    for(i=0; i<params.length; i++) {
	var arg=create_ty(i+1,params[i],mycallparams).focus(
	    (function (paramnum) {return (
		function (evt) {
		    $('#msgargname').empty().text(params[paramnum].name)
		    $('#msgargdoc').empty().text(params[paramnum].doc)
		}
	    )})(i));
	if(i==0)
	    arg.focus();
	var labinner=$($.create("span",{ns:xhtmlns,"class":"apicallspan"},[params[i].name]))
        var label=$($.create("label",{ns:xhtmlns,"class":"apicalllab"},[])).append(labinner).append(arg);
	form.append(label);
	form.append($($.create("br",{ns:xhtmlns},[])));

    }
    var button=$('button',form.parent());
    if(button.length==0) {
	button=$($.create("button",{ns:xhtmlns},["apply"]));
	button.appendTo(form.parent());
    }
    button.unbind().click(function() {
	if (typeof netscape != "undefined") { 
	    //netscape.security.PrivilegeManager.enablePrivilege("UniversalXPConnect UniversalBrowserRead"); 
	}
	var curobj=$xapi.xapi;
	var hierarchy=data[0].split(".");
	for(i=0; i<hierarchy.length; i++)
	    curobj=curobj[hierarchy[i]];
	mycallparams[0]=function(res) {
	    var contents;
	    var n={ns:xhtmlns};
	    contents=["tr",n,["td",n,["Status"],"td",n,[res.result.status]]];
	    if(res.result.Status=="Failure") {
		contents=contents.concat(["tr",n,["td",n,["Error"],"td",n,[res.result.ErrorDescription[0]]]]);
		for(var i=1; i<res.result.ErrorDescription.length; i++) {
		    contents=contents.concat(["tr",n,["td",n,["Error parameter "+i],"td",n,[res.result.ErrorDescription[i]]]]);
		}
		$('#msgresdiv').append($($.create("h3",n,["Result"],"table",n,contents))).show();
	    } else if(res.result.Status=="Success") {
		$('#msgresdiv').append($($.create("h3",n,["Result"],"table",n,contents,"textarea",{ns:xhtmlns,style:"width:500px"},[res.result.Value]))).show();
	    }
	}
	curobj.apply($xapi,mycallparams);
    });
}


function create_api_choice() {
    var choices=[];
    var i=0;
    var message;
    for(message in messages) {
	choices[i++]=message;
    }
    var ret=$($.create("label",{ns:xhtmlns,style:"text-weight:bold"},["API Call:"],"input",{ns:xhtmlns,id:"apicallinput"},[]));
    $(ret[1]).autocomplete(choices,{matchContains:true}).result(api_result);
    return ret;
}

function test() {
    var win=$dw.createWindow("mytest","mytest");
    $dw.changeWindowDimensions(win,600);
    var content=$('.content',win).css("padding","5px");
    var form=content.append($($.create(
	"form",{ns:xhtmlns,id:"msgnameform"},[],
	"hr",{ns:xhtmlns},[],
	"div",{ns:xhtmlns},[
	    "h3",{ns:xhtmlns},["Call documentation"],
	    "div",{ns:xhtmlns,id:"msgdoc"},[]],
	"hr",{ns:xhtmlns},[],
	"div",{ns:xhtmlns},[
	    "h3",{ns:xhtmlns},["Parameters"],
	    "form",{ns:xhtmlns,id:"msgargsform"},[],
	    "h3",{ns:xhtmlns,id:"msgargname"},[],
	    "p",{ns:xhtmlns,id:"msgargdoc"},[],
	    "br",{ns:xhtmlns,style:"clear:both"},[]	    
	],
	"hr",{ns:xhtmlns},[],
	"div",{ns:xhtmlns,id:"msgresdiv",style:"display:none"},[
	    "h3",{ns:xhtmlns},["Result"]
	]
    )));
    var api=create_api_choice();
    $('#msgnameform',form).append(api);
    apiwin=win;
}

