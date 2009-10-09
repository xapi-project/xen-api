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

// NASTY NASTY Don't look too closely written in a hurry!!

function populateCacheFromFile(file) {
	if (typeof netscape != "undefined") { 
	    //netscape.security.PrivilegeManager.enablePrivilege("UniversalXPConnect UniversalBrowserRead"); 
	} 
	$.ajax({
	    type:"GET",
	    url:file,
	    dataType:"xml",
	    success:(function(data,status) {
		var i=0;
		$xapi.tables=[]
		$.each($('table',data),function (tablei,node) {
		    var tablename=node.attributes[0].nodeValue.toLowerCase();
		    $xapi.xo[tablename]={};
		    $.each($('row',node),function (rowi,rownode) {
			var row;
			
			$.each(rownode.attributes,function (attri,attr) {
			    if(attr.nodeName=="ref") {
				row=attr.nodeValue;
			    }
			});
			
			if(row) {
			    $xapi.xo[tablename][row]={};
			    $.each(rownode.attributes,function (attri,attr) {
				if(attr.nodeName!="ref" && attr.nodeName!="_ref") {
				    var fieldname=attr.nodeName.replace(/__/g,"_");
				    var fieldvalue=attr.nodeValue;

				    // try to guess what the type is:
				    if(fieldvalue=="true" || fieldvalue=="false")
					fieldvalue=(fieldvalue=="true");

				    // sets or maps
				    
				    if(fieldvalue.charAt && fieldvalue.charAt(0)=="(") {
					var myinner = fieldvalue.substring(1,fieldvalue.length-1);
					var myinnerarr = myinner.split(" ");
					if(myinner.charAt(0)=="(") {
					    // map
					    var myinnerarr2 = [[]];
					    var yy=0;
					    var zz=0;
					    for(var xx=0; xx<myinnerarr.length; xx++) {
						if(myinnerarr[xx].charAt(0)=="(") {
						    myinnerarr[xx]=myinnerarr[xx].substring(1,myinnerarr[xx].length);
						}
						if(myinnerarr[xx].charAt(myinnerarr[xx].length-1)==")") {
						    myinnerarr2[yy][zz]=myinnerarr[xx].substring(1,myinnerarr[xx].length-2);
						    if(myinnerarr2[yy][zz]=="true" || myinnerarr2[yy][zz]=="false") {
							myinnerarr2[yy][zz]=(myinnerarr2[yy][zz]=="true");
						    }
						    yy++;
						    myinnerarr2[yy]=[];
						    zz=0;
						} else {
						    myinnerarr2[yy][zz++]=myinnerarr[xx].substring(1,myinnerarr[xx].length-1);
						}
					    }
					    fieldvalue={};
					    for(yy=0; yy<myinnerarr2.length-1; yy++) {
						fieldvalue[myinnerarr2[yy][0]]=myinnerarr2[yy][1];
					    }
					} else {
					    var fieldvalue2=fieldvalue.substring(1,fieldvalue.length-1).split(" ");
					    if(!fieldvalue2) { fieldvalue2=[] }
					    fieldvalue=[];
					    for(var xx=0; xx<fieldvalue2.length-1; xx++) {
						fieldvalue[xx]=fieldvalue2[xx].substring(1,fieldvalue2[xx].length-1);
					    }
					}
				    }

				    $xapi.xo[tablename][row][fieldname]=fieldvalue;
				}
			    })
			}
		    });
		});

		for(var vbd in $xapi.xo.vbd) {
		    var myvbd=$xapi.xo.vbd[vbd];
		    var vm=myvbd.VM;
		    if(! $xapi.xo.vm[vm].VBDs) {
			$xapi.xo.vm[vm].VBDs=[];
		    }
		    $xapi.xo.vm[vm].VBDs = $xapi.xo.vm[vm].VBDs.concat(vbd);
		}

		for(var pbd in $xapi.xo.pbd) {
		    var mypbd=$xapi.xo.pbd[pbd];
		    var sr=mypbd.SR;
		    if(! $xapi.xo.sr[sr].PBDs) {
			$xapi.xo.sr[sr].PBDs=[];
		    }
		    $xapi.xo.sr[sr].PBDs = $xapi.xo.sr[sr].PBDs.concat(pbd);
		}
		
		$dw.populateXapiObjects();

		$xapi.moo=data;
	    })
	})

    // fill backreferences


}





$(document).ready(function() {
    $('#topbar').append(
	$($.create("label",{ns:xhtmlns,"for":"localdb"},
		   ["Local DB URI:","input",{ns:xhtmlns,"class":"credentials",id:"localdbfilename"},[]],
		   "button",{ns:xhtmlns,"class":"topbutton"},["Load"])).click(function() {populateCacheFromFile($('#localdbfilename').val())}))});
