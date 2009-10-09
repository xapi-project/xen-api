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

/* Construct the tree on the left */

var nbsp="\u00a0";


function belongs_on(vm,ignore_resident_on)
{
    /* A VM belongs on a host if it has a vdi in a non-shared sr or
       it's resident on the host */

    if($xapi.xo.host[$xapi.xo.vm[vm].resident_on] && !ignore_resident_on)
	return $xapi.xo.vm[vm].resident_on;

    /* Non-shared SRs */

    var nonsharedsrs=new Object;

    for(sr in $xapi.xo.sr) {
	if(!$xapi.xo.sr[sr].shared && $xapi.xo.sr[sr].PBDs) {
	    
	    var pbd=$xapi.xo.sr[sr].PBDs[0];
	    if(pbd)
		nonsharedsrs[sr]=$xapi.xo.pbd[pbd].host;
	}
    }

    /* For the VBDs, check if the VDIs are in the non-shared SRs */

    if($xapi.xo.vm[vm].VBDs) {
	for(var i=0; i<$xapi.xo.vm[vm].VBDs.length; i++) {
	    var vbd=$xapi.xo.vm[vm].VBDs[i];
	    if(!$xapi.xo.vbd[vbd].empty) {
		var sr=$xapi.xo.vdi[$xapi.xo.vbd[vbd].VDI].SR;
		if(nonsharedsrs[sr])
		    return nonsharedsrs[sr];
	    }
	}
    }

    return null;
}

function mkvms(vms,defaulttemplatesonly)
{
    var result=new Array;

    for(var i=0; i<vms.length; i++) {
	var img;
	var attr={ "class":"vm_tree", id:("vm_"+vms[i]), ns:xhtmlns };
	switch($xapi.xo.vm[vms[i]].power_state.toLowerCase()) {
	case "running":
	    img="images/tree_running_16.png";
	    attr["class"]=attr["class"]+" draggable";
	    break;
	case "suspended":
	    img="images/tree_suspended_16.png";
	    break;
	case "halted":
	    img="images/tree_stopped_16.png";
	    break;
	};

	if($xapi.xo.vm[vms[i]].is_a_template)
	    img="images/template_16.png";

	if((defaulttemplatesonly && $xapi.xo.vm[vms[i]].other_config.default_template)
	   || (!defaulttemplatesonly && !$xapi.xo.vm[vms[i]].other_config.default_template))
	    result.push("li",attr,["img",{src:img,ns:xhtmlns},[],"span",{ns:xhtmlns,id:"treeview_"+vms[i],"class":"clickable"},[nbsp+$xapi.xo.vm[vms[i]].name_label]]);
    }

    return result;
}



function mksrs(srs)
{
    var results=new Array;

    for(var i=0; i<srs.length; i++) {
	results.push("li",{ns:xhtmlns},["img",{ns:xhtmlns,src:"images/storage_16.png"},[],"span",{ns:xhtmlns,id:"treeview_"+srs[i],"class":"clickable"},[nbsp+$xapi.xo.sr[srs[i]].name_label]]);	
    }

    return results;

}

function mkhosts(vms,srs)
{
    var result=new Array;

    for(var host in vms) {
	var attr={"class":"droptarget",ns:xhtmlns,
		  id:"host_"+host}

	if(host != "none")
	    result.push("li",attr,["img",{ns:xhtmlns,src:"images/tree_running_16.png"},[],
				   "span",{ns:xhtmlns,id:"treeview_"+host,"class":"clickable"},[nbsp+$xapi.xo.host[host].name_label],
				   "ul",{ns:xhtmlns},mkvms(vms[host]).concat(mksrs(srs[host]))]);
    }

    result.push("li",{ns:xhtmlns,"class":"closed"},["img",{ns:xhtmlns,src:"images/template_16.png"},[],
			   nbsp+"Default templates",
			   "ul",{ns:xhtmlns},mkvms(vms["none"],true)]);

    var vms=mkvms(vms["none"]);
    for(var i=0; i<vms.length; i++)
	result.push(vms[i]);

    var srs=mksrs(srs["none"]);
    for(var i=0; i<srs.length; i++)
	result.push(srs[i]);

    return result;
}

function mkpool()
{

    /* Build up a list of affinities of VMs (via visibility of their SRs on hosts) */

    var vms=new Object;
    
    for(host in $xapi.xo.host) {
	vms[host]=new Array;
    }

    vms["none"]=new Array;

    for(vm in $xapi.xo.vm) {
	if(!$xapi.xo.vm[vm].is_control_domain) {
	    var host;
	    host=belongs_on(vm);
	    if(host)
		vms[host].push(vm);
	    else
		vms["none"].push(vm);
	}
    }

    /* Equivalently, build up a list of SRs */

    var srs=new Object;

    for(host in $xapi.xo.host) {
	srs[host]=new Array;
    }

    srs["none"]=new Array;

    for(sr in $xapi.xo.sr) {
	var host;
	var found=false;

	if(!$xapi.xo.sr[sr].shared) {
	    if($xapi.xo.sr[sr].PBDs) {
		for(var j=0; j<$xapi.xo.sr[sr].PBDs.length; j++) {
		    
		    var pbd=$xapi.xo.sr[sr].PBDs[j];
		    if(pbd) {
			host=$xapi.xo.pbd[pbd].host;
			srs[host].push(sr);
			found=true;
		    }	
		}
	    }
	}

	if(!found)
	    srs["none"].push(sr);
    }
	    
    /* Get the Pool object */
    var pool;
    var poolref;
    for(poolref in $xapi.xo.pool)
	pool=$xapi.xo.pool[poolref];

    return $.create("ul",{ns:xhtmlns},["li",{ns:xhtmlns},["img",{ns:xhtmlns,src:"images/poolconnected_16.png"},[],
				       "span",{ns:xhtmlns,"class":"clickable",id:"treeview_"+poolref},[nbsp+pool.name_label],
				       "ul",{ns:xhtmlns},mkhosts(vms,srs)]]);
}

function doclick(evt) {
    var id=$(evt.currentTarget).attr("id");
    var ref=id.slice(9+10);
    $dw.createObjView(ref);
}

function mktree(div)
{
    var popupmenutarget;

    var pool = $(mkpool()).treeview({persist:"cookie"}).find(".clickable").hover(function() {$(this).addClass("hoverbold")}, function(){$(this).removeClass("hoverbold")}).click(doclick).end();
    /*.find(".draggable").Draggable({ghosting:true, revert:true}).end();
    pool.find(".droptarget").Droppable({
	accept : "draggable",
	activeclass : "greenhighlight",
	hoverclass : "redhighlight",
	tolerance : "pointer",
	ondrop: function (drag) {
	    var host=this.id.substring(5);
	    var vm=drag.id.substring(3);
	    var task=xapi_check(xenapi.Async.VM.pool_migrate(session,vm,host,{}));
	    do_log(vm,"Migrating...",task);
	}
    });
    */

    /*
    $("li.vm_tree",pool).contextMenu('mytreemenu', {
	onShowMenu: function(e,menu) {
	    var vm=$(e.target).attr('id');
	    vm=vm.substring(3);
	    popupmenutarget=vm; // for bindings below
	    //console.log(vm);
       	    $(menu).addClass("dontshowme");
	    for(var i=0; i<$xapi.xo.vm[vm].allowed_operations.length; i++) {
		var op = $xapi.xo.vm[vm].allowed_operations[i];
		//console.log("op="+op);
		$("#"+op,menu).removeClass("dontshowme");
	    }
       	    $(".dontshowme",menu).remove();
	    
	    return menu;
	},
	bindings: {
	    'clone': function(t) {vm_clone(popupmenutarget);}
	    ,'start': function(t) {vm_start(popupmenutarget);}
	    ,'clean_shutdown': function(t) {vm_clean_shutdown(popupmenutarget);}
	    ,'clean_reboot': function(t) {vm_clean_reboot(popupmenutarget);}
	    ,'suspend': function(t) {vm_suspend(popupmenutarget);}
	    ,'resume':function(t) {vm_resume(popupmenutarget);}		
	}
	
    }).bind("click",{"foo":"bar"}, function(event) {
	$(event.currentTarget).addClass("selected");
    	var vm=this.id.substring(3); 
    	$vmd.populateVMPane(vm); });
    */
    div.empty().append(pool);   
}

function dotree() {
    var win=$dw.createWindow("treeview","Treeview",30,30);

    var content=$('.content',win);
    
    mktree(content);

    $xapi.registerEventListener("tree_adder",["vm"],"add",function() {mktree(content)});
    $xapi.registerEventListener("tree_deler",["vm"],"del",function() {mktree(content)});
    $xapi.registerEventListener("tree_moder",["vm"],"mod",function() {mktree(content)});
}


$(document).ready(function(){$('#topbar').append($($.create("button",{ns:xhtmlns,"class":"topbutton"},["tree"])).click(dotree))})
