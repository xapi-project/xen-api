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

/* Developers web stuff */

var xhtmlns="http://www.w3.org/1999/xhtml";

// populate the Xapi Objects window

function $devweb() {
    this.windows=[];
    this.createdClasses=[];
    this.watchedObjects={};
    this.editables=new $editables();
    this.overrides=new $overrides();

    this.devwebmain=null;

    this.maxzindex=4;

}

$devweb.prototype = {
    populateXapiObjects : function () {
	var oldthis=this;

	if(!this.devwebmain)
	    this.devwebmain=this.createWindow("devwebmain","Xapi Objects",400,30);
	
	$('.content',this.devwebmain).empty().append($.create("table",{ns:xhtmlns},["tbody",{ns:xhtmlns,"id":"devwebmainbody"},[]]));
	var tbody=$('#devwebmainbody',this.devwebmain);
	for(o in $xapi.xo) {
	    var inner=$($.create("tr",{ns:xhtmlns,"class":"xapiobjectstr"},["td",{id:o,ns:xhtmlns,"class":"xapiobjectstd"},[o]]));
	    tbody.append(inner);
	}
	tbody.children().click(function() { oldthis.createClassView(this.firstChild["id"],this.firstChild["id"])})
	.hover(function() {$(this).addClass("hoverbold")}, function(){$(this).removeClass("hoverbold");});

	var tableheight=tbody.height();
	this.changeWindowDimensions(this.devwebmain,204,tableheight+37);

//	dotree();
    },

    setZIndices : function(elt) {
	var target=elt.target;
	var jqstuff=$(target).parent().parent().parent();
	var current=jqstuff.css("z-index");
	jqstuff.css("z-index",this.maxzindex++);
    },

    testme : function(target,method,newwidth,newheight) {
	if(method=='r') {
	    var window=$(target);
	    if(window) {
		$dw.changeWindowDimensions(window,null,newheight);
	    }
	}
    },
    
    changeWindowDimensions : function(window,width,height) {
	height && window.height(height);
	width && window.width(width);
	var content=$('.content',window);
	var pre=$(".pre",window);
	if(height) {
	    content.height(height-33-pre.height());
	}
    },

    createWindow : function(id,title,x,y) {
	var oldthis=this;
	var m={ns:xhtmlns};
	var window=$($.create(
	    "div",{id:id,"class":"window jqDnR",ns:xhtmlns},[
		"div",{"class":"windowinner",ns:xhtmlns},[
		    "div",{"class":"jqDrag windowtitlebar",ns:xhtmlns},[
			"div",{ns:xhtmlns,"class":"windowtitletext"},[title],
			"div",{ns:xhtmlns,"class":"windowtitlebuttons"},[
			    "div",{ns:xhtmlns,"class":"winclose windowclosebutton"},[" "]]],
		    "div",{ns:xhtmlns,"class":"windowcontent"},[
			"div",{ns:xhtmlns,"class":"pre"},[],
			"div",{ns:xhtmlns,"class":"content"},[],
			"div",{ns:xhtmlns,"class":"post"},[]],
		    "div",{ns:xhtmlns,"class":"windowstatusbar"},[
			"div",{ns:xhtmlns,"class":"jqResize windowresize"},[" "],
			"div",{ns:xhtmlns,"class":"windowstatusbartext"},[]
		    ]
		]
	    ]));
	window.css("z-index",this.maxzindex++);
	$('.jqDrag',window).click(function(e) { oldthis.setZIndices(e) });
	var left;
	var top;
	if(x)
	    left=x+"px";
	else
	    left=parseInt(Math.random() * 500.0)+"px"

	if(y)
	    top=y+"px";
	else
	    top=parseInt(Math.random() * 500.0)+"px"

	window.jqDrag('.jqDrag').jqResize('.jqResize',this.testme).css("left",left).css("top",top);
	$('.winclose',window).click(function(e) { oldthis.closeWin($(this).parent().parent().parent().parent()) });
	$('body').append(window);
	return window;
    },

    updateClassView : function (win,objs) {
	var c=win.attr("id");
	var content=$('.content',win);	
	var elts = new Array;
	var showtemplates=false;
	var dw=this;

	var myobjs={};
	myobjs[c]=new Object;

	for(obj in $xapi.xo[c])
	    myobjs[c][obj]=$xapi.xo[c][obj];

	if(objs)
	    for(obj in objs[c])
		myobjs[c][obj]=objs[c][obj];

	if(c=="vm") {
	    if($('.showtemplates',win).length==1) {
		showtemplates=$('.showtemplates',win)[0].checked;
	    } else {
		$('.pre',win).append($.create("label",{ns:xhtmlns,"class":"prelabel"},["Show templates","input",{ns:xhtmlns,type:"checkbox","class":"showtemplates"},[]]));
		$('.showtemplates',win).change(function() { dw.updateClassView(win) });
	    }
	}

	for(i in myobjs[c]) {
	    var text=[];
	    if(c!="vm" || ((! myobjs[c][i].is_a_template) || showtemplates)) {
		if(myobjs[c][i].name_label) {
		    text=[i,"br",{ns:xhtmlns},[],"("+myobjs[c][i].name_label+")"];
		} else if(myobjs[c][i].uuid) {
		    text=[i,"br",{ns:xhtmlns},[],"("+myobjs[c][i].uuid+")"];
		} else {
		    text=[i];
		}

		if(myobjs[c][i].VM && $xapi.xo.vm[myobjs[c][i].VM]) {
		    text=text.concat(["br",{ns:xhtmlns},[],"vm: ("+$xapi.xo.vm[myobjs[c][i].VM].name_label+")"]);
		} 

		if(myobjs[c][i].__deleted__)
		    text=["span",{ns:xhtmlns,"class":"deleted"},text];
			    
		elts=elts.concat(["p",{ns:xhtmlns,"id":i.slice(10), "class":"classviewp"},text]);
	    }
	}
	
	var oldthis=this;

	var elts2 = $($.create("div",{ns:xhtmlns},elts)).children().click(function (e) {oldthis.createObjView(this.id);}).hover(function() {$(this).addClass("hoverbold")}, function(){$(this).removeClass("hoverbold");}).end();

	content.empty().append(elts2);
    },

    createClassView : function (c) {
	if(this.createdClasses[c])
	    return;

	var win=this.createWindow(c,c);
	this.windows.push(win);

	win.width("400px");
	var oldthis=this;
	$xapi.registerEventListener(c+"_list_adder",[c],"add",function(classes) {oldthis.updateClassView(win,classes);});
	$xapi.registerEventListener(c+"_list_deler",[c],"del",function(classes) {oldthis.updateClassView(win,classes);});
	$xapi.registerEventListener(c+"_list_moder",[c],"mod",function(classes) {oldthis.updateClassView(win,classes);});
	
	this.updateClassView(win);	
	this.changeWindowDimensions(win);

    },

    findClassFromRef : function(ref) {
	var c="Unknown";

	for(i in $xapi.xo) {
	    if(ref in $xapi.xo[i])
		c=i;
	}

	return c;	
    },

    updateObjectViews : function(c,classes,force) {
	for(var i in classes[c]) {
	    var win=$('#_'+i.slice(10));
	    var content=$('.content',win);
	    var fields=[];
	    var data=[];
	    var m=0;

	    if(content.length>0) {
		for(var j in $xapi.xo[c][i]) {
		    var diff=false;
		    if(classes[c][i][j].sort) {
			classes[c][i][j].sort();
			$xapi.xo[c][i][j].sort();
			for(var z=0; z<classes[c][i][j].length; z++)
			{
			    if(classes[c][i][j][z]!=$xapi.xo[c][i][j][z])
				diff=true;
			}
		    } else if(typeof(classes[c][i][j])=="object") {
			for(z in classes[c][i][j])
			    if(!$xapi.xo[c][i][j][z] || (classes[c][i][j][z]!=$xapi.xo[c][i][j][z]))
				diff=true;

			for(z in $xapi.xo[c][i][j])
			    if(!classes[c][i][j][z] || (classes[c][i][j][z]!=$xapi.xo[c][i][j][z]))
				diff=true;
		    } else if(classes[c][i][j] != $xapi.xo[c][i][j])
			diff=true;


		    if(diff || force) {
			var text=[];
			var val=classes[c][i][j];
			if(val.sort) { // is array! 
			    for(var k=0; k<val.length; k++) {
				var t=val[k].toString();
				if(t.slice(0,10)=="OpaqueRef:" && t!="OpaqueRef:NULL") {
				    text=text.concat(["span",{ns:xhtmlns,"class":"reflink"},[val[k].toString()]]);			    
				} else {
				    text=text.concat([val[k].toString()]);			    
				}
				text=text.concat(["br",{ns:xhtmlns},[]]);
			    }
			} else if(typeof(val)=="object") {
			    for(var k in val) {
				var v=val[k].toString();
				if(v.slice(0,10)=="OpaqueRef:") {
				    text=text.concat(["span",{ns:xhtmlns,"class":"reflink"},[k+": "+val[k].toString()]]);			    
				} else {
				    text=text.concat([k+": "+val[k].toString()]);			    
				}
				text=text.concat(["br",{ns:xhtmlns},[]]);
			    }
			} else {
			    var t=val.toString();
			    if(t.slice(0,10)=="OpaqueRef:") {
				text=text.concat(["span",{ns:xhtmlns,"class":"reflink"},[t]]);			    
			    } else {
				text=text.concat([t]);
			    }
			    
			}
			
			var rowlabel=[j]
			if(this.editables[c] && this.editables[c][j]) {
			    rowlabel=["span",{ns:xhtmlns,"class":"editable"},[j]];
			    text=["div",{ns:xhtmlns,"class":"editable_"+j},text];
			}
			fields[m]=rowlabel;
			data[m]=text;
		    }
		    m++;
		}
		    
		if(content.children().length==0) {
		    var rows=[];
		    
		    for(var xx=0; xx<fields.length; xx++) {
			rows=rows.concat(["tr",{ns:xhtmlns},
					  ["td",{ns:xhtmlns},fields[xx],
					   "td",{ns:xhtmlns},["span",{ns:xhtmlns},data[xx]]]]);
		    }
		    
		    var stuff=$($.create("table",{ns:xhtmlns, "class":"mytable"},
			["tbody",{ns:xhtmlns},
			["tr",{ns:xhtmlns},
			["th",{ns:xhtmlns},["Name"],
			"th",{ns:xhtmlns},["Value"]]].concat(rows)]))
		    
		    $("tr:odd",stuff).addClass("oddrow");
		    
		    content.empty().append(stuff);
		} else {
		    $('td:last-child',content).each(function(xx,e) {
			if(data[xx]) {
			    $(e).empty().append($($.create("span",{ns:xhtmlns},data[xx])))
			    $(e).parent().css('background-color','#ff0000').animate({backgroundColor:(((xx% 2)==1)?"#eeeeff":"#ddddff")},1500);
			}
		    });
		}
	    
		
		var oldthis=this;

		content.find(".reflink").click(function() {oldthis.createObjView($(this).text().slice(10))})
		.hover(function() {$(this).addClass("hoverbold")}, function(){$(this).removeClass("hoverbold");})
		.end();
		
		
		for(var row in this.editables[c]) {
		    var editables=$(".editable_"+row,content);
		    var me=this;
		    var tmp = function() {
			var myrow=row;
			var myclass=c;
			editables.editable(function(a,b) {
				if (typeof netscape != "undefined") { //netscape.security.PrivilegeManager.enablePrivilege("UniversalXPConnect UniversalBrowserRead");
				}
			    me.editables[myclass][myrow](i,a);});
		    };
		    tmp();
		}

		if(this.overrides[c]) {
		    var override = this.overrides[c];
		    override(win,i,classes);
		}

	    }
   
	}
    },
    
    closeWin : function(elt) {
	// determine type of window
	var winid=elt.attr("id");
	var ref=winid.slice(1);
	var c=this.findClassFromRef("OpaqueRef:"+ref);

	if(winid in $xapi.xo) {
// It was a class window
	    elt.remove();
	    delete this.createdClasses[winid];
	    $xapi.unregisterEventListener(winid+"_list_adder");
	    $xapi.unregisterEventListener(winid+"_list_deler");
	} else {
	    // Hack hack hack - the jquery seems to have some problems with removing java
	    if($('.javacontainer').length>0) {
		var div=$('.javacontainer',elt);
		var parent=div.parent();
		parent[0].removeChild(div[0]);
	    }
	    // Make some attempt here to remove event listeners?
	    elt.remove();
	}
    },

    createObjView : function(myid) {
	var c=this.findClassFromRef("OpaqueRef:"+myid);
	
	var id="_"+myid;

	if($('#'+id).length > 0) {
	    $('#'+id).css("z-index",this.maxzindex++);
	    return;
	}
	
	var win=this.createWindow(id,c+" : "+myid).addClass(c);
	this.windows.push(win);

	win.width("500px");

	var oldthis=this;
	if(!this.watchedObjects[myid])
	    $xapi.registerEventListener(c+"_obj_updater",[c],"mod",function(classes) {oldthis.updateObjectViews(c,classes)});
	this.watchedObjects[myid]=true;

	var obj={};
	obj[c]={};
	obj[c]["OpaqueRef:"+myid]=$xapi.xo[c]["OpaqueRef:"+myid];
	
	this.updateObjectViews(c,obj,true);

	if(c!="console") {
	    var table=$('.content',win).children();
	    var ovwidth=table.width();
	    var ovheight=table.height();
	    
	    if(ovwidth>498) ovwidth=498;
	    if(ovheight>498) ovheight=498;
	    this.changeWindowDimensions(win,ovwidth+2,ovheight+40);
	}

    },

    testrrd : function(mainfilter,groupings) {
	var re=new RegExp(mainfilter);
	var stats=new Array();

	for(var set in sets) {
	    if(re.test(set)) {
		stats.push(set);
	    }
	}

	if(stats.length==0)
	    return;

	var win=this.createWindow("graph:"+mainfilter,"graph:"+mainfilter);
	var content=$('.content',win);
	win.width("690px");
	
	var list=$($.create('ul',{ns:xhtmlns},[]));

	list.appendTo(content.empty());	

	
	//this.changeWindowDimensions(win,null,50+groupings.length*250);

	for(var j=0; j<groupings.length; j++)
	{
	    var group=groupings[j];
	    var grouped_stats=new Array();
	    var group_regex=new RegExp(group);

	    for(var i=0; i<stats.length; i++) {
		if(group_regex.test(stats[i])) grouped_stats.push(stats[i]);
	    }

	    if(grouped_stats.length>0) {
		var svg=$($.create("svg",{ns:SVG_NS,width:650,height:210},[]));
		var li=$($.create("li",{ns:xhtmlns,"class":"rrdgroup"},["a",{ns:xhtmlns,href:'#'},[group]]));
		var container=$($.create("div",{ns:xhtmlns},[]));

		svg.appendTo(container);
		container.appendTo(li);
		li.appendTo(list);		
		
		var graph_group=creategraph(svg,"graph_"+mainfilter+"_"+group,640,200);		

		for(i=0; i<grouped_stats.length; i++) 
		    graph_group.bindxy('t',grouped_stats[i]);

		graph_group.max_x=0;
		graph_group.min_x=-10;
		graph_group.setup_graph(false,true);
		graph_group.plotdatapoints();

		$('a',li).click(function(svg) { return(function() {svg.toggle()})}(svg));

	    }
	}

	this.windows.push(win);
    },

    closeall : function() {
	for(win in this.windows) {
	    try {
		this.closeWin(this.windows[win]);
	    } catch(e) {
	    }
	}
    }
}
