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

var ring = function(size) {
    this.size=size;
    this.data=new Array(size);
    this.current=size-1;
}

ring.prototype = {
    push:function(value) {
	this.current = this.current + 1
	if(this.current==this.size)
	    this.current=0;
	this.data[this.current]=value;
    },

    peek:function(i) {
	if(i>=this.size)
	    return 0.0;

	var index = this.current-i;
	if(index<0)
	    index=index+this.size;

	return this.data[index];	
    },
    
    exprt:function(arr) {
	for(var i=0; i<arr.length; i++) {
	    arr[i]=this.peek(i);
	}
    }
}

var rrd = function(names,values) {
    this.rings=new Object;
    for(var i=0; i<names.length; i++) {
	this.rings[names[i]]=new ring(100);
    }
    this.update(names,values);
}

rrd.prototype = {
    update:function(names,values) {
	for(var i=0; i<names.length; i++) {
	    for(var j=values[i].length-1; j>=0; j--) {
		if(!this.rings[names[i]]) {
		    this.rings[names[i]]=new ring(100);
		}
		this.rings[names[i]].push(values[i][j])
	    }
	}
    },
    exprt:function(name,arr) {
	if(this.rings[name])
	    this.rings[name].exprt(arr);
    }
}




function processrrd(host,exp) {
    var rows=exp.meta.rows;
    var columns=exp.meta.columns;

    if(rows==0)
	return $xapi.hoststats[host].rings["t"].peek(0); // last update time
    
    var data=new Array(columns+1);
    
    var testre=/([^\:]*)\:([^\:]*):([^\:]*):(.*)/;

    for(var i=0; i<columns+1; i++) {
	data[i]=new Array(rows);
	
	for(var j=0; j<rows; j++) {
	    if(i==0)
		data[i][j]=exp.data[j].t;
	    else
		data[i][j]=exp.data[j].values[i-1];
	}
    }

    var myrrd=$xapi.hoststats[host];
    if(!myrrd) {
	myrrd=new rrd(["t"].concat(exp.meta.legend),data);
	$xapi.hoststats[host]=myrrd;
    } else {
	myrrd.update(["t"].concat(exp.meta.legend),data);
    }
    
    function setit(name) {
	var set = sets[name];
	if(!set) {
	    var shortname=name;
 	    var shortername=name;
	    var myarr=testre.exec(name);
	    if(myarr) {
		shortname=myarr[3] + ":" + myarr[4];
		shortername=myarr[4];
	    }
	    set=new dset(new Array(100),shortname,shortername,0,0);
	    sets[name]=set;
	}
	myrrd.exprt(name,set.data);
	var bounds=getminmax(set.data);
	set.max=bounds.max;
	set.min=bounds.min;
    }

    var allsets = ["t"].concat(exp.meta.legend);

    for(var i=0; i<columns+1; i++) {
	setit(allsets[i],data[i]);
    }
    
    var currenttime=myrrd.rings["t"].peek(0);

    var tset=sets["t"];
    for(i=0; i<100; i++)
    {
	tset.data[i]-=currenttime
	tset.data[i]/=60;
    }
    var bounds=getminmax(tset.data);
    tset.max=bounds.max;
    tset.min=bounds.min;

    for(var graph in graphs) {
	var gsets = graphs[graph].setsy;
	var rescale = false;
	for(var i=0; i<gsets.length; i++) {
	    if(sets[gsets[i]].max > graphs[graph].max_y)
		rescale=true;
	}
	if(rescale)
	    graphs[graph].setup_graph(false,true);

	graphs[graph].plotdatapoints();
    }
    
    return myrrd.rings["t"].peek(0); // last update time
}