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

/* Base module, keeps track of xapi's state */

/* status: 0==disconnected
           1==connected
*/

var Xenapi = function() {
    this.xo = {};
    this.xo_deleted = {};
    this.listeners = {};
    this.session = {};
    this.use_json = true;
    this.cache_objects = true;
    this.get_rrds = false; /* will only work with cache_objects */
    this.username="root";
    this.password="";
    this.status=0; 
    this.hoststats={};
    this.lastupdatetime={};
    this.master_address="";
};

Xenapi.prototype = {
    registerEventListener : function(name,classes,type,cb) {
	this.listeners[name]={classes:classes, ty:type, callback:cb};
    },

    unregisterEventListener : function(name) {
	delete this.listeners[name];
    },

    eventCallback : function(result) {

	/* Nb, I think the callbacks are the wrong way around. When mod or
           add events happen, the cache is not updated until after the callbacks,
           and the new objects are passed in the argument to the callback. This
           is backwards - the cache should be updated, and the _old_ objects 
           passed as the argument (for diffing purposes). */

	var res=result;
	var list;
	var modifiedclasses={};
	var addedclasses={};
	var delclasses={};
	var c, ref, i, cb;

	for(i=0; i<res.length; i++) {
	    c = res[i]['class'];
	    ref = res[i].ref;

	    switch(res[i].operation) {
	    case "mod": 
		if(!modifiedclasses[c]) { modifiedclasses[c]={}; }
		modifiedclasses[c][ref]=res[i].snapshot; 
		break;
	    case "add": 
		if(!addedclasses[c]) { addedclasses[c]={}; }
		addedclasses[c][ref]=res[i].snapshot;
		break;
	    case "del": 
		//delete this.xo[c][ref];
		this.xo[c][ref]=res[i].snapshot;
		this.xo[c][ref].__deleted__=true;

		if(modifiedclasses[c] && modifiedclasses[c][ref]) {
		    delete modifiedclasses[c][ref];
		}
		if(addedclasses[c] && addedclasses[c][ref]) {
		    delete addedclasses[c][ref];
		}
		break;
	    default:
		break;
	    }
	}
   
	var callbacks={};
	
	for(cb in this.listeners) {
	    if(this.listeners.hasOwnProperty(cb)) {
		var call=false;
		var classes;
		switch(this.listeners[cb].ty) {
		case "mod":
		    classes=modifiedclasses;
		    break;
		case "add":
		    classes=addedclasses;
		    break;
		case "del":
		    classes=delclasses;
		    break;
		default:
		    classes=modifiedclasses;
		}
		
		for(c in classes) {
		    if(classes.hasOwnProperty(c)) {
			for(i=0; i<this.listeners[cb].classes.length; i++) {
			    if(this.listeners[cb].classes[i]==c) {
				call=true;
			    }
			}
		    }
		}
		
		if(call) {
		    try {
			this.listeners[cb].callback(classes);
		    } catch(e) {
		    }
		}
	    }
	}

	for(c in modifiedclasses) {
	    if(modifiedclasses.hasOwnProperty(c)) {
		if(!this.xo[c]) {
		    this.xo[c]={};
		}

		for(ref in modifiedclasses[c]) {
		    if(modifiedclasses[c].hasOwnProperty(ref)) {
			this.xo[c][ref]=modifiedclasses[c][ref];
		    }
		}
	    }
	}

	for(c in addedclasses) {
	    if(addedclasses.hasOwnProperty(c)) {
		if(!this.xo[c]) {
		    this.xo[c]={};
		}
		for(ref in addedclasses[c]) {
		    if(addedclasses[c].hasOwnProperty(ref)) {
			this.xo[c][ref]=addedclasses[c][ref];
		    }
		}
	    }
	}
	
	var parent=this;
	parent.xapi.event.next(function(result) {parent.eventCallback(parent.check(result));},parent.session);
    },

    callEventListeners : function(classchanged) {
	var objects = {};
	objects[classchanged]={};
	
	for(var c in this.xo[classchanged]) {
	    if(this.xo[classchanged].hasOwnProperty(c)) {
		objects[classchanged][c]=true;
	    }
	}
	
	for(var cb in this.listeners) { 
	    if(this.listeners.hasOwnProperty(cb)) {
		for(var i=0; i<this.listeners[cb].classes.length; i++) {
		    if(this.listeners[cb].classes[i]==classchanged) {
			this.listeners[cb].callback(objects);
		    }
		}
	    }
	}
    },

    metricsTick : function () {
	var parent=this;
	
	if(this.apiversion.minor < 3) {
	    return;
	}

	var error = function(xhr,text,error) { if(text) {alert("text: "+text);} if(error) {alert("error: "+error);} };
	var successfn = function(host) {return function(data) {var t=processrrd(host,eval("("+data+")")); parent.lastupdatetime[host]=t;};};
	
	for(var host in this.xo.host) {
	    if(this.xo.host.hasOwnProperty(host)) {
		var t=this.lastupdatetime[host];
		
		if(!t) {
		    var d=new Date();
		    t=(d.getTime()/1000)-500;
		} else {
		    t=t+1;
		}
		
		if (typeof netscape != "undefined") { 
		    //netscape.security.PrivilegeManager.enablePrivilege("UniversalXPConnect UniversalBrowserRead"); 
		} 
		
		var url = "http://"+this.xo.host[host].address+"/rrd_updates";
		var success = successfn(host);
		
		$.ajax({
			type: "GET",
			    url: url,
			    error: error,
			    success: success,
			    data: "start="+parseInt(t,10)+"&cf=AVERAGE&json=true&interval=1&host=true&session_id="+this.session
			    });
	    }
	}
    },

    check : function(result) {
	result=result.result;
            if(result.Status=="Failure") {
                var message=result.ErrorDescription[0];
                for(var i=1; i<result.ErrorDescription.length; i++) {
                    message+=","+result.ErrorDescription[i];
		}
                alert("Request failed: Error='"+message+"'");
                throw new Error("failed!");
            }
	try {
            result = eval("("+result.Value+")");
            return result;
	} catch(e) {
	    // Silently ignore errors!
	}
    },

    detectServerVersion : function(next) {
	var tmprpc;
	var s;
	var h;
	var p;
	var major, minor;
	var x=this;

	function check5(result) { minor=x.check(result); x.version={major:major, minor:minor}; tmprpc.session.logout(next,s); }
	function check4(result) { major=x.check(result); tmprpc.host.get_API_version_minor(check5, s, h); }
	function check3(result) { 
	    p=x.check(result); 
	    var ref;
	    for(r in p) {
		if(p.hasOwnProperty(r)) {
		    ref=r;
		}
	    }
	    h=p[ref].master;
	    tmprpc.host.get_API_version_major(check4, s, h); }
	function check2(result) { s=x.check(result); tmprpc.pool.get_all_records(check3, s);}
	function check1() { tmprpc.session.login_with_password(check2, x.username, x.password); }

	tmprpc= new $.rpc(
	    "/json",
	    "xml", 
	    check1,
	    null,
	    ["session.login_with_password","pool.get_all_records","host.get_API_version_major","host.get_API_version_minor","session.logout"]
	); 	
    },

    syncDetectServerVersion : function() {
	var tmprpc,session,poolrefrec,poolref,poolrec,host,majorver,minorver;

	if (typeof netscape != "undefined") { 
	    //netscape.security.PrivilegeManager.enablePrivilege("UniversalXPConnect UniversalBrowserRead"); 
	} 

	tmprpc = new $.rpc(
	    "/json",
	    "xml", 
	    null,
	    null,
	    ["session.login_with_password","pool.get_all_records","host.get_API_version_major","host.get_API_version_minor","session.logout", "host.get_software_version"]);
	session = this.check(tmprpc.session.login_with_password(this.username, this.password));
	poolrefrec = this.check(tmprpc.pool.get_all_records(session));
	for(r in poolrefrec) {
	    if(poolrefrec.hasOwnProperty(r)) {
		poolref=r;
		poolrec=poolrefrec[r];
	    }
	}
	host = poolrec.master;
	majorver = parseInt(this.check(tmprpc.host.get_API_version_major(session,host)));
	minorver = parseInt(this.check(tmprpc.host.get_API_version_minor(session,host)));
	this.mastersoftwareversion = this.check(tmprpc.host.get_software_version(session,host));
	this.apiversion = {major:majorver, minor:minorver};
	this.check(tmprpc.session.logout(session));
	return this.apiversion;
    },

    init : function(finishedfn) {
	if (typeof netscape != "undefined") { 
	    //netscape.security.PrivilegeManager.enablePrivilege("UniversalXPConnect UniversalBrowserRead"); 
	} 

	if(this.myticker) {
	    clearInterval(this.myticker);
	}

	this.status=0;
	this.session="OpaqueRef:NULL";
	for(obj in this.xo) {
	    if(this.xo.hasOwnProperty(obj)) {
		try {
		    delete this.xo[obj];
		} catch (e) {}
	    }
	}

	var x=this;

	function check_all_done() {
	    for(var cc in x.xapi) {
		if(x.xo[cc.toLowerCase()]==-1) {
		    return false;
		}
	    }
	    return true;
	}
	
	var sequence = [
	    function() {
		x.xapi.session.login_with_password(
		    function(result) {
			x.session=x.check(result);
			sequence[1](); },
		    x.username,x.password); },
	    function() {
		if(!x.cache_objects) {
		    return;
		}
		x.xapi.event.register(
		    function(result) {
			x.check(result);
			sequence[2](); },
		    x.session,["*"]); },
	    function() {
		for(c in x.xapi) {
		    if(x.xapi[c].get_all_records) {
			x.xo[c.toLowerCase()]=-1;
			x.xapi[c].get_all_records(
			    function(c) {
				return function(result) {
				    x.xo[c]=x.check(result); 
				    if(check_all_done()) {
					x.xapi.event.next(function(result) {x.eventCallback(x.check(result));}, x.session);
					x.status=1;
					x.startMetrics();
					finishedfn();
				    }
				};
			    } (c.toLowerCase()),x.session);
		    }
		}
	    }
	    
	];
	
	if(this.apiversion.minor>2) {
	    /* Orlando and above */
	    this.xapi = new $.rpc(
		"/json",
		"xml", 
		function() {sequence[0]();},
		null
	    ); 
	} else {
	    /* Miami and below */
	    this.xapi = new $.rpc(
		"/json",
		"xml", 
		function() {sequence[0]();},
		null,
		mymessages);
	}
		
	return;	
    },

    startMetrics : function() {
	var x=this;
	if(!x.get_rrds) {
	    return;
	}
	x.metricsTick();
	this.myticker=window.setInterval(function() {x.metricsTick();},5000.0);
	for(var cb in x.listeners) {
	    if(x.listeners.hasOwnProperty(cb)) {
		x.listeners[cb].callback();
	    }
	}
    }
};

$xapi=new Xenapi();
