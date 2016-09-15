/*
 * Copyright (C) Jon Ludlam, 2005
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

var SVG_NS="http://www.w3.org/2000/svg";

var graphs = new Object;
var sets = new Object;
	       
/* This function was taken from grace */

function nicenum(x) {
  if (x==0.0)
    return 0.0;
  
  var xsign;
  if (x<0.0)
    xsign=-1.0;
  else
    xsign=1.0;
  
  x = Math.abs(x);
  
  var fexp = Math.floor(Math.log(x) / Math.log(10.0));
  
  var sx = x / Math.pow(10.0,fexp) / 10.0;
  var rx = Math.floor(sx);
  var f = 10.0 * (sx - rx);
  var y;
  if(f<1.0)
    y=1.0;
  else if(f<3.0)
    y=2.0;
  else if(f<7.0)
    y=5.0;
  else y=10.0;
  var sx2 = rx + (y / 10.0);
  
  return (xsign * sx2 * 10.0 * Math.pow(10.0,fexp));

}

function dset (data,name,displayname,min,max) {
  this.min = min;
  this.max = max;
  this.data = data;
  this.name = name;
  this.displayname = displayname;
  return this;
}

function graph(div,name,w,h) {
  this.name = name;
  this.mygraph = div;
    
  this.width = w;
  this.height = h;
    
  this.size_x = 0.8;
  this.size_y = 0.85;
    
  this.min_x = -5.0;
  this.max_x = 10.0;
  this.min_y = -5.0;
  this.max_y = 10.0;
  this.tick_x = 2.0;
  this.tick_y = 2.0;
  this.tick_length = Math.round(w / 100.0);
    
  this.o_x = 0;
  this.o_y = 0;
    
  this.setsy = [];
  this.setx = "none";

    this.autoscale_min=0;
    this.autoscale_max=1;
    
  $($.create("g",{ns:SVG_NS,transform:"translate(0.5,0.5)"},[
	       "rect",{ns:SVG_NS,id:this.name+"background"},[],
	       "rect",{ns:SVG_NS,id:this.name+"foreground"},[],
	       "clipPath",{ns:SVG_NS,id:this.name+"foreclip"},[
		 "rect",{ns:SVG_NS},[]],
		 "path",{ns:SVG_NS,id:this.name+"xaxis"},[],
		 "path",{ns:SVG_NS,id:this.name+"yaxis"},[],
		 "g",{ns:SVG_NS,id:this.name+"xticks"},[],
      "g",{ns:SVG_NS,id:this.name+"yticks"},[],
      "g",{ns:SVG_NS,id:this.name+"legbox"},[]])).appendTo(this.mygraph);

    this.cols=["rgb(0,0,0)","rgb(255,0,0)","rgb(0,255,0)","rgb(0,0,255)","rgb(255,255,0)","rgb(255,0,255)","rgb(0,255,255)"];

    this.mygraphg=$('g:first',this.mygraph);
    this.background=$('#'+this.name+"background");
    this.foreground=$('#'+this.name+"foreground");
    this.forecliprect=$('#'+this.name+"foreclip").children();
    this.xaxis=$('#'+this.name+"xaxis");
    this.yaxis=$('#'+this.name+"yaxis");
    this.legboxg=$('#'+this.name+"legbox");
    
    this.leg_font_size = 8 + "pt";
    this.leg_spacing = 15.0;
}

graph.prototype = {
  xtosx:function(x) { with(this) { return (o_x + ((x - min_x) / (max_x - min_x)) * size_x * width); }},
  ytosy:function(y) { with(this) { return (height - (o_y + ((y - min_y) / (max_y - min_y)) * size_y * height)); }},
  sxtox:function(sx) { with(this) { return ((sx - o_x)*((max_x - min_x)/(size_x*width))+min_x); }},
  sytoy:function(sy) { with(this) { return (((height - sy) - o_y)*((max_y - min_y)/(size_y*height))+min_y);}},
  rsxtox:function(sx) { with(this) { return (sxtox(sx - mygraph.getScreenCTM().e)); }},
  rsytoy:function(sy) { with(this) { return (sytoy(sy - mygraph.getScreenCTM().f)); }},

  plotdatapoints: function() {
    with(this) {
	for(var i=0; i<setsy.length; i++) {
	    var sety=setsy[i];
	    var mysetx = sets[setx];
	    var mysety = sets[sety];
	    
	    if(mysetx&&mysety) {
		var x0 = this.xtosx(mysetx.data[0]);
		var y0 = this.ytosy(mysety.data[0]);
		
		var path = "M"+x0+","+y0;
		
		for(var j=1; j<mysetx.data.length; j++) {
		    var x = mysetx.data[j];
		    var y = mysety.data[j];	

		    if(isNaN(y))
			y=0;

		    if(!isNaN(x)) 
			path = path + "L"+ xtosx(x) + ","+ytosy(y);
		}
		
		var myplot=$("#"+name+i+"datapointplot");
		
		if(myplot.length>0) {
		    myplot.attr("d",path);
		} else {
		    myplot=$($.create('path',
				      {ns:SVG_NS,d:path,id:name+i+"datapointplot",
				       "clip-path":"url(#"+this.name+"foreclip",
				       style:"fill:none; stroke-width:0.5px; stroke:"+cols[i]+";"},
				      [])).appendTo(mygraphg);
		}
	    }
	}
    }
  },

  do_ticks:function() {
    with(this) {	    
	var xticksg = $('#'+name+"xticks").empty();
	var yticksg = $('#'+name+"yticks").empty();

	var initx = Math.round(((min_x / tick_x) * 10.0 ) + 0.5);
	var inity = Math.round(((min_y / tick_y) * 10.0 ) + 0.5);
	var endx = Math.round(((max_x / tick_x) * 10.0 ) + 0.5);
	var endy = Math.round(((max_y / tick_y) * 10.0 ) + 0.5);
	
	for(i=initx; i<endx; i++) {
	    if(i!=0) {
		var x = Math.round(xtosx(i*tick_x/10.0));	
		
		if(min_y < 0 && max_y > 0)
		    var y = Math.round(ytosy(0));
		else
		    var y = Math.round(ytosy(min_y));
		
		var y1 = y+tick_length;
		var ymx = Math.round(ytosy(max_y));
		var ymn = Math.round(ytosy(min_y));

		var stroke = (i%10==0)?("rgb(150,150,255)"):("rgb(200,200,255)");
		if(i%10==0) {
		    xticksg.append($($.create("path",{ns:SVG_NS,"d":"M"+x+","+y+"L"+x+","+y1,style:"fill:none; stroke-width:0.5px; stroke:rgb(0,0,0);"},[],
					      "path",{ns:SVG_NS,d:"M"+x+","+ymn+"L"+x+","+ymx,style:"fill:none; stroke-width:0.5px; stroke:"+stroke+";"},[],
					      "text",{ns:SVG_NS,"x":x, "y":y1+this.tick_length+8.0,fill:"black", "text-anchor":"middle", "font-size":font_size},[(i*tick_x/10).toPrecision(2)])));
		} else {
		    xticksg.append($($.create("path",{ns:SVG_NS,d:"M"+x+","+ymn+"L"+x+","+ymx,style:"fill:none; stroke-width:0.5px; stroke:"+stroke+";"},[])));
		}
		
	    }
	}
	
	for(i=inity; i<endy; i++) {
	    if(i!=0) {
		var y = Math.round(ytosy(i*tick_y/10.0));
		
		if(min_x < 0 && max_x > 0)
		    var x = Math.round(xtosx(0));
		else
		    var x = Math.round(xtosx(min_x));
		
		var x1 = x-tick_length;
		var xmx = Math.round(xtosx(max_x));
		var xmn = Math.round(xtosx(min_x));

		if(i%10==0) {
		    yticksg.append($($.create("path",{ns:SVG_NS,"d":"M "+x+","+y+" L "+x1+","+y,style:"fill:none; stroke-width:0.5px; stroke:rgb(0,0,0);"},[],
					      "path",{ns:SVG_NS,"d":"M "+xmn+","+y+" L "+xmx+","+y,style:"fill:none; stroke-width:0.5px; stroke:rgb(120,120,255);"},[],
					      "text",{ns:SVG_NS,"x":x-this.tick_length-5.0,"y":y+5.0,fill:"black","text-anchor":"end","font-size":font_size},[(i*tick_y/10).toPrecision(2)])));
		} else {
		    yticksg.append($($.create("path",{ns:SVG_NS,"d":"M "+xmn+","+y+" L "+xmx+","+y,style:"fill:none; stroke-width:0.5px; stroke:rgb(200,200,255);"},[])));
		}
	    }

	    
	}
      dolegend();
    }
  },


    setup_graph:function(autoscalex,autoscaley) {
	with(this) {
	    mygraph.attr({width:width+10,
			  height:height+10});
	    
	    background.attr({"x":5,"y":5,width:width, height:height,
			     style:"fill:rgb(224,224,255);stroke-width:1px;stroke:rgb(0,0,0);"});
	    
	    var x = width * ((1.0 - size_x) / 2.0);
	    var y = height * ((1.0 - size_y) / 2.0);
	    o_x = x;
	    o_y = y;
	    
	    foreground.attr({"x":x,"y":y,width:width*size_x,height:height*size_y,
			     style:"fill:rgb(255,255,255);stroke-width:1px;stroke:rgb(0,0,0);"});
	    
	    forecliprect.attr({"x":x,"y":y,"width":width*size_x,height:height*size_y});
	    
	    if(autoscalex) {
		max_x=autoscale_max;
		min_x=autoscale_min;
		
		var mysetx = sets[setx];	
		
		if(mysetx) {
		    min_x = mysetx.min;
		    max_x = mysetx.max;
		}
		
		var widthx=max_x-min_x;
		var midx=(min_x+max_x) / 2;
		min_x=midx - (widthx / 2)*1.1;
		max_x=midx + (widthx / 2)*1.1;
		
		
	    }
	    
	    
	    if(autoscaley) {
		max_y=autoscale_max;
		min_y=autoscale_min;
		
		for(var i=0; i<setsy.length; i++) {
		    var sety=setsy[i];
		    var mysety = sets[sety];
		    
		    if(mysety) {
			min_y = (mysety.min < min_y) ? mysety.min : min_y;
			max_y = (mysety.max > max_y) ? mysety.max : max_y;
		    }
		}
		
		var widthy=max_y-min_y;
		var midy=(min_y+max_y) / 2;
		min_y=midy - (widthy / 2)*1.1;
		max_y=midy + (widthy / 2)*1.1;
	    }
	    
	    
	    this.font_size = Math.round(2.0 + (width / 100.0)) + "pt";
	    
	    var sxmin = Math.round(xtosx(min_x));
	    var sxmax = Math.round(xtosx(max_x));
	    var sx0 = Math.round(xtosx(0));
	    var symin = Math.round(ytosy(min_y));
	    var symax = Math.round(ytosy(max_y));
	    var sy0 = Math.round(ytosy(0));
	    
	    if(height<width) {
		tick_x = nicenum(max_x - min_x) / 10.0;
		tick_y = nicenum((max_y - min_y)*width/height) / 10.0;
	    } else {
		tick_x = nicenum((max_x - min_x)*height/width) / 10.0;
		tick_y = nicenum(max_y - min_y) / 10.0;
	    }
	    
	    var attrs={style:"fill:none; stroke-width:0.5px; stroke:rgb(0,0,0);"};
	    attrs.d="M " + sxmin +","+sy0+" L "+sxmax+","+sy0;
	    xaxis.attr(attrs);
	    attrs.d="M " + sx0 +","+symin+" L "+sx0+","+symax;
	    yaxis.attr(attrs);
	    
	    do_ticks();
	}
    },
    
    dolegend : function()
    {
	with(this) {
	    legboxg.empty();

	    var y=0;

	    for(var i=0; i<setsy.length; i++)
	    {
		var set=sets[setsy[i]];
		
		if(set) {
		    legboxg.append($($.create("text",
					      {ns:SVG_NS,x:o_x+70.0,y:o_y+y*leg_spacing+22.0,
					       fill:"black","text-anchor":"start","font-size":leg_font_size},[set.displayname])));
		    legboxg.append($($.create("path",{ns:SVG_NS,
						      d:"M "+(o_x+20.0)+","+(o_y+y*leg_spacing+16.0)+" L "+(o_x+50.0)+","+(o_y+y*leg_spacing+16.0),
						      style:"stroke-width:0.5pt; stroke:"+cols[i]},[])));
		    y++;
		}
	    }

	    legboxg.attr("height",y*15.0+10.0);
	}

    },

  bindxy: function(setxname,setyname) {
      this.setsy.push(setyname);
      this.setx = setxname;
  }
}

function creategraph(div,name,w,h) {
    graphs[name]=new graph(div,name,w,h);
    graphs[name].setup_graph(true);
    return graphs[name];
}

function find_graph(name) {
  return graphs[name];
}

function getminmax(data) 
{
  var min=data[0];
  var max=data[0];

  for(var i=0; i<data.length; i++) {
    if(data[i]>max)
      max=data[i];
    if(data[i]<min)
      min=data[i];
  }

  return {max:max,min:min};
}

function bind(graph,setxname,setyname) {
  graph=find_graph(graph);
  graph.bindxy(setxname,setyname);
}


