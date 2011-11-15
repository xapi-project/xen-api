// function from http://www.bloggingdeveloper.com/post/JavaScript-QueryString-ParseGet-QueryString-with-Client-Side-JavaScript.aspx
function getQuerystring(key, default_)
{
	if (default_ == null) default_ = ""; 
	key = key.replace(/[\[]/,"\\\[").replace(/[\]]/,"\\\]");
	var regex = new RegExp("[\\?&]"+key+"=([^&#]*)");
	var qs = regex.exec(window.location.href);
	if (qs == null)
		return default_;
	else
		return qs[1];
}

function set_content(html)
{
	document.getElementById('content').innerHTML = html;
}

function append_content(html)
{
	document.getElementById('content').innerHTML += html;
}

function set_sidebar(html)
{
	document.getElementById('sidebar').innerHTML = html;
}

function append_sidebar(html)
{
	document.getElementById('sidebar').innerHTML += html;
}

function toggle(i)
{
	if (i % 2 == 0)
		return ""
	else
		return "2"
}

function showhide(obj)
{
	if (obj.style.display == '')
		obj.style.display = 'none';
	else
		obj.style.display = '';
}

// functional stuff

function filter(f, l)
{
	var x = [];
	for (i in l) {
		if (f(l[i]))
			x.push(l[i]);
	}
	return x;
}

function map(f, l)
{
	var x = [];
	for (i in l) {
		x[i] = f(l[i]);
	}
	return x;
}

function fold(f, a, l)
{
	if (l.length == 0)
		return a;
	else
		return fold(f, f(a, l[0]), l.slice(1));
}

// compare function for sorting
function compare(a, b)
{
	if (a < b) return -1;
	if (a > b) return 1;
	return 0;
}

// include a JS script
function load_script(url, callback)
{
	var head= document.getElementsByTagName('head')[0];
	var script= document.createElement('script');
	script.type= 'text/javascript';
	script.src= url;
	script.onreadystatechange = callback;
	script.onload = callback
	head.appendChild(script);
}

