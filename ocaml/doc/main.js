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

