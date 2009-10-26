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

function toggle(i)
{
	if (i % 2 == 0)
		return ""
	else
		return "2"
}

function value(v, n)
{
	l = v.name.split('.');
	name = l[l.length - 1];
	
	html = '<div class="field' + toggle(n) + '">';
	html += '<div class="field-type"><a name="' + name + '">[value]</a></div>';
	html += '<div class="field-name">' + name + '</div>';
	html += '<table>';
	html += '<tr><td width="100px"><span class="field-head">Type:</span></td><td>' + v.type + '</td></tr>';
	html += '<tr><td><span class="field-head">Description:</span></td><td>';
	if (v.info.description != undefined)
		html += v.info.description + '</td></tr>';
	else
		html += '<span class="empty">to be completed!</span></td></tr>';
	html += '</table>';
	html += '</div>';
	return html;
}

function exception(v, n)
{
	l = v.name.split('.');
	name = l[l.length - 1];
			
	html = '<div class="field' + toggle(n) + '">';
	html += '<div class="field-type"><a name="' + name + '">[exception]</a></div>';
	html += '<div class="field-name">' + name + '</div>';
	html += '<table>';
	html += '<tr><td width="100px"><span class="field-head">Type:</span></td><td>' + v.type + '</td></tr>';
	html += '<tr><td><span class="field-head">Description:</span></td><td>';
	if (v.info.description != undefined)
		html += v.info.description + '</td></tr>';
	else
		html += '<span class="empty">to be completed!</span></td></tr>';
	html += '</table>';
	html += '</div>';
	return html;
}

function variant(v)
{
	cons = v.constructors;
	html = '';
	html += '<table>';
	html += '<tr><th width="25%">Constructor</th><th>Type</th><th>Description</th></tr>';
	for (c in cons) {
		html += '<tr><td>' + cons[c].name + '</td>'
		html += '<td>' + cons[c].type + '</td>'
		if (cons[c].description != undefined)
			html += '<td>' + cons[c].description + '</td>';
		else
			html += '<td><span class="empty">to be completed!</span></td></tr>';
		html += '</tr>';
	}
	html += '</table>';
	return html;
}

function record(v)
{
	fields = v.fields;
	html = '<table>';
	html += '<tr><th width="25%">Field</th><th width="20%">Type</th><th>Description</th></tr>';
	for (c in fields) {
		html += '<tr><td>' + fields[c].name + '</td>'
		html += '<td>' + fields[c].type + '</td>'
		if (fields[c].description != undefined)
			html += '<td>' + fields[c].description + '</td>';
		else
			html += '<td><span class="empty">to be completed!</span></td></tr>';
		html += '</tr>';
	}
	html += '</table>';
	return html;
}

function type(v, n)
{
	l = v.name.split('.');
	name = l[l.length - 1];
		
	html = '<div class="field' + toggle(n) + '">';
	html += '<div class="field-type"><a name="' + name + '">[type]</a></div>';
	html += '<div class="field-name">' + name + '</div>';
	html += '<div class="field-description">';
	if (v.info.description != undefined)
		html += v.info.description + '</div>';
	else
		html += '<span class="empty">to be completed!</span></div>';
	if (v.kind.type == 'variant')
		html += variant(v.kind);
	else if (v.kind.type == 'record')
		html += record(v.kind);
	else if (v.kind.type == 'abstract')
		html += '<tr><td>&nbsp;</td>abstract</td></tr>'
	html += '</div>';
	return html;
}

function included_module(v, n)
{
	if (i % 2 == 0)
		toggle = ""
	else
		toggle = "2"
		
	html = '<div class="field' + toggle + '">';
	html += '<div class="field-type">[module]</div>';
	html += '<div class="field-name">' + v.name + '</div>';
	html += '<table>';
	html += '<tr><td width="100px"><span class="field-head">Type:</span></td><td>' + v.type + '</td></tr>';
	html += '<tr><td><span class="field-head">Description:</span></td><td>';
	if (v.info.description != undefined)
		html += v.info.description + '</td></tr>';
	else
		html += '<span class="empty">to be completed!</span></td></tr>';
	html += '</table>';
	html += '</div>';
	return html;
}

function comment(m)
{
	return m;
}

function parse_structure(structure)
{	
	details = "";
	values = [];
	exceptions = [];
	types = [];
	for (i in structure) {
		item = structure[i];
		for (j in item) {
			switch (j) {
			case 'included_module':
				details += included_module(item[j], i);
				break;
			case 'value':
				details += value(item[j], i);
				l = item[j].name.split('.');
				name = l[l.length - 1];
				values.push (name);
				break;
			case 'exception':
				details += exception(item[j], i);
				l = item[j].name.split('.');
				name = l[l.length - 1];
				exceptions.push (name);
				break;
			case 'type':
				details += type(item[j], i);
				l = item[j].name.split('.');
				name = l[l.length - 1];
				types.push (name);
				break;
			case 'comment':
				details += comment(item[j], i);
				break;
			default: break;
			}
		}
	}
	append_content(details);
	
	types.sort();
	values.sort();
	exceptions.sort();
	
	html = "";
	html += '<h1>Contents</h1>';
	html += '<h2>Types</h2>';
	for (i in types)
		html += '<a href="#' + types[i] + '">' + types[i] + '</a><br>';
	html += '<h2>Functions and Constants</h2>';
	for (i in values)
		html += '<a href="#' + values[i] + '">' + values[i] + '</a><br>';
	html += '<h2>Exceptions</h2>';
	for (i in exceptions)
		html += '<a href="#' + exceptions[i] + '">' + exceptions[i] + '</a><br>';
	append_sidebar(html);
}

function known_module(m)
{
	modules = component_modules[component];
	for (j in modules)
		if (modules[j].name == m)
			return true;
	return false;
}

function make_dependencies(deps)
{
	uses = deps.uses.sort();
	used_by = deps.used_by.sort();

	html = "<h1>Dependencies</h1>";
	html += '<h2>Uses</h2>';
	for (i in uses) {
		if (known_module(uses[i]))
			html += '<a href="?c=' + component + '&m=' + uses[i] + '">' + uses[i] + '</a><br>';
		else
			html += '<span class="grey">' + uses[i] + '</span><br>';
	}
	html += '<h2>Used by</h2>';
	for (i in used_by)
		html += '<a href="?c=' + component + '&m=' + used_by[i] + '">' + used_by[i] + '</a><br>';
	append_sidebar(html);
}

function moduledoc()
{
	mod = odoc.module;
	
	set_sidebar("");
	make_dependencies(mod.dependencies);
	
	html = "";
	html += '<h1 class="title">Module: ' + mod.name + '</h1>\n';
	html += '<div class="defined">Defined in ' + mod.file + '</div>';
	html += '<div class="description">';
	if (mod.info.description != undefined)
		html += mod.info.description + '</div>';
	else
		html += '<span class="empty">to be completed!</span></div>';
	set_content(html);
			
	parse_structure(mod.module_structure);
}

function index()
{	
	html = "";
	html += '<h1 class="title">List of Modules</h1>\n';
	for (i in components) {
		html += '<h1><a name="' + components[i] + '">' + components[i] + '</a></h1>';
		html += '<table><tr><th>Module</th><th>Description</th></tr>\n';
		modules = component_modules[components[i]];
		for (j in modules) {
			html += '<tr><td><a href="?c=' + components[i] + '&m=' + modules[j].name + '">' + modules[j].name + '</a></td>\n';
			html += '<td>' + modules[j].description + '</td></tr>\n';
		}
		html += '</table>\n';
	}
	set_content(html);
	
	html = "<h1>Components</h1>";
	for (i in components)
		html += '<a href="#' + components[i] + '">' + components[i] + '</a><br />';
	set_sidebar(html);
}

function build()
{
	fill_components();
	if (module == "")
		index();
	else
		moduledoc();
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

var module = getQuerystring('m');
var component = getQuerystring('c');
if (component == "") component = "xapi";

