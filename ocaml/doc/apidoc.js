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
 
// global variables

var cls = getQuerystring('c');

function qualifier(q)
{
	if (q == 'DynamicRO' || q == 'StaticRO')
		return 'read-only';
	else
		return 'read/write';
}

function transform_type(t)
{
	switch (t) {
	case "String":
	case "Int":
	case "Float":
	case "Bool":
	case "DateTime":
		return t.toLowerCase();
	}
	
	switch (t[0]) {
	case "Ref":
		return '<a href="?c=' + t[1] + '">' + t[1] + '</a> ref';
	case "Record":
		return t[1] + ' record';
	case "Enum":
		return t[1];
	case "Set":
		return transform_type(t[1]) + ' set';
	case "Map":
		return '(' + transform_type(t[1]) + ' \u2192 ' + transform_type(t[2]) + ') map';
	}
	return 'Unknown[' + t + ']';
}

function transform_default(t)
{
	switch (t[0]) {
	case "VString":
		return '"' + t[1] + '"';
	case "VInt":
	case "VFloat":
	case "VBool":
	case "VDateTime":
	case "VEnum":
		return t[1];
	case "VRef":
		return (t[1] == "" ? "Null" : t[1]);
	case "VRecord":
		return t[1] + ' record';
		return t[1];
	case "VSet":
		return '{' + t[1].map(function(v){return transform_default(v)}) + '}';
	case "VMap":
		return '{' + t[1].map(function(v){return transform_default(v[0]) + ' \u2192 ' + transform_default(v[1])}) + '}';
	}
	return 'Unknown[' + t + ']';
}

function make_field(fld, n)
{
	name = fld.field_name;

	html = "";	
	html = '<div class="field' + toggle(n) + '">';
	html += '<input type="button" class="small-button" value="details" onclick="showhide(document.getElementById(\'' + name + '_details\'))" />';
	html += '<div><span class="inline-type">' + transform_type(fld.ty) + '</span> <span class="field-name">' + name + '</span> <span class="inline-qualifier">[' + qualifier(fld.qualifier) + ']</span></div>';
	
	html += '<div  id="' + name + '_details" style="display: none">';
	html += '<div class="field-description">' + fld.field_description + '</div>';
	
	html += '<table class="field-table">';
	if (fld.release != undefined) {
		html += '<tr><td width="130px"><span class="field-head">Introduced in:</span></td><td>' + fld.release.internal[1] + '</td></tr>';
		if (fld.release.internal_deprecated_since != undefined)
			html += '<tr><td width="130px"><span class="field-head">Deprecated since:</span></td><td>' + fld.release.internal_deprecated_since + '</td></tr>';
	}
	if (fld.default_value != undefined)
		html += '<tr><td width="130px"><span class="field-head">Default value:</span></td><td>' + transform_default(fld.default_value) + '</td></tr>';
	
	html += '</table>';
	html += '</div></div>';
	
	return html;
}

function make_message(msg, n)
{
	name = msg.msg_name;
	
	html = "";
	
	html += '<div class="field' + toggle(n) + '">';
	html += '<input type="button" class="small-button" value="details" onclick="showhide(document.getElementById(\'' + name + '_details\'))" />';
	html += '<div><span class="inline-type">' + 
		(msg.msg_result != undefined ? transform_type(msg.msg_result[0]) : 'void') + 
		'</span> <span class="field-name">' + name + '</span> <span class="inline-params">(' +
		msg.msg_params.map(function(p){return transform_type(p.param_type)}).join(', ') + 
		')</span></div>';
	
	html += '<div class="field-description">' + msg.msg_doc + '</div>';
	
	html += '<div  id="' + name + '_details" style="display: none">';
	html += '<table class="field-table">';
	
	html += '<tr id="' + name + '_params"><td width="130px"><span class="field-head">Parameters:</span></td><td>';
	html += '<table>';
	for (i in msg.msg_params) {
		p = msg.msg_params[i];
		html += '<tr><td style="padding: 0 .4em 0 0">' + transform_type(p.param_type) + '  ' + p.param_name + '</td>';
		html += '<td style="padding: 0 0 0 .4em">' + p.param_doc + '</td></tr>';
	}
	html += '</table></td></tr>';	

	html += '<tr><td><span class="field-head">Minimum role:</span></td><td>' + msg.msg_allowed_roles[msg.msg_allowed_roles.length - 1] + '</td></tr>';
	if (msg.msg_result != undefined)
		html += '<tr><td><span class="field-head">Result:</span></td><td>' + msg.msg_result[1] + '</td></tr>';
	if (msg.msg_errors != undefined && msg.msg_errors.length > 0) {
		html += '<tr><td><span class="field-head">Errors:</span></td><td>'
		html += '<table>';
		for (i in msg.msg_errors) {
			e = msg.msg_errors[i];
			html += '<tr><td style="padding: 0 .4em 0 0">' + e.err_name + '</td>';
			html += '<td style="padding: 0 0 0 .4em">' + e.err_doc + '</td></tr>';
		}
		html += '</table></td></tr>';
	}
	if (msg.msg_release != undefined) {
		html += '<tr><td><span class="field-head">Introduced in:</span></td><td>' + msg.msg_release.internal[1] + '</td></tr>';
		if (msg.msg_release.internal_deprecated_since != undefined)
			html += '<tr><td><span class="field-head">Deprecated since:</span></td><td>' + msg.msg_release.internal_deprecated_since + '</td></tr>';
	}
	html += '</table>';
	
	html += '</div></div>';	
	
	return html;
}

function class_doc()
{	
	contents = clsdoc.contents;
	fields = contents.filter(function(f){return f[0] == "Field";}).map(function(f){return f[1];});
	fields.sort(function(a, b){return a.field_name.toLowerCase().charCodeAt(0) - b.field_name.toLowerCase().charCodeAt(0);});
	messages = clsdoc.messages;
	messages = messages.filter(function(m){return m.msg_hide_from_docs == false;})
	messages.sort(function(a, b){return a.msg_name.toLowerCase().charCodeAt(0) - b.msg_name.toLowerCase().charCodeAt(0)});

	html = "";
	html += '<h1 class="title">Class: ' + cls + '</h1>\n';
	
	html += '<div class="description">' + clsdoc.description + '</div>';
	
	html += '<h2>Fields</h2>';
	if (fields.length > 0) {
		for (i in fields)
			html += make_field(fields[i], i);
	}
	else
		html += '<p>None.</p>';
	
	html += '<h2>Messages</h2>';
	if (messages.length > 0) {
		for (i in messages)
			html += make_message(messages[i], i);
	}
	else
		html += '<p>None.</p>';
	
	set_content(html);
}

function build()
{
	html = "";
	html = '<h2 class="title">Classes</h2>';
	
	classes.sort(function(a, b){return a.toLowerCase().charCodeAt(0) - b.toLowerCase().charCodeAt(0)});
	for (i in classes) {
		c = classes[i];
		html += '<a href="?c=' + c + '">' + c + '</a><br>';
	}
	
	append_sidebar(html);
	
	if (cls != "") {
		class_doc();
	}
}

