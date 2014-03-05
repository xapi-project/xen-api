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
var rel = getQuerystring('r');

function qualifier(q)
{
	if (q == 'DynamicRO' || q == 'StaticRO')
		return 'read-only';
	else
		return 'read/write';
}

function make_enum(t)
{
	n = t[1];
	obj = document.getElementById('enums');
	i = obj.children.length + 1;
	obj.style.display = ''
	
	if (document.getElementById('enum_' + n) == null) {
		html = '<div id="enum_' + n + '" class="field' + toggle(i) + '" ';
		html += 'onclick="showhide(document.getElementById(\'enum_' + n + '_details\'))">';
		html += '<div class="field-name">' + n + '</div>';
		html += '<div id="enum_' + n + '_details" style="display: none">';
		
		html += '<table class="field-table">';
		var first = true;
		for (i in t[2]) {
			html += '<tr><td style="white-space: nowrap">' + (first ? '<span class="field-head">Values:</span>' : '') + '</td>';
			html += '<td style="white-space: nowrap">' + i + '</td><td>' + t[2][i] + '</td></tr>';
			first = false;
		}
		html += '</table>';
		html += '</div></div>';

		obj.innerHTML += html;
	}
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
		make_enum(t);
		return '<a href="#enum_' + t[1] + '">' + t[1] + '</a>';
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
		return '{' + map(function(v){return transform_default(v)}, t[1]) + '}';
	case "VMap":
		return '{' + map(function(v){return transform_default(v[0]) + ' \u2192 ' + transform_default(v[1])}, t[1]) + '}';
	}
	return 'Unknown[' + t + ']';
}

function current_lifecycle_stage(s)
{
	if (s.length == 0)
		return 'Prototype';
	else {
		last_transition = s[s.length-1][0];
		switch (last_transition) {
		case 'Prototyped':
			return 'Prototype';
			break;
		case 'Deprecated':
			return 'Deprecated';
			break;
		case 'Removed':
			return 'Removed';
			break;
		case 'Published':
		case 'Changed':
		case 'Extended':
		default:
			return '';
			break;
		}
	}
}

function make_field(fld, n)
{
	name = fld.full_name.join('_');

	html = "";	
	html = '<div class="field' + toggle(n) + '" ';
	html += 'onclick="showhide(document.getElementById(\'' + name + '_details\'))">';
	html += '<div class="lifecycle">' + current_lifecycle_stage(fld.lifecycle) + '</div>';
	html += '<div><span class="inline-type">' + transform_type(fld.ty) + '</span> <span class="field-name">' + name + '</span> <span class="inline-qualifier">[' + qualifier(fld.qualifier) + ']</span></div>';
	
	html += '<div id="' + name + '_details" style="display: none">';
	html += '<div class="field-description">' + fld.field_description + '</div>';
	
	html += '<table class="field-table">';
	if (fld.default_value != undefined)
		html += '<tr><td style="white-space: nowrap"><span class="field-head">Default value:</span></td><td colspan="2">' + transform_default(fld.default_value) + '</td></tr>';
		
	for (i in fld.lifecycle) {
		l = fld.lifecycle[i];
		html += '<tr><td style="white-space: nowrap"><span class="field-head">' + l[0] + ' in:</span></td><td style="white-space: nowrap">' + get_release_name(l[1]) + '</td><td>' + l[2] + '</td></tr>';
	}
	html += '</table>';
	html += '</div></div>';
	
	return html;
}

function make_message(msg, n)
{
	name = msg.msg_name;
	
	html = "";
	
	html += '<div class="field' + toggle(n) + '" ';
	html += 'onclick="showhide(document.getElementById(\'' + name + '_details\'))">';
	html += '<div class="lifecycle">' + current_lifecycle_stage(msg.msg_lifecycle) + '</div>';
	html += '<div><span class="inline-type">' + 
		(msg.msg_result != undefined ? transform_type(msg.msg_result[0]) : 'void') + 
		'</span> <span class="field-name">' + name + '</span> <span class="inline-params">(' +
		map(function(p){return transform_type(p.param_type)}, msg.msg_params).join(', ') + 
		')</span></div>';
	
	html += '<div class="field-description">' + msg.msg_doc + '</div>';
	
	html += '<div id="' + name + '_details" style="display: none">';
	html += '<table class="field-table">';
	for (i in msg.msg_params) {
		p = msg.msg_params[i];
		html += '<tr><td style="white-space: nowrap">' + (i == 0 ? '<span class="field-head">Parameters:</span>' : '') + '</td>';
		html += '<td style="white-space: nowrap">' + transform_type(p.param_type) + '  ' + p.param_name + '</td>';
		html += '<td>' + p.param_doc + '</td></tr>';
	}

	html += '<tr><td><span class="field-head">Minimum role:</span></td><td colspan="2">' + msg.msg_allowed_roles[msg.msg_allowed_roles.length - 1] + '</td></tr>';
	
	if (msg.msg_result != undefined)
		html += '<tr><td><span class="field-head">Result:</span></td><td colspan="2">' + msg.msg_result[1] + '</td></tr>';
		
	if (msg.msg_errors != undefined && msg.msg_errors.length > 0) {
		for (i in msg.msg_errors) {
			e = msg.msg_errors[i];
			html += '<tr><td style="white-space: nowrap">' + (i == 0 ? '<span class="field-head">Errors:</span>' : '') + '</td>';
			html += '<td style="white-space: nowrap">' + e.err_name + '</td>';
			html += '<td>' + e.err_doc + '</td></tr>';
		}
	}
	
	for (i in msg.msg_lifecycle) {
		l = msg.msg_lifecycle[i];
		html += '<tr><td style="white-space: nowrap"><span class="field-head">' + l[0] + ' in:</span></td>';
		html += '<td style="white-space: nowrap">' + get_release_name(l[1]) + '</td><td>' + l[2] + '</td></tr>';
	}
	html += '</table>';
	html += '</div></div>';	
	
	return html;
}

function class_doc()
{	
	contents = clsdoc.contents;	
	fields = fold(function(a, x){
		if (x[0] == "Field") a.push(x);
		else a = a.concat(x[2]);
		return a;
		}, [], contents);
	fields = map(function(f){return f[1];}, fields);
	
	fields = filter(function(f){return f.internal_only == false;}, fields);
	fields.sort(function(a, b){return compare(a.full_name.join('_').toLowerCase(), b.full_name.join('_').toLowerCase());});
	messages = clsdoc.messages;
	messages = filter(function(m){return m.msg_hide_from_docs == false;}, messages);
	messages.sort(function(a, b){return compare(a.msg_name.toLowerCase(), b.msg_name.toLowerCase())});

	html = "";
	html += '<div class="lifecycle">' + current_lifecycle_stage(clsdoc.obj_lifecycle) + '</div>';
	html += '<h1 class="title" onclick="showhide(document.getElementById(\'class_details\'))" style="cursor: pointer">Class: ' + cls + '</h1>\n';
	
	html += '<div class="description">' + clsdoc.description + '</div>';
	
	html += '<div  id="class_details" style="display: none">';
	html += '<table class="field-table">';
	for (i in clsdoc.obj_lifecycle) {
		l = clsdoc.obj_lifecycle[i];
		html += '<tr><td width="130px"><span class="field-head">' + l[0] + ' in:</span></td><td width="130px">' + get_release_name(l[1]) + '</td><td>' + l[2] + '</td></tr>';
	}
	html += '</table>';
	html += '</div>';
	
	html += '<div id="enums" style="display: none"><h2>Enums</h2></div>';
	
	set_content(html);
	
	html = '<h2>Fields</h2>';
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
	
	append_content(html);
}

function compare_release_notes(a, b)
{
	function change_to_num(x) {
		if (x.indexOf('Published') > -1) return '0';
		else if (x.indexOf('Extended') > -1) return '1';
		else if (x.indexOf('Changed') > -1) return '2';
		else if (x.indexOf('Deprecated') > -1) return '3';
		else if (x.indexOf('Removed') > -1) return '4';
		else return '5';
	}
	function element_to_num(x) {
		if (x.indexOf('class') > -1) return '0';
		else if (x.indexOf('field') > -1) return '1';
		else if (x.indexOf('message') > -1) return '2';
		else return '3';
	}
	x = change_to_num(a[0]) + element_to_num(a[0]) + (a[1]+a[2]).toLowerCase();
	y = change_to_num(b[0]) + element_to_num(b[0]) + (b[1]+b[2]).toLowerCase();
	return compare(x, y);
}

function release_doc()
{	
	changes = [];
	
	for (i in release_info) {
		c = release_info[i];
		for (j in c.obj_changes)
			changes.push([c.obj_changes[j][0] + ' class', c.cls, '', c.obj_changes[j][2]]);
		for (j in c.field_changes)
			changes.push([c.field_changes[j][0] + ' field', c.cls, c.field_changes[j][1], c.field_changes[j][2]]);
		for (j in c.msg_changes)
			changes.push([c.msg_changes[j][0] + ' message', c.cls, c.msg_changes[j][1], c.msg_changes[j][2]]);
	}
	
	changes.sort(compare_release_notes);
	
	html = "";
	html += '<h1 class="title">Release notes: ' + get_release_name(rel) + '</h1>\n';

	html += '<table><tr><th style="width: 12em">Change</th><th>Element</th><th>Description</th></tr>';
		
	for (i in changes) {
		html += '<tr><td>' + changes[i][0] + '</td><td><a href="?c=' + changes[i][1] + (changes[i][2] != '' ? '#' + changes[i][2] : '') + '">' +
			changes[i][1] + (changes[i][2] != '' ? '.' + changes[i][2] : '') + '</a></td><td>' + changes[i][3] + '</td></tr>';
	}
	
	html += '</table>';

	set_content(html);
}

function class_list()
{
	html = '<h2 class="title">Classes</h2>';
	html += '<div id="class_overview"><a href="index.html">Overview</a></div>';
	
	classes.sort(function(a, b){return compare(a.toLowerCase(), b.toLowerCase())});
	for (i in classes) {
		c = classes[i];
		html += '<a href="?c=' + c + '">' + c + '</a><br>';
	}
	
	append_sidebar(html);
}

function release_list()
{
	html = '<h2>Release notes</h2>';
	
	releases = releases.slice(releases.indexOf(first_release))
	for (i in releases) {
		r = releases[i];
		html += '<a href="?r=' + r + '">' + get_release_name(r) + '</a><br>';
	}
	
	append_sidebar(html);
}

function build()
{
	make_header('apidoc');
	if (cls != "") {
		class_list();
		release_list();
		class_doc();
	}
	else if (rel != "") {
		class_list();
		release_list();
		release_doc();
	}
	else {
		class_list();
		release_list();
	}
}

