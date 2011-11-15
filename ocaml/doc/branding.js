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

function make_title() {
	document.write('<title>Xapi Documentation</title>');
}

function make_header(t) {
	if (t == 'apidoc')
		title = 'Xapi &ndash; XenAPI Documentation';
	else if (t == 'codedoc')
		title = 'Xapi &ndash; OCaml Code Documentation';
	else
		title = 'Xapi &ndash; Documentation';

	html = '<h1 style="float:left"><a href="index.html" style="text-decoration: none">'
		+ title
		+ '</a></h1><ul id="menu"><li><a href="index.html">XenAPI Docs</a></li>'
		+ '<li><a href="codedoc.html">Code Docs</a></li></ul>';
	document.getElementById('header').innerHTML = html;
}

function get_release_name(s)
{
	return s;
}

