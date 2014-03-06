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

first_release = 'midnight-ride';

function get_release_name(s)
{
	switch (s) {
	case 'rio':
	case 'miami':
	case 'symc':
	case 'orlando':
	case 'orlando-update-1':
	case 'george':
	case 'midnight-ride':
		return 'XCP 0.5';
		break;
	case 'cowley':
		return 'XCP 1.0';
		break;
	case 'boston':
		return 'XCP 1.5';
		break;
	case 'tampa':
		return 'XCP 1.6';
		break;
	case 'clearwater':
		return 'XenServer 6.2';
		break;
	case 'vgpu-tech-preview':
		return 'XenServer 6.2 vGPU preview';
		break;
	case 'vgpu-productisation':
		return 'XenServer 6.2 SP1'
		break;
	case 'clearwater-felton':
		return 'XenServer 6.2 SP1 Hotfix 4'
		break;
	default:
		return (s + ' (unreleased)');
		break;
	}
}

