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

	html = '<h1 style="float:left; font-size: 24px;"><a href="index.html">XenServer Management API</a></h1>'
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
	case 'cowley':
		return 'XCP 1.0';
	case 'boston':
		return 'XCP 1.5';
	case 'tampa':
		return 'XCP 1.6';
	case 'clearwater':
		return 'XenServer 6.2';
	case 'vgpu-tech-preview':
		return 'XenServer 6.2 vGPU preview';
	case 'vgpu-productisation':
		return 'XenServer 6.2 SP1';
	case 'clearwater-felton':
		return 'XenServer 6.2 SP1 Hotfix 4';
	case 'clearwater-whetstone':
		return 'XenServer 6.2 SP1 Hotfix 11';
	case 'creedence':
		return 'XenServer 6.5';
	case 'cream':
		return 'XenServer 6.5 SP1';
	case 'dundee':
		return 'XenServer 7.0';
	default:
		return (s + ' (unreleased)');
	}
}

