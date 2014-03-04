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
	document.write('<title>Citrix XenServer Management API</title>');
}

function make_header(t) {
	html = '<img src="http://citrix.com/site/resources/v4_includes/css/5.0/citrix_logo.gif" alt="Citrix" width="208px" height="36px" border="0" style="float:left">'
		+ '<h1 style="float:left; font-size: 24px;"><a href="index.html">XenServer Management API</a></h1>'
	document.getElementById('header').innerHTML = html;
}

first_release = 'rio';

function get_release_name(s)
{
	switch (s) {
	case 'rio':
		return 'XenServer 4.0';
		break;
	case 'miami':
		return 'XenServer 4.1';
		break;
	case 'symc':
		return 'XenServer 4.1.1';
		break;
	case 'orlando':
		return 'XenServer 5.0';
		break;
	case 'orlando-update-1':
		return 'XenServer 5.0 Update 1';
		break;
	case 'george':
		return 'XenServer 5.5';
		break;
	case 'midnight-ride':
		return 'XenServer 5.6';
		break;
	case 'cowley':
		return 'XenServer 5.6 FP1';
		break;
	case 'boston':
		return 'XenServer 6.0';
		break;
	case 'tampa':
		return 'XenServer 6.1';
		break;
	case 'clearwater':
		return 'XenServer 6.2';
		break;
	default:
		return 'Unreleased';
		break;
	}
}

