#!/usr/bin/python
#
# Copyright (c) 2008-2011 Citrix Systems, Inc.
#
# Permission to use, copy, modify, and distribute this software for any
# purpose with or without fee is hereby granted, provided that the above
# copyright notice and this permission notice appear in all copies.
#
# THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
# WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
# ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
# WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
# ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
# OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
#
import cgitb; cgitb.enable()
import cgi, os

# Note: XenAPI.py is shipped as part of the SDK
#  see http://community.citrix.com/display/xs/Download+SDKs
import XenAPI


def main():
	print "Content-type: text/html\n"
	form = cgi.FieldStorage()

	session = None
	if ("hostName" in form):
		hostName = form.getfirst("hostName", "")
		userName = form.getfirst("userName", "")
		password = form.getfirst("password", "")
		session = XenAPI.Session("http://%s/"%hostName)
		session.xenapi.login_with_password(userName, password)

		if ("vmId" in form):
			vmId = form.getfirst('vmId')
			vmRef = session.xenapi.VM.get_by_uuid(vmId)
			con = session.xenapi.VM.get_consoles(vmRef)
			url = session.xenapi.console.get_location(con[0])
			print '<html><body><center><table><tr><td>'
			print '<applet code="com/citrix/xenserver/console/Initialize.class"'
			print ' archive="/XenServerConsole.jar"'
			print ' width="800" height="600">'
			print ' <PARAM NAME=SESSION VALUE="%s">'%session._session
			print ' <PARAM NAME=URL VALUE="%s">'%url
			print ' <PARAM NAME=USEURL VALUE="true">'
			print '</applet>'
			print '</td></tr></table></center></body></html>'
			session.logout()
			return
		else:
			# List the running VMs
			vms = session.xenapi.VM.get_all()
			for vm in vms:
				if int(session.xenapi.VM.get_domid(vm)) > 0:
					print '<a href="%s?hostName=%s&userName=%s&password=%s&vmId=%s"' \
					    'target="_new">Connect to %s</a><br>'% \
					    (os.environ['REQUEST_URI'],
					     hostName, userName, password,
					     session.xenapi.VM.get_uuid(vm),
					     session.xenapi.VM.get_name_label(vm))
			session.logout()

	print '<form method="POST" action="%s">'%os.environ['REQUEST_URI']
	print '<p> Server: <input type="text" name="hostName"/>'
	print '<p> Username: <input type="text" name="userName"/>'
	print '<p> Password: <input type="password" name="password"/>'
	print '<input type="submit" name="connect" value="List VMs"/><br>'

main()
