#!/usr/bin/python

"""
Copyright (C) 2006-2009 Citrix Systems Inc.
This program is free software; you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as published
by the Free Software Foundation; version 2.1 only. with the special
exception on linking described in file LICENSE.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Lesser General Public License for more details.
"""

import os
import sys
import string

docdir = sys.argv[1]
name = sys.argv[2]
if sys.argv[3] == "library": is_library = True
else: is_library = False
modules = set(sys.argv[4].split())
includes = sys.argv[5].split()
packs = sys.argv[6]
libs = sys.argv[7]

dest = docdir + '/' + name

packs = packs.replace(',', ' ')
packs = packs.split()

if is_library: packs.extend(['threads', 'bigarray', 'stdext', 'log', 'unix', 'xmlm', 'xml-light2', 'uuid', 'cdrom', 'mmap', 'xc', 'xb', 'xs'])

if len(packs) > 0:
	packages = "-package " + str(','.join(packs))
else:
	packages = ""

doc_command = 'ocamlfind ocamldoc -v ' + packages + ' -I +threads -sort -g /myrepos/xen-api.hg/ocaml/doc/odoc_json.cma -d ' + dest + ' '

files = []
for m in modules:
	d, f = os.path.split(m)
	l = os.listdir('./' + d)
	if f + '.ml' in l:
		files.append(m + '.ml')
	if f + '.mli' in l:
		files.append(m + '.mli')

includesx = []
for i in includes:
	includesx.append('-I ' + i)
	
# run ocamldoc

os.system(doc_command + ' ' + string.join(includesx) + ' ' + string.join(files))

# add library dependencies to index files

f = file(dest + '/index.json', 'a')
libs = libs.split()
libs.extend(packs)
libs = map(lambda s: '"' + s.split('/')[-1] + '"', libs)
libs = list(set(libs))	# remove duplicates
s = 'deps_' + name.replace("-", "") + ' = [' + ', '.join(libs) + '];'
f.write(s)
f.close()

# update components file

executables = []
libraries = []
	
try:
	f = file(docdir + '/components.js', 'r')
	exec(f.readline())
	exec(f.readline())
	f.close()
except:
	pass

if is_library:
	libraries.append(name)
	libraries = list(set(libraries))
else:
	executables.append(name)
	executables = list(set(executables))

f = file(docdir + '/components.js', 'w')
f.write('executables = ' + str(executables) + '\n')
f.write('libraries = ' + str(libraries))
f.close()

