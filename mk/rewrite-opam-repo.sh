#!/bin/bash

# Rewrite opam repository to point to local git repos rather than remote ones

pushd $1

for i in packages/*/url
do 
	sed s/git:\\/\\/github.com\\/[^\\/]*\\//\\/repos\\//g $i > $i.tmp
	sed s/.git\"$/\"/ $i.tmp > $i
	rm $i.tmp
done

opam-mk-repo

popd
