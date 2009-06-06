#!/bin/sh

set -eu

function get_tex()
{
  cat OMakefile | sed -n "s#$1 = \\(.*\\)\$#\\1#p"
}

tex="$(get_tex 'SHARED_TEX') $(get_tex 'OPEN_TEX')"
eps="$(get_tex 'SHARED_EPS') $(get_tex 'OPEN_EPS')"
dot=$(ls ${eps//.eps/.dot} 2>/dev/null || true)

tempdir=$(mktemp -d)

dir=$(dirname "$0")

cd "$dir"

omake clean
omake xenapi.pdf

version=$(cat xenapi-coversheet.tex |
          sed -n 's#.*revstring[^0-9]*\([0-9][0-9.]*\).*$#\1#p')

mkdir "$tempdir/xenapi-src-$version"
cp xenapi.pdf "$tempdir/xenapi-$version.pdf"
cp xenapi.ps "$tempdir/xenapi-$version.ps"
for file in $tex
do
 sed $file -e 's#% All rights reserved.#% Permission is granted to copy, distribute and/or modify this document under\n% the terms of the GNU Free Documentation License, Version 1.2 or any later\n% version published by the Free Software Foundation; with no Invariant\n% Sections, no Front-Cover Texts and no Back-Cover Texts.  A copy of the\n% license is included in the section entitled\n% "GNU Free Documentation License" or the file fdl.tex.#g' \
  >"$tempdir/xenapi-src-$version/$file"
done
cp fdl.tex $eps $dot "$tempdir/xenapi-src-$version/"

tar cjf "xenapi-src-$version.tar.bz2" -C "$tempdir" "xenapi-src-$version"

echo -e "\n\nYour release is in $tempdir."
