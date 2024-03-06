# Python3 Scripts

This directory is intended for scripts that only run on python3. As more scripts are
ported to python3 this directory should start to fill up. The intended structure of
the directory is as follows:

- bin: This contains files to be installed in bin and are meant to be run by users
- libexec: This contains files to be installed in libexec and are meant to only be
run by xapi and other daemons.
- packages: This contains files to be installed in python's site-packages and are meant
to be modules and packages to be imported by other scripts or executed via python3 -m
- plugins: This contains files that are meant to be xapi plugins
