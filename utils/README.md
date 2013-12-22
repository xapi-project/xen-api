The `reader` and `writer` executables together make up a system test and
experimentation tool for the library as a whole.

`writer` can be run in file or page mode. In file mode it requires a filename
and protocol argument, and in page mode it requires a domid (i.e. the domain
with which the page will be shared) and a protocol argument. `writer` will
write a set of datasources to the shared resource every 5 seconds, and
periodically the number of datasources writtern will change. In page mode,
`writer` will print the grant reference of the shared page to stdout.

`reader` can also be run in file or page mode. In file mode it requires a
filename and protocol argument, and in page mode it requires the domid of the
domain sharing the page, a grant reference and a protocol argument. It can used
both to verify the output of `writer`, and also to read and print the output of
[rrdd](https://github.com/xapi-project/xcp-rrdd) plugins for debugging.
