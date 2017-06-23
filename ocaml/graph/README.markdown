# Describe object relationships via graphviz

To create a graph showing relationships between

* Virtual Machines (VMs)
* Virtual Disk Images (VDIs)
* Virtual Block Devices (VBDs)
* Storage Repositories (SRs)

run:

```bash
graph.native -h <host> -u root -pw <password> VM VDI VBD SR > graph.dot
```

Then install graphviz and run:

```bash
dot -Tsvg graph.dot -o graph.svg
```

See the script [go](go) for some more examples of graphs you can create.

## Notes

Unlike the rest of our ocaml client code, this example doesn't use our regular ocaml
XenAPI bindings. Instead the code parses the XMLRPC responses by walking over the
type declarations in the datamodel directly.
Unfortunately this means that a lot of unnecessary modules have to be linked in.
We should probably improve this by:

  1. creating an actual concrete syntax for the IDL, so you wouldn't have to link all of xapi in to get it
  2. extending the system.listMethods to allow more introspection (e.g. include type information)
