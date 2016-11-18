<!--- OASIS_START --->
<!--- DO NOT EDIT (digest: fa32be719c6a7821d017064998dcc337) --->

utf8val - utf8val
=================

See the file [INSTALL.md](INSTALL.md) for building and installation
instructions.

Copyright and license
---------------------

utf8val is distributed under the terms of the Berkeley software distribution
license (3 clauses).

<!--- OASIS_STOP --->


Utf8val
=======

UTF-8 validation function written in OCaml.
It does not require any runtime table or library.

Installation
------------

Requires OCaml, Make, ocamlfind (Findlib), oasis and ocamlbuild.

```
$ ./prebuild.sh
$ oasis setup
$ ./configure
$ make doc
$ make install
```

Uninstall with:

```
$ make uninstall
```
