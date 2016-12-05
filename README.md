Event Structure Compiler
========================

Turns programs into relational representations of event structures.

## License

This program is licensed under the GPLv3 license. Some portions of this software
are derived from [Scott Owens' example
compiler](https://github.com/SOwens/example-compiler), notably the tokeniser and
parser. See COPYING for the full license text.

## Building

As with Scott's compiler from which this is derived, you'll need a handful of
dependencies:

```
opam install extlib ppx_deriving
```

Then all going well, you can run `make` to build the binaries.
