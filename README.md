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

## Design

This is a fairly boring compiler:

### Source input language
- [x] Mostly done

A C-like language with some aditional concurrency notation and a limited set of
control flow structures. Should be enough to express litmus tests.

### Tokeniser
- [x] Mostly done

Converts the input language into a list of tokens for the parser.

### Parser
- [x] Mostly done

Converts the token list formed from the input language into an AST.

### Location Transltion
- [x] Mostly done

Swaps all locations for the AST for virtual memory or register locations. This
is just a simple regex, variables which begin "r" such as `r12` will be treated
as a register, any other variable will be a memory location. E.g. `x`, `y`,
`foo`.

### Event Structure IR
- [ ] Work in progress

This is Alan Jeffrey style notation for event structures as presented in his
LICS'16 paper *Towards an Event Structure Memory Model of Relaxed Memory*.

### Event Structure
- [ ] Work in progress

A relational realisation of an actual event structure.

### Output stages

This should provide various useful outputs built from the relational
representations.

#### Isabelle
- [ ] Work in progress

To go into Jon's work on a formal Isabelle definition of event structures.

#### Alloy
- [ ] Work in progress

To go into my work on modelling an event structure memory model in Alloy.

#### DOT
- [ ] Work in progress

Output for display of an event structure.

#### Adga
To go into Alan Jeffrey's orginal event structure memory model artifact.
