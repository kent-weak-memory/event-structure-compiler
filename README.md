Event Structure Compiler
========================

Turns programs into relational representations of event structures.

## License

This program is licensed under the GPLv3 license. Some portions of this software
are derived from [Scott Owens' example
compiler](https://github.com/SOwens/example-compiler), notably the tokeniser and
parser. See COPYING for the full license text.

## Language support

This supports an abstraction of C like programming languages. It currently does
not support loops. Loop support will be implemented by loop unrolling soon.

### Input
Appoximate grammar:

```
<block> ::= { <stmts>* }
<stmts> ::= l = l
         |  l = v
         |  l = <expr>
         |  if(<condition) <block> else <block>
<expr>  ::= (<expr> * <expr>)
         |  (<expr> / <expr>)
         |  (<expr> + <expr>)
         |  (<expr> - <expr>)
         |  v
         |  l
<cond>  ::= <expr> > <expr>
         |  <expr> < <expr>
         |  <expr> == <expr>
         |  <expr> != <expr>
         |  (<cond> && <cond>)
         
<program> ::= <block>
           |  PAR_LIST <program> <program>
```

### Output

| Extension   |  Format  |
|:-----------:|:---------|
| `.es`       | Relations for QBF |
| `.dot`      | Graphviz |
| `.tex`      | Tikz     |
| `.thy`      | Isabelle Theory |
| `.als`      | Alloy    |


## Usage

`./compile.native <input file.jef> [output] [options]`

Defaults to spitting out `.es` format onto standard out.

### Options

| Flag               | Function              |
|:-------------------|:----------------------|
| `--print-tokens`   | Print out a list of tokens before the program is parsed. |
| `--alloy-path`     | Specify the path to the alloy model files for alloy output |
| `--values` \| `-V` | Set the max value (V) such that the modeled. Values = {v | v ≤ V} |
| `--long-names`     | Print long names in output, e.g. `c_Rx1_r2` |

## Building

As with Scott's compiler from which this is derived, you'll need a handful of
dependencies:

```
opam install extlib ppx_deriving ppx_monadic ocamlgraph
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

### Location Translation
- [x] Mostly done

Swaps all locations for the AST for virtual memory or register locations. This
is just a simple regex, variables which begin "r" such as `r12` will be treated
as a register, any other variable will be a memory location. E.g. `x`, `y`,
`foo`.

### Event Structure IR
- [x] Mostly done

This is Alan Jeffrey style notation for event structures as presented in his
LICS'16 paper *Towards an Event Structure Memory Model of Relaxed Memory*.

### Event Structure
- [x] Mostly done

A relational realisation of an actual event structure. Transformations from this
to the various output formats will be very simplistic, as each of the output
formats is intended to be relational.

### Output stages

This should provide various useful outputs built from the relational
representations.

#### Isabelle
- [x] Done

To go into Jon's work on a formal Isabelle definition of event structures.

#### Alloy
- [ ] Nearly there

To go into my work on modelling an event structure memory model in Alloy.

#### DOT
- [x] Done

Output for display of an event structure.
