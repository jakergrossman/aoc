# Advent of Code
Solutions to [Advent of Code](https://adventofcode.com/) in LISP.

## Requirements
- Git

One of:
- [SBCL](http://www.sbcl.org/)
- [CLISP](https://clisp.sourceforge.io/)
- [GNU Common LISP](https://www.gnu.org/software/gcl/)

## Running with the run script

```
Usage: ./aoc [run|create] <day-specifier> ...
Available options:
  -h, --help               Print a brief help summary (this message).
  -i, --interp INTERPRETER Select the Lisp interpreter to use. [Default: sbcl]
```

### Examples

```console
$ ./aoc run 1                 # run day one
$ ./aoc -i clisp run day01 2  # run days 1 and 2 with CLISP as the interpreter
$ ./aoc run all               # run every day
```

The run script is currently only supported on SBCL, but where possible the solutions to each day are
compatible with SBCL, CLISP, and GCL. They can be invoked manually by navigating inside the
day to run and executing the appropriate command:

- `sbcl --script run.lsp`
- `clisp run.lsp`
- `gcl -f run.lsp`