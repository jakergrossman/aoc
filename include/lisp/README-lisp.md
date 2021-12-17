# Advent of Code
Solutions to [Advent of Code](https://adventofcode.com/) in LISP.

## Requirements
- Git

One of:
- [SBCL](http://www.sbcl.org/)
- [CLISP](https://clisp.sourceforge.io/)
- [GNU Common LISP](https://www.gnu.org/software/gcl/)

## Running
```
Usage: ./run [-c <new-day>] [-i <interpreter>] [-e <run-command>] <day-specifier> ...
  -h      Show help (this message)
  -i      Which interpreter to use (one of "sbcl", "clisp", or "gcl"; defualts to "sbcl")
  -e      Executable command to run each file with. Overrides -i.
  -c      Create a new day.
  -t      Only display runtimes, not answers
  -a      Only display answers, not runtimes
```

To run and print outputs, invoke `run` with one or more days, specified as `dayx` (with leading zeros, i.e.
`day01`) or just `x`.

Alternatively, use `all` to run all days:

### Examples

```console
$ ./run 1               # run day one with default options
$ ./run -i clisp day02  # run day 2 with the CLISP interpreter
$ ./run -t day01 02 3   # run days 1, 2, and 3, printing only times
```