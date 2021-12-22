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
Usage: ./aoc [run|test|create|help] [-i <interpreter>] <day-specifier> ...

AVAILABLE SUBCOMMANDS
    run     Run the specified day(s).
    test    Test the specified day(s) for correct output.
    create  Create the specified day(s).
    help    Show help (this message).

OPTIONS
    -i      Which interpreter to use (one of "sbcl", "clisp", or "gcl"; defualts to "sbcl").
```

### Examples

```console
$ ./aoc run 1                 # run day one
$ ./aoc run -i clisp day01 2  # run days 1 and 2 with CLISP as the interpreter
$ ./aoc run all               # run every day
$ ./aoc test 1 day02          # test days 1 and 2
$ ./aoc test all              # test all days
```
