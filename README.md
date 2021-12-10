# Advent of Code 2021
Solutions to [Advent of Code 2021](https://adventofcode.com/2021) in LISP.

## Requirements
[GNU Common LISP](https://www.gnu.org/software/gcl/)

## Running
```
Usage: ./run.sh [-i <interpreter>] [-e <run-command>] <day-specifier> ...
  -h      Show help (this message)
  -i      Which interpreter to use (one of "clisp" or "gcl", defaults to "gcl")
  -e      Executable command to run each file with. Overrides -i.
  -t      Only display runtimes, not answers
  -a      Only display answers, not runtimes
```

To run and print outputs, invoke `run.sh` with one or more days, specified as `dayx` (with leading zeros, i.e.
`day01`) or just `x`.

Alternatively, use `all` to run all days:

### Examples

```console
$ ./run.sh 1               # run day one with default options
$ ./run.sh -i clisp day02  # run day 2 with the CLISP interpreter
$ ./run.sh -t day01 02 3   # run days 1, 2, and 3, printing only times
```
