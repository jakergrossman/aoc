# Advent of Code
Solutions to [Advent of Code](https://adventofcode.com/) in C++.

## Requirements
- Git
- [g++](https://gcc.gnu.org/)

## Running
```
Usage: ./run [-Bc] [-n <new-day>] <day-specifier> ...
  -h      Show help (this message)'
  -n      Create a new day.'
  -c      Clean build artifacts.'
  -B      Always compile each day before running.'
```

To compile, run, and print, invoke `run` with one or more days, specified as `dayx` (with leading zeros, i.e.
`day01`) or just `x`.

Alternatively, use `all` to run all days:

### Examples

```console
$ ./run 1         # run day 1, compiling if necessary
$ ./run -c        # delete build output
$ ./run -B 1 2 3  # run days 1, 2, and 3, compiling each beforehand
```
