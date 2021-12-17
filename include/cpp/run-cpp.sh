#!/bin/sh

set -e

COMMON_DIR="$(dirname $0)"

CC=g++
CXXFLAGS="-Wall -Wextra -pedantic -ggdb --std=c++11"
INCLUDES="-I$COMMON_DIR"

usage () { echo "Usage: ./run [-Bc] [-n <new-day>] <day-specifier> ... "; }
error () { >&2 echo "[ERROR] $1"; }
help () {
    usage
    echo '  -h      Show help (this message)'
    echo '  -n      Create a new day.'
    echo '  -c      Clean build artifacts.'
    echo '  -B      Always compile each day before running.'
    exit 
}
create () {
    if [ -d "$1" ]; then
        error "Target '$1' already exists!"
        error "$(usage)"
        exit 1
    fi

    mkdir "$1"
    touch "$1"/input.txt
    cp "$COMMON_DIR/template.cpp" "$1"/main.cpp
}

while getopts "hn:Bc" o; do
    case "${o}" in
        n)
            create ${OPTARG}
            exit 0
            ;;
        h)
            help
            ;;
        B)
            COMPILE_ALWAYS="true"
            ;;
        c)
            echo "Cleaning build artifacts..."
            set -x

            rm -rf build

            { set +x; } 2>/dev/null
            exit 0
            ;;
        *)
            usage
            exit 1
            ;;
    esac
done
shift $((OPTIND-1))

DAYS=$(find . -type d -name 'day*' | sed 's/^\.\///' | sort --version-sort)
TIME=$(which time)

if [ "$#" -eq 0 ]; then
    error "$(usage)"
    error 'No day selected!'
    error 'Completed days:'
    for day in $DAYS; do
        error "  $day"
    done
    exit 1
fi

run_day () {
    echo "--- $1 ---"

    mkdir -p ./build

    if [ ! -z "$COMPILE_ALWAYS" ] || [ ! -f ./build/"$1".out ] || [ ./"$1"/main.cpp -nt ./build/"$1".out ]; then
        # compile
        echo "Compiling..."

        set -x

        "$CC" $CXXFLAGS $INCLUDES -o ./build/"$1".out ./"$1"/main.cpp "$COMMON_DIR"/common.cpp

        { set +x; } 2>/dev/null
    fi

    cd "$1"
    ../build/"$1".out
    cd ..
}

# run all days
if [ "$1" = "all" ]; then
    for day in $DAYS; do
        run_day "$day"
    done
    exit 0
else
    for arg in $@; do
        case $arg in
            ''|*[!0-9]*)
                # long form, i.e. day01 day02 day03
                EFFECTIVE_DAY="$arg"
                ;;
            *)
                # short form, i.e 1, 02, 3
                EFFECTIVE_DAY="day$(printf "%02d" $arg)"
                ;;
        esac

        if ! echo "$DAYS" | grep -q "$EFFECTIVE_DAY"; then
            error "$(usage)"
            error "Invalid day selected ($arg)!"
            error 'Completed days:'
            for day in $DAYS; do
                error "  $day"
            done
            exit 1
        fi

        run_day "$EFFECTIVE_DAY"
    done
fi
