#!/bin/sh

usage () { echo "Usage: $0 [-g <gcl path>] <day-specifier> ..."; }
error () { >&2 echo "[ERROR] $1"; }
help () {
    usage
    echo '  -g      Path to GCL executable'
    echo '  -h      Show help (this message)'
    exit 0
}

while getopts ":hg:" o; do
    case "${o}" in
        g)
            GCL=${OPTARG}
            ;;
        h)
            help
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

if [ -z "$GCL" ]; then
    # try to find GCL
    GCL=$(which gcl)
    if [ -z "$GCL" ]; then
        # GCL not found
        error 'No GCL installation specified and no installation could be found'
        error "$(usage)"
        exit 1
    fi
fi

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
    cd "$1"

    if [ ! -z "$TIME" ]; then
        OUTPUT=$("$TIME" -p "$GCL" -f run.lsp 2>&1 | tail -n 5)
        echo "$OUTPUT" | head -n 2 # output answers
        echo "Time: $(echo "$OUTPUT" | tail -n 4 | grep ^real | awk '{ print $2 }')s"
    else
        "$GCL" -f run.lsp 2>&1 | tail -n 2
    fi
    cd ..
    echo
}

# run all days
if [ "$1" = "all" ]; then
    for day in $DAYS; do
        run_day "$day"
    done
    exit 0
else
    for arg in $@; do
        if echo "$DAYS" | grep -q "day$arg"; then
            run_day "day$arg"
        elif echo "$DAYS" | grep -q "$arg"; then
            run_day "$arg"
        else
            error "$(usage)"
            error "Invalid day selected ($arg)!"
            error 'Completed days:'
            for day in $DAYS; do
                error "  $day"
            done
            exit 1
        fi
    done
fi
