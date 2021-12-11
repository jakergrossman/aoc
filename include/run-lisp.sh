#!/bin/sh

set -e

usage () { echo "Usage: ./run [-i <interpreter>] [-e <run-command>] <day-specifier> ..."; }
error () { >&2 echo "[ERROR] $1"; }
help () {
    usage
    echo '  -h      Show help (this message)'
    echo '  -i      Which interpreter to use (one of "gcl", "clisp", or "sbcl"; defualts to "gcl")'
    echo '  -e      Executable command to run each file with. Overrides -i.'
    echo '  -t      Only display runtimes, not answers'
    echo '  -a      Only display answers, not runtimes'
    exit 0
}

INTERP="gcl"

while getopts "hi:e:ta" o; do
    case "${o}" in
        i)
            INTERP=${OPTARG}
            ;;
        e)
            EXEC=${OPTARG}
            ;;
        h)
            help
            ;;
        t)
            TIME_ONLY="true"
            ;;
        a)
            ANSWERS_ONLY="true"
            ;;
        *)
            usage
            exit 1
            ;;
    esac
done
shift $((OPTIND-1))

if [ ! -z $TIME_ONLY ] && [ ! -z $ANSWERS_ONLY ]; then
    error "Cannot use both -t (time only) and -a (answers only)"
    error "$(usage)"
    exit 1
fi

DAYS=$(find . -type d -name 'day*' | sed 's/^\.\///' | sort --version-sort)
TIME=$(which time)

if [ -z "$EXEC" ]; then
    # try to find executable
    case $INTERP in
        clisp | CLISP)
            EXEC=$(which clisp)
            ;;
        gcl | GCL)
            EXEC="$(which gcl)"
            if [ ! -z EXEC ]; then EXEC="$EXEC -f"; fi
            ;;
        sbcl | SBCL)
            EXEC="$(which sbcl)"
            if [ ! -z EXEC ]; then EXEC="$EXEC --script"; fi
            ;;
        *)
            error "Invalid interpreter '$INTERP'"
            error "$(usage)"
            exit 1
            ;;
    esac
    if [ -z "$EXEC" ]; then
        # GCL not found
        error "No '$INTERP' installation specified and no installation could be found"
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

    # echo $EXEC

    if [ ! -z "$TIME" ]; then
        # save runtime & answer
        OUTPUT=$("$TIME" -p $EXEC run.lsp 2>&1 | tail -n 5)

        # output answers
        if [ -z $TIME_ONLY ]; then
            echo "$OUTPUT" | head -n 2
        fi

        # output time
        if [ -z $ANSWERS_ONLY ]; then
            echo "Time: $(echo "$OUTPUT" | tail -n 4 | grep ^real | awk '{ print $2 }')s"
        fi
    else
        if [ ! -z $TIME_ONLY ]; then
            error "-t was specified by could not find a 'time' executable"
            error "$(usage)"
            exit 1
        fi
        $EXEC run.lsp 2>&1 | tail -n 2
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
