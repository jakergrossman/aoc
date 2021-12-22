#!/bin/sh

set -e

COMMON_DIR="$(dirname $0)"
INTERP="sbcl"
DAYS=$(find . -type d -name 'day*' | sed 's/^\.\///' | sort --version-sort)
TIME=$(which time)

usage () { echo "Usage: ./aoc [run|test|create|help] [-i <interpreter>] <day-specifier> ..."; }

error () { >&2 echo "[ERROR] $1"; }
help () {
    usage
    echo
    echo 'AVAILABLE SUBCOMMANDS'
    echo '    run     Run the specified day(s).'
    echo '    test    Test the specified day(s) for correct output.'
    echo '    create  Create the specified day(s).'
    echo '    help    Show help (this message).'
    echo
    echo 'OPTIONS'
    echo '    -i      Which interpreter to use (one of "sbcl", "clisp", or "gcl"; defualts to "sbcl").'
    exit 0
}

resolve_day () {
    case $1 in
        ''|*[!0-9]*)
            # long form, i.e. day01 day02 day03
            EFFECTIVE_DAY="$1"
            ;;
        *)
            # short form, i.e 1, 02, 3
            EFFECTIVE_DAY="day$(printf "%02d" $1)"
            ;;
    esac
    echo $EFFECTIVE_DAY
}

resolve_exec () {
    case $1 in
        sbcl | SBCL)
            echo "sbcl --script"
            ;;
        clisp | CLISP)
            echo "clisp -C"
            ;;
        gcl | GCL)
            echo "gcl -f"
            ;;
    esac
}

day_output () {
    echo "--- $1 ---"
    cd "$1"

    EXEC=$(resolve_exec $INTERP)

    OUTPUT=$("$TIME" -p $EXEC run.lsp 2>&1 | tail -n 5)

    # output answers
    echo "$OUTPUT" | head -n 2

    # output time
    echo "Time: $(echo "$OUTPUT" | tail -n 4 | grep ^real | awk '{ print $2 }')s"

    cd ..
}

create () {
    for day in $@; do
        day=$(resolve_day $day)
        if [ -d $day ]; then
            error "Target '$day' already exists!"
            while true; do
                case $yn in
                    [Yy]* ) unset yn; break;;
                    [Nn]* ) exit 1;;
                    * ) read -p "Do you want to continue? [Y/n]: " yn
                esac
            done
        else
            mkdir "$day"
            touch "$day"/input.txt
            cp "$COMMON_DIR/template.lsp" "$day"/run.lsp
        fi
    done

}

run_days () {
    if [ $# -eq 0 ]; then
        error "$(usage)"
        error 'No day selected!'
        error 'Completed days:'
        for day in $DAYS; do
            error "  $day"
        done
        exit 1
    elif [ $1 = "all" ]; then
        days="$DAYS"
    else
        days="$@"
    fi

    for day in $days; do
        day=$(resolve_day $day)
        day_output $day
    done
}

run_test () {
    if [ $1 = "all" ]; then
        test_days="$DAYS"
    else
        test_days="$@"
    fi

    for day in $test_days; do
        day=$(resolve_day $day)
        echo "[INFO] Testing $day"
        if [ ! -f tests/$day.txt ]; then
            error "No test for $day"
        else
            expected=`cat tests/$day.txt`
            for interp in sbcl clisp gcl; do
                if [ ! -z "$(which $interp)" ]; then
                    INTERP=$(printf "%-5s" $interp)
                    output=$(day_output $day)
                    results=$(echo "$output" | tail -n +2 | head -n -1)
                    time=$(echo "$output" | tail -n 1)

                    if [ "$expected" = "$results" ]; then
                        echo "  [PASS | $INTERP] $day ($time)"
                    else
                        echo "  [FAIL | $INTERP] $day"
                        echo "************************"
                        echo "  EXPECTED:"
                        echo "$expected"
                        echo "  ACTUAL:"
                        echo "$results"
                        echo "************************"
                    fi
                fi
            done
        fi
    done
}

if [ ! -z $1 ]; then
    subcommand=$1
    shift 1
fi

while getopts "hi:e:tac:" o; do
    case "${o}" in
        i)
            INTERP=${OPTARG}
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

case $subcommand in
    run)
        run_days $@
        exit 0
        ;;
    test)
        run_test $@
        exit 0
        ;;
    create)
        create $@
        exit 0
        ;;
    help)
        help
        exit 0
        ;;
    *)
        if [ -z $subcommand ]; then
            error "No subcommand specified"
        else
            error "Unknown subcommand '$subcommand'"
        fi
        error "$(usage)"
        exit 1
        ;;
esac
