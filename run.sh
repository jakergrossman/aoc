#!/bin/sh

DAYS=$(find . -type d -name 'day*' -printf '%P\n' | sort)
TIME=$(which time)
GCL=$(which gcl)

if [ -z "$GCL" ]; then
    >&2 echo '[ERROR] No GCL installation found'
    exit 1
fi

if [ "$#" -eq 0 ]; then
    >&2 echo '[ERROR] No day selected!'
    >&2 echo '[ERROR] Completed days:'
    echo "$DAYS" | sed 's/^/[ERROR]   /' 1>&2
    exit 1
fi

run_day () {
    echo "--- $1 ---"
    cd "$1"

    if [ ! -z "$TIME" ]; then
        "$TIME" -f "Time: %E" "$GCL" -f run.lsp 2>&1 | tail -n 3
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
            >&2 echo "[ERROR] Invalid day selected ($arg)!"
            >&2 echo "[ERROR] Completed days:"
            echo "$DAYS" | sed 's/\(\w\+\).lsp/[ERROR]   \1/' 1>&2
            exit 1
        fi
    done
fi
