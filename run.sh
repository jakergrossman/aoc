#!/bin/sh

FILES=$(find . -name 'day*.lsp' -printf '%P\n' | sort)

if [ "$#" -eq 0 ]; then
    >&2 echo '[ERROR] No day selected!'
    >&2 echo '[ERROR] Completed days:'
    echo "$FILES" | sed 's/\(\w\+\).lsp/[ERROR]   \1/' 1>&2
    exit 1
fi

run_file () {
    echo "--- $1 ---"
    gcl -eval < "$1" | tail -n 5 # only care about answers
    echo
}

# run all files
if [ "$1" = "all" ]; then
    for file in $FILES; do
        run_file "$file"
    done
    exit 0
else
    for arg in $@; do
        if echo "$FILES" | grep -q "day$arg.lsp"; then
            run_file "day$arg.lsp"
        elif echo "$FILES" | grep -q "$arg.lsp"; then
            run_file "$arg.lsp"
        else
            >&2 echo "[ERROR] Invalid day selected ($arg)!"
            >&2 echo "[ERROR] Completed days:"
            echo "$FILES" | sed 's/\(\w\+\).lsp/[ERROR]   \1/' 1>&2
            exit 1
        fi
    done
fi
