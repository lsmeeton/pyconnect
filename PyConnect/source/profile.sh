#!/bin/bash

OUTPUT=$1

if [ "x$OUTPUT" == "x" ]; then
    echo "Need to provide output file name"
    exit
fi
python -m cProfile -o $OUTPUT ../../source/disconnectDPS.py > /dev/null

$HOME/.local/bin/gprof2dot -f pstats $OUTPUT | dot -Tpdf -o $OUTPUT.pdf