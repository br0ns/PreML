#!/bin/bash

SML=mlton

if [ -f /usr/local/bin/preml ] && [ -f /usr/local/bin/premlton ]; then
    premlton src/PreML.mlb
else
    echo "PreML is not installed.  Bootstrapping..."
    echo ""
    $SML -output preml bootstrap/PreML.mlb
    PATH=.:$PATH ./premlton src/PreML.mlb
    echo ""
    echo "You are now ready for 'sudo make install'."
fi
mv src/PreML preml