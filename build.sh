#!/bin/bash

SML=mlton

if [ -f /usr/bin/preml ] && [ -f /usr/bin/premlton ]; then
    premlton PreML.mlb
else
    echo "PreML is not installed.  Bootstrapping..."
    echo ""
    $SML -output preml bootstrap/PreML.mlb
    PATH=.:$PATH ./premlton PreML.mlb
    echo ""
    echo "You are now ready for 'sudo make install'."
fi
mv PreML preml