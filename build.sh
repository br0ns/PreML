#!/bin/bash

SML=mlton

if [ `command -v preml` ] && [ `command -v premlton` ]; then
    premlton src/PreML.mlb
else
    echo "PreML is not installed.  Bootstrapping..."
    echo ""
    $SML -output bin/preml bootstrap/PreML.mlb
    PATH="`pwd`/bin":$PATH premlton src/PreML.mlb
fi
mv src/PreML bin/preml