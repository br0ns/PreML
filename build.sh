#!/bin/bash

SML=mlton

if [ ! `command -v preml` ]; then
    echo "PreML is not installed.  Bootstrapping..."
    $SML -output bin/preml bootstrap/PreML.mlb
    echo "Done!"
fi

bin/preml src/PreML.mlb
$SML -output bin/preml src/PreML.preml.mlb
bin/preml src/PreML.mlb -c
