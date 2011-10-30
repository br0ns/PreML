#!/bin/bash

SML=mlton
PREML=preml

if [ ! `command -v preml` ]; then
    echo "PreML is not installed.  Bootstrapping..."
    $SML -output bin/preml bootstrap/PreML.mlb
    echo "Done!"
    PREML=bin/preml
fi

$PREML src/PreML.mlb
$SML -output bin/preml src/PreML.preml.mlb
$PREML src/PreML.mlb -c
