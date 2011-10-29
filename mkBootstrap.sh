#!/bin/bash

if [ `command -v preml` ] && [ `command -v premlton` ]; then
    OUTPUT=/tmp/preml-output

    rm -rf bootstrap
    cp -a src bootstrap 2> /dev/null

    echo ""
    echo "Running PreML"
    preml bootstrap/PreML.mlb

    echo ""
    echo "Renaming files"
    for FILE in bootstrap/*; do
        if [ `echo $FILE | grep ".preml."` ]; then
            echo $FILE
            mv $FILE `echo $FILE | sed s/\.preml//`
        fi
    done

    echo ""
    echo "Stripping '%pre' from MLB-files"
    cp src/*.mlb bootstrap 2> /dev/null
    TMP=/tmp/preml-buffer
    for FILE in bootstrap/*.mlb; do
        echo $FILE
        sed s/%pre//g $FILE > $TMP
        mv $TMP $FILE
    done
else
    echo "PreML and MLTon needs to be installed.  Aborting."
fi