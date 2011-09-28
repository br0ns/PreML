#!/bin/bash

if [ -f /usr/bin/preml ] && [ -f /usr/bin/premlton ]; then
    OUTPUT=/tmp/preml-output

    rm -rf bootstrap/*
    cp *.sml bootstrap 2> /dev/null
    cp *.sig bootstrap 2> /dev/null
    cp *.fun bootstrap 2> /dev/null
    cp *.mlb bootstrap 2> /dev/null

    echo "Running PreML"
    preml PreML.mlb | tee $OUTPUT

    echo ""
    echo "(re)Moving processed files"
    while read -r LINE; do
        DST=`echo $LINE | sed "s/ ->.*//"` ;
        SRC=`echo $LINE | sed "s/.*-> //"` ;
        EXT="${DST##*.}"
        if [[ "$EXT" != "mlb" ]]; then
            mv $SRC bootstrap/$DST
            echo mv $SRC bootstrap/$DST
        else
            rm $SRC
            echo rm $SRC
        fi
    done < $OUTPUT

    rm $OUTPUT

    echo ""
    echo "Stripping '%pre' from MLB-files"
    TMP=/tmp/preml-buffer
    for FILE in bootstrap/*.mlb; do
        echo $FILE
        sed s/%pre//g $FILE > $TMP
        mv $TMP $FILE
    done
else
    echo "PreML and MLTon needs to be installed.  Aborting."
fi