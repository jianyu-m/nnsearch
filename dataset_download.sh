#!/bin/bash

if [ ! -f "loc-gowalla_totalCheckins.txt" ]; then

    if [ ! -f "loc-gowalla_totalCheckins.txt.gz" ];then
        wget http://snap.stanford.edu/data/loc-gowalla_totalCheckins.txt.gz
    fi

    gunzip loc-gowalla_totalCheckins.txt.gz

fi
