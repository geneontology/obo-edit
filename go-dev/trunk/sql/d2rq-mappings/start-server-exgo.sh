#!/bin/sh
THIS=`pwd`/`dirname $0`
SPATH=`which d2r-server`
SDIR=`dirname $SPATH`
cd $SDIR && d2r-server -p 2021 -b http://spade.lbl.gov:2021 $* $THIS/mappings-exgo.n3 
echo "now point yr browser at: http://127.0.0.1:2020/snorql/"
