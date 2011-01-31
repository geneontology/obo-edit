#!/bin/sh
THIS=`pwd`/`dirname $0`
SPATH=`which d2r-server`
SDIR=`dirname $SPATH`
cd $SDIR && d2r-server $* $THIS/mappings-go-test.n3 
echo "now point yr browser at: http://127.0.0.1:2020/snorql/"
