#!/bin/sh

srcdir=$1
bindir=$2
rm -f ${bindir}/Testing/*.vtu ${bindir}/Testing/*.pvtu 
printf "Done testing!\n"
