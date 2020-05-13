#!/bin/sh

streamout=${2}
streambin=${1}

if [ "${streamout}" == "" ] || [ "${streambin}" == "" ] ; then
    printf "Usage: ${0} <stream_binary> <output_file>\n"
    exit 1
fi

printf "runstream: Running stream@(${streambin}) --> file@(${streamout})\n" 

if [ -e ${streamout} ]; then rm -f ${streamout}; fi

${streambin} >& ${streamout}
