#!/bin/bash

streamsrc=${1}
arraysize=${2}
streambin=${3}

if [ "${streamsrc}" == "" ] || [ "${arraysize}" == "" ] || [ "${streambin}" == "" ] ; then
    printf "Unrecognized command: ${0} ${1} ${2} ${3}\n" 
    printf "Usage: ${0} <stream_source> <array_size> <stream_binary>\n"
    exit 1
fi

printf "makestream: Building stream_src@(${streamsrc}) --> stream_bin@(${streambin})\n" 

${CC} -fopenmp -O3 ${streamsrc} -DSTREAM_ARRAY_SIZE=${arraysize} -o ${streambin}
