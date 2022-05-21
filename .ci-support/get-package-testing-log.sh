#!/bin/bash

data_path=${1}
suite_name=${2}
testing_host=${data_path}

if [ ! -d ${data_path} ];
then
    printf "Data path (${data_path}) does not exist.\n"
    exit 1
fi

timestamp_filename="${data_path}/testing-latest-timestamp.txt"

if [ ! -f "${timestamp_filename}" ];
then
    printf "Testing timestamp file (${timestamp_filename}) not found.\n"
    exit 1
fi

timestamp=$(cat ${timestamp_filename})

if [ "${timestamp}" == "" ];
then
    echo "Testing timestamp is missing."
    exit 1
fi

printf "Found testing timestamp: ${timestamp}\n"

testing_logfile="${data_path}/testing-log-${suite_name}-${testing_host}-${timestamp}.txt"

if [ ! -f "${testing_logfile}" ];
then
    printf "Testing output log ($testing_logfile) not found.\n"
    exit 1
fi

printf "Found testing output log: ($testing_logfile).\n"
cat $testing_logfile
