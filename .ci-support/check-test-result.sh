#!/bin/bash

testing_host=${1}
suite_name=${2}
test_name=${3}

data_path=${testing_host}

origin=$(pwd)

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

testing_resultsfile="${data_path}/testing-results-${suite_name}-${testing_host}-${timestamp}.txt"

if [ ! -f "${testing_resultsfile}" ];
then
    printf "Testing results file (${testing_resultsfile}) not found.\n"
    exit 1
fi

printf "Found latest test results file: (${testing_resultsfile}).\n"
printf "Grabbing results for test (${test_name}).\n"

test_result_line=$(grep "^${test_name}:" ${testing_resultsfile})

if [ "${test_result_line}" == "" ];
then
    printf "Result for test (${test_name}) not found.\n"
    exit 1
fi

printf "Testing results line: ($test_result_line).\n"

if [ "${test_result_line}" == "${test_name}: 0" ];
then
    echo "Test passed."
    exit 0
fi

echo "Test failed."
exit 1
