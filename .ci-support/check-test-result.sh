#!/bin/bash

# Usage: check-test-result.sh <data_path> <suite_name> <test_name>
# 
# This script checks for a testing result by looking in the
# <data_path> directory for the following files:
# (1) <data_path>/testing-latest-timestamp.txt => <timestamp>
# (2) <data_path>/testing-log-<suite_name>-<testing_host>-<timestamp>.txt
# (3) <data_path>/testing-results-<suite_name>-<testing_host>-<timestamp>.txt
#
# CI is the main user of this script.  The call to it can be configured in
# > testing/.github/workflows/ci.yaml
# CI examines the return code of this script to indicate whether the test
# passes (0) or fails (non-zero).
#
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

# The timestamp for the latest tests is recorded in this file.
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

# The testing (pass=0, fail!=0) results for the whole suite
# are in this file. In the form "test-name: 0". 
testing_resultsfile="${data_path}/testing-results-${suite_name}-${testing_host}-${timestamp}.txt"

if [ ! -f "${testing_resultsfile}" ];
then
    printf "Testing results file (${testing_resultsfile}) not found.\n"
    exit 1
fi

# The extra prints improve transparency at the CI level
printf "Found latest test results file: (${testing_resultsfile}).\n"
printf "Grabbing results for test (${test_name}).\n"
printf "The expected result is, in general: 'test-name: 0'\n"
printf "In this case, ('test-name') == (${test_name}).\n"
printf "Anything other than the expected result is a fail.\n"
test_result_line=$(grep "^${test_name}:" ${testing_resultsfile})

if [ "${test_result_line}" == "" ];
then
    printf "Result for test (${test_name}) not found.\n"
    exit 1
fi

printf "Found testing results line: ($test_result_line).\n"

if [ "${test_result_line}" == "${test_name}: 0" ];
then
    echo "Test passed."
    exit 0
fi

echo "Test failed."
exit 1
