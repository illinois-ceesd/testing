#!/bin/bash

# Usage: get-package-testing-log.sh <data_path> <suite_name>

# This script checks for and spews a testing output
# (i.e., any stdout/err created by the package as it is tested),
# and looks in the <data_path> directory for the following files:
# (1) <data_path>/testing-latest-timestamp.txt => <timestamp>
# (2) <data_path>/testing-log-<suite_name>-<testing_host>-<timestamp>.txt
#
# CI is the main user of this script.  The call to it can be configured in
# > testing/.github/workflows/ci.yaml
# CI examines the return code of this script to indicate whether the test
# passes (0) or fails (non-zero).
#
data_path=${1}
suite_name=${2}
testing_host=${data_path}

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

testing_logfile="${data_path}/testing-log-${suite_name}-${testing_host}-${timestamp}.txt"

printf "The expected filename is: (<data_path>/testing-log-<suite_name>-<testing_host>-<timestamp>.txt)\n"
printf "In this case: (${testing_logfile})\n"
printf "If the file does not exist, then this phase of the test fails.\n"

if [ ! -f "${testing_logfile}" ];
then
    printf "Testing output log ($testing_logfile) not found.\n"
    exit 1
fi

printf "Found testing output log: ($testing_logfile).\n"
# Just spew the contents of the file to the CI output
cat $testing_logfile
