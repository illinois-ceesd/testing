#!/bin/bash

platform_path=${1}
test_pattern=${3}

latest_logfile=$(ls -t ${platform_path}/testing-log* | head -1)
latest_resultsfile=$(ls -t ${platform_path}/testing-results* | head -1)

if [ "${latest_logfile}" == "" ];
then
    printf "Test did not run.\n"
    exit 1
fi

cat $latest_logfile
return_code=$(grep ${test_pattern} ${latest_resultsfile} | cut -d ":" -f 2)

exit $(($return_code+0))
