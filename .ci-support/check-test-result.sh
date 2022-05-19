#!/bin/bash

platform_path=${1}
test_pattern=${2}

latest_resultsfile=$(ls -t ${platform_path}/testing-results*${test_pattern}* | head -1)

if [ "${latest_resultsfile}" == "" ];
then
    printf "Test did not run.\n"
    exit 1
fi

return_code=$(grep ${test_pattern} ${latest_resultsfile} | cut -d ":" -f 2)

if [ "${return_code}" == "" ];
then
    return_code="1"
fi

exit $(($return_code+0))
