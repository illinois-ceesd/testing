#!/bin/bash

resultsfile=${1}
rm -f wave*vtu
python ./wave-eager.py
result=$?
if [ $result -eq 0 ]; then
    test_result="Wave Pass"
else
    test_result="Wave Fail"
fi
if [ -z "${resultsfile}" ]; then
    printf "${test_result}\n"
else
    printf "${test_result}\n" >> $resultsfile
fi
