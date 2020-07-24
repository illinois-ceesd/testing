#!/bin/bash

resultsfile=${1}
rm -f Vortex*vtu
python ./vortex.py
result=$?
if [ $result -eq 0 ]; then
    test_result="Vortex Pass"
else
    test_result="Vortex Fail"
fi
if [ -z "${resultsfile}" ]; then
    printf "${test_result}\n"
else
    printf "${test_result}\n" >> $resultsfile
fi
