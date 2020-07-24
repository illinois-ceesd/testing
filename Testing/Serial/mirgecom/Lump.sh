#!/bin/bash

resultsfile=${1}
rm -f Lump*vtu
python ./lump2.py
result=$?
if [ $result -eq 0 ]; then
    test_result="Lump Pass"
else
    test_result="Lump Fail"
fi
if [ -z "${resultsfile}" ]; then
    printf "${test_result}\n"
else
    printf "${test_result}\n" >> $resultsfile
fi
