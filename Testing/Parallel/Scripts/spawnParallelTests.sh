#!/bin/sh

SpawnParallel = ${1}
TestList      = ${2}
OutFileName   = ${3}

for testname in $(cat ${TestList})
do
    if [ ! -z ${4} ]; then printf "Running test (${testname})...\n"; fi
    ${SpawnParallel} python ${testname}.py ${OutFileName}
done    
