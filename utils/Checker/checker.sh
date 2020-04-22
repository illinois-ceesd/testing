#!/bin/bash

# printf "CALLED with ${1} and ${2}\n"

for filename in $(find ${1} -name "*.py")
do
    if [ ! -z ${2} ]; then printf "Checking ${filename}...\n"; fi
    flake8 ${filename}
done    



