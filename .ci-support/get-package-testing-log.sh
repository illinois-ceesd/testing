#!/bin/bash

platform_path=${1}
package_pattern=${2}

latest_logfile=$(ls -t ${platform_path}/testing-log*${package_pattern}* | head -1)

if [ "${latest_logfile}" == "" ];
then
    printf "Test did not run.\n"
    exit 1
fi

cat $latest_logfile
