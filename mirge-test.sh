#!/bin/bash

set -e
set -x
set -o pipefail

testing_configfile=${1}
date
timestamp=$(date "+%Y.%m.%d-%H.%M.%S")
TIME_SINCE_EPOCH=$(date +%s)
TESTING_RUN_HOME=$(pwd)
testing_configfile="${TESTING_RUN_HOME}/${testing_configfile}"

TESTING_RUN_HOSTNAME=$(hostname)
if [[ "$TESTING_RUN_HOSTNAME" == "lassen"* ]]; then
    TESTING_RUN_HOST="lassen"
else
    TESTING_RUN_HOST="$TESTING_RUN_HOSTNAME"
fi
TESTING_RUN_DATE=$(date "+%Y-%m-%d %H:%M")
TESTING_RUN_PLATFORM=$(uname)
TESTING_RUN_ARCH=$(uname -m)

# Individual developers should set this to their
# own pkg-specific or fork-specific repo.
# CEESD nightly testing writes here:
TESTING_RESULTS_REPO="illinois-ceesd/testing.git"
TESTING_RESULTS_BRANCH="add-isolator-test"
TESTING_ENV_NAME="ceesd.testing.env"
MIRGE_PKG_FORK="illinois-ceesd"
MIRGE_PKG_REPO="${MIRGE_PKG_FORK}/mirgecom"
MIRGE_PKG_BRANCH="production"

# -- Install conda env, dependencies and MIRGE-Com via *emirge*
# --- remove old run if it exists
if [ -f "INSTALL_MIRGECOM" ] || [ ! -d "emirge" ];
then
    if [ -d "emirge" ];
    then
        echo "Removing old installation."
        mv -f emirge emirge.old
        rm -rf emirge.old &
    fi

    # --- grab emirge and install MIRGE-Com 
    git clone git@github.com:/illinois-ceesd/emirge.git
    cd emirge
    ./install.sh --fork=${MIRGE_PKG_FORK} --branch=${MIRGE_PKG_BRANCH} --env-name=${TESTING_ENV_NAME}
    cd ../
    rm -f INSTALL_MIRGECOM
fi

# -- Activate the env we just created above
export EMIRGE_HOME="${TESTING_RUN_HOME}/emirge"
source ${EMIRGE_HOME}/config/activate_env.sh

cd emirge/mirgecom
git pull

# For each "driver package" to test
while read -r line
do
    cd ${TESTING_RUN_HOME}/emirge/mirgecom

    printf "Testing config: ${line}\n"
    TESTING_PKG_NAME=$(printf "${line}" | cut -d "|" -f 1)
    TESTING_PKG_REPO=$(printf "${line}" | cut -d "|" -f 2)
    TESTING_PKG_BRANCH=$(printf "${line}" | cut -d "|" -f 3)

    TESTING_LOGFILE_NAME="${TESTING_RUN_HOME}/testing-log-${TESTING_PKG_NAME}-${TESTING_RUN_HOST}-${timestamp}.txt"
    TESTING_RESULTSFILE_NAME="${TESTING_RUN_HOME}/testing-results-${TESTING_PKG_NAME}-${TESTING_RUN_HOST}-${timestamp}.txt"

    rm -f ${TESTING_LOGFILE_NAME} ${TESTING_RESULTSFILE_NAME}

    printf "# ----- Automated testing logfile ($TESTING_RUN_DATE) ----\n" > ${TESTING_LOGFILE_NAME} 
    printf "# ----- Automated testing resultsfile ($TESTING_RUN_DATE) ----\n" > ${TESTING_RESULTSFILE_NAME}

    # --- Grab the case driver repo
    if [ "${TESTING_PKG_NAME}" != "mirgecom" ]
    then
        if [ -f "${TESTING_RUN_HOME}/INSTALL_${TESTING_PKG_NAME}" ] || [ ! -d "${TESTING_PKG_NAME}" ];
        then
            rm -Rf ${TESTING_PKG_NAME}
            git clone -b ${TESTING_PKG_BRANCH} git@github.com:/${TESTING_PKG_REPO} ${TESTING_PKG_NAME}
            rm "${TESTING_RUN_HOME}/INSTALL_${TESTING_PKG_NAME}"
        fi
        cd ${TESTING_PKG_NAME}
    fi

    TESTING_PKG_HASH=$(git rev-parse HEAD)
    
    # automatically look for the 'testing' directory
    if [ ! -d "testing" ]
    then
        printf "Testing error: did not find testing directory for testing runs. Aborting tests for ${TESTING_PKG_NAME}.\n"
        printf "${TESTING_PKG_NAME}-package-testing: 1\n" >> ${TESTING_RESULTSFILE_NAME}
        continue        
    fi
    
    cd testing
    printf "Running ${TESTING_PKG_NAME} tests on Host: ${TESTING_RUN_HOST}\n"
    date
    
    TESTING_SCRIPT_NAME="test-${TESTING_RUN_HOST}.sh"
    if [ ! -f "${TESTING_SCRIPT_NAME}" ]
    then
        printf "Testing script (${TESTING_SCRIPT_NAME}) not found.\n"
        TESTING_SCRIPT_NAME="test-${TESTING_RUN_PLATFORM}.sh"
        if [ ! -f "${TESTING_SCRIPT_NAME}" ]
        then
            printf "Testing script (${TESTING_SCRIPT_NAME}) not found, aborting test.\n"
            printf "${TESTING_PKG_NAME}-package-testing: 1\n" >> ${TESTING_RESULTSFILE_NAME}
            continue
        fi
    fi

    printf "Testing with testing script (./${TESTING_SCRIPT_NAME}).\n"
    printf "# Automated testing logging for testing script (${TESTING_SCRIPT_NAME})\n" >> ${TESTING_LOGFILE_NAME} 
    printf "# Automated testing resultsfile capture for testing script (${TESTING_SCRIPT_NAME})\n" >> ${TESTING_RESULTSFILE_NAME}
    ./${TESTING_SCRIPT_NAME} ${EMIRGE_HOME} ${TESTING_RESULTSFILE_NAME} ${TESTING_LOGFILE_NAME}
    TESTING_SCRIPT_STATUS=$?
    printf "# Return status for testing script (${TESTING_SCRIPT_NAME}): ${TESTING_SCRIPT_STATUS}\n" >> ${TESTING_LOGFILE_NAME}
    printf "${TESTING_PKG_NAME}-package-testing: ${TESTING_SCRIPT_STATUS}\n" >> ${TESTING_RESULTSFILE_NAME}
    cd ../../

done < ${testing_configfile}

printf "Testing done for all packages.\n"
date

rm -rf testing-run-results
git clone -b ${TESTING_RESULTS_BRANCH} git@github.com:/${TESTING_RESULTS_REPO} testing-run-results
mkdir -p testing-run-results/${TESTING_RUN_HOST}
rm -f testing-run-results/${TESTING_RUN_HOST}/testing-latest-timestamp.txt
printf "${timestamp}" > testing-run-results/${TESTING_RUN_HOST}/testing-latest-timestamp.txt
cp ${TESTING_RUN_HOME}/testing-log*${timestamp}* testing-run-results/${TESTING_RUN_HOST}
cp ${TESTING_RUN_HOME}/testing-results*${timestamp}* testing-run-results/${TESTING_RUN_HOST}
cd testing-run-results
git add ${TESTING_RUN_HOST}
(git commit -am "Automatic commit: ${TESTING_RUN_HOST} ${TESTING_RUN_DATE}" && git push)
cd ../
rm -rf testing-run-results

