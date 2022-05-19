#!/bin/bash

set -e
set -x
set -o pipefail

date
timestamp=$(date "+%Y.%m.%d-%H.%M.%S")
TIME_SINCE_EPOCH=$(date +%s)
TESTING_RUN_HOME=$(pwd)
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
TESTING_RESULTS_BRANCH="main"
TESTING_LOGFILE_NAME="${TESTING_RUN_HOME}/testing-log-${TESTING_RUN_HOST}-${timestamp}.txt"
TESTING_RESULTSFILE_NAME="${TESTING_RUN_HOME}/testing-results-${TESTING_RUN_HOST}-${timestamp}.txt"
TESTING_ENV_NAME="ceesd.testing.env"
MIRGE_PKG_FORK="illinois-ceesd"
MIRGE_PKG_REPO="${MIRGE_PKG_FORK}/mirgecom"
MIRGE_PKG_BRANCH="production"

# For each "driver package" to test
#TESTING_PKG_NAME="y2-isolator"
TESTING_PKG_NAME="mirgecom"
TESTING_PKG_FORK="illinois-ceesd"
TESTING_PKG_REPO="${TESTING_PKG_FORK}/${TESTING_PKG_NAME}"
TESTING_PKG_BRANCH="production"

# -- Install conda env, dependencies and MIRGE-Com via *emirge*
# --- remove old run if it exists
if [ -f "INSTALL_MIRGECOM" ] || [ ! -d "emirge" ];
then
    if [ -d "emirge" ];
    then
        echo "Removing old timing run."
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

# --- Grab the case driver repo
if [ "${TESTING_PKG_NAME}" != "mirgecom" ]
then
    if [ -f "INSTALL_${TESTING_PKG_NAME}" ]
    then
        rm -Rf ${TESTING_PKG_NAME}
        git clone -b ${TESTING_PKG_BRANCH} git@github.com:/${TESTING_PKG_REPO} ${TESTING_PKG_NAME}
    fi
    cd ${TESTING_PKG_NAME}
fi

TESTING_PKG_HASH=$(git rev-parse HEAD)

# automatically look for the 'testing' directory
if [ ! -d "testing" ]
then
    printf "Testing error: did not find testing directory for testing runs. Aborting tests for ${TESTING_PKG_NAME}.\n"
    exit 1
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
        exit 1
    fi
fi

printf "Testing with testing script (./${TESTING_SCRIPT_NAME}).\n"
printf "# Automated testing logfile for testing script (${TESTING_SCRIPT_NAME})\n" > ${TESTING_LOGFILE_NAME} 
printf "# Automated testing resultsfile for testing script (${TESTING_SCRIPT_NAME})\n" > ${TESTING_RESULTSFILE_NAME}
./${TESTING_SCRIPT_NAME} ${EMIRGE_HOME} ${TESTING_RESULTSFILE_NAME} ${TESTING_LOGFILE_NAME}
TESTING_SCRIPT_STATUS=$?

printf "Testing done for all packages.\n"
date

git clone -b ${TESTING_RESULTS_BRANCH} git@github.com:/${TESTING_RESULTS_REPO} testing-run-results
mkdir -p testing-run-results/${TESTING_RUN_HOST}
cp ${TESTING_LOGFILE_NAME} testing-run-results/${TESTING_RUN_HOST}
cp ${TESTING_RESULTSFILE_NAME} testing-run-results/${TESTING_RUN_HOST}
cd testing-run-results
git add ${TESTING_RUN_HOST}
(git commit -am "Automatic commit: ${TIMING_HOST} ${TIMING_DATE}" && git push)
cd ../

exit ${TESTING_SCRIPT_STATUS}
