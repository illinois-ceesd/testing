#!/bin/bash
# Usage: mirge-test.sh <package_config_file>
# 
# This script automatically pulls and executes package-specific tests
# potentially using the batch system of remote sites, and helps 
# transmit the testing results back to github.
# 
# The <package_config_file> should be arranged with each line specifying
# a package to test as follows:
#
# <package-name>|<package-repo>|<package-branch>
# 
# For example, for mirgecom nightly testing, it looks like this:
# > cat package_config_file
# mirgecom|illinois-ceesd/mirgecom|production
# isolator|illinois-ceesd/drivers_y2-isolator|slim-faster
#
# This testing script will use `emirge` to install *MIRGE-Com*
# and can be controlled by setting:
# MIRGE_PKG_FORK, MIRGE_PKG_REPO, MIRGE_PKG_BRANCH
# (default is illinois-ceesd/mirgecom@production)
#
# Then each testing package specified by <package_config_file> 
# will be cloned/checked-out into a directory called:
# <package_name>, and will execute this file:
# <package_name>/testing/test-<testing-host>.sh.
# 
# Each package needs to implement the desired tests in
# their native package in the `testing` directory.  Generic
# platforms can be supported by providing "test-linux.sh",
# for example. On Lassen, for example, the script looks for
# `test-lassen.sh`.  The top-level testing script should dispatch
# as many tests as desired for the package.
#
# Package specific test script(s): test-<testing_platform>.sh
# This (mirge-test.sh) script will call the package specific 
# testing script with the following arguments:
#
# > test-<testing_platform>.sh <emirge_home_path> <log_file_path> <results_file_path>
#
# <emirge_home_path>: where to find the base emirge and mirgecom installation
# <log_file_path>: where the package should spew its output/errors
# <results_file_path>: where the package should stuff its results
#
# This script (mirge-test.sh) automatically generates the output and results 
# files without user interaction or cotnrol,
#
# Testing results should go into the <results_file_path> in the following
# format:
# <test-name>: 0
# Anything other than a 0 indicates a test failure.  
#
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
TESTING_RESULTS_BRANCH="main"
TESTING_ENV_NAME="ceesd.testing.env"

# Get the base mirgecom installation from here
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
# update mirgecom just in case it was not installed just now
git pull

# For each "driver package" to test
while read -r line
do
    # reset back to the mirgecom installation directory jic pkg scripting did something naughty
    cd ${TESTING_RUN_HOME}/emirge/mirgecom

    # Parse the pkg configuration
    printf "Testing config: ${line}\n"
    TESTING_PKG_NAME=$(printf "${line}" | cut -d "|" -f 1)
    TESTING_PKG_REPO=$(printf "${line}" | cut -d "|" -f 2)
    TESTING_PKG_BRANCH=$(printf "${line}" | cut -d "|" -f 3)

    # generate the output and results file names
    TESTING_LOGFILE_NAME="${TESTING_RUN_HOME}/testing-log-${TESTING_PKG_NAME}-${TESTING_RUN_HOST}-${timestamp}.txt"
    TESTING_RESULTSFILE_NAME="${TESTING_RUN_HOME}/testing-results-${TESTING_PKG_NAME}-${TESTING_RUN_HOST}-${timestamp}.txt"

    # Create the output and results log files
    rm -f ${TESTING_LOGFILE_NAME} ${TESTING_RESULTSFILE_NAME}
    printf "# ----- Automated testing logfile ($TESTING_RUN_DATE) ----\n" > ${TESTING_LOGFILE_NAME} 
    printf "# ----- Automated testing resultsfile ($TESTING_RUN_DATE) ----\n" > ${TESTING_RESULTSFILE_NAME}

    # --- Grab the case driver repo (if we aren't testing mirgecom itself)
    if [ "${TESTING_PKG_NAME}" != "mirgecom" ]
    then
        # -- Only install if we must, or are forced
        if [ -f "${TESTING_RUN_HOME}/INSTALL_${TESTING_PKG_NAME}" ] || [ ! -d "${TESTING_PKG_NAME}" ];
        then
            rm -Rf ${TESTING_PKG_NAME}
            git clone -b ${TESTING_PKG_BRANCH} git@github.com:/${TESTING_PKG_REPO} ${TESTING_PKG_NAME}
            rm "${TESTING_RUN_HOME}/INSTALL_${TESTING_PKG_NAME}"
        fi
        cd ${TESTING_PKG_NAME}
    fi

    TESTING_PKG_HASH=$(git rev-parse HEAD)
    
    # automatically look for the 'testing' directory in the package
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
    then  # try a generic platform name for the host (e.g., linux).
        printf "Testing script (${TESTING_SCRIPT_NAME}) not found.\n"
        TESTING_SCRIPT_NAME="test-${TESTING_RUN_PLATFORM}.sh"
        if [ ! -f "${TESTING_SCRIPT_NAME}" ]
        then
            printf "Testing script (${TESTING_SCRIPT_NAME}) not found, aborting test.\n"
            printf "${TESTING_PKG_NAME}-package-testing: 1\n" >> ${TESTING_RESULTSFILE_NAME}
            continue
        fi
    fi

    # Execute the pkg's platform/host-specifc test script
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

# Update github with the results from all the package's testing results
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

