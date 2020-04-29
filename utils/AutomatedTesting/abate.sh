#!/bin/sh

#
# ABaTe interface
#

#
# Copyright (c) 2020, University of Illinois at Urbana-Champaign, CEESD
# License: MIT, http://opensource.org/licenses/MIT
#

if [ -z ${1} ]; then
  printf "Usage:\n"
  printf "${0} <project_file> [mode=Experimental|Nightly|Continuous]\n"
  printf "                    [working directory] [specific_project] [continuous_duration]\n"
  printf "                    [continuous_interval]\n"
  printf "\n\n\n"
  printf "<project_file> is the full path to the *project_file* which has the\n"
  printf "               following content and format:\n\n"
  printf "    ProjectName|BranchName|Repository|RepoType|ConfigType|DocTarget\n\n"
  printf "  Where ProjectName = the name of the project to build\n"
  printf "        BranchName  = the name of the repository branch\n"
  printf "        Repository  = the path to the source repository\n"
  printf "        RepoType    = type of repository (git|svn)\n"
  printf "        ConfigType  = type of build management (autotools|cmake)\n"
  printf "        DocTarget   = build target to make documentation [optional]\n"
  printf "  Example project line:\n"
  printf "    MyProject|master|git@bitbucket.org/mygroup/myproject|git|cmake|doc\n\n"
  printf "[mode] - optionally specify the CTest testing mode. The following modes are available:\n"
  printf "  Experimental - any generic build, typically run manually [default]\n"
  printf "  Nightly      - typically used once a day after hours\n"
  printf "  Continuous   - for <continuous_duration> seconds, the repository will\n"
  printf "                 check for repository updates every <continuous_interval>\n"
  printf "                 seconds, triggering re-build and re-test if updated.\n\n"
  printf "[working_directory] - optionally specify the full path to the directory in which to run [default=CWD]\n"
  printf "[specific_project] - optionally indicates to build a specific project (from <project_file>)\n"
  printf "[continuous_duration] - optionally specifies the duration (seconds) to do continuous testing [default=9hrs]\n" 
  printf "[continuous_interval] - optionally specifies the interval (seconds) between repo checks [default=5min]\n\n" 
  printf "Influential environment variables: [default=<working_directory>]\n"
  printf "  ABATE_BUILD_DIRECTORY     : Specifies base path where ABaTe should perform project builds.\n"
  printf "  ABATE_SOURCE_DIRECTORY    : Specifies base path where ABaTe should perform project repo checkouts.\n"
  printf "  ABATE_CONFIG_PATH         : Specifies base path where ABaTe should look for platform and project configurations.\n"
  printf "  ABATE_CONFIG_PLATFORM     : Specifies the name of the platform ABaTe uses when looking for configurations.\n"
  printf "  ABATE_CONTINUOUS_DURATION : Specifies <continuous_duration>\n"
  printf "  ABATE_CONTINUOUS_INTERVAL : Specifies <continuous_interval>\n"
  printf "\n"
  exit 0
fi

# ===== Initialization and setup =========
project_file="$1"
mode="$2"
rundir="$3"
specific_test="$4"
cduration="$5"
cinterval="$6"

if [ -z "${project_file}" ]; then
  printf "ABaTe/RunTests: Error: No project file specified.\n"
  exit 1
fi

if [ -z "${mode}" ]; then
  mode="Experimental"
fi
if [[ "${mode}" != "Continuous"   && \
          "${mode}" != "Experimental" && \
          "${mode}" != "Nightly" ]]; then
    printf "ABaTe/RunTests: Error: Unrecognized mode(${mode}).\n"
    exit 1
fi
          
if [ -z "${rundir}" ]; then
  rundir=`pwd`
fi

if [ -z "${cduration}" ]; then
    if [ -z "${ABATE_CONTINUOUS_DURATION}" ]; then
        cduration=5400
    else
        cduration=${ABATE_CONTINUOUS_DURATION}
    fi
fi

if [ -z "${cinterval}"]; then
    if [ -z "${ABATE_CONTINUOUS_INTERVAL}" ]; then
        cinterval=300
    else
        cinterval=${ABATE_CONTINUOUS_INTERVAL}
    fi
fi

# Use DRYRUN for testing this script without executing 
# DRYRUN="YES"

# These variables are needed by the next
# actor in the automated testing toolchain
export CONTINUOUS_DURATION=${cduration}
export CONTINUOUS_INTERVAL=${cinterval}

myhostname=`hostname -s`
platformname=$myhostname
if [ ! -z ${ABATE_CONFIG_PLATFORM} ]; then platformname=${ABATE_CONFIG_PLATFORM}; fi

printf "ABaTe/RunTests: Starting ${mode} tests on host(${myhostname}).\n"
printf "ABaTe/RunTests: Platform Name: ${platformname}\n"
printf "ABaTe/RunTests: Using project file: ${project_file}.\n"

if [ ! -z ${specific_test} ]; then
    printf "ABaTe/RunTests: Running specific project(${specific_test}).\n"
fi

cd ${rundir}
mypwd=`pwd`
printf "ABaTe/RunTests: Running at working path(${mypwd}).\n" 

configdir=${mypwd}
if [ ! -z ${ABATE_CONFIG_PATH} ]; then
    configdir=${ABATE_CONFIG_PATH}
fi

if [ ! -f ${configdir}/abate.cmake ]; then
    printf "ABaTe/RunTests: Could not find (abate.cmake) at ${configdir}. Aborting.\n"
    exit 1
fi

if [ ${configdir} != ${rundir} ]; then
    cp ${configdir}/abate.cmake ${rundir}
fi

printf "ABaTe/RunTests: Using global configuration path($configdir).\n"

# This looks for:
# <GLOBALCONFIG_PATH>/{platformname}_config directory
# <GLOBALCONFIG_PATH>/{hostname}_config directory
# <GLOBALCONFIG_PATH>/platform_configuration (legacy support)
platformconfigdir=""
if [ -d ${configdir}/${platformname}_config ]; then
    printf "ABaTe/RunTests: Found ${configdir}/${platformname}_configuration directory.\n"
    platformconfigdir=${configdir}/${platformname}_config
elif [ -d ${configdir}/${myhostname}_configuration ]; then
    printf "ABaTe/RunTests: Found ${configdir}/${myhostname}_configuration directory.\n"
    platformconfigdir=${configdir}/${myhostname}_configuration
elif [ -d ${configdir}/platform_configuration ]; then
    printf "ABaTe/RunTests: Found ${configdir}/platform_configuration directory.\n"
    platformconfigdir=${configdir}/platform_configuration
fi

if [ ! -z ${platformconfigdir} ]; then
    printf "ABaTe/RunTests: Using platform configuration path (${platformconfigdir}).\n"
    for platform_configuration_file in $(ls ${platformconfigdir}/*.sh )
    do
        printf "ABaTe/RunTests: Applying platform-config: ${platform_configuration_file}\n"
        if [ -z ${DRYRUN} ]; then
            source ${platform_configuration_file}
        else
            printf "ABaTe/RunTests: ==== PlatformConfig($platform_configuration_file) (DRYRUN) ====\n"
            cat ${platform_configuration_file}
            printf "ABaTe/RunTests: ==== PlatformConfig($platform_configuration_file) (DRYRUN) ====\n"
        fi
    done
else
    printf "ABaTe/RunTests: No platform-specific configuration found.\n"
fi

if [ -z ${ABATE_BUILD_DIRECTORY} ]; then
    projectbuilddir=${rundir}
else
    projectbuilddir=${ABATE_BUILD_DIRECTORY}
fi

if [ -z ${ABATE_SOURCE_DIRECTORY} ]; then
    projectsourcedir=${rundir}
else
    projectsourcedir=${ABATE_SOURCE_DIRECTORY}
fi

printf "ABaTe/RunTests: Using base project build path($projectbuilddir)\n"
printf "ABaTe/RunTests: Using base project source path($projectsourcedir)\n"

if [ ! -d ${projectbuilddir} ]; then
    printf "ABaTe/RunTests: Creating build path($projectbuilddir).\n"
    if [ -z ${DRYRUN} ]; then mkdir -p ${projectbuilddir}; fi
fi

if [ ! -d ${projectsourcedir} ]; then
    printf "ABaTe/RunTests: Creating source path($projectsourcedir).\n"
    if [ -z ${DRYRUN} ]; then mkdir -p ${projectsourcedir}; fi
fi

# ========================================

# === Build/Test Environment Settings ====
export SVNCOMMAND=`which svn`
export GITCOMMAND=`which git`
export CTESTCOMMAND=`which ctest`
export CMAKECOMMAND=`which cmake`

printf "ABaTe/RunTests: Found SVN($SVNCOMMAND)\n"
printf "ABaTe/RunTests: Found GIT($GITCOMMAND)\n"
printf "ABaTe/RunTests: Found CTEST($CTESTCOMMAND)\n"
printf "ABaTe/RunTests: Found CMAKE($CMAKECOMMAND)\n"

# export BUILD_DOCUMENTATION="ON"
# export PROJECT_CONFIGURATION_OPTIONS="-DBUILD_DOCUMENATATION=YES"

# == Loop over projects and invoke ctest =
for projectline in $(cat ${project_file} | grep -v "#")
do
    project=`printf ${projectline} | cut -d "#" -f 1`
    if [ -z "${project}" ]; then
        continue
    fi
    projectname=`printf "${project}" | cut -d "|" -f 1`
    if [ -z "${projectname}" ]; then
        printf "ABaTe/RunTests: Error: Couldn't get project name.\n"
    fi               
    branchname=`printf "${project}" | cut -d "|" -f 2`
    if [ -z "${branchname}" ]; then
        printf "ABaTe/RunTests: Error: Couldn't get branch name.\n"
    fi
    # version of branchname with "/" replaced by "_"
    branchname_safe=`printf "${project}" | cut -d "|" -f 2 | sed 's/\//_/g'`
    if [ -z "${branchname_safe}" ]; then
        printf "ABaTe/RunTests: Error: Couldn't get safe branchname.\n"
    fi
    branchpath=`printf "${project}" | cut -d "|" -f 3`
    if [ -z "${branchpath}" ]; then
        printf "ABaTe/RunTests: Error: Couldn't get branchpath.\n"
    fi
    repotype=`printf "${project}" | cut -d "|" -f 4`
    if [ -z "${repotype}" ]; then
        printf "ABaTe/RunTests: Error: Couldn't get repotype.\n"
    fi
    configtype=`printf "${project}" | cut -d "|" -f 5`
    if [ -z "${configtype}" ]; then
        printf "ABaTe/RunTests: Error: Couldn't get configtype.\n"
    fi
    doctarget=`printf "${project}" | cut -d "|" -f 6`
    
    if [[ "${projectname}" == "" || \
          "${branchname}" == "" || \
          "${branchname_safe}" == "" || \
          "${branchpath}" == "" || \
          "${repotype}" == "" || \
          "${configtype}" == "" ]]; then
        printf "ABaTe/RunTests:Error: Project line malformed for project(${projectname}) file: ${project_file}\n"
        continue
    fi               
    
    if [[ ${specific_test} == ${projectname} || ${specific_test} == "" ]]; then  

        export REPO_TYPE=${repotype}
        export CONFIG_TYPE=${configtype}
        export DOC_TARGET=${doctarget}
        export TARGET_PROJECT_NAME=${projectname}
        export PROJECT_SOURCE=${projectsourcedir}/${projectname}_${branchname_safe}
        export PROJECT_ROOT=${branchpath}
        export PROJECT_BUILD_DIRECTORY=${projectbuilddir}/${projectname}-${myhostname}-${branchname_safe}-${mode}
        export BRANCH=${branchname}
        export BRANCHTAG=${branchname_safe}

        # @todo put all the project-specific settings here
        export PROJECT_BUILD_TAG=""
        export PROJECT_CONFIGURATION_OPTIONS=""
        
        projectconfigdir=./
        if [ -d ${configdir}/${projectname}_configuration ]; then
            projectconfigdir=${configdir}/${projectname}_configuration
        fi
        
        printf "ABaTe/RunTests: Using project(${projectname}) configuration path(${projectconfigdir}).\n"

        if [ -e ${projectconfigdir}/initialize_test_environment ]; then
            printf "ABaTe/RunTests: Initializing test environment for project(${projectname}).\n"
            printf "ABaTe/RunTests: Source: ${projectconfigdir}/initialize_test_environment.\n"
            if [ -z ${DRYRUN} ]; then
                source ${projectconfigdir}/initialize_test_environment
            else
                printf "ABaTe/RunTests: ==== ${projectname} Environment (DRYRUN) ===\n"
                cat ${projectconfigdir}/initialize_test_environment
                printf "ABaTe/RunTests: ==== ${projectname} Environment (DRYRUN) ===\n"
            fi
        fi
        
        printf "ABaTe/RunTests: ***************************************************\n"
        printf "ABaTe/RunTests: Processing ${projectname} ${branchname} on ${myhostname}\n"
        printf "ABaTe/RunTests: ***************************************************\n"
        printf "ABaTe/RunTests: Project name:      ${projectname}\n"
        printf "ABaTe/RunTests: Project branch:    ${branchname}\n"
        printf "ABaTe/RunTests: Safe branch name:  ${branchname_safe}\n"
        printf "ABaTe/RunTests: Repo type:         ${repotype}\n"
        printf "ABaTe/RunTests: Project repo path: ${branchpath}\n"
        printf "ABaTe/RunTests: Project source:    ${PROJECT_SOURCE}\n"
        printf "ABaTe/RunTests: Project build:     ${PROJECT_BUILD_DIRECTORY}\n"
        printf "ABaTe/RunTests: Mode:              ${mode}\n"
        if [ "${mode}" == "Continuous" ]; then
          printf "ABaTe/RunTests: Continuous Interval:  ${CONTINUOUS_INTERVAL}(s)\n"
          printf "ABaTe/RunTests: Continuous Duraction: ${CONTINUOUS_DURATION}(s)\n"
        fi
        mydate=`date`
        printf "ABaTe/RunTests: Running ABaTe/CTest for ${projectname} Date(${mydate})\n"
        
        # ======================= Run ABaTe/CTest for project (setup done)  ===========================
    
        mylogfile=abate_${projectname}_${branchname_safe}_${mode}_log.txt
        
        if [ ! -f ${mylogfile} ]; then touch ${mylogfile}; fi
        
        printf "\n\nABaTe/RunTests: *************${mydate}***************\n" >> ${mylogfile}
        printf "ABaTe/RunTests: VVVVVVVVVV Build Environment VVVVVVVVV\n" >> ${mylogfile}
        env >> ${mylogfile}
        printf "ABaTe/RunTests: ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^\n" >> ${mylogfile}
          
        if [ "${mode}" == "Continuous" ]; then
            abate_continuous_logfile=abate_${projectname}_${branchname_safe}_${mode}_continuousLog.txt
            rm -f ${abate_continuous_logfile}
            if [ -z ${DRYRUN} ]; then
                ctest -S ./abate.cmake,Continuous -V >& ${abate_continuous_logfile} &
            else
                printf "ABaTe/RunTests:--- DRY RUN ---\n" > ${abate_continuous_logfile}
                printf "ABaTe/RunTests: Command: ctest -S ./abate.cmake,Continuous -V\n" >> ${abate_continuous_logfile}
                printf "ABaTe/RunTests: - END DRY RUN -\n" >> ${abate_continuous_logfile}
            fi
            printf "ABaTe/RunTests: Done spawning continuous testing.\n"
        else
            abate_temp_logfile=abate_${projectname}_${branchname_safe}_${mode}_temp_log.txt
            rm -f ${abate_temp_logfile}
            if [ -z ${DRYRUN} ]; then
                ctest -S ./abate.cmake,${mode} -V >& ${abate_temp_logfile}
            else
                printf "ABaTe/RunTests:---- DRY RUN ----\n" > ${abate_temp_logfile}
                printf "ABaTe/RunTests: Command: ctest -S ./abate.cmake,${mode} -V\n" >> ${abate_temp_logfile}
                printf "ABaTe/RunTests:--- END DRY RUN -\n" >> ${abate_temp_logfile}
            fi
            cat ${abate_temp_logfile} >> ${mylogfile}
            rm -f ${abate_temp_logfile}
        fi
        mydate=`date`        
        printf "ABaTe/RunTests: ***************${mydate}*****************\n" >> ${mylogfile}
        # ======================= ABaTe/CTest done running for project ================================
        printf "ABaTe/RunTests: Done running ABaTe/CTest for ${projectname} Date(${mydate})\n"

        if [ -e ${projectconfigdir}/deinitialize_test_environment ]; then
            printf "ABaTe/RunTests: Tearing down test environment for project(${projectname}).\n"
            if [ -z ${DRYRUN} ]; then
                source ${projectconfigdir}/deinitialize_test_environment
            else
                cat ${projectconfigdir}/deinitialize_test_environment
            fi
        fi

    fi
done

#reset environment
#setenv MODULEPATH $MODULEPATHSAV
# ========================================
