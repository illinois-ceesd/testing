#
# Copyright (c) 2020, University of Illinois at Urbana-Champaign, CEESD
# License: MIT, http://opensource.org/licenses/MIT
#
# ===== ABaTe Script for CTest ======
#
# All these variables get set by an interface script,
# presumably abate.sh shell script, or its ilk.
SET (_PROJECT_NAME_ $ENV{TARGET_PROJECT_NAME})
SET (_CONTINUOUS_INTERVAL_ $ENV{CONTINUOUS_INTERVAL})
SET (_CONTINUOUS_DURATION_ $ENV{CONTINUOUS_DURATION})
SET (_PATH_TO_SVN_ $ENV{SVNCOMMAND})
SET (_PATH_TO_GIT_ $ENV{GITCOMMAND})
SET (_PATH_TO_PROJECT_SOURCE_ $ENV{PROJECT_SOURCE})
SET (_PROJECT_ROOT_ $ENV{PROJECT_ROOT})
SET (_CTEST_COMMAND_ $ENV{CTESTCOMMAND})
SET (_CMAKE_COMMAND_ $ENV{CMAKECOMMAND})
SET (_PATH_TO_PROJECT_BINARY_ $ENV{PROJECT_BUILD_DIRECTORY})
SET (_REPO_TYPE_ $ENV{REPO_TYPE})
SET (_BRANCH_ $ENV{BRANCH})
SET (_BRANCHTAG_ $ENV{BRANCHTAG})
SET (_PROJECT_CONFIG_PATH_ $ENV{PROJECT_CONFIGURATION_PATH})
SET (_PROJECT_CONFIG_TYPE_ $ENV{PROJECT_CONFIGURATION_TYPE})
SET (_PROJECT_BUILD_OPTIONS_ $ENV{PROJECT_BUILD_OPTIONS})
SET (_PROJECT_BUILD_OPTIONS_ $ENV{PROJECT_BUILD_OPTIONS})
SET (_PROJECT_BUILD_TAG_ $ENV{PROJECT_BUILD_TAG})
SET (_PROJECT_ENABLE_COVERAGE_ $ENV{PROJECT_ENABLE_COVERAGE})

# OPTIONAL
SET (_CMAKE_CONFIGURATION_OPTIONS_ $ENV{PROJECT_CONFIGURATION_OPTIONS})
# specific options related to building documentation
SET (_BUILD_DOCUMENTATION_ $ENV{BUILD_DOCS})
IF(${_BUILD_DOCUMENTATION_} MATCHES TRUE)
  SET (_DOCUMENTATION_TARGET_ $ENV{DOC_TARGET})
ENDIF(${_BUILD_DOCUMENTATION_} MATCHES TRUE)
# =========================================
FIND_PROGRAM(MAKE NAMES make)
SET(CTEST_TEST_TIMEOUT 7200)

#
# See if there are any custom targets for this project
#
IF(EXISTS "${_PROJECT_NAME_}_custom_targets")
  FILE(STRINGS ${_PROJECT_NAME_}_custom_targets PROJECT_CUSTOM_TARGETS)
  MESSAGE("ABaTe/CTest: Local custom targets for ${_PROJECT_NAME_} = ${PROJECT_CUSTOM_TARGETS}")
ELSEIF(EXISTS "${_PROJECT_CONFIG_PATH_}/${_PROJECT_NAME_}_custom_targets")
  FILE(STRINGS ${_PROJECT_CONFIG_PATH_}/${_PROJECT_NAME_}_custom_targets PROJECT_CUSTOM_TARGETS)
  MESSAGE("ABaTe/CTest: Project global targets for ${_PROJECT_NAME_} = ${PROJECT_CUSTOM_TARGETS}")
ENDIF()

## -- Set hostname
## --------------------------
FIND_PROGRAM(HOSTNAME_CMD NAMES hostname)
MACRO(gethostname name flag)
  EXEC_PROGRAM("${HOSTNAME_CMD}" ARGS "${flag}" OUTPUT_VARIABLE "${name}")
ENDMACRO(gethostname)

gethostname(shorthost -s)
EXEC_PROGRAM(${HOSTNAME_CMD} ARGS OUTPUT_VARIABLE HOSTNAME)
SET(CTEST_SITE                          "${HOSTNAME}")

## -- Set build name
## --------------------------
FIND_PROGRAM(UNAME NAMES uname)
MACRO(getuname name flag)
  EXEC_PROGRAM("${UNAME}" ARGS "${flag}" OUTPUT_VARIABLE "${name}")
ENDMACRO(getuname)

getuname(osname -s)
getuname(osrel  -r)
getuname(cpu    -m)

IF(_PROJECT_BUILD_TAG_) 
  SET(CTEST_BUILD_NAME                    "${_BRANCHTAG_}-${_PROJECT_BUILD_TAG_}")
ELSE()
  SET(CTEST_BUILD_NAME                    "${_BRANCHTAG_}-${osname}-${cpu}")
ENDIF()

SET (CTEST_SOURCE_DIRECTORY "${_PATH_TO_PROJECT_SOURCE_}")
SET (CTEST_BINARY_DIRECTORY "${_PATH_TO_PROJECT_BINARY_}")
MESSAGE("ABaTe/CTest:Setting CTEST_SOURCE_DIRECTORY = ${CTEST_SOURCE_DIRECTORY}")

# should ctest wipe the binary tree before running
IF(${_PROJECT_CONFIG_TYPE_} MATCHES "autotools")
  SET (CTEST_START_WITH_EMPTY_BINARY_DIRECTORY TRUE)
ENDIF()

IF(${_REPO_TYPE_} MATCHES "svn")
  SET (CTEST_REPO_COMMAND "${_PATH_TO_SVN_}")
  SET (CTEST_REPO_CHECKOUT  "${CTEST_REPO_COMMAND} co ${_PROJECT_ROOT_} ${CTEST_SOURCE_DIRECTORY}")
ELSEIF(${_REPO_TYPE_} MATCHES "git")
  SET (CTEST_REPO_COMMAND "${_PATH_TO_GIT_}")
  SET (CTEST_REPO_CHECKOUT  "${CTEST_REPO_COMMAND} clone --recursive -b ${_BRANCH_} ${_PROJECT_ROOT_} ${CTEST_SOURCE_DIRECTORY}")
ELSE()
  MESSAGE(FATAL_ERROR "ABaTe/CTest:ERROR:Unknown repository type, ${_REPO_TYPE_}, specified in projects file, exiting.")
ENDIF()

# which ctest command to use for running the dashboard
SET(MODEL Experimental)
IF(${CTEST_SCRIPT_ARG} MATCHES Nightly)
  SET(MODEL Nightly)
ENDIF(${CTEST_SCRIPT_ARG} MATCHES Nightly)
IF(${CTEST_SCRIPT_ARG} MATCHES Continuous)
  SET(MODEL Continuous)
ENDIF(${CTEST_SCRIPT_ARG} MATCHES Continuous)

#SET (CTEST_COMMAND "${_CTEST_COMMAND_} -V ${_CTEST_CONFIGURATION_OPTIONS_} -D ${MODEL}")

FIND_PROGRAM(CTEST_COVERAGE_COMMAND NAMES gcov)
FIND_PROGRAM(CTEST_MEMORYCHECK_COMMAND NAMES valgrind)

IF(NOT EXISTS "${CTEST_SOURCE_DIRECTORY}")
    SET(CTEST_CHECKOUT_COMMAND "${CTEST_REPO_CHECKOUT}")
ENDIF()
SET(CTEST_UPDATE_COMMAND "${CTEST_REPO_COMMAND}")

# what cmake command to use for configuring this dashboard
SET (CTEST_CMAKE_COMMAND "${_CMAKE_COMMAND_}")

# this is the initial cache to use for the binary tree, be careful to escape
# any quotes inside of this string if you use it
IF(${_PROJECT_CONFIG_TYPE_} MATCHES "autotools")
  SET (CTEST_INITIAL_CACHE "
  CMAKE_GENERATOR:INTERNAL=Unix Makefiles
  BUILDNAME:STRING=${CTEST_BUILD_NAME}
  SITE:STRING=${CTEST_SITE}
  SVN_UPDATE_OPTIONS:STRING=update
  ")
ENDIF()

SET(CTEST_CMAKE_GENERATOR "Unix Makefiles")
IF(${_PROJECT_CONFIG_TYPE_} MATCHES "autotools")
  SET(CTEST_CONFIGURE_COMMAND            "${CTEST_SOURCE_DIRECTORY}/configure ${_CMAKE_CONFIGURATION_OPTIONS_}")
  SET(CTEST_BUILD_COMMAND                "${MAKE} ${_PROJECT_BUILD_OPTIONS_}")
ELSE()
  SET(CTEST_CONFIGURE_COMMAND "${CMAKE_COMMAND} ${_CMAKE_CONFIGURATION_OPTIONS_} ${CTEST_SOURCE_DIRECTORY}")
ENDIF()

# =========== EXECUTE THE UPDATE/CONFIGURE/BUILD/TEST ===========

IF(${MODEL} MATCHES Continuous)
  WHILE (${CTEST_ELAPSED_TIME} LESS ${_CONTINUOUS_DURATION_})
    SET(START_TIME ${CTEST_ELAPSED_TIME})

    CTEST_START(Continuous)

    MESSAGE("ABaTe/CTest:\nABaTe/CTest:***** Getting latest source from the repository *****")
    IF(EXISTS "${CTEST_SOURCE_DIRECTORY}")
      UNSET(CTEST_CHECKOUT_COMMAND)
    ENDIF()
    
    CTEST_UPDATE(RETURN_VALUE count)

    IF(${_PROJECT_CONFIG_TYPE_} MATCHES "autotools")
      CONFIGURE_FILE(${CTEST_SOURCE_DIRECTORY}/CMake/CTestTestfile.cmake ${CTEST_BINARY_DIRECTORY}/CTestTestfile.cmake @ONLY)
      CTEST_READ_CUSTOM_FILES("${CTEST_BINARY_DIRECTORY}")
    ENDIF()
    
    ## -- Update git submodules
    IF (EXISTS "${CTEST_SOURCE_DIRECTORY}/.gitmodules")
      MESSAGE("ABaTe/CTest: -- Updating submodules --")
      EXECUTE_PROCESS(COMMAND ${GIT_EXECUTABLE} submodule update --init --recursive
        WORKING_DIRECTORY ${CTEST_SOURCE_DIRECTORY})
      EXECUTE_PROCESS(COMMAND ${GIT_EXECUTABLE} submodule foreach git pull origin master
        WORKING_DIRECTORY ${CTEST_SOURCE_DIRECTORY})
    ENDIF ()
    
    IF (count GREATER 0) # True if project source changed
      MESSAGE("ABaTe/CTest:\nABaTe/CTest:****** Project Change Detected.... ******")
      MESSAGE("ABaTe/CTest:\n***** Configuring Project  *****")
      
      IF(${_PROJECT_CONFIG_TYPE_} MATCHES "autotools")
        MESSAGE("ABaTe/CTest: -- AutoGen ${MODEL} - ${CTEST_BUILD_NAME} --")
        MESSAGE("ABaTe/CTest:CTEST_SOURCE_DIRECTORY = ${CTEST_SOURCE_DIRECTORY}")
        EXECUTE_PROCESS(COMMAND /bin/sh ./autogen.sh
          WORKING_DIRECTORY ${CTEST_SOURCE_DIRECTORY} 
          RESULT_VARIABLE autogenResult 
          OUTPUT_VARIABLE autogenLog 
          ERROR_VARIABLE autogenLog)
        FILE(WRITE ${CTEST_BINARY_DIRECTORY}/Testing/autogen.log "${autogenLog}")
  
        #  message(" -- Autoreconf ${MODEL} - ${CTEST_BUILD_NAME} --")
        #  execute_process(COMMAND autoreconf -f -i
        #    WORKING_DIRECTORY ${CTEST_SOURCE_DIRECTORY} 
        #    RESULT_VARIABLE autoreconfResult 
        #    OUTPUT_VARIABLE autoreconfLog 
        #    ERROR_VARIABLE autoreconfLog)
        #  file(WRITE ${CTEST_BINARY_DIRECTORY}/Testing/autoreconf.log "${autoreconfLog}")
        
        IF( NOT ${autogenResult} )
          ## -- Configure
          MESSAGE("ABaTe/CTest: -- Configure ${MODEL} - ${CTEST_BUILD_NAME} --")
          CTEST_CONFIGURE(BUILD  "${CTEST_BINARY_DIRECTORY}" RETURN_VALUE res)
    
          ## -- BUILD
          IF( res EQUAL 0 ) # only continue if last command succeeded
            MESSAGE("ABaTe/CTest: -- Build ${MODEL} - ${CTEST_BUILD_NAME} --")
            #      set(CTEST_BUILD_COMMAND "${MAKE} clean")
            #      ctest_build( BUILD "${CTEST_BINARY_DIRECTORY}" RETURN_VALUE res)
            #      set(CTEST_BUILD_COMMAND "${MAKE} ${_PROJECT_BUILD_OPTIONS_}")
            CTEST_BUILD(    BUILD  "${CTEST_BINARY_DIRECTORY}" RETURN_VALUE res)
            IF( res EQUAL 0 )
              FOREACH(PROJECT_CUSTOM_TARGET ${PROJECT_CUSTOM_TARGETS})
                IF( res EQUAL 0 )
                  MESSAGE("ABaTe/CTest:     -- processing custom target: ${PROJECT_CUSTOM_TARGET}")
                  SET(CTEST_BUILD_COMMAND "${MAKE} ${PROJECT_CUSTOM_TARGET}")
                  CTEST_BUILD(BUILD "${CTEST_BINARY_DIRECTORY}" RETURN_VALUE res)
                  SET(CTEST_BUILD_COMMAND                "${MAKE} ${_PROJECT_BUILD_OPTIONS_}")
                ENDIF()
              ENDFOREACH(PROJECT_CUSTOM_TARGET)
            ENDIF()
          ENDIF()
       
      
          ## -- INSTALL
          IF ( res EQUAL 0 )
            MESSAGE("ABaTe/CTest: -- Install ${MODEL} - ${CTEST_BUILD_NAME} --")
            EXECUTE_PROCESS(COMMAND "${MAKE} install ${_PROJECT_BUILD_OPTIONS_}" WORKING_DIRECTORY ${CTEST_BINARY_DIRECTORY} 
              RESULT_VARIABLE makeInstallResult OUTPUT_VARIABLE makeInstallLog ERROR_VARIABLE makeInstallLog)
            FILE(WRITE ${CTEST_BINARY_DIRECTORY}/Testing/makeinstall.log "${makeInstallLog}")
          ENDIF()
    
          IF( NOT ${makeInstallResult} )
            ## -- TEST
            MESSAGE("ABaTe/CTest: -- Test ${MODEL} - ${CTEST_BUILD_NAME} --")
            CTEST_TEST(     BUILD  "${CTEST_BINARY_DIRECTORY}" RETURN_VALUE res)
          ENDIF()

        ENDIF( NOT ${autogenResult} )
        
      ELSE() # ================= CMAKE PROJECT  ====================
        
        MESSAGE("ABaTe/CTest:\nABaTe/CTest:***** Configuring Project  *****")
        CTEST_CONFIGURE(RETURN_VALUE res)
        IF( res EQUAL 0) 
          MESSAGE("ABaTe/CTest:\nABaTe/CTest:***** Building Project  *****")
          CTEST_BUILD(RETURN_VALUE res)
          IF(res EQUAL 0) 
            MESSAGE("ABaTe/CTest:\nABaTe/CTest:***** Building Project Custom Targets *****")
            FOREACH(PROJECT_CUSTOM_TARGET ${PROJECT_CUSTOM_TARGETS})
              IF(res EQUAL 0) 
                MESSAGE("ABaTe/CTest:     -- processing custom target: ${PROJECT_CUSTOM_TARGET}")
                CTEST_BUILD(BUILD "${CTEST_BINARY_DIRECTORY}" TARGET ${PROJECT_CUSTOM_TARGET} RETURN_VALUE res)
              ENDIF(res EQUAL 0)
            ENDFOREACH(PROJECT_CUSTOM_TARGET)
          ENDIF(res EQUAL 0)
          IF(res EQUAL 0) 
            MESSAGE("ABaTe/CTest:\nABaTe/CTest:***** Running Project Tests *****")
            CTEST_TEST()
          ENDIF(res EQUAL 0)
        ENDIF(res EQUAL 0)
      ENDIF()
      
      IF(CTEST_COVERAGE_COMMAND)
        MESSAGE("ABaTe/CTest:CTEST_COVERAGE_COMMAND = ${CTEST_COVERAGE_COMMAND}")
        IF(_PROJECT_ENABLE_COVERAGE_) 
          MESSAGE("ABaTe/CTest:\nABaTe/CTest:***** Checking Project Test Coverage *****")
          CTEST_COVERAGE()
        ELSE()
          MESSAGE("ABaTe/CTest: - Coverage testing not enabled. -")
        ENDIF()
      ENDIF()
      
      IF(WITH_MEMCHECK AND CTEST_MEMORYCHECK_COMMAND)
        MESSAGE("ABaTe/CTest:CTEST_MEMORYCHECK_COMMAND = ${CTEST_MEMORYCHECK_COMMAND}")
        CTEST_MEMCHECK()
      ENDIF()
      
      MESSAGE("ABaTe/CTest:\nABaTe/CTest:***** Submiting Results to CDash *****")
      # this is broken for scp transfer since I need to submit outside the script (from the driver)
      IF($ENV{CTEST_DROP_METHOD})
        IF(NOT $ENV{CTEST_DROP_METHOD} STREQUAL "scp")
          MESSAGE("ABaTe/CTest:\nABaTe/CTest:***** Submiting Results to CDash *****")
          CTEST_SUBMIT()
        ENDIF()
      ELSE()
        CTEST_SUBMIT()
      ENDIF()
    ENDIF()
    CTEST_SLEEP( ${START_TIME} ${_CONTINUOUS_INTERVAL_} ${CTEST_ELAPSED_TIME})
  ENDWHILE()
  
ELSE()  # ================== Not Continuous ===================
  
  CTEST_START("${MODEL}")
  MESSAGE("ABaTe/CTest:\nABaTe/CTest:***** Getting latest source from the repository *****")
  IF(EXISTS "${CTEST_SOURCE_DIRECTORY}")
    UNSET(CTEST_CHECKOUT_COMMAND)
  ENDIF()
  
  CTEST_UPDATE()
  
  IF(${_PROJECT_CONFIG_TYPE_} MATCHES "autotools")
    CONFIGURE_FILE(${CTEST_SOURCE_DIRECTORY}/CMake/CTestTestfile.cmake ${CTEST_BINARY_DIRECTORY}/CTestTestfile.cmake @ONLY)
    #    configure_file(${CTEST_SOURCE_DIRECTORY}/CMake/CTestCustom.cmake   ${CTEST_BINARY_DIRECTORY}/CTestCustom.cmake)
    CTEST_READ_CUSTOM_FILES("${CTEST_BINARY_DIRECTORY}")
  ENDIF()
  
  ## -- Update git submodules
  IF (EXISTS "${CTEST_SOURCE_DIRECTORY}/.gitmodules")
    MESSAGE("ABaTe/CTest: -- Updating submodules --")
    EXECUTE_PROCESS(COMMAND ${GIT_EXECUTABLE} submodule update --init --recursive
      WORKING_DIRECTORY ${CTEST_SOURCE_DIRECTORY})
    EXECUTE_PROCESS(COMMAND ${GIT_EXECUTABLE} submodule foreach git pull origin master
      WORKING_DIRECTORY ${CTEST_SOURCE_DIRECTORY})
  ENDIF ()
  
  IF(${_PROJECT_CONFIG_TYPE_} MATCHES "autotools") # ======== Nasty Autoconf Project

    MESSAGE("ABaTe/CTest: -- AutoGen ${MODEL} - ${CTEST_BUILD_NAME} --")
    MESSAGE("ABaTe/CTest:CTEST_SOURCE_DIRECTORY = ${CTEST_SOURCE_DIRECTORY}")
    EXECUTE_PROCESS(COMMAND /bin/sh ./autogen.sh
      WORKING_DIRECTORY ${CTEST_SOURCE_DIRECTORY} 
      RESULT_VARIABLE autogenResult 
      OUTPUT_VARIABLE autogenLog 
      ERROR_VARIABLE autogenLog)
    FILE(WRITE ${CTEST_BINARY_DIRECTORY}/Testing/autogen.log "${autogenLog}")

    #    message(" -- Autoreconf ${MODEL} - ${CTEST_BUILD_NAME} --")
    #    execute_process(COMMAND autoreconf -f -i
    #      WORKING_DIRECTORY ${CTEST_SOURCE_DIRECTORY} 
    #      RESULT_VARIABLE autoreconfResult 
    #      OUTPUT_VARIABLE autoreconfLog 
    #      ERROR_VARIABLE autoreconfLog)
    #    file(WRITE ${CTEST_BINARY_DIRECTORY}/Testing/autoreconf.log "${autoreconfLog}")
    
    IF( NOT ${autogenResult} )

      ## -- Configure
      MESSAGE("ABaTe/CTest: -- Configure ${MODEL} - ${CTEST_BUILD_NAME} --")
      CTEST_CONFIGURE(BUILD  "${CTEST_BINARY_DIRECTORY}" RETURN_VALUE res)

      ## -- BUILD
      IF(res EQUAL 0) 
        MESSAGE("ABaTe/CTest: -- Build ${MODEL} - ${CTEST_BUILD_NAME} --")
        #  set(CTEST_BUILD_COMMAND "${MAKE} clean")
        #  ctest_build( BUILD "${CTEST_BINARY_DIRECTORY}" RETURN_VALUE res)
        #  set(CTEST_BUILD_COMMAND "${MAKE} ${_PROJECT_BUILD_OPTIONS_}")
        CTEST_BUILD(    BUILD  "${CTEST_BINARY_DIRECTORY}" RETURN_VALUE res)
        MESSAGE("ABaTe/CTest:\nABaTe/CTest:***** Building Project Custom Targets *****")
        IF(res EQUAL 0) 
          FOREACH(PROJECT_CUSTOM_TARGET ${PROJECT_CUSTOM_TARGETS})
            IF(res EQUAL 0) 
              MESSAGE("ABaTe/CTest:     -- processing custom target: ${PROJECT_CUSTOM_TARGET}")
              SET(CTEST_BUILD_COMMAND "${MAKE} ${PROJECT_CUSTOM_TARGET}")
              CTEST_BUILD(BUILD "${CTEST_BINARY_DIRECTORY}" RETURN_VALUE res)
              SET(CTEST_BUILD_COMMAND                "${MAKE} ${_PROJECT_BUILD_OPTIONS_}")
            ENDIF()
          ENDFOREACH(PROJECT_CUSTOM_TARGET)
        ENDIF()
      ENDIF()

      ## -- INSTALL
      IF(res EQUAL 0) 
        MESSAGE("ABaTe/CTest: -- Install ${MODEL} - ${CTEST_BUILD_NAME} --")
        EXECUTE_PROCESS(COMMAND "${MAKE} install ${_PROJECT_BUILD_OPTIONS_}" WORKING_DIRECTORY ${CTEST_BINARY_DIRECTORY} 
          RESULT_VARIABLE makeInstallResult OUTPUT_VARIABLE makeInstallLog ERROR_VARIABLE makeInstallLog)
        FILE(WRITE ${CTEST_BINARY_DIRECTORY}/Testing/makeinstall.log "${makeInstallLog}")
      ENDIF()

      ## -- TEST
      IF(NOT ${makeInstallResult}) 
        MESSAGE("ABaTe/CTest: -- Test ${MODEL} - ${CTEST_BUILD_NAME} --")
        CTEST_TEST(     BUILD  "${CTEST_BINARY_DIRECTORY}" RETURN_VALUE res)
      ENDIF()

    ENDIF( NOT ${autogenResult} )

  ELSE() # =================== CMake Project ===================
    
    MESSAGE("ABaTe/CTest:\nABaTe/CTest:***** Configuring *****")
    CTEST_CONFIGURE(RETURN_VALUE res)
    IF(res EQUAL 0) 
      MESSAGE("ABaTe/CTest:\nABaTe/CTest:***** Building Project *****")
      CTEST_BUILD(RETURN_VALUE res)
      IF(res EQUAL 0) 
        MESSAGE("ABaTe/CTest:\nABaTe/CTest:***** Building Project Custom Targets *****")
        FOREACH(PROJECT_CUSTOM_TARGET ${PROJECT_CUSTOM_TARGETS})
          IF(res EQUAL 0)
            MESSAGE("ABaTe/CTest:     -- processing custom target: ${PROJECT_CUSTOM_TARGET}")
            CTEST_BUILD(BUILD "${CTEST_BINARY_DIRECTORY}" TARGET ${PROJECT_CUSTOM_TARGET} RETURN_VALUE res)
          ENDIF()
        ENDFOREACH(PROJECT_CUSTOM_TARGET)
        IF(res EQUAL 0) 
          IF(${_BUILD_DOCUMENTATION_} MATCHES TRUE)
            MESSAGE("ABaTe/CTest:\nABaTe/CTest:***** Building documentation *****")
            CTEST_BUILD(BUILD ${CTEST_BINARY_DIRECTORY} TARGET ${_DOCUMENTATION_TARGET_})
          ENDIF(${_BUILD_DOCUMENTATION_} MATCHES TRUE)
          MESSAGE("ABaTe/CTest:\nABaTe/CTest:***** Running Project Tests *****")
          CTEST_TEST()
        ENDIF()
      ENDIF()
    ENDIF()
  ENDIF(${_PROJECT_CONFIG_TYPE_} MATCHES "autotools")

  IF(CTEST_COVERAGE_COMMAND)
    MESSAGE("ABaTe/CTest:CTEST_COVERAGE_COMMAND  = ${CTEST_COVERAGE_COMMAND}")
    IF(_PROJECT_ENABLE_COVERAGE_) 
      MESSAGE("ABaTe/CTest:\nABaTe/CTest:***** Checking Project Test Coverage *****")
      CTEST_COVERAGE()
    ELSE()
      MESSAGE("ABaTe/CTest: - Coverage testing not enabled. -")
    ENDIF()
  ENDIF()
  
  IF(WITH_MEMCHECK AND CTEST_MEMORYCHECK_COMMAND)
    MESSAGE("ABaTe/CTest:CTEST_MEMCHECK_COMMAND  = ${CTEST_MEMCHECK_COMMAND}")
    CTEST_MEMCHECK()
  ENDIF()
  
  IF($ENV{CTEST_DROP_METHOD})
    IF(NOT $ENV{CTEST_DROP_METHOD} STREQUAL "scp")
      MESSAGE("ABaTe/CTest:\nABaTe/CTest:***** Submiting Results to CDash *****")
      CTEST_SUBMIT()
    ENDIF()
  ELSE()
    CTEST_SUBMIT()
  ENDIF()
  
ENDIF(${MODEL} MATCHES Continuous) # Not continuous
