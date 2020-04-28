#
# Copyright (c) 2020, University of Illinois at Urbana-Champaign, XPACC
# License: MIT, http://opensource.org/licenses/MIT
#
# ===== Automated Test Steering Script for CTest ======
#
# All these variables get set by an interface script,
# presumably *run_automated_tests* csh script provided
# with the *AutomatedTesting* suite of tools from
# bitbucket.org/MTCam/AutomatedTesting
#
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
SET (_PROJECT_CONFIG_TYPE_ $ENV{PROJECT_CONFIGURATION_TYPE})
SET (_PROJECT_BUILD_OPTIONS_ $ENV{PROJECT_BUILD_OPTIONS})
SET (_PROJECT_BUILD_OPTIONS_ $ENV{PROJECT_BUILD_OPTIONS})
SET (_PROJECT_BUILD_TAG_ $ENV{PROJECT_BUILD_TAG})
# OPTIONAL
SET (_CMAKE_CONFIGURATION_OPTIONS_ $ENV{PROJECT_CONFIGURATION_OPTIONS})
# specific options related to building documentation
SET (_BUILD_DOCUMENTATION_ $ENV{BUILD_DOCS})
IF(${_BUILD_DOCUMENTATION_} MATCHES TRUE)
  SET (_DOCUMENTATION_TARGET_ $ENV{DOC_TARGET})
ENDIF(${_BUILD_DOCUMENTATION_} MATCHES TRUE)
# =========================================
find_program(MAKE NAMES make)
SET(CTEST_TEST_TIMEOUT 7200)

#
# See if there are any custom targets for this project
#
if(EXISTS "custom_targets/${_PROJECT_NAME_}")
  FILE(STRINGS custom_targets/${_PROJECT_NAME_} PROJECT_CUSTOM_TARGETS)
  MESSAGE("Custom targets for ${_PROJECT_NAME_} = ${PROJECT_CUSTOM_TARGETS}")
endif()

# -----------------------------------------------------------  
# -- Get environment
# -----------------------------------------------------------  
SET (CTEST_ENVIRONMENT
  "PATH=/home/mtcampbe/Software/Install/bin:${PATH}"
  "LD_LIBRARY_PATH=/home/mtcampbe/Software/Install/lib:${LD_LIBRARY_PATH}"
  )

## -- Set hostname
## --------------------------
find_program(HOSTNAME_CMD NAMES hostname)
macro(gethostname name flag)
  exec_program("${HOSTNAME_CMD}" ARGS "${flag}" OUTPUT_VARIABLE "${name}")
endmacro(gethostname)

gethostname(shorthost -s)
exec_program(${HOSTNAME_CMD} ARGS OUTPUT_VARIABLE HOSTNAME)
set(CTEST_SITE                          "${HOSTNAME}")

## -- Set build name
## --------------------------
find_program(UNAME NAMES uname)
macro(getuname name flag)
  exec_program("${UNAME}" ARGS "${flag}" OUTPUT_VARIABLE "${name}")
endmacro(getuname)

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
MESSAGE("Setting CTEST_SOURCE_DIRECTORY = ${CTEST_SOURCE_DIRECTORY}")

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
  MESSAGE(FATAL_ERROR "Unknown repository type, ${_REPO_TYPE_}, specified in projects file, exiting.")
ENDIF()

# which ctest command to use for running the dashboard
SET(MODEL Experimental)
IF(${CTEST_SCRIPT_ARG} MATCHES Nightly)
  SET(MODEL Nightly)
ENDIF(${CTEST_SCRIPT_ARG} MATCHES Nightly)
IF(${CTEST_SCRIPT_ARG} MATCHES Continuous)
  SET(MODEL Continuous)
ENDIF(${CTEST_SCRIPT_ARG} MATCHES Continuous)
SET (CTEST_COMMAND "${_CTEST_COMMAND_} -V ${_CTEST_CONFIGURATION_OPTIONS_} -D ${MODEL}")

find_program(CTEST_COVERAGE_COMMAND NAMES gcov)
find_program(CTEST_MEMORYCHECK_COMMAND NAMES valgrind)

if(NOT EXISTS "${CTEST_SOURCE_DIRECTORY}")
    set(CTEST_CHECKOUT_COMMAND "${CTEST_REPO_CHECKOUT}")
endif()
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

set(CTEST_CMAKE_GENERATOR "Unix Makefiles")
IF(${_PROJECT_CONFIG_TYPE_} MATCHES "autotools")
  set(CTEST_CONFIGURE_COMMAND            "${CTEST_SOURCE_DIRECTORY}/configure ${_CMAKE_CONFIGURATION_OPTIONS_}")
  set(CTEST_BUILD_COMMAND                "${MAKE} ${_PROJECT_BUILD_OPTIONS_}")
ELSE()
  set(CTEST_CONFIGURE_COMMAND "${CMAKE_COMMAND} ${_CMAKE_CONFIGURATION_OPTIONS_} ${CTEST_SOURCE_DIRECTORY}")
ENDIF()

# =========== EXECUTE THE UPDATE/CONFIGURE/BUILD/TEST ===========

IF(${MODEL} MATCHES Continuous)
  while (${CTEST_ELAPSED_TIME} LESS ${_CONTINUOUS_DURATION_})
    set (START_TIME ${CTEST_ELAPSED_TIME})
    ctest_start (Continuous)
    MESSAGE("\n***** Getting latest source from the repository *****\n")
    if(EXISTS "${CTEST_SOURCE_DIRECTORY}")
      unset(CTEST_CHECKOUT_COMMAND)
    endif()
    ctest_update (RETURN_VALUE count)
    IF(${_PROJECT_CONFIG_TYPE_} MATCHES "autotools")
      configure_file(${CTEST_SOURCE_DIRECTORY}/CMake/CTestTestfile.cmake ${CTEST_BINARY_DIRECTORY}/CTestTestfile.cmake @ONLY)
#      configure_file(${CTEST_SOURCE_DIRECTORY}/CMake/CTestCustom.cmake   ${CTEST_BINARY_DIRECTORY}/CTestCustom.cmake)
      ctest_read_custom_files("${CTEST_BINARY_DIRECTORY}")
    ENDIF()
    ## -- Update git submodules
    IF (EXISTS "${CTEST_SOURCE_DIRECTORY}/.gitmodules")
      message (" -- Updating submodules --")
      execute_process (COMMAND ${GIT_EXECUTABLE} submodule update --init --recursive
  WORKING_DIRECTORY ${CTEST_SOURCE_DIRECTORY})
      execute_process (COMMAND ${GIT_EXECUTABLE} submodule foreach git pull origin master
  WORKING_DIRECTORY ${CTEST_SOURCE_DIRECTORY})
    ENDIF ()
    if (count GREATER 0)
      MESSAGE("\n****** Project Change Detected.... ******\n")
      MESSAGE("\n***** Configuring Project  *****\n")
      IF(${_PROJECT_CONFIG_TYPE_} MATCHES "autotools")
  
        message(" -- AutoGen ${MODEL} - ${CTEST_BUILD_NAME} --")
        message("CTEST_SOURCE_DIRECTORY = ${CTEST_SOURCE_DIRECTORY}")
        execute_process(COMMAND /bin/sh ./autogen.sh
          WORKING_DIRECTORY ${CTEST_SOURCE_DIRECTORY} 
          RESULT_VARIABLE autogenResult 
          OUTPUT_VARIABLE autogenLog 
          ERROR_VARIABLE autogenLog)
        file(WRITE ${CTEST_BINARY_DIRECTORY}/Testing/autogen.log "${autogenLog}")
  
      #  message(" -- Autoreconf ${MODEL} - ${CTEST_BUILD_NAME} --")
      #  execute_process(COMMAND autoreconf -f -i
      #    WORKING_DIRECTORY ${CTEST_SOURCE_DIRECTORY} 
      #    RESULT_VARIABLE autoreconfResult 
      #    OUTPUT_VARIABLE autoreconfLog 
      #    ERROR_VARIABLE autoreconfLog)
      #  file(WRITE ${CTEST_BINARY_DIRECTORY}/Testing/autoreconf.log "${autoreconfLog}")
  
        if( NOT ${autogenResult} )
    
          ## -- Configure
          message(" -- Configure ${MODEL} - ${CTEST_BUILD_NAME} --")
          ctest_configure(BUILD  "${CTEST_BINARY_DIRECTORY}" RETURN_VALUE res)
    
          ## -- BUILD
          IF( res EQUAL 0 )
            message(" -- Build ${MODEL} - ${CTEST_BUILD_NAME} --")
      #      set(CTEST_BUILD_COMMAND "${MAKE} clean")
      #      ctest_build( BUILD "${CTEST_BINARY_DIRECTORY}" RETURN_VALUE res)
      #      set(CTEST_BUILD_COMMAND "${MAKE} ${_PROJECT_BUILD_OPTIONS_}")
            ctest_build(    BUILD  "${CTEST_BINARY_DIRECTORY}" RETURN_VALUE res)
            IF( res EQUAL 0 )
              FOREACH(PROJECT_CUSTOM_TARGET ${PROJECT_CUSTOM_TARGETS})
                IF( res EQUAL 0 )
                  MESSAGE("     -- processing custom target: ${PROJECT_CUSTOM_TARGET}")
                  set(CTEST_BUILD_COMMAND "${MAKE} ${PROJECT_CUSTOM_TARGET}")
                  ctest_build(BUILD "${CTEST_BINARY_DIRECTORY}" RETURN_VALUE res)
                  set(CTEST_BUILD_COMMAND                "${MAKE} ${_PROJECT_BUILD_OPTIONS_}")
                ENDIF()
              ENDFOREACH(PROJECT_CUSTOM_TARGET)
            ENDIF()
          ENDIF()
       
      
          ## -- INSTALL
          IF ( res EQUAL 0 )
            message(" -- Install ${MODEL} - ${CTEST_BUILD_NAME} --")
            execute_process(COMMAND "${MAKE} install ${_PROJECT_BUILD_OPTIONS_}" WORKING_DIRECTORY ${CTEST_BINARY_DIRECTORY} 
              RESULT_VARIABLE makeInstallResult OUTPUT_VARIABLE makeInstallLog ERROR_VARIABLE makeInstallLog)
            file(WRITE ${CTEST_BINARY_DIRECTORY}/Testing/makeinstall.log "${makeInstallLog}")
          ENDIF()
    
          IF( NOT ${makeInstallResult} )
            ## -- TEST
            message(" -- Test ${MODEL} - ${CTEST_BUILD_NAME} --")
            ctest_test(     BUILD  "${CTEST_BINARY_DIRECTORY}" RETURN_VALUE res)
          ENDIF()

        endif( NOT ${autogenResult} )
      ELSE() # ============ For CMake-based project ================
        MESSAGE("\n***** Configuring Project  *****\n")
        ctest_configure(RETURN_VALUE res)
        IF( res EQUAL 0) 
          MESSAGE("\n***** Building Project  *****\n")
          ctest_build(RETURN_VALUE res)
          IF(res EQUAL 0) 
            MESSAGE("\n***** Building Project Custom Targets *****\n")
            FOREACH(PROJECT_CUSTOM_TARGET ${PROJECT_CUSTOM_TARGETS})
              IF(res EQUAL 0) 
                MESSAGE("     -- processing custom target: ${PROJECT_CUSTOM_TARGET}")
                ctest_build(BUILD "${CTEST_BINARY_DIRECTORY}" TARGET ${PROJECT_CUSTOM_TARGET} RETURN_VALUE res)
              ENDIF(res EQUAL 0)
            ENDFOREACH(PROJECT_CUSTOM_TARGET)
          ENDIF(res EQUAL 0)
          IF(res EQUAL 0) 
            MESSAGE("\n***** Running Project Tests *****\n")
            ctest_test()
          ENDIF(res EQUAL 0)
        ENDIF(res EQUAL 0)
      ENDIF()
      
      message("CTEST_COVERAGE_COMMAND ${CTEST_COVERAGE_COMMAND}")
      if (CTEST_COVERAGE_COMMAND)
        MESSAGE("\n***** Checking Project Test Coverage *****\n")
        ctest_coverage()
      endif (CTEST_COVERAGE_COMMAND)
      if (WITH_MEMCHECK AND CTEST_MEMORYCHECK_COMMAND)
        ctest_memcheck()
      endif (WITH_MEMCHECK AND CTEST_MEMORYCHECK_COMMAND)
      MESSAGE("\n***** Submiting Results to CDash *****\n")
      # this is broken for scp transfer since I need to submit outside the script (from the driver)
      if ($ENV{CTEST_DROP_METHOD})
        if (NOT $ENV{CTEST_DROP_METHOD} STREQUAL "scp")
          MESSAGE("\n***** Submiting Results to CDash *****\n")
          ctest_submit()
        endif()
      else()
        ctest_submit()
      endif()
    endif ()
    ctest_sleep( ${START_TIME} ${_CONTINUOUS_INTERVAL_} ${CTEST_ELAPSED_TIME})
  endwhile()
ELSE() # ============= Experimental or Nightly ====================
  ctest_start("${MODEL}")
  MESSAGE("\n***** Getting latest source from the repository *****\n")
  
  IF(EXISTS "${CTEST_SOURCE_DIRECTORY}")
    unset(CTEST_CHECKOUT_COMMAND)
  ENDIF()
  
  ctest_update()
  
# ---- Sets up CTestTestfile.cmake for autotools-based project
  IF(${_PROJECT_CONFIG_TYPE_} MATCHES "autotools")
    configure_file(${CTEST_SOURCE_DIRECTORY}/CMake/CTestTestfile.cmake ${CTEST_BINARY_DIRECTORY}/CTestTestfile.cmake @ONLY)
#    configure_file(${CTEST_SOURCE_DIRECTORY}/CMake/CTestCustom.cmake   ${CTEST_BINARY_DIRECTORY}/CTestCustom.cmake)
    ctest_read_custom_files("${CTEST_BINARY_DIRECTORY}")
  ENDIF()
  
  ## -- Update git submodules
  IF (EXISTS "${CTEST_SOURCE_DIRECTORY}/.gitmodules")
    message (" -- Updating submodules --")
    execute_process (COMMAND ${GIT_EXECUTABLE} submodule update --init --recursive
      WORKING_DIRECTORY ${CTEST_SOURCE_DIRECTORY})
    execute_process (COMMAND ${GIT_EXECUTABLE} submodule foreach git pull origin master
      WORKING_DIRECTORY ${CTEST_SOURCE_DIRECTORY})
  ENDIF ()
  
  IF(${_PROJECT_CONFIG_TYPE_} MATCHES "autotools")

    message(" -- AutoGen ${MODEL} - ${CTEST_BUILD_NAME} --")
    message("CTEST_SOURCE_DIRECTORY = ${CTEST_SOURCE_DIRECTORY}")
    execute_process(COMMAND /bin/sh ./autogen.sh
      WORKING_DIRECTORY ${CTEST_SOURCE_DIRECTORY} 
      RESULT_VARIABLE autogenResult 
      OUTPUT_VARIABLE autogenLog 
      ERROR_VARIABLE autogenLog)
    file(WRITE ${CTEST_BINARY_DIRECTORY}/Testing/autogen.log "${autogenLog}")

#    message(" -- Autoreconf ${MODEL} - ${CTEST_BUILD_NAME} --")
#    execute_process(COMMAND autoreconf -f -i
#      WORKING_DIRECTORY ${CTEST_SOURCE_DIRECTORY} 
#      RESULT_VARIABLE autoreconfResult 
#      OUTPUT_VARIABLE autoreconfLog 
#      ERROR_VARIABLE autoreconfLog)
#    file(WRITE ${CTEST_BINARY_DIRECTORY}/Testing/autoreconf.log "${autoreconfLog}")

    if( NOT ${autogenResult} )

      ## -- Configure
      message(" -- Configure ${MODEL} - ${CTEST_BUILD_NAME} --")
      ctest_configure(BUILD  "${CTEST_BINARY_DIRECTORY}" RETURN_VALUE res)

      ## -- BUILD
      IF(res EQUAL 0) 
        message(" -- Build ${MODEL} - ${CTEST_BUILD_NAME} --")
#  set(CTEST_BUILD_COMMAND "${MAKE} clean")
#  ctest_build( BUILD "${CTEST_BINARY_DIRECTORY}" RETURN_VALUE res)
#  set(CTEST_BUILD_COMMAND "${MAKE} ${_PROJECT_BUILD_OPTIONS_}")
        ctest_build(    BUILD  "${CTEST_BINARY_DIRECTORY}" RETURN_VALUE res)
        MESSAGE("\n***** Building Project Custom Targets *****\n")
        IF(res EQUAL 0) 
          FOREACH(PROJECT_CUSTOM_TARGET ${PROJECT_CUSTOM_TARGETS})
            IF(res EQUAL 0) 
              MESSAGE("     -- processing custom target: ${PROJECT_CUSTOM_TARGET}")
              set(CTEST_BUILD_COMMAND "${MAKE} ${PROJECT_CUSTOM_TARGET}")
              ctest_build(BUILD "${CTEST_BINARY_DIRECTORY}" RETURN_VALUE res)
              set(CTEST_BUILD_COMMAND                "${MAKE} ${_PROJECT_BUILD_OPTIONS_}")
            ENDIF()
          ENDFOREACH(PROJECT_CUSTOM_TARGET)
        ENDIF()
      ENDIF()

      ## -- INSTALL
      IF(res EQUAL 0) 
        message(" -- Install ${MODEL} - ${CTEST_BUILD_NAME} --")
        execute_process(COMMAND "${MAKE} install ${_PROJECT_BUILD_OPTIONS_}" WORKING_DIRECTORY ${CTEST_BINARY_DIRECTORY} 
          RESULT_VARIABLE makeInstallResult OUTPUT_VARIABLE makeInstallLog ERROR_VARIABLE makeInstallLog)
        file(WRITE ${CTEST_BINARY_DIRECTORY}/Testing/makeinstall.log "${makeInstallLog}")
      ENDIF()

      ## -- TEST
      IF(NOT ${makeInstallResult}) 
        message(" -- Test ${MODEL} - ${CTEST_BUILD_NAME} --")
        ctest_test(     BUILD  "${CTEST_BINARY_DIRECTORY}" RETURN_VALUE res)
      ENDIF()

    endif( NOT ${autogenResult} )
    
  ELSE()  # ============= Experimental or Nightly for CMake-based project =========
    
    MESSAGE("\n***** Configuring *****\n")
    ctest_configure(RETURN_VALUE res)
    IF(res EQUAL 0) 
      MESSAGE("\n***** Building Project *****\n")
      ctest_build(RETURN_VALUE res)
      IF(res EQUAL 0) 
        MESSAGE("\n***** Building Project Custom Targets *****\n")
        FOREACH(PROJECT_CUSTOM_TARGET ${PROJECT_CUSTOM_TARGETS})
          IF(res EQUAL 0)
            MESSAGE("     -- processing custom target: ${PROJECT_CUSTOM_TARGET}")
            ctest_build(BUILD "${CTEST_BINARY_DIRECTORY}" TARGET ${PROJECT_CUSTOM_TARGET} RETURN_VALUE res)
          ENDIF()
        ENDFOREACH(PROJECT_CUSTOM_TARGET)
        IF(res EQUAL 0) 
          IF(${_BUILD_DOCUMENTATION_} MATCHES TRUE)
            MESSAGE("\n***** Building documentation *****\n")
            ctest_build(BUILD ${CTEST_BINARY_DIRECTORY} TARGET ${_DOCUMENTATION_TARGET_})
          ENDIF(${_BUILD_DOCUMENTATION_} MATCHES TRUE)
          MESSAGE("\n***** Running Project Tests *****\n")
          ctest_test()
        ENDIF()
      ENDIF()
    ENDIF()
  ENDIF(${_PROJECT_CONFIG_TYPE_} MATCHES "autotools")

  if (CTEST_COVERAGE_COMMAND)
    MESSAGE("\n***** Checking Project Test Coverage *****\n")
    ctest_coverage()
  endif (CTEST_COVERAGE_COMMAND)
  if (WITH_MEMCHECK AND CTEST_MEMORYCHECK_COMMAND)
    ctest_memcheck()
  endif (WITH_MEMCHECK AND CTEST_MEMORYCHECK_COMMAND)
  if ($ENV{CTEST_DROP_METHOD})
    if (NOT $ENV{CTEST_DROP_METHOD} STREQUAL "scp")
      MESSAGE("\n***** Submiting Results to CDash *****\n")
      ctest_submit()
    endif()
  else()
    ctest_submit()
  endif()

ENDIF(${MODEL} MATCHES Continuous)
