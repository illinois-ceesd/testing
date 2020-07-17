# Global ABaTe testing setup - DO NOT REMOVE
ADD_TEST(TestSetup ${CMAKE_CURRENT_SOURCE_DIR}/PreTest/testsetup.sh ${CMAKE_SOURCE_DIR} ${CMAKE_BINARY_DIR})
IF(SHOWTESTS) 
  MESSAGE(STATUS "${Cyan}${PROJECT_NAME}:\t Adding ABaTe pretest: TestSetup${ColorReset}")
ENDIF()

# Pretests are a user hook allowing stage-in of any testing data (if needed)
# ALL subsquent tests will depend on pretests
# Pretests DO NOT go through the batch system - they are executed on the submit host
# TO ADD PRETESTS:
# Stick your pretest(s) code in:
# <project>/Testing/PreTest/<testname>.{sh,py}
# Stick <testname> into <project>/Testing/PreTest/testlist.txt
SET(PRETESTPATH ${CMAKE_CURRENT_SOURCE_DIR}/PreTest)
MESSAGE(STATUS "${Blue}${PROJECT_NAME}: Checking for pretests in ${PRETESTPATH}...${ColorReset}")
SET(LISTPATH ${PRETESTPATH}/testlist.txt)
IF(EXISTS ${LISTPATH})
  
  FILE(READ ${LISTPATH} testListing)
  STRING(REGEX REPLACE "\n" " " testList ${testListing})
  SEPARATE_ARGUMENTS(testList)

  FOREACH(testName ${testList})
        SET(TEST_NAME PreTest:${testName})
        IF(SHOWTESTS) 
          MESSAGE(STATUS "${Cyan}${PROJECT_NAME}:\t Adding user pretest: ${TEST_NAME}${ColorReset}")
        ENDIF()
        SET(TEST_PYTHON_FILE ${PRETESTPATH}/${testName}.py)
        SET(TEST_SCRIPT_FILE ${PRETESTPATH}/${testName}.sh)
        IF(EXISTS ${TEST_PYTHON_FILE})
          ADD_TEST(${TEST_NAME} python ${TEST_PYTHON_FILE}
            ${CMAKE_SOURCE_DIR} ${CMAKE_BINARY_DIR})
          SET_TESTS_PROPERTIES(${TEST_NAME} PROPERTIES ENVIRONMENT
            "PYTHONPATH=${PROJECT_PYTHON_PATH}:$ENV{PYTHONPATH}")
        ELSEIF(EXISTS ${TEST_SCRIPT_FILE})
          ADD_TEST(${TEST_NAME} ${TEST_SCRIPT_FILE}
            ${CMAKE_SOURCE_DIR} ${CMAKE_BINARY_DIR})
        ENDIF()
        SET_TESTS_PROPERTIES(${TEST_NAME} PROPERTIES DEPENDS TestSetup)
        IF(${testName} MATCHES "FAILS")
          SET_TESTS_PROPERTIES(${TEST_NAME} PROPERTIES WILL_FAIL TRUE)
        ENDIF()
  ENDFOREACH()
  
ENDIF()

# SINGLE TESTS
# The "Single Tests" DO NOT run through the batch systems
# To ADD SINGLE TEST SUITE:
# mkdir <project>/Testing/<suitepath>
# Add <suitepath> to <project>/Testing/testdirectory.txt
# To ADD TESTS TO SINGLE TEST SUITE:
# Add code to implement your test in <project>/Testing/<suitepath>/<testname>.{py,sh}
# Add <testname> to <project>/Testing/<suitepath>/testlist.txt
FILE(READ ${CMAKE_CURRENT_SOURCE_DIR}/testdirectory.txt testPaths)
STRING(REGEX REPLACE "\n" " " testPathList ${testPaths})
SEPARATE_ARGUMENTS(testPathList)

FOREACH(TEST_SUBDIR ${testPathList})
  
  SET(TEST_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/${TEST_SUBDIR})
  IF(IS_DIRECTORY ${TEST_DIRECTORY}) 
    IF(EXISTS ${TEST_DIRECTORY}/testlist.txt )
      MESSAGE(STATUS "${Blue}${PROJECT_NAME}: Discovering single tests in ${TEST_DIRECTORY}...${ColorReset}")
      FILE(READ ${TEST_DIRECTORY}/testlist.txt dirTests)
      STRING(REGEX REPLACE "\n" " " testList ${dirTests})
      SEPARATE_ARGUMENTS(testList)
      
      FOREACH(testName ${testList})
        SET(TEST_NAME ${TEST_SUBDIR}:${testName})
        IF(SHOWTESTS) 
          MESSAGE(STATUS "${Cyan}${PROJECT_NAME}:\t Adding single test: ${TEST_NAME}${ColorReset}")
        ENDIF()
        SET(TEST_PYTHON_FILE ${TEST_DIRECTORY}/${testName}.py)
        SET(TEST_SCRIPT_FILE ${TEST_DIRECTORY}/${testName}.sh)
        IF(EXISTS ${TEST_PYTHON_FILE})
          ADD_TEST(${TEST_NAME} python ${TEST_PYTHON_FILE} ${CMAKE_SOURCE_DIR} ${CMAKE_BINARY_DIR})
          SET_TESTS_PROPERTIES(${TEST_NAME} PROPERTIES ENVIRONMENT
            "PYTHONPATH=${PROJECT_PYTHON_PATH}:$ENV{PYTHONPATH}")
        ELSEIF(EXISTS ${TEST_SCRIPT_FILE})
          ADD_TEST(${TEST_NAME} ${TEST_SCRIPT_FILE} ${CMAKE_SOURCE_DIR} ${CMAKE_BINARY_DIR})
        ENDIF()
        SET_TESTS_PROPERTIES(${TEST_NAME} PROPERTIES DEPENDS TestSetup)
        IF(${testName} MATCHES "FAILS")
          SET_TESTS_PROPERTIES(${TEST_NAME} PROPERTIES WILL_FAIL TRUE)
        ENDIF()
      ENDFOREACH()
    ELSE()
      MESSAGE(STATUS "${BoldYellow}${PROJECT_NAME}: Warning: Single test ${TEST_DIRECTORY}/testlist.txt not found.${ColorReset}")  
    ENDIF()
  ELSE()
    MESSAGE(STATUS "${BoldYellow}${PROJECT_NAME}: Warning: Single test suite ${TEST_DIRECTORY} not found.${ColorReset}")
  ENDIF()
ENDFOREACH()

# SERIAL TESTS
# A batch job (if needed) is created for every SERIAL SUITE.  All tests in the
# suite will be run inside the batch job.
# To ADD SERIAL TEST SUITE:
# mkdir <project>/Testing/Serial/<suitepath>
# Add <suitepath> to <project>/Testing/Serial/serialsuites.txt
# To ADD TESTS TO EXISTING SERIAL TEST SUITE:
# Add code to implement your test in <project>/Testing/Serial/<suitepath>/<testname>.{py,sh}
# Add <testname> to <project>/Testing/Serial/<suitepath>/testlist.txt
set(SERIALTESTPATH ${CMAKE_CURRENT_SOURCE_DIR}/Serial)
set(SERIALSUITELIST ${SERIALTESTPATH}/serialsuites.txt)
IF(EXISTS ${SERIALSUITELIST})

  FILE(READ ${SERIALSUITELIST} suitePaths)
  STRING(REGEX REPLACE "\n" " " suitePathList ${suitePaths})
  SEPARATE_ARGUMENTS(suitePathList)

  FOREACH(SUITE_SUBDIR ${suitePathList})
  
    SET(SUITENAME ${SUITE_SUBDIR})
    SET(SUITE_DIRECTORY "${SERIALTESTPATH}/${SUITE_SUBDIR}")
    SET(SUITE_TEST_LIST ${SUITE_DIRECTORY}/testlist.txt)
    IF(EXISTS ${SUITE_TEST_LIST})
      SET(SETUPNAME "Serial:${SUITENAME}:Setup")
      SET(RUNSUITE "Serial:${SUITENAME}:Run")
      SET(FIXTURENAME "Serial_${SUITENAME}_Fixture")
      SET(RESULTSFILE "${PROJECT_BINARY_DIR}/Testing/serial_${SUITENAME}_results.txt")
      MESSAGE(STATUS "${Blue}${PROJECT_NAME}: Discovering serial test suite in ${SUITE_DIRECTORY}...${ColorReset}")
      ADD_TEST(
        NAME ${SETUPNAME}
        COMMAND python ${PROJECT_PYTHON_SOURCE}/generate_serial_suite_runner.py ${SUITENAME} ${SUITE_DIRECTORY} ${PROJECT_BINARY_DIR}/Testing)
      SET_TESTS_PROPERTIES(${SETUPNAME} PROPERTIES FIXTURES_SETUP ${FIXTURENAME})
      SET_TESTS_PROPERTIES(${SETUPNAME} PROPERTIES ENVIRONMENT
        "PYTHONPATH=${PROJECT_PYTHON_PATH}:$ENV{PYTHONPATH}")
      ADD_TEST(
        NAME ${RUNSUITE}
        COMMAND ${PROJECT_BINARY_DIR}/Testing/serial_${SUITENAME}.sh)
      SET_TESTS_PROPERTIES(${RUNSUITE} PROPERTIES FIXTURES_SETUP ${FIXTURENAME})
      SET_TESTS_PROPERTIES(${RUNSUITE} PROPERTIES DEPENDS ${SETUPNAME})
      #  IF(EXISTS ${SUITE_DIRECTORY}/testlist.txt )
      FILE(READ ${SUITE_TEST_LIST} dirTests)
      STRING(REGEX REPLACE "\n" " " testList ${dirTests})
      SEPARATE_ARGUMENTS(testList)

      FOREACH(testName ${testList})
        SET(FULL_TESTNAME "Serial:${SUITENAME}:${testName}")
        IF(SHOWTESTS) 
          MESSAGE(STATUS "${Cyan}${PROJECT_NAME}:\t Adding serial test: ${FULL_TESTNAME}${ColorReset}")
        ENDIF()
        ADD_TEST(
          NAME ${FULL_TESTNAME}
          COMMAND python ${PROJECT_PYTHON_SOURCE}/CheckTest.py ${testName} ${RESULTSFILE})
        SET_TESTS_PROPERTIES(${FULL_TESTNAME} PROPERTIES ENVIRONMENT
          "PYTHONPATH=${PROJECT_PYTHON_PATH}:$ENV{PYTHONPATH}")
        SET_TESTS_PROPERTIES(${FULL_TESTNAME} PROPERTIES DEPENDS TestSetup)
        SET_TESTS_PROPERTIES(${FULL_TESTNAME} PROPERTIES FIXTURES_REQUIRED ${SETUPNAME})
        IF(${testName} MATCHES "FAILS")
          SET_TESTS_PROPERTIES(${FULL_TESTNAME} PROPERTIES WILL_FAIL TRUE)
        ENDIF()
      ENDFOREACH()    
    ELSE()
      MESSAGE(STATUS "${BoldYellow}${PROJECT_NAME}: Warning: Serial ${SUITE_TEST_LIST} not found.${ColorReset}")  
    ENDIF()
  ENDFOREACH()
ENDIF()

# MPI PARALLEL TESTS - will automatically use a parallel spawner (e.g. mpiexec, srun, jsrun, etc)
# A batch job (if needed) is created for every PARALLEL SUITE.  All tests in the
# suite will be run inside the batch job.
# To ADD PARALLEL TEST SUITE:
# mkdir <project>/Testing/Parallel/<suitepath>
# Add <suitepath> to <project>/Testing/Parallel/parallelsuites.txt
# To ADD TESTS TO EXISTING PARALLEL TEST SUITE:
# Add code to implement your test in <project>/Testing/Parallel/<suitepath>/<testname>.{py,sh}
# Add <testname> to <project>/Testing/Parallel/<suitepath>/testlist.txt
FILE(READ ${CMAKE_CURRENT_SOURCE_DIR}/Parallel/parallelsuites.txt suitePaths)
STRING(REGEX REPLACE "\n" " " suitePathList ${suitePaths})
SEPARATE_ARGUMENTS(suitePathList)

FOREACH(SUITE_SUBDIR ${suitePathList})
  
  SET(SUITENAME ${SUITE_SUBDIR})
  SET(SUITE_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/Parallel/${SUITE_SUBDIR}")
  SET(SETUPNAME "Parallel:${SUITENAME}:Setup")
  SET(RUNSUITE "Parallel:${SUITENAME}:Run")
  SET(FIXTURENAME "Parallel_${SUITENAME}_Fixture")
  SET(RESULTSFILE "${PROJECT_BINARY_DIR}/Testing/parallel_${SUITENAME}_results.txt")
  ADD_TEST(
    NAME ${SETUPNAME}
    COMMAND python ${PROJECT_PYTHON_SOURCE}/generate_parallel_suite_runner.py ${SUITENAME} ${SUITE_DIRECTORY} ${PROJECT_BINARY_DIR}/Testing)
  SET_TESTS_PROPERTIES(${SETUPNAME} PROPERTIES FIXTURES_SETUP ${FIXTURENAME})
  SET_TESTS_PROPERTIES(${SETUPNAME} PROPERTIES ENVIRONMENT
    "PYTHONPATH=${PROJECT_PYTHON_PATH}:$ENV{PYTHONPATH}")
  ADD_TEST(
    NAME ${RUNSUITE}
    COMMAND ${PROJECT_BINARY_DIR}/Testing/parallel_${SUITENAME}.sh)
  SET_TESTS_PROPERTIES(${RUNSUITE} PROPERTIES FIXTURES_SETUP ${FIXTURENAME})
  SET_TESTS_PROPERTIES(${RUNSUITE} PROPERTIES DEPENDS ${SETUPNAME})
  IF(EXISTS ${SUITE_DIRECTORY}/testlist.txt )
    MESSAGE(STATUS "${Blue}${PROJECT_NAME}: Discovering parallel test suite in ${SUITE_DIRECTORY}...${ColorReset}")
    FILE(READ ${SUITE_DIRECTORY}/testlist.txt dirTests)
    STRING(REGEX REPLACE "\n" " " testList ${dirTests})
    SEPARATE_ARGUMENTS(testList)

    FOREACH(testName ${testList})
      SET(FULL_TESTNAME "Parallel:${SUITENAME}:${testName}")
      IF(SHOWTESTS) 
        MESSAGE(STATUS "${Cyan}${PROJECT_NAME}:\t Adding parallel test: ${FULL_TESTNAME}${ColorReset}")
      ENDIF()
      ADD_TEST(
        NAME ${FULL_TESTNAME}
        COMMAND python ${PROJECT_PYTHON_SOURCE}/CheckTest.py ${testName} ${RESULTSFILE})
      SET_TESTS_PROPERTIES(${FULL_TESTNAME} PROPERTIES ENVIRONMENT
        "PYTHONPATH=${PROJECT_PYTHON_PATH}:$ENV{PYTHONPATH}")
        SET_TESTS_PROPERTIES(${FULL_TESTNAME} PROPERTIES DEPENDS TestSetup)
      SET_TESTS_PROPERTIES(${FULL_TESTNAME} PROPERTIES FIXTURES_REQUIRED ${SETUPNAME})
      IF(${testName} MATCHES "FAILS")
        SET_TESTS_PROPERTIES(${FULL_TESTNAME} PROPERTIES WILL_FAIL TRUE)
      ENDIF()
    ENDFOREACH()
  ELSE()
    MESSAGE(STATUS "${BoldYellow}${PROJECT_NAME}: Warning: Parallel ${SUITE_DIRECTORY}/testlist.txt not found.${ColorReset}")  
  ENDIF()
  
ENDFOREACH()

# Posttests are a user hook allowing post-processing of test data (if required/desired)
# These are used for example to produce plots of performace data generated during testing
# Posttests DO NOT go through the batch system; they are executed on the submit host
# TO ADD POSTTESTS:
# Stick your posttest(s) code in:
# <project>/Testing/PostTest/<testname>.{sh,py}
# Stick <testname> into <project>/Testing/PostTest/testlist.txt
SET(POSTTESTPATH ${CMAKE_CURRENT_SOURCE_DIR}/PostTest)
MESSAGE(STATUS "${Blue}${PROJECT_NAME}: Checking for posttests in ${POSTTESTPATH}...${ColorReset}")
SET(LISTPATH ${POSTTESTPATH}/testlist.txt)
IF(EXISTS ${LISTPATH})
  
  FILE(READ ${LISTPATH} testListing)
  STRING(REGEX REPLACE "\n" " " testList ${testListing})
  SEPARATE_ARGUMENTS(testList)

  FOREACH(testName ${testList})
        SET(TEST_NAME PostTest:${testName})
        IF(SHOWTESTS) 
          MESSAGE(STATUS "${Cyan}${PROJECT_NAME}:\t Adding user posttest: ${TEST_NAME}${ColorReset}")
        ENDIF()
        SET(TEST_PYTHON_FILE ${POSTTESTPATH}/${testName}.py)
        SET(TEST_SCRIPT_FILE ${POSTTESTPATH}/${testName}.sh)
        IF(EXISTS ${TEST_PYTHON_FILE})
          ADD_TEST(${TEST_NAME} python ${TEST_PYTHON_FILE}
            ${CMAKE_SOURCE_DIR} ${CMAKE_BINARY_DIR})
          SET_TESTS_PROPERTIES(${TEST_NAME} PROPERTIES ENVIRONMENT
            "PYTHONPATH=${PROJECT_PYTHON_PATH}:$ENV{PYTHONPATH}")
        ELSEIF(EXISTS ${TEST_SCRIPT_FILE})
          ADD_TEST(${TEST_NAME} python ${TEST_SCRIPT_FILE}
            ${CMAKE_SOURCE_DIR} ${CMAKE_BINARY_DIR})
        ENDIF()
        SET_TESTS_PROPERTIES(${TEST_NAME} PROPERTIES DEPENDS TestSetup)
        IF(${testName} MATCHES "FAILS")
          SET_TESTS_PROPERTIES(${TEST_NAME} PROPERTIES WILL_FAIL TRUE)
        ENDIF()
  ENDFOREACH()
  
ENDIF()

IF(SHOWTESTS) 
  MESSAGE(STATUS "${Cyan}${PROJECT_NAME}:\t Adding ABaTe posttest: TestTearDown${ColorReset}")
ENDIF()
ADD_TEST(TestTearDown ${CMAKE_CURRENT_SOURCE_DIR}/PostTest/donetesting.sh ${CMAKE_SOURCE_DIR} ${CMAKE_BINARY_DIR})
