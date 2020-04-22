
#ADD_TEST(TestSetup ${RUNTEST} -f ${CMAKE_CURRENT_SOURCE_DIR}/Scripts/test_setup.sh -s ${CMAKE_CURRENT_SOURCE_DIR} -b ${CMAKE_BINARY_DIR}/bin -o setup.txt)
FILE(READ ${CMAKE_CURRENT_SOURCE_DIR}/testdirectory.txt testPaths)
STRING(REGEX REPLACE "\n" " " testPathList ${testPaths})
SEPARATE_ARGUMENTS(testPathList)

FOREACH(TEST_SUBDIR ${testPathList})
  
  SET(TEST_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/${TEST_SUBDIR})
  MESSAGE(STATUS "${Blue}Discovering tests in ${TEST_DIRECTORY}...${ColorReset}")
  FILE(READ ${TEST_DIRECTORY}/testlist.txt dirTests)
  STRING(REGEX REPLACE "\n" " " testList ${dirTests})
  SEPARATE_ARGUMENTS(testList)

  FOREACH(testName ${testList})
    IF(SHOWTESTS) 
      MESSAGE(STATUS "${Cyan}\t Adding test: ${testName}${ColorReset}")
    ENDIF()
    ADD_TEST(${testName} python ${TEST_DIRECTORY}/${testName}.py)
    IF(${testName} MATCHES "FAILS")
      SET_TESTS_PROPERTIES(${testName} PROPERTIES WILL_FAIL TRUE)
    ENDIF()
  ENDFOREACH()

ENDFOREACH()
