
#ADD_TEST(TestSetup ${RUNTEST} -f ${CMAKE_CURRENT_SOURCE_DIR}/Scripts/test_setup.sh -s ${CMAKE_CURRENT_SOURCE_DIR} -b ${CMAKE_BINARY_DIR}/bin -o setup.txt)
SET(TEST_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/Tests/Environment)
message(STATUS "${Blue}Discovering tests in ${TEST_DIRECTORY}...${ColorReset}")
file(READ ${TEST_DIRECTORY}/testlist.txt dirTests)
string(REGEX REPLACE "\n" " " testList ${dirTests})
separate_arguments(testList)

FOREACH(testName ${testList})
  IF(SHOWTESTS) 
    message(STATUS "${Cyan}\t Adding test: ${testName}${ColorReset}")
  ENDIF()
  ADD_TEST(${testName} python ${TEST_DIRECTORY}/${testName}.py)
  IF(${testName} MATCHES "FAILS")
    SET_TESTS_PROPERTIES(${testName} PROPERTIES WILL_FAIL true)
  ENDIF()
ENDFOREACH()

