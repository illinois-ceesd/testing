
FIND_PROGRAM(PYLINTER_PATH flake8)
IF(PYLINTER_PATH-NOTFOUND)
  MESSAGE(STATUS "${BoldYellow}${PROJECT_NAME}: Warning: Python linter not found; Python checking disabled.${ColorReset}")
ELSE()
  IF(NOISY_LINTING)
    set(NOISYOP YES)
  ENDIF()
  GET_FILENAME_COMPONENT(PYLINTER ${PYLINTER_PATH} NAME)
  MESSAGE(STATUS "${Green}${PROJECT_NAME}: Found Python linter: ${PYLINTER_PATH}${ColorReset}")
  MESSAGE(STATUS "${Green}${PROJECT_NAME}: Use 'make checkpy' to check Python.${ColorReset}")
  ADD_CUSTOM_TARGET(
    checkpy
    COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/Checker/checker.sh ${CMAKE_SOURCE_DIR} ${NOISYOP}
    COMMENT "${Cyan}${PROJECT_NAME}: Checking Python style and syntax with ${PYLINTER} ...${ColorReset}"
  )
ENDIF()

IF(BUILD_PROVENANCE)
  set(PROVOP YES)
ENDIF()
add_custom_target(
  pyjuke
  COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/PyJuKe/pyjuke.sh ${CMAKE_CURRENT_SOURCE_DIR} ${PROJECT_BINARY_DIR} ${PROVOP}
  COMMENT "${Cyan}${PROJECT_NAME}: Generating JuKe Python module ...${ColorReset}"
  )

add_custom_target(
  pympiprof
  DEPENDS pyjuke
  COMMAND cp ${CMAKE_SOURCE_DIR}/utils/Profiler/profiler.py ${PROJECT_BINARY_DIR}/PyJuke 
  COMMENT "${Cyan}${PROJECT_NAME}: Installing JuKe Profiler module ...${ColorReset}"
)
