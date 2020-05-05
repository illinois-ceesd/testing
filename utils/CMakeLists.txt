
FIND_PROGRAM(PYLINTERPATH flake8)
IF(NOT PYLINTERPATH)
  MESSAGE(STATUS "${BoldYellow}${PROJECT_NAME}: Warning: Python linter not found; Python checking disabled.${ColorReset}")
ELSE()
  IF(NOISY_LINTING)
    set(NOISYOP YES)
  ENDIF()
  GET_FILENAME_COMPONENT(PYLINTER ${PYLINTERPATH} NAME)
  MESSAGE(STATUS "${Green}${PROJECT_NAME}: Found Python linter: ${PYLINTERPATH}${ColorReset}")
  MESSAGE(STATUS "${Green}${PROJECT_NAME}: Use 'make checkpy' to check Python.${ColorReset}")
  ADD_CUSTOM_TARGET(
    checkpy
    COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/Checker/checker.sh ${CMAKE_SOURCE_DIR} ${NOISYOP}
    COMMENT "${Cyan}${PROJECT_NAME}: Checking Python style and syntax with ${PYLINTER} ...${ColorReset}"
  )
  MESSAGE(STATUS "${Green}${PROJECT_NAME}: Use 'make strictpy' to strictly check Python.${ColorReset}")
  ADD_CUSTOM_TARGET(
    strictpy
    COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/Checker/checker.sh ${CMAKE_SOURCE_DIR} "STRICT"
    COMMENT "${Cyan}${PROJECT_NAME}: Checking Python style and syntax with ${PYLINTER} ...${ColorReset}"
  )
ENDIF()

IF(BUILD_PROVENANCE)
  MESSAGE(STATUS "${Green}${PROJECT_NAME}: Enabling GIT provenance.${ColorReset}")
  set(PROVOP YES)
ELSE()
  MESSAGE(STATUS "${BoldYellow}${PROJECT_NAME}: GIT provenance is OFF.${ColorReset}")
ENDIF()

add_custom_target(
  pyjuke
  COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/PyJuKe/pyjuke.sh ${CMAKE_SOURCE_DIR} ${PROJECT_BINARY_DIR} ${PROVOP}
  COMMENT "${Cyan}${PROJECT_NAME}: Generating JuKe Python module ...${ColorReset}"
  )
