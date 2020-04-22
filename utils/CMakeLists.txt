
FIND_PROGRAM(PYLINTER_PATH flake8)
IF(PYLINTER_PATH-NOTFOUND)
  MESSAGE(STATUS "${Yellow}${PROJECT_NAME}: Warning: Python linter not found; Python checking disabled.${ColorReset}")
ELSE()
  GET_FILENAME_COMPONENT(PYLINTER ${PYLINTER_PATH} NAME)
  MESSAGE(STATUS "${Green}${PROJECT_NAME}: Found Python linter: ${PYLINTER_PATH}${ColorReset}")
  MESSAGE(STATUS "${Green}${PROJECT_NAME}: Use 'make checkpy' to check Python.${ColorReset}")
  ADD_CUSTOM_TARGET(
    checkpy
    COMMAND flake8 ${CMAKE_CURRENT_SOURCE_DIR}
    COMMENT "${Cyan}${PROJECT_NAME}: Checking Python style and syntax with ${PYLINTER} ...${ColorReset}"
  )
ENDIF()
