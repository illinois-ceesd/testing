message("Building Kernel Documentation")

FIND_PACKAGE(Doxygen)
IF(NOT DOXYGEN_FOUND)
  MESSAGE(WARNING "${PROJECT_NAME}: Doxygen is not found, documentation cannot be built.")
ENDIF()

# add documentation for TPL's
#set(IX-DocDir ${CMAKE_SOURCE_DIR}/IX)
#set(PCPP-DocDir ${CMAKE_SOURCE_DIR}/PCPP)
#if(CS-Tools)
#  message("Building CS-Tool Documentation")
#  set(CS-ToolDocDir ${CMAKE_SOURCE_DIR}/CS-Tools)
#endif()

#  FILE(COPY ${CMAKE_CURRENT_SOURCE_DIR}/doc DESTINATION ${CMAKE_CURRENT_BINARY_DIR})
  
  
SET( doxyfile_in          ${CMAKE_CURRENT_SOURCE_DIR}/${PROJECT_NAME}.cfg.in )
SET( doxyfile             ${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}.cfg        )
SET( docdir               ${CMAKE_CURRENT_BINARY_DIR}/docs                )

CONFIGURE_FILE( ${doxyfile_in} ${doxyfile} @ONLY )

ADD_CUSTOM_COMMAND( OUTPUT ${docdir}
                    COMMAND ${DOXYGEN_EXECUTABLE} ${doxyfile}
                    MAIN_DEPENDENCY ${doxyfile} ${doxyfile_in}
                    DEPENDS OK ${doxy_extra_files} ${CMAKE_CURRENT_SOURCE_DIR}/*.dox
                    COMMENT "${PROJECT_NAME}: Generating documentation")

ADD_CUSTOM_TARGET( documentation DEPENDS ${docdir} )

INSTALL( DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/docs DESTINATION share/${PROJECT_NAME} OPTIONAL)


