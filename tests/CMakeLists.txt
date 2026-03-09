if (NOT TARGET "test-drive::test-drive")
   list(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake")
   find_package("test-drive" REQUIRED)
endif()

macro(ADDTEST name)
   add_executable("${name}" "test_${name}.f90")
   target_link_libraries("${name}"
      PRIVATE
      ${GLOBALS_LIBRARY}
      "test-drive::test-drive"
   )
   add_test(NAME "${PROJECT_NAME}/${name}" COMMAND "${name}")
endmacro(ADDTEST)

ADDTEST("kinddeclaration")
