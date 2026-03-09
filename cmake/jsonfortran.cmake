set(JSONFORTRAN_LIBRARY "jsonfortran::jsonfortran")

if(NOT TARGET "${JSONFORTRAN_LIBRARY}")

   set(JSONFORTRAN_SUBPROJECT "external/json-fortran")
   set(JSONFORTRAN_SOURCE_DIR "${PROJECT_SOURCE_DIR}/${JSONFORTRAN_SUBPROJECT}")
   set(JSONFORTRAN_BINARY_DIR "${PROJECT_BINARY_DIR}/${JSONFORTRAN_SUBPROJECT}")

   set(JSONFORTRAN_ENABLE_DOC_GENERATION OFF)  # disable generation documentation
   set(JSONFORTRAN_ENABLE_TESTS OFF)           # disable tests
   set(JSONFORTRAN_STATIC_LIBRARY_ONLY ON)     # build only static library
   add_subdirectory(
      ${JSONFORTRAN_SOURCE_DIR}
      ${JSONFORTRAN_BINARY_DIR}
      EXCLUDE_FROM_ALL
   )

   add_library("${JSONFORTRAN_LIBRARY}" ALIAS "jsonfortran")

   set(JSONFORTRAN_MOD_DIR "${JSONFORTRAN_BINARY_DIR}/mod" CACHE STRING "" FORCE)
   message(STATUS "*** ${JSONFORTRAN_MOD_DIR}")

   set_target_properties("jsonfortran"
      PROPERTIES
      Fortran_MODULE_DIRECTORY ${JSONFORTRAN_MOD_DIR}
   )

   target_include_directories("jsonfortran" INTERFACE
      $<BUILD_INTERFACE:${JSONFORTRAN_MOD_DIR}>
      $<INSTALL_INTERFACE:mod>
   )

   include(GNUInstallDirs)

   install(DIRECTORY ${JSONFORTRAN_MOD_DIR}/
      DESTINATION mod
      COMPONENT library
      FILES_MATCHING PATTERN "*.mod"
   )

   install(TARGETS "jsonfortran"
      EXPORT ${PROJECT_NAME}-targets
      ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
      LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
      RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
      COMPONENT library
   )

   # string(TOLOWER "jsonfortran-${CMAKE_Fortran_COMPILER_ID}" JSONFORTRAN_PACKAGE_NAME)
   #
   # set(JSONFORTRAN_FROM_GIT ON CACHE BOOL "Get 'jsonfortran' library from Github")
   #
   # if (DEFINED JSONFORTRAN_ROOT)
   #    set(${JSONFORTRAN_PACKAGE_NAME}_ROOT "${JSONFORTRAN_ROOT}" CACHE STRING "jsonfortran root")
   #    set(JSONFORTRAN_FROM_GIT OFF)
   # elseif (DEFINED jsonfortran_ROOT)
   #    set(${JSONFORTRAN_PACKAGE_NAME}_ROOT "${jsonfortran_ROOT}" CACHE STRING "jsonfortran root")
   #    set(JSONFORTRAN_FROM_GIT OFF)
   # elseif (DEFINED jsonfortran_DIR)
   #    set(${JSONFORTRAN_PACKAGE_NAME}_DIR "${jsonfortran_DIR}" CACHE STRING "jsonfortran dir")
   #    set(JSONFORTRAN_FROM_GIT OFF)
   # endif()
   #
   # set(JSONFORTRAN_VERSION "9.2.1" CACHE STRING "'jsonfortran' library version")
   #
   # if(JSONFORTRAN_FROM_GIT)
   #
   #    set(JSONFORTRAN_GITHUB_URL "https://github.com/jacobwilliams/json-fortran.git")
   #    set(JSONFORTRAN_GITHUB_TAG "${JSONFORTRAN_VERSION}")
   #
   #    message(STATUS "*** Using 'FetchContent' to obtain library 'jsonfortran' from GitHub")
   #    message(STATUS "*** ${JSONFORTRAN_GITHUB_URL}, version: ${JSONFORTRAN_GITHUB_TAG}")
   #
   #    include(FetchContent)
   #    set(JSONFORTRAN_ENABLE_DOC_GENERATION OFF)  # disable generation documentation
   #    set(JSONFORTRAN_ENABLE_TESTS OFF)           # disable tests
   #    set(JSONFORTRAN_STATIC_LIBRARY_ONLY ON)     # build only static library
   #    FetchContent_Declare(
   #      jsonfortran
   #      GIT_REPOSITORY ${JSONFORTRAN_GITHUB_URL}
   #      GIT_TAG ${JSONFORTRAN_GITHUB_TAG}
   #      GIT_SHALLOW ON
   #    )
   #    FetchContent_MakeAvailable(jsonfortran)
   #    add_library("${JSONFORTRAN_LIBRARY}" ALIAS "jsonfortran")
   #
   # else(JSONFORTRAN_FROM_GIT)
   #
   #    # Try to find "jsonfortran" library using
   #    # -Djsonfortran_DIR: path to the `*config.cmake` configuration file
   #    # -Djsonfortran_ROOT and -DJSONFORTRAN_ROOT: path to the root directory where 'jsonfortran' is installed
   #    # -DJSONFORTRAN_ROOT project name can be written using capital letters since CMake 3.27 (see policy CMP0144)
   #
   #    find_package(${JSONFORTRAN_PACKAGE_NAME} REQUIRED)
   #    add_library("${JSONFORTRAN_LIBRARY}" ALIAS "${JSONFORTRAN_PACKAGE_NAME}::jsonfortran")
   #
   #    message(STATUS "*** Library 'jsonfortran' found: ${${JSONFORTRAN_PACKAGE_NAME}_DIR}")
   #
   # endif(JSONFORTRAN_FROM_GIT)

endif(NOT TARGET "${JSONFORTRAN_LIBRARY}")
