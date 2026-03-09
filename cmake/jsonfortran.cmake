set(JSONFORTRAN_LIBRARY "jsonfortran::jsonfortran")

if(NOT TARGET "${JSONFORTRAN_LIBRARY}")

   string(TOLOWER "jsonfortran-${CMAKE_Fortran_COMPILER_ID}" JSONFORTRAN_PACKAGE_NAME)

   set(JSONFORTRAN_FROM_GIT ON CACHE BOOL "Get 'jsonfortran' library from Github")

   if (DEFINED JSONFORTRAN_ROOT)
      set(${JSONFORTRAN_PACKAGE_NAME}_ROOT "${JSONFORTRAN_ROOT}" CACHE STRING "jsonfortran root")
      set(JSONFORTRAN_FROM_GIT OFF)
   elseif (DEFINED jsonfortran_ROOT)
      set(${JSONFORTRAN_PACKAGE_NAME}_ROOT "${jsonfortran_ROOT}" CACHE STRING "jsonfortran root")
      set(JSONFORTRAN_FROM_GIT OFF)
   elseif (DEFINED jsonfortran_DIR)
      set(${JSONFORTRAN_PACKAGE_NAME}_DIR "${jsonfortran_DIR}" CACHE STRING "jsonfortran dir")
      set(JSONFORTRAN_FROM_GIT OFF)
   endif()

   set(JSONFORTRAN_VERSION "9.2.1" CACHE STRING "'jsonfortran' library version")

   if(JSONFORTRAN_FROM_GIT)

      set(JSONFORTRAN_GITHUB_URL "https://github.com/jacobwilliams/json-fortran.git")
      set(JSONFORTRAN_GITHUB_TAG "${JSONFORTRAN_VERSION}")

      message(STATUS "*** Using 'FetchContent' to obtain library 'jsonfortran' from GitHub")
      message(STATUS "*** ${JSONFORTRAN_GITHUB_URL}, version: ${JSONFORTRAN_GITHUB_TAG}")

      include(FetchContent)
      set(JSONFORTRAN_ENABLE_DOC_GENERATION OFF)  # disable generation documentation
      set(JSONFORTRAN_ENABLE_TESTS OFF)           # disable tests
      set(JSONFORTRAN_STATIC_LIBRARY_ONLY ON)     # build only static library
      FetchContent_Declare(
        jsonfortran
        GIT_REPOSITORY ${JSONFORTRAN_GITHUB_URL}
        GIT_TAG ${JSONFORTRAN_GITHUB_TAG}
        GIT_SHALLOW ON
      )
      FetchContent_MakeAvailable(jsonfortran)
      add_library("${JSONFORTRAN_LIBRARY}" ALIAS "jsonfortran")

   else(JSONFORTRAN_FROM_GIT)

      # Try to find "jsonfortran" library using
      # -Djsonfortran_DIR: path to the `*config.cmake` configuration file
      # -Djsonfortran_ROOT and -DJSONFORTRAN_ROOT: path to the root directory where 'jsonfortran' is installed
      # -DJSONFORTRAN_ROOT project name can be written using capital letters since CMake 3.27 (see policy CMP0144)

      find_package(${JSONFORTRAN_PACKAGE_NAME} REQUIRED)
      add_library("${JSONFORTRAN_LIBRARY}" ALIAS "${JSONFORTRAN_PACKAGE_NAME}::jsonfortran")

      message(STATUS "*** Library 'jsonfortran' found: ${${JSONFORTRAN_PACKAGE_NAME}_DIR}")

   endif(JSONFORTRAN_FROM_GIT)

endif(NOT TARGET "${JSONFORTRAN_LIBRARY}")
