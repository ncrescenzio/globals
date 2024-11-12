# Main CMakeLists.txt file to build library "globals"

cmake_minimum_required( VERSION 3.10 )

# set the project name
project(globals LANGUAGES Fortran VERSION 1.0)

#####################################################################

# Set default build type
set(CMAKE_BUILD_TYPE "Release" CACHE STRING "Default build configuration" FORCE)

# Set flags for each build type
set(CMAKE_Fortran_FLAGS_BASE "-fPIC -ffree-line-length-none")
set(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_BASE} -O5")
set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_BASE} -g -DNDEBUG -C -Wall -fcheck=all -O")
set(CMAKE_Fortran_FLAGS_RELWITHIEEE "${CMAKE_Fortran_FLAGS_RELEASE} -fno-unsafe-math-optimizations -frounding-math -fsignaling-nans")

# Check if build type is in the list of admissible build types
set(CMAKE_CONFIGURATION_TYPES Release Debug RelWithIEEE)

list(FIND CMAKE_CONFIGURATION_TYPES ${CMAKE_BUILD_TYPE} FOUND_MODE)
if(FOUND_MODE EQUAL -1)
   message(WARNING "Unknown CMAKE_BUILD_TYPE '${CMAKE_BUILD_TYPE}', using 'Release'.")
   set(CMAKE_BUILD_TYPE Release)
endif(FOUND_MODE EQUAL -1)

message(STATUS "Build type: ${CMAKE_BUILD_TYPE}")

#####################################################################

include(cmake/lapack_and_blas.cmake)

#####################################################################

# Build "globals" library
add_subdirectory(libsrcs)

# Build tools
add_subdirectory(tools)

#####################################################################

install(EXPORT globals-targets
   FILE globalsTargets.cmake
   DESTINATION lib/cmake/globals
   COMPONENT library
)

include(CMakePackageConfigHelpers)

# generate the config file that includes the exports
configure_package_config_file(${CMAKE_CURRENT_SOURCE_DIR}/Config.cmake.in
  "${CMAKE_CURRENT_BINARY_DIR}/globalsConfig.cmake"
  INSTALL_DESTINATION "lib/cmake/globals"
  NO_SET_AND_CHECK_MACRO
  NO_CHECK_REQUIRED_COMPONENTS_MACRO
  )

# generate the version file for the config file
write_basic_package_version_file(
  "${CMAKE_CURRENT_BINARY_DIR}/globalsConfigVersion.cmake"
  VERSION "${globals_VERSION_MAJOR}.${globals_VERSION_MINOR}"
  COMPATIBILITY AnyNewerVersion
)

# install the generated configuration files
install(FILES
  ${CMAKE_CURRENT_BINARY_DIR}/globalsConfig.cmake
  ${CMAKE_CURRENT_BINARY_DIR}/globalsConfigVersion.cmake
  DESTINATION lib/cmake/globals
  COMPONENT library
  )

# generate the export targets for the build tree
# needs to be after the install(TARGETS) command
export(EXPORT globals-targets
  FILE "${CMAKE_CURRENT_BINARY_DIR}/globalsTargets.cmake"
)

#####################################################################

# # Add CPack to project
# set(CPACK_PACKAGE_NAME "globals")
# set(CPACK_PACKAGE_VENDOR "UniPD")
# set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "Fortran library for the declaration of global variables and methods")
# set(CPACK_PACKAGE_HOMEPAGE_URL "https://github.com/ncrescenzio/globals")
# set(CPACK_RESOURCE_FILE_README "README.md")
# set(CPACK_PACKAGE_CONTACT "nicolo.crescenzio@math.unipd.it")
# set(CPACK_PACKAGE_VERSION "1.0.0")
# set(CPACK_PACKAGE_VERSION_MAJOR "1")
# set(CPACK_PACKAGE_VERSION_MINOR "0")
# set(CPACK_PACKAGE_VERSION_PATCH "0")
# set(CPACK_GENERATOR "STGZ;TGZ")
# set(CPACK_SOURCE_GENERATOR "TGZ")
# set(CPACK_SOURCE_IGNORE_FILES ".git*;build")
# set(CPACK_PACKAGE_DIRECTORY "packaging") # output directory in which CPack will put the packages (can be overridden using -B)
# set(CPACK_PACKAGE_INSTALL_DIRECTORY "${CPACK_PACKAGE_NAME}.${CPACK_PACKAGE_VERSION}")
#
# include(CPack)
