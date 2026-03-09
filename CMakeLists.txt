# Main CMakeLists.txt file to build library "globals"

cmake_minimum_required(VERSION 3.28)

# set the project name
project(globals LANGUAGES Fortran VERSION 1.0)

include(CTest) # this automatically calls enable_testing(), except if BUILD_TESTING=OFF

#####################################################################

# Define list of admissible build types
set(CMAKE_CONFIGURATION_TYPES Release Debug RelWithIEEE)

# Set compiler flags for each build type
set(CMAKE_Fortran_FLAGS_BASE "-fPIC -ffree-line-length-none")
set(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_BASE} -O5")
set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_BASE} -g -DNDEBUG -C -Wall -fcheck=all -O")
set(CMAKE_Fortran_FLAGS_RELWITHIEEE "${CMAKE_Fortran_FLAGS_RELEASE} -fno-unsafe-math-optimizations -frounding-math -fsignaling-nans")

# Build type not defined: set default build type (Release)
if(NOT CMAKE_BUILD_TYPE)
   set(CMAKE_BUILD_TYPE Release)

# Build type not in the list of admissible types: set default (Release)
elseif(NOT CMAKE_BUILD_TYPE IN_LIST CMAKE_CONFIGURATION_TYPES)
   message(WARNING "Unknown CMAKE_BUILD_TYPE '${CMAKE_BUILD_TYPE}', using 'Release'.")
   set(CMAKE_BUILD_TYPE Release)

endif(NOT CMAKE_BUILD_TYPE)

message(STATUS "*** Build type: ${CMAKE_BUILD_TYPE}")

#####################################################################

# Find lapack and blas libraries
include(cmake/lapack_and_blas.cmake)
include(cmake/jsonfortran.cmake)

#####################################################################

# Build "globals" library
add_subdirectory(srcs)
# add_subdirectory(examples)

#####################################################################

# set export directory
set(EXPORT_DIR "${CMAKE_CURRENT_BINARY_DIR}/cmake")

include(CMakePackageConfigHelpers)

# generate the config file that includes the exports
configure_package_config_file(
  "${CMAKE_CURRENT_SOURCE_DIR}/cmake/Config.cmake.in"
  "${EXPORT_DIR}/${PROJECT_NAME}Config.cmake"
  INSTALL_DESTINATION "${CMAKE_INSTALL_LIBDIR}/cmake/${PROJECT_NAME}"
  NO_SET_AND_CHECK_MACRO
  NO_CHECK_REQUIRED_COMPONENTS_MACRO
  )

# generate the version file for the config file
write_basic_package_version_file(
  "${EXPORT_DIR}/${PROJECT_NAME}ConfigVersion.cmake"
  VERSION "${PROJECT_VERSION_MAJOR}.${PROJECT_VERSION_MINOR}"
  COMPATIBILITY AnyNewerVersion
)

# install the generated configuration files
install(FILES
  "${EXPORT_DIR}/${PROJECT_NAME}Config.cmake"
  "${EXPORT_DIR}/${PROJECT_NAME}ConfigVersion.cmake"
  DESTINATION "${CMAKE_INSTALL_LIBDIR}/cmake/${PROJECT_NAME}"
  COMPONENT library
)

# generate the export targets for the build tree
# needs to be after the install(TARGETS) command
export(EXPORT "${PROJECT_NAME}-targets"
   NAMESPACE "${PROJECT_NAME}::"
   FILE "${EXPORT_DIR}/${PROJECT_NAME}Targets.cmake"
)

# install target export files
install(EXPORT "${PROJECT_NAME}-targets"
   NAMESPACE "${PROJECT_NAME}::"
   FILE "${PROJECT_NAME}Targets.cmake"
   DESTINATION "${CMAKE_INSTALL_LIBDIR}/cmake/${PROJECT_NAME}"
   COMPONENT library
)

#####################################################################

option(GLOBALS_BUILD_TESTING "Enable testing for this project" OFF)
if(BUILD_TESTING AND GLOBALS_BUILD_TESTING)
   add_subdirectory(tests)
endif(BUILD_TESTING AND GLOBALS_BUILD_TESTING)
