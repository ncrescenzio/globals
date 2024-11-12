# Main CMakeLists.txt file to build library "globals"

cmake_minimum_required( VERSION 3.10 )

# set the project name
project(globals LANGUAGES Fortran VERSION 1.0)

#####################################################################

# set default
if (NOT DEFINED BUILD_TYPE)
  set (BUILD_TYPE "RELEASE")
endif()

set (CMAKE_Fortran_FLAGS_BASE "-fPIC -ffree-line-length-none")
set (CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_BASE} -O5")
set (CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_BASE} -g -C -Wall -fcheck=all -O")
set (CMAKE_Fortran_FLAGS_RelWithIEEE "${CMAKE_Fortran_FLAGS_RELEASE} -fno-unsafe-math-optimizations -frounding-math -fsignaling-nans")

# set build type (DEBUG/RELEASE)
if ( ${BUILD_TYPE} MATCHES "DEBUG" )
  message("** Compiling in debug mode")
  set ( CMAKE_BUILD_TYPE Debug )
elseif ( ${BUILD_TYPE} MATCHES "RELEASE"   )
  message("** Compiling in release mode")
  set ( CMAKE_BUILD_TYPE RELEASE )
elseif ( ${BUILD_TYPE} MATCHES "IEEE"   )
  message("** Compiling in release with IEEE mode")
  set ( CMAKE_BUILD_TYPE RelWithIEEE )
else ()
  set ( CMAKE_BUILD_TYPE user )
  set ( CMAKE_Fortran_FLAGS_user  "${CMAKE_Fortran_FLAGS_BASE} ${BUILD_TYPE}" )
  message("** Compiling in user mode:  ${CMAKE_Fortran_FLAGS_user}")
endif()

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
