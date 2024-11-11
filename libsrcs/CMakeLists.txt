
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/mod)

FILE(GLOB F90SOURCES ${CMAKE_CURRENT_LIST_DIR}/*.f90)
add_library(globals ${F90SOURCES})

target_include_directories(globals INTERFACE
   $<BUILD_INTERFACE:${CMAKE_Fortran_MODULE_DIRECTORY}>
   $<INSTALL_INTERFACE:mod>
)

target_link_libraries(globals PRIVATE ${LAPACK_BLAS_LIBRARIES})

include(GNUInstallDirs)

install(DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/
   DESTINATION mod
   COMPONENT library
)
install(TARGETS globals
   EXPORT globals-targets
   ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
   COMPONENT library
)
