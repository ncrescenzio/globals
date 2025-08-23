# Library is specified by the user
if (LAPACK_LIBRARIES)
   foreach(lib ${LAPACK_LIBRARIES})
      if (NOT EXISTS ${lib})
         message(FATAL_ERROR "Lapack library does not exists: ${lib}")
      endif ()
   endforeach()

# Find directly the library
elseif (LAPACK_LIBRARY_DIR)
   find_library(LAPACK_LIBRARIES
      NAME lapack
      PATHS ${LAPACK_LIBRARY_DIR}
      NO_DEFAULT_PATH REQUIRED)

# Load lapack-config.cmake file
elseif (LAPACK_DIR OR LAPACK_ROOT)
   find_package(LAPACK CONFIG
      PATHS ${LAPACK_DIR} ${LAPACK_ROOT}
      NO_DEFAULT_PATH REQUIRED)

# Use cmake FindLAPACK.cmake module
else ()
   find_package(LAPACK MODULE REQUIRED)

endif ()


# Library is specified by the user
if (BLAS_LIBRARIES)
   foreach(lib ${BLAS_LIBRARIES})
      if (NOT EXISTS ${lib})
         message(FATAL_ERROR "Blas library do not exist: ${lib}")
      endif ()
   endforeach()

# Find directly the library
elseif (BLAS_LIBRARY_DIR)
   find_library(BLAS_LIBRARIES
      NAME blas openblas
      PATHS ${BLAS_LIBRARY_DIR}
      NO_DEFAULT_PATH REQUIRED)

# Load blas-config.cmake file
elseif (BLAS_DIR OR BLAS_ROOT)
   find_package(BLAS CONFIG
      PATHS ${BLAS_DIR} ${BLAS_ROOT}
      NO_DEFAULT_PATH REQUIRED)

# Use cmake FindBLAS.cmake module
else ()
   find_package(BLAS MODULE REQUIRED)

endif ()

list(REMOVE_DUPLICATES LAPACK_LIBRARIES)
list(REMOVE_DUPLICATES BLAS_LIBRARIES)

message(STATUS "*** lapack libraries: ${LAPACK_LIBRARIES}")
message(STATUS "*** blas libraries: ${BLAS_LIBRARIES}")

set(LAPACK_BLAS_LIBRARIES "${LAPACK_LIBRARIES};${BLAS_LIBRARIES}")

message(STATUS "lapack found? ${LAPACK_FOUND}")
message(STATUS "lapack found? ${lapack_FOUND}")
