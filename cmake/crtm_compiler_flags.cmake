
if( NOT CMAKE_BUILD_TYPE MATCHES "Debug" )
  add_definitions( -DNDEBUG )
endif()

#######################################################################################
# Fortran
#######################################################################################

message(STATUS "Compiler ID: ${CMAKE_Fortran_COMPILER_ID}")
message(STATUS "Compiler: ${CMAKE_Fortran_COMPILER}")

set(CMAKE_FORTRAN_STANDARD 08)
set(CMAKE_FORTRAN_STANDARD_REQUIRED ON)
set(CMAKE_FORTRAN_EXTENSIONS OFF)

if( CMAKE_Fortran_COMPILER_ID MATCHES "GNU" )
  include( compiler_flags_GNU_Fortran )

elseif( CMAKE_Fortran_COMPILER_ID MATCHES "Intel" )
  if( CMAKE_Fortran_COMPILER MATCHES ".*ifx.*" )
    message(STATUS "Detected Intel ifx (LLVM-based) Fortran compiler")
    include( compiler_flags_IntelLLVM_Fortran )  # <-- new file for ifx
  else()
    message(STATUS "Detected Intel ifort (classic) Fortran compiler")
    include( compiler_flags_Intel_Fortran )
  endif()

elseif( CMAKE_Fortran_COMPILER_ID MATCHES "PGI" OR CMAKE_Fortran_COMPILER_ID MATCHES "NVHPC" )
  include( compiler_flags_NVHPC_Fortran )

elseif( CMAKE_Fortran_COMPILER_ID MATCHES "XL" )
  include( compiler_flags_XL_Fortran )

elseif( CMAKE_Fortran_COMPILER_ID MATCHES "Cray" )
  include( compiler_flags_Cray_Fortran )

else()
  message( STATUS "Fortran compiler with ID ${CMAKE_Fortran_COMPILER_ID} will be used with CMake default options")
endif()
