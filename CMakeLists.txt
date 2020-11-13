cmake_minimum_required(VERSION 3.15)

file(STRINGS "VERSION" pVersion)

project(crtm VERSION ${pVersion} LANGUAGES Fortran)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake")

include(GNUInstallDirs)


if(NOT CMAKE_BUILD_TYPE MATCHES "^(Debug|Release|RelWithDebInfo|MinSizeRel)$")
  message(STATUS "Setting build type to 'Release' as none was specified.")
  set(CMAKE_BUILD_TYPE
      "Release"
      CACHE STRING "Choose the type of build." FORCE)
  set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release"
                                               "MinSizeRel" "RelWithDebInfo")
endif()

if(NOT CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel|GNU|Clang|AppleClang)$")
  message(
    WARNING "Compiler not officially supported: ${CMAKE_Fortran_COMPILER_ID}")
endif()

## Dependencies
find_package( OpenMP COMPONENTS Fortran )
find_package(NetCDF REQUIRED Fortran)



add_subdirectory(libsrc)
