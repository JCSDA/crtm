#
# CRTM
# Copyright and License: see LICENSE

cmake_minimum_required( VERSION 3.12 )
project( crtm VERSION 2.4.1 LANGUAGES Fortran )

## Ecbuild integration
find_package( ecbuild QUIET )
include( ecbuild_system NO_POLICY_SCOPE )
ecbuild_declare_project()
list( APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake )
set( CMAKE_DIRECTORY_LABELS ${PROJECT_NAME} )

## Configuration options
include( ${PROJECT_NAME}_compiler_flags )
include(GNUInstallDirs)

## Dependencies
find_package( OpenMP COMPONENTS Fortran )
find_package( NetCDF REQUIRED COMPONENTS Fortran )

## Sources
add_subdirectory(src)
add_subdirectory(test)


## Finalise configuration
# prepares a tar.gz of the sources and/or binaries
ecbuild_install_project( NAME crtm )

# print the summary of the configuration
ecbuild_print_summary()

include(${CMAKE_CURRENT_SOURCE_DIR}/cmake/cdash-integration.cmake)
include(CTest)
