# CRTM
# Copyright and License: see LICENSE

cmake_minimum_required( VERSION 3.12 )
project( crtm LANGUAGES Fortran )

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
add_subdirectory( libsrc )
add_subdirectory( test )

ecbuild_install_project( NAME ${PROJECT_NAME} )
ecbuild_print_summary()
