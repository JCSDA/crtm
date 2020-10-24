#
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
add_subdirectory(src)
add_subdirectory(test)
# download binary data make the fix/ directory super hacky, needs to be better
message("TTT1: ${CMAKE_CURRENT_SOURCE_DIR}")
execute_process (
    COMMAND bash -c "sh ${CMAKE_CURRENT_SOURCE_DIR}/Get_CRTM_Binary_Files.sh"
    ARGUMENTS "jedi"
    OUTPUT_VARIABLE outVar
)
if (EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/fix")
    execute_process (
        COMMAND bash -c "mv fix/ ${CMAKE_CURRENT_SOURCE_DIR}/."
    )
endif()

## Finalise configuration
# prepares a tar.gz of the sources and/or binaries
ecbuild_install_project( NAME crtm )

# print the summary of the configuration
ecbuild_print_summary()

