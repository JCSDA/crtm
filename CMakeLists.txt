################################################################################
# CRTM
################################################################################

cmake_minimum_required( VERSION 2.8.4 FATAL_ERROR )

project( crtm C CXX Fortran )

set( CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake" ${CMAKE_MODULE_PATH} )

set( CMAKE_DIRECTORY_LABELS "crtm" )

set( ECBUILD_DEFAULT_BUILD_TYPE Release )
set( ENABLE_OS_TESTS           OFF CACHE BOOL "Disable OS tests" FORCE )
set( ENABLE_LARGE_FILE_SUPPORT OFF CACHE BOOL "Disable testing of large file support" FORCE )

include( ecbuild_system NO_POLICY_SCOPE )

ecbuild_requires_macro_version( 2.5 )

################################################################################
# CRTM project
################################################################################

ecbuild_declare_project()

ecbuild_enable_fortran( REQUIRED )

################################################################################
# Dependencies
################################################################################

# NetCDF
set( NETCDF_F90 ON CACHE BOOL "Compile with Fortran NetCDF" )
find_package( NetCDF REQUIRED )
include_directories( ${NETCDF_INCLUDE_DIR} )

################################################################################
# Export package info
################################################################################

list( APPEND CRTM_TPLS NetCDF )

set( CRTM_INCLUDE_DIRS ${CMAKE_CURRENT_SOURCE_DIR}/libsrc ${CMAKE_CURRENT_BINARY_DIR}/libsrc
  ${CMAKE_Fortran_MODULE_DIRECTORY} )
set( CRTM_LIBRARIES crtm )

get_directory_property( CRTM_DEFINITIONS COMPILE_DEFINITIONS )

foreach( _tpl ${CRTM_TPLS} )
  string( TOUPPER ${_tpl} TPL )
  list( APPEND CRTM_EXTRA_DEFINITIONS   ${${TPL}_DEFINITIONS}  ${${TPL}_TPL_DEFINITIONS}  )
  list( APPEND CRTM_EXTRA_INCLUDE_DIRS  ${${TPL}_INCLUDE_DIRS} ${${TPL}_TPL_INCLUDE_DIRS} )
  list( APPEND CRTM_EXTRA_LIBRARIES     ${${TPL}_LIBRARIES}    ${${TPL}_TPL_LIBRARIES}    )
endforeach()

################################################################################
# Sources
################################################################################

set( CRTM_LINKER_LANGUAGE Fortran )

include( crtm_compiler_flags )
include_directories( ${CRTM_INCLUDE_DIRS} ${CRTM_EXTRA_INCLUDE_DIRS} )
add_subdirectory( libsrc )

if(ECBUILD_INSTALL_FORTRAN_MODULES)
  install(DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/${CMAKE_CFG_INTDIR} DESTINATION ${INSTALL_INCLUDE_DIR} )
endif()

################################################################################
# Finalise configuration
################################################################################

# prepares a tar.gz of the sources and/or binaries
ecbuild_install_project( NAME crtm )

# print the summary of the configuration
ecbuild_print_summary()
