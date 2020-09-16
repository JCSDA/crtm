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
add_subdirectory( libsrc )

## Install coefficients to build and install tree
#
# Coefficients will be provided in both the build-tree and the install-tree.
#
# The coefficient files are installed with the exact folder structure of the `fix` subfolders
#
# A `UFOCoeff` folder is also provided as a single flat folder of symlinks into the `fix` subfolders
# for the appropriate architecture endianness.  For efficiency no extra copies of coefficients are created
# at build time or in the install tree, only symlinks are added.
#
# These locations will be exported to crtm dependencies as `crtm_COEFFICIENT_DIR` and `crtm_UFO_COEFFICIENT_DIR`
# with locations set depending on if the CRTM package was found in the build-tree or the install-tree.

#Build-tree location for Coefficients
set(COEFFICIENT_BUILD_DIR ${CMAKE_SOURCE_DIR}/fix)
#Build-tree location for UFOCoeff
set(UFO_COEFFICIENT_BUILD_DIR ${CMAKE_CURRENT_BINARY_DIR}/UFOCoeff)
#Build-tree staging location in which to prepare the install-tree's UFOCoeff symlinks.
set(UFO_COEFFICIENT_PRE_INSTALL_DIR ${CMAKE_CURRENT_BINARY_DIR}/pre-install/UFOCoeff)
#Install-tree (relative) location for crtm share data
set(CRTM_SHARE_INSTALL_DIR ${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME})
#install-tree (relative) location for the UFOCoeff symlinks.
set(UFO_COEFFICIENT_INSTALL_DIR ${CRTM_SHARE_INSTALL_DIR}/UFOCoeff)

#Test endianness of system
include(TestBigEndian)
test_big_endian(IS_BIG_ENDIAN)
if(IS_BIG_ENDIAN)
    message(WARNING "CRTM detected this is a big-endian architecture. \
                     Some coefficient files may not be included by default but may be obtained separately.")
    set(COEFFICIENT_ENDIAN_DIR Big_Endian)
else()
    set(COEFFICIENT_ENDIAN_DIR Little_Endian)
endif()

# Symlink installed coefficients into a common directory for UFO observation operators
# List of all coefficient sub-directories
set(UFO_COEFFICIENT_SRC_DIRS AerosolCoeff/${COEFFICIENT_ENDIAN_DIR}
                             CloudCoeff/${COEFFICIENT_ENDIAN_DIR}
                             EmisCoeff/IR_Ice/SEcategory/${COEFFICIENT_ENDIAN_DIR}
                             EmisCoeff/IR_Land/SEcategory/${COEFFICIENT_ENDIAN_DIR}
                             EmisCoeff/IR_Snow/SEcategory/${COEFFICIENT_ENDIAN_DIR}
                             EmisCoeff/IR_Water/${COEFFICIENT_ENDIAN_DIR}
                             EmisCoeff/MW_Water/${COEFFICIENT_ENDIAN_DIR}
                             EmisCoeff/VIS_Ice/SEcategory/${COEFFICIENT_ENDIAN_DIR}
                             EmisCoeff/VIS_Land/SEcategory/${COEFFICIENT_ENDIAN_DIR}
                             EmisCoeff/VIS_Snow/SEcategory/${COEFFICIENT_ENDIAN_DIR}
                             EmisCoeff/VIS_Water/SEcategory/${COEFFICIENT_ENDIAN_DIR}
                             SpcCoeff/${COEFFICIENT_ENDIAN_DIR}
                             TauCoeff/ODAS/${COEFFICIENT_ENDIAN_DIR}
                             TauCoeff/ODPS/${COEFFICIENT_ENDIAN_DIR})


#Create symlink directory structure (these will run at CMake configure-time)
file(MAKE_DIRECTORY ${UFO_COEFFICIENT_BUILD_DIR})
file(MAKE_DIRECTORY ${UFO_COEFFICIENT_PRE_INSTALL_DIR})
foreach(_dir IN LISTS UFO_COEFFICIENT_SRC_DIRS)
    set(_src_dir ${PROJECT_SOURCE_DIR}/fix/${_dir})
    if(EXISTS ${_src_dir})
        file(GLOB _all_files RELATIVE ${_src_dir} ${_src_dir}/*.bin)
        foreach(_file IN LISTS _all_files)
            #Install coefficient symlinks for build-phase in-bundle testing
            execute_process(COMMAND ${CMAKE_COMMAND} -E create_symlink ${_src_dir}/${_file} ${UFO_COEFFICIENT_BUILD_DIR}/${_file})
            #Install coefficient symlinks for install phase
            execute_process(COMMAND ${CMAKE_COMMAND} -E create_symlink ../fix/${_dir}/${_file} ${UFO_COEFFICIENT_PRE_INSTALL_DIR}/${_file})
        endforeach()
        #Install coefficient `fix` directory preserving structure for appropriate endianness coefficients only
        install(DIRECTORY ${_src_dir}/ DESTINATION ${CRTM_SHARE_INSTALL_DIR}/fix/${_dir})
    else()
        message(WARNING "Missing Coefficients Endianness:${COEFFICIENT_ENDIAN_DIR} At: ${_src_dir}")
    endif()
endforeach()
install(DIRECTORY ${UFO_COEFFICIENT_PRE_INSTALL_DIR} DESTINATION ${CRTM_SHARE_INSTALL_DIR})

## Package Config
# Export variables with build-tree and install-tree locations of coefficients to fill in `crtm-config.cmake.in` template.

ecbuild_install_project( NAME ${PROJECT_NAME} )
#Build Tree exports
set(EXPORT_COEFFICIENT_BUILD_DIR ${PROJECT_SOURCE_DIR}/fix) #build-tree coefficient location
set(EXPORT_UFO_COEFFICIENT_BUILD_DIR ${UFO_COEFFICIENT_BUILD_DIR}) #build-tree coefficient location
#Install Tree exports
set(EXPORT_COEFFICIENT_INSTALL_DIR ${CRTM_SHARE_INSTALL_DIR}/fix) #install-tree coefficient location
set(EXPORT_UFO_COEFFICIENT_INSTALL_DIR ${UFO_COEFFICIENT_INSTALL_DIR}) #install-tree coefficient location
ecbuild_generate_project_config(${PROJECT_NAME}-config.cmake.in
                                PATH_VARS EXPORT_COEFFICIENT_BUILD_DIR   EXPORT_UFO_COEFFICIENT_BUILD_DIR
                                          EXPORT_COEFFICIENT_INSTALL_DIR EXPORT_UFO_COEFFICIENT_INSTALL_DIR)

ecbuild_print_summary()
