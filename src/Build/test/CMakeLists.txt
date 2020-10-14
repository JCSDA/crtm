
#
#  CMakeLists.txt 
#
#  Description:
#  ============
#  Creates the makefile for the CRTM TL/AD unit tests.
#  The original code was written for CRTM REL-2.4 .
#
#  Record of Revisions:
#  ====================
#
#  Author:             Date:            Description:
#  =======             =====            ============
#  Patrick Stegmann    2020-10-10       Original Code
#  Patrick Stegmann    2020-10-13       PR#109 Bug Fixes
#                                       Linking OMP, netCDF, 
#                                       and libgfortran on macOS
#
#

##################
## Project Setup
##################

cmake_minimum_required (VERSION 3.5)
set(CMAKE_TRY_COMPILE_TARGET_TYPE "STATIC_LIBRARY")
project("TL_test" Fortran)
enable_language (C Fortran)

# Add our local modlues to the module path
SET(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake/Modules/")


##############
# MACROS
##############

function(CREATE_SYMLINK src dst)
    foreach (FILENAME ${ARGN})
        execute_process( COMMAND ${CMAKE_COMMAND} -E create_symlink
            ${src}/${FILENAME}
            ${dst}/${FILENAME} )
        endforeach(FILENAME)
endfunction(CREATE_SYMLINK)

# macro to create a symlink from src to dst with just filename
function(CREATE_SYMLINK_FILENAME src dst)
    foreach (FILENAME ${ARGN})
        get_filename_component(filename ${FILENAME} NAME )
        execute_process( COMMAND ${CMAKE_COMMAND} -E create_symlink
            ${src}/${FILENAME}
            ${dst}/${filename} )
        endforeach(FILENAME)
endfunction(CREATE_SYMLINK_FILENAME)


##############
# LIBRARIES
##############

if(APPLE)
  set(CMAKE_C_COMPILER "/usr/local/Cellar/gcc/10.2.0/bin/gcc-10")
  set(CMAKE_Fortran_COMPILER "/usr/local/Cellar/gcc/10.2.0/bin/gfortran-10")
  set(OPENMP_LIBRARIES "/usr/local/Cellar/libomp/10.0.1/lib")
  set(OPENMP_INCLUDES "/usr/local/Cellar/libomp/10.0.1/include")
endif()

OPTION (USE_OPENMP "Use OpenMP to enamble <omp.h>" ON)
INCLUDE(${CMAKE_MODULE_PATH}/SetParallelizationLibrary.cmake)

# Find OpenMP
if(APPLE AND USE_OPENMP)
  set(OpenMP_C "${CMAKE_C_COMPILER}")
  set(OpenMP_C_FLAGS "-fopenmp -Wno-unused-command-line-argument")
  set(OpenMP_Fortran "${CMAKE_Fortran_COMPILER}")
  set(OpenMP_Fortran_FLAGS "-fopenmp -Wno-unused-command-line-argument")
endif()

if(USE_OPENMP)
  FIND_package(OpenMP_Fortran REQUIRED)
endif(USE_OPENMP)

if (OPENMP_FOUND)
	  message("OPENMP_INCLUDES:" ${OPENMP_INCLUDES})
    message("OpenMP_Fortran_FLAGS:" ${OpenMP_Fortran_FLAGS})
    include_directories("${OPENMP_INCLUDES}")
    link_directories("${OPENMP_LIBRARIES}")
    set (CMAKE_C_FLAGS "${OpenMP_C_FLAGS}")
    set (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${OpenMP_Fortran_FLAGS}")
endif(OPENMP_FOUND)



## Searching the CRTM library
find_library(CRTM
	     NAMES libcrtm.a libcrtm.so crtm CRTM_Module
	     HINTS "$ENV{CRTM_SOURCE_ROOT}/Build/crtm_v2.4.0-alpha/lib")

if(NOT CRTM)
	message(FATAL_ERROR "CRTM library not found!")
endif()

message(${CRTM})

find_package (NetCDF REQUIRED COMPONENTS C Fortran)

include_directories(${NETCDF_INCLUDES})

find_library(NETCDF_LIBRARIES
		NAMES netcdff) 
	
if(NOT NETCDF_LIBRARIES)
	message(FATAL_ERROR "netCDF library not found!")
endif()

message(${NETCDF_LIBRARIES})

#find_library(GFORTRAN_LIBRARY 
#		NAMES libgfortran.a libgfortran.so libgfortran libgfortran.3 libgfortran.3.dylib gfortran
#		HINTS "/usr/local/Cellar/gcc/10.2.0/lib/gcc/10/" )

#if(NOT GFORTRAN_LIBRARY)
#	message(FATAL_ERROR "libgfortran not found!")
#endif()


#find_library(QUADMATH_LIBRARY
#		NAMES libquadmath.a
#		HINTS "/usr/local/Cellar/gcc/10.2.0/lib/gcc/10/" )

#if(NOT QUADMATH_LIBRARY)
#	message(FATAL_ERROR "libquadmath not found!")
#endif()

#link coefficient files into testinput/  modified from JCSDA/crtm CmakeLists.txt 
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

list( APPEND crtm_test_input
Test_Input/ECMWF_5K/Big_Endian/ecmwf_5k_atmosphereccol.bin
Test_Input/ECMWF_5K/Big_Endian/ecmwf_5k_surfaceccol.bin
Test_Input/ECMWF_5K/Big_Endian/ecmwf_5k_geometryccol.bin
AerosolCoeff/Little_Endian/AerosolCoeff.bin
CloudCoeff/Little_Endian/CloudCoeff.bin
EmisCoeff/MW_Water/Little_Endian/FASTEM6.MWwater.EmisCoeff.bin
EmisCoeff/IR_Ice/SEcategory/Little_Endian/NPOESS.IRice.EmisCoeff.bin
EmisCoeff/IR_Land/SEcategory/Little_Endian/NPOESS.IRland.EmisCoeff.bin
EmisCoeff/IR_Snow/SEcategory/Little_Endian/NPOESS.IRsnow.EmisCoeff.bin
EmisCoeff/VIS_Ice/SEcategory/Little_Endian/NPOESS.VISice.EmisCoeff.bin
EmisCoeff/VIS_Land/SEcategory/Little_Endian/NPOESS.VISland.EmisCoeff.bin
EmisCoeff/VIS_Snow/SEcategory/Little_Endian/NPOESS.VISsnow.EmisCoeff.bin
EmisCoeff/VIS_Water/SEcategory/Little_Endian/NPOESS.VISwater.EmisCoeff.bin
EmisCoeff/IR_Water/Little_Endian/Nalli.IRwater.EmisCoeff.bin
EmisCoeff/IR_Land/SEcategory/Little_Endian/USGS.IRland.EmisCoeff.bin
EmisCoeff/VIS_Land/SEcategory/Little_Endian/USGS.VISland.EmisCoeff.bin
SpcCoeff/Little_Endian/hirs4_metop-a.SpcCoeff.bin
TauCoeff/ODPS/Little_Endian/hirs4_metop-a.TauCoeff.bin
SpcCoeff/Little_Endian/amsua_n19.SpcCoeff.bin
TauCoeff/ODPS/Little_Endian/amsua_n19.TauCoeff.bin
SpcCoeff/Little_Endian/amsua_metop-a.SpcCoeff.bin
TauCoeff/ODPS/Little_Endian/amsua_metop-a.TauCoeff.bin
SpcCoeff/Little_Endian/gmi_gpm.SpcCoeff.bin
TauCoeff/ODPS/Little_Endian/gmi_gpm.TauCoeff.bin
SpcCoeff/Little_Endian/seviri_m08.SpcCoeff.bin
TauCoeff/ODAS/Little_Endian/seviri_m08.TauCoeff.bin
SpcCoeff/Little_Endian/cris-fsr_npp.SpcCoeff.bin
TauCoeff/ODPS/Little_Endian/cris-fsr_npp.TauCoeff.bin
SpcCoeff/Little_Endian/iasi_metop-a.SpcCoeff.bin
TauCoeff/ODPS/Little_Endian/iasi_metop-a.TauCoeff.bin
SpcCoeff/Little_Endian/iasi_metop-b.SpcCoeff.bin
TauCoeff/ODPS/Little_Endian/iasi_metop-b.TauCoeff.bin
SpcCoeff/Little_Endian/mhs_n19.SpcCoeff.bin
TauCoeff/ODPS/Little_Endian/mhs_n19.TauCoeff.bin
SpcCoeff/Little_Endian/sndrD1_g15.SpcCoeff.bin
TauCoeff/ODPS/Little_Endian/sndrD1_g15.TauCoeff.bin
SpcCoeff/Little_Endian/sndrD2_g15.SpcCoeff.bin
TauCoeff/ODPS/Little_Endian/sndrD2_g15.TauCoeff.bin
SpcCoeff/Little_Endian/sndrD3_g15.SpcCoeff.bin
TauCoeff/ODPS/Little_Endian/sndrD3_g15.TauCoeff.bin
SpcCoeff/Little_Endian/sndrD4_g15.SpcCoeff.bin
TauCoeff/ODPS/Little_Endian/sndrD4_g15.TauCoeff.bin
SpcCoeff/Little_Endian/airs_aqua.SpcCoeff.bin
TauCoeff/ODPS/Little_Endian/airs_aqua.TauCoeff.bin
SpcCoeff/Little_Endian/modis_aqua.SpcCoeff.bin
TauCoeff/ODPS/Little_Endian/modis_aqua.TauCoeff.bin
SpcCoeff/Little_Endian/cris399_npp.SpcCoeff.bin
TauCoeff/ODPS/Little_Endian/cris399_npp.TauCoeff.bin
SpcCoeff/Little_Endian/crisB1_npp.SpcCoeff.bin
TauCoeff/ODPS/Little_Endian/crisB1_npp.TauCoeff.bin
SpcCoeff/Little_Endian/atms_npp.SpcCoeff.bin
TauCoeff/ODPS/Little_Endian/atms_npp.TauCoeff.bin
SpcCoeff/Little_Endian/v.viirs-m_npp.SpcCoeff.bin
TauCoeff/ODAS/Little_Endian/v.viirs-m_npp.TauCoeff.bin
SpcCoeff/Little_Endian/v.abi_gr.SpcCoeff.bin
TauCoeff/ODAS/Little_Endian/v.abi_gr.TauCoeff.bin
TauCoeff/ODPS/Little_Endian/zssmis_f20.TauCoeff.bin
TauCoeff/ODPS/Little_Endian/zssmis_f19.TauCoeff.bin
TauCoeff/ODPS/Little_Endian/zssmis_f18.TauCoeff.bin
TauCoeff/ODPS/Little_Endian/zssmis_f17.TauCoeff.bin
TauCoeff/ODPS/Little_Endian/zssmis_f16.TauCoeff.bin
TauCoeff/ODPS/Little_Endian/ssmis_f20.TauCoeff.bin
TauCoeff/ODPS/Little_Endian/ssmis_f19.TauCoeff.bin
TauCoeff/ODPS/Little_Endian/ssmis_f18.TauCoeff.bin
TauCoeff/ODPS/Little_Endian/ssmis_f17.TauCoeff.bin
TauCoeff/ODPS/Little_Endian/ssmis_f16.TauCoeff.bin
SpcCoeff/Little_Endian/ssmis_f20.SpcCoeff.bin
SpcCoeff/Little_Endian/ssmis_f18.SpcCoeff.bin
SpcCoeff/Little_Endian/ssmis_f19.SpcCoeff.bin
SpcCoeff/Little_Endian/ssmis_f16.SpcCoeff.bin
SpcCoeff/Little_Endian/ssmis_f17.SpcCoeff.bin
TauCoeff/ODPS/Little_Endian/ssu_n06.TauCoeff.bin
TauCoeff/ODPS/Little_Endian/ssu_n07.TauCoeff.bin
TauCoeff/ODPS/Little_Endian/ssu_n08.TauCoeff.bin
TauCoeff/ODPS/Little_Endian/ssu_n09.TauCoeff.bin
TauCoeff/ODPS/Little_Endian/ssu_n11.TauCoeff.bin
TauCoeff/ODPS/Little_Endian/ssu_n14.TauCoeff.bin
SpcCoeff/Little_Endian/ssu_n06.SpcCoeff.bin
SpcCoeff/Little_Endian/ssu_n07.SpcCoeff.bin
SpcCoeff/Little_Endian/ssu_n08.SpcCoeff.bin
SpcCoeff/Little_Endian/ssu_n09.SpcCoeff.bin
SpcCoeff/Little_Endian/ssu_n11.SpcCoeff.bin
SpcCoeff/Little_Endian/ssu_n14.SpcCoeff.bin
)

# Symlink all CRTM files (creates links in testinput, may want to modify to Data to be consistent with other projects)
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/testinput)
CREATE_SYMLINK_FILENAME( $ENV{CRTM_FIXFILE_ROOT}
                         ${CMAKE_CURRENT_BINARY_DIR}/testinput
                         ${crtm_test_input} )

#### start add_executable block (this compiles) ####

add_executable(TEST_TL mains/unit/Unit_Test/test_TL.f90)

set_target_properties(TEST_TL PROPERTIES RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/mains/unit/Unit_Test")

target_link_libraries(TEST_TL 
                      ${CRTM} 
											${NETCDF_LIBRARIES} 
											${OpenMP_Fortran_LIBRARIES})
#had to specifically include the "include" directory for CRTM, probably a better way to do this:
target_include_directories(TEST_TL PRIVATE $ENV{CRTM_SOURCE_ROOT}/Build/crtm_v2.4.0-alpha/include)

#### end add_executable block ####


#### start add_test block ####

enable_testing ()
add_test (NAME Mytest1 COMMAND $<TARGET_FILE:TEST_TL>)

#### end add_test block ####

#STOP HERE FOR NOW
return()




################
# Functions
################


## OS-independent name-mangling to connect Fortran test functions
## to the main.c driver executable.
function (mangle_fortran_name CNAME FNAME)
    set (TMP)
    if (WIN32)
        string (TOUPPER "${FNAME}" TMP)
    else ()
        string (TOLOWER "${FNAME}_" TMP)
    endif ()
    set (${CNAME} ${TMP} PARENT_SCOPE)
endfunction ()

## Mangling Fortran file name lists.
function (mangle_fortran_filename_list MANGLED)
    set (TMP)
    foreach (TFILE ${ARGN})
        string (REGEX REPLACE ".f90$" "" TESTNAME ${TFILE})
        mangle_fortran_name (C_TESTNAME ${TESTNAME})
        list (APPEND TMP ${C_TESTNAME})
    endforeach ()
    set (${MANGLED} ${TMP} PARENT_SCOPE)
endfunction()

## Modified function to add a Fortran test.
function (add_fortran_test_executable TARGET)
    set (TEST_FILES ${ARGN})
    mangle_fortran_filename_list (TEST_FILES_MANGLED ${TEST_FILES})

    create_test_sourcelist (_ main.c ${TEST_FILES_MANGLED})

    add_library (${TARGET}_fortran ${TEST_FILES})
    add_executable (${TARGET} main.c)
    target_link_libraries (${TARGET} ${TARGET}_fortran)
#    target_link_libraries (${TARGET} ${QUADMATH_LIBRARY})
		message("everlong: " "$ENV{CRTM_SOURCE_ROOT}/Build/crtm_v2.4.0-alpha/include")
    target_include_directories(${TARGET}_fortran PUBLIC "$ENV{CRTM_SOURCE_ROOT}/Build/crtm_v2.4.0-alpha/include")

    target_link_libraries(${TARGET}_fortran
                        ${CRTM} 
			${NETCDF_LIBRARIES} 
			${OpenMP_Fortran_LIBRARIES})

    set (INDEX 0)
    list (LENGTH TEST_FILES LEN)
    while (${LEN} GREATER ${INDEX})
        list (GET TEST_FILES ${INDEX} TEST_FILE)
        list (GET TEST_FILES_MANGLED ${INDEX} TEST_FILE_MANGLED)
        add_test (
            NAME ${TEST_FILE}
            COMMAND $<TARGET_FILE:${TARGET}> ${TEST_FILE_MANGLED})
        math (EXPR INDEX "${INDEX} + 1")
    endwhile ()
endfunction ()
#add_subdirectory(mains/unit/Unit_Test)
## Adding Fortran test source code.

add_fortran_test_executable (
    TL_TEST
    test_TL.f90
		)


