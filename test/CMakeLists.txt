# (C) Copyright 2017-2018 UCAR.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

cmake_minimum_required (VERSION 3.12)
project("CRTM_Tests" VERSION 1.0.1 LANGUAGES Fortran C)
enable_testing ()

## Ecbuild integration
find_package( ecbuild QUIET )
include( ecbuild_system NO_POLICY_SCOPE )
ecbuild_declare_project()
list( APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake )
set( CMAKE_DIRECTORY_LABELS ${PROJECT_NAME} )

# macro to create a symlink from src to dst
set(CMAKE_VERBOSE_MAKEFILE ON)

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

# Create Data directory for test input config and symlink all files
list( APPEND crtm_test_input
      testinput/single_profile.yaml
)

file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/testinput)
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/results)
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/results/adjoint)
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/results/tangent_linear)
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/results/k_matrix)
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/results/forward)
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/results/unit)
CREATE_SYMLINK( ${CMAKE_CURRENT_SOURCE_DIR} ${CMAKE_CURRENT_BINARY_DIR} ${crtm_test_input} )


if( DEFINED ENV{LOCAL_PATH_JEDI_TESTFILES})
    set(LOCAL_PATH_JEDI_TESTFILES "$ENV{LOCAL_PATH_JEDI_TESTFILES}")
endif()

if ( DEFINED ENV{CRTM_TEST_ROOT})
    set(CRTM_TEST_ROOT "$ENV{CRTM_TEST_ROOT}")
else()
    set(CRTM_TEST_ROOT ${CMAKE_CURRENT_SOURCE_DIR})
endif()

set( CRTM_COEFFS_BRANCH "2.4.1" )

# If local path to testfiles is defined don't download
#if( DEFINED LOCAL_PATH_JEDI_TESTFILES )
IF(EXISTS  ${CMAKE_SOURCE_DIR}/fix)
  set( CRTM_COEFFS_PATH ${CMAKE_SOURCE_DIR}/fix )
  message(STATUS "use LOCAL_PATH_JEDI_TESTFILES: ${CRTM_COEFFS_PATH}")
  message("Using existing local fix directory instead of downloading.")
# Download CRTM coefficients
else()
  set( CRTM_COEFFS_PATH ${CMAKE_BINARY_DIR}/test_data/crtm/${CRTM_COEFFS_BRANCH})
  file(MAKE_DIRECTORY ${CRTM_COEFFS_PATH})
  set( ECBUILD_DOWNLOAD_BASE_URL https://gdex.ucar.edu/dataset/jedi-skylab/file )
  set( test_files_dirname crtm_coefficients_${CRTM_COEFFS_BRANCH}.tar.gz )
  set( checksum "0")
  message(STATUS "download CRTM coeffs files from: ${ECBUILD_DOWNLOAD_BASE_URL} to ${CRTM_COEFFS_PATH}")

  list( APPEND CRTM_DATA_DOWNLOADER_ARGS
                ${ECBUILD_DOWNLOAD_BASE_URL}
                ${CMAKE_BINARY_DIR}/test_data
                ${test_files_dirname}
                ${checksum} )

  # Create download script for ufo_get_*_test_data test
  set ( FILENAME crtm_data_downloader.py)
  set ( SOURCE_FILE ${CMAKE_CURRENT_SOURCE_DIR}/${FILENAME} )
  set ( DEST_FILE ${CMAKE_BINARY_DIR}/bin/${FILENAME} )

  if( EXISTS "${SOURCE_FILE}.in" )
    configure_file( ${SOURCE_FILE}.in ${DEST_FILE} @ONLY )
  else()
    configure_file( ${SOURCE_FILE}    ${DEST_FILE} @ONLY )
  endif()

  # add test for downloading data
  ecbuild_add_test( TARGET    get_crtm_coeffs
                    TYPE      SCRIPT
                    COMMAND   ${CMAKE_BINARY_DIR}/bin/crtm_data_downloader.py
                    ARGS      ${CRTM_DATA_DOWNLOADER_ARGS} )
endif()

# Add files to cmake resources
ecbuild_add_resources( TARGET crtm_test_scripts
                       SOURCES_PACK
                       ${crtm_test_input}
                     )

# Create list of sensor ids for testing
list( APPEND Simple_Sensor_Ids
        atms_npp
        cris399_npp
        v.abi_gr
	abi_g18
        modis_aqua
)

list( APPEND ScatteringSwitch_Sensor_Ids
        atms_npp
        cris399_npp
        v.abi_gr
	abi_g18
        modis_aqua
)

list( APPEND SOI_Sensor_Ids
        atms_npp
        cris399_npp
        v.abi_gr
	abi_g18
        modis_aqua
)

list( APPEND VerticalCoordinates_Sensor_Ids
        atms_npp
        cris399_npp
        v.abi_gr
	abi_g18
        modis_aqua
)

list( APPEND SSU_Sensor_Ids
        ssu_n06
        ssu_n14
)

list( APPEND ClearSky_Sensor_Ids
        atms_npp
        cris399_npp
        v.abi_gr
	abi_g18
        modis_aqua
)

# Create list of sensor ids for testing
list( APPEND AOD_Sensor_Ids
        cris399_npp
        v.abi_gr
	abi_g18
        airs_aqua
)

# Create list of sensor ids for testing
list( APPEND Zeeman_Sensor_Ids
      ssmis_f20
      ssmis_f19
      ssmis_f18
      ssmis_f17
      ssmis_f16
)


list( APPEND ChannelSubset_Sensor_Ids
      iasi_metop-b
)

list( APPEND Aircraft_Sensor_Ids
      crisB1_npp
)

list( APPEND OMPoverChannels_Sensor_Ids
        atms_npp
)

list (APPEND common_tests
     Simple
     AOD
     Zeeman
     ChannelSubset
     ClearSky
     Aircraft
     ScatteringSwitch
     SOI
     SSU
     VerticalCoordinates
)

list (APPEND omp_tests
     OMPoverChannels
)


list (APPEND regression_types
     forward
     k_matrix
)

list (APPEND TLAD_types
     adjoint
     tangent_linear
)

list (APPEND TLAD_tests
     Simple
     ClearSky
)


#####################################################################

ecbuild_add_test( TARGET  test_check_crtm
                  SOURCES mains/application/check_crtm.F90
                  ARGS    "testinput/single_profile.yaml"
                  OMP     $ENV{OMP_NUM_THREADS}
                  LIBS    crtm
                  TEST_DEPENDS get_crtm_coeffs)

# ecbuild_add_test( TARGET  test_check_crtm_random
#                   SOURCES mains/application/check_crtm_random_profiles.F90
#                   ARGS    "testinput/single_profile.yaml"
#                   OMP     $ENV{OMP_NUM_THREADS}
#                   LIBS    crtm
#                   TEST_DEPENDS get_crtm_coeffs)

#---------------------------------------------------------------------------------
#unit tests

#first upper level Unit_Test

#implementation of Patrick's modified TL convergence test.
ecbuild_add_executable (
    TARGET  "TL_TEST"
    SOURCES mains/unit/Unit_Test/test_TL.f90
    LIBS    crtm
    NOINSTALL 
    )
ecbuild_add_test (
    TARGET "Unit_TL_TEST"
    COMMAND "TL_TEST"
    OMP     $ENV{OMP_NUM_THREADS}
    TEST_DEPENDS get_crtm_coeffs
    )

#(PS) SpcCoeff Version 3 I/O test
ecbuild_add_executable (
	TARGET "test_spc_io"
	SOURCES mains/unit/input_output/test_SpcCoeff/test_spc_io.f90
	LIBS crtm
	NOINSTALL
	)
ecbuild_add_test(
	TARGET "Unit_test_spc_io"
	COMMAND "test_spc_io"
	OMP     $ENV{OMP_NUM_THREADS}
	TEST_DEPENDS get_crtm_coeffs
	)

#(PS) SpcCoeff netCDF I/O test
ecbuild_add_executable (
    TARGET "test_spc_io_nc"
	SOURCES mains/unit/input_output/test_SpcCoeff_NC/test_spc_io_nc.f90
	LIBS crtm
	NOINSTALL
	)
ecbuild_add_test(
    TARGET "Unit_test_spc_io_nc"
	COMMAND "test_spc_io_nc"
	OMP $ENV{OMP_NUM_THREADS}
	TEST_DEPENDS get_crtm_coeffs
	)

#(PS) TauCoeff netCDF I/O test
ecbuild_add_executable (
	TARGET "test_taucoeff_io_nc"
	SOURCES mains/unit/input_output/test_TauCoeff_NC/test_taucoeff_io_nc.f90 
	LIBS crtm
	NOINSTALL
	)

ecbuild_add_test(
	TARGET "Unit_test_tc_io_nc"
	COMMAND "test_taucoeff_io_nc"
	OMP $ENV{OMP_NUM_THREADS}
	TEST_DEPENDS get_crtm_coeffs
	)

#(CD) AerosolCoeff I/O test
ecbuild_add_executable (
    TARGET "test_aerosol_coeff_io"
        SOURCES mains/unit/input_output/test_AerosolCoeff/test_aerosol_coeff_io.f90
        LIBS crtm
        NOINSTALL
        )
ecbuild_add_test(
    TARGET "Unit_test_aerosol_coeff_io"
        COMMAND "test_aerosol_coeff_io"
        OMP $ENV{OMP_NUM_THREADS}
        TEST_DEPENDS get_crtm_coeffs
        )

#(CD) AerosolCoeff netCDF I/O test
ecbuild_add_executable (
    TARGET "test_aerosol_coeff_io_nc"
        SOURCES mains/unit/input_output/test_AerosolCoeff_NC/test_aerosol_coeff_io_nc.f90
        LIBS crtm
        NOINSTALL
        )
ecbuild_add_test(
    TARGET "Unit_test_aerosol_coeff_io_nc"
        COMMAND "test_aerosol_coeff_io_nc"
        OMP $ENV{OMP_NUM_THREADS}
        TEST_DEPENDS get_crtm_coeffs
        )

#(CD) EmisCoeff I/O test
ecbuild_add_executable (
    TARGET "test_emis_coeff_io"
        SOURCES mains/unit/input_output/test_EmisCoeff/test_emis_coeff_io.f90
        LIBS crtm
        NOINSTALL
        )
ecbuild_add_test(
    TARGET "Unit_test_emis_coeff_io"
        COMMAND "test_emis_coeff_io"
        OMP $ENV{OMP_NUM_THREADS}
        TEST_DEPENDS get_crtm_coeffs
        )

#(CD) EmisCoeff netCDF I/O test
ecbuild_add_executable (
    TARGET "test_emis_coeff_io_nc"
        SOURCES mains/unit/input_output/test_EmisCoeff_NC/test_emis_coeff_io_nc.f90
        LIBS crtm
        NOINSTALL
        )
ecbuild_add_test(
    TARGET "Unit_test_emis_coeff_io_nc"
        COMMAND "test_emis_coeff_io_nc"
        OMP $ENV{OMP_NUM_THREADS}
        TEST_DEPENDS get_crtm_coeffs
        )

#(PS) Hypsometric Equation test
ecbuild_add_executable (
    TARGET "test_hypsometric_eq"
    SOURCES mains/unit/Unit_Test/test_Hypsometric.f90
    LIBS crtm
    NOINSTALL
    )

ecbuild_add_test(
    TARGET "Unit_test_hypsometric_eq"
    COMMAND "test_hypsometric_eq"
    OMP $ENV{OMP_NUM_THREADS}
    )


#=================================================================================
#forward and k_matrix regression tests
foreach(regtype IN LISTS regression_types)
  string(COMPARE EQUAL ${regtype} "k_matrix" isregtype)
  foreach(testtype IN LISTS common_tests)
    string(COMPARE EQUAL ${testtype} "Aircraft" istesttype)
    if (isregtype AND istesttype)
       continue() #skip Aircraft type for k_Matrix
    endif()
    ecbuild_add_executable( TARGET  "test_${regtype}_test_${testtype}"
                            SOURCES "mains/regression/${regtype}/test_${testtype}/test_${testtype}.f90"
                            LIBS    crtm
                            NOINSTALL)
 
    foreach(sensor_id IN LISTS ${testtype}_Sensor_Ids)
  
          ecbuild_add_test( TARGET  "test_${regtype}_${testtype}_${sensor_id}"
                            OMP     $ENV{OMP_NUM_THREADS}
                            COMMAND "test_${regtype}_test_${testtype}"
                            ARGS    "${sensor_id}"
                            TEST_DEPENDS get_crtm_coeffs)
    endforeach()
  endforeach() 
endforeach()


#---------------------------------------------------------------------------------
#TLAD Regression tests
foreach(regtype IN LISTS TLAD_types)
  foreach(testtype IN LISTS TLAD_tests)
    if (isregtype AND istesttype)
       continue() #skip Aircraft type for k_Matrix
    endif()
    ecbuild_add_executable( TARGET  "test_${regtype}_test_${testtype}"
                            SOURCES "mains/regression/${regtype}/test_${testtype}/test_${testtype}.f90"
                            LIBS    crtm
                            NOINSTALL)
  
    foreach(sensor_id IN LISTS ${testtype}_Sensor_Ids)
          ecbuild_add_test( TARGET  "test_${regtype}_${testtype}_${sensor_id}"
                            OMP     $ENV{OMP_NUM_THREADS}
                            COMMAND "test_${regtype}_test_${testtype}"
                            ARGS    "${sensor_id}"
                            TEST_DEPENDS get_crtm_coeffs)
    endforeach()
  endforeach() 
endforeach()


#=================================================================================
#OpenMP regression tests
foreach(regtype IN LISTS regression_types)
  string(COMPARE EQUAL ${regtype} "k_matrix" isregtype)
  if (isregtype)
    continue() #skip all k_matrix tests
  endif()
  foreach(testtype IN LISTS omp_tests)
    ecbuild_add_executable( TARGET  "test_${regtype}_test_${testtype}"
                            SOURCES "mains/regression/${regtype}/test_${testtype}/test_${testtype}.F90"
                            LIBS    crtm
                            NOINSTALL)
 
    foreach(sensor_id IN LISTS ${testtype}_Sensor_Ids)
  
          ecbuild_add_test( TARGET  "test_${regtype}_${testtype}_${sensor_id}"
                            OMP     $ENV{OMP_NUM_THREADS}
                            COMMAND "test_${regtype}_test_${testtype}"
                            ARGS    "${sensor_id}"
                            TEST_DEPENDS get_crtm_coeffs)
    endforeach()
  endforeach() 
endforeach()



#####################################################################
# Files for CRTM tests
#####################################################################

list( APPEND crtm_test_input
Test_Input/ECMWF_5K/Big_Endian/ecmwf_5k_atmosphereccol.bin
Test_Input/ECMWF_5K/Big_Endian/ecmwf_5k_surfaceccol.bin
Test_Input/ECMWF_5K/Big_Endian/ecmwf_5k_geometryccol.bin
AerosolCoeff/Little_Endian/AerosolCoeff.bin
AerosolCoeff/netCDF/AerosolCoeff.nc4
CloudCoeff/Little_Endian/CloudCoeff.bin
EmisCoeff/MW_Water/Little_Endian/FASTEM6.MWwater.EmisCoeff.bin
EmisCoeff/IR_Ice/SEcategory/Little_Endian/NPOESS.IRice.EmisCoeff.bin
EmisCoeff/IR_Ice/SEcategory/netCDF/NPOESS.IRice.EmisCoeff.nc4
EmisCoeff/IR_Land/SEcategory/Little_Endian/NPOESS.IRland.EmisCoeff.bin
EmisCoeff/IR_Land/SEcategory/netCDF/NPOESS.IRland.EmisCoeff.nc4
EmisCoeff/IR_Snow/SEcategory/Little_Endian/NPOESS.IRsnow.EmisCoeff.bin
EmisCoeff/IR_Snow/SEcategory/netCDF/NPOESS.IRsnow.EmisCoeff.nc4
EmisCoeff/IR_Snow/Nalli/Little_Endian/Nalli.IRsnow.EmisCoeff.bin
EmisCoeff/IR_Snow/Nalli/netCDF/Nalli.IRsnow.EmisCoeff.nc4
EmisCoeff/VIS_Ice/SEcategory/Little_Endian/NPOESS.VISice.EmisCoeff.bin
EmisCoeff/VIS_Ice/SEcategory/netCDF/NPOESS.VISice.EmisCoeff.nc4
EmisCoeff/VIS_Land/SEcategory/Little_Endian/NPOESS.VISland.EmisCoeff.bin
EmisCoeff/VIS_Land/SEcategory/netCDF/NPOESS.VISland.EmisCoeff.nc4
EmisCoeff/VIS_Snow/SEcategory/Little_Endian/NPOESS.VISsnow.EmisCoeff.bin
EmisCoeff/VIS_Snow/SEcategory/netCDF/NPOESS.VISsnow.EmisCoeff.nc4
EmisCoeff/VIS_Water/SEcategory/Little_Endian/NPOESS.VISwater.EmisCoeff.bin
EmisCoeff/VIS_Water/SEcategory/netCDF/NPOESS.VISwater.EmisCoeff.nc4
EmisCoeff/IR_Water/Little_Endian/Nalli.IRwater.EmisCoeff.bin
EmisCoeff/IR_Water/Little_Endian/Nalli2.IRwater.EmisCoeff.bin
EmisCoeff/IR_Water/netCDF/Nalli.IRwater.EmisCoeff.nc4
EmisCoeff/IR_Water/netCDF/Nalli2.IRwater.EmisCoeff.nc4
EmisCoeff/IR_Land/SEcategory/Little_Endian/USGS.IRland.EmisCoeff.bin
EmisCoeff/VIS_Land/SEcategory/Little_Endian/USGS.VISland.EmisCoeff.bin
SpcCoeff/Little_Endian/hirs4_metop-a.SpcCoeff.bin
TauCoeff/ODPS/Little_Endian/hirs4_metop-a.TauCoeff.bin
SpcCoeff/Little_Endian/amsua_n19.SpcCoeff.bin
TauCoeff/ODPS/Little_Endian/amsua_n19.TauCoeff.bin
SpcCoeff/Little_Endian/amsua_metop-a.SpcCoeff.bin
TauCoeff/ODPS/Little_Endian/amsua_metop-a.TauCoeff.bin
SpcCoeff/netCDF/amsua_aqua.SpcCoeff.nc
TauCoeff/ODPS/netCDF/amsua_aqua.TauCoeff.nc
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
SpcCoeff/Little_Endian/abi_g18.SpcCoeff.bin
TauCoeff/ODPS/Little_Endian/abi_g18.TauCoeff.bin
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

# Symlink all CRTM files
CREATE_SYMLINK_FILENAME( ${CRTM_COEFFS_PATH}
                         ${CMAKE_CURRENT_BINARY_DIR}/testinput
                         ${crtm_test_input} )
