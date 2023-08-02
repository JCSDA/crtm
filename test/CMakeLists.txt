# (C) Copyright 2017-2018 UCAR.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

cmake_minimum_required (VERSION 3.12)
project("CRTM_Tests" VERSION 1.0.1 LANGUAGES Fortran C)
enable_testing ()

## Ecbuild integration
find_package( ecbuild QUIET )
if(NOT ecbuild_FOUND)
	message(WARNING "ecbuild is required for testing and was not found!")
	message(WARNING "CRTM tests are disabled!")
	return()
endif()

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

file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/testinput)
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/results)
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/results/adjoint)
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/results/tangent_linear)
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/results/k_matrix)
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/results/forward)
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/results/unit)
#CREATE_SYMLINK( ${CMAKE_CURRENT_SOURCE_DIR} ${CMAKE_CURRENT_BINARY_DIR} ${crtm_test_input} )


if( DEFINED ENV{LOCAL_PATH_JEDI_TESTFILES})
    set(LOCAL_PATH_JEDI_TESTFILES "$ENV{LOCAL_PATH_JEDI_TESTFILES}")
endif()

if ( DEFINED ENV{CRTM_TEST_ROOT})
    set(CRTM_TEST_ROOT "$ENV{CRTM_TEST_ROOT}")
else()
    set(CRTM_TEST_ROOT ${CMAKE_CURRENT_SOURCE_DIR})
endif()

set( REPO_VERSION crtm/2.4.0 )
# If local path to testfiles is defined don't download
IF(EXISTS  ${CMAKE_SOURCE_DIR}/fix)
  set( CRTM_COEFFS_PATH ${CMAKE_SOURCE_DIR}/fix )
  message(STATUS "use LOCAL_PATH_JEDI_TESTFILES: ${CRTM_COEFFS_PATH}")
  message("Using existing local fix directory instead of downloading.")
# Download CRTM coefficients
else()
  set( CRTM_COEFFS_PATH ${CMAKE_BINARY_DIR}/test_data/${REPO_VERSION})
  file(MAKE_DIRECTORY ${CRTM_COEFFS_PATH})
  set( ECBUILD_DOWNLOAD_BASE_URL https://bin.ssec.wisc.edu/pub/s4/CRTM/ )
  set( test_files_dirname fix_REL-2.4.0_emc_07112023.tgz )
  set( checksum "0")
  message(STATUS "download CRTM coeffs files from: ${ECBUILD_DOWNLOAD_BASE_URL} to ${CRTM_COEFFS_PATH}")

  list( APPEND CRTM_DATA_DOWNLOADER_ARGS
                ${ECBUILD_DOWNLOAD_BASE_URL}
                ${CRTM_COEFFS_PATH}
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
#	add_test(NAME get_crtm_coeffs
#		COMMAND ${CMAKE_BINARY_DIR}/bin/crtm_data_downloader.py ${CRTM_DATA_DOWNLOADER_ARGS})
	add_custom_target(get_crtm_coeffs
    COMMAND ${CMAKE_BINARY_DIR}/bin/crtm_data_downloader.py ${CRTM_DATA_DOWNLOADER_ARGS})

endif()

# create lists of sensor IDs for each type of test.  The testing harness will iterate over each item in the list
# verify that the ids added here are present in the long crtm_test_input list toward the end of this file so that
# they will be properly softlinked. 

list( APPEND Simple_Sensor_Ids
        atms_n21
        cris-fsr_n21
        v.abi_g17
				abi_g18
        modis_aqua
)

list( APPEND ScatteringSwitch_Sensor_Ids
        atms_n21
        cris-fsr_n21
        v.abi_g17
				abi_g18
        modis_aqua
)

list( APPEND SOI_Sensor_Ids
        atms_n21
        cris-fsr_n21
        v.abi_g17
				abi_g18
        modis_aqua
)

list( APPEND VerticalCoordinates_Sensor_Ids
        atms_n21
        cris-fsr_n21
        v.abi_g17
				abi_g18
        modis_aqua
)

list( APPEND SSU_Sensor_Ids
        ssu_n06
        ssu_n14
)

list( APPEND ClearSky_Sensor_Ids
        atms_n21
        cris-fsr_n21
        v.abi_g17
				abi_g18
        modis_aqua
)

# Create list of sensor ids for testing
list( APPEND AOD_Sensor_Ids
        cris-fsr_n21
        v.abi_g17
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
      cris-fsr_n21
)

#add new test types here, should have a corresponding test code in test/mains/* 
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

#these will permute with the above common tests
list (APPEND regression_types
     forward
     k_matrix
)

#these will permute with the below TLAD_tests
list (APPEND TLAD_types
     adjoint
     tangent_linear
)

list (APPEND TLAD_tests
     Simple
     ClearSky
)


#####################################################################

add_executable(test_check_crtm mains/application/check_crtm.F90)
target_link_libraries(test_check_crtm PRIVATE crtm)
add_test(NAME test_check_crtm
         COMMAND test_check_crtm)
set_tests_properties(test_check_crtm PROPERTIES ENVIRONMENT "OMP_NUM_THREADS=$ENV{OMP_NUM_THREADS}")
add_dependencies(test_check_crtm get_crtm_coeffs)


#ecbuild_add_test( TARGET  test_check_crtm
#                  SOURCES mains/application/check_crtm.F90
#                  OMP     $ENV{OMP_NUM_THREADS}
#                  LIBS    crtm
#                  TEST_DEPENDS get_crtm_coeffs)

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

ecbuild_add_test( 
	  TARGET  SpcCoeff_Inspect
    SOURCES mains/application/SpcCoeff_Inspect.f90
    ARGS    "testinput/atms_n21.SpcCoeff.bin"
    OMP     $ENV{OMP_NUM_THREADS}
    LIBS    crtm
    TEST_DEPENDS get_crtm_coeffs
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


#####################################################################
# Files for CRTM tests
#####################################################################

list( APPEND crtm_test_input
fix/AerosolCoeff/Big_Endian/AerosolCoeff.bin
fix/AerosolCoeff/netCDF/AerosolCoeff.nc4
fix/CloudCoeff/Big_Endian/CloudCoeff.bin
fix/EmisCoeff/MW_Water/Big_Endian/FASTEM6.MWwater.EmisCoeff.bin
fix/EmisCoeff/IR_Ice/SEcategory/Big_Endian/NPOESS.IRice.EmisCoeff.bin
fix/EmisCoeff/IR_Land/SEcategory/Big_Endian/NPOESS.IRland.EmisCoeff.bin
fix/EmisCoeff/IR_Snow/SEcategory/Big_Endian/NPOESS.IRsnow.EmisCoeff.bin
fix/EmisCoeff/VIS_Ice/SEcategory/Big_Endian/NPOESS.VISice.EmisCoeff.bin
fix/EmisCoeff/VIS_Land/SEcategory/Big_Endian/NPOESS.VISland.EmisCoeff.bin
fix/EmisCoeff/VIS_Snow/SEcategory/Big_Endian/NPOESS.VISsnow.EmisCoeff.bin
fix/EmisCoeff/VIS_Water/SEcategory/Big_Endian/NPOESS.VISwater.EmisCoeff.bin
fix/EmisCoeff/IR_Water/Big_Endian/Nalli.IRwater.EmisCoeff.bin
fix/EmisCoeff/IR_Land/SEcategory/Big_Endian/USGS.IRland.EmisCoeff.bin
fix/EmisCoeff/VIS_Land/SEcategory/Big_Endian/USGS.VISland.EmisCoeff.bin
fix/SpcCoeff/Big_Endian/hirs4_metop-a.SpcCoeff.bin
fix/TauCoeff/ODPS/Big_Endian/hirs4_metop-a.TauCoeff.bin
fix/SpcCoeff/Big_Endian/amsua_n19.SpcCoeff.bin
fix/TauCoeff/ODPS/Big_Endian/amsua_n19.TauCoeff.bin
fix/SpcCoeff/Big_Endian/amsua_metop-a.SpcCoeff.bin
fix/TauCoeff/ODPS/Big_Endian/amsua_metop-a.TauCoeff.bin
fix/SpcCoeff/Big_Endian/gmi_gpm.SpcCoeff.bin
fix/TauCoeff/ODPS/Big_Endian/gmi_gpm.TauCoeff.bin
fix/SpcCoeff/Big_Endian/seviri_m08.SpcCoeff.bin
fix/TauCoeff/ODAS/Big_Endian/seviri_m08.TauCoeff.bin
fix/SpcCoeff/Big_Endian/cris-fsr_npp.SpcCoeff.bin
fix/TauCoeff/ODPS/Big_Endian/cris-fsr_npp.TauCoeff.bin
fix/SpcCoeff/Big_Endian/cris_n20.SpcCoeff.bin
fix/TauCoeff/ODPS/Big_Endian/cris_n20.TauCoeff.bin
fix/SpcCoeff/Big_Endian/cris-fsr_n21.SpcCoeff.bin
fix/TauCoeff/ODPS/Big_Endian/cris-fsr_n21.TauCoeff.bin
fix/SpcCoeff/Big_Endian/iasi_metop-a.SpcCoeff.bin
fix/TauCoeff/ODPS/Big_Endian/iasi_metop-a.TauCoeff.bin
fix/SpcCoeff/Big_Endian/iasi_metop-b.SpcCoeff.bin
fix/TauCoeff/ODPS/Big_Endian/iasi_metop-b.TauCoeff.bin
fix/SpcCoeff/Big_Endian/mhs_n19.SpcCoeff.bin
fix/TauCoeff/ODPS/Big_Endian/mhs_n19.TauCoeff.bin
fix/SpcCoeff/Big_Endian/sndrD1_g15.SpcCoeff.bin
fix/TauCoeff/ODPS/Big_Endian/sndrD1_g15.TauCoeff.bin
fix/SpcCoeff/Big_Endian/sndrD2_g15.SpcCoeff.bin
fix/TauCoeff/ODPS/Big_Endian/sndrD2_g15.TauCoeff.bin
fix/SpcCoeff/Big_Endian/sndrD3_g15.SpcCoeff.bin
fix/TauCoeff/ODPS/Big_Endian/sndrD3_g15.TauCoeff.bin
fix/SpcCoeff/Big_Endian/sndrD4_g15.SpcCoeff.bin
fix/TauCoeff/ODPS/Big_Endian/sndrD4_g15.TauCoeff.bin
fix/SpcCoeff/Big_Endian/airs_aqua.SpcCoeff.bin
fix/TauCoeff/ODPS/Big_Endian/airs_aqua.TauCoeff.bin
fix/SpcCoeff/Big_Endian/modis_aqua.SpcCoeff.bin
fix/TauCoeff/ODPS/Big_Endian/modis_aqua.TauCoeff.bin
fix/SpcCoeff/Big_Endian/cris399_npp.SpcCoeff.bin
fix/TauCoeff/ODPS/Big_Endian/cris399_npp.TauCoeff.bin
fix/SpcCoeff/Big_Endian/crisB1_npp.SpcCoeff.bin
fix/TauCoeff/ODPS/Big_Endian/crisB1_npp.TauCoeff.bin
fix/SpcCoeff/Big_Endian/atms_npp.SpcCoeff.bin
fix/TauCoeff/ODPS/Big_Endian/atms_npp.TauCoeff.bin
fix/SpcCoeff/Big_Endian/v.viirs-m_npp.SpcCoeff.bin
fix/TauCoeff/ODAS/Big_Endian/v.viirs-m_npp.TauCoeff.bin
fix/SpcCoeff/Big_Endian/atms_n20.SpcCoeff.bin
fix/TauCoeff/ODPS/Big_Endian/atms_n20.TauCoeff.bin
fix/SpcCoeff/Big_Endian/atms_n21.SpcCoeff.bin
fix/TauCoeff/ODPS/Big_Endian/atms_n21.TauCoeff.bin
fix/SpcCoeff/Big_Endian/abi_g18.SpcCoeff.bin
fix/TauCoeff/ODPS/Big_Endian/abi_g18.TauCoeff.bin
fix/SpcCoeff/Big_Endian/v.abi_g17.SpcCoeff.bin
fix/TauCoeff/ODAS/Big_Endian/v.abi_g17.TauCoeff.bin
fix/TauCoeff/ODPS/Big_Endian/zssmis_f20.TauCoeff.bin
fix/TauCoeff/ODPS/Big_Endian/zssmis_f19.TauCoeff.bin
fix/TauCoeff/ODPS/Big_Endian/zssmis_f18.TauCoeff.bin
fix/TauCoeff/ODPS/Big_Endian/zssmis_f17.TauCoeff.bin
fix/TauCoeff/ODPS/Big_Endian/zssmis_f16.TauCoeff.bin
fix/TauCoeff/ODPS/Big_Endian/ssmis_f20.TauCoeff.bin
fix/TauCoeff/ODPS/Big_Endian/ssmis_f19.TauCoeff.bin
fix/TauCoeff/ODPS/Big_Endian/ssmis_f18.TauCoeff.bin
fix/TauCoeff/ODPS/Big_Endian/ssmis_f17.TauCoeff.bin
fix/TauCoeff/ODPS/Big_Endian/ssmis_f16.TauCoeff.bin
fix/SpcCoeff/Big_Endian/ssmis_f20.SpcCoeff.bin
fix/SpcCoeff/Big_Endian/ssmis_f18.SpcCoeff.bin
fix/SpcCoeff/Big_Endian/ssmis_f19.SpcCoeff.bin
fix/SpcCoeff/Big_Endian/ssmis_f16.SpcCoeff.bin
fix/SpcCoeff/Big_Endian/ssmis_f17.SpcCoeff.bin
fix/TauCoeff/ODPS/Big_Endian/ssu_n06.TauCoeff.bin
fix/TauCoeff/ODPS/Big_Endian/ssu_n07.TauCoeff.bin
fix/TauCoeff/ODPS/Big_Endian/ssu_n08.TauCoeff.bin
fix/TauCoeff/ODPS/Big_Endian/ssu_n09.TauCoeff.bin
fix/TauCoeff/ODPS/Big_Endian/ssu_n11.TauCoeff.bin
fix/TauCoeff/ODPS/Big_Endian/ssu_n14.TauCoeff.bin
fix/SpcCoeff/Big_Endian/ssu_n06.SpcCoeff.bin
fix/SpcCoeff/Big_Endian/ssu_n07.SpcCoeff.bin
fix/SpcCoeff/Big_Endian/ssu_n08.SpcCoeff.bin
fix/SpcCoeff/Big_Endian/ssu_n09.SpcCoeff.bin
fix/SpcCoeff/Big_Endian/ssu_n11.SpcCoeff.bin
fix/SpcCoeff/Big_Endian/ssu_n14.SpcCoeff.bin
fix/SpcCoeff/netcdf/atms_n21.SpcCoeff.nc
fix/SpcCoeff/netcdf/viirs-i_n21.SpcCoeff.nc
fix/SpcCoeff/netcdf/atms_n21-srf.SpcCoeff.nc
fix/SpcCoeff/netcdf/mhs_metop-c.SpcCoeff.nc
fix/SpcCoeff/netcdf/v.viirs-m_j2.SpcCoeff.nc
fix/SpcCoeff/netcdf/cris-fsr_n21.SpcCoeff.nc
fix/SpcCoeff/netcdf/v.viirs-i_n21.SpcCoeff.nc
fix/SpcCoeff/netcdf/viirs-m_n21.SpcCoeff.nc
fix/SpcCoeff/netcdf/v.viirs-m_n21.SpcCoeff.nc
fix/SpcCoeff/netcdf/amsua_metop-c_v2.SpcCoeff.nc
fix/SpcCoeff/netcdf/v.viirs-i_j2.SpcCoeff.nc
)

# Symlink all CRTM files
CREATE_SYMLINK_FILENAME( ${CRTM_COEFFS_PATH}
                         ${CMAKE_CURRENT_BINARY_DIR}/testinput
                         ${crtm_test_input} )
