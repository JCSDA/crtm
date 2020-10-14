
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

#need to automatically choose coefficients based on library endian type

set(ENDIAN_TYPE "Little_Endian") #Little_Endian #Big_Endian (#netCDF not supported yet)

list( APPEND crtm_test_input
Test_Input/ECMWF_5K/Big_Endian/ecmwf_5k_atmosphereccol.bin
Test_Input/ECMWF_5K/Big_Endian/ecmwf_5k_surfaceccol.bin
Test_Input/ECMWF_5K/Big_Endian/ecmwf_5k_geometryccol.bin
AerosolCoeff/${ENDIAN_TYPE}/AerosolCoeff.bin
CloudCoeff/${ENDIAN_TYPE}/CloudCoeff.bin
EmisCoeff/MW_Water/${ENDIAN_TYPE}/FASTEM6.MWwater.EmisCoeff.bin
EmisCoeff/IR_Ice/SEcategory/${ENDIAN_TYPE}/NPOESS.IRice.EmisCoeff.bin
EmisCoeff/IR_Land/SEcategory/${ENDIAN_TYPE}/NPOESS.IRland.EmisCoeff.bin
EmisCoeff/IR_Snow/SEcategory/${ENDIAN_TYPE}/NPOESS.IRsnow.EmisCoeff.bin
EmisCoeff/VIS_Ice/SEcategory/${ENDIAN_TYPE}/NPOESS.VISice.EmisCoeff.bin
EmisCoeff/VIS_Land/SEcategory/${ENDIAN_TYPE}/NPOESS.VISland.EmisCoeff.bin
EmisCoeff/VIS_Snow/SEcategory/${ENDIAN_TYPE}/NPOESS.VISsnow.EmisCoeff.bin
EmisCoeff/VIS_Water/SEcategory/${ENDIAN_TYPE}/NPOESS.VISwater.EmisCoeff.bin
EmisCoeff/IR_Water/${ENDIAN_TYPE}/Nalli.IRwater.EmisCoeff.bin
EmisCoeff/IR_Land/SEcategory/${ENDIAN_TYPE}/USGS.IRland.EmisCoeff.bin
EmisCoeff/VIS_Land/SEcategory/${ENDIAN_TYPE}/USGS.VISland.EmisCoeff.bin
SpcCoeff/${ENDIAN_TYPE}/hirs4_metop-a.SpcCoeff.bin
TauCoeff/ODPS/${ENDIAN_TYPE}/hirs4_metop-a.TauCoeff.bin
SpcCoeff/${ENDIAN_TYPE}/amsua_n19.SpcCoeff.bin
TauCoeff/ODPS/${ENDIAN_TYPE}/amsua_n19.TauCoeff.bin
SpcCoeff/${ENDIAN_TYPE}/amsua_metop-a.SpcCoeff.bin
TauCoeff/ODPS/${ENDIAN_TYPE}/amsua_metop-a.TauCoeff.bin
SpcCoeff/${ENDIAN_TYPE}/gmi_gpm.SpcCoeff.bin
TauCoeff/ODPS/${ENDIAN_TYPE}/gmi_gpm.TauCoeff.bin
SpcCoeff/${ENDIAN_TYPE}/seviri_m08.SpcCoeff.bin
TauCoeff/ODAS/${ENDIAN_TYPE}/seviri_m08.TauCoeff.bin
SpcCoeff/${ENDIAN_TYPE}/cris-fsr_npp.SpcCoeff.bin
TauCoeff/ODPS/${ENDIAN_TYPE}/cris-fsr_npp.TauCoeff.bin
SpcCoeff/${ENDIAN_TYPE}/iasi_metop-a.SpcCoeff.bin
TauCoeff/ODPS/${ENDIAN_TYPE}/iasi_metop-a.TauCoeff.bin
SpcCoeff/${ENDIAN_TYPE}/iasi_metop-b.SpcCoeff.bin
TauCoeff/ODPS/${ENDIAN_TYPE}/iasi_metop-b.TauCoeff.bin
SpcCoeff/${ENDIAN_TYPE}/mhs_n19.SpcCoeff.bin
TauCoeff/ODPS/${ENDIAN_TYPE}/mhs_n19.TauCoeff.bin
SpcCoeff/${ENDIAN_TYPE}/sndrD1_g15.SpcCoeff.bin
TauCoeff/ODPS/${ENDIAN_TYPE}/sndrD1_g15.TauCoeff.bin
SpcCoeff/${ENDIAN_TYPE}/sndrD2_g15.SpcCoeff.bin
TauCoeff/ODPS/${ENDIAN_TYPE}/sndrD2_g15.TauCoeff.bin
SpcCoeff/${ENDIAN_TYPE}/sndrD3_g15.SpcCoeff.bin
TauCoeff/ODPS/${ENDIAN_TYPE}/sndrD3_g15.TauCoeff.bin
SpcCoeff/${ENDIAN_TYPE}/sndrD4_g15.SpcCoeff.bin
TauCoeff/ODPS/${ENDIAN_TYPE}/sndrD4_g15.TauCoeff.bin
SpcCoeff/${ENDIAN_TYPE}/airs_aqua.SpcCoeff.bin
TauCoeff/ODPS/${ENDIAN_TYPE}/airs_aqua.TauCoeff.bin
SpcCoeff/${ENDIAN_TYPE}/modis_aqua.SpcCoeff.bin
TauCoeff/ODPS/${ENDIAN_TYPE}/modis_aqua.TauCoeff.bin
SpcCoeff/${ENDIAN_TYPE}/cris399_npp.SpcCoeff.bin
TauCoeff/ODPS/${ENDIAN_TYPE}/cris399_npp.TauCoeff.bin
SpcCoeff/${ENDIAN_TYPE}/crisB1_npp.SpcCoeff.bin
TauCoeff/ODPS/${ENDIAN_TYPE}/crisB1_npp.TauCoeff.bin
SpcCoeff/${ENDIAN_TYPE}/atms_npp.SpcCoeff.bin
TauCoeff/ODPS/${ENDIAN_TYPE}/atms_npp.TauCoeff.bin
SpcCoeff/${ENDIAN_TYPE}/v.viirs-m_npp.SpcCoeff.bin
TauCoeff/ODAS/${ENDIAN_TYPE}/v.viirs-m_npp.TauCoeff.bin
SpcCoeff/${ENDIAN_TYPE}/v.abi_gr.SpcCoeff.bin
TauCoeff/ODAS/${ENDIAN_TYPE}/v.abi_gr.TauCoeff.bin
TauCoeff/ODPS/${ENDIAN_TYPE}/zssmis_f20.TauCoeff.bin
TauCoeff/ODPS/${ENDIAN_TYPE}/zssmis_f19.TauCoeff.bin
TauCoeff/ODPS/${ENDIAN_TYPE}/zssmis_f18.TauCoeff.bin
TauCoeff/ODPS/${ENDIAN_TYPE}/zssmis_f17.TauCoeff.bin
TauCoeff/ODPS/${ENDIAN_TYPE}/zssmis_f16.TauCoeff.bin
TauCoeff/ODPS/${ENDIAN_TYPE}/ssmis_f20.TauCoeff.bin
TauCoeff/ODPS/${ENDIAN_TYPE}/ssmis_f19.TauCoeff.bin
TauCoeff/ODPS/${ENDIAN_TYPE}/ssmis_f18.TauCoeff.bin
TauCoeff/ODPS/${ENDIAN_TYPE}/ssmis_f17.TauCoeff.bin
TauCoeff/ODPS/${ENDIAN_TYPE}/ssmis_f16.TauCoeff.bin
SpcCoeff/${ENDIAN_TYPE}/ssmis_f20.SpcCoeff.bin
SpcCoeff/${ENDIAN_TYPE}/ssmis_f18.SpcCoeff.bin
SpcCoeff/${ENDIAN_TYPE}/ssmis_f19.SpcCoeff.bin
SpcCoeff/${ENDIAN_TYPE}/ssmis_f16.SpcCoeff.bin
SpcCoeff/${ENDIAN_TYPE}/ssmis_f17.SpcCoeff.bin
TauCoeff/ODPS/${ENDIAN_TYPE}/ssu_n06.TauCoeff.bin
TauCoeff/ODPS/${ENDIAN_TYPE}/ssu_n07.TauCoeff.bin
TauCoeff/ODPS/${ENDIAN_TYPE}/ssu_n08.TauCoeff.bin
TauCoeff/ODPS/${ENDIAN_TYPE}/ssu_n09.TauCoeff.bin
TauCoeff/ODPS/${ENDIAN_TYPE}/ssu_n11.TauCoeff.bin
TauCoeff/ODPS/${ENDIAN_TYPE}/ssu_n14.TauCoeff.bin
SpcCoeff/${ENDIAN_TYPE}/ssu_n06.SpcCoeff.bin
SpcCoeff/${ENDIAN_TYPE}/ssu_n07.SpcCoeff.bin
SpcCoeff/${ENDIAN_TYPE}/ssu_n08.SpcCoeff.bin
SpcCoeff/${ENDIAN_TYPE}/ssu_n09.SpcCoeff.bin
SpcCoeff/${ENDIAN_TYPE}/ssu_n11.SpcCoeff.bin
SpcCoeff/${ENDIAN_TYPE}/ssu_n14.SpcCoeff.bin
)

# Symlink all CRTM files (creates links in testinput, may want to modify to Data to be consistent with other projects)
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/testinput)
CREATE_SYMLINK_FILENAME( $ENV{CRTM_FIXFILE_ROOT}
                         ${CMAKE_CURRENT_BINARY_DIR}/testinput
                         ${crtm_test_input} )

file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/results)
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/results/adjoint)
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/results/tangent_linear)
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/results/k_matrix)
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/results/forward)
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/results/unit)

# Create list of sensor ids for testing
list( APPEND Simple_Sensor_Ids
        atms_npp
        cris399_npp
        v.abi_gr
        modis_aqua
)

list( APPEND ScatteringSwitch_Sensor_Ids
        atms_npp
        cris399_npp
        v.abi_gr
        modis_aqua
)

list( APPEND SOI_Sensor_Ids
        atms_npp
        cris399_npp
        v.abi_gr
        modis_aqua
)

list( APPEND VerticalCoordinates_Sensor_Ids
        atms_npp
        cris399_npp
        v.abi_gr
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
        modis_aqua
)

# Create list of sensor ids for testing
list( APPEND AOD_Sensor_Ids
        cris399_npp
        v.abi_gr
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



#---------------------------------------------------------------------------------
enable_testing ()
#TLAD Regression tests
foreach(regtype IN LISTS TLAD_types)
  foreach(testtype IN LISTS TLAD_tests)
    if (isregtype AND istesttype)
       continue() #skip Aircraft type for k_Matrix
    endif()

		set(TARGET test_${regtype}_test_${testtype})
#    set(SOURCE ${CMAKE_BINARY_DIR}/mains/regression/${regtype}/test_${testtype}/test_${testtype}.f90)
    set(SOURCE $ENV{CRTM_SOURCE_ROOT}/Build/test/mains/regression/${regtype}/test_${testtype}/test_${testtype}.f90)
		message("TARGET:" ${TARGET})
		message("SOURCE:" ${SOURCE})

		add_executable(${TARGET} ${SOURCE})

#		set_target_properties(${TARGET} PROPERTIES RUNTIME_OUTPUT_DIRECTORY )

		target_link_libraries( ${TARGET}
		                       ${CRTM} 
													 ${NETCDF_LIBRARIES} 
													 ${OpenMP_Fortran_LIBRARIES})
		#had to specifically include the "include" directory for CRTM, probably a better way to do this:
		target_include_directories(${TARGET} PRIVATE $ENV{CRTM_SOURCE_ROOT}/Build/crtm_v2.4.0-alpha/include)

    foreach(sensor_id IN LISTS ${testtype}_Sensor_Ids)
					set(TESTNAME test_${regtype}_${testtype}_${sensor_id})
					message ("SID: " ${sensor_id} " " ${TESTNAME} " " ${TARGET})
					add_test (NAME ${TESTNAME}  COMMAND $<TARGET_FILE:${TARGET}> ${sensor_id})
    endforeach()
  endforeach()
endforeach()

##=================================================================================
#forward and k_matrix regression tests
foreach(regtype IN LISTS regression_types)
  string(COMPARE EQUAL ${regtype} "k_matrix" isregtype)
  foreach(testtype IN LISTS common_tests)
    string(COMPARE EQUAL ${testtype} "Aircraft" istesttype)
    if (isregtype AND istesttype)
       continue() #skip Aircraft type for k_Matrix
    endif()

		set(TARGET test_${regtype}_test_${testtype})
    set(SOURCE $ENV{CRTM_SOURCE_ROOT}/Build/test/mains/regression/${regtype}/test_${testtype}/test_${testtype}.f90)
		message("TARGET:" ${TARGET})
		message("SOURCE:" ${SOURCE})

		add_executable(${TARGET} ${SOURCE})

		target_link_libraries( ${TARGET}
		                       ${CRTM} 
													 ${NETCDF_LIBRARIES} 
													 ${OpenMP_Fortran_LIBRARIES})
		#had to specifically include the "include" directory for CRTM, probably a better way to do this:
		target_include_directories(${TARGET} PRIVATE $ENV{CRTM_SOURCE_ROOT}/Build/crtm_v2.4.0-alpha/include)

    foreach(sensor_id IN LISTS ${testtype}_Sensor_Ids)
					set(TESTNAME test_${regtype}_${testtype}_${sensor_id})
					message ("SID: " ${sensor_id} " " ${TESTNAME} " " ${TARGET})
					add_test (NAME ${TESTNAME}  COMMAND $<TARGET_FILE:${TARGET}> ${sensor_id})

    endforeach()
  endforeach()
endforeach()


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


add_test (NAME Mytest1 COMMAND $<TARGET_FILE:TEST_TL>)

#### end add_test block ####

#STOP HERE FOR NOW
return()





