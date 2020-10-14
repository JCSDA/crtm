# CMake project file for CRTM

##################################################
# Define the project and the depencies that it has
##################################################

CMAKE_MINIMUM_REQUIRED(VERSION 2.8.5)
cmake_policy(SET CMP0057 NEW)
cmake_policy(SET CMP0079 NEW)

PROJECT(crtm Fortran)

# Set the CRTM version
SET(VERSION crtm_2.4.0-alpha)

# temporary for testing/building
SET(CMAKE_FC_COMPILER /opt/intel/18/compilers_and_libraries_2018.3.222/linux/bin/intel64/ifort)

# Uncomment if it is required that Fortran 90 is supported
IF(NOT CMAKE_Fortran_COMPILER_SUPPORTS_F90)
    MESSAGE(FATAL_ERROR "Fortran compiler does not support F90")
ENDIF(NOT CMAKE_Fortran_COMPILER_SUPPORTS_F90)

# Add our local modlues to the module path
SET(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake/Modules/")

find_package( NetCDF REQUIRED COMPONENTS Fortran )


# Set some options the user may choose
# Uncomment the below if you want the user to choose a parallelization library
#OPTION(USE_MPI "Use the MPI library for parallelization" OFF)

OPTION(USE_OPENMP "Use OpenMP for parallelization" ON)


# This INCLUDE statement executes code that sets the compile flags for DEBUG,
# RELEASE, and TESTING.  You should  review this file and make sure the flags 
# are to your liking.
#INCLUDE(${CMAKE_MODULE_PATH}/SetFortranFlags.cmake) 

# Locate and set parallelization libraries.  There are some CMake peculiarities
# taken care of here, such as the fact that the FindOpenMP routine doesn't know
# about Fortran.
INCLUDE(${CMAKE_MODULE_PATH}/SetParallelizationLibrary.cmake)
FIND_PACKAGE(OpenMP_Fortran)

# Setup the LAPACK libraries.  This also takes care of peculiarities, such as
# the fact the searching for MKL requires a C compiler, and that the results
# are not stored in the cache. 
#INCLUDE(${CMAKE_MODULE_PATH}/SetUpLAPACK.cmake)

# There is an error in CMAKE with this flag for pgf90.  Unset it
GET_FILENAME_COMPONENT(FCNAME ${CMAKE_Fortran_COMPILER} NAME)
IF(FCNAME STREQUAL "pgf90")
    UNSET(CMAKE_SHARED_LIBRARY_LINK_Fortran_FLAGS)
ENDIF(FCNAME STREQUAL "pgf90")

message(${CMAKE_FC_COMPILER})

############################################################
# Define the actual files and folders that make up the build
############################################################

## Sources
add_subdirectory( libsrc )
add_subdirectory( test )

return()

#Build-tree location for Coefficients
set(COEFFICIENT_BUILD_DIR ${CMAKE_SOURCE_DIR}/fix) #source
#Build-tree location for coefficients
set(UFO_COEFFICIENT_BUILD_DIR ${CMAKE_CURRENT_BINARY_DIR}/coefficients) #build target location

#Install-tree (relative) location for crtm share data
set(CRTM_SHARE_INSTALL_DIR ${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME})    #build/crtm
#install-tree (relative) location for the coefficients symlinks.
set(UFO_COEFFICIENT_INSTALL_DIR ${CRTM_SHARE_INSTALL_DIR}/coefficients) #build/crtm/coefficients

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

#Build Tree exports
set(EXPORT_COEFFICIENT_BUILD_DIR ${PROJECT_SOURCE_DIR}/fix) #build-tree coefficient location
#Install Tree exports
set(EXPORT_COEFFICIENT_INSTALL_DIR ${CRTM_SHARE_INSTALL_DIR}/fix) #install-tree coefficient location



# Define some directories
SET(SRC ${CMAKE_SOURCE_DIR}/src)
SET(LIB ${CMAKE_SOURCE_DIR}/lib)
SET(BIN ${CMAKE_SOURCE_DIR}/bin)

# Have the .mod files placed in the lib folder
SET(CMAKE_Fortran_MODULE_DIRECTORY ${LIB})

# The source for the BAR library and have it placed in the lib folder
ADD_SUBDIRECTORY(${SRCBAR} ${LIB})

# The source for the FOO binary and have it placed in the bin folder
ADD_SUBDIRECTORY(${SRCFOO} ${BINX})

# The source for the tomp library and have it placed in the bin folder
ADD_SUBDIRECTORY(${SRCTOMP} ${BIN})

SET_TARGET_PROPERTIES(${tomp} PROPERTIES
                      COMPILE_FLAGS "${OpenMP_Fortran_FLAGS}"
                      LINK_FLAGS "${OpenMP_Fortran_FLAGS}")

target_link_libraries(tomp PUBLIC NetCDF::NetCDF_Fortran)

# Add a distclean target to the Makefile
ADD_CUSTOM_TARGET(distclean 
    COMMAND ${CMAKE_COMMAND} -P ${CMAKE_SOURCE_DIR}/distclean.cmake
)


enable_testing()


add_test(NAME TOMP
				 COMMAND tomp
				 WORKING_DIRECTORY ../bin
				 )
