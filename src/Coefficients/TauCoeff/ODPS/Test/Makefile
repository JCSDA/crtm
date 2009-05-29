#==============================================================================
#
# Makefile for Get_Ref_Profile program
#
#==============================================================================
CRTM_SOURCE_ROOT=../../..

# -------------
# Define macros
# -------------
include $(CRTM_SOURCE_ROOT)/make.macros

# This makefile
MAKE_FILE = Makefile

# Executable files
EXE_FILE = Test_TauCoeff 
#Get_Ref_Profile

# Source files to link
SRC_FILES = Test_TauCoeff.f90 \
            Message_Handler.f90 \
            Type_Kinds.f90 \
            Compare_Float_Numbers.f90 \
            Sort_Utility.f90 \
            File_Utility.f90 \
            Endian_Utility.f90 \
            Binary_File_Utility.f90 \
            ODPS_TauCoeff.f90 \
            ODPS_Define.f90 \
            ODPS_Binary_IO.f90

              
# Obj files used in link phase
OBJ_FILES = ${SRC_FILES:.f90=.o} 
             

# Include and library definitions
#INCLUDES = -I$(HOME)/local/netcdf/include
#LIBRARIES = -L$(HOME)/local/netcdf/lib -lnetcdf

INCLUDES = -I$(HOME)/local/netcdf/include
LIBRARIES = -lessl -L$(HOME)/local/netcdf/lib -lnetcdf 


# ------------------
# Define dependecies
# ------------------
include $(CRTM_SOURCE_ROOT)/make.common_targets

# Create and remove source links
create_links:
	@linkfiles $(CRTM_SOURCE_ROOT) $(SRC_FILES)
                   
remove_links:
	@unlinkfiles $(SRC_FILES)

# Source dependencies
include make.dependencies


# --------------------
# Define default rules
# --------------------
include $(CRTM_SOURCE_ROOT)/make.rules
