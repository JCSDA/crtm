#==============================================================================
#
# Makefile for Get_Ref_Profile program
#
#==============================================================================
ROOT=$(CRTM_SOURCE_ROOT)

# -------------
# Define macros
# -------------
include $(ROOT)/make.macros

# This makefile
MAKE_FILE = Makefile

# Executable files
EXE_FILE = Merge_ODPS_ODAS 
#Get_Ref_Profile

# Source files to link
SRC_FILES = Type_Kinds.f90 \
            Message_Handler.f90 \
            Compare_Float_Numbers.f90 \
            File_Utility.f90 \
            Binary_File_Utility.f90 \
            netCDF_Utility.f90 \
            netCDF_Dimension_Utility.f90 \
            netCDF_Variable_Utility.f90 \
            netCDF_Attribute_Utility.f90 \
            Sort_Utility.f90 \
            Compare_Float_Numbers.f90 \
            Endian_Utility.f90 \
            ODAS_Define.f90 \
            ODAS_netCDF_IO.f90 \
            ODPS_Define.f90 \
            ODPS_netCDF_IO.f90 \
            ODPS_Predictor_Define.f90 \
            ODPS_Predictor.f90 \
            ODPS_ZAtmAbsorption.f90


EXE_SRC_FILE = Merge_ODPS_ODAS.f90
                      
  
# Obj files used in link phase
OBJ_FILES = ${SRC_FILES:.f90=.o} \
            ${EXE_SRC_FILE:.f90=.o} 
             

# Include and library definitions
#INCLUDES = -I$(HOME)/local/netcdf/include
#LIBRARIES = -L$(HOME)/local/netcdf/lib -lnetcdf

INCLUDES = -I$(HOME)/local/netcdf/include
#LIBRARIES = -lessl -L$(HOME)/local/netcdf/lib -lnetcdf -L$(HOME)/local/lapack/bin -llapack -lblas
LIBRARIES = -lessl -L$(HOME)/local/netcdf/lib -lnetcdf 


# ------------------
# Define dependecies
# ------------------
include $(ROOT)/make.common_targets

# Create and remove source links
create_links:
	@linkfiles $(ROOT) $(SRC_FILES)
                   
remove_links:
	@unlinkfiles $(SRC_FILES)

# Source dependencies
include make.dependencies


# --------------------
# Define default rules
# --------------------
include $(ROOT)/make.rules
