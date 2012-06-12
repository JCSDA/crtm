#==============================================================================
#
# Makefile for GetSenInfo program
#
#==============================================================================
SOURCE_ROOT=$(CRTM_SOURCE_ROOT)
NETCDF=/apps/netcdf/4.1.3-intel

# -------------
# Define macros
# -------------
include $(SOURCE_ROOT)/make.macros

# This makefile
MAKE_FILE = Makefile

# Executable file
EXE_FILE = GetSenInfo 

# Source files to link
SRC_FILES = Type_Kinds.f90 \
            File_Utility.f90 \
            Compare_Float_Numbers.f90 \
            Message_Handler.f90 \
            netCDF_Utility.f90 \
            netCDF_Dimension_Utility.f90 \
            netCDF_Variable_Utility.f90 \
            netCDF_Attribute_Utility.f90 \
            SpcCoeff_Define.f90 \
            SpcCoeff_netCDF_IO.f90 \
            SensorInfo_Parameters.f90 \
            ACCoeff_Define.f90 \
            Sort_Utility.f90 \
            String_Utility.f90 \
            Compare_Float_Numbers.f90 \
            GetSenInfo.f90 \
            Subset_Define.f90 \
            NLTECoeff_Define.f90 
  

# Obj files used in link phase
OBJ_FILES = ${SRC_FILES:.f90=.o}
             


# Include and library definitions
#INCLUDES = -I$(HOME)/local/netcdf/include
#LIBRARIES = -L$(HOME)/local/netcdf/lib -lnetcdf

#INCLUDES = -I$(HOME)/local/netcdf/include
#LIBRARIES = -lessl -L$(HOME)/local/netcdf/lib -lnetcdf 
INCLUDES = -I$(NETCDF)/include
LIBRARIES = -mkl=sequential -L$(NETCDF)/lib -lnetcdff 


# ------------------
# Define dependecies
# ------------------
include $(SOURCE_ROOT)/make.common_targets

# Create and remove source links
create_links::
	@$(LINK_SCRIPT) $(SOURCE_ROOT) $(SRC_FILES)
                   
remove_links::
	@$(UNLINK_SCRIPT) $(SRC_FILES)

# Source dependencies
include make.dependencies


# --------------------
# Define default rules
# --------------------
include $(SOURCE_ROOT)/make.rules
