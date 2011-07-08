#==============================================================================
#
# Makefile for Create_FTS_SpcCoeff program
#
#==============================================================================

# Define macros
include $(CRTM_SOURCE_ROOT)/make.macros

# This makefile
MAKE_FILE = Makefile

# Executable file
EXE_FILE = Create_FTS_SpcCoeff

# Source files to link
SRC_FILES = Type_Kinds.f90 \
            File_Utility.f90 \
            Message_Handler.f90 \
            Compare_Float_Numbers.f90 \
            Fundamental_Constants.f90 \
            Search_Utility.f90 \
            String_Utility.f90 \
            Linear_Interpolation.f90 \
            Sort_Utility.f90 \
            Spectral_Units_Conversion.f90 \
            Subset_Define.f90 \
            SensorInfo_Parameters.f90 \
            SensorInfo_Define.f90 \
            SensorInfo_LinkedList.f90 \
            SensorInfo_IO.f90 \
            SpcCoeff_Define.f90 \
              ACCoeff_Define.f90 \
              NLTECoeff_Define.f90 \
            SpcCoeff_Parameters.f90 \
            SpcCoeff_netCDF_IO.f90 \
            FFT_Spectral_Utility.f90 \
            IASI_Define.f90 \
            CrIS_Define.f90 \
            Solar_Define.f90 \
            Solar_IO.f90

# Obj files used in link phase
OBJ_FILES = ${SRC_FILES:.f90=.o} \
            $(EXE_FILE).o

# Include and library definitions
NCDF_DIR = $(HOME)/local/netcdf
FFT_DIR = $(HOME)/local/FFT/pfft
INCLUDES = -I$(NCDF_DIR)/include -I$(FFT_DIR)/include 
LIBRARIES = -L$(NCDF_DIR)/lib -lnetcdf -L$(FFT_DIR)/lib -lfft

# Define common make targets (all, build, clean, install)
include $(CRTM_SOURCE_ROOT)/make.common_targets

# Source link creation and removal
create_links::
	@$(LINK_SCRIPT) $(CRTM_SOURCE_ROOT) $(SRC_FILES)

remove_links::
	@$(UNLINK_SCRIPT) $(SRC_FILES)

# Source dependency lists
include make.dependencies

# Define default rules
include $(CRTM_SOURCE_ROOT)/make.rules
