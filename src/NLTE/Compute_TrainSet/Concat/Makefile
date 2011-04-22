#==============================================================================
#
# Makefile for Apodize_TauSpc_with_IRF program
#
#==============================================================================

# Define macros
include $(CRTM_SOURCE_ROOT)/make.macros

# This makefile
MAKE_FILE = Makefile

# Executable file
EXE_FILE = Cat_TrainSet

# Source files to link
SRC_FILES = Type_Kinds.f90 \
            File_Utility.f90 \
            Message_Handler.f90 \
            Fundamental_Constants.f90 \
            Compare_Float_Numbers.f90 \
            Search_Utility.f90 \
            Linear_Interpolation.f90 \
            netCDF_Utility.f90 \
            netCDF_Dimension_Utility.f90 \
            netCDF_Attribute_Utility.f90 \
            netCDF_Variable_Utility.f90 \
            DeltaRad_TrainSet_Define.f90 \
            DeltaRadTrainSet_netCDF_IO.f90

# Obj files used in link phase
OBJ_FILES = ${SRC_FILES:.f90=.o} \
            $(EXE_FILE).o

# Library and include macros
NETCDF_LIB = $(HOME)/local/netcdf
FFT_LIB = $(HOME)/local/FFT
INCLUDES = -I$(NETCDF_LIB)/include -I$(FFT_LIB)/include
LIBRARIES = -L$(NETCDF_LIB)/lib -lnetcdf -L$(FFT_LIB)/lib -lfft

# Define common make targets (all, build, clean, install)
include $(CRTM_SOURCE_ROOT)/make.common_targets

# Source link creation and removal
create_links:
	@linkfiles $(CRTM_SOURCE_ROOT) $(SRC_FILES)

remove_links:
	@unlinkfiles $(SRC_FILES)

# Squeaky clean target
realclean: clean remove_links

# Source dependency lists
include make.dependencies

# Define default rules
include $(CRTM_SOURCE_ROOT)/make.rules

