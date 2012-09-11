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
EXE_FILE = Apodize_TauSpc_with_IRF

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
            LBLRTM_Parameters.f90 \
            LBLRTM_netCDF_IO.f90 \
            FFT_Spectral_Utility.f90 \
            CrIS_Define.f90 \
            IASI_Define.f90 \
            TauProfile_Define.f90 \
            TauProfile_netCDF_IO.f90 \
            Tau_Production_Parameters.f90 \
            Tau_Production_Utility.f90

# Obj files used in link phase
OBJ_FILES = ${SRC_FILES:.f90=.o} \
            $(EXE_FILE).o

# Library and include macros
#NETCDF_LIB = $(HOME)/local/netcdf
FFT_LIB = $(HOME)/local/FFT
INCLUDES = -I$(NETCDF)/include -I$(FFT_LIB)/include
LIBRARIES = -L$(NETCDF)/lib -lnetcdff -L$(FFT_LIB)/lib -lfft

# Define common make targets (all, build, clean, install)
include $(CRTM_SOURCE_ROOT)/make.common_targets

# Source link creation and removal
create_links::
	@$(LINK_SCRIPT) $(CRTM_SOURCE_ROOT) $(SRC_FILES)

remove_links::
	@$(UNLINK_SCRIPT) $(SRC_FILES)

# Squeaky clean target
realclean:: clean remove_links

# Source dependency lists
include make.dependencies

# Define default rules
include $(CRTM_SOURCE_ROOT)/make.rules

