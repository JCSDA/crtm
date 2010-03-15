#==============================================================================
#
# Makefile for AerosolCoeff_R2_to_R3 program
#
#==============================================================================

# Define macros
include $(CRTM_SOURCE_ROOT)/make.macros

# This makefile
MAKE_FILE = Makefile

# Executable file
EXE_FILE = AerosolCoeff_R2_to_R3

# Source files to link
SRC_FILES = Type_Kinds.f90 \
            File_Utility.f90 \
            Message_Handler.f90 \
            Compare_Float_Numbers.f90 \
            Endian_Utility.f90 \
            Binary_File_Utility.f90 \
            String_Utility.f90 \
            Fundamental_Constants.f90 \
            Spectral_Utility.f90 \
              Spectral_Units_Conversion.f90 \
            netCDF_Utility.f90 \
            netCDF_Dimension_Utility.f90 \
            netCDF_Attribute_Utility.f90 \
            netCDF_Variable_Utility.f90 \
            AerosolCoeff_Define.f90 AerosolCoeff_netCDF_IO.f90

R2_FILES = AerosolCoeff_Define_R2.f90 AerosolCoeff_netCDF_IO_R2.f90

# Obj files used in link phase
OBJ_FILES = ${SRC_FILES:.f90=.o} \
            ${R2_FILES:.f90=.o} \
            $(EXE_FILE).o

# Include and library definitions
INCLUDES = -I$(HOME)/local/netcdf/include
LIBRARIES = -L$(HOME)/local/netcdf/lib -lnetcdf

# Define common make targets (all, build, clean, install)
include $(CRTM_SOURCE_ROOT)/make.common_targets

# Source link creation and removal
create_links:
	@$(LINK_SCRIPT) $(CRTM_SOURCE_ROOT) $(SRC_FILES)

remove_links:
	@$(UNLINK_SCRIPT) $(SRC_FILES)

# Squeaky clean target
realclean: clean remove_links

# Source dependency lists
include make.dependencies

# Define default rules
include $(CRTM_SOURCE_ROOT)/make.rules

