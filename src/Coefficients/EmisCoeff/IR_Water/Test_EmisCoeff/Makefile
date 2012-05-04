#==============================================================================
#
# Makefile for Test_EmisCoeff program
#
#==============================================================================

# -------------
# Define macros
# -------------
include $(CRTM_SOURCE_ROOT)/make.macros

# Executable files
EXE_FILE = Test_EmisCoeff

# This makefile
MAKE_FILE = Makefile

# Source files for linking
SRC_FILES = Type_Kinds.f90 \
            File_Utility.f90 \
            Message_Handler.f90 \
            Compare_Float_Numbers.f90 \
            Binary_File_Utility.f90 \
            Endian_Utility.f90 \
            netCDF_Utility.f90 \
            netCDF_Attribute_Utility.f90 \
            netCDF_Dimension_Utility.f90 \
            netCDF_Variable_Utility.f90 \
            EmisCoeff_Define.f90 \
            EmisCoeff_Binary_IO.f90 \
            EmisCoeff_netCDF_IO.f90

# Object files for link phase
OBJ_FILES = ${SRC_FILES:.f90=.o} $(EXE_FILE).o

# Include and library definitions
INCLUDES = -I$(HOME)/local/include
LIBRARIES = -L$(HOME)/local/lib -lnetcdf

# Common targets
include $(CRTM_SOURCE_ROOT)/make.common_targets

clean::
	$(REMOVE) *.bin *.nc

# Create links for compilation
create_links::
	$(LINK_SCRIPT) $(CRTM_SOURCE_ROOT) $(SRC_FILES)

remove_links::
	$(UNLINK_SCRIPT) $(SRC_FILES)

# Source code dependency list
include make.dependencies

# Define default rules
include $(CRTM_SOURCE_ROOT)/make.rules

