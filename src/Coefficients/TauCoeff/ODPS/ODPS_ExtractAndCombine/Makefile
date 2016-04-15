#==============================================================================
#
# Makefile for ODPS_ExtractAndCombine
#
#==============================================================================

# Define macros
include $(CRTM_SOURCE_ROOT)/make.macros

# This makefile
MAKE_FILE = Makefile

# Executable files
EXE_FILE = ODPS_ExtractAndCombine

# Source files to link
SRC_FILES = Type_Kinds.f90 \
            File_Utility.f90 \
            Message_Handler.f90 \
            Compare_Float_Numbers.f90 \
            Sort_Utility.f90 \
            String_Utility.f90 \
            CRTM_Parameters.f90 \
            oSubset_Define.f90 \
            oODPS_Define.f90 \
            oODPS_IO.f90

INC_FILES = oODPS_IO.inc


# Obj files used in link phase
OBJ_FILES = ${SRC_FILES:.f90=.o} \
            $(EXE_FILE).o

# Include and library definitions
INCLUDES = -I$(NETCDF)/include
LIBRARIES = -L$(NETCDF)/lib -lnetcdff

# Define common make targets (all, build, clean, install)
include $(CRTM_SOURCE_ROOT)/make.common_targets

# Source link creation and removal
create_links::
	@$(LINK_SCRIPT) $(CRTM_SOURCE_ROOT) $(SRC_FILES) $(INC_FILES)

remove_links::
	@$(UNLINK_SCRIPT) $(SRC_FILES) $(INC_FILES)

# Source dependency lists
include make.dependencies

# Define default rules
include $(CRTM_SOURCE_ROOT)/make.rules

