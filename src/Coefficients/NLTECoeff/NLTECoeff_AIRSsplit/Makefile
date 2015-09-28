#==============================================================================
#
# Makefile for NLTECoeff_AIRSsplit program
#
#==============================================================================

# Define macros
include $(CRTM_SOURCE_ROOT)/make.macros

# This makefile
MAKE_FILE = Makefile

# Executable files
EXE_FILE = NLTECoeff_AIRSsplit

# Source files to link
SRC_FILES = Type_Kinds.f90 \
            File_Utility.f90 \
            Message_Handler.f90 \
            Compare_Float_Numbers.f90 \
            String_Utility.f90 \
            Sort_Utility.f90 \
            Subset_Define.f90 \
            SensorInfo_Parameters.f90 \
            NLTECoeff_Define.f90 \
            NLTECoeff_netCDF_IO.f90

# Obj files used in link phase
OBJ_FILES = ${SRC_FILES:.f90=.o} \
            $(EXE_FILE).o

# Include and library definitions
INCLUDES = -I$(HOME)/local/include
LIBRARIES = -L$(HOME)/local/lib -lnetcdff

# The only existing fix file I'm interested in
FIX_FILES = airs_aqua.NLTECoeff.nc

# Define common make targets (all, build, clean, install)
include $(CRTM_SOURCE_ROOT)/make.common_targets

# Source link creation and removal
create_links::
	@$(LINK_SCRIPT) $(CRTM_SOURCE_ROOT) $(SRC_FILES)
	@$(LINK_SCRIPT) $(CRTM_FIXFILE_ROOT) $(FIX_FILES)

remove_links::
	@$(UNLINK_SCRIPT) $(SRC_FILES)
	@$(UNLINK_SCRIPT) $(FIX_FILES)

# Source dependency lists
include make.dependencies

# Define default rules
include $(CRTM_SOURCE_ROOT)/make.rules

