#==============================================================================
#
# Makefile for MWwaterCoeff_CreateFile program
#
#==============================================================================

# Define macros
include $(CRTM_SOURCE_ROOT)/make.macros

# This makefile
MAKE_FILE = Makefile

# Executable files
EXE_FILE = MWwaterCoeff_CreateFile

# Source files to link
LINK_SRC_FILES = Type_Kinds.f90 \
                 File_Utility.f90 \
                 Message_Handler.f90 \
                 Compare_Float_Numbers.f90 \
                 Binary_File_Utility.f90 \
                 Endian_Utility.f90 \
                 MWwaterCoeff_Define.f90 \
                 FitCoeff_Define.f90 \
                 MWwaterLUT_Define.f90
LOCAL_SRC_FILES = MWwaterCoeff_FASTEM4.f90 \
                  MWwaterCoeff_FASTEM5.f90 \
                  MWwaterCoeff_FASTEM6.f90
SRC_FILES = $(LINK_SRC_FILES) $(LOCAL_SRC_FILES)
INC_FILES = FitCoeff_Destroy.inc \
            FitCoeff_SetValue.inc \
            FitCoeff_Info.inc \
            FitCoeff_ReadFile.inc \
            FitCoeff_WriteFile.inc \
            FitCoeff_Equal.inc

# Obj files used in link phase
OBJ_FILES = ${SRC_FILES:.f90=.o} \
            $(EXE_FILE).o

# Define common make targets (all, build, clean, install)
include $(CRTM_SOURCE_ROOT)/make.common_targets

# Source link creation and removal
create_links::
	@$(LINK_SCRIPT) $(CRTM_SOURCE_ROOT) $(LINK_SRC_FILES) $(INC_FILES)

remove_links::
	@$(UNLINK_SCRIPT) $(LINK_SRC_FILES) $(INC_FILES)

# Source dependency lists
include make.dependencies

# Define default rules
include $(CRTM_SOURCE_ROOT)/make.rules

