#==============================================================================
#
# Makefile for Extract_CrIS_SpcCoeff_Subset program
#
#==============================================================================

# -------------
# Define macros
# -------------
include $(CRTM_SOURCE_ROOT)/make.macros

# This makefile
MAKE_FILE = Makefile

# Executable files
EXE_FILE = Extract_CrIS_SpcCoeff_Subset

# Source files to link
SRC_FILES = Type_Kinds.f90 \
            File_Utility.f90 \
            Message_Handler.f90 \
            Compare_Float_Numbers.f90 \
            Fundamental_Constants.f90 \
            Endian_Utility.f90 \
            Binary_File_Utility.f90 \
            ListFile_Utility.f90 \
            Sort_Utility.f90 \
            String_Utility.f90 \
            Subset_Define.f90 \
            CrIS_Define.f90 \
              Apodisation_Utility.f90 \
              SPC_IFG_Utility.f90 \
            CrIS_Subset.f90 \
            SensorInfo_Parameters.f90 \
            SpcCoeff_Define.f90 \
              ACCoeff_Define.f90 \
              NLTECoeff_Define.f90 \
            SpcCoeff_Binary_IO.f90 \
              ACCoeff_Binary_IO.f90 \
              NLTECoeff_Binary_IO.f90

# Obj files used in link phase
OBJ_FILES = ${SRC_FILES:.f90=.o} \
            $(EXE_FILE).o

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
