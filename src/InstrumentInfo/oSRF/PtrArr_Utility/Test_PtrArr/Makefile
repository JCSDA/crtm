#==============================================================================
#
# Makefile for Test_PtrArr program
#
#==============================================================================

# Define macros
include $(CRTM_SOURCE_ROOT)/make.macros

# This makefile
MAKE_FILE = Makefile

# Executable file
EXE_FILE = Test_PtrArr

# Source files to link
F90_FILES = Type_Kinds.f90 \
            File_Utility.f90 \
            Message_Handler.f90 \
            Compare_Float_Numbers.f90 \
            Unit_Test.f90
FPP_FILES = PtrArr_Define.F90

SRC_FILES = $(F90_FILES) $(FPP_FILES)
            
# Obj files used in link phase
OBJ_FILES = ${F90_FILES:.f90=.o} \
            ${FPP_FILES:.F90=.o} \
            $(EXE_FILE).o

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
