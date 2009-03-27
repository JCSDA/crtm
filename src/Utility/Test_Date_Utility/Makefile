#==============================================================================
#
# Makefile for Test_Date_Utility program
#
#==============================================================================

include $(CRTM_SOURCE_ROOT)/make.macros

# This makefile
MAKE_FILE = Makefile

# Executable files
EXE_FILE = Test_Date_Utility

# Source files to link
SRC_FILES = File_Utility.f90 \
            Message_Handler.f90 \
            Date_Utility.f90
            
# Obj files used in link phase
OBJ_FILES = ${SRC_FILES:.f90=.o} \
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
