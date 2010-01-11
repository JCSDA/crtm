#==============================================================================
#
# Makefile for Write_IASI_SensorInfo program
#
#==============================================================================

# Define macros
include $(CRTM_SOURCE_ROOT)/make.macros

# This makefile
MAKE_FILE = Makefile

# Executable file
EXE_FILE = Write_IASI_SensorInfo

# Source files to link
SRC_FILES = Type_Kinds.f90 \
            File_Utility.f90 \
            Message_Handler.f90 \
            Fundamental_Constants.f90 \
            String_Utility.f90 \
            Sort_Utility.f90 \
            List_File_Utility.f90 \
            Channel_Subset_Define.f90 \
            IASI_Define.f90 \
            IASI_Subset.f90 \
            SensorInfo_Parameters.f90 \
            SensorInfo_Define.f90 \
            SensorInfo_LinkedList.f90 \
            SensorInfo_IO.f90

# Obj files used in link phase
OBJ_FILES = ${SRC_FILES:.f90=.o} \
            $(EXE_FILE).o

# Define common make targets (all, build, clean, install)
include $(CRTM_SOURCE_ROOT)/make.common_targets

# Source file link creation and removal
create_links:
	@$(LINK_SCRIPT) $(CRTM_SOURCE_ROOT) $(SRC_FILES)

remove_links:
	@$(UNLINK_SCRIPT) $(SRC_FILES)

# Squeaky clean target
realclean: remove_links clean

# Source dependency lists
include make.dependencies

# Define default rules
include $(CRTM_SOURCE_ROOT)/make.rules
