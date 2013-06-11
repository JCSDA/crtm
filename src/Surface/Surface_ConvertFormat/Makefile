#==============================================================================
#
# Makefile for Surface_ConvertFormat program
#
#==============================================================================

# Define macros
include $(CRTM_SOURCE_ROOT)/make.macros

# This makefile
MAKE_FILE = Makefile

# Executable file
EXE_FILE = Surface_ConvertFormat

# Source files to link
SRC_FILES = Type_Kinds.f90 \
            File_Utility.f90 \
            Message_Handler.f90 \
            Compare_Float_Numbers.f90 \
            Endian_Utility.f90 \
            Binary_File_Utility.f90 \
            CRTM_Parameters.f90 \
            CRTM_Surface_Define.f90 \
            CRTM_SensorData_Define.f90
OLD_SRC_FILES = CRTM_Surface_Define_old.f90 \
                CRTM_Surface_IO_old.f90 \
                CRTM_SensorData_Define_old.f90 \
                CRTM_SensorData_IO_old.f90

# Obj files used in link phase
OBJ_FILES = ${SRC_FILES:.f90=.o} \
            ${OLD_SRC_FILES:.f90=.o} \
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

