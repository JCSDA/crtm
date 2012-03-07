#==============================================================================
#
# Makefile for tg_ASG_Rewrite program
#
#==============================================================================

# Define macros
include $(CRTM_SOURCE_ROOT)/make.macros

# This makefile
MAKE_FILE = Makefile

# Executable file
EXE_FILE = tg_ASG_Rewrite

# Source files to link
SRC_FILES = Type_Kinds.f90 \
            File_Utility.f90 \
            Message_Handler.f90 \
            Compare_Float_Numbers.f90 \
            Endian_Utility.f90 \
            Binary_File_Utility.f90 \
            CRTM_Parameters.f90 \
            CRTM_Atmosphere_Define.f90 \
              CRTM_Cloud_Define.f90 \
              CRTM_Aerosol_Define.f90 \
            CRTM_Atmosphere_Binary_IO.f90 \
              CRTM_Cloud_Binary_IO.f90 \
              CRTM_Aerosol_Binary_IO.f90 \
            CRTM_Surface_Define.f90 \
              CRTM_SensorData_Define.f90 \
            CRTM_Surface_Binary_IO.f90 \
            CRTM_GeometryInfo_Define.f90 \
            CRTM_GeometryInfo_IO.f90

# Obj files used in link phase
OBJ_FILES = ${SRC_FILES:.f90=.o} \
            $(EXE_FILE).o

# Define common make targets (all, build, clean, install)
include $(CRTM_SOURCE_ROOT)/make.common_targets

# Common definitions and targets for test data
include $(CRTM_SOURCE_ROOT)/make.test_data
 
# Source link creation and removal
create_links: create_ginfo_links
	@$(LINK_SCRIPT) $(CRTM_SOURCE_ROOT) $(SRC_FILES)

remove_links: remove_ginfo_links
	@$(UNLINK_SCRIPT) $(SRC_FILES)

# Squeaky clean target
realclean: clean remove_links

# Source dependency lists
include make.dependencies

# Define default rules
include $(CRTM_SOURCE_ROOT)/make.rules

