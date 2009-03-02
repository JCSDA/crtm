#==============================================================================
#
# Makefile for Test_FitStats program
#
#==============================================================================

# Define macros
include $(CRTM_SOURCE_ROOT)/make.macros

# This makefile
MAKE_FILE = Makefile

# Executable file
EXE_FILE = Test_FitStats

# Source files to link
SRC_FILES = Type_Kinds.f90 \
            File_Utility.f90 \
            Message_Handler.f90 \
            Compare_Float_Numbers.f90 \
            SensorInfo_Parameters.f90 \
            String_Utility.f90 \
            FitStats_Define.f90 \
            FitStats_netCDF_IO.f90

# Obj files used in link phase
OBJ_FILES = ${SRC_FILES:.f90=.o} \
            $(EXE_FILE).o

# Include and library definitions
INCLUDES = -I$(HOME)/local/netcdf/include
LIBRARIES = -L$(HOME)/local/netcdf/lib -lnetcdf

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

