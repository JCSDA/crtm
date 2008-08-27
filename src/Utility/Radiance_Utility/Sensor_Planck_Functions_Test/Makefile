#==============================================================================
#
# Makefile for Sensor Planck functions test program
#
#==============================================================================

# Define macros
include $(CRTM_SOURCE_ROOT)/make.macros

# This makefile
MAKE_FILE = Makefile

# Executable file
EXE_FILE = Sensor_Planck_Functions_Test

# Source files to link
SRC_FILES = Type_Kinds.f90 \
            File_Utility.f90 \
            Message_Handler.f90 \
            Fundamental_Constants.f90 \
            Compare_Float_Numbers.f90 \
            netCDF_Utility.f90 \
            netCDF_Dimension_Utility.f90 \
            netCDF_Variable_Utility.f90 \
            netCDF_Attribute_Utility.f90 \
            AntCorr_Define.f90 \
            AntCorr_netCDF_IO.f90 \
            SpcCoeff_Define.f90 \
            SpcCoeff_netCDF_IO.f90 \
            Planck_Functions.f90 \
            Sensor_Planck_Functions.f90

# Obj files used in link phase
OBJ_FILES = ${SRC_FILES:.f90=.o} \
            $(EXE_FILE).o

# Test data file
DATA_FILES = hirs3_n17.SpcCoeff.nc

# Include and library definitions
INCLUDES = -I$(HOME)/local/netcdf/include
LIBRARIES = -L$(HOME)/local/netcdf/lib -lnetcdf

# Define common make targets (all, build, clean, install)
include $(CRTM_SOURCE_ROOT)/make.common_targets

# Source link creation and removal
create_links:
	@linkfiles $(CRTM_SOURCE_ROOT) $(SRC_FILES)
	@linkfiles $(CRTM_FIXFILE_ROOT) $(DATA_FILES)

remove_links:
	@unlinkfiles $(SRC_FILES)
	@unlinkfiles $(DATA_FILES)

# Squeaky clean target
realclean: clean remove_links

# Source dependency lists
include make.dependencies

# Define default rules
include $(CRTM_SOURCE_ROOT)/make.rules
