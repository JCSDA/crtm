#==============================================================================
#
# Makefile for Create_AtmProfile program
#
#==============================================================================

# -------------
# Define macros
# -------------
include $(CRTM_SOURCE_ROOT)/make.macros

# This makefile
MAKE_FILE = Makefile

# Executable files
EXE_FILE = Create_AtmProfile

# Source files to link
SRC_FILES = Type_Kinds.f90 \
            File_Utility.f90 \
            Message_Handler.f90 \
            Compare_Float_Numbers.f90 \
            Fundamental_Constants.f90 \
            String_Utility.f90 \
            Search_Utility.f90 \
            Interpolate_Utility.f90 \
              Linear_Interpolation.f90 \
              Polynomial_Interpolation.f90 \
              Spline_Interpolation.f90 \
            AtmProfile_Parameters.f90 \
            AtmProfile_Define.f90 \
            AtmProfile_netCDF_IO.f90 \
            Profile_Utility_Parameters.f90 \
            Atmospheric_Properties.f90 \
            Units_Conversion.f90 \
            Geopotential.f90 \
            UMBC_Profile_Set.f90 \
            CIMSS_Profile_Set.f90 \
            ECMWF52_Profile_Set.f90 \
            Model_Profile_Set.f90

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
