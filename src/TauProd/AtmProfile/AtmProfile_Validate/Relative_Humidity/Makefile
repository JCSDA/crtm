#==============================================================================
#
# Makefile for AtmProfile_Validate_RH
#
#==============================================================================

# Define macros
include $(CRTM_SOURCE_ROOT)/make.macros

# This makefile
MAKE_FILE = Makefile

# Executable file
EXE_FILE = AtmProfile_Validate_RH

# Source files to link
SRC_FILES = Type_Kinds.f90 \
            File_Utility.f90 \
            Message_Handler.f90 \
            Fundamental_Constants.f90 \
            Profile_Utility_Parameters.f90 \
            Atmospheric_Properties.f90 \
            RH_MR.f90 \
            String_Utility.f90 \
            Compare_Float_Numbers.f90 \
            AtmProfile_Define.f90 \
            AtmProfile_netCDF_IO.f90

# Obj files used in link phase
OBJ_FILES = ${SRC_FILES:.f90=.o} \
            $(EXE_FILE).o
            
# Include and library definitions
INCLUDES = -I$(HOME)/local/netcdf/include
LIBRARIES = -L$(HOME)/local/netcdf/lib -lnetcdf

# Define common make targets (all, build, clean, install)
include $(CRTM_SOURCE_ROOT)/make.common_targets

# AtmProfile files to link
ATMPROFILE_FILES = CIMSS32.AtmProfile.nc \
                   ECMWF52.AtmProfile.nc \
                   ECMWF5K.AtmProfile.nc \
                   ECMWF83.AtmProfile.nc \
                   Model6.AtmProfile.nc \
                   UMBC48.AtmProfile.nc

# Source link creation and removal
create_links:
	@$(LINK_SCRIPT) $(CRTM_SOURCE_ROOT) $(SRC_FILES)
	@$(LINK_SCRIPT) $(CRTM_FIXFILE_ROOT) $(ATMPROFILE_FILES)

remove_links:
	@$(UNLINK_SCRIPT) $(SRC_FILES)
	@$(UNLINK_SCRIPT) $(ATMPROFILE_FILES)        

# Squeaky clean target
realclean: clean remove_links

# Source dependency lists
include make.dependencies

# Define default rules
include $(CRTM_SOURCE_ROOT)/make.rules
