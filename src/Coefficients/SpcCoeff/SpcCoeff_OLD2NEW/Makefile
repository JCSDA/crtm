#==============================================================================
#
# Makefile for SpcCoeff_OLD2NEW program
#
# $Id$
#
#==============================================================================

# Define macros
include $(CRTM_SOURCE_ROOT)/make.macros

# This makefile
MAKE_FILE = Makefile

# Executable file
EXE_FILE = SpcCoeff_OLD2NEW

# Source files to link
SRC_FILES = Type_Kinds.f90 \
            File_Utility.f90 \
            Sort_Utility.f90 \
            Message_Handler.f90 \
            Compare_Float_Numbers.f90 \
            Fundamental_Constants.f90 \
            String_Utility.f90 \
            Search_Utility.f90 \
            SensorInfo_Parameters.f90 \
            SpcCoeff_Define.f90 \
              ACCoeff_Define.f90 \
              NLTECoeff_Define.f90 \
            SpcCoeff_netCDF_IO.f90 \
            SpcCoeff_Define_old.f90 \
              AntCorr_Define_old.f90 \
            SpcCoeff_netCDF_IO_old.f90 \
              AntCorr_netCDF_IO_old.f90

# Obj files used in link phase
OBJ_FILES = ${SRC_FILES:.f90=.o} \
            $(EXE_FILE).o

# Include and library definitions
INCLUDES = -I$(HOME)/local/netcdf/include
LIBRARIES = -L$(HOME)/local/netcdf/lib -lnetcdf

# Define common make targets (all, build, clean, install)
include $(CRTM_SOURCE_ROOT)/make.common_targets

# Source link creation and removal
create_links::
	@$(LINK_SCRIPT) $(CRTM_SOURCE_ROOT) $(SRC_FILES)

remove_links::
	@$(UNLINK_SCRIPT) $(SRC_FILES)

# Squeaky clean target
realclean:: clean remove_links

# Source dependency lists
include make.dependencies

# Define default rules
include $(CRTM_SOURCE_ROOT)/make.rules
