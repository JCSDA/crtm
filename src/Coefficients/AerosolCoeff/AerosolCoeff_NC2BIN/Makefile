#==============================================================================
#
# Makefile for AerosolCoeff_NC2BIN program
#
#==============================================================================

# Define macros
include $(CRTM_SOURCE_ROOT)/make.macros

# This makefile
MAKE_FILE = Makefile

# Executable file
EXE_FILE = AerosolCoeff_NC2BIN
SCRIPT_FILE = run_$(EXE_FILE).sh

# Source files to link
SRC_FILES = Type_Kinds.f90 \
            File_Utility.f90 \
            Message_Handler.f90 \
            Compare_Float_Numbers.f90 \
            Endian_Utility.f90 \
            Binary_File_Utility.f90 \
            String_Utility.f90 \
            Fundamental_Constants.f90 \
            Spectral_Utility.f90 \
              Spectral_Units_Conversion.f90 \
            SignalFile_Utility.f90 \
            AerosolCoeff_Define.f90 \
            AerosolCoeff_IO.f90 \
              AerosolCoeff_Binary_IO.f90 \
              AerosolCoeff_netCDF_IO.f90

# Obj files used in link phase
OBJ_FILES = ${SRC_FILES:.f90=.o} \
            $(EXE_FILE).o

# Include and library definitions
INCLUDES  = -I$(NC4_DIR)/include \
	    -I$(HDF_DIR)/include
LIBRARIES = -L$(NC4_DIR)/lib -lnetcdf -lnetcdff \
	    -L$(HDF_DIR)/lib -lhdf5


# Define common make targets (all, build, clean, install)
include $(CRTM_SOURCE_ROOT)/make.common_targets

# Source link creation and removal
create_links::
	@$(LINK_SCRIPT) $(CRTM_SOURCE_ROOT) $(SRC_FILES)

remove_links::
	@$(UNLINK_SCRIPT) $(SRC_FILES)

# Script install target
install_script:
	@$(COPY) $(SCRIPT_FILE) ${HOME}/bin

# Source dependency lists
include make.dependencies

# Define default rules
include $(CRTM_SOURCE_ROOT)/make.rules

