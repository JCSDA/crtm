#==============================================================================
#
# Makefile for Compute_RadDiag_Stats program
#
#==============================================================================

# Define macros
include $(CRTM_SOURCE_ROOT)/make.macros

# This makefile
MAKE_FILE = Makefile

# Executable file
EXE_FILE = Compute_RadDiag_Stats

# Source files to link
SRC_FILES = Type_Kinds.f90 \
            File_Utility.f90 \
            Message_Handler.f90 \
            String_Utility.f90 \
            List_File_Utility.f90 \
            SensorInfo_Parameters.f90 \
            SensorInfo_Define.f90 \
            SensorInfo_LinkedList.f90 \
            SensorInfo_IO.f90 \
            SatInfo_Define.f90 \
            SatInfo_IO.f90 \
            SatBias_Define.f90 \
            SatBias_IO.f90 \
            RadDiag_Define.f90 RadDiag_Hdr_Define.f90 RadDiag_Data_Define.f90 \
            RadDiag_IO.f90 \
            RadDiag_Stats_Define.f90 \
            RadDiag_Stats_netCDF_IO.f90

# Obj files used in link phase
OBJ_FILES = $(SRC_FILES:.f90=.o) \
            $(EXE_FILE).o

# Include and library definitions
INCLUDES = -I$(HOME)/local/netcdf/include
LIBRARIES = -L$(HOME)/local/netcdf/lib -lnetcdf

# Define common make targets (all, build, clean, install)
include $(CRTM_SOURCE_ROOT)/make.common_targets

# Create and remove source links
create_links:
	@$(LINK_SCRIPT) $(CRTM_SOURCE_ROOT) $(SRC_FILES)

remove_links:
	@$(UNLINK_SCRIPT) $(SRC_FILES)
        
# Squeaky clean target
realclean: clean remove_links

# Source dependency lists
include make.dependencies

# Define default rules
include $(CRTM_SOURCE_ROOT)/make.rules
