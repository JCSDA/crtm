#==============================================================================
#
# Makefile for Compute_RadDiag_Stats program
#
#==============================================================================

# -------------
# Define macros
# -------------
include $(CRTM_SOURCE_ROOT)/make.macros

# This makefile
MAKE_FILE = Makefile

# Executable file
EXE_FILE = Compute_RadDiag_Stats

# Source files to link
CRTM_SRC_FILES = Type_Kinds.f90 \
                 File_Utility.f90 \
                 Message_Handler.f90 \
                 List_File_Utility.f90 \
                 netCDF_Utility.f90 \
                 netCDF_Dimension_Utility.f90 \
                 netCDF_Variable_Utility.f90 \
                 netCDF_Attribute_Utility.f90
GSI_SRC_FILES = Sensor_Utility.f90 \
                SatInfo_Define.f90 \
                SatInfo_IO.f90 \
                SatBias_Define.f90 \
                SatBias_IO.f90 \
                RadDiag_Define.f90 \
                RadDiag_IO.f90 \
                RadDiag_Stats_Define.f90 \
                RadDiag_Stats_netCDF_IO.f90

# Obj files used in link phase
OBJ_FILES = $(CRTM_SRC_FILES:.f90=.o) \
            $(GSI_SRC_FILES:.f90=.o) \
            $(EXE_FILE).o

# Include and library definitions
INCLUDES = -I$(HOME)/local/netcdf/include
LIBRARIES = -L$(HOME)/local/netcdf/lib -lnetcdf


# ------------------
# Define dependecies
# ------------------
include $(CRTM_SOURCE_ROOT)/make.common_targets

# Create and remove source links
create_links:
	@linkfiles $(CRTM_SOURCE_ROOT) $(CRTM_SRC_FILES)
	@linkfiles $(GSI_SOURCE_ROOT) $(GSI_SRC_FILES)

remove_links:
	@unlinkfiles $(CRTM_SRC_FILES) $(GSI_SRC_FILES)
        
# Source dependencies
include make.dependencies


# --------------------
# Define default rules
# --------------------
include $(CRTM_SOURCE_ROOT)/make.rules
