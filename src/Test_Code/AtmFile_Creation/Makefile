#==============================================================================
#
# Makefile for CRTM CloudScatter Tangent-Linear Test program
#
#==============================================================================

include $(CRTM_SOURCE_ROOT)/make.macros

# This makefile
MAKE_FILE = Makefile

# Executable files
EXE_FILE = Atmosphere_DataFile_Create

# Source files to link
SRC_FILES = Type_Kinds.f90 \
            File_Utility.f90 \
            Message_Handler.f90 \
            Compare_Float_Numbers.f90 \
            Endian_Utility.f90 \
            Binary_File_Utility.f90 \
            Sort_Utility.f90 \
            CRTM_Aerosol_Define.f90 \
            CRTM_Cloud_Define.f90 \
            Fundamental_Constants.f90 \
            CRTM_Parameters.f90 \
            CRTM_Atmosphere_Define.f90 \
            CRTM_Surface_Define.f90 \
            CRTM_SensorData_Define.f90 \
            CRTM_Cloud_Binary_IO.f90 \
            CRTM_Aerosol_Binary_IO.f90 \
            netCDF_Dimension_Utility.f90 \
            netCDF_Attribute_Utility.f90 \
            netCDF_Variable_Utility.f90 \
            netCDF_Utility.f90 \
            CRTM_Atmosphere_Binary_IO.f90 \
            CRTM_Surface_Binary_IO.f90 
                       
# Obj files used in link phase
OBJ_FILES = ${SRC_FILES:.f90=.o} \
            $(EXE_FILE).o

# Include and library definitions
INCLUDES = -I$(HOME)/local/netcdf/include
LIBRARIES = -L$(HOME)/local/netcdf/lib -lnetcdf

# Source and (big-endian) data file link creation and removal
SENSORID_LIST = amsua_n17 \
                hirs3_n17 \
                ssmis_f16 \
                imgr_g11

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

