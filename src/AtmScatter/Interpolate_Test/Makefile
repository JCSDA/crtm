#==============================================================================
#
# Makefile for Test_Interp_2D program
#
#==============================================================================

# Define macros
include $(CRTM_SOURCE_ROOT)/make.macros

# This makefile
MAKE_FILE = Makefile

# Executable file
EXE_FILE = Test_CScatter_Interp

# Source files to link
SRC_FILES = Type_Kinds.f90 \
            File_Utility.f90 \
            Compare_Float_Numbers.f90 \
            Message_Handler.f90 \
            Endian_Utility.f90 \
            Binary_File_Utility.f90 \
            CRTM_Parameters.f90 \
            SpcCoeff_Binary_IO.f90 \
            CRTM_Cloud_Define.f90 \
            CRTM_Aerosol_Define.f90 \
            CRTM_Aerosol_Binary_IO.f90 \
            CRTM_Cloud_Binary_IO.f90 \
            CRTM_Atmosphere_Binary_IO.f90 \
            CRTM_Atmosphere_Define.f90 \
            SpcCoeff_Define.f90 \
            CRTM_SpcCoeff.f90 \
            CloudCoeff_Binary_IO.f90 \
            CloudCoeff_Define.f90 \
            CRTM_CloudCoeff.f90 \
            CRTM_GeometryInfo_Define.f90 \
            CRTM_AtmScatter_Define.f90 \
            CRTM_CloudScatter.f90 \
            Interp_ND.f90
            
            
# Source and (big-endian) data file link creation and removal
SENSORID_LIST = amsua_n17 \
	        hirs3_n17 \
		ssmis_f16 \
		airs281SUBSET_aqua
            

# Obj files used in link phase
OBJ_FILES = ${SRC_FILES:.f90=.o} \
            $(EXE_FILE).o

# Define common make targets (all, build, clean, install)
include $(CRTM_SOURCE_ROOT)/make.common_targets

# Source link creation and removal
create_links:
	@linkfiles $(CRTM_SOURCE_ROOT) $(SRC_FILES)
	@linkcoeffdata -b $(SENSORID_LIST)

remove_links:
	@unlinkfiles $(SRC_FILES)

# Squeaky clean target
realclean: clean remove_links

# Source dependency lists
#include make.dependencies

# Define default rules
include $(CRTM_SOURCE_ROOT)/make.rules
