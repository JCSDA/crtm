#==============================================================================
#
# Makefile for CRTM AtmAbsorption Tangent-Linear Test program
#
# $Id: Makefile,v 1.15 2006/11/21 22:34:46 dgroff Exp $
#
#==============================================================================

include $(CRTM_SOURCE_ROOT)/make.macros

# This makefile
MAKE_FILE = Makefile

# Executable file
EXE_FILE = Test_Tangent_Linear

# Source files
SRC_FILES = Type_Kinds.f90 \
            File_Utility.f90 \
            Message_Handler.f90 \
            Compare_Float_Numbers.f90 \
            Endian_Utility.f90 \
            Binary_File_Utility.f90 \
	    Sort_Utility.f90 \
	    SpcCoeff_Define.f90 \
	    SpcCoeff_Binary_IO.f90 \
	    TauCoeff_Define.f90 \
	    TauCoeff_Binary_IO.f90 \
	    AerosolCoeff_Define.f90 \
	    AerosolCoeff_Binary_IO.f90 \
	    CloudCoeff_Define.f90 \
	    CloudCoeff_Binary_IO.f90 \
	    EmisCoeff_Define.f90 \
	    EmisCoeff_Binary_IO.f90 \
            CRTM_Parameters.f90 \
            CRTM_Cloud_Define.f90 \
            CRTM_Aerosol_Define.f90 \
            CRTM_Cloud_Binary_IO.f90 \
            CRTM_Aerosol_Binary_IO.f90 \
	    CRTM_SpcCoeff.f90 \
	    CRTM_TauCoeff.f90 \
	    CRTM_AerosolCoeff.f90 \
	    CRTM_CloudCoeff.f90 \
	    CRTM_EmisCoeff.f90 \
	    CRTM_Atmosphere_Define.f90 \
	    CRTM_Atmosphere_Binary_IO.f90 \
 	    CRTM_ChannelInfo_Define.f90 \
 	    CRTM_GeometryInfo_Define.f90 \
 	    CRTM_Predictor_Define.f90 \
   	    CRTM_Predictor.f90 \
   	    CRTM_AtmAbsorption_Define.f90 \
   	    CRTM_AtmAbsorption.f90 \
	    CRTM_LifeCycle.f90 \
            netCDF_Dimension_Utility.f90 \
            netCDF_Variable_Utility.f90 \
            netCDF_Attribute_Utility.f90 \
            netCDF_Utility.f90 \
	    ComponentTest_Define.f90 \
 	    ComponentTest_netCDF_IO.f90 

# Obj files for linking
OBJ_FILES = ${SRC_FILES:.f90=.o} $(EXE_FILE).o

# Include and library definitions
INCLUDES = -I$(HOME)/local/netcdf/include
LIBRARIES = -L$(HOME)/local/netcdf/lib -lnetcdf


# -------------------
# Define dependencies
# -------------------
# Common targets (all, build, clean, install)
include $(CRTM_SOURCE_ROOT)/make.common_targets

# Very clean
realclean: remove_links clean

# Source and (big-endian) data file link creation and removal
SENSORID_LIST = amsua_n17 \
		hirs3_n17 \
		ssmis_f16 \
		imgr_g11

create_links:
	@linkfiles $(CRTM_SOURCE_ROOT) $(SRC_FILES)
	@linkcoeffdata -b $(SENSORID_LIST)
	@linktestdata -b

remove_links:
	@unlinkfiles $(SRC_FILES)
	@unlinkcoeffdata
	@unlinktestdata


# Dependency list
include make.dependencies

# Default suffix rules
include $(CRTM_SOURCE_ROOT)/make.rules

