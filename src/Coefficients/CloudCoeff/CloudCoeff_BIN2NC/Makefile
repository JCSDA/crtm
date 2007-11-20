#==============================================================================
#
# Makefile for writing a CloudCoeff netcdf file from
# binary CloudCoeff file
#
# $Id: Makefile,v 1.2 2006/05/02 14:58:34 dgroff Exp $
#
#==============================================================================

include $(CRTM_SOURCE_ROOT)/make.macros

# -------------
# This makefile
# -------------

MAKE_FILE = Makefile

# ---------------
# Executable file
# ---------------

EXE_FILE = CloudCoeff_BIN2NC


# --------------------
# Source files to link
# --------------------
SRC_FILES = Type_Kinds.f90 \
            File_Utility.f90 \
            Message_Handler.f90 \
            Compare_Float_Numbers.f90 \
            Endian_Utility.f90 \
            Binary_File_Utility.f90 \
            CloudCoeff_Define.f90 \
            CloudCoeff_Binary_IO.f90 \
	    netCDF_Dimension_Utility.f90 \
	    netCDF_Variable_Utility.f90 \
	    netCDF_Attribute_Utility.f90 \
	    netCDF_Utility.f90  \
	    CloudCoeff_netCDF_IO.f90 

OBJ_FILES = ${SRC_FILES:.f90=.o} \
            $(EXE_FILE).o
	    
	    
# Include and library definitions
INCLUDES = -I$(HOME)/local/netcdf/include
LIBRARIES = -L$(HOME)/local/netcdf/lib -lnetcdf
 
SENSORID_LIST =  
       
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
#include make.dependencies

# Define default rules
include $(CRTM_SOURCE_ROOT)/make.rules 
          


