#==============================================================================
#
# Makefile for SSMIS training program
#
#==============================================================================
ROOT=$(CRTM_SOURCE_ROOT)
ODPS_TRAINING_DIR=$(CRTM_SOURCE_ROOT)/TauRegress/ODPS/ODPS_Regress
# -------------
# Define macros
# -------------
include $(ROOT)/make.macros

# This makefile
MAKE_FILE = Makefile

# Executable files
EXE_FILE = Compute_Coeff 
  
# Source files to link
SRC_FILES = Type_Kinds.f90 \
            File_Utility.f90 \
            Compare_Float_Numbers.f90 \
            Message_Handler.f90 \
            netCDF_Utility.f90 \
            netCDF_Dimension_Utility.f90 \
            netCDF_Variable_Utility.f90 \
            netCDF_Attribute_Utility.f90 \
            SpcCoeff_Define.f90 \
            SpcCoeff_netCDF_IO.f90 \
            SensorInfo_Parameters.f90 \
            AntCorr_Define.f90 \
            AntCorr_netCDF_IO.f90 \
            Sort_Utility.f90 \
            String_Utility.f90 \
            Compare_Float_Numbers.f90 \
            Endian_Utility.f90 \
            Binary_File_Utility.f90 \
            ODPS_ZAtmAbsorption.f90 \
            ODPS_Define.f90 \
            ODPS_Binary_IO.f90 \
            ODPS_Predictor_Define.f90 \
            ODPS_Predictor.f90 \
            ODPS_netCDF_IO.f90

ODPS_TRAINING_SRC_FILES=Regression.f90 \
                  PredictorSubsetIndex.f90

GEN_TRANS_SRC_FILES = Compute_Coeff.f90 \
                      TauProfile.f90


# Obj files used in link phase
OBJ_FILES = ${SRC_FILES:.f90=.o} \
            ${ODPS_TRAINING_SRC_FILES:.f90=.o}\
            ${GEN_TRANS_SRC_FILES:.f90=.o}
             

INCLUDES = -I$(HOME)/local/netcdf/include
LIBRARIES = -lessl -L$(HOME)/local/netcdf/lib -lnetcdf

# ------------------
# Define dependecies
# ------------------
include $(ROOT)/make.common_targets

# Create and remove source links
create_links:
	@linkfiles $(ROOT) $(SRC_FILES)
	@linkfiles $(ODPS_TRAINING_DIR) $(ODPS_TRAINING_SRC_FILES) 
                   
remove_links:
	@unlinkfiles $(SRC_FILES)
	@unlinkfiles $(ODPS_TRAINING_SRC_FILES)
 
# Squeaky clean target
realclean:: clean remove_links

# Source dependencies
include make.dependencies

# --------------------
# Define default rules
# --------------------
include $(ROOT)/make.rules
