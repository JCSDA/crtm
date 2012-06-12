#==============================================================================
#
# Makefile for Get_Ref_Profile program
#
#==============================================================================
ROOT=$(CRTM_SOURCE_ROOT)
NETCDF=/apps/netcdf/4.1.3-intel

# -------------
# Define macros
# -------------
include $(ROOT)/make.macros

# This makefile
MAKE_FILE = Makefile

# Executable files
EXE_FILE = Compute_Coeff 
#Get_Ref_Profile

# Source files to link
SRC_FILES = Type_Kinds.f90 \
            File_Utility.f90 \
            Compare_Float_Numbers.f90 \
            Message_Handler.f90 \
            String_Utility.f90 \
            netCDF_Utility.f90 \
            netCDF_Dimension_Utility.f90 \
            netCDF_Variable_Utility.f90 \
            netCDF_Attribute_Utility.f90 \
            AtmProfile_Define.f90 \
            AtmProfile_netCDF_IO.f90 \
            String_Utility.f90 \
            TauProfile_Define.f90 \
            TauProfile_netCDF_IO.f90 \
            SpcCoeff_Define.f90 \
            SpcCoeff_netCDF_IO.f90 \
            SensorInfo_Parameters.f90 \
            ACCoeff_Define.f90 \
            Sort_Utility.f90 \
            Compare_Float_Numbers.f90 \
            Endian_Utility.f90 \
            Binary_File_Utility.f90 \
            ODPS_Predictor.f90 \
            ODPS_Predictor_Define.f90 \
            ODPS_Define.f90 \
            ODPS_netCDF_IO.f90 \
            CRTM_Parameters.f90 \
            Subset_Define.f90 \
            NLTECoeff_Define.f90 
            


GEN_TRANS_SRC_FILES = Parameters.f90 \
                      Compute_Coeff.f90 \
                      PlanckFunc.f90 \
                      CalcStatTransTemp.f90 \
                      Utilities_Statistics.f90 \
		      PredictorSubsetIndex.f90 \
                      Interpolation.f90 \
                      Regression.f90
  
# Obj files used in link phase
OBJ_FILES = ${SRC_FILES:.f90=.o} \
            ${GEN_TRANS_SRC_FILES:.f90=.o} 
             


#INCLUDES = -I$(HOME)/local/netcdf/include
#LIBRARIES = -lessl -L$(HOME)/local/netcdf/lib -lnetcdf -L$(HOME)/local/lapack/bin -llapack -lblas
#LIBRARIES = -lessl -L$(HOME)/local/netcdf/lib -lnetcdf 
INCLUDES = -I$(NETCDF)/include
LIBRARIES = -mkl=sequential -L$(NETCDF)/lib -lnetcdff 


# ------------------
# Define dependecies
# ------------------
include $(ROOT)/make.common_targets

# Create and remove source links
create_links::
	@$(LINK_SCRIPT) $(ROOT) $(SRC_FILES)
                   
remove_links::
	@$(UNLINK_SCRIPT) $(SRC_FILES)

# Source dependencies
include make.dependencies


# --------------------
# Define default rules
# --------------------
include $(ROOT)/make.rules
