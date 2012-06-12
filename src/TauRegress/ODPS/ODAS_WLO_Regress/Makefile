include $(CRTM_SOURCE_ROOT)/make.macros
NETCDF=/apps/netcdf/4.1.3-intel
 
# -------------
# This makefile
# -------------

  MAKE_FILE = Makefile


# ----------------------
# Executable load module
# ----------------------

  EXE_FILE = gencoef

# -----------------
# Log file for make
# -----------------

  LOG_FILE = log.make.$(EXE_FILE)


# ------------
# Source files
# ------------

GEN_TRANS_SRC_FILES = \
	ParametersGenCoef.f90		\
 	ReadParameters.f90		\
 	ReadProfile_netCDF.f90		\
	AbsorberAmount.f90		\
        PredictandPredictor.f90         \
        WriteTransTable.f90		\
        predictors.f90 \
 	GenTransCoef.f90

SRC_FILES = Type_Kinds.f90 \
            File_Utility.f90 \
            Message_Handler.f90 \
            Compare_Float_Numbers.f90 \
            String_Utility.f90 \
            Sort_Utility.f90 \
            netCDF_Utility.f90 \
            netCDF_Dimension_Utility.f90 \
            netCDF_Variable_Utility.f90 \
   	    netCDF_Attribute_Utility.f90 \
            SensorInfo_Parameters.f90 \
            SpcCoeff_Define.f90 \
            SpcCoeff_netCDF_IO.f90 \
            ACCoeff_Define.f90 \
            TmpODAS_Define.f90 \
            TmpODAS_netCDF_IO.f90 \
            MR_PPMV.f90 \
            Fundamental_Constants.f90 \
            Profile_Utility_Parameters.f90 \
            Atmospheric_Properties.f90 \
            TauProfile_Define.f90 \
            TauProfile_netCDF_IO.f90 \
            TmpAtmProfile_Define.f90 \
            TmpAtmProfile_netCDF_IO.f90 \
   	    PlanckFunc.f90 \
            Utilities_Statistics.f90 \
            absorber_profile.f90 \
            parameters.f90 \
            CalcRegCoef.f90 \
            PredAbsCoefTransTemp.f90 \
            CalcStatTransTemp.f90 \
            CalcRegWeight.f90 \
            ChanType.f90 \
            ConvLevelLayer.f90 \
            Subset_Define.f90 \
            NLTECoeff_Define.f90 

# Obj files used in link phase
 
OBJ_FILES = ${SRC_FILES:.f90=.o} \
            ${GEN_TRANS_SRC_FILES:.f90=.o}
 

# Include and library definitions
#INCLUDES = -I$(HOME)/local/netcdf/include
#LIBRARIES = -lessl -L$(HOME)/local/netcdf/lib -lnetcdf 
#LIBRARIES = -L$(HOME)/local/netcdf/lib -lnetcdf
INCLUDES = -I$(NETCDF)/include
LIBRARIES = -mkl=sequential -L$(NETCDF)/lib -lnetcdff 


# ------------------
# Define dependecies
# ------------------
include $(CRTM_SOURCE_ROOT)/make.common_targets

# Create and remove source links
create_links::
	@$(LINK_SCRIPT) $(CRTM_SOURCE_ROOT) $(SRC_FILES)
                   
remove_links::
	@$(UNLINK_SCRIPT) $(SRC_FILES)

# Source dependencies
include make.dependencies


# --------------------
# Define default rules
# --------------------
include $(CRTM_SOURCE_ROOT)/make.rules
 
