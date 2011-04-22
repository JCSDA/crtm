#==============================================================================
#
# Makefile for Apodize_TauSpc_with_IRF program
#
#==============================================================================

# Define macros

include make.macros

# This makefile
MAKE_FILE = Makefile

# Executable file
EXE_FILE = Compute_NLTE_Coeff

# Source files to link
SRC_FILES = netCDF_Utility.f90 \
            netCDF_Dimension_Utility.f90 \
            netCDF_Attribute_Utility.f90 \
            netCDF_Variable_Utility.f90 \
            DeltaRadTrainSet_netCDF_IO.f90 \
            DeltaRad_TrainSet_Define.f90 \
            Type_Kinds.f90 \
            Message_Handler.f90 \
            File_Utility.f90 \
            Endian_Utility.f90 \
            String_Utility.f90 \
            Binary_File_Utility.f90 \
            CRTM_SpcCoeff.f90 \
            CRTM_Planck_Functions.f90 \
            Compare_Float_Numbers.f90 \
            ACCoeff_Define.f90 \
            ACCoeff_Binary_IO.f90 \
            NLTE_Parameters.f90 \
            NLTECoeff_Define.f90 \
            NLTECoeff_netCDF_IO.f90 \
            NLTECoeff_Binary_IO.f90 \
            Regression.f90 \
            SensorInfo_Parameters.f90 \
            SpcCoeff_Define.f90 \
            SpcCoeff_Binary_IO.f90 \
            CRTM_Parameters.f90
            
# Obj files used in link phase
OBJ_FILES = ${SRC_FILES:.f90=.o} \
            $(EXE_FILE).o

# Library and include macros
NETCDF_LIB = $(HOME)/local/netcdf
INCLUDES = -I$(NETCDF_LIB)/include -I$(FFT_LIB)/include #-I${CRTM_SOURCE_ROOT}/Build/include
LIBRARIES = -lessl -L$(NETCDF_LIB)/lib -lnetcdf # -L${CRTM_SOURCE_ROOT}/Build/lib -lCRTM 

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

