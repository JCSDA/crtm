#==============================================================================
#
# Makefile for Create_SpcCoeff_from_SRF program
#
#==============================================================================

# Define macros
include $(CRTM_SOURCE_ROOT)/make.macros

# This makefile
MAKE_FILE = Makefile

# Executable file
EXE_FILE = Create_SpcCoeff

# Source files to link
SRC_FILES = Type_Kinds.f90 \
            File_Utility.f90 \
            Sort_Utility.f90 \
            Message_Handler.f90 \
            Compare_Float_Numbers.f90 \
            Fundamental_Constants.f90 \
            String_Utility.f90 \
            SignalFile_Utility.f90 \
            Search_Utility.f90 \
            Interpolate_Utility.f90 \
            Linear_Interpolation.f90 \
            Polynomial_Interpolation.f90 \
            Spline_Interpolation.f90 \
            Integrate_Utility.f90 \
            Planck_Functions.f90 \
            Spectral_Units_Conversion.f90 \
            SensorInfo_Parameters.f90 \
            SpcCoeff_Define.f90 \
            ACCoeff_Define.f90 \
            NLTECoeff_Define.f90 \
            Subset_Define.f90 \
            SpcCoeff_netCDF_IO.f90 \
            MW_SensorData_Define.f90 \
            PtrArr_Define.f90 \
            oSRF_Parameters.f90 \
            oSRF_Define.f90 \
            oSRF_File_Define.f90 \
            Solar_Define.f90 \
            Solar_IO.f90  
            
INC_FILES = oSRF_Flag_Parameters.inc \
            oSRF_Flag_Visibilities.inc \
            oSRF_Flag_Procedures.inc \
            oSRF_Flag_Usage.inc \
            oSRF_Sensor_Visibilities.inc \
            oSRF_Sensor_Procedures.inc \
            oSRF_Sensor_Usage.inc


# Obj files used in link phase
OBJ_FILES = ${SRC_FILES:.f90=.o} \
            $(EXE_FILE).o

# Include and library definitions
INCLUDES = -I$(HOME)/local/include
LIBRARIES = -L$(HOME)/local/lib -lnetcdff

# Define common make targets (all, build, clean, install)
include $(CRTM_SOURCE_ROOT)/make.common_targets

# Source link creation and removal
create_links::
	@$(LINK_SCRIPT) $(CRTM_SOURCE_ROOT) $(SRC_FILES) $(INC_FILES)

remove_links::
	@$(UNLINK_SCRIPT) $(SRC_FILES) $(INC_FILES)

# Squeaky clean target
realclean:: clean remove_links

# Source dependency lists
include make.dependencies

# Define default rules
include $(CRTM_SOURCE_ROOT)/make.rules
