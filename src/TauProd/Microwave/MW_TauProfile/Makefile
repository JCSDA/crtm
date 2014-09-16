#==============================================================================
#
# Makefile for MW_TauProfile program
#
#==============================================================================

# -------------
# Define macros
# -------------
include $(CRTM_SOURCE_ROOT)/make.macros

# This makefile
MAKE_FILE = Makefile

# Executable files
EXE_FILE = MW_TauProfile

# Source files to link
SRC_FILES = Type_Kinds.f90 \
            File_Utility.f90 \
            Message_Handler.f90 \
            Compare_Float_Numbers.f90 \
            Binary_File_Utility.f90 \
            Endian_Utility.f90 \
            Fundamental_Constants.f90 \
            String_Utility.f90 \
            Sort_Utility.f90 \
            Search_Utility.f90 \
            Integrate_Utility.f90 \
            Interpolate_Utility.f90 \
            Linear_Interpolation.f90 \
            Polynomial_Interpolation.f90 \
            Spline_Interpolation.f90 \
            Date_Utility.f90 \
            DateTime_Utility.f90 \
            Timing_Utility.f90 \
            Subset_Define.f90 \
            netCDF_Utility.f90 \
            netCDF_Dimension_Utility.f90 \
            netCDF_Variable_Utility.f90 \
            netCDF_Attribute_Utility.f90 \
            SensorInfo_Parameters.f90 \
            SensorInfo_Define.f90 \
            SensorInfo_LinkedList.f90 \
            SensorInfo_IO.f90 \
            TauProfile_Define.f90 \
            TauProfile_netCDF_IO.f90 \
            AtmProfile_Parameters.f90 \
            AtmProfile_Define.f90 \
            AtmProfile_netCDF_IO.f90 \
            ACCoeff_Define.f90 \
            NLTECoeff_Define.f90 \
            SpcCoeff_Define.f90 \
            PtrArr_Define.f90 \
            oSRF_Parameters.f90 \
            oSRF_Define.f90 \
            oSRF_File_Define.f90 \
            Planck_Functions.f90 \
            Spectral_Units_Conversion.f90 \
            CRTM_GeometryInfo.f90 \
            CRTM_Parameters.f90 \
            CRTM_GeometryInfo_Define.f90 \
            CRTM_Geometry_Define.f90 \
            MW_SensorData_Define.f90 \
            Liebe89_Coefficients.f90 \
            Liebe92_Coefficients.f90 \
            Liebe93_Coefficients.f90 \
            Rosenkranz03_Coefficients.f90 \
            MWLBL_Liebe89.f90 \
            MWLBL_Liebe93.f90 \
            MWLBL_Rosenkranz03.f90 \
            MWLBL_Transmittance.f90
            
INC_FILES = AtmProfile_netCDF_IO.inc \
            oSRF_Flag_Parameters.inc \
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
NETCDF_INCLUDE = -I$(HOME)/local/include
NETCDF_LIBRARY = -L$(HOME)/local/lib -lnetcdff
PROFILE_UTILITY_INCLUDE = -I$(HOME)/local/profile_utility/include
PROFILE_UTILITY_LIBRARY = -L$(HOME)/local/profile_utility/lib -lprofile_utility
INCLUDES = $(NETCDF_INCLUDE) $(PROFILE_UTILITY_INCLUDE)
LIBRARIES = $(NETCDF_LIBRARY) $(PROFILE_UTILITY_LIBRARY)

# Define common make targets (all, build, clean, install)
include $(CRTM_SOURCE_ROOT)/make.common_targets

# Create and remove source links
create_links::
	@$(LINK_SCRIPT) $(CRTM_SOURCE_ROOT) $(SRC_FILES) $(INC_FILES)
                   
remove_links::
	@$(UNLINK_SCRIPT) $(SRC_FILES) $(INC_FILES)

# Source dependencies
include make.dependencies

# Define default rules
include $(CRTM_SOURCE_ROOT)/make.rules
