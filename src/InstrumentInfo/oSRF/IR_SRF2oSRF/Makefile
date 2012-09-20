#========================================
#
# Makefile for program IR_SRF2oSRF
#
#========================================

# Define macros
include $(CRTM_SOURCE_ROOT)/make.macros

# This makefile
MAKE_FILE = Makefile

# Executable file
EXE_FILE = IR_SRF2oSRF

# Source files to link
SRC_FILES = Type_Kinds.f90 \
            File_Utility.f90 \
            String_Utility.f90 \
            Sort_Utility.f90 \
            CRTM_Parameters.f90 \
            Planck_Functions.f90 \
            Spectral_Units_Conversion.f90 \
            Fundamental_Constants.f90 \
            PtrArr_Define.f90 \
            Message_Handler.f90 \
            Compare_Float_Numbers.f90 \
            SensorInfo_Parameters.f90 \
            SensorInfo_Define.f90 \
            SensorInfo_LinkedList.f90 \
            SensorInfo_IO.f90 \
            Search_Utility.f90 \
            Polynomial_Interpolation.f90 \
            Spline_Interpolation.f90 \
            Linear_Interpolation.f90 \
            Interpolate_Utility.f90 \
            Integrate_Utility.f90 \
            netCDF_Dimension_Utility.f90 \
            netCDF_Variable_Utility.f90 \
            netCDF_Attribute_Utility.f90 \
            netCDF_Utility.f90 \
            SRF_Define_old.f90 \
            SRF_netCDF_IO_old.f90 \
            oSRF_Define.f90 \
            oSRF_File_Define.f90 
            
# Obj files used in link phase
OBJ_FILES = ${SRC_FILES:.f90=.o} \
            $(EXE_FILE).o

# Include and library definitions
#INCLUDES = -I$(HOME)/local/netcdf/include
#LIBRARIES = -L$(HOME)/local/netcdf/lib -lnetcdf
INCLUDES = -I$(NETCDF)/include
LIBRARIES = -L$(NETCDF)/lib -lnetcdff

# Define common make targets (all, build, clean, install)
include $(CRTM_SOURCE_ROOT)/make.common_targets

# Source link creation and removal
create_links::
	@$(LINK_SCRIPT) $(CRTM_SOURCE_ROOT) $(SRC_FILES)

remove_links::
	@$(UNLINK_SCRIPT) $(SRC_FILES)

# Squeaky clean target
realclean:: clean remove_links

# Source dependency lists
include make.dependencies

# Define default rules
include $(CRTM_SOURCE_ROOT)/make.rules
