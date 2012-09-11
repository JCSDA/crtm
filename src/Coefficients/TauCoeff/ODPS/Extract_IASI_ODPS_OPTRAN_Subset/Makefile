#==============================================================================
#
# Makefile for Extract_IASI_ODPS_Subset program
#
#==============================================================================

# Define macros
include $(CRTM_SOURCE_ROOT)/make.macros

# This makefile
MAKE_FILE = Makefile

# Executable files
EXE_FILE = Extract_IASI_TauCoeff_Subset

# Source files to link
SRC_FILES = Type_Kinds.f90 \
            File_Utility.f90 \
            Message_Handler.f90 \
            Compare_Float_Numbers.f90 \
            Fundamental_Constants.f90 \
            SensorInfo_Parameters.f90 \
            List_File_Utility.f90 \
            Sort_Utility.f90 \
            netCDF_Utility.f90 \
            netCDF_Dimension_Utility.f90 \
            netCDF_Variable_Utility.f90 \
            netCDF_Attribute_Utility.f90 \
            Subset_Define.f90 \
            IASI_Define.f90 \
            IASI_Subset.f90 \
            CRTM_Parameters.f90 \
            ODPS_Define.f90 \
            ODPS_netCDF_IO.f90
# Coeff files to link
COEFF_FILES = iasiB1_metop-a.TauCoeff.nc \
              iasiB2_metop-a.TauCoeff.nc \
              iasiB3_metop-a.TauCoeff.nc


# Obj files used in link phase
OBJ_FILES = ${SRC_FILES:.f90=.o} \
            $(EXE_FILE).o

# Include and library definitions
#INCLUDES = -I$(HOME)/local/netcdf/include
#LIBRARIES = -L$(HOME)/local/netcdf/lib -lnetcdf
INCLUDES = -I$(NETCDF)/include
LIBRARIES = -mkl=sequential -L$(NETCDF)/lib -lnetcdff

# Define common make targets (all, build, clean, install)
include $(CRTM_SOURCE_ROOT)/make.common_targets

# Source link creation and removal
create_links::
	@$(LINK_SCRIPT) $(CRTM_SOURCE_ROOT) $(SRC_FILES)
	@$(LINK_SCRIPT) -d ODPS $(CRTM_FIXFILE_ROOT) $(COEFF_FILES)

remove_links::
	@$(UNLINK_SCRIPT) $(SRC_FILES)
	@$(UNLINK_SCRIPT) $(COEFF_FILES)

# Squeaky clean target
realclean:: clean remove_links

# Source dependency lists
include make.dependencies

# Define default rules
include $(CRTM_SOURCE_ROOT)/make.rules

