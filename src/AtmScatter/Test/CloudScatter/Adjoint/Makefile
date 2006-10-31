#==============================================================================
#
# Makefile for CRTM CloudScatter Adjoint Test program
#
#==============================================================================

# -------------
# Define macros
# -------------
include $(CRTM_SOURCE_ROOT)/make.macros

# This makefile
MAKE_FILE = Makefile

# Executable files
EXE_FILE = Test_Adjoint

# Source files to link
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
            CRTM_SpcCoeff.f90 \
            CRTM_TauCoeff.f90 \
            CRTM_CloudCoeff.f90 \
            CRTM_AerosolCoeff.f90 \
            CRTM_EmisCoeff.f90 \
            CRTM_Atmosphere_Define.f90 \
            CRTM_Cloud_Define.f90 \
            CRTM_Aerosol_Define.f90 \
            CRTM_Atmosphere_Binary_IO.f90 \
            CRTM_Cloud_Binary_IO.f90 \
            CRTM_Aerosol_Binary_IO.f90 \
            CRTM_ChannelInfo_Define.f90 \
            CRTM_ChannelInfo.f90 \
            CRTM_GeometryInfo_Define.f90 \
            CRTM_AtmScatter_Define.f90 \
            CRTM_CloudScatter.f90 \
            CRTM_Parameters.f90 \
            CRTM_LifeCycle.f90 \
            ComponentTest_Define.f90 \
            ComponentTest_netCDF_IO.f90 \
            netCDF_Utility.f90 \
            netCDF_Dimension_Utility.f90 \
            netCDF_Variable_Utility.f90 \
            netCDF_Attribute_Utility.f90

# Obj files used in link phase
OBJ_FILES = ${SRC_FILES:.f90=.o} \
            $(EXE_FILE).o

# Include and library definitions
INCLUDES = -I$(HOME)/local/netcdf/include
LIBRARIES = -L$(HOME)/local/netcdf/lib -lnetcdf


# ------------------
# Define dependecies
# ------------------
# Make based on OS type
all:
	@echo "OS type detected: "`uname -s`
	@case `uname -s` in \
	  "SunOS")  make -f $(MAKE_FILE) build $(SUNOS_FLAGS) ;; \
	  "AIX")    make -f $(MAKE_FILE) build $(AIX_FLAGS) ;; \
	  "IRIX64") make -f $(MAKE_FILE) build $(IRIX64_FLAGS) ;; \
	  "Linux")  make -f $(MAKE_FILE) build $(LINUX_FLAGS) ;; \
	  *) echo "This system is not supported" ;; \
       esac

# Build the program
build: create_links $(OBJ_FILES)
	$(FL) $(OBJ_FILES) $(FL_FLAGS) $(EXE_FILE)

# Clean up
clean:
	$(REMOVE) $(OBJ_FILES) *.mod *.MOD *.stb

realclean: remove_links clean


# Source and (big-endian) data file link creation and removal
SENSORID_LIST = amsua_n17 \
                hirs3_n17 \
                ssmis_f16 \
                airs281SUBSET_aqua
create_links:
	@linkfiles $(CRTM_SOURCE_ROOT) $(SRC_FILES)
	@linkcoeffdata -b $(SENSORID_LIST)
	@linktestdata -b

remove_links:
	@unlinkfiles $(SRC_FILES)
	@unlinkcoeffdata
	@unlinktestdata

# Source dependency lists
include make.dependencies


# --------------------
# Define default rules
# --------------------
include $(CRTM_SOURCE_ROOT)/make.rules

