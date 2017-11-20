#==============================================================================
#
# Makefile for Cloud_Fraction_CRTM program
#
#==============================================================================

# Define file macros for this build
MAKE_FILE = Makefile
# ...Executable file
EXE_FILE = Cloud_Fraction
# ...Source files to link
SRC_FILES = Timing_Utility.f90 \
            CRTM_CloudCover_Define.f90
# ...Obj files used in link phase
OBJ_FILES = ${SRC_FILES:.f90=.o} \
            $(EXE_FILE).o


# Library and include macros
CRTM_VERSION = `tr -d "'" < $(CRTM_SOURCE_ROOT)/CRTM_Version.inc`
CRTM_DIR = $(CRTM_SOURCE_ROOT)/Build/crtm_$(CRTM_VERSION)
INCLUDES = -I$(CRTM_DIR)/include
LIBRARIES = -L$(CRTM_DIR)/lib -lcrtm


# Coefficient macro definitions
ENDIAN_TYPE = Big_Endian
TAUCOEFF_TYPE = ODPS
SENSOR_IDS = cris399_npp atms_npp amsr2_gcom-w1


# Profile data macro definitions
PROFILE_DATA_FILES = ecmwf_5k_atmosphereccol.bin \
                     ecmwf_5k_geometryccol.bin \
                     ecmwf_5k_surfaceccol.bin \
                     Model6.Atmosphere.bin \
                     Model6.Surface.bin \
                     Model6.Geometry.bin \
                     gsi58_atm.bin \
                     gsi58_geo.bin \
                     gsi58_sfc.bin


# Common make include files
include $(CRTM_SOURCE_ROOT)/make.macros
include $(CRTM_SOURCE_ROOT)/make.common_targets
include $(CRTM_SOURCE_ROOT)/make.test_data_targets


# Create/remove the source and data links
create_links:: create_test_data_links
	@$(LINK_SCRIPT) $(CRTM_SOURCE_ROOT) $(SRC_FILES)
remove_links:: remove_test_data_links
	@$(UNLINK_SCRIPT) $(SRC_FILES)


# Source dependency lists
include make.dependencies


# Define default rules
include $(CRTM_SOURCE_ROOT)/make.rules
