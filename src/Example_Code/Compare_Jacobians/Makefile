#==============================================================================
#
# Makefile for CRTM Compare_Jacobians local test program
#
# $Id: $
#
#==============================================================================

# -------------
# Define macros
# -------------
include $(CRTM_SOURCE_ROOT)/make.macros

# This makefile
MAKE_FILE = Makefile

# The executable
EXE_FILE = Compare_Jacobians

# Source files to link
SRC_FILES = CRTM_Test_Utility.f90

# Obj files
OBJ_FILES = ${SRC_FILES:.f90=.o} \
            $(EXE_FILE).o

# CRTM Library and include macros
INCLUDES = -I$(CRTM_SOURCE_ROOT)/Build/include
LIBRARIES = -L$(CRTM_SOURCE_ROOT)/Build/lib -lCRTM

# Macros for linking source and data
LINK_SCRIPT = linkfiles
UNLINK_SCRIPT = unlinkfiles
COEFF_DATA_FILES = EmisCoeff.bin AerosolCoeff.bin CloudCoeff.bin
SENSOR_IDS = hirs4_n18 amsua_n18 mhs_n18
DATA_ENDIAN_TAG = Big_Endian

# Define common make targets (all, build, clean, install)
include $(CRTM_SOURCE_ROOT)/make.common_targets

# Create/remove the source and data links
create_links:
	@$(LINK_SCRIPT) $(CRTM_SOURCE_ROOT) $(SRC_FILES); \
	$(LINK_SCRIPT) -d ${DATA_ENDIAN_TAG} ${CRTM_FIXFILE_ROOT} ${COEFF_DATA_FILES}; \
	for id in ${SENSOR_IDS}; do \
	  $(LINK_SCRIPT) -d ${DATA_ENDIAN_TAG} ${CRTM_FIXFILE_ROOT} $$id.SpcCoeff.bin; \
	  $(LINK_SCRIPT) -d ${DATA_ENDIAN_TAG} ${CRTM_FIXFILE_ROOT} $$id.TauCoeff.bin; \
	done

remove_links:
	@$(UNLINK_SCRIPT) $(SRC_FILES); \
	$(UNLINK_SCRIPT) ${COEFF_DATA_FILES}; \
	for id in ${SENSOR_IDS}; do \
	  $(UNLINK_SCRIPT) $$id.SpcCoeff.bin; \
	  $(UNLINK_SCRIPT) $$id.TauCoeff.bin; \
	done

# Squeaky clean target
realclean: clean remove_links

# Source dependency lists
include make.dependencies

# Define default rules
include $(CRTM_SOURCE_ROOT)/make.rules
