#==============================================================================
#
# Makefile for setup of library build in local working copy.
#
# All this makefile does is link in source and coefficient files in the
# required places. Maybe change it to a script?
#
#==============================================================================

# Define macros
include make.macros


# Source files to link
include $(BUILD_DIR)/$(LIBSRC_DIR)/make.filelist

# Make include files to link
MAKE_INC_FILES = make.rules

# Sensor ids to link for build check target
CHECK_SENSOR_ID_LIST = cris399_npp atms_npp


# Main target
all: create_links

clean: remove_links
realclean real_clean: clean


# File link creation and removal
create_links: create_coeff_links
	@echo
	@cd $(BUILD_DIR)/$(LIBSRC_DIR) && \
	  echo "Creating source code links in directory `pwd`" && \
	  echo && \
	  $(LINK_SCRIPT) $(CRTM_SOURCE_ROOT) $(SRC_FILES) $(INC_FILES) $(MAKE_INC_FILES) || exit 1
	@echo
	@echo "Returning to directory `pwd`"
	@echo

remove_links: remove_coeff_links
	@echo
	@cd $(BUILD_DIR)/$(LIBSRC_DIR) && \
	  echo "Removing source code links in directory `pwd`" && \
	  echo && \
	  $(UNLINK_SCRIPT) $(SRC_FILES) $(INC_FILES) $(MAKE_INC_FILES) || exit 1
	@echo
	@echo "Returning to directory `pwd`"
	@echo

create_coeff_links:
	@echo
	@cd $(BUILD_DIR)/$(LIBSRC_DIR)/$(CHECK_DIR)/$(COEFF_DIR) && \
	  link_coeffs.sh -t big-endian $(CRTM_FIXFILE_ROOT) big_endian $(CHECK_SENSOR_ID_LIST) && \
	  link_coeffs.sh -t little-endian $(CRTM_FIXFILE_ROOT) little_endian $(CHECK_SENSOR_ID_LIST) && \
          link_coeffs.sh -t netcdf4 $(CRTM_FIXFILE_ROOT) netcdf $(CHECK_SENSOR_ID_LIST) || exit 1
	@echo
	@echo "Returning to directory `pwd`"
	@echo

remove_coeff_links:
	@echo
	@cd $(BUILD_DIR)/$(LIBSRC_DIR)/$(CHECK_DIR)/$(COEFF_DIR) && \
	  $(REMOVE) big_endian/*.bin && \
	  $(REMOVE) little_endian/*.bin && \
          $(REMOVE) netcdf/*.nc4 || exit 1
	@echo
	@echo "Returning to directory `pwd`"
	@echo
