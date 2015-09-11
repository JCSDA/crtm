#==============================================================================
#
# Makefile for setup of library build in local working copy.
#
# All this makefile does is link in source files in the required places.
# Maybe change it to a script?
#
#==============================================================================

# Define macros
include $(CRTM_SOURCE_ROOT)/make.macros

# ...Define the library directories
BUILD_DIR = Build
LIBSRC_DIR = libsrc


# Source files to link
include $(BUILD_DIR)/$(LIBSRC_DIR)/make.filelist


# Targets
all: create_links

clean: remove_links
realclean real_clean: clean


# ...File link creation and removal
create_links:
	@echo
	@cd $(BUILD_DIR)/$(LIBSRC_DIR) && \
	  echo "Creating source code links in directory `pwd`" && \
	  echo && \
	  $(LINK_SCRIPT) $(CRTM_SOURCE_ROOT) $(SRC_FILES) $(INC_FILES) || exit 1
	@echo
	@echo "Returning to directory `pwd`"
	@echo

remove_links:
	@echo
	@cd $(BUILD_DIR)/$(LIBSRC_DIR) && \
	  echo "Removing source code links in directory `pwd`" && \
	  echo && \
	  $(UNLINK_SCRIPT) $(SRC_FILES) $(INC_FILES) || exit 1
	@echo
	@echo "Returning to directory `pwd`"
	@echo

