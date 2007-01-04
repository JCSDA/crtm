#==============================================================================
#
# Makefile for local JCSDA CRTM Build package
#
#==============================================================================

# -------------
# Define macros
# -------------
include make.macros

# The library directories
BUILD_DIR = Build
SOURCE_DIR = src
LIBRARY_DIR = lib
INCLUDE_DIR = include

# The linkfile script
LINKSCRIPT = linkFiles.rb
#LINKSCRIPT = linkfiles

# Source files to link
include $(BUILD_DIR)/$(SOURCE_DIR)/make.filelist


# ------------------
# Define dependecies
# ------------------
# Make based on OS type
all: create_links
	@cd $(BUILD_DIR); make all; cd ..

# Install the library and include files in their repective directories
install:
	@cd $(BUILD_DIR); make install; cd ..

# Clean up
clean:
	@cd $(BUILD_DIR); make clean; cd ..

distclean:
	@cd $(BUILD_DIR); make distclean; cd ..


# File link creation and removal
macro_links:
	@cd $(BUILD_DIR); \
	$(LINKSCRIPT) $(CRTM_SOURCE_ROOT) make.macros ; \
	cd $(SOURCE_DIR); \
	$(LINKSCRIPT) $(CRTM_SOURCE_ROOT) make.rules; \
	cd ../..

create_links: macro_links
	@cd $(BUILD_DIR)/$(SOURCE_DIR); \
	$(LINKSCRIPT) $(CRTM_SOURCE_ROOT) $(SRC_FILES); \
	cd ../..

remove_links:
	@cd $(BUILD_DIR); \
	unlinkfiles make.macros ; \
	cd $(SOURCE_DIR); \
	unlinkfiles make.rules $(SRC_FILES) ; \
	cd ../..


# --------------------------------------------------
# Create tarball for distribution
# Invoke make like so:
#
#   make tar BUILD=My.Different.BuildName.JCSDA_CRTM
#
# to create a tarball with different
# name from the default (JCSDA_CRTM)
# --------------------------------------------------

BUILD = JCSDA_CRTM
TAR_DIR = $(BUILD)
TAR_FILE = $(TAR_DIR).tar
tar: create_links distclean
	@$(LINK) $(BUILD_DIR) $(TAR_DIR)
	$(TARBALL_CREATE) $(TAR_FILE) --exclude .svn $(TAR_DIR)
	@$(REMOVE) $(TAR_DIR)
	compress $(TAR_FILE)
