#==============================================================================
#
# Makefile for local working copy Sensor Planck Utility library build
#
# $Id$
#
#==============================================================================

# Define macros
include Build/make.macros


# Source files to link
include Build/libsrc/make.filelist


# Main target
all: create_links Build/all

install: Build/install

clean: Build/clean

distclean: remove_links Build/distclean

# ...Alternative names for distclean
realclean real_clean: distclean


# Subtargets. Format: <subdirectory>/<target>
Build/all       \
Build/install   \
Build/clean     \
Build/distclean \
:
	@subdir=`echo $@ | sed 's,/.*,,'`; \
	target=`echo $@ | sed 's,.*/,,'`; \
	$(MAKE) SUBDIR=$$subdir TARGET=$$target subdir_target


subdir_target:
	@echo ""
	@cd $(SUBDIR) && \
	  echo "Making \`$(TARGET)' in directory `pwd`" && \
	  echo "" && \
	  $(MAKE) $(TARGET) || exit 1
	@echo ""
	@echo "Returning to directory `pwd`"
	@echo ""


# File link creation and removal
create_links:
	@echo
	@cd Build/libsrc && \
	  echo "Creating source code links in directory `pwd`" && \
	  echo && \
	  $(LINK_SCRIPT) $(CRTM_SOURCE_ROOT) $(SRC_FILES) || exit 1
	@echo
	@echo "Returning to directory `pwd`"
	@echo

remove_links:
	@echo
	@cd Build/libsrc && \
	  echo "Removing source code links in directory `pwd`" && \
	  echo && \
	  $(UNLINK_SCRIPT) $(SRC_FILES) || exit 1
	@echo
	@echo "Returning to directory `pwd`"
	@echo


# Create tarball for distribution
# Invoke make like so:
#
#   $ make tar BUILD=My.Different.BuildName
#
# to create a tarball with different
# name from the default
BUILD = $(PACKAGE)
TAR_DIR = $(BUILD)
TAR_FILE = $(TAR_DIR).tar
tar:: clean create_links
	@$(LINK) $(BUILD_DIR) $(TAR_DIR)
	$(TARBALL_CREATE) $(TAR_FILE) --exclude .svn $(TAR_DIR)
	@$(REMOVE) $(TAR_DIR)
	compress $(TAR_FILE)
