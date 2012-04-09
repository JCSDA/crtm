#==============================================================================
#
# Makefile for *local, working copy* JCSDA CRTM library build
#
#==============================================================================

# Define macros
include make.macros


# Source files to link
include $(BUILD_DIR)/$(LIBSRC_DIR)/make.filelist


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
	$(MAKE) -s SUBDIR=$$subdir TARGET=$$target subdir_target


subdir_target:
	@echo ""
	@cd $(SUBDIR) && \
	  echo "Making \`$(TARGET)' in directory `pwd`" && \
	  echo "" && \
	  $(MAKE) -s $(TARGET) || exit 1
	@echo ""
	@echo "Returning to directory `pwd`"
	@echo ""


# File link creation and removal
create_links: create_coeff_links
	@echo
	@cd $(BUILD_DIR)/$(LIBSRC_DIR) && \
	  echo "Creating source code links in directory `pwd`" && \
	  echo && \
	  $(LINK_SCRIPT) $(CRTM_SOURCE_ROOT) $(SRC_FILES) $(INC_FILES)  || exit 1
	@echo
	@echo "Returning to directory `pwd`"
	@echo

remove_links: remove_coeff_links
	@echo
	@cd $(BUILD_DIR)/$(LIBSRC_DIR) && \
	  echo "Removing source code links in directory `pwd`" && \
	  echo && \
	  $(UNLINK_SCRIPT) $(SRC_FILES) $(INC_FILES) || exit 1
	@echo
	@echo "Returning to directory `pwd`"
	@echo

create_coeff_links remove_coeff_links:
	@echo
	@cd $(BUILD_DIR)/$(TEST_DIR) && \
	CATEGORY_DIR=`ls -d */` && \
	for CATEGORY in $$CATEGORY_DIR; do \
	  cd $$CATEGORY && \
	  EXAMPLE_DIR=`ls -d Example*_*` && \
	  for EXAMPLE in $$EXAMPLE_DIR; do \
	    cd $$EXAMPLE/coefficients && \
	    $(MAKE) -s -f make.link_data $@ && \
	    cd ../..; \
	  done; \
          cd ..; \
	done || exit 1
	@echo
	@echo "Returning to directory `pwd`"
	@echo
        

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
tar: create_links
	@$(LINK) $(BUILD_DIR) $(TAR_DIR)
	$(TARBALL_CREATE) $(TAR_FILE) --exclude-from make.tarball_exclude $(TAR_DIR)
	@$(REMOVE) $(TAR_DIR)
	compress $(TAR_FILE)
