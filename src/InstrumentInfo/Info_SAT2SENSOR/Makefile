#==============================================================================
#
# Makefile for Info_SAT2SENSOR program
#
#==============================================================================

# -------------
# Define macros
# -------------
include $(CRTM_SOURCE_ROOT)/make.macros

# This makefile
MAKE_FILE = Makefile

# Executable files
EXE_FILE = Info_SAT2SENSOR

# Source files to link
BUILD_DIR = ./Build
include $(BUILD_DIR)/make.filelist

# Macro files to link
MACRO_FILES = make.macros make.rules make.common_targets

# The Link script
LINK_SCRIPT = linkFiles.rb


# ------------------
# Define dependecies
# ------------------
# Make based on OS type
all: create_links
	@cd $(BUILD_DIR); make; cd ..

# Clean up
clean: create_macro_links
	@cd $(BUILD_DIR); make clean; cd ..

realclean: clean remove_links
	@cd $(BUILD_DIR); $(REMOVE) $(EXE_FILE); cd ..

# Install it
install: create_macro_links
	@cd $(BUILD_DIR); make install; cd ..


# Source file link creation and removal
create_links: create_macro_links create_source_links

create_macro_links:
	@cd $(BUILD_DIR); \
	  $(LINK_SCRIPT) $(CRTM_SOURCE_ROOT) $(MACRO_FILES); \
	cd ..

create_source_links:
	@cd $(BUILD_DIR); \
	  $(LINK_SCRIPT) $(CRTM_SOURCE_ROOT) $(CRTM_SRC_FILES); \
	  $(LINK_SCRIPT) $(GSI_SOURCE_ROOT) $(GSI_SRC_FILES); \
	cd ..

remove_links:
	@cd $(BUILD_DIR); \
	  unlinkfiles $(MACRO_FILES) $(SRC_FILES); \
	cd ..
        
# Create tarball for distribution
TAR_DIR = $(EXE_FILE)
TAR_FILE = $(EXE_FILE)_software.tar
tar: clean create_links
	@$(LINK) $(BUILD_DIR) $(TAR_DIR)
	$(TARBALL_CREATE) $(TAR_FILE) --exclude .svn $(TAR_DIR)
	@$(REMOVE) $(TAR_DIR)
	compress $(TAR_FILE)
