#==============================================================================
#
# Makefile for local FFT Build package
#
#==============================================================================

# -------------
# Define macros
# -------------
include $(CRTM_SOURCE_ROOT)/make.macros

# The library directories
SOURCE_DIR = src
LIBRARY_DIR = lib
INCLUDE_DIR = include

# Source files to link
SRC_FILES = pkind.f90 \
            pfft.f90 \
            pfft2.f90 \
            fft.f90


# ------------------
# Define dependecies
# ------------------
# Make based on OS type
all: create_links
	@make -f makefile.dist all

# Install the library and include files in their repective directories
install:
	@make -f makefile.dist install

# Clean up
clean: remove_links
	@make -f makefile.dist clean

distclean: remove_links
	@make -f makefile.dist distclean


# File link creation and removal
macro_links:
	@linkfiles $(CRTM_SOURCE_ROOT) make.macros ; \
	cd $(SOURCE_DIR); \
	linkfiles $(CRTM_SOURCE_ROOT) make.rules; \
	cd ..

create_links: macro_links
	@cd $(SOURCE_DIR); \
	linkfiles $(CRTM_SOURCE_ROOT) $(SRC_FILES); \
	cd ..

remove_links: macro_links
	@cd $(SOURCE_DIR); \
	unlinkfiles $(SRC_FILES); \
	cd ..


# -------------------------------
# Create tarball for distribution
# -------------------------------
TAR_FILE = FFT.tar
tar: clean create_links
	$(TARBALL_CREATE) $(TAR_FILE) $(SOURCE_DIR)
	@$(COPY) makefile.dist makefile
	$(TARBALL_APPEND) $(TAR_FILE) makefile
	@$(REMOVE) makefile
	$(TARBALL_APPEND) $(TAR_FILE) make.macros
	compress $(TAR_FILE)
