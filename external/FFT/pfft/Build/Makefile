#==============================================================================
#
# Makefile for local FFT Build package
#
#==============================================================================

# Define macros
# -------------
include $(CRTM_SOURCE_ROOT)/make.macros

# The library name
LIBRARY_TAG = fft
LIBRARY_NAME = lib$(LIBRARY_TAG).a

# The library directories
SOURCE_DIR = src
LIBRARY_DIR = lib
INCLUDE_DIR = include

# Location of "local" install directory
LOCAL_DIR = $(HOME)/local/FFT/pfft

# Source files to link
SRC_FILES = pkind.f90 \
            pfft.f90 \
            fft.f90

# Macro files to link in SOURCE_DIR
MACRO_FILES = make.macros make.rules


# Make based on OS type
# ---------------------
all: create_links
	cd $(SOURCE_DIR); make; cd ..


# Install the library and include files
# -------------------------------------
install: install_lib install_include

install_lib:
	@if [ ! -d $(LIBRARY_DIR) ]; then \
	  mkdir $(LIBRARY_DIR); \
	fi; \
	cd $(SOURCE_DIR); \
	if [ -f $(LIBRARY_NAME) ]; then \
	  $(MOVE) $(LIBRARY_NAME) ../$(LIBRARY_DIR); \
	fi ; \
	cd ..

install_include:
	@if [ ! -d $(INCLUDE_DIR) ]; then \
	  mkdir $(INCLUDE_DIR); \
	fi; \
	cd $(SOURCE_DIR); \
	for FILE in `ls -1 | egrep "\.(mod|MOD|m|M)$$"`; do \
	  if [ -f $$FILE ]; then \
	    $(COPY) -f $$FILE ../$(INCLUDE_DIR); \
	  fi ; \
	done ; \
	cd ..

install_local: install
	@if [ -d $(LOCAL_DIR) ]; then \
	  $(COPY) -r $(INCLUDE_DIR) $(LOCAL_DIR); \
	  $(COPY) -r $(LIBRARY_DIR) $(LOCAL_DIR); \
	else \
	  echo "Unable to install FFT lib to ${LOCAL_DIR}"; \
	fi


# Clean up
# --------
distclean: clean_src clean_lib clean_include
	@make remove_links

clean_src:
	@cd $(SOURCE_DIR); make clean; cd ..

clean_lib:
	$(REMOVE)r $(LIBRARY_DIR) &2>/dev/null

clean_include:
	$(REMOVE)r $(INCLUDE_DIR) &2>/dev/null


# File link creation and removal
# ------------------------------
create_links:
	@cd $(SOURCE_DIR); \
	$(LINK_SCRIPT) $(CRTM_SOURCE_ROOT) $(MACRO_FILES); \
	$(LINK_SCRIPT) $(CRTM_EXTERNALS_ROOT) $(SRC_FILES); \
	cd ..

remove_links:
	@cd $(SOURCE_DIR); \
	$(UNLINK_SCRIPT) $(MACRO_FILES) $(SRC_FILES); \
	cd ..
