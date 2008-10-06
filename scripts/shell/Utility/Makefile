#==============================================================================
#
# Makefile to install CRTM Utility scripts
#
#==============================================================================

# -------------
# Define macros
# -------------
include $(CRTM_SOURCE_ROOT)/make.macros

# Shell scripts to install
SRC_DIR = $(CRTM_SCRIPTS_ROOT)/shell/Utility

SRC_FILES = linkfiles \
            unlinkfiles \
            Link_SensorInfo \
            Link_Solar \
            Link_SRF

# ------------
# Install them
# ------------
install:
	@if [ -d $(HOME)/bin ]; then \
	  cd $(HOME)/bin; \
	  for script_file in ${SRC_FILES}; do \
	    $(LINK) ${SRC_DIR}/$$script_file .; \
	  done; \
	fi

# ---------------
# Un-install them
# ---------------
uninstall:
	@if [ -d $(HOME)/bin ]; then \
	  cd $(HOME)/bin; \
	  for script_file in ${SRC_FILES}; do \
	    $(REMOVE) $$script_file; \
	  done; \
	fi
