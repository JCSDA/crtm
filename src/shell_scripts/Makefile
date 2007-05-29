#==============================================================================
#
# Makefile to install CRTM scripts
#
# $Id: Makefile,v 1.5 2006/06/28 19:45:29 wd20pd Exp $
#
#==============================================================================

# -------------
# Define macros
# -------------

include $(CRTM_SOURCE_ROOT)/make.macros

# Shell scripts to install
SHELL_SRC_DIR = $(CRTM_SOURCE_ROOT)/shell_scripts

SHELL_SRC_FILES = diff_CRTM \
                  tag_CRTM \
                  linkfiles \
                  unlinkfiles \
                  linkcoeffdata \
                  unlinkcoeffdata \
                  linktestdata \
                  unlinktestdata

# ------------
# Install them
# ------------
install:
	@if [ -d $(HOME)/bin ]; then \
	  cd $(HOME)/bin; \
	  for script_file in ${SHELL_SRC_FILES}; do \
	    $(LINK) ${SHELL_SRC_DIR}/$$script_file .; \
	  done; \
	fi

# ---------------
# Un-install them
# ---------------
uninstall:
	@if [ -d $(HOME)/bin ]; then \
	  cd $(HOME)/bin; \
	  for script_file in ${SHELL_SRC_FILES}; do \
	    $(REMOVE) $$script_file; \
	  done; \
	fi
