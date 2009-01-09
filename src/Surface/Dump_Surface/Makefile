#==============================================================================
#
# Makefile for Dump_Surface program
#
#==============================================================================

# Define macros
include $(CRTM_SOURCE_ROOT)/make.macros

# This makefile
MAKE_FILE = Makefile

# Executable file
EXE_FILE = Dump_Surface

# Obj files used in link phase
OBJ_FILES = $(EXE_FILE).o

# Include and library definitions
INCLUDE_DIR = $(CRTM_SOURCE_ROOT)/Build/include
LIBRARY_DIR = $(CRTM_SOURCE_ROOT)/Build/lib
LIBRARY_NAME = CRTM
INCLUDES = -I$(INCLUDE_DIR)
LIBRARIES = -L$(LIBRARY_DIR) -l$(LIBRARY_NAME)

# Define common make targets (all, build, clean, install)
include $(CRTM_SOURCE_ROOT)/make.common_targets

# Source dependency lists
include make.dependencies

# Define default rules
include $(CRTM_SOURCE_ROOT)/make.rules
