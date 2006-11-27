#==============================================================================
#
# Makefile for Assemble_Coefficient_File program
#
#==============================================================================

# -------------
# Define macros
# -------------
include make.macros

# This makefile
MAKE_FILE = Makefile

# Executable file
EXE_FILE = Assemble_Coefficient_File

# Source files
include make.filelist

# Obj files used in link phase
OBJ_FILES = ${SRC_FILES:.f90=.o}

# Include and library definitions
INCLUDES = -I$(HOME)/local/netcdf/include
LIBRARIES = -L$(HOME)/local/netcdf/lib -lnetcdf


# -------------------
# Define dependencies
# -------------------
include make.common_targets
include make.dependencies


# --------------------
# Define default rules
# --------------------
include make.rules
