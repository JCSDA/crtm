#==============================================================================
#
# Makefile for Liebe89 test program
#
#==============================================================================

# -------------
# Define macros
# -------------
include $(CRTM_SOURCE_ROOT)/make.macros

# This makefile
MAKE_FILE = Makefile

# Executable files
EXE_FILE = Liebe93_test

# Source files to link
SRC_FILES = Type_Kinds.f90 \
            File_Utility.f90 \
            Message_Handler.f90 \
            Liebe92_Coefficients.f90 \
            Liebe93_Coefficients.f90 \
            MWLBL_Liebe93.f90

# Obj files used in link phase
OBJ_FILES = ${SRC_FILES:.f90=.o} \
            $(EXE_FILE).o

# Extra compiler options to promote REALs to Double
EXTRA_FC_FLAGS=-r8


# ------------------
# Define dependecies
# ------------------
include $(CRTM_SOURCE_ROOT)/make.common_targets

# Create and remove source links
create_links:
	@linkfiles $(CRTM_SOURCE_ROOT) $(SRC_FILES)
                   
remove_links:
	@unlinkfiles $(SRC_FILES)

# Source dependencies
include make.dependencies


# --------------------
# Define default rules
# --------------------
include $(CRTM_SOURCE_ROOT)/make.rules
