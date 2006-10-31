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
EXE_FILE = Rosenkranz03_test

# Source files to link
SRC_FILES = Type_Kinds.f90 \
            File_Utility.f90 \
            Message_Handler.f90 \
            Fundamental_Constants.f90 \
            Profile_Utility_Parameters.f90 \
            Atmospheric_Properties.f90 \
            Units_Conversion.f90 \
            Rosenkranz03_Coefficients.f90 \
            MWLBL_Rosenkranz03.f90

# Obj files used in link phase
OBJ_FILES = ${SRC_FILES:.f90=.o} \
            $(EXE_FILE).o


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
