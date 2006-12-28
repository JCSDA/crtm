#==============================================================================
#
# Makefile for Test_CRTM_SpcCoeff program
#
#==============================================================================

# Define macros
include $(CRTM_SOURCE_ROOT)/make.macros

# This makefile
MAKE_FILE = Makefile

# Executable file
EXE_FILE = Test_CRTM_SpcCoeff

# Source files to link
SRC_FILES = Type_Kinds.f90 \
            File_Utility.f90 \
            Binary_File_Utility.f90 \
            Message_Handler.f90 \
            Compare_Float_Numbers.f90 \
            Endian_Utility.f90 \
            SpcCoeff_Define.f90 \
            SpcCoeff_Binary_IO.f90 \
            CRTM_Parameters.f90 \
            CRTM_SpcCoeff.f90

# Obj files used in link phase
OBJ_FILES = ${SRC_FILES:.f90=.o} \
            $(EXE_FILE).o

# Data files to link
DATA_FILES = amsua_n17.SpcCoeff.bin \
             hirs3_n17.SpcCoeff.bin \
             imgr_g11.SpcCoeff.bin \
             ssmis_f16.SpcCoeff.bin

# Define common make targets (all, build, clean, install)
include $(CRTM_SOURCE_ROOT)/make.common_targets

# Link creation and removal
create_links:
	@linkfiles $(CRTM_SOURCE_ROOT) $(SRC_FILES)
	@linkfiles -d Big_Endian $(CRTM_FIXFILE_ROOT) $(DATA_FILES)
	@ln -sf amsua_n17.SpcCoeff.bin SpcCoeff.bin

remove_links:
	@unlinkfiles $(SRC_FILES)
	@unlinkfiles $(DATA_FILES)
	@rm -f SpcCoeff.bin

# Squeaky clean target
realclean: clean remove_links

# Source dependency lists
include make.dependencies

# Define default rules
include $(CRTM_SOURCE_ROOT)/make.rules
